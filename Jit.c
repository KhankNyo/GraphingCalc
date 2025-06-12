#include "JitCommon.h"
#include "TargetEnv.h"
#include "Jit.h"
#include "DefTable.h"

#include <string.h> /* memset */
#include <stdio.h>
#include <math.h>
#include <stdarg.h>


typedef enum precedence 
{
    PREC_NONE = 0,
    PREC_EXPR, 
    PREC_PLUSMINUS,
    PREC_MULDIV,
    PREC_POW,
    PREC_UNARY,
} precedence;

typedef enum jit_ir_op_type 
{
    IR_OP_ADD,
    IR_OP_SUB,
    IR_OP_MUL,
    IR_OP_DIV,
    IR_OP_NEG,
    IR_OP_LOAD,
    IR_OP_CALL_ARG_START,
    IR_OP_CALL,
    IR_OP_SWAP,
    IR_OP_STORE,
    IR_OP_FN_BEGIN,
    IR_OP_ENTRY, 
    IR_OP_RETURN,
} jit_ir_op_type;
typedef enum jit_ir_data_type 
{
    IR_DATA_CONST,
    IR_DATA_VAR_REF,
    IR_DATA_PARAM,
} jit_ir_data_type;
typedef struct jit_ir_data 
{
    jit_ir_data_type Type;
    union {
        double Const;
        jit_token VarRef;
        struct {
            strview Name;
            jit_location Location;
        } Param;
    } As;
} jit_ir_data;
typedef struct jit_ir_op
{
    union {
        int LoadIndex; 
        struct {
            int ArgCount;
            jit_token FnName;
        } Call;
        struct {
            i32 StackSize;
        } Entry;
        jit_function *FnBegin;
        jit_mem StoreDst;
    };
    jit_ir_op_type Type;
} jit_ir_op;

#define FN_LOCATION_UNDEFINED -1


static int Jit_Ir_CompileExpr(jit *Jit, precedence Prec, bool8 AllowForwardReference);

static const char *Get_OpName(jit_ir_op_type Op)
{
    const char *Name[] = {
        [IR_OP_ADD] = "IR_OP_ADD", 
        [IR_OP_SUB] = "IR_OP_SUB", 
        [IR_OP_MUL] = "IR_OP_MUL", 
        [IR_OP_DIV] = "IR_OP_DIV", 
        [IR_OP_NEG] = "IR_OP_NEG", 
        [IR_OP_LOAD] = "IR_OP_LOAD", 
        [IR_OP_CALL_ARG_START] = "IR_OP_CALL_ARG_START", 
        [IR_OP_CALL] = "IR_OP_CALL", 
        [IR_OP_SWAP] = "IR_OP_SWAP", 
        [IR_OP_STORE] = "IR_OP_STORE", 
        [IR_OP_FN_BEGIN] = "IR_OP_FN_BEGIN", 
        [IR_OP_ENTRY] = "IR_OP_ENTRY",  
        [IR_OP_RETURN] = "IR_OP_RETURN", 
    };
    return Name[Op];
}


static jit_location Jit_AllocateStack(jit *Jit)
{
    return (jit_location) {
        .Storage = STORAGE_MEM,
        .As.Mem = Storage_AllocateStack(&Jit->Storage),
    };
}



static u8 *Jit_GetEmitterBuffer(const jit *Jit)
{
    return Jit->Emitter.Base.Buffer;
}

static int Jit_GetEmitterBufferSize(const jit *Jit)
{
    return Jit->Emitter.Base.BufferSize;
}

static int Jit_Scratchpad_BytesRemain(const jit *Jit)
{
    i64 Result = (i64)Jit->ScratchpadCapacity - ((i64)Jit->ScratchpadLeftByteCount + (i64)Jit->ScratchpadRightByteCount);
    ASSERT(Result >= 0, "unreachable");
    return (uint)Result;
}

static void *Jit_Scratchpad_LeftPtr(jit *Jit)
{
    return Jit->ScratchpadPtr + Jit->ScratchpadLeftByteCount;
}

static void *Jit_Scratchpad_RightPtr(jit *Jit)
{
    return Jit->ScratchpadPtr + Jit->ScratchpadCapacity - Jit->ScratchpadRightByteCount;
}

static void *Jit_Scratchpad_PushLeft(jit *Jit, int DataSize)
{
    ASSERT(DataSize <= Jit_Scratchpad_BytesRemain(Jit), "bad data size");
    void *Ptr = Jit->ScratchpadPtr + Jit->ScratchpadLeftByteCount;
    Jit->ScratchpadLeftByteCount += DataSize;
    return Ptr;
}

static void *Jit_Scratchpad_PushRight(jit *Jit, int DataSize)
{
    ASSERT(DataSize <= Jit_Scratchpad_BytesRemain(Jit), "bad data size");
    void *Ptr = Jit->ScratchpadPtr + Jit->ScratchpadCapacity - (Jit->ScratchpadRightByteCount + DataSize);
    Jit->ScratchpadRightByteCount += DataSize;
    return Ptr;
}

static void Jit_Scratchpad_PopLeft(jit *Jit, int DataSize)
{
    ASSERT(DataSize <= Jit->ScratchpadLeftByteCount, "bad data size");
    Jit->ScratchpadLeftByteCount -= DataSize;
}

static void Jit_Scratchpad_PopRight(jit *Jit, int DataSize)
{
    ASSERT(DataSize <= Jit->ScratchpadRightByteCount, "bad data size");
    Jit->ScratchpadRightByteCount -= DataSize;
}




typedef struct jit_ir_stack 
{
    jit *Jit;
    int Count;
} jit_ir_stack;

static jit_ir_stack Ir_Stack_Init(jit *Jit)
{
    return (jit_ir_stack) {
        .Jit = Jit,
    };
}

static jit_location *Ir_Stack_Top(jit_ir_stack *Stack, int Offset)
{
    ASSERT(Offset >= 0, "bad offset");
    return (jit_location *)Jit_Scratchpad_LeftPtr(Stack->Jit) - 1 - Offset;
}

static jit_location *Ir_Stack_Push(jit_ir_stack *Stack, const jit_location *Value)
{
    Stack->Count++;
    jit_location *Top = Jit_Scratchpad_PushLeft(Stack->Jit, sizeof(*Top));
    *Top = *Value;
    return Top;
}

static jit_location *Ir_Stack_Pop(jit_ir_stack *Stack)
{
    ASSERT(Stack->Count != 0, "Cannot pop stack");
    Stack->Count--;
    jit_location *Top = Ir_Stack_Top(Stack, 0);
    Jit_Scratchpad_PopLeft(Stack->Jit, sizeof(jit_location));
    return Top;
}

static void Ir_Stack_PopMultiple(jit_ir_stack *Stack, int Count)
{
    ASSERT(Stack->Count >= Count, "unreachable");
    Stack->Count -= Count;
    Jit_Scratchpad_PopLeft(Stack->Jit, Count*sizeof(jit_location));
}

static void Ir_Stack_SpillReg(jit_ir_stack *Stack, jit *Jit)
{
    for (int i = 0; i < Stack->Count; i++)
    {
        jit_location *Elem = Ir_Stack_Top(Stack, i);
        switch (Elem->Storage)
        {
        case STORAGE_MEM: break; /* already in memory, nothing to do */
        case STORAGE_REG:
        {
            jit_reg Src = Elem->As.Reg;
            jit_location Location = Jit_AllocateStack(Jit);
            Emit_Store(&Jit->Emitter, Src, Location.As.Mem.BaseReg, Location.As.Mem.Offset);
            Storage_DeallocateReg(&Jit->Storage, Src);

            *Elem = Location;
        } break;
        }
    }
}


typedef struct jit_fnref_stack
{
    jit *Jit;
    int Base;
} jit_fnref_stack;

typedef struct jit_fnref 
{
    const jit_function *Function;
    uint Location;
} jit_fnref;

static jit_fnref_stack Ir_FnRef_Init(jit *Jit)
{
    return (jit_fnref_stack) {
        .Jit = Jit,
        .Base = Jit->ScratchpadRightByteCount,
    };
}

static jit_fnref *Ir_FnRef_Top(jit_fnref_stack *FnRef, int Offset)
{
    ASSERT(Offset >= 0, "bad offset");
    return (jit_fnref *)Jit_Scratchpad_RightPtr(FnRef->Jit) + Offset;
}

static jit_fnref *Ir_FnRef_Push(jit_fnref_stack *FnRef, const jit_function *Function, uint CallLocation)
{
    jit_fnref *Top = Jit_Scratchpad_PushRight(FnRef->Jit, sizeof(*Top));
    Top->Function = Function;
    Top->Location = CallLocation;
    return Top;
}

static jit_fnref *Ir_FnRef_Pop(jit_fnref_stack *FnRef)
{
    jit_fnref *Top = Ir_FnRef_Top(FnRef, 0);
    Jit_Scratchpad_PopRight(FnRef->Jit, sizeof(jit_fnref));
    return Top;
}

static int Ir_FnRef_Count(const jit_fnref_stack *FnRef)
{
    return ABS(FnRef->Jit->ScratchpadRightByteCount - FnRef->Base) / sizeof(jit_fnref);
}







static bool8 ChInRange(char Lower, char n, char Upper)
{
    return IN_RANGE(Lower, n, Upper);
}

static bool8 IsNumber(char Ch)
{
    return ChInRange('0', Ch, '9');
}

static bool8 IsIdentifier(char Ch)
{
    return '_' == Ch || ChInRange('a', Lowercase(Ch), 'z');
}

static bool8 AtEnd(jit *Jit)
{
    return ((Jit)->End[0] == '\0');
}

static char Peek(jit *Jit, int Offset)
{
    for (int i = Jit->SafePeekDist; i < Offset; i++)
    {
        if (Jit->End[i] == '\0')
            return '\0';
    }
    if (Offset > Jit->SafePeekDist)
        Jit->SafePeekDist = Offset;
    return Jit->End[Offset];
}

static char Advance(jit *Jit)
{
    if (Jit->SafePeekDist > 0 || !AtEnd(Jit))
    {
        char Ch = *Jit->End++;
        Jit->Offset++;
        Jit->SafePeekDist -= Jit->SafePeekDist > 0;
        return Ch;
    }
    return '\0';
}

static jit_token CreateToken(jit *Jit, jit_token_type Type)
{
    int Len = Jit->End - Jit->Start;
    jit_token Tok = {
        .Type = Type,
        .Line = Jit->Line,
        .Offset = Jit->Offset - Len, 
        .Str = {
            .Ptr = Jit->Start,
            .Len = Len,
        },
    };
    Jit->Start = Jit->End;
    return Tok;
}

static jit_token ErrorToken(jit *Jit, const char *ErrMsg)
{
    jit_token Tok = CreateToken(Jit, TOK_ERR);
    Tok.As.ErrMsg = ErrMsg;
    return Tok;
}

static jit_token ParseNumber(jit *Jit, char First)
{
    double Number = First - '0';
    while (IsNumber(Peek(Jit, 0)))
    {
        Number *= 10;
        Number += Advance(Jit) - '0';
    }

    if ('.' == Peek(Jit, 0))
    {
        Advance(Jit); /* skip ; */
        double Decimal = 0;
        double Pow10 = 1;
        while (IsNumber(Peek(Jit, 0)))
        {
            Decimal *= 10;
            Pow10 *= 10;
            Decimal += Advance(Jit) - '0';
        }
        Number += Decimal / Pow10;
    }
    jit_token Tok = CreateToken(Jit, TOK_NUMBER);
    Tok.As.Number = Number;
    return Tok;
}

static jit_token ParseIdentifier(jit *Jit)
{
#define STREQU(literal, strv) \
    (((sizeof(literal) - 1) == (strv).Len)\
     && StrEqu(literal, (strv).Ptr, sizeof(literal) - 1))
    while (IsIdentifier(Peek(Jit, 0)) 
    || '_' == Peek(Jit, 0) 
    || IsNumber(Peek(Jit, 0)))
    {
        Advance(Jit);
    }

    jit_token Identifier = CreateToken(Jit, TOK_IDENTIFIER);
    return Identifier;
#undef STREQU
}


static void ConsumeSpace(jit *Jit)
{
    while (1)
    {
        switch (Peek(Jit, 0))
        {
        case ' ':
        case '\t':
        case '\r':
        {
            Advance(Jit);
        } break;
        case '#': /* # comment */
        {
            do {
                Advance(Jit);
            } while (!AtEnd(Jit) && Peek(Jit, 0) != '\n');
        } break;
        default: goto Out;
        }
    }

Out:
    Jit->Start = Jit->End;
}

static jit_token NewlineToken(jit *Jit)
{
    jit_token Tok = CreateToken(Jit, TOK_NEWLINE);
    Jit->Line++;
    Jit->Offset = 1;
    return Tok;
}

static jit_token Jit_Tokenize(jit *Jit)
{
    ConsumeSpace(Jit);
    char Ch = Advance(Jit);

    if (IsNumber(Ch) 
    || ( Peek(Jit, 0) == '.' && IsNumber(Peek(Jit, 1)) ))
    {
        return ParseNumber(Jit, Ch);
    }

    if (IsIdentifier(Ch))
    {
        return ParseIdentifier(Jit);
    }

    switch (Ch)
    {
    case '+': return CreateToken(Jit, TOK_PLUS);
    case '-': return CreateToken(Jit, TOK_MINUS);
    case '/': return CreateToken(Jit, TOK_SLASH);
    case '*': return CreateToken(Jit, TOK_STAR);
    case '^': return CreateToken(Jit, TOK_CARET);
    case '%': return CreateToken(Jit, TOK_PERCENT);
    case '(': return CreateToken(Jit, TOK_LPAREN);
    case ')': return CreateToken(Jit, TOK_RPAREN);
    case '[': return CreateToken(Jit, TOK_LBRACKET);
    case ']': return CreateToken(Jit, TOK_RBRACKET);
    case ',': return CreateToken(Jit, TOK_COMMA);
    case '=': return CreateToken(Jit, TOK_EQUAL);
    case '\n': return NewlineToken(Jit);
    case '\0': return CreateToken(Jit, TOK_EOF);
    default: return ErrorToken(Jit, "Invalid token.");
    }
}



static jit_token CurrToken(const jit *Jit)
{
    return Jit->Curr;
}

static jit_token NextToken(const jit *Jit)
{
    return Jit->Next;
}

static jit_token ConsumeToken(jit *Jit)
{
    if (TOK_ERR == Jit->Next.Type)
    {
        Error_AtToken(&Jit->Error, &Jit->Next, "%s", Jit->Next.As.ErrMsg);
    }
    Jit->Curr = Jit->Next;
    Jit->Next = Jit_Tokenize(Jit);
    return Jit->Curr;
}


static bool8 ConsumeIfNextTokenIs(jit *Jit, jit_token_type ExpectedToken)
{
    if (NextToken(Jit).Type == ExpectedToken)
    {
        ConsumeToken(Jit);
        return true;
    }
    return false;
}


static bool8 ConsumeOrError(jit *Jit, jit_token_type ExpectedType, const char *ErrMsg, ...)
{
    if (NextToken(Jit).Type != ExpectedType)
    {
        va_list Args;
        va_start(Args, ErrMsg);
        Error_AtTokenVA(&Jit->Error, &Jit->Next, ErrMsg, Args);
        va_end(Args);
        return false;
    }
    ConsumeToken(Jit);
    return true;
}




static jit_location Jit_AllocateReg(jit *Jit, jit_ir_stack *Stack)
{
    jit_location Location = {
        .Storage = STORAGE_REG,
        .As.Reg = Storage_TryAllocateReg(&Jit->Storage),
    };
    if (JIT_REG_INVALID == Location.As.Reg)
    {
        /* spill stack */
        Ir_Stack_SpillReg(Stack, Jit);
        Location.As.Reg = Storage_TryAllocateReg(&Jit->Storage);
        ASSERT(Location.As.Reg != JIT_REG_INVALID, "unreachable");
    }
    return Location;
}


static jit_reg Jit_ToReg(jit *Jit, jit_ir_stack *Stack, const jit_location *Location)
{
    jit_reg Result = -1;
    switch (Location->Storage)
    {
    case STORAGE_REG:
    {
        Result = Location->As.Reg;
    } break;
    case STORAGE_MEM:
    {
        Result = Jit_AllocateReg(Jit, Stack).As.Reg;
        Emit_Load(&Jit->Emitter, Result, Location->As.Mem.BaseReg, Location->As.Mem.Offset);
    } break;
    }
    return Result;
}

static jit_reg Jit_CopyToReg(jit *Jit, jit_reg Reg, const jit_location *Location)
{
    switch (Location->Storage)
    {
    case STORAGE_MEM:
    {
        Emit_Load(&Jit->Emitter, Reg, Location->As.Mem.BaseReg, Location->As.Mem.Offset);
    } break;
    case STORAGE_REG:
    {
        Emit_Move(&Jit->Emitter, Reg, Location->As.Reg);
    } break;
    }
    return Reg;
}





static int Ir_GetDataCount(const jit *Jit)
{
    return Jit->ScratchpadRightByteCount / sizeof(jit_ir_data);
}
static int Ir_GetOpCount(const jit *Jit)
{
    return Jit->ScratchpadLeftByteCount / sizeof(jit_ir_op);
}

static jit_ir_data *Jit_Ir_GetData(jit *Jit, int Index)
{
    ASSERT(Jit->IrData, "nullptr");
    jit_ir_data *Data = Jit->IrData - (intptr_t)Index - 1;
    return Data;
}

static jit_ir_op *Ir_PushOp(jit *Jit, jit_ir_op_type Type)
{
    jit_ir_op *Op = Jit_Scratchpad_PushLeft(Jit, sizeof(jit_ir_op));
    Op->Type = Type;
    return Op;
}

static jit_ir_data *Ir_PushData(jit *Jit, jit_ir_data_type Type)
{
    jit_ir_data *Data = Jit_Scratchpad_PushRight(Jit, sizeof(jit_ir_data));
    Data->Type = Type;
    return Data;
}

static void Ir_PopData(jit *Jit)
{
    Jit_Scratchpad_PopRight(Jit, sizeof(jit_ir_data));
}

static void Ir_PushOpLoad(jit *Jit, int Index)
{
    Ir_PushOp(Jit, IR_OP_LOAD)->LoadIndex = Index;
}

static int Ir_PushConst(jit *Jit, double Const)
{
    int Index = Ir_GetDataCount(Jit);
    jit_ir_data *Data = Ir_PushData(Jit, IR_DATA_CONST);
    Data->As.Const = Const;
    return Index;
}

static int Ir_PushVarRef(jit *Jit, const jit_token *VarName)
{
    int Count = Ir_GetDataCount(Jit);
    Ir_PushData(Jit, IR_DATA_VAR_REF)->As.VarRef = *VarName;
    return Count;
}

static int Ir_PushParam(jit *Jit, strview VarName, int ParamIndex)
{
    int Count = Ir_GetDataCount(Jit);
    jit_ir_data *Data = Ir_PushData(Jit, IR_DATA_PARAM);
    Data->As.Param.Name = VarName;
    Data->As.Param.Location = (jit_location) {
        .Storage = STORAGE_MEM,
        .As.Mem = TargetEnv_GetParam(ParamIndex, Jit->Storage.DataSize),
    };
    return Count;
}



static jit_location *Jit_FindVariable(jit *Jit, strview VarName, uint LocalScopeBase, uint LocalScopeVarCount)
{
    const char *Ptr = VarName.Ptr;
    int Len = VarName.Len;
    for (uint i = 0; i < LocalScopeVarCount; i++)
    {
        jit_ir_data *Local = Jit_Ir_GetData(Jit, LocalScopeBase + i);
        ASSERT(IR_DATA_PARAM == Local->Type, "unreachable");

        if (Len == Local->As.Param.Name.Len 
        && StrEqu(Ptr, Local->As.Param.Name.Ptr, Len))
        {
            return &Local->As.Param.Location;
        }
    }

    /* failed local scope search, switch to global */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, Ptr, Len, TYPE_VARIABLE);
    if (NULL == Entry)
    {
        return NULL;
    }
    return &Entry->As.Variable.Location;
}



/* returns data index in IrData, or -1 if data evaluation is defered */
static int Jit_ParseUnary(jit *Jit, bool8 AllowForwardReference)
{
    int DataIndex = -1;
    jit_token Token = ConsumeToken(Jit);
    switch (Token.Type)
    {
    case TOK_PLUS:  /* positive sign (nop) */
    {
        DataIndex = Jit_ParseUnary(Jit, AllowForwardReference);
    } break;
    case TOK_MINUS: /* negate */
    {
        DataIndex = Jit_ParseUnary(Jit, AllowForwardReference);
        /* negate on ir stack */
        if (-1 == DataIndex)
        {
            Ir_PushOp(Jit, IR_OP_NEG);
            break;
        }

        /* negate const */
        jit_ir_data *Data = Jit_Ir_GetData(Jit, DataIndex);
        if (IR_DATA_CONST == Data->Type)
        {
            Data->As.Const = -Data->As.Const;
        }
        /* value is not a const, load it onto the ir stack and negate it */
        else
        {
            Ir_PushOpLoad(Jit, DataIndex);
            Ir_PushOp(Jit, IR_OP_NEG);
            DataIndex = -1;
        }
    } break; 
    case TOK_NUMBER: /* number */
    {
        DataIndex = Ir_PushConst(Jit, Token.As.Number);
    } break;
    case TOK_LPAREN: /* (expr) */
    {
        DataIndex = Jit_Ir_CompileExpr(Jit, PREC_EXPR, AllowForwardReference);
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after expression.");
    } break;
    case TOK_IDENTIFIER:
    {
        /* function call */
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            int ArgCount = 0;
            Ir_PushOp(Jit, IR_OP_CALL_ARG_START);
            if (TOK_RPAREN != NextToken(Jit).Type)
            {
                /* compile the arguments and load them on the ir stack */
                do {
                    int Arg = Jit_Ir_CompileExpr(Jit, PREC_EXPR, AllowForwardReference);
                    if (Arg > -1)
                    {
                        Ir_PushOpLoad(Jit, Arg);
                    }
                    Storage_PushStack(&Jit->Storage, 1);
                    ArgCount++;
                } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
            }
            ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument list.");
            Storage_PopStack(&Jit->Storage, ArgCount);

            /* call Function with ArgCount arguments on stack */
            jit_ir_op *Op = Ir_PushOp(Jit, IR_OP_CALL);
            Op->Call.ArgCount = ArgCount;
            Op->Call.FnName = Token;

            /* no data index, result on ir stack */
            DataIndex = -1;
        }
        /* variable reference */
        else
        {
            if (!AllowForwardReference)
            {
                jit_location *Var = Jit_FindVariable(Jit, Token.Str, 0, 0);
                if (!Var)
                {
                    Error_AtToken(&Jit->Error, &Token, "Variable was not defined before use.");
                    break;
                }
            }
            DataIndex = Ir_PushVarRef(Jit, &Token);
        }
    } break;
    default:
    {
        Error_AtToken(&Jit->Error, &Token, "Expected an expression.");
    } break;
    }
    return DataIndex;
}

static precedence PrecedenceOf(jit_token_type Operator)
{
    switch (Operator)
    {
    case TOK_PLUS:
    case TOK_MINUS:
        return PREC_PLUSMINUS;
    case TOK_STAR:
    case TOK_SLASH:
        return PREC_MULDIV;
    case TOK_CARET:
        return PREC_POW;
    default: return PREC_NONE;
    }
}


static int Jit_Ir_CompileExpr(jit *Jit, precedence Prec, bool8 AllowForwardReference)
{
    int Left = Jit_ParseUnary(Jit, AllowForwardReference);
    while (PrecedenceOf(NextToken(Jit).Type) >= Prec)
    {
        jit_token_type Oper = ConsumeToken(Jit).Type;
        int Right = Jit_Ir_CompileExpr(Jit, PrecedenceOf(Oper) + 1, AllowForwardReference);

        /* const expr, evaluate data now */
        if (Left != -1 && Right != -1 
        && IR_DATA_CONST == Jit_Ir_GetData(Jit, Left)->Type 
        && IR_DATA_CONST == Jit_Ir_GetData(Jit, Right)->Type)
        {
#define CONST(index) Jit_Ir_GetData(Jit, index)->As.Const
            ASSERT(Left == Ir_GetDataCount(Jit) - 2, "");
            ASSERT(Right == Ir_GetDataCount(Jit) - 1, "");
            switch (Oper)
            {
            case TOK_PLUS:  CONST(Left) += CONST(Right); break;
            case TOK_MINUS: CONST(Left) -= CONST(Right); break;
            case TOK_STAR:  CONST(Left) *= CONST(Right); break;
            case TOK_SLASH: CONST(Left) /= CONST(Right); break;
            default:
            {
                UNREACHABLE();
            } break;
            }
            Ir_PopData(Jit);
            Left = Ir_GetDataCount(Jit) - 1;
#undef CONST
        }
        /* data evaluation is deferred to run time (ir stack) */
        else 
        {
            if (Left != -1)
                Ir_PushOpLoad(Jit, Left);
            if (Right != -1)
                Ir_PushOpLoad(Jit, Right);
            /* emit swap if Right operand was in the data arary first */
            if (Left != -1 && -1 == Right)
                Ir_PushOp(Jit, IR_OP_SWAP);

            switch (Oper)
            {
            case TOK_PLUS:  Ir_PushOp(Jit, IR_OP_ADD); break;
            case TOK_MINUS: Ir_PushOp(Jit, IR_OP_SUB); break;
            case TOK_STAR:  Ir_PushOp(Jit, IR_OP_MUL); break;
            case TOK_SLASH: Ir_PushOp(Jit, IR_OP_DIV); break;
            default:
            {
                UNREACHABLE();
            } break;
            }
            Left = -1;
        }
    }
    return Left;
}

static jit_location Jit_IrDataAsLocation(jit *Jit, const jit_ir_data *Data, int LocalScopeBase, int LocalScopeVarCount)
{
    jit_location Result = { 0 };
    switch (Data->Type)
    {
    case IR_DATA_CONST:
    {
        Result.Storage = STORAGE_MEM;
        Result.As.Mem = Storage_AllocateConst(&Jit->Storage, Data->As.Const);
    } break;
    case IR_DATA_VAR_REF:
    {
        jit_location *Entry = Jit_FindVariable(Jit, Data->As.VarRef.Str, LocalScopeBase, LocalScopeVarCount);
        if (!Entry)
        {
            Error_AtToken(&Jit->Error, &Data->As.VarRef, "Undefined variable.");
        }
        else
        {
            Result = *Entry;
        }
    } break;
    case IR_DATA_PARAM:
    {
        Result = Data->As.Param.Location;
    } break;
    }
    return Result;
}








static void Jit_EmitCallArgs(jit_ir_stack *Stack, int ArgCount)
{
    jit *Jit = Stack->Jit;
    int IrStackCount = Stack->Count;
    ASSERT(ArgCount <= IrStackCount, "arg count");

    /* emit arguments */
    for (int i = 0; i < ArgCount; i++)
    {
        jit_location *Location = Ir_Stack_Top(Stack, ArgCount - i - 1);
        jit_location Arg = TargetEnv_GetArg(i, Jit->Storage.DataSize);
        switch (Arg.Storage)
        {
        case STORAGE_REG:
        {
            Jit_CopyToReg(Jit, Arg.As.Reg, Location);
            Storage_ForceAllocateReg(&Jit->Storage, Arg.As.Reg);
        } break;
        case STORAGE_MEM:
        {
            jit_reg Tmp = Jit_ToReg(Jit, Stack, Location);
            Emit_Store(&Jit->Emitter, Tmp, Arg.As.Mem.BaseReg, Arg.As.Mem.Offset);
            Storage_DeallocateReg(&Jit->Storage, Tmp);
        } break;
        }
    }
    /* done emitting actual instructions, pop ir stack */
    Ir_Stack_PopMultiple(Stack, ArgCount);

    /* deallocate argument registers */
    for (int i = 0; i < ArgCount && TargetEnv_IsArgumentInReg(i); i++)
    {
        jit_reg Reg = TargetEnv_GetArg(i, Jit->Storage.DataSize).As.Reg;
        Storage_DeallocateReg(&Jit->Storage, Reg);
    }
}


static jit_function *Jit_FindFunction(jit *Jit, const jit_token *FnName)
{
    def_table_entry *Entry = DefTable_Find(
        &Jit->Global, FnName->Str.Ptr, FnName->Str.Len, TYPE_FUNCTION
    );
    if (!Entry) /* undefined function */
    {
        return NULL;
    }
    return &Entry->As.Function;
}

static void Jit_TranslateIr(jit *Jit)
{
    jit_ir_stack Stack_ = Ir_Stack_Init(Jit);
    jit_ir_stack *Stack = &Stack_;
    jit_fnref_stack FnRef_ = Ir_FnRef_Init(Jit);
    jit_fnref_stack *FnRef = &FnRef_;

    int IrOpCount = Ir_GetOpCount(Jit);
    jit_function *Fn = NULL;
    for (int i = 0; i < IrOpCount; i++)
    {
        jit_ir_op *Op = Jit->IrOp + i;
        switch (Op->Type)
        {
        case IR_OP_SWAP:
        {
            ASSERT(Stack->Count >= 2, "size");
            /* swap */
            jit_location *Left = Ir_Stack_Top(Stack, 0);
            jit_location *Right = Ir_Stack_Top(Stack, 1);
            SWAP(jit_location, *Left, *Right);
        } break;
        case IR_OP_LOAD:
        {
            jit_ir_data *IrData = Jit_Ir_GetData(Jit, Op->LoadIndex);
            int ParamStart = 0;
            int ParamCount = 0;
            if (Fn)
            {
                ParamStart = Fn->ParamStart;
                ParamCount = Fn->ParamCount;
            }
            jit_location Location = Jit_IrDataAsLocation(Jit, IrData, ParamStart, ParamCount);
            Ir_Stack_Push(Stack, &Location);
        } break;
        case IR_OP_STORE: /* store expr on stack to dst */
        {
            jit_reg Src = Jit_ToReg(Jit, Stack, Ir_Stack_Pop(Stack));
            jit_mem Dst = Op->StoreDst;
            Emit_Store(&Jit->Emitter, Src, Dst.BaseReg, Dst.Offset);
            Storage_DeallocateReg(&Jit->Storage, Src);
        } break;
        case IR_OP_FN_BEGIN:
        {
            Fn = Op->FnBegin;
            Fn->Location = Emit_FunctionEntry(&Jit->Emitter);
        } break;
        case IR_OP_ENTRY:
        {
            ASSERT(Ir_GetDataCount(Jit) >= Fn->ParamStart + Fn->ParamCount, "unreachable");

            /* set parameters to valid location */
            for (int i = 0; i < Fn->ParamCount && TargetEnv_IsArgumentInReg(i); i++)
            {
                int Index = Fn->ParamStart + i;
                jit_ir_data *Data = Jit_Ir_GetData(Jit, Index);
                ASSERT(Data->Type == IR_DATA_PARAM, "unreachable");
                jit_mem NonVolatileParam = TargetEnv_GetParam(i, Jit->Storage.DataSize);

                jit_reg VolatileParam = TargetEnv_GetArg(i, Jit->Storage.DataSize).As.Reg;
                Emit_Store(&Jit->Emitter, 
                    VolatileParam,
                    NonVolatileParam.BaseReg,
                    NonVolatileParam.Offset
                );
            }
        } break;
        case IR_OP_RETURN:
        {
            /* pop the top of the stack and emit return ins */
            jit_location *ReturnValue = Ir_Stack_Pop(Stack);
            Jit_CopyToReg(Jit, TargetEnv_GetReturnReg(), ReturnValue);
            Emit_FunctionExit(&Jit->Emitter, Fn->Location, Storage_GetMaxStackSize(&Jit->Storage));
            Fn->InsByteCount = Jit_GetEmitterBufferSize(Jit) - Fn->Location;

            /* deallocate return register and other param regs */
            if (STORAGE_REG == ReturnValue->Storage)
                Storage_DeallocateReg(&Jit->Storage, ReturnValue->As.Reg);

            Fn = NULL;
        } break;
        case IR_OP_CALL_ARG_START:
        {
            /* make all previous expr on the stack a memory location */
            Ir_Stack_SpillReg(Stack, Jit);
        } break;
        case IR_OP_CALL:
        {
            jit_function *Function = Jit_FindFunction(Jit, &Op->Call.FnName);
            if (!Function)
            {
                Error_AtToken(&Jit->Error, &Op->Call.FnName, "Undefined function.");
                break;
            }

            /* check param and arg count */
            if (Function->ParamCount != Op->Call.ArgCount)
            {
                const char *Plural = Function->ParamCount > 1? 
                    "arguments" : "argument";
                Error_AtToken(&Jit->Error, &Op->Call.FnName, "Expected %d %s, got %d instead.", 
                    Function->ParamCount, Plural, Op->Call.ArgCount
                );
                break;
            }
            /* reserve space on the stack for the call */
            Storage_PushStack(&Jit->Storage, Op->Call.ArgCount);

            /* emit the args and call itself */
            Jit_EmitCallArgs(Stack, Function->ParamCount);
            uint CallLocation = Emit_Call(&Jit->Emitter, Function->Location);
            if (FN_LOCATION_UNDEFINED == Function->Location)
            {
                Ir_FnRef_Push(FnRef, Function, CallLocation);
            }
            /* TODO: this is callee cleanup, support caller cleanup in the future, 
             * or move ir translation step to the emitter */
            Storage_PopStack(&Jit->Storage, Op->Call.ArgCount);

            /* return */
            Storage_ForceAllocateReg(&Jit->Storage, TargetEnv_GetReturnReg());
            jit_location Result = {
                .Storage = STORAGE_REG, 
                .As.Reg = TargetEnv_GetReturnReg(),
            };

            /* push return value on the stack */
            Ir_Stack_Push(Stack, &Result);
        } break;
        case IR_OP_ADD:
        case IR_OP_SUB:
        case IR_OP_MUL:
        case IR_OP_DIV:
        {
            jit_location *Right = Ir_Stack_Pop(Stack);
            jit_location *Left = Ir_Stack_Pop(Stack);

            /* if commutative and right was in reg first, swap operands */
            if ((IR_OP_ADD == Op->Type || IR_OP_MUL == Op->Type)
            && STORAGE_REG == Right->Storage)
            {
                SWAP(jit_location *, Left, Right);
            }

            jit_location Result = {
                .Storage = STORAGE_REG,
                .As.Reg = Jit_ToReg(Jit, Stack, Left),
            };
            if (Right->Storage == STORAGE_REG)
            {
                Storage_DeallocateReg(&Jit->Storage, Right->As.Reg);
            }

#define OP(op_name, left_reg, right_expr) do {\
    if (STORAGE_MEM == (right_expr)->Storage)\
        Emit_ ## op_name (&Jit->Emitter, left_reg, (right_expr)->As.Mem.BaseReg, (right_expr)->As.Mem.Offset);\
    else Emit_ ## op_name ## Reg(&Jit->Emitter, left_reg, (right_expr)->As.Reg);\
} while (0)
            switch (Jit->IrOp[i].Type)
            {
            case IR_OP_ADD: OP(Add, Result.As.Reg, Right); break;
            case IR_OP_SUB: OP(Sub, Result.As.Reg, Right); break;
            case IR_OP_MUL: OP(Mul, Result.As.Reg, Right); break;
            case IR_OP_DIV: OP(Div, Result.As.Reg, Right); break;
            default: 
            {
                UNREACHABLE();
            } break;
            }

            Ir_Stack_Push(Stack, &Result);
        } break;
        case IR_OP_NEG:
        {
            /* stack.top = 0 - stack.top */
            jit_location *Value = Ir_Stack_Pop(Stack);
            jit_location Result = Jit_AllocateReg(Jit, Stack);
            Emit_LoadZero(&Jit->Emitter, Result.As.Reg);
            OP(Sub, Result.As.Reg, Value);
            Ir_Stack_Push(Stack, &Result);

            if (Value->Storage == STORAGE_REG)
            {
                Storage_DeallocateReg(&Jit->Storage, Value->As.Reg);
            }
        } break;
        }
#undef OP
    }

    /* patch function calls */
    int FunctionCallCount = Ir_FnRef_Count(FnRef);
    for (int i = 0; i < FunctionCallCount; i++)
    {
        jit_fnref *Ref = Ir_FnRef_Pop(FnRef);
        ASSERT(Ref->Function, "nullptr");
        Emitter_PatchCall(&Jit->Emitter, Ref->Location, Ref->Function->Location);
    }
}







static jit_function *DefineFunction(jit *Jit, const char *Name, int NameLen)
{
    def_table_entry *Label = DefTable_Define(&Jit->Global, Name, NameLen, TYPE_FUNCTION);
    jit_function *Function = &Label->As.Function;
    jit_ir_op *Op = Ir_PushOp(Jit, IR_OP_FN_BEGIN);
    Op->FnBegin = Function;
    Function->Location = FN_LOCATION_UNDEFINED;
    return Function;
}

static void Jit_Function_Decl(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */
    jit_function *Function = DefineFunction(Jit, FnName->Str.Ptr, FnName->Str.Len);
    Storage_PushScope(&Jit->Storage);

    /* function params */
    Function->ParamCount = 0;
    Function->ParamStart = Ir_GetDataCount(Jit);
    if (TOK_RPAREN != NextToken(Jit).Type)
    {
        do {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
            jit_token Parameter = CurrToken(Jit); 
            Ir_PushParam(Jit, Parameter.Str, Function->ParamCount);
            Function->ParamCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after function declaration.");

    /* function body */
    jit_ir_op *Op = Ir_PushOp(Jit, IR_OP_ENTRY);
    int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR, true);
    if (Index != -1)
    {
        Ir_PushOpLoad(Jit, Index);
    }
    Ir_PushOp(Jit, IR_OP_RETURN);

    /* function end */
    Op->Entry.StackSize = Storage_GetMaxStackSize(&Jit->Storage);
    Storage_PopScope(&Jit->Storage);
}

static void Jit_Variable_Decl(jit *Jit, const jit_token *VarName)
{
    /* consumed VarName */
    def_table_entry *Entry = DefTable_Define(&Jit->Global, VarName->Str.Ptr, VarName->Str.Len, TYPE_VARIABLE);
    jit_variable *Variable = &Entry->As.Variable;

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after variable name.");
    
    /* expression */
    int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR, false);
    if (Index != -1)
    {
        Ir_PushOpLoad(Jit, Index);
    }

    jit_location Global = {
        .Storage = STORAGE_MEM,
        .As.Mem = Storage_AllocateGlobal(&Jit->Storage),
    };
    Ir_PushOp(Jit, IR_OP_STORE)->StoreDst = Global.As.Mem;
    Variable->Location = Global;
}


static void Jit_DisassembleCode(const u8 *Buffer, uint Start, uint End, uint BytesPerLine)
{
    char Instruction[64];
    for (u64 i = Start; i < End;)
    {
        uint InstructionBytes = 
            DisasmSingleInstruction(i, Buffer + i, End - i, Instruction);


        int Spaces = printf("%8x:  ", (u32)i);
        uint k;
        for (k = 0; k < InstructionBytes; k++)
        {
            printf("%02x ", Buffer[i + k]);
        }
        while (k < BytesPerLine)
        {
            printf("   ");
            k++;
        }

        printf("%s\n", Instruction);

        if (0xE9 == Buffer[i]) /* jmp instruction, jump to its dst and keep disassemble from there */
        {
            for (int k = 0; k < Spaces; k++)
                printf(" ");
            printf("...\n");
            i32 Offset = 
                (u32)Buffer[i + 1]
                | (u32)Buffer[i + 2] << 8
                | (u32)Buffer[i + 3] << 16
                | (u32)Buffer[i + 4] << 24;
            i += InstructionBytes + Offset;
        }
        else
        {
            i += InstructionBytes;
        }
    }
}

static void Jit_Disassemble(const jit *Jit)
{
    uint BytesPerLine = 10;
    printf("=================================\n");
    printf("         x64 disassembly:        \n");
    printf("=================================\n");
#if 1
    def_table_entry *i = Jit->Global.Head;
    const char *Type[] = { 
        [TYPE_VARIABLE] = "variable",
        [TYPE_FUNCTION] = "function"
    };
    while (i)
    {
        strview Str = i->As.Str;
        int Location = 0;
        int ByteCount = 0;
        switch (i->Type)
        {
        case TYPE_VARIABLE:
        {
            Location = i->As.Variable.InsLocation;
            ByteCount = i->As.Variable.InsByteCount;
        } break;
        case TYPE_FUNCTION:
        {
            Location = i->As.Function.Location;
            ByteCount = i->As.Function.InsByteCount;
        } break;
        }

        printf("\n<%08x>: (%s) %.*s\n", Location, Type[i->Type], Str.Len, Str.Ptr);
        Jit_DisassembleCode(Jit_GetEmitterBuffer(Jit), Location, Location + ByteCount, BytesPerLine);
        i = i->Next;
    }
#else
    Jit_DisassembleCode(Jit_GetEmitterBuffer(Jit), 0, Jit_GetEmitterBufferSize(Jit), BytesPerLine);
#endif

    printf("Consts: \n");
    for (uint i = 0; i < Jit->Storage.GlobalSize; i += Jit->Storage.DataSize)
    {
        printf("Global[%d] = %g\n", i/Jit->Storage.DataSize, Storage_GetConst(&Jit->Storage, i));
    }
}

static u32 Jit_Hash(const char *Str, int StrLen)
{
    if (0 == StrLen)
        return 0;
    return (u32)(Str[0] & 0x7F) << 8 | Str[StrLen - 1];
}


static void Jit_Reset(jit *Jit, const char *Location, jit_compilation_flags Flags)
{
    Jit->ScratchpadLeftByteCount = 0;
    Jit->ScratchpadRightByteCount = 0;
    Jit->Flags = Flags;
    Jit->Start = Location; 
    Jit->End = Location;
    Jit->Line = 1;
    Jit->Offset = 1;

    /* ir op array starts from the left and grows rightward in the scratchpad */
    Jit->IrOp = Jit_Scratchpad_LeftPtr(Jit);
    /* ir data array starts from the right and grows leftward in the scratchpad */
    Jit->IrData = Jit_Scratchpad_RightPtr(Jit);

    /* pump the tokenizer */
    Jit->Next = (jit_token) { .Type = TOK_EOF };
    ConsumeToken(Jit);

    /* reset other members */
    Error_Reset(&Jit->Error);
    DefTable_Reset(&Jit->Global);

    bool8 EmitFloat32Instructions = 0 != (Flags & JIT_COMPFLAG_FLOAT32);
    int FltSize = EmitFloat32Instructions? sizeof(float) : sizeof(double);
    Storage_Reset(&Jit->Storage, FltSize);
    Emitter_Reset(&Jit->Emitter, EmitFloat32Instructions);
}



uint Jit_Init(
    jit *Jit,
    void *Scratchpad, uint ScratchpadCapacity, 
    void *GlobalMemory, uint GlobalMemCapacity, 
    void *ProgramMemory, uint ProgramMemCapacity,
    def_table_entry *DefTableArray, uint DefTableCapacity
)
{
    uint MinScratchpadCapacity = 64*1024;
    if (ScratchpadCapacity < MinScratchpadCapacity
    || NULL == Jit)
    {
        return MinScratchpadCapacity;
    }

    *Jit = (jit) {
        .Storage = Storage_Init(GlobalMemory, GlobalMemCapacity),
        .Global = DefTable_Init(DefTableArray, DefTableCapacity, Jit_Hash),
        .ScratchpadPtr = Scratchpad,
        .ScratchpadCapacity = ScratchpadCapacity,
    };
    Emitter_Init(&Jit->Emitter, ProgramMemory, ProgramMemCapacity);
    return 0;
}

void Jit_Destroy(jit *Jit)
{
    *Jit = (jit) { 0 };
}


static void PrintVars(jit *Jit, int Start, int Count)
{
    ASSERT(Start + Count <= Ir_GetDataCount(Jit), "unreachable");
    for (int i = Start; i < Start + Count; i++)
    {
        jit_ir_data *Data = Jit_Ir_GetData(Jit, i);
        switch (Data->Type)
        {
        case IR_DATA_CONST:   printf("const  %d = %f\n", i, Data->As.Const); break;
        case IR_DATA_VAR_REF: printf("varref %d = '%.*s'\n", i, Data->As.VarRef.Str.Len, Data->As.VarRef.Str.Ptr); break;
        case IR_DATA_PARAM: printf("param %d = '%.*s'\n", i, Data->As.Param.Name.Len, Data->As.Param.Name.Ptr); break;
        default: UNREACHABLE(); break;
        }
    }
}


jit_result Jit_Compile(jit *Jit, jit_compilation_flags Flags, const char *Location)
{
    Jit_Reset(Jit, Location, Flags);

    jit_function *Init = DefineFunction(Jit, "init", 4);
    {
        do {
            /* definition */
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected a variable or function declaration.");
            jit_token Identifier = CurrToken(Jit);
            if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
            {
                Jit_Function_Decl(Jit, &Identifier);
            }
            else 
            {
                Jit_Variable_Decl(Jit, &Identifier);
            }

            /* skip all newlines */
            while (!Jit->Error.Available && ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
            {}
        } while (!Jit->Error.Available && TOK_EOF != NextToken(Jit).Type);
    }
    Init->InsByteCount = Jit_GetEmitterBufferSize(Jit);
    if (Jit->Error.Available)
        goto ErrReturn;

    printf("IR OK, free mem: %d\n", Jit_Scratchpad_BytesRemain(Jit));
    printf("============= var table ============\n");
    PrintVars(Jit, 0, Ir_GetDataCount(Jit));

    printf("============= instructions ============\n");
    for (int i = 0; i < Ir_GetOpCount(Jit); i++)
    {
        jit_ir_op *Op = Jit->IrOp + i;
        printf("%3d: %s\n", i, Get_OpName(Op->Type));
    }

    Jit_TranslateIr(Jit);
    Jit_Disassemble(Jit);
    if (Jit->Error.Available)
        goto ErrReturn;

    return (jit_result) {
        .GlobalData = Jit->Storage.GlobalMemory,
        .GlobalSymbol = Jit->Global.Head->Next,
    };

ErrReturn:
    return (jit_result) {
        .ErrMsg = Jit->Error.Msg,
    };
}


jit_init32 Jit_GetInit32(jit *Jit, const jit_result *Result)
{
    return (jit_init32)Jit_GetFunctionPtr(Jit, &Result->GlobalSymbol->As.Function);
}

jit_init64 Jit_GetInit64(jit *Jit, const jit_result *Result)
{
    return (jit_init64)Jit_GetFunctionPtr(Jit, &Result->GlobalSymbol->As.Function);
}

void *Jit_GetFunctionPtr(jit *Jit, const jit_function *Fn)
{
    ASSERT(Fn->Location < Jit_GetEmitterBufferSize(Jit), "Function with invalid location");
    ASSERT(Fn->Location + Fn->InsByteCount <= Jit_GetEmitterBufferSize(Jit), "Function with invalid size");
    u8 *FnPtr = Jit_GetEmitterBuffer(Jit) + Fn->Location;
    return FnPtr;
}

