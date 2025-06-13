#include "JitCommon.h"
#include "TargetEnv.h"
#include "Jit.h"
#include "DefTable.h"

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
static int Jit_Ir_CompileExpr(jit *Jit, precedence Prec, bool8 AllowForwardReference);


static void Jit_Scratchpad_Reset(jit_scratchpad *S, void *Buffer, int BufferCapacity)
{
    *S = (jit_scratchpad) {
        .Ptr = Buffer,
        .Capacity = BufferCapacity,
        .LeftCount = 0,
        .RightCount = 0,
    };
}

static int Jit_Scratchpad_BytesRemain(const jit_scratchpad *S)
{
    i64 Result = S->Capacity - (S->LeftCount + S->RightCount);
    ASSERT(IN_RANGE(0, Result, S->Capacity), "unreachable");
    return (int)Result;
}

static void *Jit_Scratchpad_LeftPtr(jit_scratchpad *S)
{
    return S->Ptr + S->LeftCount;
}

static void *Jit_Scratchpad_RightPtr(jit_scratchpad *S)
{
    return S->Ptr + S->Capacity - S->RightCount;
}

static void *Jit_Scratchpad_PushLeft(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= Jit_Scratchpad_BytesRemain(S), "bad data size");
    void *Ptr = Jit_Scratchpad_LeftPtr(S);
    S->LeftCount += DataSize;
    return Ptr;
}

static void *Jit_Scratchpad_PushRight(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= Jit_Scratchpad_BytesRemain(S), "bad data size");
    void *Ptr = (u8 *)Jit_Scratchpad_RightPtr(S) - DataSize;
    S->RightCount += DataSize;
    return Ptr;
}

static void Jit_Scratchpad_PopLeft(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->LeftCount, "bad data size");
    S->LeftCount -= DataSize;
}

static void Jit_Scratchpad_PopRight(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->RightCount, "bad data size");
    S->RightCount -= DataSize;
}



jit_ir_stack Ir_Stack_Init(jit *Jit)
{
    return (jit_ir_stack) {
        .S = &Jit->S,
    };
}

jit_location *Ir_Stack_Top(jit_ir_stack *Stack, int Offset)
{
    ASSERT(Offset >= 0, "bad offset");
    return (jit_location *)Jit_Scratchpad_LeftPtr(Stack->S) - 1 - Offset;
}

jit_location *Ir_Stack_Push(jit_ir_stack *Stack, const jit_location *Value)
{
    Stack->Count++;
    jit_location *Top = Jit_Scratchpad_PushLeft(Stack->S, sizeof(*Top));
    *Top = *Value;
    return Top;
}

jit_location *Ir_Stack_Pop(jit_ir_stack *Stack)
{
    ASSERT(Stack->Count != 0, "Cannot pop stack");
    Stack->Count--;
    jit_location *Top = Ir_Stack_Top(Stack, 0);
    Jit_Scratchpad_PopLeft(Stack->S, sizeof(jit_location));
    return Top;
}

void Ir_Stack_PopMultiple(jit_ir_stack *Stack, int Count)
{
    ASSERT(Stack->Count >= Count, "unreachable");
    Stack->Count -= Count;
    Jit_Scratchpad_PopLeft(Stack->S, Count*sizeof(jit_location));
}



jit_fnref_stack Ir_FnRef_Init(jit_scratchpad *S)
{
    return (jit_fnref_stack) {
        .S = S,
        .Base = S->RightCount,
    };
}

static jit_fnref *Ir_FnRef_Top(jit_fnref_stack *FnRef, int Offset)
{
    ASSERT(Offset >= 0, "bad offset");
    return (jit_fnref *)Jit_Scratchpad_RightPtr(FnRef->S) + Offset;
}

jit_fnref *Ir_FnRef_Push(jit_fnref_stack *FnRef, const jit_function *Function, uint CallLocation)
{
    jit_fnref *Top = Jit_Scratchpad_PushRight(FnRef->S, sizeof(*Top));
    Top->Function = Function;
    Top->Location = CallLocation;
    return Top;
}

jit_fnref *Ir_FnRef_Pop(jit_fnref_stack *FnRef)
{
    jit_fnref *Top = Ir_FnRef_Top(FnRef, 0);
    Jit_Scratchpad_PopRight(FnRef->S, sizeof(jit_fnref));
    return Top;
}

int Ir_FnRef_Count(const jit_fnref_stack *FnRef)
{
    ASSERT(FnRef->S->RightCount >= FnRef->Base, "unreachable");
    return (FnRef->S->RightCount - FnRef->Base) / sizeof(jit_fnref);
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

static bool8 ConsumeIfPeekIs(jit *Jit, int Offset, char Ch)
{
    if (Peek(Jit, Offset) == Ch)
    {
        Advance(Jit);
        return true;
    }
    return false;
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
    case '<': 
        if (ConsumeIfPeekIs(Jit, 0, '='))
            return CreateToken(Jit, TOK_LESS_EQUAL);
        else return CreateToken(Jit, TOK_LESS);
    case '>':
        if (ConsumeIfPeekIs(Jit, 0, '='))
            return CreateToken(Jit, TOK_GREATER_EQUAL);
        else return CreateToken(Jit, TOK_GREATER);
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





/*========================================================== 
 *                          IR OP
 *==========================================================*/


int Ir_Op_GetArgSize(jit_ir_op_type Op)
{
    switch (Op)
    {
    case IR_OP_LESS:            /* Instruction(1) */
    case IR_OP_GREATER:         /* Instruction(1) */
    case IR_OP_GREATER_EQUAL:   /* Instruction(1) */
    case IR_OP_LESS_EQUAL:      /* Instruction(1) */
    case IR_OP_ADD:             /* Instruction(1) */
    case IR_OP_SUB:             /* Instruction(1) */
    case IR_OP_MUL:             /* Instruction(1) */
    case IR_OP_DIV:             /* Instruction(1) */
    case IR_OP_NEG:             /* Instruction(1) */
    case IR_OP_CALL_ARG_START:  /* Instruction(1) */
    case IR_OP_SWAP:            /* Instruction(1) */
    {
        return 0;
    } break;
    case IR_OP_FN_END:          /* Instruction(1) + Offset(4) */
    case IR_OP_VAR_END:         /* Instruction(1) + Offset(4) */
    case IR_OP_STORE:           /* Instruction(1) + Offset(4) */
    case IR_OP_LOAD:            /* Instruction(1) + LoadIndex(4) */
    {
        return 4;
    } break;
    case IR_OP_VAR_BEGIN:       /* Instruction(1) + (jit_variable *)(sizeof(ptr))*/
    case IR_OP_FN_BEGIN:        /* Instruction(1) + (jit_function *)(sizeof(ptr)) */
    {
        return sizeof(void *);
    } break;
    case IR_OP_CALL:            /* Instruction(1) + ArgCount(4) + jit_token(sizeof(jit_token)) */
    {
        return 4 + sizeof(jit_token);
    } break;
    }
    UNREACHABLE();
    return 0;
}


static u8 *Ir_Op_PushAndReserveArgSize(jit *Jit, jit_ir_op_type Type, int ArgSize)
{
    Jit->IrOpByteCount += 1 + ArgSize;
    u8 *Ptr = Jit_Scratchpad_PushLeft(&Jit->S, 1 + ArgSize);
    *Ptr = Type;
    return Ptr + 1;
}
static void Ir_Op_Push(jit *Jit, jit_ir_op_type Type)
{
    Ir_Op_PushAndReserveArgSize(Jit, Type, 0);
}
static void Ir_Op_PushOp4(jit *Jit, jit_ir_op_type Type, i32 Arg)
{
    ASSERT(Type <= 0xFF, "unreachable");

    u8 *ArgPtr = Ir_Op_PushAndReserveArgSize(Jit, Type, sizeof(i32));
    MemCpy(ArgPtr, &Arg, sizeof(i32));
}
static void Ir_Op_PushCall(jit *Jit, i32 ArgCount, const jit_token *FnName)
{
    u8 *ArgPtr = Ir_Op_PushAndReserveArgSize(Jit, IR_OP_CALL, sizeof(i32) + sizeof(jit_token));
    MemCpy(ArgPtr, &ArgCount, sizeof(i32));
    MemCpy(ArgPtr + sizeof(i32), FnName, sizeof(jit_token));
}
static void Ir_Op_PushOpPtr(jit *Jit, jit_ir_op_type Op, const void *Ptr)
{
    u8 *ArgPtr = Ir_Op_PushAndReserveArgSize(Jit, Op, sizeof(void *));
    MemCpy(ArgPtr, &Ptr, sizeof(void *));
}
static void Ir_Op_PushLoad(jit *Jit, i32 Index)
{
    Ir_Op_PushOp4(Jit, IR_OP_LOAD, Index);
}

static void Ir_Op_PatchOp4(jit *Jit, int Location, i32 Arg)
{
    ASSERT(Location < Jit->IrOpByteCount, "invalid location");
    ASSERT(Location >= 0, "invalid location");
    MemCpy(Jit->IrOp + Location, &Arg, sizeof(Arg));
}







int Ir_Data_GetSize(const jit_ir_data *Data)
{
    return Data->ByteCount;
}

u8 *Ir_Data_Get(jit_ir_data *Data, i32 Offset)
{
    ASSERT(Data->ByteCount > Offset, "Invalid Offset");
    u8 *DataPtr = Data->Ptr - Offset - 1;
    return DataPtr;
}


/* returns pointer to payload space */
static u8 *Ir_Data_PushReserveSize(jit *Jit, jit_ir_data_type Type)
{
    Jit->IrData.ByteCount += Type + 1;
    u8 *Ptr = Jit_Scratchpad_PushRight(&Jit->S, Type + 1);
    *(Ptr + Type) = Type;
    return Ptr;
}

static void Ir_PopData(jit *Jit)
{
    u8 *Ptr = Jit_Scratchpad_RightPtr(&Jit->S);
    jit_ir_data_type DataType = *Ptr;
    int DataSize = Ir_Data_GetPayloadSize(DataType);
    Jit_Scratchpad_PopRight(&Jit->S, 1 + DataSize);
}

static int Ir_Data_PushConst(jit *Jit, double Const)
{
    int Index = Ir_Data_GetSize(&Jit->IrData);
    u8 *Ptr = Ir_Data_PushReserveSize(Jit, IR_DATA_CONST);
    MemCpy(Ptr, &Const, sizeof(Const));
    return Index;
}

static int Ir_Data_PushRef(jit *Jit, const jit_token *VarName)
{
    int Count = Ir_Data_GetSize(&Jit->IrData);
    u8 *Ptr = Ir_Data_PushReserveSize(Jit, IR_DATA_VARREF);

    jit_token_skinny Token = {
        .Str = VarName->Str.Ptr,
        .StrLen = VarName->Str.Len,
        .Line = VarName->Line,
        .Offset = VarName->Offset,
        .Type = VarName->Type,
    };
    MemCpy(Ptr, &Token, sizeof(Token));
    return Count;
}

static int Ir_Data_PushParam(jit *Jit, strview VarName, i32 ParamIndex)
{
    int Count = Ir_Data_GetSize(&Jit->IrData);
    u8 *Ptr = Ir_Data_PushReserveSize(Jit, IR_DATA_PARAM);
    jit_ir_data_param Param = {
        .Str = VarName.Ptr,
        .StrLen = VarName.Len,
        .Index = ParamIndex,
    };
    MemCpy(Ptr, &Param, sizeof Param);
    return Count;
}

static jit_location Ir_Data_GetParamLocation(const jit *Jit, const u8 *Data)
{
    jit_ir_data_param Param;
    Ir_Data_GetPayload(Data, &Param);
    return (jit_location) {
        .Storage = STORAGE_MEM,
        .As.Mem = TargetEnv_GetParam(Param.Index, Jit->Storage.DataSize),
    };
}

static bool8 Jit_FindVariable(
    jit *Jit, 
    const char *Str, int Len,
    int LocalScopeBase, int LocalScopeVarCount, 
    jit_location *VarLocation)
{
    /* search local scope if provided */
    i32 DataOffset = LocalScopeBase;
    for (int i = 0; i < LocalScopeVarCount; i++)
    {
        u8 *Data = Ir_Data_Get(&Jit->IrData, DataOffset);
        jit_ir_data_type Type = *Data;
        ASSERT(IR_DATA_PARAM == Type, "unreachable");
        jit_ir_data_param Param; 
        Ir_Data_GetPayload(Data, &Param);

        if (Len == Param.StrLen
        && StrEqu(Str, Param.Str, Len))
        {
            if (NULL != VarLocation)
            {
                *VarLocation = Ir_Data_GetParamLocation(Jit, Data);
            }
            return true;
        }

        DataOffset += 1 + Ir_Data_GetPayloadSize(Type);
    }

    /* failed local scope search, switch to global */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, Str, Len, TYPE_VARIABLE);
    if (NULL == Entry)
    {
        return false;
    }
    if (NULL != VarLocation)
    {
        *VarLocation = Entry->As.Variable.Location;
    }
    return true;
}

jit_location Ir_Data_GetLocation(jit *Jit, const u8 *Data, int LocalScopeBase, int LocalScopeVarCount)
{
    jit_location Result = { 0 };
    jit_ir_data_type Type = *Data;
    switch (Type)
    {
    case IR_DATA_CONST:
    {
        double Const;
        Ir_Data_GetPayload(Data, &Const);
        Result.Storage = STORAGE_MEM;
        Result.As.Mem = Storage_AllocateConst(&Jit->Storage, Const);
    } break;
    case IR_DATA_VARREF:
    {
        jit_token_skinny Name;
        Ir_Data_GetPayload(Data, &Name);
        if (!Jit_FindVariable(Jit, Name.Str, Name.StrLen, LocalScopeBase, LocalScopeVarCount, &Result))
        {
            Error_AtStr(&Jit->Error, Name.Str, Name.StrLen, Name.Line, Name.Offset, "Undefined variable.");
        }
        ASSERT(Result.Storage == STORAGE_MEM, "unreachable");
    } break;
    case IR_DATA_PARAM:
    {
        Result = Ir_Data_GetParamLocation(Jit, Data);
    } break;
    }
    return Result;
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
            Ir_Op_Push(Jit, IR_OP_NEG);
            break;
        }

        /* negate const */
        u8 *Data = Ir_Data_Get(&Jit->IrData, DataIndex);
        if (IR_DATA_CONST == *Data)
        {
            double Const;
            Ir_Data_GetPayload(Data, &Const);
            Const = -Const;
            Ir_Data_SetPayload(Data, &Const);
        }
        /* value is not a const, load it onto the ir stack and negate it */
        else
        {
            Ir_Op_PushLoad(Jit, DataIndex);
            Ir_Op_Push(Jit, IR_OP_NEG);
            DataIndex = -1;
        }
    } break; 
    case TOK_NUMBER: /* number */
    {
        DataIndex = Ir_Data_PushConst(Jit, Token.As.Number);
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
            Ir_Op_Push(Jit, IR_OP_CALL_ARG_START);
            if (TOK_RPAREN != NextToken(Jit).Type)
            {
                /* compile the arguments and load them on the ir stack */
                do {
                    int Arg = Jit_Ir_CompileExpr(Jit, PREC_EXPR, AllowForwardReference);
                    if (Arg > -1)
                    {
                        Ir_Op_PushLoad(Jit, Arg);
                    }
                    ArgCount++;
                } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
            }
            ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument list.");

            /* call Function with ArgCount arguments on stack */
            Ir_Op_PushCall(Jit, ArgCount, &Token);

            /* no data index, result on ir stack */
            DataIndex = -1;
        }
        /* variable reference */
        else
        {
            if (!AllowForwardReference
            && !Jit_FindVariable(Jit, Token.Str.Ptr, Token.Str.Len, 0, 0, NULL))
            {
                Error_AtToken(&Jit->Error, &Token, "Variable was not defined before use.");
                break;
            }
            DataIndex = Ir_Data_PushRef(Jit, &Token);
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
        && IR_DATA_CONST == Ir_Data_GetType(&Jit->IrData, Left) 
        && IR_DATA_CONST == Ir_Data_GetType(&Jit->IrData, Right))
        {
            ASSERT(Left == Ir_Data_GetSize(&Jit->IrData) - 2, "");
            ASSERT(Right == Ir_Data_GetSize(&Jit->IrData) - 1, "");

            double ConstLeft, ConstRight;
            u8 *LeftPtr = Ir_Data_Get(&Jit->IrData, Left);
            u8 *RightPtr = Ir_Data_Get(&Jit->IrData, Left);
            Ir_Data_GetPayload(LeftPtr, &ConstLeft);
            Ir_Data_GetPayload(RightPtr, &ConstRight);
            switch (Oper)
            {
            case TOK_PLUS:          ConstLeft += ConstRight; break;
            case TOK_MINUS:         ConstLeft -= ConstRight; break;
            case TOK_STAR:          ConstLeft *= ConstRight; break;
            case TOK_SLASH:         ConstLeft /= ConstRight; break;
            case TOK_LESS:          ConstLeft = ConstLeft < ConstRight; break;
            case TOK_LESS_EQUAL:    ConstLeft = ConstLeft <= ConstRight; break;
            case TOK_GREATER:       ConstLeft = ConstLeft > ConstRight; break;
            case TOK_GREATER_EQUAL: ConstLeft = ConstLeft >= ConstRight; break;
            default:
            {
                UNREACHABLE();
            } break;
            }
            Ir_Data_SetPayload(LeftPtr, &ConstLeft);
            Ir_PopData(Jit);
            Left = Ir_Data_GetSize(&Jit->IrData) - 1;
        }
        /* data evaluation is deferred to run time (ir stack) */
        else 
        {
            if (Left != -1)
                Ir_Op_PushLoad(Jit, Left);
            if (Right != -1)
                Ir_Op_PushLoad(Jit, Right);
            /* emit swap if Right operand was in the data arary first */
            if (Left != -1 && -1 == Right)
                Ir_Op_Push(Jit, IR_OP_SWAP);

            switch (Oper)
            {
            case TOK_PLUS:          Ir_Op_Push(Jit, IR_OP_ADD); break;
            case TOK_MINUS:         Ir_Op_Push(Jit, IR_OP_SUB); break;
            case TOK_STAR:          Ir_Op_Push(Jit, IR_OP_MUL); break;
            case TOK_SLASH:         Ir_Op_Push(Jit, IR_OP_DIV); break;
            case TOK_LESS:          Ir_Op_Push(Jit, IR_OP_LESS); break;
            case TOK_LESS_EQUAL:    Ir_Op_Push(Jit, IR_OP_LESS_EQUAL); break;
            case TOK_GREATER:       Ir_Op_Push(Jit, IR_OP_GREATER); break;
            case TOK_GREATER_EQUAL: Ir_Op_Push(Jit, IR_OP_GREATER_EQUAL); break;
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







jit_function *Jit_FindFunction(jit *Jit, const jit_token *FnName, int ArgCount)
{
    def_table_entry *Entry = DefTable_Find(
        &Jit->Global, FnName->Str.Ptr, FnName->Str.Len, TYPE_FUNCTION
    );

    if (!Entry)
    {
        Error_AtToken(&Jit->Error, FnName, "Undefined function.");
        return NULL;
    }

    /* check param and arg count */
    jit_function *Function = &Entry->As.Function;
    if (Entry->As.Function.ParamCount != ArgCount)
    {
        const char *Plural = Function->ParamCount > 1? 
            "arguments" : "argument";
        Error_AtToken(&Jit->Error, FnName, "Expected %d %s, got %d instead.", 
            Function->ParamCount, Plural, ArgCount
        );
        return NULL;
    }

    return Function;
}

jit_function *Jit_DefineFunction(jit *Jit, const char *Name, int NameLen)
{
    def_table_entry *Label = DefTable_Define(&Jit->Global, Name, NameLen, TYPE_FUNCTION);
    jit_function *Function = &Label->As.Function;

    Function->Location = FN_LOCATION_UNDEFINED;
    return Function;
}









static void Jit_Function_Decl(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */
    jit_function *Function = Jit_DefineFunction(Jit, FnName->Str.Ptr, FnName->Str.Len);

    /* function params */
    Function->ParamCount = 0;
    Function->ParamStart = Ir_Data_GetSize(&Jit->IrData);
    if (TOK_RPAREN != NextToken(Jit).Type)
    {
        do {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
            jit_token Parameter = CurrToken(Jit); 
            Ir_Data_PushParam(Jit, Parameter.Str, Function->ParamCount);
            Function->ParamCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after function declaration.");

    /* housekeeping: patching previous function to here */
    if (Jit->PrevFnEnd != FN_END_LAST)
    {
        Ir_Op_PatchOp4(Jit, Jit->PrevFnEnd - 4, Jit->IrOpByteCount - Jit->PrevFnEnd);
    }
    Ir_Op_PushOpPtr(Jit, IR_OP_FN_BEGIN, Function);

    /* function body */
    int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR, true);
    if (Index != -1)
    {
        Ir_Op_PushLoad(Jit, Index);
    }

    /* function end */
    Ir_Op_PushOp4(Jit, IR_OP_FN_END, FN_END_LAST);
    Jit->PrevFnEnd = Jit->IrOpByteCount;
}

static void Jit_Variable_Decl(jit *Jit, const jit_token *VarName)
{
    /* consumed VarName */
    def_table_entry *Entry = DefTable_Define(&Jit->Global, VarName->Str.Ptr, VarName->Str.Len, TYPE_VARIABLE);
    jit_variable *Variable = &Entry->As.Variable;

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after variable name.");
    
    /* houskeeping, patching up last declared variable */
    if (Jit->PrevVarEnd != VAR_END_LAST)
    {
        Ir_Op_PatchOp4(Jit, Jit->PrevVarEnd - 4, Jit->IrOpByteCount - Jit->PrevVarEnd);
    }
    Ir_Op_PushOpPtr(Jit, IR_OP_VAR_BEGIN, Variable);

    /* expression */
    int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR, false);
    if (Index != -1)
    {
        Ir_Op_PushLoad(Jit, Index);
    }
    /* store the result of the expression */
    jit_location Global = {
        .Storage = STORAGE_MEM,
        .As.Mem = Storage_AllocateGlobal(&Jit->Storage),
    };
    Ir_Op_PushOp4(Jit, IR_OP_STORE, Global.As.Mem.Offset);
    Variable->Location = Global;

    /* end variable decl */
    Ir_Op_PushOp4(Jit, IR_OP_VAR_END, VAR_END_LAST);
    Jit->PrevVarEnd = Jit->IrOpByteCount;
}


static void Jit_DisassembleCode(const u8 *Buffer, uint Start, uint End, uint BytesPerLine)
{
    char Instruction[64];
    for (u64 i = Start; i < End;)
    {
        uint InstructionBytes = DisasmSingleInstruction(
            i, 
            Buffer + i, 
            End - i, 
            Instruction
        );


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
        Jit_DisassembleCode(Emitter_GetBuffer(&Jit->Emitter), Location, Location + ByteCount, BytesPerLine);
        i = i->Next;
    }
#else
    Jit_DisassembleCode(Emitter_GetBuffer(&Jit->Emitter), 0, Emitter_GetBufferSize(&Jit->Emitter), BytesPerLine);
#endif

    printf("Consts: \n");
    for (uint i = 0; i < Jit->Storage.GlobalSize; i += Jit->Storage.DataSize)
    {
        printf("Global[%x] = %g\n", i, Storage_GetConst(&Jit->Storage, i));
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
    Jit->PrevVarEnd = VAR_END_LAST;
    Jit->PrevFnEnd = FN_END_LAST;
    Jit->Flags = Flags;
    Jit->Start = Location; 
    Jit->End = Location;
    Jit->Line = 1;
    Jit->Offset = 1;

    Jit_Scratchpad_Reset(&Jit->S, Jit->S.Ptr, Jit->S.Capacity);

    /* ir op array starts from the left and grows rightward in the scratchpad */
    Jit->IrOpByteCount = 0;
    Jit->IrOp = Jit_Scratchpad_LeftPtr(&Jit->S);
    /* ir data array starts from the right and grows leftward in the scratchpad */
    Jit->IrData.ByteCount = 0;
    Jit->IrData.Ptr = Jit_Scratchpad_RightPtr(&Jit->S);

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
    };
    Jit_Scratchpad_Reset(&Jit->S, Scratchpad, ScratchpadCapacity);
    Emitter_Init(&Jit->Emitter, ProgramMemory, ProgramMemCapacity);
    return 0;
}

void Jit_Destroy(jit *Jit)
{
    *Jit = (jit) { 0 };
}


static void PrintVars(jit *Jit)
{
    i32 Offset = 0;
    int i = 0;
    while (Offset < Ir_Data_GetSize(&Jit->IrData))
    {
        jit_ir_data_type Type = Ir_Data_GetType(&Jit->IrData, Offset);
        const u8 *Data = Ir_Data_Get(&Jit->IrData, Offset);
        switch (Type)
        {
        case IR_DATA_CONST:  
        {
            double Const;
            Ir_Data_GetPayload(Data, &Const);
            printf("Const %d = %f\n", i, Const);
        } break;
        case IR_DATA_VARREF: 
        {
            jit_token_skinny VarRef;
            Ir_Data_GetPayload(Data, &VarRef);
            printf("VarRef %d = '%.*s'\n", i, VarRef.StrLen, VarRef.Str);
        } break;
        case IR_DATA_PARAM:  
        {
            jit_ir_data_param Param;
            Ir_Data_GetPayload(Data, &Param);
            printf("Param %d = '%.*s', %d\n", i, Param.StrLen, Param.Str, Param.Index);
        } break;
        default: UNREACHABLE(); break;
        }
        i++;
        Offset += 1 + Ir_Data_GetPayloadSize(Type);
    }
}


jit_result Jit_Compile(jit *Jit, jit_compilation_flags Flags, const char *Location)
{
    Jit_Reset(Jit, Location, Flags);

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
    if (Jit->Error.Available)
        goto ErrReturn;

    printf("IR OK, free mem: %d\n", Jit_Scratchpad_BytesRemain(&Jit->S));
#if 1
    printf("============= var table ============\n");
    PrintVars(Jit);

#else
    printf("============= instructions ============\n");
    for (int i = 0; i < Ir_GetOpCount(Jit); i++)
    {
        jit_ir_op *Op = Jit->IrOp + i;
        printf("%3d: %s\n", i, Get_OpName(Op->Type));
    }
#endif

    Emitter_TranslateIr(Jit);
    Jit_Disassemble(Jit);
    if (Jit->Error.Available)
        goto ErrReturn;

    return (jit_result) {
        .GlobalData = Jit->Storage.GlobalMemory,
        .GlobalSymbol = Jit->Global.Head,
    };

ErrReturn:
    return (jit_result) {
        .ErrMsg = Jit->Error.Msg,
    };
}


jit_init32 Jit_GetInit32(jit *Jit, const jit_result *Result)
{
    def_table_entry *Init = Jit->Global.Tail;
    ASSERT(Init, "nullptr");
    ASSERT(Init->Type == TYPE_FUNCTION, "unreachable");
    ASSERT(StrEqu(Init->As.Str.Ptr, "init", 4), "unreachable");
    return (jit_init32)Jit_GetFunctionPtr(Jit, &Init->As.Function);
}

jit_init64 Jit_GetInit64(jit *Jit, const jit_result *Result)
{
    def_table_entry *Init = Jit->Global.Tail;
    ASSERT(Init, "nullptr");
    ASSERT(Init->Type == TYPE_FUNCTION, "unreachable");
    ASSERT(StrEqu(Init->As.Str.Ptr, "init", 4), "unreachable");
    return (jit_init64)Jit_GetFunctionPtr(Jit, &Init->As.Function);
}

void *Jit_GetFunctionPtr(jit *Jit, const jit_function *Fn)
{
    ASSERT(Fn->Location < Emitter_GetBufferSize(&Jit->Emitter), "Function with invalid location");
    ASSERT(Fn->Location + Fn->InsByteCount <= Emitter_GetBufferSize(&Jit->Emitter), "Function with invalid size");
    u8 *FnPtr = (u8 *)Emitter_GetBuffer(&Jit->Emitter) + Fn->Location;
    return FnPtr;
}

