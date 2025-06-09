#include "Include/JitCommon.h"
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
    IR_OP_CALL,
    IR_OP_SWAP,
    IR_OP_STORE,
    IR_OP_ENTRY, 
    IR_OP_RETURN,
} jit_ir_op_type;
typedef enum jit_ir_data_type 
{
    IR_DATA_CONST,
    IR_DATA_VAR_REF,
    IR_DATA_VAR_DEF,
} jit_ir_data_type;
struct jit_ir_data 
{
    jit_ir_data_type Type;
    union {
        double Const;
        strview VarRef;
        jit_variable VarDef;
    } As;
};
struct jit_ir_op
{
    union {
        int LoadIndex; 
        struct {
            int ArgCount;
            strview FnName;
        } Call;
        struct {
            jit_function *Function;
            i32 StackSize;
        } Entry;
        jit_mem StoreDst;
    };
    jit_ir_op_type Type;
};

#define SCRATCHPAD_COMMIT(p_jit, ptr, count) (ptr = Jit_Scratchpad_Commit(p_jit, count, sizeof((ptr)[0])))
#define PUSH_LEFT(p_jit, ptr, count) (ptr = Jit_Scratchpad_PushLeft(p_jit, (count) * sizeof((ptr)[0])))
#define PUSH_RIGHT(p_jit, ptr, count) (ptr = Jit_Scratchpad_PushRight(p_jit, (count) * sizeof((ptr)[0])))



static int Jit_Ir_CompileExpr(jit *Jit, precedence Prec);

static const char *Get_OpName(jit_ir_op_type Op)
{
    const char *Name[] = {
        [IR_OP_ADD] = "IR_OP_ADD",
        [IR_OP_SUB] = "IR_OP_SUB",
        [IR_OP_MUL] = "IR_OP_MUL",
        [IR_OP_DIV] = "IR_OP_DIV",
        [IR_OP_NEG] = "IR_OP_NEG",
        [IR_OP_LOAD] = "IR_OP_LOAD",
        [IR_OP_CALL] = "IR_OP_CALL",
        [IR_OP_SWAP] = "IR_OP_SWAP",
        [IR_OP_STORE] = "IR_OP_STORE",
        [IR_OP_ENTRY] = "IR_OP_ENTRY", 
        [IR_OP_RETURN] = "IR_OP_RETURN",
    };
    return Name[Op];
}



static jit_ir_data *Jit_Ir_GetData(jit *Jit, int Index)
{
    ASSERT(Index < Jit->IrDataCount, "bad index");
    ASSERT(Jit->IrData, "nullptr");
    jit_ir_data *Data = Jit->IrData - (intptr_t)Index - 1;
    return Data;
}

static u8 *Jit_GetEmitterBuffer(jit *Jit)
{
    return Jit->Emitter.Base.Buffer;
}

static u8 Jit_GetEmitterBufferSize(const jit *Jit)
{
    return Jit->Emitter.Base.BufferSize;
}

static int Jit_Scratchpad_BytesRemain(const jit *Jit)
{
    i64 Result = (i64)Jit->ScratchpadCapacity - (i64)Jit->ScratchpadLeftByteCount + (i64)Jit->ScratchpadRightByteCount;
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

static jit_token ErrorToken(jit *Jit, const char *Fmt, ...)
{
    jit_token Tok = CreateToken(Jit, TOK_ERR);
    va_list Arg;
    va_start(Arg, Fmt);
    vsnprintf(Tok.As.ErrMsg, sizeof Tok.As.ErrMsg, Fmt, Arg);
    va_end(Arg);
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
        case '#': 
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
    default: return ErrorToken(Jit, "Unknown jit_token '%c'.", Ch);
    }
}



static jit_token ConsumeToken(jit *Jit)
{
    Jit->Curr = Jit->Next;
    Jit->Next = Jit_Tokenize(Jit);
    return Jit->Curr;
}

static jit_token CurrToken(const jit *Jit)
{
    return Jit->Curr;
}

static jit_token NextToken(const jit *Jit)
{
    return Jit->Next;
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





static jit_reg Jit_ToReg(jit *Jit, jit_expression Expr)
{
    switch (Expr.Storage)
    {
    case STORAGE_REG: return Expr.As.Reg;
    case STORAGE_MEM:
    {
        jit_reg Reg = Storage_AllocateReg(&Jit->Storage).As.Reg;
        Emit_Load(&Jit->Emitter, Reg, Expr.As.Mem.BaseReg, Expr.As.Mem.Offset);
        return Reg;
    } break;
    }
    UNREACHABLE();
}

static jit_reg Jit_CopyToReg(jit *Jit, jit_reg Reg, const jit_expression *Expr)
{
    switch (Expr->Storage)
    {
    case STORAGE_MEM:
    {
        Emit_Load(&Jit->Emitter, Reg, Expr->As.Mem.BaseReg, Expr->As.Mem.Offset);
    } break;
    case STORAGE_REG:
    {
        Emit_Move(&Jit->Emitter, Reg, Expr->As.Reg);
    } break;
    }
    return Reg;
}


static jit_variable *Jit_FindVariable(jit *Jit, strview VarName, uint LocalScopeBase, uint LocalScopeVarCount)
{
    const char *Ptr = VarName.Ptr;
    int Len = VarName.Len;
    for (uint i = 0; i < LocalScopeVarCount; i++)
    {
        jit_ir_data *Local = Jit_Ir_GetData(Jit, LocalScopeBase - i);
        ASSERT(IR_DATA_VAR_DEF == Local->Type, "unreachable");
        jit_variable *Variable = &Local->As.VarDef;

        if (Len == Variable->Dbg.Str.Len 
        && StrEqu(Ptr, Variable->Dbg.Str.Ptr, Len))
        {
            return Variable;
        }
    }
    /* failed local scope search, switch to global */

    /* global scope */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, Ptr, Len, TYPE_VARIABLE);
    if (NULL == Entry)
    {
        return NULL;
    }
    return &Entry->As.Variable;
}



static jit_ir_op *Ir_PushOp(jit *Jit, jit_ir_op_type Type)
{
    Jit->IrOpCount++;
    jit_ir_op *Op = Jit_Scratchpad_PushLeft(Jit, sizeof(jit_ir_op));
    Op->Type = Type;
    return Op;
}

static jit_ir_data *Ir_PushData(jit *Jit, jit_ir_data_type Type)
{
    Jit->IrDataCount++;
    jit_ir_data *Data = Jit_Scratchpad_PushRight(Jit, sizeof(jit_ir_data));
    Data->Type = Type;
    return Data;
}
static void Ir_PushOpLoad(jit *Jit, int Index)
{
    Ir_PushOp(Jit, IR_OP_LOAD)->LoadIndex = Index;
}

static uint Ir_PushConst(jit *Jit, double Const)
{
    uint Index = Jit->IrDataCount;
    jit_ir_data *Data = Ir_PushData(Jit, IR_DATA_CONST);
    Data->As.Const = Const;
    return Index;
}

static uint Ir_PushVarRef(jit *Jit, strview VarName)
{
    uint Index = Jit->IrDataCount;
    Ir_PushData(Jit, IR_DATA_VAR_REF)->As.VarRef = VarName;
    return Index;
}

static uint Ir_PushVarDef(jit *Jit, strview VarName)
{
    uint Index = Jit->IrDataCount;
    Ir_PushData(Jit, IR_DATA_VAR_DEF)->As.VarDef.Dbg.Str = VarName;
    return Index;
}


/* returns data index in IrData, or -1 if data evaluation is defered */
static int Jit_ParseUnary(jit *Jit)
{
    int DataIndex = -1;
    jit_token Token = ConsumeToken(Jit);
    switch (Token.Type)
    {
    case TOK_PLUS:  /* positive sign (nop) */
    {
        DataIndex = Jit_ParseUnary(Jit);
    } break;
    case TOK_MINUS: /* negate */
    {
        DataIndex = Jit_ParseUnary(Jit);
        /* negate on eval stack */
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
        /* negate on data stack */
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
        DataIndex = Jit_Ir_CompileExpr(Jit, PREC_EXPR);
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after expression.");
    } break;
    case TOK_IDENTIFIER:
    {
        /* function call */
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            int ArgCount = 0;
            if (TOK_RPAREN != NextToken(Jit).Type)
            {
                /* compile the arguments and load them on the ir stack */
                do {
                    int Arg = Jit_Ir_CompileExpr(Jit, PREC_EXPR);
                    if (Arg > -1)
                    {
                        Ir_PushOpLoad(Jit, Arg);
                    }
                    ArgCount++;
                } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
            }
            ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument list.");

            /* call Function with ArgCount arguments on stack */
            jit_ir_op *Op = Ir_PushOp(Jit, IR_OP_CALL);
            Op->Call.ArgCount = ArgCount;
            Op->Call.FnName = Token.Str;

            /* no data index, result on ir stack */
        }
        /* variable reference */
        else
        {
            DataIndex = Ir_PushVarRef(Jit, Token.Str);
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


static int Jit_Ir_CompileExpr(jit *Jit, precedence Prec)
{
    int Left = Jit_ParseUnary(Jit);
    while (PrecedenceOf(NextToken(Jit).Type) >= Prec)
    {
        jit_token_type Oper = ConsumeToken(Jit).Type;
        int Right = Jit_Ir_CompileExpr(Jit, PrecedenceOf(Oper) + 1);

        /* const expr, evaluate data now */
        if (Left != -1 && Right != -1 
        && IR_DATA_CONST == Jit_Ir_GetData(Jit, Left)->Type 
        && IR_DATA_CONST == Jit_Ir_GetData(Jit, Right)->Type)
        {
#define CONST(index) Jit_Ir_GetData(Jit, index)->As.Const
            ASSERT(Left == Jit->IrDataCount - 2, "");
            ASSERT(Right == Jit->IrDataCount - 1, "");
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
            Jit->IrDataCount--;
            Left = Jit->IrDataCount - 1;
#undef CONST
        }
        /* data evaluation is deferred to run time */
        else 
        {
            if (Left != -1)
                Ir_PushOpLoad(Jit, Left);
            if (Right != -1)
                Ir_PushOpLoad(Jit, Right);
            /* Right operand was on stack first */
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

static jit_expression Jit_IrDataAsExpr(jit *Jit, const jit_ir_data *Data, int LocalScopeBase, int LocalScopeVarCount)
{
    switch (Data->Type)
    {
    case IR_DATA_CONST:
    {
        return Storage_AllocateConst(&Jit->Storage, Data->As.Const);
    } break;
    case IR_DATA_VAR_REF:
    {
        jit_variable *Entry = Jit_FindVariable(Jit, Data->As.VarRef, LocalScopeBase, LocalScopeVarCount);
        ASSERT(Entry, "TODO: undefined variable");
        return Entry->Expr;
    } break;
    case IR_DATA_VAR_DEF:
    {
        return Data->As.VarDef.Expr;
    } break;
    }
    UNREACHABLE();
}


static uint Jit_EmitOpCall(jit *Jit, 
    strview FnName, int ArgCount, 
    jit_expression *IrStack, int IrStackCount, int IrStackCapacity)
{
    ASSERT(ArgCount <= (int)IrStackCount, "arg count");

    /* find the function */
    const def_table_entry *Entry = DefTable_Find(&Jit->Global, FnName.Ptr, FnName.Len, TYPE_FUNCTION);
    ASSERT(Entry, "TODO: undefined function");
    jit_function Function = Entry->As.Function;
    ASSERT(Function.ParamCount == ArgCount, "TODO: mismatched arg count");

    /* spill registers that are in use */
    storage_spill_data Spilled = Storage_Spill(&Jit->Storage);
    for (int i = 0; i < Spilled.Count; i++)
    {
        Emit_Store(&Jit->Emitter, Spilled.Reg[i], TargetEnv_GetStackFrameReg(), Spilled.StackOffset[i]);
    }

    /* emit argument */
    int ArgStackSize = TargetEnv_GetArgStackSize(ArgCount, Jit->Storage.DataSize);
    int ArgStackPtr = Storage_PushStack(&Jit->Storage, ArgStackSize);
    int ArgRegCount = MIN(Function.ParamCount, TargetEnv_GetArgRegCount());
    for (int i = 0; i < ArgCount; i++)
    {
        int Arg = IrStackCount - ArgCount + i; /* TODO: this is rtl calling conv, support ltr */
        jit_expression *ArgExpr = IrStack + Arg;
        if (i < TargetEnv_GetArgRegCount()) /* register argument */
        {
            int ArgReg = TargetEnv_GetArgReg(i);
            switch (ArgExpr->Storage)
            {
            case STORAGE_MEM:
            {
                Emit_Load(&Jit->Emitter, ArgReg, ArgExpr->As.Mem.BaseReg, ArgExpr->As.Mem.Offset);
            } break;
            case STORAGE_REG:
            {
                Emit_Move(&Jit->Emitter, ArgReg, ArgExpr->As.Reg);
                Storage_DeallocateReg(&Jit->Storage, ArgExpr->As.Reg);
            } break;
            }

            Storage_ForceAllocateReg(&Jit->Storage, ArgReg);
        }
        else /* memory argument */
        {
            i32 ArgOffset = TargetEnv_GetArgOffset(ArgStackPtr, i, Jit->Storage.DataSize);
            jit_reg ArgBaseReg = TargetEnv_GetArgBaseReg();
            jit_reg Tmp = Jit_CopyToReg(Jit, Storage_AllocateReg(&Jit->Storage).As.Reg, ArgExpr);
            Emit_Store(&Jit->Emitter, Tmp, ArgBaseReg, ArgOffset);
            Storage_DeallocateReg(&Jit->Storage, Tmp);
        }
    }
    Storage_PopStack(&Jit->Storage, ArgStackSize);

    /* deallocate argument registers */
    for (int i = 0; i < ArgRegCount; i++)
    {
        Storage_DeallocateReg(&Jit->Storage, TargetEnv_GetArgReg(i));
    }

    /* emit call */
    Emit_Call(&Jit->Emitter, Function.Dbg.Location);

    /* unspill spilled registers */
    bool8 UsingReturnReg = true;
    jit_reg ReturnReg = TargetEnv_GetReturnReg();
    jit_expression Result;
    Storage_Unspill(&Jit->Storage, &Spilled);
    for (int i = 0; i < Spilled.Count; i++)
    {
        if (TargetEnv_GetReturnReg() == Spilled.Reg[i]) /* the return register is about to get unspilled */
        {
            /* save its value in a different register */
            Result = Storage_AllocateReg(&Jit->Storage);
            Emit_Move(&Jit->Emitter, Result.As.Reg, ReturnReg);
            UsingReturnReg = false;
        }
        Emit_Load(&Jit->Emitter, Spilled.Reg[i], TargetEnv_GetStackFrameReg(), Spilled.StackOffset[i]);
    }

    if (UsingReturnReg)
    {
        Result = Storage_ForceAllocateReg(&Jit->Storage, ReturnReg);
    }
    IrStackCount -= ArgCount;
    IrStack[IrStackCount++] = Result;
    ASSERT(IrStackCount <= IrStackCapacity, "size");
    return IrStackCount;
}

static void Jit_TranslateIr(jit *Jit)
{
#define PSH(data) (IrStack[IrStackCount++] = (data))
#define POP() (IrStack[--IrStackCount])
    jit_expression *IrStack = Jit_Scratchpad_LeftPtr(Jit);
    int IrStackCapacity = Jit_Scratchpad_BytesRemain(Jit) / sizeof(jit_expression);
    int IrStackCount = 0;
    jit_function *Fn = NULL;
    for (int i = 0; i < Jit->IrOpCount; i++)
    {
        ASSERT(IrStackCount < IrStackCapacity, "size");
        jit_ir_op *Op = Jit->IrOp + i;
        switch (Op->Type)
        {
        case IR_OP_SWAP:
        {
            ASSERT(IrStackCount >= 2, "size");
            SWAP(jit_expression, IrStack[IrStackCount - 1], IrStack[IrStackCount - 2]);
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
            jit_expression Expr = Jit_IrDataAsExpr(Jit, IrData, ParamStart, ParamCount);
            PSH(Expr);
        } break;
        case IR_OP_STORE: /* store expr on stack to dst */
        {
            jit_reg Src = Jit_ToReg(Jit, POP());
            jit_mem Dst = Op->StoreDst;
            Emit_Store(&Jit->Emitter, Src, Dst.BaseReg, Dst.Offset);
        } break;
        case IR_OP_ENTRY:
        {
            Fn = Op->Entry.Function;
            ASSERT(Jit->IrDataCount >= Fn->ParamStart + Fn->ParamCount, "unreachable");

            Fn->Dbg.Location = Emit_FunctionEntry(&Jit->Emitter, Op->Entry.StackSize);

            /* set parameters to valid location */
            jit_reg ParamBaseReg = TargetEnv_GetParamBaseReg();
            for (int i = 0; i < Fn->ParamCount; i++)
            {
                i32 Displacement = TargetEnv_GetParamDisplacement(i, Jit->Storage.DataSize); 
                if (i < TargetEnv_GetArgRegCount())
                {
                    Emit_Store(&Jit->Emitter, TargetEnv_GetArgReg(i), ParamBaseReg, Displacement);
                }

                jit_ir_data *Param = Jit_Ir_GetData(Jit, Fn->ParamStart - i);
                ASSERT(IR_DATA_VAR_DEF == Param->Type, "unreachable");
                Param->As.VarDef.Expr = (jit_expression) {
                    .Storage = STORAGE_MEM,
                    .As.Mem = {
                        .Offset = Displacement,
                        .BaseReg = ParamBaseReg
                    },
                };
            }
        } break;
        case IR_OP_RETURN:
        {
            jit_expression ReturnValue = POP();
            Jit_CopyToReg(Jit, TargetEnv_GetReturnReg(), &ReturnValue);
            if (STORAGE_REG == ReturnValue.Storage)
                Storage_DeallocateReg(&Jit->Storage, ReturnValue.As.Reg);
            Emit_FunctionExit(&Jit->Emitter);
            Fn->Dbg.ByteCount = Jit_GetEmitterBufferSize(Jit) - Fn->Dbg.Location;
            Fn = NULL;
        } break;
        case IR_OP_CALL:
        {
            IrStackCount = Jit_EmitOpCall(
                Jit, 
                Op->Call.FnName, Op->Call.ArgCount,
                IrStack, IrStackCount, IrStackCapacity
            );
        } break;
        case IR_OP_ADD:
        case IR_OP_SUB:
        case IR_OP_MUL:
        case IR_OP_DIV:
        {
            jit_expression Right = POP();
            jit_expression Left = POP();
            jit_expression Result = {
                .Storage = STORAGE_REG,
                .As.Reg = Jit_ToReg(Jit, Left),
            };
            if (Right.Storage == STORAGE_REG)
            {
                Storage_DeallocateReg(&Jit->Storage, Right.As.Reg);
            }

#define OP(op_name, left_reg, right_expr) do {\
    if (STORAGE_MEM == (right_expr).Storage)\
        Emit_ ## op_name (&Jit->Emitter, left_reg, (right_expr).As.Mem.BaseReg, (right_expr).As.Mem.Offset);\
    else Emit_ ## op_name ## Reg(&Jit->Emitter, left_reg, (right_expr).As.Reg);\
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

            PSH(Result);
        } break;
        case IR_OP_NEG:
        {
            jit_expression Ptr = POP();
            jit_expression Result = Storage_AllocateReg(&Jit->Storage);
            Emit_LoadZero(&Jit->Emitter, Result.As.Reg);
            OP(Sub, Result.As.Reg, Ptr);
            PSH(Result);

            if (Ptr.Storage == STORAGE_REG)
            {
                Storage_DeallocateReg(&Jit->Storage, Ptr.As.Reg);
            }
        } break;
        }
#undef OP
    }
#undef PSH
#undef POP
}







static jit_function *DefineFunction(jit *Jit, const char *Name, int NameLen)
{
    def_table_entry *Label = DefTable_Define(&Jit->Global, Name, NameLen, TYPE_FUNCTION);
    jit_function *Function = &Label->As.Function;
    Function->Dbg.Location = Jit_GetEmitterBufferSize(Jit);
    return Function;
}




static void Jit_Function_Decl(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */
    jit_function *Function = DefineFunction(Jit, FnName->Str.Ptr, FnName->Str.Len);
    Storage_PushScope(&Jit->Storage);

    /* function params */
    Function->ParamCount = 0;
    Function->ParamStart = Jit->IrDataCount;
    if (TOK_RPAREN != NextToken(Jit).Type)
    {
        do {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
            jit_token Parameter = CurrToken(Jit); 
            Ir_PushVarDef(Jit, Parameter.Str);
            Function->ParamCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after function declaration.");

    /* function body */
    jit_ir_op *Op = Ir_PushOp(Jit, IR_OP_ENTRY);
    int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR);
    if (Index != -1)
    {
        Ir_PushOpLoad(Jit, Index);
    }
    Ir_PushOp(Jit, IR_OP_RETURN);

    /* function end */
    Op->Entry.StackSize = Storage_GetMaxStackSize(&Jit->Storage);
    Op->Entry.Function = Function;

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
    int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR);
    if (Index != -1)
    {
        Ir_PushOpLoad(Jit, Index);
    }
    jit_expression Global = Storage_AllocateGlobal(&Jit->Storage);
    Ir_PushOp(Jit, IR_OP_STORE)->StoreDst = Global.As.Mem;
    Variable->Expr = Global;
}


static void Jit_DisassembleCode(u8 *Buffer, uint Start, uint End, uint BytesPerLine)
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

static void Jit_Disassemble(jit *Jit)
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
        jit_debug_info Dbg = i->As.Common;

        printf("\n<%08x>: (%s) %.*s\n", Dbg.Location, Type[i->Type], Dbg.Str.Len, Dbg.Str.Ptr);
        Jit_DisassembleCode(Jit_GetEmitterBuffer(Jit), Dbg.Location, Dbg.Location + Dbg.ByteCount, BytesPerLine);
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


static void Jit_Reset(jit *Jit, const char *Expr, jit_compilation_flags Flags)
{
    Jit->ScratchpadLeftByteCount = 0;
    Jit->ScratchpadRightByteCount = 0;
    Jit->Flags = Flags;
    Jit->Start = Expr; 
    Jit->End = Expr;
    Jit->Line = 1;
    Jit->Offset = 1;

    /* ir op array starts from the left and grows rightward in the scratchpad */
    Jit->IrOpCount = 0;
    Jit->IrOp = Jit_Scratchpad_LeftPtr(Jit);
    /* ir data array starts from the right and grows leftward in the scratchpad */
    Jit->IrDataCount = 0;
    Jit->IrData = Jit_Scratchpad_RightPtr(Jit);

    /* pump the tokenizer */
    Jit->Next = (jit_token) { 0 };
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
    uint MinEvalStackCapacity = 1*1024;
    uint MinIrDataCapacity = 1*1024;
    uint MinIrOpCapacity = 6*1024;
    uint MinScratchpadCapacity = 
        + MinIrDataCapacity*sizeof(Jit->IrData[0])
        + MinIrOpCapacity*sizeof(Jit->IrOp[0])
        + MinEvalStackCapacity;
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




jit_result Jit_Compile(jit *Jit, jit_compilation_flags Flags, const char *Expr)
{
    Jit_Reset(Jit, Expr, Flags);

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
    Init->Dbg.ByteCount = Jit_GetEmitterBufferSize(Jit);
    if (Jit->Error.Available)
        goto ErrReturn;

    printf("============= var table ============\n");
    for (int i = 0; i < Jit->IrDataCount; i++)
    {
        jit_ir_data *Data = Jit_Ir_GetData(Jit, i);
        switch (Data->Type)
        {
        case IR_DATA_CONST:   printf("const  %d = %f\n", i, Data->As.Const); break;
        case IR_DATA_VAR_REF: printf("varref %d = '%.*s'\n", i, Data->As.VarRef.Len, Data->As.VarRef.Ptr); break;
        case IR_DATA_VAR_DEF: printf("vardef %d = '%.*s'\n", i, Data->As.VarDef.Dbg.Str.Len, Data->As.VarDef.Dbg.Str.Ptr); break;
        default: UNREACHABLE(); break;
        }
    }
    printf("============= instructions ============\n");
    for (int i = 0; i < Jit->IrOpCount; i++)
    {
        jit_ir_op *Op = Jit->IrOp + i;
        printf("%3d: %s\n", i, Get_OpName(Op->Type));
    }

    Jit_TranslateIr(Jit);
    Jit_Disassemble(Jit);
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
    ASSERT(Fn->Dbg.Location < Jit_GetEmitterBufferSize(Jit), "Function with invalid location");
    ASSERT(Fn->Dbg.Location + Fn->Dbg.ByteCount <= Jit_GetEmitterBufferSize(Jit), "Function with invalid size");
    u8 *FnPtr = Jit_GetEmitterBuffer(Jit) + Fn->Dbg.Location;
    return FnPtr;
}

