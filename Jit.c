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

#if 0
static bool8 Jit_ParseExpr(jit *Jit, precedence Prec);
#else
static int Jit_ParseExpr(jit *Jit, precedence Prec);
#endif


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

static char Peek(jit *Jit, uint Offset)
{
    for (uint i = Jit->SafePeekDist; i < Offset; i++)
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


static void Jit_Reset(jit *Jit, const char *Expr, jit_compilation_flags Flags)
{
    Jit->Flags = Flags;
    Jit->Start = Expr;
    Jit->End = Expr;
    Jit->Line = 1;
    Jit->Offset = 1;
    Jit->LocalVarCount = 0;
    Jit->LocalVarBase = 0;
    Jit->VarDeclEnd = -1;
#if 0
    Jit->ExprStackSize = 0;
#else
#endif
    ConsumeToken(Jit);

    Error_Reset(&Jit->Error);
    DefTable_Reset(&Jit->Global);

    bool8 EmitFloat32Instructions = 0 != (Flags & JIT_COMPFLAG_FLOAT32);
    int FltSize = EmitFloat32Instructions? sizeof(float) : sizeof(double);
    Storage_Reset(&Jit->Storage, FltSize);
    Emitter_Reset(&Jit->Emitter, EmitFloat32Instructions);
}


static jit_expression Jit_CopyToReg(jit *Jit, int Reg, const jit_expression *Expr)
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
    return (jit_expression) {
        .Storage = STORAGE_REG,
        .As.Reg = Reg,
    };
}


static jit_variable *Jit_FindVariable(jit *Jit, strview VarName)
{
    const char *Ptr = VarName.Ptr;
    int Len = VarName.Len;
    if (Jit->ScopeCount) /* in local scope */
    {
        for (int i = Jit->LocalVarBase; i < Jit->LocalVarCount; i++)
        {
            jit_variable *Variable = &Jit->LocalVars[i];
            if (Len == Variable->Dbg.Str.Len 
            && StrEqu(Ptr, Variable->Dbg.Str.Ptr, Len))
            {
                return Variable;
            }
        }
        /* failed local scope search, switch to global */
    }

    /* global scope */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, Ptr, Len, TYPE_VARIABLE);
    if (NULL == Entry)
    {
        return NULL;
    }
    return &Entry->As.Variable;
}




static jit_ir_op *Ir_PushOp(jit *Jit, jit_ir_op_type Type, int LoadIndexOrArgCount)
{
    ASSERT(Jit->IrOpCount < Jit->IrOpCapacity, "size");

    jit_ir_op *Op = &Jit->IrOp[Jit->IrOpCount++];
    Op->Type = Type;
    Op->ArgCount = LoadIndexOrArgCount;
    return Op;
}
static uint Ir_PushData(jit *Jit, const jit_ir_data *Data)
{
    ASSERT(Jit->IrDataCount < Jit->IrDataCapacity, "size");
    int Index = Jit->IrDataCount++;
    Jit->IrData[Index] = *Data;
    return Index;
}
static uint Ir_PushConst(jit *Jit, double Const)
{
    jit_ir_data Tmp = {
        .Type = IR_DATA_CONST,
        .As.Const = Const,
    };
    return Ir_PushData(Jit, &Tmp);
}
static uint Ir_PushVarRef(jit *Jit, strview VarName)
{
    jit_ir_data Tmp = {
        .Type = IR_DATA_VARREF,
        .As.VarRef = VarName,
    };
    return Ir_PushData(Jit, &Tmp);
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
        if (-1 == DataIndex)
        {
            Ir_PushOp(Jit, IR_OP_NEG, 1);
            break;
        }

        if (IR_DATA_CONST == Jit->IrData[DataIndex].Type)
        {
            Jit->IrData[DataIndex].As.Const = -Jit->IrData[DataIndex].As.Const;
        }
        else
        {
            Ir_PushOp(Jit, IR_OP_LOAD, DataIndex);
            Ir_PushOp(Jit, IR_OP_NEG, 1);
            DataIndex = -1;
        }
    } break; 
    case TOK_NUMBER: /* number */
    {
        DataIndex = Ir_PushConst(Jit, Token.As.Number);
    } break;
    case TOK_LPAREN: /* (expr) */
    {
        DataIndex = Jit_ParseExpr(Jit, PREC_EXPR);
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
                    int Arg = Jit_ParseExpr(Jit, PREC_EXPR);
                    if (Arg > -1)
                        Ir_PushOp(Jit, IR_OP_LOAD, Arg);
                    ArgCount++;
                } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
            }
            ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument list.");

            /* call Function with ArgCount arguments on stack */
            Ir_PushOp(Jit, IR_OP_CALL, ArgCount)->FnName = Token.Str;

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


static int Jit_ParseExpr(jit *Jit, precedence Prec)
{
    int Left = Jit_ParseUnary(Jit);
    while (PrecedenceOf(NextToken(Jit).Type) >= Prec)
    {
        jit_token_type Oper = ConsumeToken(Jit).Type;
        int Right = Jit_ParseExpr(Jit, PrecedenceOf(Oper) + 1);
        /* const expr, evaluate data now */
        if (Left != -1 && Right != -1 
        && IR_DATA_CONST == Jit->IrData[Left].Type 
        && IR_DATA_CONST == Jit->IrData[Right].Type)
        {
#define CONST(index) Jit->IrData[index].As.Const
            ASSERT((uint)Left == Jit->IrDataCount - 2, "");
            ASSERT((uint)Right == Jit->IrDataCount - 1, "");
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
                Ir_PushOp(Jit, IR_OP_LOAD, Left);
            if (Right != -1)
                Ir_PushOp(Jit, IR_OP_LOAD, Right);
            /* Right operand was on stack first */
            if (Left != -1 && -1 == Right)
                Ir_PushOp(Jit, IR_OP_SWAP, 2);

            switch (Oper)
            {
            case TOK_PLUS:  Ir_PushOp(Jit, IR_OP_ADD, 2); break;
            case TOK_MINUS: Ir_PushOp(Jit, IR_OP_SUB, 2); break;
            case TOK_STAR:  Ir_PushOp(Jit, IR_OP_MUL, 2); break;
            case TOK_SLASH: Ir_PushOp(Jit, IR_OP_DIV, 2); break;
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

static jit_expression Jit_IrDataAsExpr(jit *Jit, const jit_ir_data *Data)
{
    switch (Data->Type)
    {
    case IR_DATA_CONST:
    {
        return Storage_AllocateConst(&Jit->Storage, Data->As.Const);
    } break;
    case IR_DATA_VARREF:
    {
        jit_variable *Entry = Jit_FindVariable(Jit, Data->As.VarRef);
        ASSERT(Entry, "TODO: undefined variable");
        return Entry->Expr;
    } break;
    }
    UNREACHABLE();
}


static uint Jit_EmitOpCall(jit *Jit, 
    strview FnName, int ArgCount, 
    jit_expression *IrStack, uint IrStackCount, uint IrStackCapacity)
{
    ASSERT(ArgCount <= (int)IrStackCount, "arg count");

    /* find the function */
    const def_table_entry *Entry = DefTable_Find(&Jit->Global, FnName.Ptr, FnName.Len, TYPE_FUNCTION);
    ASSERT(Entry, "TODO: undefined function");
    jit_function Function = Entry->As.Function;
    ASSERT(Function.ParamCount == ArgCount, "TODO: mismatched arg count");

    /* spill registers that are in use */
    storage_spill_data Spilled = Storage_Spill(&Jit->Storage);
    for (uint i = 0; i < Spilled.Count; i++)
    {
        Emit_Store(&Jit->Emitter, Spilled.Reg[i], TargetEnv_GetStackFrameReg(), Spilled.StackOffset[i]);
    }

    /* emit argument */
    int ArgStackSize = TargetEnv_GetArgStackSize(ArgCount, Jit->Storage.DataSize);
    int ArgStackTop = Storage_PushStack(&Jit->Storage, ArgStackSize);
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
            int ArgOffset = TargetEnv_GetArgOffset(ArgStackTop, i, Jit->Storage.DataSize);
            int ArgBaseReg = TargetEnv_GetArgBaseReg();
            int Tmp = Jit_CopyToReg(Jit, Storage_AllocateReg(&Jit->Storage).As.Reg, ArgExpr).As.Reg;
            Emit_Store(&Jit->Emitter, Tmp, ArgBaseReg, ArgOffset);
            Storage_DeallocateReg(&Jit->Storage, Tmp);
        }
    }
    /* deallocate argument registers */
    for (int i = 0; i < ArgRegCount; i++)
    {
        Storage_DeallocateReg(&Jit->Storage, TargetEnv_GetArgReg(i));
    }

    /* emit call */
    Emit_Call(&Jit->Emitter, Function.Dbg.Location);

    /* unspill spilled registers */
    bool8 UsingReturnReg = true;
    jit_expression Result;
    Storage_Unspill(&Jit->Storage, &Spilled);
    for (uint i = 0; i < Spilled.Count; i++)
    {
        if (Function.ReturnReg == Spilled.Reg[i]) /* the return register is about to get unspilled */
        {
            /* save its value in a different register */
            Result = Storage_AllocateReg(&Jit->Storage);
            Emit_Move(&Jit->Emitter, Result.As.Reg, Function.ReturnReg);
            UsingReturnReg = false;
        }
        Emit_Load(&Jit->Emitter, Spilled.Reg[i], TargetEnv_GetStackFrameReg(), Spilled.StackOffset[i]);
    }

    if (UsingReturnReg)
    {
        Result = Storage_ForceAllocateReg(&Jit->Storage, Function.ReturnReg);
    }
    IrStackCount -= ArgCount;
    IrStack[IrStackCount++] = Result;
    ASSERT(IrStackCount <= IrStackCapacity, "size");
    return IrStackCount;
}

static jit_expression Jit_TranslateIr(jit *Jit)
{
#define PSH(data) (IrStack[IrStackCount++] = (data))
#define POP() (IrStack[--IrStackCount])
    jit_expression IrStack[256];
    uint IrStackCount = 0, 
         IrStackCapacity = STATIC_ARRAY_SIZE(IrStack);
    for (uint i = 0; i < Jit->IrOpCount; i++)
    {
        ASSERT(IrStackCount < IrStackCapacity, "size");

        if (IR_OP_SWAP == Jit->IrOp[i].Type)
        {
            ASSERT(IrStackCount >= 2, "size");
            SWAP(jit_expression, IrStack[IrStackCount - 1], IrStack[IrStackCount - 2]);
        }
        else if (IR_OP_LOAD == Jit->IrOp[i].Type)
        {
            jit_ir_data *IrData = &Jit->IrData[Jit->IrOp[i].LoadIndex];
            jit_expression Expr = Jit_IrDataAsExpr(Jit, IrData);
            PSH(Expr);
        }
        else if (IR_OP_CALL == Jit->IrOp[i].Type)
        {
            IrStackCount = Jit_EmitOpCall(
                Jit, 
                Jit->IrOp[i].FnName, Jit->IrOp[i].ArgCount,
                IrStack, IrStackCount, IrStackCapacity
            );
        }
        else if (Jit->IrOp[i].ArgCount == 2) /* binary op */
        {
            jit_expression Right = POP();
            jit_expression Left = POP();
            jit_expression Result = Left;
            if (Left.Storage != STORAGE_REG)
            {
                Result = Jit_CopyToReg(Jit, Storage_AllocateReg(&Jit->Storage).As.Reg, &Left);
            }
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
        }
        else switch (Jit->IrOp[i].Type)
        {
        case IR_OP_NEG:
        {
            jit_expression Top = POP();
            jit_expression Result = Storage_AllocateReg(&Jit->Storage);
            Emit_LoadZero(&Jit->Emitter, Result.As.Reg);
            OP(Sub, Result.As.Reg, Top);
            PSH(Result);

            if (Top.Storage == STORAGE_REG)
            {
                Storage_DeallocateReg(&Jit->Storage, Top.As.Reg);
            }
        } break;
        default: 
        {
            UNREACHABLE();
        } break;
        }
#undef OP
    }
    ASSERT(IrStackCount == 1, "stack");
    return IrStack[0];
#undef PSH
#undef POP
}




static u8 *Jit_GetEmitterBuffer(jit *Jit)
{
    return Jit->Emitter.Base.Buffer;
}

static u8 Jit_GetEmitterBufferSize(const jit *Jit)
{
    return Jit->Emitter.Base.BufferSize;
}


static void Jit_FunctionBeginScope(jit *Jit, jit_function *Function)
{
    Function->ParamStart = Jit->LocalVarCount;
    Function->ParamCount = 0;

    /* add a new scope */
    Jit->ScopeCount++;
    Jit->LocalVarBase = Jit->LocalVarCount;
    /* reset mem stack */
    Storage_PushScope(&Jit->Storage);
}

static void Jit_FunctionEndScope(jit *Jit)
{
    /* back up a scope */
    Jit->ScopeCount--;
    Storage_PopScope(&Jit->Storage);
}

static void Jit_FunctionPushLocal(jit *Jit, jit_function *Function, const jit_token *Parameter)
{
    if (Jit->LocalVarCount + 1 > Jit->LocalVarCapacity)
    {
        Error_AtToken(&Jit->Error, Parameter, "Out of memory for param.");
        return;
    }

    Jit->LocalVars[Jit->LocalVarCount] = (jit_variable) {
        .Dbg.Str = Parameter->Str,
    };
    Jit->LocalVarCount++;
    Function->ParamCount++;
}

static jit_function *DefineFunction(jit *Jit, const char *Name, int NameLen)
{
    def_table_entry *Label = DefTable_Define(&Jit->Global, Name, NameLen, TYPE_FUNCTION);
    jit_function *Function = &Label->As.Function;
    Function->Dbg.Location = Jit_GetEmitterBufferSize(Jit);
    return Function;
}


static void Jit_ResetIr(jit *Jit)
{
    Jit->IrOpCount = 0;
    Jit->IrOpCapacity = STATIC_ARRAY_SIZE(Jit->IrOp);
    Jit->IrDataCount = 0;
    Jit->IrDataCapacity = STATIC_ARRAY_SIZE(Jit->IrData);
}

static void FunctionDecl(jit *Jit, jit_token FnName)
{
    /* consumed '(' */
    jit_function *Function = DefineFunction(Jit, FnName.Str.Ptr, FnName.Str.Len);
    Jit_FunctionBeginScope(Jit, Function);

    if (TOK_RPAREN != NextToken(Jit).Type)
    {
        do {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
            jit_token Parameter = CurrToken(Jit); 
            Jit_FunctionPushLocal(Jit, Function, &Parameter);
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");

    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after function declaration.");

    {
        Jit_ResetIr(Jit);
        int Index = Jit_ParseExpr(Jit, PREC_EXPR);
        if (Index != -1)
            Ir_PushOp(Jit, IR_OP_LOAD, Index);

        Function->Dbg.Location = Emit_FunctionEntry(&Jit->Emitter, Jit->LocalVars + Jit->LocalVarBase, Function->ParamCount);

        jit_expression ReturnValue = Jit_TranslateIr(Jit);

        Jit_CopyToReg(Jit, TargetEnv_GetReturnReg(), &ReturnValue);
        Emit_FunctionExit(&Jit->Emitter);
        Function->Dbg.ByteCount = Jit_GetEmitterBufferSize(Jit) - Function->Dbg.Location;
        Emit_PatchStackSize(&Jit->Emitter, Function->Dbg.Location, Storage_GetMaxStackSize(&Jit->Storage));
    }
    
    Jit_FunctionEndScope(Jit);
}

static void VariableDecl(jit *Jit, jit_token VarName)
{
    /* consumed VarName */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after variable name.");
    
    Jit_ResetIr(Jit);
    Jit_ParseExpr(Jit, PREC_EXPR);
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


uint Jit_Init(
    jit *Jit,
    void *Scratchpad, uint ScratchpadCapacity, 
    void *GlobalMemory, uint GlobalMemCapacity, 
    void *ProgramMemory, uint ProgramMemCapacity,
    def_table_entry *DefTableArray, uint DefTableCapacity
)
{
    uint MinScratchpadCapacity = 256*sizeof(jit_variable) + 128*sizeof(jit_expression);
    if (ScratchpadCapacity < MinScratchpadCapacity)
    {
        return MinScratchpadCapacity;
    }

    u8 *ScratchpadPtr = Scratchpad;
    jit_expression *ExprStack = Scratchpad;
    uint ExprStackCapacity = 128;
    jit_variable *Locals = (jit_variable *)(ScratchpadPtr + ExprStackCapacity*sizeof(jit_expression));
    uint LocalCapacity = (ScratchpadCapacity - ExprStackCapacity*sizeof(jit_expression)) / sizeof(jit_variable);

    *Jit = (jit) {
        .Storage = Storage_Init(GlobalMemory, GlobalMemCapacity),
        .Global = DefTable_Init(DefTableArray, DefTableCapacity, Jit_Hash),

#if 0
        .ExprStack = ExprStack,
        .ExprStackCapacity = ExprStackCapacity,
#else
#endif
        .LocalVars = Locals,
        .LocalVarCapacity = LocalCapacity,
    };
    Emitter_Init(&Jit->Emitter, ProgramMemory, ProgramMemCapacity);
    return 0;
}

void Jit_Destroy(jit *Jit)
{
    memset(Jit, 0, sizeof(jit));
}




jit_result Jit_Compile(jit *Jit, jit_compilation_flags Flags, const char *Expr)
{
    Jit_Reset(Jit, Expr, Flags);

    jit_function *Init = DefineFunction(Jit, "init", 4);
    {
        Emit_FunctionEntry(&Jit->Emitter, NULL, 0);
        Jit->VarDeclEnd = Emit_Jump(&Jit->Emitter);
        do {
            /* definition */
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected a variable or function declaration.");
            jit_token Identifier = CurrToken(Jit);
            if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
            {
                FunctionDecl(Jit, Identifier);
            }
            else 
            {
                VariableDecl(Jit, Identifier);
            }

            /* skip all newlines */
            while (!Jit->Error.Available && ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
            {}
        } while (!Jit->Error.Available && TOK_EOF != NextToken(Jit).Type);
    }
#if 0
    Jit_PatchJump(Jit, Jit_GetEmitterBufferSize(Jit));
#endif
    Emit_FunctionExit(&Jit->Emitter);
    Emit_PatchStackSize(&Jit->Emitter, Init->Dbg.Location, Storage_GetMaxStackSize(&Jit->Storage));
    Init->Dbg.ByteCount = Jit_GetEmitterBufferSize(Jit);

    if (!Jit->Error.Available)
    {
        ASSERT(Jit->Global.Head, "Missing init routine");
        Jit_Disassemble(Jit);
        return (jit_result) {
            .GlobalData = Jit->Storage.GlobalMemory,
            .GlobalSymbol = Jit->Global.Head->Next,
        };
    }
    else
    {
        return (jit_result) {
            .ErrMsg = Jit->Error.Msg,
        };
    }
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

