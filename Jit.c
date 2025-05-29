#include "Jit.h"
#include "DefTable.h"
#include "Emitter.h"

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

static bool8 Jit_ParseExpr(jit *Jit, precedence Prec);


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
    jit_token Tok = {
        .Type = Type,
        .Line = Jit->Line,
        .Offset = Jit->Offset, 
        .Str = {
            .Ptr = Jit->Start,
            .Len = Jit->End - Jit->Start,
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
    while (IsIdentifier(Peek(Jit, 0)))
    {
        Advance(Jit);
    }

    jit_token Identifier = CreateToken(Jit, TOK_IDENTIFIER);
    if (STREQU("def", Identifier.Str))
    {
        Identifier.Type = TOK_DEF;
    }
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
    Jit->Start = Jit->End;
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



static void ErrorVA(jit *Jit, const char *ErrMsg, va_list Args)
{
    if (Jit->Error)
        return;

    Jit->Error = true;
    vsnprintf(Jit->ErrMsg, sizeof Jit->ErrMsg, ErrMsg, Args);
}

static void Error(jit *Jit, const char *ErrMsg, ...)
{
    if (Jit->Error)
        return;

    va_list Args;
    va_start(Args, ErrMsg);
    ErrorVA(Jit, ErrMsg, Args);
    va_end(Args);
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
        ErrorVA(Jit, ErrMsg, Args);
        va_end(Args);
        return false;
    }
    ConsumeToken(Jit);
    return true;
}


static void Jit_Reset(jit *Jit, const char *Expr)
{
    Jit->Start = Expr;
    Jit->End = Expr;
    Jit->Line = 1;
    Jit->Offset = 1;
    Jit->Error = false;
    Jit->LocalVarCount = 0;
    Jit->LocalVarBase = 0;
    Jit->LocalVarCapacity = STATIC_ARRAY_SIZE(Jit->LocalVars);
    Jit->ExprStackSize = 0;
    Jit->ExprStackCapacity = STATIC_ARRAY_SIZE(Jit->ExprStack);
    /* 5 = RBP, 0 = RAX */
    Jit->Storage = Storage_Init(5, 0);
    Jit->Sign.As.Uint = 1llu << 63;
    Jit->Sign.Location = Storage_AllocateConst(&Jit->Storage, Jit->Sign.As.Double);
    ConsumeToken(Jit);
}




static jit_variable *Jit_FindVariable(jit *Jit, const jit_token *VarName)
{
    const char *Ptr = VarName->Str.Ptr;
    int Len = VarName->Str.Len;

    if (Jit->ScopeCount) /* in local scope */
    {
        for (int i = Jit->LocalVarBase; i < Jit->LocalVarCount; i++)
        {
            jit_variable *Variable = &Jit->LocalVars[i];
            if (Len == Variable->Str.Len 
            && StrEqu(Ptr, Variable->Str.Ptr, Len))
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


static jit_expression *Expr_Peek(jit *Jit)
{
    assert(Jit->ExprStackSize > 0 && "Expr_Peek");
    return &Jit->ExprStack[Jit->ExprStackSize - 1];
}

static jit_expression *Expr_Pop(jit *Jit)
{
    assert(Jit->ExprStackSize > 0 && "Expr_Pop");
    return &Jit->ExprStack[--Jit->ExprStackSize];
}

static void Expr_Push(jit *Jit, const jit_expression *E)
{
    assert(Jit->ExprStackSize < Jit->ExprStackCapacity && "Expr_Push");
    Jit->ExprStack[Jit->ExprStackSize++] = *E;
}


static void Expr_PushNumber(jit *Jit, double Number)
{
    jit_expression Expr = {
        .Storage = STORAGE_CONST,
        .As.Const = Number,
    };
    Expr_Push(Jit, &Expr);
}

static void Expr_Neg(jit *Jit)
{
    jit_expression *E = Expr_Peek(Jit);
    switch (E->Storage)
    {
    case STORAGE_CONST:
    {
        E->As.Const = -E->As.Const;
    } break;
    case STORAGE_MEM:
    {
        int Reg = Jit->Sign.Location.As.Mem.BaseReg;
        i32 Offset = Jit->Sign.Location.As.Mem.Offset;
        jit_expression Result = Storage_AllocateReg(&Jit->Storage);
        int TmpReg = Storage_AllocateReg(&Jit->Storage).As.Reg;

        Emit_Load(&Jit->Emitter, TmpReg, Reg, Offset);
        Emit_Load(&Jit->Emitter, Result.As.Reg, E->As.Mem.BaseReg, E->As.Mem.Offset);
        Emit_XorReg(&Jit->Emitter, Result.As.Reg, TmpReg);
        Storage_DeallocateReg(&Jit->Storage, TmpReg);

        *E = Result;
    } break;
    case STORAGE_REG:
    {
        int TmpReg = Storage_AllocateReg(&Jit->Storage).As.Reg;
        int Reg = Jit->Sign.Location.As.Mem.BaseReg;
        i32 Offset = Jit->Sign.Location.As.Mem.Offset;

        Emit_Load(&Jit->Emitter, TmpReg, Reg, Offset);
        Emit_XorReg(&Jit->Emitter, E->As.Reg, TmpReg);
        Storage_DeallocateReg(&Jit->Storage, TmpReg);
    } break;
    }
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
    case STORAGE_CONST:
    {
        /* allocate the const */
        jit_expression Const = Storage_AllocateConst(&Jit->Storage, Expr->As.Const);
        Emit_Load(&Jit->Emitter, Reg, Const.As.Mem.BaseReg, Const.As.Mem.Offset);
    } break;
    }
    return (jit_expression) {
        .Storage = STORAGE_REG,
        .As.Reg = Reg,
    };
}


#define DEFINE_BINARY_EXPR(name, op)\
static void Expr_ ## name (jit *Jit, jit_expression *Lhs, const jit_expression *Rhs) {\
    if (Lhs->Storage == STORAGE_CONST && Rhs->Storage == STORAGE_CONST) {\
        Lhs->As.Const op ## = Rhs->As.Const;\
    } else {\
        if (STORAGE_REG != Lhs->Storage) {\
            *Lhs = Jit_CopyToReg(Jit, Storage_AllocateReg(&Jit->Storage).As.Reg, Lhs);\
        }\
        switch (Rhs->Storage) {\
        case STORAGE_MEM: {\
            Emit_ ## name (&Jit->Emitter, Lhs->As.Reg, Rhs->As.Mem.BaseReg, Rhs->As.Mem.Offset);\
        } break;\
        case STORAGE_REG: {\
            Emit_ ## name ## Reg(&Jit->Emitter, Lhs->As.Reg, Rhs->As.Reg);\
        } break;\
        case STORAGE_CONST: {\
            jit_expression Tmp = Storage_AllocateConst(&Jit->Storage, Rhs->As.Const);\
            Emit_ ## name (&Jit->Emitter, Lhs->As.Reg, Tmp.As.Mem.BaseReg, Tmp.As.Mem.Offset);\
        } break;\
        }\
    }\
}\
static void Expr_ ## name (jit *Jit, jit_expression *Left, const jit_expression *Right) 


DEFINE_BINARY_EXPR(Add, +);
DEFINE_BINARY_EXPR(Sub, -);
DEFINE_BINARY_EXPR(Mul, *);
DEFINE_BINARY_EXPR(Div, /);

#undef DEFINE_STORAGE_BINARY

static bool8 Expr_ParseArgs(jit *Jit, const jit_function *Fn)
{
    int ArgCount = 0;
    if (NextToken(Jit).Type != TOK_RPAREN)
    {
        do {
            assert(ArgCount < 4 && "TODO: more args");
            if (Jit_ParseExpr(Jit, PREC_EXPR))
            {
                jit_expression *Tmp = Expr_Pop(Jit);
                switch (Tmp->Storage)
                {
                case STORAGE_CONST:
                {
                    jit_expression Global = Storage_AllocateConst(&Jit->Storage, Tmp->As.Const);
                    Emit_Load(&Jit->Emitter, ArgCount, Global.As.Mem.BaseReg, Global.As.Mem.Offset);
                } break;
                case STORAGE_MEM:
                {
                    Emit_Load(&Jit->Emitter, ArgCount, Tmp->As.Mem.BaseReg, Tmp->As.Mem.Offset);
                } break;
                case STORAGE_REG:
                {
                    Emit_Move(&Jit->Emitter, ArgCount, Tmp->As.Reg);
                    Storage_DeallocateReg(&Jit->Storage, Tmp->As.Reg);
                } break;
                }

                Storage_ForceAllocateReg(&Jit->Storage, ArgCount);
            }
            ArgCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after arguments.");

    /* check arg count */
    if (ArgCount != Fn->ParamCount)
    {
        Error(Jit, "Expected %d arguments to '%.*s', got %d instead.", 
            Fn->ParamCount, Fn->Str.Len, Fn->Str.Ptr, ArgCount
        );
        return false;
    }
    return true;
}

static jit_expression Expr_Call(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */

    /* find the function */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, FnName->Str.Ptr, FnName->Str.Len, TYPE_FUNCTION);
    if (!Entry)
    {
        Error(Jit, "Call to undefined function: '%.*s'.", FnName->Str.Len, FnName->Str.Ptr);
        goto ErrReturn;
    }
    jit_function *Function = &Entry->As.Function;


    /* spill registers that are in use */
    storage_spill_data Spilled = Storage_Spill(&Jit->Storage); 
    for (uint i = 0; i < Spilled.Count; i++)
    {
        Emit_Store(&Jit->Emitter, Spilled.Reg[i], Jit->Storage.StackPtrReg, Spilled.StackOffset[i]);
    }

    /* parse args and emit call */
    if (!Expr_ParseArgs(Jit, Function))
        goto ErrReturn;
    Emit_Call(&Jit->Emitter, Function->Location);
    /* deallocate registers that were used as argument to the call */
    for (int i = 0; i < Function->ParamCount; i++)
    {
        Storage_DeallocateReg(&Jit->Storage, i);
    }

    /* reload spilled registers */
    Storage_Unspill(&Jit->Storage, &Spilled);
    bool8 DifferentReturnReg = false;
    jit_expression Result;
    for (uint i = 0; i < Spilled.Count; i++)
    {
        if (Function->ReturnReg == Spilled.Reg[i]) /* the return register is about to get unspilled */
        {
            /* save its value in a different register */
            Result = Storage_AllocateReg(&Jit->Storage);
            Emit_Move(&Jit->Emitter, Result.As.Reg, Function->ReturnReg);
            DifferentReturnReg = true;
        }
        Emit_Load(&Jit->Emitter, Spilled.Reg[i], Jit->Storage.StackPtrReg, Spilled.StackOffset[i]);
    }

    if (DifferentReturnReg)
        return Result;
    return Storage_ForceAllocateReg(&Jit->Storage, Function->ReturnReg);
ErrReturn:
    return (jit_expression) { 0 };
}

static jit_expression Expr_Variable(jit *Jit, const jit_token *VarName)
{
    /* consumed VarName */
    /* find the variable */
    jit_variable *Variable = Jit_FindVariable(Jit, VarName);
    if (NULL == Variable)
    {
        Error(Jit, "Undefined variable.");
        return (jit_expression) { 0 };
    }
    return Variable->Expr;
}


static bool8 Jit_ParseUnary(jit *Jit)
{
    jit_token Left = ConsumeToken(Jit);
    switch (Left.Type)
    {
    case TOK_PLUS:
    {
        return Jit_ParseUnary(Jit);
    }
    case TOK_MINUS:
    {
        if (!Jit_ParseUnary(Jit))
            return !Jit->Error;

        Expr_Neg(Jit);
    } break; 
    case TOK_NUMBER:
    {
        Expr_PushNumber(Jit, Left.As.Number);
    } break;
    case TOK_LPAREN:
    {
        Jit_ParseExpr(Jit, PREC_EXPR);
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after expression.");
    } break;
    case TOK_IDENTIFIER:
    {
        jit_expression Result;
        /* function call */
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            Result = Expr_Call(Jit, &Left);
        }
        /* variable reference */
        else
        {
            Result = Expr_Variable(Jit, &Left);
        }
        Expr_Push(Jit, &Result);
    } break;
    default:
    {
        Error(Jit, "Expected an expression.");
    } break;
    }

    return !Jit->Error;
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

static bool8 Jit_ParseExpr(jit *Jit, precedence Prec)
{
    bool8 Error = !Jit_ParseUnary(Jit);
    while (PrecedenceOf(NextToken(Jit).Type) >= Prec)
    {
        jit_token_type Oper = ConsumeToken(Jit).Type;
        Error = Error || !Jit_ParseExpr(Jit, Oper + 1);
        if (Error)
            continue;

        jit_expression *Right = Expr_Pop(Jit);
        jit_expression *Result = Expr_Peek(Jit);
        switch (Oper)
        {
        case TOK_PLUS:  Expr_Add(Jit, Result, Right); break;
        case TOK_MINUS: Expr_Sub(Jit, Result, Right); break;
        case TOK_STAR:  Expr_Mul(Jit, Result, Right); break;
        case TOK_SLASH: Expr_Div(Jit, Result, Right); break;
        case TOK_CARET: 
        {
            assert(false && "TODO: pow(x, y)");
        } break;
        default: 
        {
            /* unreachable */
            assert(false && "UNREACHABLE");
        } break;
        }

        switch (Right->Storage)
        {
        case STORAGE_REG:
        {
            Storage_DeallocateReg(&Jit->Storage, Right->As.Reg);
        } break;
        case STORAGE_MEM:
        {
            Storage_PopStack(&Jit->Storage, sizeof(double));
        } break;
        case STORAGE_CONST:
        {
        } break;
        }

    }
    return !Error && !Jit->Error;
}




static void Jit_FunctionBeginScope(jit *Jit, jit_function *Function)
{
    assert(Jit->LocalVarCount < Jit->LocalVarCapacity && "TODO: Dynamic capacity.");
    Function->ParamStart = Jit->LocalVarCount;
    Function->ParamCount = 0;

    /* add a new scope */
    Jit->ScopeCount++;
    Jit->LocalVarBase = Jit->LocalVarCount;
    /* reset mem stack */
    Storage_ResetTmpAndStack(&Jit->Storage);
}

static void Jit_FunctionEndScope(jit *Jit)
{
    /* back up a scope */
    Jit->ScopeCount--;
}

static void Jit_FunctionPushLocal(jit *Jit, jit_function *Function, const jit_token *Parameter)
{
    assert(Jit->LocalVarCount + 1 < Jit->LocalVarCapacity && "TODO: Dynamic capacity.");
    Jit->LocalVars[Jit->LocalVarCount] = (jit_variable) {
        .Str = Parameter->Str,
    };
    Jit->LocalVarCount++;
    Function->ParamCount++;
}



static void FunctionDecl(jit *Jit, jit_token FnName)
{
    /* consumed '(' */
    def_table_entry *Label = DefTable_Define(&Jit->Global, FnName.Str.Ptr, FnName.Str.Len, TYPE_FUNCTION);
    jit_function *Function = &Label->As.Function;
    Jit_FunctionBeginScope(Jit, Function);
    {
        /* parameter */
        if (NextToken(Jit).Type == TOK_IDENTIFIER)
        {
            do {
                ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
                jit_token Parameter = CurrToken(Jit); 
                Jit_FunctionPushLocal(Jit, Function, &Parameter);
            } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
        }
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");

        /* equ sign */
        ConsumeOrError(Jit, TOK_EQUAL, "Expected '='.");

        /* emit function entry code */
        Function->Location = Emit_FunctionEntry(&Jit->Emitter, Jit->LocalVars + Jit->LocalVarBase, Function->ParamCount);

        /* function body */
        if (Jit_ParseExpr(Jit, PREC_EXPR))
        {
            jit_expression Result = *Expr_Pop(Jit);

            int ReturnReg = 0;
            switch (Result.Storage)
            {
            case STORAGE_CONST:
            {
                Result = Storage_AllocateConst(&Jit->Storage, Result.As.Const);
                Emit_Load(&Jit->Emitter, ReturnReg, Result.As.Mem.BaseReg, Result.As.Mem.Offset);
            } break;
            case STORAGE_MEM:
            {
                Emit_Load(&Jit->Emitter, ReturnReg, Result.As.Mem.BaseReg, Result.As.Mem.Offset);
            } break;
            case STORAGE_REG:
            {
                Emit_Move(&Jit->Emitter, ReturnReg, Result.As.Reg);
            } break;
            }

            Function->ReturnReg = ReturnReg;
        }

        /* emit function exit code */
        Emit_FunctionExit(&Jit->Emitter);
        Emit_PatchStackSize(&Jit->Emitter, Function->Location, Jit->Storage.MaxStackSize);
    }
    Jit_FunctionEndScope(Jit);
}

static void VariableDecl(jit *Jit, jit_token Identifier)
{
    /* consumed Identifier */
    def_table_entry *Definition = DefTable_Define(&Jit->Global, Identifier.Str.Ptr, Identifier.Str.Len, TYPE_VARIABLE);

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '='.");

    /* parse expr */
    if (Jit_ParseExpr(Jit, PREC_EXPR))
    {
        Definition->As.Variable.Expr = *Expr_Pop(Jit);
        jit_expression *Var = &Definition->As.Variable.Expr;
        switch (Var->Storage)
        {
        case STORAGE_CONST:
        {
            *Var = Storage_AllocateConst(&Jit->Storage, Var->As.Const);
        } break;
        case STORAGE_MEM:
        {
            jit_expression Global = Storage_AllocateGlobal(&Jit->Storage);

            jit_expression Tmp = Storage_AllocateReg(&Jit->Storage);
            Emit_Load(&Jit->Emitter, Tmp.As.Reg, Var->As.Mem.BaseReg, Var->As.Mem.Offset);
            Emit_Store(&Jit->Emitter, Tmp.As.Reg, Global.As.Mem.BaseReg, Global.As.Mem.Offset);
            Storage_DeallocateReg(&Jit->Storage, Tmp.As.Reg);

            *Var = Global;
        } break;
        case STORAGE_REG:
        {
            jit_expression Global = Storage_AllocateGlobal(&Jit->Storage);
            Emit_Store(&Jit->Emitter, Var->As.Reg, Global.As.Mem.BaseReg, Global.As.Mem.Offset);
            Storage_DeallocateReg(&Jit->Storage, Var->As.Reg);

            *Var = Global;
        } break;
        }
    }
}


jit Jit_Init(void)
{
    jit Jit = { 0 };
    return Jit;
}

jit_result Jit_Evaluate(jit *Jit, const char *Expr)
{
    Jit_Reset(Jit, Expr);
    while (CurrToken(Jit).Type != TOK_EOF)
    {
        /* definition */
        if (ConsumeIfNextTokenIs(Jit, TOK_DEF))
        {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected an identifier.");
            jit_token Identifier = CurrToken(Jit);
            if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
            {
                FunctionDecl(Jit, Identifier);
            }
            else if (NextToken(Jit).Type == TOK_EQUAL)
            {
                VariableDecl(Jit, Identifier);
            }
            else
            {
                Error(Jit, "Expected function name.");
            }
        }
        /* expression */
        else 
        {
            Jit_ParseExpr(Jit, PREC_EXPR);
            goto Done;
        }

        if (!ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
        {
            ConsumeOrError(Jit, TOK_EOF, "Expected new line.");
        }
    }

Done:
    ;
    char Instruction[64] = { 0 };
    uint BytesPerLine = 10;
    printf("Const count: %d\n", Jit->Storage.ConstCount);
    for (u64 i = 0; i < Jit->Emitter.InstructionByteCount;)
    {
        uint InstructionBytes = 
            DisasmSingleInstruction(i, Jit->Emitter.InstructionBuffer + i, Jit->Emitter.InstructionByteCount - i, Instruction);


        printf("%08x:  ", (u32)i);
        uint k;
        for (k = 0; k < InstructionBytes; k++)
        {
            printf("%02x ", Jit->Emitter.InstructionBuffer[i + k]);
        }
        while (k < BytesPerLine)
        {
            printf("   ");
            k++;
        }

        printf("%s\n", Instruction);

        i += InstructionBytes;
    }
    printf("Consts: \n");
    for (int i = 0; i < Jit->Storage.ConstCount; i++)
    {
        printf("[rax + %d] = %g\n", Jit->Storage.ConstOffset[i], Jit->Storage.Consts[i]);
    }

    if (Jit->Error)
    {
        return (jit_result) {
            .Valid = false,
            .As.ErrMsg = Jit->ErrMsg,
        };
    }
    else
    {
        return (jit_result) {
            .Valid = true,
            .As.Number = Jit->ExprStack[0].As.Const,
        };
    } 
}

void Jit_Destroy(jit *Jit)
{
}

