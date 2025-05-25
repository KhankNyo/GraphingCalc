#include "Jit.h"
#include "DefTable.h"

#include <assert.h>
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
    while (IsIdentifier(Peek(Jit, 0)))
    {
        Advance(Jit);
    }
    return CreateToken(Jit, TOK_IDENTIFIER);
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
    Jit->LocalVarCapacity = STATIC_ARRAY_SIZE(Jit->LocalVars);
    Jit->ExprStackSize = 0;
    Jit->ExprStackCapacity = STATIC_ARRAY_SIZE(Jit->ExprStack);
    ConsumeToken(Jit);
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
        .Type = EXPR_CONST,
        .As.Number = Number,
    };
    Expr_Push(Jit, &Expr);
}

static void Expr_Neg(jit *Jit)
{
    jit_expression *E = Expr_Peek(Jit);
    switch (E->Type)
    {
    case EXPR_CONST:
    {
        E->As.Number = -E->As.Number;
    } break;
    }
}

#define DEFINE_EXPR_BINARY(name, op)\
static jit_expression Expr_ ## name (jit *Jit, jit_expression *Left, jit_expression *Right) {\
    (void)Jit;\
    jit_expression Result = { .Type = EXPR_CONST, };\
    if (Left->Type == EXPR_CONST && Right->Type == EXPR_CONST) {\
        Result.As.Number = Left->As.Number op Right->As.Number;\
    } else {\
        assert(false && "non const expr.");\
    }\
    return Result;\
}\
static jit_expression Expr_ ## name (jit *Jit, jit_expression *Left, jit_expression *Right) 

DEFINE_EXPR_BINARY(Add, +);
DEFINE_EXPR_BINARY(Sub, -);
DEFINE_EXPR_BINARY(Mul, *);
DEFINE_EXPR_BINARY(Div, /);

#undef DEFINE_EXPR_BINARY

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

    /* parse args */
    int ArgCount = 0;
    if (NextToken(Jit).Type != TOK_RPAREN)
    {
        do {
            Jit_ParseExpr(Jit, PREC_EXPR);
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument%s.", 
        ArgCount > 1? "s" : ""
    );

    if (ArgCount != Function->ParamCount)
    {
        Error(Jit, "Expected %d arguments to '%.*s', got %d instead.", 
            Function->ParamCount, FnName->Str.Len, FnName->Str.Ptr, ArgCount
        );
        goto ErrReturn;
    }

    /* call the function */
    jit_expression Result = { .Type = EXPR_CONST, };
    if (Function->Body.Type == EXPR_CONST) /* const function, returns the value */
    {
        Result = Function->Body;
    }
    else /* non-const function, execute the call */
    {
        assert(false && "TODO: non-const call.");
    }
    return Result;
ErrReturn:
    return (jit_expression) { 0 };
}

static jit_expression Expr_Variable(jit *Jit, const jit_token *VarName)
{
    /* consumed VarName */
    /* find the variable */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, VarName->Str.Ptr, VarName->Str.Len, TYPE_VARIABLE);
    if (!Entry)
    {
        Error(Jit, "Undefined variable: '%.*s'.", VarName->Str.Len, VarName->Str.Ptr);
        goto ErrReturn;
    }
    jit_variable *Variable = &Entry->As.Variable;

    /* get the value */
    jit_expression Result = { .Type = EXPR_CONST };
    if (Variable->Expr.Type == EXPR_CONST)
    {
        Result.As.Number = Variable->Expr.As.Number;
    }
    else
    {
        assert(false && "TODO: non-const variable.");
    }
    return Result;
ErrReturn:
    return (jit_expression) { 0 };
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
        jit_expression *Left = Expr_Peek(Jit);
        jit_expression *Result = Left;
        switch (Oper)
        {
        case TOK_PLUS: *Result = Expr_Add(Jit, Left, Right); break;
        case TOK_MINUS: *Result = Expr_Sub(Jit, Left, Right); break;
        case TOK_STAR: *Result = Expr_Mul(Jit, Left, Right); break;
        case TOK_SLASH: *Result = Expr_Div(Jit, Left, Right); break;
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
    }
    return !Error && !Jit->Error;
}




static void Jit_FunctionBeginScope(jit *Jit, jit_function *Function)
{
    assert(Jit->LocalVarCount < Jit->LocalVarCapacity && "TODO: Dynamic capacity.");
    Function->ParamStart = Jit->LocalVarCount;
    Function->ParamCount = 0;
    Jit->ScopeCount++;
}

static void Jit_FunctionEndScope(jit *Jit)
{
    Jit->ScopeCount--;
}

static void Jit_FunctionPushLocal(jit *Jit, jit_function *Function, const jit_token *Parameter)
{
    assert(Jit->LocalVarCount + 1 < Jit->LocalVarCapacity && "TODO: Dynamic capacity.");
    Jit->LocalVars[Jit->LocalVarCount] = (jit_variable) {
        .Str = Parameter->Str,
        .Offset = Function->ParamCount,
    };
    Jit->LocalVarCount++;
    Function->ParamCount++;
}

static void Jit_PushGlobal(jit *Jit, jit_variable *Global)
{
    Global->Offset = Jit->Global.Count - 1;
}



static void FunctionDecl(jit *Jit, jit_token FnName)
{
    /* consumed '(' */
    def_table_entry *Label = DefTable_Define(&Jit->Global, FnName.Str.Ptr, FnName.Str.Len, TYPE_FUNCTION);
    Jit_FunctionBeginScope(Jit, &Label->As.Function);
    {
        /* parameter */
        if (NextToken(Jit).Type == TOK_IDENTIFIER)
        {
            do {
                ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
                jit_token Parameter = CurrToken(Jit); 
                Jit_FunctionPushLocal(Jit, &Label->As.Function, &Parameter);
            } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
        }
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");

        /* equ sign */
        ConsumeOrError(Jit, TOK_EQUAL, "Expected '='.");

        /* function body */
        if (Jit_ParseExpr(Jit, PREC_EXPR))
        {
            Label->As.Function.Body = *Expr_Pop(Jit);
        }
    }
    Jit_FunctionEndScope(Jit);
}

static void VariableDecl(jit *Jit, jit_token Identifier)
{
    /* consumed Identifier */
    def_table_entry *Definition = DefTable_Define(&Jit->Global, Identifier.Str.Ptr, Identifier.Str.Len, TYPE_VARIABLE);
    Jit_PushGlobal(Jit, &Definition->As.Variable);

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '='.");

    /* expr */
    if (Jit_ParseExpr(Jit, PREC_EXPR))
    {
        Definition->As.Variable.Expr = *Expr_Pop(Jit);
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
    while (1)
    {
        /* declaration */
        if (ConsumeIfNextTokenIs(Jit, TOK_IDENTIFIER))
        {
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
        else 
        {
            Jit_ParseExpr(Jit, PREC_EXPR);
            printf("Stack: %d/%d, value = %3.2f\n", Jit->ExprStackSize, Jit->ExprStackCapacity, Jit->ExprStack[0].As.Number);
            goto Done;
        }

        if (!ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
        {
            ConsumeOrError(Jit, TOK_EOF, "Expected new line.");
        }
    }
Done:
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
            .As.Number = Jit->ExprStack[0].As.Number,
        };
    }
#if 0
    if (!JitCompile(Jit))
    {
        return Jit.Result;
    }
    double Value = JitRun(Jit);
    Jit.Result.Valid = true;
    Jit.Result.Number = Value;
    return Jit.Result;
#endif 
}

void Jit_Destroy(jit *Jit)
{
}

