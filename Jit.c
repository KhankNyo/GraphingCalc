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
    Jit->ExprStackSize = 0;
    Jit->VarDeclEnd = -1;
    ConsumeToken(Jit);

    Error_Reset(&Jit->Error);
    DefTable_Reset(&Jit->Global);

    bool8 EmitFloat32Instructions = 0 != (Flags & JIT_COMPFLAG_FLOAT32);
    int FltSize = EmitFloat32Instructions? sizeof(float) : sizeof(double);
    Storage_Reset(&Jit->Storage, FltSize);
    Emitter_Reset(&Jit->Emitter, EmitFloat32Instructions);
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


static jit_expression *Expr_Peek(jit *Jit)
{
    ASSERT(Jit->ExprStackSize > 0, "Expr_Peek");
    return &Jit->ExprStack[Jit->ExprStackSize - 1];
}

static jit_expression *Expr_Pop(jit *Jit)
{
    ASSERT(Jit->ExprStackSize > 0, "Expr_Pop");
    return &Jit->ExprStack[--Jit->ExprStackSize];
}

static void Expr_Push(jit *Jit, const jit_expression *E)
{
    ASSERT(Jit->ExprStackSize < Jit->ExprStackCapacity, "Bad expr stack size");
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
        jit_expression Result = Storage_AllocateReg(&Jit->Storage);

        Emit_LoadZero(&Jit->Emitter, Result.As.Reg);
        Emit_Sub(&Jit->Emitter, Result.As.Reg, E->As.Mem.BaseReg, E->As.Mem.Offset);

        *E = Result;
    } break;
    case STORAGE_REG:
    {
        jit_expression Result = Storage_AllocateReg(&Jit->Storage);

        Emit_LoadZero(&Jit->Emitter, Result.As.Reg);
        Emit_SubReg(&Jit->Emitter, Result.As.Reg, E->As.Reg);

        Storage_DeallocateReg(&Jit->Storage, E->As.Reg);
    } break;
    }
}
 




static void Jit_EmitLoadConst(jit *Jit, int Reg, double Const)
{
    if (0.0 == Const || -0.0 == Const)
    {
        Emit_LoadZero(&Jit->Emitter, Reg);
    }
    else
    {
        jit_expression Storage = Storage_AllocateConst(&Jit->Storage, Const);
        Emit_Load(&Jit->Emitter, Reg, Storage.As.Mem.BaseReg, Storage.As.Mem.Offset);
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
        Jit_EmitLoadConst(Jit, Reg, Expr->As.Const);
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
    /* Consumed '(' */
    int ArgCount = 0;
    int ArgStackSize = TargetEnv_GetArgStackSize(Fn->ParamCount, Jit->Storage.DataSize);
    int ArgStackTop = Storage_PushStack(&Jit->Storage, ArgStackSize);

    Error_PushMarker(&Jit->Error, &Jit->Curr);
    if (NextToken(Jit).Type != TOK_RPAREN)
    {
        do {
            if (!Jit_ParseExpr(Jit, PREC_EXPR))
                break;

            jit_expression *ArgExpr = Expr_Pop(Jit);
            if (ArgCount < TargetEnv_GetArgRegCount()) /* register argument */
            {
                int ArgReg = TargetEnv_GetArgReg(ArgCount);
                switch (ArgExpr->Storage)
                {
                case STORAGE_CONST:
                {
                    Jit_EmitLoadConst(Jit, ArgReg, ArgExpr->As.Const);
                } break;
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
                int ArgOffset = TargetEnv_GetArgOffset(ArgStackTop, ArgCount, Jit->Storage.DataSize);
                int ArgBaseReg = TargetEnv_GetArgBaseReg();
                int Tmp = Jit_CopyToReg(Jit, Storage_AllocateReg(&Jit->Storage).As.Reg, ArgExpr).As.Reg;
                Emit_Store(&Jit->Emitter, Tmp, ArgBaseReg, ArgOffset);
                Storage_DeallocateReg(&Jit->Storage, Tmp);
            }
            ArgCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument%s.", 
        ArgCount > 1? "s" : ""
    );

    /* check arg count */
    if (ArgCount != Fn->ParamCount)
    {
        Error_WithMarker(&Jit->Error, &Jit->Curr, 
            "Expected %d argument%s but got %d instead.", 
            Fn->ParamCount, Fn->ParamCount > 1? "s" : "", ArgCount
        );
    }
    Storage_PopStack(&Jit->Storage, ArgStackSize);
    Error_PopMarker(&Jit->Error);
    return !Jit->Error.Available;
}

static jit_expression Expr_Call(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */

    /* find the function */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, FnName->Str.Ptr, FnName->Str.Len, TYPE_FUNCTION);
    if (!Entry)
    {
        Error_AtToken(&Jit->Error, FnName, "Call to undefined function.");
        goto ErrReturn;
    }
    jit_function *Function = &Entry->As.Function;


    /* spill registers that are in use */
    storage_spill_data Spilled = Storage_Spill(&Jit->Storage); 
    for (uint i = 0; i < Spilled.Count; i++)
    {
        Emit_Store(&Jit->Emitter, Spilled.Reg[i], TargetEnv_GetStackFrameReg(), Spilled.StackOffset[i]);
    }

    /* parse args and emit call */
    if (!Expr_ParseArgs(Jit, Function))
        goto ErrReturn;
    Emit_Call(&Jit->Emitter, Function->Dbg.Location);
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
        Emit_Load(&Jit->Emitter, Spilled.Reg[i], TargetEnv_GetStackFrameReg(), Spilled.StackOffset[i]);
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
        Error_AtToken(&Jit->Error, VarName, "Undefined variable");
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
            return !Jit->Error.Available;

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
        Error_AtToken(&Jit->Error, &Left, "Expected an expression.");
    } break;
    }

    return !Jit->Error.Available;
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
            TODO("pow(x, y)");
        } break;
        default: 
        {
            UNREACHABLE();
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
            Storage_PopStack(&Jit->Storage, Jit->Storage.DataSize);
        } break;
        case STORAGE_CONST:
        {
        } break;
        }

    }
    return !Error && !Jit->Error.Available;
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


static void FunctionDecl(jit *Jit, jit_token FnName)
{
    /* consumed '(' */
    jit_function *Function = DefineFunction(Jit, FnName.Str.Ptr, FnName.Str.Len);
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
        Function->Dbg.Location = Emit_FunctionEntry(&Jit->Emitter, Jit->LocalVars + Jit->LocalVarBase, Function->ParamCount);

        /* function body */
        if (Jit_ParseExpr(Jit, PREC_EXPR))
        {
            jit_expression Result = *Expr_Pop(Jit);

            int ReturnReg = TargetEnv_GetReturnReg();
            switch (Result.Storage)
            {
            case STORAGE_CONST:
            {
                Jit_EmitLoadConst(Jit, ReturnReg, Result.As.Const);
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
        Function->Dbg.ByteCount = Jit_GetEmitterBufferSize(Jit) - Function->Dbg.Location;
        Emit_PatchStackSize(&Jit->Emitter, Function->Dbg.Location, Storage_GetMaxStackSize(&Jit->Storage));
    }
    Jit_FunctionEndScope(Jit);
}


static void Jit_PatchJump(jit *Jit, uint Dst)
{
    if (-1 == Jit->VarDeclEnd)
        return;
    Emitter_PatchJump(&Jit->Emitter, Jit->VarDeclEnd, Dst);
}

static void VariableDecl(jit *Jit, jit_token Identifier)
{
    /* consumed Identifier */
    def_table_entry *Definition = DefTable_Define(&Jit->Global, Identifier.Str.Ptr, Identifier.Str.Len, TYPE_VARIABLE);

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '='.");

    /* parse expr */
    Definition->As.Common.Location = Jit_GetEmitterBufferSize(Jit);
    uint JumpDst = Jit_GetEmitterBufferSize(Jit);
    if (Jit_ParseExpr(Jit, PREC_EXPR))
    {
        Definition->As.Variable.Expr = *Expr_Pop(Jit);
        jit_expression *Var = &Definition->As.Variable.Expr;
        bool8 ShouldPatchJump = STORAGE_CONST != Var->Storage;
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

        if (ShouldPatchJump)
        {
            Jit_PatchJump(Jit, JumpDst);
            Jit->VarDeclEnd = Emit_Jump(&Jit->Emitter);
            Definition->As.Common.ByteCount = Jit->VarDeclEnd - Definition->As.Common.Location;
        }
    }
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
    for (uint i = 0; i < Jit->Storage.GlobalSize; i++)
    {
        printf("Global[%d] = %g\n", i, Storage_GetConst(&Jit->Storage, i*8));
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

        .ExprStack = ExprStack,
        .ExprStackCapacity = ExprStackCapacity,
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
    Jit_PatchJump(Jit, Jit_GetEmitterBufferSize(Jit));
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

