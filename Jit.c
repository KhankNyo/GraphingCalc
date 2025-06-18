#include "JitCommon.h"
#include "JitBackend.h"
#include "Jit.h"
#include "DefTable.h"

#include <stdio.h>
#include <math.h>
#include <stdarg.h>
#include <string.h> /* memset */


typedef enum precedence 
{
    PREC_NONE = 0,
    PREC_EXPR, 
    PREC_COMPARE,
    PREC_PLUSMINUS,
    PREC_MULDIV,
    PREC_POW,
    PREC_UNARY,
} precedence;

typedef struct jit_eval_data jit_eval_data;
static void Jit_ParseExpr(jit *Jit, precedence Prec);


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

void *Jit_Scratchpad_LeftPtr(jit_scratchpad *S)
{
    return S->Ptr + S->LeftCount;
}

static void *Jit_Scratchpad_RightPtr(jit_scratchpad *S)
{
    return S->Ptr + S->Capacity - S->RightCount;
}

void *Jit_Scratchpad_PushLeft(jit_scratchpad *S, int DataSize)
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

void *Jit_Scratchpad_PopLeft(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->LeftCount, "bad data size");
    S->LeftCount -= DataSize;
    return Jit_Scratchpad_LeftPtr(S);
}

static void *Jit_Scratchpad_PopRight(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->RightCount, "bad data size");
    void *Data = Jit_Scratchpad_RightPtr(S);
    S->RightCount -= DataSize;
    return Data;
}




/*========================================================== 
 *                    REFERENCE RECORD
 *==========================================================*/
typedef struct reference 
{
    const char *Str;
    i32 Len;
    i32 Line, Offset;
    i32 RefLocation;
    i32 ArgCount;
} reference;

void RefStack_PushFunctionReference(jit *Jit, const jit_token *Function, i32 ArgCount, i32 RefLocation)
{
    reference *Ptr = Jit_Scratchpad_PushRight(&Jit->FrontendData, sizeof(reference));
    *Ptr = (reference) {
        .Str = Function->Str.Ptr,
        .Len = Function->Str.Len,
        .Line = Function->Line,
        .Offset = Function->Offset,
        .RefLocation = RefLocation,
        .ArgCount = ArgCount,
    };
}

reference *RefStack_Pop(jit *Jit)
{
    return Jit_Scratchpad_PopRight(&Jit->FrontendData, sizeof(reference));
}

bool8 RefStack_IsEmpty(jit *Jit)
{
    return 0 == Jit->FrontendData.RightCount;
}



/*========================================================== 
 *                      EVAL STACK
 *==========================================================*/

typedef enum jit_eval_data_type 
{
    EVAL_CONSTANT,
    EVAL_LOCAL,
    EVAL_GLOBAL,
    EVAL_DYNAMIC,
} jit_eval_data_type;
struct jit_eval_data 
{
    jit_eval_data_type Type;
    union {
        double Const;
        i32 LocalIndex;
        i32 GlobalIndex;
    } As;
};
static jit_eval_data EvalData_Const(double Const)
{
    return (jit_eval_data) {
        .Type = EVAL_CONSTANT,
        .As.Const = Const,
    };
}
static jit_eval_data EvalData_Dynamic(void)
{
    return (jit_eval_data) {
        .Type = EVAL_DYNAMIC,
    };
}
static jit_eval_data EvalData_Local(i32 Index)
{
    return (jit_eval_data) {
        .Type = EVAL_LOCAL,
        .As.LocalIndex = Index,
    };
}
static jit_eval_data EvalData_Global(i32 Index)
{
    return (jit_eval_data) {
        .Type = EVAL_GLOBAL,
        .As.GlobalIndex = Index,
    };
}


static void EvalStack_Push(jit *Jit, jit_eval_data Data)
{
    jit_eval_data *Location = Jit_Scratchpad_PushLeft(&Jit->FrontendData, sizeof(jit_eval_data));
    *Location = Data;
}

static jit_eval_data *EvalStack_Pop(jit *Jit)
{
    jit_eval_data *Location = Jit_Scratchpad_PopLeft(&Jit->FrontendData, sizeof(jit_eval_data));
    return Location;
}

static jit_eval_data *EvalStack_Top(jit *Jit)
{
    ASSERT((uint)Jit->FrontendData.LeftCount >= sizeof(jit_eval_data), "empty stack");
    jit_eval_data *Location = Jit_Scratchpad_LeftPtr(&Jit->FrontendData);
    return Location - 1;
}

/* returns true if data was dynamic */
static void EvalStack_TransferDataToBackend(jit *Jit, jit_eval_data *Data)
{
    switch (Data->Type)
    {
    case EVAL_GLOBAL: /* load the global var */
    {
        Backend_Op_LoadGlobal(&Jit->Backend, Data->As.GlobalIndex);
    } break;
    case EVAL_LOCAL: /* load the local var */
    {
        Backend_Op_LoadLocal(&Jit->Backend, Data->As.LocalIndex);
    } break;
    case EVAL_CONSTANT: /* load the constant */
    {
        i32 GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, Data->As.Const);
        Backend_Op_LoadGlobal(&Jit->Backend, GlobalIndex);
    } break;
    case EVAL_DYNAMIC: /* nothing to load, already on backend's eval stack */
    {
    } break;
    }
}



/*========================================================== 
 *                      LOCAL VAR STACK
 *==========================================================*/


static i32 LocalVarStack_Count(const jit *Jit)
{
    return Jit->BackendData.RightCount / sizeof(strview);
}

static void Jit_StartLocalScope(jit *Jit)
{
    Jit->LocalVarBase = LocalVarStack_Count(Jit);
    Jit->LocalVarEnd = Jit->LocalVarBase;
}

static void LocalVarStack_Push(jit *Jit, const jit_token *Local)
{
    Jit->LocalVarEnd++;
    strview *LocalVar = Jit_Scratchpad_PushRight(&Jit->BackendData, sizeof(strview));
    *LocalVar = Local->Str;
}

static strview *LocalVarStack_Get(jit *Jit, i32 Index)
{
    i32 LocalVarCount = LocalVarStack_Count(Jit);
    ASSERT(IN_RANGE(0, Index, LocalVarCount - 1), "invalid index");
    strview *Top = Jit_Scratchpad_RightPtr(&Jit->BackendData);
    return Top + LocalVarCount - Index - 1;
}

static void Jit_EndLocalScope(jit *Jit)
{
    Jit->LocalVarBase = -1;
}

static bool8 Jit_InLocalScope(const jit *Jit)
{
    return Jit->LocalVarBase != -1;
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




static void Jit_ParseUnary(jit *Jit)
{
    jit_token Token = ConsumeToken(Jit);
    switch (Token.Type)
    {
    case TOK_NUMBER: /* push(number) */
    {
        EvalStack_Push(Jit, 
            EvalData_Const(Token.As.Number)
        );
    } break;
    case TOK_IDENTIFIER: 
    {
        /* function call: push(result of function call) */
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            int ArgCount = 0;
            Backend_Op_ArgStart(&Jit->Backend);
            if (TOK_RPAREN != NextToken(Jit).Type)
            {
                /* compile the arguments and load them on the ir stack */
                do {
                    Jit_ParseExpr(Jit, PREC_EXPR);
                    EvalStack_TransferDataToBackend(Jit, EvalStack_Pop(Jit));
                    ArgCount++;
                } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
            }
            ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument list.");

            /* emit instruction to call fn */
            i32 CallLocation = Backend_Op_Call(&Jit->Backend, ArgCount);
            /* push function result on eval stack */
            EvalStack_Push(Jit, EvalData_Dynamic());

            /* save call location to reference record */
            RefStack_PushFunctionReference(
                Jit, 
                &Token, 
                ArgCount, 
                CallLocation
            );
        }
        /* variable reference */
        else
        {
            /* search in local scope */
            if (Jit_InLocalScope(Jit))
            {
                for (int i = Jit->LocalVarBase; i < Jit->LocalVarEnd; i++)
                {
                    strview *Local = LocalVarStack_Get(Jit, i);
                    if (Local->Len == Token.Str.Len 
                    && StrEqu(Local->Ptr, Token.Str.Ptr, Token.Str.Len))
                    {
                        EvalStack_Push(Jit, 
                            EvalData_Local(i - Jit->LocalVarBase)
                        );
                        return;
                    }
                }
                /* unable to find local variable, switch to global */
            }
            /* not in local scope, switch to global */

            def_table_entry *Entry = DefTable_Find(&Jit->Global, 
                Token.Str.Ptr, Token.Str.Len, TYPE_VARIABLE
            );
            if (!Entry)
            {
                Error_AtToken(&Jit->Error, &Token, "Variable must be defined before use.");
                EvalStack_Push(Jit, EvalData_Global(0)); /* dummy */
            }
            else
            {
                EvalStack_Push(Jit, 
                    EvalData_Global(Entry->As.Variable.GlobalIndex)
                );
            }
        }
    } break;
    case TOK_PLUS:  /* positive sign (nop) */
    {
        Jit_ParseUnary(Jit);
    } break;
    case TOK_MINUS: /* negate */
    {
        Jit_ParseUnary(Jit);
        jit_eval_data *Top = EvalStack_Top(Jit);
        switch (Top->Type)
        {
        case EVAL_LOCAL:
        {
            Backend_Op_LoadLocal(&Jit->Backend, Top->As.LocalIndex);
            Backend_Op_Neg(&Jit->Backend);
        } break;
        case EVAL_GLOBAL:
        {
            Backend_Op_LoadGlobal(&Jit->Backend, Top->As.GlobalIndex);
            Backend_Op_Neg(&Jit->Backend);
        } break;
        case EVAL_DYNAMIC:
        {
            Backend_Op_Neg(&Jit->Backend);
        } break;
        case EVAL_CONSTANT:
        {
            Top->As.Const = -Top->As.Const;
        } break;
        }
    } break; 
    case TOK_LPAREN: /* push('(expr)') */
    {
        Jit_ParseExpr(Jit, PREC_EXPR);
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after expression.");
    } break;
    default:
    {
        Error_AtToken(&Jit->Error, &Token, "Expected an expression.");
        EvalStack_Push(Jit, EvalData_Global(0)); /* dummy */
    } break;
    }
}

static precedence PrecedenceOf(jit_token_type Operator)
{
    switch (Operator)
    {
    case TOK_LESS:
    case TOK_LESS_EQUAL:
    case TOK_GREATER:
    case TOK_GREATER_EQUAL:
        return PREC_COMPARE;
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


/* returns the stack's top */
static void Jit_ParseExpr(jit *Jit, precedence Prec)
{
    Jit_ParseUnary(Jit);
    while (PrecedenceOf(NextToken(Jit).Type) >= Prec)
    {
        jit_token_type Oper = ConsumeToken(Jit).Type;
        Jit_ParseExpr(Jit, PrecedenceOf(Oper) + 1);

        jit_eval_data *Right = EvalStack_Pop(Jit);
        jit_eval_data *Left = EvalStack_Pop(Jit);
        jit_eval_data ResultData;

        if (EVAL_CONSTANT == Right->Type
        && EVAL_CONSTANT == Left->Type) /* constant expr, evaluate now */
        {
            double Result = 0;
            switch (Oper)
            {
            case TOK_PLUS:          Result = Left->As.Const + Right->As.Const; break;
            case TOK_MINUS:         Result = Left->As.Const - Right->As.Const; break;
            case TOK_STAR:          Result = Left->As.Const * Right->As.Const; break;
            case TOK_SLASH:         Result = Left->As.Const / Right->As.Const; break;
            case TOK_LESS:          Result = Left->As.Const < Right->As.Const; break;
            case TOK_LESS_EQUAL:    Result = Left->As.Const <= Right->As.Const; break;
            case TOK_GREATER:       Result = Left->As.Const > Right->As.Const; break;
            case TOK_GREATER_EQUAL: Result = Left->As.Const >= Right->As.Const; break;
            default:
            {
                UNREACHABLE();
            } break;
            }
            ResultData = EvalData_Const(Result);
        }
        else /* runtime expr */
        {
            EvalStack_TransferDataToBackend(Jit, Left);
            EvalStack_TransferDataToBackend(Jit, Right);
            if (Left->Type != EVAL_DYNAMIC 
            && Right->Type == EVAL_DYNAMIC) /* right data was on stack before left data */
            {
                Backend_Op_Swap(&Jit->Backend);
            }

            switch (Oper)
            {
            case TOK_PLUS:          Backend_Op_Add(&Jit->Backend); break;
            case TOK_MINUS:         Backend_Op_Sub(&Jit->Backend); break;
            case TOK_STAR:          Backend_Op_Mul(&Jit->Backend); break;
            case TOK_SLASH:         Backend_Op_Div(&Jit->Backend); break;
            case TOK_LESS:          Backend_Op_Less(&Jit->Backend); break;
            case TOK_GREATER:       Backend_Op_Greater(&Jit->Backend); break;
            case TOK_LESS_EQUAL:    Backend_Op_LessOrEqual(&Jit->Backend); break;
            case TOK_GREATER_EQUAL: Backend_Op_GreaterOrEqual(&Jit->Backend); break;
            default:
            {
                UNREACHABLE();
            } break;
            }

            ResultData = EvalData_Dynamic();
        }

        EvalStack_Push(Jit, ResultData);
        Left = EvalStack_Top(Jit);
    }
}






static jit_function *Jit_DefineFunction(jit *Jit, const char *Name, int NameLen)
{
    def_table_entry *Label = DefTable_Define(&Jit->Global, Name, NameLen, TYPE_FUNCTION);
    jit_function *Function = &Label->As.Function;

    Function->Location = INT32_MAX;
    Function->ParamStart = 0;
    Function->ParamCount = 0;
    return Function;
}


static void Jit_Function_Decl(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */
    jit_function *Function = Jit_DefineFunction(Jit, FnName->Str.Ptr, FnName->Str.Len);
    Jit_StartLocalScope(Jit);

    /* function params */
    Function->ParamCount = 0;
    Function->ParamStart = Jit->LocalVarBase;
    if (TOK_RPAREN != NextToken(Jit).Type)
    {
        do {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
            jit_token Parameter = CurrToken(Jit); 
            LocalVarStack_Push(Jit, &Parameter);
            Function->ParamCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    /* because IrData is a downward-growing stack, Param starting point is the latest param */
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after function declaration.");


    /* compile the block */
    Function->Location = Backend_Op_FnEntry(&Jit->Backend, Function->ParamCount);
    {
        /* function body */
        Jit_ParseExpr(Jit, PREC_EXPR);
        EvalStack_TransferDataToBackend(Jit, EvalStack_Pop(Jit));
    }
    i32 FnEnd = Backend_Op_FnReturn(&Jit->Backend, Function->Location, true);
    Function->InsByteCount = FnEnd - Function->Location;

    Jit_EndLocalScope(Jit);
}

static void Jit_Variable_Decl(jit *Jit, const jit_token *VarName)
{
    /* consumed VarName */
    def_table_entry *Entry = DefTable_Define(&Jit->Global, VarName->Str.Ptr, VarName->Str.Len, TYPE_VARIABLE);
    jit_variable *Variable = &Entry->As.Variable;

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after variable name.");
    
    /* compile the expression */
    bool8 ConstExpr = false;
    i32 Location = Backend_GetProgramSize(&Jit->Backend);
    Jit_ParseExpr(Jit, PREC_EXPR);
    jit_eval_data *Result = EvalStack_Pop(Jit);
    switch (Result->Type)
    {
    case EVAL_LOCAL: /* impossible to store a local variable to a global variable */
    {
        UNREACHABLE();
    } break;
    case EVAL_DYNAMIC: /* dynamic expression, pop runtime stack and load to global */
    {
        Variable->GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, 0.0);
        Backend_Op_StoreGlobal(&Jit->Backend, Variable->GlobalIndex);
    } break;
    case EVAL_CONSTANT: /* constant expression, no instructions needed */
    {
        Variable->GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, Result->As.Const);
        ConstExpr = true;
    } break;
    case EVAL_GLOBAL: /* aliasing a global variable */
    {
        Variable->GlobalIndex = Result->As.GlobalIndex;
        ConstExpr = true;
    } break;
    }

    if (!ConstExpr)
    {
        Variable->InsLocation = Location;
        Variable->InsByteCount = Backend_GetProgramSize(&Jit->Backend) - Location;
    }
}

static u32 Jit_Hash(const char *Str, int StrLen)
{
    if (0 == StrLen)
        return 0;
    return (u32)(Str[0] & 0x7F) << 8 | Str[StrLen - 1];
}


static void Jit_Reset(jit *Jit, const char *Src, jit_compilation_flags Flags)
{
    Jit->Flags = Flags;
    Jit->Start = Src; 
    Jit->End = Src;
    Jit->Line = 1;
    Jit->Offset = 1;

#if 0
    Jit_Scratchpad_Reset(&Jit->S, Jit->S.Ptr, Jit->S.Capacity);

    /* ir op array starts from the left and grows rightward in the scratchpad */
    Jit->IrOpByteCount = 0;
    Jit->IrOp = Jit_Scratchpad_LeftPtr(&Jit->S);
    /* ir data array starts from the right and grows leftward in the scratchpad */
    Jit->IrData.SrcBegin = Src;
    Jit->IrData.S = &Jit->S;
    Jit->IrData.ByteCount = 0;
    Jit->IrData.Ptr = Jit_Scratchpad_RightPtr(&Jit->S);
#else
    Jit_Scratchpad_Reset(&Jit->FrontendData, Jit->FrontendData.Ptr, Jit->FrontendData.Capacity);
    Jit_Scratchpad_Reset(&Jit->BackendData, Jit->BackendData.Ptr, Jit->BackendData.Capacity);
#endif

    /* pump the tokenizer */
    Jit->Next = (jit_token) { .Type = TOK_EOF };
    ConsumeToken(Jit);

    /* reset other members */
    Error_Reset(&Jit->Error);
    DefTable_Reset(&Jit->Global);

    bool8 EmitFloat32Instructions = 0 != (Flags & JIT_COMPFLAG_FLOAT32);
    int FltSize = EmitFloat32Instructions? sizeof(float) : sizeof(double);
    Backend_Reset(&Jit->Backend, FltSize);
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
        .Global = DefTable_Init(DefTableArray, DefTableCapacity, Jit_Hash),
    };
    u8 *ScratchpadPtr = Scratchpad;
    Jit_Scratchpad_Reset(&Jit->FrontendData, ScratchpadPtr, ScratchpadCapacity/2);
    Jit_Scratchpad_Reset(&Jit->BackendData, ScratchpadPtr + ScratchpadCapacity/2, ScratchpadCapacity/2);
    Backend_Init(
        &Jit->Backend, 
        &Jit->BackendData,
        ProgramMemory, ProgramMemCapacity,
        GlobalMemory, GlobalMemCapacity
    );
    return 0;
}

void Jit_Destroy(jit *Jit)
{
    *Jit = (jit) { 0 };
}


jit_result Jit_Compile(jit *Jit, jit_compilation_flags Flags, const char *Src)
{
    Jit_Reset(Jit, Src, Flags);

    /* compilation stage */
    /* newlines */
    while (ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
    {}

    bool8 VarDeclared = false;
    bool8 FnDeclared = false;
    jit_function *Init = Jit_DefineFunction(Jit, "#init", 5);
    do {
        /* definition */
        ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected a variable or function declaration.");
        jit_token Identifier = CurrToken(Jit);
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            if (!FnDeclared)
            {
                if (!VarDeclared)
                {
                    Init->Location = Backend_Op_FnEntry(&Jit->Backend, 0);
                }
                Init->InsByteCount = Backend_Op_FnReturn(&Jit->Backend, Init->Location, false) - Init->Location;
                FnDeclared = true;
            }
            Jit_Function_Decl(Jit, &Identifier);
        }
        else 
        {
            if (FnDeclared)
            {
                Error_AtToken(&Jit->Error, &Identifier, "Variable declaration cannot be after function declaration.");
                break;
            }
            if (!VarDeclared)
            {
                Init->Location = Backend_Op_FnEntry(&Jit->Backend, 0);
                VarDeclared = true;
            }
            Jit_Variable_Decl(Jit, &Identifier);
        }

        /* skip all newlines */
        while (!Jit->Error.Available && ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
        {}
    } while (!Jit->Error.Available && TOK_EOF != NextToken(Jit).Type);
    if (Jit->Error.Available)
        goto ErrReturn;


    /* ref-patching stage */
    while (!RefStack_IsEmpty(Jit))
    {
        reference *Ref = RefStack_Pop(Jit);
        def_table_entry *Entry = DefTable_Find(
            &Jit->Global, 
            Ref->Str, 
            Ref->Len, 
            TYPE_FUNCTION
        );
        if (NULL == Entry)
        {
            Error_AtStr(
                &Jit->Error, 
                Ref->Str, 
                Ref->Len, 
                Ref->Line, 
                Ref->Offset, 
                "Undefined reference to function."
            );
            break;
        }
        int ParamCount = Entry->As.Function.ParamCount;
        if (ParamCount != Ref->ArgCount)
        {
            Error_AtStr(
                &Jit->Error, 
                Ref->Str, 
                Ref->Len, 
                Ref->Line, 
                Ref->Offset, 
                "Expected %d argument%s but got %d instead.", 
                ParamCount, ParamCount > 0? "s" : "", Ref->ArgCount
            );
            break;
        }
        Backend_Patch_Call(&Jit->Backend, Ref->RefLocation, Entry->As.Function.Location);
    }

    if (Jit->Error.Available)
        goto ErrReturn;

    /* done */
    Backend_Disassemble(&Jit->Backend, &Jit->Global);
    printf("free: %d\n", Jit_Scratchpad_BytesRemain(&Jit->FrontendData) + Jit_Scratchpad_BytesRemain(&Jit->BackendData));
    return (jit_result) {
        .GlobalData = Backend_GetDataPtr(&Jit->Backend),
        .GlobalSymbol = Jit->Global.Head->Next,
    };
ErrReturn:
    return (jit_result) {
        .ErrMsg = Jit->Error.Msg,
    };
}


jit_init32 Jit_GetInit32(jit *Jit, const jit_result *Result)
{
    def_table_entry *Init = Jit->Global.Head;
    ASSERT(Init, "nullptr");
    ASSERT(Init->Type == TYPE_FUNCTION, "unreachable");
    ASSERT(StrEqu(Init->As.Str.Ptr, "#init", 5), "unreachable");
    return (jit_init32)Jit_GetFunctionPtr(Jit, &Init->As.Function);
}

jit_init64 Jit_GetInit64(jit *Jit, const jit_result *Result)
{
    def_table_entry *Init = Jit->Global.Tail;
    ASSERT(Init, "nullptr");
    ASSERT(Init->Type == TYPE_FUNCTION, "unreachable");
    ASSERT(StrEqu(Init->As.Str.Ptr, "#init", 5), "unreachable");
    return (jit_init64)Jit_GetFunctionPtr(Jit, &Init->As.Function);
}

const void *Jit_GetFunctionPtr(jit *Jit, const jit_function *Fn)
{
    ASSERT(Fn->Location < Backend_GetProgramSize(&Jit->Backend), "Function with invalid location");
    ASSERT(Fn->Location + Fn->InsByteCount <= Backend_GetProgramSize(&Jit->Backend), "Function with invalid size");
    const u8 *FnPtr = Backend_GetProgramPtr(&Jit->Backend) + Fn->Location;
    return FnPtr;
}

