#ifndef JIT_H
#define JIT_H

#include "Platform.h"

typedef struct jit_result
{
    bool8 Valid;
    union {
        char ErrMsg[256];
        double Number;
    } As;
} jit_result;

enum jit_token_type 
{
    TOK_ERR = 0,
    TOK_NUMBER,
    TOK_PLUS,
    TOK_DASH,
    TOK_SLASH,
    TOK_STAR,

    TOK_CARET,
    TOK_PERCENT,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACKET,

    TOK_RBRACKET,
    TOK_EQUAL,
    TOK_IDENTIFIER,
    TOK_EOF,
};

struct jit_token 
{
    enum jit_token_type Type;
    int Offset, Line;
    int StrLen;
    const char *Str;

    union {
        double Number;
        char ErrMsg[256];
    } As;
};

typedef struct jit 
{
    jit_result Result;

    const char *Start, *End;
    int Line;
    int Offset;
    uint SafePeekDist;

    struct jit_token Curr, Next;
} jit;


jit Jit_Init(void);
jit_result Jit_Evaluate(jit *Jit, const char *Expr);
void Jit_Destroy(jit *Jit);

#endif /* JIT_H */


#ifdef JIT_IMPL
#include <stdio.h>
#include <stdarg.h>


typedef struct jit_token token;
typedef enum jit_token_type token_type;

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

static token CreateToken(jit *Jit, token_type Type)
{
    token Tok = {
        .Type = Type,
        .Line = Jit->Line,
        .Offset = Jit->Offset, 
        .Str = Jit->Start,
        .StrLen = Jit->End - Jit->Start,
    };
    Jit->Start = Jit->End;
    return Tok;
}

static token TokenError(jit *Jit, const char *Fmt, ...)
{
    token Tok = CreateToken(Jit, TOK_ERR);
    va_list Arg;
    va_start(Arg, Fmt);
    vsnprintf(Tok.As.ErrMsg, sizeof Tok.As.ErrMsg, Fmt, Arg);
    va_end(Arg);
    return Tok;
}

static token ParseNumber(jit *Jit)
{
    double Number = 0;
    while (IsNumber(Peek(Jit, 0)))
    {
        Number += Advance(Jit) - '0';
        Number *= 10;
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
    token Tok = CreateToken(Jit, TOK_NUMBER);
    Tok.As.Number = Number;
    return Tok;
}

static token ParseIdentifier(jit *Jit)
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
        case '\n':
        {
            Advance(Jit);
            Jit->Line++;
            Jit->Offset = 0;
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

static token Jit_Tokenize(jit *Jit)
{
    ConsumeSpace(Jit);
    char Ch = Advance(Jit);

    if (IsNumber(Ch) 
    || ( Peek(Jit, 0) == '.' && IsNumber(Peek(Jit, 1)) ))
    {
        return ParseNumber(Jit);
    }

    if (IsIdentifier(Ch))
    {
        return ParseIdentifier(Jit);
    }

    switch (Ch)
    {
    case '+': return CreateToken(Jit, TOK_PLUS);
    case '-': return CreateToken(Jit, TOK_DASH);
    case '/': return CreateToken(Jit, TOK_SLASH);
    case '*': return CreateToken(Jit, TOK_STAR);
    case '^': return CreateToken(Jit, TOK_CARET);
    case '%': return CreateToken(Jit, TOK_PERCENT);
    case '(': return CreateToken(Jit, TOK_LPAREN);
    case ')': return CreateToken(Jit, TOK_RPAREN);
    case '[': return CreateToken(Jit, TOK_LBRACKET);
    case ']': return CreateToken(Jit, TOK_RBRACKET);
    case '=': return CreateToken(Jit, TOK_RBRACKET);
    case '\0': return CreateToken(Jit, TOK_EOF);
    default: return TokenError(Jit, "Unknown token '%c'.", Ch);
    }
}



jit Jit_Init(void)
{
    jit Jit = { 0 };
    return Jit;
}

jit_result Jit_Evaluate(jit *Jit, const char *Expr)
{
    Jit->Start = Expr;
    Jit->End = Expr;
    Jit->Line = 1;
    Jit->Offset = 1;
    Jit->Result.Valid = false;
    Jit->Curr = Jit->Next;
    Jit->Next = Jit_Tokenize(Jit);

    int i = 0;
    do {
        Jit->Curr = Jit->Next;
        Jit->Next = Jit_Tokenize(Jit);

        printf("%2d: '%.*s' - %d\n", i, Jit->Curr.StrLen, Jit->Curr.Str, Jit->Curr.Type);
        i++;
    } while (Jit->Curr.Type != TOK_EOF);
    
    return Jit->Result;
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

#endif /* JIT_IMPL */

