#ifndef JIT_H
#define JIT_H

#include "Common.h"

typedef struct jit_result
{
    bool8 Valid;
    union {
        const char *ErrMsg;
        double Number;
    } As;
} jit_result;

enum jit_token_type 
{
    TOK_ERR = 0,
    TOK_NUMBER,
    TOK_PLUS,
    TOK_MINUS,
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
    TOK_NEWLINE,
    TOK_COMMA, 

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
    const char *Start, *End;
    int Line;
    int Offset;
    uint SafePeekDist;

    struct jit_token Curr, Next;
    bool8 Error;
    char ErrMsg[256];
} jit;


jit Jit_Init(void);
jit_result Jit_Evaluate(jit *Jit, const char *Expr);
void Jit_Destroy(jit *Jit);

#endif /* JIT_H */

