#ifndef JIT_COMMON_H
#define JIT_COMMON_H

typedef struct jit_expression
{
} jit_expression;

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

#endif /* JIT_COMMON_H */

