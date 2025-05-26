#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "Common.h"

typedef struct jit_token jit_token;
typedef struct jit_expression jit_expression;
typedef struct jit_function_parameter jit_function_parameter;
typedef struct jit_variable jit_variable;
typedef struct jit_function jit_function;

typedef enum jit_expression_type 
{
    EXPR_MEM = 0,
    EXPR_CONST,
} jit_expression_type;

typedef enum jit_token_type 
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

    TOK_DEF,
    TOK_EOF,
} jit_token_type;

struct jit_expression
{
    jit_expression_type Type;
    union {
        double Number;
        struct {
            int MemOffset;
        };
    } As;
};

struct jit_variable
{
    strview Str;
    int Offset;
    jit_expression Expr;
};

struct jit_function 
{
    strview Str;
    int ParamStart;
    int ParamCount;
    jit_expression Result;
};

struct jit_token 
{
    strview Str;
    int Offset, Line;
    enum jit_token_type Type;

    union {
        double Number;
        char ErrMsg[256];
    } As;
};


#endif /* JIT_COMMON_H */

