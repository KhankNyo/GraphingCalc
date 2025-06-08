#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "Common.h"
#include "JitData.h"

typedef struct jit_variable jit_variable;
typedef struct jit_function jit_function;
typedef struct jit_debug_info jit_debug_info;
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

    TOK_EOF,
} jit_token_type;

typedef struct jit_token 
{
    strview Str;
    int Offset, Line;
    enum jit_token_type Type;

    union {
        double Number;
        char ErrMsg[256];
    } As;
} jit_token;

struct jit_definition_info 
{
    strview SrcName;
    uint Location;
    uint Length;
};

struct jit_variable
{
    jit_definition_info Definition;
    jit_data Data;
};

struct jit_function 
{
    jit_debug_info Dbg;
    int ParamStart;
    int ParamCount;
    int ReturnReg;
};



#endif /* JIT_COMMON_H */

