#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "Common.h"

#define JIT_REG_INVALID -1
typedef i8 jit_reg;
typedef struct jit_token jit_token;
typedef struct jit_expression jit_expression;
typedef struct jit_function_parameter jit_function_parameter;
typedef struct jit_variable jit_variable;
typedef struct jit_function jit_function;
typedef struct jit_debug_info jit_debug_info;
typedef struct jit_ir_op jit_ir_op;
typedef struct jit_ir_data jit_ir_data;

typedef enum jit_storage_type 
{
    STORAGE_MEM = 0,
    STORAGE_REG,
} jit_storage_type;

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

typedef struct jit_mem 
{
    i32 Offset;
    jit_reg BaseReg;
} jit_mem;
struct jit_expression
{
    jit_storage_type Storage;
    union {
        jit_mem Mem;
        jit_reg Reg;
    } As;
};

struct jit_debug_info 
{
    strview Str;
    int Location;
    int ByteCount;
};

struct jit_variable
{
    jit_debug_info Dbg;
    jit_expression Expr;
};

struct jit_function 
{
    jit_debug_info Dbg;
    int ParamStart;
    int ParamCount;
};

struct jit_token 
{
    strview Str;
    int Offset, Line;
    enum jit_token_type Type;

    union {
        double Number;
        const char *ErrMsg;
    } As;
};


#endif /* JIT_COMMON_H */

