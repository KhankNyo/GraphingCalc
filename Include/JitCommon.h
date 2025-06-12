#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "Common.h"

#define JIT_REG_INVALID -1
typedef i8 jit_reg;


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

typedef enum jit_storage_type 
{
    STORAGE_MEM = 0,
    STORAGE_REG,
} jit_storage_type;

typedef struct jit_mem 
{
    i32 Offset;
    jit_reg BaseReg;
} jit_mem;
typedef struct jit_location
{
    jit_storage_type Storage;
    union {
        jit_mem Mem;
        jit_reg Reg;
    } As;
} jit_location;

typedef struct jit_variable
{
    strview Name;
    int InsLocation, InsByteCount;
    jit_location Location;
} jit_variable;

typedef struct jit_function 
{
    strview Name;
    int Location, InsByteCount;
    int ParamStart;
    int ParamCount;
} jit_function;

typedef struct jit_token 
{
    strview Str;
    int Offset, Line;
    enum jit_token_type Type;

    union {
        double Number;
        const char *ErrMsg;
    } As;
} jit_token;


#endif /* JIT_COMMON_H */

