#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "Common.h"


typedef struct jit jit;
typedef struct jit_storage_manager jit_storage_manager;
typedef struct jit_emitter jit_emitter;



typedef struct jit_variable
{
    strview Name;
    int InsLocation, InsByteCount;
    i32 GlobalIndex;
} jit_variable;

typedef struct jit_function 
{
    strview Name;
    int Location, InsByteCount;
    int ParamStart;
    int ParamCount;
} jit_function;


/* internal jit functions */
typedef struct jit_scratchpad 
{
    u8 *Ptr;
    int LeftCount, RightCount, Capacity;
} jit_scratchpad;

void *Jit_Scratchpad_LeftPtr(jit_scratchpad *S);
void *Jit_Scratchpad_PushLeft(jit_scratchpad *S, int DataSize);
void *Jit_Scratchpad_PopLeft(jit_scratchpad *S, int DataSize);


/*=======================================
 *              jit_token
 *======================================= */
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

    TOK_LESS, 
    TOK_GREATER, 
    TOK_LESS_EQUAL, 
    TOK_GREATER_EQUAL,

    TOK_EOF,
} jit_token_type;


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

