#ifndef JIT_H
#define JIT_H

#include "Common.h"
#include "JitCommon.h"
#include "DefTable.h"

typedef struct jit_result
{
    bool8 Valid;
    union {
        const char *ErrMsg;
        double Number;
    } As;
} jit_result;

typedef struct jit 
{
    const char *Start, *End;
    int Line;
    int Offset;
    uint SafePeekDist;

    int FunctionScopeCount, FunctionScopeCapacity;
    def_table FunctionScopes[256];
    def_table GlobalScope;
    def_table *Scope;

    struct jit_token Curr, Next;
    bool8 Error;
    char ErrMsg[256];
} jit;

jit Jit_Init(void);
jit_result Jit_Evaluate(jit *Jit, const char *Expr);
void Jit_Destroy(jit *Jit);



#endif /* JIT_H */

