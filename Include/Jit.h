#ifndef JIT_H
#define JIT_H

#include "JitCommon.h"
#include "Emitter.h"
#include "DefTable.h"
#include "Storage.h"

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

    int LocalVarCount, LocalVarBase, LocalVarCapacity;
    jit_variable LocalVars[256*4];
    int ScopeCount;
    def_table Global;

    struct jit_token Curr, Next;
    bool8 Error;
    char ErrMsg[256];

    jit_expression ExprStack[128];
    int ExprStackSize, ExprStackCapacity;

    struct {
        union {
            double Double;
            u64 Uint;
        } As;
        jit_expression Location;
    } Sign;

    jit_emitter Emitter;
    jit_storage_manager Storage;
} jit;

jit Jit_Init(void);
jit_result Jit_Evaluate(jit *Jit, const char *Expr);
void Jit_Destroy(jit *Jit);



#endif /* JIT_H */

