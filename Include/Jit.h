#ifndef JIT_H
#define JIT_H

#include "JitCommon.h"
#include "JitError.h"
#include "Emitter.h"
#include "DefTable.h"
#include "Storage.h"


typedef struct jit_result
{
    const char *ErrMsg;
    void *Code;
    uint CodeSize;
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

    jit_expression ExprStack[128];
    int ExprStackSize, ExprStackCapacity;

    jit_emitter Emitter;
    jit_storage_manager Storage;
    jit_error Error;
} jit;

jit Jit_Init(void);
void Jit_Destroy(jit *Jit);

jit_result Jit_Compile(jit *Jit, const char *Expr);
double Jit_Execute(jit *Jit, jit_result *Code, double Param);



#endif /* JIT_H */

