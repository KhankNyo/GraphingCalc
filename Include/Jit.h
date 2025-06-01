#ifndef JIT_H
#define JIT_H

#include "JitCommon.h"
#include "JitError.h"
#include "Emitter.h"
#include "DefTable.h"
#include "Storage.h"


typedef void (*jit_init)(double *GlobalMemory);
typedef struct jit_graphable jit_graphable;
typedef struct jit_result
{
    const char *ErrMsg;
    double *GlobalData;
    def_table_entry *GlobalSymbol;
} jit_result;

typedef struct jit 
{
    const char *Start, *End;
    int Line;
    int Offset;
    uint SafePeekDist;

    int LocalVarCount, LocalVarBase, LocalVarCapacity;
    jit_variable *LocalVars;
    int ScopeCount;
    def_table Global;

    struct jit_token Curr, Next;

    jit_expression *ExprStack;
    int ExprStackSize, ExprStackCapacity;

    int VarDeclEnd;
    jit_emitter Emitter;
    jit_storage_manager Storage;
    jit_error Error;
} jit;

/* returns 0 on success, otherwise returns minimum scratch pad memory size */
uint Jit_Init(
    jit *Jit,
    void *Scratchpad, uint ScratchpadCapacity, /* used internally when Jit_Compile is called */
    double *GlobalMemory, uint GlobalMemCapacity, /* used to store the compiled machine code */
    void *ProgramMemory, uint ProgramMemCapacity, /* used to store global data referenced by the compiled code */
    def_table_entry *DefTableArray, uint DefTableCapacity /* used to store global symbols */
);
void Jit_Destroy(jit *Jit);

jit_result Jit_Compile(jit *Jit, const char *Expr);

jit_init Jit_GetInit(jit *Jit, const jit_result *Result);
void *Jit_GetFunctionPtr(jit *Jit, const jit_function *Fn);


#endif /* JIT_H */

