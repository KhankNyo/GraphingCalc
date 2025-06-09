#ifndef JIT_H
#define JIT_H

#include "JitCommon.h"
#include "JitError.h"
#include "DefTable.h"
#include "Storage.h"
#include <wchar.h>


typedef enum jit_compilation_flags 
{
    JIT_COMPFLAG_NONE       = 0,
    JIT_COMPFLAG_FLOAT32    = 1 << 0,   /* default is float64 (aka double) */
    JIT_COMPFLAG_SIMD       = 1 << 1,   /* default is scalar code, NOTE: if this flag is specified, GlobalMemory passed into Jit_Init must be aligned to the target's alignment requirement for vector code */
} jit_compilation_flags;

typedef void (*jit_init32)(float *GlobalMemory);
typedef void (*jit_init64)(double *GlobalMemory);
typedef struct jit_result
{
    const char *ErrMsg;
    void *GlobalData;
    def_table_entry *GlobalSymbol;
} jit_result;

typedef struct jit 
{
    jit_compilation_flags Flags;

    const char *Start, *End;
    int Line;
    int Offset;
    uint SafePeekDist;

    bool8 InLocalScope;
    int TmpParamCount, TmpParamCapacity;
    jit_variable *TmpParam;
    def_table Global;
    jit_token Curr, Next;

    jit_ir_op *IrOp;
    uint IrOpCount, IrOpCapacity;
    jit_ir_data *IrData;
    uint IrDataCount, IrDataCapacity;
    jit_expression *IrStack;
    uint IrStackCount, IrStackCapacity;

    int VarDeclEnd;
    jit_emitter Emitter;
    jit_storage_manager Storage;
    jit_error Error;

    u8 *ScratchpadPtr;
    uint ScratchpadSize, ScratchpadCapacity;
    bool8 IsScratchpadReserved;
} jit;

/* returns 0 on success, otherwise returns minimum scratch pad memory size */
uint Jit_Init(
    jit *Jit,
    void *Scratchpad, uint ScratchpadCapacity,      /* used internally when Jit_Compile is called */
    void *GlobalMemory, uint GlobalMemCapacity,     /* used to store global data referenced by the compiled code */
    void *ProgramMemory, uint ProgramMemCapacity,   /* used to store the compiled machine code */
    def_table_entry *DefTableArray, uint DefTableCapacity /* used to store global symbols */
);
void Jit_Destroy(jit *Jit);

jit_result Jit_Compile(jit *Jit, jit_compilation_flags CompFlags, const char *Expr);
jit_init32 Jit_GetInit32(jit *Jit, const jit_result *Result);
jit_init64 Jit_GetInit64(jit *Jit, const jit_result *Result);
/* NOTE: every function compiled has double/float *GlobalMemory as first param */
void *Jit_GetFunctionPtr(jit *Jit, const jit_function *Fn);


#endif /* JIT_H */

