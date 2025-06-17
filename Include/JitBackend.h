#ifndef JIT_BACKEND_H
#define JIT_BACKEND_H

#include "JitCommon.h"
#include "DefTable.h"

/* static assertion */
void TCC_STATIC_ASSERT_FN__(void)
{
    STATIC_ASSERT(sizeof(float) == 4, "float must be 32 bit");
    STATIC_ASSERT(sizeof(double) == 8, "double must be 64 bit"); /* TODO: some compilers might use long double for 64 bit float */
}


/* JIT_BACKEND_<platform name> must be defined via compilation flags */
/* every backend must define their jit_backend struct, 
 * it must NOT be opaque, but its field won't be accessed by the jit front end. */
#if defined(JIT_BACKEND_X64_WINDOWS)
#   include "JitBackend_x64_Windows.h"
#else 
#   error("Unsupported backend.")
#endif


void Backend_Init(
    jit_backend *Backend, 
    u8 *ProgramBuffer, i32 ProgramBufferSizeByte,
    void *GlobalDataBuffer, i32 GlobalDataSizeByte
);
void Backend_Reset(jit_backend *Backend, int DataSize);

/* get and set */
i32 Backend_AllocateGlobal(jit_backend *Backend, double InitialValue);
i32 Backend_GetProgramSize(const jit_backend *Backend);
const u8 *Backend_GetProgramPtr(const jit_backend *Backend);
void *Backend_GetDataPtr(jit_backend *Backend);

const u8 *Backend_TranslateBlock(
    jit_backend *Backend, 
    const u8 *IrBlock, /* a block starts with IR_OP_*_BEGIN, and ends with IR_OP_*_END */
    jit_ir_stack *Stack,
    jit_fnref_stack *FnRef,
    jit *Jit
);
uint Backend_EmitFunctionEntry(jit_backend *Backend);
void Backend_EmitFunctionExit(jit_backend *Backend, uint EntryLocation);
void Backend_PatchCall(jit_backend *Backend, uint CallLocation, uint FunctionLocation);

/* to stdout */
void Backend_Disassemble(const jit_backend *Backend, const def_table *Global);


#endif /* JIT_BACKEND_H */

