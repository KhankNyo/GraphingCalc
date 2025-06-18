#ifndef JIT_BACKEND_X64_WINDOWS_H
#define JIT_BACKEND_X64_WINDOWS_H


#include "JitCommon.h"

/* TODO: SSE only supports 8 regs, but AVX can do 16 */
#define REG_COUNT 8


typedef struct jit_backend_x64_windows
{
    /* eval stack */
    jit_scratchpad *EvalStack;

    /* program memory */
    u8 *Program; 
    uint ProgramSize; 
    uint ProgramCapacity;

    /* data memory */
    void *GlobalData;
    uint GlobalDataSize;
    uint GlobalDataCapacity;
    int DataSize;

    /* reg management */
    i32 BusyReg[REG_COUNT];
    uint BusyRegCount;
    /* stack management */
    int StackSize;
    int MaxStackSize;
    /* global management */
    i32 One;

    u8 StoreSingle[3];
    u8 LoadSingle[3];
    u8 FloatOpcode;
} jit_backend;



#endif /* JIT_BACKEND_X64_WINDOWS_H */

