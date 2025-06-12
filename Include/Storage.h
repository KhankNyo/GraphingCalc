#ifndef JIT_STORAGE_H
#define JIT_STORAGE_H


#include "TargetEnv.h"


typedef struct jit_storage_manager
{
    int DataSize;

    int MaxStackSize;
    int StackSize;
    int BusyRegCount;
    bool8 RegIsBusy[TARGETENV_REG_COUNT];

    void *GlobalMemory;
    uint GlobalCapacity;
    uint GlobalSize;
} jit_storage_manager;

/* reset must be called to set data size after init */
jit_storage_manager Storage_Init(void *GlobalMemory, uint GlobalCapacityBytes);
void Storage_Reset(jit_storage_manager *S, int DataSize);

void Storage_ForceAllocateReg(jit_storage_manager *S, jit_reg Reg);
/* returns JIT_REG_INVALID if a register could not be allocated */
jit_reg Storage_TryAllocateReg(jit_storage_manager *S);
jit_mem Storage_AllocateStack(jit_storage_manager *S);
jit_mem Storage_AllocateGlobal(jit_storage_manager *S);
jit_mem Storage_AllocateConst(jit_storage_manager *S, double Const);

void Storage_DeallocateReg(jit_storage_manager *S, uint Reg);
int Storage_PushStack(jit_storage_manager *S, int Size); /* returns -StackSize */
void Storage_PopStack(jit_storage_manager *S, int Size);

double Storage_GetConst(const jit_storage_manager *S, i32 GlobalOffset);
int Storage_GetMaxStackSize(const jit_storage_manager *S);


#endif /* JIT_STORAGE_H */

