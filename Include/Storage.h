#ifndef JIT_STORAGE_H
#define JIT_STORAGE_H


#include "JitCommon.h"
#include "Emitter.h"

typedef struct jit_storage_manager
{
    int MaxStackSize;
    int StackSize;
    int BusyRegCount;
    bool8 RegIsBusy[TARGETENV_REG_COUNT];

    double *GlobalMemory;
    uint GlobalCapacity;
    uint GlobalSize;
} jit_storage_manager;

typedef struct storage_spill_data 
{
    uint Count;
    u8 Reg[TARGETENV_REG_COUNT];
    i32 StackOffset[TARGETENV_REG_COUNT];
} storage_spill_data;

jit_storage_manager Storage_Init(double *GlobalMemory, uint GlobalCapacity);
void Storage_ResetTmpAndStack(jit_storage_manager *S);

/* spill all busy registers */
storage_spill_data Storage_Spill(jit_storage_manager *S);
void Storage_Unspill(jit_storage_manager *S, storage_spill_data *Spill);

jit_expression Storage_ForceAllocateReg(jit_storage_manager *S, uint Reg);
jit_expression Storage_AllocateReg(jit_storage_manager *S);
jit_expression Storage_AllocateStack(jit_storage_manager *S);
jit_expression Storage_AllocateGlobal(jit_storage_manager *S);
jit_expression Storage_AllocateConst(jit_storage_manager *S, double Const);

void Storage_DeallocateReg(jit_storage_manager *S, uint Reg);
int Storage_PushStack(jit_storage_manager *S, int Size); /* returns -StackSize */
void Storage_PopStack(jit_storage_manager *S, int Size);

double Storage_GetConst(const jit_storage_manager *S, i32 GlobalOffset);


#endif /* JIT_STORAGE_H */

