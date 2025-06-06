#ifndef JIT_STORAGE_H
#define JIT_STORAGE_H


#include "JitCommon.h"
#include "Emitter.h"

typedef struct jit_storage_manager
{
    int DataSize;

    int Scope;
    int MaxStackSize[2];
    int StackSize[2];
    int BusyRegCount[2];
    bool8 RegIsBusy[2][TARGETENV_REG_COUNT];

    void *GlobalMemory;
    uint GlobalCapacity;
    uint GlobalSize;
} jit_storage_manager;

typedef struct storage_spill_data 
{
    uint Count;
    u8 Reg[TARGETENV_REG_COUNT];
    i32 StackOffset[TARGETENV_REG_COUNT];
} storage_spill_data;

/* reset must be called to set data size after init */
jit_storage_manager Storage_Init(void *GlobalMemory, uint GlobalCapacityBytes);
void Storage_Reset(jit_storage_manager *S, int DataSize);
void Storage_PushScope(jit_storage_manager *S);
void Storage_PopScope(jit_storage_manager *S);

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
int Storage_GetMaxStackSize(const jit_storage_manager *S);


#endif /* JIT_STORAGE_H */

