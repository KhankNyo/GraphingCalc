#ifndef JIT_STORAGE_H
#define JIT_STORAGE_H


#include "JitCommon.h"
#include "Emitter.h"


typedef struct jit_storage_manager
{
    int MaxStackSize;
    int StackSize;
    int GlobalSize;
    int StackPtrReg;
    int GlobalPtrReg;
    int ConstCount;
    int BusyRegCount;
    bool8 RegIsBusy[8];
    i32 ConstOffset[256];
    double Consts[256];
} jit_storage_manager;

typedef struct storage_spill_data 
{
    uint Count;
    u8 Reg[8];
    i32 StackOffset[8];
} storage_spill_data;

jit_storage_manager Storage_Init(int StackPtrReg, int GlobalPtrReg);
void Storage_ResetTmpAndStack(jit_storage_manager *S);

/* spill all registers from RegBegin to RegEnd */
storage_spill_data Storage_Spill(jit_storage_manager *S);
void Storage_Unspill(jit_storage_manager *S, storage_spill_data *Spill);

jit_expression Storage_ForceAllocateReg(jit_storage_manager *S, uint Reg);
jit_expression Storage_AllocateReg(jit_storage_manager *S);
jit_expression Storage_AllocateStack(jit_storage_manager *S);
jit_expression Storage_AllocateGlobal(jit_storage_manager *S);
jit_expression Storage_AllocateConst(jit_storage_manager *S, double Const);

void Storage_DeallocateReg(jit_storage_manager *S, uint Reg);
void Storage_PopStack(jit_storage_manager *S, int Size);

double Storage_GetConst(const jit_storage_manager *S, i32 GlobalOffset);


#endif /* JIT_STORAGE_H */

