
#include <string.h> /* memset */
#include "Emitter.h"
#include "Storage.h"



static int Storage_TryAllocateReg(jit_storage_manager *S)
{
    uint RegCount = TARGETENV_REG_COUNT;
    for (uint i = 0; i < RegCount; i++)
    {
        if (!S->RegIsBusy[i])
        {
            S->RegIsBusy[i] = true;
            S->BusyRegCount++;
            return i;
        }
    }
    return -1;
}

int Storage_PushStack(jit_storage_manager *S, int Size)
{
    S->StackSize += Size;
    S->MaxStackSize = MAX(S->StackSize, S->MaxStackSize);
    S->MaxStackSize = TargetEnv_AlignStackSize(S->MaxStackSize);
    return -S->StackSize;
}

void Storage_PopStack(jit_storage_manager *S, int Size)
{
    S->StackSize -= Size;
}



jit_storage_manager Storage_Init(double *GlobalMemory, uint GlobalMemCapacity)
{
    jit_storage_manager S = {
        .GlobalMemory = GlobalMemory,
        .GlobalCapacity = GlobalMemCapacity,
    };
    Storage_ResetTmpAndStack(&S);
    return S;
}

void Storage_ResetTmpAndStack(jit_storage_manager *S)
{
    S->StackSize = 32; /* shadow space */
    S->MaxStackSize = S->StackSize;
    S->BusyRegCount = 0;
    memset(S->RegIsBusy, 0, TARGETENV_REG_COUNT);
}

void Storage_Reset(jit_storage_manager *S)
{
    Storage_ResetTmpAndStack(S);
    S->GlobalSize = 0;
}


storage_spill_data Storage_Spill(jit_storage_manager *S)
{
    storage_spill_data Spill = { 0 };

    for (uint i = 0; i < TARGETENV_REG_COUNT && S->BusyRegCount; i++)
    {
        if (S->RegIsBusy[i])
        {
            int RegToSpill = TargetEnv_GetArgReg(i);
            i32 StackOffset = Storage_PushStack(S, sizeof(double));

            /* spill it to stack memory */
            int Count = Spill.Count;
            Spill.Reg[Count] = RegToSpill;
            Spill.StackOffset[Count] = StackOffset;
            Spill.Count++;
            Storage_DeallocateReg(S, RegToSpill);
        }
    }
    return Spill;
}


void Storage_Unspill(jit_storage_manager *S, storage_spill_data *Spill)
{
    for (uint i = 0; i < Spill->Count; i++)
    {
        Storage_ForceAllocateReg(S, Spill->Reg[i]);
    }
}


jit_expression Storage_ForceAllocateReg(jit_storage_manager *S, uint Reg)
{
    assert(Reg < TARGETENV_REG_COUNT);
    S->BusyRegCount += !S->RegIsBusy[Reg];
    S->RegIsBusy[Reg] = true;

    return (jit_expression) {
        .Storage = STORAGE_REG,
        .As.Reg = Reg
    };
}

jit_expression Storage_AllocateReg(jit_storage_manager *S)
{
    int Reg = Storage_TryAllocateReg(S);
    assert(Reg != -1 && "TODO: register spilling in expression");
    return (jit_expression) {
        .Storage = STORAGE_REG,
        .As.Reg = Reg,
    };
}

void Storage_DeallocateReg(jit_storage_manager *S, uint Reg)
{
    assert(Reg < TARGETENV_REG_COUNT - 1);

    S->BusyRegCount -= S->RegIsBusy[Reg];
    S->RegIsBusy[Reg] = false;
}


jit_expression Storage_AllocateStack(jit_storage_manager *S)
{
    jit_expression Stack = {
        .Storage = STORAGE_MEM,
        .As.Mem = {
            .Offset = Storage_PushStack(S, sizeof(double)),
            .BaseReg = TargetEnv_GetStackFrameReg(),
        },
    };
    S->MaxStackSize = MAX(S->StackSize, S->MaxStackSize);
    return Stack;
}

jit_expression Storage_AllocateGlobal(jit_storage_manager *S)
{
    assert(S->GlobalSize < S->GlobalCapacity);
    jit_expression Global = {
        .Storage = STORAGE_MEM,
        .As.Mem = {
            .Offset = S->GlobalSize * sizeof(double),
            .BaseReg = TargetEnv_GetGlobalPtrReg(),
        },
    };
    S->GlobalSize++;
    return Global;
}

jit_expression Storage_AllocateConst(jit_storage_manager *S, double Const)
{
    jit_expression Global = Storage_AllocateGlobal(S);
    S->GlobalMemory[Global.As.Mem.Offset / sizeof(double)] = Const;
    return Global;
}

double Storage_GetConst(const jit_storage_manager *S, i32 GlobalOffset)
{
    assert(IN_RANGE(0, GlobalOffset, (i64)S->GlobalCapacity));
    return S->GlobalMemory[GlobalOffset / sizeof(double)];
}


