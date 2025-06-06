
#include <string.h> /* memset */
#include "Emitter.h"
#include "Storage.h"


#define CURR_SCOPE(x) (x)[S->Scope]

static int Storage_TryAllocateReg(jit_storage_manager *S)
{
    uint RegCount = TARGETENV_REG_COUNT;
    for (uint i = 0; i < RegCount; i++)
    {
        if (!CURR_SCOPE(S->RegIsBusy)[i])
        {
            CURR_SCOPE(S->RegIsBusy)[i] = true;
            CURR_SCOPE(S->BusyRegCount)++;
            return i;
        }
    }
    return -1;
}

int Storage_PushStack(jit_storage_manager *S, int Size)
{
    CURR_SCOPE(S->StackSize) += Size;
    CURR_SCOPE(S->MaxStackSize) = MAX(CURR_SCOPE(S->StackSize), CURR_SCOPE(S->MaxStackSize));
    CURR_SCOPE(S->MaxStackSize) = TargetEnv_AlignStackSize(CURR_SCOPE(S->MaxStackSize));
    return -CURR_SCOPE(S->StackSize);
}

void Storage_PopStack(jit_storage_manager *S, int Size)
{
    CURR_SCOPE(S->StackSize) -= Size;
}



jit_storage_manager Storage_Init(double *GlobalMemory, uint GlobalMemCapacity)
{
    jit_storage_manager S = {
        .GlobalMemory = GlobalMemory,
        .GlobalCapacity = GlobalMemCapacity,
    };
    return S;
}

void Storage_PushScope(jit_storage_manager *S)
{
    ASSERT(S->Scope < (int)STATIC_ARRAY_SIZE(S->StackSize), "invalid scope count");
    S->Scope++;
    CURR_SCOPE(S->StackSize) = TargetEnv_GetShadowSpaceSize(); /* shadow space */
    CURR_SCOPE(S->MaxStackSize) = CURR_SCOPE(S->StackSize);
    CURR_SCOPE(S->BusyRegCount) = 0;
    memset(CURR_SCOPE(S->RegIsBusy), 0, TARGETENV_REG_COUNT);
}

void Storage_PopScope(jit_storage_manager *S)
{
    ASSERT(S->Scope > 0, "invalid scope count");
    S->Scope--;
}

void Storage_Reset(jit_storage_manager *S)
{
    S->Scope = 0;
    S->GlobalSize = 0;
}


storage_spill_data Storage_Spill(jit_storage_manager *S)
{
    storage_spill_data Spill = { 0 };

    for (uint i = 0; i < TARGETENV_REG_COUNT && CURR_SCOPE(S->BusyRegCount); i++)
    {
        if (CURR_SCOPE(S->RegIsBusy)[i])
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
    ASSERT(Reg < TARGETENV_REG_COUNT, "invalid reg");
    CURR_SCOPE(S->BusyRegCount) += !CURR_SCOPE(S->RegIsBusy)[Reg];
    CURR_SCOPE(S->RegIsBusy)[Reg] = true;

    return (jit_expression) {
        .Storage = STORAGE_REG,
        .As.Reg = Reg
    };
}

jit_expression Storage_AllocateReg(jit_storage_manager *S)
{
    int Reg = Storage_TryAllocateReg(S);
    if (-1 == Reg)
        TODO("registeer spilling in expression");
    return (jit_expression) {
        .Storage = STORAGE_REG,
        .As.Reg = Reg,
    };
}

void Storage_DeallocateReg(jit_storage_manager *S, uint Reg)
{
    ASSERT(Reg < TARGETENV_REG_COUNT - 1, "invalid reg");

    CURR_SCOPE(S->BusyRegCount) -= CURR_SCOPE(S->RegIsBusy)[Reg];
    CURR_SCOPE(S->RegIsBusy)[Reg] = false;
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
    CURR_SCOPE(S->MaxStackSize) = MAX(CURR_SCOPE(S->StackSize), CURR_SCOPE(S->MaxStackSize));
    return Stack;
}

jit_expression Storage_AllocateGlobal(jit_storage_manager *S)
{
    ASSERT(S->GlobalSize < S->GlobalCapacity, "out of memory");
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
    ASSERT(IN_RANGE(0, GlobalOffset, (i64)S->GlobalCapacity), "invalid const offset");
    return S->GlobalMemory[GlobalOffset / sizeof(double)];
}

int Storage_GetMaxStackSize(const jit_storage_manager *S)
{
    return CURR_SCOPE(S->MaxStackSize);
}

