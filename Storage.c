
#include <string.h> /* memset */
#include "Storage.h"


#define CURR_SCOPE(x) (x)[S->Scope]


int Storage_PushStack(jit_storage_manager *S, int Count)
{
    CURR_SCOPE(S->StackSize) += Count * TargetEnv_StackMinAlignment(S->DataSize);
    CURR_SCOPE(S->MaxStackSize) = MAX(CURR_SCOPE(S->StackSize), CURR_SCOPE(S->MaxStackSize));
    CURR_SCOPE(S->MaxStackSize) = TargetEnv_StackMaxAlignment(CURR_SCOPE(S->MaxStackSize));
    return -CURR_SCOPE(S->StackSize);
}

void Storage_PopStack(jit_storage_manager *S, int Count)
{
    CURR_SCOPE(S->StackSize) -= Count * TargetEnv_StackMinAlignment(S->DataSize);
}



jit_storage_manager Storage_Init(void *GlobalMemory, uint GlobalMemCapacity)
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

void Storage_Reset(jit_storage_manager *S, int DataSize)
{
    S->Scope = 0;
    S->GlobalSize = 0;
    S->DataSize = DataSize;
}



jit_reg Storage_TryAllocateReg(jit_storage_manager *S)
{
    jit_reg RegCount = TARGETENV_REG_COUNT;
    for (jit_reg i = 0; i < RegCount; i++)
    {
        if (!CURR_SCOPE(S->RegIsBusy)[i])
        {
            CURR_SCOPE(S->RegIsBusy)[i] = true;
            CURR_SCOPE(S->BusyRegCount)++;
            return i;
        }
    }
    return JIT_REG_INVALID;
}


void Storage_DeallocateReg(jit_storage_manager *S, uint Reg)
{
    ASSERT(Reg < TARGETENV_REG_COUNT - 1, "invalid reg");

    CURR_SCOPE(S->BusyRegCount) -= CURR_SCOPE(S->RegIsBusy)[Reg];
    CURR_SCOPE(S->RegIsBusy)[Reg] = false;
}


void Storage_ForceAllocateReg(jit_storage_manager *S, jit_reg Reg)
{
    ASSERT(Reg < TARGETENV_REG_COUNT, "invalid reg");
    CURR_SCOPE(S->BusyRegCount) += !CURR_SCOPE(S->RegIsBusy)[Reg];
    CURR_SCOPE(S->RegIsBusy)[Reg] = true;
}
jit_mem Storage_AllocateStack(jit_storage_manager *S)
{
    jit_mem Mem = {
        .Offset = Storage_PushStack(S, 1),
        .BaseReg = TargetEnv_GetStackFrameReg(),
    };
    CURR_SCOPE(S->MaxStackSize) = MAX(CURR_SCOPE(S->StackSize), CURR_SCOPE(S->MaxStackSize));
    return Mem;
}
jit_mem Storage_AllocateGlobal(jit_storage_manager *S)
{
    ASSERT(S->GlobalSize < S->GlobalCapacity, "out of memory");
    jit_mem Global = {
        .Offset = S->GlobalSize,
        .BaseReg = TargetEnv_GetGlobalPtrReg(),
    };
    S->GlobalSize += S->DataSize;
    return Global;
}
jit_mem Storage_AllocateConst(jit_storage_manager *S, double Const)
{
    jit_mem Global = Storage_AllocateGlobal(S);
    u8 *Ptr = (u8 *)S->GlobalMemory + Global.Offset;
    if (sizeof(float) == S->DataSize)
    {
        float Const32 = Const;
        MemCpy(Ptr, &Const32, S->DataSize);
    }
    else if (sizeof(double) == S->DataSize)
    {
        MemCpy(Ptr, &Const, S->DataSize);
    }
    else UNREACHABLE();
    return Global;
}

double Storage_GetConst(const jit_storage_manager *S, i32 GlobalOffset)
{
    ASSERT(IN_RANGE(0, GlobalOffset, (i64)S->GlobalCapacity), "invalid const offset");
    ASSERT(IN_RANGE(0, GlobalOffset + S->DataSize, (i64)S->GlobalCapacity), "invalid const offset && size");

    const u8 *Ptr = (const u8 *)S->GlobalMemory + GlobalOffset;
    if (sizeof(float) == S->DataSize)
    {
        float Out;
        MemCpy(&Out, Ptr, S->DataSize);
        return (double)Out;
    }
    else if (sizeof(double) == S->DataSize)
    {
        double Out;
        MemCpy(&Out, Ptr, S->DataSize);
        return Out;
    }
    else UNREACHABLE();
    return 0;
}

int Storage_GetMaxStackSize(const jit_storage_manager *S)
{
    return CURR_SCOPE(S->MaxStackSize);
}

