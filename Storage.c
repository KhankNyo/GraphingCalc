#include "Storage.h"


int Storage_PushStack(jit_storage_manager *S, int Count)
{
    S->StackSize += Count * TargetEnv_StackMinAlignment(S->DataSize);
    S->MaxStackSize = MAX(S->StackSize, S->MaxStackSize);
    S->MaxStackSize = TargetEnv_StackMaxAlignment(S->MaxStackSize);
    return -S->StackSize;
}

void Storage_PopStack(jit_storage_manager *S, int Count)
{
    S->StackSize -= Count * TargetEnv_StackMinAlignment(S->DataSize);
}



jit_storage_manager Storage_Init(void *GlobalMemory, uint GlobalMemCapacity)
{
    jit_storage_manager S = {
        .GlobalMemory = GlobalMemory,
        .GlobalCapacity = GlobalMemCapacity,
    };
    return S;
}

void Storage_Reset(jit_storage_manager *S, int DataSize)
{
    S->GlobalSize = 0;
    S->DataSize = DataSize;
}



jit_reg Storage_TryAllocateReg(jit_storage_manager *S)
{
    jit_reg RegCount = TARGETENV_REG_COUNT;
    for (jit_reg i = 0; i < RegCount; i++)
    {
        if (!S->RegIsBusy[i])
        {
            S->RegIsBusy[i] = true;
            S->BusyRegCount++;
            return i;
        }
    }
    return JIT_REG_INVALID;
}


void Storage_DeallocateReg(jit_storage_manager *S, uint Reg)
{
    ASSERT(Reg < TARGETENV_REG_COUNT - 1, "invalid reg");

    S->BusyRegCount -= S->RegIsBusy[Reg];
    S->RegIsBusy[Reg] = false;
}


void Storage_ForceAllocateReg(jit_storage_manager *S, jit_reg Reg)
{
    ASSERT(Reg < TARGETENV_REG_COUNT, "invalid reg");
    S->BusyRegCount += !S->RegIsBusy[Reg];
    S->RegIsBusy[Reg] = true;
}
jit_mem Storage_AllocateStack(jit_storage_manager *S)
{
    jit_mem Mem = {
        .Offset = Storage_PushStack(S, 1),
        .BaseReg = TargetEnv_GetStackFrameReg(),
    };
    S->MaxStackSize = MAX(S->StackSize, S->MaxStackSize);
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

void Storage_SetMaxStackCount(jit_storage_manager *S, int Size)
{
    S->StackSize = Size*S->DataSize;
    S->MaxStackSize = Size*S->DataSize;
}

