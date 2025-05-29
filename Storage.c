
#include <string.h> /* memset */
#include "Storage.h"
#include "Emitter.h"


static uint Storage_RegCount(const jit_storage_manager *S)
{
    return sizeof S->RegIsBusy;
}

static int Storage_TryAllocateReg(jit_storage_manager *S)
{
    uint RegCount = Storage_RegCount(S);
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

static int Storage_PushStack(jit_storage_manager *S, int Size)
{
    S->StackSize += Size;
    S->MaxStackSize = MAX(S->StackSize, S->MaxStackSize);
    return -S->StackSize;
}

void Storage_PopStack(jit_storage_manager *S, int Size)
{
    S->StackSize -= Size;
}


jit_storage_manager Storage_Init(int StackPtrReg, int GlobalPtrReg)
{
    jit_storage_manager S = {
        .StackPtrReg = StackPtrReg,
        .GlobalPtrReg = GlobalPtrReg,
    };
    Storage_ResetTmpAndStack(&S);
    return S;
}

void Storage_ResetTmpAndStack(jit_storage_manager *S)
{
    S->StackSize = 0x20; /* shadow space */
    S->MaxStackSize = S->StackSize;
    S->BusyRegCount = 0;
    memset(S->RegIsBusy, 0, Storage_RegCount(S));
}


storage_spill_data Storage_Spill(jit_storage_manager *S)
{
    storage_spill_data Spill = { 0 };

    for (uint i = 0; i < Storage_RegCount(S) && S->BusyRegCount; i++)
    {
        if (S->RegIsBusy[i])
        {
            int RegToSpill = i;
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
    assert(Reg < Storage_RegCount(S));
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
    assert(Reg < Storage_RegCount(S) - 1);

    S->BusyRegCount -= S->RegIsBusy[Reg];
    S->RegIsBusy[Reg] = false;
}


jit_expression Storage_AllocateStack(jit_storage_manager *S)
{
    jit_expression Stack = {
        .Storage = STORAGE_MEM,
        .As.Mem = {
            .Offset = Storage_PushStack(S, sizeof(double)),
            .BaseReg = S->StackPtrReg,
        },
    };
    S->MaxStackSize = MAX(S->StackSize, S->MaxStackSize);
    return Stack;
}

jit_expression Storage_AllocateGlobal(jit_storage_manager *S)
{
    jit_expression Global = {
        .Storage = STORAGE_MEM,
        .As.Mem = {
            .Offset = S->GlobalSize,
            .BaseReg = S->GlobalPtrReg,
        },
    };
    S->GlobalSize += sizeof(double);
    return Global;
}

jit_expression Storage_AllocateConst(jit_storage_manager *S, double Const)
{
    jit_expression Global = Storage_AllocateGlobal(S);
    S->Consts[S->ConstCount] = Const;
    S->ConstOffset[S->ConstCount] = Global.As.Mem.Offset;
    S->ConstCount++;
    return Global;
}

double Storage_GetConst(const jit_storage_manager *S, i32 GlobalOffset)
{
    /* binary search */
    assert(S->ConstCount > 0);
    uint Lower = 0;
    uint Upper = S->ConstCount - 1;
    while (Lower + 1 < Upper)
    {
        uint Mid = Lower + (Upper - Lower)/2;
        if (S->ConstOffset[Mid] < GlobalOffset)
        {
            Lower = Mid;
        }
        else if (S->ConstOffset[Mid] > GlobalOffset)
        {
            Upper = Mid;
        }
        else
        {
            return S->Consts[Mid];
        }
    }

    if (S->ConstOffset[Lower] == GlobalOffset)
        return S->Consts[Lower];
    if (S->ConstOffset[Upper] == GlobalOffset)
        return S->Consts[Upper];

    assert(false && "Could not get const.");
    return 0;
}


