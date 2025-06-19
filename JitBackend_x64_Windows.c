
#include <stdarg.h>
#include <stdio.h>
#include <limits.h> /* INT_MAX */
#include "JitBackend.h"

#define RM(v) ((v) & 0x7)
#define REG(v) (((v) & 0x7) << 3)
#define MODRM(mod, reg, rm) \
    (((mod) & 0x3) << 6)\
    | REG(reg) | RM(rm)
#define SIB(scale, index, base) \
    (((scale) & 0x3) << 6)\
    | ((index) & 0x7) << 3\
    | ((base) & 0x7) 
#define XMM(n) (n)

typedef enum sse_cmp_type 
{
    CMP_EQ, 
    CMP_LT, 
    CMP_LE, 
    CMP_UNORD, 
    CMP_NEQ, 
    CMP_NLT, 
    CMP_NLE, 
    CMP_ORD,
} sse_cmp_type;

typedef enum reg_index 
{
    RAX = 0, 
    RCX, 
    RDX, 
    RBX, 
    RSP, 
    RBP, 
    RSI, 
    RDI,
} reg_index; 

/*=======================================
 *              jit_location 
 *======================================= */
typedef enum jit_location_type 
{
    LOCATION_MEM = 1,
    LOCATION_REG,
    LOCATION_REF,
} jit_location_type;
typedef i8 jit_reg;
typedef struct jit_mem 
{
    i32 Offset;
    jit_reg BaseReg;
} jit_mem;
typedef struct jit_location
{
    jit_location_type Type;
    union {
        jit_mem Mem;
        jit_reg Reg;
        struct jit_location *Ref;
    } As;
} jit_location;
#define JIT_REG_INVALID -1



typedef struct disasm_data
{
    const u8 *Memory;
    char *DisasmBuffer;

    int MemoryCapacity;
    int InstructionSize;
    uint DisasmBufferSize;
    uint DisasmBufferCap;

} disasm_data;

typedef enum disasm_modrm_type 
{
    MODRM_DST_SRC,
    MODRM_SRC_DST,
} disasm_modrm_type;

static const char *sIntReg[] = { 
    [RAX] = "rax", 
    [RCX] = "rcx",
    [RDX] = "rdx",
    [RBX] = "rbx", 
    [RSP] = "rsp",
    [RBP] = "rbp",
    [RSI] = "rsi", 
    [RDI] = "rdi",
};
static const char *sXmmReg[] = {
    "xmm0",
    "xmm1",
    "xmm2",
    "xmm3",
    "xmm4",
    "xmm5",
    "xmm6",
    "xmm7",
};
static const char *sCmpType[] = {
    [CMP_EQ] = "eq", 
    [CMP_LT] = "lt", 
    [CMP_LE] = "le", 
    [CMP_UNORD] = "unord", 
    [CMP_NEQ] = "ne", 
    [CMP_NLT] = "ge", 
    [CMP_NLE] = "gt", 
    [CMP_ORD] = "ord",
};


static jit_reg Backend_TmpReg(jit_backend *Backend);
static void Backend_ForceAllocateReg(jit_backend *Backend, jit_reg Reg, jit_location *EvalStackLocation);
static void Backend_DeallocateReg(jit_backend *Backend, jit_reg Reg);


static bool8 Backend_x64_IsAVXSupported(void);


#if (defined(_MSC_VER) && !defined(__clang__) /* clang is real funny */)\
|| (defined(_WIN32) && defined(__TINYC__)) /* tcc for windows */

static void Backend_x64_GetCPUID(u32 Feature, u32 *OutEAX, u32 *OutEBX, u32 *OutECX, u32 *OutEDX)
{
    int Bitfield[4];
    __cpuid(Bitfield, Feature);
    *OutEAX = Bitfield[0];
    *OutEBX = Bitfield[1];
    *OutECX = Bitfield[2];
    *OutEDX = Bitfield[3];
}

#else /* non-msvc compiler targeting windows */
#   include <cpuid.h>

static void Backend_x64_GetCPUID(u32 Feature, u32 *OutEAX, u32 *OutEBX, u32 *OutECX, u32 *OutEDX)
{
    __get_cpuid(Feature, OutEAX, OutEBX, OutECX, OutEDX);
}

#endif /* (_MSC_VER && !__clang__) || (_WIN32 && __TINYC__) */


static bool8 Backend_x64_IsAVXSupported(void)
{
    u32 EAX, 
        EBX, 
        ECX = 0, 
        EDX;
    Backend_x64_GetCPUID(1, &EAX, &EBX, &ECX, &EDX);
    return (ECX & (1 << 20)) != 0;
}

static int Backend_GetArgRegCount(void)
{
    /* rcx taken as global ptr reg */
    /* xmm1 = arg0  */
    /* xmm2 = arg1  */
    /* xmm3 = arg2  */
    /* rest are on stack */
    return 3;
}
static bool8 Backend_IsArgumentInReg(int ArgIndex)
{
    return ArgIndex < Backend_GetArgRegCount();
}
static jit_reg Backend_CallerSideArgReg(int ArgIndex)
{
    ASSERT(Backend_IsArgumentInReg(ArgIndex), "unreachable");
    return ArgIndex + 1;
}
static jit_mem Backend_CallerSideArgMem(int ArgIndex)
{
    ASSERT(!Backend_IsArgumentInReg(ArgIndex), "unreachable");
    return (jit_mem) {
        .BaseReg = RSP,
        .Offset = (ArgIndex + 1)*8, /* arg0 = global ptr (rcx) */
    };
}
static jit_mem Backend_CalleeSideParamMem(int ArgIndex)
{
    return (jit_mem) {
        .BaseReg = RBP,
        .Offset = 0x10 + (ArgIndex + 1)*8, /* arg0 = global ptr (rcx) */
    };
}



static jit_location LocationFromReg(jit_reg R)
{
    return (jit_location) {
        .Type = LOCATION_REG,
        .As.Reg = R,
    };
}

static jit_location LocationFromMem(jit_mem Mem)
{
    return (jit_location) {
        .Type = LOCATION_MEM,
        .As.Mem = Mem,
    };
}

/* ms x64 calling conv */
static int Backend_GlobalPtrReg(void)
{
    return RCX;
}



static int EmitCode(jit_backend *Backend, int Count, ...)
{
    int InstructionOffset = Backend->ProgramSize;
    va_list Args;
    va_start(Args, Count);
    for (int i = 0; i < Count && (uint)Backend->ProgramSize < Backend->ProgramCapacity; i++)
    {
        Backend->Program[Backend->ProgramSize++] = va_arg(Args, uint);
    }
    va_end(Args);
    return InstructionOffset;
}

static int EmitCodeSequence(jit_backend *Backend, const u8 Array[], int Count)
{
    int InstructionOffset = Backend->ProgramSize;
    for (int i = 0; i < Count && (uint)Backend->ProgramSize < Backend->ProgramCapacity; i++)
    {
        Backend->Program[Backend->ProgramSize++] = Array[i];
    }
    return InstructionOffset;
}

static int ModFromDisplacement(i32 Displacement)
{
    if (0 == Displacement)
        return 0;
    if (IN_RANGE(INT8_MIN, Displacement, INT8_MAX))
        return 1;
    return 2;
}

/* does not support rip-relative offset and index-scale mode */
static i32 Backend_EmitGenericModRm(jit_backend *Backend, int Reg, int Rm, i32 Displacement)
{
    int Mod = ModFromDisplacement(Displacement);
    if (RBP == Rm && 0 == Displacement)
        Mod = 1;
    u8 ModRm = MODRM(Mod, Reg, Rm);

    EmitCode(Backend, 1, ModRm);
    if (RSP == Rm)
    {
        EmitCode(Backend, 1, SIB(0, RSP, RSP));
    }

    i32 DisplacementLocation = Backend->ProgramSize;
    switch (Mod)
    {
    case 0:
    case 3:
        break; /* no displacement to emit */
    case 1: EmitCode(Backend, 1, Displacement); break;
    case 2: EmitCodeSequence(Backend, (u8 *)&Displacement, sizeof Displacement); break;
    }
    return DisplacementLocation;
}

static int Backend_EmitFloatOpcode(jit_backend *Backend, u8 Opcode, int DstReg, int SrcBase, i32 SrcOffset)
{
    int InstructionOffset = EmitCode(Backend, 3, Backend->FloatOpcode, 0x0F, Opcode);
    Backend_EmitGenericModRm(Backend, DstReg, SrcBase, SrcOffset);
    return InstructionOffset;
}

static void Backend_EmitFloatOpcodeReg(jit_backend *Backend, u8 Opcode, int DstReg, int SrcReg)
{
    u8 ModRm = MODRM(3, DstReg, SrcReg);
    EmitCode(Backend, 4, Backend->FloatOpcode, 0x0F, Opcode, ModRm);
}

static int Backend_EvalStack_IndexOf(const jit_backend *Backend, const jit_location *Location)
{
    const jit_location *Base = (const jit_location *)Backend->EvalStack->Ptr;
    return Location - Base;
}
static void Backend_EvalStack_Push(jit_backend *Backend, jit_location Location)
{
    jit_location *Slot = Jit_Scratchpad_PushLeft(Backend->EvalStack, sizeof(*Slot));
    *Slot = Location;
    if (Location.Type == LOCATION_REG)
        Backend_ForceAllocateReg(Backend, Location.As.Reg, Slot);
}
static jit_location Backend_EvalStack_Pop(jit_backend *Backend)
{
    jit_location *Top = Jit_Scratchpad_PopLeft(Backend->EvalStack, sizeof(jit_location));
    if (LOCATION_REG == Top->Type)
    {
        Backend_DeallocateReg(Backend, Top->As.Reg);
    }
    return *Top;
}

static jit_location *Backend_EvalStack_Top(jit_backend *Backend, int IndexFromTop)
{
    ASSERT(Backend->EvalStack->LeftCount >= (int)sizeof(jit_location), "empty stack");
    ASSERT(Backend->EvalStack->LeftCount >= (int)((IndexFromTop + 1) * sizeof(jit_location)), "empty stack");
    return (jit_location *)Jit_Scratchpad_LeftPtr(Backend->EvalStack) - IndexFromTop - 1;
}
static jit_location *Backend_EvalStack_Array(jit_backend *Backend, int Index)
{
    ASSERT(Index*(int)sizeof(jit_location) < Backend->EvalStack->LeftCount, "invalid index");
    return (jit_location *)Backend->EvalStack->Ptr + Index;
}
static int Backend_EvalStack_Count(const jit_backend *Backend)
{
    return Backend->EvalStack->LeftCount / sizeof(jit_location);
}





void Backend_Init(
    jit_backend *Backend, 
    jit_scratchpad *LeftStack,
    u8 *ProgramBuffer, i32 ProgramBufferSizeByte,
    void *GlobalDataBuffer, i32 GlobalDataSizeByte
)
{
    ASSERT(NULL != Backend, "nullptr");
    ASSERT(NULL != LeftStack, "nullptr");
    ASSERT(NULL != ProgramBuffer, "nullptr");
    ASSERT(NULL != GlobalDataBuffer, "nullptr");
    *Backend = (jit_backend) {
        .Program = ProgramBuffer,
        .ProgramCapacity = ProgramBufferSizeByte, 
        .GlobalData = GlobalDataBuffer,
        .GlobalDataCapacity = GlobalDataSizeByte,
        .EvalStack = LeftStack,
    };
}

void Backend_Reset(jit_backend *Backend, int DataSize)
{
    /* stores and loads are kinda anti-pattern */
    const u8 StoreSingle32[] = {
        0x66, 0x0F, 0x7E,
    };
    const u8 StoreSingle64[] = {
        0x66, 0xF, 0xD6,
    };
    const u8 LoadSingle32[] = {
        0x66, 0x0F, 0x6E,
    };
    const u8 LoadSingle64[] = {
        0xF3, 0x0F, 0x7E
    };
    STATIC_ASSERT(sizeof StoreSingle32 == sizeof Backend->StoreSingle, "unreachable");
    STATIC_ASSERT(sizeof StoreSingle64 == sizeof Backend->StoreSingle, "unreachable");
    STATIC_ASSERT(sizeof LoadSingle32 == sizeof Backend->LoadSingle, "unreachable");
    STATIC_ASSERT(sizeof LoadSingle64 == sizeof Backend->LoadSingle, "unreachable");

    Backend->ProgramSize = 0;
    Backend->GlobalDataSize = 0;
    if (sizeof(float) == DataSize)
    {
        Backend->DataSize = 4;
        Backend->FloatOpcode = 0xF3;
        MemCpy(Backend->StoreSingle, StoreSingle32, sizeof StoreSingle32);
        MemCpy(Backend->LoadSingle, LoadSingle32, sizeof LoadSingle32);
    }
    else if (sizeof(double) == DataSize)
    {
        Backend->DataSize = 8;
        Backend->FloatOpcode = 0xF2;
        MemCpy(Backend->StoreSingle, StoreSingle64, sizeof StoreSingle64);
        MemCpy(Backend->LoadSingle, LoadSingle64, sizeof LoadSingle64);
    }
    else UNREACHABLE();

    Backend->One = Backend_AllocateGlobal(Backend, 1.0);
}


static void Backend_EmitMove(jit_backend *Backend, jit_reg DstReg, jit_reg SrcReg)
{
    if (DstReg != SrcReg)
    {
        /* movaps dst, src */
        EmitCode(Backend, 3, 0x0F, 0x28, MODRM(3, DstReg, SrcReg));
    }
}


static i32 Backend_EmitLoad(jit_backend *Backend, jit_reg DstReg, jit_reg SrcBase, i32 SrcOffset)
{
    /* emit it as a store instruction initially */
    uint PrevSize = EmitCodeSequence(Backend, 
        Backend->StoreSingle, 
        sizeof Backend->StoreSingle
    );
    i32 DisplacementLocation = Backend_EmitGenericModRm(Backend, DstReg, SrcBase, SrcOffset);
    uint InstructionLength = Backend->ProgramSize - PrevSize;

    /* compared the emitted instruction with the last*/
    u8 *PrevIns = Backend->Program + PrevSize - InstructionLength;
    u8 *CurrIns = Backend->Program + PrevSize;
    if (Backend->ProgramSize >= 2*InstructionLength
    && MemEqu(PrevIns, CurrIns, InstructionLength))
    {
        /* if matched, 
         * this current instruction was trying to read the value we just stored into the same regsiter, 
         * optimize it out */
        Backend->ProgramSize -= InstructionLength;
    }
    else
    {
        /* otherwise, change the emitted instruction into a load */
        for (uint i = 0; i < sizeof Backend->LoadSingle; i++)
            CurrIns[i] = Backend->LoadSingle[i];
    }
    return DisplacementLocation;
}

static void Backend_EmitStore(jit_backend *Backend, const jit_location *Src, jit_mem Dst)
{
    /* movq [base + offset], src */
    switch (Src->Type)
    {
    case LOCATION_REF:
    {
        Backend_EmitStore(Backend, Src->As.Ref, Dst);
    } break;
    case LOCATION_REG:
    {
        EmitCodeSequence(Backend, 
            Backend->StoreSingle,
            sizeof Backend->StoreSingle
        );
        Backend_EmitGenericModRm(Backend, Src->As.Reg, Dst.BaseReg, Dst.Offset);
    } break;
    case LOCATION_MEM:
    {
        jit_reg TmpReg = Backend_TmpReg(Backend);
        Backend_EmitLoad(Backend, TmpReg, Src->As.Mem.BaseReg, Src->As.Mem.Offset);
        EmitCodeSequence(Backend, 
            Backend->StoreSingle,
            sizeof Backend->StoreSingle
        );
        Backend_EmitGenericModRm(Backend, TmpReg, Dst.BaseReg, Dst.Offset);
    } break;
    }
}



static void Backend_EmitAdd(jit_backend *Backend, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Backend_EmitFloatOpcode(Backend, 0x58, Dst, Base, Offset);
}

static void Backend_EmitSub(jit_backend *Backend, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Backend_EmitFloatOpcode(Backend, 0x5C, Dst, Base, Offset);
}

static void Backend_EmitMul(jit_backend *Backend, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Backend_EmitFloatOpcode(Backend, 0x59, Dst, Base, Offset);
}

static void Backend_EmitDiv(jit_backend *Backend, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Backend_EmitFloatOpcode(Backend, 0x5E, Dst, Base, Offset);
}

static void Backend_EmitAddReg(jit_backend *Backend, jit_reg Dst, jit_reg Src)
{
    Backend_EmitFloatOpcodeReg(Backend, 0x58, Dst, Src);
}

static void Backend_EmitSubReg(jit_backend *Backend, jit_reg Dst, jit_reg Src)
{
    Backend_EmitFloatOpcodeReg(Backend, 0x5C, Dst, Src);
}

static void Backend_EmitMulReg(jit_backend *Backend, jit_reg Dst, jit_reg Src)
{
    Backend_EmitFloatOpcodeReg(Backend, 0x59, Dst, Src);
}

static void Backend_EmitDivReg(jit_backend *Backend, jit_reg Dst, jit_reg Src)
{
    Backend_EmitFloatOpcodeReg(Backend, 0x5E, Dst, Src);
}

static void Backend_EmitLoadZero(jit_backend *Backend, jit_reg DstReg)
{
    /* xorps dst, dst */
    EmitCode(Backend, 3, 0x0F, 0x57, MODRM(3, DstReg, DstReg));
}

static void Backend_EmitCmpReg(jit_backend *Backend, jit_reg Dst, jit_reg B)
{
    /* cmpss dst, b */
    Backend_EmitFloatOpcodeReg(Backend, 0xC2, Dst, B);
    EmitCode(Backend, 1, Backend->CmpType);

    /* andps dst, [1.0f] */
    EmitCode(Backend, 2, 0x0F, 0x54);
    Backend_EmitGenericModRm(Backend, Dst, Backend_GlobalPtrReg(), Backend->One);
}

static void Backend_EmitCmp(jit_backend *Backend, jit_reg Dst, jit_reg SrcBase, i32 SrcOffset)
{
    /* cmpss dst, b */
    Backend_EmitFloatOpcode(Backend, 0xC2, Dst, SrcBase, SrcOffset);
    EmitCode(Backend, 1, Backend->CmpType);

    /* andps dst, [1.0f] */
    EmitCode(Backend, 2, 0x0F, 0x54);
    Backend_EmitGenericModRm(Backend, Dst, Backend_GlobalPtrReg(), Backend->One);
}




static uint Backend_EmitFunctionEntry(jit_backend *Backend)
{
    /* push rbp 
     * mov rbp, rsp
     * sub rsp, memsize
     * */
    while (Backend->ProgramSize % 0x10 != 0)
    {
        EmitCode(Backend, 1, 0x90); /* nop */
    }
    uint Location = EmitCode(Backend, 1 + 3, 
        0x50 + RBP,                     /* push rbp */
        0x48, 0x89, MODRM(3, RSP, RBP)  /* mov rbp, rsp */
    );
    EmitCode(Backend, 3 + 4, 
        0x48, 0x81, MODRM(3, 5, RSP),   /* sub rsp, i32 */
        0, 0, 0, 0
    );
    return Location;
}


static void Backend_EmitFunctionExit(jit_backend *Backend, uint EntryLocation)
{
    uint PrologueSize = 1 + 3 + 3 + 4;
    ASSERT(EntryLocation + PrologueSize <= Backend->ProgramSize, "invalid location");

    /* leave 
     * ret 
     */
    EmitCode(Backend, 2, 0xC9, 0xC3);

    /* patch stack size */
    i32 StackSize = Backend->MaxStackSize;
    uint StackSizeLocation = EntryLocation + 1 + 3 + 3;
    MemCpy(
        Backend->Program + StackSizeLocation,
        &StackSize, 
        sizeof StackSize
    );
}


static uint Backend_EmitCall(jit_backend *Backend, uint FunctionLocation)
{
    /* call rel32 */
    i32 Rel32 = FunctionLocation - (Backend->ProgramSize + 5);
    uint Location = EmitCode(Backend, 1, 0xE8);
    EmitCodeSequence(Backend, (u8 *)&Rel32, sizeof Rel32);
    return Location;
}



/* ======================================================================
 *                           Register allocation
 * ====================================================================== */

static void Backend_SpillReg(jit_backend *Backend, int RegCount);

static void Backend_ResetTmp(jit_backend *Backend)
{
    Backend->MaxStackSize = 0;
    Backend->StackSize = 0;
    Backend->EvalStack->LeftCount = 0;
    for (int i = 0; i < REG_COUNT; i++)
        Backend->RegLocation[i] = NULL;
    Backend->BusyRegCount = 0;
}

static bool8 Backend_RegIsBusy(const jit_backend *Backend, jit_reg Reg)
{
    return NULL != Backend->RegLocation[Reg];
}

static void Backend_ForceAllocateReg(jit_backend *Backend, jit_reg Reg, jit_location *EvalStackLocation)
{
    ASSERT(Reg != JIT_REG_INVALID, "unreachable");
    ASSERT(Reg < REG_COUNT, "unreachable");
    ASSERT(!Backend_RegIsBusy(Backend, Reg), "attempting to allocate a busy reg");

    Backend->BusyRegCount += 1;
    Backend->RegLocation[Reg] = EvalStackLocation;
}

static jit_reg Backend_FindFreeReg(jit_backend *Backend)
{
    for (jit_reg i = 0; i < REG_COUNT; i++)
    {
        if (!Backend_RegIsBusy(Backend, i))
        {
            return i;
        }
    }
    return JIT_REG_INVALID;
}

static void Backend_DeallocateReg(jit_backend *Backend, jit_reg Reg)
{
    ASSERT(Reg < REG_COUNT, "invalid reg");
    ASSERT(Backend_RegIsBusy(Backend, Reg), "attempting to deallocate a free reg");

    Backend->BusyRegCount -= 1;
    Backend->RegLocation[Reg] = NULL;
}




static i32 Backend_PushStack(jit_backend *Backend, int DataCount)
{
    /* allocate some stack space, 
     * ensure max stack size is always aligned to 16-byte boundary (required by windows) */
    int Size = DataCount * 8;
    Backend->StackSize += Size;
    int AlignedStackSizeSSE = ROUND_UP_TO_MULTIPLE(Backend->StackSize, 16);
    Backend->MaxStackSize = MAX(AlignedStackSizeSSE, Backend->MaxStackSize);

    return Backend->StackSize;
}

static void Backend_PopStack(jit_backend *Backend, int DataCount)
{
    int PopSize = DataCount * 8;
    ASSERT(PopSize <= Backend->StackSize, "unreachable");
    Backend->StackSize -= PopSize;
}

static i32 Backend_PushGlobal(jit_backend *Backend)
{
    i32 Offset = Backend->GlobalDataSize;
    Backend->GlobalDataSize += Backend->DataSize;
    return Offset;
}

i32 Backend_AllocateGlobal(jit_backend *Backend, double InitialValue)
{
    i32 Offset = Backend_PushGlobal(Backend);
    u8 *Global = Backend->GlobalData;
    if (Backend->DataSize == sizeof(double))
    {
        MemCpy(Global + Offset, &InitialValue, sizeof(double));
    }
    else
    {
        float Value = InitialValue;
        MemCpy(Global + Offset, &Value, sizeof(float));
    }
    return Offset;
}

static jit_mem Backend_AllocateStack(jit_backend *Backend)
{
    /* mem = [rbp - stacksize} */
    jit_mem Mem = {
        .Offset = -Backend_PushStack(Backend, 1),
        .BaseReg = RBP,
    };
    return Mem;
}


static jit_reg Backend_TmpReg(jit_backend *Backend)
{
    jit_reg Reg = Backend_FindFreeReg(Backend);
    if (JIT_REG_INVALID == Reg)
    {
        Backend_SpillReg(Backend, 1);
        Reg = Backend_FindFreeReg(Backend);
        ASSERT(Reg != JIT_REG_INVALID, "unreachable");
    }
    return Reg;
}

static void Backend_SpillReg(jit_backend *Backend, int NumberOfRegToSpill)
{
    for (int i = 0, 
        SpillCount = 0; 
        SpillCount < NumberOfRegToSpill 
        && i < REG_COUNT; 
        i++)
    {
        if (NULL == Backend->RegLocation[i])
        {
            continue;
        }

        jit_location *Location = Backend->RegLocation[i];
        ASSERT(LOCATION_REG == Location->Type, "location is not a reg");
        ASSERT(IN_RANGE(0, Backend_EvalStack_IndexOf(Backend, Location), Backend_EvalStack_Count(Backend) - 1), 
            "reg location is not in stack"
        );

        jit_mem SaveLocation = Backend_AllocateStack(Backend);
        Backend_EmitStore(Backend, Location, SaveLocation);
        Backend_DeallocateReg(Backend, Location->As.Reg);

        *Location = LocationFromMem(SaveLocation);
        SpillCount++;
    }
}


static jit_reg Backend_EmitCopyToReg(jit_backend *Backend, jit_reg Reg, const jit_location *Location)
{
    switch (Location->Type)
    {
    case LOCATION_REF:
    {
        jit_location *Org = Location->As.Ref;
        Backend_EmitCopyToReg(Backend, Reg, Org);
    } break;
    case LOCATION_MEM:
    {
        Backend_EmitLoad(Backend, Reg, Location->As.Mem.BaseReg, Location->As.Mem.Offset);
    } break;
    case LOCATION_REG:
    {
        Backend_EmitMove(Backend, Reg, Location->As.Reg);
    } break;
    }
    return Reg;
}
static void Backend_EmitMoveToReg(jit_backend *Backend, jit_reg Dst, const jit_location *Src)
{
    switch (Src->Type)
    {
    case LOCATION_REF:
    {
        ASSERT(!Backend_RegIsBusy(Backend, Dst), "Reg must not be busy");
        Backend_EmitMoveToReg(Backend, Dst, Src->As.Ref);
    } break;
    case LOCATION_MEM:
    {
        ASSERT(!Backend_RegIsBusy(Backend, Dst), "Reg must not be busy");
        Backend_EmitLoad(Backend, Dst, Src->As.Mem.BaseReg, Src->As.Mem.Offset);
    } break;
    case LOCATION_REG:
    {
        Backend_EmitMove(Backend, Dst, Src->As.Reg);
    } break;
    }
}

static void Backend_EmitCallArgs(jit_backend *Backend, int ArgCount)
{
    int EvalStackCount = Backend_EvalStack_Count(Backend);
    ASSERT(ArgCount <= EvalStackCount, "arg count");

    /* reserve physical stack space for arguments */
    int StackSpace = MAX(4, ArgCount); /* 4 args for shadow space */
    Backend_PushStack(Backend, StackSpace);

    /* emit arguments */
    for (int i = 0; i < ArgCount; i++)
    {
        jit_location Tmp = Backend_EvalStack_Pop(Backend);
        int ArgIndex = ArgCount - i - 1;
        if (Backend_IsArgumentInReg(ArgIndex))
        {
            jit_reg ArgReg = Backend_CallerSideArgReg(ArgIndex);
            Backend_EmitMoveToReg(Backend, ArgReg, &Tmp);
        }
        else
        {
            jit_mem ArgMem = Backend_CallerSideArgMem(ArgIndex);
            Backend_EmitStore(Backend, &Tmp, ArgMem);
        }
    }

    /* pop arguments on the physical stack */
    Backend_PopStack(Backend, StackSpace);

    /* pop arguments on the eval stack */
}





typedef void (*backend_emit_fn)(jit_backend *, jit_reg Dst, jit_reg SrcBase, i32 Offset);
typedef void (*backend_emit_reg_fn)(jit_backend *, jit_reg Dst, jit_reg Src);
void Backend_Op_Binary(
    jit_backend *Backend, 
    backend_emit_fn Emit, 
    backend_emit_reg_fn EmitReg, 
    bool8 Commutative)
{
    /* stack, storage manipulation and register allocation */
    jit_location Right = Backend_EvalStack_Pop(Backend);
    jit_location Left = Backend_EvalStack_Pop(Backend);
    if (Commutative && Left.Type != LOCATION_REG && Right.Type == LOCATION_REG) 
    {
        SWAP(jit_location, Left, Right);
    }

    jit_reg Accum;
    if (LOCATION_REG == Left.Type)
    {
        Accum = Left.As.Reg;
        Backend_EvalStack_Push(Backend, Left);
    }
    else
    {
        Accum = Backend_TmpReg(Backend);
        Backend_EmitMoveToReg(Backend, Accum, &Left); 
        Backend_EvalStack_Push(Backend, LocationFromReg(Accum));
    }


    /* emitting instructions */
    while (LOCATION_REF == Right.Type)
    {
        ASSERT(Right.As.Ref, "nullptr");
        Right = *Right.As.Ref;
    }
    switch (Right.Type)
    {
    case LOCATION_REF:
    {
        UNREACHABLE();
    } break;
    case LOCATION_REG:
    {
        EmitReg(Backend, Accum, Right.As.Reg);
    } break;
    case LOCATION_MEM:
    {
        Emit(Backend, Accum, Right.As.Mem.BaseReg, Right.As.Mem.Offset);
    } break;
    }

}

void Backend_Op_Add(jit_backend *Backend)
{
    Backend_Op_Binary(Backend, Backend_EmitAdd, Backend_EmitAddReg, true);
}

void Backend_Op_Mul(jit_backend *Backend)
{
    Backend_Op_Binary(Backend, Backend_EmitMul, Backend_EmitMulReg, true);
}

void Backend_Op_Div(jit_backend *Backend)
{
    Backend_Op_Binary(Backend, Backend_EmitDiv, Backend_EmitDivReg, false);
}

void Backend_Op_Sub(jit_backend *Backend)
{
    Backend_Op_Binary(Backend, Backend_EmitSub, Backend_EmitSubReg, false);
}

void Backend_Op_Less(jit_backend *Backend)
{
    Backend->CmpType = CMP_LT;
    Backend_Op_Binary(Backend, Backend_EmitCmp, Backend_EmitCmpReg, false);
}
void Backend_Op_LessOrEqual(jit_backend *Backend)
{
    Backend->CmpType = CMP_LE;
    Backend_Op_Binary(Backend, Backend_EmitCmp, Backend_EmitCmpReg, false);
}
void Backend_Op_Greater(jit_backend *Backend)
{
    Backend->CmpType = CMP_NLE;
    Backend_Op_Binary(Backend, Backend_EmitCmp, Backend_EmitCmpReg, false);
}
void Backend_Op_GreaterOrEqual(jit_backend *Backend)
{
    Backend->CmpType = CMP_NLT;
    Backend_Op_Binary(Backend, Backend_EmitCmp, Backend_EmitCmpReg, false);
}

void Backend_Op_Swap(jit_backend *Backend)
{
    jit_location *Right = Backend_EvalStack_Top(Backend, 0);
    jit_location *Left = Backend_EvalStack_Top(Backend, 1);
    SWAP(jit_location, *Left, *Right);
}


void Backend_Op_Neg(jit_backend *Backend)
{
    jit_reg Zero = Backend_TmpReg(Backend);
    Backend_EmitLoadZero(Backend, Zero);
    jit_location Tmp = Backend_EvalStack_Pop(Backend);

    Backend_EvalStack_Push(Backend, LocationFromReg(Zero));
    Backend_EvalStack_Push(Backend, Tmp);
    Backend_Op_Binary(Backend, Backend_EmitSub, Backend_EmitSubReg, false);
}


i32 Backend_Op_FnEntry(jit_backend *Backend, int ParamCount)
{
    Backend_ResetTmp(Backend);
    i32 Location = Backend_EmitFunctionEntry(Backend);

    /* move arguments to eval stack */
    for (int i = 0; i < ParamCount; i++)
    {
        if (Backend_IsArgumentInReg(i))
        {
            jit_reg ArgReg = Backend_CallerSideArgReg(i);
            Backend_EvalStack_Push(Backend, LocationFromReg(ArgReg));
        }
        else
        {
            jit_mem Mem = Backend_CalleeSideParamMem(i);
            Backend_EvalStack_Push(Backend, LocationFromMem(Mem));
        }
    }
    return Location;
}
i32 Backend_Op_FnReturn(jit_backend *Backend, i32 EntryLocation, bool8 HasReturnValue)
{
    if (HasReturnValue)
    {
        jit_location ReturnValue = Backend_EvalStack_Pop(Backend);
        Backend_EmitCopyToReg(Backend, XMM(0), &ReturnValue);
    }

    /* cleanup */

    /* exit */
    Backend_EmitFunctionExit(Backend, EntryLocation);
    return Backend->ProgramSize;
}
void Backend_Op_ArgStart(jit_backend *Backend)
{
    Backend_SpillReg(Backend, Backend->BusyRegCount);
}
i32 Backend_Op_Call(jit_backend *Backend, int ArgCount) /* returns location for patching */
{
    /* emit args */
    Backend_EmitCallArgs(Backend, ArgCount);
    
    /* emit call instruction */
    uint CallLocation = Backend_EmitCall(Backend, 0); /* don't know the location yet */

    /* push return value onto the eval stack */
    Backend_EvalStack_Push(Backend, LocationFromReg(XMM(0)));

    /* return call site for instruction patching */
    return CallLocation;
}

void Backend_Patch_Call(jit_backend *Backend, i32 CallLocation, i32 FunctionLocation)
{
    ASSERT(FunctionLocation < (i32)Backend->ProgramSize, "bad dst location");
    ASSERT(CallLocation + 5 < (i32)Backend->ProgramSize, "bad src location");
    i32 Offset = FunctionLocation - (CallLocation + 5);
    MemCpy(Backend->Program + CallLocation + 1, &Offset, 4);
}



void Backend_Op_LoadLocal(jit_backend *Backend, int LocalIndex)
{
    jit_location *Local = Backend_EvalStack_Array(Backend, LocalIndex);
    jit_location Reference = {
        .Type = LOCATION_REF,
        .As.Ref = Local,
    };
    Backend_EvalStack_Push(Backend, Reference);
}

void Backend_Op_LoadGlobal(jit_backend *Backend, i32 GlobalIndex) /* returns location for patching */ 
{
    jit_location Global = {
        .Type = LOCATION_MEM,
        .As.Mem = {
            .BaseReg = Backend_GlobalPtrReg(),
            .Offset = GlobalIndex,
        },
    };
    Backend_EvalStack_Push(Backend, Global);
}

void Backend_Op_StoreGlobal(jit_backend *Backend, i32 GlobalIndex)
{
    jit_location ValueToStore = Backend_EvalStack_Pop(Backend);
    jit_mem Dst = {
        .BaseReg = Backend_GlobalPtrReg(), 
        .Offset = GlobalIndex,
    };

    Backend_EmitStore(Backend, &ValueToStore, Dst);
}

i32 Backend_GetProgramSize(const jit_backend *Backend)
{
    return Backend->ProgramSize;
}

const u8 *Backend_GetProgramPtr(const jit_backend *Backend)
{
    return Backend->Program;
}

void *Backend_GetDataPtr(jit_backend *Backend)
{
    return Backend->GlobalData;
}

int Backend_GetBufferSize(const jit_backend *Backend)
{
    return Backend->ProgramSize;
}

const u8 *Backend_GetBuffer(const jit_backend *Backend)
{
    return Backend->Program;
}




/* ======================================== */
/*              Disassembler                */
/* ======================================== */


#define PEEK(p_dd, offset) ((p_dd)->InstructionSize + (offset) < (p_dd)->MemoryCapacity\
    ? (p_dd)->Memory[(p_dd)->InstructionSize + (offset)]\
    : 0)

static u8 ConsumeByte(disasm_data *Data)
{
    u8 Byte = PEEK(Data, 0);
    Data->InstructionSize++;
    return Byte;
}

static u32 ConsumeDWord(disasm_data *Data)
{
    u32 DWord = ConsumeByte(Data);
    DWord |= (u32)ConsumeByte(Data) << 8;
    DWord |= (u32)ConsumeByte(Data) << 16;
    DWord |= (u32)ConsumeByte(Data) << 24;
    return DWord;
}

static void WriteInstruction(disasm_data *Data, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    int PotentialBytesWritten = vsnprintf(
        Data->DisasmBuffer + Data->DisasmBufferSize, 
        Data->DisasmBufferCap - Data->DisasmBufferSize,
        Fmt, Args
    );
    va_end(Args);

    if (PotentialBytesWritten + Data->DisasmBufferSize > Data->DisasmBufferCap)
    {
        Data->DisasmBufferSize = Data->DisasmBufferCap;
    }
    else
    {
        Data->DisasmBufferSize += PotentialBytesWritten;
    }
}

static i32 X64ModOffset(disasm_data *Data, uint Mod)
{
    if (0 == Mod)
        return 0;
    if (1 == Mod)
    {
        u32 Value = ConsumeByte(Data);
        return Value | ~((Value & 0x80) - 1);
    }
    return ConsumeDWord(Data);
}

static void X64DisasmModRM(disasm_data *Data, disasm_modrm_type Type, const char **Regs)
{
#define WRITE_OPERANDS(dststr, dstreg, srcstr, ...) do {\
    if (Type == MODRM_DST_SRC) {\
        WriteInstruction(Data, \
                dststr", "srcstr, \
                dstreg, __VA_ARGS__);\
    } else {\
        WriteInstruction(Data, \
                srcstr", "dststr, \
                __VA_ARGS__, dstreg);\
    }\
} while (0)
#define SIGNED_NUMBER(n) ((n) < 0? '-' : '+'), ABS(n)

    u8 ModRM = ConsumeByte(Data);
    uint Mod = ModRM >> 6;
    reg_index Reg = (ModRM >> 3) & 0x7;
    reg_index Rm = (ModRM) & 0x7;

    if (0x3 == Mod) /* reg mode */
    {
        WRITE_OPERANDS("%s", Regs[Reg], "%s", Regs[Rm]);
    }
    else if (0x4 == Rm) /* SIB byte present */
    {
        u8 SIB = ConsumeByte(Data);
        uint Scale = 1 << (SIB >> 6);
        reg_index Index = (SIB >> 3) & 0x7;
        reg_index Base = (SIB & 0x7);
        i32 Offset = X64ModOffset(Data, Mod);

        if (RSP == Index) /* no index reg */
        {
            if (0 == Mod) /* no displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s]", sIntReg[Base]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s %c 0x%x]", sIntReg[Base], SIGNED_NUMBER(Offset));
            }
        }
        else /* SIB with index */
        {
            if (0 == Mod) /* no displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %x*%s]", sIntReg[Base], Scale, sIntReg[Index]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %x*%s %c 0x%x]", sIntReg[Base], Scale, sIntReg[Index], SIGNED_NUMBER(Offset));
            }
        }
    }
    else if (0 == Mod && 0x5 == Rm) /* displacement only (rip-relative in 64 bit mode) */
    {
        i32 DWord = ConsumeDWord(Data);
        WRITE_OPERANDS("%s", Regs[Reg], "[rip %c 0x%x]", SIGNED_NUMBER(DWord));
    }
    else /* [register]/[register + displacement] */
    {
        if (0 == Mod) /* no displacement */
        {
            WRITE_OPERANDS("%s", Regs[Reg], "[%s]", sIntReg[Rm]);
        }
        else /* 8/32 bit displacement */
        {
            i32 Offset = X64ModOffset(Data, Mod);
            WRITE_OPERANDS("%s", sXmmReg[Reg], "[%s %c 0x%x]", sIntReg[Rm], SIGNED_NUMBER(Offset));
        }
    }
#undef WRITE_OPERANDS
#undef SIGNED_NUMBER
}


static i32 DisasmSingleInstruction(u64 Addr, const u8 *Memory, i32 MemorySize, char ResultBuffer[64])
{
    ASSERT(MemorySize > 0, "bad memory size");
    disasm_data Disasm = {
        .Memory = Memory,
        .MemoryCapacity = MemorySize,
        .DisasmBuffer = ResultBuffer,
        .DisasmBufferCap = 64,
    };

    u8 FirstByte = ConsumeByte(&Disasm);
    bool8 Unknown = false;
    switch (FirstByte)
    {
    case 0x50 + RBP: WriteInstruction(&Disasm, "push rbp"); break;
    case 0xC3: WriteInstruction(&Disasm, "ret"); break;
    case 0xC9: WriteInstruction(&Disasm, "leave"); break;
    case 0x90: WriteInstruction(&Disasm, "nop"); break;
    case 0xE8:
    {
        i32 Rel32 = ConsumeDWord(&Disasm);
        u64 Dst = Addr + 5 + (i64)Rel32;
        WriteInstruction(&Disasm, "call %016x", Dst);
    } break;
    case 0xE9:
    {
        i32 Rel32 = ConsumeDWord(&Disasm);
        u64 Dst = Addr + 5 + (i64)Rel32;
        WriteInstruction(&Disasm, "jmp %016x", Dst);
    } break;
    case 0x48:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x89 == Second) /* mov rm, r */
        {
            WriteInstruction(&Disasm, "mov ");
            X64DisasmModRM(&Disasm, MODRM_SRC_DST, sIntReg);
        }
        else if (0x81 == Second) /* sub rm, imm32 */
        {
            u8 ModRm = ConsumeByte(&Disasm);
            uint Mod = ModRm >> 6;
            uint Reg = (ModRm >> 3) & 0x7;
            uint Rm = ModRm & 0x7;
            if (0x3 == Mod && 5 == Reg) /* sub r, imm */
            {
                WriteInstruction(&Disasm, "sub ");
                i32 Immediate = ConsumeDWord(&Disasm);
                WriteInstruction(&Disasm, "%s, 0x%x", sIntReg[Rm], Immediate);
            }
            else Unknown = true;
        }
        else if (0x83 == Second) 
        {
            u8 ModRm = ConsumeByte(&Disasm);
            uint Mod = ModRm >> 6;
            uint Reg = (ModRm >> 3) & 0x7;
            uint Rm = ModRm & 0x7;
            if (0x3 == Mod && 5 == Reg) /* sub r, imm */
            {
                WriteInstruction(&Disasm, "sub ");
                i32 Immediate = (i32)(i8)ConsumeByte(&Disasm);
                WriteInstruction(&Disasm, "%s, 0x%x", sIntReg[Rm], Immediate);
            }
            else Unknown = true;
        }
        else Unknown = true;
    } break;
    case 0x0F:
    {
        u8 Second = ConsumeByte(&Disasm);
        switch (Second)
        {
        case 0x57:
        {
            WriteInstruction(&Disasm, "xorps ");
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        } break;
        case 0x28:
        {
            WriteInstruction(&Disasm, "movaps ");
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        } break;
        case 0x54:
        {
            WriteInstruction(&Disasm, "andps ");
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        } break;
        default:
        {
            Unknown = true;
        } break;
        }
    } break;
    case 0xF2:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            switch (ConsumeByte(&Disasm))
            {
            case 0x58: WriteInstruction(&Disasm, "addsd "); break;
            case 0x5C: WriteInstruction(&Disasm, "subsd "); break;
            case 0x59: WriteInstruction(&Disasm, "mulsd "); break;
            case 0x5E: WriteInstruction(&Disasm, "divsd "); break;
            case 0x51: WriteInstruction(&Disasm, "sqrtsd "); break;
            case 0xC2: 
            {
                WriteInstruction(&Disasm, "cmpsd ");
                X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
                sse_cmp_type CmpType = ConsumeByte(&Disasm);
                WriteInstruction(&Disasm, ", ");
                WriteInstruction(&Disasm, sCmpType[CmpType]);
            } goto Out;
            default:  
            {
                Unknown = true;
            } goto Out;
            }

            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        }
        else Unknown = true;
    } break;
    case 0xF3:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            u8 Third = ConsumeByte(&Disasm);
            switch (Third)
            {
            case 0x58: WriteInstruction(&Disasm, "addss "); break;
            case 0x5C: WriteInstruction(&Disasm, "subss "); break;
            case 0x59: WriteInstruction(&Disasm, "mulss "); break;
            case 0x5E: WriteInstruction(&Disasm, "divss "); break;
            case 0x51: WriteInstruction(&Disasm, "sqrtss "); break;
            case 0x7E: WriteInstruction(&Disasm, "movq "); break;
            case 0xC2: 
            {
                WriteInstruction(&Disasm, "cmpss ");
                X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
                sse_cmp_type CmpType = ConsumeByte(&Disasm);
                WriteInstruction(&Disasm, ", ");
                WriteInstruction(&Disasm, sCmpType[CmpType]);
            } goto Out;
            default: 
            {
                Unknown = true;
            } goto Out;
            }
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        }
        else Unknown = true;
    } break;
    case 0x66:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            u8 Third = ConsumeByte(&Disasm);
            switch (Third)
            {
            case 0xD6: 
            {
                WriteInstruction(&Disasm, "movq ");
                X64DisasmModRM(&Disasm, MODRM_SRC_DST, sXmmReg);
            } break;
            case 0x6E:
            {
                WriteInstruction(&Disasm, "movd ");
                X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
            } break;
            case 0x7E:
            {
                WriteInstruction(&Disasm, "movd ");
                X64DisasmModRM(&Disasm, MODRM_SRC_DST, sXmmReg);
            } break;
            default: 
            {
                Unknown = true;
            } goto Out;
            }
        }
        else Unknown = true;
    } break;
    }

Out: 
    if (Unknown)
    {
        WriteInstruction(&Disasm, "???");
        Disasm.InstructionSize++;
    }
    return Disasm.InstructionSize;
}

void Backend_Disassemble(const jit_backend *Backend, const def_table *Global)
{
    const def_table_entry *Entry = Global->Head;
    const u8 *Program = Backend->Program;
    int BytesPerLine = 10;
    printf("=============== Instructions ===============\n");
    while (Entry)
    {
        switch (Entry->Type)
        {
        case TYPE_FUNCTION:
        {
            const jit_function *Function = &Entry->As.Function;

            char Instruction[64];
            i32 Addr = Function->Location;
            i32 i = 0;
            printf("\n%016x: <%.*s>\n", Addr, Entry->As.Function.Name.Len, Entry->As.Function.Name.Ptr);
            while (i < Function->InsByteCount)
            {
                int InstructionSize = DisasmSingleInstruction(
                    Addr, 
                    Program + Addr, 
                    Function->InsByteCount - i, 
                    Instruction
                );
                printf("%8x: ", Addr);
                int k = 0;
                for (; k < InstructionSize; k++)
                {
                    printf("%02x ", Program[Addr + k]);
                }
                for (; k < BytesPerLine; k++)
                {
                    printf("   ");
                }
                printf(" %s\n", Instruction);

                Addr += InstructionSize;
                i += InstructionSize;
            }
        } break;
        case TYPE_VARIABLE:
        {
            /* skip, will be disassembled in init */
        } break;
        }

        Entry = Entry->Next;
    }

    printf("=============== Globals ===============\n");
    i32 Count = Backend->GlobalDataSize / Backend->DataSize;
    if (sizeof(double) == Backend->DataSize)
    {
        double *Ptr = Backend->GlobalData;
        for (i32 i = 0; i < Count; i++)
        {
            printf("[rcx + 0x%x] = %f\n", i * Backend->DataSize, Ptr[i]);
        }
    }
    else
    {
        float *Ptr = Backend->GlobalData;
        for (i32 i = 0; i < Count; i++)
        {
            printf("[rcx + 0x%x] = %f\n", i * Backend->DataSize, Ptr[i]);
        }
    }
    
}

#undef PEEK
#undef PROLOGUE_SIZE

