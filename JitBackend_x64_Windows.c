
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


static jit_reg Backend_AllocateReg(jit_backend *Backend, jit_ir_stack *Stack);
static void Backend_DeallocateReg(jit_backend *Backend, jit_reg Reg);


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
static void Backend_EmitGenericModRm(jit_backend *Backend, int Reg, int Rm, i32 Displacement)
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
    switch (Mod)
    {
    case 0:
    case 3:
        break; /* no displacement to emit */
    case 1: EmitCode(Backend, 1, Displacement); break;
    case 2: EmitCodeSequence(Backend, (u8 *)&Displacement, sizeof Displacement); break;
    }
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





void Backend_Init(
    jit_backend *Backend, 
    u8 *ProgramBuffer, i32 ProgramBufferSizeByte,
    void *GlobalDataBuffer, i32 GlobalDataSizeByte
)
{
    ASSERT(NULL != Backend, "nullptr");
    *Backend = (jit_backend) {
        .Program = ProgramBuffer,
        .ProgramCapacity = ProgramBufferSizeByte, 
        .GlobalData = GlobalDataBuffer,
        .GlobalDataCapacity = GlobalDataSizeByte,
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

    Backend->One = (jit_mem) {
        .BaseReg = Backend_GetGlobalPtrReg(),
        .Offset = Backend_AllocateGlobal(Backend, 1.0),
    };
}



static void Backend_EmitMove(jit_backend *Backend, jit_reg DstReg, jit_reg SrcReg)
{
    if (DstReg != SrcReg)
    {
        /* movaps dst, src */
        EmitCode(Backend, 3, 0x0F, 0x28, MODRM(3, DstReg, SrcReg));
    }
}

static void Backend_EmitStore(jit_backend *Backend, jit_reg SrcReg, jit_mem Mem)
{
    /* movq [base + offset], src */
    EmitCodeSequence(Backend, 
        Backend->StoreSingle,
        sizeof Backend->StoreSingle
    );
    Backend_EmitGenericModRm(Backend, SrcReg, Mem.BaseReg, Mem.Offset);
}

static void Backend_EmitLoad(jit_backend *Backend, jit_reg DstReg, jit_reg SrcBase, i32 SrcOffset)
{
    /* emit it as a store instruction initially */
    uint PrevSize = EmitCodeSequence(Backend, 
        Backend->StoreSingle, 
        sizeof Backend->StoreSingle
    );
    Backend_EmitGenericModRm(Backend, DstReg, SrcBase, SrcOffset);
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

static sse_cmp_type Backend_GetCmpType(jit_ir_op_type Type)
{
    switch (Type)
    {
    case IR_OP_LESS: return CMP_LT;
    case IR_OP_GREATER: return CMP_NLE;
    case IR_OP_LESS_EQUAL: return CMP_LE;
    case IR_OP_GREATER_EQUAL: return CMP_NLT;
    default: UNREACHABLE();
    }
    return 0;
}

static void Backend_EmitCmpReg(jit_backend *Backend, jit_reg Dst, jit_reg B, sse_cmp_type CmpType)
{
    /* cmpss dst, b */
    Backend_EmitFloatOpcodeReg(Backend, 0xC2, Dst, B);
    EmitCode(Backend, 1, CmpType);

    /* movd tmp, 1.0f */
    Backend_EmitLoad(Backend, B, Backend->One.BaseReg, Backend->One.Offset);

    /* andps dst, tmp */
    EmitCode(Backend, 3, 0x0F, 0x54, MODRM(3, Dst, B));
}




uint Backend_EmitFunctionEntry(jit_backend *Backend)
{
    /* reset stack */
    Backend->MaxStackSize = 0;
    Backend->StackSize = 0;

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


void Backend_EmitFunctionExit(jit_backend *Backend, uint EntryLocation)
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

void Backend_PatchCall(jit_backend *Backend, uint CallLocation, uint FunctionLocation)
{
    ASSERT(FunctionLocation < Backend->ProgramSize, "bad dst location");
    ASSERT(CallLocation + 5 <  Backend->ProgramSize, "bad src location");
    i32 Offset = FunctionLocation - (CallLocation + 5);
    MemCpy(Backend->Program + CallLocation + 1, &Offset, 4);
}



/* ======================================================================
 *                           Register allocation
 * ====================================================================== */

#define SPILL_ALL INT_MAX
static void Backend_SpillReg(jit_backend *Backend, jit_ir_stack *Stack, int RegCount);

static jit_reg Backend_TryAllocateReg(jit_backend *Backend)
{
    for (jit_reg i = 0; i < REG_COUNT; i++)
    {
        if (!Backend->BusyReg[i])
        {
            Backend->BusyReg[i] = true;
            Backend->BusyRegCount++;
            return i;
        }
    }
    return JIT_REG_INVALID;
}

static void Backend_ForceAllocateReg(jit_backend *Backend, jit_reg Reg)
{
    ASSERT(Reg < REG_COUNT, "unreachable");
    Backend->BusyRegCount += Backend->BusyReg[Reg];
    Backend->BusyReg[Reg] = true;
}

static void Backend_DeallocateReg(jit_backend *Backend, jit_reg Reg)
{
    ASSERT(Reg < REG_COUNT, "invalid reg");

    Backend->BusyRegCount -= Backend->BusyReg[Reg];
    Backend->BusyReg[Reg] = false;
}




static i32 Backend_PushStack(jit_backend *Backend, int DataCount)
{
    /* allocate some stack space, 
     * ensure max stack size is always aligned to 16-byte boundary (required by windows) */
    int Size = DataCount * Backend->DataSize;
    Backend->StackSize += Size;
    int AlignedStackSize = ROUND_UP_TO_MULTIPLE(Backend->StackSize, 16);
    Backend->MaxStackSize = MAX(AlignedStackSize, Backend->MaxStackSize);

    return Backend->StackSize;
}

static void Backend_PopStack(jit_backend *Backend, int DataCount)
{
    int PopSize = DataCount * Backend->DataSize;
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

static jit_reg Backend_AllocateReg(jit_backend *Backend, jit_ir_stack *Stack)
{
    jit_reg Reg = Backend_TryAllocateReg(Backend);
    if (JIT_REG_INVALID == Reg)
    {
        Backend_SpillReg(Backend, Stack, 1);
        Reg = Backend_TryAllocateReg(Backend);
        ASSERT(Reg != JIT_REG_INVALID, "unreachable");
    }
    return Reg;
}


static void Backend_SpillReg(jit_backend *Backend, jit_ir_stack *Stack, int NumberOfRegToSpill)
{
    for (int i = 0; 
        i < Stack->Count 
        && NumberOfRegToSpill > 0; 
        i++)
    {
        /* TODO: spill the furthest instead of the most recent element? */
        jit_location *Elem = Ir_Stack_Top(Stack, i);
        switch (Elem->Type)
        {
        case LOCATION_MEM: break; /* already in memory, nothing to do */
        case LOCATION_REG: /* in register, spill out to stack */
        {
            jit_reg Src = Elem->As.Reg;
            jit_mem SaveLocation = Backend_AllocateStack(Backend);
            Backend_EmitStore(Backend, Src, SaveLocation);
            Backend_DeallocateReg(Backend, Src);

            *Elem = LocationFromMem(SaveLocation);
            NumberOfRegToSpill--;
        } break;
        }
    }
}


static jit_reg Backend_EmitMoveToReg(jit_backend *Backend, jit_ir_stack *Stack, const jit_location *Location)
{
    jit_reg Result = -1;
    switch (Location->Type)
    {
    case LOCATION_REG:
    {
        Result = Location->As.Reg;
    } break;
    case LOCATION_MEM:
    {
        Result = Backend_AllocateReg(Backend, Stack);
        Backend_EmitLoad(Backend, Result, Location->As.Mem.BaseReg, Location->As.Mem.Offset);
    } break;
    }
    return Result;
}

static jit_reg Backend_EmitCopyToReg(jit_backend *Backend, jit_reg Reg, const jit_location *Location)
{
    switch (Location->Type)
    {
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

static void Backend_EmitCallArgs(jit_backend *Backend, jit_ir_stack *Stack, int ArgCount)
{
    int IrStackCount = Stack->Count;
    ASSERT(ArgCount <= IrStackCount, "arg count");

    /* reserve stack space for arguments */
    int StackSpace = MAX(4, ArgCount); /* 4 args for shadow space */
    Backend_PushStack(Backend, StackSpace);

    /* emit arguments */
    int NumberOfArgReg = 0;
    for (int i = 0; i < ArgCount; i++)
    {
        jit_location *Location = Ir_Stack_Top(Stack, ArgCount - i - 1);
        if (Backend_IsArgumentInReg(i))
        {
            jit_reg ArgReg = Backend_CallerSideArgReg(i);
            Backend_EmitCopyToReg(Backend, ArgReg, Location);
            Backend_ForceAllocateReg(Backend, ArgReg);
            NumberOfArgReg++;
        }
        else /* argument is in memory */
        {
            jit_reg Tmp = Backend_EmitMoveToReg(Backend, Stack, Location);
            Backend_EmitStore(Backend, Tmp, Backend_CallerSideArgMem(i));
            Backend_DeallocateReg(Backend, Tmp);
        }
    }

    /* deallocate argument registers */
    for (int i = 0; i < NumberOfArgReg; i++)
    {
        jit_reg Reg = Backend_CallerSideArgReg(i);
        Backend_DeallocateReg(Backend, Reg);
    }

    /* pop argument stack */
    Backend_PopStack(Backend, ArgCount);
}








/* ======================================================================
 *                           Ir translation
 * ====================================================================== */

#define IR_CONSUME_BYTE() *IP++
#define IR_CONSUME_AND_INTERPRET(datatype) ((IP += sizeof(datatype)), (datatype *)(IP - sizeof(datatype)))
#define FETCH(typename) *IR_CONSUME_AND_INTERPRET(typename)

const u8 *Backend_TranslateBlock(
    jit_backend *Backend, 
    const u8 *IrBlock, 
    jit_ir_stack *Stack,
    jit_fnref_stack *FnRef,
    jit *Jit)
{
    jit_function *Fn = NULL;
    jit_variable *Var = NULL;
    const u8 *IP = IrBlock;
    const u8 *End = IP + 1 + Bytecode_GetArgSize(IR_OP_FN_BLOCK);
    const u8 *NextBlock = NULL;
    while (IP < End)
    {
        jit_ir_op_type Op = IR_CONSUME_BYTE();

        switch (Op)
        {
        case IR_OP_VAR_BLOCK:
        {
            Var = *IR_CONSUME_AND_INTERPRET(jit_variable *);
            u16 BlockSize = FETCH(u16);
            u16 NextBlockOffset = FETCH(u16);

            NextBlock = Bytecode_GetNextBlock(IP, NextBlockOffset);
            End = Bytecode_GetBlockEnd(IP, BlockSize);

            Var->InsLocation = Backend->ProgramSize;
        } break;
        case IR_OP_FN_BLOCK:
        {
            Fn = *IR_CONSUME_AND_INTERPRET(jit_function *);
            u16 BlockSize = FETCH(u16);
            u16 NextBlockOffset = FETCH(u16);

            NextBlock = Bytecode_GetNextBlock(IP, NextBlockOffset);
            End = Bytecode_GetBlockEnd(IP, BlockSize);

            Fn->Location = Backend_EmitFunctionEntry(Backend);

            /* move arguments to stack */
            for (int i = 0; i < Fn->ParamCount && Backend_IsArgumentInReg(i); i++)
            {
                jit_reg ArgReg = Backend_CallerSideArgReg(i);
                jit_mem Param = Backend_CalleeSideParamMem(i);
                Backend_EmitStore(Backend, ArgReg, Param);
            }
        } break;
        case IR_OP_STORE: /* store value on top of the eval stack to op store's param */
        {
            i32 Offset = FETCH(i32);

            jit_reg Src = Backend_EmitMoveToReg(Backend, Stack, Ir_Stack_Pop(Stack));
            jit_mem Dst = {
                .BaseReg = Backend_GetGlobalPtrReg(),
                .Offset = Offset,
            };
            Backend_EmitStore(Backend, Src, Dst);
            Backend_DeallocateReg(Backend, Src);

            Var->InsByteCount = Backend->ProgramSize - Var->InsLocation;
            Var = NULL;
        } break;
        case IR_OP_RETURN:
        {
            /* copy the topmost elem of the eval stack into return register (xmm0) */
            jit_location *ReturnValue = Ir_Stack_Pop(Stack);
            Backend_EmitCopyToReg(Backend, XMM(0), ReturnValue);
            if (LOCATION_REG == ReturnValue->Type) /* return value had a diff register to the actual return register */
            {
                Backend_DeallocateReg(Backend, ReturnValue->As.Reg);
            }

            /* emit return */
            Backend_EmitFunctionExit(Backend, Fn->Location);

            /* debug info */
            Fn->InsByteCount = Backend->ProgramSize - Fn->Location;
            Fn = NULL;
        } break;

        case IR_OP_SWAP:
        {
            ASSERT(Stack->Count >= 2, "size");
            /* swap */
            jit_location *Left = Ir_Stack_Top(Stack, 0);
            jit_location *Right = Ir_Stack_Top(Stack, 1);
            SWAP(jit_location, *Left, *Right);
        } break;
        case IR_OP_LOAD:
        {
            i32 LoadIndex = FETCH(i32);

            int ParamStart = 0;
            int ParamCount = 0;
            if (Fn)
            {
                ParamStart = Fn->ParamStart;
                ParamCount = Fn->ParamCount;
            }
            jit_location DataLocation = Ir_Data_GetLocation(Jit, LoadIndex, ParamStart, ParamCount);
            Ir_Stack_Push(Stack, &DataLocation);
        } break;
        case IR_OP_CALL_ARG_START:
        {
            /* make all previous location on the stack a memory location */
            Backend_SpillReg(Backend, Stack, SPILL_ALL);
        } break;
        case IR_OP_CALL:
        {
            /* get args */
            const char *FnName = *IR_CONSUME_AND_INTERPRET(const char *);
            i32 FnNameLen = FETCH(i32);
            i32 FnNameLine = FETCH(i32);
            i32 FnNameOffset = FETCH(i32);
            i32 FnArgCount = FETCH(i32);

            const jit_function *Function = Jit_FindFunction(
                Jit, 
                FnName, 
                FnNameLen, 
                FnNameLine, 
                FnNameOffset, 
                FnArgCount
            );
            if (!Function)
                break;

            /* emit the args and call itself */
            /* TODO: couple call and arguments together */
            Backend_EmitCallArgs(Backend, Stack, Function->ParamCount);
            uint CallLocation = Backend_EmitCall(Backend, Function->Location);
            if (INVALID_FN_LOCATION == Function->Location)
            {
                Ir_FnRef_Push(FnRef, Function, CallLocation);
            }

            /* return */
            Backend_ForceAllocateReg(Backend, XMM(0));
            jit_location Result = LocationFromReg(XMM(0));

            /* pop arguments and push return value on the eval stack */
            Ir_Stack_PopMultiple(Stack, Function->ParamCount);
            Ir_Stack_Push(Stack, &Result);
        } break;
        case IR_OP_ADD:
        case IR_OP_SUB:
        case IR_OP_MUL:
        case IR_OP_DIV:
        {
            jit_location *Right = Ir_Stack_Pop(Stack);
            jit_location *Left = Ir_Stack_Pop(Stack);

            /* if commutative and right was in reg first, swap operands */
            if ((IR_OP_ADD == Op || IR_OP_MUL == Op)
            && LOCATION_REG == Right->Type)
            {
                SWAP(jit_location *, Left, Right);
            }

            jit_location Result = LocationFromReg(
                Backend_EmitMoveToReg(Backend, Stack, Left)
            );
            if (Right->Type == LOCATION_REG)
            {
                Backend_DeallocateReg(Backend, Right->As.Reg);
            }

#define OP(op_name, left_reg, right_expr) do {\
    if (LOCATION_MEM == (right_expr)->Type)\
        Backend_Emit ## op_name (Backend, left_reg, (right_expr)->As.Mem.BaseReg, (right_expr)->As.Mem.Offset);\
    else Backend_Emit ## op_name ## Reg(Backend, left_reg, (right_expr)->As.Reg);\
} while (0)
            switch (Op)
            {
            case IR_OP_ADD: OP(Add, Result.As.Reg, Right); break;
            case IR_OP_SUB: OP(Sub, Result.As.Reg, Right); break;
            case IR_OP_MUL: OP(Mul, Result.As.Reg, Right); break;
            case IR_OP_DIV: OP(Div, Result.As.Reg, Right); break;
            default: 
            {
                UNREACHABLE();
            } break;
            }

            Ir_Stack_Push(Stack, &Result);
        } break;
        case IR_OP_LESS:
        case IR_OP_GREATER:
        case IR_OP_LESS_EQUAL:
        case IR_OP_GREATER_EQUAL:
        {
            jit_location *RightLocation = Ir_Stack_Pop(Stack);
            jit_location *LeftLocation = Ir_Stack_Pop(Stack);
            jit_reg Left = Backend_EmitMoveToReg(Backend, Stack, LeftLocation);
            jit_reg Right = Backend_EmitMoveToReg(Backend, Stack, RightLocation);
            jit_location Result = LocationFromReg(Left);

            Backend_EmitCmpReg(Backend, Left, Right, Backend_GetCmpType(Op));

            Backend_DeallocateReg(Backend, Right);
            Ir_Stack_Push(Stack, &Result);
        } break;
        case IR_OP_NEG:
        {
            /* stack.top = 0 - stack.top */
            jit_location *Value = Ir_Stack_Pop(Stack);
            jit_reg ResultReg = Backend_AllocateReg(Backend, Stack);
            Backend_EmitLoadZero(Backend, ResultReg);
            OP(Sub, ResultReg, Value);
            {
                jit_location Result = LocationFromReg(ResultReg);
                Ir_Stack_Push(Stack, &Result);
            }

            if (Value->Type == LOCATION_REG)
            {
                Backend_DeallocateReg(Backend, Value->As.Reg);
            }
        } break;
        }
#undef OP
    }
    return NextBlock;
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

}

#undef PEEK
#undef PROLOGUE_SIZE

