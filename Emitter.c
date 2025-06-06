
#include <stdarg.h>
#include <stdio.h>
#include "Emitter.h"

#define RM(v) ((v) & 0x7)
#define REG(v) (((v) & 0x7) << 3)
#define MODRM(mod, reg, rm) \
    (((mod) & 0x3) << 6)\
    | REG(reg) | RM(rm)
#define SIB(scale, index, base) \
    (((scale) & 0x3) << 6)\
    | ((index) & 0x7) << 3\
    | ((base) & 0x7) 



typedef struct disasm_data
{
    u8 *Memory;
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
static const u8 sPrologue[] = {
    0x50 + RBP,                                 /* push rbp */
    0x48, 0x89, MODRM(3, RSP, RBP),             /* mov rbp, rsp */
    0x48, 0x81, MODRM(3, 5, RSP), 0, 0, 0, 0    /* sub rsp, 0 */
};
static const u8 sStoreSingle32[] = {
    0x66, 0x0F, 0x7E,
};
static const u8 sStoreSingle64[] = {
    0x66, 0xF, 0xD6,
};
static const u8 sLoadSingle32[] = {
    0x66, 0x0F, 0x6E,
};
static const u8 sLoadSingle64[] = {
    0xF3, 0x0F, 0x7E
};

static int Emit(jit_emitter *Emitter, int Count, ...)
{
    int InstructionOffset = Emitter->BufferSize;
    va_list Args;
    va_start(Args, Count);
    for (int i = 0; i < Count && (uint)Emitter->BufferSize < Emitter->BufferCapacity; i++)
    {
        Emitter->Buffer[Emitter->BufferSize++] = va_arg(Args, uint);
    }
    va_end(Args);
    return InstructionOffset;
}

static int EmitArray(jit_emitter *Emitter, const u8 Array[], int Count)
{
    int InstructionOffset = Emitter->BufferSize;
    for (int i = 0; i < Count && (uint)Emitter->BufferSize < Emitter->BufferCapacity; i++)
    {
        Emitter->Buffer[Emitter->BufferSize++] = Array[i];
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
static void Emit_GenericModRm(jit_emitter *Emitter, int Reg, int Rm, i32 Displacement)
{
    int Mod = ModFromDisplacement(Displacement);
    if (RBP == Rm && 0 == Displacement)
        Mod = 1;
    u8 ModRm = MODRM(Mod, Reg, Rm);

    Emit(Emitter, 1, ModRm);
    if (RSP == Rm)
    {
        Emit(Emitter, 1, SIB(0, RSP, RSP));
    }
    switch (Mod)
    {
    case 0:
    case 3:
        break; /* no displacement to emit */
    case 1: Emit(Emitter, 1, Displacement); break;
    case 2: EmitArray(Emitter, (u8 *)&Displacement, sizeof Displacement); break;
    }
}

static int Emit_FloatOpcode(jit_emitter *Emitter, u8 Opcode, int DstReg, int SrcBase, i32 SrcOffset)
{
    int InstructionOffset = Emit(Emitter, 3, Emitter->FloatOpcode, 0x0F, Opcode);
    Emit_GenericModRm(Emitter, DstReg, SrcBase, SrcOffset);
    return InstructionOffset;
}

static void Emit_FloatOpcodeReg(jit_emitter *Emitter, u8 Opcode, int DstReg, int SrcReg)
{
    u8 ModRm = MODRM(3, DstReg, SrcReg);
    Emit(Emitter, 4, Emitter->FloatOpcode, 0x0F, Opcode, ModRm);
}



/* ms x64 calling conv */
int TargetEnv_GetShadowSpaceSize(void)
{
    return 32;
}
/* align to 16-byte boundary */
int TargetEnv_AlignStackSize(int StackSize)
{
    if (StackSize % 16)
        return ROUND_TO_MULTIPLE(StackSize, 16);
    return StackSize;
}
/* MS x64 first argument */
int TargetEnv_GetGlobalPtrReg(void)
{
    return RCX;
}
/* base pointer */
int TargetEnv_GetStackFrameReg(void)
{
    return RBP;
}
int TargetEnv_GetArgStackSize(int ArgCount, int DataSize)
{
    ASSERT(DataSize == sizeof(double) || DataSize == sizeof(float), "invalid data size");
    /* rcx as global ptr reg, but we won't store it on stack */
    int StackSize = (ArgCount + 1) * DataSize;
    if (StackSize < 32)
        return 32;
    return StackSize;
}
int TargetEnv_GetArgRegCount(void)
{
    /* rcx taken as global ptr reg */
    /* xmm1 = arg0  */
    /* xmm2 = arg1  */
    /* xmm3 = arg2  */
    /* rest are on stack */
    return 3;
}
int TargetEnv_GetArgBaseReg(void)
{
    return RBP;
}
int TargetEnv_GetArgReg(int ArgIndex)
{
    ASSERT(ArgIndex < 4, "bad arg index");
    return ArgIndex + 1;
}
int TargetEnv_GetArgOffset(int StackTop, int ArgIndex, int DataSize)
{
    ASSERT(DataSize == sizeof(double) || DataSize == sizeof(float), "invalid data size");
    ASSERT(ArgIndex >= 4, "bad arg index");
    return (ArgIndex + 1) * DataSize + StackTop;
}
int TargetEnv_GetReturnReg(void)
{
    return 0; /* xmm0 */
}




jit_emitter Emitter_Init(u8 *Buffer, uint BufferCapacity)
{
    jit_emitter Emitter = { 
        .Buffer = Buffer,
        .BufferCapacity = BufferCapacity,
    };
    return Emitter;
}

void Emitter_Reset(jit_emitter *Emitter, bool8 EmitFloat32Instructions)
{
    STATIC_ASSERT(sizeof sStoreSingle32 == sizeof Emitter->StoreSingle, "unreachable");
    STATIC_ASSERT(sizeof sStoreSingle64 == sizeof Emitter->StoreSingle, "unreachable");
    STATIC_ASSERT(sizeof sLoadSingle32 == sizeof Emitter->LoadSingle, "unreachable");
    STATIC_ASSERT(sizeof sLoadSingle64 == sizeof Emitter->LoadSingle, "unreachable");

    Emitter->BufferSize = 0;
    if (EmitFloat32Instructions)
    {
        Emitter->FloatOpcode = 0xF3;
        MemCpy(Emitter->StoreSingle, sStoreSingle32, sizeof sStoreSingle32);
        MemCpy(Emitter->LoadSingle, sLoadSingle32, sizeof sLoadSingle32);
    }
    else
    {
        Emitter->FloatOpcode = 0xF2;
        MemCpy(Emitter->StoreSingle, sStoreSingle64, sizeof sStoreSingle64);
        MemCpy(Emitter->LoadSingle, sLoadSingle64, sizeof sLoadSingle64);
    }
}



void Emit_Move(jit_emitter *Emitter, int DstReg, int SrcReg)
{
    if (DstReg != SrcReg)
    {
        /* movaps dst, src */
        Emit(Emitter, 3, 0x0F, 0x28, MODRM(3, DstReg, SrcReg));
    }
}

void Emit_Store(jit_emitter *Emitter, int SrcReg, int DstBase, i32 SrcOffset)
{
    /* movq [base + offset], src */
    EmitArray(Emitter, 
        Emitter->StoreSingle,
        sizeof Emitter->StoreSingle
    );
    Emit_GenericModRm(Emitter, SrcReg, DstBase, SrcOffset);
}

void Emit_Load(jit_emitter *Emitter, int DstReg, int SrcBase, i32 SrcOffset)
{
    /* emit it as a store instruction initially */
    uint PrevSize = EmitArray(Emitter, 
        Emitter->StoreSingle, 
        sizeof Emitter->StoreSingle
    );
    Emit_GenericModRm(Emitter, DstReg, SrcBase, SrcOffset);
    uint InstructionLength = Emitter->BufferSize - PrevSize;

    /* compared the emitted instruction with the last*/
    u8 *PrevIns = Emitter->Buffer + PrevSize - InstructionLength;
    u8 *CurrIns = Emitter->Buffer + PrevSize;
    if (Emitter->BufferSize >= 2*InstructionLength
    && MemEqu(PrevIns, CurrIns, InstructionLength))
    {
        /* if matched, 
         * this current instruction was trying to read the value we just stored into the same regsiter, 
         * optimize it out */
        Emitter->BufferSize -= InstructionLength;
    }
    else
    {
        /* otherwise, change the emitted instruction into a load */
        for (uint i = 0; i < sizeof Emitter->LoadSingle; i++)
            CurrIns[i] = Emitter->LoadSingle[i];
    }
}


void Emit_Add(jit_emitter *Emitter, int Dst, int Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x58, Dst, Base, Offset);
}

void Emit_Sub(jit_emitter *Emitter, int Dst, int Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x5C, Dst, Base, Offset);
}

void Emit_Mul(jit_emitter *Emitter, int Dst, int Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x59, Dst, Base, Offset);
}

void Emit_Div(jit_emitter *Emitter, int Dst, int Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x5E, Dst, Base, Offset);
}


void Emit_AddReg(jit_emitter *Emitter, int Dst, int Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x58, Dst, Src);
}

void Emit_SubReg(jit_emitter *Emitter, int Dst, int Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x5C, Dst, Src);
}

void Emit_MulReg(jit_emitter *Emitter, int Dst, int Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x59, Dst, Src);
}

void Emit_DivReg(jit_emitter *Emitter, int Dst, int Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x5E, Dst, Src);
}

void Emit_LoadZero(jit_emitter *Emitter, int DstReg)
{
    /* xorps dst, dst */
    Emit(Emitter, 3, 0x0F, 0x57, MODRM(3, DstReg, DstReg));
}




uint Emit_FunctionEntry(jit_emitter *Emitter, jit_variable *Params, int ParamCount)
{
    /* push rbp 
     * mov rbp, rsp
     * sub rsp, memsize
     * ; x64 windows: using shadow space of 32 bytes below return addr as argument storage
     * ; arg[i] = [rbp + 0x18 + i*8]
     * movsd [rbp + i*8 + 0x18], xmm(i) 
     * ...
     * */
    while (Emitter->BufferSize % 0x10 != 0)
    {
        Emit(Emitter, 1, 0x90); /* nop */
    }
    uint FunctionLocation = EmitArray(Emitter, sPrologue, sizeof sPrologue);
    for (int i = 0; i < ParamCount; i++)
    {
        /* 0x10 = rbp and retaddr, 
         * 0x8 = shadow space for first argument (rcx/global ptr reg) */
        int Displacement = 0x10 + 0x8 + i*sizeof(double); 
        if (i < TargetEnv_GetArgRegCount())
        {
            Emit_Store(Emitter, TargetEnv_GetArgReg(i), RBP, Displacement);
        }

        Params[i].Expr = (jit_expression) {
            .Storage = STORAGE_MEM,
            .As.Mem = {
                .Offset = Displacement,
                .BaseReg = RBP
            },
        };
    }
    return FunctionLocation;
}

void Emit_FunctionExit(jit_emitter *Emitter)
{
    /* leave 
     * ret 
     */
    Emit(Emitter, 2, 0xC9, 0xC3);
}

void Emit_PatchStackSize(jit_emitter *Emitter, uint FunctionLocation, i32 Value)
{
    ASSERT(FunctionLocation + sizeof(sPrologue) <= Emitter->BufferCapacity, "bad function location");
    u8 *Location = Emitter->Buffer 
        + FunctionLocation 
        + sizeof(sPrologue) - 4;
    MemCpy(Location, &Value, 4);
}

void Emit_Call(jit_emitter *Emitter, uint FunctionLocation)
{
    /* call rel32 */
    i32 Rel32 = FunctionLocation - (Emitter->BufferSize + 5);
    Emit(Emitter, 1, 0xE8);
    EmitArray(Emitter, (u8 *)&Rel32, sizeof Rel32);
}

int Emit_Jump(jit_emitter *Emitter)
{
    /* jmp 0 */
    u32 Location = 0;
    int PatchLocation = Emit(Emitter, 1, 0xE9);
    EmitArray(Emitter, (u8 *)&Location, sizeof Location);
    return PatchLocation;
}

void Emitter_PatchJump(jit_emitter *Emitter, uint JumpInsLocation, uint Dst)
{
    ASSERT(JumpInsLocation < Emitter->BufferCapacity, "patch jump instruction location");
    i32 Rel32 = (i64)Dst - (i64)(JumpInsLocation + 5);
    MemCpy(Emitter->Buffer + JumpInsLocation + 1, &Rel32, sizeof Rel32);
}


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


uint DisasmSingleInstruction(u64 Addr, u8 *Memory, int MemorySize, char ResultBuffer[64])
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
        else if (0x81 == Second)
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

#undef PEEK


