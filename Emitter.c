
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

static int Emit(jit_emitter *Emitter, int Count, ...)
{
    int InstructionOffset = Emitter->InstructionByteCount;
    va_list Args;
    va_start(Args, Count);
    for (int i = 0; i < Count && (uint)Emitter->InstructionByteCount < sizeof(Emitter->InstructionBuffer); i++)
    {
        Emitter->InstructionBuffer[Emitter->InstructionByteCount++] = va_arg(Args, uint);
    }
    va_end(Args);
    return InstructionOffset;
}

static int EmitArray(jit_emitter *Emitter, const u8 Array[], int Count)
{
    int InstructionOffset = Emitter->InstructionByteCount;
    for (int i = 0; i < Count && (uint)Emitter->InstructionByteCount < sizeof(Emitter->InstructionBuffer); i++)
    {
        Emitter->InstructionBuffer[Emitter->InstructionByteCount++] = Array[i];
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
    int InstructionOffset = Emit(Emitter, 3, 0xF2, 0x0F, Opcode);
    Emit_GenericModRm(Emitter, DstReg, SrcBase, SrcOffset);
    return InstructionOffset;
}

static void Emit_FloatOpcodeReg(jit_emitter *Emitter, u8 Opcode, int DstReg, int SrcReg)
{
    u8 ModRm = MODRM(3, DstReg, SrcReg);
    Emit(Emitter, 4, 0xF2, 0x0F, Opcode, ModRm);
}




void Emit_Move(jit_emitter *Emitter, int DstReg, int SrcReg)
{
    if (DstReg != SrcReg)
    {
        Emit_FloatOpcodeReg(Emitter, 0x10, DstReg, SrcReg);
    }
}

void Emit_Store(jit_emitter *Emitter, int SrcReg, int DstBase, i32 SrcOffset)
{
    /* movsd [base + offset], src */
    Emit_FloatOpcode(Emitter, 0x11, SrcReg, DstBase, SrcOffset);
}

void Emit_Load(jit_emitter *Emitter, int DstReg, int SrcBase, i32 SrcOffset)
{
    /* movsd dst, [base + offset] */
    u8 *Curr = Emitter->InstructionBuffer + Emit_FloatOpcode(Emitter, 0x11, DstReg, SrcBase, SrcOffset);
    const u8 *Next = Emitter->InstructionBuffer + Emitter->InstructionByteCount;

    /* if last instruction was a store to the same location that we'll be loading from, 
     * omit the load */
    uint InstructionLength = Next - Curr;
    u8 *Prev = Curr - InstructionLength;
    if (Emitter->InstructionByteCount >= 2*InstructionLength
    && MemEqu(Prev, Curr, InstructionLength))
    {
        Emitter->InstructionByteCount -= InstructionLength;
        return;
    }
    else 
    {
        Curr[2] = 0x10;
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




uint Emit_FunctionEntry(jit_emitter *Emitter, jit_variable *Params, int ParamCount)
{
    /* push rbp 
     * mov rbp, rsp
     * sub rsp, memsize
     * ; x64 windows: using shadow space of 32 bytes below return addr as argument storage
     * ; arg[i] = [rbp + 0x10 + i*8]
     * movsd [rbp + i*8], xmm(i) 
     * ...
     * */
    while (Emitter->InstructionByteCount % 0x10 != 0)
    {
        Emit(Emitter, 1, 0x90); /* nop */
    }
    uint FunctionLocation = EmitArray(Emitter, sPrologue, sizeof sPrologue);
    int NumArgsInReg = 4;
    for (int i = 0; i < ParamCount; i++)
    {
        int Displacement = 0x10 + i*8;
        if (i < NumArgsInReg)
        {
            Emit_Store(Emitter, i, RBP, Displacement);
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
    assert(FunctionLocation + sizeof(sPrologue) <= sizeof(Emitter->InstructionBuffer));
    u8 *Location = Emitter->InstructionBuffer 
        + FunctionLocation 
        + sizeof(sPrologue) - 4;
    MemCpy(Location, &Value, 4);
}

void Emit_Call(jit_emitter *Emitter, uint FunctionLocation)
{
    /* call rel32 */
    i32 Rel32 = FunctionLocation - (Emitter->InstructionByteCount + 5);
    Emit(Emitter, 1, 0xE8);
    EmitArray(Emitter, (u8 *)&Rel32, sizeof Rel32);
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
                WRITE_OPERANDS("%s", Regs[Reg], "[%s %c %d]", sIntReg[Base], SIGNED_NUMBER(Offset));
            }
        }
        else /* SIB with index */
        {
            if (0 == Mod) /* no displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %d*%s]", sIntReg[Base], Scale, sIntReg[Index]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %d*%s %c %d]", sIntReg[Base], Scale, sIntReg[Index], SIGNED_NUMBER(Offset));
            }
        }
    }
    else if (0 == Mod && 0x5 == Rm) /* displacement only (rip-relative in 64 bit mode) */
    {
        i32 DWord = ConsumeDWord(Data);
        WRITE_OPERANDS("%s", Regs[Reg], "[rip %c %d]", SIGNED_NUMBER(DWord));
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
            WRITE_OPERANDS("%s", sXmmReg[Reg], "[%s %c %d]", sIntReg[Rm], SIGNED_NUMBER(Offset));
        }
    }
#undef WRITE_OPERANDS
#undef SIGNED_NUMBER
}


uint DisasmSingleInstruction(u64 Addr, u8 *Memory, int MemorySize, char ResultBuffer[64])
{
    assert(MemorySize > 0);
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
                WriteInstruction(&Disasm, "%s, %d", sIntReg[Rm], Immediate);
            }
            else Unknown = true;
        }
        else Unknown = true;
    } break;
    case 0x0F:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x57 == Second)
        {
            Disasm.InstructionSize += 2;
            WriteInstruction(&Disasm, "xorps ");
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        }
        else Unknown = true;
    } break;
    case 0xF2:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            disasm_modrm_type ModRmType = MODRM_DST_SRC;
            switch (ConsumeByte(&Disasm))
            {
            case 0x58: WriteInstruction(&Disasm, "addsd "); break;
            case 0x5C: WriteInstruction(&Disasm, "subsd "); break;
            case 0x59: WriteInstruction(&Disasm, "mulsd "); break;
            case 0x5E: WriteInstruction(&Disasm, "divsd "); break;
            case 0x51: WriteInstruction(&Disasm, "sqrtsd "); break;
            case 0x10: WriteInstruction(&Disasm, "movsd "); break; /* movsd r, r/m */
            case 0x11: /* movsd r/m, r */
            {
                WriteInstruction(&Disasm, "movsd ");
                ModRmType = MODRM_SRC_DST;
            } break;
            default:  
            {
                Unknown = true;
            } goto Done;
            }

            X64DisasmModRM(&Disasm, ModRmType, sXmmReg);
Done:
            ;
        }
        else Unknown = true;
    } break;
    }
    if (Unknown)
    {
        WriteInstruction(&Disasm, "???");
        Disasm.InstructionSize++;
    }
    return Disasm.InstructionSize;
}

#undef PEEK


