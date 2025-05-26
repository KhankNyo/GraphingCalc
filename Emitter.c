
#include <stdarg.h>
#include <stdio.h>
#include "Emitter.h"


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

static const char *IntReg[] = { 
    [RAX] = "rax", 
    [RCX] = "rcx",
    [RDX] = "rdx",
    [RBX] = "rbx", 
    [RSP] = "rsp",
    [RBP] = "rbp",
    [RSI] = "rsi", 
    [RDI] = "rdi",
};
static const char *XmmReg[] = {
    "xmm0",
    "xmm1",
    "xmm2",
    "xmm3",
    "xmm4",
    "xmm5",
    "xmm6",
    "xmm7",
};

#define RM(v) ((v) & 0x7)
#define REG(v) (((v) & 0x7) << 3)
#define MODRM(mod, reg, rm) \
    (((mod) & 0x3) << 6)\
    | REG(reg) | RM(rm)


static void Emit(jit_emitter *Emitter, int Count, ...)
{
    va_list Args;
    va_start(Args, Count);
    for (int i = 0; i < Count && (uint)Emitter->InstructionByteCount < sizeof(Emitter->InstructionBuffer); i++)
    {
        Emitter->InstructionBuffer[Emitter->InstructionByteCount++] = va_arg(Args, uint);
    }
    va_end(Args);
}

void Emit_Load(jit_emitter *Emitter, int DstReg, const jit_expression *Mem)
{
    u8 ModRm = MODRM(0x2, DstReg, Mem->As.Mem.BaseReg);
    u32 Offset = Mem->As.Mem.Offset;
    Emit(Emitter, 8, 0xF2, 0x0F, 0x10, ModRm, 
        Offset >> 0, 
        Offset >> 8,
        Offset >> 16,
        Offset >> 24
    );
}

void Emit_Store(jit_emitter *Emitter, int SrcReg, const jit_expression *Mem)
{
    u8 ModRm = MODRM(0x2, SrcReg, Mem->As.Mem.BaseReg);
    u32 Offset = Mem->As.Mem.Offset;
    Emit(Emitter, 8, 0xF2, 0x0F, 0x11, ModRm, 
        Offset >> 0, 
        Offset >> 8,
        Offset >> 16,
        Offset >> 24
    );
}

void Emit_Add(jit_emitter *Emitter, int DstReg, const jit_expression *Mem)
{
    u8 ModRm = MODRM(0x2, DstReg, Mem->As.Mem.BaseReg);
    u32 Offset = Mem->As.Mem.Offset;
    Emit(Emitter, 8, 0xF2, 0x0F, 0x58, ModRm, 
        Offset >> 0, 
        Offset >> 8,
        Offset >> 16,
        Offset >> 24
    );
}


void Emit_FunctionEntry(jit_emitter *Emitter, jit_variable *Params, int ParamCount)
{
    /* push rbp 
     * mov rbp, rsp
     * sub rsp, memsize
     * mov [rbp - index*8], Params[index]
     * ...
     * */
    Emit(Emitter, 1, 0x50 + RBP);
    Emit(Emitter, 3, 0x48, 0x89, MODRM(0x3, RSP, RBP));
    int Size = ParamCount*8;
    Emit(Emitter, 7, 0x48, 0x81, MODRM(0x3, 5, RSP), 
        Size >> 0, 
        Size >> 8, 
        Size >> 16, 
        Size >> 24
    );
    assert(ParamCount < 4 && "TODO: calling conv");
    for (int i = 0; i < ParamCount; i++)
    {
        int Offset = -i*8;
        u8 ModRm = MODRM(0x2, i, RBP);
        Emit(Emitter, 7, 0x48, 0x89, ModRm, 
            Offset >> 0, 
            Offset >> 8, 
            Offset >> 16, 
            Offset >> 24
        );

        /* TODO: couple emitter and compiler with expression? */
        Params[i].Expr = (jit_expression) {
            .Type = EXPR_MEM,
            .As.Mem = {
                .BaseReg = RBP,
                .Offset = Offset,
            },
        };
    }
}

void Emit_FunctionExit(jit_emitter *Emitter)
{
    /* leave 
     * ret 
     */
    Emit(Emitter, 2, 0xC9, 0xC3);
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
        return (i32)(i8)ConsumeByte(Data);
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
                WRITE_OPERANDS("%s", Regs[Reg], "[%s]", IntReg[Base]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %d]", IntReg[Base], Offset);
            }
        }
        else /* SIB with index */
        {
            if (0 == Mod) /* no displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %d*%s]", IntReg[Base], Scale, IntReg[Index]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %d*%s + %d]", IntReg[Base], Scale, IntReg[Index], Offset);
            }
        }
    }
    else if (0 == Mod && 0x5 == Rm) /* displacement only (rip-relative in 64 bit mode) */
    {
        i32 DWord = ConsumeDWord(Data);
        WRITE_OPERANDS("%s", Regs[Reg], "[rip + %d]", DWord);
    }
    else /* [register]/[register + displacement] */
    {
        if (0 == Mod) /* no displacement */
        {
            WRITE_OPERANDS("%s", Regs[Reg], "[%s]", IntReg[Rm]);
        }
        else /* 8/32 bit displacement */
        {
            i32 Offset = X64ModOffset(Data, Mod);
            WRITE_OPERANDS("%s", XmmReg[Reg], "[%s + %d]", IntReg[Rm], Offset);
        }
    }
}


uint DisasmSingleInstruction(u8 *Memory, int MemorySize, char ResultBuffer[64])
{
    assert(MemorySize > 0);
    disasm_data Disasm = {
        .Memory = Memory,
        .MemoryCapacity = MemorySize,
        .DisasmBuffer = ResultBuffer,
        .DisasmBufferCap = 64,
    };

    u8 FirstByte = ConsumeByte(&Disasm);
    bool8 Unknown = true;
    switch (FirstByte)
    {
    case 0x50 + RBP: Unknown = false; WriteInstruction(&Disasm, "push rbp"); break;
    case 0xC3: Unknown = false; WriteInstruction(&Disasm, "ret"); break;
    case 0xC9: Unknown = false; WriteInstruction(&Disasm, "leave"); break;
    case 0x48:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x89 == Second) /* mov rm, r */
        {
            Unknown = false;
            WriteInstruction(&Disasm, "mov ");
            X64DisasmModRM(&Disasm, MODRM_SRC_DST, IntReg);
        }
        else if (0x81 == Second)
        {
            u8 ModRm = ConsumeByte(&Disasm);
            uint Mod = ModRm >> 6;
            uint Reg = (ModRm >> 3) & 0x7;
            uint Rm = ModRm & 0x7;
            if (0x3 == Mod && 5 == Reg) /* sub r, imm */
            {
                Unknown = false;
                WriteInstruction(&Disasm, "sub ");
                i32 Immediate = ConsumeDWord(&Disasm);
                WriteInstruction(&Disasm, "%s, %d", IntReg[Rm], Immediate);
            }
        }
    } break;
    case 0x0F:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x57 == Second)
        {
            Unknown = false;
            Disasm.InstructionSize += 2;
            WriteInstruction(&Disasm, "xorps ");
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, XmmReg);
        }
    } break;
    case 0xF2:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            Unknown = false;
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

            X64DisasmModRM(&Disasm, ModRmType, XmmReg);
Done:
            ;
        }
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


