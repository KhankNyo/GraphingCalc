
#include <stdarg.h>
#include <stdio.h>
#include "x86Disasm.h"


typedef struct disasm_data
{
    u8 *Memory;
    char *DisasmBuffer;

    uint MemoryCapacity;
    uint InstructionSize;
    uint DisasmBufferSize;
    uint DisasmBufferCap;
} disasm_data;

typedef enum disasm_modrm_type 
{
    MODRM_DST_SRC,
    MODRM_SRC_DST,
} disasm_modrm_type;

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

static void X64DisasmModRM(disasm_data *Data, disasm_modrm_type Type)
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


    u8 ModRM = ConsumeByte(Data);
    uint Mod = ModRM >> 6;
    reg_index Reg = (ModRM >> 3) & 0x7;
    reg_index Rm = (ModRM) & 0x7;

    if (0x3 == Mod) /* reg mode */
    {
        WRITE_OPERANDS("xmm%d", Reg, "xmm%d", Rm);
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
                WRITE_OPERANDS("xmm%d", Reg, "[%s]", IntReg[Base]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("xmm%d", Reg, "[%s + %d]", IntReg[Base], Offset);
            }
        }
        else /* SIB with index */
        {
            if (0 == Mod) /* no displacement */
            {
                WRITE_OPERANDS("xmm%d", Reg, "[%s + %d*%s]", IntReg[Base], Scale, IntReg[Index]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("xmm%d", Reg, "[%s + %d*%s + %d]", IntReg[Base], Scale, IntReg[Index], Offset);
            }
        }
    }
    else if (0 == Mod && 0x5 == Rm) /* displacement only (rip-relative in 64 bit mode) */
    {
        i32 DWord = ConsumeDWord(Data);
        WRITE_OPERANDS("xmm%d", Reg, "[rip + %d]", DWord);
    }
    else /* [register]/[register + displacement] */
    {
        if (0 == Mod) /* no displacement */
        {
            WRITE_OPERANDS("xmm%d", Reg, "[%s]", IntReg[Rm]);
        }
        else /* 8/32 bit displacement */
        {
            i32 Offset = X64ModOffset(Data, Mod);
            WRITE_OPERANDS("xmm%d", Reg, "[%s + %d]", IntReg[Rm], Offset);
        }
    }
}


uint X64DisasmSingleInstruction(u8 *Memory, uint MemorySize, char *ResultBuffer, uint ResultBufferCapacity)
{
    assert(MemorySize > 0);
    disasm_data Disasm = {
        .Memory = Memory,
        .MemoryCapacity = MemorySize,
        .DisasmBuffer = ResultBuffer,
        .DisasmBufferCap = ResultBufferCapacity,
    };

    if (0x0F == PEEK(&Disasm, 0) && 0x57 == PEEK(&Disasm, 1))
    {
        Disasm.InstructionSize += 2;
        WriteInstruction(&Disasm, "xorps ");

        X64DisasmModRM(&Disasm, MODRM_DST_SRC);
    }
    else if (0xF2 == PEEK(&Disasm, 0) && 0x0F == PEEK(&Disasm, 1))
    {
        disasm_modrm_type ModRmType = MODRM_DST_SRC;
        u8 InstructionByte = PEEK(&Disasm, 3);
        Disasm.InstructionSize += 3;
        switch (InstructionByte)
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
            WriteInstruction(&Disasm, "??? ");
            /* next byte is probably modrm */
        } break;
        }

        X64DisasmModRM(&Disasm, ModRmType);
    }
    return Disasm.InstructionSize;
}

#undef PEEK


