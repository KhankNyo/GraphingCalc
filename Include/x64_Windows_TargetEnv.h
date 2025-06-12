#ifndef TARGETENV_X64_WINDOWS_H
#define TARGETENV_X64_WINDOWS_H

#include "JitCommon.h"
/* TODO: SSE only supports 8 regs, but AVX can do 16 */
#define TARGETENV_REG_COUNT 8


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


static inline bool8 TargetEnv_x64_IsAVXSupported(void);


#if (defined(_MSC_VER) && !defined(__clang__) /* clang is real funny */)\
|| (defined(_WIN32) && defined(__TINYC__)) /* tcc for windows */

static inline void TargetEnv_x64_GetCPUID(u32 Feature, u32 *OutEAX, u32 *OutEBX, u32 *OutECX, u32 *OutEDX)
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

static inline void TargetEnv_x64_GetCPUID(u32 Feature, u32 *OutEAX, u32 *OutEBX, u32 *OutECX, u32 *OutEDX)
{
    __get_cpuid(Feature, OutEAX, OutEBX, OutECX, OutEDX);
}

#endif /* (_MSC_VER && !__clang__) || (_WIN32 && __TINYC__) */


static inline bool8 TargetEnv_x64_IsAVXSupported(void)
{
    u32 EAX, 
        EBX, 
        ECX = 0, 
        EDX;
    TargetEnv_x64_GetCPUID(1, &EAX, &EBX, &ECX, &EDX);
    return (ECX & (1 << 20)) != 0;
}

/* ms x64 calling conv */
static inline int TargetEnv_StackMaxAlignment(int StackSize)
{
    if (StackSize % 16)
        return ROUND_UP_TO_MULTIPLE(StackSize, 16);
    return StackSize;
}
static inline int TargetEnv_StackMinAlignment(int StackSize)
{
    if (StackSize % 8)
        return ROUND_UP_TO_MULTIPLE(StackSize, 8);
    return StackSize;
}
static inline int TargetEnv_GetShadowSpaceSize(void)
{
    return 32;
}
static inline int TargetEnv_GetGlobalPtrReg(void)
{
    return RCX;
}
static inline int TargetEnv_GetStackFrameReg(void)
{
    return RBP;
}
static inline int TargetEnv_GetArgStackSize(int ArgCount, int DataSize)
{
    (void)DataSize; /* stack will always be aligned to at least 8 byte boundary */
    ASSERT(DataSize == sizeof(double) || DataSize == sizeof(float), "invalid data size");
    /* rcx as global ptr reg, but we won't store it on stack */
    int StackSize = (ArgCount + 1) * 8;
    if (StackSize < 32)
        return 32;
    return StackSize;
}
static inline int TargetEnv_GetArgRegCount(void)
{
    /* rcx taken as global ptr reg */
    /* xmm1 = arg0  */
    /* xmm2 = arg1  */
    /* xmm3 = arg2  */
    /* rest are on stack */
    return 3;
}
static inline bool8 TargetEnv_CallerShouldSave(int Reg)
{
    return IN_RANGE(0, Reg, TARGETENV_REG_COUNT - 1);
}
static inline bool8 TargetEnv_IsArgumentInReg(int ArgIndex)
{
    return ArgIndex < TargetEnv_GetArgRegCount();
}
static inline jit_expression TargetEnv_GetArg(int Index, int DataSize)
{
    (void)DataSize;
    if (TargetEnv_IsArgumentInReg(Index))
    {
        jit_expression Reg = {
            .Storage = STORAGE_REG,
            .As.Reg = Index + 1,
        };
        return Reg;
    }
    else
    {
        jit_expression Mem = {
            .Storage = STORAGE_MEM,
            .As.Mem = {
                .BaseReg = RSP,
                .Offset = (Index + 1) * 8, /* always align to 8-byte boundary */
            },
        };
        return Mem;
    }
}
static inline jit_mem TargetEnv_GetParam(int Index, int DataSize)
{
    (void)DataSize;
    return (jit_mem) {
        .BaseReg = RBP,
        .Offset = 0x10 + (Index + 1) * 8, /* always align to 8-byte boundary */
    };
}
static inline int TargetEnv_GetReturnReg(void)
{
    return 0; /* xmm0 */
}

#endif /* TARGETENV_X64_WINDOWS_H */
