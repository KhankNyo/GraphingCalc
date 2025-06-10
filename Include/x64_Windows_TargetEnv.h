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


#if defined(_MSC_VER) && !defined(__clang__) /* clang is real funny */
/* fuck MSVC */
#   if (defined(_M_IX86_FP) && _M_IX86_FP == 0) /* MSVC not supporting/enabling SSE */
#       error("Compiler must support SSE.")
#   endif /* _M_IX86_FP */

#   include <intrin.h>
#   include <immintrin.h>

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
#   if !defined(__SSE__)
#       error("Compiler must support SSE.")
#   endif /* __SSE__ */

#   include <cpuid.h>
#   include <x86intrin.h>

static inline void TargetEnv_x64_GetCPUID(u32 Feature, u32 *OutEAX, u32 *OutEBX, u32 *OutECX, u32 *OutEDX)
{
    __get_cpuid(Feature, OutEAX, OutEBX, OutECX, OutEDX);
}

#endif /* _MSC_VER */


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
static inline int TargetEnv_GetShadowSpaceSize(void)
{
    return 32;
}
/* align to 16-byte boundary */
static inline int TargetEnv_AlignStackSize(int StackSize)
{
    if (StackSize % 16)
        return ROUND_UP_TO_MULTIPLE(StackSize, 16);
    return StackSize;
}
/* MS x64 first argument */
static inline int TargetEnv_GetGlobalPtrReg(void)
{
    return 1; /* RCX */
}
/* base pointer */
static inline int TargetEnv_GetStackFrameReg(void)
{
    return 5; /* RBP */
}
static inline int TargetEnv_GetArgStackSize(int ArgCount, int DataSize)
{
    ASSERT(DataSize == sizeof(double) || DataSize == sizeof(float), "invalid data size");
    /* rcx as global ptr reg, but we won't store it on stack */
    int StackSize = (ArgCount + 1) * DataSize;
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
static inline int TargetEnv_GetParamBaseReg(void)
{
    return 5; /* RBP */
}
static inline i32 TargetEnv_GetParamDisplacement(int ArgIndex, int DataSize)
{
    /* [rbp + 0x18 + index*size] */
    return 0x18 + ArgIndex*DataSize; /* 0x10 = rbp, retaddr, 0x8 = rcx (global ptr) */
}
static inline bool8 TargetEnv_CallerShouldSave(int Reg)
{
    return IN_RANGE(0, Reg, TARGETENV_REG_COUNT);
}
static inline bool8 TargetEnv_IsArgumentInReg(int ArgIndex)
{
    return ArgIndex < TargetEnv_GetArgRegCount();
}
#if 0
static inline int TargetEnv_GetArgOffset(int StackTop, int ArgIndex, int DataSize)
{
    ASSERT(DataSize == sizeof(double) || DataSize == sizeof(float), "invalid data size");
    ASSERT(ArgIndex >= 4, "bad arg index");
    return (ArgIndex + 1) * DataSize + StackTop;
}
static inline int TargetEnv_GetArgBaseReg(void)
{
    return 3; /* RBP */
}
static inline int TargetEnv_GetArgReg(int ArgIndex)
{
    ASSERT(ArgIndex < 4, "bad arg index");
    return ArgIndex + 1;
}
#else
static inline jit_expression TargetEnv_GetArg(int Index, int DataSize)
{
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
                .Offset = (Index + 1) * DataSize,
            },
        };
        return Mem;
    }
}
#endif
static inline int TargetEnv_GetReturnReg(void)
{
    return 0; /* xmm0 */
}

#endif /* TARGETENV_X64_WINDOWS_H */
