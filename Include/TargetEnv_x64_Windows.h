#ifndef TARGETENV_X64_WINDOWS_H
#define TARGETENV_X64_WINDOWS_H

#include "Common.h"

#ifdef _MSC_VER

#   include <intrin.h>

static inline bool8 TargetEnv_x86_IsAVXSupported(void)
{
    int Features[4];
    __cpuid(Features, 1);
    return (Features[2] & (1 << 20)) != 0; /* AVX feature flag */
}

#else /* non-msvc compiler targeting windows */

#   include <cpuid.h>

static inline bool8 TargetEnv_x86_IsAVXSupported(void)
{
    unsigned int EAX = 0, 
                 EBX = 0, 
                 ECX = 0, 
                 EDX = 0;
    __get_cpuid(1, &EAX, &EBX, &ECX, &EDX);
    return (ECX & (1 << 20)) != 0; /* AVX feature flag */
}

#endif /* _MSC_VER */


/* ms x64 calling conv */
static inline int TargetEnv_GetShadowSpaceSize(void)
{
    return 32;
}
/* align to 16-byte boundary */
static inline int TargetEnv_AlignStackSize(int StackSize)
{
    if (StackSize % 16)
        return ROUND_TO_MULTIPLE(StackSize, 16);
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
    return 3; /* RBP */
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
static inline int TargetEnv_GetArgBaseReg(void)
{
    return 3; /* RBP */
}
static inline int TargetEnv_GetArgReg(int ArgIndex)
{
    ASSERT(ArgIndex < 4, "bad arg index");
    return ArgIndex + 1;
}
static inline int TargetEnv_GetArgOffset(int StackTop, int ArgIndex, int DataSize)
{
    ASSERT(DataSize == sizeof(double) || DataSize == sizeof(float), "invalid data size");
    ASSERT(ArgIndex >= 4, "bad arg index");
    return (ArgIndex + 1) * DataSize + StackTop;
}
static inline int TargetEnv_GetReturnReg(void)
{
    return 0; /* xmm0 */
}


#endif /* TARGETENV_X64_WINDOWS_H */
