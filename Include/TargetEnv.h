#ifndef TARGET_ENV_H
#define TARGET_ENV_H

#include "Common.h"

/* common TargetEnv_* functions */
static inline int TargetEnv_AlignStackSize(int StackSize);
static inline int TargetEnv_GetShadowSpaceSize(void);
static inline int TargetEnv_GetGlobalPtrReg(void);
static inline int TargetEnv_GetStackFrameReg(void);
static inline int TargetEnv_GetArgStackSize(int ArgCount, int DataSize);
static inline int TargetEnv_GetArgRegCount(void);
static inline int TargetEnv_GetArgBaseReg(void);
static inline int TargetEnv_GetArgReg(int ArgIndex);
static inline int TargetEnv_GetArgOffset(int StackTop, int ArgIndex, int DataSize);
static inline int TargetEnv_GetReturnReg(void);


/* x64 */
#if defined(_M_X64) || defined(_M_AMD64)\
    || defined(__amd64) || defined(__amd64__)\
    || defined(__x86_64) || defined(__x86_64__)

static inline bool8 TargetEnv_x64_IsAVXSupported(void);

    /* win32 */
#   if defined(_WIN32)
#       include "TargetEnv_x64_Windows.h"
    /* other OS */
#   else 
#       error("Unsupported x64 OS.")
#   endif

/* other arch */
#else 
#   error("Unsupported architecture.")
#endif 


#endif /* TARGET_ENV_H */
