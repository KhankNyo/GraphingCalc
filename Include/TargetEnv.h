#ifndef TARGET_ENV_H
#define TARGET_ENV_H

#include "JitCommon.h"
void TCC_STATIC_ASSERT_FN__(void)
{
    STATIC_ASSERT(sizeof(float) == 4, "float must be 32 bit");
    STATIC_ASSERT(sizeof(double) == 8, "double must be 64 bit"); /* TODO: some compilers might use long double for 64 bit float */
}

/* common TargetEnv_* functions */
static inline int TargetEnv_StackMaxAlignment(int StackSize);
static inline int TargetEnv_StackMinAlignment(int StackSize);
static inline int TargetEnv_GetShadowSpaceSize(void);
static inline int TargetEnv_GetGlobalPtrReg(void);
static inline int TargetEnv_GetStackFrameReg(void);

static inline bool8 TargetEnv_IsArgumentInReg(int ArgIndex);
static inline bool8 TargetEnv_CallerShouldSave(int Reg);
static inline int TargetEnv_GetReturnReg(void);
static inline int TargetEnv_GetArgStackSize(int ArgCount, int DataSize);
static inline jit_location TargetEnv_GetArg(int Index, int DataSize);
static inline jit_mem TargetEnv_GetParam(int Index, int DataSize);


/* each platform defines their own emitter */
typedef struct jit_emitter jit_emitter;

/* common emitter functions */
int DisasmSingleInstruction(u64 Addr, const u8 *Memory, int MemorySize, char ResultBuffer[64]);

void Emitter_Init(jit_emitter *TargetEnvEmitter, u8 *Buffer, int BufferCapacity);
int Emitter_GetBufferSize(const jit_emitter *Emitter);
const u8 *Emitter_GetBuffer(const jit_emitter *Emitter);
void Emitter_Reset(jit_emitter *TargetEnvEmitter, bool8 EmitFloat32Instructions);

void Emitter_TranslateIr(jit *Jit);


/* TARGETENV_<platform name> must be defined via compilation flags */
/* x64 Windows */
#if defined(TARGETENV_X64_WINDOWS)
static inline bool8 TargetEnv_x64_IsAVXSupported(void);
#   include "x64_Windows_TargetEnv.h"
#   include "x64_Windows_Emitter.h"
/* other platforms */
#else 
#   error("Unsupported target environment.")
#endif

/* macros */
#if !defined(TARGETENV_REG_COUNT)
#   error("target environment must define TARGETENV_REG_COUNT")
#endif /* TARGETENV_REG_COUNT */


#endif /* TARGET_ENV_H */
