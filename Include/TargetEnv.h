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
static inline jit_expression TargetEnv_GetArg(int Index, int DataSize);
static inline jit_expression TargetEnv_GetParam(int Index, int DataSize);


/* each platform defines their own emitter */
typedef struct jit_emitter jit_emitter;
/* every platform must have jit_generic_emitter as the first member named 'Base', i.e.
 * struct jit_emitter {
 *     jit_emitter Base; 
 *     ... private members 
 * }; */
typedef struct jit_generic_emitter 
{
    u8 *Buffer; 
    uint BufferSize; 
    uint BufferCapacity;
} jit_generic_emitter;

/* common emitter functions */
uint DisasmSingleInstruction(u64 Addr, u8 *Memory, int MemorySize, char ResultBuffer[64]);

void Emitter_Init(jit_emitter *TargetEnvEmitter, u8 *Buffer, uint BufferCapacity);
void Emitter_Reset(jit_emitter *TargetEnvEmitter, bool8 EmitFloat32Instructions);

void Emit_Move(jit_emitter *Emitter, int DstReg, int SrcReg);
void Emit_Load(jit_emitter *Emitter, int DstReg, int SrcBase, i32 SrcOffset);
void Emit_Store(jit_emitter *Emitter, int SrcReg, int DstBase, i32 DstOffset);
void Emit_Add(jit_emitter *Emitter, int DstReg, int SrcBase, i32 SrcOffset);
void Emit_Sub(jit_emitter *Emitter, int DstReg, int SrcBase, i32 SrcOffset);
void Emit_Mul(jit_emitter *Emitter, int DstReg, int SrcBase, i32 SrcOffset);
void Emit_Div(jit_emitter *Emitter, int DstReg, int SrcBase, i32 SrcOffset);

void Emit_AddReg(jit_emitter *Emitter, int DstReg, int SrcReg);
void Emit_SubReg(jit_emitter *Emitter, int DstReg, int SrcReg);
void Emit_MulReg(jit_emitter *Emitter, int DstReg, int SrcReg);
void Emit_DivReg(jit_emitter *Emitter, int DstReg, int SrcReg);
void Emit_LoadZero(jit_emitter *Emitter, int DstReg);

/* returns the function's location */
uint Emit_FunctionEntry(jit_emitter *);
void Emit_FunctionAllocateStack(jit_emitter *Emitter, i32 StackSize);
void Emit_FunctionExit(jit_emitter *Emitter);
uint Emit_Call(jit_emitter *Emitter, uint FunctionLocation);
void Emitter_PatchCall(jit_emitter *Emitter, uint CallLocation, uint FunctionLocation);


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
