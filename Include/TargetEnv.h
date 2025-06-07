#ifndef TARGET_ENV_H
#define TARGET_ENV_H

#include "JitCommon.h"
STATIC_ASSERT(sizeof(float) == 4, "float must be 32 bit");
STATIC_ASSERT(sizeof(double) == 8, "double must be 64 bit"); /* TODO: some compilers might use long double for 64 bit float */

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
uint Emit_FunctionEntry(jit_emitter *Emitter, jit_variable *Params, int ParamCount);
void Emit_FunctionExit(jit_emitter *Emitter);
void Emit_PatchStackSize(jit_emitter *Emitter, uint FunctionLocation, i32 Value);

void Emit_Call(jit_emitter *Emitter, uint FunctionLocation);
int Emit_Jump(jit_emitter *Emitter);
void Emitter_PatchJump(jit_emitter *Emitter, uint JumpInsLocation, uint Dst);



/* x64 */
#if defined(_M_X64) || defined(_M_AMD64)\
    || defined(__amd64) || defined(__amd64__)\
    || defined(__x86_64) || defined(__x86_64__)

static inline bool8 TargetEnv_x64_IsAVXSupported(void);

    /* win32 */
#   if defined(_WIN32)
#       include "x64_Windows_TargetEnv.h"
#       include "x64_Windows_Emitter.h"
    /* other OS */
#   else 
#       error("Unsupported x64 OS.")
#   endif

/* other arch */
#else 
#   error("Unsupported architecture.")
#endif 


#endif /* TARGET_ENV_H */
