#ifndef EMITTER_H
#define EMITTER_H


#include "JitCommon.h"

#define TARGETENV_REG_COUNT 8

typedef struct jit_emitter 
{
    u8 *Buffer;
    uint BufferSize;
    uint BufferCapacity;

    /* members below should not be accessed directly */
    u8 StoreSingle[3];
    u8 LoadSingle[3];
    u8 FloatOpcode;
} jit_emitter;


uint DisasmSingleInstruction(u64 Addr, u8 *Memory, int MemorySize, char ResultBuffer[64]);


int TargetEnv_AlignStackSize(int StackSize);
int TargetEnv_GetShadowSpaceSize(void);
int TargetEnv_GetGlobalPtrReg(void);
int TargetEnv_GetStackFrameReg(void);
int TargetEnv_GetArgStackSize(int ArgCount, int DataSize);
int TargetEnv_GetArgRegCount(void);
int TargetEnv_GetArgBaseReg(void);
int TargetEnv_GetArgReg(int ArgIndex);
int TargetEnv_GetArgOffset(int StackTop, int ArgIndex, int DataSize);
int TargetEnv_GetReturnReg(void);



jit_emitter Emitter_Init(u8 *Buffer, uint BufferCapacity);
void Emitter_Reset(jit_emitter *Emitter, bool8 EmitFloat32Instructions);

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

#endif /* EMITTER_H */

