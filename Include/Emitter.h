#ifndef EMITTER_H
#define EMITTER_H


#include "Common.h"
#include "JitCommon.h"

typedef struct jit_emitter 
{
    u8 *Buffer;
    uint BufferSize;
    uint BufferCapacity;
} jit_emitter;

jit_emitter Emitter_Init(void *Buffer, uint BufferCapacity);
void Emitter_Reset(jit_emitter *Emitter);

uint DisasmSingleInstruction(u64 Addr, u8 *Memory, int MemorySize, char ResultBuffer[64]);

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

#endif /* EMITTER_H */

