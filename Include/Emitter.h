#ifndef EMITTER_H
#define EMITTER_H


#include "Common.h"
#include "JitCommon.h"

typedef struct jit_emitter 
{
    u8 InstructionBuffer[256];
    int InstructionByteCount;
} jit_emitter;

uint DisasmSingleInstruction(u8 *Memory, int MemorySize, char ResultBuffer[64]);

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

/* returns the location of the stack size for patching */
uint Emit_FunctionEntry(jit_emitter *Emitter, jit_variable *Params, int ParamCount);
void Emit_FunctionExit(jit_emitter *Emitter);
void Emit_PatchStackSize(jit_emitter *Emitter, uint Location, i32 Value);

#endif /* EMITTER_H */

