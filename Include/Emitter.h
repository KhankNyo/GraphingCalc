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

void Emit_Load(jit_emitter *Emitter, int DstReg, const jit_expression *Mem);
void Emit_Store(jit_emitter *Emitter, int SrcReg, const jit_expression *Mem);
void Emit_Add(jit_emitter *Emitter, int DstReg, const jit_expression *Mem);

void Emit_FunctionEntry(jit_emitter *Emitter, jit_variable *Params, int ParamCount);
void Emit_FunctionExit(jit_emitter *Emitter);

#endif /* EMITTER_H */

