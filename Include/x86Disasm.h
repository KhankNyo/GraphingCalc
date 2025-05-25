#ifndef X86DISASM_H
#define X86DISASM_H

#include "Common.h"

uint X64DisasmSingleInstruction(u8 *Memory, uint MemorySize, char *ResultBuffer, uint ResultBufferCapacity);

#endif /* X86DISASM_H */

