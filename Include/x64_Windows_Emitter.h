#ifndef X64_WINDOWS_EMITTER_H
#define X64_WINDOWS_EMITTER_H


#include "Common.h"

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


#endif /* X64_WINDOWS_EMITTER_H */

