#ifndef X64_WINDOWS_EMITTER_H
#define X64_WINDOWS_EMITTER_H


#include "TargetEnv.h"


typedef struct jit_emitter 
{
    jit_generic_emitter Base;

    /* members below should not be accessed directly */
    u8 StoreSingle[3];
    u8 LoadSingle[3];
    u8 FloatOpcode;
} jit_emitter;


#endif /* X64_WINDOWS_EMITTER_H */

