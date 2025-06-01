#ifndef JITERROR_H
#define JITERROR_H

#include "JitCommon.h"

typedef struct jit_error
{
    jit_token Marker[32];
    uint MarkerCount;

    char Msg[1024];
    uint MsgLen;

    bool8 Available;
} jit_error;


void Error_Reset(jit_error *E);

void Error_AtTokenVA(jit_error *E, const jit_token *Token, const char *Fmt, va_list Args);
void Error_AtToken(jit_error *E, const jit_token *Token, const char *Fmt, ...);

void Error_PushMarker(jit_error *E, const jit_token *Token);
void Error_PopMarker(jit_error *E);
void Error_WithMarker(jit_error *E, const jit_token *MarkerEnd, const char *Fmt, ...);

#endif /* JITERROR_H */

