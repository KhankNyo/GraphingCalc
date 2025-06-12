#ifndef JITERROR_H
#define JITERROR_H

#include "JitCommon.h"

typedef struct jit_error
{
    char Msg[1024];
    uint MsgLen;

    bool8 Available;
} jit_error;


void Error_Reset(jit_error *E);

void Error_AtTokenVA(jit_error *E, const jit_token *Token, const char *Fmt, va_list Args);
void Error_AtToken(jit_error *E, const jit_token *Token, const char *Fmt, ...);
void Error_AtStr(jit_error *E, const char *Str, int Len, int Line, int Offset, const char *Fmt, ...);
#endif /* JITERROR_H */

