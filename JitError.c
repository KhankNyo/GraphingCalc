
#include <stdarg.h>
#include <stdio.h>
#include <string.h> /* memset */

#include "JitCommon.h"
#include "JitError.h"


static const char *Error_GetLineEnd(const char *TokenStr)
{
    const char *i = TokenStr;
    while (*i && '\n' != *i)
    {
        printf("%c\n", *i);
        i++;
    }

    return i;
}

static void Error_MsgPushChar(jit_error *E, char Ch, uint Count)
{
    for (uint i = 0; i < Count && E->MsgLen < sizeof E->Msg; i++)
        E->Msg[E->MsgLen++] = Ch;
}


static void Error_AtStrVA(jit_error *E, const char *Str, int Len, int Line, int Offset, const char *Fmt, va_list Args)
{
#define PUSH_STR(printfn, ...) do {\
        E->MsgLen += printfn(E->Msg + E->MsgLen, sizeof(E->Msg) - E->MsgLen, \
            __VA_ARGS__\
        );\
        if (E->MsgLen > sizeof E->Msg)\
            E->MsgLen = sizeof E->Msg;\
    } while (0)

    if (E->Available)
        return;

    E->Available = true;
    const char *LineStart = Str - Offset + 1;
    const char *LineEnd = Error_GetLineEnd(Str);
    int LineLength = LineEnd - LineStart;
    if (0 == Len)
        Len = 1;

    /* print the line */
    E->MsgLen = 0;
    PUSH_STR(snprintf, "[ERROR]:%d:%d |", Line, Offset);
    int BarOffset = E->MsgLen;
    int ErrMsgLineOffset = 2;
    int StrOffset = BarOffset + ErrMsgLineOffset + (Offset - 1);
    Error_MsgPushChar(E, ' ', ErrMsgLineOffset);
    PUSH_STR(snprintf, "%.*s\n", LineLength, LineStart);

    /* print the marker */
    Error_MsgPushChar(E, ' ', BarOffset - 1);
    Error_MsgPushChar(E, '|', 1);
    Error_MsgPushChar(E, ' ', ErrMsgLineOffset + (Offset - 1));
    Error_MsgPushChar(E, '^', Len);
    Error_MsgPushChar(E, '\n', 1);
            
    /* print the error message */
    Error_MsgPushChar(E, ' ', StrOffset);
    PUSH_STR(vsnprintf, Fmt, Args);
    Error_MsgPushChar(E, '\n', 1);

    ASSERT(E->MsgLen > 0, "unreachable");

    /* null terminate */
    if (E->MsgLen < sizeof E->Msg)
    {
        E->Msg[E->MsgLen - 1] = '\0';
    }
    else
    {
        E->Msg[sizeof(E->Msg) - 1] = '\0';
    }

#undef PUSH_STR
}



void Error_Reset(jit_error *E)
{
    memset(E, 0, sizeof *E);
}


void Error_AtTokenVA(jit_error *E, const jit_token *Token, const char *Fmt, va_list Args)
{
    Error_AtStrVA(E, Token->Str.Ptr, Token->Str.Len, Token->Line, Token->Offset, Fmt, Args);
}

void Error_AtToken(jit_error *E, const jit_token *Token, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    Error_AtStrVA(E, Token->Str.Ptr, Token->Str.Len, Token->Line, Token->Offset, Fmt, Args);
    va_end(Args);
}

void Error_PushMarker(jit_error *E, const jit_token *Token)
{
    ASSERT(E->MarkerCount < sizeof E->Marker, "Error_PushMarker");
    E->Marker[E->MarkerCount++] = *Token;
}

void Error_PopMarker(jit_error *E)
{
    ASSERT(E->MarkerCount > 0, "Error_PopMarker");
    E->MarkerCount--;
}

void Error_WithMarker(jit_error *E, const jit_token *MarkerEnd, const char *Fmt, ...)
{
    ASSERT(E->MarkerCount > 0, "Error_WithMarker");
    const jit_token *MarkerBegin = &E->Marker[E->MarkerCount - 1];

    va_list Args;
    va_start(Args, Fmt);
    const char *Start = MarkerBegin->Str.Ptr;
    const char *End = MarkerEnd->Str.Ptr + MarkerEnd->Str.Len;
    Error_AtStrVA(E, Start, End - Start, MarkerBegin->Line, MarkerBegin->Offset, Fmt, Args);
    va_end(Args);
}

