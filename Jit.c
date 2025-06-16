#include "JitCommon.h"
#include "JitBackend.h"
#include "Jit.h"
#include "DefTable.h"

#include <stdio.h>
#include <math.h>
#include <stdarg.h>


typedef enum precedence 
{
    PREC_NONE = 0,
    PREC_EXPR, 
    PREC_PLUSMINUS,
    PREC_MULDIV,
    PREC_POW,
    PREC_UNARY,
} precedence;
static int Jit_Ir_CompileExpr(jit *Jit, precedence Prec, bool8 AllowForwardReference);


static void Jit_Scratchpad_Reset(jit_scratchpad *S, void *Buffer, int BufferCapacity)
{
    *S = (jit_scratchpad) {
        .Ptr = Buffer,
        .Capacity = BufferCapacity,
        .LeftCount = 0,
        .RightCount = 0,
    };
}

static int Jit_Scratchpad_BytesRemain(const jit_scratchpad *S)
{
    i64 Result = S->Capacity - (S->LeftCount + S->RightCount);
    ASSERT(IN_RANGE(0, Result, S->Capacity), "unreachable");
    return (int)Result;
}

static void *Jit_Scratchpad_LeftPtr(jit_scratchpad *S)
{
    return S->Ptr + S->LeftCount;
}

static void *Jit_Scratchpad_RightPtr(jit_scratchpad *S)
{
    return S->Ptr + S->Capacity - S->RightCount;
}

static void *Jit_Scratchpad_PushLeft(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= Jit_Scratchpad_BytesRemain(S), "bad data size");
    void *Ptr = Jit_Scratchpad_LeftPtr(S);
    S->LeftCount += DataSize;
    return Ptr;
}

static void *Jit_Scratchpad_PushRight(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= Jit_Scratchpad_BytesRemain(S), "bad data size");
    void *Ptr = (u8 *)Jit_Scratchpad_RightPtr(S) - DataSize;
    S->RightCount += DataSize;
    return Ptr;
}

static void Jit_Scratchpad_PopLeft(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->LeftCount, "bad data size");
    S->LeftCount -= DataSize;
}

static void Jit_Scratchpad_PopRight(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->RightCount, "bad data size");
    S->RightCount -= DataSize;
}



static jit_ir_stack Ir_Stack_Init(jit *Jit)
{
    return (jit_ir_stack) {
        .S = &Jit->S,
    };
}

jit_location *Ir_Stack_Top(jit_ir_stack *Stack, int Offset)
{
    ASSERT(Offset >= 0, "bad offset");
    return (jit_location *)Jit_Scratchpad_LeftPtr(Stack->S) - 1 - Offset;
}

jit_location *Ir_Stack_Push(jit_ir_stack *Stack, const jit_location *Value)
{
    Stack->Count++;
    jit_location *Top = Jit_Scratchpad_PushLeft(Stack->S, sizeof(*Top));
    *Top = *Value;
    return Top;
}

jit_location *Ir_Stack_Pop(jit_ir_stack *Stack)
{
    ASSERT(Stack->Count != 0, "Cannot pop stack");
    Stack->Count--;
    jit_location *Top = Ir_Stack_Top(Stack, 0);
    Jit_Scratchpad_PopLeft(Stack->S, sizeof(jit_location));
    return Top;
}

void Ir_Stack_PopMultiple(jit_ir_stack *Stack, int Count)
{
    ASSERT(Stack->Count >= Count, "unreachable");
    Stack->Count -= Count;
    Jit_Scratchpad_PopLeft(Stack->S, Count*sizeof(jit_location));
}



static jit_fnref_stack Ir_FnRef_Init(jit_scratchpad *S)
{
    return (jit_fnref_stack) {
        .S = S,
        .Base = S->RightCount,
    };
}

static jit_fnref *Ir_FnRef_Top(jit_fnref_stack *FnRef, int Offset)
{
    ASSERT(Offset >= 0, "bad offset");
    return (jit_fnref *)Jit_Scratchpad_RightPtr(FnRef->S) + Offset;
}

jit_fnref *Ir_FnRef_Push(jit_fnref_stack *FnRef, const jit_function *Function, uint CallLocation)
{
    jit_fnref *Top = Jit_Scratchpad_PushRight(FnRef->S, sizeof(*Top));
    Top->Function = Function;
    Top->Location = CallLocation;
    return Top;
}

static jit_fnref *Ir_FnRef_Pop(jit_fnref_stack *FnRef)
{
    jit_fnref *Top = Ir_FnRef_Top(FnRef, 0);
    Jit_Scratchpad_PopRight(FnRef->S, sizeof(jit_fnref));
    return Top;
}

static int Ir_FnRef_Count(const jit_fnref_stack *FnRef)
{
    ASSERT(FnRef->S->RightCount >= FnRef->Base, "unreachable");
    return (FnRef->S->RightCount - FnRef->Base) / sizeof(jit_fnref);
}







static bool8 ChInRange(char Lower, char n, char Upper)
{
    return IN_RANGE(Lower, n, Upper);
}

static bool8 IsNumber(char Ch)
{
    return ChInRange('0', Ch, '9');
}

static bool8 IsIdentifier(char Ch)
{
    return '_' == Ch || ChInRange('a', Lowercase(Ch), 'z');
}

static bool8 AtEnd(jit *Jit)
{
    return ((Jit)->End[0] == '\0');
}

static char Peek(jit *Jit, int Offset)
{
    for (int i = Jit->SafePeekDist; i < Offset; i++)
    {
        if (Jit->End[i] == '\0')
            return '\0';
    }
    if (Offset > Jit->SafePeekDist)
        Jit->SafePeekDist = Offset;
    return Jit->End[Offset];
}

static char Advance(jit *Jit)
{
    if (Jit->SafePeekDist > 0 || !AtEnd(Jit))
    {
        char Ch = *Jit->End++;
        Jit->Offset++;
        Jit->SafePeekDist -= Jit->SafePeekDist > 0;
        return Ch;
    }
    return '\0';
}

static jit_token CreateToken(jit *Jit, jit_token_type Type)
{
    int Len = Jit->End - Jit->Start;
    jit_token Tok = {
        .Type = Type,
        .Line = Jit->Line,
        .Offset = Jit->Offset - Len, 
        .Str = {
            .Ptr = Jit->Start,
            .Len = Len,
        },
    };
    Jit->Start = Jit->End;
    return Tok;
}

static jit_token ErrorToken(jit *Jit, const char *ErrMsg)
{
    jit_token Tok = CreateToken(Jit, TOK_ERR);
    Tok.As.ErrMsg = ErrMsg;
    return Tok;
}

static jit_token ParseNumber(jit *Jit, char First)
{
    double Number = First - '0';
    while (IsNumber(Peek(Jit, 0)))
    {
        Number *= 10;
        Number += Advance(Jit) - '0';
    }

    if ('.' == Peek(Jit, 0))
    {
        Advance(Jit); /* skip ; */
        double Decimal = 0;
        double Pow10 = 1;
        while (IsNumber(Peek(Jit, 0)))
        {
            Decimal *= 10;
            Pow10 *= 10;
            Decimal += Advance(Jit) - '0';
        }
        Number += Decimal / Pow10;
    }
    jit_token Tok = CreateToken(Jit, TOK_NUMBER);
    Tok.As.Number = Number;
    return Tok;
}

static jit_token ParseIdentifier(jit *Jit)
{
#define STREQU(literal, strv) \
    (((sizeof(literal) - 1) == (strv).Len)\
     && StrEqu(literal, (strv).Ptr, sizeof(literal) - 1))
    while (IsIdentifier(Peek(Jit, 0)) 
    || '_' == Peek(Jit, 0) 
    || IsNumber(Peek(Jit, 0)))
    {
        Advance(Jit);
    }

    jit_token Identifier = CreateToken(Jit, TOK_IDENTIFIER);
    return Identifier;
#undef STREQU
}


static void ConsumeSpace(jit *Jit)
{
    while (1)
    {
        switch (Peek(Jit, 0))
        {
        case ' ':
        case '\t':
        case '\r':
        {
            Advance(Jit);
        } break;
        case '#': /* # comment */
        {
            do {
                Advance(Jit);
            } while (!AtEnd(Jit) && Peek(Jit, 0) != '\n');
        } break;
        default: goto Out;
        }
    }

Out:
    Jit->Start = Jit->End;
}

static jit_token NewlineToken(jit *Jit)
{
    jit_token Tok = CreateToken(Jit, TOK_NEWLINE);
    Jit->Line++;
    Jit->Offset = 1;
    return Tok;
}

static bool8 ConsumeIfPeekIs(jit *Jit, int Offset, char Ch)
{
    if (Peek(Jit, Offset) == Ch)
    {
        Advance(Jit);
        return true;
    }
    return false;
}

static jit_token Jit_Tokenize(jit *Jit)
{
    ConsumeSpace(Jit);
    char Ch = Advance(Jit);

    if (IsNumber(Ch) 
    || ( Peek(Jit, 0) == '.' && IsNumber(Peek(Jit, 1)) ))
    {
        return ParseNumber(Jit, Ch);
    }

    if (IsIdentifier(Ch))
    {
        return ParseIdentifier(Jit);
    }

    switch (Ch)
    {
    case '+': return CreateToken(Jit, TOK_PLUS);
    case '-': return CreateToken(Jit, TOK_MINUS);
    case '/': return CreateToken(Jit, TOK_SLASH);
    case '*': return CreateToken(Jit, TOK_STAR);
    case '^': return CreateToken(Jit, TOK_CARET);
    case '%': return CreateToken(Jit, TOK_PERCENT);
    case '(': return CreateToken(Jit, TOK_LPAREN);
    case ')': return CreateToken(Jit, TOK_RPAREN);
    case '[': return CreateToken(Jit, TOK_LBRACKET);
    case ']': return CreateToken(Jit, TOK_RBRACKET);
    case ',': return CreateToken(Jit, TOK_COMMA);
    case '=': return CreateToken(Jit, TOK_EQUAL);
    case '<': 
        if (ConsumeIfPeekIs(Jit, 0, '='))
            return CreateToken(Jit, TOK_LESS_EQUAL);
        else return CreateToken(Jit, TOK_LESS);
    case '>':
        if (ConsumeIfPeekIs(Jit, 0, '='))
            return CreateToken(Jit, TOK_GREATER_EQUAL);
        else return CreateToken(Jit, TOK_GREATER);
    case '\n': return NewlineToken(Jit);
    case '\0': return CreateToken(Jit, TOK_EOF);
    default: return ErrorToken(Jit, "Invalid token.");
    }
}



static jit_token CurrToken(const jit *Jit)
{
    return Jit->Curr;
}

static jit_token NextToken(const jit *Jit)
{
    return Jit->Next;
}

static jit_token ConsumeToken(jit *Jit)
{
    if (TOK_ERR == Jit->Next.Type)
    {
        Error_AtToken(&Jit->Error, &Jit->Next, "%s", Jit->Next.As.ErrMsg);
    }
    Jit->Curr = Jit->Next;
    Jit->Next = Jit_Tokenize(Jit);
    return Jit->Curr;
}


static bool8 ConsumeIfNextTokenIs(jit *Jit, jit_token_type ExpectedToken)
{
    if (NextToken(Jit).Type == ExpectedToken)
    {
        ConsumeToken(Jit);
        return true;
    }
    return false;
}


static bool8 ConsumeOrError(jit *Jit, jit_token_type ExpectedType, const char *ErrMsg, ...)
{
    if (NextToken(Jit).Type != ExpectedType)
    {
        va_list Args;
        va_start(Args, ErrMsg);
        Error_AtTokenVA(&Jit->Error, &Jit->Next, ErrMsg, Args);
        va_end(Args);
        return false;
    }
    ConsumeToken(Jit);
    return true;
}





/*========================================================== 
 *                          IR OP
 *==========================================================*/


static u8 *Ir_Op_PushAndReserveArgSize(jit *Jit, jit_ir_op_type Type, int ArgSize)
{
    Jit->IrOpByteCount += 1 + ArgSize;
    u8 *Ptr = Jit_Scratchpad_PushLeft(&Jit->S, 1 + ArgSize);
    *Ptr = Type;
    return Ptr + 1;
}
static void Ir_Op_Push(jit *Jit, jit_ir_op_type Type)
{
    Ir_Op_PushAndReserveArgSize(Jit, Type, 0);
}
static void Ir_Op_PushOp4(jit *Jit, jit_ir_op_type Type, i32 Arg)
{
    ASSERT(Type <= 0xFF, "unreachable");

    u8 *ArgPtr = Ir_Op_PushAndReserveArgSize(Jit, Type, sizeof(i32));
    MemCpy(ArgPtr, &Arg, sizeof(i32));
}
static void Ir_Op_PushCall(jit *Jit, i32 ArgCount, const jit_token *FnName)
{
    u8 *ArgPtr = Ir_Op_PushAndReserveArgSize(Jit, IR_OP_CALL,
        Ir_Op_GetArgSize(IR_OP_CALL)
    );
    MemCpy(ArgPtr, &FnName->Str.Ptr, sizeof(const char *));
    MemCpy(ArgPtr + 8, &FnName->Str.Len, 4);
    MemCpy(ArgPtr + 8 + 4, &FnName->Line, 4);
    MemCpy(ArgPtr + 8 + 2*4, &FnName->Offset, 4);
    MemCpy(ArgPtr + 8 + 3*4, &ArgCount, 4);
}
static i32 Ir_Op_StartBlock(jit *Jit, jit_ir_op_type Op, const void *Ptr)
{
    i32 Location = Jit->IrOpByteCount;
    u8 *ArgPtr = Ir_Op_PushAndReserveArgSize(Jit, Op, Ir_Op_GetArgSize(Op));
    MemCpy(ArgPtr, &Ptr, sizeof(void *));
    return Location;
}
static void Ir_Op_EndBlock(jit *Jit, i32 Location)
{
    ASSERT(Location < Jit->IrOpByteCount, "invalid location");
    ASSERT(Location >= 0, "invalid location");

    /* calculate block size */
    i32 BlockSize = Jit->IrOpByteCount - Location;
    ASSERT(IN_RANGE(0, BlockSize, UINT16_MAX), "block too big");
    u16 ShortBlockSize = BlockSize;

    u8 *PatchLocation = Jit->IrOp + Location + 1 + sizeof(void *);
    MemCpy(
        PatchLocation,
        &ShortBlockSize, 
        sizeof(ShortBlockSize)
    );
}
static void Ir_Op_LinkBlock(jit *Jit, i32 PrevBlockLocation, i32 CurrBlockLocation)
{
    ASSERT(PrevBlockLocation < Jit->IrOpByteCount, "invalid location");
    ASSERT(PrevBlockLocation >= 0, "invalid location");

    /* calculate offset */
    i32 Offset = CurrBlockLocation - (PrevBlockLocation + 1 + Ir_Op_GetArgSize(IR_OP_FN_BLOCK));
    ASSERT(IN_RANGE(0, Offset, UINT16_MAX), "blocks too far from each other");
    u16 ShortOffset = Offset;

    /* patch the offset */
    u8 *PatchLocation = Jit->IrOp + PrevBlockLocation + 1 + sizeof(void *) + sizeof(u16);
    MemCpy(
        PatchLocation, 
        &ShortOffset, 
        sizeof(ShortOffset)
    );
}
static void Ir_Op_PushLoad(jit *Jit, i32 Index)
{
    Ir_Op_PushOp4(Jit, IR_OP_LOAD, Index);
}






/* returns pointer to payload space */
static u8 *Ir_Data_PushReserveSize(jit_ir_data_manager *D, jit_ir_data_type Type)
{
    int DataSize = Type + 1;
    D->ByteCount += DataSize;
    u8 *Ptr = Jit_Scratchpad_PushRight(D->S, DataSize);
    *(Ptr + 0) = Type;
    return Ptr + 1;
}

static void Ir_PopData(jit_ir_data_manager *D)
{
    u8 *Ptr = Jit_Scratchpad_RightPtr(D->S);
    jit_ir_data_type DataType = *Ptr;
    int DataSize = DataType + 1;
    Jit_Scratchpad_PopRight(D->S, DataSize);
    D->ByteCount -= DataSize;
}

static int Ir_PushConstData(jit_ir_data_manager *D, double Const)
{
    u8 *Ptr = Ir_Data_PushReserveSize(D, IR_DATA_CONST);
    int Index = D->ByteCount;
    MemCpy(Ptr, &Const, sizeof(Const));
    return Index;
}

static int Ir_PushRefData(jit_ir_data_manager *D, const jit_token *VarName)
{
    u8 *Ptr = Ir_Data_PushReserveSize(D, IR_DATA_VARREF);
    int Index = D->ByteCount;

    i32 StrBegin = VarName->Str.Ptr - D->SrcBegin;
    i32 StrLen = VarName->Str.Len;
    i32 Line = VarName->Line;
    i32 Offset = VarName->Offset;
    MemCpy(Ptr + 0, &StrBegin, 4);
    MemCpy(Ptr + 4, &StrLen, 4);
    MemCpy(Ptr + 8, &Line, 4);
    MemCpy(Ptr + 12, &Offset, 4);
    return Index;
}

static int Ir_PushParamData(jit_ir_data_manager *D, strview VarName, i32 ParamIndex)
{
    u8 *Ptr = Ir_Data_PushReserveSize(D, IR_DATA_PARAM);
    int Index = D->ByteCount;
    i32 StrBegin = VarName.Ptr - D->SrcBegin;
    i32 StrLen = VarName.Len;
    MemCpy(Ptr + 0, &StrBegin, 4);
    MemCpy(Ptr + 4, &StrLen, 4);
    MemCpy(Ptr + 8, &ParamIndex, 4);
    return Index;
}


static u8 *Ir_GetDataTypePtr(jit_ir_data_manager *D, i32 Offset)
{
    ASSERT(Offset <= D->ByteCount, "bad offset");
    u8 *DataTypePtr = D->Ptr - Offset;
    return DataTypePtr;
}

jit_ir_data Ir_GetData(jit_ir_data_manager *D, i32 Offset)
{
    ASSERT(Offset <= D->ByteCount, "Invalid Offset to type");
    ASSERT(Offset > 0, "Invalid Offset to type");

    u8 *DataTypePtr = Ir_GetDataTypePtr(D, Offset);
    jit_ir_data Data = {
        .Type = *DataTypePtr,
    };
    switch ((jit_ir_data_type)*DataTypePtr)
    {
    case IR_DATA_CONST:
    {
        MemCpy(&Data.As.Const, DataTypePtr + 1, sizeof(double));
    } break;
    case IR_DATA_PARAM:
    {
        i32 StrBegin, StrLen, ParamIndex;
        MemCpy(&StrBegin,   DataTypePtr + 1, 4);
        MemCpy(&StrLen,     DataTypePtr + 1 + 4, 4);
        MemCpy(&ParamIndex, DataTypePtr + 1 + 8, 4);
        Data.As.Param.Str = D->SrcBegin + StrBegin;
        Data.As.Param.StrLen = StrLen;
        Data.As.Param.Index = ParamIndex;
    } break;
    case IR_DATA_VARREF:
    {
        i32 StrBegin, StrLen, Line, Offset;
        MemCpy(&StrBegin,   DataTypePtr + 1, 4);
        MemCpy(&StrLen,     DataTypePtr + 1 + 4, 4);
        MemCpy(&Line,       DataTypePtr + 1 + 8, 4);
        MemCpy(&Offset,     DataTypePtr + 1 + 12, 4);
        Data.As.VarRef.Str = D->SrcBegin + StrBegin;
        Data.As.VarRef.StrLen = StrLen;
        Data.As.VarRef.Line = Line;
        Data.As.VarRef.Offset = Offset;
    } break;
    default:
    {
        UNREACHABLE();
    } break;
    }
    return Data;
}

static i32 Ir_GetNextDataOffset(jit_ir_data_manager *D, i32 CurrentOffset)
{
    ASSERT(CurrentOffset <= D->ByteCount, "bad offset");
    ASSERT(CurrentOffset > 0, "bad offset");
    return CurrentOffset - ((i32)(i8)*Ir_GetDataTypePtr(D, CurrentOffset) + 1);
}

static bool8 Jit_FindVariable(
    jit *Jit, 
    const char *Str, int Len,
    int LocalScopeBase, int LocalScopeVarCount, 
    jit_location *OutVarLocation)
{
    /* search local scope if provided */
    i32 DataOffset = LocalScopeBase;
    for (int i = 0; i < LocalScopeVarCount; i++)
    {
        jit_ir_data Data = Ir_GetData(&Jit->IrData, DataOffset);
        ASSERT(IR_DATA_PARAM == Data.Type, "unreachable");

        if (Len == Data.As.Param.StrLen && StrEqu(Str, Data.As.Param.Str, Len))
        {
            if (NULL != OutVarLocation)
            {
                *OutVarLocation = LocationFromMem(
                    Backend_CalleeSideParamMem(Data.As.Param.Index)
                ); 
            }
            return true;
        }

        DataOffset = Ir_GetNextDataOffset(&Jit->IrData, DataOffset);
    }

    /* failed local scope search, switch to global */
    def_table_entry *Entry = DefTable_Find(&Jit->Global, Str, Len, TYPE_VARIABLE);
    if (NULL == Entry)
    {
        return false;
    }
    if (NULL != OutVarLocation)
    {
        *OutVarLocation = Entry->As.Variable.Location;
    }
    return true;
}

jit_location Ir_Data_GetLocation(jit *Jit, i32 DataIndex, int LocalScopeBase, int LocalScopeVarCount)
{
    jit_ir_data Data = Ir_GetData(&Jit->IrData, DataIndex);
    jit_location Result = { 0 };
    switch (Data.Type)
    {
    case IR_DATA_CONST:
    {
        Result.Type = LOCATION_MEM;
        Result.As.Mem = Backend_AllocateGlobal(&Jit->Backend, Data.As.Const);
    } break;
    case IR_DATA_VARREF:
    {
        const char *Str = Data.As.VarRef.Str;
        i32 StrLen = Data.As.VarRef.StrLen;
        i32 Line = Data.As.VarRef.Line;
        i32 Offset = Data.As.VarRef.Offset;
        if (!Jit_FindVariable(Jit, Str, StrLen, LocalScopeBase, LocalScopeVarCount, &Result))
        {
            Error_AtStr(&Jit->Error, Str, StrLen, Line, Offset, "Undefined variable.");
        }
    } break;
    case IR_DATA_PARAM:
    {
        Result = LocationFromMem(
            Backend_CalleeSideParamMem(Data.As.Param.Index)
        );
    } break;
    default:
    {
        UNREACHABLE();
    } break;
    }
    return Result;
}





/* returns data index in IrData, or -1 if data evaluation is defered */
static int Jit_ParseUnary(jit *Jit, bool8 AllowForwardReference)
{
    int DataIndex = -1;
    jit_token Token = ConsumeToken(Jit);
    switch (Token.Type)
    {
    case TOK_PLUS:  /* positive sign (nop) */
    {
        DataIndex = Jit_ParseUnary(Jit, AllowForwardReference);
    } break;
    case TOK_MINUS: /* negate */
    {
        DataIndex = Jit_ParseUnary(Jit, AllowForwardReference);
        /* negate on ir stack */
        if (-1 == DataIndex)
        {
            Ir_Op_Push(Jit, IR_OP_NEG);
            break;
        }

        /* negate const */
        jit_ir_data Data = Ir_GetData(&Jit->IrData, DataIndex);
        if (IR_DATA_CONST == Data.Type)
        {
            double Negated = -Data.As.Const;
            Ir_PopData(&Jit->IrData);
            DataIndex = Ir_PushConstData(&Jit->IrData, Negated);
        }
        /* value is not a const, load it onto the ir stack and negate it */
        else
        {
            Ir_Op_PushLoad(Jit, DataIndex);
            Ir_Op_Push(Jit, IR_OP_NEG);
            DataIndex = -1;
        }
    } break; 
    case TOK_NUMBER: /* number */
    {
        DataIndex = Ir_PushConstData(&Jit->IrData, Token.As.Number);
    } break;
    case TOK_LPAREN: /* (expr) */
    {
        DataIndex = Jit_Ir_CompileExpr(Jit, PREC_EXPR, AllowForwardReference);
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after expression.");
    } break;
    case TOK_IDENTIFIER:
    {
        /* function call */
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            int ArgCount = 0;
            Ir_Op_Push(Jit, IR_OP_CALL_ARG_START);
            if (TOK_RPAREN != NextToken(Jit).Type)
            {
                /* compile the arguments and load them on the ir stack */
                do {
                    int Arg = Jit_Ir_CompileExpr(Jit, PREC_EXPR, AllowForwardReference);
                    if (Arg > -1)
                    {
                        Ir_Op_PushLoad(Jit, Arg);
                    }
                    ArgCount++;
                } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
            }
            ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument list.");

            /* call Function with ArgCount arguments on stack */
            Ir_Op_PushCall(Jit, ArgCount, &Token);

            /* no data index, result on ir stack */
            DataIndex = -1;
        }
        /* variable reference */
        else
        {
            if (!AllowForwardReference
            && !Jit_FindVariable(Jit, Token.Str.Ptr, Token.Str.Len, 0, 0, NULL))
            {
                Error_AtToken(&Jit->Error, &Token, "Variable was not defined before use.");
                break;
            }
            DataIndex = Ir_PushRefData(&Jit->IrData, &Token);
        }
    } break;
    default:
    {
        Error_AtToken(&Jit->Error, &Token, "Expected an expression.");
    } break;
    }
    return DataIndex;
}

static precedence PrecedenceOf(jit_token_type Operator)
{
    switch (Operator)
    {
    case TOK_PLUS:
    case TOK_MINUS:
        return PREC_PLUSMINUS;
    case TOK_STAR:
    case TOK_SLASH:
        return PREC_MULDIV;
    case TOK_CARET:
        return PREC_POW;
    default: return PREC_NONE;
    }
}


static int Jit_Ir_CompileExpr(jit *Jit, precedence Prec, bool8 AllowForwardReference)
{
    int Left = Jit_ParseUnary(Jit, AllowForwardReference);
    while (PrecedenceOf(NextToken(Jit).Type) >= Prec)
    {
        jit_token_type Oper = ConsumeToken(Jit).Type;
        int Right = Jit_Ir_CompileExpr(Jit, PrecedenceOf(Oper) + 1, AllowForwardReference);

        if (Left != -1 && Right != -1)
        {
            jit_ir_data LeftData = Ir_GetData(&Jit->IrData, Left);
            jit_ir_data RightData = Ir_GetData(&Jit->IrData, Right);
            /* const expr, evaluate data now */
            if (IR_DATA_CONST == LeftData.Type
            && IR_DATA_CONST == RightData.Type)
            {
                double ConstLeft = LeftData.As.Const;
                double ConstRight = RightData.As.Const;
                double Result = 0;
                switch (Oper)
                {
                case TOK_PLUS:          Result = ConstLeft + ConstRight; break;
                case TOK_MINUS:         Result = ConstLeft - ConstRight; break;
                case TOK_STAR:          Result = ConstLeft * ConstRight; break;
                case TOK_SLASH:         Result = ConstLeft / ConstRight; break;
                case TOK_LESS:          Result = ConstLeft < ConstRight; break;
                case TOK_LESS_EQUAL:    Result = ConstLeft <= ConstRight; break;
                case TOK_GREATER:       Result = ConstLeft > ConstRight; break;
                case TOK_GREATER_EQUAL: Result = ConstLeft >= ConstRight; break;
                default:
                {
                    UNREACHABLE();
                } break;
                }
                Ir_PopData(&Jit->IrData);
                Ir_PopData(&Jit->IrData);
                Left = Ir_PushConstData(&Jit->IrData, Result);

                continue;
            }
            /* otherwise, data evaluation is deferred to run time (ir stack) */
        }
        /* otherwise, data evaluation is deferred to run time (ir stack) */

        if (Left != -1)
            Ir_Op_PushLoad(Jit, Left);
        if (Right != -1)
            Ir_Op_PushLoad(Jit, Right);
        /* emit swap if Right operand was in the data array first */
        if (Left != -1 && -1 == Right)
            Ir_Op_Push(Jit, IR_OP_SWAP);

        switch (Oper)
        {
        case TOK_PLUS:          Ir_Op_Push(Jit, IR_OP_ADD); break;
        case TOK_MINUS:         Ir_Op_Push(Jit, IR_OP_SUB); break;
        case TOK_STAR:          Ir_Op_Push(Jit, IR_OP_MUL); break;
        case TOK_SLASH:         Ir_Op_Push(Jit, IR_OP_DIV); break;
        case TOK_LESS:          Ir_Op_Push(Jit, IR_OP_LESS); break;
        case TOK_LESS_EQUAL:    Ir_Op_Push(Jit, IR_OP_LESS_EQUAL); break;
        case TOK_GREATER:       Ir_Op_Push(Jit, IR_OP_GREATER); break;
        case TOK_GREATER_EQUAL: Ir_Op_Push(Jit, IR_OP_GREATER_EQUAL); break;
        default:
        {
            UNREACHABLE();
        } break;
        }
        Left = -1;
    }
    return Left;
}







jit_function *Jit_FindFunction(jit *Jit, 
    const char *FnNameStr, 
    i32 FnNameStrLen, 
    i32 FnNameLine, 
    i32 FnNameOffset, 
    int ArgCount)
{
    def_table_entry *Entry = DefTable_Find(
        &Jit->Global, FnNameStr, FnNameStrLen, TYPE_FUNCTION
    );

    if (!Entry)
    {
        Error_AtStr(&Jit->Error, 
            FnNameStr, 
            FnNameStrLen, 
            FnNameLine, 
            FnNameOffset, 
            "Undefined function."
        );
        return NULL;
    }

    /* check param and arg count */
    jit_function *Function = &Entry->As.Function;
    if (Entry->As.Function.ParamCount != ArgCount)
    {
        const char *Plural = Function->ParamCount > 1? 
            "arguments" : "argument";
        Error_AtStr(&Jit->Error, 
            FnNameStr, 
            FnNameStrLen,
            FnNameLine, 
            FnNameOffset,
            "Expected %d %s, got %d instead.", 
            Function->ParamCount, Plural, ArgCount
        );
        return NULL;
    }

    return Function;
}

jit_function *Jit_DefineFunction(jit *Jit, const char *Name, int NameLen)
{
    def_table_entry *Label = DefTable_Define(&Jit->Global, Name, NameLen, TYPE_FUNCTION);
    jit_function *Function = &Label->As.Function;

    Function->Location = INVALID_FN_LOCATION;
    Function->ParamStart = 0;
    Function->ParamCount = 0;
    return Function;
}









static void Jit_Function_Decl(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */
    jit_function *Function = Jit_DefineFunction(Jit, FnName->Str.Ptr, FnName->Str.Len);

    /* function params */
    Function->ParamCount = 0;
    if (TOK_RPAREN != NextToken(Jit).Type)
    {
        do {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
            jit_token Parameter = CurrToken(Jit); 
            Ir_PushParamData(&Jit->IrData, Parameter.Str, Function->ParamCount);
            Function->ParamCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    /* because IrData is a downward-growing stack, Param starting point is the latest param */
    Function->ParamStart = Jit->IrData.ByteCount;
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after function declaration.");


    /* compile the block */
    i32 BlockLocation = Ir_Op_StartBlock(Jit, IR_OP_FN_BLOCK, Function);
    {
        /* function body */
        int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR, true);
        if (Index != -1)
        {
            Ir_Op_PushLoad(Jit, Index);
        }
        Ir_Op_Push(Jit, IR_OP_RETURN);
    }
    Ir_Op_EndBlock(Jit, BlockLocation);


    /* link previous function block to here if available */
    if (Jit->PrevFnBlock != INVALID_BLOCK_LOCATION)
    {
        Ir_Op_LinkBlock(Jit, Jit->PrevFnBlock, BlockLocation);
    }
    /* update prev block */
    Jit->PrevFnBlock = BlockLocation; 
    /* update head if this block was the first function block */
    if (INVALID_BLOCK_LOCATION == Jit->FnBlockHead)
    {
        Jit->FnBlockHead = BlockLocation;
    }
}

static void Jit_Variable_Decl(jit *Jit, const jit_token *VarName)
{
    /* consumed VarName */
    def_table_entry *Entry = DefTable_Define(&Jit->Global, VarName->Str.Ptr, VarName->Str.Len, TYPE_VARIABLE);
    jit_variable *Variable = &Entry->As.Variable;

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after variable name.");
    

    /* compile the block */
    i32 BlockLocation = Ir_Op_StartBlock(Jit, IR_OP_VAR_BLOCK, Variable);
    {
        /* expression */
        int Index = Jit_Ir_CompileExpr(Jit, PREC_EXPR, false);
        if (Index != -1)
        {
            Ir_Op_PushLoad(Jit, Index);
        }
        /* store the result of the expression */
        Variable->Location = LocationFromMem(
            Backend_AllocateGlobal(&Jit->Backend, 0)
        );
        Ir_Op_PushOp4(Jit, IR_OP_STORE, Variable->Location.As.Mem.Offset);
    }
    Ir_Op_EndBlock(Jit, BlockLocation);
    

    /* link previous function block to here if available */
    if (Jit->PrevVarBlock != INVALID_BLOCK_LOCATION)
    {
        Ir_Op_LinkBlock(Jit, Jit->PrevVarBlock, BlockLocation);
    }
    /* update prev block */
    Jit->PrevVarBlock = BlockLocation; 
    /* update head if this block was the first variable block */
    if (INVALID_BLOCK_LOCATION == Jit->VarBlockHead)
    {
        Jit->VarBlockHead = BlockLocation;
    }
}

static u32 Jit_Hash(const char *Str, int StrLen)
{
    if (0 == StrLen)
        return 0;
    return (u32)(Str[0] & 0x7F) << 8 | Str[StrLen - 1];
}


static void Jit_Reset(jit *Jit, const char *Src, jit_compilation_flags Flags)
{
    Jit->FnBlockHead = INVALID_BLOCK_LOCATION;
    Jit->PrevFnBlock = INVALID_BLOCK_LOCATION;
    Jit->VarBlockHead = INVALID_BLOCK_LOCATION;
    Jit->PrevVarBlock = INVALID_BLOCK_LOCATION;
    Jit->Flags = Flags;
    Jit->Start = Src; 
    Jit->End = Src;
    Jit->Line = 1;
    Jit->Offset = 1;

    Jit_Scratchpad_Reset(&Jit->S, Jit->S.Ptr, Jit->S.Capacity);

    /* ir op array starts from the left and grows rightward in the scratchpad */
    Jit->IrOpByteCount = 0;
    Jit->IrOp = Jit_Scratchpad_LeftPtr(&Jit->S);
    /* ir data array starts from the right and grows leftward in the scratchpad */
    Jit->IrData.SrcBegin = Src;
    Jit->IrData.S = &Jit->S;
    Jit->IrData.ByteCount = 0;
    Jit->IrData.Ptr = Jit_Scratchpad_RightPtr(&Jit->S);

    /* pump the tokenizer */
    Jit->Next = (jit_token) { .Type = TOK_EOF };
    ConsumeToken(Jit);

    /* reset other members */
    Error_Reset(&Jit->Error);
    DefTable_Reset(&Jit->Global);

    bool8 EmitFloat32Instructions = 0 != (Flags & JIT_COMPFLAG_FLOAT32);
    int FltSize = EmitFloat32Instructions? sizeof(float) : sizeof(double);
    Backend_Reset(&Jit->Backend, FltSize);
}



uint Jit_Init(
    jit *Jit,
    void *Scratchpad, uint ScratchpadCapacity, 
    void *GlobalMemory, uint GlobalMemCapacity, 
    void *ProgramMemory, uint ProgramMemCapacity,
    def_table_entry *DefTableArray, uint DefTableCapacity
)
{
    uint MinScratchpadCapacity = 64*1024;
    if (ScratchpadCapacity < MinScratchpadCapacity
    || NULL == Jit)
    {
        return MinScratchpadCapacity;
    }

    *Jit = (jit) {
        .Global = DefTable_Init(DefTableArray, DefTableCapacity, Jit_Hash),
    };
    Jit_Scratchpad_Reset(&Jit->S, Scratchpad, ScratchpadCapacity);
    Backend_Init(
        &Jit->Backend, 
        ProgramMemory, ProgramMemCapacity,
        GlobalMemory, GlobalMemCapacity
    );
    return 0;
}

void Jit_Destroy(jit *Jit)
{
    *Jit = (jit) { 0 };
}


static void PrintVars(jit *Jit)
{
    i32 Offset = Jit->IrData.ByteCount;
    int i = 0;
    while (Offset > 0)
    {
        jit_ir_data Data = Ir_GetData(&Jit->IrData, Offset);
        switch (Data.Type)
        {
        case IR_DATA_CONST:  
        {
            printf("Const %d = %f\n", i, Data.As.Const);
        } break;
        case IR_DATA_VARREF: 
        {
            printf("VarRef %d = '%.*s'\n", i, Data.As.VarRef.StrLen, Data.As.VarRef.Str);
        } break;
        case IR_DATA_PARAM:  
        {
            printf("Param %d = '%.*s', offset=%d\n", 
                i, 
                Data.As.Param.StrLen, Data.As.Param.Str, 
                Backend_CalleeSideParamMem(Data.As.Param.Index).Offset
            );
        } break;
        default: UNREACHABLE(); break;
        }
        i++;
        Offset = Ir_GetNextDataOffset(&Jit->IrData, Offset);
    }
}

static void Jit_TranslateIrBlocks(jit *Jit, i32 BlockHead, jit_ir_stack *Stack, jit_fnref_stack *FnRef)
{
    const u8 *IP = Jit->IrOp + BlockHead;
    const u8 *End = Jit->IrOp + Jit->IrOpByteCount;
    while (IP < End)
    {
        const u8 *NextBlock = Backend_TranslateBlock(
            &Jit->Backend, 
            IP, 
            Stack, 
            FnRef, 
            Jit
        );
        if (NULL == NextBlock)
            break;
        IP = NextBlock;
    }
}


jit_result Jit_Compile(jit *Jit, jit_compilation_flags Flags, const char *Src)
{
    Jit_Reset(Jit, Src, Flags);

    do {
        /* definition */
        ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected a variable or function declaration.");
        jit_token Identifier = CurrToken(Jit);
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            Jit_Function_Decl(Jit, &Identifier);
        }
        else 
        {
            Jit_Variable_Decl(Jit, &Identifier);
        }

        /* skip all newlines */
        while (!Jit->Error.Available && ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
        {}
    } while (!Jit->Error.Available && TOK_EOF != NextToken(Jit).Type);
    if (Jit->Error.Available)
        goto ErrReturn;

    printf("IR OK, free mem: %d\n", Jit_Scratchpad_BytesRemain(&Jit->S));
#if 1
    printf("============= var table ============\n");
    PrintVars(Jit);

#else
    printf("============= instructions ============\n");
    for (int i = 0; i < Ir_GetOpCount(Jit); i++)
    {
        jit_ir_op *Op = Jit->IrOp + i;
        printf("%3d: %s\n", i, Get_OpName(Op->Type));
    }
#endif

    /* translate blocks */
    jit_ir_stack Stack = Ir_Stack_Init(Jit);
    jit_fnref_stack FnRef = Ir_FnRef_Init(&Jit->S);
    Jit_TranslateIrBlocks(Jit, Jit->FnBlockHead, &Stack, &FnRef);
    {
        jit_function *Function = Jit_DefineFunction(Jit, "#init", 5);
        Function->Location = Backend_EmitFunctionEntry(&Jit->Backend);
        Jit_TranslateIrBlocks(Jit, Jit->VarBlockHead, &Stack, &FnRef);
        Backend_EmitFunctionExit(&Jit->Backend, Function->Location);
        Function->InsByteCount = Backend_GetProgramSize(&Jit->Backend) - Function->Location;
    }

    /* resolve function calls */
    int FnRefCount = Ir_FnRef_Count(&FnRef);
    for (int i = 0; i < FnRefCount; i++)
    {
        jit_fnref *Ref = Ir_FnRef_Pop(&FnRef);
        ASSERT(Ref->Function, "nullpltr");
        Backend_PatchCall(&Jit->Backend, Ref->Location, Ref->Function->Location);
    }

    Backend_Disassemble(&Jit->Backend, &Jit->Global);
    if (Jit->Error.Available)
        goto ErrReturn;

    return (jit_result) {
        .GlobalData = Backend_GetDataPtr(&Jit->Backend),
        .GlobalSymbol = Jit->Global.Head,
    };

ErrReturn:
    return (jit_result) {
        .ErrMsg = Jit->Error.Msg,
    };
}


jit_init32 Jit_GetInit32(jit *Jit, const jit_result *Result)
{
    def_table_entry *Init = Jit->Global.Tail;
    ASSERT(Init, "nullptr");
    ASSERT(Init->Type == TYPE_FUNCTION, "unreachable");
    ASSERT(StrEqu(Init->As.Str.Ptr, "#init", 5), "unreachable");
    return (jit_init32)Jit_GetFunctionPtr(Jit, &Init->As.Function);
}

jit_init64 Jit_GetInit64(jit *Jit, const jit_result *Result)
{
    def_table_entry *Init = Jit->Global.Tail;
    ASSERT(Init, "nullptr");
    ASSERT(Init->Type == TYPE_FUNCTION, "unreachable");
    ASSERT(StrEqu(Init->As.Str.Ptr, "#init", 5), "unreachable");
    return (jit_init64)Jit_GetFunctionPtr(Jit, &Init->As.Function);
}

const void *Jit_GetFunctionPtr(jit *Jit, const jit_function *Fn)
{
    ASSERT(Fn->Location < Backend_GetProgramSize(&Jit->Backend), "Function with invalid location");
    ASSERT(Fn->Location + Fn->InsByteCount <= Backend_GetProgramSize(&Jit->Backend), "Function with invalid size");
    const u8 *FnPtr = Backend_GetProgramPtr(&Jit->Backend) + Fn->Location;
    return FnPtr;
}

