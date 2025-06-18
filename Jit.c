#include "JitCommon.h"
#include "JitBackend.h"
#include "Jit.h"
#include "DefTable.h"

#include <stdio.h>
#include <math.h>
#include <stdarg.h>
#include <string.h> /* memset */


typedef enum precedence 
{
    PREC_NONE = 0,
    PREC_EXPR, 
    PREC_COMPARE,
    PREC_PLUSMINUS,
    PREC_MULDIV,
    PREC_POW,
    PREC_UNARY,
} precedence;

typedef struct jit_eval_data jit_eval_data;
static void Jit_ParseExpr(jit *Jit, precedence Prec, bool8 AllowForwardReference);


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

void *Jit_Scratchpad_LeftPtr(jit_scratchpad *S)
{
    return S->Ptr + S->LeftCount;
}

static void *Jit_Scratchpad_RightPtr(jit_scratchpad *S)
{
    return S->Ptr + S->Capacity - S->RightCount;
}

void *Jit_Scratchpad_PushLeft(jit_scratchpad *S, int DataSize)
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

void *Jit_Scratchpad_PopLeft(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->LeftCount, "bad data size");
    S->LeftCount -= DataSize;
    return Jit_Scratchpad_LeftPtr(S);
}

static void *Jit_Scratchpad_PopRight(jit_scratchpad *S, int DataSize)
{
    ASSERT(DataSize <= S->RightCount, "bad data size");
    void *Data = Jit_Scratchpad_RightPtr(S);
    S->RightCount -= DataSize;
    return Data;
}


#if 0
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


/*========================================================== 
 *                        fn ref
 *==========================================================*/

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


/*========================================================== 
 *                          IR OP
 *==========================================================*/


static u8 *Bytecode_PushAndReserveArgSize(jit *Jit, jit_ir_op_type Type, int ArgSize)
{
    Jit->IrOpByteCount += 1 + ArgSize;
    u8 *Ptr = Jit_Scratchpad_PushLeft(&Jit->S, 1 + ArgSize);
    *Ptr = Type;
    return Ptr + 1;
}
static void Bytecode_Push(jit *Jit, jit_ir_op_type Type)
{
    Bytecode_PushAndReserveArgSize(Jit, Type, 0);
}
static void Bytecode_PushOp4(jit *Jit, jit_ir_op_type Type, i32 Arg)
{
    ASSERT(Type <= 0xFF, "unreachable");

    u8 *ArgPtr = Bytecode_PushAndReserveArgSize(Jit, Type, sizeof(i32));
    MemCpy(ArgPtr, &Arg, sizeof(i32));
}
static void Bytecode_PushCall(jit *Jit, i32 ArgCount, const jit_token *FnName)
{
    u8 *ArgPtr = Bytecode_PushAndReserveArgSize(Jit, IR_OP_CALL,
        Bytecode_GetArgSize(IR_OP_CALL)
    );
    MemCpy(ArgPtr, &FnName->Str.Ptr, sizeof(const char *));
    MemCpy(ArgPtr + 8, &FnName->Str.Len, 4);
    MemCpy(ArgPtr + 8 + 4, &FnName->Line, 4);
    MemCpy(ArgPtr + 8 + 2*4, &FnName->Offset, 4);
    MemCpy(ArgPtr + 8 + 3*4, &ArgCount, 4);
}
static i32 Bytecode_StartBlock(jit *Jit, jit_ir_op_type Op, const void *Ptr)
{
    i32 Location = Jit->IrOpByteCount;
    u8 *ArgPtr = Bytecode_PushAndReserveArgSize(Jit, Op, Bytecode_GetArgSize(Op));
    MemCpy(ArgPtr, &Ptr, sizeof(void *));
    return Location;
}
static void Bytecode_EndBlock(jit *Jit, i32 Location)
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
static void Bytecode_LinkBlock(jit *Jit, i32 PrevBlockLocation, i32 CurrBlockLocation)
{
    ASSERT(PrevBlockLocation < Jit->IrOpByteCount, "invalid location");
    ASSERT(PrevBlockLocation >= 0, "invalid location");

    /* calculate offset */
    i32 Offset = CurrBlockLocation - (PrevBlockLocation + 1 + Bytecode_GetArgSize(IR_OP_FN_BLOCK));
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
static void Bytecode_PushLoad(jit *Jit, i32 Index)
{
    Bytecode_PushOp4(Jit, IR_OP_LOAD, Index);
}



/*========================================================== 
 *                        IR DATA
 *=========================================================*/
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


#else

/*========================================================== 
 *                    REFERENCE RECORD
 *==========================================================*/
typedef enum reference_type 
{
    REF_FUNCTION,
    REF_VARIABLE,
} reference_type;
typedef struct reference 
{
    reference_type Type;
    const char *Str;
    i32 Len;
    i32 Line, Offset;
    i32 RefLocation;
    i32 ArgCount;
} reference;

void RefStack_PushVariableReference(
    jit *Jit, 
    const char *Name, i32 Len, 
    i32 Line, i32 Offset, 
    i32 RefLocation)
{
    reference *Ptr = Jit_Scratchpad_PushRight(&Jit->FrontendData, sizeof(reference));
    *Ptr = (reference) {
        .Type = REF_VARIABLE,
        .Str = Name,
        .Len = Len,
        .Line = Line,
        .Offset = Offset,
        .RefLocation = RefLocation,
    };
}

void RefStack_PushFunctionReference(jit *Jit, const jit_token *Function, i32 ArgCount, i32 RefLocation)
{
    reference *Ptr = Jit_Scratchpad_PushRight(&Jit->FrontendData, sizeof(reference));
    *Ptr = (reference) {
        .Type = REF_FUNCTION,
        .Str = Function->Str.Ptr,
        .Len = Function->Str.Len,
        .Line = Function->Line,
        .Offset = Function->Offset,
        .RefLocation = RefLocation,
        .ArgCount = ArgCount,
    };
}

reference *RefStack_Pop(jit *Jit)
{
    return Jit_Scratchpad_PopRight(&Jit->FrontendData, sizeof(reference));
}

bool8 RefStack_IsEmpty(jit *Jit)
{
    return 0 == Jit->FrontendData.RightCount;
}





/*========================================================== 
 *                      BYTECODE STACK
 *==========================================================*/
#if 0
static i32 Bytecode_Size(const jit *Jit)
{
    return Jit->IrProgram.LeftCount;
}

static u8 *Bytecode_Push(jit *Jit, jit_ir_op_type Op)
{
    u8 *Location = Jit_Scratchpad_PushLeft(&Jit->IrProgram, Bytecode_GetArgSize(Op));
    Location[0] = Op;
    return Location + 1;
}

static i32 Bytecode_PushLoadGlobal(jit *Jit, i32 GlobalIndex)
{
    i32 Location = Jit->IrProgram.LeftCount;
    u8 *Arg = Bytecode_Push(Jit, IR_OP_LOAD_GLOBAL);
    MemCpy(Arg, &GlobalIndex, sizeof(i32));

    return Location;
}
static void Bytecode_PushLoadLocal(jit *Jit, i32 Offset)
{
    u8 *Arg = Bytecode_Push(Jit, IR_OP_LOAD_LOCAL);
    MemCpy(Arg, &Offset, sizeof(Offset));
}
static void Bytecode_PushStoreGlobal(jit *Jit, i32 Offset)
{
    u8 *Arg = Bytecode_Push(Jit, IR_OP_STORE_GLOBAL);
    MemCpy(Arg, &Offset, sizeof Offset);
}
static i32 Bytecode_PushCall(jit *Jit, u8 ArgCount)
{
    i32 Location = Jit->IrProgram.LeftCount;
    u8 *Arg = Bytecode_Push(Jit, IR_OP_CALL);
    Arg[0] = ArgCount;
    memset(Arg + 1, 0, sizeof(jit_function *));

    return Location;
}
static i32 Bytecode_StartBlock(jit *Jit, jit_ir_op_type Op, void *BlockInfo)
{
    u8 *Arg = Bytecode_Push(Jit, Op);
    

}
static void Bytecode_EndBlock(jit *Jit)
{
}
static void Bytecode_LinkBlock(jit *Jit)
{
}
#endif


/*========================================================== 
 *                      EVAL STACK
 *==========================================================*/

typedef enum jit_eval_data_type 
{
    EVAL_CONSTANT,
    EVAL_DYNAMIC,
} jit_eval_data_type;
struct jit_eval_data 
{
    jit_eval_data_type Type;
    union {
        double Const;
    } As;
};
static jit_eval_data EvalData_Const(double Const)
{
    return (jit_eval_data) {
        .Type = EVAL_CONSTANT,
        .As.Const = Const,
    };
}
static jit_eval_data EvalData_Dynamic(void)
{
    return (jit_eval_data) {
        .Type = EVAL_DYNAMIC,
    };
}


static void EvalStack_Push(jit *Jit, jit_eval_data Data)
{
    jit_eval_data *Location = Jit_Scratchpad_PushLeft(&Jit->FrontendData, sizeof(jit_eval_data));
    *Location = Data;
}

static jit_eval_data *EvalStack_Pop(jit *Jit)
{
    jit_eval_data *Location = Jit_Scratchpad_PopLeft(&Jit->FrontendData, sizeof(jit_eval_data));
    return Location;
}

static jit_eval_data *EvalStack_Top(jit *Jit)
{
    ASSERT((uint)Jit->FrontendData.LeftCount >= sizeof(jit_eval_data), "empty stack");
    jit_eval_data *Location = Jit_Scratchpad_LeftPtr(&Jit->FrontendData);
    return Location - 1;
}

static void EvalStack_LoadToRuntimeStack(jit *Jit, jit_eval_data *Data)
{
    switch (Data->Type)
    {
    case EVAL_DYNAMIC:
    /* nothing to do, loading the arg has been handled */
    {
    } break;
    case EVAL_CONSTANT:
    /* must load constant to the runtime stack */
    {
        i32 GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, Data->As.Const);
        Backend_Op_LoadGlobal(&Jit->Backend, GlobalIndex);
    } break;
    }
}



/*========================================================== 
 *                      LOCAL VAR STACK
 *==========================================================*/


static i32 LocalVarStack_Count(const jit *Jit)
{
    return Jit->BackendData.RightCount / sizeof(strview);
}

static void Jit_StartLocalScope(jit *Jit)
{
    Jit->LocalVarBase = LocalVarStack_Count(Jit);
    Jit->LocalVarEnd = Jit->LocalVarBase;
}

static void LocalVarStack_Push(jit *Jit, const jit_token *Local)
{
    Jit->LocalVarEnd++;
    strview *LocalVar = Jit_Scratchpad_PushRight(&Jit->BackendData, sizeof(strview));
    *LocalVar = Local->Str;
}

static strview *LocalVarStack_Get(jit *Jit, i32 Index)
{
    i32 LocalVarCount = LocalVarStack_Count(Jit);
    ASSERT(IN_RANGE(0, Index, LocalVarCount - 1), "invalid index");
    strview *Top = Jit_Scratchpad_RightPtr(&Jit->BackendData);
    return Top + LocalVarCount - Index - 1;
}

static void Jit_EndLocalScope(jit *Jit)
{
    Jit->LocalVarBase = -1;
}

static bool8 Jit_InLocalScope(const jit *Jit)
{
    return Jit->LocalVarBase != -1;
}




#endif










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




static void Jit_ParseUnary(jit *Jit, bool8 AllowForwardReference)
{
    jit_token Token = ConsumeToken(Jit);
    switch (Token.Type)
    {
    case TOK_NUMBER: /* push(number) */
    {
        EvalStack_Push(Jit, 
            EvalData_Const(Token.As.Number)
        );
    } break;
    case TOK_IDENTIFIER: 
    {
        /* function call: push(result of function call) */
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            int ArgCount = 0;
            Backend_Op_ArgStart(&Jit->Backend);
            if (TOK_RPAREN != NextToken(Jit).Type)
            {
                /* compile the arguments and load them on the ir stack */
                do {
                    Jit_ParseExpr(Jit, PREC_EXPR, AllowForwardReference);
                    jit_eval_data *Arg = EvalStack_Pop(Jit);
                    EvalStack_LoadToRuntimeStack(Jit, Arg);
                    ArgCount++;
                } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
            }
            ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after argument list.");

            /* emit instruction to call fn */
            i32 CallLocation = Backend_Op_Call(&Jit->Backend, ArgCount);

            /* save call location to reference record */
            RefStack_PushFunctionReference(
                Jit, 
                &Token, 
                ArgCount, 
                CallLocation
            );

            /* push function result on eval stack */
            EvalStack_Push(Jit, EvalData_Dynamic());
        }
        /* variable reference */
        /* push(expr dyn) */
        else
        {
#if 0
            if (!AllowForwardReference
            && !Jit_FindVariable(Jit, Token.Str.Ptr, Token.Str.Len, 0, 0, NULL))
            {
                Error_AtToken(&Jit->Error, &Token, "Variable was not defined before use.");
                break;
            }
#endif
            /* emit instruction to load variable */
            /* search in local scope */
            if (Jit_InLocalScope(Jit))
            {
                for (int i = Jit->LocalVarBase; i < Jit->LocalVarEnd; i++)
                {
                    strview *Local = LocalVarStack_Get(Jit, i);
                    if (Local->Len == Token.Str.Len 
                    && StrEqu(Local->Ptr, Token.Str.Ptr, Token.Str.Len))
                    {
                        /* found the local variable being referenced, 
                         * emit instruction to load it */
                        Backend_Op_LoadLocal(&Jit->Backend, i - Jit->LocalVarBase);
                        /* load it onto the eval stack */
                        EvalStack_Push(Jit, EvalData_Dynamic());
                        return;
                    }
                }
                /* unable to find local variable, switch to global */
            }
            /* not in local scope, switch to global */

            /* global variable references will be resolved in the reference resolution stage */
            /* emit instruction to load the variable */
            i32 RefLocation = Backend_Op_LoadGlobal(&Jit->Backend, INT32_MAX/4);

            /* push variable info onto the reference record */
            RefStack_PushVariableReference(Jit, 
                Token.Str.Ptr, 
                Token.Str.Len, 
                Token.Line, 
                Token.Offset, 
                RefLocation
            );

            /* push dynamic variable onto the eval stack */
            EvalStack_Push(Jit, EvalData_Dynamic());
        }
    } break;
    case TOK_PLUS:  /* positive sign (nop) */
    {
        Jit_ParseUnary(Jit, AllowForwardReference);
    } break;
    case TOK_MINUS: /* negate */
    {
        Jit_ParseUnary(Jit, AllowForwardReference);
        jit_eval_data *Top = EvalStack_Top(Jit);
        switch (Top->Type)
        {
        case EVAL_DYNAMIC:
        {
            Backend_Op_Neg(&Jit->Backend);
        } break;
        case EVAL_CONSTANT:
        {
            Top->As.Const = -Top->As.Const;
        } break;
        }
    } break; 
    case TOK_LPAREN: /* push('(expr)') */
    {
        Jit_ParseExpr(Jit, PREC_EXPR, AllowForwardReference);
        ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after expression.");
    } break;
    default:
    {
        Error_AtToken(&Jit->Error, &Token, "Expected an expression.");
        EvalStack_Push(Jit, EvalData_Dynamic()); /* pushes dummy */
    } break;
    }
}

static precedence PrecedenceOf(jit_token_type Operator)
{
    switch (Operator)
    {
    case TOK_LESS:
    case TOK_LESS_EQUAL:
    case TOK_GREATER:
    case TOK_GREATER_EQUAL:
        return PREC_COMPARE;
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


/* returns the stack's top */
static void Jit_ParseExpr(jit *Jit, precedence Prec, bool8 AllowForwardReference)
{
    Jit_ParseUnary(Jit, AllowForwardReference);
    while (PrecedenceOf(NextToken(Jit).Type) >= Prec)
    {
        jit_token_type Oper = ConsumeToken(Jit).Type;
        Jit_ParseExpr(Jit, PrecedenceOf(Oper) + 1, AllowForwardReference);

        jit_eval_data *Right = EvalStack_Pop(Jit);
        jit_eval_data *Left = EvalStack_Pop(Jit);
        jit_eval_data ResultData;

        if (EVAL_CONSTANT == Right->Type
        && EVAL_CONSTANT == Left->Type) /* constant expr, evaluate now */
        {
            double Result = 0;
            switch (Oper)
            {
            case TOK_PLUS:          Result = Left->As.Const + Right->As.Const; break;
            case TOK_MINUS:         Result = Left->As.Const - Right->As.Const; break;
            case TOK_STAR:          Result = Left->As.Const * Right->As.Const; break;
            case TOK_SLASH:         Result = Left->As.Const / Right->As.Const; break;
            case TOK_LESS:          Result = Left->As.Const < Right->As.Const; break;
            case TOK_LESS_EQUAL:    Result = Left->As.Const <= Right->As.Const; break;
            case TOK_GREATER:       Result = Left->As.Const > Right->As.Const; break;
            case TOK_GREATER_EQUAL: Result = Left->As.Const >= Right->As.Const; break;
            default:
            {
                UNREACHABLE();
            } break;
            }
            ResultData = EvalData_Const(Result);
        }
        else /* runtime expr */
        {
            if (EVAL_CONSTANT == Left->Type)
            {
                /* right operand was pushed onto backend's eval stack first, swap it */
                i32 GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, Left->As.Const);
                Backend_Op_LoadGlobal(&Jit->Backend, GlobalIndex);
                Backend_Op_Swap(&Jit->Backend);
            }
            if (EVAL_CONSTANT == Right->Type)
            {
                /* left operand was pushed onto the backend's eval stack first, this is ok */
                i32 GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, Right->As.Const);
                Backend_Op_LoadGlobal(&Jit->Backend, GlobalIndex);
            }

            switch (Oper)
            {
            case TOK_PLUS:          Backend_Op_Add(&Jit->Backend); break;
            case TOK_MINUS:         Backend_Op_Sub(&Jit->Backend); break;
            case TOK_STAR:          Backend_Op_Mul(&Jit->Backend); break;
            case TOK_SLASH:         Backend_Op_Div(&Jit->Backend); break;
            case TOK_LESS:          Backend_Op_Less(&Jit->Backend); break;
            case TOK_GREATER:       Backend_Op_Greater(&Jit->Backend); break;
            case TOK_LESS_EQUAL:    Backend_Op_LessOrEqual(&Jit->Backend); break;
            case TOK_GREATER_EQUAL: Backend_Op_GreaterOrEqual(&Jit->Backend); break;
            default:
            {
                UNREACHABLE();
            } break;
            }
            ResultData = EvalData_Dynamic();
        }

        EvalStack_Push(Jit, ResultData);
        Left = EvalStack_Top(Jit);
    }
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

    Function->Location = INT32_MAX;
    Function->ParamStart = 0;
    Function->ParamCount = 0;
    return Function;
}









static void Jit_Function_Decl(jit *Jit, const jit_token *FnName)
{
    /* consumed '(' */
    jit_function *Function = Jit_DefineFunction(Jit, FnName->Str.Ptr, FnName->Str.Len);
    Jit_StartLocalScope(Jit);

    /* function params */
    Function->ParamCount = 0;
    Function->ParamStart = Jit->LocalVarBase;
    if (TOK_RPAREN != NextToken(Jit).Type)
    {
        do {
            ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected parameter name.");
            jit_token Parameter = CurrToken(Jit); 
            LocalVarStack_Push(Jit, &Parameter);
            Function->ParamCount++;
        } while (ConsumeIfNextTokenIs(Jit, TOK_COMMA));
    }
    /* because IrData is a downward-growing stack, Param starting point is the latest param */
    ConsumeOrError(Jit, TOK_RPAREN, "Expected ')' after parameter list.");
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after function declaration.");


    /* compile the block */
    Function->Location = Backend_Op_FnEntry(&Jit->Backend, Function->ParamCount);
    {
        /* function body */
        Jit_ParseExpr(Jit, PREC_EXPR, true);
        jit_eval_data *Result = EvalStack_Pop(Jit); /* pop result since we don't need it anymore */
        EvalStack_LoadToRuntimeStack(Jit, Result);
    }
    i32 FnEnd = Backend_Op_FnReturn(&Jit->Backend, Function->Location, true);
    Function->InsByteCount = FnEnd - Function->Location;

    Jit_EndLocalScope(Jit);
}

static void Jit_Variable_Decl(jit *Jit, const jit_token *VarName)
{
    /* consumed VarName */
    def_table_entry *Entry = DefTable_Define(&Jit->Global, VarName->Str.Ptr, VarName->Str.Len, TYPE_VARIABLE);
    jit_variable *Variable = &Entry->As.Variable;

    /* equal sign */
    ConsumeOrError(Jit, TOK_EQUAL, "Expected '=' after variable name.");
    
    /* compile the block */
    {
        /* expression */
        Jit_ParseExpr(Jit, PREC_EXPR, true);
        jit_eval_data *Result = EvalStack_Pop(Jit);
        switch (Result->Type)
        {
        case EVAL_DYNAMIC:
        /* dynamic expression, pop runtime stack and load to global */
        {
            Variable->GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, 0.0);
            Backend_Op_StoreGlobal(&Jit->Backend, Variable->GlobalIndex);
        } break;
        case EVAL_CONSTANT:
        /* constant expression, no instructions needed */
        {
            Variable->GlobalIndex = Backend_AllocateGlobal(&Jit->Backend, Result->As.Const);
        } break;
        }
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
    Jit->Flags = Flags;
    Jit->Start = Src; 
    Jit->End = Src;
    Jit->Line = 1;
    Jit->Offset = 1;

#if 0
    Jit_Scratchpad_Reset(&Jit->S, Jit->S.Ptr, Jit->S.Capacity);

    /* ir op array starts from the left and grows rightward in the scratchpad */
    Jit->IrOpByteCount = 0;
    Jit->IrOp = Jit_Scratchpad_LeftPtr(&Jit->S);
    /* ir data array starts from the right and grows leftward in the scratchpad */
    Jit->IrData.SrcBegin = Src;
    Jit->IrData.S = &Jit->S;
    Jit->IrData.ByteCount = 0;
    Jit->IrData.Ptr = Jit_Scratchpad_RightPtr(&Jit->S);
#else
    Jit_Scratchpad_Reset(&Jit->FrontendData, Jit->FrontendData.Ptr, Jit->FrontendData.Capacity);
    Jit_Scratchpad_Reset(&Jit->BackendData, Jit->BackendData.Ptr, Jit->BackendData.Capacity);
#endif

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
    u8 *ScratchpadPtr = Scratchpad;
    Jit_Scratchpad_Reset(&Jit->FrontendData, ScratchpadPtr, ScratchpadCapacity/2);
    Jit_Scratchpad_Reset(&Jit->BackendData, ScratchpadPtr + ScratchpadCapacity/2, ScratchpadCapacity/2);
    Backend_Init(
        &Jit->Backend, 
        &Jit->BackendData,
        ProgramMemory, ProgramMemCapacity,
        GlobalMemory, GlobalMemCapacity
    );
    return 0;
}

void Jit_Destroy(jit *Jit)
{
    *Jit = (jit) { 0 };
}


#if 0
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
#else
jit_result Jit_Compile(jit *Jit, jit_compilation_flags Flags, const char *Src)
{
    Jit_Reset(Jit, Src, Flags);

    /* compilation stage */
    /* newlines */
    while (ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
    {}

    bool8 VarDeclared = false;
    bool8 FnDeclared = false;
    jit_function *Init = Jit_DefineFunction(Jit, "#init", 5);
    do {
        /* definition */
        ConsumeOrError(Jit, TOK_IDENTIFIER, "Expected a variable or function declaration.");
        jit_token Identifier = CurrToken(Jit);
        if (ConsumeIfNextTokenIs(Jit, TOK_LPAREN))
        {
            if (!FnDeclared)
            {
                if (!VarDeclared)
                {
                    Init->Location = Backend_Op_FnEntry(&Jit->Backend, 0);
                }
                Init->InsByteCount = Backend_Op_FnReturn(&Jit->Backend, Init->Location, false) - Init->Location;
                FnDeclared = true;
            }
            Jit_Function_Decl(Jit, &Identifier);
        }
        else 
        {
            if (FnDeclared)
            {
                Error_AtToken(&Jit->Error, &Identifier, "Variable declaration cannot be after function declaration.");
                break;
            }
            if (!VarDeclared)
            {
                Init->Location = Backend_Op_FnEntry(&Jit->Backend, 0);
                VarDeclared = true;
            }
            Jit_Variable_Decl(Jit, &Identifier);
        }

        /* skip all newlines */
        while (!Jit->Error.Available && ConsumeIfNextTokenIs(Jit, TOK_NEWLINE))
        {}
    } while (!Jit->Error.Available && TOK_EOF != NextToken(Jit).Type);
    if (Jit->Error.Available)
        goto ErrReturn;


    /* ref-patching stage */
    const def_table_entry_type Types[] = {
        [REF_FUNCTION] = TYPE_FUNCTION,
        [REF_VARIABLE] = TYPE_VARIABLE,
    };
    const char *StrTypes[] = {
        [REF_FUNCTION] = "function",
        [REF_VARIABLE] = "variable",
    };
    while (!RefStack_IsEmpty(Jit))
    {
        reference *Ref = RefStack_Pop(Jit);
        def_table_entry *Entry = DefTable_Find(
            &Jit->Global, 
            Ref->Str, 
            Ref->Len, 
            Types[Ref->Type]
        );
        if (NULL == Entry)
        {
            Error_AtStr(
                &Jit->Error, 
                Ref->Str, 
                Ref->Len, 
                Ref->Line, 
                Ref->Offset, 
                "Undefined reference to %s.",
                StrTypes[Ref->Type]
            );
            break;
        }

        switch (Ref->Type)
        {
        case REF_FUNCTION:
        {
            Backend_Patch_Call(&Jit->Backend, Ref->RefLocation, Entry->As.Function.Location);
        } break;
        case REF_VARIABLE:
        {
            Backend_Patch_LoadGlobal(&Jit->Backend, Ref->RefLocation, Entry->As.Variable.GlobalIndex);
        } break;
        }
    }

    if (Jit->Error.Available)
        goto ErrReturn;

    /* done */
    Backend_Disassemble(&Jit->Backend, &Jit->Global);
    extern void exit(int);
    exit(0);
    return (jit_result) {
        .GlobalData = Backend_GetDataPtr(&Jit->Backend),
        .GlobalSymbol = Jit->Global.Head,
    };
ErrReturn:
    return (jit_result) {
        .ErrMsg = Jit->Error.Msg,
    };
}
#endif


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

