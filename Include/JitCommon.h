#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "Common.h"


typedef struct jit jit;
typedef struct jit_storage_manager jit_storage_manager;
typedef struct jit_emitter jit_emitter;


/*=======================================
 *              jit_location 
 *======================================= */
typedef enum jit_location_type 
{
    LOCATION_MEM = 1,
    LOCATION_REG,
} jit_location_type;
typedef i8 jit_reg;
typedef struct jit_mem 
{
    i32 Offset;
    jit_reg BaseReg;
} jit_mem;
typedef struct jit_location
{
    jit_location_type Type;
    union {
        jit_mem Mem;
        jit_reg Reg;
    } As;
} jit_location;
#define JIT_REG_INVALID -1
static inline jit_location LocationFromReg(jit_reg R)
{
    return (jit_location) {
        .Type = LOCATION_REG,
        .As.Reg = R,
    };
}
static inline jit_location LocationFromMemVerbose(jit_reg Base, i32 Offset)
{
    return (jit_location) {
        .Type = LOCATION_MEM,
        .As.Mem = {
            .BaseReg = Base,
            .Offset = Offset,
        },
    };
}
static inline jit_location LocationFromMem(jit_mem Mem)
{
    return (jit_location) {
        .Type = LOCATION_MEM,
        .As.Mem = Mem,
    };
}



typedef struct jit_variable
{
    strview Name;
    int InsLocation, InsByteCount;
    jit_location Location;
} jit_variable;

typedef struct jit_function 
{
    strview Name;
    int Location, InsByteCount;
    int ParamStart;
    int ParamCount;
} jit_function;


/* internal jit functions */
typedef struct jit_scratchpad 
{
    u8 *Ptr;
    int LeftCount, RightCount, Capacity;
} jit_scratchpad;




/*=======================================
 *              jir_ir_data
 *======================================= */


/* stored jit_ir_data format: 
 * IR_DATA_CONST:
 *  u8 Tag
 *  double Const
 * IR_DATA_VARREF
 *  u8 Tag
 *  i32 StrBegin
 *  i32 StrLen 
 *  i32 Line 
 *  i32 LineOffset
 * IR_DATA_PARAM:
 *  u8 Tag
 *  i32 StrBegin
 *  i32 StrLen
 *  i32 ParamIndex
 * */
typedef enum jit_ir_data_type 
{
    IR_DATA_CONST = sizeof(double),
    IR_DATA_PARAM = 3*sizeof(i32),
    IR_DATA_VARREF = 4*sizeof(i32),
} jit_ir_data_type;
typedef struct jit_ir_data 
{
    jit_ir_data_type Type;
    union {
        struct {
            const char *Str;
            i32 StrLen;
            int Index;
        } Param;
        struct {
            const char *Str;
            i32 StrLen;
            i32 Line;
            i32 Offset;
        } VarRef;
        double Const;
    } As;
} jit_ir_data;
typedef struct jit_ir_data_manager 
{
    const char *SrcBegin;
    jit_scratchpad *S;
    u8 *Ptr; /* data is stored in a packed form */
    int ByteCount;
} jit_ir_data_manager;

jit_location Ir_Data_GetLocation(jit *Jit, i32 DataIndex, int LocalScopeBase, int LocalScopeVarCount);
jit_ir_data Ir_GetData(jit_ir_data_manager *Data, i32 Offset);



/*=======================================
 *              jir_ir_stack
 *======================================= */
typedef struct jit_ir_stack 
{
    jit_scratchpad *S;
    int Count;
} jit_ir_stack;

jit_location *Ir_Stack_Top(jit_ir_stack *Stack, int Offset);
jit_location *Ir_Stack_Push(jit_ir_stack *Stack, const jit_location *Value);
jit_location *Ir_Stack_Pop(jit_ir_stack *Stack);
void Ir_Stack_PopMultiple(jit_ir_stack *Stack, int Count);


/*=======================================
 *              jir_fnref_stack
 *======================================= */
typedef struct jit_fnref_stack
{
    jit_scratchpad *S;
    int Base;
} jit_fnref_stack;

typedef struct jit_fnref 
{
    const jit_function *Function;
    uint Location;
} jit_fnref;

jit_fnref *Ir_FnRef_Push(jit_fnref_stack *FnRef, const jit_function *Function, uint CallLocation);



#define INVALID_BLOCK_LOCATION INT32_MAX
#define INVALID_FN_LOCATION INT32_MAX
/* these instruction operates on a stack: 
 *  stack[n] denotes the elem on the n-th position in said stack
 *  push(elem): pushes an element onto the evaluation stack (ir stack) 
 *  pop(count=1): pops the <count> element(s) from the evaluation stack (ir stack), default is pop(1) 
 *  fetch(c_type): relative to Instruction Pointer (IP), 
 *                  treat the next sizeof(c_type) bytes as c_type, i.e. (c_type *)IP, 
 *                  then IP += sizeof(c_type) */
typedef enum jit_ir_op_type 
{
    /* for instruction argument size, see Ir_Op_GetArgSize() */
    IR_OP_LESS,             /* stack[1] = if (stack[1] < stack[0])  -> 1.0f else -> 0.0f;   pop(); */
    IR_OP_LESS_EQUAL,       /* stack[1] = if (stack[1] <= stack[0]) -> 1.0f else -> 0.0f;   pop(); */
    IR_OP_GREATER,          /* stack[1] = if (stack[1] > stack[0])  -> 1.0f else -> 0.0f;   pop(); */
    IR_OP_GREATER_EQUAL,    /* stack[1] = if (stack[1] >= stack[0]) -> 1.0f else -> 0.0f;   pop(); */
    IR_OP_ADD,              /* stack[1] = stack[1] + stack[0];                              pop(); */
    IR_OP_SUB,              /* stack[1] = stack[1] - stack[0];                              pop(); */
    IR_OP_MUL,              /* stack[1] = stack[1] * stack[0];                              pop(); */
    IR_OP_DIV,              /* stack[1] = stack[1] / stack[0];                              pop(); */
    IR_OP_NEG,              /* stack[0] = 0 - stack[0];                                     pop(); */
    IR_OP_LOAD,             /* push(Ir_GetData(fetch(i32))) */
    IR_OP_CALL_ARG_START,   /* implementation might use this to spill call argument registers */
    IR_OP_CALL,             /* FnNameStr = fetch(const char *)
                             * FnNameLen = fetch(i32)
                             * FnNameLine = fetch(i32)
                             * FnNameLineOffset = fetch(i32)
                             * FnArgCount = fetch(i32)
                             * 
                             * find function, if function is available, emit instructions to call it, then 
                             * pop(FnArgCount);
                             * push(CallResult);
                             */
    IR_OP_SWAP,             /* swap(stack[1], stack[0]) */
    IR_OP_STORE,            /* GlobalOffset = fetch(i32); 
                             * Value = pop();
                             * emit instruction to store Value to GlobalOffset
                             */
    IR_OP_RETURN,           /* Value = pop();
                             * emit instruction to return Value
                             * */
    IR_OP_FN_BLOCK,         /* jit_function *FnInfo = fetch(ptr)
                             * u16 BlockSize = fetch(u16)
                             * u16 NextBlock = fetch(u16)
                             * */
    IR_OP_VAR_BLOCK,        /* jit_variable *VarInfo = fetch(ptr)
                             * u16 BlockSize = fetch(u16)
                             * u16 NextBlock = fetch(u16) 
                             * */
} jit_ir_op_type;

static inline int Ir_Op_GetArgSize(jit_ir_op_type Op)
{
    switch (Op)
    {
    case IR_OP_LESS:           /* Instruction(1) */
    case IR_OP_GREATER:        /* Instruction(1) */
    case IR_OP_GREATER_EQUAL:  /* Instruction(1) */
    case IR_OP_LESS_EQUAL:     /* Instruction(1) */
    case IR_OP_ADD:            /* Instruction(1) */
    case IR_OP_SUB:            /* Instruction(1) */
    case IR_OP_MUL:            /* Instruction(1) */
    case IR_OP_DIV:            /* Instruction(1) */
    case IR_OP_NEG:            /* Instruction(1) */
    case IR_OP_CALL_ARG_START: /* Instruction(1) */
    case IR_OP_SWAP:           /* Instruction(1) */
    case IR_OP_RETURN:         /* Instruction(1) */ /* also marks the end of a function block */
    {
        return 0;
    } break;
    case IR_OP_STORE:          /* Instruction(1) + Offset(4) */ /* also marks the end of a variable block */
    case IR_OP_LOAD:           /* Instruction(1) + LoadIndex(4) */
    {
        return 4;
    } break;

    case IR_OP_VAR_BLOCK:      /* Instruction(1) + VarInfoPtr(sizeof(ptr)) + BlockSize(u16) + NextBlock(u16) */
    case IR_OP_FN_BLOCK:       /* Instruction(1) + FnInfoPtr(sizeof(ptr)) + BlockSize(u16) + NextBlock(u16) */
    {
        return sizeof(void *) + 2*sizeof(u16);
    } break;
    case IR_OP_CALL:           /* Instruction(1) 
                                  + FnNamePtr(8) 
                                  + FnNameLen(4) 
                                  + FnNameLine(4) 
                                  + FnNameOffset(4) 
                                  + ArgCount(4)  */
    {
        return sizeof(const char *) + 4*sizeof(i32);
    } break;
    }
    UNREACHABLE();
    return 0;
}
static inline const u8 *Ir_Op_GetNextBlock(const u8 *IP, u16 BlockOffset)
{
    if (0 == BlockOffset)
        return NULL;
    return IP + BlockOffset;
}
static inline const u8 *Ir_Op_GetBlockEnd(const u8 *IP, u16 BlockSize)
{
    return IP - (1 + Ir_Op_GetArgSize(IR_OP_FN_BLOCK)) + BlockSize;
}

jit_location Jit_IrDataAsLocation(jit *Jit, const u8 *Data, int LocalScopeBase, int LocalScopeVarCount);
jit_function *Jit_FindFunction(jit *Jit, const char *FnNameStr, i32 StrLen, i32 Line, i32 Offset, int ArgCount);
jit_function *Jit_DefineFunction(jit *Jit, const char *Name, int NameLen);





/*=======================================
 *              jit_token
 *======================================= */
typedef enum jit_token_type 
{
    TOK_ERR = 0,
    TOK_NUMBER,
    TOK_PLUS,
    TOK_MINUS,
    TOK_SLASH,
    TOK_STAR,

    TOK_CARET,
    TOK_PERCENT,
    TOK_LPAREN,
    TOK_RPAREN,
    TOK_LBRACKET,

    TOK_RBRACKET,
    TOK_EQUAL,
    TOK_IDENTIFIER,
    TOK_NEWLINE,
    TOK_COMMA, 

    TOK_LESS, 
    TOK_GREATER, 
    TOK_LESS_EQUAL, 
    TOK_GREATER_EQUAL,

    TOK_EOF,
} jit_token_type;


typedef struct jit_token 
{
    strview Str;
    int Offset, Line;
    enum jit_token_type Type;

    union {
        double Number;
        const char *ErrMsg;
    } As;
} jit_token;

#endif /* JIT_COMMON_H */

