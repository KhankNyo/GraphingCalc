#ifndef JIT_COMMON_H
#define JIT_COMMON_H

#include "Common.h"

#define JIT_REG_INVALID -1
typedef i8 jit_reg;

typedef struct jit jit;
typedef struct jit_storage_manager jit_storage_manager;
typedef struct jit_emitter jit_emitter;

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

typedef enum jit_storage_type 
{
    STORAGE_MEM = 1,
    STORAGE_REG,
} jit_storage_type;

typedef struct jit_mem 
{
    i32 Offset;
    jit_reg BaseReg;
} jit_mem;
typedef struct jit_location
{
    jit_storage_type Storage;
    union {
        jit_mem Mem;
        jit_reg Reg;
    } As;
} jit_location;

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



/* internal jit functions */
typedef struct jit_scratchpad 
{
    u8 *Ptr;
    int LeftCount, RightCount, Capacity;
} jit_scratchpad;




/*=======================================
 *              jir_ir_data
 *======================================= */

typedef struct jit_ir_data_param
{
    const char *Str;
    i32 StrLen;
    i32 Index;
} jit_ir_data_param;
typedef struct jit_token_skinny
{
    const char *Str;
    i32 StrLen;
    jit_token_type Type;
    i32 Line; 
    i32 Offset;
} jit_token_skinny;
typedef enum jit_ir_data_type 
{
    IR_DATA_CONST  = sizeof(double),                    /* Const(double)                   + Tag(1) */
    IR_DATA_VARREF = sizeof(jit_token_skinny),          /* Name(jit_token_skinny)          + Tag(1) */
    IR_DATA_PARAM  = sizeof(jit_ir_data_param),         /* Name(strview) + ParamIndex(i32) + Tag(1) */
} jit_ir_data_type;
typedef struct jit_ir_data 
{
    u8 *Ptr;
    int ByteCount;
} jit_ir_data;

int Ir_Data_GetSize(const jit_ir_data *Data);
u8 *Ir_Data_Get(jit_ir_data *Data, i32 Offset);
jit_location Ir_Data_GetLocation(jit *Jit, const u8 *Data, int LocalScopeBase, int LocalScopeVarCount);

static inline int Ir_Data_GetPayloadSize(jit_ir_data_type DataType)
{
    STATIC_ASSERT(IR_DATA_CONST <= 0xFF, "cannot fit data size into byte");
    STATIC_ASSERT(IR_DATA_VARREF <= 0xFF, "cannot fit data size into byte");
    STATIC_ASSERT(IR_DATA_PARAM <= 0xFF, "cannot fit data size into byte");
    switch (DataType)
    {
    case IR_DATA_CONST:     
    case IR_DATA_PARAM:     
    case IR_DATA_VARREF:    
        return DataType;
    }
    UNREACHABLE();
    return 0;
}

static inline jit_ir_data_type Ir_Data_GetType(jit_ir_data *Data, i32 Index)
{
    return *Ir_Data_Get(Data, Index);
}

static inline void Ir_Data_GetPayload(const u8 *Data, void *Payload)
{
    jit_ir_data_type PayloadSize = Ir_Data_GetPayloadSize(*Data);
    MemCpy(Payload, Data - PayloadSize, PayloadSize);
}

static inline void Ir_Data_SetPayload(u8 *Data, const void *Payload)
{
    jit_ir_data_type PayloadSize = Ir_Data_GetPayloadSize(*Data);
    MemCpy(Data - PayloadSize, Payload, PayloadSize);
}




/*=======================================
 *              jir_ir_stack
 *======================================= */
typedef struct jit_ir_stack 
{
    jit_scratchpad *S;
    int Count;
} jit_ir_stack;

jit_ir_stack Ir_Stack_Init(jit *Jit);
jit_location *Ir_Stack_Top(jit_ir_stack *Stack, int Offset);
jit_location *Ir_Stack_Push(jit_ir_stack *Stack, const jit_location *Value);
jit_location *Ir_Stack_Pop(jit_ir_stack *Stack);
void Ir_Stack_PopMultiple(jit_ir_stack *Stack, int Count);
void Ir_Stack_SpillReg(jit_emitter *Emitter, jit_ir_stack *Stack, jit_storage_manager *Storage);
void Jit_EmitCallArgs(jit_ir_stack *Stack, int ArgCount);


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

jit_fnref_stack Ir_FnRef_Init(jit_scratchpad *S);
int Ir_FnRef_Count(const jit_fnref_stack *FnRef);
jit_fnref *Ir_FnRef_Push(jit_fnref_stack *FnRef, const jit_function *Function, uint CallLocation);
jit_fnref *Ir_FnRef_Pop(jit_fnref_stack *FnRef);



#define FN_LOCATION_UNDEFINED -1
#define VAR_END_LAST 0
#define FN_END_LAST 0
typedef enum jit_ir_op_type 
{
    /* for instruction argument size, see Ir_Op_GetArgSize() */
    IR_OP_ADD,              
    IR_OP_SUB,              
    IR_OP_MUL,              
    IR_OP_DIV,              
    IR_OP_NEG,              
    IR_OP_LOAD,             
    IR_OP_CALL_ARG_START,   
    IR_OP_CALL,             
    IR_OP_SWAP,             
    IR_OP_STORE,            
    IR_OP_FN_BEGIN,         
    IR_OP_FN_END,           
    IR_OP_VAR_BEGIN,        
    IR_OP_VAR_END,          
    IR_OP_LESS, 
    IR_OP_LESS_EQUAL, 
    IR_OP_GREATER, 
    IR_OP_GREATER_EQUAL, 
} jit_ir_op_type;

int Ir_Op_GetArgSize(jit_ir_op_type Op);

jit_location Jit_AllocateReg(jit *Jit, jit_ir_stack *Stack);
jit_reg Jit_CopyToReg(jit *Jit, jit_reg Reg, const jit_location *Location);
jit_location Jit_IrDataAsLocation(jit *Jit, const u8 *Data, int LocalScopeBase, int LocalScopeVarCount);
jit_reg Jit_ToReg(jit *Jit, jit_ir_stack *Stack, const jit_location *Location);
jit_function *Jit_FindFunction(jit *Jit, const jit_token *FnName);
jit_function *Jit_DefineFunction(jit *Jit, const char *Name, int NameLen);


#endif /* JIT_COMMON_H */

