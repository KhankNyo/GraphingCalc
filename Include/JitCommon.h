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
 *  u8 DataSize
 * IR_DATA_PARAM:
 *  u8 Tag
 *  i32 StrBegin
 *  i32 StrLen
 *  i32 ParamIndex
 * */
typedef enum jit_ir_data_type 
{
    IR_DATA_CONST = sizeof(double),
    IR_DATA_PARAM = 3*sizeof(i32) + 1*sizeof(u8),
    IR_DATA_VARREF = 4*sizeof(i32),
} jit_ir_data_type;
typedef struct jit_ir_data 
{
    jit_ir_data_type Type;
    union {
        struct {
            const char *Str;
            i32 StrLen;
            jit_location Location;
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

jit_location Ir_Data_GetLocation(jit *Jit, const jit_ir_data *Data, int LocalScopeBase, int LocalScopeVarCount);
jit_ir_data Ir_GetData(jit_ir_data_manager *Data, i32 Offset);



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
    IR_OP_LESS,             
    IR_OP_LESS_EQUAL, 
    IR_OP_GREATER, 
    IR_OP_GREATER_EQUAL, 
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
} jit_ir_op_type;

int Ir_Op_GetArgSize(jit_ir_op_type Op);

jit_location Jit_IrDataAsLocation(jit *Jit, const u8 *Data, int LocalScopeBase, int LocalScopeVarCount);
jit_function *Jit_FindFunction(jit *Jit, const char *FnNameStr, i32 StrLen, i32 Line, i32 Offset, int ArgCount);
jit_function *Jit_DefineFunction(jit *Jit, const char *Name, int NameLen);


#endif /* JIT_COMMON_H */

