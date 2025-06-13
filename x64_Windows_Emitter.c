
#include <stdarg.h>
#include <stdio.h>
#include "TargetEnv.h"

#define RM(v) ((v) & 0x7)
#define REG(v) (((v) & 0x7) << 3)
#define MODRM(mod, reg, rm) \
    (((mod) & 0x3) << 6)\
    | REG(reg) | RM(rm)
#define SIB(scale, index, base) \
    (((scale) & 0x3) << 6)\
    | ((index) & 0x7) << 3\
    | ((base) & 0x7) 



typedef struct disasm_data
{
    const u8 *Memory;
    char *DisasmBuffer;

    int MemoryCapacity;
    int InstructionSize;
    uint DisasmBufferSize;
    uint DisasmBufferCap;

} disasm_data;

typedef enum disasm_modrm_type 
{
    MODRM_DST_SRC,
    MODRM_SRC_DST,
} disasm_modrm_type;

static const char *sIntReg[] = { 
    [RAX] = "rax", 
    [RCX] = "rcx",
    [RDX] = "rdx",
    [RBX] = "rbx", 
    [RSP] = "rsp",
    [RBP] = "rbp",
    [RSI] = "rsi", 
    [RDI] = "rdi",
};
static const char *sXmmReg[] = {
    "xmm0",
    "xmm1",
    "xmm2",
    "xmm3",
    "xmm4",
    "xmm5",
    "xmm6",
    "xmm7",
};
static const u8 sStoreSingle32[] = {
    0x66, 0x0F, 0x7E,
};
static const u8 sStoreSingle64[] = {
    0x66, 0xF, 0xD6,
};
static const u8 sLoadSingle32[] = {
    0x66, 0x0F, 0x6E,
};
static const u8 sLoadSingle64[] = {
    0xF3, 0x0F, 0x7E
};

static int Emit(jit_emitter *Emitter, int Count, ...)
{
    int InstructionOffset = Emitter->BufferSize;
    va_list Args;
    va_start(Args, Count);
    for (int i = 0; i < Count && (uint)Emitter->BufferSize < Emitter->BufferCapacity; i++)
    {
        Emitter->Buffer[Emitter->BufferSize++] = va_arg(Args, uint);
    }
    va_end(Args);
    return InstructionOffset;
}

static int EmitArray(jit_emitter *Emitter, const u8 Array[], int Count)
{
    int InstructionOffset = Emitter->BufferSize;
    for (int i = 0; i < Count && (uint)Emitter->BufferSize < Emitter->BufferCapacity; i++)
    {
        Emitter->Buffer[Emitter->BufferSize++] = Array[i];
    }
    return InstructionOffset;
}

static int ModFromDisplacement(i32 Displacement)
{
    if (0 == Displacement)
        return 0;
    if (IN_RANGE(INT8_MIN, Displacement, INT8_MAX))
        return 1;
    return 2;
}

/* does not support rip-relative offset and index-scale mode */
static void Emit_GenericModRm(jit_emitter *Emitter, int Reg, int Rm, i32 Displacement)
{
    int Mod = ModFromDisplacement(Displacement);
    if (RBP == Rm && 0 == Displacement)
        Mod = 1;
    u8 ModRm = MODRM(Mod, Reg, Rm);

    Emit(Emitter, 1, ModRm);
    if (RSP == Rm)
    {
        Emit(Emitter, 1, SIB(0, RSP, RSP));
    }
    switch (Mod)
    {
    case 0:
    case 3:
        break; /* no displacement to emit */
    case 1: Emit(Emitter, 1, Displacement); break;
    case 2: EmitArray(Emitter, (u8 *)&Displacement, sizeof Displacement); break;
    }
}

static int Emit_FloatOpcode(jit_emitter *Emitter, u8 Opcode, int DstReg, int SrcBase, i32 SrcOffset)
{
    int InstructionOffset = Emit(Emitter, 3, Emitter->FloatOpcode, 0x0F, Opcode);
    Emit_GenericModRm(Emitter, DstReg, SrcBase, SrcOffset);
    return InstructionOffset;
}

static void Emit_FloatOpcodeReg(jit_emitter *Emitter, u8 Opcode, int DstReg, int SrcReg)
{
    u8 ModRm = MODRM(3, DstReg, SrcReg);
    Emit(Emitter, 4, Emitter->FloatOpcode, 0x0F, Opcode, ModRm);
}





void Emitter_Init(jit_emitter *Emitter, u8 *Buffer, int BufferCapacity)
{
    ASSERT(NULL != Emitter, "nullptr");
    *Emitter = (jit_emitter) {
        .Buffer = Buffer, 
        .BufferCapacity = BufferCapacity,
    };
}

void Emitter_Reset(jit_emitter *Emitter, bool8 EmitFloat32Instructions)
{
    STATIC_ASSERT(sizeof sStoreSingle32 == sizeof Emitter->StoreSingle, "unreachable");
    STATIC_ASSERT(sizeof sStoreSingle64 == sizeof Emitter->StoreSingle, "unreachable");
    STATIC_ASSERT(sizeof sLoadSingle32 == sizeof Emitter->LoadSingle, "unreachable");
    STATIC_ASSERT(sizeof sLoadSingle64 == sizeof Emitter->LoadSingle, "unreachable");

    Emitter->BufferSize = 0;
    if (EmitFloat32Instructions)
    {
        Emitter->FloatOpcode = 0xF3;
        MemCpy(Emitter->StoreSingle, sStoreSingle32, sizeof sStoreSingle32);
        MemCpy(Emitter->LoadSingle, sLoadSingle32, sizeof sLoadSingle32);
    }
    else
    {
        Emitter->FloatOpcode = 0xF2;
        MemCpy(Emitter->StoreSingle, sStoreSingle64, sizeof sStoreSingle64);
        MemCpy(Emitter->LoadSingle, sLoadSingle64, sizeof sLoadSingle64);
    }
}



void Emit_Move(jit_emitter *Emitter, jit_reg DstReg, jit_reg SrcReg)
{
    if (DstReg != SrcReg)
    {
        /* movaps dst, src */
        Emit(Emitter, 3, 0x0F, 0x28, MODRM(3, DstReg, SrcReg));
    }
}

void Emit_Store(jit_emitter *Emitter, jit_reg SrcReg, jit_reg DstBase, i32 SrcOffset)
{
    /* movq [base + offset], src */
    EmitArray(Emitter, 
        Emitter->StoreSingle,
        sizeof Emitter->StoreSingle
    );
    Emit_GenericModRm(Emitter, SrcReg, DstBase, SrcOffset);
}

void Emit_Load(jit_emitter *Emitter, jit_reg DstReg, jit_reg SrcBase, i32 SrcOffset)
{
    /* emit it as a store instruction initially */
    uint PrevSize = EmitArray(Emitter, 
        Emitter->StoreSingle, 
        sizeof Emitter->StoreSingle
    );
    Emit_GenericModRm(Emitter, DstReg, SrcBase, SrcOffset);
    uint InstructionLength = Emitter->BufferSize - PrevSize;

    /* compared the emitted instruction with the last*/
    u8 *PrevIns = Emitter->Buffer + PrevSize - InstructionLength;
    u8 *CurrIns = Emitter->Buffer + PrevSize;
    if (Emitter->BufferSize >= 2*InstructionLength
    && MemEqu(PrevIns, CurrIns, InstructionLength))
    {
        /* if matched, 
         * this current instruction was trying to read the value we just stored into the same regsiter, 
         * optimize it out */
        Emitter->BufferSize -= InstructionLength;
    }
    else
    {
        /* otherwise, change the emitted instruction into a load */
        for (uint i = 0; i < sizeof Emitter->LoadSingle; i++)
            CurrIns[i] = Emitter->LoadSingle[i];
    }
}


void Emit_Add(jit_emitter *Emitter, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x58, Dst, Base, Offset);
}

void Emit_Sub(jit_emitter *Emitter, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x5C, Dst, Base, Offset);
}

void Emit_Mul(jit_emitter *Emitter, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x59, Dst, Base, Offset);
}

void Emit_Div(jit_emitter *Emitter, jit_reg Dst, jit_reg Base, i32 Offset)
{
    Emit_FloatOpcode(Emitter, 0x5E, Dst, Base, Offset);
}

void Emit_AddReg(jit_emitter *Emitter, jit_reg Dst, jit_reg Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x58, Dst, Src);
}

void Emit_SubReg(jit_emitter *Emitter, jit_reg Dst, jit_reg Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x5C, Dst, Src);
}

void Emit_MulReg(jit_emitter *Emitter, jit_reg Dst, jit_reg Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x59, Dst, Src);
}

void Emit_DivReg(jit_emitter *Emitter, jit_reg Dst, jit_reg Src)
{
    Emit_FloatOpcodeReg(Emitter, 0x5E, Dst, Src);
}

void Emit_LoadZero(jit_emitter *Emitter, jit_reg DstReg)
{
    /* xorps dst, dst */
    Emit(Emitter, 3, 0x0F, 0x57, MODRM(3, DstReg, DstReg));
}




uint Emit_FunctionEntry(jit_emitter *Emitter)
{
    /* push rbp 
     * mov rbp, rsp
     * sub rsp, memsize
     * */
    while (Emitter->BufferSize % 0x10 != 0)
    {
        Emit(Emitter, 1, 0x90); /* nop */
    }
    uint Location = Emit(Emitter, 1 + 3, 
        0x50 + RBP,                     /* push rbp */
        0x48, 0x89, MODRM(3, RSP, RBP)  /* mov rbp, rsp */
    );
    Emit(Emitter, 3 + 4, 
        0x48, 0x81, MODRM(3, 5, RSP),   /* sub rsp, i32 */
        0, 0, 0, 0
    );
    return Location;
}


void Emit_FunctionExit(jit_emitter *Emitter, uint Location, i32 StackSize)
{
    uint PrologueSize = 1 + 3 + 3 + 4;
    ASSERT(Location + PrologueSize <= Emitter->BufferSize, "invalid location");

    /* leave 
     * ret 
     */
    Emit(Emitter, 2, 0xC9, 0xC3);

    /* patch stack size */
    uint StackSizeLocation = Location + 1 + 3 + 3;
    MemCpy(
        Emitter->Buffer + StackSizeLocation,
        &StackSize, 
        sizeof StackSize
    );
}


uint Emit_Call(jit_emitter *Emitter, uint FunctionLocation)
{
    /* call rel32 */
    i32 Rel32 = FunctionLocation - (Emitter->BufferSize + 5);
    uint Location = Emit(Emitter, 1, 0xE8);
    EmitArray(Emitter, (u8 *)&Rel32, sizeof Rel32);
    return Location;
}

void Emitter_PatchCall(jit_emitter *Emitter, uint CallLocation, uint FunctionLocation)
{
    ASSERT(FunctionLocation < Emitter->BufferSize, "bad dst location");
    ASSERT(CallLocation + 5 <  Emitter->BufferSize, "bad src location");
    i32 Offset = FunctionLocation - (CallLocation + 5);
    MemCpy(Emitter->Buffer + CallLocation + 1, &Offset, 4);
}


#include "Jit.h"

static jit_location Jit_AllocateStack(jit_storage_manager *Storage)
{
    return (jit_location) {
        .Storage = STORAGE_MEM,
        .As.Mem = Storage_AllocateStack(Storage),
    };
}


void Ir_Stack_SpillReg(jit_emitter *Emitter, jit_ir_stack *Stack, jit_storage_manager *Storage)
{
    for (int i = 0; i < Stack->Count; i++)
    {
        jit_location *Elem = Ir_Stack_Top(Stack, i);
        switch (Elem->Storage)
        {
        case STORAGE_MEM: break; /* already in memory, nothing to do */
        case STORAGE_REG:
        {
            jit_reg Src = Elem->As.Reg;
            jit_location Location = Jit_AllocateStack(Storage);
            Emit_Store(Emitter, Src, Location.As.Mem.BaseReg, Location.As.Mem.Offset);
            Storage_DeallocateReg(Storage, Src);

            *Elem = Location;
        } break;
        }
    }
}



jit_location Jit_AllocateReg(jit_emitter *Emitter, jit_ir_stack *Stack, jit_storage_manager *Storage)
{
    jit_location Location = {
        .Storage = STORAGE_REG,
        .As.Reg = Storage_TryAllocateReg(Storage),
    };
    if (JIT_REG_INVALID == Location.As.Reg)
    {
        /* spill stack */
        Ir_Stack_SpillReg(Emitter, Stack, Storage);
        Location.As.Reg = Storage_TryAllocateReg(Storage);
        ASSERT(Location.As.Reg != JIT_REG_INVALID, "unreachable");
    }
    return Location;
}


jit_reg Jit_ToReg(jit_emitter *Emitter, jit_ir_stack *Stack, jit_storage_manager *Storage, const jit_location *Location)
{
    jit_reg Result = -1;
    switch (Location->Storage)
    {
    case STORAGE_REG:
    {
        Result = Location->As.Reg;
    } break;
    case STORAGE_MEM:
    {
        Result = Jit_AllocateReg(Emitter, Stack, Storage).As.Reg;
        Emit_Load(Emitter, Result, Location->As.Mem.BaseReg, Location->As.Mem.Offset);
    } break;
    }
    return Result;
}

jit_reg Jit_CopyToReg(jit_emitter *Emitter, jit_reg Reg, const jit_location *Location)
{
    switch (Location->Storage)
    {
    case STORAGE_MEM:
    {
        Emit_Load(Emitter, Reg, Location->As.Mem.BaseReg, Location->As.Mem.Offset);
    } break;
    case STORAGE_REG:
    {
        Emit_Move(Emitter, Reg, Location->As.Reg);
    } break;
    }
    return Reg;
}

void Jit_EmitCallArgs(jit_ir_stack *Stack, int ArgCount)
{
    int IrStackCount = Stack->Count;
    ASSERT(ArgCount <= IrStackCount, "arg count");

    /* reserve stack space for arguments */
    Storage_PushStack(&Jit->Storage, ArgCount);

    /* emit arguments */
    for (int i = 0; i < ArgCount; i++)
    {
        jit_location *Location = Ir_Stack_Top(Stack, ArgCount - i - 1);
        jit_location Arg = TargetEnv_GetArg(i, Jit->Storage.DataSize);
        switch (Arg.Storage)
        {
        case STORAGE_REG:
        {
            Jit_CopyToReg(Jit, Arg.As.Reg, Location);
            Storage_ForceAllocateReg(&Jit->Storage, Arg.As.Reg);
        } break;
        case STORAGE_MEM:
        {
            jit_reg Tmp = Jit_ToReg(Jit, Stack, Location);
            Emit_Store(&Jit->Emitter, Tmp, Arg.As.Mem.BaseReg, Arg.As.Mem.Offset);
            Storage_DeallocateReg(&Jit->Storage, Tmp);
        } break;
        }
    }
    /* done emitting actual instructions, pop ir stack */
    Ir_Stack_PopMultiple(Stack, ArgCount);

    /* deallocate argument registers */
    for (int i = 0; i < ArgCount && TargetEnv_IsArgumentInReg(i); i++)
    {
        jit_reg Reg = TargetEnv_GetArg(i, Jit->Storage.DataSize).As.Reg;
        Storage_DeallocateReg(&Jit->Storage, Reg);
    }

    /* deallocate stack space used for arguments, 
     * TODO: this is for caller cleanup, how about callee cleanup? */
    Storage_PopStack(&Jit->Storage, ArgCount);
}







#define IR_CONSUME_BYTE() *IP++
#define IR_CONSUME_AND_INTERPRET(datatype) ((IP += sizeof(datatype)), (datatype *)(IP - sizeof(datatype)))
#define IR_CONSUME_I32() *IR_CONSUME_AND_INTERPRET(i32)

static u8 *Emitter_TranslateSingleUnit(
#if 0
    jit *Jit, 
    u8 *IP, 
    jit_ir_stack *Stack, jit_fnref_stack *FnRef, 
    jit_ir_op_type BeginOp, jit_ir_op_type EndOp
#else
    jit_emitter *Emitter, 
    jit_ir_data *Data, jit_storage_manager *Storage,
    //jit *Jit,
    jit_ir_stack *Stack, jit_fnref_stack *FnRef, 
    u8 *IP, const u8 *End,
    jit_ir_op_type BeginOp, jit_ir_op_type EndOp
#endif
)
{
    while (IP < End && *IP != BeginOp)
    {
        jit_ir_op_type Op = IR_CONSUME_BYTE();
        IP += Ir_Op_GetArgSize(Op);
    }

    jit_function *Fn = NULL;
    jit_variable *Var = NULL;
    i32 EndOffset = 0;
    jit_ir_op_type Op = -1;
    while (IP < End && Op != EndOp)
    {
        Op = IR_CONSUME_BYTE();

        switch (Op)
        {
        case IR_OP_VAR_BEGIN:
        {
            Var = *IR_CONSUME_AND_INTERPRET(jit_variable *);
            Var->InsLocation = Emitter->BufferSize;
        } break;
        case IR_OP_VAR_END:
        {
            EndOffset = IR_CONSUME_I32();
            Var->InsByteCount = Emitter->BufferSize - Var->InsLocation;

            Var = NULL;
        } break;
        case IR_OP_FN_BEGIN:
        {
            Fn = *IR_CONSUME_AND_INTERPRET(jit_function *);
            ASSERT(Fn, "nullptr");

            Fn->Location = Emit_FunctionEntry(Emitter);
            ASSERT(Ir_Data_GetSize(Data) >= Fn->ParamStart + Fn->ParamCount, "unreachable");

            /* set parameters to valid location */
            for (int i = 0; i < Fn->ParamCount && TargetEnv_IsArgumentInReg(i); i++)
            {
                jit_mem NonVolatileParam = TargetEnv_GetParam(i, Storage->DataSize);
                jit_reg VolatileParam = TargetEnv_GetArg(i, Storage->DataSize).As.Reg;
                Emit_Store(Emitter, 
                    VolatileParam,
                    NonVolatileParam.BaseReg,
                    NonVolatileParam.Offset
                );
            }
        } break;
        case IR_OP_FN_END:
        {
            /* offset to next fn */
            EndOffset = IR_CONSUME_I32();

            /* pop the top of the stack and emit return ins */
            jit_location *ReturnValue = Ir_Stack_Pop(Stack);
            Jit_CopyToReg(Emitter, TargetEnv_GetReturnReg(), ReturnValue);
            /* deallocate return register */
            if (STORAGE_REG == ReturnValue->Storage)
            {
                Storage_DeallocateReg(Storage, ReturnValue->As.Reg);
            }

            /* emit return */
            Emit_FunctionExit(Emitter, Fn->Location, Storage->MaxStackSize);
            Fn->InsByteCount = Emitter->BufferSize - Fn->Location;

            Fn = NULL;
        } break;
        case IR_OP_SWAP:
        {
            ASSERT(Stack->Count >= 2, "size");
            /* swap */
            jit_location *Left = Ir_Stack_Top(Stack, 0);
            jit_location *Right = Ir_Stack_Top(Stack, 1);
            SWAP(jit_location, *Left, *Right);
        } break;
        case IR_OP_LOAD:
        {
            i32 LoadIndex = IR_CONSUME_I32();

            const u8 *IrData = Ir_Data_Get(Data, LoadIndex);
            int ParamStart = 0;
            int ParamCount = 0;
            if (Fn)
            {
                ParamStart = Fn->ParamStart;
                ParamCount = Fn->ParamCount;
            }
            jit_location Location = Ir_Data_GetLocation(Emitter->Jit, IrData, ParamStart, ParamCount);
            Ir_Stack_Push(Stack, &Location);
        } break;
        case IR_OP_STORE: /* store expr on stack to dst */
        {
            i32 Offset = IR_CONSUME_I32();

            jit_reg Src = Jit_ToReg(Emitter, Stack, Ir_Stack_Pop(Stack));
            jit_mem Dst = {
                .BaseReg = TargetEnv_GetGlobalPtrReg(),
                .Offset = Offset,
            };
            Emit_Store(Emitter, Src, Dst.BaseReg, Dst.Offset);
            Storage_DeallocateReg(Storage, Src);
        } break;
        case IR_OP_CALL_ARG_START:
        {
            /* make all previous location on the stack a memory location */
            Ir_Stack_SpillReg(Emitter, Stack, Storage);
        } break;
        case IR_OP_CALL:
        {
            /* get args */
            i32 ArgCount = IR_CONSUME_I32();
            const jit_token *FnName = IR_CONSUME_AND_INTERPRET(jit_token);

            const jit_function *Function = Jit_FindFunction(Jit, FnName);
            if (!Function)
            {
                Error_AtToken(&Jit->Error, FnName, "Undefined function.");
                break;
            }

            /* check param and arg count */
            if (Function->ParamCount != ArgCount)
            {
                const char *Plural = Function->ParamCount > 1? 
                    "arguments" : "argument";
                Error_AtToken(&Jit->Error, FnName, "Expected %d %s, got %d instead.", 
                    Function->ParamCount, Plural, ArgCount
                );
                break;
            }

            /* emit the args and call itself */
            Jit_EmitCallArgs(Stack, Function->ParamCount);
            uint CallLocation = Emit_Call(Emitter, Function->Location);
            if (FN_LOCATION_UNDEFINED == Function->Location)
            {
                Ir_FnRef_Push(FnRef, Function, CallLocation);
            }

            /* return */
            Storage_ForceAllocateReg(Storage, TargetEnv_GetReturnReg());
            jit_location Result = {
                .Storage = STORAGE_REG, 
                .As.Reg = TargetEnv_GetReturnReg(),
            };

            /* push return value on the stack */
            Ir_Stack_Push(Stack, &Result);
        } break;
        case IR_OP_ADD:
        case IR_OP_SUB:
        case IR_OP_MUL:
        case IR_OP_DIV:
        case IR_OP_LESS:            
        case IR_OP_GREATER:         
        case IR_OP_GREATER_EQUAL:   
        case IR_OP_LESS_EQUAL:      
        {
            jit_location *Right = Ir_Stack_Pop(Stack);
            jit_location *Left = Ir_Stack_Pop(Stack);

            /* if commutative and right was in reg first, swap operands */
            if ((IR_OP_ADD == Op || IR_OP_MUL == Op)
            && STORAGE_REG == Right->Storage)
            {
                SWAP(jit_location *, Left, Right);
            }

            jit_location Result = {
                .Storage = STORAGE_REG,
                .As.Reg = Jit_ToReg(Jit, Stack, Left),
            };
            if (Right->Storage == STORAGE_REG)
            {
                Storage_DeallocateReg(Storage, Right->As.Reg);
            }

#define OP(op_name, left_reg, right_expr) do {\
    if (STORAGE_MEM == (right_expr)->Storage)\
        Emit_ ## op_name (Emitter, left_reg, (right_expr)->As.Mem.BaseReg, (right_expr)->As.Mem.Offset);\
    else Emit_ ## op_name ## Reg(Emitter, left_reg, (right_expr)->As.Reg);\
} while (0)
            switch (Op)
            {
            case IR_OP_ADD: OP(Add, Result.As.Reg, Right); break;
            case IR_OP_SUB: OP(Sub, Result.As.Reg, Right); break;
            case IR_OP_MUL: OP(Mul, Result.As.Reg, Right); break;
            case IR_OP_DIV: OP(Div, Result.As.Reg, Right); break;
            default: 
            {
                UNREACHABLE();
            } break;
            }

            Ir_Stack_Push(Stack, &Result);
        } break;
        case IR_OP_NEG:
        {
            /* stack.top = 0 - stack.top */
            jit_location *Value = Ir_Stack_Pop(Stack);
            jit_location Result = Jit_AllocateReg(Jit, Stack);
            Emit_LoadZero(Emitter, Result.As.Reg);
            OP(Sub, Result.As.Reg, Value);
            Ir_Stack_Push(Stack, &Result);

            if (Value->Storage == STORAGE_REG)
            {
                Storage_DeallocateReg(Storage, Value->As.Reg);
            }
        } break;
        }
#undef OP
    }

    return IP + EndOffset;
}


void Emitter_TranslateIr(jit *Jit)
{
    jit_ir_stack Stack_ = Ir_Stack_Init(Jit);
    jit_ir_stack *Stack = &Stack_;
    jit_fnref_stack FnRef_ = Ir_FnRef_Init(Jit);
    jit_fnref_stack *FnRef = &FnRef_;

    int FunctionCallCount;
    u8 *IP = Jit->IrOp;
    const u8 *InsEnd = IP + Jit->IrOpByteCount;
    while (IP < InsEnd)
    {
        IP = Emitter_TranslateSingleUnit(
                &Jit->Emitter,
            Jit, 
            Stack, FnRef, 
            IP, InsEnd,
            IR_OP_FN_BEGIN, IR_OP_FN_END
        );
    }
    /* patch function calls */
    FunctionCallCount = Ir_FnRef_Count(FnRef);
    for (int i = 0; i < FunctionCallCount; i++)
    {
        jit_fnref *Ref = Ir_FnRef_Pop(FnRef);
        ASSERT(Ref->Function, "nullptr");
        Emitter_PatchCall(&Jit->Emitter, Ref->Location, Ref->Function->Location);
    }

    jit_function *Init = Jit_DefineFunction(Jit, "init", 4);
    Storage_SetMaxStackCount(&Jit->Storage, 0);
    Init->Location = Emit_FunctionEntry(&Jit->Emitter);
    IP = Jit->IrOp;
    while (IP < InsEnd)
    {
        IP = Emitter_TranslateSingleUnit(
                &Jit->Emitter,
            Jit, 
            Stack, FnRef, 
            IP, InsEnd,
            IR_OP_VAR_BEGIN, IR_OP_VAR_END
        );
    }
    Emit_FunctionExit(&Jit->Emitter, Init->Location, Jit->Storage.MaxStackSize);
    Init->InsByteCount = Jit->Emitter.BufferSize - Init->Location;
}


int Emitter_GetBufferSize(const jit_emitter *Emitter)
{
    return Emitter->BufferSize;
}

const u8 *Emitter_GetBuffer(const jit_emitter *Emitter)
{
    return Emitter->Buffer;
}




/* ======================================== */
/*              Disassembler                */
/* ======================================== */


#define PEEK(p_dd, offset) ((p_dd)->InstructionSize + (offset) < (p_dd)->MemoryCapacity\
    ? (p_dd)->Memory[(p_dd)->InstructionSize + (offset)]\
    : 0)

static u8 ConsumeByte(disasm_data *Data)
{
    u8 Byte = PEEK(Data, 0);
    Data->InstructionSize++;
    return Byte;
}

static u32 ConsumeDWord(disasm_data *Data)
{
    u32 DWord = ConsumeByte(Data);
    DWord |= (u32)ConsumeByte(Data) << 8;
    DWord |= (u32)ConsumeByte(Data) << 16;
    DWord |= (u32)ConsumeByte(Data) << 24;
    return DWord;
}

static void WriteInstruction(disasm_data *Data, const char *Fmt, ...)
{
    va_list Args;
    va_start(Args, Fmt);
    int PotentialBytesWritten = vsnprintf(
        Data->DisasmBuffer + Data->DisasmBufferSize, 
        Data->DisasmBufferCap - Data->DisasmBufferSize,
        Fmt, Args
    );
    va_end(Args);

    if (PotentialBytesWritten + Data->DisasmBufferSize > Data->DisasmBufferCap)
    {
        Data->DisasmBufferSize = Data->DisasmBufferCap;
    }
    else
    {
        Data->DisasmBufferSize += PotentialBytesWritten;
    }
}

static i32 X64ModOffset(disasm_data *Data, uint Mod)
{
    if (0 == Mod)
        return 0;
    if (1 == Mod)
    {
        u32 Value = ConsumeByte(Data);
        return Value | ~((Value & 0x80) - 1);
    }
    return ConsumeDWord(Data);
}

static void X64DisasmModRM(disasm_data *Data, disasm_modrm_type Type, const char **Regs)
{
#define WRITE_OPERANDS(dststr, dstreg, srcstr, ...) do {\
    if (Type == MODRM_DST_SRC) {\
        WriteInstruction(Data, \
                dststr", "srcstr, \
                dstreg, __VA_ARGS__);\
    } else {\
        WriteInstruction(Data, \
                srcstr", "dststr, \
                __VA_ARGS__, dstreg);\
    }\
} while (0)
#define SIGNED_NUMBER(n) ((n) < 0? '-' : '+'), ABS(n)

    u8 ModRM = ConsumeByte(Data);
    uint Mod = ModRM >> 6;
    reg_index Reg = (ModRM >> 3) & 0x7;
    reg_index Rm = (ModRM) & 0x7;

    if (0x3 == Mod) /* reg mode */
    {
        WRITE_OPERANDS("%s", Regs[Reg], "%s", Regs[Rm]);
    }
    else if (0x4 == Rm) /* SIB byte present */
    {
        u8 SIB = ConsumeByte(Data);
        uint Scale = 1 << (SIB >> 6);
        reg_index Index = (SIB >> 3) & 0x7;
        reg_index Base = (SIB & 0x7);
        i32 Offset = X64ModOffset(Data, Mod);

        if (RSP == Index) /* no index reg */
        {
            if (0 == Mod) /* no displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s]", sIntReg[Base]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s %c 0x%x]", sIntReg[Base], SIGNED_NUMBER(Offset));
            }
        }
        else /* SIB with index */
        {
            if (0 == Mod) /* no displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %x*%s]", sIntReg[Base], Scale, sIntReg[Index]);
            }
            else /* 8/32 bit displacement */
            {
                WRITE_OPERANDS("%s", Regs[Reg], "[%s + %x*%s %c 0x%x]", sIntReg[Base], Scale, sIntReg[Index], SIGNED_NUMBER(Offset));
            }
        }
    }
    else if (0 == Mod && 0x5 == Rm) /* displacement only (rip-relative in 64 bit mode) */
    {
        i32 DWord = ConsumeDWord(Data);
        WRITE_OPERANDS("%s", Regs[Reg], "[rip %c 0x%x]", SIGNED_NUMBER(DWord));
    }
    else /* [register]/[register + displacement] */
    {
        if (0 == Mod) /* no displacement */
        {
            WRITE_OPERANDS("%s", Regs[Reg], "[%s]", sIntReg[Rm]);
        }
        else /* 8/32 bit displacement */
        {
            i32 Offset = X64ModOffset(Data, Mod);
            WRITE_OPERANDS("%s", sXmmReg[Reg], "[%s %c 0x%x]", sIntReg[Rm], SIGNED_NUMBER(Offset));
        }
    }
#undef WRITE_OPERANDS
#undef SIGNED_NUMBER
}


int DisasmSingleInstruction(u64 Addr, const u8 *Memory, int MemorySize, char ResultBuffer[64])
{
    ASSERT(MemorySize > 0, "bad memory size");
    disasm_data Disasm = {
        .Memory = Memory,
        .MemoryCapacity = MemorySize,
        .DisasmBuffer = ResultBuffer,
        .DisasmBufferCap = 64,
    };

    u8 FirstByte = ConsumeByte(&Disasm);
    bool8 Unknown = false;
    switch (FirstByte)
    {
    case 0x50 + RBP: WriteInstruction(&Disasm, "push rbp"); break;
    case 0xC3: WriteInstruction(&Disasm, "ret"); break;
    case 0xC9: WriteInstruction(&Disasm, "leave"); break;
    case 0x90: WriteInstruction(&Disasm, "nop"); break;
    case 0xE8:
    {
        i32 Rel32 = ConsumeDWord(&Disasm);
        u64 Dst = Addr + 5 + (i64)Rel32;
        WriteInstruction(&Disasm, "call %016x", Dst);
    } break;
    case 0xE9:
    {
        i32 Rel32 = ConsumeDWord(&Disasm);
        u64 Dst = Addr + 5 + (i64)Rel32;
        WriteInstruction(&Disasm, "jmp %016x", Dst);
    } break;
    case 0x48:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x89 == Second) /* mov rm, r */
        {
            WriteInstruction(&Disasm, "mov ");
            X64DisasmModRM(&Disasm, MODRM_SRC_DST, sIntReg);
        }
        else if (0x81 == Second) /* sub rm, imm32 */
        {
            u8 ModRm = ConsumeByte(&Disasm);
            uint Mod = ModRm >> 6;
            uint Reg = (ModRm >> 3) & 0x7;
            uint Rm = ModRm & 0x7;
            if (0x3 == Mod && 5 == Reg) /* sub r, imm */
            {
                WriteInstruction(&Disasm, "sub ");
                i32 Immediate = ConsumeDWord(&Disasm);
                WriteInstruction(&Disasm, "%s, 0x%x", sIntReg[Rm], Immediate);
            }
            else Unknown = true;
        }
        else if (0x83 == Second) 
        {
            u8 ModRm = ConsumeByte(&Disasm);
            uint Mod = ModRm >> 6;
            uint Reg = (ModRm >> 3) & 0x7;
            uint Rm = ModRm & 0x7;
            if (0x3 == Mod && 5 == Reg) /* sub r, imm */
            {
                WriteInstruction(&Disasm, "sub ");
                i32 Immediate = (i32)(i8)ConsumeByte(&Disasm);
                WriteInstruction(&Disasm, "%s, 0x%x", sIntReg[Rm], Immediate);
            }
            else Unknown = true;
        }
        else Unknown = true;
    } break;
    case 0x0F:
    {
        u8 Second = ConsumeByte(&Disasm);
        switch (Second)
        {
        case 0x57:
        {
            WriteInstruction(&Disasm, "xorps ");
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        } break;
        case 0x28:
        {
            WriteInstruction(&Disasm, "movaps ");
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        } break;
        default:
        {
            Unknown = true;
        } break;
        }
    } break;
    case 0xF2:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            switch (ConsumeByte(&Disasm))
            {
            case 0x58: WriteInstruction(&Disasm, "addsd "); break;
            case 0x5C: WriteInstruction(&Disasm, "subsd "); break;
            case 0x59: WriteInstruction(&Disasm, "mulsd "); break;
            case 0x5E: WriteInstruction(&Disasm, "divsd "); break;
            case 0x51: WriteInstruction(&Disasm, "sqrtsd "); break;
            default:  
            {
                Unknown = true;
            } goto Out;
            }

            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        }
        else Unknown = true;
    } break;
    case 0xF3:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            u8 Third = ConsumeByte(&Disasm);
            switch (Third)
            {
            case 0x58: WriteInstruction(&Disasm, "addss "); break;
            case 0x5C: WriteInstruction(&Disasm, "subss "); break;
            case 0x59: WriteInstruction(&Disasm, "mulss "); break;
            case 0x5E: WriteInstruction(&Disasm, "divss "); break;
            case 0x51: WriteInstruction(&Disasm, "sqrtss "); break;
            case 0x7E: WriteInstruction(&Disasm, "movq "); break;
            default: 
            {
                Unknown = true;
            } goto Out;
            }
            X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
        }
        else Unknown = true;
    } break;
    case 0x66:
    {
        u8 Second = ConsumeByte(&Disasm);
        if (0x0F == Second)
        {
            u8 Third = ConsumeByte(&Disasm);
            switch (Third)
            {
            case 0xD6: 
            {
                WriteInstruction(&Disasm, "movq ");
                X64DisasmModRM(&Disasm, MODRM_SRC_DST, sXmmReg);
            } break;
            case 0x6E:
            {
                WriteInstruction(&Disasm, "movd ");
                X64DisasmModRM(&Disasm, MODRM_DST_SRC, sXmmReg);
            } break;
            case 0x7E:
            {
                WriteInstruction(&Disasm, "movd ");
                X64DisasmModRM(&Disasm, MODRM_SRC_DST, sXmmReg);
            } break;
            default: 
            {
                Unknown = true;
            } goto Out;
            }
        }
        else Unknown = true;
    } break;
    }

Out: 
    if (Unknown)
    {
        WriteInstruction(&Disasm, "???");
        Disasm.InstructionSize++;
    }
    return Disasm.InstructionSize;
}

#undef PEEK
#undef PROLOGUE_SIZE

