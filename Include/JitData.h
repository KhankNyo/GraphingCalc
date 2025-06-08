#ifndef JIT_DATA_H
#define JIT_DATA_H

#include "Common.h"

typedef enum jit_data_storage_type 
{
    STORAGE_TYPE_MEM = 0,
    STORAGE_TYPE_REG,
    STORAGE_TYPE_CONST,
} jit_data_storage_type;

typedef struct jit_data
{
    jit_data_storage_type StorageType;
    union {
        double Const;
        struct {
            i32 Offset;
            uint BaseReg;
        } Mem;
        int Reg;
    } As;
} jit_data;

static inline jit_data Data_Const(double Value) 
{
    return (jit_data) {
        .StorageType = STORAGE_TYPE_CONST,
        .As.Const = Value,
    };
}

static inline jit_data Data_Mem(uint Reg, i32 Offset)
{
    return (jit_data) {
        .StorageType = STORAGE_TYPE_MEM,
        .As.Mem = {
            .BaseReg = Reg, 
            .Offset = Offset,
        },
    };
}

static inline jit_data Data_Reg(uint Reg)
{
    return (jit_data) {
        .StorageType = STORAGE_TYPE_REG,
        .As.Reg = Reg,
    };
}

#endif /* JIT_DATA_H */

