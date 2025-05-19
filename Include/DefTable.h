#ifndef DEF_TABLE_H
#define DEF_TABLE_H

#include "Common.h"
#include "JitExpression.h"

typedef struct jit_token jit_token;
typedef struct def_table_entry def_table_entry;

typedef enum def_table_entry_type 
{
    ENTRY_FUNCTION,
    ENTRY_VARIABLE,
} def_table_entry_type;

typedef struct def_table 
{
    def_table_entry *Ptr;
    uint Count; 
    uint Capacity;
} def_table;

struct def_table_entry
{
    const char *Str;
    int StrLen;
    def_table_entry_type Type;
    def_table Scope;
    jit_expression Expr;
};

def_table DefTable_Init(uint Count);
void DefTable_Destroy(def_table *Table);

def_table_entry *DefTable_Define(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type);


#endif /* DEF_TABLE_H */


