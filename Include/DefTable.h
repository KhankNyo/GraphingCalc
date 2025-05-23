#ifndef DEF_TABLE_H
#define DEF_TABLE_H

#include "Common.h"
#include "JitCommon.h"

typedef struct def_table_entry def_table_entry;
typedef struct def_table def_table;

typedef enum def_table_entry_type 
{
    TYPE_FUNCTION,
    TYPE_VARIABLE,
} def_table_entry_type;

struct def_table_entry
{
    u32 Hash;
    def_table_entry_type Type;

    union {
        strview Str;
        jit_function Function;
        jit_variable Variable;
    } As;
};

struct def_table 
{
    uint Count; 
    def_table_entry Array[256];
};

def_table DefTable_Init(void);
void DefTable_Destroy(def_table *Table);

def_table_entry *DefTable_Define(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type);
def_table_entry *DefTable_Find(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type);
bool8 DefTable_Delete(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type);


#endif /* DEF_TABLE_H */


