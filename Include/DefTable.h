#ifndef DEF_TABLE_H
#define DEF_TABLE_H

#include "Common.h"
#include "JitCommon.h"

typedef struct jit_token jit_token;
typedef struct def_table_entry def_table_entry;
typedef struct def_table def_table;

struct def_table_entry
{
    const char *Str;
    u32 Hash;
    int StrLen;
    def_table *Scope;
    jit_expression Expr;
};

struct def_table 
{
    def_table_entry Array[256];
    uint Count; 
};


def_table DefTable_Init(void);
void DefTable_Destroy(def_table *Table);

def_table_entry *DefTable_Define(def_table *Table, const char *Str, int StrLen);
def_table_entry *DefTable_Find(def_table *Table, const char *Str, int StrLen);


#endif /* DEF_TABLE_H */


