#include "DefTable.h"


def_table DefTable_Init(uint InitialCapacity)
{
    def_table Table = {
        .Count = 0,
        .Capacity = InitialCapacity,
    };
    return Table;
}

void DefTable_Destroy(def_table *Table)
{
}

def_table_entry *DefTable_Define(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type)
{
}


