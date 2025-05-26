#include "Common.h"
#include "DefTable.h"


#define EMPTY(p_e) (NULL == (p_e)->As.Str.Ptr)
#define DELETED(p_e) (0 == (p_e)->As.Str.Len)
#define TableSize(p_t) (uint)STATIC_ARRAY_SIZE((p_t)->Array)

static u32 DefTable_Hash(const char *Str, int StrLen)
{
    return (Str[0] & 0x1F) | (Str[StrLen - 1] << 6);
}



static def_table_entry *GetArray(def_table *Table)
{
    return Table->Array;
}

static u32 NextIndex(const def_table *Table, u32 i)
{
    return (i + 1) % TableSize(Table);
}



def_table DefTable_Init(void)
{
    return (def_table) { 0 };
}

void DefTable_Destroy(def_table *Table)
{
    (void)Table;
}

def_table_entry *DefTable_Define(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type)
{
    if (Table->Count == TableSize(Table))
        return NULL; /* table is full */

    def_table_entry *Array = GetArray(Table);
    u32 Hash = DefTable_Hash(Str, StrLen);
    u32 Index = Hash % TableSize(Table);
    while (1)
    {
        def_table_entry *Entry = &Array[Index];
        if (EMPTY(Entry) || DELETED(Entry))
        {
            *Entry = (def_table_entry) {
                .Type = Type,
                .Hash = Hash,
                .As.Str = {
                    .Ptr = Str,
                    .Len = StrLen,
                },
            };
            Table->Count++;
            return Entry;
        }
        Index = NextIndex(Table, Index);
    }
}

def_table_entry *DefTable_Find(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type)
{
    def_table_entry *Array = GetArray(Table);
    u32 Hash = DefTable_Hash(Str, StrLen);
    u32 Index = Hash % TableSize(Table);
    for (uint i = 0; i < TableSize(Table); i++)
    {
        def_table_entry *Entry = &Array[Index];
        if (EMPTY(Entry))
        {
            /* this entry is empty, so entries after it with the same key has never been used, 
             * therefore it was never in the table to begin with */
            return NULL;
        }
        else if (Hash == Entry->Hash 
        && Type == Entry->Type
        && StrLen == Entry->As.Str.Len 
        && StrEqu(Str, Entry->As.Str.Ptr, StrLen))
        {
            /* found entry */
            return Entry;
        }

        Index = NextIndex(Table, Index);
    }
    return NULL;
}

bool8 DefTable_Delete(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type)
{
    def_table_entry *Entry = DefTable_Find(Table, Str, StrLen, Type);
    if (!Entry)
        return false;

    Entry->As.Str.Len = 0;
    return true;
}

