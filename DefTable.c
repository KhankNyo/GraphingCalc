#include "DefTable.h"


#define EMPTY(p_e) (NULL == (p_e)->Str)
#define DELETED(p_e) (0 == (p_e)->StrLen)

static u32 DefTable_Hash(const char *Str, int StrLen)
{
    return (Str[0] & 0x1F) | (Str[StrLen - 1] << 6);
}

static bool8 StrEqu(const char *A, const char *B, int Len)
{
    int i = 0;
    for (; i < Len; i++)
    {
        if (A[i] != B[i])
            return false;
    }
    return true;
}

static u32 NextIndex(const def_table *Table, u32 i)
{
    return (i + 1) % STATIC_ARRAY_SIZE(Table->Array);
}


def_table DefTable_Init(void)
{
    return (def_table) { 0 };
}

void DefTable_Destroy(def_table *Table)
{
    (void)Table;
}

def_table_entry *DefTable_Define(def_table *Table, const char *Str, int StrLen)
{
    if (Table->Count == STATIC_ARRAY_SIZE(Table->Array))
        return NULL; /* table is full */

    u32 Hash = DefTable_Hash(Str, StrLen);
    u32 Index = Hash % STATIC_ARRAY_SIZE(Table->Array);
    while (1)
    {
        def_table_entry *Entry = &Table->Array[Index];
        if (EMPTY(Entry) || DELETED(Entry))
        {
            *Entry = (def_table_entry) {
                .Str = Str, 
                .StrLen = StrLen,
                .Hash = Hash,
            };
            Table->Count++;
            return Entry;
        }
        Index = NextIndex(Table, Index);
    }
}

def_table_entry *DefTable_Find(def_table *Table, const char *Str, int StrLen)
{
    u32 Hash = DefTable_Hash(Str, StrLen);
    u32 Index = Hash % STATIC_ARRAY_SIZE(Table->Array);
    for (uint i = 0; i < STATIC_ARRAY_SIZE(Table->Array); i++)
    {
        def_table_entry *Entry = &Table->Array[Index];
        if (EMPTY(Entry))
        {
            /* this entry is empty, so entries after it with the same key has never been used, 
             * therefore it was never in the table to begin with */
            return NULL;
        }
        else if (StrLen == Entry->StrLen 
        && Hash == Entry->Hash
        && StrEqu(Str, Entry->Str, StrLen))
        {
            /* found entry */
            return Entry;
        }

        Index = NextIndex(Table, Index);
    }
    return NULL;
}


