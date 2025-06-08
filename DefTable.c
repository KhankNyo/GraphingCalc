#include "Common.h"
#include "DefTable.h"


#define EMPTY(p_e) (NULL == (p_e)->As.Common.Str.Ptr)
#define DELETED(p_e) (0 == (p_e)->As.Common.Str.Len)
#define TableCapacity(p_t) (uint)(p_t)->Capacity
#define DefTable_Hash(s, l) (Table->HashFn(s, l))


static def_table_entry *GetArray(def_table *Table)
{
    return Table->Array;
}

static u32 NextIndex(const def_table *Table, u32 i)
{
    return (i + 1) & (TableCapacity(Table) - 1);
}



def_table DefTable_Init(def_table_entry *Array, uint Capacity, def_table_hash_fn HashFn)
{
    ASSERT(Capacity % 2 == 0, "bad capacity (should be pow of 2)");
    return (def_table) { 
        .Array = Array,
        .Capacity = Capacity,
        .HashFn = HashFn,
    };
}

void DefTable_Reset(def_table *Table)
{
    Table->Count = 0;
    def_table_entry *i = Table->Head;
    while (i)
    {
        def_table_entry *Next = i->Next;
        *i = (def_table_entry) { 0 };
        i = Next;
    }

    Table->Head = NULL;
    Table->Tail = NULL;
}

def_table_entry *DefTable_Define(def_table *Table, const char *Str, int StrLen, def_table_entry_type Type)
{
    if (Table->Count == TableCapacity(Table))
        return NULL; /* table is full */

    def_table_entry *Array = GetArray(Table);
    u32 Hash = DefTable_Hash(Str, StrLen);
    u32 Index = Hash & (TableCapacity(Table) - 1);
    while (1)
    {
        def_table_entry *Entry = &Array[Index];
        if (EMPTY(Entry) || DELETED(Entry))
        {
            *Entry = (def_table_entry) {
                .Type = Type,
                .Hash = Hash,
                .As.Common.Str = {
                    .Ptr = Str,
                    .Len = StrLen,
                },
            };

            if (!Table->Tail)
            {
                ASSERT(!Table->Head, "bad linked list");
                Table->Head = Entry;
                Table->Tail = Entry;
            }
            else
            {
                Table->Tail->Next = Entry;
                Entry->Prev = Table->Tail;
                Table->Tail = Entry;
            }
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
    u32 Index = Hash & (TableCapacity(Table) - 1);
    for (uint i = 0; i < TableCapacity(Table); i++)
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
        && StrLen == Entry->As.Common.Str.Len 
        && StrEqu(Str, Entry->As.Common.Str.Ptr, StrLen))
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

    def_table_entry *Prev = Entry->Prev;
    def_table_entry *Next = Entry->Next;
    if (!Prev) /* removing head node */
        Table->Head = Next;
    else
        Prev->Next = Next;
    if (!Next) /* removing tail node */
        Table->Tail = Prev;
    else
        Next->Prev = Prev;
    Entry->Next = NULL;
    Entry->Prev = NULL;

    Entry->As.Common.Str.Len = 0;
    return true;
}

