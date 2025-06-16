#ifndef COMMON_H
#define COMMON_H

#include <stdint.h>
#include <stddef.h>
#include <assert.h>

#if defined(_MSC_VER)
#  define STATIC_ASSERT(x, msg) static_assert(x, msg) /* msvc is fucking retarded */
#elif defined(__TINYC__)
#  define STATIC_ASSERT(x, msg) typedef char static_assertion[(x)?(int)sizeof(msg):-1] 
#else
#  define STATIC_ASSERT(x, msg) _Static_assert(x, msg)
#endif /* _MSC_VER */




#define ASSERT(x, msg) assert((x) && (msg))
#define TODO(msg) assert(false && "TODO: "msg)
#define UNREACHABLE() do {\
    assert(false && "Unreachable");\
    *((volatile char *)0) = 0;\
} while (0)
#define SWAP(typ, a, b) do {\
    typ t = a;\
    a = b;\
    b = t;\
} while (0)
#define IN_RANGE(lower, n, upper) ((lower) <= (n) && (n) <= (upper))
#define IN_RANGEX(lower, n, upper) ((lower) < (n) && (n) < (upper))
#define ABSI(integer) ((integer) < 0? -(integer) : (integer))
#define MAX(a, b) ((a) > (b)? (a) : (b))
#define MIN(a, b) ((a) < (b)? (a) : (b))
#define STATIC_ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))
#define ABS(a) ((a) < 0? -(a) : a)
#define ROUND_UP_TO_MULTIPLE(x, multiple) (((x) + (multiple)) / (multiple) * (multiple))
#define ROUND_DOWN_TO_MULTIPLE(x, multiple) ((x) / (multiple) * (multiple))

#define true 1
#define false 0
typedef uint8_t bool8;
typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;

typedef int64_t i64;
typedef int32_t i32;
typedef int16_t i16;
typedef int8_t i8;

typedef unsigned uint;

typedef struct strview 
{
    const char *Ptr;
    int Len;
} strview;


static inline double AbsF(double x)
{
    union {
        double d;
        u64 u;
    } As = {.d = x};
    As.u = As.u & ~(1llu << 63);
    return As.d;
}

static inline double Frac(double x)
{
    return x - (i64)x;
}

static inline double RecipFrac(double x)
{
    return 1 - Frac(x);
}

static inline double Round(double x)
{
    return (i64)(x + .5);
}

static inline char Lowercase(char Ch)
{
    return Ch | 0x20;
}

static inline bool8 StrEqu(const char *A, const char *B, int Len)
{
    for (int i = 0; i < Len && A[i] && B[i]; i++)
    {
        if (A[i] != B[i])
            return false;
    }
    return true;
}

static inline bool8 MemEqu(const void *A, const void *B, int ByteCount)
{
    const u8 *PtrA = A;
    const u8 *PtrB = B;
    for (int i = 0; i < ByteCount; i++)
    {
        if (PtrA[i] != PtrB[i])
            return false;
    }
    return true;
}

static inline void MemCpy(void *restrict Dst, const void *Src, int ByteCount)
{
    u8 *DstPtr = Dst;
    const u8 *SrcPtr = Src;
    while (ByteCount --> 0)
    {
        *DstPtr++ = *SrcPtr++;
    }
}


#endif /* COMMON_H */

