#ifndef COMMON_H
#define COMMON_H

#include <stdint.h>

#define SWAP(typ, a, b) do {\
    typ t = a;\
    a = b;\
    b = t;\
} while (0)
#define IN_RANGE(lower, n, upper) ((lower) <= (n) && (n) <= (upper))
#define IN_RANGEX(lower, n, upper) ((lower) < (n) && (n) < (upper))
#define ABSI(integer) ((integer) < 0? -(integer) : (integer))
#define MAX(a, b) ((a) > (b)? (a) : (b))

#define true 1
#define false 0
typedef uint8_t bool8;
typedef uint64_t u64;
typedef int64_t i64;
typedef uint32_t u32;
typedef int32_t i32;
typedef int16_t i16;
typedef uint8_t u8;



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



#endif /* COMMON_H */

