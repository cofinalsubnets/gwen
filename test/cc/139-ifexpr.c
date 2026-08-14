/* #if arithmetic is intmax_t/uintmax_t (C11 6.10.1) -- so a value is a 64-bit bit
 * pattern PLUS a signedness, and love's exact unbounded integers give neither for
 * free. Three separate wrongs lived here:
 *
 *   1. TRUTH was read as `0 <` rather than `!= 0`, so `#if -1` was FALSE and so was
 *      `#if -1 && 1`. Any header branching on a negative constant took the wrong arm,
 *      in silence. This is the one that could cost a right answer.
 *   2. No signedness, so `1UL - 2` answered -1 where C wraps it to a huge unsigned,
 *      and `-1 < 1U` read true where C reads false.
 *   3. `&`, `|`, `^` answered nothing on a big -- love's bit ops stop at the fixnum,
 *      and every pattern past 2^62 is a big. (`<<`/`>>` already routed around it.)
 *
 * Each check drives a runtime value rather than an #error, so a disagreement shows up
 * as this program's exit code and the battery names the number.
 */

static int n;

int main(void)
{
    /* 1 -- truth is "nonzero", every face of it */
#if -1
    n |= 1;
#endif
#if (-1 && 1)
    n |= 2;
#endif
#if !(-1)
    n |= 4;      /* must NOT fire */
#endif
#if (-1 ? 1 : 0)
    n |= 8;
#endif
    if (n != 11) return 1;

    /* 2 -- signedness, and the usual arithmetic conversions at one rank */
    n = 0;
#if 1UL - 2 < 0
    n |= 1;      /* must NOT fire: the subtraction wraps unsigned */
#endif
#if -1 < 1U
    n |= 2;      /* must NOT fire: -1 converts to a huge unsigned */
#endif
#if (0xffffffffffffffffUL / 2) == 0x7fffffffffffffffUL
    n |= 4;
#endif
#if (-7 / 2) == -3 && (-7 % 2) == -1
    n |= 8;      /* C truncates toward zero; % takes the dividend's sign */
#endif
#if (-8 >> 1) == -4
    n |= 16;     /* a signed >> is arithmetic */
#endif
#if (1UL << 63) == 0x8000000000000000UL
    n |= 32;
#endif
    if (n != 60) return 2;

    /* 3 -- the bitwise trio on a value past the fixnum */
    n = 0;
#if (0xffffffffffffffffUL & 0xff) == 0xff
    n |= 1;
#endif
#if (0xfffffffffffffffeUL | 1) == 0xffffffffffffffffUL
    n |= 2;
#endif
#if (0xffffffffffffffffUL ^ 0xff) == 0xffffffffffffff00UL
    n |= 4;
#endif
#if (0xffffffffffffffffUL >> 63) == 1
    n |= 8;      /* the width idiom every portable header uses */
#endif
#if ~0 == -1
    n |= 16;
#endif
    if (n != 31) return 3;

    return 0;
}
