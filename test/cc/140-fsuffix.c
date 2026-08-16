/* Two bugs, one symptom. C11 6.4.4.2: an `f` suffix makes a FLOAT constant -- ours
 * kept 53 bits, so sizeof(1.5f) was 8 and `0.1f == 0.1` was TRUE. Fixing the type
 * alone did not fix the value, which exposed the deeper one:
 *
 * ⚠ A CAST TO float NEVER ROUNDED. gen keeps every float as a double in a register
 * and narrows only at a STORE (fstf), so `(float)d` passed the double straight
 * through -- (float)0.1 == 0.1 read true for a variable too, not just a literal.
 * The cast now round-trips through single precision, which is where the rounding
 * becomes observable. The integer twin -- an `l` suffix, landed 2026-08-16 -- sits
 * at the end.
 */
#include <stdio.h>

static double dv = 0.1;
static float  fv = 0.1f;

int main(void)
{
    if (sizeof(1.5f) != sizeof(float)) return 1;
    if (sizeof(1.5f + 1.5f) != sizeof(float)) return 2;
    if (sizeof(1.5) != sizeof(double)) return 3;
    if (sizeof(1.5f + 1.5) != sizeof(double)) return 4;   /* the usual conversions still widen */

    /* the value, three ways in and one answer */
    if (0.1f == 0.1) return 5;
    if ((float)0.1 == 0.1) return 6;
    if ((float)dv == dv) return 7;
    if ((double)fv != (double)(float)0.1) return 8;

    /* ..and rounding is IDEMPOTENT, so a second narrowing changes nothing */
    if ((float)(float)dv != (float)dv) return 9;

    /* a value that survives single precision exactly must be untouched */
    if ((float)0.5 != 0.5) return 10;
    if (0.25f != 0.25) return 11;
    if (16777216.0f != 16777216.0) return 12;

    /* the classic: 2^24+1 has no float, and rounds down to 2^24 */
    if (16777217.0f != 16777216.0f) return 13;

    /* the INTEGER twin (6.4.4.1): an l/L suffix makes a long, and typing it by
       magnitude alone computed `i + 2L` in int */
    if (sizeof(1L) != sizeof(long)) return 14;
    if (sizeof(1LL) != sizeof(long long)) return 15;
    if (sizeof(1 + 2L) != sizeof(long)) return 16;
    if (sizeof(1L + 2) != sizeof(long)) return 17;

    return 0;
}
