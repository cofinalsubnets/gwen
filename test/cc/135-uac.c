/* the usual arithmetic conversions are a RANK rule, not "either operand is
 * unsigned" (C99 6.3.1.8).
 *
 * When one operand is signed and the other unsigned, and the SIGNED type's rank
 * is strictly greater, the signed type wins if it can represent every value of
 * the unsigned one. On LP64 that is exactly `long` (or `long long`) meeting
 * `unsigned int`: the common type is SIGNED. gen read that pair as unsigned in
 * two independent places, and both answers were a whole quadrant off:
 *
 *   1. `ubin` typed every mixed binary result unsigned, so
 *      `0xffffffff | ((i64)0x7fffffff<<32)` came out `unsigned long`.
 *   2. the constant strength-reduction lane fired on "either operand unsigned"
 *      AND a power-of-two divisor, turning a signed divide into a LOGICAL shift.
 *
 * ⚠ SQLITE IS WHERE THIS SURFACED, and the path is worth keeping because nothing
 * smaller reached it: LARGEST_INT64 is spelled with that exact `|`, so
 * SMALLEST_INT64 is `(i64)-1 - LARGEST_INT64`, and sqlite3MulInt64's overflow
 * guard divides it by the multiplier. Reading INT64_MIN/2 as positive made EVERY
 * multiply look like an overflow, so sqlite fell back to floating point and
 * `SELECT 4294967296*2` answered `8589934592.0` where every other sqlite in the
 * world answers the integer `8589934592`.
 *
 * ⚠ the power-of-two divisor is what makes this nasty: /3u was always right, /2u
 * and /4u were wrong, so the shape that looks safest is the one that broke. */
#include <stdio.h>

typedef long long i64;
#define LARGEST_INT64  (0xffffffff|(((i64)0x7fffffff)<<32))
#define SMALLEST_INT64 (((i64)-1) - LARGEST_INT64)

/* sqlite3MulInt64's portable lane, verbatim in shape */
static int mulchk(i64 *pA, i64 iB)
{
	i64 iA = *pA;
	if (iB > 0) {
		if (iA > LARGEST_INT64 / iB) return 1;
		if (iA < SMALLEST_INT64 / iB) return 1;
	}
	*pA = iA * iB;
	return 0;
}

static i64 dv(i64 a, i64 b) { return a / b; }

int main(void)
{
	i64 neg = -8, n12 = -12, prod = 4294967296LL;
	unsigned int u2 = 2u;
	int bad = 0;

	/* the two bounds must fold to the real ones */
	if (LARGEST_INT64  !=  9223372036854775807LL) bad += 1;
	if (SMALLEST_INT64 != -9223372036854775807LL - 1) bad += 2;

	/* ..and dividing them must stay signed. This is the exact comparison
	 * sqlite's overflow guard makes. */
	if (SMALLEST_INT64 / (i64)2 != -4611686018427387904LL) bad += 4;
	if (LARGEST_INT64  / (i64)2 !=  4611686018427387903LL) bad += 8;

	/* the strength-reduction lane: a power-of-two UNSIGNED literal divisor
	 * against a wider signed left. /3u always worked; /2u and /4u did not. */
	if (neg / 2u != -4) bad += 16;
	if (n12 / 4u != -3) bad += 32;
	if (n12 / 3u != -4) bad += 64;
	if (neg % 2u !=  0) bad += 128;

	/* an unsigned VARIABLE takes the same rule as an unsigned literal */
	if (neg / u2 != -4) bad += 256;

	/* ubin: an unsigned-int literal folded into a 64-bit signed value must
	 * not make the result unsigned. 3 is not a power of two, so this is the
	 * general divide, not the reduction lane. */
	if ((((i64)-8) | 0u) / (i64)3 != -2) bad += 512;
	/* ⚠ and the rule cuts BOTH ways: because the common type is signed long
	 * long, `~0u` widens to +4294967295 rather than sign-extending, so this
	 * masks off the high word. -8 is the wrong answer here, 4294967288 the
	 * right one -- the same rank rule, read in the other direction. */
	if ((((i64)-8) & ~0u) != 4294967288LL) bad += 1024;

	/* and the whole reason: no overflow, and an exact integer product */
	if (mulchk(&prod, 2) != 0) bad += 2048;
	if (prod != 8589934592LL) bad += 4096;

	/* runtime division was never wrong -- keep it beside the folds so a
	 * regression names which half moved */
	if (dv(SMALLEST_INT64, 2) != -4611686018427387904LL) bad += 8192;

	printf("%d %lld %lld %lld %lld\n", bad, LARGEST_INT64, SMALLEST_INT64,
	       SMALLEST_INT64 / (i64)2, prod);
	printf("%lld %lld %lld\n", neg / 2u, n12 / 4u, (((i64)-8) | 0u) / (i64)3);
	/* ⚠ NOT `return bad` -- an exit code is taken mod 256, and the first four
	 * failures here sum past it (8192 would exit 0). The printed line is what
	 * the differential compares; the status only has to be nonzero. */
	return bad ? 1 : 0;
}
