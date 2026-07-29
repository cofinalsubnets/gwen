/* an unsigned 64-bit value converts to double UNSIGNED.
 *
 * cvtsi2sd is a SIGNED conversion, so a word at or above 2^63 lands negative:
 * (double)~0UL answered -1 instead of 18446744073709551616. the 32-bit unsigned
 * types are safe on a 64-bit target because they widen into a positive signed
 * 64-bit value first -- there is nothing wider to widen `unsigned long` into,
 * which is exactly the case that was missed.
 *
 * gen.l now splits the word: both halves are 32-bit, hence positive and exact
 * as signed, and hi*2^32 + lo rounds ONCE, so the result is the correctly
 * rounded whole -- the assertions below check the exact bit patterns, not a
 * tolerance.
 *
 * ⚠ found by the am.c ulp differential's REDUCTION scan (test_ulp) and by
 * nothing else. am.c's rbig builds a 192-bit fraction whose middle word
 * crosses 2^63 on most inputs, so sin/cos drifted to 1609 ulp above the
 * Payne-Hanek handoff at 2^19 -- while every argument BELOW it stayed exact,
 * which is why the ordinary sweeps and the whole corpus stayed green.
 *
 * the values below straddle the boundary deliberately: just under 2^63 (where
 * the signed path was always right), exactly 2^63, and above it. */

static double u2d(unsigned long u)   /* through a call, so nothing folds */
{
	return (double) u;
}

static unsigned long bits(double d)
{
	union { double d; unsigned long u; } b;
	b.d = d;
	return b.u;
}

int main(void)
{
	int r = 0;

	/* below 2^63: the signed lane was already correct, and must stay so */
	r += bits(u2d(0UL)) == 0x0000000000000000UL;
	r += bits(u2d(1UL)) == 0x3ff0000000000000UL;
	r += bits(u2d(0x7fffffffffffffffUL)) == 0x43e0000000000000UL;

	/* ⚠ AT AND ABOVE 2^63 -- every one of these went negative */
	r += bits(u2d(0x8000000000000000UL)) == 0x43e0000000000000UL;
	r += bits(u2d(0x8000000000000001UL)) == 0x43e0000000000000UL;
	r += bits(u2d(0xc000000000000001UL)) == 0x43e8000000000000UL;
	r += bits(u2d(0xffffffffffffffffUL)) == 0x43f0000000000000UL;
	r += bits(u2d(0xfffffffffffff800UL)) == 0x43efffffffffffffUL;

	/* the word am.c's rbig actually produced when this was found */
	r += bits(u2d(0xb81669940ac0e972UL)) == 0x43e702cd3281581dUL;

	/* the sign is the coarsest symptom: nothing unsigned converts negative */
	r += u2d(0x8000000000000000UL) > 0.0;
	r += u2d(0xffffffffffffffffUL) > 0.0;

	/* ..and the magnitude is right, not merely positive */
	r += u2d(0xffffffffffffffffUL) > 1.8e19;
	r += u2d(0x8000000000000000UL) == 9223372036854775808.0;

	/* the 32-bit unsigned types keep taking the widening lane */
	{ unsigned int w = 0xffffffffU;
	  r += (double) w == 4294967295.0; }
	{ unsigned short h = 0xffff;
	  r += (double) h == 65535.0; }

	/* a signed long is untouched by any of this */
	r += (double) (long) -1 == -1.0;

	return r;
}
