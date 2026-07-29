/* a double converts to an unsigned 64-bit value UNSIGNED.
 *
 * the mirror of 107-u2d.c, and it was the same hole from the other side.
 * cvttsd2si is a SIGNED truncation, so every double at or above 2^63 saturated
 * to 0x8000000000000000: (unsigned long)1.0e19 answered 9223372036854775808
 * instead of 10000000000000000000. t32 rides its own cvttsd2ui and the pair lane
 * peels bits by hand; the 64-bit targets have exactly one truncating instruction
 * and it only speaks signed.
 *
 * gen.l now biases around 2^63 -- under it convert straight, at or above it
 * subtract 2^63 first and put the bit back. the subtract is EXACT (a d >= 2^63
 * has an ulp of at least 2^11 and d - 2^63 is a multiple of it, landing under
 * 2^63), so no rounding enters and the values below are exact, not approximate.
 *
 * ⚠ the conversion happens at every context that has a destination type, not
 * just a cast -- so this exercises the cast, a store, an initializer, an
 * argument and a return, which are the five places gen.l routes through asintt.
 * fixing only the cast would leave `unsigned long v = d;` wrong.
 *
 * ⚠ NOT asserted: negative, out-of-range and NaN sources. C leaves all three
 * undefined, and a test that pins them would be pinning whatever two compilers
 * happen to share rather than a law. */

static unsigned long d2u(double d)          /* through a call, so nothing folds */
{
	return (unsigned long) d;
}

static unsigned long viastore(double d)     /* the store lane, no cast */
{
	unsigned long v;
	v = d;
	return v;
}

static unsigned long viainit(double d)      /* the initializer lane */
{
	unsigned long v = d;
	return v;
}

static unsigned long takes(unsigned long v) { return v; }

static unsigned long viaarg(double d)       /* the outgoing-argument lane */
{
	return takes(d);
}

static unsigned long viaret(double d)       /* the return lane */
{
	return d;
}

int main(void)
{
	int r = 0;

	/* below 2^63: the signed lane was already right, and must stay right */
	r += d2u(0.0) == 0UL;
	r += d2u(1.0) == 1UL;
	r += d2u(9.3e18) == 9300000000000000000UL;

	/* ⚠ AT AND ABOVE 2^63 -- every one of these saturated */
	r += d2u(9223372036854775808.0) == 9223372036854775808UL;      /* exactly 2^63 */
	r += d2u(1.0e19) == 10000000000000000000UL;
	r += d2u(18446744073709549568.0) == 18446744073709549568UL;    /* the largest exact */

	/* the truncation still truncates toward zero, above the boundary too */
	r += d2u(1.0e19 + 0.5) == 10000000000000000000UL;
	r += d2u(3.9) == 3UL;

	/* every lane with a destination type, not just the cast */
	r += viastore(1.0e19) == 10000000000000000000UL;
	r += viainit(1.0e19) == 10000000000000000000UL;
	r += viaarg(1.0e19) == 10000000000000000000UL;
	r += viaret(1.0e19) == 10000000000000000000UL;

	/* the round trip, both new lanes composed */
	r += (unsigned long) (double) 9223372036854775808UL == 9223372036854775808UL;
	r += (unsigned long) (double) 18446744073709549568UL == 18446744073709549568UL;

	/* the narrower unsigned types keep taking the signed-then-narrow lane */
	r += (unsigned int) 3000000000.0 == 3000000000U;
	r += (unsigned short) 65535.0 == 65535;

	/* and a signed destination is untouched by any of this */
	r += (long) -1.5 == -1L;
	r += (long) 9.2e18 == 9200000000000000000L;

	return r;
}
