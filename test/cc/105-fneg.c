/* unary minus on a float, a double and a _Complex -- NEGATION, not a subtract
 * from zero.
 *
 * the two agree on every input but one. `0.0 - d` cannot produce -0.0, because
 * 0.0 - 0.0 is +0.0 by IEEE 754; only flipping the sign (or subtracting from
 * MINUS zero, which is what gen.l does) gets it right. this lane read `0.0 - d`
 * for years and nothing noticed: love has no negative zero at all, so the fault
 * was unreachable from the language and visible only to a C program.
 *
 * ⚠ it was found by test/libc/num.c -- strtod("-0.0") against glibc -- and it
 * lives HERE because it is a codegen law, freestanding and exit-code compared
 * like the rest of this battery. the zero cases are the whole point: drop them
 * and the test passes against the broken lowering.
 *
 * NaN's sign is deliberately not asserted -- C promises nothing about it. */

static int dneg(double d)      /* through a call, so nothing is folded away */
{
	union { double d; unsigned long u; } b;
	b.d = -d;
	return (int) (b.u >> 63);
}

static int fneg(float f)
{
	union { float f; unsigned u; } b;
	b.f = -f;
	return (int) (b.u >> 31);
}

static double dbits(unsigned long u)
{
	union { double d; unsigned long u; } b;
	b.u = u;
	return b.d;
}

int main(void)
{
	volatile double pz = 0.0, one = 1.0, big = 1e308;
	volatile float fz = 0.0f, fone = 1.0f;
	double nz = dbits(0x8000000000000000UL);
	int r = 0;

	/* ⚠ THE CASE THAT MATTERS: -(+0.0) is -0.0, and -(-0.0) is +0.0 */
	r += dneg(pz) == 1;
	r += dneg(nz) == 0;
	r += fneg(fz) == 1;

	/* the ordinary values keep working */
	r += dneg(one) == 1;
	r += dneg(-one) == 0;
	r += dneg(big) == 1;
	r += fneg(fone) == 1;
	r += fneg(-fone) == 0;

	/* the value is negated, not merely the sign bit */
	r += (-one == -1.0) ? 1 : 0;
	r += (-(-one) == 1.0) ? 1 : 0;
	r += (-2.5 == 0.0 - 2.5) ? 1 : 0;

	/* ..and -0.0 still COMPARES equal to 0.0, which is why the sign bit is
	   the only way to see this at all */
	r += (-pz == 0.0) ? 1 : 0;

	/* double negation of a zero returns to +0.0 */
	{ union { double d; unsigned long u; } b;
	  b.d = -(-pz);
	  r += (b.u >> 63) == 0; }

	/* infinity flips too */
	r += dneg(dbits(0x7FF0000000000000UL)) == 1;
	r += dneg(dbits(0xFFF0000000000000UL)) == 0;

	return r;
}
