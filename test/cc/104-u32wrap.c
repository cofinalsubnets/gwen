/* 32-bit unsigned arithmetic WRAPS -- `unsigned` is a modular ring at 2^32, and
 * C says so for + - * << (& | ^ cannot carry, / and % cannot grow).
 *
 * cc computes in 64-bit registers and used to truncate only on a STORE, so a
 * uint result consumed IN PLACE -- by a shift, a compare, an index -- carried
 * bits above 31 that C had already thrown away. Parking the same value in a
 * variable first hid it, which is why every package gate passed for months: the
 * shape that catches it is a raw overflowing product fed straight to a shift,
 * and the classic instance is a de Bruijn ctz. musl's mallocng has one, and the
 * garbage index it read walked mallocng into unbounded recursion.
 *
 * The type matters as much as the wrap: `(x & -x) * K` needs the `&` to answer
 * `unsigned int` too, or the multiply sees a 64-bit operand and skips its own
 * narrowing. Constant folding needs it as well -- the folder reads its types off
 * the casts the literal lane emits for a `u` suffix.
 *
 * Each check contributes 1, so the exit code IS the number that passed. */

typedef unsigned int u32;

static const char debruijn32[32] = {
	0, 1, 23, 2, 29, 24, 19, 3, 30, 27, 25, 11, 20, 8, 4, 13,
	31, 22, 28, 18, 26, 10, 7, 12, 21, 17, 9, 6, 16, 5, 15, 14
};
static int a_ctz_32(u32 x) { return debruijn32[(x & -x) * 0x076be629 >> 27]; }
static int a_clz_32(u32 x)
{
	x >>= 1;
	x |= x>>1; x |= x>>2; x |= x>>4; x |= x>>8; x |= x>>16;
	x++;
	return 31 - a_ctz_32(x);
}
static u32 mul(u32 a, u32 b) { return a * b; }

/* the folder's lane: a `u`-suffixed constant expression that overflows */
u32 gmul = (64u * 0x076be629u) >> 27;
u32 gshl = (0x40000000u << 3) >> 28;
unsigned long gwide = (64ul * 0x076be629ul) >> 27;   /* 64-bit: nothing to wrap */

int main(void)
{
	int ok = 0;
	u32 a = 64, b = 0x076be629, c = 0xffffffff, d = 3, e = 0x40000000;

	/* the four carrying ops, result consumed in place */
	ok += ((a * b) >> 27) == 27;
	ok += ((c + d) >> 28) == 0;
	ok += ((1u - d) >> 28) == 15;
	ok += ((e << d) >> 28) == 0;

	/* ..and with a constant operand, which rides the immediate lane */
	ok += ((a * 0x076be629u) >> 27) == 27;
	ok += ((c + 3u) >> 28) == 0;
	ok += ((e << 3) >> 28) == 0;

	/* consumed by a COMPARE rather than a shift */
	ok += ((a * b) > 4000000000u) == 0;
	ok += ((c + d) < 10u) == 1;

	/* mixed uint/int is still uint (usual arithmetic conversions) */
	{ int s = 3; ok += ((c + s) >> 28) == 0; }

	/* a non-carrying op must still answer `unsigned int`, or the multiply
	 * wrapped around it sees a 64-bit operand: this is the mallocng shape */
	ok += (((a & -a) * 0x076be629u) >> 27) == 27;
	ok += ((((a | 0u) * b)) >> 27) == 27;
	ok += ((((c / d) + 0u) * 4u) >> 30) == 1;

	/* the de Bruijn kernel whole, and the clz built on it */
	ok += a_ctz_32(64) == 6;
	ok += a_ctz_32(4) == 2;
	ok += a_clz_32(127) == 25;
	ok += ((28 - a_clz_32(127)) * 4 + 8) == 20;   /* musl size_to_class's index */

	/* across a call boundary and a store round trip */
	ok += (mul(a, b) >> 27) == 27;
	{ u32 t = a * b; ok += (t >> 27) == 27; }

	/* 64-bit stays 64-bit -- nothing narrows what C did not narrow */
	{ unsigned long w = 0xffffffff; ok += ((w * 3) >> 32) == 2; }
	{ unsigned long w = c; int s = 3; ok += ((w + s) >> 28) == 16; }

	/* the constant folder */
	ok += gmul == 27;
	ok += gshl == 0;
	ok += gwide == 59;                            /* 64-bit: the product does NOT wrap */
	ok += ((1u << 20) * 3u) >> 20 == 3;

	return ok;                                     /* 25 */
}
