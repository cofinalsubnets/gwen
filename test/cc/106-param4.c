/* the 4th integer parameter survives its own function body.
 *
 * on x64 the SysV argument registers are rdi rsi rdx rcx r8 r9, and mooncc's
 * home pool is positionally arg-aligned at every position BUT two: arg 3 (rdx)
 * homes to r9 and arg 4 (rcx) to r10, so those two alone need a real prologue
 * mov. the ride analysis may instead let such a param stay in its arrival
 * register -- but it decided that by reading pass 1, where every param sits in
 * a frame slot, and then licensed a REBUILD that allocates differently. pass 2
 * reached for rcx as a store's address shuttle and the 4th parameter was gone;
 * the two out-pointers collapsed onto one, so `*d` wrote through `c`.
 *
 * ⚠ this is a SILENT wrong answer before it is ever a crash: with both stores
 * aimed at the same valid address nothing faults, one variable just never gets
 * written. it reached the shipping binary through am.c's mul64, whose `lo`
 * out-pointer became a wild address -- am_sin then segfaulted for every
 * |x| >= 2^19, i.e. `(sine 1e20)` in out/host/love, under a green test_all.
 *
 * found by the am.c ulp differential (test_ulp), which is the only thing in
 * the tree that had ever compared mooncc's float output against another
 * compiler's. it lives HERE because it is a codegen law about parameters and
 * has nothing to do with math.
 *
 * the register pressure is load-bearing: with a short body the allocator never
 * wants the arrival register and the fault does not appear. keep the temps. */

/* the exact shape from crew/moon/lib/math/am.c -- 64x64 -> 128 without
 * __int128, four params, the last two out-pointers */
static void mul64(unsigned long a, unsigned long b,
                  unsigned long *hi, unsigned long *lo)
{
	unsigned long ah = a >> 32, al = a & 0xffffffffUL;
	unsigned long bh = b >> 32, bl = b & 0xffffffffUL;
	unsigned long p0 = al * bl, p1 = al * bh, p2 = ah * bl, p3 = ah * bh;
	unsigned long mid = p1 + (p0 >> 32);
	unsigned long c = mid < p1 ? (1UL << 32) : 0;

	mid += p2;
	if (mid < p2)
		c += 1UL << 32;
	*hi = p3 + (mid >> 32) + c;
	*lo = (mid << 32) | (p0 & 0xffffffffUL);
}

/* two values in, two out: the plainest form of the same fault */
static void two(unsigned long a, unsigned long b,
                unsigned long *c, unsigned long *d)
{
	unsigned long t0 = a + 1, t1 = a + 2;
	unsigned long s = t0 ^ t1;
	unsigned long u = s + t0 * b + t1 * b;

	*c = s;
	*d = u;
}

/* five params: positions 0, 1 and 4 are arg-aligned and were always right --
 * only 2 and 3 could be lost, so a passing test must pin all three outputs */
static void five(unsigned long a, unsigned long b,
                 unsigned long *c, unsigned long *d, unsigned long *e)
{
	unsigned long t0 = a + 1, t1 = a + 2;
	unsigned long s = t0 ^ t1;
	unsigned long u = s + t0 * b + t1 * b;

	*c = s;
	*d = u;
	*e = s + u;
}

int main(void)
{
	unsigned long hi = 0, lo = 0, x = 0, y = 0, z = 0;
	int r = 0;

	/* the am.c case, at the window values rbig actually produces */
	mul64(0x1e42d130773b76UL, 0x27bac7ebe5f17b3dUL, &hi, &lo);
	r += hi == 0x4b2420c3281b4UL;
	r += lo == 0xf30205e35e10dd1eUL;

	/* both out-params written, and to their OWN addresses */
	two(10, 20, &x, &y);
	r += x == 7;
	r += y == 467;

	x = 0; y = 0;
	five(10, 20, &x, &y, &z);
	r += x == 7;
	r += y == 467;
	r += z == 474;

	/* the pointers must stay distinct: a collapse writes one twice */
	x = 111; y = 222;
	two(1, 1, &x, &y);
	r += x != y;

	return r;
}
