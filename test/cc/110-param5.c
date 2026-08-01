/* the 5th integer parameter is a POINTER, not the frame.
 *
 * 106-param4's arm64 sibling, and a different fault under the same lane. gen
 * speaks r4 for the frame base on every target, and a4ize retargets it to fp
 * at the end of build BY POSITION: an r4 sitting in a memory op's BASE slot
 * becomes fp, an r4 anywhere else is left alone. On AArch64 gp 4 arrives in
 * x4 -- so the moment the ride analysis lets the 5th parameter stay in its
 * arrival register, and the body uses it the way a pointer is used, its base
 * slot reads as the frame pointer. `*p`, `p[i]` and `p + i` all addressed the
 * FRAME instead of p.
 *
 * ⚠ silent, and it answers rather than faults: fp is a valid address, so the
 * load lands in the caller's own frame and returns plausible garbage. Lua 5.4
 * found it -- lstrlib's prepstate takes six parameters, its 6th spilled and
 * its 5th rode x4, so ms->p_end came out as sp + lp and EVERY pattern match
 * silently returned nil. string.match, string.gsub and string.find with a
 * pattern were all dead in an interpreter that otherwise ran its whole test
 * battery, printed floats correctly and ran coroutines through setjmp.
 *
 * x64 cannot reach it (r4 is rbp, never an argument register) and neither can
 * thumb (4 arg registers) or riscv (nhome = 0, so nothing rides) -- which is
 * why 110 programs and the whole love corpus under mooncc/arm64 stayed green.
 *
 * The register pressure is load-bearing, exactly as in 106: the 6th parameter
 * must exist and be live, or the 5th never rides and the fault does not
 * appear. Keep all six.
 *
 * Each check contributes 1, so the exit code IS the number that passed. */

#include <stdio.h>

struct span {
	const char *init;
	const char *end;
	const char *p_end;
	void *owner;
	int depth;
};

/* the exact shape from lua-5.4.7 lstrlib.c prepstate: six parameters, the
 * last two a pointer and its length, written into a caller-owned struct */
static void prepstate(struct span *ms, void *owner,
                      const char *s, unsigned long ls,
                      const char *p, unsigned long lp)
{
	ms->owner = owner;
	ms->depth = 200;
	ms->init = s;
	ms->end = s + ls;
	ms->p_end = p + lp;
}

/* the 5th parameter DEREFERENCED rather than offset */
static long deref5(long a, long b, long c, long d, const long *p, long n)
{
	return p[0] + p[n] + (p[1] * 2);
}

/* the 5th parameter STORED THROUGH */
static void store5(long a, long b, long c, long d, long *out, long n)
{
	out[0] = n;
	out[1] = n + 1;
	out[n] = 99;
}

int main(void)
{
	struct span ms;
	const char *s = "hello";
	const char *p = "ell";
	long v[4] = {10, 20, 30, 40};
	long w[4] = {0, 0, 0, 0};
	int r = 0;

	prepstate(&ms, (void *)0, s, 5, p, 3);
	r += ms.init == s;
	r += ms.end == s + 5;
	r += ms.p_end == p + 3;      /* the one that came out as sp + 3 */
	r += ms.depth == 200;

	/* and the same three spans measured, so a wrong answer PRINTS */
	printf("%d %d %d\n", (int)(ms.init - s), (int)(ms.end - s),
	       (int)(ms.p_end - p));

	r += deref5(1, 2, 3, 4, v, 3) == 10 + 40 + 40;
	r += deref5(1, 2, 3, 4, v, 0) == 10 + 10 + 40;

	store5(1, 2, 3, 4, w, 3);
	r += w[0] == 3;
	r += w[1] == 4;
	r += w[3] == 99;
	r += w[2] == 0;

	printf("%ld %ld %ld %ld\n", w[0], w[1], w[2], w[3]);

	return r;
}
