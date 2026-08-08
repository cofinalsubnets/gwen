/* `E1 op= E2` evaluates E1 ONCE -- the other half of 114-rmwlv.
 *
 * The desugar (asn lv (bin op lv rhs)) duplicates the lvalue, so a stepping one
 * runs its effect twice and the store lands where the load did not. parse keeps
 * an impure lvalue whole (rmw) and gen moors its address in a frame temp, so
 * every target shape still rides its own store lane: the narrowing store, the
 * pointer scale, a double, a bitfield.
 *
 * The two shapes that reach shipped code: sqlite3.c's `aOut[j++] += c` and
 * `p->a[k++] ^= x[j]`.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

#include <stdio.h>

struct bf { unsigned a : 5; unsigned b : 7; };
struct pt { int n; char c; };

static int iv[6];
static unsigned char cv[6];   /* unsigned: plain char's sign is the TARGET's business */
static unsigned uv[4];
static double dv[4];
static struct bf bs[3];
static struct pt ps[3];
static int *pv[4];
static int anchor[8];

int main(void)
{
	int *p;
	unsigned char *cp;
	unsigned *up;
	double *dp;
	struct bf *bp;
	struct pt *sp;
	int **pp;
	int i, r = 0;

	/* the plain integer shapes: the pointer steps once, the store lands under it */
	iv[0] = 10; iv[1] = 20; iv[2] = 30;
	p = iv;
	*p++ += 5;
	r += iv[0] == 15;
	r += iv[1] == 20;
	r += (p - iv) == 1;

	p = iv;
	*p++ *= 3;                          /* 15*3 */
	r += iv[0] == 45;
	r += (p - iv) == 1;

	p = iv;
	*p++ /= 9;                          /* the divide register contract */
	r += iv[0] == 5;
	p = iv;
	*p++ %= 3;
	r += iv[0] == 2;
	r += (p - iv) == 1;

	/* an index that steps -- sqlite's shape */
	iv[0] = 1; iv[1] = 2; iv[2] = 4; iv[3] = 8;
	i = 1;
	iv[i++] += 100;
	r += iv[1] == 102;
	r += iv[2] == 4;
	r += i == 2;

	i = 2;
	iv[i++] ^= 12;                      /* 4 ^ 12 */
	r += iv[2] == 8;
	r += i == 3;

	i = 3;
	iv[i++] <<= 4;
	r += iv[3] == 128;
	i = 3;
	iv[i++] >>= 2;
	r += iv[3] == 32;
	r += i == 4;

	/* a NARROW target: the value truncates on the way back into the slot */
	cv[0] = 100; cv[1] = 7;
	cp = cv;
	*cp++ += 200;                       /* 300 does not fit a byte */
	r += cv[0] == 44;
	r += cv[1] == 7;
	r += (cp - cv) == 1;

	/* unsigned wrap on a 32-bit target */
	uv[0] = 4294967295u; uv[1] = 1;
	up = uv;
	*up++ += 2u;
	r += uv[0] == 1u;
	r += uv[1] == 1u;
	r += (up - uv) == 1;

	/* a POINTER target: the step scales by the pointee, once */
	pv[0] = anchor; pv[1] = 0;
	pp = pv;
	*pp++ += 3;
	r += (pv[0] - anchor) == 3;
	r += (pp - pv) == 1;

	/* a double target rides the float store lane */
	dv[0] = 2.5; dv[1] = 1.0;
	dp = dv;
	*dp++ *= 4.0;
	r += dv[0] == 10.0;
	r += dv[1] == 1.0;
	r += (dp - dv) == 1;

	i = 1;
	dv[i++] += 0.5;
	r += dv[1] == 1.5;
	r += i == 2;

	/* a struct member behind a stepping pointer */
	ps[0].n = 4; ps[0].c = 'a'; ps[1].n = 9;
	sp = ps;
	sp++->n += 6;
	r += ps[0].n == 10;
	r += ps[1].n == 9;
	r += (sp - ps) == 1;

	/* a BITFIELD member behind one: the unit is read, masked and written once */
	bs[0].a = 3; bs[0].b = 5; bs[1].a = 1;
	bp = bs;
	(bp++)->a += 4;
	r += bs[0].a == 7;
	r += bs[0].b == 5;
	r += bs[1].a == 1;
	r += (bp - bs) == 1;

	bp = bs;
	(bp++)->a += 28;                    /* 7+28 = 35, and a 5-bit field keeps 3 */
	r += bs[0].a == 3;
	r += bs[0].b == 5;

	/* and the calm lvalues, which still take the plain desugar */
	i = 5;
	i += 2;
	r += i == 7;
	iv[2] = 6;
	iv[2] *= 5;
	r += iv[2] == 30;
	p = iv;
	*p += 1;
	r += iv[0] == 2;
	r += (p - iv) == 0;

	printf("%d %d %d %d %d %d\n", iv[0], iv[1], iv[2], iv[3], (int)cv[0], i);
	printf("%u %u %g %g %d %u %u\n", uv[0], uv[1], dv[0], dv[1], ps[0].n,
	       bs[0].a, bs[0].b);
	printf("%d %ld\n", ps[1].n, (long)(pv[0] - anchor));
	return r;
}
