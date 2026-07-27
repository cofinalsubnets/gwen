/* SysV MEMORY-class aggregates (x64): a struct PAST 16 bytes passes wholly
 * on the stack and returns through a hidden pointer. musl's fopencookie takes
 * a 32-byte cookie_io_functions_t by value; the return half completes the
 * pairing (the v6-M base ABI already did it past 4 bytes -- one gate now).
 *
 * covered: a MEMORY param behind register args, one that overflows LATE
 * (6 gp args ahead of it, so it lands deep in the block), several in one
 * call (the block's slot arithmetic), a MEMORY return, a return fed straight
 * into another call, and the by-value COPY semantics (a callee mutating its
 * parameter must not touch the caller's object). freestanding, exit-code
 * only -- the cross-toolchain ABI check lives in the moon-* package gates. */

typedef int (readfn)(void *, char *, unsigned long);
typedef struct { readfn *read; readfn *write; void *seek; void *close; } iofn_t;
struct big { long a, b, c, d, e; };          /* 40 bytes */

static int r1(void *c, char *b, unsigned long n) { (void)c; (void)b; (void)n; return 1; }
static int w1(void *c, char *b, unsigned long n) { (void)c; (void)b; (void)n; return 2; }

static int take(int lead, iofn_t f, int tail)
{
	return lead * 100 + (f.read ? 1 : 0) + (f.write ? 2 : 0)
	     + (f.seek ? 4 : 0) + (f.close ? 8 : 0) + tail;
}

static int deep(int a, int b, int c, int d, int e, iofn_t f, int g)
{
	return a + b + c + d + e + g + (f.read ? 16 : 0);
}

static long pairup(struct big s, struct big t)   /* two MEMORY args in one call */
{
	return s.a + t.e * 10;
}

static long mutate(struct big s)                 /* by-value: the caller's copy stands */
{
	s.a = 99; s.e = 99;
	return s.a + s.e;
}

static struct big mk(long k, long j)             /* a MEMORY return */
{
	struct big s;
	s.a = k; s.b = k * 2; s.c = k * 3; s.d = k * 4; s.e = k * 5 + j;
	return s;
}

int main(void)
{
	iofn_t f = { r1, w1, 0, (void *)1 };
	struct big s = { 1, 2, 3, 4, 5 }, t = { 6, 7, 8, 9, 10 };
	int r = take(2, f, 7);                    /* 218 */
	r += deep(1, 2, 3, 4, 5, f, 6);           /* 37 */
	r += (int)pairup(s, t);                   /* 1 + 100 = 101 */
	r += (int)mutate(s) + (int)s.a + (int)s.e;   /* 198 + 1 + 5 = 204 */
	struct big u = mk(3, 7);                  /* 3,6,9,12,22 */
	r += (int)(u.a + u.b + u.c + u.d + u.e);  /* 52 */
	r += (int)pairup(mk(1, 0), u);            /* 1 + 220 = 221 */
	return r & 0xff;                          /* 833 & 255 = 65 */
}
