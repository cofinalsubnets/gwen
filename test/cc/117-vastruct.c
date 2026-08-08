/* a by-value struct NAMED in a variadic function's parameter list.
 *
 * The variadic prologue lays a 176-byte register save area and addresses the named
 * params inside it -- and it classified them as scalars, so every aggregate shape
 * read the wrong bytes. c-testsuite's 00140 is the MEMORY-class one and segfaulted.
 *
 * The three answers a named aggregate can want, and they are not interchangeable:
 * a MEMORY-class one (past 16 bytes) binds STRAIGHT to the caller's overflow block;
 * one eightbyte is addressed where its register was saved; and a PAIR touching xmm
 * must be COPIED, because the save area steps the xmm slots 16 apart where a pair's
 * eightbytes must sit 8 apart. Only the all-INT pair is contiguous where it landed.
 *
 * va_arg rides on the same counting: the gp/xmm offsets va_start seeds are what the
 * named params left behind, so an aggregate consuming two registers where the walk
 * charged one hands the anonymous args out shifted.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

#include <stdarg.h>

struct mem { int i, j, k; char *p; float v; };  /* 32 bytes -- MEMORY */
struct ii  { long a, b; };                      /* INT, INT   -- rides two gp, contiguous */
struct dd  { double a, b; };                    /* SSE, SSE   -- two xmm, 16 apart */
struct id  { long a; double b; };               /* INT, SSE   -- one of each */
struct di  { double a; long b; };                /* SSE, INT   -- the other order */
struct i1  { int a, b; };                       /* one INT eightbyte */
struct f1  { float a, b; };                     /* one SSE eightbyte */

static int memarg(struct mem f, struct mem *p, int n, ...)
{
	if (f.i != p->i)
		return 0;
	return p->j + n;
}

static int pairs(struct ii a, struct dd b, struct id c, struct di d, int n, ...)
{
	return (int)(a.a + a.b + b.a + b.b + c.a + c.b + d.a + d.b) + n;
}

static int ones(struct i1 a, struct f1 b, int n, ...)
{
	return a.a + a.b + (int)(b.a + b.b) + n;
}

/* four INT pairs is eight eightbytes: the last one runs the gp registers out and
   lands in the caller's overflow block instead */
static int spilt(struct ii a, struct ii b, struct ii c, struct ii d, int n, ...)
{
	return (int)(a.a + a.b + b.a + b.b + c.a + c.b + d.a + d.b) + n;
}

/* the anonymous args must arrive where the named aggregates left the counters */
static int walk(struct mem f, struct ii g, struct dd h, int n, ...)
{
	va_list ap;
	int i, s = 0;

	va_start(ap, n);
	for (i = 0; i < n; i++)
		s += va_arg(ap, int);
	s += (int)va_arg(ap, double);
	va_end(ap);
	return (int)(f.i + f.v + g.a + g.b + h.a + h.b) + s;
}

int main(void)
{
	struct mem f;
	struct ii a; struct dd b; struct id c; struct di d;
	struct i1 o1; struct f1 o2;
	int r = 0;

	f.i = f.j = 1; f.k = 0; f.p = 0; f.v = 100;
	a.a = 1; a.b = 2;
	b.a = 3; b.b = 4;
	c.a = 5; c.b = 6;
	d.a = 7; d.b = 8;
	o1.a = 9; o1.b = 10;
	o2.a = 11; o2.b = 12;

	r += memarg(f, &f, 2) == 3;                    /* the 00140 shape, no anonymous args */
	r += memarg(f, &f, 2, 1, f, &f) == 3;          /* and with a struct passed anonymously */
	r += pairs(a, b, c, d, 5) == 41;
	r += ones(o1, o2, 5) == 47;
	r += spilt(a, a, a, a, 5) == 17;
	r += walk(f, a, b, 3, 1, 2, 3, 4.0) == 121;
	return r;
}
