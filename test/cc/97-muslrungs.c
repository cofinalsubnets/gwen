/* the musl rungs: C99 [static n] array parameters, the __builtin_va_list
 * builtin typedef (+ __builtin_va_copy), and __typeof/__typeof__ -- the
 * declaration surface musl 1.2.5 stands on (its alltypes.h typedefs va_list
 * from the builtin; its weak_alias declares through __typeof; syscall.h's
 * __procfdname takes a [static n] buffer). freestanding, exit-code only. */

typedef __builtin_va_list va_list;
typedef __builtin_va_list __isoc_va_list;

/* [static n] and [const n] parameter declarators: the promise costs nothing,
 * the parameter is a pointer */
static void fill(char buf[static 8], int n[const 2])
{
	buf[0] = 'm'; buf[7] = 'z';
	n[0] = 3; n[1] = 4;
}

static int vtake(int n, va_list ap0)
{
	va_list ap;
	__builtin_va_copy(ap, ap0);
	int s = 0;
	for (int i = 0; i < n; i++) s += __builtin_va_arg(ap, int);
	double d = __builtin_va_arg(ap, double);
	__builtin_va_end(ap);
	return s + (int)d;
}

static int take(int n, ...)
{
	va_list ap;
	__builtin_va_start(ap, n);
	int a = vtake(n, ap);       /* a va_list param decays and rides through */
	int b = vtake(n, ap);       /* the copy left the original unmoved */
	__builtin_va_end(ap);
	return a + b;
}

/* __typeof over globals and functions -- musl's weak_alias shape */
#define weak_alias(old, new) \
	extern __typeof(old) new __attribute__((__weak__, __alias__(#old)))
static long gval = 40;
static int gfn(int x) { return x + 1; }
weak_alias(gfn, gfn_alias);
weak_alias(gval, gval_alias);

int main(void)
{
	char b[8]; int m[2];
	fill(b, m);
	__typeof__(gval) x = gval + m[0] + m[1];        /* 47 */
	__typeof__(x) y = sizeof(__typeof__(gval));     /* 8 */
	int v = take(2, 5, 6, 1.5);                     /* (5+6+1) twice = 24 */
	int c = b[0] == 'm' && b[7] == 'z' ? 50 : 0;
	return (int)x + (int)y + v + c + gfn(0);        /* 47+8+24+50+1 = 130 */
}
