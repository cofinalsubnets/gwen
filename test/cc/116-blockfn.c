/* a function DECLARED inside a block is a function, not a local.
 *
 * `int f1(char *);` in a body declares an extern function with block scope. It
 * bound a local slot instead, so the call jumped through an uninitialized frame
 * word -- a SIGSEGV out of ordinary C that reads as harmless. c-testsuite's
 * 00078 is the shape; the declaration's own params are skipped at block scope,
 * so the sig it registers carries the return type and an empty list, which the
 * arity check exempts.
 *
 * The K&R spelling -- `void f(), g();`, empty parens, several to a line -- is
 * the one that matters in the field: xlander (1992) writes fourteen of them
 * across four files, and each of those four alone was enough to segfault it.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

static int add1(char *p)  { return *p + 1; }
static long twice(long n) { return n + n; }

/* declared block-scope below and defined here, so nothing at file scope says it */
int blockonly(int n);
int blockonly(int n) { return n * 3; }

int seven(void)  { return 7; }
int eleven(void) { return 11; }

static int deep(void)
{
	int blockonly(int);      /* an inner block's declaration, under a local of its own */
	int n = 4;
	return blockonly(n) == 12;
}

int main(void)
{
	char s = 1;
	int v[16];
	int r = 0;
	int (*fp)(char *) = add1;   /* a function POINTER stays a local */

	{
		int add1(char *);           /* the 00078 shape */
		long twice(long), t = 7;    /* mixed with an object sharing the base type */

		r += add1(&s) == 2;
		r += twice(t) == 14;
		r += t == 7;                /* the object beside it is still an object */
	}
	{
		/* the K&R spelling: empty parens, comma-listed. Both take no argument,
		   because a C23 gcc reads `f()` as `f(void)` and the battery's control
		   is gcc at its default standard. */
		int seven(), eleven();

		r += seven() == 7;
		r += eleven() == 11;
	}

	r += deep();
	r += fp(&s) == 2;
	v[0] = r;
	return v[0];
}
