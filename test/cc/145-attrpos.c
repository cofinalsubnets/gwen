/* where an __attribute__ run may sit, held to gcc. The LEADING position on a local
 * was always skipped (pquals drops the whole qualifier/attribute run); the three
 * TRAILING ones -- a local declarator, a parameter, a struct member -- are the same
 * skip, one door over, and the kernel's __maybe_unused / __packed / __aligned are
 * written in all four.
 *
 * ⚠ what is skipped is dropped: an `aligned` ask on a local or a member aligns
 * NOTHING, exactly as the leading spelling has always dropped it. The rows below
 * ask only for the layout the types themselves give.
 * ⚠ the skip takes __attribute__ ALONE. `int x __asm__("y")` still refuses, because
 * dropping an asm name would rename an object in silence.
 *
 * `__label__` at a block head declares local labels; it is parsed and dropped, since
 * a label already mangles to fn.NAME.
 */

struct S {
	int a __attribute__((deprecated));
	char b __attribute__((unused));
	int c[2] __attribute__((unused));
};

static int par(int x __attribute__((unused)), int y, char *p __attribute__((unused))) {
	return y;
}

__attribute__((unused)) static int lead(void) { return 1; }

int main(void) {
	int q __attribute__((unused));
	int r __attribute__((unused)) = 4;
	__attribute__((unused)) int s = 5;
	char buf[4] __attribute__((unused));
	struct S v;

	{
		__label__ again;
		int k = 0;
	again:
		k++;
		if (k < 3) goto again;
		if (k != 3) return 10;
	}
	{
		__label__ a, b;   /* a list, and neither is reached */
		if (r == 0) goto a;
		if (r == 1) goto b;
		goto done;
	a:
		return 11;
	b:
		return 12;
	done:;
	}

	v.a = 1;
	v.b = 2;
	v.c[0] = 3;
	if (sizeof(struct S) != 16) return 20;
	if (v.a + v.b + v.c[0] != 6) return 21;
	if (r + s != 9) return 22;
	if (par(1, 7, buf) != 7) return 23;
	return 42;
}
