/* __extension__ (gcc) is a no-op accepted at a declaration's head -- file scope,
 * block scope, member, before typedef -- and as a cast-expression prefix. glibc
 * opens `__extension__ typedef` (bits/types.h), and pthread.h's
 * __atomic_wide_counter rides it at member position. A typedef's declarator
 * carries a trailing attribute run too (`} __pthread_unwind_buf_t
 * __attribute__((__aligned__));`).
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

__extension__ unsigned long long v = 40;
__extension__ typedef unsigned long long ull;
struct S { __extension__ ull w; int a; };
union U { __extension__ long long a; int b; };
typedef struct TT { int x; } TT __attribute__((__aligned__));

int main(void)
{
	__extension__ long long b = 2;
	ull y = __extension__ 3;
	__extension__ int arr[2] = {4, 5};
	struct S s = { 6, 7 };
	union U u;
	TT t = { 9 };
	__extension__ typedef int bt;
	bt z = 11;
	int r = 0;
	u.a = 8;
	r += v == 40;
	r += b == 2;
	r += y == 3;
	r += arr[0] + arr[1] == 9;
	r += s.w == 6 && s.a == 7;
	r += u.a == 8;
	r += t.x == 9;
	r += z == 11;
	r += (__extension__ 1 + 2) == 3;
	r += (int)(__extension__ (long long)10) == 10;
	return r;
}
