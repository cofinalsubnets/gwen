/* the gcc builtins that answer a CONSTANT at parse, plus the library ones and the
 * l/ll counting spellings -- all held to gcc, which is the only reason the exact
 * values below are written out.
 *
 *   - __builtin_offsetof rides the same fold as the hand-written &((T*)0)->m idiom,
 *     so the two spellings cannot disagree; the designator takes .b and [i] tails.
 *   - __builtin_types_compatible_p compares the MARKED types _Generic keeps, so an
 *     inner const tells `const char *` from `char *`, and NOTHING decays: T[] is
 *     not T*, which is the question linux's __must_be_array asks it.
 *   - __builtin_constant_p is 1 only where the parse-time fold settles the operand.
 *     CONSERVATIVE: a miss answers 0 and sends the consumer down its runtime lane.
 *   - __builtin_memcpy and friends ARE the plain functions, declared on the way past.
 */
#include <stddef.h>

struct S { int a; char b; int c[4]; };
struct T { struct S s; long l; };

typedef char *cp;

static int lib(void) {
	char d[8];
	__builtin_memset(d, 0, 8);
	__builtin_memcpy(d, "abc", 4);
	if (__builtin_strlen(d) != 3) return 1;
	if (__builtin_memcmp(d, "abc", 4) != 0) return 2;
	__builtin_memmove(d + 1, d, 3);
	if (d[1] != 'a') return 3;
	__builtin_strcpy(d, "xy");
	if (d[0] != 'x' || d[2] != 0) return 4;
	return 0;
}

int main(void) {
	int r = 4;

	if (__builtin_offsetof(struct S, a) != 0) return 11;
	if (__builtin_offsetof(struct S, b) != 4) return 12;
	if (__builtin_offsetof(struct S, c) != 8) return 13;
	if (__builtin_offsetof(struct S, c[2]) != 16) return 14;
	if (__builtin_offsetof(struct T, s.c[1]) != 12) return 15;
	if (__builtin_offsetof(struct T, l) != 24) return 16;
	if (__builtin_offsetof(struct S, c) != offsetof(struct S, c)) return 17;

	if (!__builtin_types_compatible_p(int, int)) return 20;
	if (__builtin_types_compatible_p(int, unsigned int)) return 21;
	if (__builtin_types_compatible_p(int, long)) return 22;
	if (!__builtin_types_compatible_p(struct S, struct S)) return 23;
	if (__builtin_types_compatible_p(struct S, struct T)) return 24;
	if (!__builtin_types_compatible_p(char *, char *)) return 25;
	if (__builtin_types_compatible_p(const char *, char *)) return 26;
	if (__builtin_types_compatible_p(int[4], int *)) return 27;
	if (!__builtin_types_compatible_p(int, const int)) return 28;   /* top-level drops */
	if (!__builtin_types_compatible_p(cp, char *)) return 29;
	if (!__builtin_types_compatible_p(__typeof__(r), int)) return 30;

	if (!__builtin_constant_p(3 + 4)) return 40;
	if (!__builtin_constant_p(sizeof(struct S))) return 41;
	if (__builtin_constant_p(r)) return 42;

	if (__builtin_clz(1u) != 31) return 50;
	if (__builtin_clzl(1ul) != 63) return 51;
	if (__builtin_clzll(1ull) != 63) return 52;
	if (__builtin_clzl(1ul << 40) != 23) return 53;
	if (__builtin_ctz(8u) != 3) return 54;
	if (__builtin_ctzl(6ul) != 1) return 55;
	if (__builtin_ctzl(1ul << 40) != 40) return 56;
	if (__builtin_ctzll(1ull << 33) != 33) return 57;

	if (lib()) return 60 + lib();
	return 42;
}
