/* the gcc-shaped predefine table (moon.l stddefs): the <stdint>/<limits>/
 * <float.h> family a library config reads instead of probing -- pdclib,
 * gnulib and musl typedef straight off these names. forked once on the word
 * width plus the wchar ABI fork; __LDBL_* deliberately answer double's
 * values (no long double here). and __LINE__ is TRUE: the -D text stamps
 * 1-k..0 (clexat), so the TU's own numbering starts at 1 unskewed.
 *
 * Every check contributes 1, so the exit code IS the number that passed. */

extern int printf(const char *, ...);

union endian { int i; char c[4]; };

int main(void)
{
	__SIZE_TYPE__ s = (__SIZE_TYPE__)-1;
	__WCHAR_TYPE__ w = (__WCHAR_TYPE__)-1;
	__INT64_TYPE__ b = -1;
	union endian u;
	double e = __DBL_EPSILON__;
	int r = 0;

	u.i = 1;
	r += sizeof(__SIZE_TYPE__) == sizeof(void *);
	r += sizeof(__PTRDIFF_TYPE__) == sizeof(void *);
	r += sizeof(__INT8_TYPE__) == 1 && sizeof(__INT16_TYPE__) == 2;
	r += sizeof(__INT32_TYPE__) == 4 && sizeof(__INT64_TYPE__) == 8;
	r += sizeof(__INTMAX_TYPE__) == 8 && sizeof(__INTPTR_TYPE__) == sizeof(void *);
	r += s > 0;                       /* size_t is unsigned */
	r += b < 0 && b == -1;
	r += (__WCHAR_MAX__ > 0x7fffffff) == (w > 0);   /* the wchar fork agrees with itself (clang carries no __WCHAR_MIN__) */
	r += __CHAR_BIT__ == 8;
	r += __SCHAR_MAX__ == 127 && __SHRT_MAX__ == 32767;
	r += __UINT32_MAX__ == 4294967295U;
	r += __INT64_MAX__ == __INTMAX_MAX__;
	r += __UINT64_C(0xffffffff) * 4 == 0x3fffffffcUL;   /* the C macro widens */
	r += __INT64_C(1) << 40 == 1099511627776LL;
	r += (u.c[0] == 1) == (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__);
	r += 1.0 + e > 1.0 && 1.0 + e / 4 == 1.0;   /* DBL_EPSILON is the real ulp */
	r += __FLT_MANT_DIG__ == 24 && __DBL_MANT_DIG__ == 53;
#ifdef __mooncc__
	r += __LDBL_MANT_DIG__ == __DBL_MANT_DIG__;   /* the deliberate deviation */
#else
	r += 1;                           /* gcc's long double is its own */
#endif
	r += __FLT_RADIX__ == 2 && __DBL_DIG__ == 15;
#ifdef __LP64__
	r += sizeof(long) == 8 && sizeof(void *) == 8;
#else
	r += sizeof(long) == 4;
#endif
	r += __LINE__ == 52;              /* TRUE lines: no -D skew (this IS line 52) */
	return r;
}
