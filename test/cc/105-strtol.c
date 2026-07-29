/* strtol / strtoul ON OVERFLOW: they SATURATE, they do not wrap.
 *
 * ⚠ DELIBERATELY NOT FREESTANDING, unlike every other program in this battery --
 * here the libc IS the subject. gcc links glibc, mooncc links
 * crew/moon/lib/nolibc.c, and the harness compares exit codes, so this is a
 * differential over OUR strtol rather than over codegen alone. it earns the
 * exception: nolibc's accumulator used to wrap, love.c's reader leaned on that
 * wrap to carry hex kernel addresses, and the same source text read as two
 * different numbers depending on which libc the binary happened to carry. the
 * reader has its own integer readers now (ai_big_read_dec/_hex/_oct) and never
 * calls strtol, which is exactly why the saturation needs a gate of its own.
 *
 * libc/str.c, the kernel's freestanding floor, no longer carries strtol at all.
 */
#include <stdlib.h>
#include <limits.h>
#include <errno.h>

int main(void)
{
	char const *s;
	char *e;
	int r = 0;

	/* decimal past the ceiling saturates -- and endptr still spans every
	   digit, and ERANGE says why. an overflow is REPORTED, not disguised. */
	errno = 0;
	r += strtol("99999999999999999999999999", &e, 10) == LONG_MAX;
	r += *e == 0;
	r += errno == ERANGE;

	/* ..and the floor is LONG_MIN, not -LONG_MAX: the magnitude is one past
	   what a signed accumulator could have held on the way. */
	r += strtol("-99999999999999999999999999", &e, 10) == LONG_MIN;

	/* the kernel-address shape that started all this, both by base and by 0 */
	r += strtol("0xffffffff80200000", &e, 16) == LONG_MAX;
	r += strtol("0xffffffff80200000", &e, 0) == LONG_MAX;
	r += strtol("01777777777777777777777", &e, 0) == LONG_MAX;

	/* the unsigned twin saturates at its own ceiling.. */
	r += strtoul("99999999999999999999999999", &e, 10) == ULONG_MAX;
	/* ..but a leading minus still NEGATES modulo the width, which is the one
	   wrap the standard does ask for. */
	r += strtoul("-1", &e, 10) == ULONG_MAX;

	/* and everything that FITS is untouched, in all three bases, including
	   both exact bounds -- the digits that land ON the limit must not trip
	   the overflow test. */
	r += strtol("9223372036854775807", &e, 10) == LONG_MAX;
	r += strtol("-9223372036854775808", &e, 10) == LONG_MIN;
	r += strtol("0x7fffffffffffffff", &e, 0) == LONG_MAX;
	r += strtol("0755", &e, 0) == 493;
	r += strtol("-42", &e, 10) == -42;
	r += strtol("2147483647", &e, 10) == 2147483647L;

	/* no digits at all: zero, and endptr back at the start */
	s = "abc";
	r += strtol(s, &e, 10) == 0;
	r += (char const *) e == s;

	return r;
}
