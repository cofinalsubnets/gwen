/* the number parsers -- the part of the floor that has a RULE in it rather than
 * a loop, and therefore the part that actually drifted.
 *
 * nolibc's strtol used to WRAP on overflow where glibc saturates, so one source
 * text read as two different numbers depending on which libc the binary carried;
 * love.c's reader leaned on the wrap to carry hex kernel addresses
 * (port/inle/klink.l). the reader now reads all three integer bases itself and
 * never calls strtol, which is exactly why this needs a gate of its own: nothing
 * else in the tree observes the difference any more.
 *
 * ⚠ endptr is reported as an OFFSET (say.h) -- the addresses differ, the
 * offsets do not. */
#include <stdlib.h>
#include <limits.h>
#include <errno.h>
#include "say.h"

static void L(char const *nm, char const *s, int base)
{
	char *e;
	errno = 0;
	long v = strtol(s, &e, base);
	say_n(nm, v);
	say_n(nm, (long) (e - s));
	say_n(nm, errno == ERANGE ? 1 : 0);
}

static void U(char const *nm, char const *s, int base)
{
	char *e;
	errno = 0;
	unsigned long v = strtoul(s, &e, base);
	say_u(nm, v);
	say_n(nm, (long) (e - s));
	say_n(nm, errno == ERANGE ? 1 : 0);
}

int main(void)
{
	/* --- the overflow rule: SATURATE, do not wrap, and say ERANGE --- */
	L("over.dec", "99999999999999999999999999", 10);
	L("over.neg", "-99999999999999999999999999", 10);
	L("over.hex", "0xffffffff80200000", 16);
	L("over.hex0", "0xffffffff80200000", 0);
	L("over.oct", "01777777777777777777777", 0);
	U("over.udec", "99999999999999999999999999", 10);
	U("over.uhex", "0xffffffffffffffffff", 0);

	/* --- the exact bounds: the digits that land ON the limit must NOT trip
	   the overflow test, in either direction --- */
	L("bound.max", "9223372036854775807", 10);
	L("bound.min", "-9223372036854775808", 10);
	L("bound.maxhex", "0x7fffffffffffffff", 0);
	L("bound.overmax", "9223372036854775808", 10);
	L("bound.undermin", "-9223372036854775809", 10);
	U("bound.umax", "18446744073709551615", 10);
	U("bound.overumax", "18446744073709551616", 10);

	/* --- ..and the one wrap the standard DOES ask for: a leading minus
	   negates the magnitude modulo the width --- */
	U("neg.one", "-1", 10);
	U("neg.two", "-2", 10);

	/* --- the ordinary readings, every base and prefix --- */
	L("plain.dec", "12345", 10);
	L("plain.neg", "-42", 10);
	L("plain.pos", "+42", 10);
	L("plain.zero", "0", 10);
	L("plain.hex", "0xff", 16);
	L("plain.hexbare", "ff", 16);
	L("plain.hexup", "0XFF", 0);
	L("plain.oct", "0755", 0);
	L("plain.oct8", "0755", 8);
	L("plain.autodec", "755", 0);
	L("plain.b36", "zz", 36);
	L("plain.b2", "1011", 2);

	/* --- the edges of the SCAN: leading space, no digits, a partial parse,
	   a prefix with nothing after it, a base that forbids the digits --- */
	L("scan.space", "   \t\n 17rest", 10);
	L("scan.nodigits", "abc", 10);
	L("scan.empty", "", 10);
	L("scan.signonly", "-", 10);
	L("scan.partial", "12ab", 10);
	L("scan.0x_nodigit", "0x", 16);
	L("scan.0x_nodigit0", "0xg", 0);
	L("scan.oct9", "0778", 8);          /* stops at the 8 */
	L("scan.plusminus", "+-5", 10);
	L("scan.dec_x", "12x", 0);
	/* ⚠ no invalid-base case here. base must be 0 or 2..36 and the standard
	   says nothing about the rest: BOTH libraries return 0 and leave endptr
	   untouched, so a differential on it compares two uninitialized pointers
	   and fails at random. (it did, on this gate's first run.) */

	/* --- the wider twins share the body; check they answer alike --- */
	{ char *e;
	  say_u("strtoull.over", strtoull("99999999999999999999999999", &e, 10));
	  say_u("strtoull.plain", strtoull("4294967296", &e, 10)); }

	/* --- atoi / atol: strtol base 10 without the reporting --- */
	say_n("atoi", atoi("  -1234xyz"));
	say_n("atoi.none", atoi("zz"));
	say_n("atol", atol("9223372036854775807"));
	say_n("atol.neg", atol("-7"));

	/* --- abs / labs, including the value with no positive twin --- */
	say_n("abs", abs(-5));
	say_n("abs.pos", abs(5));
	say_n("abs.zero", abs(0));
	say_n("labs", labs(-1234567890123L));

	/* --- strtod: ours is am_strtod, correctly rounded, and the reader's float
	   lane rides it on every target. compared through its BITS, because a
	   decimal reprint would hide a one-ulp difference -- which is the whole
	   thing am_strtod exists to get right. --- */
	{ char *e;
	  double d;
	  char const *cases[] = { "0.3", "1.5", "-2.25", "1e10", "1e-10", "3.14159265358979",
	                          "0.1", "123456789.123456789", "5e-324", "1.7976931348623157e308",
	                          "0", "-0.0", "  42.5xyz", "nope", 0 };
	  for (int i = 0; cases[i]; i++) {
		union { double d; unsigned long u; } bits;
		errno = 0;
		d = strtod(cases[i], &e);
		bits.d = d;
		say_u("strtod", bits.u);
		say_n("strtod", (long) (e - cases[i]));
	  }
	  say_n("atof", (long) (atof("2.5") * 4.0)); }

	return 0;
}
