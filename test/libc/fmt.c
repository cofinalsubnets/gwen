/* the formatter: snprintf / sprintf / vsnprintf over one body (__femit's twin),
 * with %s %c %d %u %x %o, the l/z widths, and the %[-0]WIDTH.PREC flags.
 *
 * ⚠ this is the one program whose subject appears in the PAYLOAD rather than the
 * frame: say.h turns its own digits by hand, so a drifted %d shows up here as a
 * wrong bracketed string and NOT as garbage across the other five programs. if
 * every family diffs at once, read this one first.
 *
 * ⚠ snprintf's RETURN is the length it WOULD have written, not what it did --
 * the difference is the whole reason the function exists, and a truncating
 * implementation that returns the truncated length is the classic bug. */
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include "say.h"

static char b[64];

/* one case: run it, report the bytes AND the return */
static void F(char const *nm, int r) { say_s(nm, b); say_n(nm, r); }

static int vtest(char *p, size_t n, char const *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	int r = vsnprintf(p, n, fmt, ap);
	va_end(ap);
	return r;
}

int main(void)
{
	/* --- the plain conversions --- */
	F("s", snprintf(b, sizeof b, "[%s]", "abc"));
	F("s.empty", snprintf(b, sizeof b, "[%s]", ""));
	F("c", snprintf(b, sizeof b, "[%c]", 'q'));
	F("d", snprintf(b, sizeof b, "%d", 42));
	F("d.neg", snprintf(b, sizeof b, "%d", -42));
	F("d.zero", snprintf(b, sizeof b, "%d", 0));
	F("d.min", snprintf(b, sizeof b, "%d", -2147483647 - 1));
	F("u", snprintf(b, sizeof b, "%u", 4294967295u));
	F("x", snprintf(b, sizeof b, "%x", 48879));
	F("x.zero", snprintf(b, sizeof b, "%x", 0));
	F("o", snprintf(b, sizeof b, "%o", 493));
	F("pct", snprintf(b, sizeof b, "100%%"));

	/* --- the l and z widths --- */
	F("ld", snprintf(b, sizeof b, "%ld", 9223372036854775807L));
	F("ld.min", snprintf(b, sizeof b, "%ld", -9223372036854775807L - 1));
	F("lu", snprintf(b, sizeof b, "%lu", 18446744073709551615UL));
	F("lx", snprintf(b, sizeof b, "%lx", 11400714819323198485UL));
	F("zu", snprintf(b, sizeof b, "%zu", (size_t) 1234));

	/* --- width, the zero flag and the left flag. ⚠ zeros hug the digits and
	   sit INSIDE the sign; spaces sit outside it. --- */
	F("w", snprintf(b, sizeof b, "[%6d]", 42));
	F("w.left", snprintf(b, sizeof b, "[%-6d]", 42));
	F("w.zero", snprintf(b, sizeof b, "[%06d]", 42));
	F("w.zeroneg", snprintf(b, sizeof b, "[%06d]", -42));
	F("w.spaceneg", snprintf(b, sizeof b, "[%6d]", -42));
	F("w.leftneg", snprintf(b, sizeof b, "[%-6d]", -42));
	F("w.short", snprintf(b, sizeof b, "[%2d]", 12345));   /* width never truncates */
	F("w.str", snprintf(b, sizeof b, "[%8s]", "ab"));
	F("w.strleft", snprintf(b, sizeof b, "[%-8s]", "ab"));
	F("w.hex", snprintf(b, sizeof b, "[%08x]", 48879));

	/* --- precision --- */
	F("p.str", snprintf(b, sizeof b, "[%.2s]", "abcdef"));
	F("p.strover", snprintf(b, sizeof b, "[%.9s]", "abc"));
	F("p.strzero", snprintf(b, sizeof b, "[%.0s]", "abc"));
	F("p.both", snprintf(b, sizeof b, "[%8.2s]", "abcdef"));
	/* on an integer the precision is a MINIMUM digit count, it retires the 0
	   flag, and .0 of a zero is the empty field (C99 7.19.6.1p6). */
	F("p.d", snprintf(b, sizeof b, "[%.3d]", 33));
	F("p.dover", snprintf(b, sizeof b, "[%.2d]", 12345));
	F("p.dneg", snprintf(b, sizeof b, "[%.4d]", -7));
	F("p.dzero", snprintf(b, sizeof b, "[%.0d]", 0));
	F("p.dzero1", snprintf(b, sizeof b, "[%.1d]", 0));
	F("p.dwidth", snprintf(b, sizeof b, "[%8.3d]", 33));
	F("p.dwidthleft", snprintf(b, sizeof b, "[%-8.3d]", 33));
	F("p.dzeroflag", snprintf(b, sizeof b, "[%08.3d]", 33));
	F("p.x", snprintf(b, sizeof b, "[%.4x]", 255));
	F("p.xalt", snprintf(b, sizeof b, "[%#.4x]", 255));
	F("p.u", snprintf(b, sizeof b, "[%.5u]", 42));
	F("p.o", snprintf(b, sizeof b, "[%.4o]", 8));

	/* --- TRUNCATION: the bytes stop, the return does not --- */
	memset(b, '#', sizeof b);
	F("cut.5", snprintf(b, 5, "%s", "abcdefgh"));
	memset(b, '#', sizeof b);
	F("cut.1", snprintf(b, 1, "%s", "abcdefgh"));          /* just the NUL */
	memset(b, '#', sizeof b);
	{ int r = snprintf(b, 0, "%s", "abcdefgh");             /* touches NOTHING */
	  say_b("cut.0", b, 4);
	  say_n("cut.0", r); }
	memset(b, '#', sizeof b);
	F("cut.exact", snprintf(b, 4, "abc"));                 /* fits with its NUL */
	memset(b, '#', sizeof b);
	F("cut.byone", snprintf(b, 4, "abcd"));

	/* --- several conversions in one run, and the empty format --- */
	F("mix", snprintf(b, sizeof b, "%s=%d/%x|%c", "k", -7, 255, '!'));
	F("mix.adjacent", snprintf(b, sizeof b, "%d%d%d", 1, 2, 3));
	F("empty", snprintf(b, sizeof b, ""));
	F("noconv", snprintf(b, sizeof b, "plain text"));

	/* --- sprintf and vsnprintf ride the same body --- */
	F("sprintf", sprintf(b, "%s-%d", "v", 9));
	F("vsnprintf", vtest(b, sizeof b, "%s/%d", "v", 9));
	memset(b, '#', sizeof b);
	F("vsnprintf.cut", vtest(b, 4, "%s/%d", "vvv", 9));

	return 0;
}
