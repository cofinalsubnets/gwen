/* the ctype thirteen, over the WHOLE argument domain.
 *
 * each predicate answers 256 times -- one line of 256 '0'/'1' -- so the
 * differential is exhaustive rather than sampled, and a drift names both the
 * function and the exact byte. that is worth doing here and nowhere else: the
 * domain is small enough to enumerate, which is rare.
 *
 * ⚠ the C locale is the whole contract. glibc without setlocale is in it, and
 * ours has no locales at all, so the two agree by construction from 0..127. the
 * 128..255 half is the interesting one -- a table-driven ctype that indexed with
 * a SIGNED char, or one that let the high half through, would show here.
 *
 * ⚠ tolower/toupper are reported as VALUES, not flags: they must pass every
 * non-letter through unchanged, which a mapping table gets wrong at exactly the
 * bytes nobody tries by hand. */
#include <ctype.h>
#include "say.h"

static void mask(char const *nm, int (*f)(int))
{
	char row[257];
	for (int c = 0; c < 256; c++) row[c] = f(c) ? '1' : '0';
	row[256] = 0;
	say_s(nm, row);
}

static void mapping(char const *nm, int (*f)(int))
{
	/* the fixed points collapse to a single mark; only the MOVED bytes are
	   spelled out, so a drift reads as a short diff rather than 256 numbers */
	for (int c = 0; c < 256; c++) {
		int r = f(c);
		if (r != c) { say_n(nm, c); say_n(nm, r); }
	}
}

int main(void)
{
	mask("isupper", isupper);
	mask("islower", islower);
	mask("isalpha", isalpha);
	mask("isdigit", isdigit);
	mask("isalnum", isalnum);
	mask("isxdigit", isxdigit);
	mask("isspace", isspace);
	mask("isprint", isprint);
	mask("iscntrl", iscntrl);
	mask("ispunct", ispunct);
	mask("isgraph", isgraph);

	mapping("tolower", tolower);
	mapping("toupper", toupper);

	/* the partition laws, checked rather than assumed: every byte is exactly
	   one of print/cntrl or neither (the 128..255 half is neither), and
	   graph = print minus the space.
	   ⚠ every answer is normalized with !! first. a predicate's TRUE is
	   implementation-defined -- glibc hands back the mask bit it tested
	   (_ISalnum is 8), ours hands back 1 -- so comparing two of them with !=
	   finds a difference that is not one. found by this gate, in this file. */
	{ int bad = 0;
	  for (int c = 0; c < 256; c++) {
		if (isprint(c) && iscntrl(c)) bad++;
		if (isgraph(c) && !isprint(c)) bad++;
		if (!!isalnum(c) != (!!isalpha(c) || !!isdigit(c))) bad++;
		if (!!isalpha(c) != (!!isupper(c) || !!islower(c))) bad++;
		if (ispunct(c) && isalnum(c)) bad++;
	  }
	  say_n("partitions.violations", bad); }

	return 0;
}
