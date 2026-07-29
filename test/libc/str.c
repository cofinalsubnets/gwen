/* the str* family: nineteen functions, the widest part of the floor and the
 * part most likely to drift, because every one of them has an edge (the n= cases
 * that do NOT terminate, the ones that pad, the ones that answer a pointer into
 * their own argument, and strtok's hidden state).
 *
 * ⚠ comparisons report a SIGN and pointers report an OFFSET -- see say.h. */
#include <string.h>
#include <stdlib.h>
#include "say.h"

static char buf[64], b2[64];

int main(void)
{
	char const *s = "hello, world";
	char const *e = "";

	/* --- strlen --- */
	say_u("strlen", strlen(s));
	say_u("strlen.empty", strlen(e));
	say_u("strlen.one", strlen("x"));

	/* --- strcmp / strncmp: sign only, and the length cut --- */
	say_c("strcmp.eq", strcmp("abc", "abc"));
	say_c("strcmp.lt", strcmp("abc", "abd"));
	say_c("strcmp.gt", strcmp("abd", "abc"));
	say_c("strcmp.prefix", strcmp("ab", "abc"));
	say_c("strcmp.empty", strcmp("", "a"));
	say_c("strcmp.both", strcmp("", ""));
	/* ⚠ char compares UNSIGNED here: "\x80" is ABOVE "\x7f" */
	say_c("strcmp.high", strcmp("\x80", "\x7f"));
	say_c("strncmp.cut", strncmp("abcXX", "abcYY", 3));
	say_c("strncmp.at", strncmp("abcX", "abcY", 4));
	say_c("strncmp.0", strncmp("a", "b", 0));
	say_c("strncmp.over", strncmp("ab", "ab", 99));   /* stops at the NUL */

	/* --- strcasecmp / strncasecmp / strcoll --- */
	say_c("strcasecmp.eq", strcasecmp("AbC", "aBc"));
	say_c("strcasecmp.lt", strcasecmp("abc", "ABD"));
	say_c("strcasecmp.digit", strcasecmp("a1", "A1"));
	say_c("strncasecmp.cut", strncasecmp("ABCxx", "abcyy", 3));
	say_c("strcoll.eq", strcoll("abc", "abc"));       /* the C locale IS strcmp */
	say_c("strcoll.lt", strcoll("abc", "abd"));

	/* --- strcpy / strncpy.
	   ⚠ strncpy is the trap: it PADS with NULs to n and does NOT terminate
	   when the source fills n exactly. both halves are asserted. --- */
	memset(buf, '.', sizeof buf);
	strcpy(buf, "abc");
	say_b("strcpy", buf, 8);
	memset(buf, '.', sizeof buf);
	strcpy(buf, "");
	say_b("strcpy.empty", buf, 4);
	memset(buf, '.', sizeof buf);
	strncpy(buf, "abc", 8);
	say_b("strncpy.pad", buf, 10);                    /* pads to 8 with NULs */
	memset(buf, '.', sizeof buf);
	strncpy(buf, "abcdefgh", 4);
	say_b("strncpy.cut", buf, 6);                     /* NO terminator */
	memset(buf, '.', sizeof buf);
	strncpy(buf, "abcd", 4);
	say_b("strncpy.exact", buf, 6);                   /* still no terminator */
	memset(buf, '.', sizeof buf);
	strncpy(buf, "abc", 0);
	say_b("strncpy.0", buf, 4);

	/* --- strcat / strncat.
	   ⚠ strncat is NOT strncpy's twin: n bounds the SOURCE, and it always
	   terminates. --- */
	strcpy(buf, "abc");
	strcat(buf, "def");
	say_s("strcat", buf);
	strcpy(buf, "abc");
	strcat(buf, "");
	say_s("strcat.empty", buf);
	strcpy(buf, "abc");
	strncat(buf, "defgh", 3);
	say_s("strncat.cut", buf);
	strcpy(buf, "abc");
	strncat(buf, "de", 9);
	say_s("strncat.over", buf);
	strcpy(buf, "abc");
	strncat(buf, "de", 0);
	say_s("strncat.0", buf);

	/* --- strchr / strrchr: offsets. the NUL is findable, by law. --- */
	say_p("strchr.first", s, strchr(s, 'l'));
	say_p("strchr.miss", s, strchr(s, 'q'));
	say_p("strchr.nul", s, strchr(s, 0));
	say_p("strrchr.last", s, strrchr(s, 'l'));
	say_p("strrchr.miss", s, strrchr(s, 'q'));
	say_p("strrchr.nul", s, strrchr(s, 0));
	say_p("strchr.empty", e, strchr(e, 'a'));

	/* --- strstr / strpbrk --- */
	say_p("strstr.hit", s, strstr(s, "wor"));
	say_p("strstr.head", s, strstr(s, "hell"));
	say_p("strstr.miss", s, strstr(s, "worm"));
	say_p("strstr.empty", s, strstr(s, ""));          /* the empty needle hits at 0 */
	say_p("strstr.self", s, strstr(s, s));
	say_p("strstr.partial", s, strstr(s, "lox"));     /* a false start must back up */
	say_p("strpbrk.hit", s, strpbrk(s, "xyzw"));
	say_p("strpbrk.miss", s, strpbrk(s, "QZ"));
	say_p("strpbrk.empty", s, strpbrk(s, ""));

	/* --- strspn / strcspn --- */
	say_u("strspn.some", strspn("aabbcc", "ab"));
	say_u("strspn.none", strspn("xaabb", "ab"));
	say_u("strspn.all", strspn("abab", "ab"));
	say_u("strspn.emptyset", strspn("abc", ""));
	say_u("strcspn.some", strcspn("abcXd", "XY"));
	say_u("strcspn.none", strcspn("Xabc", "XY"));
	say_u("strcspn.all", strcspn("abc", "XY"));
	say_u("strcspn.emptyset", strcspn("abc", ""));

	/* --- strtok: the hidden state, the run of separators, the tail --- */
	strcpy(b2, "  one,,two  three,");
	for (char *t = strtok(b2, " ,"); t; t = strtok(0, " ,")) say_s("strtok", t);
	strcpy(b2, "solo");
	say_s("strtok.solo", strtok(b2, ","));
	say_s("strtok.done", strtok(0, ","));             /* exhausted -> null */
	strcpy(b2, ",,,");
	say_s("strtok.allsep", strtok(b2, ","));          /* separators only -> null */

	/* --- strdup --- */
	{ char *d = strdup("copy me");
	  say_s("strdup", d);
	  say_c("strdup.same", strcmp(d, "copy me"));
	  free(d);
	  d = strdup("");
	  say_s("strdup.empty", d);
	  free(d); }

	/* --- strerror: ⚠ the TEXT is not compared. ours is the canonical POSIX
	   wording (m4's check suite string-compares it) and glibc's mostly agrees,
	   but that agreement is not a law worth gating -- what is gated is that
	   every errno in the classic range answers something non-empty. --- */
	{ int ok = 1;
	  for (int i = 1; i <= 34; i++) { char const *m = strerror(i); if (!m || !*m) ok = 0; }
	  say_n("strerror.nonempty", ok); }

	return 0;
}
