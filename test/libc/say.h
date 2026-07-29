/* test/libc/say.h -- the reporting side of the libc differential.
 *
 * ⚠ DELIBERATELY INDEPENDENT OF THE LIBC UNDER TEST. digits are turned by hand
 * and everything leaves through putchar, so nothing here touches the formatter.
 * report with printf instead and a drifted %d corrupts the FRAME of all six
 * programs at once, which reads as "everything is broken" when one thing is.
 * fmt.c tests the formatter by putting its output in the PAYLOAD.
 *
 * ⚠ AND THE COMPARISONS REPORT A SIGN, NEVER A VALUE. the standard fixes only
 * the sign of memcmp/strcmp/strncmp/strcasecmp/strcoll -- glibc hands back the
 * byte difference, ours hands back -1/0/1, and both are right. a differential
 * that compared the number would fail on a difference that is not one.
 *
 * ⚠ likewise a POINTER result is reported as an OFFSET from its base (-1 for
 * null): the addresses differ between two builds of the same program, the
 * offsets do not.
 */
#ifndef LIBC_SAY_H
#define LIBC_SAY_H
#include <stdio.h>
#include <stddef.h>

static void s_put(char const *s) { while (*s) putchar(*s++); }

static void s_unum(unsigned long m) {
  char b[24];
  int i = 24;
  b[--i] = 0;
  do b[--i] = (char) ('0' + (int) (m % 10)); while (m /= 10);
  s_put(b + i); }

static void s_key(char const *k) { s_put(k); s_put(": "); }

static void say_n(char const *k, long v) {
  s_key(k);
  if (v < 0) { putchar('-'); s_unum(0UL - (unsigned long) v); }
  else s_unum((unsigned long) v);
  putchar('\n'); }

static void say_u(char const *k, unsigned long v) { s_key(k); s_unum(v); putchar('\n'); }

/* a string, bracketed so a trailing space or an empty answer is visible */
static void say_s(char const *k, char const *v) {
  s_key(k);
  if (!v) { s_put("(null)\n"); return; }
  putchar('[');
  s_put(v);
  s_put("]\n"); }

/* n bytes, bracketed, with the unprintables spelled \xHH -- memset/memcpy
 * answers are byte strings, not C strings, and may hold a 0 */
static void say_b(char const *k, void const *p, size_t n) {
  unsigned char const *b = p;
  char const *hx = "0123456789abcdef";
  s_key(k);
  putchar('[');
  for (size_t i = 0; i < n; i++) {
    if (b[i] >= 32 && b[i] < 127) putchar((int) b[i]);
    else { s_put("\\x"); putchar(hx[b[i] >> 4]); putchar(hx[b[i] & 15]); } }
  s_put("]\n"); }

/* the SIGN of a comparison, which is all the standard promises */
static void say_c(char const *k, int r) { say_n(k, r < 0 ? -1 : r > 0 ? 1 : 0); }

/* a pointer INTO a known base, as an offset; null is -1 */
static void say_p(char const *k, void const *base, void const *p) {
  say_n(k, p ? (long) ((char const *) p - (char const *) base) : -1L); }

#endif
