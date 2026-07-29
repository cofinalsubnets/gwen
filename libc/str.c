#include <stddef.h>
#include "love.h"

// -nostdinc, so no limits.h: the two bounds by construction.
#define l_max ((long) (~0UL >> 1))
#define l_min (-l_max - 1)

size_t strlen(char const *c) {
  size_t len = 0;
  while (*c++) len++;
  return len; }

// the math floor's exact reader (crew/moon/lib/math/am.c, linked everywhere):
// correctly rounded, so read(show x) = x holds off-host too. The old naive
// accumulator here parsed "0.3" one ulp off -- masked until the printer went
// shortest-roundtrip, then loud.
double am_strtod(char const*, char**);
double strtod(char const *restrict s, char **restrict end) {
 return am_strtod(s, (char**) end); }

int isspace(int), tolower(int);
void *memchr(void const*, int, size_t);
static char const digits[] = ai_digits;

long int strtol(const char *s, char **endptr, int base) {
 char const *p = s;
 int sign = 1;
 while (isspace(*p)) p++;
 if (*p == '-') sign = -1, p++;
 else if (*p == '+') p++;
 if (*p == '0') {
  ++p;
  if ((base == 0 || base == 16) && (*p == 'x' || *p == 'X')) {
   base = 16;
   ++p;
   if (!memchr(digits, tolower(*p), base)) p -= 2; }
  else if (base == 0) base = 8, --p;
  else --p; }
 else if (!base) base = 10;
 if ( base < 2 || base > 36 ) return 0;
 // OVERFLOW SATURATES -- it does not wrap, which is what the standard says and
 // what every strtol we sit beside does. crew/moon/lib/nolibc.c's twin saturates
 // the same way, and it MUST: when the two disagreed, one source text read as two
 // different numbers depending on which libc the binary carried. the accumulation
 // runs UNSIGNED so the negative bound's magnitude is reachable without signed
 // overflow on the way. (freestanding has no errno -- the limit IS the signal.)
 unsigned long lim = sign < 0 ? (unsigned long) l_max + 1UL : (unsigned long) l_max,
               cut = lim / (unsigned long) base, cutd = lim % (unsigned long) base, rc = 0;
 int digit = -1, over = 0;
 for (const char *x; (x = memchr(digits, tolower(*p), base)); p++) {
  digit = (int) (x - digits);
  if (over || rc > cut || (rc == cut && (unsigned long) digit > cutd)) over = 1;
  else rc = rc * (unsigned long) base + (unsigned long) digit; }
 if (digit == -1) p = NULL, rc = 0, over = 0;
 if (endptr) *endptr = (char*) (p ? p : s);
 if (over) return sign < 0 ? l_min : l_max;
 return (long) (sign < 0 ? 0UL - rc : rc); }
