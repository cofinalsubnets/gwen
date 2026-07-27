#include <stddef.h>
#include "love.h"

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
 int digit = -1;
 long rc = 0;
 for (const char *x; (x = memchr(digits, tolower(*p), base)); p++)
  digit = x - digits,
  rc = rc * base + digit;
 if (digit == -1) p = NULL, rc = 0;
 if (endptr) *endptr = (char*) (p ? p : s);
 return sign * rc; }
