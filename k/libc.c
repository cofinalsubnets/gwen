#include "g.h"
#include <stdint.h>
#include <stddef.h>
#include <limits.h>
int errno;
#define ERANGE 34

#define unsign(x) ((unsigned char)(x))

void *memcpy(void *restrict dest, void const *restrict src, size_t n) {
  uint8_t *restrict pdest = dest;
  uint8_t const *restrict psrc = src;
  for (size_t i = 0; i < n; i++) pdest[i] = psrc[i];
  return dest; }

void *memset(void *s, int c, size_t n) {
  uint8_t *p = (uint8_t*) s;
  for (size_t i = 0; i < n; i++) p[i] = (uint8_t) c;
  return s; }

void *memmove(void *dest, void const *src, size_t n) {
  uint8_t *pdest = dest;
  const uint8_t *psrc = src;
  if (src > dest)
    for (size_t i = 0; i < n; i++) pdest[i] = psrc[i];
  else if (src < dest)
    for (size_t i = n; i > 0; i--) pdest[i-1] = psrc[i-1];
  return dest; }

int memcmp(void const *s1, void const *s2, size_t n) {
  const uint8_t *p1 = s1, *p2 = s2;
  for (size_t i = 0; i < n; i++)
    if (p1[i] != p2[i]) return p1[i] < p2[i] ? -1 : 1;
  return 0; }

void *memchr(void const *s, int c, size_t n) {
  for (unsigned char const *p = s; n-- ; p++)
    if (*p == (int) c) return (void*) p;
  return NULL; }

static char const
  digits[] = g_digits,
  spaces[] = " \n\t\v\r\f";

int isspace(int c) { return !!memchr(spaces, c, 6); }
int tolower(int c) { return c + ('A' <= c && c <= 'Z' ? 'a' - 'A' : 0); }
int toupper(int c) { return c + ('a' <= c && c <= 'z' ? 'A' - 'a' : 0); }

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
