#include "g.h"
#include <stdint.h>
#include <stddef.h>
#include <limits.h>

#define unsign(x) ((unsigned char)(x))
// this file includes functions adapted from the original limine c template
// and also from github.com/DevSolar/pdclib
//
int strncmp(const char * s1, const char * s2, size_t n) {
  while (n && *s1 && *s1 == *s2) s1++, s2++, n--;
  if (!n) return 0;
  return unsign(*s1) - unsign(*s2); }
  //return ( *(unsigned char*)s1 - *(unsigned char *)s2 ); }

size_t strlen(const char *s) {
  size_t rc = 0;
  while (s[rc]) rc++;
  return rc; }

void *memcpy(void *restrict dest, const void *restrict src, size_t n) {
  uint8_t *restrict pdest = dest;
  const uint8_t *restrict psrc = src;
  for (size_t i = 0; i < n; i++) pdest[i] = psrc[i];
  return dest; }

void *memset(void *s, int c, size_t n) {
  uint8_t *p = (uint8_t*) s;
  for (size_t i = 0; i < n; i++) p[i] = (uint8_t) c;
  return s; }

void *memmove(void *dest, const void *src, size_t n) {
  uint8_t *pdest = dest;
  const uint8_t *psrc = src;
  if (src > dest)
    for (size_t i = 0; i < n; i++) pdest[i] = psrc[i];
  else if (src < dest)
    for (size_t i = n; i > 0; i--) pdest[i-1] = psrc[i-1];
  return dest; }

int memcmp(const void *s1, const void *s2, size_t n) {
  const uint8_t *p1 = s1, *p2 = s2;
  for (size_t i = 0; i < n; i++)
    if (p1[i] != p2[i]) return p1[i] < p2[i] ? -1 : 1;
  return 0; }

void *memchr(const void *s, int c, size_t n) {
  for (const unsigned char *p = s; n-- ; p++)
    if (*p == (int) c) return (void*) p;
  return NULL; }

static char const
  digits[] = g_digits,
  spaces[] = " \n\t\v\r\f";

#define LEN(_) (sizeof(_)/sizeof(*_))
int isspace( int c ) {
  for (long unsigned int i = 0; i < LEN(spaces); i++)
    if (c == spaces[i]) return 1;
  return 0; }

int tolower(int c) { return 64 < c && c < 91 ? c + 32 : c; }

const char *_PDCLIB_strtox_prelim(const char *p, char *sign, int *base) {
    /* skipping leading whitespace */
    while (isspace((int) *p)) p++;
    /* determining / skipping sign */
    if ( *p != '+' && *p != '-' ) *sign = '+';
    else *sign = *( p++ );
    /* determining base */
    if ( *p == '0' ) {
      ++p;
      if ((*base == 0 || *base == 16) && (*p == 'x' || *p == 'X')) {
          *base = 16;
          ++p;
          if (!memchr(digits, tolower((int) *p), *base)) p -= 2; }
      else if (*base == 0) *base = 8, --p;
      else --p; }
    else if (!*base) *base = 10;

    return ((*base >= 2) && (*base <= 36)) ? p : NULL; }
int errno;
#define ERANGE 34

uintmax_t _PDCLIB_strtox_main(const char ** p, unsigned int base, uintmax_t error, uintmax_t limval, int limdigit, char * sign ) {
    uintmax_t rc = 0;
    int digit = -1;
    const char * x;
    while ((x = (const char*) memchr(digits, tolower((int) **p), base))) {
      digit = x - digits;
      if (( rc < limval) || ((rc == limval) && (digit <= limdigit)))
        rc = rc * base + ( unsigned )digit,
        ++(*p);
      else {
        errno = ERANGE;
        while (memchr(digits, tolower((int)**p), base) != NULL ) ++(*p);
        *sign = '+';
        return error; } }
    if (digit == -1) return *p = NULL, 0;
    return rc; }


long int strtol(const char *s, char **endptr, int base) {
    long int rc;
    char sign = '+';
    const char * p = _PDCLIB_strtox_prelim( s, &sign, &base );

    if ( base < 2 || base > 36 ) return 0;

    if ( sign == '+' )
    {
        rc = ( long int )_PDCLIB_strtox_main( &p, ( unsigned )base, ( uintmax_t )LONG_MAX, ( uintmax_t )( LONG_MAX / base ), ( int )( LONG_MAX % base ), &sign );
    }
    else
    {
        rc = ( long int )_PDCLIB_strtox_main( &p, ( unsigned )base, ( uintmax_t )LONG_MIN, ( uintmax_t )( LONG_MIN / -base ), ( int )( -( LONG_MIN % base ) ), &sign );
    }

    if (endptr) *endptr = (char*) (p ? p : s);

    return sign == '+' ? rc : -rc; }

static uint64_t rol64(uint64_t x, int k) {
  return (x << k) | (x >> (64 - k)); }

static struct xoshiro256pp_state { uint64_t s[4]; } g_prng_state;
static uint64_t xoshiro256pp(struct xoshiro256pp_state *state) {
  uint64_t *s = state->s;
  uint64_t const result = rol64(s[0] + s[3], 23) + s[0];
  uint64_t const t = s[1] << 17;
  s[2] ^= s[0];
  s[3] ^= s[1];
  s[1] ^= s[2];
  s[0] ^= s[3];
  s[2] ^= t;
  s[3] = rol64(s[3], 45);
  return result; }

int rand(void) {
  return (int) xoshiro256pp(&g_prng_state); }
void srand(unsigned int seed) {
  uint64_t s = seed;
  s += s << 32;
  g_prng_state.s[0] = g_prng_state.s[1] = g_prng_state.s[2] = g_prng_state.s[3] = s; }
