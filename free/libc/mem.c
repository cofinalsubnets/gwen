#include <stddef.h>
#include <stdint.h>

void *memchr(void const *s, int c, size_t n) {
  for (unsigned char const *p = s; n-- ; p++)
    if (*p == (int) c) return (void*) p;
  return NULL; }

int memcmp(void const *s1, void const *s2, size_t n) {
  const uint8_t *p1 = s1, *p2 = s2;
  for (size_t i = 0; i < n; i++)
    if (p1[i] != p2[i]) return p1[i] < p2[i] ? -1 : 1;
  return 0; }

void *memcpy(void *restrict dest, void const *restrict src, size_t n) {
  uint8_t *restrict pdest = dest;
  uint8_t const *restrict psrc = src;
  for (size_t i = 0; i < n; i++) pdest[i] = psrc[i];
  return dest; }

void *memmove(void *dest, void const *src, size_t n) {
  uint8_t *pdest = dest;
  const uint8_t *psrc = src;
  if (src > dest)
    for (size_t i = 0; i < n; i++) pdest[i] = psrc[i];
  else if (src < dest)
    for (size_t i = n; i > 0; i--) pdest[i-1] = psrc[i-1];
  return dest; }

void *memset(void *s, int c, size_t n) {
  uint8_t *p = (uint8_t*) s;
  for (size_t i = 0; i < n; i++) p[i] = (uint8_t) c;
  return s; }
