// ustar.c -- see ustar.h. no allocation and no io: every one of these reads bytes the
// caller already has, so the hosted lane and the freestanding one take the same file.
#include "ustar.h"
#include <string.h>

bool ai_gz_body(unsigned char const *z, uintptr_t zn, uintptr_t *off, uintptr_t *isize) {
  if (zn < 18 || z[0] != 0x1f || z[1] != 0x8b || z[2] != 8) return false;
  uintptr_t o = 10;
  unsigned f = z[3];
  if (f & 4) o += 2 + (uintptr_t) z[o] + ((uintptr_t) z[o + 1] << 8);   // FEXTRA
  if (f & 8) { while (o < zn && z[o]) o++; o++; }                       // FNAME
  if (f & 16) { while (o < zn && z[o]) o++; o++; }                      // FCOMMENT
  if (f & 2) o += 2;                                                    // FHCRC
  if (o + 8 >= zn) return false;
  *off = o;
  *isize = (uintptr_t) z[zn - 4] | (uintptr_t) z[zn - 3] << 8
         | (uintptr_t) z[zn - 2] << 16 | (uintptr_t) z[zn - 1] << 24;
  return true; }

uintptr_t ai_ustar_octal(unsigned char const *p, int n) {
  uintptr_t v = 0;
  for (int i = 0; i < n && p[i] >= '0' && p[i] <= '7'; i++) v = v * 8 + (uintptr_t)(p[i] - '0');
  return v; }

bool ai_ustar_member(unsigned char const *h) {
  return (h[156] == '0' || h[156] == 0 || h[156] == '2') && !memcmp(h + 257, "ustar", 5); }

uintptr_t ai_ustar_name(unsigned char const *h, char *out, uintptr_t cap) {
  char nm[256];
  uintptr_t ln = 0;
  for (int i = 345; i < 500 && h[i] && ln < 254; i++) nm[ln++] = (char) h[i];
  if (ln) nm[ln++] = '/';
  for (int i = 0; i < 100 && h[i] && ln < 255; i++) nm[ln++] = (char) h[i];
  uintptr_t cut = 0;                                   // past TOP/
  while (cut < ln && nm[cut] != '/') cut++;
  cut = cut < ln ? cut + 1 : 0;
  uintptr_t n = ln - cut;
  if (n > cap) n = cap;
  memcpy(out, nm + cut, n);
  return n; }

uintptr_t ai_ustar_link(unsigned char const *h, char *out, uintptr_t cap) {
  uintptr_t n = 0;
  while (n < 100 && n < cap && h[157 + n]) { out[n] = (char) h[157 + n]; n++; }
  return n; }

uintptr_t ai_lnk_canon(char const *at, char const *ln, char *out, uintptr_t cap) {
  uintptr_t n = 0;
  if (ln[0] != '/') {
    uintptr_t d = strlen(at);
    while (d && at[d - 1] != '/') d--;
    if (d && d <= cap) memcpy(out, at, n = d - 1); }        // dirname, no trailing slash
  for (uintptr_t i = 0; ln[i];) {
    while (ln[i] == '/') i++;
    uintptr_t j = i;
    while (ln[j] && ln[j] != '/') j++;
    uintptr_t k = j - i;
    if (!k) break;
    if (k == 1 && ln[i] == '.') { i = j; continue; }
    if (k == 2 && ln[i] == '.' && ln[i + 1] == '.') {
      while (n && out[n - 1] != '/') n--;
      if (n) n--;
      i = j; continue; }
    if (n && n < cap - 1) out[n++] = '/';
    while (i < j && n < cap - 1) out[n++] = ln[i++]; }
  return n; }
