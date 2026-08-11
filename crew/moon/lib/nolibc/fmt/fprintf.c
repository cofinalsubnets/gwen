#include "../impl.h"

static unsigned long const bd_p10[9] = {
  1UL, 10UL, 100UL, 1000UL, 10000UL, 100000UL, 1000000UL, 10000000UL, 100000000UL };
/* the digit at index k -- BD_U is the units place, larger is further right.
 * OUTSIDE the array every digit is a zero, which is what lets an arbitrary
 * precision print with no buffer and no cap. */
static unsigned bd_dig(unsigned long const *d, int k) {
  if (k < 0 || k >= BD_D) return 0;
  return (unsigned) (d[k / 9] / bd_p10[8 - k % 9] % 10UL); }
/* the live range shrinks toward index 0 as the value grows and toward BD_N as
 * it shrinks, so both walks carry their bound rather than sweeping the array:
 * a shift is O(digits that exist), not O(1413). */
static int bd_mul2(unsigned long *d, int lo, int hi, int s) {
  unsigned long carry = 0;
  int i;
  for (i = hi; i >= 0; i--) {
    if (i < lo && !carry) break;
    unsigned long cur = (d[i] << s) + carry;
    d[i] = cur % BD_B;
    carry = cur / BD_B; }
  return i + 1 < lo ? i + 1 : lo; }
static int bd_div2(unsigned long *d, int lo, int hi, int s) {
  unsigned long rem = 0, m = 1UL << s;
  int i;
  for (i = lo; i < BD_N; i++) {
    if (i > hi && !rem) break;
    unsigned long cur = rem * BD_B + d[i];
    d[i] = cur / m;
    rem = cur % m; }
  return i - 1 > hi ? i - 1 : hi; }
/* zero every digit after index k */
static void bd_trunc(unsigned long *d, int k) {
  int i;
  if (k >= BD_D) return;
  if (k < 0) i = 0;
  else { d[k / 9] -= d[k / 9] % bd_p10[8 - k % 9]; i = k / 9 + 1; }
  for (; i < BD_N; i++) d[i] = 0; }
/* add one at digit index k, carrying toward index 0. the array has 324 integer
 * digits against a largest double of 309, so the carry always lands. */
static int bd_inc(unsigned long *d, int k, int lo) {
  int i = k / 9;
  unsigned long add = bd_p10[8 - k % 9];
  while (i >= 0) {
    d[i] += add;
    if (d[i] < BD_B) break;
    d[i] -= BD_B; add = 1; i--; }
  return i >= 0 && i < lo ? i : lo; }
/* round to keep digits through index k. the value is exact, so a TIE is a real
 * tie and breaks to even -- %.0f of 2.5 is 2 and of 3.5 is 4. */
static int bd_round(unsigned long *d, int k, int lo) {
  unsigned n = bd_dig(d, k + 1);
  int up = n > 5;
  if (n == 5) {
    int j = k + 2, any = 0, lim = (j + 8) / 9 * 9;
    for (; j < lim && j < BD_D; j++) if (bd_dig(d, j)) { any = 1; break; }
    if (!any) for (j = lim / 9; j < BD_N; j++) if (d[j]) { any = 1; break; }
    up = any || (bd_dig(d, k) & 1); }
  bd_trunc(d, k);
  return up ? bd_inc(d, k, lo) : lo; }
/* the index of the most significant digit that is not a zero, or the units
 * place when the value is zero (which reads 0 and dates the exponent at 0). */
static int bd_msd(unsigned long const *d, int lo) {
  for (int i = lo; i < BD_N; i++)
    if (d[i]) { int k = i * 9; while (!bd_dig(d, k)) k++; return k; }
  return BD_U; }
/* the float lanes: %f %e %g %a and their upper-case twins. */
static void __fmtflo(void (*put)(void *, int), void *ctx, double v, int conv,
                     int prec, int width, int fl) {
  unsigned long bits;
  memcpy(&bits, &v, sizeof bits);
  int neg = (int) (bits >> 63);            /* ⚠ from the SIGN BIT, not v < 0:
                                              -0.0 is not less than zero, and
                                              printf must still say -0 */
  int be = (int) ((bits >> 52) & 0x7ffUL);
  unsigned long man = bits & 0xfffffffffffffUL;
  int up = conv == 'E' || conv == 'G' || conv == 'F' || conv == 'A';
  if (up) conv += 32;
  int sgn = __fmtsgn(neg, fl);

  /* --- infinity and not-a-number: three letters, and the zero flag does not
     reach them -- a wide %08.1f of -inf pads with spaces. */
  if (be == 0x7ff) {
    char const *w = man ? "nan" : "inf";
    int len = 3 + (sgn ? 1 : 0);
    int pad = width > len ? width - len : 0;
    if (!(fl & FF_LEFT)) __pad(put, ctx, pad, 32);
    if (sgn) put(ctx, sgn);
    for (int i = 0; i < 3; i++) put(ctx, up ? w[i] - 32 : w[i]);
    if (fl & FF_LEFT) __pad(put, ctx, pad, 32);
    return; }

  /* --- %a: the bits themselves, four to a hex digit, so only the rounding at
     a short precision needs any care. a subnormal keeps the -1022 exponent and
     shows a leading 0 rather than renormalising, and so does a mantissa that
     rounds up past f -- 0.999999 at %.1a is 0x2.0p-1, not 0x1.0p+0. */
  if (conv == 'a') {
    int lead = be ? 1 : 0;
    int xe = be ? be - 1023 : man ? -1022 : 0;
    int nd = 13;
    if (prec >= 0 && prec < 13) {
      nd = prec;
      unsigned g = (unsigned) ((man >> (48 - 4 * nd)) & 0xfUL);
      unsigned long rest = man & ((1UL << (48 - 4 * nd)) - 1);
      int odd = nd ? (int) ((man >> (52 - 4 * nd)) & 1UL) : lead & 1;
      int rup = g > 8 || (g == 8 && (rest || odd));
      man &= ~((1UL << (52 - 4 * nd)) - 1);
      if (rup) { man += 1UL << (52 - 4 * nd);
                 if (man >> 52) { man &= 0xfffffffffffffUL; lead++; } } }
    else if (prec >= 0) nd = prec;                    /* past 13, all zeros */
    else while (nd && !((man >> (52 - 4 * nd)) & 0xfUL)) nd--;
    int pt = nd > 0 || (fl & FF_ALT);
    int ax = xe < 0 ? -xe : xe;
    int nx = 1; for (int t = ax; t >= 10; t /= 10) nx++;
    int len = (sgn ? 1 : 0) + 2 + 1 + (pt ? 1 + nd : 0) + 2 + nx;
    int pad = width > len ? width - len : 0;
    if (!(fl & (FF_LEFT | FF_ZERO))) __pad(put, ctx, pad, 32);
    if (sgn) put(ctx, sgn);
    put(ctx, '0'); put(ctx, up ? 'X' : 'x');
    if (!(fl & FF_LEFT) && (fl & FF_ZERO)) __pad(put, ctx, pad, 48);
    put(ctx, (char) ('0' + lead));
    if (pt) {
      put(ctx, '.');
      for (int j = 0; j < nd; j++) {
        unsigned h = j < 13 ? (unsigned) ((man >> (48 - 4 * j)) & 0xfUL) : 0;
        put(ctx, h < 10 ? '0' + h : (up ? 55 : 87) + h); } }
    put(ctx, up ? 'P' : 'p');
    put(ctx, xe < 0 ? '-' : '+');
    for (int t = nx; t; t--) { int q = ax; for (int u = 1; u < t; u++) q /= 10; put(ctx, '0' + q % 10); }
    if (fl & FF_LEFT) __pad(put, ctx, pad, 32);
    return; }

  /* --- the decimal lanes, off the exact digits. */
  unsigned long bd[BD_N];
  for (int i = 0; i < BD_N; i++) bd[i] = 0;
  unsigned long m = be ? man | 0x10000000000000UL : man;
  int e2 = be ? be - 1075 : -1074;               /* the value is m * 2^e2 */
  bd[BD_I - 1] = m % BD_B;
  bd[BD_I - 2] = m / BD_B;                       /* m < 2^53, so two limbs */
  int lo = BD_I - 2, hi = BD_I - 1;
  for (int s = e2; s > 0; ) { int k = s > 30 ? 30 : s; lo = bd_mul2(bd, lo, hi, k); s -= k; }
  for (int s = -e2; s > 0; ) { int k = s > 30 ? 30 : s; hi = bd_div2(bd, lo, hi, k); s -= k; }

  int msd = bd_msd(bd, lo);
  int fprec, cut, strip = 0;
  if (conv == 'g') {
    int p = prec < 0 ? 6 : prec ? prec : 1;
    lo = bd_round(bd, msd + p - 1, lo);
    msd = bd_msd(bd, lo);                        /* 999 -> 1000 moves it */
    int dexp = BD_U - msd;
    conv = (dexp < -4 || dexp >= p) ? 'e' : 'f';
    fprec = conv == 'e' ? p - 1 : p - 1 - dexp;
    if (fprec < 0) fprec = 0;
    strip = !(fl & FF_ALT); }
  else {
    fprec = prec < 0 ? 6 : prec;
    cut = conv == 'e' ? msd + fprec : BD_U + fprec;
    lo = bd_round(bd, cut, lo);
    msd = bd_msd(bd, lo); }

  /* the digits to show: one at msd for %e, or the whole integer part for %f --
     and when the value has none, the units place, which reads the 0 that C
     asks for. */
  int i0 = conv == 'e' ? msd : msd < BD_U ? msd : BD_U;
  int i1 = conv == 'e' ? msd : BD_U;
  if (strip) while (fprec && !bd_dig(bd, i1 + fprec)) fprec--;
  int pt = fprec > 0 || (fl & FF_ALT);
  int xe = BD_U - msd, ax = xe < 0 ? -xe : xe, nx = 1;
  for (int t = ax; t >= 10; t /= 10) nx++;
  if (nx < 2) nx = 2;                            /* the exponent shows two */

  int len = (sgn ? 1 : 0) + (i1 - i0 + 1) + (pt ? 1 + fprec : 0)
          + (conv == 'e' ? 2 + nx : 0);
  int pad = width > len ? width - len : 0;
  if (!(fl & (FF_LEFT | FF_ZERO))) __pad(put, ctx, pad, 32);
  if (sgn) put(ctx, sgn);
  if (!(fl & FF_LEFT) && (fl & FF_ZERO)) __pad(put, ctx, pad, 48);
  for (int k = i0; k <= i1; k++) put(ctx, '0' + bd_dig(bd, k));
  if (pt) { put(ctx, '.');
            for (int j = 1; j <= fprec; j++) put(ctx, '0' + bd_dig(bd, i1 + j)); }
  if (conv == 'e') {
    put(ctx, up ? 'E' : 'e');
    put(ctx, xe < 0 ? '-' : '+');
    for (int t = nx; t; t--) { int q = ax; for (int u = 1; u < t; u++) q /= 10; put(ctx, '0' + q % 10); } }
  if (fl & FF_LEFT) __pad(put, ctx, pad, 32); }
static void __fmt(void (*put)(void *, int), void *ctx, char const *fmt, va_list ap) {
  for (; *fmt; fmt++) {
    if (*fmt != '%') { put(ctx, *fmt); continue; }
    fmt++;
    int fl = 0, width = 0, prec = -1, wide = 0;
    for (; ; fmt++) {                        /* flags: all five of them act */
      if (*fmt == '-') fl |= FF_LEFT;
      else if (*fmt == '0') fl |= FF_ZERO;
      else if (*fmt == '+') fl |= FF_PLUS;
      else if (*fmt == ' ') fl |= FF_SPC;
      else if (*fmt == '#') fl |= FF_ALT;
      else break; }
    while (*fmt >= '0' && *fmt <= '9') { width = width * 10 + (*fmt - 48); fmt++; }
    if (*fmt == '.') { fmt++; prec = 0; while (*fmt >= '0' && *fmt <= '9') { prec = prec * 10 + (*fmt - 48); fmt++; } }
    while (*fmt == 'l' || *fmt == 'z' || *fmt == 'h') { if (*fmt != 'h') wide = 1; fmt++; }
    if (fl & FF_LEFT) fl &= ~FF_ZERO;
    if (fl & FF_PLUS) fl &= ~FF_SPC;         /* + outranks the space */
    if (*fmt == 's') {
      char const *s = va_arg(ap, char const *);
      if (!s) s = "(null)";
      int len = 0;
      while (s[len] && (prec < 0 || len < prec)) len++;
      int pad = width > len ? width - len : 0;
      if (!(fl & FF_LEFT)) __pad(put, ctx, pad, 32);
      for (int i = 0; i < len; i++) put(ctx, s[i]);
      if (fl & FF_LEFT) __pad(put, ctx, pad, 32); }
    else if (*fmt == 'c') {
      int pad = width > 1 ? width - 1 : 0;
      if (!(fl & FF_LEFT)) __pad(put, ctx, pad, 32);
      put(ctx, va_arg(ap, int));
      if (fl & FF_LEFT) __pad(put, ctx, pad, 32); }
    else if (*fmt == 'd' || *fmt == 'i') {
      long v = wide ? va_arg(ap, long) : (long) va_arg(ap, int);
      unsigned long u = (unsigned long) v;
      int neg = v < 0;
      if (neg) u = 0UL - u;
      __fmtnum(put, ctx, u, 10, neg, width, fl, 0); }
    else if (*fmt == 'u')
      __fmtnum(put, ctx, wide ? va_arg(ap, unsigned long) : (unsigned long) va_arg(ap, unsigned int), 10, 0, width, fl, 0);
    else if (*fmt == 'x' || *fmt == 'X')
      __fmtnum(put, ctx, wide ? va_arg(ap, unsigned long) : (unsigned long) va_arg(ap, unsigned int), 16, 0, width, fl, *fmt == 'X');
    else if (*fmt == 'o')
      __fmtnum(put, ctx, wide ? va_arg(ap, unsigned long) : (unsigned long) va_arg(ap, unsigned int), 8, 0, width, fl, 0);
    else if (*fmt == 'f' || *fmt == 'F' || *fmt == 'e' || *fmt == 'E' || *fmt == 'g' || *fmt == 'G'
             || *fmt == 'a' || *fmt == 'A')
      __fmtflo(put, ctx, va_arg(ap, double), *fmt, prec, width, fl);
    else if (*fmt == 'p') { put(ctx, 48); put(ctx, 120); __fmtnum(put, ctx, (unsigned long) va_arg(ap, void *), 16, 0, 0, 0, 0); }
    else if (*fmt == '%') put(ctx, 37);
    else { put(ctx, 37); if (*fmt) put(ctx, *fmt); else fmt--; } }
}
int fprintf(FILE *f, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  __fmt(__femit, f, fmt, ap);
  va_end(ap);
  return 0; }
int snprintf(char *p, size_t n, char const *fmt, ...) {
  struct __sctx s;
  s.p = p; s.n = n; s.at = 0;
  va_list ap; va_start(ap, fmt);
  __fmt(__semit, &s, fmt, ap);
  va_end(ap);
  if (n) p[s.at < n ? s.at : n - 1] = 0;
  return (int) s.at; }
int printf(char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  __fmt(__femit, stdout, fmt, ap);
  va_end(ap);
  return 0; }
/* the v-variants: __fmt already threads a va_list, so these just forward it. */
int vfprintf(FILE *f, char const *fmt, va_list ap) {
  __fmt(__femit, f, fmt, ap); return 0; }
int vprintf(char const *fmt, va_list ap) {
  __fmt(__femit, stdout, fmt, ap); return 0; }
int vsnprintf(char *p, size_t n, char const *fmt, va_list ap) {
  struct __sctx s; s.p = p; s.n = n; s.at = 0;
  __fmt(__semit, &s, fmt, ap);
  if (n) p[s.at < n ? s.at : n - 1] = 0;
  return (int) s.at; }
/* asprintf: measure with a null sink, then format into a fresh block. two
 * passes over the format rather than a growing buffer -- __fmt counts either way. */
int vasprintf(char **out, char const *fmt, va_list ap) {
  struct __sctx s; s.p = 0; s.n = 0; s.at = 0;
  va_list m;                                       /* the measuring pass takes a COPY:
                                                    * __fmt walks the list to its end */
  va_copy(m, ap);
  __fmt(__semit, &s, fmt, m);
  va_end(m);
  char *b = malloc(s.at + 1);
  if (!b) return *out = 0, -1;
  struct __sctx t; t.p = b; t.n = s.at + 1; t.at = 0;
  __fmt(__semit, &t, fmt, ap);
  b[t.at] = 0;
  return *out = b, (int) t.at; }
