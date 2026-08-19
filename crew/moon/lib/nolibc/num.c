/* crew/moon/lib/nolibc/num.c -- strtol/strtod, and the stdio odds and ends the
 * HEADERS promised (sscanf, ferror, popen). */
#include "impl.h"

/* ---- strtol / strtod: the reader's number path. SATURATION INCLUDED (the
 * kernel corpus runs them; the naive
 * strtod measured corpus-green against glibc's in the rung-4 differential). ---- */
static int __digval(int c) {
  if (c >= 48 && c <= 57) return c - 48;
  if (c >= 97 && c <= 122) return c - 87;
  if (c >= 65 && c <= 90) return c - 55;
  return 99; }
long strtol(char const *s, char **endptr, int base) {
  char const *p = s;
  int sign = 1;
  while (*p == 32 || (*p >= 9 && *p <= 13)) p++;
  if (*p == '-') { sign = -1; p++; }
  else if (*p == '+') p++;
  if (*p == '0') {
    ++p;
    if ((base == 0 || base == 16) && (*p == 'x' || *p == 'X')) {
      base = 16;
      ++p;
      if (__digval(*p) >= base) p -= 2; }
    else if (base == 0) { base = 8; --p; }
    else --p; }
  else if (!base) base = 10;
  if (base < 2 || base > 36) return 0;
  /* OVERFLOW SATURATES -- it does not wrap. the standard says so, and every
   * strtol we sit beside (glibc, musl, newlib) does it; the accumulator used to
   * wrap, which is how ONE source text came to read as two different numbers
   * depending on which libc the binary carried. saturating also leaves the
   * caller a signal (the limit value, and ERANGE) where wrapping leaves a
   * plausible lie. the accumulation runs UNSIGNED so LONG_MIN's magnitude is
   * reachable without signed overflow on the way. */
  unsigned long lim = sign < 0 ? (unsigned long) LONG_MAX + 1UL : (unsigned long) LONG_MAX,
                cut = lim / (unsigned long) base, cutd = lim % (unsigned long) base, rc = 0;
  int any = 0, over = 0;
  for (int d; (d = __digval(*p)) < base; p++) {
    any = 1;
    if (over || rc > cut || (rc == cut && (unsigned long) d > cutd)) over = 1;
    else rc = rc * (unsigned long) base + (unsigned long) d; }
  if (endptr) *endptr = (char *) (any ? p : s);
  if (!any) return 0;
  if (over) { errno = ERANGE; return sign < 0 ? LONG_MIN : LONG_MAX; }
  return (long) (sign < 0 ? 0UL - rc : rc); }
double atof(char const *s) { return strtod(s, 0); }
/* the libc math faces over the am floor (am.c's seven transcendentals ride
 * m_am.o in every ladder link); the rest are exact derivations. tan and the
 * arc trio are DERIVED (a few ulp looser than a dedicated kernel) -- enough
 * for the ladder; a consumer that measures gets its own am kernel. */
double am_sqrt(double), am_exp(double), am_log(double);
double am_sin(double), am_cos(double), am_atan2(double, double), am_pow(double, double);
double sqrt(double x) { return am_sqrt(x); }
double exp(double x) { return am_exp(x); }
double log(double x) { return am_log(x); }
double sin(double x) { return am_sin(x); }
double cos(double x) { return am_cos(x); }
double tan(double x) { return am_sin(x) / am_cos(x); }
double pow(double x, double y) { return am_pow(x, y); }
double atan2(double y, double x) { return am_atan2(y, x); }
double atan(double x) { return am_atan2(x, 1.0); }
double asin(double x) { return am_atan2(x, am_sqrt(1.0 - x * x)); }
double acos(double x) { return am_atan2(am_sqrt(1.0 - x * x), x); }
double log2(double x) { return am_log(x) * 1.4426950408889634; }
double log10(double x) { return am_log(x) * 0.4342944819032518; }
double sinh(double x) { double e = am_exp(x); return (e - 1.0 / e) / 2.0; }
double cosh(double x) { double e = am_exp(x); return (e + 1.0 / e) / 2.0; }
double tanh(double x) { double e = am_exp(2.0 * x); return (e - 1.0) / (e + 1.0); }
double fabs(double x) { return x <= 0 ? 0.0 - x : x; }
static double __trunc9(double x) {                 /* |x| < 2^52 assumed */
  double t = (double) (long) x;
  return t; }
double floor(double x) {
  if (x != x || x >= 9007199254740992.0 || x <= -9007199254740992.0) return x;
  double t = __trunc9(x);
  return t > x ? t - 1.0 : t; }
double ceil(double x) {
  if (x != x || x >= 9007199254740992.0 || x <= -9007199254740992.0) return x;
  double t = __trunc9(x);
  return t < x ? t + 1.0 : t; }
double fmod(double x, double y) {
  if (y == 0.0 || x != x || y != y) return 0.0 / 0.0;
  double q = x / y;
  if (q >= 9007199254740992.0 || q <= -9007199254740992.0) return 0.0;   /* quotient past exact-int: stance */
  double r = x - __trunc9(q) * y;
  return r; }
/* the C99 float twins: the double face, narrowed once at the return. a dedicated
 * binary32 kernel would be faster and no more accurate -- the double result is
 * already correct past float's 24 bits for every one of these. */
float sinf(float x) { return (float) sin(x); }
float cosf(float x) { return (float) cos(x); }
float tanf(float x) { return (float) tan(x); }
float asinf(float x) { return (float) asin(x); }
float acosf(float x) { return (float) acos(x); }
float atanf(float x) { return (float) atan(x); }
float expf(float x) { return (float) exp(x); }
float logf(float x) { return (float) log(x); }
float log2f(float x) { return (float) log2(x); }
float log10f(float x) { return (float) log10(x); }
float sqrtf(float x) { return (float) sqrt(x); }
float fabsf(float x) { return (float) fabs(x); }
float floorf(float x) { return (float) floor(x); }
float ceilf(float x) { return (float) ceil(x); }
float atan2f(float y, float x) { return (float) atan2(y, x); }
float powf(float x, float y) { return (float) pow(x, y); }
float fmodf(float x, float y) { return (float) fmod(x, y); }
/* frexp/ldexp: exact exponent surgery on the IEEE bits (no math floor needed) */
double frexp(double x, int *e) {
  union { double d; unsigned long u; } b;
  b.d = x;
  int ex = (int) ((b.u >> 52) & 2047);
  *e = 0;
  if (ex == 2047 || x == 0) return x;              /* inf/nan/0 ride through, *e 0 */
  if (ex == 0) {                                   /* denormal: normalize by 2^64 first */
    b.d = x * 18446744073709551616.0;
    ex = (int) ((b.u >> 52) & 2047) - 64; }
  *e = ex - 1022;
  b.u = (b.u & 0x800ffffffffffffful) | 0x3fe0000000000000ul;
  return b.d; }
static double __e2d(int n) {                       /* 2^n for normal n */
  union { double d; unsigned long u; } b;
  b.u = ((unsigned long) (n + 1023)) << 52;
  return b.d; }
double ldexp(double x, int n) {                    /* x * 2^n, clamped through the rim in steps */
  if (n > 1023) { x *= __e2d(1023); n -= 1023;
    if (n > 1023) { x *= __e2d(1023); n -= 1023; if (n > 1023) n = 1023; } }
  else if (n < -1022) { x *= __e2d(-969); n += 969;
    if (n < -1022) { x *= __e2d(-969); n += 969; if (n < -1022) n = -1022; } }
  return x * __e2d(n); }
/* the math floor's exact reader (crew/moon/lib/math/am.c -- linked wherever
   nolibc is: the raw love build and the whole moon userland): correctly
   rounded, so read(show x) = x holds off-glibc too. The naive accumulator
   that lived here parsed "0.3" one ulp off -- masked until love's printer
   went shortest-roundtrip, then loud in test_raw. */
double am_strtod(char const *, char **);
/* ⚠ THE LIBC FACE IS NOT am_strtod's FACE, and the wrapper is where they part:
 * am_strtod is love's float reader, and the reader hands it a whole TOKEN, so
 * it skips no leading space. C's strtod owes that, and owes endptr = the
 * ORIGINAL nptr when nothing converts. doing it here keeps am.c exactly what
 * love wants -- correctly rounded and nothing else. found by test/libc/num.c.
 * ⚠ the SIGN of a zero needs nothing: am_strtod gets -0.0 right on its own.
 * it did not while mooncc lowered -d as 0.0 - d (crew/moon/gen.l), and a
 * wrapper that "fixed" it here would now flip the sign BACK, since -0.0 == 0.0
 * tests true. */
double strtod(char const *s, char **end) {
  char const *p = s;
  while (*p == 32 || (*p >= 9 && *p <= 13)) p++;
  char *e = (char *) p;
  double v = am_strtod(p, &e);
  if (e == p) { if (end) *end = (char *) s; return 0.0; }   /* no conversion: the ORIGINAL s */
  if (end) *end = e;
  return v; }
/* the unsigned twin: strtol's digit walk, saturating at ULONG_MAX the same way,
 * with the ONE wrap the standard does ask for -- a leading minus negates the
 * magnitude modulo 2^64 rather than refusing. */
static unsigned long __strtoux(char const *s, char **endptr, int base) {
  char const *p = s;
  int neg = 0;
  while (*p == 32 || (*p >= 9 && *p <= 13)) p++;
  if (*p == '-') { neg = 1; p++; } else if (*p == '+') p++;
  if (*p == '0') {
    ++p;
    if ((base == 0 || base == 16) && (*p == 'x' || *p == 'X')) { base = 16; ++p; if (__digval(*p) >= base) p -= 2; }
    else if (base == 0) { base = 8; --p; }
    else --p; }
  else if (!base) base = 10;
  if (base < 2 || base > 36) return 0;
  unsigned long cut = ULONG_MAX / (unsigned long) base, cutd = ULONG_MAX % (unsigned long) base, rc = 0;
  int any = 0, over = 0;
  for (int d; (d = __digval(*p)) < base; p++) {
    any = 1;
    if (over || rc > cut || (rc == cut && (unsigned long) d > cutd)) over = 1;
    else rc = rc * (unsigned long) base + (unsigned long) d; }
  if (endptr) *endptr = (char *) (any ? p : s);
  if (!any) return 0;
  if (over) { errno = ERANGE; return ULONG_MAX; }
  return neg ? 0UL - rc : rc; }
long strtoll(char const *s, char **endptr, int base) { return strtol(s, endptr, base); }
unsigned long strtoul(char const *s, char **endptr, int base) { return __strtoux(s, endptr, base); }
unsigned long strtoull(char const *s, char **endptr, int base) { return __strtoux(s, endptr, base); }
unsigned long strtoumax(char const *s, char **endptr, int base) { return __strtoux(s, endptr, base); }

/* qsort: shellsort (Ciura-ish 3x gaps would be nicer, but n/2 halving is small
 * and tar's arrays are short). in-place byte swap of size-sz elements. */
void qsort(void *base, size_t n, size_t sz, int (*cmp)(void const *, void const *)) {
  char *a = base;
  for (size_t gap = n / 2; gap > 0; gap /= 2)
    for (size_t i = gap; i < n; i++)
      for (size_t j = i; j >= gap && cmp(a + (j - gap) * sz, a + j * sz) > 0; j -= gap) {
        char *x = a + (j - gap) * sz, *y = a + j * sz;
        for (size_t k = 0; k < sz; k++) { char t = x[k]; x[k] = y[k]; y[k] = t; } } }
/* qsort's twin, and the headers already named it: a plain binary search over
 * the half-open span, answering the ELEMENT or null. */
void *bsearch(void const *key, void const *base, size_t n, size_t sz,
              int (*cmp)(void const *, void const *)) {
  char const *a = base;
  size_t lo = 0, hi = n;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    int r = cmp(key, a + mid * sz);
    if (r == 0) return (void *) (a + mid * sz);
    if (r < 0) hi = mid; else lo = mid + 1; }
  return 0; }

/* exec's variadic pair: gather (arg0, .., NULL) off the stack, then execv[p]. */
int execl(char const *p, char const *a0, ...) {
  char *av[256]; int n = 0;
  va_list ap; va_start(ap, a0);
  av[n++] = (char *) a0;
  while (n < 255 && (av[n] = va_arg(ap, char *))) n++;
  av[n] = 0;
  va_end(ap);
  return execv(p, av); }
int execlp(char const *f, char const *a0, ...) {
  char *av[256]; int n = 0;
  va_list ap; va_start(ap, a0);
  av[n++] = (char *) a0;
  while (n < 255 && (av[n] = va_arg(ap, char *))) n++;
  av[n] = 0;
  va_end(ap);
  return execvp(f, av); }
/* system: fork, /bin/sh -c, wait. no signal juggling (love is single-threaded). */
int system(char const *cmd) {
  if (!cmd) return 1;                          /* a shell is available */
  int pid = fork();
  if (pid < 0) return -1;
  if (pid == 0) {
    char *av[4]; av[0] = "sh"; av[1] = "-c"; av[2] = (char *) cmd; av[3] = 0;
    execv("/bin/sh", av);
    _exit(127); }
  int st = 0;
  while (waitpid(pid, &st, 0) < 0) if (errno != EINTR) return -1;
  return st; }
/* popen: system's shape with a pipe spliced onto the child's stdout ("r") or
 * stdin ("w"); the child pid rides the FILE for pclose's wait (m4 esyscmd). */
FILE *popen(char const *cmd, char const *mode) {
  int fds[2];
  int rd = mode[0] == 'r';
  if (pipe(fds) < 0) return 0;
  int pid = fork();
  if (pid < 0) { close(fds[0]); close(fds[1]); return 0; }
  if (pid == 0) {
    dup2(rd ? fds[1] : fds[0], rd ? 1 : 0);
    close(fds[0]); close(fds[1]);
    char *av[4]; av[0] = "sh"; av[1] = "-c"; av[2] = (char *) cmd; av[3] = 0;
    execv("/bin/sh", av);
    _exit(127); }
  close(rd ? fds[1] : fds[0]);
  FILE *f = malloc(sizeof(FILE) + 4096);
  if (!f) { close(rd ? fds[0] : fds[1]); return 0; }
  memset(f, 0, sizeof(FILE));
  f->fd = rd ? fds[0] : fds[1];
  f->wr = !rd;
  f->heap = 1;
  f->pid = pid;
  if (!rd) { f->buf = (unsigned char *) (f + 1); f->cap = 4096; }
  return f; }
int pclose(FILE *f) {
  int pid = f->pid, st = 0;
  int r = fclose(f);
  if (r == EOF) return -1;
  while (waitpid(pid, &st, 0) < 0) if (errno != EINTR) return -1;
  return st; }
/* tmpfile: a mktemp'd /tmp file opened w+ and unlinked at once, so it lives
 * exactly as long as the FILE (m4's diversions write it, rewind, read back). */
FILE *tmpfile(void) {
  char buf[16];
  strcpy(buf, "/tmp/aiXXXXXX");
  mktemp(buf);
  if (!buf[0]) return 0;
  FILE *f = fopen(buf, "w+");
  if (f) unlink(buf);
  return f; }
int remove(char const *p) {                        /* the ISO face: unlink, a directory falls to rmdir */
  int r = unlink(p);
  return r == 0 ? 0 : rmdir(p); }
char *tmpnam(char *s) {                            /* the ISO face over mktemp (lua's os.tmpname) */
  static char b[20];
  if (!s) s = b;
  strcpy(s, "/tmp/aiXXXXXX");
  mktemp(s);
  return s[0] ? s : 0; }
/* mktemp: fill the trailing XXXXXX from the pid and bump until the name is
 * free (racy by design -- the caller opens it; m4's diversion files). */
char *mktemp(char *tmpl) {
  size_t n = strlen(tmpl);
  if (n < 6 || strcmp(tmpl + n - 6, "XXXXXX")) { tmpl[0] = 0; return tmpl; }
  char *x = tmpl + n - 6;
  unsigned long v = (unsigned long) getpid();
  for (int k = 0; k < 100; k++, v += 7777) {
    unsigned long w = v;
    for (int i = 0; i < 6; i++) { x[i] = 'a' + w % 26; w /= 26; }
    if (access(tmpl, 0) < 0) return tmpl; }
  tmpl[0] = 0;
  return tmpl; }
/* one fixed "C" locale, so setlocale just answers its name. */
char *setlocale(int cat, char const *loc) { (void) cat; (void) loc; return (char *) "C"; }
struct lconv *localeconv(void) {                   /* the C locale's table: "." and empties */
  static struct lconv c = { (char *) ".", (char *) "", (char *) "",
    (char *) "", (char *) "", (char *) "", (char *) "", (char *) "",
    (char *) "", (char *) "", 127, 127, 127, 127, 127, 127, 127, 127 };
  return &c; }

/* getc/fputs/ferror over the unbuffered read streams; fscanf reads char-by-char
 * (no ungetc, so it consumes the field terminator -- tar's lone use is "%d"). */
int getc(FILE *f) {
  unsigned char c;
  if (f->un) { int r = f->un - 1; f->un = 0; return r; }
  long k = read(f->fd, &c, 1);
  if (k <= 0) { if (k < 0) f->err = 1; else f->eof = 1; return EOF; }
  return c; }
int ungetc(int c, FILE *f) {
  if (c == EOF || f->un) return EOF;
  f->un = (c & 255) + 1;
  f->eof = 0;
  return c & 255; }
int fputs(char const *s, FILE *f) { size_t n = strlen(s); return fwrite(s, 1, n, f) == n ? 0 : EOF; }
/* ---- the stdout/stdin shorthands and the odds and ends the HEADERS already
 * promised. every one of these was declared in crew/moon/include/ with no body
 * anywhere, so a program calling it compiled and then died at the LINK under
 * CC=mooncc while building fine against glibc -- gnulib's progname module
 * reaches getprogname exactly that way. test/libc/'s header-completeness phase
 * (test/gate/libc.sh) is what found them and is what keeps the promise honest
 * from here: a name the headers declare must have a definition. ---- */
int putchar(int c) { return fputc(c, stdout); }
int puts(char const *s) { return fputs(s, stdout) == EOF || fputc('\n', stdout) == EOF ? EOF : 0; }
int fgetc(FILE *f) { return getc(f); }
int getchar(void) { return getc(stdin); }
int ferror(FILE *f) { return f->err; }
int feof(FILE *f) { return f->eof; }
void clearerr(FILE *f) { f->err = 0; f->eof = 0; }
/* sscanf, the string twin, %d only (m4 builtin.c's lone use: a divert number). */
int sscanf(char const *s, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int got = 0;
  for (; *fmt; fmt++) {
    if (*fmt == '%' && fmt[1] == 'd') {
      char *e;
      long v = strtol(s, &e, 10);
      if (e == s) break;
      *va_arg(ap, int *) = (int) v;
      s = e; fmt++; got++; }
    else if (*fmt == ' ') { while (*s == ' ' || (*s >= 9 && *s <= 13)) s++; }
    else { if (*s != *fmt) break; s++; } }
  va_end(ap);
  return got; }
static int __vfscanf(FILE *f, char const *fmt, va_list ap) {
  int got = 0, c;
  for (; *fmt; fmt++) {
    if (*fmt == '%') {
      fmt++;
      if (*fmt == 'd' || *fmt == 'u' || *fmt == 'x' || *fmt == 's')
        do { c = getc(f); } while (c == 32 || (c >= 9 && c <= 13));
      if (*fmt == 'd' || *fmt == 'u' || *fmt == 'x') {
        int base = *fmt == 'x' ? 16 : 10, sign = 1, any = 0, d;
        if (*fmt == 'd' && (c == '-' || c == '+')) { if (c == '-') sign = -1; c = getc(f); }
        long v = 0;
        while ((d = __digval(c)) < base) { v = v * base + d; any = 1; c = getc(f); }
        if (!any) break;
        *va_arg(ap, int *) = (int) (sign * v);
        got++; }
      else if (*fmt == 's') {
        char *out = va_arg(ap, char *); int i = 0;
        while (c != EOF && !(c == 32 || (c >= 9 && c <= 13))) { out[i++] = (char) c; c = getc(f); }
        out[i] = 0; got++; }
      else if (*fmt == 'c') { c = getc(f); if (c == EOF) break; *va_arg(ap, char *) = (char) c; got++; } }
    else if (*fmt == 32 || (*fmt >= 9 && *fmt <= 13)) ;   /* fmt whitespace: no peek, skip */
    else { c = getc(f); if (c != (unsigned char) *fmt) break; } }
  return got; }
/* one body, three faces -- fscanf and scanf differ only in which stream */
int fscanf(FILE *f, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int r = __vfscanf(f, fmt, ap);
  va_end(ap);
  return r; }
int scanf(char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int r = __vfscanf(stdin, fmt, ap);
  va_end(ap);
  return r; }

/* no name database yet: every passwd/group lookup misses, so tar prints numeric
 * owner/group (its own fallback). a real /etc/passwd walk is a later rung. */
struct passwd *getpwuid(uid_t u) { (void) u; return 0; }
struct passwd *getpwnam(char const *n) { (void) n; return 0; }
struct group *getgrgid(gid_t g) { (void) g; return 0; }
struct group *getgrnam(char const *n) { (void) n; return 0; }
void setgrent(void) { }

