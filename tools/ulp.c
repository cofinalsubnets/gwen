// tools/ulp.c -- the differential ulp harness: the math floor (crew/moon/lib/
// math/am.c) vs the host libm oracle. deterministic sweeps (xorshift, fixed
// seed) over per-function domains + hand-picked edges; reports max ulp +
// where, and the >0.5/>1/>2 ulp counts. `make ulp` builds + runs it (hosted
// only -- the oracle is glibc); `./out/host/ulp reduce` prints the sin ulp
// by magnitude band (the reduction scan).
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>

double am_sqrt(double), am_exp(double), am_log(double);
double am_sin(double), am_cos(double), am_atan2(double, double), am_pow(double, double);

static uint64_t rng = 0x243F6A8885A308D3ull;   // pi digits, fixed seed
static uint64_t xr(void) { rng ^= rng << 13; rng ^= rng >> 7; rng ^= rng << 17; return rng; }
static double rnd01(void) { return (double)(xr() >> 11) * 0x1p-53; }
// log-uniform magnitude in [2^elo, 2^ehi), random sign if s
static double rmag(int elo, int ehi, int s) {
 double e = elo + rnd01() * (ehi - elo);
 double v = exp2(e) * (1 + rnd01());
 return (s && (xr() & 1)) ? -v : v; }

static int64_t bits(double x) { int64_t b; memcpy(&b, &x, 8); return b; }
// ordered-integer distance in ulps; inf/nan mismatches -> huge
static double ulpd(double got, double want) {
 if (isnan(got) && isnan(want)) return 0;
 if (isnan(got) || isnan(want)) return 1e18;
 if (isinf(got) || isinf(want)) return got == want ? 0 : 1e18;
 int64_t a = bits(got), b = bits(want);
 if (a < 0) a = INT64_MIN - a;
 if (b < 0) b = INT64_MIN - b;
 double d = (double)(a - b);
 return d < 0 ? -d : d; }

struct acc { double max; double at, at2; long n, n05, n1, n2; };
static void tally(struct acc *a, double x, double y, double got, double want) {
 double u = ulpd(got, want);
 a->n++;
 if (u > 0.5) a->n05++;
 if (u > 1.0) a->n1++;
 if (u > 2.0) a->n2++;
 if (u > a->max) { a->max = u; a->at = x; a->at2 = y; } }
static void report(const char *nm, struct acc *a, int two) {
 printf("%-6s max %8.3f ulp at %-24.17g", nm, a->max, a->at);
 if (two) printf(" y=%-24.17g", a->at2);
 printf("  >0.5: %ld/%ld  >1: %ld  >2: %ld\n", a->n05, a->n, a->n1, a->n2); }

typedef double (*f1)(double);
static void sweep1(const char *nm, f1 f, f1 oracle, int elo, int ehi, int sign, long n) {
 struct acc a = {0};
 for (long i = 0; i < n; i++) { double x = rmag(elo, ehi, sign); tally(&a, x, 0, f(x), oracle(x)); }
 static const double edges[] = { 0.0, -0.0, 1.0, -1.0, 0.5, 2.0, 1e-300, 4.9e-324, 1.7e308,
   3.141592653589793, 1.5707963267948966, 0.6931471805599453, 2.718281828459045 };
 for (unsigned i = 0; i < sizeof edges / sizeof *edges; i++)
  tally(&a, edges[i], 0, f(edges[i]), oracle(edges[i]));
 report(nm, &a, 0); }

// the reduction-quality scan: sin ulp by argument MAGNITUDE band (the stance data)
static void redscan(void) {
 puts("-- sin ulp by |x| band (the reduction stance scan) --");
 for (int e = 0; e <= 1020; e += (e < 40 ? 4 : 60)) {
  struct acc a = {0};
  for (long i = 0; i < 20000; i++) { double x = rmag(e, e + 4, 1); tally(&a, x, 0, am_sin(x), sin(x)); }
  printf("  2^%-4d max %10.3f ulp  >1: %ld/%ld\n", e, a.max, a.n1, a.n); } }

int main(int argc, char **argv) {
 long n = 2000000;
 if (argc > 1 && !strcmp(argv[1], "reduce")) { redscan(); return 0; }
 sweep1("sqrt", am_sqrt, sqrt, -1022, 1024, 0, n);
 sweep1("exp",  am_exp,  exp,  -12, 10, 1, n);       // useful exp range (past it: over/underflow)
 sweep1("exp2", am_exp,  exp,  -1022, -12, 1, n / 4); // tiny args (result ~1)
 sweep1("log",  am_log,  log,  -1022, 1024, 0, n);
 sweep1("logn1",am_log,  log,  -4, 2, 0, n);
 sweep1("sin",  am_sin,  sin,  -4, 8, 1, n);
 sweep1("cos",  am_cos,  cos,  -4, 8, 1, n);
 { struct acc a = {0};                                  // atan2: both args swept
   for (long i = 0; i < n; i++) { double yy = rmag(-40, 40, 1), xx = rmag(-40, 40, 1);
     tally(&a, yy, xx, am_atan2(yy, xx), atan2(yy, xx)); }
   report("atan2", &a, 1); }
 { struct acc a = {0};                                  // pow: x > 0, y modest
   for (long i = 0; i < n; i++) { double xx = rmag(-40, 40, 0), yy = rmag(-6, 8, 1);
     double w = yy * log(xx); if (w > 700 || w < -730) continue;
     tally(&a, xx, yy, am_pow(xx, yy), pow(xx, yy)); }
   report("pow", &a, 1); }
 { struct acc a = {0};                                  // pow near the overflow rim
   for (long i = 0; i < n / 4; i++) { double xx = rmag(-2, 2, 0), yy = rmag(8, 11, 1);
     double w = yy * log(xx); if (w > 700 || w < -730) continue;
     tally(&a, xx, yy, am_pow(xx, yy), pow(xx, yy)); }
   report("powrim", &a, 1); }
 return 0; }
