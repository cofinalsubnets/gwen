// crew/moon/lib/math/am.c -- the math floor: our own transcendentals, ONE file on
// every frontend (host, love0, kernel, wasm, the gcc-free raw build -- the last
// vendored math, fdlibm, retired here). binary64, portable C in the mooncc subset:
// unions for bit access, uint64 arithmetic, no fma/int128/builtins. coefficients
// are TAYLOR/exact-rational (re-derivable, no minimax magic) at mpmath-verified
// double roundings; constants carry hi/lo splits where a product must stay exact.
//
// the surface is the seven love.c consumes (its ai_* defines): am_sqrt EXACT
// (IEEE-correct rounding); am_exp/am_log <= 1 ulp; am_sin/am_cos <= 2 ulp at
// EVERY magnitude (compact Payne-Hanek -- no domain stance); am_atan2 <= 3 ulp;
// am_pow <= 2 ulp typical, degrading ~linearly in |y ln x| toward the
// representable rim (~40 ulp at 1e+-300) -- the one documented stance -- with
// the algebraic exponents (y in {2, 1, -1, 1/2}) EXACT: the spec's power-is-
// application identities ride am_pow. measured against glibc by the differential
// ulp harness (2M+ deterministic samples a function, mk/tools/ulp.c), and the
// mooncc-compiled object is GATED to measure identical to the system cc's --
// test_ulp, both sweep modes. that second half was a hand check and a header
// claim until 2026-07-29, when it turned out to be false and two mooncc bugs
// were living behind it (a lost 4th parameter segfaulting am_sin above 2^19,
// and an unsigned 64-bit word converting to double SIGNED, which cost rbig
// 1609 ulp). float bits are where a codegen fault hides best: the corpus, the
// C battery and every other gate stayed green through both.
#include <stdint.h>

typedef union { double d; uint64_t u; } db;
static double mkd(uint64_t u) { db b; b.u = u; return b.d; }
static uint64_t mku(double d) { db b; b.d = d; return b.u; }

#define D_NAN  mkd(0x7ff8000000000000ull)
#define D_INF  mkd(0x7ff0000000000000ull)

// -- Dekker: split a double into 26+27 bit halves so products are exact --
static void dsplit(double a, double *hi, double *lo) {
 double c = a * 0x8000001p0;                     // a * (2^27 + 1)
 *hi = c - (c - a);
 *lo = a - *hi; }
// exact a*b as hi+lo
static void dmul(double a, double b, double *hi, double *lo) {
 double ah, al, bh, bl;
 dsplit(a, &ah, &al); dsplit(b, &bh, &bl);
 *hi = a * b;
 *lo = ((ah * bh - *hi) + ah * bl + al * bh) + al * bl; }

// ============================== sqrt ==============================
// Newton in double from a bit-level seed, then a CORRECT-ROUNDING fixup:
// candidates y and its ulp neighbor, pick whichever square (evaluated
// exactly via Dekker) sits nearer x. Ties are perfect squares (r == 0).
double am_sqrt(double x) {
 uint64_t ux = mku(x);
 if (x != x) return D_NAN;
 if (ux == 0 || ux == 0x8000000000000000ull) return x;      // +-0
 if (ux >> 63) return D_NAN;                                 // negative
 if ((ux >> 52) == 0x7ff) return x;                          // +inf
 int scale = 0;
 // pre-scale ALL tiny x (denormals included): the rounding fixup's Dekker
 // products must stay NORMAL (y^2's low half sits ~80 bits under x -- an
 // x below ~2^-512 would sink it denormal and the exactness with it)
 if (x < 0x1p-512) { x *= 0x1p540; ux = mku(x); scale = -270; }
 // seed: halve the exponent field (a ~5-bit guess), sharpen by Newton
 uint64_t uy = ((ux >> 1) + (0x3ffull << 51)) & 0x7fffffffffffffffull;
 double y = mkd(uy);
 y = 0.5 * (y + x / y);
 y = 0.5 * (y + x / y);
 y = 0.5 * (y + x / y);
 y = 0.5 * (y + x / y);                                      // within ~1 ulp now
 // fixup: r = x - y*y exactly; step toward x if the neighbor is nearer
 double ph, pl, r, r2, yn;
 for (int i = 0; i < 4; i++) {                               // walk to the nearest-square neighbor
  dmul(y, y, &ph, &pl);
  r = (x - ph) - pl;
  if (r == 0) break;
  yn = mkd(mku(y) + (r > 0 ? 1 : (uint64_t) -1));
  dmul(yn, yn, &ph, &pl);
  r2 = (x - ph) - pl;
  double ar = r < 0 ? -r : r, ar2 = r2 < 0 ? -r2 : r2;
  if (ar2 < ar || (ar2 == ar && !(mku(yn) & 1))) y = yn;     // nearer, or the even side of a tie
  else break;
 }
 return scale ? y * 0x1p-270 : y; }

// ============================== exp ==============================
// x = k ln2 + r, |r| <= ln2/2; e^r by Taylor to r^13 (tail < 2^-57), then
// scale by 2^k in the exponent bits (two-step near the denormal floor).
static const double LN2_HI = 0x1.62e42f8000000p-1, LN2_LO = 0x1.be8e7bcd5e4f2p-27,
                    LOG2E  = 0x1.71547652b82fep+0;
static double scale2k(double y, int k) {                     // y * 2^k, k in [-2098, 2098]
 while (k > 1023)  { y *= 0x1p1023;  k -= 1023; }
 while (k < -1022) { y *= 0x1p-1022; k += 1022; }
 return y * mkd((uint64_t)(k + 1023) << 52); }
double am_exp(double x) {
 if (x != x) return x;
 if (x >  709.782712893383996) return D_INF;                 // overflow
 if (x < -745.133219101941222) return 0.0;                   // underflow to 0
 int k = (int)(x * LOG2E + (x > 0 ? 0.5 : -0.5));
 double r = (x - k * LN2_HI) - k * LN2_LO;
 double z = r * r;
 double p = r + z * (0x1.0000000000000p-1
       + r * (0x1.5555555555555p-3
       + r * (0x1.5555555555555p-5
       + r * (0x1.1111111111111p-7
       + r * (0x1.6c16c16c16c17p-10
       + r * (0x1.a01a01a01a01ap-13
       + r * (0x1.a01a01a01a01ap-16
       + r * (0x1.71de3a556c734p-19
       + r * (0x1.27e4fb7789f5cp-22
       + r * (0x1.ae64567f544e4p-26
       + r * (0x1.1eed8eff8d898p-29
       + r *  0x1.6124613a86d09p-33)))))))))));
 return scale2k(1.0 + p, k); }

// ============================== log ==============================
// x = 2^k m, m in [sqrt2/2, sqrt2); s = (m-1)/(m+1); ln m = 2 atanh s =
// 2s(1 + s^2/3 + ... + s^22/23); result k ln2_hi + (ln m + k ln2_lo).
double am_log(double x) {
 uint64_t ux = mku(x);
 if (x != x) return x;
 if (ux == 0 || ux == 0x8000000000000000ull) return -D_INF;  // +-0 -> -inf
 if (ux >> 63) return D_NAN;                                 // negative
 if ((ux >> 52) == 0x7ff) return x;                          // +inf
 int k = 0;
 if (ux < (1ull << 52)) { x *= 0x1p54; ux = mku(x); k = -54; }  // denormal
 k += (int)(ux >> 52) - 1023;
 double m = mkd((ux & 0xfffffffffffffull) | (0x3ffull << 52));       // m in [1, 2)
 if ((ux & 0xfffffffffffffull) > 0x6a09e667f3bcdull) { m *= 0.5; k += 1; }  // -> [sqrt2/2, sqrt2)
 double f = m - 1.0;
 double s = f / (2.0 + f);
 double slo = ((f - 2.0 * s) - s * f) / (2.0 + f);           // the division's residue
 double z = s * s;
 double p = 0x1.5555555555555p-2
      + z * (0x1.999999999999ap-3
      + z * (0x1.2492492492492p-3
      + z * (0x1.c71c71c71c71cp-4
      + z * (0x1.745d1745d1746p-4
      + z * (0x1.3b13b13b13b14p-4
      + z * (0x1.1111111111111p-4
      + z * (0x1.e1e1e1e1e1e1ep-5
      + z * (0x1.af286bca1af28p-5
      + z * (0x1.8618618618618p-5
      + z *  0x1.642c8590b2164p-5)))))))));
 double lnm = 2.0 * s + (2.0 * slo + 2.0 * s * z * p);
 return k * LN2_HI + (lnm + k * LN2_LO); }

// ============================ sin / cos ============================
// reduction: j = round(x 2/pi), r = x - j pi/2 by a THREE-PART Cody-Waite
// (33+33+rest bits), remainder carried hi+lo; kernels are Taylor to r^17 /
// r^16 on |r| <= pi/4 with the lo word folded in linearly. the reduction is
// honest to a MEASURED |x| bound (the redscan) -- that bound is the stance.
static const double INVPIO2 = 0x1.45f306dc9c883p-1,
  PIO2_1 = 0x1.921fb54400000p+0, PIO2_2 = 0x1.0b4611a600000p-34,
  PIO2_3 = 0x1.3198a2e037073p-69;
// reduce x to (r, rlo) with quadrant j; answers j (0 on the no-reduce fast path)
// 64x64 -> 128 without __int128: 32-bit halves
static void mul64(uint64_t a, uint64_t b, uint64_t *hi, uint64_t *lo) {
 uint64_t ah = a >> 32, al = a & 0xffffffffull, bh = b >> 32, bl = b & 0xffffffffull;
 uint64_t p0 = al * bl, p1 = al * bh, p2 = ah * bl, p3 = ah * bh;
 uint64_t mid = p1 + (p0 >> 32);
 uint64_t c = mid < p1 ? (1ull << 32) : 0;   // a mid wrap weighs 2^96 = 2^32 in hi units
 mid += p2;
 if (mid < p2) c += 1ull << 32;
 *hi = p3 + (mid >> 32) + c;
 *lo = (mid << 32) | (p0 & 0xffffffffull); }

// Payne-Hanek: (|x| * 2/pi) mod 4 from the stored bit stream of 2/pi, to 192
// fraction bits -- honest reduction at ANY magnitude. x = m 2^(E-52); the
// product's bits around the binary point come from a 3-word window of the
// stream at offset E-55; nearest-multiple rounding leaves |r| <= pi/4.
static const uint64_t TPB[] = {          // 2/pi binary fraction, 64-bit words
 0xa2f9836e4e441529ull, 0xfc2757d1f534ddc0ull, 0xdb6295993c439041ull,
 0xfe5163abdebbc561ull, 0xb7246e3a424dd2e0ull, 0x06492eea09d1921cull,
 0xfe1deb1cb129a73eull, 0xe88235f52ebb4484ull, 0xe99c7026b45f7e41ull,
 0x3991d639835339f4ull, 0x9c845f8bbdf9283bull, 0x1ff897ffde05980full,
 0xef2f118b5a0a6d1full, 0x6d367ecf27cb09b7ull, 0x4f463f669e5fea2dull,
 0x7527bac7ebe5f17bull, 0x3d0739f78a5292eaull, 0x6bfb5fb11f8d5d08ull,
 0x56033046fc7b6babull, 0xf0cfbc209af4361dull, 0xa9e391615ee61b08ull,
 0x6599855f14a06840ull };
static const double PIO2_HI = 0x1.921fb54442d18p+0, PIO2_LO = 0x1.1a62633145c07p-54;
static uint64_t tpw(int i) { return (i < 0 || i >= (int)(sizeof TPB / sizeof *TPB)) ? 0 : TPB[i]; }
static int rbig(double ax, double *r, double *rlo) {
 uint64_t ux = mku(ax);
 uint64_t m = (ux & 0xfffffffffffffull) | (1ull << 52);      // 53-bit mantissa
 int E = (int)(ux >> 52) - 1023;
 int S = E - 55;                                             // window offset: 3 guard bits above mod-4
 int q = S >> 6, b = S & 63;                                 // word / bit split (S may be negative)
 uint64_t W0, W1, W2;
 if (b) { W0 = (tpw(q) << b) | (tpw(q + 1) >> (64 - b));
          W1 = (tpw(q + 1) << b) | (tpw(q + 2) >> (64 - b));
          W2 = (tpw(q + 2) << b) | (tpw(q + 3) >> (64 - b)); }
 else   { W0 = tpw(q); W1 = tpw(q + 1); W2 = tpw(q + 2); }
 uint64_t h0, l0, h1, l1, h2, l2;
 mul64(m, W0, &h0, &l0);
 mul64(m, W1, &h1, &l1);
 mul64(m, W2, &h2, &l2);
 uint64_t F0 = l0 + h1, cI = F0 < l0 ? 1 : 0;                // fixed point after I
 uint64_t F1 = l1 + h2, F2 = l2;
 if (F1 < l1) { F0 += 1; cI += F0 == 0; }                    // carries, in order
 uint64_t I = h0 + cI;
 // times 8 (the 3 guard bits): quadrant = top 2, fraction slides up
 int j = (int)(((I << 3) | (F0 >> 61)) & 3);
 uint64_t G0 = (F0 << 3) | (F1 >> 61), G1 = (F1 << 3) | (F2 >> 61), G2 = F2 << 3;
 int neg = 0;
 if (G0 >> 63) {                                             // frac >= 1/2: round to the NEXT multiple
  j = (j + 1) & 3; neg = 1;
  G2 = ~G2 + 1; G1 = ~G1 + (G2 == 0); G0 = ~G0 + (G1 == 0 && G2 == 0); }
 double fh = (double) G0 * 0x1p-64 + (double) G1 * 0x1p-128; // |frac| as hi+low tail
 double ft = (double) G2 * 0x1p-192;
 double sh = fh + ft, sl = (fh - sh) + ft;                   // normalize the pair
 if (neg) { sh = -sh; sl = -sl; }
 double ph, pl;                                              // r = frac * pi/2, in dd
 dmul(sh, PIO2_HI, &ph, &pl);
 *r = ph;
 *rlo = pl + sh * PIO2_LO + sl * PIO2_HI;
 double rr = *r + *rlo;                                      // renormalize
 *rlo = (*r - rr) + *rlo;
 *r = rr;
 return j; }
static int rpio2(double x, double *r, double *rlo) {
 double ax = x < 0 ? -x : x;
 if (ax <= 0x1.921fb54442d18p-1) { *r = x; *rlo = 0; return 0; }   // |x| <= pi/4
 if (ax >= 0x1p19) {                                         // Cody-Waite's honest bound: hand off
  int j = rbig(ax, r, rlo);
  if (x < 0) { *r = -*r; *rlo = -*rlo; j = (4 - j) & 3; }    // sin/cos symmetry: reduce |x|, flip
  return j; }
 double dj = x * INVPIO2 + (x > 0 ? 0.5 : -0.5);
 int64_t j = (int64_t) dj;
 double fj = (double) j;
 double w1 = x - fj * PIO2_1;                  // exact: j < 2^20 against 33-bit parts
 double w2 = w1 - fj * PIO2_2;
 double t  = fj * PIO2_3;
 *r = w2 - t;
 *rlo = (w2 - *r) - t;
 return (int)(j & 3); }
// sin on the reduced (r, rlo), |r| <= ~pi/4
static double ksin(double r, double rlo) {
 double z = r * r;
 double p = 0x1.1111111111111p-7
      + z * (-0x1.a01a01a01a01ap-13
      + z * (0x1.71de3a556c734p-19
      + z * (-0x1.ae64567f544e4p-26
      + z * (0x1.6124613a86d09p-33
      + z * (-0x1.ae7f3e733b81fp-41
      + z *  0x1.952c77030ad4ap-49)))));
 return r + (z * (r * (-0x1.5555555555555p-3 + z * p)) + rlo); }
// cos on the reduced (r, rlo)
static double kcos(double r, double rlo) {
 double z = r * r;
 double p = 0x1.5555555555555p-5
      + z * (-0x1.6c16c16c16c17p-10
      + z * (0x1.a01a01a01a01ap-16
      + z * (-0x1.27e4fb7789f5cp-22
      + z * (0x1.1eed8eff8d898p-29
      + z * (-0x1.93974a8c07c9dp-37
      + z *  0x1.ae7f3e733b81fp-45)))));
 double hz = 0.5 * z;
 return (1.0 - hz) + (z * (z * p) - r * rlo);
}
double am_sin(double x) {
 if (x != x) return x;
 uint64_t ax = mku(x) & 0x7fffffffffffffffull;
 if (ax >= 0x7ff0000000000000ull) return D_NAN;              // +-inf
 double r, rl;
 int j = rpio2(x, &r, &rl);
 switch (j) {
  case 0: return ksin(r, rl);
  case 1: return kcos(r, rl);
  case 2: return -ksin(r, rl);
  default: return -kcos(r, rl); } }
double am_cos(double x) {
 if (x != x) return x;
 uint64_t ax = mku(x) & 0x7fffffffffffffffull;
 if (ax >= 0x7ff0000000000000ull) return D_NAN;
 double r, rl;
 int j = rpio2(x, &r, &rl);
 switch (j) {
  case 0: return kcos(r, rl);
  case 1: return -ksin(r, rl);
  case 2: return -kcos(r, rl);
  default: return ksin(r, rl); } }

// ============================ atan / atan2 ============================
// t in [0,1] reduces against the nearest center c in {0, 1/4, 1/2, 3/4, 1}:
// u = (t-c)/(1+ct) keeps |u| <= ~1/7, and a 10-term alternating Taylor closes
// under a half ulp; t > 1 flips through pi/2 - atan(1/t). centers carry hi/lo.
static const double ATC_H[] = { 0.0, 0x1.f5b75f92c80ddp-3, 0x1.dac670561bb4fp-2,
  0x1.4978fa3269ee1p-1, 0x1.921fb54442d18p-1 };
static const double ATC_L[] = { 0.0, 0x1.8ab6e3cf7afbdp-57, 0x1.a2b7f222f65e2p-56,
  0x1.2419a87f2a458p-56, 0x1.1a62633145c07p-55 };
static const double CVAL[] = { 0.0, 0.25, 0.5, 0.75, 1.0 };
static double katan(double t) {                              // t in [0, 1]
 int i = t < 0.125 ? 0 : t < 0.375 ? 1 : t < 0.625 ? 2 : t < 0.875 ? 3 : 4;
 double u = i ? (t - CVAL[i]) / (1.0 + CVAL[i] * t) : t;
 double z = u * u;
 double p = -0x1.5555555555555p-2
      + z * (0x1.999999999999ap-3
      + z * (-0x1.2492492492492p-3
      + z * (0x1.c71c71c71c71cp-4
      + z * (-0x1.745d1745d1746p-4
      + z * (0x1.3b13b13b13b14p-4
      + z * (-0x1.1111111111111p-4
      + z * (0x1.e1e1e1e1e1e1ep-5
      + z * (-0x1.af286bca1af28p-5
      + z *  0x1.8618618618618p-5))))))));
 double a = u + u * z * p;
 return i ? ATC_H[i] + (ATC_L[i] + a) : a; }
static double am_atan(double x) {
 uint64_t ax = mku(x) & 0x7fffffffffffffffull;
 if (x != x) return x;
 double t = mkd(ax), r;
 if (ax >= 0x7ff0000000000000ull) r = 0x1.921fb54442d18p+0;  // +-inf -> +-pi/2
 else if (t <= 1.0) r = katan(t);
 else r = 0x1.921fb54442d18p+0 + (0x1.1a62633145c07p-54 - katan(1.0 / t));
 return x < 0 ? -r : r; }
static const double PI_H = 0x1.921fb54442d18p+1, PI_L = 0x1.1a62633145c07p-53;
double am_atan2(double y, double x) {
 if (x != x || y != y) return D_NAN;
 uint64_t uy = mku(y), ux2 = mku(x);
 int sy = (int)(uy >> 63), sx = (int)(ux2 >> 63);
 uint64_t ay = uy & 0x7fffffffffffffffull, ax = ux2 & 0x7fffffffffffffffull;
 if (ay == 0)                                                // y = +-0
  return sx ? (sy ? -PI_H : PI_H) : y;                       // x<0: +-pi; x>=0: +-0
 if (ax == 0) return sy ? -0x1.921fb54442d18p+0 : 0x1.921fb54442d18p+0;
 if (ax == 0x7ff0000000000000ull) {                          // x = +-inf
  double q = ay == 0x7ff0000000000000ull ? (sx ? 3 * PI_H / 4 : PI_H / 4) : (sx ? PI_H : 0.0);
  return sy ? -q : q; }
 if (ay == 0x7ff0000000000000ull) return sy ? -0x1.921fb54442d18p+0 : 0x1.921fb54442d18p+0;
 double a = am_atan(mkd(ay) / mkd(ax));                      // |y/x| angle in [0, pi/2]
 if (sx) a = PI_H - (a - PI_L);                              // second quadrant
 return sy ? -a : a; }

// ============================== pow ==============================
// |x|^y = exp(y ln|x|) with the log carried HI+LO (the series' s already has
// its division residue; k ln2 is exact by the split), y*ln in dd, and exp
// taking the lo linearly. sign/edge cases walk the IEEE table first.
static int am_oddint(double y) {                             // 2 even int, 1 odd int, 0 not int
 if (y != y || y - y != 0) return 0;                         // nan/inf
 double ay = y < 0 ? -y : y;
 if (ay >= 0x1p53) return 2;                                 // huge: every double there is even
 int64_t i = (int64_t) y;
 if ((double) i != y) return 0;
 return (i & 1) ? 1 : 2; }
static void dd_log(double x, double *hi, double *lo) {       // x > 0, finite: ln x as hi+lo
 uint64_t ux = mku(x);
 int k = 0;
 if (ux < (1ull << 52)) { x *= 0x1p54; ux = mku(x); k = -54; }
 k += (int)(ux >> 52) - 1023;
 double m = mkd((ux & 0xfffffffffffffull) | (0x3ffull << 52));
 if ((ux & 0xfffffffffffffull) > 0x6a09e667f3bcdull) { m *= 0.5; k += 1; }
 double f = m - 1.0;
 double s = f / (2.0 + f);
 double sfh, sfl;                                            // s*f EXACTLY: the residue below is a
 dmul(s, f, &sfh, &sfl);                                     // cancellation at s*f's rounding scale
 double slo = (((f - 2.0 * s) - sfh) - sfl) / (2.0 + f);
 double z = s * s;
 double p = 0x1.5555555555555p-2
      + z * (0x1.999999999999ap-3
      + z * (0x1.2492492492492p-3
      + z * (0x1.c71c71c71c71cp-4
      + z * (0x1.745d1745d1746p-4
      + z * (0x1.3b13b13b13b14p-4
      + z * (0x1.1111111111111p-4
      + z * (0x1.e1e1e1e1e1e1ep-5
      + z * (0x1.af286bca1af28p-5
      + z * (0x1.8618618618618p-5
      + z *  0x1.642c8590b2164p-5)))))))));
 double t = 2.0 * s, tl = 2.0 * slo + t * z * p;             // lnm = t + tl
 double h = k * LN2_HI + t;                                  // exact k ln2_hi; t <= 0.7
 double e = (k * LN2_HI - h) + t;                            // the add's residue
 *hi = h;
 *lo = e + tl + k * LN2_LO; }
double am_pow(double x, double y) {
 if (y == 0.0) return 1.0;
 if (x == 1.0) return 1.0;
 if (x != x || y != y) return D_NAN;
 uint64_t ux = mku(x), uy = mku(y);
 uint64_t axb = ux & 0x7fffffffffffffffull, ayb = uy & 0x7fffffffffffffffull;
 int oi = am_oddint(y);
 if (ayb == 0x7ff0000000000000ull) {                         // y = +-inf
  if (axb == 0x3ff0000000000000ull) return 1.0;              // |x| = 1
  int big = axb > 0x3ff0000000000000ull;
  return (uy >> 63) ? (big ? 0.0 : D_INF) : (big ? D_INF : 0.0); }
 if (axb == 0) {                                             // x = +-0
  double z = (uy >> 63) ? D_INF : 0.0;
  return ((ux >> 63) && oi == 1) ? -z : z; }
 if (axb == 0x7ff0000000000000ull) {                         // x = +-inf
  double z = (uy >> 63) ? 0.0 : D_INF;
  return ((ux >> 63) && oi == 1) ? -z : z; }
 int neg = 0;
 if (ux >> 63) {                                             // negative base: integer y only
  if (!oi) return D_NAN;
  neg = oi == 1;
  x = -x; }
 // the algebraic exponents answer EXACTLY (power is application: the spec's
 // ((/ 1 2) 9) = 3.0 rides here) -- and skip the whole log/exp tower
 if (y == 2.0)  return x * x;
 if (y == 1.0)  return neg ? -x : x;
 if (y == -1.0) return neg ? -1.0 / x : 1.0 / x;
 if (y == 0.5)  return am_sqrt(x);                           // x > 0 here
 double lh, ll;
 dd_log(x, &lh, &ll);
 double wh, wl, qh, ql;                                      // w = y * ln x, dd
 dmul(y, lh, &wh, &wl);
 dmul(y, ll, &qh, &ql);                                      // exact: the lo product matters at the rim
 wl += qh + ql;
 double w = wh + wl, we = (wh - w) + wl;                     // renormalize: |we| <= ulp(w)
 if (w >  709.782712893383996) return neg ? -D_INF : D_INF;
 if (w < -745.133219101941222) return neg ? -0.0 : 0.0;
 double e = am_exp(w);
 e = e + e * we;                                             // now the linear step is honest
 return neg ? -e : e; }

// -- the float twins (the 32-bit lane: wasm, the MCUs): compute in binary64,
// narrow once -- correct within a float ulp, and the dispatch can still take
// their ADDRESS (love.c hands ai_sin to lvm_math1 as a pointer, so the 32-bit
// ai_* must be real functions, not casting macros). --
float am_sinf(float x) { return (float) am_sin(x); }
float am_cosf(float x) { return (float) am_cos(x); }
float am_atan2f(float y, float x) { return (float) am_atan2(y, x); }
float am_sqrtf(float x) { return (float) am_sqrt(x); }
float am_expf(float x) { return (float) am_exp(x); }
float am_logf(float x) { return (float) am_log(x); }
float am_powf(float x, float y) { return (float) am_pow(x, y); }


// -- am_strtod: correctly rounded decimal -> binary64, the READ half of the
// shortest-roundtrip pair (love.c's printer is the SHOW half; the two share
// one mental model). a naive seed lands within a bounded ulp distance; the
// walk then compares the INPUT DIGITS -- exactly, as decimal integers under
// x2/x5 carry walks, no allocation -- against each candidate's binary
// rounding interval (the midpoints 4m-2/4m+2, 4m-1 at a binade floor, at
// 2^(e-2); closed on an even mantissa, the printer's own predicate) and
// steps one ulp at a time by bit arithmetic until the input falls inside.
// exact and the same answer on every target: glibc's strtod leaves the
// trusted base, and the naive accumulators that read "0.3" one ulp off
// (free/libc/str.c, crew/moon/lib/nolibc.c) both delegate here now.
enum { am_dgmax = 800 };   // a boundary expansion: 17 digits + one per x5 step (<= 1076)
static void am_dgmul(unsigned char *d, int *n, int k) {   // k = 2 or 5
 int c = 0;
 for (int i = 0; i < *n; i++) { int t = d[i] * k + c; d[i] = t % 10; c = t / 10; }
 while (c) d[(*n)++] = c % 10, c /= 10; }
static void am_dgexp(unsigned char *d, int *n, uint64_t m, int e2) {
 *n = 0;
 if (!m) d[(*n)++] = 0;
 while (m) d[(*n)++] = m % 10, m /= 10;
 for (; e2 > 0; e2--) am_dgmul(d, n, 2);
 for (; e2 < 0; e2++) am_dgmul(d, n, 5); }
// input digits a (LSB first, shifted up by sh virtual zeros, sticky marking a
// truncated nonzero tail) vs boundary digits b: -1 / 0 / +1
static int am_dgcmp(unsigned char const *a, int na, int sh, int sticky,
                    unsigned char const *b, int nb) {
 if (sh < 0) {                                   // mirrored: shift b instead
  int r = am_dgcmp(b, nb, -sh, 0, a, na);
  return r ? -r : (sticky ? 1 : 0); }
 if (na + sh != nb) return na + sh < nb ? -1 : 1;
 for (int i = nb - 1; i >= 0; i--) {
  int da = i >= sh ? a[i - sh] : 0;
  if (da != b[i]) return da < b[i] ? -1 : 1; }
 return sticky ? 1 : 0; }
double am_strtod(char const *s, char **end) {
 char const *p = s;
 int sign = 1;
 if (*p == '-') sign = -1, p++;
 else if (*p == '+') p++;
 // significant digits: up to 780 kept exactly (din, MSB collected then
 // reversed -- correct rounding can genuinely need ~768 digits; a mere sticky
 // bit cannot carry a truncated tail's MAGNITUDE through a mid-comparison,
 // only break an exact tie, and the 3.2M-parse differential found the 1-ulp
 // misses that proves). beyond 780 the tail folds to sticky, which by the
 // worst-case bound can then only break ties. fr counts post-point digits.
 unsigned char din[790];
 int any = 0, pt = 0, fr = 0, nin = 0, extra = 0, sticky = 0;
 double v0 = 0;
 for (;; p++) {
  if (*p == '.' && !pt) { pt = 1; continue; }
  if (*p < '0' || *p > '9') break;
  any = 1;
  if (pt) fr++;
  if (*p == '0' && !nin) continue;               // leading zeros: scale only (fr keeps them)
  if (nin < 780) {
   din[nin++] = (unsigned char) (*p - '0');      // MSB first while collecting
   if (nin <= 19) v0 = v0 * 10 + (*p - '0'); }
  else extra++, sticky |= *p != '0'; }
 for (int i = 0, j = nin - 1; i < j; i++, j--) {  // flip to LSB first for the compares
  unsigned char t = din[i]; din[i] = din[j]; din[j] = t; }
 if (!any) { if (end) *end = (char*) s; return 0; }
 if (end) *end = (char*) p;
 int e10 = 0;
 if (*p == 'e' || *p == 'E') {
  char const *q = p++;
  int esign = 1;
  if (*p == '-') esign = -1, p++;
  else if (*p == '+') p++;
  if (!('0' <= *p && *p <= '9')) p = q;
  else {
   while ('0' <= *p && *p <= '9') { if (e10 < 100000) e10 = e10 * 10 + (*p - '0'); p++; }
   e10 *= esign;
   if (end) *end = (char*) p; } }
 if (!nin) return sign > 0 ? 0.0 : -0.0;         // all zeros
 int kk = e10 - fr + extra;                      // value = din x 10^kk exactly (+ sticky)
 int mag = nin + kk;                             // value in [10^(mag-1), 10^mag)
 if (mag > 310)  return sign * D_INF;
 if (mag < -342) return sign > 0 ? 0.0 : -0.0;
 // the seed: v0 holds only the TOP 19 kept digits, so its scale carries the
 // rest of din's length; x 10 in bounded chunks (gradual under/overflow lands close)
 { int e = kk + (nin > 19 ? nin - 19 : 0);
   while (e >= 300)  v0 *= 1e300, e -= 300;
   while (e <= -300) v0 /= 1e300, e += 300;
   double sc = 1;
   for (int a = e < 0 ? -e : e; a > 0; a--) sc *= 10;
   v0 = e < 0 ? v0 / sc : v0 * sc; }
 db b; b.d = v0;
 if (b.u >= 0x7ff0000000000000ull) b.u = 0x7fefffffffffffffull;   // seed stays finite
 unsigned char dg[am_dgmax]; int ndg;
 for (;;) {
  uint64_t mant = b.u & ((1ull << 52) - 1);
  int be = (int) (b.u >> 52);
  uint64_t m = be ? mant | (1ull << 52) : mant;
  int e2 = (be ? be : 1) - 1023 - 52;
  int lo2 = be > 1 && mant == 0;                 // binade floor: the gap below halves
  int even = !(m & 1);
  int scb = e2 - 2 < 0 ? e2 - 2 : 0, sh = kk - scb;
  int cl, ch;
  if (!m) cl = 1;                                // zero: any positive input clears its floor
  else {
   am_dgexp(dg, &ndg, 4 * m - (lo2 ? 1 : 2), e2 - 2);
   cl = am_dgcmp(din, nin, sh, sticky, dg, ndg); }
  am_dgexp(dg, &ndg, m ? 4 * m + 2 : 2, e2 - 2);
  ch = am_dgcmp(din, nin, sh, sticky, dg, ndg);
  if ((cl > 0 || (cl == 0 && even)) && (ch < 0 || (ch == 0 && even))) break;
  if (ch >= 0) {                                 // at or past the upper midpoint: up one ulp
   if (b.u >= 0x7fefffffffffffffull) { b.u = 0x7ff0000000000000ull; break; }
   b.u++; }
  else {                                         // under the lower midpoint: down one ulp
   if (!b.u) break;                              // beneath half the least denormal: zero
   b.u--; } }
 return sign * b.d; }
