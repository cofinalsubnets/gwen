// num.c -- big. one translation unit of the runtime;
// the shared layouts and the cross-TU seam are src/love_int.h.
#include "love_int.h"
// this file's own, forward-declared so order within it does not matter.
static ai_dlimb div128by64(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem);
static ai_limb div2by1(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem);
static ai_limb rdigit(char c);
static ai_noinline bool vquot_needs_float(word a, word b);
static ai_noinline int mag_add(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb);
static ai_noinline int mag_cmp(ai_limb const *a, int na, ai_limb const *b, int nb);
static ai_noinline int mag_mul_add_small(ai_limb *a, int n, ai_limb mul, ai_limb add);
static ai_noinline int mag_sub(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb);
static ai_noinline void mag_divmod(ai_limb *q, ai_limb *r,
  ai_limb const *u, int m, ai_limb const *v, int n, ai_limb *un, ai_limb *vn);
static ai_noinline void mag_mul(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb);
static ai_noinline void vbin_fill(struct ai_tray *r, word a, word b, int op, bool fdom);
static ai_noinline void vmap1_fill(struct ai_tray *r, struct ai_tray *a, ai_flo_t (*fn)(ai_flo_t));
static ai_noinline void vmap2_fill(struct ai_tray *r, word a, word b, ai_flo_t (*fn)(ai_flo_t, ai_flo_t));
bool ratio_ifit(word x, int64_t *v);
bool ratio_iview(word x, int64_t *n, int64_t *d);
static bool ratio_xcmp(int64_t n1, int64_t d1, int64_t n2, int64_t d2, intptr_t *c);
static int big_mul_mag(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb, ai_limb *t);
int big_nlimbs(word x);
int cmp_rank(struct ai *g, word x);
int load_int_mag(word x, ai_limb scratch[wlimbs], ai_limb const **out, bool *neg);
static int mag_copy(ai_limb *dst, ai_limb const *src, int n);
static intptr_t bytes_cmp(const char *pa, uintptr_t la, const char *pb, uintptr_t lb);
static intptr_t galaxy_tie(struct ai_tray *va, struct ai_tray *vb);
static intptr_t mint_cmp(struct ai *g, word a, word b);
static intptr_t vcmp_sign(int op, int s);
static intptr_t vop_int(int op, intptr_t a, intptr_t b);
static lvm(lvm_aextreme);
static lvm(lvm_cmp_ord);
static struct ai *ai_bdiv_setup(struct ai *g, int which);
static struct ai *ai_bmul_setup(struct ai *g);
static struct ai *ai_kmul_setup(struct ai *g);
static struct ai *big_read_radix(struct ai *g, ai_limb radix, int chunk, uintptr_t pfx);
struct ai_zn tray_cell_zn(struct ai_tray *v, uintptr_t i);
static uintptr_t bdim(uintptr_t da, uintptr_t db);
union u *as_big(ai_word **hp, word x);
static void big_addsub(ai_limb *r, int *rn, bool *rneg,
  ai_limb const *a, int na, bool nega, ai_limb const *b, int nb, bool negb, bool subtract);
static void mag_add_off(ai_limb *r, int rn, ai_limb const *s, int sn, int off);
static void mag_mul_kara(ai_limb *r, ai_limb const *a, ai_limb const *b, int n, ai_limb *t);
void ratio_mag_mul(uint64_t a, uint64_t b, uint64_t *hi, uint64_t *lo);
// ============================================================================
// big
// ============================================================================
// bignums close the tower fixnum -> sun box -> bignum. all multi-limb work
// lives in ai_noinline magnitude helpers over raw limb arrays (no allocation),
// so the VM entries keep their tail calls and the GC never sees a half-built
// object. products/divides stay hardware (mulq/divq via div2by1) -- never
// __multi3/__udivti3, which the -nostdlib freestanding port cannot supply.


// |slen| of a heap bignum.
ai_inline int big_nlimbs(word x) {
 intptr_t s = big(x)->slen;
 return (int) (s < 0 ? -s : s); }

uintptr_t ai_big_bytes(struct ai_big *b) {
 intptr_t n = b->slen < 0 ? -b->slen : b->slen;
 return sizeof(struct ai_big) + (uintptr_t) n * sizeof(ai_limb); }

// --- raw magnitude primitives (little-endian limb arrays); callers pass
// normalized inputs and normalize outputs via ai_big_canon

static int mag_copy(ai_limb *dst, ai_limb const *src, int n) {
 for (int i = 0; i < n; i++) dst[i] = src[i];
 return n; }

// compare magnitudes: -1 if a<b, 0 if equal, 1 if a>b.
static ai_noinline int mag_cmp(ai_limb const *a, int na, ai_limb const *b, int nb) {
 while (na > 0 && a[na-1] == 0) na--;
 while (nb > 0 && b[nb-1] == 0) nb--;
 if (na != nb) return na < nb ? -1 : 1;
 for (int i = na - 1; i >= 0; i--) if (a[i] != b[i]) return a[i] < b[i] ? -1 : 1;
 return 0; }

// r = a + b. r distinct from a,b; capacity >= max(na,nb)+1. returns limb count.
static ai_noinline int mag_add(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb) {
 if (na < nb) { ai_limb const *t = a; a = b; b = t; int u = na; na = nb; nb = u; }
 ai_dlimb c = 0; int i = 0;
 for (; i < nb; i++) { ai_dlimb s = (ai_dlimb) a[i] + b[i] + c; r[i] = (ai_limb) s; c = s >> limb_bits; }
 for (; i < na; i++) { ai_dlimb s = (ai_dlimb) a[i] + c;        r[i] = (ai_limb) s; c = s >> limb_bits; }
 if (c) r[i++] = (ai_limb) c;
 return i; }

// r = a - b, requires a >= b (magnitudes). r distinct from a,b. returns na
// (caller normalizes away any high zero limbs the subtraction produced).
static ai_noinline int mag_sub(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb) {
 ai_sdlimb borrow = 0; int i = 0;
 for (; i < nb; i++) {
  ai_sdlimb d = (ai_sdlimb) a[i] - b[i] - borrow;
  if (d < 0) d += (ai_sdlimb) limb_base, borrow = 1; else borrow = 0;
  r[i] = (ai_limb) d; }
 for (; i < na; i++) {
  ai_sdlimb d = (ai_sdlimb) a[i] - borrow;
  if (d < 0) d += (ai_sdlimb) limb_base, borrow = 1; else borrow = 0;
  r[i] = (ai_limb) d; }
 return na; }

// r = a * b (schoolbook). r must be distinct from a,b; capacity >= na+nb. used
// one-shot by ai_big_binop (the object-array elementwise lane); the scalar `*`
// path instead drives a chunked, yieldable copy of this loop in lvm_bmul.
static ai_noinline void mag_mul(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb) {
 for (int i = 0; i < na + nb; i++) r[i] = 0;
 for (int i = 0; i < na; i++) {
  ai_dlimb carry = 0; ai_limb ai = a[i];            // ai stays a limb so ai*b[j] is the hardware 64x64->128 (not a 128x128 __multi3)
  for (int j = 0; j < nb; j++) {
   ai_dlimb s = (ai_dlimb) ai * b[j] + r[i+j] + carry;
   r[i+j] = (ai_limb) s; carry = s >> limb_bits; }
  r[i+nb] = (ai_limb) carry; } }

// a = a*mul + add, in place (mul,add < 2^limb_bits). a capacity must allow one
// carry limb at a[n]. returns the new limb count. used by the decimal reader.
static ai_noinline int mag_mul_add_small(ai_limb *a, int n, ai_limb mul, ai_limb add) {
 ai_dlimb c = add;
 for (int i = 0; i < n; i++) { ai_dlimb s = (ai_dlimb) a[i] * mul + c; a[i] = (ai_limb) s; c = s >> limb_bits; }
 if (c) a[n++] = (ai_limb) c;
 return n; }

// 128/64 -> quotient + remainder, caller guarantees the quotient fits a limb
// (hi < d): the hardware divq on x86-64, never __udivti3
ai_inline ai_limb div2by1(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem) {
#if defined(__x86_64__) && limb_bits == 64 && defined(__GNUC__)
 // gcc/clang take the one-divq asm; mooncc compiles the C face below natively
 // (its u128/u64 divide is the same two-step divq dance, emitted whole)
 __asm__("divq %2" : "+a"(lo), "+d"(hi) : "r"(d));
 return *rem = hi, lo;
#else
 ai_dlimb num = ((ai_dlimb) hi << limb_bits) | lo;
 return *rem = (ai_limb) (num % d), (ai_limb) (num / d);
#endif
}
// 128/64 -> full quotient + remainder for the q-hat step, as two divq-safe steps
ai_inline ai_dlimb div128by64(ai_limb hi, ai_limb lo, ai_limb d, ai_limb *rem) {
 ai_limb qhi = hi / d, r1 = hi % d;
 ai_limb qlo = div2by1(r1, lo, d, rem);
 return ((ai_dlimb) qhi << limb_bits) | qlo; }

// knuth Algorithm D long division (Hacker's Delight divmnu): u (m limbs) / v (n
// limbs, m >= n) -> q (m-n+1 limbs), r (n limbs); un/vn are normalization scratch.
static ai_noinline void mag_divmod(ai_limb *q, ai_limb *r,
  ai_limb const *u, int m, ai_limb const *v, int n, ai_limb *un, ai_limb *vn) {
 ai_dlimb const B = limb_base;
 if (n == 1) {                                  // single-limb divisor: simple
  ai_limb rem = 0;
  for (int j = m - 1; j >= 0; j--) { ai_limb rr; q[j] = div2by1(rem, u[j], v[0], &rr); rem = rr; }
  r[0] = rem; return; }
 int s = limb_clz(v[n-1]);                       // normalize so v[n-1] has its top bit set
 for (int i = n - 1; i > 0; i--) vn[i] = (v[i] << s) | (s ? (ai_dlimb) v[i-1] >> (limb_bits - s) : 0);
 vn[0] = v[0] << s;
 un[m] = s ? (ai_dlimb) u[m-1] >> (limb_bits - s) : 0;
 for (int i = m - 1; i > 0; i--) un[i] = (u[i] << s) | (s ? (ai_dlimb) u[i-1] >> (limb_bits - s) : 0);
 un[0] = u[0] << s;
 for (int j = m - n; j >= 0; j--) {
  ai_limb rr;                                   // 128/64 q-hat: divq-safe two-step, no __udivti3
  ai_dlimb qhat = div128by64(un[j+n], un[j+n-1], vn[n-1], &rr), rhat = rr;
  while (qhat >= B || qhat * vn[n-2] > ((rhat << limb_bits) | un[j+n-2])) {
   qhat--; rhat += vn[n-1];
   if (rhat >= B) break; }
  ai_sdlimb borrow = 0;                          // multiply and subtract qhat*v
  for (int i = 0; i < n; i++) {
   ai_dlimb p = (ai_dlimb) (ai_limb) qhat * vn[i];   // qhat < B here: a limb, so 64x64->128 not 128x128
   ai_sdlimb sub = (ai_sdlimb) un[i+j] - borrow - (ai_sdlimb) (ai_limb) p;
   un[i+j] = (ai_limb) sub;
   borrow = (ai_sdlimb) (p >> limb_bits) - (sub >> limb_bits); }
  ai_sdlimb sub = (ai_sdlimb) un[j+n] - borrow;
  un[j+n] = (ai_limb) sub;
  q[j] = (ai_limb) qhat;
  if (sub < 0) {                                // qhat was one too big: add back
   q[j]--;
   ai_dlimb carry = 0;
   for (int i = 0; i < n; i++) { ai_dlimb t = (ai_dlimb) un[i+j] + vn[i] + carry; un[i+j] = (ai_limb) t; carry = t >> limb_bits; }
   un[j+n] = (ai_limb) (un[j+n] + carry); } }
 for (int i = 0; i < n; i++) r[i] = s ? (un[i] >> s) | ((ai_dlimb) un[i+1] << (limb_bits - s)) : un[i]; }

// --- operand loading + tier conversions -------------------------------------

// load integer operand x (fixnum / sun box / bignum -- never a float) as a
// magnitude. a fixnum/box fills `scratch` (wlimbs limbs: 1 with native-width
// limbs, 2 with 32-bit limbs on a 64-bit word) and points *out at it; a bignum
// points *out into its heap limbs (stable only while no GC runs). sets *neg and
// returns the limb count (0 for the value zero). wlimbs = limbs to hold one word.
int load_int_mag(word x, ai_limb scratch[wlimbs], ai_limb const **out, bool *neg) {
 if (bigp(x)) { struct ai_big *b = big(x); intptr_t s = b->slen;
  *neg = s < 0, *out = b->limb; return (int) (s < 0 ? -s : s); }
 intptr_t v = charmp(x) ? (intptr_t) getcharm(x) : sun_get(x);
 *neg = v < 0;
 uintptr_t u = *neg ? (uintptr_t) 0 - (uintptr_t) v : (uintptr_t) v;
 int k = 0;
 for (int i = 0; i < wlimbs; i++) { scratch[i] = (ai_limb) (u >> (limb_bits * i)); if (scratch[i]) k = i + 1; }
 *out = scratch;
 return k; }

ai_flo_t ai_big_to_flo(word x) {
 struct ai_big *b = big(x);
 intptr_t sl = b->slen;
 bool neg = sl < 0;
 int n = (int) (neg ? -sl : sl);
 double r = 0;
 for (int i = n - 1; i >= 0; i--) r = r * (double) limb_base + (double) b->limb[i];
 return (ai_flo_t) (neg ? -r : r); }

// the bignum's two's-complement value mod 2^W (its low machine word). used when
// an integer-array elementwise op must broadcast a bignum scalar down to one
// machine-int element ("arrays win; demote the bignum by its low bits").
intptr_t ai_big_low(word x) {
 struct ai_big *b = big(x);
 intptr_t sl = b->slen;
 bool neg = sl < 0;
 int n = (int) (neg ? -sl : sl);
 uintptr_t u = 0;
 for (int i = 0; i < n && i < wlimbs; i++) u |= (uintptr_t) b->limb[i] << (limb_bits * i);
 return (intptr_t) (neg ? (uintptr_t) 0 - u : u); }

int ai_big_cmp(word a, word b) {
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool na, nb;
 int nla = load_int_mag(a, sa, &la, &na), nlb = load_int_mag(b, sb, &lb, &nb);
 bool aneg = na && nla > 0, bneg = nb && nlb > 0;   // zero is non-negative
 if (aneg != bneg) return aneg ? -1 : 1;
 int c = mag_cmp(la, nla, lb, nlb);
 return aneg ? -c : c; }

// demote a magnitude to the smallest tier: fixnum, sun box, bignum -- the single
// sink that keeps the tiers disjoint, so eqv / table keys stay well defined
word ai_big_canon(ai_word **hp, ai_limb const *limb, int n, bool neg) {
 while (n > 0 && limb[n-1] == 0) n--;
 if (n == 0) return zero;
 if (n <= wlimbs) {
  uintptr_t u = 0;
  for (int i = 0; i < n; i++) u |= (uintptr_t) limb[i] << (limb_bits * i);   // combine limbs into a word
  uintptr_t const fixmag = (uintptr_t) 1 << (Bits - 2),   // |mincharm|  = 2^(W-2)
                  boxmag = (uintptr_t) 1 << (Bits - 1);   // |INT_MIN|  = 2^(W-1)
  intptr_t val;
  if (!neg) {
   if (u <= fixmag - 1) return putcharm((intptr_t) u);       // maxcharm = 2^(W-2)-1
   if (u > boxmag - 1) goto big;                            // > INTPTR_MAX -> bignum
   val = (intptr_t) u; }
  else {
   if (u <= fixmag) return putcharm((intptr_t) ((uintptr_t) 0 - u));   // incl mincharm
   if (u > boxmag) goto big;                                          // < INTPTR_MIN -> bignum
   val = (intptr_t) ((uintptr_t) 0 - u); }                            // incl INTPTR_MIN
  return mk_sun(hp, val); }
big: ;                                   // C11 wants a statement before a declaration
 struct ai_big *b = ini_big(big(*hp), neg ? -n : n);
 for (int i = 0; i < n; i++) b->limb[i] = limb[i];
 *hp += b2w(sizeof(struct ai_big) + (size_t) n * sizeof(ai_limb));
 return word(b); }

// --- arithmetic (sign-magnitude over the loaded operands) -------------------

// r = a +/- b (subtract flips b's sign), result magnitude + sign.
static void big_addsub(ai_limb *r, int *rn, bool *rneg,
  ai_limb const *a, int na, bool nega, ai_limb const *b, int nb, bool negb, bool subtract) {
 bool sb = subtract ? !negb : negb;             // effective sign of the b operand
 if (nega == sb) { *rn = mag_add(r, a, na, b, nb); *rneg = nega; }
 else { int c = mag_cmp(a, na, b, nb);
  if (c == 0) { *rn = 0; *rneg = false; }
  else if (c > 0) { *rn = mag_sub(r, a, na, b, nb); *rneg = nega; }
  else { *rn = mag_sub(r, b, nb, a, na); *rneg = sb; } } }

// add magnitude s (sn limbs) into r at limb offset off, carrying up. r is sized
// for the full result, so the carry settles within it.
static void mag_add_off(ai_limb *r, int rn, ai_limb const *s, int sn, int off) {
 ai_dlimb c = 0; int i = 0;
 for (; i < sn; i++)            { ai_dlimb t = (ai_dlimb) r[off+i] + s[i] + c; r[off+i] = (ai_limb) t; c = t >> limb_bits; }
 for (; c && off + i < rn; i++) { ai_dlimb t = (ai_dlimb) r[off+i] + c;        r[off+i] = (ai_limb) t; c = t >> limb_bits; } }

// karatsuba for equal-length operands: three half-size products in place of one
// full one; below kara_cutoff schoolbook's lower constant wins. t is scratch.
#define kara_cutoff 40   // limbs/operand above which Karatsuba beats schoolbook (measured crossover)
static void mag_mul_kara(ai_limb *r, ai_limb const *a, ai_limb const *b, int n, ai_limb *t) {
 if (n < kara_cutoff) { mag_mul(r, a, n, b, n); return; }
 int m = n / 2, h = n - m;                           // low m limbs, high h (m or m+1) limbs
 mag_mul_kara(r,       a,     b,     m, t);           // z0 -> r[0..2m)
 mag_mul_kara(r + 2*m, a + m, b + m, h, t);           // z2 -> r[2m..2n)
 int z0n = 2*m;  while (z0n > 0 && r[z0n-1] == 0) z0n--;
 int z2n = 2*h;  while (z2n > 0 && r[2*m + z2n-1] == 0) z2n--;
 ai_limb *sa = t, *sb = sa + (h+1), *z1 = sb + (h+1);
 int nsa = mag_add(sa, a, m, a + m, h), nsb = mag_add(sb, b, m, b + m, h);
 mag_mul(z1, sa, nsa, sb, nsb);                       // z1 = (a0+a1)(b0+b1) on the half-size sums
 int nz1 = nsa + nsb;                       while (nz1 > 0 && z1[nz1-1] == 0) nz1--;
 nz1 = mag_sub(z1, z1, nz1, r,       z0n);  while (nz1 > 0 && z1[nz1-1] == 0) nz1--;   // z1 -= z0
 nz1 = mag_sub(z1, z1, nz1, r + 2*m, z2n);  while (nz1 > 0 && z1[nz1-1] == 0) nz1--;   // z1 -= z2
 mag_add_off(r, 2*n, z1, nz1, m); }                  // r += z1 * B^m

static int big_mul_mag(ai_limb *r, ai_limb const *a, int na, ai_limb const *b, int nb, ai_limb *t) {
 if (na == nb) mag_mul_kara(r, a, b, na, t); else mag_mul(r, a, na, b, nb);
 int n = na + nb; while (n > 0 && r[n-1] == 0) n--;
 return n; }

// the packed multi-precision lane for + - * / % (zero divisor screened by the
// caller): computes a (vop) b, leaves the result at g->sp[1], pops one, advances
// ip -- so the caller is just Pack; binop; Unpack; Continue.
struct ai *ai_big_binop(struct ai *g, int vop) {
 word a = g->sp[0], b = g->sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2,
     bound = na + nb + 2,                        // result magnitude upper bound
     work = 4 * (na + nb) + 16;                  // divmod scratch upper bound
 uintptr_t res_area = Width(struct ai_big) + b2w((size_t) bound * sizeof(ai_limb)),
           ws_words = b2w((size_t) (bound + work) * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, res_area + ws_words))) return g;
 a = g->sp[0], b = g->sp[1];                     // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int nla = load_int_mag(a, sa, &la, &nega), nlb = load_int_mag(b, sb, &lb, &negb);
 ai_limb *rmag = (ai_limb*) (g->hp + res_area), *scr = rmag + bound;
 int rn = 0; bool rneg = false;
 switch (vop) {
  case vop_add: big_addsub(rmag, &rn, &rneg, la, nla, nega, lb, nlb, negb, false); break;
  case vop_sub: big_addsub(rmag, &rn, &rneg, la, nla, nega, lb, nlb, negb, true); break;
  case vop_mul: rn = big_mul_mag(rmag, la, nla, lb, nlb, scr); rneg = nega != negb; break;
  default: {                                     // vop_quot / vop_rem (truncated)
   int c = mag_cmp(la, nla, lb, nlb);
   if (c < 0) {                                  // |a| < |b|: q = 0, r = a
    if (vop == vop_rem) rn = mag_copy(rmag, la, nla), rneg = nega; }
   else {
    ai_limb *q = scr, *rem = q + (nla - nlb + 1), *un = rem + nlb, *vn = un + (nla + 1);
    mag_divmod(q, rem, la, nla, lb, nlb, un, vn);
    if (vop != vop_rem) {                          // vop_quot / vop_fquot: truncated quotient
     int qn = nla - nlb + 1; while (qn > 0 && q[qn-1] == 0) qn--;
     rn = mag_copy(rmag, q, qn), rneg = nega != negb; }
    else {
     int rr = nlb; while (rr > 0 && rem[rr-1] == 0) rr--;
     rn = mag_copy(rmag, rem, rr), rneg = nega; } } } }
 return *++g->sp = ai_big_canon(&g->hp, rmag, rn, rneg), ++g->ip, g; }

// the integer rungs' exact lane (int / ceil / saturate) for a ratio coin: above
// 2^53 the float net rounds, so a rung riding it lands on the wrong integer.
// domain: a net-mode-2 coin over (n d), both exact integers, d nonzero (a zero
// divisor keeps the float lane's inf/sign story).
bool ai_ratio_exact(struct ai *g, word x) {
 if (!coinp(x) || die_get(g, coin_die(x), DieNet) != putcharm(2)) return false;
 word p = coin_load(x);
 if (!chainp(p) || !chainp(B(p))) return false;
 word n = A(p), d = A(B(p));
 if (!(charmp(n) || sunp(n) || bigp(n)) || !(charmp(d) || sunp(d) || bigp(d))) return false;
 return charmp(d) ? d != putcharm(0) : sunp(d) ? sun_get(d) != 0 : true; }
// ..the lane: trunc(n/d) by long division, clamped to the charm bounds like every
// rung (the codomain law), then the rung's own adjustment -- ceil rounds a dropped
// remainder up, saturate is ceil with its floor raised to 0. the operand rides
// g->sp[0] across ai_have's GC edge; scratch sits above hp and is never committed.
struct ai *ai_ratio_rung(struct ai *g, int rung) {
 word x = g->sp[0], p = coin_load(x), a = A(p), b = A(B(p));
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 if (!ai_ok(g = ai_have(g, b2w((size_t) (4 * (na + nb) + 16) * sizeof(ai_limb))))) return g;
 x = g->sp[0], p = coin_load(x), a = A(p), b = A(B(p));       // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int nla = load_int_mag(a, sa, &la, &nega), nlb = load_int_mag(b, sb, &lb, &negb);
 bool rneg = nega != negb, rnz = false, sat = false;
 uintptr_t uq = 0;
 if (nla == 0) ;                                              // 0/d: q 0, r 0
 else if (mag_cmp(la, nla, lb, nlb) < 0) rnz = true;          // |a| < |b|: q 0, r a
 else {
  ai_limb *q = (ai_limb*) g->hp, *rem = q + (nla - nlb + 1), *un = rem + nlb, *vn = un + (nla + 1);
  mag_divmod(q, rem, la, nla, lb, nlb, un, vn);
  int qn = nla - nlb + 1; while (qn > 0 && q[qn-1] == 0) qn--;
  for (int i = 0; i < nlb; i++) if (rem[i]) { rnz = true; break; }
  if (qn > wlimbs) sat = true;
  else { for (int i = 0; i < qn; i++) uq |= (uintptr_t) q[i] << (limb_bits * i);
         if (uq > (uintptr_t) maxcharm + (rneg ? 1 : 0)) sat = true; } }
 intptr_t t = sat ? (rneg ? mincharm : maxcharm)
                  : rneg ? -(intptr_t) uq : (intptr_t) uq;
 if (rung >= 1 && !sat && rnz && !rneg && t < maxcharm) t++;  // ceil: a dropped remainder rounds up
 if (rung == 2 && t < 0) t = 0;                               // saturate: the floor rises to 0
 g->sp[0] = putcharm(t);
 g->ip = (union u*) g->ip + 1;
 return g; }

// `/` over the bignum lane: like ai_big_binop's truncated quotient, but the result
// stays an exact integer only when b divides a; a nonzero remainder promotes to a
// float box of a/b (the bignum analogue of the scalar `/` int promotion). operands
// at g->sp[0..1] are integers; a zero divisor is screened off by the caller.
struct ai *ai_big_quot_true(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2,
     bound = na + nb + 2, work = 4 * (na + nb) + 16;
 uintptr_t res_area = Width(struct ai_big) + b2w((size_t) bound * sizeof(ai_limb)),
           ws_words = b2w((size_t) (bound + work) * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, res_area + ws_words + box_req))) return g;
 a = g->sp[0], b = g->sp[1];                     // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int nla = load_int_mag(a, sa, &la, &nega), nlb = load_int_mag(b, sb, &lb, &negb);
 ai_limb *rmag = (ai_limb*) (g->hp + res_area), *scr = rmag + bound;
 bool rneg = false, exact;
 int rn = 0, c = mag_cmp(la, nla, lb, nlb);
 if (c < 0) exact = (nla == 0);                  // |a| < |b|: q = 0, exact iff a == 0
 else {
  ai_limb *q = scr, *rem = q + (nla - nlb + 1), *un = rem + nlb, *vn = un + (nla + 1);
  mag_divmod(q, rem, la, nla, lb, nlb, un, vn);
  int rr = nlb; while (rr > 0 && rem[rr-1] == 0) rr--;
  exact = (rr == 0);
  int qn = nla - nlb + 1; while (qn > 0 && q[qn-1] == 0) qn--;
  rn = mag_copy(rmag, q, qn), rneg = nega != negb; }
 if (exact) g->sp[1] = ai_big_canon(&g->hp, rmag, rn, rneg);
 else g->sp[1] = mk_gem(&g->hp, toflo(a) / toflo(b));  // a,b still valid: no GC since the re-fetch, and toflo is alloc-free
 g->sp++;
 g->ip = (union u*) g->ip + 1;
 return g; }

// --- resumable (yieldable) multiply ---
// schoolbook run as one C call never yields, so a huge product would block every
// peer task. drive it as a self-looping VM instruction instead: the partial
// product lives in a cask, each dispatch folds ~bmul_chunk limb-mults, and the
// work state rides the l stack [i, r, ret_ip, a, b]. operands stay heap bignums
// so the loop reads stable limb pointers (no &scratch -- the sibcall law).
#define bmul_chunk (1 << 14)
union u const bmul_loop[1] = { { .ap = lvm_bmul } };

// materialize integer x as a heap ai_big (a bignum returns in place)
union u *as_big(ai_word **hp, word x) {
 if (bigp(x)) return cell(x);
 intptr_t v = toint(x);
 bool neg = v < 0;
 uintptr_t u = neg ? (uintptr_t) 0 - (uintptr_t) v : (uintptr_t) v;
 ai_limb tmp[wlimbs]; int n = 0;                                  // a machine word is wlimbs limbs
 for (int i = 0; i < wlimbs; i++) { tmp[i] = (ai_limb) (u >> (limb_bits * i)); if (tmp[i]) n = i + 1; }
 struct ai_big *b = ini_big(big(*hp), neg ? -n : n);
 for (int i = 0; i < n; i++) b->limb[i] = tmp[i];
 *hp += b2w(sizeof(struct ai_big) + (size_t) n * sizeof(ai_limb));
 return cell((word) b); }

// promote both operands, allocate the zeroed result cask, lay out the work frame;
// one ai_have so no half-built state is ever seen
static struct ai *ai_bmul_setup(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 uintptr_t rbytes = (uintptr_t) (na + nb) * sizeof(ai_limb),
           sreq = str_width(rbytes),
           breq = Width(struct ai_cask) + Width(struct ai_tag),
           bigmax = Width(struct ai_big) + b2w((size_t) wlimbs * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, 2 * bigmax + sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                       // re-fetch (ai_have may have GC'd)
 union u *abig = as_big(&g->hp, a), *bbig = as_big(&g->hp, b), *ret = g->ip + 1;
 struct ai_str *s = ini_str(str(g->hp), rbytes);
 g->hp += sreq; memset(txt(s), 0, rbytes);
 union u *k = (union u*) g->hp; g->hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = s;
 tagthread(k, Width(struct ai_cask));
 g->sp -= 3;                                       // [i, r, ret_ip, abig, bbig]
 g->sp[0] = putcharm(0), g->sp[1] = word(k), g->sp[2] = word(ret);
 g->sp[3] = word(abig), g->sp[4] = word(bbig);
 g->ip = (union u*) bmul_loop;
 return g; }

// --- resumable Karatsuba multiply: a true recursive karatsuba (O(n^1.585)) as a
// yieldable VM instruction. the whole computation lives in one pinned cask
// [hdr | job stack | A(n) | B(n) | R(2n) | scratch], re-read by offset each
// dispatch. a job either splits (push the three half-size children, LIFO) or
// combines (z1 -= z0; z1 -= z2; r += z1<<m). only na==nb routes here.
#define kmul_chunk (1 << 14)   // leaf limb-mults folded per dispatch before a yield check
#define KmulHdr 8             // ws header limbs: [0]=n [1]=top (stack ptr) [2]=sign [3]=r_off
#define KmulJw  6             // job record limbs: ar, br, n, rr, sr, state
union u const kmul_loop[1] = { { .ap = lvm_kmul } };

static struct ai *ai_kmul_setup(struct ai *g) {
 word a = g->sp[0], b = g->sp[1];
 // caller contract: bigp(a)&&bigp(b), na==nb==n, n>=kara_cutoff
 int n = big_nlimbs(a), d = 0;
 for (int t = n; t >= kara_cutoff; t = (t + 1) / 2) d++;   // karatsuba depth
 uintptr_t njob = (uintptr_t) 8 * d + 32,                 // job-stack capacity (~3d live, generous)
           scrn = (uintptr_t) 6 * n + 16 * (uintptr_t) d + 256,      // O(n) scratch, with margin
           jobs_off = KmulHdr,
           a_off = jobs_off + njob * KmulJw,
           b_off = a_off + n,
           r_off = b_off + n,
           scr_off = r_off + 2 * (uintptr_t) n,
           wslimbs = scr_off + scrn,
           sreq = str_width(wslimbs * sizeof(ai_limb)),
           breq = Width(struct ai_cask) + Width(struct ai_tag);
 if (!ai_ok(g = ai_have(g, sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                              // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 (void) load_int_mag(a, sa, &la, &nega);
 (void) load_int_mag(b, sb, &lb, &negb);
 struct ai_str *ws_s = ini_str(str(g->hp), wslimbs * sizeof(ai_limb));
 g->hp += sreq;
 ai_limb *ws = (ai_limb*) txt(ws_s);
 for (uintptr_t i = 0; i < wslimbs; i++) ws[i] = 0;       // zero everything: clean result + scratch slots
 for (int i = 0; i < n; i++) ws[a_off + i] = la[i];
 for (int i = 0; i < n; i++) ws[b_off + i] = lb[i];
 ws[0] = (ai_limb) n, ws[1] = 1, ws[2] = (ai_limb) (nega != negb), ws[3] = r_off;   // n, top=1, sign, r_off
 ai_limb *j0 = ws + jobs_off;                             // the root job: multiply a x B -> R
 j0[0] = a_off, j0[1] = b_off, j0[2] = (ai_limb) n, j0[3] = r_off, j0[4] = scr_off, j0[5] = 0;
 union u *k = (union u*) g->hp; g->hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = ws_s;
 tagthread(k, Width(struct ai_cask));
 union u *ret = g->ip + 1;
 g->sp[0] = word(k), g->sp[1] = word(ret);                // frame [ws_buf, ret_ip]  (was [a, b])
 g->ip = (union u*) kmul_loop;
 return g; }

// FIXME can we choose different types to reduce the amount of explicit casting in this function?
lvm(lvm_kmul) {
 ai_limb *ws = (ai_limb*) txt(cask(Sp[0])->str);
 int n = (int) ws[0], top = (int) ws[1];
 ai_limb *jobs = ws + KmulHdr;
 long budget = kmul_chunk;
 while (top > 0 && budget > 0) {
  ws[1] = (ai_limb) top; YieldCheck();             // persist top, then a per-job yield check:
                                                   // a once-per-dispatch check yields too rarely here
  ai_limb *J = jobs + (uintptr_t) (top - 1) * KmulJw;
  uintptr_t ar = J[0], br = J[1];
  uintptr_t rr = J[3], sr = J[4];
  int jn = (int) J[2];
  int st = (int) J[5];
  if (jn < kara_cutoff) {                                 // leaf: schoolbook jn x jn -> ws[rr..rr+2jn)
   mag_mul(ws + rr, ws + ar, jn, ws + br, jn);
   budget -= (long) jn * jn; top--; continue; }
  int m = jn / 2, h = jn - m;                             // low m limbs, high h (m or m+1)
  if (st == 0) {                                          // split
   uintptr_t saO = sr, sbO = sr + (uintptr_t) (h + 1),
             z1O = sr + 2 * (uintptr_t) (h + 1), csr = sr + 4 * (uintptr_t) (h + 1);
   int ns = mag_add(ws + saO, ws + ar, m, ws + ar + m, h);            // sa = a_lo + a_hi
   for (int i = ns; i < h + 1; i++) ws[saO + i] = 0;                  // zero-extend to exactly h+1
   int nt = mag_add(ws + sbO, ws + br, m, ws + br + m, h);            // sb = b_lo + b_hi
   for (int i = nt; i < h + 1; i++) ws[sbO + i] = 0;
   for (int i = 0; i < 2 * (h + 1); i++) ws[z1O + i] = 0;            // clear z1's output slot
   J[5] = 1;                                                          // this job combines when it returns
   ai_limb *z1J = jobs + (uintptr_t) top       * KmulJw;            // push z1 = sa*sb (pops first)
   z1J[0] = saO, z1J[1] = sbO, z1J[2] = (ai_limb) (h + 1), z1J[3] = z1O, z1J[4] = csr, z1J[5] = 0;
   ai_limb *z2J = jobs + (uintptr_t) (top + 1) * KmulJw;            // push z2 = a_hi*b_hi -> r[2m..]
   z2J[0] = ar + m, z2J[1] = br + m, z2J[2] = (ai_limb) h, z2J[3] = rr + 2 * (uintptr_t) m, z2J[4] = csr, z2J[5] = 0;
   ai_limb *z0J = jobs + (uintptr_t) (top + 2) * KmulJw;            // push z0 = a_lo*b_lo -> r[0..] (pops last)
   z0J[0] = ar, z0J[1] = br, z0J[2] = (ai_limb) m, z0J[3] = rr, z0J[4] = csr, z0J[5] = 0;
   top += 3; budget -= jn;                                            // pop order z0,z2,z1 then this (combine)
  } else {                                                // combine (st == 1)
   uintptr_t z1O = sr + 2 * (uintptr_t) (h + 1);
   int z0n = 2 * m;
   while (z0n > 0 && ws[rr + (uintptr_t) z0n - 1] == 0) z0n--;
   int z2n = 2 * h;
   while (z2n > 0 && ws[rr + 2 * (uintptr_t) m + (uintptr_t) z2n - 1] == 0) z2n--;
   int nz1 = 2 * (h + 1);
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   nz1 = mag_sub(ws + z1O, ws + z1O, nz1, ws + rr, z0n);                       // z1 -= z0
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   nz1 = mag_sub(ws + z1O, ws + z1O, nz1, ws + rr + 2 * (uintptr_t) m, z2n);   // z1 -= z2
   while (nz1 > 0 && ws[z1O + (uintptr_t) nz1 - 1] == 0) nz1--;
   mag_add_off(ws + rr, 2 * jn, ws + z1O, nz1, m);                             // r += z1 * B^m
   budget -= jn; top--; }
 }
 ws[1] = (ai_limb) top;                                   // persist the stack pointer before any yield
 if (top > 0) { YieldCheck(); ai_musttail return Continue(); }
 bool neg = ws[2]; uintptr_t r_off = ws[3];               // done: ws[r_off..r_off+2n) is the product
 Have(Width(struct ai_big) + b2w(((size_t) 2 * (size_t) n + 1) * sizeof(ai_limb)));
 ws = (ai_limb*) txt(cask(Sp[0])->str);                    // re-fetch (Have may have GC'd)
 n = (int) ws[0], r_off = ws[3];
 word ret = Sp[1], res;
 Pack(g);
 res = ai_big_canon(&g->hp, ws + r_off, 2 * n, neg);
 Unpack(g);
 Sp += 1; Sp[0] = res; Ip = cell(ret); ai_musttail return Continue(); }

lvm(lvm_bmul_start) {
 // small-product fast path: a product that fits one chunk never yields, so the
 // resumable setup is pure overhead -- and that is the common case. one-shot it
 // through ai_big_binop. (na <= chunk/nb keeps na*nb from overflowing a 32-bit int.)
 word a = Sp[0], b = Sp[1];
 int na = bigp(a) ? big_nlimbs(a) : 2, nb = bigp(b) ? big_nlimbs(b) : 2;
 if (na <= bmul_chunk / nb) {
  Pack(g); g = ai_big_binop(g, vop_mul);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (bigp(a) && bigp(b) && na == nb) {           // equal-length large: subquadratic Karatsuba
  Pack(g); g = ai_kmul_setup(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 Pack(g); g = ai_bmul_setup(g);                  // unequal-length large: chunked schoolbook
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 ai_musttail return Resume(); }

lvm(lvm_bmul) {
 int i = (int) getcharm(Sp[0]);
 struct ai_big *A = big(Sp[3]), *B = big(Sp[4]);
 intptr_t sla = A->slen, slb = B->slen;
 int na = sla < 0 ? -sla : sla, nb = slb < 0 ? -slb : slb;
 if (!na || !nb) {                                // a zero operand: product is 0
  word ret = Sp[2]; Sp += 4; Sp[0] = zero; Ip = cell(ret); ai_musttail return Continue(); }
 ai_limb *la = A->limb, *lb = B->limb, *rl = (ai_limb*) txt(cask(Sp[1])->str);
 int end = min(i + max(1, bmul_chunk / nb), na);
 for (; i < end; i++) {                           // schoolbook outer loop, one chunk of rows
  ai_dlimb carry = 0; ai_limb ai = la[i];           // limb-typed so ai*lb[j] is the hardware 64x64->128, not a 128x128 multiply
  for (int j = 0; j < nb; j++) {
   ai_dlimb t = (ai_dlimb) ai * lb[j] + rl[i+j] + carry;
   rl[i+j] = (ai_limb) t, carry = t >> limb_bits; }
  rl[i+nb] = (ai_limb) carry; }
 Sp[0] = putcharm(i);                               // persist progress before any yield/GC
 if (i < na) { YieldCheck(); ai_musttail return Continue(); }
 bool neg = (sla < 0) != (slb < 0); word ret;     // done: canonicalize the product
 Have(Width(struct ai_big) + b2w((size_t) (na + nb) * sizeof(ai_limb)));
 ret = Sp[2];
 ai_limb *rmag = (ai_limb*) txt(cask(Sp[1])->str);  // re-fetch (Have may have GC'd)
 Pack(g);                                          // canon needs the synced g->hp (not &Hp: stack-local escapes block the sibcall)
 word res = ai_big_canon(&g->hp, rmag, na + nb, neg);
 Unpack(g);
 Sp += 4; Sp[0] = res; Ip = cell(ret); ai_musttail return Continue(); }

// --- resumable long division (lvm_bmul's divmod twin): normalize once into a
// pinned workspace [hdr | vn | un | q], grind the Knuth-D loop in chunks,
// persisting the index j. every entry re-reads the base from Sp[1] (a GC moves
// it). cheap divides one-shot through ai_big_binop. which: 0 = //, 1 = %.
#define bdiv_chunk (1 << 14)
#define BdivHdr 6           // ws header limbs: m, n, s(shift), which, nega, negb
union u const bdiv_loop[1] = { { .ap = lvm_bdiv } };

static struct ai *ai_bdiv_setup(struct ai *g, int which) {
 word a = g->sp[0], b = g->sp[1];
 int mub = bigp(a) ? big_nlimbs(a) : wlimbs, nub = bigp(b) ? big_nlimbs(b) : wlimbs;
 uintptr_t wslimbs = BdivHdr + (uintptr_t) nub + (mub + 1) + (mub - nub + 1),
           sreq = str_width(wslimbs * sizeof(ai_limb)),
           breq = Width(struct ai_cask) + Width(struct ai_tag);
 if (!ai_ok(g = ai_have(g, sreq + breq + 3))) return g;
 a = g->sp[0], b = g->sp[1];                          // re-fetch (ai_have may have GC'd)
 ai_limb sa[wlimbs], sb[wlimbs]; ai_limb const *la, *lb; bool nega, negb;
 int m = load_int_mag(a, sa, &la, &nega), n = load_int_mag(b, sb, &lb, &negb);
 struct ai_str *ws_s = ini_str(str(g->hp), wslimbs * sizeof(ai_limb));
 g->hp += sreq;
 ai_limb *ws = (ai_limb*) txt(ws_s);
 ai_limb *vn = ws + BdivHdr, *un = vn + n, *q = un + (m + 1);
 int s = limb_clz(lb[n-1]);                           // normalize so v[n-1]'s top bit is set
 for (int i = n-1; i > 0; i--) vn[i] = (lb[i] << s) | (s ? (ai_dlimb) lb[i-1] >> (limb_bits - s) : 0);
 vn[0] = lb[0] << s;
 un[m] = s ? (ai_dlimb) la[m-1] >> (limb_bits - s) : 0;
 for (int i = m-1; i > 0; i--) un[i] = (la[i] << s) | (s ? (ai_dlimb) la[i-1] >> (limb_bits - s) : 0);
 un[0] = la[0] << s;
 for (int i = 0; i < m - n + 1; i++) q[i] = 0;
 ws[0] = (ai_limb) m, ws[1] = (ai_limb) n, ws[2] = (ai_limb) s, ws[3] = (ai_limb) which;
 ws[4] = (ai_limb) nega, ws[5] = (ai_limb) negb;
 union u *k = (union u*) g->hp; g->hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = ws_s;
 tagthread(k, Width(struct ai_cask));
 union u *ret = g->ip + 1;
 g->sp -= 1;                                          // [j, ws_buf, ret_ip]  (was [a, b])
 g->sp[0] = putcharm(m - n), g->sp[1] = word(k), g->sp[2] = word(ret);
 g->ip = (union u*) bdiv_loop;
 return g; }

lvm(lvm_bdiv_start) {
 int vop = (int) g->b;
 word a = Sp[0], b = Sp[1];
 int m = bigp(a) ? big_nlimbs(a) : wlimbs, n = bigp(b) ? big_nlimbs(b) : wlimbs;
 // one-shot the cheap cases: |a|<|b| (q=0), single-limb divisor, or a short quotient.
 if (m < n || n < 2 || m - n < (int) (bdiv_chunk / (uintptr_t) n)) {
  Pack(g); g = ai_big_binop(g, vop);
  if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
  return Resume(); }
 Pack(g); g = ai_bdiv_setup(g, vop == vop_rem);
 if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
 return Resume(); }

lvm(lvm_bdiv) {
 ai_limb *ws = (ai_limb*) txt(cask(Sp[1])->str);
 int m = (int) ws[0], n = (int) ws[1], s = (int) ws[2], which = (int) ws[3];
 bool nega = ws[4], negb = ws[5];
 ai_limb *vn = ws + BdivHdr, *un = vn + n, *q = un + (m + 1);
 int j = (int) getcharm(Sp[0]);
 ai_dlimb const B = limb_base;
 int steps = max(1, (int) (bdiv_chunk / (uintptr_t) n));
 for (int c = 0; c < steps && j >= 0; c++, j--) {      // one Knuth-D quotient limb per iteration
  ai_limb rr;
  ai_dlimb qhat = div128by64(un[j+n], un[j+n-1], vn[n-1], &rr), rhat = rr;
  while (qhat >= B || qhat * vn[n-2] > ((rhat << limb_bits) | un[j+n-2])) {
   qhat--; rhat += vn[n-1]; if (rhat >= B) break; }
  ai_sdlimb borrow = 0;                                // multiply and subtract qhat*v
  for (int i = 0; i < n; i++) {
   ai_dlimb p = (ai_dlimb) (ai_limb) qhat * vn[i];
   ai_sdlimb sub = (ai_sdlimb) un[i+j] - borrow - (ai_sdlimb) (ai_limb) p;
   un[i+j] = (ai_limb) sub;
   borrow = (ai_sdlimb) (p >> limb_bits) - (sub >> limb_bits); }
  ai_sdlimb sub = (ai_sdlimb) un[j+n] - borrow;
  un[j+n] = (ai_limb) sub;
  q[j] = (ai_limb) qhat;
  if (sub < 0) {                                       // qhat one too big: add back
   q[j]--;
   ai_dlimb carry = 0;
   for (int i = 0; i < n; i++) { ai_dlimb t = (ai_dlimb) un[i+j] + vn[i] + carry; un[i+j] = (ai_limb) t; carry = t >> limb_bits; }
   un[j+n] = (ai_limb) (un[j+n] + carry); } }
 if (j >= 0) { Sp[0] = putcharm(j); YieldCheck(); ai_musttail return Continue(); }
 // done: canonicalize the requested output. denormalize the remainder into vn (now
 // dead), not in place, so a GC-retry of this tail stays idempotent. persist j=-1
 // first so a retry skips the loop.
 Sp[0] = putcharm(-1);
 int outn = which ? n : (m - n + 1);
 Have(Width(struct ai_big) + b2w((size_t) (outn + 1) * sizeof(ai_limb)));
 ws = (ai_limb*) txt(cask(Sp[1])->str);                 // re-fetch (Have may have GC'd)
 vn = ws + BdivHdr, un = vn + n, q = un + (m + 1);
 bool rneg = which ? nega : (nega != negb);
 word ret = Sp[2], res;
 Pack(g);
 if (which) {
  for (int i = 0; i < n; i++) vn[i] = s ? (un[i] >> s) | ((ai_dlimb) un[i+1] << (limb_bits - s)) : un[i];
  res = ai_big_canon(&g->hp, vn, n, rneg); }
 else res = ai_big_canon(&g->hp, q, m - n + 1, rneg);
 Unpack(g);
 Sp += 2; Sp[0] = res; Ip = cell(ret); ai_musttail return Continue(); }

// --- reader / printer -------------------------------------------------------

// one digit, either radix -- decimal digits sort below 'a', so the same fold
// reads both and hex takes either case.
static ai_inline ai_limb rdigit(char c) {
 return (ai_limb) (c <= '9' ? c - '0' : (c | 32) - 'a' + 10); }

// g->sp[0] is a [+-]?<pfx><digits> token; replace it with the canonical value.
// accumulates `chunk` digits per mul-add pass (radix**chunk fits one limb).
static struct ai *big_read_radix(struct ai *g, ai_limb radix, int chunk, uintptr_t pfx) {
 struct ai_str *tok = str(g->sp[0]);
 uintptr_t n = tok->len;
 char const *s = tok->bytes;
 bool neg = n && s[0] == '-';
 uintptr_t i = ((n && (s[0] == '-' || s[0] == '+')) ? 1 : 0) + pfx, ndig = n - i;
 int cap = (int) (ndig / (uintptr_t) chunk) + 3;  // upper-bound magnitude limbs (>= ndig/digits-per-limb)
 uintptr_t res_area = Width(struct ai_big) + b2w((size_t) cap * sizeof(ai_limb));
 if (!ai_ok(g = ai_have(g, res_area + b2w((size_t) cap * sizeof(ai_limb))))) return g;
 tok = str(g->sp[0]), s = tok->bytes;            // re-fetch post-GC
 ai_limb *mag = (ai_limb*) (g->hp + res_area);
 int m = 0;
 while (i < n) {
  ai_limb acc = 0, pw = 1; int k = 0;
  for (; i < n && k < chunk; i++, k++) acc = acc * radix + rdigit(s[i]), pw *= radix;
  m = mag_mul_add_small(mag, m, pw, acc); }
 g->sp[0] = ai_big_canon(&g->hp, mag, m, neg);
 return g; }
struct ai *ai_big_read_dec(struct ai *g) { return big_read_radix(g, 10, limb_dec_chunk, 0); }
struct ai *ai_big_read_hex(struct ai *g) { return big_read_radix(g, 16, limb_hex_chunk, 2); }
struct ai *ai_big_read_oct(struct ai *g) { return big_read_radix(g,  8, limb_oct_chunk, 1); }

// --- (tray witness shape-list vals): the typed array constructor (mopped; the
// prel's *-tray wrap it). the witness names its tier by example (0/0.0/~(0 0)/()
// -> z/r/c/o); vals fills row-major (missing stays 0, extras ignored). bad
// witness / negative dim / over-rank -> zero.
lvm(lvm_trayctor) {
 word t = Sp[0], shp = Sp[1];                  // t = a witness gem (names its tier), vals = Sp[2]
 // the type is read off the witness's kind -- a value inhabiting the tier: 0 -> Z,
 // 0.0 -> R, ~(0 0) -> C, and anything else (canonically (), the O floor) -> O.
 intptr_t ty = twinp(t) ? ai_C : gemp(t) ? ai_R
             : (charmp(t) || sunp(t) || bigp(t)) ? ai_Z : ai_O;
 uintptr_t rank = 0, nelem = 1;
 for (word l = shp; chainp(l); l = B(l)) {
  word d = A(l);
  if (!charmp(d) || getcharm(d) < 0) ai_musttail return Answerp(2, zero);
  rank++, nelem *= (uintptr_t) getcharm(d); }
 if (rank > maxrank) ai_musttail return Answerp(2, zero);
 uintptr_t bytes = sizeof(struct ai_tray) + rank * sizeof(word) + nelem * ai_T[ty];
 Have(b2w(bytes));
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ty, rank);
 uintptr_t i = 0;                              // re-walk the (possibly moved) lists
 for (word l = Sp[1]; chainp(l); l = B(l)) v->shape[i++] = (uintptr_t) getcharm(A(l));
 if (ty == ai_O) for (i = 0; i < nelem; i++) tray_put_obj(v, i, ZeroPoint);  // O floor is () not 0
 else memset(tray_data(v), 0, nelem * ai_T[ty]);
 i = 0;                                        // no alloc below, so v/Sp[2] stay put
 for (word l = Sp[2]; chainp(l) && i < nelem; l = B(l), i++) tray_put(v, i, A(l));
 // only a rank-0 point (empty shape) demotes to its lone scalar gem; a
 // rank-1-len-1 stays an array (@(5) is a one-cell array, not 5 -- collapsing it
 // left the surface discontinuous). root the built tray: the box alloc can GC.
 Sp[2] = word(v);
 if (rank == 0) {
  if (ty == ai_O) ai_musttail return Answerp(2, tray_get_obj(v, 0));
  if (ty == ai_C) { Have(twin_req); v = tray(Sp[2]); ai_flo_t *fp = tray_data(v);
   Sp[2] = mk_twin(&Hp, fp[0], fp[1]); ai_musttail return Nextp(1, 2); }
  word _res; Have(box_req); v = tray(Sp[2]);
  if (ty >= ai_R) emit_gem(_res, tray_get_flo(v, 0));
  else emit_int(_res, tray_get_int(v, 0));
  ai_musttail return Answerp(2, _res); }
 ai_musttail return Answerp(2, word(v)); }

// (iota n): a z-array of 0..n-1, the array twin of `jot`; n<0 or non-fixnum -> zero
lvm(lvm_iota) {
 word nx = Sp[0];
 if (!charmp(nx) || getcharm(nx) < 0) ai_musttail return Answer(ZeroPoint);
 uintptr_t n = (uintptr_t) getcharm(nx),
           bytes = sizeof(struct ai_tray) + 1 * sizeof(word) + n * ai_T[ai_Z];
 Have(b2w(bytes));
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ai_Z, 1);
 v->shape[0] = n;
 for (uintptr_t i = 0; i < n; i++) tray_put_int(v, i, (intptr_t) i);
 ai_musttail return Answer(word(v)); }

// --- accessors -------------------------------------------------------------
// rank / element-type code as fixnums; zero for a non-tray. both 0 for a scalar box.
op11(lvm_rank, packp(Sp[0]) ? putcharm(tray(Sp[0])->rank) : ZeroPoint)
op11(lvm_atype, packp(Sp[0]) ? putcharm(tray(Sp[0])->type) : ZeroPoint)

// total element count (1 for a scalar box), zero for a non-tray.
lvm(lvm_alen) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Answer(ZeroPoint);
 ai_musttail return Answer(putcharm(tray_nelem(tray(x)))); }

// dimensions as a list (allocates rank link cells), zero for a non-tray.
lvm(lvm_shape) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Answer(ZeroPoint);
 uintptr_t r = tray(x)->rank;
 Have(r * Width(struct ai_chain));
 struct ai_tray *v = tray(Sp[0]);                 // re-read post-Have
 struct ai_chain *p = (struct ai_chain*) Hp;
 Hp += r * Width(struct ai_chain);
 word list = ZeroPoint;                             // () terminator (zero-ontology)
 for (uintptr_t i = r; i--; )
  ini_chain(p, putcharm(v->shape[i]), list), list = word(p), p++;
 ai_musttail return Answer(list); }


// ai_O reductions (sum/prod/max/min) fold through the promoting scalar op, so an
// object array reduces *exactly*. defined after the object lane (below); the
// numeric reductions divert here when their operand is a ai_O array.
struct ai *ored(struct ai *g, int kind);   // kind: 0 sum, 1 prod, 2 max, 3 min

// --- reductions: array -> scalar; identity on a scalar, which makes
// (aall (< a b)) rank-agnostic
lvm(lvm_asum) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Next(1);        // scalar: (asum 5) = 5
 if (tray(x)->type == ai_O) {
  Pack(g); g = ored(g, 0);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (tray(x)->type == ai_C) {                   // complex sum -> a complex box
  struct ai_tray *v = tray(x); uintptr_t n = tray_nelem(v);  // K=4 accumulators (see aprod)
  ai_flo_t *fp = tray_data(v);                   // read all parts before Have (no alloc here)
  ai_flo_t a0=0,b0=0, a1=0,b1=0, a2=0,b2=0, a3=0,b3=0; uintptr_t j = 0;
  for (; j + 4 <= n; j += 4) {
   a0 += fp[2*j];   b0 += fp[2*j+1]; a1 += fp[2*j+2]; b1 += fp[2*j+3];
   a2 += fp[2*j+4]; b2 += fp[2*j+5]; a3 += fp[2*j+6]; b3 += fp[2*j+7]; }
  for (; j < n; j++) a0 += fp[2*j], b0 += fp[2*j+1];
  ai_flo_t sr = (a0+a1)+(a2+a3), si = (b0+b1)+(b2+b3);
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, sr, si); ai_musttail return Next(1); }
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 bool fdom = v->type >= ai_R; word _res;
 Have(box_req);
 v = tray(Sp[0]);
 if (fdom) {                                    // K=4 accumulators (see aprod complex)
  ai_flo_t a0=0,a1=0,a2=0,a3=0; uintptr_t i = 0;
  for (; i + 4 <= n; i += 4) a0+=tray_get_flo(v,i), a1+=tray_get_flo(v,i+1), a2+=tray_get_flo(v,i+2), a3+=tray_get_flo(v,i+3);
  for (; i < n; i++) a0 += tray_get_flo(v, i);
  emit_gem(_res, (a0+a1)+(a2+a3)); }
 else {                                         // K=4 (modular, Z/2^64 is a commutative ring -> assoc+exact)
  uintptr_t a0=0,a1=0,a2=0,a3=0, i = 0;
  for (; i + 4 <= n; i += 4) a0+=(uintptr_t)tray_get_int(v,i), a1+=(uintptr_t)tray_get_int(v,i+1), a2+=(uintptr_t)tray_get_int(v,i+2), a3+=(uintptr_t)tray_get_int(v,i+3);
  for (; i < n; i++) a0 += (uintptr_t) tray_get_int(v, i);
  emit_int(_res, (intptr_t) ((a0+a1)+(a2+a3))); }
 ai_musttail return Answer(_res); }

lvm(lvm_aprod) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Next(1);
 if (tray(x)->type == ai_O) {
  Pack(g); g = ored(g, 1);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (tray(x)->type == ai_C) {                   // complex product -> a complex box
  // K=4 independent accumulators break the multiply latency chain (~3x);
  // reassociation is sound -- fp differs only in last-bit rounding per grouping
  struct ai_tray *v = tray(x); uintptr_t n = tray_nelem(v);
  ai_flo_t *fp = tray_data(v);
  ai_flo_t r0=1,i0=0, r1=1,i1=0, r2=1,i2=0, r3=1,i3=0, t; uintptr_t j = 0;
  for (; j + 4 <= n; j += 4) {
   t = r0*fp[2*j]  -i0*fp[2*j+1]; i0 = r0*fp[2*j+1]+i0*fp[2*j];   r0 = t;
   t = r1*fp[2*j+2]-i1*fp[2*j+3]; i1 = r1*fp[2*j+3]+i1*fp[2*j+2]; r1 = t;
   t = r2*fp[2*j+4]-i2*fp[2*j+5]; i2 = r2*fp[2*j+5]+i2*fp[2*j+4]; r2 = t;
   t = r3*fp[2*j+6]-i3*fp[2*j+7]; i3 = r3*fp[2*j+7]+i3*fp[2*j+6]; r3 = t; }
  for (; j < n; j++) { t = r0*fp[2*j]-i0*fp[2*j+1]; i0 = r0*fp[2*j+1]+i0*fp[2*j]; r0 = t; }
  ai_flo_t ra = r0*r1-i0*i1, ia = r0*i1+i0*r1, rb = r2*r3-i2*i3, ib = r2*i3+i2*r3;
  ai_flo_t pr = ra*rb-ia*ib, pi = ra*ib+ia*rb;
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, pr, pi); ai_musttail return Next(1); }
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 bool fdom = v->type >= ai_R; word _res;
 Have(box_req); v = tray(Sp[0]);
 if (fdom) {                                    // K=4 accumulators (see complex above)
  ai_flo_t a0=1,a1=1,a2=1,a3=1; uintptr_t i = 0;
  for (; i + 4 <= n; i += 4) a0*=tray_get_flo(v,i), a1*=tray_get_flo(v,i+1), a2*=tray_get_flo(v,i+2), a3*=tray_get_flo(v,i+3);
  for (; i < n; i++) a0 *= tray_get_flo(v, i);
  emit_gem(_res, (a0*a1)*(a2*a3)); }
 else {                                         // K=4 (modular product; imul is latency-bound, ~3x)
  uintptr_t a0=1,a1=1,a2=1,a3=1, i = 0;
  for (; i + 4 <= n; i += 4) a0*=(uintptr_t)tray_get_int(v,i), a1*=(uintptr_t)tray_get_int(v,i+1), a2*=(uintptr_t)tray_get_int(v,i+2), a3*=(uintptr_t)tray_get_int(v,i+3);
  for (; i < n; i++) a0 *= (uintptr_t) tray_get_int(v, i);
  emit_int(_res, (intptr_t) ((a0*a1)*(a2*a3))); }
 ai_musttail return Answer(_res); }

// max / min over a non-empty array (kind 2 = max, 3 = min, matching ored);
// empty -> zero; scalar -> identity. the kind selects the comparison sense.
static lvm(lvm_aextreme) {
 int kind = (int) g->b;
 word x = Sp[0];
 if (!packp(x)) return Next(1);
 if (tray(x)->type == ai_O) {
  Pack(g); g = ored(g, kind);
  if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
  return Resume(); }
 if (tray(x)->type == ai_C) return Answer(ZeroPoint);   // complex: unordered
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 if (!n) return Answer(ZeroPoint);
 bool fdom = v->type >= ai_R, ismax = kind == 2; word _res;
 Have(box_req); v = tray(Sp[0]);
 // K=4 running extremes break the latency chain; exact (selects an existing element)
 if (fdom) { ai_flo_t m0 = tray_get_flo(v, 0), m1=m0, m2=m0, m3=m0, e; uintptr_t i = 1;
  for (; i + 4 <= n; i += 4) {
   e = tray_get_flo(v,i);   if (ismax?e>m0:e<m0) m0=e;
   e = tray_get_flo(v,i+1); if (ismax?e>m1:e<m1) m1=e;
   e = tray_get_flo(v,i+2); if (ismax?e>m2:e<m2) m2=e;
   e = tray_get_flo(v,i+3); if (ismax?e>m3:e<m3) m3=e; }
  for (; i < n; i++) { e = tray_get_flo(v,i); if (ismax?e>m0:e<m0) m0=e; }
  if (ismax?m1>m0:m1<m0) m0=m1;
  if (ismax?m2>m0:m2<m0) m0=m2;
  if (ismax?m3>m0:m3<m0) m0=m3;
  emit_gem(_res, m0); }
 else { intptr_t m0 = tray_get_int(v, 0), m1=m0, m2=m0, m3=m0, e; uintptr_t i = 1;
  for (; i + 4 <= n; i += 4) {
   e = tray_get_int(v,i);   if (ismax?e>m0:e<m0) m0=e;
   e = tray_get_int(v,i+1); if (ismax?e>m1:e<m1) m1=e;
   e = tray_get_int(v,i+2); if (ismax?e>m2:e<m2) m2=e;
   e = tray_get_int(v,i+3); if (ismax?e>m3:e<m3) m3=e; }
  for (; i < n; i++) { e = tray_get_int(v,i); if (ismax?e>m0:e<m0) m0=e; }
  if (ismax?m1>m0:m1<m0) m0=m1;
  if (ismax?m2>m0:m2<m0) m0=m2;
  if (ismax?m3>m0:m3<m0) m0=m3;
  emit_int(_res, m0); }
 return Answer(_res); }
lvm(lvm_max) { { g->b = (ai_word) (2); ai_musttail return Ap(lvm_aextreme, g); } }
lvm(lvm_min) { { g->b = (ai_word) (3); ai_musttail return Ap(lvm_aextreme, g); } }

// aall: the bool conjunction reduction ("no zero element"; empty -> vacuously
// true; scalar -> identity). the disjunction is just `len`.
lvm(lvm_aall) {
 word x = Sp[0];
 if (!packp(x)) ai_musttail return Next(1);
 struct ai_tray *v = tray(x);
 uintptr_t n = tray_nelem(v);
 if (v->type == ai_O) {                         // object: a falsy element fails the conjunction
  for (uintptr_t i = 0; i < n; i++)
   if (ai_nilp(g, tray_get_obj(v, i))) ai_musttail return Answer(zero);
  ai_musttail return Answer(putcharm(1)); }
 if (v->type == ai_C) {                         // complex: a 0+0i element fails the conjunction
  ai_flo_t *fp = tray_data(v);
  for (uintptr_t i = 0; i < n; i++)
   if (fp[2*i] == 0 && fp[2*i+1] == 0) ai_musttail return Answer(zero);
  ai_musttail return Answer(putcharm(1)); }
 // a short-circuit sound, not an accumulator chain -- already load-bound (the
 // compiler vectorizes it), so multi-accumulating buys nothing; left as is.
 bool fdom = v->type >= ai_R;
 for (uintptr_t i = 0; i < n; i++)
  if (fdom ? tray_get_flo(v, i) == 0 : tray_get_int(v, i) == 0)
   ai_musttail return Answer(zero);
 ai_musttail return Answer(putcharm(1)); }

// (outer a b): result[I,J] = a[I] * b[J], shape a.shape ++ b.shape; complex/
// object or over-rank -> zero
lvm(lvm_outer) {
 word a = Sp[0], b = Sp[1];
 if (!(trayp(a) && trayp(b))) ai_musttail return Push(ZeroPoint);
 struct ai_tray *va = tray(a), *vb = tray(b);
 if (va->type > ai_R || vb->type > ai_R) ai_musttail return Push(ZeroPoint);
 uintptr_t M = tray_nelem(va), N = tray_nelem(vb), n = M * N, rank = va->rank + vb->rank;
 if (rank > maxrank) ai_musttail return Push(ZeroPoint);
 bool fdom = va->type == ai_R || vb->type == ai_R;
 enum ai_tray_type rt = fdom ? ai_R : ai_Z;
 uintptr_t bytes = sizeof(struct ai_tray) + rank * sizeof(word) + ai_T[rt] * n;
 Have(b2w(bytes));
 va = tray(Sp[0]), vb = tray(Sp[1]);          // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, rank);
 for (uintptr_t i = 0; i < va->rank; i++) r->shape[i] = va->shape[i];
 for (uintptr_t i = 0; i < vb->rank; i++) r->shape[va->rank + i] = vb->shape[i];
 if (fdom) { ai_flo_t *rp = tray_data(r);
  for (uintptr_t i = 0; i < M; i++) { ai_flo_t av = tray_get_flo(va, i);
   for (uintptr_t j = 0; j < N; j++) rp[i*N+j] = av * tray_get_flo(vb, j); } }
 else { intptr_t *rp = tray_data(r);
  for (uintptr_t i = 0; i < M; i++) { intptr_t av = tray_get_int(va, i);
   for (uintptr_t j = 0; j < N; j++) rp[i*N+j] = (intptr_t)((uintptr_t)av * (uintptr_t)tray_get_int(vb, j)); } }
 ai_musttail return Push(word(r)); }   // arity 2

// (inner a b): +.× -- contract a's last axis with b's first (1D·1D = dot, 2D·2D =
// matmul); mismatch/complex/object/over-rank -> zero. ikj order so the inner j
// loop vectorizes.
lvm(lvm_inner) {
 word a = Sp[0], b = Sp[1];
 if (!(trayp(a) && trayp(b))) ai_musttail return Push(ZeroPoint);
 struct ai_tray *va = tray(a), *vb = tray(b);
 if (va->type > ai_R || vb->type > ai_R || va->rank < 1 || vb->rank < 1)
  ai_musttail return Push(ZeroPoint);
 uintptr_t K = va->shape[va->rank - 1];
 if (K != vb->shape[0]) ai_musttail return Push(ZeroPoint);   // contracted axes must agree
 uintptr_t M = 1, N = 1;
 for (uintptr_t i = 0; i + 1 < va->rank; i++) M *= va->shape[i];
 for (uintptr_t i = 1; i < vb->rank; i++) N *= vb->shape[i];
 uintptr_t rank = (va->rank - 1) + (vb->rank - 1), n = M * N;
 if (rank > maxrank) ai_musttail return Push(ZeroPoint);
 bool fdom = va->type == ai_R || vb->type == ai_R, ar = va->type == ai_R, br = vb->type == ai_R;
 if (n == 1) {                                  // 1D·1D dot, or any 1-cell contraction -> scalar (invariant)
  word _res;
  if (fdom) {
   ai_flo_t *Ad = tray_data(va), *Bd = tray_data(vb);
   intptr_t *Ai = tray_data(va), *Bi = tray_data(vb); ai_flo_t acc = 0;
   for (uintptr_t l = 0; l < K; l++) acc += (ar ? Ad[l] : (ai_flo_t) Ai[l]) * (br ? Bd[l] : (ai_flo_t) Bi[l]);
   Have(box_req); emit_gem(_res, acc); }
  else {
   intptr_t *A = tray_data(va), *B = tray_data(vb);
   uintptr_t acc = 0;
   for (uintptr_t l = 0; l < K; l++) acc += (uintptr_t) A[l] * (uintptr_t) B[l];
   Have(box_req);
   emit_int(_res, (intptr_t) acc); }
  ai_musttail return Push(_res); }
 enum ai_tray_type rt = fdom ? ai_R : ai_Z;
 uintptr_t bytes = sizeof(struct ai_tray) + rank * sizeof(word) + ai_T[rt] * n;
 Have(b2w(bytes));
 va = tray(Sp[0]), vb = tray(Sp[1]);
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, rank);
 { uintptr_t s = 0;
   for (uintptr_t i = 0; i + 1 < va->rank; i++) r->shape[s++] = va->shape[i];
   for (uintptr_t i = 1; i < vb->rank; i++) r->shape[s++] = vb->shape[i]; }
 if (fdom) { ai_flo_t *C = tray_data(r);
  ai_flo_t *Ad = tray_data(va), *Bd = tray_data(vb);
  intptr_t *Ai = tray_data(va), *Bi = tray_data(vb);
  for (uintptr_t p = 0; p < n; p++) C[p] = 0;
  for (uintptr_t i = 0; i < M; i++)
   for (uintptr_t l = 0; l < K; l++) { ai_flo_t av = ar ? Ad[i*K+l] : (ai_flo_t) Ai[i*K+l];
    for (uintptr_t j = 0; j < N; j++) C[i*N+j] += av * (br ? Bd[l*N+j] : (ai_flo_t) Bi[l*N+j]); } }
 else { intptr_t *C = tray_data(r), *A = tray_data(va), *B = tray_data(vb);
  for (uintptr_t p = 0; p < n; p++) C[p] = 0;
  for (uintptr_t i = 0; i < M; i++)
   for (uintptr_t l = 0; l < K; l++) { intptr_t av = A[i*K+l];
    for (uintptr_t j = 0; j < N; j++) C[i*N+j] = (intptr_t)((uintptr_t)C[i*N+j] + (uintptr_t)av * (uintptr_t)B[l*N+j]); } }
 ai_musttail return Push(word(r)); }   // arity 2

// --- elementwise monadic math over an array -> a float array of the same shape;
// the fill loop takes no &local, so the lvm wrapper keeps its tail call
static ai_noinline void vmap1_fill(struct ai_tray *r, struct ai_tray *a, ai_flo_t (*fn)(ai_flo_t)) {
 uintptr_t n = tray_nelem(r);
 for (uintptr_t i = 0; i < n; i++) tray_put_flo(r, i, fn(tray_get_flo(a, i))); }

lvm(lvm_vmap1) {
 ai_flo1 fn = (ai_flo1) (uintptr_t) g->b;
 struct ai_tray *a = tray(Sp[0]);
 uintptr_t rank = a->rank, n = tray_nelem(a),
           bytes = sizeof(struct ai_tray) + rank * sizeof(word) + n * ai_T[ai_R];
 Have(b2w(bytes));
 a = tray(Sp[0]);                               // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(r, ai_R, rank);
 for (uintptr_t i = 0; i < rank; i++) r->shape[i] = a->shape[i];
 vmap1_fill(r, a, fn);
 return Answer(word(r)); }

// --- elementwise dyadic engine with broadcasting. integer division guards /0
// and INT_MIN/-1 -> 0 (one element can't change the whole result's domain).
ai_flo_t vop_flo(int op, ai_flo_t a, ai_flo_t b) {
 switch (op) {
  case vop_sub: return a - b; case vop_mul: return a * b;
  case vop_quot: return a / b; case vop_fquot: return ai_trunc(a / b);
  case vop_rem: return ai_fmod(a, b);
  default: return a + b; } }                   // vop_add
static intptr_t vop_int(int op, intptr_t a, intptr_t b) {
 switch (op) {
  case vop_sub: return (intptr_t)((uintptr_t) a - (uintptr_t) b);
  case vop_mul: return (intptr_t)((uintptr_t) a * (uintptr_t) b);
  case vop_quot: case vop_fquot: return (b == 0 || (a == INTPTR_MIN && b == -1)) ? 0 : a / b;
  case vop_rem:  return (b == 0 || (a == INTPTR_MIN && b == -1)) ? 0 : a % b;
  case vop_band: return a & b;
  case vop_bor:  return a | b;
  case vop_bxor: return a ^ b;
  case vop_bsl:  return (intptr_t)((uintptr_t) a << shmask(b));
  case vop_bsr:  return a >> shmask(b);
  default: return (intptr_t)((uintptr_t) a + (uintptr_t) b); } } // vop_add
intptr_t vcmp_flo(int op, ai_flo_t a, ai_flo_t b) {
 switch (op) {
  case vop_lt: return a < b; case vop_le: return a <= b;
  case vop_gt: return a > b; case vop_ge: return a >= b;
  default: return a == b; } }                   // vop_eq
intptr_t vcmp_int(int op, intptr_t a, intptr_t b) {
 switch (op) {
  case vop_lt: return a < b; case vop_le: return a <= b;
  case vop_gt: return a > b; case vop_ge: return a >= b;
  default: return a == b; } }                   // vop_eq

// === ordered comparison: the true-blue total order over all values ===========
// low -> high: () < mint < string < number < tray < chain < map < hot (an array
// operand compares elementwise via lvm_vbin instead -- the mask). within a band:
// numbers by value across the tower (complex lexicographic by (re, im), NaN
// unordered), strings lex, symbols by name then serial, chains lex recursively,
// lambdas/maps by repr hash (GC-stable). only < and <= are implemented; > and >=
// reverse the operands (right for NaN: swap, never negate). a total preorder:
// hash-colliding lambdas compare equal but are not =. the compare order is
// decoupled from the enum dispatch order -- cmp_rank remaps, the matrices untouched.
ai_inline int cmp_rank(struct ai *g, word x) {
 if (nomp(x)) return 0;                            // mint/symbol -- the floor (a named sym is a (name . mint) chain)
 enum q k = ai_kind(x);
 if (k == KString) return 1;                       // string: above mint, below number
 if (isnum(x) || twinp(x)) return 2;               // the number band, by value (charm bridges up from string)
 if (k == KTrayZ || k == KTrayR || k == KTrayC) return 2;  // a galaxy folds into the number band, ordered by its net
 if (k == KTrayO) return 3;                         // object tray: above the numbers, below chain
 if (k == KChain) return 4;                        // chain: the grammar substrate -- high, just under book (only book's mutability seats it above)
 if (k == KTablet) return 5;                          // tablet: above chain
 if (coinp(x) && die_get(g, coin_die(x), DieNet) == putcharm(2))
  return 2;                                        // a ratio coin seats in the number band, by its value
 return 6; }                                       // KHot -- thread/function, the ceiling (the only kind left)
static ai_inline intptr_t bytes_cmp(const char *pa, uintptr_t la, const char *pb, uintptr_t lb) {
 uintptr_t n = la < lb ? la : lb;
 int c = n ? memcmp(pa, pb, n) : 0;
 return c ? (c < 0 ? -1 : 1) : la < lb ? -1 : la > lb ? 1 : 0; }
// the floor band (cmp_rank 0): () < bare mints (KMint, by serial) < named points
// (KNom, by name lex then serial). () is the serial-0 point, seated least of all by
// an identity guard; a named point outranks every bare mint (the na/nb split below).
static ai_inline intptr_t mint_cmp(struct ai *g, word a, word b) {
 if (a == b) return 0;
 word core = ZeroPoint;                       // () is the nameless serial-0 point: least of all
 if (a == core) return -1;                               // (a != b, so b is some other mint above it)
 if (b == core) return 1;                                // -- guarded by identity, its atom slots are never read
 // a named point is a KNom; a bare mint is the nameless atom. bare mints rank below
 // every named symbol; within a band, named by (name lex, then serial), bare by serial.
 bool na = namep(a), nb = namep(b);
 if (na != nb) return na ? 1 : -1;                       // bare mint < named symbol
 if (na) {                                               // both named (KNom): name first, then the serial
  struct ai_str *sa = str(nom(a)->name), *sb = str(nom(b)->name);
  intptr_t c = bytes_cmp(txt(sa), len(sa), txt(sb), len(sb));
  if (c) return c;
  uintptr_t ma = nom(a)->code, mb = nom(b)->code;
  return ma < mb ? -1 : ma > mb ? 1 : 0; }
 uintptr_t ca = sym(a)->code, cb = sym(b)->code;         // both bare: by serial
 return ca < cb ? -1 : ca > cb ? 1 : 0; }
// two galaxies of equal net: a strict tiebreak so cmp3 stays antisymmetric --
// shape lexicographically (rank, then dims), then cell content (re, then im),
// row-major. reached only from the number band below, both operands galaxies.
ai_inline struct ai_zn tray_cell_zn(struct ai_tray *v, uintptr_t i) {
 if (v->type == ai_C) { ai_flo_t *d = tray_data(v); return zn(d[2*i], d[2*i+1]); }
 return zn(tray_get_flo(v, i), 0); }
static intptr_t galaxy_tie(struct ai_tray *va, struct ai_tray *vb) {
 if (va->rank != vb->rank) return va->rank < vb->rank ? -1 : 1;
 for (uintptr_t i = 0; i < va->rank; i++)
  if (va->shape[i] != vb->shape[i]) return va->shape[i] < vb->shape[i] ? -1 : 1;
 uintptr_t n = tray_nelem(va);                             // same shape -> same nelem
 for (uintptr_t i = 0; i < n; i++) {
  struct ai_zn ea = tray_cell_zn(va, i), eb = tray_cell_zn(vb, i);
  if (ea.re != eb.re) return ea.re < eb.re ? -1 : 1;
  if (ea.im != eb.im) return ea.im < eb.im ? -1 : 1; }
 return 0; }
// a ratio coin orders by its value: int64-fitting components cross-multiply
// exactly (near-equal rationals order right where the float quotient ties);
// anything wider falls to the sign-exact net quotients.
ai_inline bool ratio_ifit(word x, int64_t *v) {
 if (charmp(x) || sunp(x)) return *v = toint(x), true;
 if (!bigp(x)) return false;
 struct ai_big *b = big(x);
 int n = big_nlimbs(x);
 if (n * limb_bits > 64) return false;
 uint64_t m = b->limb[n-1];
 for (int i = n - 2; i >= 0; i--) m = (m << (limb_bits - 1) << 1) | b->limb[i];
 bool neg = b->slen < 0;
 if (m > (uint64_t) INT64_MAX + neg) return false;
 return *v = (int64_t) (neg ? 0 - m : m), true; }
ai_inline bool ratio_iview(word x, int64_t *n, int64_t *d) {
 if (coinp(x)) { word p = coin_load(x);
  if (!chainp(p) || !chainp(B(p))) return false;
  return ratio_ifit(A(p), n) && ratio_ifit(A(B(p)), d) && *d != 0; }
 return ratio_ifit(x, n) && (*d = 1, true); }
#if !defined(__SIZEOF_INT128__)
// u64 x u64 -> 128-bit magnitude product from 32-bit half-word partials -- the exact
// cross-multiply for builds without __int128 (mooncc's love-raw; the thumb ports).
ai_inline void ratio_mag_mul(uint64_t a, uint64_t b, uint64_t *hi, uint64_t *lo) {
 uint64_t mask = 0xFFFFFFFFu,
          a0 = a & mask, a1 = a >> 32, b0 = b & mask, b1 = b >> 32,
          p00 = a0 * b0, p01 = a0 * b1, p10 = a1 * b0, p11 = a1 * b1,
          mid = (p00 >> 32) + (p01 & mask) + (p10 & mask);
 *lo = (p00 & mask) | (mid << 32);
 *hi = p11 + (p01 >> 32) + (p10 >> 32) + (mid >> 32); }
#endif
static ai_inline bool ratio_xcmp(int64_t n1, int64_t d1, int64_t n2, int64_t d2, intptr_t *c) {
 intptr_t s = (d1 < 0) != (d2 < 0) ? -1 : 1;
#if defined(__SIZEOF_INT128__)
 __int128 l = (__int128) n1 * d2, r = (__int128) n2 * d1;
 return *c = l == r ? 0 : (l < r ? -s : s), true;
#else
 // exact sign + magnitude: |n1|*|d2| vs |n2|*|d1| as double-word pairs, signs on top.
 uint64_t la = n1 < 0 ? (uint64_t) 0 - (uint64_t) n1 : (uint64_t) n1,
          lb = d2 < 0 ? (uint64_t) 0 - (uint64_t) d2 : (uint64_t) d2,
          ra = n2 < 0 ? (uint64_t) 0 - (uint64_t) n2 : (uint64_t) n2,
          rb = d1 < 0 ? (uint64_t) 0 - (uint64_t) d1 : (uint64_t) d1,
          lhi, llo, rhi, rlo;
 ratio_mag_mul(la, lb, &lhi, &llo);
 ratio_mag_mul(ra, rb, &rhi, &rlo);
 bool zl = !(lhi | llo), zr = !(rhi | rlo);
 bool sl = !zl && ((n1 < 0) != (d2 < 0)), sr = !zr && ((n2 < 0) != (d1 < 0));
 intptr_t cl;                                     // -1/0/1 of l - r, signs first then magnitudes
 if (sl != sr) cl = sl ? -1 : 1;
 else { intptr_t cm = lhi != rhi ? (lhi < rhi ? -1 : 1) : llo != rlo ? (llo < rlo ? -1 : 1) : 0;
        cl = sl ? -cm : cm; }
 return *c = cl == 0 ? 0 : (cl < 0 ? -s : s), true;
#endif
}
// 3-way total-order comparator (-1/0/1); the recursive engine for the chain case.
// floats collapse NaN to "equal" here (a structural total order can't carry IEEE
// unorderedness); the scalar lane below keeps NaN unordered at the top level. hash
// is alloc-free + GC-stable, so the lambda case is safe to call mid-comparison.
intptr_t cmp3(struct ai *g, word a, word b) {
 int ra = cmp_rank(g, a), rb = cmp_rank(g, b);
 if (ra != rb) return ra < rb ? -1 : 1;                    // cross-kind: the true-blue lattice (cmp_rank)
 // same band -- dispatch by the actual kind (not the synthetic cmp_rank, which remaps mint/
 // string/tray/chain off their enum ordinal). symbols first: a named sym is a chain, so the chain
 // recursion below would otherwise grab it.
 if (nomp(a)) return mint_cmp(g, a, b);                    // mint band: () < bare mints < named syms
 if (ra == 2) {                                            // number band: stars + galaxies, ordered by net
  if (coinp(a) || coinp(b)) {                              // a ratio coin in the band (cmp_rank read its
   int64_t n1, d1, n2, d2; intptr_t c;                     // mode-2 die): int64-fitting components -> exact
   if (ratio_iview(a, &n1, &d1) && ratio_iview(b, &n2, &d2)
       && ratio_xcmp(n1, d1, n2, d2, &c)) return c;
   struct ai_zn za = ai_net(g, a), zb = ai_net(g, b);      // else the sign-exact quotients
   if (za.re != zb.re) return za.re < zb.re ? -1 : 1;
   if (za.im != zb.im) return za.im < zb.im ? -1 : 1;
   if (galaxyp(a) != galaxyp(b)) return galaxyp(a) ? 1 : -1;  // net tie vs a galaxy: the star seats below
   return 0; }
  if (galaxyp(a) || galaxyp(b)) {                          // a galaxy in play -> by net (re, im), then star<galaxy, then shape/content
   bool ga = galaxyp(a), gb = galaxyp(b);
   struct ai_zn na = ai_net(g, a), nb = ai_net(g, b);
   if (na.re != nb.re) return na.re < nb.re ? -1 : 1;
   if (na.im != nb.im) return na.im < nb.im ? -1 : 1;
   if (ga != gb) return ga ? 1 : -1;                       // net tie: a star seats below a galaxy
   return galaxy_tie(tray(a), tray(b)); }                    // both galaxies, equal net: shape then content
  if (twinp(a) || twinp(b)) {                              // both scalars -- complex: (re, im) lexicographic
   ai_flo_t ar = twinp(a) ? twin_re(a) : toflo(a), br = twinp(b) ? twin_re(b) : toflo(b);
   if (ar != br) return ar < br ? -1 : 1;
   ai_flo_t ai = twinp(a) ? twin_im(a) : 0, bi = twinp(b) ? twin_im(b) : 0;
   return ai < bi ? -1 : ai > bi ? 1 : 0; }
  if (gemp(a) || gemp(b)) { ai_flo_t av = toflo(a), bv = toflo(b); return av < bv ? -1 : av > bv ? 1 : 0; }
  return ai_big_cmp(a, b); }                                // exact fix/box/big tower
 if (strp(a)) return bytes_cmp(txt(a), len(a), txt(b), len(b));
 if (chainp(a)) { intptr_t c = cmp3(g, A(a), A(b)); return c ? c : cmp3(g, B(a), B(b)); }  // chain: car, then cdr
 if (coinp(a) && coinp(b) && coin_die(a) == coin_die(b))  // same die: order by payload
  return cmp3(g, coin_load(a), coin_load(b));
 uintptr_t ha = hash(g, a), hb = hash(g, b);               // lambda/map/port/cask: by repr hash
 return ha < hb ? -1 : ha > hb ? 1 : 0; }

// (sort l): stable ascending merge by cmp3 -- one reservation up front (n result
// chains + 2n scratch in the uncommitted gap), and cmp3 is alloc-free, so nothing
// moves between reservation and fill. prel's sort dispatches (<)/(>) here.
// (tally x): the count -- how many, never how much: a string/cask its charms, a
// list its spine, an array its cells, a map its keys, a symbol its spelling.
intptr_t ai_count(struct ai *g, word l) {
 while (coinp(l)) l = coin_load(l);                  // a coin tallies its payload
 if (strp(l)) return (intptr_t) len(l);
 if (caskp(l)) return (intptr_t) len(cask(l)->str);
 if (tabp(l)) return (intptr_t) map_len(l);
 if (trayp(l)) return (intptr_t) tray_nelem(tray(l));
 if (nomp(l)) { struct ai_str *nm = nom_str(g, l); return nm ? (intptr_t) len(nm) : 0; }  // a sym counts its spelling; a bare mint / the core: 0
 intptr_t n = 0;
 while (chainp(l)) n++, l = B(l);
 return n; }

lvm(lvm_tally) {
 Sp[0] = putcharm(ai_count(g, Sp[0]));
 ai_musttail return Next(1); }

// (long? n l): is l a chain at least n links deep -- the arity question, asked once.
// `two?` per step is the spelling that reads, and it costs a cup, a load and a
// dispatch each; a destructuring pattern asks it n times to reach n fields
// (love/post.l). n <= 0 is true of anything: no link is claimed.
lvm(lvm_longp) { word l = Sp[1];
 if (!charmp(Sp[0])) ai_musttail return Push(zero);
 for (intptr_t k = getcharm(Sp[0]); k > 0; k--) {
  if (!chainp(l) || nomp(l)) ai_musttail return Push(zero);
  l = B(l); }
 ai_musttail return Push(putcharm(1)); }

lvm(lvm_sort) {
 word l = Sp[0];
 if (!chainp(l) || !chainp(B(l))) ai_musttail return Next(1);
 uintptr_t n = 0;
 for (word p = l; chainp(p); p = B(p)) n++;
 uintptr_t req = n * Width(struct ai_chain) + 2 * n;
 Have(req);
 l = Sp[0];                                        // re-read post-GC
 struct ai_chain *spine = (struct ai_chain*) Hp;
 Hp += n * Width(struct ai_chain);                   // commit the spine only
 word *a = (word*) Hp, *b = a + n;                 // scratch: the uncommitted gap
 uintptr_t i = 0;
 for (word p = l; chainp(p); p = B(p)) a[i++] = A(p);
 for (i = 0; i < n; i++) if (!charmp(a[i])) break;   // all-fixnum fast path: a tagged fixnum (v<<1|1)
 bool allfix = i == n;                               // orders as a signed word, so skip the generic cmp3
 for (uintptr_t w = 1; w < n; w *= 2) {            // bottom-up stable merge
  for (uintptr_t lo = 0; lo < n; lo += 2 * w) {
   uintptr_t m = min(lo + w, n), hi = min(lo + 2 * w, n), x = lo, y = m, o = lo;
   if (allfix) while (x < m && y < hi) b[o++] = (intptr_t) a[y] < (intptr_t) a[x] ? a[y++] : a[x++];   // branch once per segment, not per compare
   else        while (x < m && y < hi) b[o++] = cmp3(g, a[y], a[x]) < 0 ? a[y++] : a[x++];
   while (x < m) b[o++] = a[x++];
   while (y < hi) b[o++] = a[y++]; }
  word *t = a; a = b; b = t; }
 for (i = 0; i < n; i++) ini_chain(spine + i, a[i], word(spine + i + 1));
 spine[n - 1].b = ZeroPoint;                        // () terminator (zero-ontology)
 ai_musttail return Answer(word(spine)); }

// the `<` / `<=` lane (op is vop_lt or vop_le). an array operand -> elementwise
// mask (lvm_vbin); a top-level float/complex chain is IEEE-faithful (NaN ->
// unordered -> false), so e.g. (<= nan nan) is zero.
static lvm(lvm_cmp_ord) {
 int op = (int) g->b;
 word a = Sp[0], b = Sp[1]; intptr_t r;
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (op); ai_musttail return Ap(lvm_vbin, g); }      // array -> elementwise
 int ra = cmp_rank(g, a), rb = cmp_rank(g, b);
 if (ra != rb) r = vcmp_int(op, ra, rb);                   // cross-kind: the true-blue lattice (cmp_rank)
 else if (!(isnum(a) || twinp(a)) || coinp(b)) r = vcmp_int(op, cmp3(g, a, b), 0);  // same non-number band, or a ratio coin either side (a coin as `a` fails isnum; as `b` this catches it): via cmp3
 else if (twinp(a) || twinp(b)) {                          // complex: lexicographic, per op
  ai_flo_t ar = twinp(a) ? twin_re(a) : toflo(a), br = twinp(b) ? twin_re(b) : toflo(b);
  r = ar != br ? vcmp_flo(op, ar, br)
              : vcmp_flo(op, twinp(a) ? twin_im(a) : 0, twinp(b) ? twin_im(b) : 0); }
 else if (gemp(a) || gemp(b)) r = vcmp_flo(op, toflo(a), toflo(b));
 else if (bigp(a) || bigp(b)) r = vcmp_int(op, ai_big_cmp(a, b), 0);
 else r = vcmp_int(op, toint(a), toint(b));
 return Push(r ? putcharm(1) : zero); }
// `<` `<=` are the implemented side (both-fixnum fast path: tagged order is
// monotonic); `>` `>=` reverse the operands. cond fusion: when the fast path sees
// lvm_cond next it branches directly (true -> Ip+3, false -> Ip[2].m) instead of
// materializing a boolean and paying a second dispatch; the slow path falls
// through to the retained lvm_cond. the gt/ge reversers fuse for free.
cmp_lt(lvm_lt, vop_lt) cmp_lt(lvm_le, vop_le)
#undef cmp_lt
lvm(lvm_gt) { word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t; ai_musttail return Ap(lvm_lt, g); }  // a > b == b < a
lvm(lvm_ge) { word t = Sp[0]; Sp[0] = Sp[1], Sp[1] = t; ai_musttail return Ap(lvm_le, g); }  // a >= b == b <= a

// comparison from a 3-way sign: a bignum is always out of machine-int range, so
// it orders against any int element by its sign alone -- exactly
static intptr_t vcmp_sign(int op, int s) {
 switch (op) {
  case vop_lt: return s < 0; case vop_le: return s <= 0;
  case vop_gt: return s > 0; case vop_ge: return s >= 0;
  default: return s == 0; } }                   // vop_eq

// the broadcast dim: a size-1 axis takes the other size -- including 0, so an
// empty axis stays empty (a max would fill one element out of an empty operand)
static ai_inline uintptr_t bdim(uintptr_t da, uintptr_t db) {
 return da == 1 ? db : db == 1 ? da : da; }

// the broadcast plumbing shared by every elementwise lane; the shape walk runs
// twice (gate before Have, fill after), re-deriving each time (operands move).
// bshape_n: the broadcast element count, or (uintptr_t) -1 on non-conformance.
uintptr_t bshape_n(word a, word b) {
 bool atray = trayp(a), btray = trayp(b);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0,
           R = ra > rb ? ra : rb, n = 1;
 for (uintptr_t k = 0; k < R; k++) {
  uintptr_t da = (atray && k < ra) ? tray(a)->shape[ra - 1 - k] : 1,
            db = (btray && k < rb) ? tray(b)->shape[rb - 1 - k] : 1;
  if (da != db && da != 1 && db != 1) return (uintptr_t) -1;
  n *= bdim(da, db); }
 return n; }

// fill shape[0..R) with the broadcast shape of a and b (conformance already
// gated by bshape_n).
void bshape_put(uintptr_t *shape, uintptr_t R, word a, word b) {
 bool atray = trayp(a), btray = trayp(b);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0;
 for (uintptr_t k = 0; k < R; k++) {
  uintptr_t da = (atray && k < ra) ? tray(a)->shape[ra - 1 - k] : 1,
            db = (btray && k < rb) ? tray(b)->shape[rb - 1 - k] : 1;
  shape[R - 1 - k] = bdim(da, db); } }

// c[j]: the operand's flat-offset contribution of result axis j (0 when that
// axis is absent in the operand or is a size-1 broadcast axis); v == 0 reads
// a scalar operand (all zeros).
void bstride(struct ai_tray *v, uintptr_t R, intptr_t *c) {
 for (uintptr_t j = 0; j < R; j++) c[j] = 0;
 if (!v) return;
 intptr_t s = 1;
 for (intptr_t o = (intptr_t) v->rank - 1; o >= 0; o--) {
  intptr_t j = o + (intptr_t) R - (intptr_t) v->rank;
  c[j] = v->shape[o] == 1 ? 0 : s, s *= (intptr_t) v->shape[o]; } }

// one odometer tick over shape[0..R), rightmost axis fastest.
void odo_step(intptr_t *idx, uintptr_t R, uintptr_t const *shape) {
 for (intptr_t j = (intptr_t) R - 1; j >= 0; j--) {
  if (++idx[j] < (intptr_t) shape[j]) break;
  idx[j] = 0; } }

// fill the (already-shaped) result r with a `op` b, broadcasting. all the
// &-taking stack arrays (strides, odometer) live here so the lvm wrapper stays
// TCO-clean. no allocation inside, so operand pointers can't move under us.
static ai_noinline void vbin_fill(struct ai_tray *r, word a, word b, int op, bool fdom) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 // contiguous monotype fast path: no broadcasting, so the odometer and dispatch
 // vanish -- raw pointers, the op hoisted once, a body the compiler vectorizes.
 // mixed/bignum/broadcast falls through to the general loop; results bit-identical.
 { bool cmpf = op >= vop_lt,
        aok = !atray || tray_nelem(va) == n, bok = !btray || tray_nelem(vb) == n,
        nobig = !((!atray && bigp(a)) || (!btray && bigp(b)));
   if (aok && bok && nobig) {
    if (fdom && (!atray || va->type == ai_R) && (!btray || vb->type == ai_R)) {
     ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b);
     ai_flo_t *ap = atray ? (ai_flo_t*) tray_data(va) : 0, *bp = btray ? (ai_flo_t*) tray_data(vb) : 0;
     if (cmpf) { intptr_t *rp = (intptr_t*) tray_data(r);
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { ai_flo_t av = atray?ap[p]:sa, bv = btray?bp[p]:sb; rp[p] = (E)?1:0; } } while (0)
      switch (op) { case vop_lt: VBF(av<bv); return; case vop_le: VBF(av<=bv); return;
        case vop_gt: VBF(av>bv); return; case vop_ge: VBF(av>=bv); return; case vop_eq: VBF(av==bv); return; }
      #undef VBF
     } else { ai_flo_t *rp = (ai_flo_t*) tray_data(r);
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { ai_flo_t av = atray?ap[p]:sa, bv = btray?bp[p]:sb; rp[p] = (E); } } while (0)
      switch (op) { case vop_add: VBF(av+bv); return; case vop_sub: VBF(av-bv); return;
        case vop_mul: VBF(av*bv); return; case vop_quot: VBF(av/bv); return;
        case vop_fquot: VBF(ai_trunc(av/bv)); return; case vop_rem: VBF(ai_fmod(av,bv)); return; }
      #undef VBF
     }
    } else if (!fdom && (!atray || va->type == ai_Z) && (!btray || vb->type == ai_Z)) {
     intptr_t sia = atray ? 0 : (charmp(a) ? (intptr_t) getcharm(a) : sun_get(a)),
              sib = btray ? 0 : (charmp(b) ? (intptr_t) getcharm(b) : sun_get(b)),
              *ap = atray ? (intptr_t*) tray_data(va) : 0, *bp = btray ? (intptr_t*) tray_data(vb) : 0,
              *rp = (intptr_t*) tray_data(r);   // r is ai_Z for both int-arith and the mask
     if (cmpf) {
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { intptr_t av = atray?ap[p]:sia, bv = btray?bp[p]:sib; rp[p] = (E)?1:0; } } while (0)
      switch (op) { case vop_lt: VBF(av<bv); return; case vop_le: VBF(av<=bv); return;
        case vop_gt: VBF(av>bv); return; case vop_ge: VBF(av>=bv); return; case vop_eq: VBF(av==bv); return; }
      #undef VBF
     } else {
      #define VBF(E) do { for (uintptr_t p = 0; p < n; p++) { intptr_t av = atray?ap[p]:sia, bv = btray?bp[p]:sib; rp[p] = (E); } } while (0)
      switch (op) {
        case vop_add: VBF((intptr_t)((uintptr_t)av+(uintptr_t)bv)); return;
        case vop_sub: VBF((intptr_t)((uintptr_t)av-(uintptr_t)bv)); return;
        case vop_mul: VBF((intptr_t)((uintptr_t)av*(uintptr_t)bv)); return;
        case vop_quot: case vop_fquot: VBF((bv==0||(av==INTPTR_MIN&&bv==-1))?0:av/bv); return;
        case vop_rem: VBF((bv==0||(av==INTPTR_MIN&&bv==-1))?0:av%bv); return; }
      #undef VBF
     } } } }
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 bool cmp = op >= vop_lt;
 // the int domain demotes a bignum scalar by low bits for arithmetic, but a
 // comparison against one is decided exactly by its sign below
 ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b);
 intptr_t ia = atray ? 0 : charmp(a) ? getcharm(a) : bigp(a) ? ai_big_low(a) : sun_get(a),
          ib = btray ? 0 : charmp(b) ? getcharm(b) : bigp(b) ? ai_big_low(b) : sun_get(b);
 bool abig = !atray && bigp(a), bbig = !btray && bigp(b);   // at most one (the other is an array)
 int asign = abig ? (big(a)->slen < 0 ? -1 : 1) : 0,
     bsign = bbig ? (big(b)->slen < 0 ? -1 : 1) : 0;
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  if (fdom) {
   ai_flo_t av = atray ? tray_get_flo(va, oa) : sa, bv = btray ? tray_get_flo(vb, ob) : sb;
   if (cmp) tray_put_int(r, p, vcmp_flo(op, av, bv) ? 1 : 0);
   else tray_put_flo(r, p, vop_flo(op, av, bv)); }
  else {
   intptr_t av = atray ? tray_get_int(va, oa) : ia, bv = btray ? tray_get_int(vb, ob) : ib;
   if (cmp) {                                    // bignum side (if any) sorts by sign: a-b ~ asign, or -bsign
    intptr_t t = (abig || bbig) ? vcmp_sign(op, abig ? asign : -bsign) : vcmp_int(op, av, bv);
    tray_put_int(r, p, t ? 1 : 0); }
   else tray_put_int(r, p, vop_int(op, av, bv)); }
  odo_step(idx, R, r->shape); } }

// `/` over the integer domain: true if some element divides inexactly, so the
// whole result promotes to f64 (a bignum scalar forces the float lane). called
// only after conformance is checked.
static ai_noinline bool vquot_needs_float(word a, word b) {
 bool atray = trayp(a), btray = trayp(b);
 if ((!atray && bigp(a)) || (!btray && bigp(b))) return true;
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 uintptr_t ra = atray ? va->rank : 0,
           rb = btray ? vb->rank : 0,
           R = ra > rb ? ra : rb,
           n = bshape_n(a, b), shp[maxrank];
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 bshape_put(shp, R, a, b);
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 intptr_t ia = atray ? 0 : toint(a), ib = btray ? 0 : toint(b);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  intptr_t av = atray ? tray_get_int(va, oa) : ia, bv = btray ? tray_get_int(vb, ob) : ib;
  if (bv == 0 || av % bv != 0) return true;
  odo_step(idx, R, shp); }
 return false; }

lvm(lvm_vbin) {
 int op = (int) g->b;
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 // complex lane first (a complex scalar isn't isnum, so it must divert before
 // the gate below); mixing ai_C with ai_O is unsupported -- the ai_O lane wins
 if (((atray && tray(a)->type == ai_C) || (btray && tray(b)->type == ai_C) || twinp(a) || twinp(b))
     && !(atray && tray(a)->type == ai_O) && !(btray && tray(b)->type == ai_O)) {
  if (vop_bitp(op)) return Push(ZeroPoint);   // no bits on a complex
  { g->b = (ai_word) (op); ai_musttail return Ap(lvm_cbin, g); } }
 if (!(atray || isnum(a)) || !(btray || isnum(b)))   // each operand: array or scalar
  return Push(op == vop_eq ? zero : ZeroPoint);   // `=` is boolean: undefined face -> 0, not ()
 if ((atray && tray(a)->type == ai_O) || (btray && tray(b)->type == ai_O)) {
  // boxed cells are not the word lane: a big refuses the bits on a star, so the
  // object tray refuses them whole rather than answering per-element zero.
  if (vop_bitp(op)) return Push(ZeroPoint);
  { g->b = (ai_word) (op); ai_musttail return Ap(lvm_obin, g); } }                   // object array -> promoting lane
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0,
           R = ra > rb ? ra : rb;
 // compute-type = max element type; a scalar int contributes the lowest type
 // (i8) so it never widens an int array, a scalar float forces the float lane.
 int ta = atray ? (int) tray(a)->type : gemp(a) ? (int) ai_R : (int) ai_Z,
     tb = btray ? (int) tray(b)->type : gemp(b) ? (int) ai_R : (int) ai_Z,
     ct = ta > tb ? ta : tb;
 bool fdom = ct >= ai_R, cmp = op >= vop_lt;
 if (vop_bitp(op) && fdom) return Push(ZeroPoint);   // no bits on a gem
 uintptr_t n = bshape_n(a, b);                     // conformance + result size
 if (n == (uintptr_t) -1) return Push(op == vop_eq ? zero : ZeroPoint);   // non-conformant `=` -> 0
 // `/` over an all-integer broadcast promotes the whole result to f64 the moment
 // any element divides inexactly (matching the scalar `/`); `//` (vop_fquot) stays
 // integer. sound only after conformance is known good (offsets are then in range).
 if (op == vop_quot && !fdom && !cmp && vquot_needs_float(a, b)) fdom = true, ct = ai_R;
 enum ai_tray_type rt = cmp ? ai_Z : (enum ai_tray_type) ct;   // compare -> 0/1 Z mask
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                       // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, R);
 bshape_put(r->shape, R, a, b);
 vbin_fill(r, a, b, op, fdom);
 return Push(word(r)); }

// --- dyadic math map with broadcasting (pow / atan2 over arrays): lvm_vbin's
// float-domain twin -- the result is always a float array, each element fn(av, bv)
static ai_noinline void vmap2_fill(struct ai_tray *r, word a, word b, ai_flo_t (*fn)(ai_flo_t, ai_flo_t)) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  ai_flo_t av = atray ? tray_get_flo(va, oa) : sa, bv = btray ? tray_get_flo(vb, ob) : sb;
  tray_put_flo(r, p, fn(av, bv));
  odo_step(idx, R, r->shape); } }

lvm(lvm_vmap2) {
 ai_flo2 fn = (ai_flo2) (uintptr_t) g->b;
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 if (!(atray || isnum(a)) || !(btray || isnum(b)))   // each operand: array or scalar
  return Push(ZeroPoint);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0,
           R = ra > rb ? ra : rb, n = bshape_n(a, b);
 if (n == (uintptr_t) -1) return Push(ZeroPoint);
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_R];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                       // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, ai_R, R);
 bshape_put(r->shape, R, a, b);
 vmap2_fill(r, a, b, fn);
 return Push(word(r)); }

