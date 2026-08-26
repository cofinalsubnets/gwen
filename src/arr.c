// arr.c -- generic-op lane, rng, eq, obin. one translation unit of the runtime;
// the shared layouts and the cross-TU seam are src/love_int.h.
#include "love_int.h"
// this file's own, forward-declared so order within it does not matter.
static lvm(lvm_mul_cart);
static lvm(lvm_mul_rep);
// ============================================================================
// generic-op lane aps, the dispatch matrices, then the `+`/`*` dispatchers
// ============================================================================

// `*` repeat lane: a sequence times a scalar count n is n copies joined ("repeated
// +"). the count law (the associativity arc): a count acts by |count| when it is an
// exact integer -- magnitude is the one multiplicative hom that survives the sign
// crossing ((-1)*(-2) re-enters the positives) -- and any inexact count (gem, twin,
// tray) answers the absorbing () (those classes are closed under *, so the refusal
// composes: (x * 2.5) * 2 and x * (2.5 * 2 = 5.0) both land ()).
static lvm(lvm_mul_rep) {
 word a = Sp[0], b = Sp[1];
 bool aseq = strp(a) || chainp(a) || namep(a);       // a string / list / named symbol repeats
 word seq = aseq ? a : b, cnt = aseq ? b : a;
 if ((!strp(seq) && !chainp(seq) && !namep(seq)) || (!charmp(cnt) && !bigp(cnt)))
  ai_musttail return Push(ZeroPoint);             // seq not a sequence/symbol, or count not exact
 uintptr_t n;
 if (charmp(cnt)) { intptr_t v = getcharm(cnt); n = (uintptr_t) (v < 0 ? -v : v); }
 else n = (uintptr_t) maxcharm;                      // |big|: past addressable, dies in Have()
 if (chainp(seq)) {                                   // list -> n copies of the spine
  if (!n) ai_musttail return Push(ZeroPoint);   // 0 copies -> the empty list () (zero-ontology)
  uintptr_t m = llen(seq), total = m * n;
  Have(total * Width(struct ai_chain));
  seq = chainp(Sp[0]) ? Sp[0] : Sp[1];                // re-read post-GC
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += total * Width(struct ai_chain);
  for (uintptr_t i = 0; i < n; i++)
   for (word l = seq; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = ZeroPoint;                            // list terminator () (zero-ontology)
  ai_musttail return Push(word(base)); }
 // string / symbol spelling -> repeat the bytes; a symbol re-interns the result
 bool sym = namep(seq);
 struct ai_str *src = sym ? str(nom(seq)->name) : str(seq);
 uintptr_t sl = src->len, total = sl * n;
 if (!total) ai_musttail return Push(sym ? ZeroPoint : EmptyString);  // 0 copies: () for a sym, "" for a string
 uintptr_t req = str_width(total);
 Have(req);
 word sw = sym ? (namep(Sp[0]) ? Sp[0] : Sp[1]) : (strp(Sp[0]) ? Sp[0] : Sp[1]);  // re-read post-GC
 src = sym ? str(nom(sw)->name) : str(sw);
 struct ai_str *z = ini_str(str(Hp), total);
 Hp += req;
 for (uintptr_t i = 0; i < n; i++) memcpy(txt(z) + i * sl, txt(src), sl);
 *++Sp = word(z);
 return sym ? Ap(lvm_intern, g) : (Ip++, Continue()); }

// `*` cartesian lane: chain * chain -> the ordered cartesian product (tally is
// the homomorphism; the outer loop ranges the left operand so right-
// distributivity holds on the nose). 3*pairs chains total, one Have.
static lvm(lvm_mul_cart) {
 word a = Sp[0], b = Sp[1];
 if (!chainp(a) || !chainp(b)) ai_musttail return Push(ZeroPoint);   // chain*chain only
 uintptr_t m = llen(a), n = llen(b), pairs = m * n;
 if (!pairs) ai_musttail return Push(ZeroPoint);             // empty operand annihilates
 Have(3 * pairs * Width(struct ai_chain));
 a = Sp[0], b = Sp[1];                                               // re-read post-GC
 struct ai_chain *spine = (struct ai_chain*) Hp, *pc = spine + pairs;
 Hp += 3 * pairs * Width(struct ai_chain);
 uintptr_t idx = 0;
 for (word la = a; chainp(la); la = B(la)) {
  word av = A(la);
  for (word lb = b; chainp(lb); lb = B(lb), idx++) {
   struct ai_chain *p0 = pc + 2 * idx, *p1 = p0 + 1;
   ini_chain(p1, A(lb), ZeroPoint);                                  // (bj)
   ini_chain(p0, av, word(p1));                                      // (ai bj)
   ini_chain(spine + idx, word(p0), idx + 1 < pairs ? word(spine + idx + 1) : ZeroPoint); } }
 ai_musttail return Push(word(spine)); }

// --- apply lane (the data-value `(g x)` aps) ---
// an applied data value's sentinel tail-jumps straight to its handler -- no table.
// the sequences index and juxtapose -- text by byte, a chain by element, a named point by
// its spelling -- numbers are church numerals. () is the default action: nothing is there
// to answer with, which is what an anonymous point, an opaque handle, an out-of-range
// index and a non-index operand all have in common.

// string x charm -> byte by index or ()
// string x string -> concat
// string x name -> concat
lvm(data_string_apply) {
 bool pt = namep(word(Ip));                             // a point head can answer a point
 struct ai_str *na = pt ? nom_str(g, word(Ip)) : str(word(Ip)),
               *nb = strp(Sp[0]) ? str(Sp[0]) : namep(Sp[0]) ? nom_str(g, Sp[0]) : NULL;
 if (nb) {
  bool mk = pt && namep(Sp[0]);                         // point + point -> the interned point
  uintptr_t m = na->len, n = nb->len, req = str_width(m + n);
  if (!(m + n)) { Ip = cell(*++Sp); *Sp = mk ? ZeroPoint : EmptyString; ai_musttail return Continue(); }  // the empty spelling is the zero point; no empty string is ever allocated
  Have(req + (mk ? intern_reserve(g) : 0));
  na = pt ? nom_str(g, word(Ip)) : str(word(Ip));       // re-read: a GC in Have moved the roots
  struct ai_str *z = seq_cat(g, Hp, word(na), Sp[0]);
  Hp += req;
  word v = word(z);
  if (mk) Pack(g), v = intern_checked(g, z), Unpack(g);
  Ip = cell(*++Sp); *Sp = v; ai_musttail return Continue(); }
 word v = ZeroPoint;
 if (oddp(Sp[0])) {
  word k = getcharm(Sp[0]);
  if (k < 0) k += (word) na->len;                       // -1 is the last byte
  if (k >= 0 && k < (word) na->len) v = putcharm((unsigned char) txt(na)[k]); }
 Ip = cell(*++Sp), *Sp = v;
 ai_musttail return Continue(); }

// applying a point: a named point acts as its spelling, so it rides the text lane whole.
// an anonymous point -- a gensym, and () -- has no spelling to act as, so nothing is
// there to answer with: (). name? and mint? partition nom? and () is in neither.
lvm(data_sym_apply) {
 if (namep(word(Ip))) ai_musttail return Ap(data_string_apply, g);
 Ip = cell(*++Sp), *Sp = ZeroPoint;
 ai_musttail return Continue(); }

// (n x): church-numeral application for the boxed tower -- the same
// [n, num-ap, x, ret] frame as lvm_numap
lvm(data_num_apply) {
 Have(2);
 word h = hot_hook(g->hot_numap);
 word n = word(Ip), x = Sp[0], ret = Sp[1], *dst = Sp - 2;
 dst[0] = n, dst[1] = h, dst[2] = x, dst[3] = ret;
 Sp = dst, Ip = (union u*) numap_drive;
 ai_musttail return Continue(); }

// (l k): index the spine -- the kth element, negatives from the end, out of range ().
// (l m): a chain operand juxtaposes -- the append, agreeing with (+ l m) on the nose
// (add_seq's list+list lane, spelled here). the text law, one lattice rung up: a chain
// indexes elements where text indexes bytes. every other operand answers ().
lvm(data_pair_apply) {
 if (chainp(Sp[0])) {
  uintptr_t n = llen(word(Ip));
  Have(n * Width(struct ai_chain));
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = word(Ip); chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = Sp[0];                        // last cdr -> the operand (a chain is never empty)
  Ip = cell(*++Sp); *Sp = word(base); ai_musttail return Continue(); }
 word v = ZeroPoint;
 if (oddp(Sp[0])) {
  word k = getcharm(Sp[0]), l = word(Ip);
  if (k < 0) k += (word) llen(l);            // -1 is the last element
  if (k >= 0) { while (k-- > 0 && chainp(l)) l = B(l);
                if (chainp(l)) v = A(l); } }
 Ip = cell(*++Sp); *Sp = v; ai_musttail return Continue(); }

// === the two generic-op dispatch matrices (+ and *), indexed by ai_kind =====
// lanes: *n numeric/broadcast (every star and tray kind routes identically), add_seq
// a list anywhere, add_string strings (+ a number as one byte -- the byte law),
// mul_rep sequence * count, *l a lambda-or-map operand (church add / compose),
// lvm_0 undefined -> zero. precedence: lambda > tablet > chain > text > number.
// the tables are generated: one datum (mx.l) feeds this header and the rocq model
// mx.v, so theorem and code cannot drift. edit mx.l, not mx.h; make relays it.
#include "mx.h"

// any value -> the kind it dispatches as (enum q, love.h): fixnum -> KCharm,
// non-data heap pointer -> KTablet/KHot, else the rep's kind. a tray is the one rep
// that dispatches four ways, by element tier. exported so the apply sentinels share
// it; it sits under mx.h for ai_kind_of_d, the rep -> kind crossing.
enum q ai_kind(word x) {
 if (charmp(x)) return KCharm;
 if (!datp(x)) return tabp(x) ? KTablet : KHot;
 enum d r = typ(x);
 if (r == DTray) return (enum q) (KTrayZ + tray(x)->type);
 return ai_kind_of_d[r]; }

// === the `+`/`*` dispatchers (fixnum fast path, then the matrix) ============
lvm(lvm_add) {
 word a = Sp[0], b = Sp[1]; intptr_t t;
 if (charmp(a) && charmp(b)
     && !__builtin_add_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t)
     && t >= mincharm && t <= maxcharm)
  ai_musttail return Push(putcharm(t));
 // ZeroPoint first, and this must mirror lvm_bin_unit exactly -- it is that
 // matrix lane's fast path, nothing more. () is a mint, so folding these into the
 // distinct-mints rule makes `() + m` answer () where the matrix answers m.
 if (a == ZeroPoint) ai_musttail return Push(b);
 if (b == ZeroPoint) ai_musttail return Push(a);
 if (mintp(a) && mintp(b)) ai_musttail return Push(a == b ? a : ZeroPoint);
 if (mintp(a)) ai_musttail return Push(b);
 if (mintp(b)) ai_musttail return Push(a);
 ai_musttail return Ap(ai_add_mx[ai_kind(a)][ai_kind(b)], g); }

lvm(lvm_mul) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t t;
  if (!__builtin_mul_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t)
      && t >= mincharm && t <= maxcharm)
   ai_musttail return Push(putcharm(t)); }
 // a bare mint is absent, and * repeats: a sequence taken an absent number of times
 // is nothing, so it annihilates. the matrix says the same thing (lvm_0), so this
 // stays a fast path.
 if (mintp(a) || mintp(b)) ai_musttail return Push(ZeroPoint);
 ai_musttail return Ap(ai_mul_mx[ai_kind(a)][ai_kind(b)], g); }

avm_div(fquot, /)                               // `//` fixnum fast path: truncating quotient
avm_div(rem, %)
// `/` fixnum fast path: stay exact only when b divides a; otherwise the slow lane
// promotes to a float box. the INT_MIN/-1 guard precedes the `%` (it would be UB).
lvm(lvm_quot) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t av = getcharm(a), bv = getcharm(b);
  if (bv != 0 && !(av == INTPTR_MIN && bv == -1) && av % bv == 0) {
   intptr_t t = av / bv;
   if (t >= mincharm && t <= maxcharm) ai_musttail return Push(putcharm(t)); } }
 avm_unit(a, b);
 if (coinp(a) || coinp(b)) ai_musttail return Ap(lvm_quot_coin, g);   // the die's div method, slot 8
 ai_musttail return Ap(lvm_quotn, g); }

// the ordered comparisons (lvm_lt/le/gt/ge) and their total order are defined
// after vcmp_int/vcmp_flo (the per-op trichotomy helpers), near lvm_vbin.

// bitwise and/or/xor: the both-fixnum tag trick (two odds stay odd under & and |;
// ^ clears the tag, re-set it). integer-only: any other operand yields zero.
bit_slow(band, &) bit_slow(bor, |) bit_slow(bxor, ^)

lvm(lvm_band) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) ai_musttail return Push((a & b) | 1);
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop_band); ai_musttail return Ap(lvm_vbin, g); }
 ai_musttail return Ap(lvm_band_slow, g); }

lvm(lvm_bor) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) ai_musttail return Push((a | b) | 1);
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop_bor); ai_musttail return Ap(lvm_vbin, g); }
 ai_musttail return Ap(lvm_bor_slow, g); }

lvm(lvm_bxor) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) ai_musttail return Push((a ^ b) | 1);
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop_bxor); ai_musttail return Ap(lvm_vbin, g); }
 ai_musttail return Ap(lvm_bxor_slow, g); }
// (bitwise complement is `(^ x -1)`; logical not is the `!` reader sigil / `zerop`.)

// >> : arithmetic right shift; a fixnum only shrinks, so the fast path never allocates
lvm(lvm_bsr_slow) { word a = Sp[0], b = Sp[1], _res;
 if (!(charmp(a) || sunp(a)) || !charmp(b)) ai_musttail return Push(ZeroPoint);
 Have(box_req);
 emit_int(_res, toint(a) >> shmask(getcharm(b)));
 ai_musttail return Push(_res); }
lvm(lvm_bsr) { word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b))
  ai_musttail return Push(putcharm(getcharm(a) >> shmask(getcharm(b))));
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop_bsr); ai_musttail return Ap(lvm_vbin, g); }
 ai_musttail return Ap(lvm_bsr_slow, g); }

// << : can overflow the tag, so it always runs the box/demote path; the shift is
// done in uintptr_t for well-defined overflow
lvm(lvm_bsl) { word a = Sp[0], b = Sp[1], _res;
 avm_unit(a, b);
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop_bsl); ai_musttail return Ap(lvm_vbin, g); }
 if (!(charmp(a) || sunp(a)) || !charmp(b)) ai_musttail return Push(ZeroPoint);
 Have(box_req);
 emit_int(_res, (intptr_t)((uintptr_t) toint(a) << shmask(getcharm(b))));
 ai_musttail return Push(_res); }

op(lvm_charmp, 1, oddp(Sp[0]) ? putcharm(1) : zero)   // (charm? x): a fixnum -- a charm, the tagged odd word
// (nil? x): the falsy predicate, (= 0 ($ x)) without the clamp. the single
// truthiness oracle: `?`, zerop and aall all consult ai_nilp, so the feel pass
// can drop a zerop wrapper.
op11(lvm_nilp, ai_nilp(g, Sp[0]) ? putcharm(1) : zero)

// unary math nif: numeric arg → double, call fn, box the rank-0 f64 result.
// non-numeric arg → zero. TCO-clean (no & escapes).
lvm(lvm_math1) {
 ai_flo1 fn = (ai_flo1) (uintptr_t) g->b;
 word a = Sp[0];
 if (trayp(a)) {                               // (sin a-tray) etc. -> gem tray; a twin tray is undefined
  if (tray(a)->type == ai_C) return Answer(ZeroPoint);
  { g->b = (ai_word) (uintptr_t) (fn); ai_musttail return Ap(lvm_vmap1, g); } }
 if (!isnum(a)) return Answer(ZeroPoint);
 ai_flo_t ad = toflo(a), rd = fn(ad);
 Have(gem_req);
 Sp[0] = mk_gem(&Hp, rd); return Next(1); }

lvm(lvm_math2) {
 ai_flo2 fn = (ai_flo2) (uintptr_t) g->b;
 word a = Sp[0], b = Sp[1];
 if (trayp(a) || trayp(b)) {                               // (pow arr ..) etc. -> float array
  if ((trayp(a) && tray(a)->type == ai_C) || (trayp(b) && tray(b)->type == ai_C))
   return Push(ZeroPoint);                 // complex array undefined here
  { g->b = (ai_word) (uintptr_t) (fn); ai_musttail return Ap(lvm_vmap2, g); } }
 if (!isnum(a) || !isnum(b)) return Push(ZeroPoint);
 ai_flo_t ad = toflo(a), bd = toflo(b), rd = fn(ad, bd);
 Have(gem_req);
 *++Sp = mk_gem(&Hp, rd); return Next(1); }


m1(mvm1)

// (log x): a positive real stays float; a negative real or complex widens to the
// complex principal value ~((log |z|) (arg z)) -- so (log -1) = (* i pi), euler in
// the exact direction. arrays stay elementwise float.
lvm(lvm_log) {
 word a = Sp[0];
 ai_flo_t m, th;
 if (twinp(a)) m = ai_log(twin_mod(a)), th = ai_atan2(twin_im(a), twin_re(a));
 else if (isnum(a) && toflo(a) < 0) { ai_flo_t ad = toflo(a);
  m = ai_log(-ad), th = ai_atan2(0, ad); }
 else { g->b = (ai_word) (uintptr_t) (ai_log); ai_musttail return Ap(lvm_math1, g); }
 Have(twin_req);
 Sp[0] = mk_twin(&Hp, m, th); ai_musttail return Next(1); }

op11(lvm_gemp, gemp(Sp[0]) ? putcharm(1) : zero)

// ============================================================================
// rng
// ============================================================================
// xoshiro256++ seeded by SplitMix64. C holds no RNG state and never draws: the
// primitives are wheel (fresh state) and the functional steps turn/turnf, which
// copy the state and answer (value . new-state) -- the input is never mutated.
// the global rand/randf stream is prel lisp over the same steps. not a CSPRNG.

uint64_t rotl64(uint64_t x, int k) {
 return (x << k) | (x >> (64 - k)); }

// the uint64_t scratch lives in these ai_noinline helpers, moved via memcpy:
// taking &s in a VM ap defeats the sibcall, and memcpy is alignment-safe.

// advance the 4-word state at `payload` and return one 64-bit draw
ai_noinline uint64_t rng_step(void *payload) {
 uint64_t s[4];
 memcpy(s, payload, sizeof s);
 uint64_t const result = rotl64(s[0] + s[3], 23) + s[0], t = s[1] << 17;
 s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
 s[2] ^= t; s[3] = rotl64(s[3], 45);
 memcpy(payload, s, sizeof s);
 return result; }

// fill the state from a seed via SplitMix64; the all-zero state is xoshiro's
// fixed point, so substitute a nonzero word
ai_noinline void rng_seed_into(void *payload, uint64_t seed) {
 uint64_t s[4], x = seed;
 for (int i = 0; i < rng_state_len; i++) {
  uint64_t z = (x += (uint64_t) 0x9e3779b97f4a7c15);
  z = (z ^ (z >> 30)) * (uint64_t) 0xbf58476d1ce4e5b9;
  z = (z ^ (z >> 27)) * (uint64_t) 0x94d049bb133111eb;
  s[i] = z ^ (z >> 31); }
 if (!(s[0] | s[1] | s[2] | s[3])) s[0] = 1;
 memcpy(payload, s, sizeof s); }

// map a 64-bit draw to a float in [0,1): keep the high mantissa bits and scale.
ai_flo_t u64_to_unit(uint64_t u) {
#if Bits >= 64
 return (ai_flo_t) (u >> 11) * (ai_flo_t) 0x1.0p-53;
#else
 return (ai_flo_t) (uint32_t) (u >> 40) * (ai_flo_t) 0x1.0p-24f;
#endif
}

// shape v as a state tray and seed it; no &local, so an inlining caller keeps its tail call
void ai_rng_seed(struct ai_tray *v, uint64_t seed) {
 ini_tray(v, rng_vt, 1);
 v->shape[0] = rng_state_len;
 rng_seed_into(tray_data(v), seed); }

// is x a well-formed state tray (rank-1 i64, length 4)?
bool rng_state_p(word x) {
 return packp(x) && tray(x)->rank == 1 && tray(x)->type == rng_vt
        && tray(x)->shape[0] == rng_state_len; }

// a fresh state tray at Hp copying src's limbs; caller holds Have(rng_tray_req)
struct ai_tray *rng_copy(ai_word **hp, struct ai_tray *src) {
 struct ai_tray *v = (struct ai_tray*) *hp;
 *hp += rng_tray_req;
 ini_tray(v, rng_vt, 1);
 v->shape[0] = rng_state_len;
 memcpy(tray_data(v), tray_data(src), rng_payload_bytes);
 return v; }

// canonicalize a 62-bit draw to the smallest integer tier. out-of-line so the
// limb[] scratch never forces a frame in lvm_turn (make vmret); bump-only.
ai_noinline word rng_canon(struct ai *g, uint64_t r) {
 ai_limb limb[64 / limb_bits]; int nl = 0;               // split the 64-bit draw into native limbs (1 or 2)
 for (int i = 0; (size_t) i * limb_bits < 64; i++) limb[i] = (ai_limb) (r >> (i * limb_bits)), nl = i + 1;
 return ai_big_canon(&g->hp, limb, nl, false); }

// (wheel n): a fresh state tray deterministically seeded from fixnum n. A
// non-fixnum seeds from 0.
lvm(lvm_wheel) {
 word n = Sp[0];
 uint64_t seed = charmp(n) ? (uint64_t) (intptr_t) getcharm(n) : 0;
 Have(rng_tray_req);
 struct ai_tray *v = (struct ai_tray*) Hp; Hp += rng_tray_req;
 ai_rng_seed(v, seed);
 ai_musttail return Answer(word(v)); }

// (turn st): functional draw -> (value . st'), value a fixed 62 bits so a seed
// yields the identical integer on every target; st is copied, never mutated
#define rng_draw_mask (((uint64_t) 1 << 62) - 1)              // 62 bits = 64-bit maxcharm
#define rng_draw_req  (Width(struct ai_big) + b2w((64 / limb_bits) * sizeof(ai_limb)))  // worst case: the 62-bit draw split into native limbs
lvm(lvm_turn) {
 word st = Sp[0];
 if (!rng_state_p(st)) ai_musttail return Answer(ZeroPoint);
 Have(rng_tray_req + rng_draw_req + Width(struct ai_chain));
 st = Sp[0];                                 // re-read post-Have
 struct ai_tray *v = rng_copy(&Hp, tray(st));
 uint64_t r = rng_step(tray_data(v)) & rng_draw_mask;
 Pack(g);
 word val = rng_canon(g, r);
 Unpack(g);
 struct ai_chain *p = (struct ai_chain*) Hp; Hp += Width(struct ai_chain);
 ini_chain(p, val, word(v));
 ai_musttail return Answer(word(p)); }

// (turnf st): functional draw -> (float . st'), float in [0,1).
lvm(lvm_turnf) {
 word st = Sp[0], _res;
 if (!rng_state_p(st)) ai_musttail return Answer(ZeroPoint);
 Have(rng_tray_req + box_req + Width(struct ai_chain));
 st = Sp[0];                                 // re-read post-Have
 struct ai_tray *v = rng_copy(&Hp, tray(st));
 uint64_t r = rng_step(tray_data(v));
 ai_flo_t u = u64_to_unit(r);
 emit_gem(_res, u);                                // box at Hp, into _res
 struct ai_chain *p = (struct ai_chain*) Hp; Hp += Width(struct ai_chain);
 ini_chain(p, _res, word(v));
 ai_musttail return Answer(word(p)); }

// ============================================================================
// eq
// ============================================================================
// α-equivalence of two stored lambda sources: bound variables match by binder
// position, free by symbol. `:` binders are not tracked (sound, conservative);
// a one-operand \ is quote, compared as data.
struct arib { word la, lb; int na, nb; struct arib *up; };  // binder rib: (p…body) lists + param counts
int arib_pos(word s, word l, int n) {                // index of s among the first n of l, else -1
 for (int i = 0; i < n && chainp(l); i++, l = B(l)) if (A(l) == s) return i;
 return -1; }
bool ai_isbs(struct ai *g, word h) {                  // h is the `\` symbol?
 struct ai_str *n; return (n = nom_str(g, h)) && n->len == 1 && n->bytes[0] == '\\'; }
bool salpha(struct ai *g, word a, word b, struct arib *env) {
 if (nomp(a) || nomp(b)) {
  if (!nomp(a) || !nomp(b)) return false;
  for (struct arib *r = env; r; r = r->up) {
   int ia = arib_pos(a, r->la, r->na), ib = arib_pos(b, r->lb, r->nb);
   if (ia >= 0 || ib >= 0) return ia == ib; }               // bound at this rib: positions agree
  return a == b; }                                          // both free: same symbol
 if (!chainp(a) || !chainp(b)) return eqv(g, a, b);             // numbers / strings / atoms
 if (ai_isbs(g, A(a)) && ai_isbs(g, A(b))) {                        // both `\`-headed
  word pa = B(a), pb = B(b);
  if (!chainp(pa) || !chainp(pb)
   || !chainp(B(pa)) || !chainp(B(pb))) return eqv(g, a, b);    // one-operand \ = quote: data
  int na = 0, nb = 0;                                       // (\ p1..pn body): params = init, body = last
  word t = pa;
  for (; chainp(B(t)); t = B(t)) na++;
  word ba = A(t);
  for (t = pb; chainp(B(t)); t = B(t)) nb++;
  word bb = A(t);
  if (na != nb) return false;
  struct arib r = { pa, pb, na, nb, env };
  return salpha(g, ba, bb, &r); }
 return salpha(g, A(a), A(b), env) && salpha(g, B(a), B(b), env); }  // structural: app / ? / :

// α-invariant hash of a source \-expr, parallel to salpha: a bound variable hashes by its
// binder coordinate (rib depth, position), a free variable by its symbol code, so α-equal
// lambdas hash equal and the total order (cmp3, by repr hash) agrees with `=`.
uintptr_t shash(struct ai *g, word x, struct arib *env) {
 if (nomp(x)) {
  int d = 0;
  for (struct arib *r = env; r; r = r->up, d++) {
   int i = arib_pos(x, r->la, r->na);
   if (i >= 0) return rot((uintptr_t) (d * 131 + i + 1) * mix); }
  return hash(g, x); }                  // a free variable: its stable identity hash (mint serial / interned name . mint)
 if (!chainp(x)) return hash(g, x);
 if (ai_isbs(g, A(x))) {
  word p = B(x);
  if (!chainp(p) || !chainp(B(p))) return hash(g, x);         // one-operand \ = quote: data
  int n = 0;
  word t = p;
  for (; chainp(B(t)); t = B(t)) n++;
  word body = A(t);
  struct arib r = { p, p, n, n, env };
  return (mix * (uintptr_t) (n + 7)) ^ (shash(g, body, &r) * mix); }
 return (mix ^ (shash(g, A(x), env) * mix)) ^ (shash(g, B(x), env) * mix); }

// --- the beta bridge: a closure value compares up to the capture-substitution
// ev already performed -- (adder 5) = (\ x (+ x 5)). done without allocating: the
// base source is walked virtually, its leading binders split filled (resolve to
// the captured value) and remaining (post-substitution de Bruijn coordinates).
// sound by construction; a captured closure vs a source lambda stays unbridged
// (conservative, but nf_hash mirrors shash so =-equal closures always hash equal).
enum { nf_maxcap = 64 };                                  // cap the captured-arg count we bridge; deeper -> fall back
struct clonf { word body, rem, fsyms; int nr, fn; word fv[nf_maxcap]; };  // residual: body, remaining-binder list (nr), filled-binder list (fn) + values

// load a closure value's capture-substitution residual. a partial-app over a sourced base, or a
// no-capture lambda (fn = 0). returns false for a source-less base (a bif) or a quote -- caller falls back.
bool clo_load(struct ai *c, word v, struct clonf *o) {
 if (!lamp(v) || datp(v) || !in_heap(c, v)) return false;
 union u *k = cell(v);
 word s; int na = 0;
 if (fn_partialp(k)) {
  union u *bk = fn_base(k, &na);
  if (na < 0 || na > nf_maxcap) return false;
  word base = (word) bk;
  s = fn_src(c, cell(base), base);
  for (int i = 0; i < na; i++) o->fv[i] = fn_arg(k, i, na);
 } else s = fn_src(c, k, v);
 if (!s || !lam_isp(c, s)) return false;                  // source-less base / quote: not bridged here
 word p = B(s);                                           // (b0 b1 .. body): binder list then body
 int nb = 0; word t = p;
 for (; chainp(B(t)); t = B(t)) nb++;
 if (na >= nb) return false;                              // captures consume the whole group (shouldn't for a partial-app): bail safe
 word rem = p;
 for (int i = 0; i < na; i++) rem = B(rem);               // remaining binders start past the filled ones
 o->body = A(t); o->rem = rem; o->nr = nb - na; o->fsyms = p; o->fn = na;
 return true; }

// α-invariant hash of a residual's body, mirroring shash: a genuine binder by
// coordinate, a filled binder by its captured value's hash, a free var by symbol
uintptr_t nf_hash(struct ai *g, word x, struct arib *env, word fs, int fn, word *fv) {
 if (nomp(x)) {
  int d = 0;
  for (struct arib *r = env; r; r = r->up, d++) {
   int i = arib_pos(x, r->la, r->na);
   if (i >= 0) return rot((uintptr_t) (d * 131 + i + 1) * mix); }   // genuine binder
  int j = arib_pos(x, fs, fn);
  if (j >= 0) return hash(g, fv[j]);                      // filled binder: the captured value as a literal
  return hash(g, x); }                                    // free var
 if (!chainp(x)) return hash(g, x);
 if (ai_isbs(g, A(x))) {
  word p = B(x);
  if (!chainp(B(p))) return hash(g, x);                   // quote: data
  int n = 0; word t = p;
  for (; chainp(B(t)); t = B(t)) n++;
  word body = A(t);
  struct arib r = { p, p, n, n, env };
  return (mix * (uintptr_t) (n + 7)) ^ (nf_hash(g, body, &r, fs, fn, fv) * mix); }
 return (mix ^ (nf_hash(g, A(x), env, fs, fn, fv) * mix)) ^ (nf_hash(g, B(x), env, fs, fn, fv) * mix); }
bool clo_nfhash(struct ai *g, word x, uintptr_t *out) {
 struct clonf o;
 if (!clo_load(ai_core_of(g), x, &o) || !o.fn) return false;   // o.fn == 0: a no-capture lambda, already hashed via shash upstream
 struct arib r = { o.rem, o.rem, o.nr, o.nr, 0 };
 *out = (mix * (uintptr_t) (o.nr + 7)) ^ (nf_hash(g, o.body, &r, o.fsyms, o.fn, o.fv) * mix);
 return true; }

// does runtime value V equal the meaning of source term b? filled binder ->
// compare captures; literal atom -> compare; anything else conservative false.
bool val_vs_src(struct ai *g, word V, word b, struct arib *rb, struct clonf *cb, word *scratch) {
 if (nomp(b)) {
  for (struct arib *r = rb; r; r = r->up) if (arib_pos(b, r->la, r->na) >= 0) return false;  // a remaining param
  int j = arib_pos(b, cb->fsyms, cb->fn);
  return j >= 0 ? eqv_at(g, V, cb->fv[j], scratch) : false; }   // filled: both values | free: conservative false
 if (!chainp(b)) return eqv_at(g, V, b, scratch);               // literal atom (number / string)
 return false; }                                               // compound source (app / lambda): conservative

// α + value equality of two residual bodies in lockstep: a nom classifies bound
// (by coordinate), filled (a captured value), free (by symbol), or not-a-nom
bool nf_walk(struct ai *g, word a, struct arib *ra, struct clonf *ca,
                                  word b, struct arib *rb, struct clonf *cb, word *scratch) {
 if (nomp(a) || nomp(b)) {
  int ka = 3; intptr_t ac = 0; word av = 0;              // 0 bound, 1 filled, 2 free, 3 not-a-nom
  if (nomp(a)) {
   int d = 0; ka = 2;
   for (struct arib *r = ra; r; r = r->up, d++) { int i = arib_pos(a, r->la, r->na); if (i >= 0) { ka = 0; ac = (intptr_t) d * 4096 + i; break; } }
   if (ka == 2) { int j = arib_pos(a, ca->fsyms, ca->fn); if (j >= 0) { ka = 1; av = ca->fv[j]; } } }
  int kb = 3; intptr_t bc = 0; word bv = 0;
  if (nomp(b)) {
   int d = 0; kb = 2;
   for (struct arib *r = rb; r; r = r->up, d++) { int i = arib_pos(b, r->la, r->na); if (i >= 0) { kb = 0; bc = (intptr_t) d * 4096 + i; break; } }
   if (kb == 2) { int j = arib_pos(b, cb->fsyms, cb->fn); if (j >= 0) { kb = 1; bv = cb->fv[j]; } } }
  if (ka == 0 || kb == 0) return ka == 0 && kb == 0 && ac == bc;   // a bound var matches only the same-coordinate bound var
  if (ka == 1 && kb == 1) return eqv_at(g, av, bv, scratch);       // two captured values
  if (ka == 1) return val_vs_src(g, av, b, rb, cb, scratch);
  if (kb == 1) return val_vs_src(g, bv, a, ra, ca, scratch);
  if (ka == 2 && kb == 2) return a == b;                           // two free vars
  return false; }                                                  // free vs not-a-nom
 if (!chainp(a) || !chainp(b)) return eqv_at(g, a, b, scratch);
 if (ai_isbs(g, A(a)) && ai_isbs(g, A(b))) {
  word pa = B(a), pb = B(b);
  if (!chainp(B(pa)) || !chainp(B(pb))) return eqv_at(g, a, b, scratch);   // quote: data
  int na = 0, nb = 0; word t = pa;
  for (; chainp(B(t)); t = B(t)) na++;
  word ba = A(t);
  for (t = pb; chainp(B(t)); t = B(t)) nb++;
  word bb = A(t);
  if (na != nb) return false;
  struct arib rA = { pa, pa, na, na, ra }, rB = { pb, pb, nb, nb, rb };
  return nf_walk(g, ba, &rA, ca, bb, &rB, cb, scratch); }
 return nf_walk(g, A(a), ra, ca, A(b), rb, cb, scratch) && nf_walk(g, B(a), ra, ca, B(b), rb, cb, scratch); }
bool clo_eq(struct ai *g, struct clonf *ca, struct clonf *cb, word *scratch) {  // residual α+value equality
 if (ca->nr != cb->nr) return false;                                   // different residual arity
 struct arib rA = { ca->rem, ca->rem, ca->nr, ca->nr, 0 }, rB = { cb->rem, cb->rem, cb->nr, cb->nr, 0 };
 return nf_walk(g, ca->body, &rA, ca, cb->body, &rB, cb, scratch); }

// `base` is where this frame's worklist starts: the public eqv passes off_pool; a
// re-entrant beta-bridge call passes the caller's live top, so nested scratch sits
// above the pending pairs instead of clobbering them.
bool eqv_at(struct ai *g, word a, word b, word *base) {
 word *top = off_pool(g) + g->len, *w = base;
 struct ai *c = ai_core_of(g);
 for (;;) {
  if (a != b) {
   // coins: equal iff same die and eqv payloads
   if (coinp(a) || coinp(b)) {
    if (coinp(a) && coinp(b) && coin_die(a) == coin_die(b)) {
     a = coin_load(a), b = coin_load(b); continue; }
    return false; }
   // function values: equality up to the beta the runtime already ran (the
   // bridge). a source-less base (a bif partial like (+ 1)) can't residualize:
   // fall back to base + captures pairwise. maps/ports/mixed fall to identity.
   if (lamp(a) && lamp(b) && !datp(a) && !datp(b)) {
    union u *ka = cell(a), *kb = cell(b);
    bool pa = fn_partialp(ka), pb = fn_partialp(kb);
    if (!pa && !pb) {                                      // common case: two no-capture lambdas -> α-compare sources
     word sa = fn_src(c, ka, a), sb = fn_src(c, kb, b);
     if (sa && sb) { if (!salpha(g, sa, sb, 0)) return false; a = b; continue; }
     return false; }                                      // a source-less function value -> identity (already failed)
    struct clonf ra_, rb_;                                // a partial-app is in play: bridge via the capture-substitution residual
    if (clo_load(c, a, &ra_) && clo_load(c, b, &rb_)) {
     if (!clo_eq(g, &ra_, &rb_, w)) return false;         // w = the live worklist top: the bridge's re-entrant eqv scratches above it
     a = b; continue; }                                   // residuals equal -> drain worklist
    if (pa && pb) {                                        // source-less base (a bif): compare base + captures pairwise
     int na, nb; union u *ba = fn_base(ka, &na), *bb = fn_base(kb, &nb);
     if (na != nb) return false;
     if (top - w < 2 * (na + 1)) __builtin_trap();        // worklist overflow / cycle
     for (int i = 0; i < na; i++) *w++ = fn_arg(ka, i, na), *w++ = fn_arg(kb, i, nb);
     a = (word) ba, b = (word) bb; continue; }
    return false; }
   // a number never equals a closure: bridging 0/1 to their church lambdas would
   // break congruence, the order, and tower transitivity
   if (((a | b) & 1) || !datp(a) || !datp(b) || typ(a) != typ(b)) return false;
   switch (typ(a)) {
    default: return false;
    case DChain:
     if (top - w < 2) __builtin_trap();     // worklist overflow: a cycle
     *w++ = B(a), *w++ = B(b), a = A(a), b = A(b);
     continue;
    case DTray: {
     size_t la = ai_tray_bytes(tray(a)), lb = ai_tray_bytes(tray(b));
     if (la != lb || memcmp(tray(a), tray(b), la)) return false;
     break; }
    case DGem:
     if (gem_get(a) != gem_get(b)) return false;       // two float boxes: compare the payload (parallels = / cmp)
     break;
    case DSun:
     if (sun_get(a) != sun_get(b)) return false;       // two suns: compare the payload
     break;
    case DTwin:
     if (twin_re(a) != twin_re(b) || twin_im(a) != twin_im(b)) return false;  // re and im
     break;
    case DBig: {
     struct ai_big *x = big(a), *y = big(b);
     if (x->slen != y->slen) return false;
     size_t nb = (size_t) (x->slen < 0 ? -x->slen : x->slen) * sizeof(ai_limb);
     if (memcmp(x->limb, y->limb, nb)) return false;
     break; }
    case DString:
     if (len(a) != len(b) || memcmp(txt(a), txt(b), len(a))) return false;
     break; } }
  if (w == base) return true;              // worklist drained: all equal
  b = *--w, a = *--w; } }
ai_noinline bool eqv(struct ai *g, word a, word b) { return eqv_at(g, a, b, off_pool(g)); }

// whole-array `=`: a boolean like every other kind (shapes match, every cell
// equal), not the elementwise mask -- `<` and `>` are the mask makers. cells
// compare across tiers (a z-tray equals a gem-tray of the same values); object
// cells go through eqv; an object tray never equals a numeric one.
ai_noinline bool tray_eq(struct ai *g, word a, word b) {
 if (!trayp(a) || !trayp(b)) return false;            // an array is never a scalar
 struct ai_tray *va = tray(a), *vb = tray(b);
 if (va->rank != vb->rank) return false;
 for (uintptr_t k = 0; k < va->rank; k++)
  if (va->shape[k] != vb->shape[k]) return false;   // same shape, not merely conformant
 uintptr_t n = tray_nelem(va);
 bool oa = va->type == ai_O, ob = vb->type == ai_O;
 if (oa || ob) {
  if (oa != ob) return false;
  for (uintptr_t i = 0; i < n; i++)
   if (!eqv(g, tray_get_obj(va, i), tray_get_obj(vb, i))) return false;
  return true; }
 if (va->type == ai_C || vb->type == ai_C) {        // (re,im) per cell; a real reads as (r,0)
  ai_flo_t const *pa = tray_data(va), *pb = tray_data(vb);
  for (uintptr_t i = 0; i < n; i++) {
   ai_flo_t are = va->type == ai_C ? pa[2*i] : tray_get_flo(va, i),
            aim = va->type == ai_C ? pa[2*i+1] : 0,
            bre = vb->type == ai_C ? pb[2*i] : tray_get_flo(vb, i),
            bim = vb->type == ai_C ? pb[2*i+1] : 0;
   if (are != bre || aim != bim) return false; }
  return true; }
 if (va->type == ai_Z && vb->type == ai_Z) {        // exact: no double round-trip
  for (uintptr_t i = 0; i < n; i++)
   if (tray_get_int(va, i) != tray_get_int(vb, i)) return false;
  return true; }
 for (uintptr_t i = 0; i < n; i++)                  // a float on either side: as doubles
  if (tray_get_flo(va, i) != tray_get_flo(vb, i)) return false;
 return true; }

// (= a b): value-equality with numeric promotion across the tower; falls through
// to eql for non-numeric operands. strictly looser than eqv, which still rejects
// mixed-type chains (table keys 3 and 3.0 stay distinct).
lvm(lvm_eq) {
 word a = Sp[0], b = Sp[1];
 // the common case: identity settles two charms, and a point against anything
 // (a point equals only itself). both skip the dispatch below and fuse a
 // following `?` directly (then -> Ip+3, else -> Ip[2].m).
 if (__builtin_expect((charmp(a) && charmp(b)) || nomp(a) || nomp(b), 1)) {
  bool r = a == b;
  if (Ip[1].ap == lvm_cond) { Sp += 2; Ip = r ? Ip + 3 : Ip[2].m; ai_musttail return Continue(); }
  ai_musttail return Answerp(1, r ? putcharm(1) : zero); }
 if (trayp(a) || trayp(b)) {   // whole-array equality -> a boolean; the mask lives on < and >
  bool r = tray_eq(g, a, b);
  Sp[1] = r ? putcharm(1) : zero;
  ai_musttail return Nextp(1, 1); }
 // complex: equal iff re and im match, a real reading as (r, 0); before the
 // float lane so a complex never reaches toflo
 if (twinp(a) || twinp(b)) {
  bool r = (twinp(a) || isnum(a)) && (twinp(b) || isnum(b))
        && (twinp(a) ? twin_re(a) : toflo(a)) == (twinp(b) ? twin_re(b) : toflo(b))
        && (twinp(a) ? twin_im(a) : 0) == (twinp(b) ? twin_im(b) : 0);
  Sp[1] = r ? putcharm(1) : zero;
  ai_musttail return Nextp(1, 1); }
 bool r;
 // a float operand compares as doubles across the whole tower (a bignum loses
 // precision past 2^53, the documented caveat); otherwise eql
 if (gemp(a) || gemp(b)) r = isnum(a) && isnum(b) && (toflo(a) == toflo(b));
 else r = eql(g, a, b);
 Sp[1] = r ? putcharm(1) : zero;
 ai_musttail return Nextp(1, 1); }

// (id? a b): pointer/word identity, no structural recursion
lvm(lvm_same) {
 Sp[1] = Sp[0] == Sp[1] ? putcharm(1) : zero;
 ai_musttail return Nextp(1, 1); }

// ============================================================================
// obin -- object-array elementwise lane (ai_O)
// ============================================================================
// the typed lanes wrap on overflow; the object lane routes every element through
// the promoting scalar dispatch, so a ai_O array adds/multiplies exactly. the
// inner loop allocates, so it runs Pack'd and re-fetches every live pointer.

// one element op, allocating via *fp; zero for a non-numeric/complex operand
word obin_elem(struct ai **fp, int op, word a, word b) {
 if (op >= vop_lt) {                            // comparison -> 1 / zero, no allocation
  if (!isnum(a) || !isnum(b)) return zero;       // twinp not in isnum -> unordered -> zero
  intptr_t t = (gemp(a) || gemp(b)) ? vcmp_flo(op, toflo(a), toflo(b))
             : (bigp(a) || bigp(b)) ? vcmp_int(op, ai_big_cmp(a, b), 0)
                                    : vcmp_int(op, toint(a), toint(b));
  return t ? putcharm(1) : zero; }
 if (!isnum(a) || !isnum(b)) return zero;
 struct ai *g = *fp;
 if (gemp(a) || gemp(b)) {                      // float domain -> float box
  ai_flo_t r = vop_flo(op, toflo(a), toflo(b));  // both operands read first: a/b are raw words
  if (!ai_ok(g = ai_have(g, gem_req))) return *fp = g, zero;   // and a float box is a heap object, so
  *fp = g;                                                    // toflo after the have reads a moved one
  return mk_gem(&g->hp, r); }
 if (!bigp(a) && !bigp(b)) {                    // machine-int fast path, overflow-checked
  intptr_t av = toint(a), bv = toint(b), t; bool of;
  switch (op) {
   case vop_quot: case vop_fquot:                         // object (ai_O) arrays truncate under both / and //
                  if (bv == 0) return putcharm(0);          // array convention: int /0 -> 0
                  of = (av == INTPTR_MIN && bv == -1); t = of ? 0 : av / bv; break;
   case vop_rem:  if (bv == 0) return putcharm(0);
                  of = (av == INTPTR_MIN && bv == -1); t = of ? 0 : av % bv; break;
   case vop_sub:  of = __builtin_sub_overflow(av, bv, &t); break;
   case vop_mul:  of = __builtin_mul_overflow(av, bv, &t); break;
   default:       of = __builtin_add_overflow(av, bv, &t); break; }   // vop_add
  if (!of) {                                    // demote-or-box the result
   if (t >= mincharm && t <= maxcharm) return putcharm(t);
   if (!ai_ok(g = ai_have(g, sun_req))) return *fp = g, zero;
   *fp = g;
   return mk_sun(&g->hp, t); } }
 // bignum lane: ai_big_binop computes sp[0] (op) sp[1], leaves it at sp[1],
 // pops one, and advances ip -- so save/restore ip and pop the net result.
 if (!ai_ok(g = ai_push(g, 2, a, b))) return *fp = g, zero;
 union u *ip0 = g->ip;
 avec(g, ip0, g = ai_big_binop(g, op));
 if (!ai_ok(g)) return *fp = g, zero;
 g->ip = ip0;
 word r = g->sp[0]; g->sp++;
 return *fp = g, r; }

// widen the numeric array at g->sp[slot] to a ai_O copy (box each element);
// allocates per element, everything re-fetched after every box
struct ai *tray_to_obj(struct ai *g, int slot) {
 struct ai_tray *src = tray(g->sp[slot]);
 uintptr_t R = src->rank, n = 1;
 for (uintptr_t i = 0; i < R; i++) n *= src->shape[i];
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_O];
 if (!ai_ok(g = ai_have(g, b2w(bytes)))) return g;
 src = tray(g->sp[slot]);
 struct ai_tray *dst = (struct ai_tray*) g->hp; g->hp += b2w(bytes);
 ini_tray(dst, ai_O, R);
 for (uintptr_t i = 0; i < R; i++) dst->shape[i] = src->shape[i];
 for (uintptr_t i = 0; i < n; i++) tray_put_obj(dst, i, zero);   // safe pre-fill (GC may see it)
 if (!ai_ok(g = ai_push(g, 1, word(dst)))) return g;             // sp[0]=dst, src now at slot+1
 for (uintptr_t i = 0; i < n; i++) {
  struct ai_tray *s = tray(g->sp[slot + 1]);
  word v;
  if (s->type >= ai_R) {                                        // float -> float box
   ai_flo_t e = tray_get_flo(s, i);
   if (!ai_ok(g = ai_have(g, gem_req))) return g;
   v = mk_gem(&g->hp, e); }
  else {                                                       // int -> fixnum or sun box
   intptr_t e = tray_get_int(s, i);
   if (e >= mincharm && e <= maxcharm) v = putcharm(e);
   else { if (!ai_ok(g = ai_have(g, sun_req))) return g;
    v = mk_sun(&g->hp, e); } }
  tray_put_obj(tray(g->sp[0]), i, v);                            // re-fetch dst post-box
  gen_wb(g, g->sp[0], v); }                                    // ... and barrier it: see obin_run
 word d = g->sp[0]; g->sp++; g->sp[slot] = d;                  // install copy, drop the parked root
 return g; }

// Pack'd body of lvm_obin (operands at g->sp[0..1], >=1 is a ai_O array).
struct ai *obin_run(struct ai *g, int op) {
 word a = g->sp[0], b = g->sp[1];
 bool atray = trayp(a), btray = trayp(b);
 if (atray && tray(a)->type != ai_O) { if (!ai_ok(g = tray_to_obj(g, 0))) return g; }
 if (btray && tray(b)->type != ai_O) { if (!ai_ok(g = tray_to_obj(g, 1))) return g; }
 a = g->sp[0], b = g->sp[1], atray = trayp(a), btray = trayp(b);
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0,
           R = ra > rb ? ra : rb, n = bshape_n(a, b), shp[maxrank];
 if (n == (uintptr_t) -1) {                                    // non-conforming -> zero
  g->sp[1] = zero, g->sp++, g->ip = (union u*) g->ip + 1; return g; }
 bshape_put(shp, R, a, b);
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_O];
 if (!ai_ok(g = ai_have(g, b2w(bytes)))) return g;
 struct ai_tray *r = (struct ai_tray*) g->hp; g->hp += b2w(bytes);
 ini_tray(r, ai_O, R);
 for (uintptr_t k = 0; k < R; k++) r->shape[k] = shp[k];
 for (uintptr_t p = 0; p < n; p++) tray_put_obj(r, p, zero);     // zero-fill before any GC
 if (!ai_ok(g = ai_push(g, 1, word(r)))) return g;               // sp: [0]=r [1]=a [2]=b
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(atray ? tray(g->sp[1]) : 0, R, ca), bstride(btray ? tray(g->sp[2]) : 0, R, cb);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  word ae = atray ? tray_get_obj(tray(g->sp[1]), oa) : g->sp[1];  // scalar operand re-read each step
  word be = btray ? tray_get_obj(tray(g->sp[2]), ob) : g->sp[2];
  word res = obin_elem(&g, op, ae, be);
  if (!ai_ok(g)) return g;
  tray_put_obj(tray(g->sp[0]), p, res);                          // re-fetch result post-alloc
  // and barrier it: a minor mid-loop promotes the result array while its
  // elements stay young -- an edge the rem set must carry, or the next minor
  // frees an element still in the array
  gen_wb(g, g->sp[0], res);
  odo_step(idx, R, shp); }
 g->sp[2] = g->sp[0], g->sp += 2, g->ip += 1;
 return g; }

lvm(lvm_obin) {
 int op = (int) g->b;
 Pack(g);
 g = obin_run(g, op);
 if (!ai_ok(g)) return Ap(_lvm_ghelp, g);
 return Resume(); }

// ai_O reduction body (kind: 0 sum, 1 prod, 2 max, 3 min). g->sp[0] is the array.
struct ai *ored(struct ai *g, int kind) {
 struct ai_tray *v = tray(g->sp[0]);
 uintptr_t n = 1; for (uintptr_t i = 0; i < v->rank; i++) n *= v->shape[i];
 if (kind >= 2) {                                              // max/min: pick an element, no alloc
  if (!n) { g->sp[0] = zero, g->ip = (union u*) g->ip + 1; return g; }
  word acc = tray_get_obj(tray(g->sp[0]), 0);
  int cop = kind == 2 ? vop_gt : vop_lt;
  for (uintptr_t i = 1; i < n; i++) {
   word e = tray_get_obj(tray(g->sp[0]), i);
   if (obin_elem(&g, cop, e, acc) == putcharm(1)) acc = e; }
  g->sp[0] = acc, g->ip = (union u*) g->ip + 1; return g; }
 word init = kind == 0 ? putcharm(0) : putcharm(1);               // sum/prod: fold with allocation
 int aop = kind == 0 ? vop_add : vop_mul;
 if (!ai_ok(g = ai_push(g, 1, init))) return g;                 // sp[0]=acc, sp[1]=array
 for (uintptr_t i = 0; i < n; i++) {
  word e = tray_get_obj(tray(g->sp[1]), i);
  word acc = obin_elem(&g, aop, g->sp[0], e);
  if (!ai_ok(g)) return g;
  g->sp[0] = acc; }
 word result = g->sp[0]; g->sp++, g->sp[0] = result;          // collapse acc into the array slot
 g->ip = (union u*) g->ip + 1;
 return g; }

// (re, im) of an operand: a complex its parts, a real (value, 0); caller
// guarantees twinp or isnum
void twin_parts(word x, ai_flo_t *re, ai_flo_t *im) {
 if (twinp(x)) *re = twin_re(x), *im = twin_im(x);
 else *re = toflo(x), *im = 0; }

// (ar,ai) `vop` (br,bi) in components: the one set of complex formulas, shared
// by the scalar lane (twin_fill) and the packed array lane (cbin_fill).
void twin_op(int vop, ai_flo_t ar, ai_flo_t ai, ai_flo_t br, ai_flo_t bi,
                             ai_flo_t *re, ai_flo_t *im) {
 switch (vop) {
  case vop_sub: *re = ar - br; *im = ai - bi; break;
  case vop_mul: *re = ar * br - ai * bi; *im = ar * bi + ai * br; break;
  case vop_quot: { ai_flo_t d = br * br + bi * bi;   // (ac+bd)/(c^2+d^2) + ...
   *re = (ar * br + ai * bi) / d; *im = (ai * br - ar * bi) / d; break; }
  default: *re = ar + br; *im = ai + bi; } }          // vop_add

// fill the complex box with a `vop` b; the &-taking lives here (the wrapper's tail call)
ai_noinline void twin_fill(struct ai_twin *v, word a, word b, int vop) {
 ai_flo_t ar, ai, br, bi, re, im;
 twin_parts(a, &ar, &ai); twin_parts(b, &br, &bi);
 twin_op(vop, ar, ai, br, bi, &re, &im);
 twin_set(v, re, im); }

// the complex arithmetic lane: a real operand promotes to (r, 0); non-numeric,
// or % (undefined on complex), yields zero
lvm(lvm_twin_bin) {
 int vop = (int) g->b;
 word a = Sp[0], b = Sp[1];
 if (!(twinp(a) || isnum(a)) || !(twinp(b) || isnum(b)) || vop > vop_quot)
  return Push(ZeroPoint);
 Have(twin_req);
 struct ai_twin *v = (struct ai_twin*) Hp; v->ap = lvm_twinbox; Hp += twin_req;
 twin_fill(v, a, b, vop);
 return Push(word(v)); }

// --- complex-array elementwise lane (ai_C): lvm_vbin's complex twin -- packed
// (re,im) broadcast, a real element promoting to (v, 0)
void cbin_part(bool istray, struct ai_tray *v, ai_flo_t sre, ai_flo_t sim,
                               uintptr_t o, ai_flo_t *re, ai_flo_t *im) {
 if (!istray) { *re = sre; *im = sim; return; }
 if (v->type == ai_C) { ai_flo_t *fp = tray_data(v); *re = fp[2*o]; *im = fp[2*o+1]; }
 else { *re = tray_get_flo(v, o); *im = 0; } }

ai_noinline void cbin_fill(struct ai_tray *r, word a, word b, int op, bool cmp) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sar = 0, sai = 0, sbr = 0, sbi = 0;
 if (!atray) { if (twinp(a)) sar = twin_re(a), sai = twin_im(a); else sar = toflo(a); }
 if (!btray) { if (twinp(b)) sbr = twin_re(b), sbi = twin_im(b); else sbr = toflo(b); }
 ai_flo_t *rf = cmp ? 0 : tray_data(r);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  ai_flo_t ar, ai, br, bi, re, im;
  cbin_part(atray, va, sar, sai, oa, &ar, &ai);
  cbin_part(btray, vb, sbr, sbi, ob, &br, &bi);
  if (cmp) {                                   // (re,im) lexicographic -- the same order
   int t;                                      // cmp3's complex arm gives a scalar pair
   if (op == vop_eq) t = ar == br && ai == bi;     // kept exact (a NaN is equal to nothing)
   else {
    int c = ar < br ? -1 : ar > br ? 1 : ai < bi ? -1 : ai > bi ? 1 : 0;
    t = op == vop_lt ? c < 0 : op == vop_le ? c <= 0
      : op == vop_gt ? c > 0 : c >= 0; }        // vop_ge
   tray_put_int(r, p, t ? 1 : 0); }
  else {
   twin_op(op, ar, ai, br, bi, &re, &im);
   rf[2*p] = re; rf[2*p+1] = im; }
  odo_step(idx, R, r->shape); } }

lvm(lvm_cbin) {
 int op = (int) g->b;
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 // % and // stay undefined on complex, but the orderings hold ((re,im)
 // lexicographic): a tray follows its scalar
 if (!(atray || twinp(a) || isnum(a)) || !(btray || twinp(b) || isnum(b))
     || op == vop_rem || op == vop_fquot)
  return Push(op == vop_eq ? zero : ZeroPoint);   // `=` is boolean: undefined face -> 0, not ()
 bool cmp = op >= vop_lt;
 uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0,
           R = ra > rb ? ra : rb, n = bshape_n(a, b);
 if (n == (uintptr_t) -1) return Push(op == vop_eq ? zero : ZeroPoint);   // non-conformant `=` -> 0
 enum ai_tray_type rt = cmp ? ai_Z : ai_C;              // compare -> i64 mask, else packed complex
 uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 a = Sp[0], b = Sp[1];                                 // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, R);
 bshape_put(r->shape, R, a, b);
 cbin_fill(r, a, b, op, cmp);
 return Push(word(r)); }

// w ** z via the principal branch: exp(z * Log w); w == 0 falls out as the IEEE limit
ai_noinline void twin_pow_fill(struct ai_twin *v, word wbase, word zexp) {
 ai_flo_t wr, wi, zr, zi;
 twin_parts(wbase, &wr, &wi); twin_parts(zexp, &zr, &zi);
 ai_flo_t lr = (ai_flo_t) 0.5 * ai_log(wr * wr + wi * wi),    // ln|w|
          li = ai_atan2(wi, wr),                             // arg w
          pr = zr * lr - zi * li, pi = zr * li + zi * lr,   // z * Log w
          e = ai_exp(pr);
 twin_set(v, e * ai_cos(pi), e * ai_sin(pi)); }

// sin/cos of pi*x, the angle reduced before multiplying by pi so a half-integer
// lands exactly on the axis -- what makes ((/ 1 2) -1) = i bit-exact
ai_flo_t ai_sinpi(ai_flo_t x) {
 intptr_t n = (intptr_t) x; ai_flo_t r = x - (ai_flo_t) n;
 if (r < 0) r += 1, n--;                              // x = n + r, r in (0,1)
 ai_flo_t s = r == (ai_flo_t) 0.5 ? 1
   : ai_sin((ai_flo_t) 3.141592653589793 * (r < (ai_flo_t) 0.5 ? r : 1 - r));
 return n & 1 ? -s : s; }
ai_flo_t ai_cospi(ai_flo_t x) {
 intptr_t n = (intptr_t) x; ai_flo_t r = x - (ai_flo_t) n;
 if (r < 0) r += 1, n--;
 ai_flo_t c = r == (ai_flo_t) 0.5 ? 0
   : r < (ai_flo_t) 0.5 ? ai_cos((ai_flo_t) 3.141592653589793 * r)
   : -ai_cos((ai_flo_t) 3.141592653589793 * (1 - r));
 return n & 1 ? -c : c; }
// finite non-integer? everything at/past 2^mantissa is an integer; nan/inf out.
bool flo_fracp(ai_flo_t x) {
 ai_flo_t lim = (ai_flo_t) (1ull << (Bits == 64 ? 53 : 24));
 return x > -lim && x < lim && (ai_flo_t) (intptr_t) x != x; }

// (power b e): complex operands take the complex lane; a finite negative real
// base to a non-integer power widens to its principal root instead of nan (pow
// climbs tiers like log). everything else keeps the IEEE real lanes.
lvm(lvm_pow) {
 word a = Sp[0], b = Sp[1];
 if (twinp(a) || twinp(b)) {
  if (!(twinp(a) || isnum(a)) || !(twinp(b) || isnum(b)))
   ai_musttail return Push(ZeroPoint);
  Have(twin_req);
  struct ai_twin *v = (struct ai_twin*) Hp;
  Hp += twin_req;
  v->ap = lvm_twinbox;
  twin_pow_fill(v, a, b);
  ai_musttail return Push(word(v)); }
 if (isnum(a) && isnum(b)) {
  ai_flo_t ad = toflo(a), bd = toflo(b);
  if (ad < 0 && !__builtin_isinf(ad) && flo_fracp(bd)) {
   ai_flo_t m = ai_pow(-ad, bd), re = m * ai_cospi(bd), im = m * ai_sinpi(bd);
   Have(twin_req);
   *++Sp = mk_twin(&Hp, re, im); ai_musttail return Next(1); } }
 { g->b = (ai_word) (uintptr_t) (ai_pow); ai_musttail return Ap(lvm_math2, g); } }

// fill a packed ai_C array with (re = a-element, im = b-element) under broadcast
ai_noinline void twin_build_fill(struct ai_tray *r, word a, word b) {
 uintptr_t R = r->rank, n = tray_nelem(r);
 bool atray = trayp(a), btray = trayp(b);
 struct ai_tray *va = atray ? tray(a) : 0, *vb = btray ? tray(b) : 0;
 intptr_t ca[maxrank], cb[maxrank], idx[maxrank];
 for (uintptr_t j = 0; j < R; j++) idx[j] = 0;
 bstride(va, R, ca), bstride(vb, R, cb);
 ai_flo_t sa = atray ? 0 : toflo(a), sb = btray ? 0 : toflo(b),
          *rf = tray_data(r);
 for (uintptr_t p = 0; p < n; p++) {
  intptr_t oa = 0, ob = 0;
  for (uintptr_t j = 0; j < R; j++) oa += idx[j] * ca[j], ob += idx[j] * cb[j];
  rf[2*p]   = atray ? tray_get_flo(va, oa) : sa;
  rf[2*p+1] = btray ? tray_get_flo(vb, ob) : sb;
  odo_step(idx, R, r->shape); } }

// (twin re im): scalars -> a complex box; a real array operand -> a packed ai_C
// array (so arg stays elementwise); complex/object array or non-numeric -> zero
lvm(lvm_twin) {
 word a = Sp[0], b = Sp[1];
 bool atray = trayp(a), btray = trayp(b);
 if (atray || btray) {
  if ((atray && tray(a)->type >= ai_C) || (btray && tray(b)->type >= ai_C)
      || (!atray && !isnum(a)) || (!btray && !isnum(b)))
   ai_musttail return Push(ZeroPoint);
  uintptr_t ra = atray ? tray(a)->rank : 0, rb = btray ? tray(b)->rank : 0,
            R = ra > rb ? ra : rb, n = bshape_n(a, b);
  if (n == (uintptr_t) -1) ai_musttail return Push(ZeroPoint);
  uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_C];
  Have(b2w(bytes));
  a = Sp[0], b = Sp[1];                                     // re-read post-Have
  struct ai_tray *r = (struct ai_tray*) Hp;
  Hp += b2w(bytes);
  ini_tray(r, ai_C, R);
  bshape_put(r->shape, R, a, b);
  twin_build_fill(r, a, b);
  ai_musttail return Push(word(r)); }
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint);
 ai_flo_t re = toflo(a), im = toflo(b);             // values extracted before alloc
 Have(twin_req);
 *++Sp = mk_twin(&Hp, re, im); ai_musttail return Next(1); }

// (twinp x): is x a complex scalar?
op11(lvm_twinp, twinp(Sp[0]) ? putcharm(1) : zero)

// fill r with component `off` (0 = re, 1 = im) of each element; off < 0 is the
// (im realarr) lane -- all zeros
ai_noinline void cpart_fill(struct ai_tray *r, struct ai_tray *v, int off) {
 uintptr_t n = tray_nelem(r);
 if (off < 0) {
  intptr_t *zp = tray_data(r);
  for (uintptr_t p = 0; p < n; p++) zp[p] = 0;
  return; }
 ai_flo_t *rf = tray_data(r), *fp = tray_data(v);
 for (uintptr_t p = 0; p < n; p++) rf[p] = fp[2*p + off]; }

// the array lane of re/im: result carries the operand's shape
lvm(lvm_cpart) {
 int off = (int) g->b;
 struct ai_tray *v = tray(Sp[0]);
 enum ai_tray_type rt = off < 0 ? ai_Z : ai_R;
 uintptr_t R = v->rank, n = tray_nelem(v),
           bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[rt];
 Have(b2w(bytes));
 v = tray(Sp[0]);                                           // re-read post-Have
 struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
 ini_tray(r, rt, R);
 for (uintptr_t i = 0; i < R; i++) r->shape[i] = v->shape[i];
 cpart_fill(r, v, off);
 return Answer(word(r)); }

// (re z) / (im z): the parts, elementwise over an array (a real array is its own
// real part; im of one is fresh zeros); object array or non-number -> zero
lvm(lvm_re) {
 word a = Sp[0], _res;
 if (twinp(a)) {
  ai_flo_t re = twin_re(a);
  Have(box_req);
  emit_gem(_res, re);
  ai_musttail return Answer(_res); }
 if (trayp(a)) {
  enum ai_tray_type t = tray(a)->type;
  if (t == ai_O) ai_musttail return Answer(ZeroPoint);   // a tray is not a number
  if (t != ai_C) ai_musttail return Next(1);          // a real array is its own real part
  { g->b = (ai_word) (0); ai_musttail return Ap(lvm_cpart, g); } }
 if (isnum(a)) ai_musttail return Next(1);            // re of a real is itself
 ai_musttail return Answer(ZeroPoint); }

lvm(lvm_im) {
 word a = Sp[0], _res;
 if (twinp(a)) {
  ai_flo_t im = twin_im(a);
  Have(box_req);
  emit_gem(_res, im);
  ai_musttail return Answer(_res); }
 if (trayp(a)) {
  enum ai_tray_type t = tray(a)->type;
  if (t == ai_O) ai_musttail return Answer(ZeroPoint);
  { g->b = (ai_word) (t == ai_C ? 1 : -1); ai_musttail return Ap(lvm_cpart, g); } }   // real array -> zeros of its shape
 if (isnum(a)) ai_musttail return Answer(putcharm(0));   // im of a real is 0
 ai_musttail return Answer(ZeroPoint); }

// (conj z): complex conjugate. conj lifts -- a real r becomes ~(r 0), so it
// always lands in C (the monadic `~`).
lvm(lvm_conj) {
 word a = Sp[0];
 if (twinp(a)) {
  ai_flo_t re = twin_re(a), im = twin_im(a);
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, re, -im);
  ai_musttail return Next(1); }
 if (isnum(a)) {
  ai_flo_t re = toflo(a);            // lift a real to ~(r 0)
  Have(twin_req);
  Sp[0] = mk_twin(&Hp, re, 0);
  ai_musttail return Next(1); }
 ai_musttail return Answer(ZeroPoint); }

// (abs z): magnitude in its own tier; |INTPTR_MIN| promotes to a bignum (the one
// magnitude the box can't hold), its limb scratch out of line per the lvm scratch rule.
ai_noinline word abs_wmin(struct ai *g) {
 uintptr_t u = (uintptr_t) 1 << (Bits - 1);
 ai_limb lb[wlimbs];
 for (int i = 0; i < wlimbs; i++) lb[i] = (ai_limb) (u >> (limb_bits * i));
 return ai_big_canon(&g->hp, lb, wlimbs, false); }
lvm(lvm_abs) {
 word a = Sp[0], _res;
 if (charmp(a)) {
  intptr_t n = getcharm(a);
  Have(box_req);
  emit_int(_res, n < 0 ? (intptr_t) (0 - (uintptr_t) n) : n);
  ai_musttail return Answer(_res); }
 if (twinp(a)) {
  ai_flo_t m = twin_mod(a);
  Have(box_req);
  emit_gem(_res, m);
  ai_musttail return Answer(_res); }
 if (gemp(a)) {
  ai_flo_t v = gem_get(a); if (v < 0) v = -v;
  Have(box_req);
  emit_gem(_res, v);
  ai_musttail return Answer(_res); }
 if (sunp(a)) { intptr_t n = sun_get(a);
  if (n == INTPTR_MIN) {                              // |INTPTR_MIN| = 2^(W-1): the bignum lane
   Have(b2w(sizeof(struct ai_big) + wlimbs * sizeof(ai_limb)));
   Pack(g);                                           // canon bumps the synced g->hp, lvm_bmul's law
   word r = abs_wmin(g);
   Unpack(g);
   ai_musttail return Answer(r); }
  Have(box_req); emit_int(_res, n < 0 ? (intptr_t) (0 - (uintptr_t) n) : n);
  ai_musttail return Answer(_res); }
 if (bigp(a)) {
  struct ai_big *x = big(a);
  if (x->slen > 0) ai_musttail return Next(1);         // already non-negative
  uintptr_t bytes = ai_big_bytes(x); Have(b2w(bytes));
  x = big(Sp[0]);                         // re-read post-Have
  struct ai_big *y = big(Hp);
  Hp += b2w(bytes);
  memcpy(y, x, bytes); y->slen = -x->slen;           // flip the sign
  ai_musttail return Answer(word(y)); }
 if (trayp(a)) {                                       // vector -> scalar: the Euclidean (L2) norm
  struct ai_tray *v = tray(a); uintptr_t i, n = tray_nelem(v);   // sqrt(sum of squares); abs of a
  ai_flo_t s = 0;                                      // complex elem is its 2-vector modulus; ai_C sums 2n floats
  if (v->type == ai_C) { ai_flo_t *fp = tray_data(v); for (i = 0; i < 2*n; i++) s += fp[i] * fp[i]; }
  else for (i = 0; i < n; i++) { ai_flo_t e = tray_get_flo(v, i); s += e * e; }
  Have(box_req);
  emit_gem(_res, ai_sqrt(s));
  ai_musttail return Answer(_res); }
 if (tabp(a)) {                                       // table: its key count (so (int (abs t)) == (len t))
  Have(box_req);
  emit_int(_res, (intptr_t) map_len(a));
  ai_musttail return Answer(_res); }
 ai_musttail return Answer(ZeroPoint); }

// fill f64 array r with arg of each element of v
ai_noinline void carg_fill(struct ai_tray *r, struct ai_tray *v) {
 uintptr_t n = tray_nelem(v);
 ai_flo_t *rf = tray_data(r);
 if (v->type == ai_C) { ai_flo_t *fp = tray_data(v);
  for (uintptr_t p = 0; p < n; p++) rf[p] = ai_atan2(fp[2*p+1], fp[2*p]); }
 else for (uintptr_t p = 0; p < n; p++) rf[p] = ai_atan2(0, tray_get_flo(v, p)); }

// (arg z): phase angle atan2(im, re); elementwise over an array, zero on a non-number
lvm(lvm_carg) {
 word a = Sp[0], _res;
 if (twinp(a)) {
  ai_flo_t r = ai_atan2(twin_im(a), twin_re(a));
  Have(box_req);
  emit_gem(_res, r);
  ai_musttail return Answer(_res); }
 if (trayp(a)) {
  struct ai_tray *v = tray(a);
  if (v->type == ai_O) ai_musttail return Answer(ZeroPoint);   // object array -> zero
  uintptr_t R = v->rank, n = 1;
  for (uintptr_t i = 0; i < R; i++) n *= v->shape[i];
  uintptr_t bytes = sizeof(struct ai_tray) + R * sizeof(word) + n * ai_T[ai_R];
  Have(b2w(bytes));
  v = tray(Sp[0]);                                           // re-read post-Have
  struct ai_tray *r = (struct ai_tray*) Hp; Hp += b2w(bytes);
  ini_tray(r, ai_R, R);
  for (uintptr_t i = 0; i < R; i++) r->shape[i] = v->shape[i];
  carg_fill(r, v);
  ai_musttail return Answer(word(r)); }
 if (isnum(a)) {
  ai_flo_t r = ai_atan2(0, toflo(a));
  Have(box_req);
  emit_gem(_res, r);
  ai_musttail return Answer(_res); }
 ai_musttail return Answer(ZeroPoint); }

