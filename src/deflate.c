// src/deflate.c -- the C twin of lib/gz.l's DEFLATE coder, auto-globbed +
// AiNif-registered (no love.c/love.h/main.c edit), inflate.c's discipline:
//
//   (deflate s) -> the raw DEFLATE stream | ()
//
// a twin held to the bytes, like gz-find's machine-code presentation and unlike a
// licensed re-compressor: same greedy parse (chain 32, min match 3, the far-3
// refusal at 4096), same 16384-symbol blocks each costed stored/fixed/dynamic,
// same two-queue Huffman merge with its leaf-wins tie, same halving walk back
// under the depth limit -- so `cmp` over any input is the differential, and a
// divergence is a bug in one of them and never a licensed difference. gz-deflate
// stays the readable statement; test/host/gzc.l holds the two to the same bytes.
//
// why this exists: the love coder is correct and 35x-to-13x off C -- but its
// real cost is the heap, not the clock. interpreted DEFLATE churns cells per
// symbol, and on the love0 egg that runs selfpack the heap grows toward the
// budget -- half the box's RAM by default -- before a collection pays. this file
// is a fixed window and some tables.
//
// scratch is not the heap: the off semispace where it is big enough (the major
// pool's spare half is dead between collections), one g->alloc block where it is
// not. and the shape is inflate's counting pass twice over: count, str0 the exact
// answer, re-derive and emit -- because str0 may collect, and a collection flips
// the spare half out from under any pointer held across it.
#include "love.h"
#include <stdint.h>
#include <string.h>

// RFC 1951 3.2.5, and lib/gz.l's gz-lbase/gz-lext/gz-dbase/gz-dext say them again
static const uint16_t df_lbase[29] = {
 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59,
 67, 83, 99, 115, 131, 163, 195, 227, 258 };
static const uint8_t df_lext[29] = {
 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
static const uint16_t df_dbase[30] = {
 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769,
 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };
static const uint8_t df_dext[30] = {
 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
 12, 12, 13, 13 };
static const uint8_t df_clord[19] = {
 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

#define DF_WSIZE 32768u
#define DF_WMASK 32767u
#define DF_HMASK 32767u
#define DF_CHAIN 32
#define DF_TOKMAX 16384u
#define DF_TOOFAR 4096u

// the bit sink: LSB-first bytes, codes handed in already reversed (gz-put's law).
// with no out it counts, which is the whole first pass.
struct df_sink { uint8_t *out; uintptr_t op, cap; uint64_t acc; unsigned nb; int err; };
static void df_put(struct df_sink *t, uint32_t v, unsigned k) {
 t->acc |= (uint64_t) (v & ((1u << k) - 1)) << t->nb;
 t->nb += k;
 while (t->nb >= 8) {
  if (t->out) { if (t->op >= t->cap) { t->err = 1; return; } t->out[t->op] = (uint8_t) (t->acc & 255); }
  t->op++; t->acc >>= 8; t->nb -= 8; } }
static void df_align(struct df_sink *t) {
 if (!t->nb) return;
 if (t->out) { if (t->op >= t->cap) { t->err = 1; return; } t->out[t->op] = (uint8_t) (t->acc & 255); }
 t->op++; t->acc = 0; t->nb = 0; }
static uint32_t df_rev(uint32_t v, unsigned k) {
 uint32_t a = 0; unsigned i;
 for (i = 0; i < k; i++) { a = (a << 1) | (v & 1); v >>= 1; }
 return a; }

// the ladders are short and walked from the top (gz-lcode/gz-dcode)
static unsigned df_lcode(unsigned v) { unsigned i; for (i = 28; i >= 1; i--) if (df_lbase[i] <= v) return i; return 0; }
static unsigned df_dcode(unsigned v) { unsigned i; for (i = 29; i >= 1; i--) if (df_dbase[i] <= v) return i; return 0; }

// --- the Huffman lengths: sorted leaves + the internal queue, ties to the leaf,
// --- and the halving walk when the code outgrows the format (gz-hpass exactly).
// scratch rows are the caller's; pa is zeroed here because depth chases it to 0.
static unsigned df_hlens(uint32_t *f, unsigned nsym, unsigned lim, uint8_t *lens,
                         uint32_t *hc, uint32_t *keys, uint32_t *w, uint32_t *sy, uint32_t *pa) {
 unsigned i, u = 0, pass;
 for (i = 0; i < nsym; i++) hc[i] = f[i];
 for (i = 0; i < nsym; i++) if (hc[i]) u++;
 if (u < 2) { if (!hc[0]) hc[0] = 1; if (!hc[1]) hc[1] = 1; }   // a code needs two symbols
 for (pass = 0;; pass++) {
  unsigned nl = 0, top, li, ii, mx = 0;
  for (i = 0; i < nsym; i++) if (hc[i]) keys[nl++] = (hc[i] << 9) + i;
  { unsigned a; for (a = 1; a < nl; a++) {                      // counts ride their symbol; ties to
     uint32_t v = keys[a]; unsigned b = a;                      // the lower symbol, sort ascending
     while (b && keys[b - 1] > v) { keys[b] = keys[b - 1]; b--; }
     keys[b] = v; } }
  for (i = 0; i < nl; i++) { w[i] = keys[i] >> 9; sy[i] = keys[i] & 511; }
  memset(pa, 0, (2 * nl - 1) * sizeof *pa);
  li = 0; ii = nl;
  for (top = nl; top + 1 <= 2 * nl - 1; top++) {
   unsigned a, b;
#define DF_PICK(L, I) ((L) < nl && ((I) >= top || w[L] <= w[I]))
   if (DF_PICK(li, ii)) a = li++; else a = ii++;
   if (DF_PICK(li, ii)) b = li++; else b = ii++;
#undef DF_PICK
   w[top] = w[a] + w[b]; pa[a] = top + 1; pa[b] = top + 1; }
  memset(lens, 0, nsym);
  for (i = 0; i < nl; i++) {
   unsigned d = 0, p = pa[i];
   while (p) { d++; p = pa[p - 1]; }
   lens[sy[i]] = (uint8_t) d;
   if (d > mx) mx = d; }
  if (mx <= lim || pass >= 32) return mx;
  for (i = 0; i < nsym; i++) if (hc[i]) { uint32_t h = hc[i] >> 1; hc[i] = h ? h : 1; } } }

// canonical, shortest first, ties by symbol, stored reversed (gz-hcodes)
static void df_hcodes(const uint8_t *lens, unsigned nsym, unsigned mx, uint32_t *code) {
 uint32_t cnt[16], nxt[16], c = 0;
 unsigned i, l;
 memset(cnt, 0, sizeof cnt);
 for (i = 0; i < nsym; i++) if (lens[i]) cnt[lens[i]]++;
 for (l = 1; l <= mx && l < 16; l++) { nxt[l] = c; c = (c + cnt[l]) << 1; }
 for (i = 0; i < nsym; i++) if (lens[i]) { code[i] = df_rev(nxt[lens[i]], lens[i]); nxt[lens[i]]++; } }

// the code lengths, run-length coded: a value once before 16 may repeat it,
// 17 and 18 the zero runs (gz-run); an entry is (sym << 7) + extra.
static unsigned df_run(const uint8_t *cl, unsigned tot, uint32_t *rl) {
 unsigned i = 0, j = 0;
 while (i < tot) {
  unsigned v = cl[i], r = 1;
  while (i + r < tot && cl[i + r] == v) r++;
  if (v) {
   rl[j++] = ((uint32_t) v << 7); i++; r--;
   while (r >= 3) { unsigned t = r > 6 ? 6 : r; rl[j++] = (16u << 7) + (t - 3); i += t; r -= t; }
   while (r) { rl[j++] = ((uint32_t) v << 7); i++; r--; } }
  else {
   while (r >= 11) { unsigned t = r > 138 ? 138 : r; rl[j++] = (18u << 7) + (t - 11); i += t; r -= t; }
   if (r >= 3) { rl[j++] = (17u << 7) + (r - 3); i += r; r = 0; }
   while (r) { rl[j++] = (0u << 7); i++; r--; } } }
 return j; }
static unsigned df_clx(unsigned sy) { return sy == 16 ? 2 : sy == 17 ? 3 : sy == 18 ? 7 : 0; }

// what a block would cost, in bits (gz-cost / gz-ccost)
static uint64_t df_cost(const uint32_t *f, const uint8_t *lens, unsigned nsym) {
 uint64_t a = 0; unsigned i;
 for (i = 0; i < nsym; i++) a += (uint64_t) f[i] * lens[i];
 return a; }
static uint64_t df_ccost(const uint32_t *rl, const uint8_t *lenc, unsigned nr) {
 uint64_t a = 0; unsigned i;
 for (i = 0; i < nr; i++) { unsigned sy = rl[i] >> 7; a += lenc[sy] + df_clx(sy); }
 return a; }

static void df_wtoks(struct df_sink *t, const uint32_t *tok, unsigned k,
                     const uint32_t *codl, const uint8_t *lenl,
                     const uint32_t *codd, const uint8_t *lend) {
 unsigned i;
 for (i = 0; i < k; i++) {
  uint32_t v = tok[i];
  if (v < 256) { df_put(t, codl[v], lenl[v]); continue; }
  { uint32_t u = v - 256;
    unsigned lc = u >> 23, dc = (u >> 18) & 31;
    df_put(t, codl[257 + lc], lenl[257 + lc]);
    if (df_lext[lc]) df_put(t, (u >> 13) & 31, df_lext[lc]);
    df_put(t, codd[dc], lend[dc]);
    if (df_dext[dc]) df_put(t, u & 8191, df_dext[dc]); } }
 df_put(t, codl[256], lenl[256]); }                             // 256 ends the block

static void df_wstored(struct df_sink *t, const uint8_t *s, uintptr_t i0, uintptr_t i1, int last) {
 uintptr_t len = i1 - i0, k;
 df_put(t, last ? 1 : 0, 1); df_put(t, 0, 2);
 df_align(t);                                                   // a stored block starts at a byte
 df_put(t, (uint32_t) (len & 255), 8);          df_put(t, (uint32_t) ((len >> 8) & 255), 8);
 df_put(t, (uint32_t) ((len ^ 65535) & 255), 8); df_put(t, (uint32_t) (((len ^ 65535) >> 8) & 255), 8);
 for (k = 0; k < len; k++) df_put(t, s[i0 + k], 8); }

// the arena, carved once: the finder's two windows outlive every block, the rest
// is per block or per code and merely reused.
struct df_ar {
 uint32_t *head, *prev, *tok, *fl, *fd;
 uint32_t *hc, *keys, *w, *sy, *pa, *rlb, *fc;
 uint32_t *codl, *codd, *codc, *fixcl, *fixcd;
 uint8_t *lenl, *lend, *lenc, *fixll, *fixld, *cl; };
#define DF_ARENA (384u << 10)
static void df_carve(uint8_t *m, struct df_ar *a) {
 uint8_t *p = m;
#define DF_TAKE(f, n, ty) a->f = (ty*) p; p += ((n) * sizeof(ty) + 7u) & ~7u
 DF_TAKE(head, 32768, uint32_t); DF_TAKE(prev, 32768, uint32_t);
 DF_TAKE(tok, DF_TOKMAX, uint32_t); DF_TAKE(fl, 286, uint32_t); DF_TAKE(fd, 30, uint32_t);
 DF_TAKE(hc, 286, uint32_t); DF_TAKE(keys, 286, uint32_t);
 DF_TAKE(w, 571, uint32_t); DF_TAKE(sy, 571, uint32_t); DF_TAKE(pa, 571, uint32_t);
 DF_TAKE(rlb, 320, uint32_t); DF_TAKE(fc, 19, uint32_t);
 DF_TAKE(codl, 286, uint32_t); DF_TAKE(codd, 30, uint32_t); DF_TAKE(codc, 19, uint32_t);
 DF_TAKE(fixcl, 288, uint32_t); DF_TAKE(fixcd, 30, uint32_t);
 DF_TAKE(lenl, 286, uint8_t); DF_TAKE(lend, 30, uint8_t); DF_TAKE(lenc, 19, uint8_t);
 DF_TAKE(fixll, 288, uint8_t); DF_TAKE(fixld, 30, uint8_t); DF_TAKE(cl, 316, uint8_t);
#undef DF_TAKE
}

#define DF_HASH(s, i) (((((uint32_t) (s)[i] << 10) ^ ((uint32_t) (s)[(i) + 1] << 5)) ^ (s)[(i) + 2]) & DF_HMASK)
static void df_ins(const uint8_t *s, uintptr_t n, uintptr_t i, uint32_t *head, uint32_t *prev) {
 if (i + 2 < n) {
  uint32_t h = DF_HASH(s, i);
  prev[i & DF_WMASK] = head[h];
  head[h] = (uint32_t) i + 1; } }

// gz-find: down the chain, nearest wins a tie, a full-limit match taken at once
static void df_find(const uint8_t *s, uintptr_t n, uintptr_t i, const uint32_t *head,
                    const uint32_t *prev, unsigned *rl, unsigned *rd) {
 unsigned lim = (n - i) < 258 ? (unsigned) (n - i) : 258;
 uint32_t k = head[DF_HASH(s, i)];
 int d = DF_CHAIN;
 unsigned bl = 0, bd = 0;
 while (k && d) {
  uintptr_t p = k - 1, dist = i - p;
  unsigned l = 0;
  if (dist > DF_WSIZE) break;
  while (l < lim && s[i + l] == s[p + l]) l++;
  k = prev[p & DF_WMASK]; d--;
  if (l > bl) {
   if (l >= lim) { *rl = l; *rd = (unsigned) dist; return; }
   bl = l; bd = (unsigned) dist; } }
 *rl = bl; *rd = bd; }

// one block: tokenize (gz-tok), then the three spellings costed and the cheapest
// written (gz-flush). the dynamic code wins its tie with the fixed one.
static uintptr_t df_block(const uint8_t *s, uintptr_t n, uintptr_t i, struct df_sink *t, struct df_ar *a) {
 unsigned k = 0;
 uint64_t xb = 0;
 uintptr_t i0 = i;
 int last;
 memset(a->fl, 0, 286 * sizeof *a->fl);
 memset(a->fd, 0, 30 * sizeof *a->fd);
 while (i < n && k < DF_TOKMAX) {
  unsigned l = 0, d = 0;
  if (i + 3 <= n) df_find(s, n, i, a->head, a->prev, &l, &d);
  if (l == 3 && d > DF_TOOFAR) l = 0;                           // a far three is a loss
  if (l < 3) {
   unsigned c = s[i];
   a->tok[k++] = c; a->fl[c]++;
   df_ins(s, n, i, a->head, a->prev); i++; }
  else {
   unsigned lc = df_lcode(l), dc = df_dcode(d), q;
   a->tok[k++] = 256u + ((uint32_t) lc << 23) + ((uint32_t) dc << 18)
               + ((uint32_t) (l - df_lbase[lc]) << 13) + (d - df_dbase[dc]);
   a->fl[257 + lc]++; a->fd[dc]++;
   xb += df_lext[lc] + df_dext[dc];
   for (q = 0; q < l; q++) df_ins(s, n, i + q, a->head, a->prev);
   i += l; } }
 last = n <= i;
 a->fl[256]++;                                                  // end-of-block, always sent
 { unsigned mxl = df_hlens(a->fl, 286, 15, a->lenl, a->hc, a->keys, a->w, a->sy, a->pa);
   unsigned mxd = df_hlens(a->fd, 30, 15, a->lend, a->hc, a->keys, a->w, a->sy, a->pa);
   unsigned hlit, hdist, hclen, nr, j2, mxc;
   uint64_t dyn, fix, raw;
   for (hlit = 286; hlit > 257 && !a->lenl[hlit - 1]; hlit--) ;
   for (hdist = 30; hdist > 1 && !a->lend[hdist - 1]; hdist--) ;
   memcpy(a->cl, a->lenl, hlit); memcpy(a->cl + hlit, a->lend, hdist);
   nr = df_run(a->cl, hlit + hdist, a->rlb);
   memset(a->fc, 0, 19 * sizeof *a->fc);
   for (j2 = 0; j2 < nr; j2++) a->fc[a->rlb[j2] >> 7]++;
   mxc = df_hlens(a->fc, 19, 7, a->lenc, a->hc, a->keys, a->w, a->sy, a->pa);
   for (hclen = 19; hclen > 4 && !a->lenc[df_clord[hclen - 1]]; hclen--) ;
   dyn = 17 + hclen * 3 + df_ccost(a->rlb, a->lenc, nr)
       + df_cost(a->fl, a->lenl, 286) + df_cost(a->fd, a->lend, 30) + xb;
   fix = 3 + df_cost(a->fl, a->fixll, 286) + df_cost(a->fd, a->fixld, 30) + xb;
   raw = (i - i0) < 65536 ? 42 + (uint64_t) (i - i0) * 8 : dyn + fix;   // no stored spelling past len
   if (raw < (dyn <= fix ? dyn : fix)) df_wstored(t, s, i0, i, last);
   else if (fix < dyn) {
    df_put(t, last ? 1 : 0, 1); df_put(t, 1, 2);
    df_wtoks(t, a->tok, k, a->fixcl, a->fixll, a->fixcd, a->fixld); }
   else {
    df_hcodes(a->lenl, 286, mxl, a->codl);
    df_hcodes(a->lend, 30, mxd, a->codd);
    df_hcodes(a->lenc, 19, mxc, a->codc);
    df_put(t, last ? 1 : 0, 1); df_put(t, 2, 2);
    df_put(t, hlit - 257, 5); df_put(t, hdist - 1, 5); df_put(t, hclen - 4, 4);
    for (j2 = 0; j2 < hclen; j2++) df_put(t, a->lenc[df_clord[j2]], 3);
    for (j2 = 0; j2 < nr; j2++) {
     uint32_t v = a->rlb[j2]; unsigned sy = v >> 7;
     df_put(t, a->codc[sy], a->lenc[sy]);
     if (df_clx(sy)) df_put(t, v & 127, df_clx(sy)); }
    df_wtoks(t, a->tok, k, a->codl, a->lenl, a->codd, a->lend); } }
 return i; }

static int64_t df_go(const uint8_t *s, uintptr_t n, uint8_t *out, uintptr_t cap, uint8_t *m) {
 struct df_ar a;
 struct df_sink t;
 uintptr_t i = 0;
 unsigned j;
 df_carve(m, &a);
 memset(a.head, 0, 32768 * sizeof *a.head);
 memset(a.prev, 0, 32768 * sizeof *a.prev);
 for (j = 0; j < 288; j++) a.fixll[j] = j < 144 ? 8 : j < 256 ? 9 : j < 280 ? 7 : 8;
 for (j = 0; j < 30; j++) a.fixld[j] = 5;
 df_hcodes(a.fixll, 288, 9, a.fixcl);
 df_hcodes(a.fixld, 30, 5, a.fixcd);
 t.out = out; t.op = 0; t.cap = cap; t.acc = 0; t.nb = 0; t.err = 0;
 do i = df_block(s, n, i, &t, &a); while (i < n && !t.err);
 df_align(&t);
 return t.err ? -2 : (int64_t) t.op; }

// str0 collects, so both the source and the spare half are re-derived after it:
// a pointer held across the bump is stale, and a major collection flips the halves.
static uint8_t *df_arena(struct ai *g, int *alloced) {
 *alloced = 0;
 if (g->major_pool && g->major_len * sizeof(ai_word) >= DF_ARENA)
  return (uint8_t*) ((g->major_base == g->major_pool) ? g->major_pool + g->major_len : g->major_pool);
 { void *p = g->alloc(g, NULL, DF_ARENA);
   if (p) *alloced = 1;
   return (uint8_t*) p; } }

ai_noinline static struct ai *host_deflate(struct ai *g) {
 ai_word sw = g->sp[0];
 uint8_t *m;
 int alloced;
 int64_t want, got;
 if (!ai_strp(sw)) { g->sp[0] = ZeroPoint; return g; }
 m = df_arena(g, &alloced);
 if (!m) { g->sp[0] = ZeroPoint; return g; }
 want = df_go((const uint8_t*) txt(sw), len(sw), 0, (uintptr_t) -1, m);
 if (alloced) g->alloc(g, m, 0);
 if (want < 0) { g->sp[0] = ZeroPoint; return g; }
 if (!ai_ok(g = str0(g, (uintptr_t) want))) return g;
 m = df_arena(g, &alloced);
 if (!m) { g->sp[1] = ZeroPoint, g->sp += 1; return g; }
 got = df_go((const uint8_t*) txt(g->sp[1]), len(g->sp[1]),
             (uint8_t*) txt(g->sp[0]), (uintptr_t) want, m);
 if (alloced) g->alloc(g, m, 0);
 if (got != want) { g->sp[1] = ZeroPoint, g->sp += 1; return g; }
 g->sp[1] = g->sp[0], g->sp += 1;
 return g; }

static lvm(lvm_deflate) {
 Pack(g); g = host_deflate(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// one operand, so the run is {impl, ret0} -- src/nifs.l states the law and lvm_cur
// curries once unconditionally, which at arity one hands the body an operand too many.
static union u const nif_deflate[] = {{lvm_deflate}, {lvm_ret0}};
AiNif("deflate", nif_deflate);
