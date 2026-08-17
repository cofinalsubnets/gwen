// host/inflate.c -- the C twin of lib/gz.l's inflate, auto-globbed + AiNif-registered
// (no love.c/love.h/main.c edit), the tls.c discipline:
//
//   (inflate s n) -> the bytes | ()    s a raw DEFLATE stream, n its inflated size or 0
//
// a TWIN, not a replacement: gz-puff stays the readable statement of RFC 1951 and the
// differential oracle (test/host/gzc.l holds the two to the same bytes over corpora and
// over torn and doctored streams). `gz-inflate` reaches for this and falls back to it.
//
// ⚠ THE ALGORITHM IS NOT THE LOVE FILE'S, and here that is the whole point. gz-puff
// walks a canonical code ONE BIT AT A TIME because in love a table would cost more to
// build than it saves; this reads a 64-bit window and indexes a table built per block.
// The two are held to the same BYTES, never to the same shape -- so a divergence is a
// bug in one of them and never a licensed difference.
//
// ⚠ AND WHERE THE STREAM IS MALFORMED THE TWIN'S ANSWER IS STILL THE LAW. gz-huff does
// not check that the code lengths describe a code, so an over-subscribed one decodes to
// nonsense there rather than failing -- and this file reproduces that nonsense exactly:
// the table is filled FIRST-WRITER-WINS so a doubly-claimed slot answers the shortest
// code, which is what the bit walk finds, and the symbol array is zeroed so an index off
// its end reads 0, which is what `peep` on a tablet answers. Checking would be better
// engineering and a differential failure, and the differential is what we have.
//
//   ⚠ the size argument is a HINT AND A BOUND: nothing grows here (host/ has no malloc,
//   and a love string cannot be extended), so the output is allocated once. A positive n
//   is believed and verified -- the gzip trailer always has it -- and a wrong or absent
//   one costs a counting pass first, which is the decode with the stores dropped.
#include "love.h"
#include <stdint.h>
#include <string.h>

// RFC 1951 §3.2.5, and lib/gz.l's gz-lbase/gz-lext/gz-dbase/gz-dext say them again
static const uint16_t inf_lbase[29] = {
 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59,
 67, 83, 99, 115, 131, 163, 195, 227, 258 };
static const uint8_t inf_lext[29] = {
 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 };
static const uint16_t inf_dbase[30] = {
 1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769,
 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577 };
static const uint8_t inf_dext[30] = {
 0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
 12, 12, 13, 13 };
static const uint8_t inf_clord[19] = {
 16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15 };

// eight unaligned bytes as a word, little-endian BY CONSTRUCTION rather than by the
// machine's say-so. ⚠ NOT memcpy AND NOT A CAST: gcc folds this to one load, and mooncc
// -- which is what compiles the shipped artifact -- emits eight loads and shifts where it
// would emit a CALL for the memcpy, which measured 1.5x slower over the whole decode.
#define LD64(p) ((uint64_t) (p)[0]       | (uint64_t) (p)[1] <<  8 \
               | (uint64_t) (p)[2] << 16 | (uint64_t) (p)[3] << 24 \
               | (uint64_t) (p)[4] << 32 | (uint64_t) (p)[5] << 40 \
               | (uint64_t) (p)[6] << 48 | (uint64_t) (p)[7] << 56)

// the table roots. a code longer than its root falls through to the bit walk, so these
// only trade build cost against how often that happens: 12 bits leaves ~1% of symbols
// walking, and the fill is 2^root writes per block whatever the symbol count.
#define LROOT 12
#define DROOT 9
#define CROOT 7

// one canonical code: the counts and symbols the walk needs, and the table over them.
// an entry is (symbol << 4) | length, and 0 -- no code is 0 bits -- means "walk it".
struct inf_code { uint16_t cnt[16], sym[288], *tab; unsigned root; };

// ⚠ FILE SCOPE, not the decoder's frame: 9 KB of table has no business on a stack this
// deep, and the nif runs to completion inside one lvm step, so nothing re-enters it.
static uint16_t inf_ltab[1 << LROOT], inf_dtab[1 << DROOT], inf_ctab[1 << CROOT];
static struct inf_code inf_lit, inf_dst, inf_cl;

static void inf_build(struct inf_code *c, const uint8_t *lens, unsigned nsym,
                      uint16_t *tab, unsigned root) {
 unsigned ofs[16], l, i, code = 0, idx = 0, size = 1u << root;
 memset(c->cnt, 0, sizeof c->cnt);
 memset(c->sym, 0, sizeof c->sym);
 memset(tab, 0, (size_t) size * sizeof *tab);
 for (i = 0; i < nsym; i++) c->cnt[lens[i]]++;
 c->cnt[0] = 0;                                  // a zero length is no code, not a code
 ofs[1] = 0;
 for (l = 1; l < 15; l++) ofs[l + 1] = ofs[l] + c->cnt[l];
 for (i = 0; i < nsym; i++) if (lens[i]) c->sym[ofs[lens[i]]++] = (uint16_t) i;
 c->tab = tab; c->root = root;
 for (l = 1; l < 16; l++) {                      // canonical order, shortest first
  for (i = 0; i < c->cnt[l]; i++, code++) {
   unsigned s = c->sym[idx++], rev = 0, b, j;
   if (l > root) continue;
   for (b = 0; b < l; b++) rev |= ((code >> b) & 1) << (l - 1 - b);   // the code, as read
   for (j = rev; j < size; j += 1u << l)
    if (!tab[j]) tab[j] = (uint16_t) ((s << 4) | l); }
  code <<= 1; } }

// the twin's own decode, for the codes the table does not hold. -1 where gz-dec answers ().
static int inf_walk(const struct inf_code *c, uint64_t bb, unsigned *used) {
 int code = 0, first = 0, index = 0, cnt;
 unsigned l;
 for (l = 1; l < 16; l++) {
  code |= (int) ((bb >> (l - 1)) & 1);
  cnt = c->cnt[l];
  if (code - cnt < first) { *used = l; return c->sym[index + (code - first)]; }
  index += cnt; first = (first + cnt) << 1; code <<= 1; }
 return -1; }

// -1 where the twin answers (), -2 where the output outruns cap. out may be NULL, and
// then nothing is stored and the answer is only how long the stream inflates to --
// which is exact, because NOTHING THE BUFFER HOLDS EVER REACHES A BRANCH.
static int64_t inf_run(const uint8_t *in, uintptr_t n, uint8_t *out, uintptr_t cap) {
 uintptr_t ip = 0, op = 0;
 uint64_t bb = 0;
 unsigned bc = 0, last, typ, i;
 uint8_t lens[320];

// ⚠ ONE UNALIGNED LOAD WHERE THERE IS ROOM. the byte loop below is the same act and
// eight times the work; the arithmetic is libdeflate's -- absorb what fits, step by the
// bytes that wholly landed, and round the count up to 56 or 63. what spilled off the top
// belongs to a byte `ip` has not passed, so it is read again and not lost.
#define FILL() do { \
   if (ip + 8 <= n) { bb |= LD64(in + ip) << bc; ip += 7 - (bc >> 3); bc |= 56; } \
   else while (bc <= 56 && ip < n) { bb |= (uint64_t) in[ip++] << bc; bc += 8; } } while (0)
#define TAKE(k, v) do { unsigned k_ = (k); \
   if (bc < k_) { FILL(); if (bc < k_) return -1; } \
   (v) = (unsigned) (bb & ((1u << k_) - 1)); bb >>= k_; bc -= k_; } while (0)
// a symbol off `c`: the table where it reaches, the walk where it does not
#define SYM(c, t, root, sy) do { unsigned u_; uint16_t e_; \
   FILL(); e_ = (t)[bb & ((1u << (root)) - 1)]; \
   if (e_) { (sy) = e_ >> 4; u_ = e_ & 15; } \
   else { int w_ = inf_walk(&(c), bb, &u_); if (w_ < 0) return -1; (sy) = (unsigned) w_; } \
   if (bc < u_) return -1; \
   bb >>= u_; bc -= u_; } while (0)

 do {
  TAKE(1, last); TAKE(2, typ);
  if (typ == 0) {                                // stored: to the byte, then raw
   uintptr_t pos;
   unsigned ln, nl, drop = bc & 7;
   bb >>= drop; bc -= drop;
   pos = ip - (bc >> 3);                         // what is buffered is whole bytes now
   if (pos + 4 > n) return -1;
   ln = in[pos] | ((unsigned) in[pos + 1] << 8);
   nl = in[pos + 2] | ((unsigned) in[pos + 3] << 8);
   if (((ln ^ 0xffff) & 0xffff) != nl || pos + 4 + ln > n) return -1;
   if (op + ln > cap) return -2;
   if (out) memcpy(out + op, in + pos + 4, ln);
   op += ln; ip = pos + 4 + ln; bb = 0; bc = 0;
   continue; }
  if (typ == 3) return -1;

  if (typ == 1) {                                // the fixed code, RFC 1951 §3.2.6
   for (i = 0; i < 288; i++) lens[i] = i < 144 ? 8 : i < 256 ? 9 : i < 280 ? 7 : 8;
   inf_build(&inf_lit, lens, 288, inf_ltab, LROOT);
   for (i = 0; i < 30; i++) lens[i] = 5;
   inf_build(&inf_dst, lens, 30, inf_dtab, DROOT); }
  else {                                         // the block's own, read through a third
   unsigned hlit, hdist, hclen, tot, prev = 0;
   uint8_t cl[19];
   TAKE(5, hlit); TAKE(5, hdist); TAKE(4, hclen);
   hlit += 257; hdist += 1; hclen += 4;
   memset(cl, 0, sizeof cl);
   for (i = 0; i < hclen; i++) { unsigned v; TAKE(3, v); cl[inf_clord[i]] = (uint8_t) v; }
   inf_build(&inf_cl, cl, 19, inf_ctab, CROOT);
   memset(lens, 0, sizeof lens);
   tot = hlit + hdist;
   // ⚠ A RUN MAY OVERSHOOT `tot` and the twin lets it: it writes into a tablet, which has
   // no end, and stops on the next look. So the write is clamped and the cursor is not.
   for (i = 0; i < tot; ) {
    unsigned sy, r, v, k;
    SYM(inf_cl, inf_ctab, CROOT, sy);
    if (sy < 16)       { r = 1; v = sy; }
    else if (sy == 16) { TAKE(2, r); r += 3; v = prev; }
    else if (sy == 17) { TAKE(3, r); r += 3; v = 0; }
    else if (sy == 18) { TAKE(7, r); r += 11; v = 0; }
    else return -1;
    for (k = 0; k < r; k++) if (i + k < sizeof lens) lens[i + k] = (uint8_t) v;
    i += r; prev = v; }
   inf_build(&inf_lit, lens, hlit, inf_ltab, LROOT);
   inf_build(&inf_dst, lens + hlit, hdist, inf_dtab, DROOT); }

  for (;;) {                                     // the symbol loop, fixed or dynamic
   unsigned sy, l, d, x;
   SYM(inf_lit, inf_ltab, LROOT, sy);
   if (sy < 256) {
    if (op >= cap) return -2;
    if (out) out[op] = (uint8_t) sy;
    op++; continue; }
   if (sy == 256) break;
   if (sy > 285) return -1;
   sy -= 257;
   l = inf_lbase[sy];
   if (inf_lext[sy]) { TAKE(inf_lext[sy], x); l += x; }
   SYM(inf_dst, inf_dtab, DROOT, sy);
   if (sy > 29) return -1;
   d = inf_dbase[sy];
   if (inf_dext[sy]) { TAKE(inf_dext[sy], x); d += x; }
   if (d > op) return -1;                        // a reach before the start
   if (op + l > cap) return -2;
   if (out) {
    // ⚠ EIGHT IN ORDER, AND THAT IS NOT A WORD MOVE. deflate lets a run overlap its own
    // source -- dist 1 len 100 is a hundred of one byte -- and written out in sequence
    // these read each byte back as they go, so they are the byte loop with its counter
    // gone and are right at every distance, no guard to get wrong. A real word move,
    // guarded at eight, was measured and is SLOWER IN BOTH LANES (gcc 22 ms against 20,
    // mooncc 39 against 37): mean match here is 8.5 bytes, one word and a tail, and the
    // branch to choose costs what the wide store saves. The plain loop is slower again
    // (mooncc 42), which is the counter and nothing else.
    uint8_t *dp = out + op, *sp = dp - d;
    unsigned k = 0;
    for (; k + 8 <= l; k += 8) {
     dp[k]     = sp[k];     dp[k + 1] = sp[k + 1]; dp[k + 2] = sp[k + 2];
     dp[k + 3] = sp[k + 3]; dp[k + 4] = sp[k + 4]; dp[k + 5] = sp[k + 5];
     dp[k + 6] = sp[k + 6]; dp[k + 7] = sp[k + 7]; }
    for (; k < l; k++) dp[k] = sp[k]; }
   op += l; }
 } while (!last);
 return (int64_t) op;
#undef FILL
#undef TAKE
#undef SYM
}

// ⚠ str0 collects, so the stream is re-read off the stack after it: a C local's pointer
// into the heap is stale across the bump. tls.c pays the same toll.
ai_noinline static struct ai *host_inflate(struct ai *g) {
 ai_word sw = g->sp[0], nw = g->sp[1];
 intptr_t hint;
 int64_t want;
 int guessed;
 if (!ai_strp(sw) || !oddp(nw)) { g->sp[1] = ZeroPoint, g->sp += 1; return g; }
 hint = getcharm(nw);
 guessed = hint > 0;
 want = guessed ? (int64_t) hint
                : inf_run((const uint8_t*) txt(sw), len(sw), 0, (uintptr_t) -1);
 for (;;) {
  if (want < 0) break;
  if (!ai_ok(g = str0(g, (uintptr_t) want))) return g;
  if (!want) { g->sp[2] = g->sp[0], g->sp += 2; return g; }   // the counting pass read it
  { ai_word s2 = g->sp[1];
    int64_t got = inf_run((const uint8_t*) txt(s2), len(s2),
                          (uint8_t*) txt(g->sp[0]), (uintptr_t) want);
    if (got == want) { g->sp[2] = g->sp[0], g->sp += 2; return g; } }
  g->sp += 1;                                    // the wrong-sized string, dropped
  if (!guessed) break;
  guessed = 0;                                   // the hint lied: count, then once more
  want = inf_run((const uint8_t*) txt(g->sp[0]), len(g->sp[0]), 0, (uintptr_t) -1); }
 g->sp[1] = ZeroPoint, g->sp += 1;
 return g; }

static lvm(lvm_inflate) {
 Pack(g); g = host_inflate(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

static union u const nif_inflate[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_inflate}, {lvm_ret0}};
AiNif("inflate", nif_inflate);
