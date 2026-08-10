// host/tls.c -- the C twins of crew/tls's two ciphers, auto-globbed + AI_NIF-
// registered (no love.c/love.h/main.c edit), the hash.c discipline:
//
//   (chacha20 key nonce ctr txt) -> a string as long as txt   | () misuse
//   (poly1305 key msg)           -> the 16-byte tag           | () misuse
//
// these are TWINS, not replacements: crew/tls/chacha.l and crew/tls/poly1305.l
// stay the readable statement of each cipher and the differential oracle
// (test/host/tlsc.l asserts the two agree byte-for-byte on the RFC's vectors and
// on every length around a block edge). value ops, so misuse answers ().
//
// ⚠ the ALGORITHM is the love file's, deliberately: poly1305 keeps the five
// 26-bit limbs rather than reaching for __int128, so what the timing compares is
// the two languages running one algorithm, not two algorithms. chacha is the one
// place they differ in SHAPE and cannot not: love vectorises across blocks
// because its per-op cost dominates, C walks one block at a time.
// crew/tls/bench.l times both, and says whose binary the number belongs to.
#include "love.h"
#include <stdint.h>
#include <string.h>

static uint32_t le32(const uint8_t *p) {
 return (uint32_t) p[0] | (uint32_t) p[1] << 8
      | (uint32_t) p[2] << 16 | (uint32_t) p[3] << 24; }

// --- chacha20 (rfc 8439 §2.3) -----------------------------------------------------
#define ROTL(x, n) (((x) << (n)) | ((x) >> (32 - (n))))
#define QR(a, b, c, d) \
 (a += b, d ^= a, d = ROTL(d, 16), c += d, b ^= c, b = ROTL(b, 12), \
  a += b, d ^= a, d = ROTL(d, 8),  c += d, b ^= c, b = ROTL(b, 7))

// one 20-round block, keystream out little-endian.
static void cc_block(const uint32_t in[16], uint8_t out[64]) {
 uint32_t x[16];
 memcpy(x, in, sizeof x);
 for (int i = 0; i < 10; i++) {
  QR(x[0], x[4], x[8],  x[12]); QR(x[1], x[5], x[9],  x[13]);
  QR(x[2], x[6], x[10], x[14]); QR(x[3], x[7], x[11], x[15]);
  QR(x[0], x[5], x[10], x[15]); QR(x[1], x[6], x[11], x[12]);
  QR(x[2], x[7], x[8],  x[13]); QR(x[3], x[4], x[9],  x[14]); }
 for (int i = 0; i < 16; i++) {
  uint32_t v = x[i] + in[i];
  out[4*i] = (uint8_t) v;         out[4*i+1] = (uint8_t) (v >> 8);
  out[4*i+2] = (uint8_t) (v >> 16); out[4*i+3] = (uint8_t) (v >> 24); } }

// xor txt with the keystream from block ctr on. encrypt and decrypt are one act.
static void cc_xor(const uint8_t *key, const uint8_t *nonce, uint32_t ctr,
                   const uint8_t *txt, uint8_t *out, uintptr_t n) {
 uint32_t st[16] = {0x61707865, 0x3320646e, 0x79622d32, 0x6b206574};
 uint8_t ks[64];
 for (int i = 0; i < 8; i++)  st[4 + i] = le32(key + 4*i);
 for (int i = 0; i < 3; i++)  st[13 + i] = le32(nonce + 4*i);
 for (uintptr_t o = 0; o < n; o += 64) {
  uintptr_t r = n - o < 64 ? n - o : 64;
  st[12] = ctr + (uint32_t) (o / 64);
  cc_block(st, ks);
  for (uintptr_t j = 0; j < r; j++) out[o + j] = txt[o + j] ^ ks[j]; } }

// --- poly1305 (rfc 8439 §2.5), five 26-bit limbs ----------------------------------
#define M26 0x3ffffff

// r, clamped by the masks folded into the split (§2.5.1).
static void po_r(const uint8_t *k, uint64_t r[5]) {
 uint32_t t0 = le32(k), t1 = le32(k + 4), t2 = le32(k + 8), t3 = le32(k + 12);
 r[0] = t0 & M26;
 r[1] = ((t0 >> 26) | (t1 << 6))  & 0x3ffff03;
 r[2] = ((t1 >> 20) | (t2 << 12)) & 0x3ffc0ff;
 r[3] = ((t2 >> 14) | (t3 << 18)) & 0x3f03fff;
 r[4] = (t3 >> 8) & 0x00fffff; }

// h += the 16 bytes at p; hi is the 2^128 term (2^24 for a full block, 0 for the
// padded final one, whose 0x01 already sits inside the sixteen).
static void po_absorb(uint64_t h[5], const uint8_t *p, uint64_t hi) {
 uint32_t t0 = le32(p), t1 = le32(p + 4), t2 = le32(p + 8), t3 = le32(p + 12);
 h[0] += t0 & M26;
 h[1] += ((t0 >> 26) | ((uint64_t) t1 << 6))  & M26;
 h[2] += ((t1 >> 20) | ((uint64_t) t2 << 12)) & M26;
 h[3] += ((t2 >> 14) | ((uint64_t) t3 << 18)) & M26;
 h[4] += (t3 >> 8) | hi; }

// h = h * r mod 2^130 - 5. the modulus never appears: what runs past 2^130 comes
// back at the bottom times five, which is what the s1..s4 terms are.
static void po_mul(uint64_t h[5], const uint64_t r[5]) {
 uint64_t h0 = h[0], h1 = h[1], h2 = h[2], h3 = h[3], h4 = h[4];
 uint64_t s1 = r[1] * 5, s2 = r[2] * 5, s3 = r[3] * 5, s4 = r[4] * 5;
 uint64_t d0 = h0*r[0] + h1*s4   + h2*s3   + h3*s2   + h4*s1;
 uint64_t d1 = h0*r[1] + h1*r[0] + h2*s4   + h3*s3   + h4*s2;
 uint64_t d2 = h0*r[2] + h1*r[1] + h2*r[0] + h3*s4   + h4*s3;
 uint64_t d3 = h0*r[3] + h1*r[2] + h2*r[1] + h3*r[0] + h4*s4;
 uint64_t d4 = h0*r[4] + h1*r[3] + h2*r[2] + h3*r[1] + h4*r[0];
 uint64_t c;
 c = d0 >> 26; h[0] = d0 & M26;
 d1 += c; c = d1 >> 26; h[1] = d1 & M26;
 d2 += c; c = d2 >> 26; h[2] = d2 & M26;
 d3 += c; c = d3 >> 26; h[3] = d3 & M26;
 d4 += c; c = d4 >> 26; h[4] = d4 & M26;
 h[0] += c * 5; c = h[0] >> 26; h[0] &= M26; h[1] += c; }

// ⚠ the conditional subtract is a MASK, never an if: both h and h-p are always
// computed and one is selected, so nothing branches on the accumulator.
static void po_fin(const uint64_t h[5], const uint8_t *key, uint8_t out[16]) {
 int64_t h0 = (int64_t) h[0], h1 = (int64_t) h[1], h2 = (int64_t) h[2],
         h3 = (int64_t) h[3], h4 = (int64_t) h[4];
 int64_t c1 = h1 >> 26,       i1 = h1 & M26;
 int64_t a2 = h2 + c1, c2 = a2 >> 26, i2 = a2 & M26;
 int64_t a3 = h3 + c2, c3 = a3 >> 26, i3 = a3 & M26;
 int64_t a4 = h4 + c3, c4 = a4 >> 26, i4 = a4 & M26;
 int64_t a0 = h0 + c4 * 5, c0 = a0 >> 26, i0 = a0 & M26;
 int64_t j1 = i1 + c0;
 // g = h - p, as h + 5 - 2^130: the top limb goes negative exactly when h < p
 int64_t b0 = i0 + 5,  k0 = b0 >> 26, n0 = b0 & M26;
 int64_t b1 = j1 + k0, k1 = b1 >> 26, n1 = b1 & M26;
 int64_t b2 = i2 + k1, k2 = b2 >> 26, n2 = b2 & M26;
 int64_t b3 = i3 + k2, k3 = b3 >> 26, n3 = b3 & M26;
 int64_t n4 = (i4 + k3) - 0x4000000;
 int64_t keep = n4 >> 63, drop = ~keep;
 int64_t p0 = (i0 & keep) | (n0 & drop), p1 = (j1 & keep) | (n1 & drop),
         p2 = (i2 & keep) | (n2 & drop), p3 = (i3 & keep) | (n3 & drop),
         p4 = (i4 & keep) | (n4 & drop);
 uint64_t w[4], f = 0;
 w[0] = (uint64_t) (p0 | (p1 << 26)) & 0xffffffff;
 w[1] = (uint64_t) ((p1 >> 6)  | (p2 << 20)) & 0xffffffff;
 w[2] = (uint64_t) ((p2 >> 12) | (p3 << 14)) & 0xffffffff;
 w[3] = (uint64_t) ((p3 >> 18) | (p4 << 8))  & 0xffffffff;
 for (int i = 0; i < 4; i++) {
  f = w[i] + le32(key + 16 + 4*i) + (f >> 32);
  out[4*i] = (uint8_t) f;           out[4*i+1] = (uint8_t) (f >> 8);
  out[4*i+2] = (uint8_t) (f >> 16); out[4*i+3] = (uint8_t) (f >> 24); } }

static void po_mac(const uint8_t *key, const uint8_t *msg, uintptr_t n,
                   uint8_t out[16]) {
 uint64_t r[5], h[5] = {0, 0, 0, 0, 0};
 uintptr_t i = 0;
 po_r(key, r);
 for (; i + 16 <= n; i += 16) { po_absorb(h, msg + i, 0x1000000); po_mul(h, r); }
 if (i < n) {
  uint8_t pad[16];
  memcpy(pad, msg + i, n - i);
  pad[n - i] = 1;
  memset(pad + (n - i) + 1, 0, 16 - (n - i) - 1);
  po_absorb(h, pad, 0); po_mul(h, r); }
 po_fin(h, key, out); }

// --- the two nifs -----------------------------------------------------------------
// ⚠ str0 can collect, so the result is allocated FIRST and the arguments re-read
// off the stack after it: the pointers a C local held are stale across the bump.
ai_noinline static struct ai *host_chacha20(struct ai *g) {
 ai_word kw = g->sp[0], nw = g->sp[1], cw = g->sp[2], tw = g->sp[3];
 if (!ai_strp(kw) || !ai_strp(nw) || !ai_strp(tw) || !oddp(cw)
     || len(kw) != 32 || len(nw) != 12 || getcharm(cw) < 0) {
  g->sp[3] = ZeroPoint, g->sp += 3; return g; }
 uintptr_t n = len(tw);
 uint32_t ctr = (uint32_t) getcharm(cw);
 if (!ai_ok(g = str0(g, n))) return g;             // pushes: out over the four args
 cc_xor((const uint8_t*) txt(g->sp[1]), (const uint8_t*) txt(g->sp[2]), ctr,
        (const uint8_t*) txt(g->sp[4]), (uint8_t*) txt(g->sp[0]), n);
 g->sp[4] = g->sp[0], g->sp += 4;
 return g; }
static lvm(lvm_chacha20) {
 Pack(g); g = host_chacha20(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 ai_musttail return Next(1); }

ai_noinline static struct ai *host_poly1305(struct ai *g) {
 ai_word kw = g->sp[0], mw = g->sp[1];
 if (!ai_strp(kw) || !ai_strp(mw) || len(kw) != 32) {
  g->sp[1] = ZeroPoint, g->sp += 1; return g; }
 uintptr_t n = len(mw);
 uint8_t tag[16];
 po_mac((const uint8_t*) txt(kw), (const uint8_t*) txt(mw), n, tag);
 if (!ai_ok(g = str0(g, 16))) return g;            // pushes: tag over the two args
 memcpy(txt(g->sp[0]), tag, 16);
 g->sp[2] = g->sp[0], g->sp += 2;
 return g; }
static lvm(lvm_poly1305) {
 Pack(g); g = host_poly1305(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 ai_musttail return Next(1); }

static union u const
  nif_chacha20[] = {{lvm_cur}, {.x = putcharm(4)}, {lvm_chacha20}, {lvm_ret0}},
  nif_poly1305[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_poly1305}, {lvm_ret0}};
AI_NIF("chacha20", nif_chacha20);
AI_NIF("poly1305", nif_poly1305);
