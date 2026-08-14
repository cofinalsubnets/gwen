// host/hash.c -- content addressing for svalbard (crew/sb/): sha-256 over a
// string's bytes. Host-only, auto-globbed + AI_NIF-registered (no love.c/love.h/
// main.c edit), the fs.c discipline:
//
//   (sha256 str) -> the 64-char lowercase hex digest | () misuse
//
// FIPS 180-4, the compact single-pass shape; a value op, so absence/misuse
// answers ().
#include "love.h"
#include <stdint.h>
#include <string.h>

static const uint32_t K[64] = {
 0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
 0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
 0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
 0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
 0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
 0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
 0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
 0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2};

static uint32_t rr(uint32_t x, int n) { return (x >> n) | (x << (32 - n)); }

static void sha_block(uint32_t h[8], const uint8_t *p) {
 uint32_t w[64];
 for (int i = 0; i < 16; i++)
  w[i] = (uint32_t) p[4*i] << 24 | (uint32_t) p[4*i+1] << 16
       | (uint32_t) p[4*i+2] << 8 | (uint32_t) p[4*i+3];
 for (int i = 16; i < 64; i++) {
  uint32_t s0 = rr(w[i - 15], 7) ^ rr(w[i - 15], 18) ^ (w[i - 15] >> 3);
  uint32_t s1 = rr(w[i - 2], 17) ^ rr(w[i - 2], 19) ^ (w[i - 2] >> 10);
  w[i] = w[i - 16] + s0 + w[i - 7] + s1; }
 uint32_t a = h[0], b = h[1], c = h[2], d = h[3],
          e = h[4], f = h[5], gg = h[6], hh = h[7];
 for (int i = 0; i < 64; i++) {
  uint32_t s1 = rr(e, 6) ^ rr(e, 11) ^ rr(e, 25);
  uint32_t ch = (e & f) ^ (~e & gg);
  uint32_t t1 = hh + s1 + ch + K[i] + w[i];
  uint32_t s0 = rr(a, 2) ^ rr(a, 13) ^ rr(a, 22);
  uint32_t mj = (a & b) ^ (a & c) ^ (b & c);
  uint32_t t2 = s0 + mj;
  hh = gg; gg = f; f = e; e = d + t1;
  d = c; c = b; b = a; a = t1 + t2; }
 h[0] += a; h[1] += b; h[2] += c; h[3] += d;
 h[4] += e; h[5] += f; h[6] += gg; h[7] += hh; }

static void sha256_hex(const uint8_t *msg, size_t len, char out[65]) {
 uint32_t h[8] = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
                  0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19};
 size_t i = 0;
 for (; i + 64 <= len; i += 64) sha_block(h, msg + i);
 uint8_t tail[128];
 size_t r = len - i;
 memcpy(tail, msg + i, r);
 tail[r++] = 0x80;
 size_t pad = (r <= 56) ? 64 : 128;
 memset(tail + r, 0, pad - 8 - r);
 uint64_t bits = (uint64_t) len << 3;
 for (int k = 0; k < 8; k++) tail[pad - 1 - k] = (uint8_t) (bits >> (8 * k));
 sha_block(h, tail);
 if (pad == 128) sha_block(h, tail + 64);
 static const char hx[] = "0123456789abcdef";
 for (int k = 0; k < 8; k++)
  for (int j = 0; j < 4; j++) {
  uint8_t b = (uint8_t) (h[k] >> (24 - 8 * j));
  out[8 * k + 2 * j] = hx[b >> 4];
  out[8 * k + 2 * j + 1] = hx[b & 15]; }
 out[64] = 0; }

ai_noinline static struct ai *host_sha256(struct ai *g) {
 if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
 struct ai_str *s = (struct ai_str*) g->sp[0];
 char hex[65];
 sha256_hex((const uint8_t*) s->bytes, (size_t) s->len, hex);
 if (!ai_ok(g = ai_strof(g, hex))) return g;                  // pushes: digest over arg
 g->sp[1] = g->sp[0];
 g->sp += 1;
 return g; }
static lvm(lvm_sha256) {
 Pack(g); g = host_sha256(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 ai_musttail return Next(1); }

static union u const nif_sha256[] = {{lvm_sha256}, {lvm_ret0}};
AI_NIF("sha256", nif_sha256);
