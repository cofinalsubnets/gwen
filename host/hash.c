// host/hash.c -- a digest over a string's bytes. Host-only, auto-globbed + AiNif-
// registered (no love.c/love.h/main.c edit), the fs.c discipline:
//
//   (sha256 str) -> the 64-char lowercase hex digest | () misuse
//   (sha256-init b) / (sha256-feed b str) / (sha256-done b)  -- the same digest over a
//                   STREAM, the state carried in a 105-byte cask; see below
//   (md5 str)    -> the 32-char lowercase hex digest | () misuse
//   (crc32 str)  -> the IEEE crc32, a charm          | () misuse
//   (cksum str)  -> POSIX cksum's crc, length folded in, a charm | () misuse
//
// FIPS 180-4, RFC 1321, IEEE 802.3 and POSIX cksum, all the compact single-pass
// shape; value ops, so absence/misuse answers (). crew/kore's cksum, md5sum and
// sha256sum applets are these four plus a line of output.
//
// ⚠ THEY ARE NOT ALL IN THE SAME POSITION and it is worth knowing which is which.
// crc32 shadows lib/gz.l's gz-crcwalk and cksum test/host/hash.l's hash-ckwalk: both
// polynomials are STATED in love, and test/host/{gzc,hash}.l hold the C to the walk at
// every length, so a disagreement there has a right answer. sha256 and md5 shadow
// NOTHING -- so crew/sb's blob and patch ids and crew/moon's cache key rest on this
// file, and what holds those two honest is the published vectors in test/host/hash.l
// and GNU coreutils in test/gate/kore.sh. That is a thinner rope than the rest of
// host/ hangs from, and the fix is a love sha-256, not another vector.
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
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// --- the same digest, resumable ----------------------------------------------------
// the one-shot above wants its whole message contiguous, and for an archive that is
// megabytes existing only to be hashed once. these three carry the state in a CASK
// instead, so a caller feeds it a bufferful at a time and holds nothing: the state is
// h[8], the running byte count, and the sub-block remainder a feed could not consume.
// the block loop is sha_block above, untouched -- one spelling of the compression
// function, two ways in, so the streamed digest cannot drift from the one-shot.
//
// THE CASK IS 105 BYTES and its layout is this file's; love allocates it and carries
// it, never reads it. big-endian throughout, like the digest, so the state is bytes
// and not this machine's words -- it can be written down, and an image that carries
// one wakes on any box.
//
//   0..31   h[8], big-endian
//   32..39  the byte count so far, big-endian
//   40      the remainder length, 0..63
//   41..104 the remainder itself
#define ShaSt 105
#define ShaRem 40
#define ShaBuf 41

static struct ai_str *sha_cask(ai_word x) {                   // the cask's bytes, or NULL
 if ((x & 1) || ((union u*) x)->ap != lvm_cask) return NULL;
 struct ai_str *s = ((struct ai_cask*) x)->str;
 return s && s->len == ShaSt ? s : NULL; }

static void sha_ld(const uint8_t *st, uint32_t h[8], uint64_t *len) {
 for (int k = 0; k < 8; k++)
  h[k] = (uint32_t) st[4*k] << 24 | (uint32_t) st[4*k+1] << 16
       | (uint32_t) st[4*k+2] << 8 | (uint32_t) st[4*k+3];
 uint64_t n = 0;
 for (int k = 0; k < 8; k++) n = n << 8 | st[32 + k];
 *len = n; }

static void sha_st(uint8_t *st, const uint32_t h[8], uint64_t len) {
 for (int k = 0; k < 8; k++) {
  st[4*k]   = (uint8_t) (h[k] >> 24); st[4*k+1] = (uint8_t) (h[k] >> 16);
  st[4*k+2] = (uint8_t) (h[k] >> 8);  st[4*k+3] = (uint8_t)  h[k]; }
 for (int k = 0; k < 8; k++) st[32 + k] = (uint8_t) (len >> (56 - 8*k)); }

// (sha256-init b) -> b, a 105-byte cask carrying FIPS 180-4's initial state and
// nothing fed | () on anything that is not such a cask.
ai_noinline static ai_word host_sha_init(ai_word x) {
 struct ai_str *s = sha_cask(x);
 if (!s) return ZeroPoint;
 static const uint32_t h0[8] = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
                                0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19};
 uint8_t *st = (uint8_t*) s->bytes;
 memset(st, 0, ShaSt);
 sha_st(st, h0, 0);
 return x; }
static lvm(lvm_sha_init) { Sp[0] = host_sha_init(Sp[0]); ai_musttail return Next(1); }

// (sha256-feed b str) -> b, str's bytes folded in | (). any chunk size: what does not
// fill a block stays in the remainder and rides to the next feed, which is the whole
// point -- a caller reads by the bufferful and never has to think in 64s.
ai_noinline static ai_word host_sha_feed(ai_word x, ai_word a) {
 struct ai_str *cs = sha_cask(x);
 if (!cs || !ai_strp(a)) return ZeroPoint;
 struct ai_str *in = (struct ai_str*) a;
 uint8_t *st = (uint8_t*) cs->bytes;
 uint32_t h[8];
 uint64_t len;
 sha_ld(st, h, &len);
 unsigned rem = st[ShaRem];
 const uint8_t *p = (const uint8_t*) in->bytes;
 uintptr_t n = in->len;
 len += n;
 if (rem) {                                                  // top the remainder up first
  unsigned want = 64 - rem;
  if (n < want) { memcpy(st + ShaBuf + rem, p, n); st[ShaRem] = (uint8_t) (rem + n);
                  sha_st(st, h, len); return x; }
  memcpy(st + ShaBuf + rem, p, want);
  sha_block(h, st + ShaBuf);
  p += want; n -= want; rem = 0; }
 for (; n >= 64; p += 64, n -= 64) sha_block(h, p);
 if (n) memcpy(st + ShaBuf, p, n);
 st[ShaRem] = (uint8_t) n;
 sha_st(st, h, len);
 return x; }
static lvm(lvm_sha_feed) {
 Sp[1] = host_sha_feed(Sp[0], Sp[1]); Sp += 1; ai_musttail return Next(1); }

// (sha256-done b) -> the 64-char lowercase hex digest | (). the pad is the one-shot's,
// over the remainder rather than the message tail; b is left spent, not reusable.
ai_noinline static struct ai *host_sha_done(struct ai *g) {
 struct ai_str *cs = sha_cask(g->sp[0]);
 if (!cs) return g->sp[0] = ZeroPoint, g;
 uint8_t *st = (uint8_t*) cs->bytes;
 uint32_t h[8];
 uint64_t len;
 sha_ld(st, h, &len);
 unsigned r = st[ShaRem];
 uint8_t tail[128];
 memcpy(tail, st + ShaBuf, r);
 tail[r++] = 0x80;
 size_t pad = (r <= 56) ? 64 : 128;
 memset(tail + r, 0, pad - 8 - r);
 uint64_t bits = len << 3;
 for (int k = 0; k < 8; k++) tail[pad - 1 - k] = (uint8_t) (bits >> (8 * k));
 sha_block(h, tail);
 if (pad == 128) sha_block(h, tail + 64);
 char hex[65];
 static const char hx[] = "0123456789abcdef";
 for (int k = 0; k < 8; k++)
  for (int j = 0; j < 4; j++) {
   uint8_t b = (uint8_t) (h[k] >> (24 - 8 * j));
   hex[8*k + 2*j] = hx[b >> 4];
   hex[8*k + 2*j + 1] = hx[b & 15]; }
 hex[64] = 0;
 if (!ai_ok(g = ai_strof(g, hex))) return g;                  // pushes: digest over arg
 g->sp[1] = g->sp[0];
 g->sp += 1;
 return g; }
static lvm(lvm_sha_done) {
 Pack(g); g = host_sha_done(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// --- md5 (RFC 1321) ---------------------------------------------------------------
// the same shape as sha256 above with the endianness turned around: md5 loads its
// words and lays its length LITTLE-endian, where sha-256 does both big.
static const uint32_t MK[64] = {
 0xd76aa478,0xe8c7b756,0x242070db,0xc1bdceee,0xf57c0faf,0x4787c62a,0xa8304613,0xfd469501,
 0x698098d8,0x8b44f7af,0xffff5bb1,0x895cd7be,0x6b901122,0xfd987193,0xa679438e,0x49b40821,
 0xf61e2562,0xc040b340,0x265e5a51,0xe9b6c7aa,0xd62f105d,0x02441453,0xd8a1e681,0xe7d3fbc8,
 0x21e1cde6,0xc33707d6,0xf4d50d87,0x455a14ed,0xa9e3e905,0xfcefa3f8,0x676f02d9,0x8d2a4c8a,
 0xfffa3942,0x8771f681,0x6d9d6122,0xfde5380c,0xa4beea44,0x4bdecfa9,0xf6bb4b60,0xbebfbc70,
 0x289b7ec6,0xeaa127fa,0xd4ef3085,0x04881d05,0xd9d4d039,0xe6db99e5,0x1fa27cf8,0xc4ac5665,
 0xf4292244,0x432aff97,0xab9423a7,0xfc93a039,0x655b59c3,0x8f0ccc92,0xffeff47d,0x85845dd1,
 0x6fa87e4f,0xfe2ce6e0,0xa3014314,0x4e0811a1,0xf7537e82,0xbd3af235,0x2ad7d2bb,0xeb86d391};
static const uint8_t MS[64] = {
 7,12,17,22, 7,12,17,22, 7,12,17,22, 7,12,17,22,
 5, 9,14,20, 5, 9,14,20, 5, 9,14,20, 5, 9,14,20,
 4,11,16,23, 4,11,16,23, 4,11,16,23, 4,11,16,23,
 6,10,15,21, 6,10,15,21, 6,10,15,21, 6,10,15,21};

static uint32_t rl(uint32_t x, int n) { return (x << n) | (x >> (32 - n)); }

static void md5_block(uint32_t h[4], const uint8_t *p) {
 uint32_t m[16], a = h[0], b = h[1], c = h[2], d = h[3];
 for (int i = 0; i < 16; i++)
  m[i] = (uint32_t) p[4*i] | (uint32_t) p[4*i+1] << 8
       | (uint32_t) p[4*i+2] << 16 | (uint32_t) p[4*i+3] << 24;
 for (int i = 0; i < 64; i++) {
  uint32_t f; int g;
  if (i < 16)      { f = (b & c) | (~b & d); g = i; }
  else if (i < 32) { f = (d & b) | (~d & c); g = (5*i + 1) & 15; }
  else if (i < 48) { f = b ^ c ^ d;          g = (3*i + 5) & 15; }
  else             { f = c ^ (b | ~d);       g = (7*i) & 15; }
  f += a + MK[i] + m[g];
  a = d; d = c; c = b; b += rl(f, MS[i]); }
 h[0] += a; h[1] += b; h[2] += c; h[3] += d; }

static void md5_hex(const uint8_t *msg, size_t len, char out[33]) {
 uint32_t h[4] = {0x67452301, 0xefcdab89, 0x98badcfe, 0x10325476};
 size_t i = 0;
 for (; i + 64 <= len; i += 64) md5_block(h, msg + i);
 uint8_t tail[128];
 size_t r = len - i;
 memcpy(tail, msg + i, r);
 tail[r++] = 0x80;
 size_t pad = (r <= 56) ? 64 : 128;
 memset(tail + r, 0, pad - 8 - r);
 uint64_t bits = (uint64_t) len << 3;
 for (int k = 0; k < 8; k++) tail[pad - 8 + k] = (uint8_t) (bits >> (8 * k));
 md5_block(h, tail);
 if (pad == 128) md5_block(h, tail + 64);
 static const char hx[] = "0123456789abcdef";
 for (int k = 0; k < 4; k++)
  for (int j = 0; j < 4; j++) {
  uint8_t b = (uint8_t) (h[k] >> (8 * j));
  out[8 * k + 2 * j] = hx[b >> 4];
  out[8 * k + 2 * j + 1] = hx[b & 15]; }
 out[32] = 0; }

ai_noinline static struct ai *host_md5(struct ai *g) {
 if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
 struct ai_str *s = (struct ai_str*) g->sp[0];
 char hex[33];
 md5_hex((const uint8_t*) s->bytes, (size_t) s->len, hex);
 if (!ai_ok(g = ai_strof(g, hex))) return g;                  // pushes: digest over arg
 g->sp[1] = g->sp[0];
 g->sp += 1;
 return g; }
static lvm(lvm_md5) {
 Pack(g); g = host_md5(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// --- crc32 (IEEE 802.3: reflected, polynomial 0xedb88320) -------------------------
// EIGHT BYTES AT A TIME, and that is the whole difference: the byte-at-a-time walk
// lib/gz.l spells is a dependency chain one link per byte, where slicing spends eight
// INDEPENDENT lookups and lets the machine overlap them. gz.l cannot do this -- eight
// tray reads per byte would cost eight times what one does.
// ⚠ the tables are built on the first call rather than laid in .rodata: 2048 entries
// off a one-line recurrence, and nothing for a reader to check against the polynomial.
static uint32_t crc_t[8][256];
static int crc_ready;

static void crc_init(void) {
 unsigned i, k;
 for (i = 0; i < 256; i++) {
  uint32_t c = i;
  for (k = 0; k < 8; k++) c = (c & 1) ? (c >> 1) ^ 0xedb88320 : c >> 1;
  crc_t[0][i] = c; }
 for (i = 0; i < 256; i++) {                    // table k is table 0 shifted k bytes on
  uint32_t c = crc_t[0][i];
  for (k = 1; k < 8; k++) { c = crc_t[0][c & 0xff] ^ (c >> 8); crc_t[k][i] = c; } }
 crc_ready = 1; }

#define LD32(p) ((uint32_t) (p)[0] | (uint32_t) (p)[1] << 8 \
               | (uint32_t) (p)[2] << 16 | (uint32_t) (p)[3] << 24)

static uint32_t crc32_of(const uint8_t *p, uintptr_t n) {
 uint32_t c = 0xffffffff;
 if (!crc_ready) crc_init();
 for (; n >= 8; p += 8, n -= 8) {
  uint32_t a = c ^ LD32(p), b = LD32(p + 4);
  c = crc_t[7][a & 0xff] ^ crc_t[6][(a >> 8) & 0xff]
    ^ crc_t[5][(a >> 16) & 0xff] ^ crc_t[4][a >> 24]
    ^ crc_t[3][b & 0xff] ^ crc_t[2][(b >> 8) & 0xff]
    ^ crc_t[1][(b >> 16) & 0xff] ^ crc_t[0][b >> 24]; }
 for (; n; p++, n--) c = crc_t[0][(c ^ *p) & 0xff] ^ (c >> 8);
 return c ^ 0xffffffff; }

ai_noinline static struct ai *host_crc32(struct ai *g) {
 if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
 { struct ai_str *s = (struct ai_str*) g->sp[0];
   g->sp[0] = putcharm(crc32_of((const uint8_t*) s->bytes, (uintptr_t) s->len)); }
 return g; }
static lvm(lvm_crc32) {
 Pack(g); g = host_crc32(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

// --- cksum (POSIX: NOT reflected, polynomial 0x04c11db7, the LENGTH folded in) -----
// ⚠ a different crc from the one above in every part: the register runs the other way,
// the seed is 0, and the message does not end at the last byte -- the byte count goes
// through the same walk, low byte first, which is what makes cksum answer 4294967295
// for the empty file rather than 0. one bit at a time, since the table it would want
// is not the one crc32 built.
static uint32_t ck_byte(uint32_t c, uint8_t b) {
 c ^= (uint32_t) b << 24;
 for (int k = 0; k < 8; k++) c = (c & 0x80000000u) ? (c << 1) ^ 0x04c11db7u : c << 1;
 return c; }

static uint32_t cksum_of(const uint8_t *p, uintptr_t n) {
 uint32_t c = 0;
 uintptr_t len = n;
 for (uintptr_t i = 0; i < n; i++) c = ck_byte(c, p[i]);
 for (; len; len >>= 8) c = ck_byte(c, (uint8_t) (len & 0xff));
 return ~c; }

ai_noinline static struct ai *host_cksum(struct ai *g) {
 if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
 { struct ai_str *s = (struct ai_str*) g->sp[0];
   g->sp[0] = putcharm(cksum_of((const uint8_t*) s->bytes, (uintptr_t) s->len)); }
 return g; }
static lvm(lvm_cksum) {
 Pack(g); g = host_cksum(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 Unpack(g);
 ai_musttail return Next(1); }

static union u const nif_sha256[] = {{lvm_sha256}, {lvm_ret0}},
                    nif_sha_init[] = {{lvm_sha_init}, {lvm_ret0}},
                    nif_sha_feed[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_sha_feed}, {lvm_ret0}},
                    nif_sha_done[] = {{lvm_sha_done}, {lvm_ret0}},
                    nif_md5[]    = {{lvm_md5},    {lvm_ret0}},
                    nif_crc32[]  = {{lvm_crc32},  {lvm_ret0}},
                    nif_cksum[]  = {{lvm_cksum},  {lvm_ret0}};
AiNif("sha256", nif_sha256);
AiNif("sha256-init", nif_sha_init);
AiNif("sha256-feed", nif_sha_feed);
AiNif("sha256-done", nif_sha_done);
AiNif("md5", nif_md5);
AiNif("crc32", nif_crc32);
AiNif("cksum", nif_cksum);
