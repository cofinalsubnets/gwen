// snap.c -- the heap-image snapshot. one translation unit of the runtime;
// the shared layouts and the cross-TU seam are src/love_int.h.
#include "love_int.h"
struct ai_chain; struct ai_image_guard; struct hc; struct image_hdr; struct img_ord;
// this file's own, forward-declared so order within it does not matter.
static ai_noinline intptr_t img_decode_cold(intptr_t v, char *code);
static int img_lt_pair(struct img_ord const *o, uintptr_t i, uintptr_t j);
static int img_lt_rank(struct img_ord const *o, uintptr_t i, uintptr_t j);
static int img_lt_word(struct img_ord const *o, uintptr_t i, uintptr_t j);
static int img_nom_before(word a, word b);
static int img_tok(word const *key, unsigned char const *tk, word v);
static int img_wxp(struct img_ctx *x, word v);
static intptr_t image_ap_index(intptr_t ap);
static intptr_t image_ap_resolve(intptr_t idx);
static intptr_t image_fn_index(intptr_t v);
static intptr_t image_fn_resolve(intptr_t j);
static intptr_t image_fn_slot(word const *cell);
static intptr_t image_imm_index(word v);
static intptr_t img_decode(intptr_t v, word *base, char *code);
static intptr_t img_encode(struct img_ctx *x, intptr_t v);
static struct ai *img_canon_symbols(struct ai *g);
static struct ai *img_wake(void const *buf, uintptr_t len,
                           void *(*al)(struct ai*, void*, size_t));
static uintptr_t hc_hchain(struct ai_chain *c);
static uintptr_t hc_hstr(struct ai_str *s);
static uintptr_t hc_off(struct hc *h, word x);
static uintptr_t hc_stride(struct ai *g, union u *p, int *fzp);
static uintptr_t image_datasize(union u *d, void const *s);
static uintptr_t image_nhost(void);
static uintptr_t image_objsize(struct ai *g, union u *p);
static uintptr_t img_dict(word *sorted, uintptr_t nw, word *dict);
static uintptr_t img_hash(word v);
static uintptr_t img_rank_assign(struct ai *g, word const *blob, uintptr_t const *slots,
                                 uintptr_t nslot, word *rank, uintptr_t nser);
static uintptr_t img_stream(unsigned char *out, word const *blob, uintptr_t nw,
                            word const *key, unsigned char const *tk);
static unsigned char const *img_expand(word *out, uintptr_t nw, unsigned char const *p,
                                       unsigned char const *end, word const *dict);
static unsigned char hc_flag(struct hc *h, word x);
static void *img_wire(struct ai *g, struct image_hdr *H, word const *blob, uintptr_t nw, char const *cseg, uintptr_t *outlen);
static void image_root_enc(struct img_ctx *x, word v, uint64_t *tag, uint64_t *val);
static void img_hashcons(struct ai *g);
static void img_ord_sift(struct img_ord const *o, uintptr_t i, uintptr_t n);
static void img_ord_swap(struct img_ord const *o, uintptr_t i, uintptr_t j);
static void img_sort(struct img_ord const *o, uintptr_t n);
static word *img_build(struct ai *g, struct image_hdr *Ho, struct ai_image_guard const *guard,
                       uintptr_t *outnw, char **cseg, uintptr_t *ncode, uint8_t *why);
static word hc_can(struct hc *h, word x);
static word hc_intern(struct hc *h, union u *p, uintptr_t hv);
static word image_root_dec(uint64_t tag, uint64_t val, word *base);
// ============================================================================
// the heap-image snapshot (doc/misc/snapshot.md): serialize the compacted live heap
// with every pointer-bearing word range-encoded in place, so a fresh process
// reconstructs by re-walking. the core owns the buffer codec; the host wraps file io.
// ============================================================================
// lvm_* that appear as an object's ap but are not in ai_def1[]
static lvm_t *const image_extra_aps[] = {
 lvm_chain, lvm_tray, lvm_sym, lvm_nom, lvm_str, lvm_big, lvm_gembox, lvm_sunbox, lvm_twinbox,  // data sentinels
 lvm_map_lookup, lvm_map_data, lvm_cask, lvm_coin, lvm_port_io,                       // thread aps
 lvm_cur, lvm_help, lvm_ret0, lvm_ap, lvm_ret,                                         // dispatchers
 // instruction fns a compiled thread embeds directly (no ai_def1 cell); odd on
 // thumb, so they would otherwise escape as "fixnums" -- raw baker addresses
 lvm_callk, lvm_kcall, lvm_jump, lvm_scare, lvm_unc,
 lvm_fputbn, lvm_yield_sw, lvm_yield_nif, lvm_task_exit,
 _lvm_yieldk };   // the yield continuation: c0'd, so a task parked mid-yield carries it
// size (words) of a data object, the same per-kind logic as the GC. d carries the
// kind, s the raw length words -- two homes only during a fused image load, where
// the decoded ap lands in the pool while the payload still sits in the source blob.
static uintptr_t image_datasize(union u *d, void const *s) {
 switch (ai_typ(d)) {
  case DChain: return Width(struct ai_chain);
  case DMint:  return Width(struct ai_mint);
  case DNom:   return Width(struct ai_nom);
  case DGem:   return Width(struct ai_gem);
  case DSun:  return Width(struct ai_sun);
  case DTwin:  return Width(struct ai_twin);
  case DString:return str_width(((struct ai_str const*) s)->len);
  case DBig:   return b2w(ai_big_bytes((struct ai_big*)(word) s));
  case DTray:  return b2w(ai_tray_bytes((struct ai_tray*)(word) s)); }
 return 0; }                                                     // unreachable: ai_typ covers the 9
static uintptr_t image_objsize(struct ai *g, union u *p) {
 if (in_data(p->ap)) return image_datasize(p, p);
 word *term = (word*) ttag(g, p);                                // thread: scan to terminator (production)
 return (uintptr_t)(term - (word*) p) + 1; }
// the host nif slice: [__start_love_nifs, __stop_love_nifs) is a link-order table whose length
// is a runtime quantity, and the token layout wants a compile-time one -- so the index space
// reserves a fixed slice and only the occupied prefix is ever spelled. a host nif's value is
// the bare fn (AiNif stores it raw), so without this lane every app nif rode as an absolute.
// the slice is indexed by position, so this table's order is part of the image's contract.
// nothing checks it by name: the anchor does the whole job, since a binary whose nif set
// differs is a different binary and its symbol gap says so.
#define ImageNHost 256u
static ai_inline uintptr_t image_nhost(void) {
 uintptr_t n = (uintptr_t)(__stop_love_nifs - __start_love_nifs);
 return n < ImageNHost ? n : ImageNHost; }
// bidirectional lvm_* table: index <-> address. supplemental table 0..E-1, ai_def1 E.., then
// the host slice last so existing indices keep their meaning.
static intptr_t image_ap_index(intptr_t ap) {
 for (uintptr_t i = 0; i < countof(image_extra_aps); i++)
  if ((intptr_t) image_extra_aps[i] == ap) return (intptr_t) i;
 for (uintptr_t j = 0; j < ai_def1_n; j++)
  if (ai_def1[j].x == ap) return (intptr_t)(countof(image_extra_aps) + j);
 for (uintptr_t k = 0, n = image_nhost(); k < n; k++)
  if (__start_love_nifs[k].x == ap)
   return (intptr_t)(countof(image_extra_aps) + ai_def1_n + k);
 return -1; }
static ai_inline intptr_t image_ap_resolve(intptr_t idx) {
 uintptr_t e = countof(image_extra_aps), d = ai_def1_n;
 if (idx < (intptr_t) e) return (intptr_t) image_extra_aps[idx];
 if (idx < (intptr_t)(e + d)) return ai_def1[idx - e].x;
 { uintptr_t k = (uintptr_t) idx - e - d;                    // the host slice; a short roster reads 0
   return k < image_nhost() ? __start_love_nifs[k].x : 0; } }
// the bare-fn lane: a compiled thread embeds a nif's fn directly; it is reachable
// symbolically as the code slot of its ai_def1 cell (cell[0], or cell[2] under lvm_cur)
static intptr_t image_fn_slot(word const *cell) {
 return (intptr_t) (cell[0] == (word) lvm_cur ? cell[2] : cell[0]); }
intptr_t image_fn_index(intptr_t v) {
 for (uintptr_t j = 0; j < ai_def1_n; j++) {
  word const *c = (word const*) ai_def1[j].x;
  if (image_fn_slot(c) == v) return (intptr_t) j; }
 for (uintptr_t k = 0, n = image_nhost(); k < n; k++) {
  word const *c = (word const*) __start_love_nifs[k].x;
  if (image_fn_slot(c) == v) return (intptr_t)(ai_def1_n + k); }
 return -1; }
static intptr_t image_fn_resolve(intptr_t j) {
 uintptr_t d = ai_def1_n;
 if (j < (intptr_t) d) return image_fn_slot((word const*) ai_def1[j].x);
 { uintptr_t k = (uintptr_t) j - d;                          // the host slice; a short roster reads 0
   return k < image_nhost() ? image_fn_slot((word const*) __start_love_nifs[k].x) : 0; } }
// the out-of-pool immortals: (), "", the std ports, NULL (a mid-eval dump meets it
// in an undressed rbuf/wbuf), map_gap appended last so existing indices stay stable
// every port vtable belongs here: a port's head carries its vt, so an imaged
// port holds a binary address that only an index survives the trip.
static const word image_immortals[] = { ZeroPoint, EmptyString, (word) &ai_stdin, (word) &ai_stdout, (word) &ai_stderr, 0, map_gap,
 (word) &ai_fd_port_vt, (word) &ai_to_vt, (word) &ai_closed_vt, (word) &ai_ci_vt,
 (word) yield_c };   // g->ip's parked value: a root holds this binary address, so only an index survives
intptr_t image_imm_index(word v) {
 for (uintptr_t i = 0; i < countof(image_immortals); i++) if (image_immortals[i] == v) return (intptr_t) i;
 return -1; }
// ai_image_save / ai_image_load, the buffer codec: save compacts g and serializes
// {header, dictionary, token stream}; load validates, expands, decodes in place.
// a mismatched buffer -> NULL, so the caller boots normally -- never wrong.
/* bump if the wire format changes -- which includes RENUMBERING image_immortals, since a
   saved index means nothing to a binary that lays the table differently. "..05": the
   immortals lost the C-string vt with the baked-source port. ⚠ test/gate/bakerep.sh greps
   the SPELLING ("AISNO05") to corrupt a header, so the two move together. */
#define ImageMagic 0x35304f4e5349411aULL
#if defined(__x86_64__)
#define ImageArch 1
#elif defined(__aarch64__)
#define ImageArch 2
#elif defined(__riscv)
#define ImageArch 3
#else
#define ImageArch 0
#endif
// the image is binary-specific: its indices and kept absolutes mean anything only
// in the binary that dumped it. two guards reject a mismatch -> NULL -> normal
// boot: `arch`, and `anchor` -- the gap between two of the binary's own symbols,
// which a cross-arch or stale build lays out differently.
// the code segment leads with [raw length, deflated?] so it describes itself: the header
// keeps saying how many bytes are STORED, and only these two words know what they hold.
#define CodeSegHead (2 * sizeof(uint64_t))
extern intptr_t ai_inflate_raw(unsigned char const*, uintptr_t, unsigned char*, uintptr_t);
extern intptr_t ai_deflate_raw(struct ai*, unsigned char const*, uintptr_t, unsigned char*, uintptr_t);
struct image_hdr {
 uint64_t magic, wordsize, nwords, arch, anchor, nroot, rsv1, nstream, next_serial, ncode;
 uint64_t root_tag[24], root_val[24];        /* symbols, tasks, then the entire v0..end region walked
                                                GENERICALLY -- a new v0 field rides with no codec change */
};
// the absolute-pointer guard: a kept absolute is only wakeable inside the
// binary's own load segments -- a W^X pointer dies with the bake process. the
// host hands the bounds check in (NULL = audit off); a bad absolute fails the dump.
// the walk's whole state, threaded: a dump owns no globals, so it is re-entrant and
// nothing survives it. base/hp are the compacted live half, hb the blob's bytes.
struct img_ctx {
 struct ai *g;
 word *base, *hp;
 uintptr_t nabs;                  // kept absolutes -- 0 means the image needs no base delta
 uintptr_t cur_off, cur_ap;       // the object being encoded: offset + its hot -- what the guard is told
 int fail;                        // sticky: any refusal ends the dump
 struct ai_image_guard const *guard;
 // the code segment: the live natives' blobs packed in walk order, each [len, pad, code..]
 // rounded to 16 as the arena lays them, and the table of what landed where
 char *cseg; uintptr_t cn, ccap;
 struct img_code { uintptr_t a, off; } *ct; uintptr_t ctn, ctcap; };
// the code rung: a native's code address -> its byte offset in the packed segment,
// appending the blob on first sight. -1 when no room (the dump refuses)
static intptr_t img_code_off(struct img_ctx *x, uintptr_t a) {
 struct ai *g = x->g;
 for (uintptr_t i = 0; i < x->ctn; i++) if (x->ct[i].a == a) return (intptr_t) x->ct[i].off;
 uintptr_t hd = 2 * sizeof(uintptr_t), n = code_len((char*) a), span = (hd + n + 1 + 15) & ~(uintptr_t) 15;
 if (x->cn + span > x->ccap) {
  uintptr_t cap = x->ccap ? 2 * x->ccap : 1u << 16;
  while (cap < x->cn + span) cap *= 2;
  char *b = g->alloc(g, NULL, cap);
  if (!b) return -1;
  if (x->cseg) memcpy(b, x->cseg, x->cn), g->alloc(g, x->cseg, 0);
  x->cseg = b, x->ccap = cap; }
 if (x->ctn == x->ctcap) {
  uintptr_t cap = x->ctcap ? 2 * x->ctcap : 256;
  struct img_code *t = g->alloc(g, NULL, cap * sizeof *t);
  if (!t) return -1;
  if (x->ct) memcpy(t, x->ct, x->ctn * sizeof *t), g->alloc(g, x->ct, 0);
  x->ct = t, x->ctcap = cap; }
 uintptr_t off = x->cn + hd;
 memcpy(x->cseg + x->cn, (char*) a - hd, hd + n + 1);
 memset(x->cseg + x->cn + hd + n + 1, 0, span - hd - n - 1);
 x->cn += span;
 x->ct[x->ctn].a = a, x->ct[x->ctn].off = off, x->ctn++;
 return (intptr_t) off; }
// an un-wakeable absolute? every legit lane excluded first: a heap pointer, the lvm table, an
// immortal, the code arena -- what is left is a pointer of the binary, which the guard audits
int img_wxp(struct img_ctx *x, word v) {
 if (!v || oddp(v)) return 0;
 if ((word*) v >= x->base && (word*) v < x->hp) return 0;  // in-pool: a value, not code
 if (image_ap_index((intptr_t) v) >= 0) return 0;       // lvm table
 if (image_imm_index(v) >= 0) return 0;                 // immortal
 if (x->g && code_in(x->g, v)) return 0;                // a native's code
 return x->guard && !x->guard->ok(x->guard->ctx, (uintptr_t) v, x->cur_off, x->cur_ap); }
// encode a live value (post-compaction) -> portable (tag,payload):
//  0 FIX raw | 1 PTR word-offset into the blob | 2 LVM table index | 3 IMM immortal index
static void image_root_enc(struct img_ctx *x, word v, uint64_t *tag, uint64_t *val) {
 intptr_t li = image_ap_index((intptr_t) v); if (li >= 0) { *tag = 2, *val = (uint64_t) li; return; }  // ap table first: thumb aps are odd (see img_encode)
 if (oddp(v)) { *tag = 0, *val = (uint64_t) v; return; }
 if ((word*) v >= x->base && (word*) v < x->hp) { *tag = 1, *val = (uint64_t)((word*) v - x->base); return; }
 intptr_t ii = image_imm_index(v); if (ii >= 0) { *tag = 3, *val = (uint64_t) ii; return; }
 *tag = 0, *val = (uint64_t) v; }            // out-of-pool non-immortal root (unexpected): keep absolute
static word image_root_dec(uint64_t tag, uint64_t val, word *base) {
 return tag == 1 ? (word)(base + val) : tag == 2 ? (word) image_ap_resolve((intptr_t) val)
      : tag == 3 ? image_immortals[val] : (word) val; }
// the self-describing blob (no reloc tables; the load re-derives by re-walking):
//   heap pointer  -> its byte offset       [0, IdxBase)
//   lvm_* ap      -> IdxBase + 2*index     [IdxBase, IdxBase+2*NLVM)
//   immortal      -> IdxBase + 2*NLVM+2*ii [.., TBOUND)
//   native code   -> CodeBase + 2*offset       [CodeBase, TBOUND): a blob's place in the segment
//   binary ptr    -> kept absolute (>= TBOUND), base-delta-shifted on load
// the lanes start at a constant, not at the blob's own length, so the encoding is a pure
// function of the heap, so one live set is one byte string under any budget.
// a floor is the only way to get that: a string's payload rides raw and can be any even
// value, so no rule downstream of the encoder can tell a lane from a byte.
// fixnums (odd) pass through; every encoded pointer is even (indices doubled), so
// parity discriminates. a binary pointer below TBOUND would alias -> dump refuses.
#define ImageNLvm ((uintptr_t)(countof(image_extra_aps) + ai_def1_n + ImageNHost))
#define ImageNImm ((uintptr_t) countof(image_immortals))
#define ImageCellW 16u   /* max nif-cell span (words) an interior link can sit in */
// the bare-fn lane's width: one slot per nif cell whose code slot a thread can embed --
// ai_def1's, then the host slice's (AiNif registers a cell too: host/main.c's nif_exit[]).
#define ImageNFn ((uintptr_t)(ai_def1_n + ImageNHost))
// the lane floor: above any heap this codec encodes (1 TB on 64-bit, 128 MB on 32-bit;
// a dump past it is refused rather than aliased) and below the absolute lane.
#define ImageIdxBase ((uintptr_t) 1 << (sizeof(uintptr_t) == 8 ? 40 : 27))
// the code rung: a native's blob rides the image as bytes in a segment of its own, and
// the cell words that name it encode as its offset there (doubled, so parity holds)
#define ImageCodeBase (ImageIdxBase + 2 * (ImageNLvm + ImageNImm) \
                                    + 2 * ImageNLvm * ImageCellW + 2 * ImageNFn)
#define ImageCodeMax ((uintptr_t) 1 << 28)   /* bytes of code an image can carry */
// TBOUND: the top of the index region. every rung above encodes below it, so anything
// at or over it is a binary pointer -- which is why one spelling, not three.
#define ImageTBound (ImageCodeBase + 2 * ImageCodeMax)
// a kept absolute is stored relative to the anchor, which is what makes a bake
// reproducible: the offset from image_immortals is the same number on every run, where
// a raw address moves with ASLR. the bias re-centres it -- the offset is signed (rodata
// sits either side of the anchor) but the encoding is unsigned and must land above
// TBOUND, so there is half a bias of headroom each way and a pointer farther than that
// refuses the dump rather than aliasing an index. TBOUND, the bias and the offset are
// all even, so parity still tells an encoded absolute from a fixnum.
#define ImageAbsBias ((uintptr_t) 1 << (sizeof(uintptr_t) == 8 ? 40 : 26))
intptr_t img_encode(struct img_ctx *x, intptr_t v) {
 uintptr_t const hb = ImageIdxBase;
 // the ap table first, before parity: on thumb every fn address is odd and would
 // ride raw as a "fixnum", valid only at the baker's base (the qemu-twins trap
 // that walled the teensy wake). a colliding fixnum: ~300 values out of 2^31.
 intptr_t idx = image_ap_index(v);
 if (idx >= 0) return (intptr_t)(hb + 2 * (uintptr_t) idx);                      // lvm_* ap (odd on thumb, even on x64 -- both land here)
 if (oddp(v)) {
  intptr_t fj = image_fn_index(v);                                               // a bare nif fn embedded as an instruction word
  if (fj >= 0) return (intptr_t)(hb + 2 * (ImageNLvm + ImageNImm)
                                    + 2 * ImageNLvm * ImageCellW
                                    + 2 * (uintptr_t) fj);
  return v; }                                                                    // fixnum
 if (v >= (intptr_t) x->base && v < (intptr_t) x->hp) return v - (intptr_t) x->base;   // in-pool heap pointer -> byte offset
 if (x->g && code_in(x->g, (uintptr_t) v)) {                                     // a native's code -> its seat in the segment
  intptr_t off = img_code_off(x, (uintptr_t) v);
  if (off < 0 || (uintptr_t) off >= ImageCodeMax) { x->fail = 1; return v; }
  return (intptr_t)(ImageCodeBase + 2 * (uintptr_t) off); }
 intptr_t ii = image_imm_index((word) v);
 if (ii >= 0) return (intptr_t)(hb + 2 * ImageNLvm + 2 * (uintptr_t) ii);       // out-of-pool immortal
 // the bare-fn lane again, for an even-pointer arch. a compiled thread embeds a nif's
 // code slot directly; on thumb the value is odd and the parity branch above catches it
 // (it must -- it would otherwise ride as a fixnum). x64/arm64 pointers are even, so the
 // same words reached the kept-absolute tail instead and made every image binary-specific.
 { intptr_t fj = image_fn_index(v);
   if (fj >= 0) return (intptr_t)(hb + 2 * (ImageNLvm + ImageNImm)
                                     + 2 * ImageNLvm * ImageCellW
                                     + 2 * (uintptr_t) fj); }
 // an interior pointer into a ai_def1 nif cell (a baked partial's curry link):
 // encode (cell index, word offset); the owning cell is the greatest base <= v
 { intptr_t bj = -1; uintptr_t boff = 0;
   for (uintptr_t j = 0; j < ai_def1_n; j++) {
    uintptr_t x = (uintptr_t) ai_def1[j].x, d = (uintptr_t) v - x;
    if ((uintptr_t) v > x && d < ImageCellW * sizeof(word) && !(d % sizeof(word))
        && (bj < 0 || x > (uintptr_t) ai_def1[bj].x)) bj = (intptr_t) j, boff = d / sizeof(word); }
   if (bj >= 0) return (intptr_t)(hb + 2 * (ImageNLvm + ImageNImm)
                                     + 2 * (((uintptr_t)(countof(image_extra_aps) + (uintptr_t) bj)) * ImageCellW + boff)); }
 // a low absolute is not evidence of garbage: netbsd loads a PIE near zero, so
 // every binary pointer there sits under the lane floor. the encoding is anchor-
 // relative either way; what vouches for a low value is the wake-safety guard
 // (in-segment = a real binary pointer). only an unaudited dump keeps the floor.
 if ((uintptr_t) v < ImageTBound && !x->guard)
  x->fail = 1;                                                                   // low absolute, no auditor to vouch for it
 if (img_wxp(x, (word) v)) x->fail = 1;                                         // un-wakeable absolute (a stray mmap)
 { uintptr_t r = (uintptr_t) v - (uintptr_t) image_immortals + ImageAbsBias;   // wraps below the anchor; the bias re-centres
   if (r >= 2 * ImageAbsBias) { x->fail = 1; return v; }                       // farther from the anchor than the bias carries
   x->nabs++;                                                                    // kept absolute: the image is now binary-specific
   return (intptr_t)(ImageTBound + r); } }                                  // binary (host nif/.rodata), anchor-relative
// the decode ladder, split hot/cold by the rung-0 census: odd,
// heap offset, lvm index and immortal are 98.7% of decodes; the cold tail keeps
// the nif-cell interior, bare-fn and kept-absolute rungs out of the walk's way.
ai_noinline intptr_t img_decode_cold(intptr_t v, char *code) {
 uintptr_t const hb = ImageIdxBase;
 uintptr_t uv = (uintptr_t) v;
 if (uv < hb + 2 * (ImageNLvm + ImageNImm) + 2 * ImageNLvm * ImageCellW) {   // nif-cell interior: base + word offset
  uintptr_t k = (uv - hb - 2 * (ImageNLvm + ImageNImm)) / 2;
  return image_ap_resolve((intptr_t)(k / ImageCellW)) + (k % ImageCellW) * sizeof(word); }
 if (uv < hb + 2 * (ImageNLvm + ImageNImm) + 2 * ImageNLvm * ImageCellW
         + 2 * ImageNFn)                                                        // bare-fn lane: the cell's code slot
  return image_fn_resolve((intptr_t)((uv - hb - 2 * (ImageNLvm + ImageNImm)
                                         - 2 * ImageNLvm * ImageCellW) / 2));
 if (uv < ImageTBound) return (intptr_t)(code + (uv - ImageCodeBase) / 2);   // native code: the woken segment
 // the kept absolute, rebuilt against this run's anchor -- so the stored bytes never
 // held an address (the anchor check upstream is the only thing left that reads one)
 return (intptr_t)((uintptr_t) image_immortals + uv - ImageTBound - ImageAbsBias); }
ai_inline intptr_t img_decode(intptr_t v, word *base, char *code) {
 uintptr_t const hb = ImageIdxBase;
 if (oddp(v)) return v;
 uintptr_t uv = (uintptr_t) v;
 if (uv < hb) return (intptr_t)((char*) base + uv);                              // byte offset -> live pointer
 if (uv < hb + 2 * ImageNLvm) return image_ap_resolve((intptr_t)((uv - hb) / 2));
 if (uv < hb + 2 * (ImageNLvm + ImageNImm)) return (intptr_t) image_immortals[(uv - hb - 2 * ImageNLvm) / 2];
 return img_decode_cold(v, code); }
// ============================================================================
// the token stream: an encoded word rides as one byte when it is one of the 248
// commonest words in the image, else as an escape naming its own width. half an
// image is 25 distinct words and the single commonest -- lvm_chain's index, the ap
// every pair wears -- is 23% of it, so the stream lands near a quarter of the blob.
// chosen by count, not by lane: a fixed budget per lane measures 3.63x against this
// 3.82x, and follows whichever image it was tuned to. always ImageNDict words -- a short
// image repeats its commonest into the spare seats, which costs nothing, spares the wake
// a bound test per word, and lets the loader read the dictionary where it lies (the
// header is a whole number of words and a shebang is padded to one).
#define ImageNDict 248u   /* tokens 0..247 name a dictionary word, 248..255 a 1..8-byte literal */
#define ImageDHash 512u   /* the encoder's value -> token map (open-addressed, 0xff = free) */
// the encoder's tables ride the allocator, never the frame: together they are kilobytes, and
// an arm32 load has 12 bits of displacement -- port/mps2 refused to compile them onto the stack.
struct img_dic { word dict[ImageNDict], key[ImageDHash]; unsigned char tk[ImageDHash]; };
static uintptr_t img_hash(word v) {
 uintptr_t h = (uintptr_t) v;
 return h ^= h >> 17,
        h *= 0x9e3779b1u,
        h ^= h >> 13; }

// one heapsort for the codec's three orders (dictionary words, intern pairs, serial
// ranks): the arrays differ in shape, so lt and stride are the caller's and the heap
// walk is shared. no recursion, no scratch, no worst case.
struct img_ord {
 int (*lt)(struct img_ord const*, uintptr_t, uintptr_t);
 word *a;
 uintptr_t stride;
 word const *blob;
 uintptr_t const *nm; };

static void img_ord_swap(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 for (uintptr_t k = 0; k < o->stride; k++) {
  word t = o->a[o->stride * i + k];
  o->a[o->stride * i + k] = o->a[o->stride * j + k], o->a[o->stride * j + k] = t; } }
static void img_ord_sift(struct img_ord const *o, uintptr_t i, uintptr_t n) {
 for (uintptr_t c; (c = 2 * i + 1) < n; i = c) {
  if (c + 1 < n && o->lt(o, c, c + 1)) c++;
  if (!o->lt(o, i, c)) break;
  img_ord_swap(o, i, c); } }
static void img_sort(struct img_ord const *o, uintptr_t n) {
 for (uintptr_t i = n / 2; i-- > 0; ) img_ord_sift(o, i, n);
 for (uintptr_t k = n; k > 1; ) { img_ord_swap(o, 0, --k); img_ord_sift(o, 0, k); } }
static int img_lt_word(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 return o->a[i] < o->a[j]; }
// the commonest words of the blob, most frequent first. exact, and the tie-break is
// total: two machines baking one tree must choose the same 248 words or the images differ
// in every token (test_bakerep). a sorted copy costs a pass and answers exactly; the
// approximate counters that would save it have a tie order, which is the thing to avoid.
static uintptr_t img_dict(word *sorted, uintptr_t nw, word *dict) {
 uintptr_t cnt[ImageNDict], nd = 0;
 struct img_ord o = { img_lt_word, sorted, 1, NULL, NULL };
 img_sort(&o, nw);
 for (uintptr_t i = 0; i < nw; ) {
  uintptr_t j = i;
  while (j < nw && sorted[j] == sorted[i]) j++;
  uintptr_t n = j - i;
  if (nd < ImageNDict || n > cnt[nd - 1]) {                               // beats the weakest seat
   uintptr_t k = nd < ImageNDict ? nd++ : ImageNDict - 1;
   for (; k && cnt[k - 1] < n; k--) dict[k] = dict[k - 1], cnt[k] = cnt[k - 1];
   dict[k] = sorted[i], cnt[k] = n; }
  i = j; }
 return nd; }
int img_tok(word const *key, unsigned char const *tk, word v) {
 for (uintptr_t h = img_hash(v) & (ImageDHash - 1); tk[h] != 0xff; h = (h + 1) & (ImageDHash - 1))
  if (key[h] == v) return tk[h];
 return -1; }
// one token per blob word. out == NULL sizes the stream instead of writing it, so the
// buffer is allocated at its true length rather than at a worst case nine times the blob.
static uintptr_t img_stream(unsigned char *out, word const *blob, uintptr_t nw,
                            word const *key, unsigned char const *tk) {
 uintptr_t n = 0;
 for (uintptr_t i = 0; i < nw; i++) {
  int t = img_tok(key, tk, blob[i]);
  if (t >= 0) {
    if (out) out[n] = (unsigned char) t;
    n++;
    continue; }
  uintptr_t uv = (uintptr_t) blob[i], q = uv; unsigned wd = 0;
  do wd++, q >>= 8; while (q);                             // unsigned: word is signed, and a
  if (out) { out[n] = (unsigned char)(ImageNDict + wd - 1);   // negative one would shift forever
   for (unsigned k = 0; k < wd; k++) out[n + 1 + k] = (unsigned char)(uv >> (8 * k)); }
  n += 1 + wd; }
 return n; }
// ..and back, into the pool. answers where the stream stopped, or NULL if it ran short --
// a foreign buffer, so the caller boots normally. it does not require the whole stream: a
// derived image is the first nw words of a longer one, and only the caller knows whether
// a leftover tail is a prefix or a corruption.
static unsigned char const *img_expand(word *out, uintptr_t nw, unsigned char const *p,
                                       unsigned char const *end, word const *dict) {
 for (uintptr_t i = 0; i < nw; i++) {
  if (p >= end) return NULL;
  unsigned t = *p++;
  if (t < ImageNDict) { out[i] = dict[t]; continue; }
  unsigned wd = t - ImageNDict + 1;
  if ((uintptr_t)(end - p) < wd) return NULL;
  uintptr_t v = 0;
  if (wd == 3) v = (uintptr_t) p[0] | (uintptr_t) p[1] << 8 | (uintptr_t) p[2] << 16;  // 4 escapes in 5 are a
  else for (unsigned k = 0; k < wd; k++) v |= (uintptr_t) p[k] << (8 * k);             // heap offset, three wide
  p += wd, out[i] = (word) v; }
 return p; }
// the intern map's slot order is its insertion history: linear probing settles a
// collision by arrival, and each major re-arrives in old slot order, so the layout
// carries when the session's collections fired -- which the GC budget moves. the dump
// re-inserts the live pairs in spelling order instead: one layout per key set,
// whatever the session lived through. in place, over the backing the compact just
// bumped, so the session keeps the canonical map too.
static int img_nom_before(word a, word b) {          // spelling order: bytes, then length
 struct ai_str *x = (struct ai_str*) a, *y = (struct ai_str*) b;
 uintptr_t n = x->len < y->len ? x->len : y->len;
 int c = memcmp(x->bytes, y->bytes, n);
 return c < 0 || (c == 0 && x->len < y->len); }
static int img_lt_pair(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 return img_nom_before(o->a[2 * i], o->a[2 * j]); }
static struct ai *img_canon_symbols(struct ai *g) {
 word m = g->symbols;
 if (!m) return g;
 uintptr_t cap = map_cap(m), mask = cap - 1, n = 0;
 word *s = map_slots(m),
      *pairs = g->alloc(g, NULL, 2 * cap * sizeof(word));
 if (!pairs) return encode(g, ai_status_scare);
 for (uintptr_t j = 0; j < cap; j++)
  if (s[2 * j] != map_gap) pairs[2 * n] = s[2 * j], pairs[2 * n + 1] = s[2 * j + 1], n++;
 { struct img_ord o = { img_lt_pair, pairs, 2, NULL, NULL };
   img_sort(&o, n); }
 for (uintptr_t j = 0; j < cap; j++) s[2 * j] = map_gap, s[2 * j + 1] = zero;
 for (uintptr_t k = 0; k < n; k++) {
  uintptr_t i = hash(g, pairs[2 * k]) & mask;
  while (s[2 * i] != map_gap) i = (i + 1) & mask;
  s[2 * i] = pairs[2 * k], s[2 * i + 1] = pairs[2 * k + 1]; }
 g->alloc(g, pairs, 0);
 return g; }
// canonical serial order: mints keep session order; named noms order by spelling
// (ties by session order). session order alone is not canonical -- a weak drop plus a
// re-intern hands a name a fresh serial at a GC-chosen moment -- and `code` is only an
// order key behind the name, so a rank that sorts names by spelling preserves every
// comparison while the bytes stop caring when the session's collections fired.
// nm[serial] is the name string's blob byte offset, 0 for the nameless. a blob string
// wears the ai_str shape (the encode touches only the ap word), so spelling order is
// img_nom_before either side of the encode.
static int img_lt_rank(struct img_ord const *o, uintptr_t i, uintptr_t j) {
 uintptr_t a = (uintptr_t) o->a[i], b = (uintptr_t) o->a[j], na = o->nm[a], nb = o->nm[b];
 if (!na || !nb) return na == nb ? a < b : !na;
 { word x = (word)((char const*) o->blob + na), y = (word)((char const*) o->blob + nb);
   return img_nom_before(x, y) ? 1 : img_nom_before(y, x) ? 0 : a < b; } }
// assign ranks 1..k to the marked serials; answers k, or -1 on oom. slots are
// (word-offset << 1 | named); a named slot's word -1 is the encoded name.
static uintptr_t img_rank_assign(struct ai *g, word const *blob, uintptr_t const *slots,
                                 uintptr_t nslot, word *rank, uintptr_t nser) {
 uintptr_t *nm = g->alloc(g, NULL, nser * sizeof(uintptr_t)),
           *live = g->alloc(g, NULL, nser * sizeof(uintptr_t)),
           n = 0, k;
 if (!nm || !live) { g->alloc(g, nm, 0); g->alloc(g, live, 0); return (uintptr_t) -1; }
 memset(nm, 0, nser * sizeof(uintptr_t));
 for (uintptr_t i = 0; i < nslot; i++) {
  uintptr_t v = (uintptr_t) blob[slots[i] >> 1];
  if (v < nser && (slots[i] & 1)) nm[v] = (uintptr_t) blob[(slots[i] >> 1) - 1]; }
 for (uintptr_t i = 1; i < nser; i++) if (rank[i]) live[n++] = i;
 { struct img_ord o = { img_lt_rank, (word*) live, 1, blob, nm };
   img_sort(&o, n); }
 for (k = 0; k < n; k++) rank[live[k]] = k + 1;
 g->alloc(g, nm, 0), g->alloc(g, live, 0);
 return n; }
// --- the bake-time hash-cons (doc/misc/snapshot.md) ----------------------------
// two structurally equal chains are one value wearing two addresses. a chain's fields are
// immutable by convention and not by structure -- poke writes whatever cell it is handed --
// but nothing in the tree writes one: c0 kept the last few, and they became boxes and
// copies, so the convention is now unbroken rather than merely respected where it counts.
// merging is invisible to `=`, which is structural already, and
// to the printer; `id?` is the one witness, and after this it answers 1 on quoted data
// that was written out twice.
// the walk is bottom-up, so both children are canonical before their parent is looked
// up and a candidate compares by POINTER on both fields -- the hash decides nothing and
// no collision can merge unequals. duplicates are left unreferenced and the compaction
// after this drops them; a reference from a root is not rewritten, so whatever the
// stack still holds simply survives.
// a STRING is merged only where nothing can write its bytes: a cask's payload and a
// port's buffers are memcpy'd through their holder, and those two pin (below).
enum { HcHead = 1, HcChain = 2, HcStr = 4, HcPin = 8, HcDone = 16, HcProg = 32 };
struct hc { word *base, *hp; unsigned char *fl; word *cn, *tab, *stk; uintptr_t mask; };
static ai_inline uintptr_t hc_off(struct hc *h, word x) { return (uintptr_t) ((word*) x - h->base); }
// the flags at x, or 0 where x does not name an object head in the walked heap
static ai_inline unsigned char hc_flag(struct hc *h, word x) {
 return !(x & (word) (sizeof(word) - 1)) && (word*) x >= h->base && (word*) x < h->hp
      ? h->fl[hc_off(h, x)] : 0; }
static ai_inline word hc_can(struct hc *h, word x) {
 return hc_flag(h, x) & HcDone ? h->cn[hc_off(h, x)] : x; }
static uintptr_t hc_hstr(struct ai_str *s) {
 uintptr_t r = 1469598103934665603u ^ s->len * 1099511628211u;
 for (uintptr_t i = 0; i < s->len; i++) r = (r ^ (unsigned char) s->bytes[i]) * 1099511628211u;
 return r; }
static uintptr_t hc_hchain(struct ai_chain *c) {
 return (uintptr_t) c->a * 0x9E3779B97F4A7C15u ^ (uintptr_t) c->b * 0xC2B2AE3D27D4EB4Fu; }
// the class representative for p: the first object of its shape the walk reached
static word hc_intern(struct hc *h, union u *p, uintptr_t hv) {
 for (uintptr_t i = hv & h->mask; ; i = (i + 1) & h->mask) {
  word q = h->tab[i];
  if (!q) return h->tab[i] = (word) p;
  union u *r = (union u*) q;
  if (ai_typ(r) != ai_typ(p)) continue;
  if (ai_typ(p) == DString) {
   if (len(r) == len(p) && !memcmp(txt(r), txt(p), len(p))) return q; }
  else if (two(r)->a == two(p)->a && two(r)->b == two(p)->b) return q; } }
// the object stride, forging a live finalizer node's width (three raw words, no header)
static uintptr_t hc_stride(struct ai *g, union u *p, int *fzp) {
 struct ai_fz *z = g->fz;
 while (z && (union u*) z != p) z = z->next;
 return (*fzp = !!z) ? Width(struct ai_fz) : image_objsize(g, p); }
static void img_hashcons(struct ai *g) {
 word *base = g->major_base, *hp = g->major_hp;
 uintptr_t nw = (uintptr_t) (hp - base), nobj = 0, cap = 16;
 int fz;
 for (union u *p = (union u*) base; (word*) p < hp; nobj++)
  p = (union u*) ((word*) p + hc_stride(g, p, &fz));
 while (cap < 2 * nobj) cap <<= 1;
 struct hc H = { base, hp, 0, 0, 0, 0, cap - 1 }, *h = &H;
 h->fl = g->alloc(g, NULL, nw);
 h->cn = g->alloc(g, NULL, nw * sizeof(word));
 h->tab = g->alloc(g, NULL, cap * sizeof(word));
 h->stk = g->alloc(g, NULL, (nobj + 1) * sizeof(word));
 if (h->fl && h->cn && h->tab) {                       // no scratch -> no dedup, never half of one
  memset(h->fl, 0, nw);
  memset(h->tab, 0, cap * sizeof(word));
  // 1. the heads, by kind. a finalizer node is not an object and never merges.
  for (union u *p = (union u*) base; (word*) p < hp; ) {
   uintptr_t sz = hc_stride(g, p, &fz), off = (uintptr_t) ((word*) p - base);
   h->fl[off] = HcHead | (fz || !in_data(p->ap) ? 0
                        : ai_typ(p) == DChain ? HcChain : ai_typ(p) == DString ? HcStr : 0);
   h->cn[off] = (word) p;
   p = (union u*) ((word*) p + sz); }
  // 2. pin every string a byte-writable holder names. the non-code thread aps are a
  // closed roster (image_extra_aps): a tablet's two halves, a coin, a cask, a port --
  // and of those only a cask's payload and a port's buffers are memcpy'd through in
  // place. every other slot anywhere replaces a POINTER and never a byte, so it says
  // nothing here. ⚠ a byte-writable holder added to that roster has to be added here.
  for (union u *p = (union u*) base; (word*) p < hp; ) {
   uintptr_t sz = hc_stride(g, p, &fz);
   if (!fz && (p->ap == lvm_cask || p->ap == lvm_port_io))
    for (uintptr_t i = 0; i + 1 < sz; i++) {
     word v = ((word*) p)[i];
     if (hc_flag(h, v) & HcStr) h->fl[hc_off(h, v)] |= HcPin; }
   p = (union u*) ((word*) p + sz); }
  for (word *s = g->sp; s < topof(g); s++)
   if (hc_flag(h, *s) & HcStr) h->fl[hc_off(h, *s)] |= HcPin;
  for (word i = 0; i < g->end - &g->v0; i++) {
   word v = (&g->v0)[i];
   if (hc_flag(h, v) & HcStr) h->fl[hc_off(h, v)] |= HcPin; }
  for (struct ai_r *r = g->root; r; r = r->n)
   if (hc_flag(h, *r->x) & HcStr) h->fl[hc_off(h, *r->x)] |= HcPin;
  // 3. strings have no children, so one pass settles them
  for (union u *p = (union u*) base; (word*) p < hp; ) {
   uintptr_t sz = hc_stride(g, p, &fz), off = (uintptr_t) ((word*) p - base);
   if ((h->fl[off] & (HcStr | HcPin)) == HcStr)
    h->cn[off] = hc_intern(h, p, hc_hstr(str(p))), h->fl[off] |= HcDone;
   p = (union u*) ((word*) p + sz); }
  // 4. chains, children first. an explicit stack: a long list is a deep chain, and the
  // recursion that shape would ask for is the one this walk cannot afford. a child still
  // in progress is a cycle -- unreachable for a chain, which nothing can rewrite into one
  // -- and leaves its whole ring unmerged rather than guessed at.
  if (h->stk) for (union u *p0 = (union u*) base; (word*) p0 < hp; ) {
   uintptr_t sz = hc_stride(g, p0, &fz), off0 = (uintptr_t) ((word*) p0 - base);
   union u *p1 = (union u*) ((word*) p0 + sz);
   if (h->fl[off0] & HcChain && !(h->fl[off0] & (HcDone | HcProg))) {
    uintptr_t sp = 0;
    h->fl[off0] |= HcProg, h->stk[sp++] = (word) p0;
    while (sp) {
     word y = h->stk[sp - 1];
     struct ai_chain *c = two(y);
     unsigned char fa = hc_flag(h, c->a), fb = hc_flag(h, c->b);
     if (fa & HcChain && !(fa & (HcDone | HcProg)))
      { h->fl[hc_off(h, c->a)] |= HcProg, h->stk[sp++] = c->a; continue; }
     if (fb & HcChain && !(fb & (HcDone | HcProg)))
      { h->fl[hc_off(h, c->b)] |= HcProg, h->stk[sp++] = c->b; continue; }
     sp--;
     uintptr_t oy = hc_off(h, y);
     if ((fa & HcChain && !(fa & HcDone)) || (fb & HcChain && !(fb & HcDone)))
      { h->fl[oy] |= HcDone; continue; }                       // on a cycle: its own class
     c->a = hc_can(h, c->a), c->b = hc_can(h, c->b);
     h->cn[oy] = hc_intern(h, (union u*) y, hc_hchain(c)), h->fl[oy] |= HcDone; } }
   p0 = p1; }
  // 5. the references chain fields did not already carry: a thread's words, a tray's
  // elements, a nom's spelling. roots are left alone -- a duplicate a root still names
  // survives, which costs a few words and keeps the running stack's values identical.
  for (union u *p = (union u*) base; (word*) p < hp; ) {
   uintptr_t sz = hc_stride(g, p, &fz);
   if (!fz) {
    if (!in_data(p->ap))
     for (uintptr_t i = 0; i + 1 < sz; i++) ((word*) p)[i] = hc_can(h, ((word*) p)[i]);
    else switch (ai_typ(p)) {
     case DNom: nom(p)->name = (uintptr_t) hc_can(h, (word) nom(p)->name); break;
     case DTray: if (tray(p)->type == ai_O) {
      word *e = (word*) tray_data(tray(p));
      for (uintptr_t i = 0, ne = tray_nelem(tray(p)); i < ne; i++) e[i] = hc_can(h, e[i]); }
      break;
     default: break; } }
   p = (union u*) ((word*) p + sz); } }
 g->alloc(g, h->fl, 0), g->alloc(g, h->cn, 0), g->alloc(g, h->tab, 0), g->alloc(g, h->stk, 0); }
// compact g and encode its live half into a fresh g->alloc'd blob, filling *Ho; NULL on
// failure. the blob is words, not the wire: img_wire tokenizes it for a file and the
// layered bake diffs two of them. dumps wherever it is called -- a mid-eval dump's
// continuation rides as wake-unreachable ballast -- and the guarded entry keeps the boot
// path honest.
// `why` is the stage, set before each fallible step, so a refusal names where it
// stopped: 1 no major pool, 2 the compaction scared, 3 out of memory, 4 an unencodable
// heap word, 5 the root table is too small, 6 an unencodable root, 8 the heap outgrew
// the lane floor -- and 0 only on the way out. (7, not quiet, is the callers' own.) it
// rides a parameter because it is true of one call and nothing else.
static word *img_build(struct ai *g, struct image_hdr *Ho, struct ai_image_guard const *guard,
                       uintptr_t *outnw, char **cseg, uintptr_t *ncode, uint8_t *why) {
 *why = 1;
 if (!g->major_pool) return NULL;                        // needs the major pool (it holds the compacted live half)
 ai_core_of(g)->io = NULL;                               // clear the non-deterministic fd before the bake
 *why = 2;
 if (!ai_ok(gen_major(g, 0, NULL))) return NULL;                  // compact: live half -> [major_base, major_hp) (oom -> no image)
 img_hashcons(g);                                        // merge equal chains/strings, in place
 if (!ai_ok(gen_major(g, 0, NULL))) return NULL;                  // ..and compact the duplicates away
 if (!ai_ok(g = img_canon_symbols(g))) return NULL;      // canonical intern layout (oom -> no image)
 *why = 3;
 word *base = g->major_base, *hp = g->major_hp;
 uintptr_t nw = (uintptr_t)(hp - base), bytes = nw * sizeof(word);
 // the heap must fit under the lane floor, or a byte offset collides with an index and
 // decodes as an ap.
 *why = 8;
 if (bytes >= ImageIdxBase) return NULL;
 *why = 3;
 *cseg = NULL, *ncode = 0;
 word *blob = g->alloc(g, NULL, bytes);                  // the encoded words: scratch, not the file
 if (!blob) return NULL;
 memcpy(blob, base, bytes);
 // canonical serials (blob-side only): the mint stream's live members rename monotone
 // to 1..k below and the header counter drops to k, so a dead mint leaves neither its
 // number nor a +1 ripple through every nom minted after it and one live heap answers
 // one byte string whatever the session's history. `code` is an order key, so rank-order
 // assignment preserves every comparison; serial 0 stays the immortal ()'s. the session
 // keeps its own serials -- the rename touches the blob alone, so a mid-eval bake's
 // continuation is unharmed. a pid charm copied into user data is unfindable and keeps
 // its old number across a bake.
 uintptr_t nslot = 0, *slots = g->alloc(g, NULL, (nw / 2 + 1) * sizeof(uintptr_t));
 if (!slots) { g->alloc(g, blob, 0); return NULL; }
 struct img_ctx X = { g, base, hp, 0, 0, 0, 0, guard, 0, 0, 0, 0, 0, 0 }, *x = &X;
 for (union u *p = (union u*) base; (word*) p < hp; ) {   // walk the live heap (ttag works on it), encode into blob
  uintptr_t off = (uintptr_t)((word*) p - base);
  // a live finalizer node sits raw in the heap (three words, no header), so no
  // walk can stride it: forge its blob copy into a dead chain of the same width.
  // the fz head lives outside the root window, so a woken session has no finalizables.
  struct ai_fz *z = g->fz;
  while (z && (union u*) z != p) z = z->next;
  if (z) {
   blob[off] = img_encode(x, (intptr_t) lvm_chain);
   blob[off + 1] = blob[off + 2] = img_encode(x, (intptr_t) ZeroPoint);
   p = (union u*) ((word*) p + Width(struct ai_fz));
   continue; }
  uintptr_t sz = image_objsize(g, p);
  x->cur_off = off, x->cur_ap = ((word*) p)[0];
  blob[off] = img_encode(x, ((word*) p)[0]);                                    // word0: the ap (a native cell's is its code)
  if (in_data(p->ap)) switch (ai_typ(p)) {
   case DChain: blob[off + 1] = img_encode(x, ((struct ai_chain*) p)->a);
                blob[off + 2] = img_encode(x, ((struct ai_chain*) p)->b); break;
   case DNom:   blob[off + 1] = img_encode(x, (intptr_t) nom(p)->name);
                slots[nslot++] = (off + 2) << 1 | 1; break;   // the serial word, canonicalized below (tagged: named)
   case DMint:  slots[nslot++] = (off + 1) << 1; break;  // mints and missings (one shape, one ap)
   case DTray:   if (tray(p)->type == ai_O) {
                 word *e = (word*) tray_data(tray(p)); uintptr_t ne = tray_nelem(tray(p)), eo = (uintptr_t)(e - (word*) p);
                 for (uintptr_t i = 0; i < ne; i++) blob[off + eo + i] = img_encode(x, e[i]); }
                break;
   // the tail padding is uninitialized heap -- a stale pointer fragment, ASLR-varying
   case DString: { uintptr_t n = ((struct ai_str*) p)->len, w = b2w(n + 1);
                   if (w) memset((char*)(blob + off + str_type_width) + n, 0,
                                 w * sizeof(word) - n);
                   break; }
   default: break; }                                     // DMint/DBig/DGem/DSun/DTwin: flat leaves
  else for (uintptr_t i = 1; i < sz; i++) blob[off + i] = img_encode(x, ((word*) p)[i]);   // thread interior + terminator
  p = (union u*) ((word*) p + sz); }
 *why = 4;
 if (x->ct) g->alloc(g, x->ct, 0);
 *cseg = x->cseg, *ncode = x->cn;
 if (x->fail) { g->alloc(g, slots, 0); g->alloc(g, blob, 0); return NULL; }   // a binary pointer landed in the index range -> refuse (caller boots normally)
 // the rename: mark live serials (the collected nom/mint slots read raw off the
 // blob -- scalars rode the memcpy -- plus the pids of both task rings), rank
 // them 1..k in img_rank_assign's canonical order, rewrite in place. rings walk
 // the live post-compaction nodes; their pid word sits at [2] as a charm.
 uintptr_t nser = g->next_serial + 1, kser = 0;
 word *rank = g->alloc(g, NULL, nser * sizeof(word));
 if (!rank) { g->alloc(g, slots, 0); g->alloc(g, blob, 0); return NULL; }
 memset(rank, 0, nser * sizeof(word));
 for (uintptr_t i = 0; i < nslot; i++)
  if ((uintptr_t) blob[slots[i] >> 1] < nser) rank[blob[slots[i] >> 1]] = 1;
 for (union u *n = g->tasks, *st = n; n; n = n->m == st ? NULL : n->m) {
  uintptr_t pid = getcharm(n[2].x);
  if (pid < nser) rank[pid] = 1; }
 if (g->parked)
  for (union u *n = g->parked, *st = n; n; n = n->m == st ? NULL : n->m) {
   uintptr_t pid = getcharm(n[2].x);
   if (pid < nser) rank[pid] = 1; }
 rank[0] = 0;                                            // the immortal ()'s, never drawn, never moved
 kser = img_rank_assign(g, blob, slots, nslot, rank, nser);
 if (kser == (uintptr_t) -1) { g->alloc(g, rank, 0); g->alloc(g, slots, 0); g->alloc(g, blob, 0); return NULL; }
 for (uintptr_t i = 0; i < nslot; i++)
  if ((uintptr_t) blob[slots[i] >> 1] < nser) blob[slots[i] >> 1] = rank[blob[slots[i] >> 1]];
 for (union u *n = g->tasks, *st = n; n; n = n->m == st ? NULL : n->m) {
  uintptr_t off = (uintptr_t)((word*) n - base), pid = getcharm(n[2].x);
  if ((word*) n >= base && (word*) n < hp && pid < nser) blob[off + 2] = putcharm(rank[pid]); }
 if (g->parked)
  for (union u *n = g->parked, *st = n; n; n = n->m == st ? NULL : n->m) {
   uintptr_t off = (uintptr_t)((word*) n - base), pid = getcharm(n[2].x);
   if ((word*) n >= base && (word*) n < hp && pid < nser) blob[off + 2] = putcharm(rank[pid]); }
 g->alloc(g, slots, 0);
 // rsv1 carries the kept-absolute count, odd-tagged ((n<<1)|1) so a pre-field image
 // (rsv1 == 0) never reads as "zero absolutes" -- those keep the strict anchor check.
 // `anchor` is the gap between the two symbols, not either address. addresses would
 // write this run's ASLR base into the header, which is the whole of what a
 // reproducible bake must not carry.
 // the counter drops to the live count: the woken twin's first mint lands
 // above every renamed 1..kser, and the bytes carry no dead mints.
 struct image_hdr H = { ImageMagic, sizeof(word), nw, ImageArch, (uint64_t)((word) &ai_image_save - (word) image_immortals), 0, (uint64_t)(x->nabs << 1) | 1u, 0, kser, x->cn, {0}, {0} };
 g->alloc(g, rank, 0);
 // roots = symbols + tasks (live outside v0), then the whole GC-traced v0..end block, generically: any
 // field added to struct ai's v0 region is serialized automatically, no codec edit (cf. the GC's v0..end loop).
 uintptr_t nv = (word*) g->end - (word*) &g->v0, nr = 2 + nv;
 *why = 5;
 if (nr > countof(H.root_tag)) { g->alloc(g, blob, 0); return NULL; }    // grew past the header table -> bump root_tag[]
 image_root_enc(x, g->symbols,      &H.root_tag[0], &H.root_val[0]);
 image_root_enc(x, (word) g->tasks, &H.root_tag[1], &H.root_val[1]);
 for (uintptr_t i = 0; i < nv; i++) image_root_enc(x, ((word*) &g->v0)[i], &H.root_tag[2 + i], &H.root_val[2 + i]);
 *why = 6;
 if (x->fail) { g->alloc(g, blob, 0); return NULL; }     // ..a root refused: the walk's own check is behind us
 H.nroot = nr;
 return *why = 0, *Ho = H, *outnw = nw, blob; }
// ..and the wire: {header, dictionary, token stream}, g->alloc'd. fills H.nstream.
static void *img_wire(struct ai *g, struct image_hdr *H, word const *blob, uintptr_t nw, char const *cseg, uintptr_t *outlen) {
 uintptr_t bytes = nw * sizeof(word);
 // the dictionary wants a sorted copy and the copy is the blob's size again -- transient,
 // and bake-time, which is the side of this trade nobody waits on.
 struct img_dic *d = g->alloc(g, NULL, sizeof *d);
 if (!d) return NULL;
 word *sorted = g->alloc(g, NULL, bytes);
 if (!sorted) { g->alloc(g, d, 0); return NULL; }
 memcpy(sorted, blob, bytes);
 uintptr_t nd = img_dict(sorted, nw, d->dict);
 g->alloc(g, sorted, 0);
 memset(d->tk, 0xff, sizeof d->tk);
 for (uintptr_t i = 0; i < nd; i++) {
  uintptr_t h = img_hash(d->dict[i]) & (ImageDHash - 1);
  while (d->tk[h] != 0xff) h = (h + 1) & (ImageDHash - 1);
  d->key[h] = d->dict[i], d->tk[h] = (unsigned char) i; }
 for (uintptr_t i = nd; i < ImageNDict; i++) d->dict[i] = nd ? d->dict[0] : 0;   // the spare seats
 uintptr_t ns = img_stream(NULL, blob, nw, d->key, d->tk), db = ImageNDict * sizeof(word);
 H->nstream = ns;
 // the code segment ships deflated: thousands of blobs share prologue and epilogue
 // shapes, so it makes about a tenth of itself and the wake pays one inflate. a stream
 // that would not shrink is stored raw under the same two words.
 uintptr_t craw = H->ncode, cstore = 0;
 unsigned char *cz = NULL;
 if (craw) {
  intptr_t got = -1;
  if ((cz = g->alloc(g, NULL, craw)))
   got = ai_deflate_raw(g, (unsigned char const*) cseg, craw, cz, craw);
  if (got > 0) cstore = CodeSegHead + (uintptr_t) got;
  else { if (cz) g->alloc(g, cz, 0); cz = NULL; cstore = CodeSegHead + craw; }
  H->ncode = cstore; }
 uintptr_t total = sizeof *H + db + ns + cstore;
 char *buf = g->alloc(g, NULL, total);
 if (!buf) { if (cz) g->alloc(g, cz, 0); g->alloc(g, d, 0); return NULL; }
 memcpy(buf, H, sizeof *H);
 memcpy(buf + sizeof *H, d->dict, db);
 img_stream((unsigned char*)(buf + sizeof *H + db), blob, nw, d->key, d->tk);
 if (craw) {
  char *p = buf + sizeof *H + db + ns;
  ((uint64_t*) p)[0] = craw, ((uint64_t*) p)[1] = cz ? 1 : 0;
  memcpy(p + CodeSegHead, cz ? (char const*) cz : cseg, cstore - CodeSegHead); }
 if (cz) g->alloc(g, cz, 0);
 g->alloc(g, d, 0);
 return *outlen = total, buf; }
void *ai_image_save_(struct ai *g, uintptr_t *outlen, struct ai_image_guard const *guard) {
 struct image_hdr H;
 uintptr_t nw = 0;
 uint8_t w_;
 char *cseg = NULL; uintptr_t ncode = 0;
 word *blob = img_build(g, &H, guard, &nw, &cseg, &ncode, &w_);
 if (!blob) { if (cseg) g->alloc(g, cseg, 0); return NULL; }
 void *buf = img_wire(g, &H, blob, nw, cseg, outlen);
 if (cseg) g->alloc(g, cseg, 0);
 return g->alloc(g, blob, 0), buf; }
void *ai_image_save(struct ai *g, uintptr_t *outlen, struct ai_image_guard const *guard) {
 if ((word*) g->sp != topof(g)) return NULL;             // quiescent: an empty ai stack at the dump point
 return ai_image_save_(g, outlen, guard); }
// the image-wake progress hook: weak no-op, overridden by a port bringing the
// wake up on new metal (a crashed wake with no debugger is otherwise invisible).
// stages: 1 header, 2 pool, 3 blob, 4 the token stream expanded, 0x100+k walk (per 64K words), 5 walk, 6 roots.
// the wake: `buf` holds the header, dictionary and token stream to read.
struct ai *img_wake(void const *buf, uintptr_t len,
                           void *(*al)(struct ai*, void*, size_t)) {
 struct image_hdr H;
 if (len < sizeof H) return NULL;
 memcpy(&H, buf, sizeof H);
 if (H.magic != ImageMagic || H.wordsize != sizeof(word) || H.arch != ImageArch) return NULL;
 uintptr_t nw = H.nwords, db = ImageNDict * sizeof(word), ns = H.nstream;
 // the stream's length is the header's, never the buffer's: a baked image arrives inside a
 // reserved section and a file may carry a shebang, so "the rest of what you handed me" is
 // the one reading that would make a good image look foreign and fall silently back to the egg.
 if (len < sizeof H + db + ns + H.ncode) return NULL;             // truncated buffer
 struct ai *g = ai_ini_m(al);
 if (!g) return NULL;
 if (nw > g->major_len) {                                // grow the major pool to fit the image
  g->alloc(g, g->major_pool, 0);
  // the slack is what the nursery ramps into, and it must CLEAR the nursery: a minor is
  // forced to a major once the pool has less free than a whole one (gen_please's
  // worst-case promotion test), and the wake seeds g->len at nw >> 1 below -- so a
  // quarter sits under it by construction and latches the first collection to a major
  // over the whole woken image. a floor besides, for the small end; the pages stay
  // untouched until the ramp wants them.
  g->major_len = nw + (nw >> 1) + (1u << 19);
  g->major_pool = g->major_base = g->alloc(g, NULL, 2 * g->major_len * sizeof(word));
  if (!g->major_pool) return NULL;
 }
 word *base = g->major_base;
 if (!base) return NULL;
 g->major_hp = base + nw;
 // the check is a distance, never two addresses: the two symbols shift together under
 // ASLR, so storing where they landed would write this run's mmap base into the header
 // and no bake could be checked by its hash. the gap is the same number every run and
 // discriminates as well -- a stale or cross-arch binary moves one symbol without the
 // other. absolutes are stored anchor-relative, so nothing on the decode side wants a shift.
 // the anchor is unconditional, symbolic image or not: relocation is not the question --
 // an index still means whatever this binary's tables say, so a foreign build reads the
 // same words as other functions. the gap between two of our own symbols answers that for
 // free and moves on any layout change.
 if ((intptr_t)((word) &ai_image_save - (word) image_immortals) != (intptr_t) H.anchor)
  return NULL;                                                                   // a different binary -> normal boot
 // expand the token stream into the pool, then decode it there in place. the two passes
 // read and write one word at a time at the same index, so src and base are the same array
 // -- and a payload word arrives already seated, which is why the flat-leaf memcpys are gone.
 { unsigned char const *p0 = (unsigned char const*) buf + sizeof H + db,
                       *q = img_expand(base, nw, p0, p0 + ns, (word const*)((char const*) buf + sizeof H));
   if (!q || q != p0 + ns) return NULL; }                          // an image consumes its stream exactly
 // the natives' code, seated before the walk names it: a chunk of the arena, sealed
 char *code = NULL;
 if (H.ncode) {
  unsigned char const *p = (unsigned char const*) buf + sizeof H + db + ns;
  uintptr_t craw;
  if (H.ncode < CodeSegHead) return NULL;
  craw = (uintptr_t) ((uint64_t const*) p)[0];
  if (((uint64_t const*) p)[1]) {                        // deflated: inflate, then adopt the blobs
   unsigned char *t = g->alloc(g, NULL, craw);
   if (!t) return NULL;
   if (ai_inflate_raw(p + CodeSegHead, H.ncode - CodeSegHead, t, craw) != (intptr_t) craw) {
    g->alloc(g, t, 0); return NULL; }
   code = code_adopt(g, (char const*) t, craw);
   g->alloc(g, t, 0); }
  else code = code_adopt(g, (char const*) p + CodeSegHead, craw);
  if (!code) return NULL; }
 word const *src = base;
 for (uintptr_t off = 0; off < nw; ) {
  uintptr_t sz;
  union u *p = (union u*)(base + off);
  word const *s = src + off;
  base[off] = (word) img_decode((intptr_t) s[0], base, code);                // word0 first: the ap (kinding needs it real)
  if (in_data(p->ap)) { sz = image_datasize(p, s);                                // data kinds: size by ai_typ + the source's raw length words
   switch (ai_typ(p)) {
    case DChain: base[off + 1] = (word) img_decode((intptr_t) s[1], base, code);
                 base[off + 2] = (word) img_decode((intptr_t) s[2], base, code); break;
    case DNom:   base[off + 1] = (word) img_decode((intptr_t) s[1], base, code); break;   // code + dig ride raw
    case DTray:  if (tray(p)->type == ai_O) { word *e = (word*) tray_data(tray(p)); uintptr_t ne = tray_nelem(tray(p));
                  for (uintptr_t i = 0; i < ne; i++) e[i] = img_decode(e[i], base, code); }
                 break;
    default:     break; }                                                         // flat leaves: payload is already seated
  } else {                                                                        // thread: the encoded terminator is its head's byte offset | tag
   word term = (word)(off * sizeof(word) + ai_thread_tag); uintptr_t k = 1;
   uintptr_t kmax = nw - off;                                                     // bound the walk: a mis-decoded word0 must refuse
   for (;; k++) {                                                                 // one pass, decoding to the terminator (rung 2):
    if (k >= kmax) return NULL;                                                   // the load, never march off the pool (on metal the                                               // the load, never march off the pool (on metal the
    if (s[k] == term) break;                                                      // pool's edge is a dead bus, and a dead bus is mute)
    base[off + k] = (word) img_decode((intptr_t) s[k], base, code); }
   base[off + k] = (word) p + ai_thread_tag;                                      // the terminator, decoded by hand: its head went live
   sz = k + 1; }
  off += sz; }
 uintptr_t nv = (word*) g->end - (word*) &g->v0;                         // same struct/binary (anchor-checked) -> same layout
 if (H.nroot != 2 + nv) return NULL;                                     // root count mismatch -> stale/foreign image -> normal boot
 g->symbols = image_root_dec(H.root_tag[0], H.root_val[0], base);
 g->tasks   = (union u*) image_root_dec(H.root_tag[1], H.root_val[1], base);
 // the parked ring is not in the image: an fd means nothing in a new process,
 // and a baker is single-tasked -- a woken runtime starts empty
 g->parked  = NULL;
 for (uintptr_t i = 0; i < nv; i++) ((word*) &g->v0)[i] = image_root_dec(H.root_tag[2 + i], H.root_val[2 + i], base);
 g->next_serial = H.next_serial;
 g->tasks[7].x = zero;   // a worn port names an fd, which means nothing in a new process -- a woken task wears the console (the parked ring's rule)
 // sp stays at ai_ini's topof(g) (empty ai stack); the dispatch re-establishes ip
 g->major_live0 = nw, g->since_major = 0;
 // the rem set names the heap this wake just freed. ai_ini_m's session has been running
 // and collecting all along -- the image arrives into a heap with a history -- so every
 // remembered address points into the major pool freed above, and the first minor walks
 // one. gen_major clears the set for the same reason and had been supplying that clear
 // by accident, which is the whole of what a forced first major was ever doing here.
 g->rem_n = 0, g->rem_miss = 0;
 // seed the nursery against the live set the image arrives with: the resize controller
 // otherwise ramps from the bare floor a doubling -- and a collection -- at a time,
 // and a woken runtime already knows how much it will be scanning past.
 { uintptr_t want = nw >> 1;
   if (want > (uintptr_t) g->len) { struct ai *h = gen_grow(g, want); if (ai_ok(h)) g = h; } }
 return g; }
struct ai *ai_image_load_m(void const *buf, uintptr_t len, void *(*al)(struct ai*, void*, size_t)) {
 return img_wake(buf, len, al); }
struct ai *ai_image_load(void const *buf, uintptr_t len) { return ai_image_load_m(buf, len, ai_libc_alloc); }

