// host/image.c -- file I/O around the core's stdio-free image codec (ai_image_save /
// ai_image_load, love.c). the core owns the heap serialization (compact + range-encode a
// {header, blob} buffer, and its inverse); the host owns stdio -- so love.c stays
// freestanding-clean. main.c calls image_bake (bake: lay the image back into the
// binary's own .image section), image_dump (bake PATH: write a plain image file), and
// image_load (wake PATH). conventions: bake/dump 0 ok / <0 error; load NULL on any
// problem so the caller falls back to a normal egg boot.
#define _GNU_SOURCE
#include "love.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <link.h>

extern size_t host_selfpath(char*, size_t);       // host/posix.c: the one selfpath door (per-OS ladder)

// the wake-safety guard: a kept-absolute pointer only survives a wake if it aims inside
// the main program's load segments (one ASLR base delta shifts them all). anything else --
// a JIT W^X page, an mmap, a shared library -- dies with the bake process, so the dump
// refuses it. the bounds ride the caller's frame and reach the codec by parameter: one
// phdr walk per bake, and the audit keeps no state between them.
struct image_segs { struct { uintptr_t lo, hi; } s[16]; int n; };
static int image_seg_phdr(struct dl_phdr_info *in, size_t sz, void *d) {
  struct image_segs *q = d;
  (void) sz;
  for (int i = 0; i < in->dlpi_phnum && q->n < 16; i++) {
    const ElfW(Phdr) *p = &in->dlpi_phdr[i];
    if (p->p_type == PT_LOAD) {
      q->s[q->n].lo = in->dlpi_addr + p->p_vaddr;
      q->s[q->n].hi = in->dlpi_addr + p->p_vaddr + p->p_memsz;
      q->n++; } }
  return 1;                                       // first object only: the main program
}
static uintptr_t image_abs_ok(void *ctx, uintptr_t v, uintptr_t off, uintptr_t ap) {
  struct image_segs *q = ctx;
  (void) off, (void) ap;
  for (int i = 0; i < q->n; i++)
    if (v >= q->s[i].lo && v < q->s[i].hi) return 1;
  return 0;
}
// segs must outlive the dump: it is what the guard reads. keep it in the frame that
// makes the ai_image_save call, never a temporary.
static struct ai_image_guard image_guard(struct image_segs *segs) {
  segs->n = 0;
  dl_iterate_phdr(image_seg_phdr, segs);
  struct ai_image_guard gd = { image_abs_ok, segs };
  return gd;
}

// `bake PATH` lays the image directly executable -- ./img runs `love wake ./img args..`,
// which is the one shape a shebang can carry (a single argument, then the file itself).
// `env -S` rather than a baked-in path, the same spelling the tree's other shebang tools
// install with, so the image follows love on PATH instead of pinning one build.
// always written, never a flag: a #! line the reader steps over costs the file 32 bytes
// and an option costs every caller a decision it has no grounds to make. reading is the
// half that stays tolerant -- an image with no shebang loads exactly as it always did,
// which is what keeps an already-dumped one working across this change.
// padded to a word. the core reads its header at offset 0 of whatever it is handed
// and takes the blob as `word*` straight after; an odd-length line would hand it an
// unaligned header, which x86 tolerates and aarch64 faults on. spaces before the
// newline cost nothing and keep every later field where the codec expects it.
// and the load side skips it in the host, never the core: a shebang is a POSIX exec
// convention, and love.c stays freestanding-clean. the .image section lane never has one.
#define ImageShebang "#!/usr/bin/env -S love wake"
static size_t image_shebang(char *sb, size_t cap) {
  size_t n = (size_t) snprintf(sb, cap, "%s", ImageShebang);
  while ((n + 1) % sizeof(uintptr_t)) sb[n++] = ' ';
  sb[n++] = '\n';
  return n;
}
int image_dump(struct ai *g, char const *path) {
  struct image_segs segs;
  struct ai_image_guard gd = image_guard(&segs);
  uintptr_t len = 0;
  void *buf = ai_image_save(g, &len, &gd);        // g->alloc'd; bake exits right after, so we don't free it
  if (!buf) return -2;
  char sb[64];
  size_t sn = image_shebang(sb, sizeof sb);
  FILE *f = fopen(path, "wb");
  int rc = !f ? -4
         : (fwrite(sb, 1, sn, f) != sn) ? -4
         : (fwrite(buf, 1, len, f) == len) ? 0 : -4;
  if (f) fclose(f);
  if (!rc) {                                      // an executable image, or the #! is decoration
    struct stat st;
    if (!stat(path, &st)) chmod(path, (st.st_mode | 0111) & 07777); }
  return rc;
}

// image_bake -- the self-bake: lay the post-warm image into the running binary's own
// .image section on disk. ETXTBSY-proof by the adopt pattern: you cannot write your own
// executing file, so copy it, lay the blob in, fsync, and atomically rename over the
// original -- a new inode, so anything still executing keeps the old one. same build =
// same layout, so the codec's anchor/refsym guards hold by construction.
//
// the image is bytes we have to put somewhere, and .image is laid last so there is room:
// the blob is appended where the section already sits and the one phdr + one shdr that
// name it are rewritten to say how far it now reaches. no reserve, no ceiling. nothing
// else in the file moves -- no vaddr changes at all -- so the anchor/refsym deltas the
// wake checks still hold. what bake_tail requires is read off the binary's own section
// headers, never told to it by a build flag, and it is one thing: .image ends the segment
// that carries it. true of a section alone in the highest PT_LOAD (host/build.mk's
// --section-start, ld and lld both) and of one riding the tail of the single segment holo
// lays. a link that laid it anywhere else is refused loudly -- there is nowhere to grow,
// and quietly booting the egg forever is not a kindness.
// the in-binary home of the post-boot heap image (doc/misc/snapshot.md): the binary
// loads its own dump at startup (main.c) -- identical layout by construction, so
// the codec's same-binary +delta relocation just works. sentinel-initialized (not
// {0}) so it lands in PROGBITS, patchable in place, never .bss. the section is
// laid last -- alone in the highest segment on the host (host/build.mk's
// --section-start), riding the tail of the single segment holo lays -- so the
// bake grows it: nothing is pre-allocated and there is no ceiling; this stub
// exists only to give the section an address.
#define ReserveWords 2u
__attribute__((section(".love_image"))) uint64_t ai_baked_image[ReserveWords] = {1};
uintptr_t ai_baked_image_len = ReserveWords * 8u;
// and the stub's size is a lie gcc believes: ReserveWords is 2 because the bake grows
// the object, so every read past the second word is out of bounds of the declaration and
// in bounds of the section. main.c says `extern uint64_t ai_baked_image[]` and never
// hears about it; this file holds the sized definition, so it does.
#if defined(__GNUC__) && !defined(__clang__) && !defined(__mooncc__)
#pragma GCC diagnostic ignored "-Warray-bounds"
#endif
// --- the image array ---------------------------------------------------------
// the section holds either one image -- its first word is the codec's own magic,
// which is what every binary before this laid -- or a directory: magic, count,
// then one {off, len, kind, base, verbs} record per image, then the blobs. offsets
// are from the section's first byte.
//
// the entries are laid smallest first and `verbs` names what each can serve, so
// "the first entry that claims this verb" is the walk up the chain, decided
// before anything is woken -- it has to be, since the verb table lives in the
// image we have not woken yet.
//
// an entry is either whole or derived, and that is the whole of the dedup: kind 0 names a
// complete image, kind 1 a derived record whose words are the first nwords of entry
// `base`'s stream plus the few this layer changed (doc/misc/plan/image-chain.md). the codec
// still never sees a container -- it is handed one buffer or a parent/sub pair -- so no
// part of the image format lives here.
#define ImgdirMagic 0x3241594152524119ULL        /* "..ARRAY2", the container's own (derived entries) */
#define ImgdirVerbs 48u
struct image_ent { uint64_t off, len, kind, base; char verbs[ImgdirVerbs]; };
struct image_dir { uint64_t magic, count; struct image_ent ent[]; };
// is `verb` one of the space- or comma-separated words in `list`? a whole-word
// match: "lib" must not claim "libra".
static int imgdir_claims(char const *list, char const *verb) {
  size_t n = strlen(verb);
  for (size_t i = 0; i < ImgdirVerbs && list[i]; ) {
    while (i < ImgdirVerbs && (list[i] == ' ' || list[i] == ',')) i++;
    size_t j = i;
    while (j < ImgdirVerbs && list[j] && list[j] != ' ' && list[j] != ',') j++;
    if (j - i == n && !memcmp(list + i, verb, n)) return 1;
    i = j; }
  return 0;
}
// what to wake for this command line: 1 and the pair filled, or 0 and the caller boots the
// egg. a NULL `*sub` means wake *blob whole, non-NULL means wake that derived record over
// it. a section that is not a directory is one whole image; a torn directory answers 0.
static int imgdir_at(struct image_dir const *d, unsigned char const *base, uintptr_t n,
                     uint64_t i, void const **buf, uintptr_t *len) {
  if (i >= d->count || d->ent[i].off > n || d->ent[i].len > n - d->ent[i].off) return 0;
  return *buf = (void const *) (base + d->ent[i].off), *len = d->ent[i].len, 1;
}
int ai_baked_pick(char const *verb, void const **blob, uintptr_t *blen,
                  void const **sub, uintptr_t *sublen) {
  unsigned char const *base = (unsigned char const *) ai_baked_image;
  uintptr_t n = ai_baked_image_len;
  struct image_dir const *d = (struct image_dir const *) (void const *) base;
  uint64_t pick, i;
  *sub = NULL, *sublen = 0;
  if (n < sizeof *d || d->magic != ImgdirMagic)
    return *blob = (void const *) base, *blen = n, n > 0;
  if (!d->count || n < sizeof *d + d->count * sizeof d->ent[0]) return 0;
  pick = d->count - 1;                            // the largest is the default
  if (verb) for (i = 0; i < d->count; i++)
    if (imgdir_claims(d->ent[i].verbs, verb)) { pick = i; break; }
  if (!d->ent[pick].kind) return imgdir_at(d, base, n, pick, blob, blen);
  // derived: its parent carries the stream. one hop only, since nothing lays a derived
  // parent -- refuse rather than chase.
  if (!imgdir_at(d, base, n, pick, sub, sublen)) return 0;
  if (d->ent[pick].base >= d->count || d->ent[d->ent[pick].base].kind) return 0;
  return imgdir_at(d, base, n, d->ent[pick].base, blob, blen);
}

struct bake_at { uintptr_t addr, off; int found; };
static int bake_phdr(struct dl_phdr_info *in, size_t sz, void *d) {
  struct bake_at *b = d;
  for (int i = 0; i < in->dlpi_phnum; i++) {
    const ElfW(Phdr) *p = &in->dlpi_phdr[i];
    uintptr_t lo = in->dlpi_addr + p->p_vaddr;
    if (p->p_type == PT_LOAD && b->addr >= lo && b->addr < lo + p->p_filesz)
      b->off = p->p_offset + (b->addr - lo), b->found = 1; }
  return 1;                                       // stop after the first object: the main program
}
#define BakeScratch (64u << 10)                  // the copy/pad window: a bake runs once, so iterations are free
// move n bytes src@soff -> dst@doff through the caller's window. the two lanes only shuttle bytes.
static int bake_move(int src, int dst, uint64_t soff, uint64_t doff, uint64_t n, char *win) {
  for (uint64_t z = 0; z < n; ) {
    size_t w = n - z < BakeScratch ? (size_t)(n - z) : BakeScratch;
    if (pread(src, win, w, (off_t)(soff + z)) != (ssize_t) w) return -6;
    if (pwrite(dst, win, w, (off_t)(doff + z)) != (ssize_t) w) return -6;
    z += w; }
  return 0;
}
// lay the image. 0 done, >0 "this binary is not laid for growth", <0 a real failure.
static int bake_tail(struct ai *g, int src, char const *tmp, void const *buf, uintptr_t len,
                     uint64_t lenoff, mode_t mode) {
  Elf64_Ehdr eh;
  Elf64_Shdr *sh = NULL;
  Elf64_Phdr *ph = NULL;
  char *str = NULL, *win = NULL;
  size_t nsh, nph, si = 0, pi;
  uint64_t head, off, cur, al;
  int dst = -1, rc = 1;
  if (pread(src, &eh, sizeof eh, 0) != (ssize_t) sizeof eh) return -6;
  if (memcmp(eh.e_ident, ELFMAG, SELFMAG) || eh.e_ident[EI_CLASS] != ELFCLASS64
      || eh.e_shentsize != sizeof(Elf64_Shdr) || eh.e_phentsize != sizeof(Elf64_Phdr)
      || eh.e_shnum < 2 || !eh.e_phnum || eh.e_shstrndx >= eh.e_shnum) return 1;
  nsh = eh.e_shnum, nph = eh.e_phnum;
  sh = g->alloc(g, NULL, nsh * sizeof *sh), ph = g->alloc(g, NULL, nph * sizeof *ph);
  win = g->alloc(g, NULL, BakeScratch);
  if (!sh || !ph || !win) { rc = -6; goto out; }
  if (pread(src, sh, nsh * sizeof *sh, (off_t) eh.e_shoff) != (ssize_t)(nsh * sizeof *sh)
      || pread(src, ph, nph * sizeof *ph, (off_t) eh.e_phoff) != (ssize_t)(nph * sizeof *ph))
    { rc = -6; goto out; }
  if (!(str = g->alloc(g, NULL, sh[eh.e_shstrndx].sh_size + 1))) { rc = -6; goto out; }
  if (pread(src, str, sh[eh.e_shstrndx].sh_size, (off_t) sh[eh.e_shstrndx].sh_offset)
      != (ssize_t) sh[eh.e_shstrndx].sh_size) { rc = -6; goto out; }
  str[sh[eh.e_shstrndx].sh_size] = 0;
  for (size_t i = 1; i < nsh; i++)
    if (sh[i].sh_name < sh[eh.e_shstrndx].sh_size && !strcmp(str + sh[i].sh_name, ".love_image")) { si = i; break; }
  if (!si) goto out;                              // no .image section at all
  // the blob goes exactly where the section already sits -- the offset never moves, so
  // the loader's offset/vaddr congruence is inherited rather than recomputed, and a
  // rebake lands on its own footprint. what has to be true is only that .image is last:
  // nothing allocated above it, and it ends the segment that carries it, so growing it
  // grows nothing else. that covers a section alone in the highest PT_LOAD (ld's
  // --section-start, the gcc/clang lane) and one riding the tail of the single segment
  // holo lays, with the same arithmetic.
  off = sh[si].sh_offset;
  for (size_t i = 1; i < nsh; i++) {
    if (i == si || sh[i].sh_type == SHT_NOBITS || !(sh[i].sh_flags & SHF_ALLOC)) continue;
    if (sh[i].sh_addr > sh[si].sh_addr || sh[i].sh_offset > off) goto out; }
  for (pi = 0; pi < nph; pi++)
    if (ph[pi].p_type == PT_LOAD && ph[pi].p_vaddr <= sh[si].sh_addr
        && sh[si].sh_addr + sh[si].sh_size == ph[pi].p_vaddr + ph[pi].p_memsz) break;
  if (pi == nph) goto out;                        // .image does not end a segment: not the tail
  for (size_t i = 0; i < nph; i++)                // ..and no other segment lives above it
    if (i != pi && ph[i].p_type == PT_LOAD && ph[i].p_vaddr > ph[pi].p_vaddr) goto out;
  head = off;                                     // everything below the blob stays put, byte for byte
  al = sh[si].sh_addr - ph[pi].p_vaddr;           // the image's own start within its segment
  if ((dst = open(tmp, O_WRONLY | O_CREAT | O_TRUNC, 0700)) < 0) { rc = -6; goto out; }
  if ((rc = bake_move(src, dst, 0, 0, head, win))) goto out;
  if (pwrite(dst, buf, len, (off_t) off) != (ssize_t) len) { rc = -6; goto out; }
  cur = off + len;
  for (size_t i = 1; i < nsh; i++) {              // the non-allocated tail, relaid past the blob
    uint64_t a;
    if (i == si || sh[i].sh_type == SHT_NOBITS || (sh[i].sh_flags & SHF_ALLOC)) continue;
    if (sh[i].sh_offset < head) continue;         // it rode along inside the head
    a = sh[i].sh_addralign ? sh[i].sh_addralign : 1;
    cur = (cur + a - 1) / a * a;
    if ((rc = bake_move(src, dst, sh[i].sh_offset, cur, sh[i].sh_size, win))) goto out;
    sh[i].sh_offset = cur;
    cur += sh[i].sh_size; }
  sh[si].sh_size = len;                           // the two records that now describe the image
  ph[pi].p_filesz = ph[pi].p_memsz = al + len;    // .image ends the segment, so its growth is the segment's
  eh.e_shoff = cur = (cur + 7) & ~(uint64_t) 7;
  { uintptr_t l = len;                            // ai_baked_image_len: what main.c hands the codec
    if (pwrite(dst, sh, nsh * sizeof *sh, (off_t) cur) != (ssize_t)(nsh * sizeof *sh)
        || pwrite(dst, ph, nph * sizeof *ph, (off_t) eh.e_phoff) != (ssize_t)(nph * sizeof *ph)
        || pwrite(dst, &eh, sizeof eh, 0) != (ssize_t) sizeof eh
        || pwrite(dst, &l, sizeof l, (off_t) lenoff) != (ssize_t) sizeof l) rc = -6; }
 out:
  if (dst >= 0) {
    if (!rc && (fchmod(dst, mode) || fsync(dst))) rc = -6;
    if (close(dst)) rc = -6; }
  g->alloc(g, sh, 0), g->alloc(g, ph, 0), g->alloc(g, str, 0), g->alloc(g, win, 0);
  return rc;
}

// `love bake -L CAT[:verbs] ..` lands here. main.c has already evaluated each layer's cat
// in one session, freezing between them, so what arrives is finished bytes: bufs[i] is a
// derived record for every layer but the last, which is the whole image they derive from.
// this only lays them.
// order is the chain, smallest first, since the picker takes the first entry claiming the
// verb. every derived entry's parent is the last one: each layer froze the one before it,
// and the last dump is the only blob carrying a stream.
int image_bake_layers(struct ai *g, void *const *bufs, uintptr_t const *lens,
                      char *const *verbs, int n) {
  struct image_dir *d = NULL;
  unsigned char *buf = NULL;
  uintptr_t tot = sizeof *d + (uintptr_t) n * sizeof d->ent[0], off = tot;
  int i, rc = -6, src = -1;
  char exe[4096], tmp[4104];
  struct stat st;
  if (n < 1) return -6;
  for (i = 0; i < n; i++) tot += (lens[i] + 7u) & ~(uintptr_t) 7;
  if (!(buf = g->alloc(g, NULL, tot))) return -6;
  memset(buf, 0, tot);
  d = (struct image_dir *) (void *) buf;
  d->magic = ImgdirMagic, d->count = (uint64_t) n;
  for (i = 0; i < n; i++) {
    if (verbs[i]) {
      size_t vl = strlen(verbs[i]);
      if (vl >= ImgdirVerbs) { fprintf(stderr, "love: bake: verb list too long\n"); goto out; }
      memcpy(d->ent[i].verbs, verbs[i], vl); }
    d->ent[i].off = off, d->ent[i].len = (uint64_t) lens[i];
    d->ent[i].kind = i + 1 < n, d->ent[i].base = (uint64_t)(n - 1);
    memcpy(buf + off, bufs[i], lens[i]);
    off += (lens[i] + 7u) & ~(uintptr_t) 7; }
  { struct bake_at bl = { (uintptr_t) &ai_baked_image_len, 0, 0 };
    dl_iterate_phdr(bake_phdr, &bl);
    if (!bl.found) { rc = -5; goto out; }
    if (!host_selfpath(exe, sizeof exe)) goto out;
    snprintf(tmp, sizeof tmp, "%s.bake", exe);
    if ((src = open(exe, O_RDONLY)) < 0 || fstat(src, &st)) goto out;
    rc = bake_tail(g, src, tmp, buf, tot, bl.off, st.st_mode & 07777);
    if (rc > 0) { fprintf(stderr, "love: .image is not laid last -- nowhere to grow the image\n"); rc = -3; }
    if (!rc && rename(tmp, exe)) rc = -6;
    if (rc) unlink(tmp); }
 out:
  if (src >= 0) close(src);
  g->alloc(g, buf, 0);
  return rc;
}

// the layered bake's two codec doors, wrapped so the wake-safety guard -- the host's, and
// riding the caller's frame -- stays in this file. rec/full are g->alloc'd and the caller
// frees them; 0 ok, <0 refused.
int image_freeze(struct ai **g, void **rec, uintptr_t *reclen) {
  struct image_segs segs;
  struct ai_image_guard gd = image_guard(&segs);
  *reclen = 0;
  uint8_t why = 0;
  if ((*rec = ai_image_freeze(g, reclen, &gd, &why))) return 0;
  return fprintf(stderr, "love: bake: the layer refused to freeze (why=%lu)\n",
                 (unsigned long) why), -2;
}
int image_save_over(struct ai *g, void *const *bases, uintptr_t const *blens, uintptr_t nbase,
                    void **subout, uintptr_t *sublens, void **full, uintptr_t *fulllen) {
  struct image_segs segs;
  struct ai_image_guard gd = image_guard(&segs);
  *fulllen = 0;
  uint8_t why = 0;
  if ((*full = ai_image_save_over(g, fulllen, &gd, &why, bases, blens, nbase, subout, sublens))) return 0;
  return fprintf(stderr, "love: bake: the image refused to save (why=%lu)\n",
                 (unsigned long) why), -4;
}

int image_bake(struct ai *g) {
  struct image_segs segs;
  struct ai_image_guard gd = image_guard(&segs);
  uintptr_t len = 0;
  void *buf = ai_image_save(g, &len, &gd);
  // the codec silently reverts any would-be-dead native reference to the bytecode
  // twin the cell carries (ai_image_redir); the bake stays correct, so there is
  // nothing to announce. only a refused bake (below) is worth a word.
  if (!buf) return -2;
  // ai_baked_image_len is patched by FILE offset, and the offset comes from the running
  // program's own phdrs (dl_iterate_phdr, first object) -- the one place a live address
  // and a file position are known to name the same byte.
  struct bake_at bl = { (uintptr_t) &ai_baked_image_len, 0, 0 };
  dl_iterate_phdr(bake_phdr, &bl);
  if (!bl.found) return -5;
  char exe[4096], tmp[4104];
  if (!host_selfpath(exe, sizeof exe)) return -6;
  snprintf(tmp, sizeof tmp, "%s.bake", exe);
  struct stat st;
  int src = open(exe, O_RDONLY);
  if (src < 0 || fstat(src, &st)) { if (src >= 0) close(src); return -6; }
  int rc = bake_tail(g, src, tmp, buf, len, bl.off, st.st_mode & 07777);
  if (rc > 0) {
    fprintf(stderr, "love: .image is not laid last -- nowhere to grow the image\n");
    rc = -3; }
  close(src);
  if (!rc && rename(tmp, exe)) rc = -6;           // the adopt: atomic, a new inode
  if (rc) unlink(tmp);
  return rc;
}

// (bake path) -- snapshot the live session to an image file, mid-eval: the running
// stack's objects ride into the blob as wake-unreachable ballast and the load side
// resets sp/ip, so `love wake path prog.l ..` boots a session carrying every global
// this one had pinned (an app baked warm: the mooncc image erases its per-run load).
// natives cannot serialize -- the glaze's own bake wrapper (love/glaze/hook.l) empties its cache
// first (they re-JIT lazily in the woken session); any other live native closure at
// bake time is on the caller. answers 1 | ().
// the frame-heavy body lives in a plain helper: path[4096] + &len escape (to
// fopen / ai_image_save_) and pin the frame, which would defeat the lvm_ ap's
// tail-jump (make vmret). the helper runs after Pack(g), on g->sp; the wrapper
// stays a thin sibcall. answers the result word (1 | ()).
static ai_noinline ai_word image_bake_do(struct ai *g) {
 if (!ai_strp(g->sp[0])) return ai_zero;
 struct ai_str *s = (struct ai_str*) g->sp[0];
 char path[4096];
 if (s->len >= sizeof path) return ai_zero;
 memcpy(path, s->bytes, s->len);                 // copy out first: the dump's gen_major moves the string
 path[s->len] = 0;
 struct image_segs segs;
 struct ai_image_guard gd = image_guard(&segs);
 uintptr_t len = 0;
 void *buf = ai_image_save_(g, &len, &gd);
 if (!buf) return ai_zero;
 FILE *f = fopen(path, "wb");
 int rc = !f ? -1 : (fwrite(buf, 1, len, f) == len) ? 0 : -1;
 if (f) fclose(f);
 g->alloc(g, buf, 0);                            // a session lives on after a bake: no leak
 return rc ? ai_zero : putcharm(1); }
static lvm(lvm_bake) {
 Pack(g);
 ai_word r = image_bake_do(g);
 Unpack(g);
 Sp[0] = r; Ip += 1;
 ai_musttail return Continue(); }
static union u const nif_bake[] = {{lvm_bake}, {lvm_ret0}};
AiNif("bake", nif_bake);

struct ai *image_load(char const *path) {
  int fd = open(path, O_RDONLY);
  if (fd < 0) return NULL;
  struct stat st;
  struct ai *g = NULL;
  if (!fstat(fd, &st) && st.st_size > 0) {        // map, don't read: the core copies the blob straight
    size_t n = (size_t) st.st_size;               // out of the page cache -- one pass, no file buffer
    void *buf = mmap(NULL, n, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
    if (buf != MAP_FAILED) {
      // step over a `bake -x` shebang, if the image wears one (image_dump pads the line
      // so what follows is still word-aligned). a plain image starts at the magic.
      size_t off = 0;
      if (n > 2 && ((char*) buf)[0] == '#' && ((char*) buf)[1] == '!') {
        char *nl = memchr(buf, '\n', n);
        if (nl) off = (size_t)(nl - (char*) buf) + 1; }
      if (off < n) g = ai_image_load((char*) buf + off, (uintptr_t)(n - off));
      munmap(buf, n); } }
  close(fd);
  return g;
}
