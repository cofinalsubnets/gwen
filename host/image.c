// host/image.c -- file I/O around the core's stdio-free image codec (ai_image_save /
// ai_image_load, love.c). The CORE owns the heap serialization (compact + range-encode a
// {header, blob} buffer, and its inverse); the HOST owns stdio -- so love.c stays
// freestanding-clean. main.c calls image_bake (--bake: lay the image back into the
// binary's own .image section), image_dump (--bake PATH: write a plain image file), and
// image_load (--wake PATH). Conventions: bake/dump 0 ok / <0 error; load NULL on any
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

// the wake-safety guard: a kept-absolute pointer only survives a
// wake if it aims inside the MAIN PROGRAM's load segments (one ASLR base delta shifts
// them all). Anything else -- a JIT W^X page, an mmap, a shared library -- dies with
// the bake process, and every post-wake use is a hardware fault the barrier eats per
// call: the storm. Collect the segments once, install the predicate before any dump,
// and on a refused bake print the offenders so the survivor names itself.
extern uintptr_t (*ai_image_absguard)(uintptr_t);
extern uintptr_t ai_image_bad[8];
extern uintptr_t ai_image_nbad;
static struct { uintptr_t lo, hi; } image_segs[16];
static int image_nsegs = 0;
static int image_seg_phdr(struct dl_phdr_info *in, size_t sz, void *d) {
  for (int i = 0; i < in->dlpi_phnum && image_nsegs < 16; i++) {
    const ElfW(Phdr) *p = &in->dlpi_phdr[i];
    if (p->p_type == PT_LOAD) {
      image_segs[image_nsegs].lo = in->dlpi_addr + p->p_vaddr;
      image_segs[image_nsegs].hi = in->dlpi_addr + p->p_vaddr + p->p_memsz;
      image_nsegs++; } }
  return 1;                                       // first object only: the main program
}
static uintptr_t image_abs_ok(uintptr_t v) {
  for (int i = 0; i < image_nsegs; i++)
    if (v >= image_segs[i].lo && v < image_segs[i].hi) return 1;
  return 0;
}
static void image_guard_arm(void) {
  if (!image_nsegs) dl_iterate_phdr(image_seg_phdr, NULL);
  ai_image_nbad = 0;
  ai_image_absguard = image_abs_ok;
}
static void image_guard_report(void) {
  if (!ai_image_nbad) return;
  fprintf(stderr, "love: bake refused -- %lu un-wakeable absolute pointer(s) in the live heap\n",
          (unsigned long) ai_image_nbad);
  for (uintptr_t i = 0; i < ai_image_nbad && i < 2; i++)
    fprintf(stderr, "love:   offender %lu: value %p in object at heap word %lu (object hot %p) -- JIT/W^X/mmap; see doc/wake-storm.md\n",
            (unsigned long) i, (void*) ai_image_bad[4 * i + 1],
            (unsigned long) ai_image_bad[4 * i], (void*) ai_image_bad[4 * i + 2]);
}

int image_dump(struct ai *g, char const *path) {
  image_guard_arm();
  uintptr_t len = 0;
  void *buf = ai_image_save(g, &len);             // g->alloc'd; --bake exits right after, so we don't free it
  if (!buf) { image_guard_report(); return -2; }
  FILE *f = fopen(path, "wb");
  int rc = !f ? -4 : (fwrite(buf, 1, len, f) == len) ? 0 : -4;
  if (f) fclose(f);
  return rc;
}

// image_bake -- the SELF-bake: lay the post-warm image into the running binary's own
// .image section on disk (what the Makefile's objdump/truncate/objcopy pipeline did).
// ETXTBSY-proof by the adopt pattern (port/inle/serve.l): you cannot write your own
// executing file, so copy it, lay the blob in, fsync, and atomically RENAME over the
// original -- a new inode, so anything still executing keeps the old one. Same build =
// same layout, so the codec's anchor/refsym guards hold by construction.
//
// TWO LANES, picked by reading the binary's own ELF, never by a build flag: the image
// is bytes we have to PUT somewhere, and where it can go is a property of the file.
//  * GROW (bake_tail) -- .image is the last allocated thing, alone in the highest
//    PT_LOAD (host/build.mk's --section-start). The blob is APPENDED at the first page
//    past every other allocated byte and the one phdr + one shdr that name it are
//    rewritten to say so. No reserve, no ceiling. Nothing else moves -- no vaddr
//    changes at all -- so the anchor/refsym deltas the wake checks still hold.
//  * RESERVE (bake_reserve) -- .image is a fixed array somewhere in the middle: fill it
//    in place, zero the rest, and error if the image outgrew it. The older shape, still
//    what the mooncc/holo lane lays until its linker grows the tail segment too.
// A binary laid for GROW takes the grow lane; anything else falls back, so one bake
// serves both and neither lane needs to be told which it is.
extern uint64_t ai_baked_image[];
extern uintptr_t ai_baked_image_len;
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
static char bake_buf[1 << 20];                    // the copy/pad scratch, shared by both lanes
// move n bytes src@soff -> dst@doff. the two lanes only ever shuttle bytes.
static int bake_move(int src, int dst, uint64_t soff, uint64_t doff, uint64_t n) {
  for (uint64_t z = 0; z < n; ) {
    size_t w = n - z < sizeof bake_buf ? (size_t)(n - z) : sizeof bake_buf;
    if (pread(src, bake_buf, w, (off_t)(soff + z)) != (ssize_t) w) return -6;
    if (pwrite(dst, bake_buf, w, (off_t)(doff + z)) != (ssize_t) w) return -6;
    z += w; }
  return 0;
}
static int bake_zero(int dst, uint64_t doff, uint64_t n) {
  memset(bake_buf, 0, sizeof bake_buf < n ? sizeof bake_buf : (size_t) n);
  for (uint64_t z = 0; z < n; ) {
    size_t w = n - z < sizeof bake_buf ? (size_t)(n - z) : sizeof bake_buf;
    if (pwrite(dst, bake_buf, w, (off_t)(doff + z)) != (ssize_t) w) return -6;
    z += w; }
  return 0;
}

// the GROW lane. 0 laid, >0 "this binary is not laid for growth -- take the reserve",
// <0 a real failure. Everything it needs it reads out of the file: no build flag says
// which shape this is, the section headers do.
static int bake_tail(int src, char const *tmp, void const *buf, uintptr_t len,
                     uint64_t lenoff, mode_t mode) {
  Elf64_Ehdr eh;
  Elf64_Shdr *sh = NULL;
  Elf64_Phdr *ph = NULL;
  char *str = NULL;
  size_t nsh, nph, si = 0, pi;
  uint64_t head, off, cur, al;
  int dst = -1, rc = 1;
  if (pread(src, &eh, sizeof eh, 0) != (ssize_t) sizeof eh) return -6;
  if (memcmp(eh.e_ident, ELFMAG, SELFMAG) || eh.e_ident[EI_CLASS] != ELFCLASS64
      || eh.e_shentsize != sizeof(Elf64_Shdr) || eh.e_phentsize != sizeof(Elf64_Phdr)
      || eh.e_shnum < 2 || !eh.e_phnum || eh.e_shstrndx >= eh.e_shnum) return 1;
  nsh = eh.e_shnum, nph = eh.e_phnum;
  sh = malloc(nsh * sizeof *sh), ph = malloc(nph * sizeof *ph);
  if (!sh || !ph) { rc = -6; goto out; }
  if (pread(src, sh, nsh * sizeof *sh, (off_t) eh.e_shoff) != (ssize_t)(nsh * sizeof *sh)
      || pread(src, ph, nph * sizeof *ph, (off_t) eh.e_phoff) != (ssize_t)(nph * sizeof *ph))
    { rc = -6; goto out; }
  if (!(str = malloc(sh[eh.e_shstrndx].sh_size + 1))) { rc = -6; goto out; }
  if (pread(src, str, sh[eh.e_shstrndx].sh_size, (off_t) sh[eh.e_shstrndx].sh_offset)
      != (ssize_t) sh[eh.e_shstrndx].sh_size) { rc = -6; goto out; }
  str[sh[eh.e_shstrndx].sh_size] = 0;
  for (size_t i = 1; i < nsh; i++)
    if (sh[i].sh_name < sh[eh.e_shstrndx].sh_size && !strcmp(str + sh[i].sh_name, ".image")) { si = i; break; }
  if (!si) goto out;                              // no .image section at all
  for (pi = 0; pi < nph; pi++)
    if (ph[pi].p_type == PT_LOAD && ph[pi].p_vaddr == sh[si].sh_addr) break;
  if (pi == nph) goto out;                        // .image does not head a segment of its own
  // HEAD: every byte that must stay exactly where it is -- the headers, every other
  // allocated section, and anything non-allocated that happens to sit among them. The
  // blob starts at the first page past it; the non-allocated tail relays after the blob.
  head = eh.e_phoff + (uint64_t) nph * sizeof(Elf64_Phdr);
  if (head < sizeof eh) head = sizeof eh;
  for (size_t i = 1; i < nsh; i++) {
    if (i == si || sh[i].sh_type == SHT_NOBITS || !(sh[i].sh_flags & SHF_ALLOC)) continue;
    if (sh[i].sh_addr > sh[si].sh_addr) goto out; // something allocated ABOVE the image: not the tail
    if (sh[i].sh_offset + sh[i].sh_size > head) head = sh[i].sh_offset + sh[i].sh_size; }
  for (int again = 1; again; ) {                  // a non-allocated section straddling the cut joins the head
    again = 0;
    for (size_t i = 1; i < nsh; i++)
      if (sh[i].sh_type != SHT_NOBITS && sh[i].sh_offset < head
          && sh[i].sh_offset + sh[i].sh_size > head && i != si)
        head = sh[i].sh_offset + sh[i].sh_size, again = 1; }
  al = ph[pi].p_align ? ph[pi].p_align : 4096;
  off = (head + al - 1) / al * al;
  if ((off - ph[pi].p_vaddr) % al) goto out;      // the loader's offset/vaddr congruence: refuse, never lie
  if ((dst = open(tmp, O_WRONLY | O_CREAT | O_TRUNC, 0700)) < 0) { rc = -6; goto out; }
  if ((rc = bake_move(src, dst, 0, 0, head))) goto out;
  if ((rc = bake_zero(dst, head, off - head))) goto out;
  if (pwrite(dst, buf, len, (off_t) off) != (ssize_t) len) { rc = -6; goto out; }
  cur = off + len;
  for (size_t i = 1; i < nsh; i++) {              // the non-allocated tail, relaid past the blob
    uint64_t a;
    if (i == si || sh[i].sh_type == SHT_NOBITS || (sh[i].sh_flags & SHF_ALLOC)) continue;
    if (sh[i].sh_offset < head) continue;         // it rode along inside the head
    a = sh[i].sh_addralign ? sh[i].sh_addralign : 1;
    cur = (cur + a - 1) / a * a;
    if ((rc = bake_move(src, dst, sh[i].sh_offset, cur, sh[i].sh_size))) goto out;
    sh[i].sh_offset = cur;
    cur += sh[i].sh_size; }
  sh[si].sh_offset = off, sh[si].sh_size = len;   // the two records that now describe the image
  ph[pi].p_offset = off, ph[pi].p_filesz = len, ph[pi].p_memsz = len;
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
  free(sh), free(ph), free(str);
  return rc;
}

// the RESERVE lane: the image lands INSIDE a fixed array, and an image too big for it
// is an error with the one knob to turn. `off` is the reserve's file offset.
static int bake_reserve(int src, char const *tmp, void const *buf, uintptr_t len,
                        uint64_t off, mode_t mode) {
  struct stat st;
  int dst, rc;
  if (len > ai_baked_image_len) {
    fprintf(stderr, "love: image %lu > .image reserve %lu -- bump RESERVE_WORDS in host/image_baked.c\n",
            (unsigned long) len, (unsigned long) ai_baked_image_len);
    return -3; }
  if (fstat(src, &st)) return -6;
  if ((dst = open(tmp, O_WRONLY | O_CREAT | O_TRUNC, 0700)) < 0) return -6;
  rc = bake_move(src, dst, 0, 0, (uint64_t) st.st_size);   // the whole exe; the running inode stays untouched
  if (!rc && pwrite(dst, buf, len, (off_t) off) != (ssize_t) len) rc = -6;
  if (!rc) rc = bake_zero(dst, off + len, ai_baked_image_len - len);   // zero the rest of the reserve
  if (!rc && (fchmod(dst, mode) || fsync(dst))) rc = -6;
  if (close(dst)) rc = -6;
  return rc;
}

int image_bake(struct ai *g) {
  image_guard_arm();
  uintptr_t len = 0;
  void *buf = ai_image_save(g, &len);
  // the codec silently reverts any would-be-dead native reference to the bytecode
  // twin the cell carries (ai_image_redir); the bake stays correct, so there is
  // nothing to announce. only a REFUSED bake (below) is worth a word.
  if (!buf) { image_guard_report(); return -2; }
  // both lanes patch by FILE OFFSET, and the offsets come from the running program's
  // own phdrs (dl_iterate_phdr, first object) -- the one place a live address and a
  // file position are known to name the same byte.
  struct bake_at b = { (uintptr_t) ai_baked_image, 0, 0 };
  struct bake_at bl = { (uintptr_t) &ai_baked_image_len, 0, 0 };
  dl_iterate_phdr(bake_phdr, &b);
  dl_iterate_phdr(bake_phdr, &bl);
  if (!b.found || !bl.found) return -5;
  char exe[4096], tmp[4104];
  ssize_t n = readlink("/proc/self/exe", exe, sizeof exe - 1);
  if (n <= 0) return -6;
  exe[n] = 0;
  snprintf(tmp, sizeof tmp, "%s.bake", exe);
  struct stat st;
  int src = open(exe, O_RDONLY);
  if (src < 0 || fstat(src, &st)) { if (src >= 0) close(src); return -6; }
  int rc = bake_tail(src, tmp, buf, len, bl.off, st.st_mode & 07777);
  if (rc > 0) rc = bake_reserve(src, tmp, buf, len, b.off, st.st_mode & 07777);
  close(src);
  if (!rc && rename(tmp, exe)) rc = -6;           // the adopt: atomic, a new inode
  if (rc) unlink(tmp);
  return rc;
}

// (bake path) -- snapshot the LIVE session to an image file, mid-eval: the running
// stack's objects ride into the blob as wake-unreachable ballast and the load side
// resets sp/ip, so `love --wake path prog.l ..` boots a session carrying every global
// this one had pinned (an app baked warm: the mooncc image erases its per-run load).
// natives cannot serialize -- the post.l wrapper empties the glaze compile cache
// first (they re-JIT lazily in the woken session); any OTHER live native closure at
// bake time is on the caller. answers 1 | ().
// the frame-heavy body lives in a plain helper: path[4096] + &len escape (to
// fopen / ai_image_save_) and pin the frame, which would defeat the lvm_ ap's
// tail-jump (make vmret). the helper runs after Pack(g), on g->sp; the wrapper
// stays a thin sibcall. answers the result word (1 | ()).
static ai_noinline ai_word image_bake_do(struct ai *g) {
 if (!ai_strp(g->sp[0])) return ai_nil;
 struct ai_str *s = (struct ai_str*) g->sp[0];
 char path[4096];
 if (s->len >= sizeof path) return ai_nil;
 memcpy(path, s->bytes, s->len);                 // copy OUT first: the dump's gen_major moves the string
 path[s->len] = 0;
 image_guard_arm();
 uintptr_t len = 0;
 void *buf = ai_image_save_(g, &len);
 if (!buf) { image_guard_report(); return ai_nil; }
 FILE *f = fopen(path, "wb");
 int rc = !f ? -1 : (fwrite(buf, 1, len, f) == len) ? 0 : -1;
 if (f) fclose(f);
 g->alloc(g, buf, 0);                            // a session lives on after a bake: no leak
 return rc ? ai_nil : putcharm(1); }
static lvm(lvm_bake) {
 Pack(g);
 ai_word r = image_bake_do(g);
 Unpack(g);
 Sp[0] = r; Ip += 1;
 return Continue(); }
static union u const nif_bake[] = {{lvm_bake}, {lvm_ret0}};
AI_NIF("bake", nif_bake);

struct ai *image_load(char const *path) {
  int fd = open(path, O_RDONLY);
  if (fd < 0) return NULL;
  struct stat st;
  struct ai *g = NULL;
  if (!fstat(fd, &st) && st.st_size > 0) {        // map, don't read: the core copies the blob straight
    size_t n = (size_t) st.st_size;               // out of the page cache -- one pass, no file buffer
    void *buf = mmap(NULL, n, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0);
    if (buf != MAP_FAILED) {
      g = ai_image_load(buf, (uintptr_t) n);
      munmap(buf, n); } }
  close(fd);
  return g;
}
