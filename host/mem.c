// host/mem.c -- raw memory and the cask windows onto it: memfd + mmap (the
// shared buffers haven's clients pass by fd), the copies between a mapping and
// a cask, a generic ioctl (the struct rides in a cask, read and written in
// place -- peek/poke-class honesty, the caller keeps its layouts straight), an
// OFFSET mmap (a DRM dumb buffer maps at the magic offset MAP_DUMB hands back),
// and 8-byte word slots on a cask (the flat solver's state, crew/sat/flat.l).
// Host-only, auto-globbed + AI_NIF-registered (no love.c/love.h/main.c edit).
// crew/haven/drm.l speaks the actual DRM vocabulary in love over these doors.
//
//   (ioctl fd req buf)  -> result charm | negative -errno ; fd a charm or an
//                          open port; buf a cask (the struct, in place) or a
//                          charm (an int-argument ioctl -- the console kind;
//                          0 covers the no-argument kind)
//   (mapfdo fd n off)   -> ptr charm | negative -errno (mmap RW shared at off)
//   (memfd n)           -> fd charm | negative -errno (sealed-size memory)
//   (mapfd fd n)        -> ptr charm | negative -errno (mmap RW shared)
//   (unmap ptr n)       -> ()
//   (mapin  dst doff ptr soff n) -> dst : mapping -> cask (compositing)
//   (mapout ptr doff src soff n) -> () : cask/string -> mapping (a brush)
//   (peepw c i)         -> the word at slot i, low 4 bytes | () misuse
//   (pinw c i v)        -> lay v zero-extended into slot i -> c | () misuse
//
// the mapping pointer is a bare charm: peek/poke-class honesty -- the app
// keeps its sizes straight, the cask side is bounds-clamped. peepw/pinw are
// value ops, so absence/misuse answers (); flat.l binds getw/putw to them when
// they are in the book, the byte path staying the portable twin (freestanding
// targets have no host glob).
#define _GNU_SOURCE     // memfd_create
#include "love.h"
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <errno.h>

// a cask's (or string's) backing bytes, or 0.
static struct ai_str *cask_bytes(ai_word x) {
 if (x & 1) return 0;
 if (((union u*) x)->ap == lvm_cask) return ((struct ai_cask*) x)->str;
 return ai_strp(x) ? (struct ai_str*) x : 0; }

// an fd charm, or an open port's fd -- the device rides either way
static intptr_t any_fd(ai_word x) {
 if (x & 1) return getcharm(x);
 if (((union u*) x)->ap == lvm_port_io) return ai_io_fd((struct ai_io*) x);
 return -1; }

static lvm(lvm_ioctl) {
 intptr_t fd = any_fd(Sp[0]);
 uintptr_t req = (Sp[1] & 1) ? (uintptr_t) getcharm(Sp[1]) : 0;
 struct ai_str *b = cask_bytes(Sp[2]);
 void *arg = b ? (void*) b->bytes
                : (Sp[2] & 1) ? (void*) getcharm(Sp[2]) : 0;
 ai_word out;
 if (fd < 0) out = putcharm(-EINVAL);
 else {
  int r = ioctl((int) fd, (unsigned long) req, arg);
  out = putcharm(r < 0 ? -errno : r); }
 Sp[2] = out;
 Sp += 2; Ip += 1; ai_musttail return Continue(); }

static lvm(lvm_mapfdo) {
 intptr_t fd  = any_fd(Sp[0]),
           n   = (Sp[1] & 1) ? getcharm(Sp[1]) : -1,
           off = (Sp[2] & 1) ? getcharm(Sp[2]) : -1;
 ai_word out = putcharm(-EINVAL);
 if (fd >= 0 && n > 0 && off >= 0) {
  void *p = mmap(0, (size_t) n, PROT_READ | PROT_WRITE, MAP_SHARED,
                   (int) fd, (off_t) off);
  out = p == MAP_FAILED ? putcharm(-errno) : putcharm((intptr_t) p); }
 Sp[2] = out;
 Sp += 2; Ip += 1; ai_musttail return Continue(); }

// (memfd n): anonymous shared memory of n bytes, by fd -- what a client
// builds its pool from.
static lvm(lvm_memfd) {
 intptr_t n = (Sp[0] & 1) ? getcharm(Sp[0]) : -1;
 ai_word out = putcharm(-1);
 if (n > 0) {
  int fd = memfd_create("haven", MFD_CLOEXEC);
  if (fd < 0) out = putcharm(-errno);
  else if (ftruncate(fd, n)) { out = putcharm(-errno); close(fd); }
  else out = putcharm(fd); }
 Sp[0] = out;
 Ip += 1; ai_musttail return Continue(); }

// (mapfd fd n): the fd's memory, mapped shared read/write.
static lvm(lvm_mapfd) {
 intptr_t fd = (Sp[0] & 1) ? getcharm(Sp[0]) : -1,
           n = (Sp[1] & 1) ? getcharm(Sp[1]) : -1;
 ai_word out = putcharm(-1);
 if (fd >= 0 && n > 0) {
  void *p = mmap(0, (size_t) n, PROT_READ | PROT_WRITE, MAP_SHARED, (int) fd, 0);
  out = p == MAP_FAILED ? putcharm(-errno) : putcharm((intptr_t) p); }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

static lvm(lvm_unmap) {
 intptr_t p = (Sp[0] & 1) ? getcharm(Sp[0]) : 0,
           n = (Sp[1] & 1) ? getcharm(Sp[1]) : 0;
 if (p && n > 0) munmap((void*) p, (size_t) n);
 Sp[1] = ZeroPoint;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

// (mapin dst doff ptr soff n): mapping -> cask, the compositing read.
static lvm(lvm_mapin) {
 struct ai_str *d = cask_bytes(Sp[0]);
 intptr_t doff = (Sp[1] & 1) ? getcharm(Sp[1]) : -1,
           p = (Sp[2] & 1) ? getcharm(Sp[2]) : 0,
           soff = (Sp[3] & 1) ? getcharm(Sp[3]) : -1,
           n = (Sp[4] & 1) ? getcharm(Sp[4]) : -1;
 if (d && !(Sp[0] & 1) && ((union u*) Sp[0])->ap == lvm_cask
      && p && doff >= 0 && soff >= 0 && n > 0
      && (uintptr_t) (doff + n) <= d->len)
    memcpy(d->bytes + doff, (char const*) p + soff, (size_t) n);
 Sp[4] = Sp[0];
 Sp += 4; Ip += 1; ai_musttail return Continue(); }

// (mapout ptr doff src soff n): cask/string -> mapping, the client's brush.
static lvm(lvm_mapout) {
 intptr_t p = (Sp[0] & 1) ? getcharm(Sp[0]) : 0,
           doff = (Sp[1] & 1) ? getcharm(Sp[1]) : -1;
 struct ai_str *s = cask_bytes(Sp[2]);
 intptr_t soff = (Sp[3] & 1) ? getcharm(Sp[3]) : -1,
           n = (Sp[4] & 1) ? getcharm(Sp[4]) : -1;
 if (p && s && doff >= 0 && soff >= 0 && n > 0
      && (uintptr_t) (soff + n) <= s->len)
    memcpy((char*) p + doff, s->bytes + soff, (size_t) n);
 Sp[4] = ZeroPoint;
 Sp += 4; Ip += 1; ai_musttail return Continue(); }

// --- 8-byte word slots on a cask, low 4 bytes live (the flat solver's state:
// the byte-at-a-time accessors were 4 dispatches per read) ----------------------
static lvm(lvm_peepw) {
 ai_word c = Sp[0], out = ZeroPoint;
 if (!(c & 1) && ((union u*) c)->ap == lvm_cask && (Sp[1] & 1)) {
  intptr_t i = getcharm(Sp[1]);
  struct ai_str *s = ((struct ai_cask*) c)->str;
  if (i >= 0 && (uintptr_t) (i + 1) * 8 <= s->len) {
   uint64_t w;
   memcpy(&w, s->bytes + 8 * i, 8);
   out = putcharm((intptr_t) (w & 0xffffffffu)); } }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

static lvm(lvm_pinw) {
 ai_word c = Sp[0], out = ZeroPoint;
 if (!(c & 1) && ((union u*) c)->ap == lvm_cask && (Sp[1] & 1) && (Sp[2] & 1)) {
  intptr_t i = getcharm(Sp[1]);
  uint64_t v = (uint64_t) getcharm(Sp[2]) & 0xffffffffu;
  struct ai_str *s = ((struct ai_cask*) c)->str;
  if (i >= 0 && (uintptr_t) (i + 1) * 8 <= s->len) {
   memcpy(s->bytes + 8 * i, &v, 8);
   out = c; } }
 Sp[2] = out;
 Sp += 2; Ip += 1; ai_musttail return Continue(); }

static union u const
  nif_ioctl[]  = {{lvm_cur}, {.x = putcharm(3)}, {lvm_ioctl}, {lvm_ret0}},
  nif_mapfdo[] = {{lvm_cur}, {.x = putcharm(3)}, {lvm_mapfdo}, {lvm_ret0}},
  nif_memfd[]  = {{lvm_memfd}, {lvm_ret0}},
  nif_mapfd[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_mapfd}, {lvm_ret0}},
  nif_unmap[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_unmap}, {lvm_ret0}},
  nif_mapin[]  = {{lvm_cur}, {.x = putcharm(5)}, {lvm_mapin}, {lvm_ret0}},
  nif_mapout[] = {{lvm_cur}, {.x = putcharm(5)}, {lvm_mapout}, {lvm_ret0}},
  nif_peepw[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_peepw}, {lvm_ret0}},
  nif_pinw[]   = {{lvm_cur}, {.x = putcharm(3)}, {lvm_pinw},  {lvm_ret0}};
AI_NIF("ioctl", nif_ioctl);
AI_NIF("mapfdo", nif_mapfdo);
AI_NIF("memfd", nif_memfd);
AI_NIF("mapfd", nif_mapfd);
AI_NIF("unmap", nif_unmap);
AI_NIF("mapin", nif_mapin);
AI_NIF("mapout", nif_mapout);
AI_NIF("peepw", nif_peepw);
AI_NIF("pinw",  nif_pinw);
