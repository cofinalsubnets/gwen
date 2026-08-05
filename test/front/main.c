// test/front/main.c -- a TEST-ONLY love frontend, and the instrument the io arc
// runs on. It links liblove.a and supplies the frontend contract itself --
// ai_fd_port_vt, ai_ready, the wait hooks, the three static ports -- which is
// exactly what lets it fault the DEVICE without love carrying a fault switch.
//
// The standing rule: love must not gain a feature whose only purpose is letting
// a test break it (doc/io.md, where the deleted LOVE_FAULT_EAGAIN hook is the
// recorded reason). The port vt has always been the frontend's job --
// host/build.mk builds liblove.a from love.c ONLY and links host/*.c direct --
// so a frontend that lies to the runtime is test code, not language surface.
// Nothing in this file is compiled into `love`.
//
// The devices are SYNTHETIC: no OS fd, no pipe, no child. A device is a byte
// queue you feed from .l, so every schedule is deterministic. That is the other
// half of why a real two-process race could not replace the deleted hook -- a
// gate that usually reddens teaches people to ignore it.
//
//   (dev ())      a fresh device port
//   (feed p s)    queue s's bytes on p (s: text, or one charm)
//   (shut p)      after the queue drains, p is at END rather than merely quiet
//   (stall p k)   the next k readn calls answer WOULD-BLOCK -- while `ai_ready`
//                 keeps saying yes. that IS the race defect 5 named: the
//                 readiness check said go and the read said no.
//   (wstall p k)  the next k writen calls land nothing
//   (wcap p k)    each writen lands at most k bytes (0 = no cap)
//   (sent p)      what has been written to p, as text
//   (wpending p)  how many bytes of p's write run the device has not taken --
//                 the number backpressure exists to bound
//   (naps ())     how many times the scheduler has reached its wait -- the gauge
//                 that tells a park from a spin
//
// ⚠ A WAIT WITH NO DEADLINE EXITS 97 rather than sleeping. A synthetic device
// can only be fed by another task, so "every task is parked with no timer" is a
// deadlock by construction -- and a loud exit beats a gate that hangs until the
// harness kills it. That makes this frontend a deadlock detector as well as a
// fault injector, which is most of its value on the rungs after this one.
#include "love.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef EOF
#define EOF (-1)
#endif

// --- the synthetic devices -------------------------------------------------
// fds 0/1/2 are the real console (1 and 2 write, 0 is always at end -- nothing
// here reads the terminal). Every fd from dev_base up is a device, and the
// table GROWS: no cap on how many devices a test may open.
#define dev_base 3

struct dev {
  unsigned char *q;                  // the queue: what the device will hand over
  uintptr_t qlen, qcap, qpos;
  unsigned char *o;                  // what has been written to it
  uintptr_t olen, ocap;
  uintptr_t rstall, wstall, wcap;    // the fault schedule, counted down per call
  int ended; };                      // drained queue reads END, not just quiet

static struct dev *devs;
static int ndev;

static void die(char const *why) {
  fflush(stdout);
  fprintf(stderr, "\n; front: %s\n", why);
  exit(97); }

static struct dev *dev_at(int i) {
  if (i >= ndev) {
    int was = ndev, want = i + 1;
    struct dev *p = realloc(devs, (size_t) want * sizeof *devs);
    if (!p) die("out of memory growing the device table");
    devs = p, ndev = want;
    memset(devs + was, 0, (size_t) (want - was) * sizeof *devs); }
  return devs + i; }

static void grow(unsigned char **b, uintptr_t *cap, uintptr_t want) {
  if (want <= *cap) return;
  uintptr_t c = *cap ? *cap : 64;
  while (c < want) c *= 2;
  unsigned char *p = realloc(*b, c);
  if (!p) die("out of memory growing a device buffer");
  *b = p, *cap = c; }

// the port -> device map is the fd, exactly as it is for a real one.
static struct dev *dev_of_fd(intptr_t fd) {
  return fd >= dev_base && fd - dev_base < ndev ? devs + (fd - dev_base) : NULL; }

static struct dev *dev_of_port(ai_word x) {
  if ((x & 1) || ((union u*) x)->ap != lvm_port_io) return NULL;
  return dev_of_fd(ai_io_fd((struct ai_io*) x)); }

// --- the clock and the waits -----------------------------------------------
uintptr_t ai_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_MONOTONIC, &ts) ? 0
       : (uintptr_t) (ts.tv_sec * 1000u + (uintptr_t) ts.tv_nsec / 1000000u); }

// every wait the scheduler reaches lands here, so counting them here counts them
// all -- see the (naps ()) nif, and the law it is the gauge for.
static uintptr_t naps;

void ai_sleep(uintptr_t ms) {
  if (!ms) die("a sleep with no deadline -- every task is parked");
  naps += 1;
  struct timespec t = { (time_t) (ms / 1000), (long) (ms % 1000) * 1000000L };
  nanosleep(&t, NULL); }

// the readiness law: a NEGATIVE fd is always ready (ai_io_fd answers -1 for
// every port with no device behind it, and those wait on nothing external),
// the console is always ready (end-of-stream IS an answer),
// and a device is ready when it has bytes or has ended.
// ⚠ rstall is NOT consulted here, and that is the whole point: `ai_ready` says
// go and the read says no, which is the one schedule no in-process test could
// otherwise reach.
// ⚠ an OUT park is ready by definition here: this frontend's devices take
// writes through `wstall`, which is a REFUSAL from the write door, not a
// readiness the scheduler can poll for. Only the read direction is a question.
bool ai_ready(int fd, int events) {
  struct dev *d = dev_of_fd(fd);
  if (events != ai_wait_in) return true;
  return d ? (d->qpos < d->qlen || d->ended) : true; }

void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) {
  (void) fds, (void) n;
  ai_sleep(ms); }                    // ms == 0 dies loudly; see the header note

// --- the port vtable -------------------------------------------------------
static intptr_t fd_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  struct dev *d = dev_of_fd(ai_io_fd(g->io));
  if (!d) return -1;                                 // the console never reads
  if (d->rstall) return d->rstall -= 1, 0;           // armed: would-block
  uintptr_t have = d->qlen - d->qpos;
  if (!have) return d->ended ? -1 : 0;
  uintptr_t k = have < n ? have : n;
  memcpy(dst, d->q + d->qpos, k);
  return d->qpos += k, (intptr_t) k; }

static intptr_t fd_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  intptr_t fd = ai_io_fd((*fp)->io);
  struct dev *d = dev_of_fd(fd);
  if (!d) {
    if (fd == 1 || fd == 2) {
      FILE *f = fd == 1 ? stdout : stderr;
      return (intptr_t) fwrite(src, 1, n, f); }
    return (intptr_t) n; }                           // fd 0: swallowed
  if (d->wstall) return d->wstall -= 1, 0;
  uintptr_t k = d->wcap && d->wcap < n ? d->wcap : n;
  grow(&d->o, &d->ocap, d->olen + k);
  memcpy(d->o + d->olen, src, k);
  return d->olen += k, (intptr_t) k; }

static struct ai *fd_flush(struct ai *g) {
  intptr_t fd = ai_io_fd(g->io);
  if (fd == 1) fflush(stdout);
  else if (fd == 2) fflush(stderr);
  return g; }

struct ai_port_vt const ai_fd_port_vt =
 { fd_flush, fd_writen, fd_readn, NULL };

struct ai_fio ai_stdin  = { { lvm_port_io, &ai_fd_port_vt, putcharm(EOF) }, putcharm(0) };
struct ai_fio ai_stdout = { { lvm_port_io, &ai_fd_port_vt, putcharm(EOF) }, putcharm(1) };
struct ai_fio ai_stderr = { { lvm_port_io, &ai_fd_port_vt, putcharm(EOF) }, putcharm(2) };

// --- the nifs --------------------------------------------------------------
// ⚠ no scratch on an lvm_ frame (CLAUDE.md, the tail-threaded VM): the bodies
// that need one go through an ai_noinline helper, and the ones here need none.

// (quit n) -- the frontend nif bao's scare tail reaches for (love/bao.l). Without
// it `(use 'bao)` compiles a form naming an unbound global and raises missing.
static lvm(lvm_quit) {
  fflush(stdout);
  for (;;) exit((int) getcharm(Sp[0]));
  return Continue(); }                 // unreached

// (dev ()) -- a fresh device port. The fd counts up from dev_base and is never
// reused, so a stale port cannot silently address a live device.
static int next_fd = dev_base;

static lvm(lvm_dev) {
  int fd = next_fd;
  dev_at(fd - dev_base);
  Pack(g);
  struct ai *r = ai_io_alloc(g, fd);
  if (!ai_ok(r)) { Unpack(g); Sp[0] = ZeroPoint; Ip += 1; return Continue(); }
  next_fd += 1;
  g = r;
  Unpack(g);
  // ai_io_alloc PUSHED the port, so the argument sits one slot up.
  Sp[1] = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }

// (feed p s) -- queue s (text or one charm) on p. Answers p.
static lvm(lvm_feed) {
  struct dev *d = dev_of_port(Sp[0]);
  ai_word x = Sp[1], out = ZeroPoint;
  if (d) {
    out = Sp[0];
    if (x & 1) {
      unsigned char b = (unsigned char) (getcharm(x) & 0xff);
      grow(&d->q, &d->qcap, d->qlen + 1);
      d->q[d->qlen++] = b; }
    else if (ai_strp(x)) {
      struct ai_str *s = (struct ai_str*) x;
      grow(&d->q, &d->qcap, d->qlen + s->len);
      memcpy(d->q + d->qlen, s->bytes, s->len);
      d->qlen += s->len; }
    else out = ZeroPoint; }
  Sp[1] = out;
  Sp += 1; Ip += 1; return Continue(); }

// (shut p) -- a drained queue now reads END rather than would-block.
static lvm(lvm_shut) {
  struct dev *d = dev_of_port(Sp[0]);
  if (d) d->ended = 1;
  else Sp[0] = ZeroPoint;
  Ip += 1; return Continue(); }

#define counter_nif(nm, field) \
  static lvm(nm) { \
    struct dev *d = dev_of_port(Sp[0]); \
    ai_word out = ZeroPoint; \
    if (d && (Sp[1] & 1)) { \
      intptr_t k = getcharm(Sp[1]); \
      d->field = k > 0 ? (uintptr_t) k : 0; \
      out = Sp[0]; } \
    Sp[1] = out; \
    Sp += 1; Ip += 1; return Continue(); }

counter_nif(lvm_stall, rstall)      // (stall p k) -- k would-block reads
counter_nif(lvm_wstall, wstall)     // (wstall p k) -- k writes that land nothing
counter_nif(lvm_wcap, wcap)         // (wcap p k) -- at most k bytes per write

// (sent p) -- what has been written to p, as text.
static lvm(lvm_sent) {
  struct dev *d = dev_of_port(Sp[0]);
  if (!d) { Sp[0] = EmptyString; Ip += 1; return Continue(); }
  uintptr_t n = d->olen;
  if (!n) { Sp[0] = EmptyString; Ip += 1; return Continue(); }
  Pack(g);
  struct ai *r = str0(g, n);
  if (!ai_ok(r)) { Unpack(g); Sp[0] = EmptyString; Ip += 1; return Continue(); }
  g = r;
  Unpack(g);
  d = dev_of_port(Sp[1]);            // the gc may have moved the port; the fd did not
  memcpy(txt(Sp[0]), d->o, n);
  Sp[1] = Sp[0];
  Sp += 1; Ip += 1; return Continue(); }

// (wpending p) -- the size of p's unsent write run, straight off love.h's own
// accessor. no device state of its own: this is the runtime's number, not ours.
static lvm(lvm_wpending) {
  ai_word x = Sp[0];
  uintptr_t n = 0;
  if (!(x & 1) && ((union u*) x)->ap == lvm_port_io) {
    Pack(g);
    n = ai_io_wpending(g, (struct ai_io*) x);
    Unpack(g); }
  Sp[0] = putcharm((intptr_t) n);
  Ip += 1; return Continue(); }

// (naps ()) -- how many times the scheduler has reached its wait. The one number
// that tells a park from a spin: a task that POLLS a peer stays runnable, so
// find_runnable answers it on every pass and this never moves at all.
static lvm(lvm_naps) {
  Sp[0] = putcharm((intptr_t) naps);
  Ip += 1; return Continue(); }

static union u const
  nif_naps[]   = {{lvm_naps},  {lvm_ret0}},
  nif_quit[]   = {{lvm_quit},  {lvm_ret0}},
  nif_wpend[]  = {{lvm_wpending}, {lvm_ret0}},
  nif_dev[]    = {{lvm_dev},   {lvm_ret0}},
  nif_shut[]   = {{lvm_shut},  {lvm_ret0}},
  nif_sent[]   = {{lvm_sent},  {lvm_ret0}},
  nif_feed[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_feed},   {lvm_ret0}},
  nif_stall[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_stall},  {lvm_ret0}},
  nif_wstall[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_wstall}, {lvm_ret0}},
  nif_wcap[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_wcap},   {lvm_ret0}};

static struct ai_def const defs[] = {
  {"quit",   (intptr_t) nif_quit},
  {"dev",    (intptr_t) nif_dev},
  {"feed",   (intptr_t) nif_feed},
  {"shut",   (intptr_t) nif_shut},
  {"stall",  (intptr_t) nif_stall},
  {"wstall", (intptr_t) nif_wstall},
  {"wcap",   (intptr_t) nif_wcap},
  {"sent",   (intptr_t) nif_sent},
  {"wpending", (intptr_t) nif_wpend},
  {"naps",   (intptr_t) nif_naps} };

// --- the boot --------------------------------------------------------------
// The corpus texts are the ones every frontend shares (out/lib, laid by lcat off
// love0). bao rides along so a law can reach `reads` -- the colist lane under it
// (flow/trickle) is prel's now, and sits on top of the would-block park.
static char const src_bao[] =
#include "bao.h"
 ;
static struct ai_lib const libs[] = {{"bao", src_bao}, {NULL, NULL}};
struct ai_lib const *ai_libs(void) { return libs; }

static ai_noinline char *slurp(char const *path) {
  FILE *f = fopen(path, "rb");
  if (!f) { fprintf(stderr, "; front: cannot read %s\n", path); exit(2); }
  size_t cap = 1 << 16, len = 0;
  char *b = malloc(cap);
  if (!b) die("out of memory reading a law file");
  for (size_t k; (k = fread(b + len, 1, cap - len - 1, f)) > 0;) {
    len += k;
    if (len + 1 >= cap) {
      char *p = realloc(b, cap *= 2);
      if (!p) die("out of memory reading a law file");
      b = p; } }
  fclose(f);
  return b[len] = 0, b; }

int main(int argc, char const **argv) {
  if (argc < 2) {
    fprintf(stderr, "usage: %s <file.l>...\n", argv[0]);
    return 2; }
  struct ai *g = ai_defn(ai_ini(), defs, countof(defs));
  g = ai_egg_(g,
#include "egg.h"
    ,
#include "p1.h"
    ,
#include "prel.h"
    " "
#include "ev.h"
    );
  g = ai_evals_(g, "(use 'bao)");
  g = ai_layer_(g);                  // the session layer: one load, one layer
  for (int i = 1; i < argc && ai_ok(g); i++) g = ai_evals_(g, slurp(argv[i]));
  if (ai_code_of(g) == ai_status_scare) ai_scare_face_(g);   // the honest face: ";; a b", or ";; oom@len=N" bare
  fflush(stdout);
  return ai_fin(g); }
