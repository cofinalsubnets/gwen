// qemu MPS2+ AN500 (Cortex-M7) frontend for love -- the SIM port. The console
// and the clock ride ARM semihosting (qemu -semihosting), so where teensy41
// talks LPUART and rp2040 talks UART0, this port's "metal" is qemu itself:
// the same CPU as both boards (a Cortex-M7), none of the wiring. It is also
// the first love built END TO END by mooncc: love.c + the am math floor +
// libc + this file all compile -t thumb2 (port/mps2/Makefile); only start.S
// (vectors + the semihosting trampoline) and the final ld are arm-none-eabi.
// The boot bakes the egg from source on the M7 -- the whole self-hosting
// double-bake runs under emulation -- then the driver tail asserts a few
// spec laws and exits through m7exit, so `make test_mps2` sees 42.
#include "../../core/love.h"

#ifndef EOF
#define EOF (-1)
#endif

// --- semihosting ----------------------------------------------------------
// start.S: sh_call(op, arg) lands op/arg in r0/r1 (AAPCS did it already) and
// traps via bkpt 0xAB; qemu answers in r0.
uintptr_t sh_call(uintptr_t op, uintptr_t arg);
#define SH_WRITEC 0x03           // arg = &byte
#define SH_CLOCK  0x10           // centiseconds since start
#define SH_EXIT_X 0x20           // arg = {0x20026, code}: qemu exits with code

static void m7_exit(uintptr_t code) {
  uintptr_t blk[2] = { 0x20026, code };   // ADP_Stopped_ApplicationExit
  sh_call(SH_EXIT_X, (uintptr_t) blk);
  for (;;) ; }

static void sh_putc(char c) { sh_call(SH_WRITEC, (uintptr_t) &c); }

uintptr_t ai_clock(void) { return sh_call(SH_CLOCK, 0) * 10; }   // cs -> ms

// any fault vectors here (start.S): name the stacked pc/lr, then exit 98 --
// loud and greppable where the bare M7 would sit in a lockup.
static void sh_hex(uintptr_t v) {
  int i;
  for (i = 28; i >= 0; i -= 4) sh_putc("0123456789abcdef"[(v >> i) & 15]); }

void fault_report(uintptr_t *frame) {    // frame: r0 r1 r2 r3 r12 lr pc xPSR
  char const *s;
  for (s = "\n; fault pc="; *s; s++) sh_putc(*s);
  sh_hex(frame[6]);
  for (s = " lr="; *s; s++) sh_putc(*s);
  sh_hex(frame[5]);
  sh_putc('\n');
  m7_exit(98); }

// --- console input: CMSDK APB UART0 ---------------------------------------
// Output rides semihosting (sh_putc, unbuffered and free), but INPUT needs a
// pollable source -- semihosting READC blocks the whole VM with no readiness
// probe, so the honest ai_ready(0) below reads UART0's RX-full flag instead.
// qemu maps UART0 at 0x40004000 and feeds it from -serial; the gate's
// </dev/null run simply never sees RX full.
#define UART0_BASE 0x40004000u
#define UREG(o) (*(volatile uint32_t *)(UART0_BASE + (o)))
static void uart_init(void) { UREG(0x10) = 16; UREG(0x08) = 3; }   // min bauddiv; TX+RX enable
static int uart_rx_ready(void) { return !!(UREG(0x04) & 2); }      // STATE bit1 = RX full

// --- cooperative waits ----------------------------------------------------
// The teensy shapes: no IRQs, so spin against an ai_clock deadline (ms;
// 0 means forever). Under qemu the spin costs nothing real.
void ai_sleep(uintptr_t ms) {
  if (!ms) { for (;;) if (uart_rx_ready()) return; }   // wait forever -- but wake on input
  uintptr_t start = ai_clock();
  while (ai_clock() - start < ms) ; }

// the readiness law (host/main.c, inle's kmain.c): a NEGATIVE fd is ALWAYS
// ready -- a string port waits on nothing external, and answering "not ready"
// parks its task on a wait no scheduler can satisfy (lvm_sound's park law
// spins sound -> yield -> sound forever: the Enter-key freeze, walled here
// and on teensy silicon alike). fd 0 is the honest poll; others nominal.
bool ai_ready(int fd, int events) { return fd || events != ai_wait_in ? 1 : uart_rx_ready(); }

void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) {
  if (n <= 0) { ai_sleep(ms); return; }
  uintptr_t start = ai_clock();
  for (;;) {
    for (int i = 0; i < n; i++) if (ai_ready(fds[i].fd, fds[i].events)) return;
    if (ms && ai_clock() - start >= ms) return; } }

// --- port vtable ----------------------------------------------------------
// Console bytes in from UART0 (pollable, never EOF -- a live wire, so a dry
// read is 0 and never -1), out through semihosting. This used to spin in
// uart_getc until a byte arrived, which stopped the vm rather than the task.
static intptr_t fd_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  (void) g;
  uintptr_t k = 0;
  while (k < n && uart_rx_ready()) dst[k++] = (unsigned char) (UREG(0x00) & 0xff);
  return (intptr_t) k; }

static intptr_t fd_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  (void) fp;
  for (uintptr_t k = 0; k < n; k++) sh_putc((char) src[k]);
  return (intptr_t) n; }

static struct ai *fd_flush(struct ai *g) { return g; }

struct ai_fio ai_stdin  = { { .ap = lvm_port_io, .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(0) };
struct ai_fio ai_stdout = { { .ap = lvm_port_io, .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(1) };
struct ai_fio ai_stderr = { { .ap = lvm_port_io, .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(1) };
struct ai_port_vt const ai_fd_port_vt = { fd_flush, fd_writen, fd_readn, NULL };

// --- the exit builtin -----------------------------------------------------
// (m7exit code) -- leave the machine through semihosting with `code` as the
// qemu exit status. The driver tail's last word.
static lvm(ai_m7exit) {
  m7_exit(getcharm(Sp[0]));
  return Continue(); }                       // unreached

static union u const nif_m7exit[] = {{ai_m7exit}, {lvm_ret0}};
static struct ai_def defs[] = { {"m7exit", (intptr_t) nif_m7exit} };

// --- the arena ------------------------------------------------------------
// The teensy first-fit free list, fed the AN500's 16 MB PSRAM (mps.ram at
// 0x60000000) by address -- no linker section, the region is just there.
// Lengths in words, header included.
static struct mem {
  struct mem *next;
  uintptr_t len;
  uintptr_t _[];
} *freelist;

#define POOL ((uint8_t*) 0x60000000u)
#define POOL_BYTES ((16u << 20) - 64)   // 64B short of the region edge: a one-past
                                        // read at a block boundary stays inside PSRAM
                                        // (a bus fault at 0x61000000 otherwise)

static ai_inline struct mem *after(struct mem *r) {
  return (struct mem*) ((uintptr_t*) r + r->len); }

static void *mallocw(uintptr_t n) {
  if (!n) return NULL;
  void *p = NULL;
  struct mem *r = NULL, *t;
  while (freelist && freelist->len < n + 2 * Width(struct mem))
    t = freelist,
    freelist = t->next,
    t->next = r,
    r = t;
  if (freelist)
    freelist->len -= n + Width(struct mem),
    t = after(freelist),
    t->len = Width(struct mem) + n,
    p = t->_;
  while (r)
    t = r,
    r = t->next,
    t->next = freelist,
    freelist = t;
  return p; }

void *malloc(size_t n) { return mallocw(b2w(n)); }

void free(void *p) {
  if (!p) return;
  struct mem *m = (struct mem*)p - 1, *r = NULL, *t;
  while (freelist && freelist < m)
    t = freelist,
    freelist = t->next,
    t->next = r,
    r = t;
  for (;; m = r, r = r->next) {
    if (freelist != after(m)) m->next = freelist;
    else m->len += freelist->len,
         m->next = freelist->next;
    freelist = m;
    if (!r) return; } }

#ifdef WAKER
// --- the waker (-D WAKER): the CROSS-BINARY wake proof ----------------------
// a DIFFERENT binary from the baker (different main.o, different layout):
// read love.img back through semihosting, wake it, run the driver laws.
// exit 42 = a portable image wakes outside its baking binary.
#define SH_OPEN  0x01
#define SH_CLOSE 0x02
#define SH_READ  0x06
#define SH_FLEN  0x0C
static void sh_puts(const char *s) { while (*s) sh_putc(*s++); }
int main(void) {
  sh_puts("\n; love/mps2 waker -- cross-binary wake\n");
  // arena at +8MB: BREAK the baker-twin address luck -- a woken value that
  // secretly depends on the baker's pool base must die here, not on silicon
  freelist = (struct mem*) (POOL + (8u << 20));
  freelist->next = NULL;
  freelist->len = ((8u << 20) - 64) / sizeof(uintptr_t);
#ifdef BAKER_RUNE
  static const char impath[] = "out/mps2/love-pd.img";
#else
  static const char impath[] = "out/mps2/love.img";
#endif
  uintptr_t o[3] = { (uintptr_t) impath, 1, sizeof impath - 1 };   // mode 1 = "rb"
  intptr_t fd = (intptr_t) sh_call(SH_OPEN, (uintptr_t) o);
  if (fd < 0) { sh_puts("; no love.img\n"); m7_exit(3); }
  uintptr_t fl[1] = { (uintptr_t) fd };
  uintptr_t len = sh_call(SH_FLEN, (uintptr_t) fl);
  sh_puts("; image bytes "); sh_hex(len); sh_putc('\n');
  // carve the read buffer off the TOP of the pool; the freelist keeps the rest
  char *buf = (char*) POOL + (8u << 20) + (8u << 20) - ((len + 63u) & ~63u);
  freelist->len = ((8u << 20) - 64 - ((len + 63u) & ~63u)) / sizeof(uintptr_t);
  uintptr_t rd[3] = { (uintptr_t) fd, (uintptr_t) buf, len };
  if (sh_call(SH_READ, (uintptr_t) rd)) { sh_puts("; short read\n"); m7_exit(4); }
  uintptr_t cl[1] = { (uintptr_t) fd };
  sh_call(SH_CLOSE, (uintptr_t) cl);
  struct ai *g = ai_image_load(buf, len);
  if (!g) { sh_puts("; wake REFUSED\n"); m7_exit(5); }
  g = ai_defn(g, defs, countof(defs), 0);
  if (ai_ok(g)) ai_core_of(g)->budget = freelist->len / 4;
  struct ai *r = ai_evals_(g,
    "(: ok (&& ((3 2) = 8)"
    "      (&& ('(2 3 4) = (map (+ 1) '(1 2 3)))"
    "      (&& (6 = $'(1 2 3))"
    "      (&& (lit? ev)"
#ifdef BAKER_RUNE
    "      (&& (! ((from ()) = ()))"            // the module registry is live: rune registered
    "          ((2 3 4) = 262144))))))"
#else
    "          ((2 3 4) = 262144)))))"
#endif
    "   _ (putc 10) _ (puts \"; the image woke -- love on the M7\") _ (putc 10)"
    "   (m7exit (? ok 42 1)))");
  if (ai_code_of(r) == ai_status_scare) ai_scare_face_(r);
  ai_fin(r);
  m7_exit(2);
  return 0; }
#else
#ifdef BAKER
// --- the baker (-D BAKER): the bake as a BUILD-TIME event -------------------
// bake the corpus on the emulated M7, then DUMP the heap image to a host file
// via semihosting -- the teensy embeds the blob in flash and WAKES from it
// (its egg lane stays as the fallback). No port nifs are installed before the
// bake and the absolute guard rejects everything, so the image is fully
// SYMBOLIC (heap offsets + lvm/immortal table indices): wakeable in any
// binary compiled from the same love.c at the same word size.
#define SH_OPEN  0x01
#define SH_WRITE 0x05
#define SH_CLOSE 0x02
// the offender log rides the BAKER'S OWN frame -- the guard is told which object carries
// each absolute, so naming them needs nothing of the core's. quads: obj-off, val, obj-hot.
struct img_bad { uintptr_t q[3 * 2]; uintptr_t n; };
static uintptr_t img_reject_all(void *ctx, uintptr_t v, uintptr_t off, uintptr_t ap) {
  struct img_bad *b = ctx;
  if (b->n < 2) b->q[3 * b->n] = off, b->q[3 * b->n + 1] = v, b->q[3 * b->n + 2] = ap, b->n++;
  return 0; }
static void sh_puts(const char *s) { while (*s) sh_putc(*s++); }
// THE BAKED MODULE, the one this baker wants: the boot evals it to register the layer,
// so the woken image serves ((from 'bao 'shell) 0) -- the teensy and nucleo launchers.
// the source strings carry no absolutes, so the absguard stays satisfied.
// bao declares itself; rune does not, so the wrapper is here -- rune is a plain text
// that every consumer loads through `use`, and this is the frontend that names it.
// bao is written in @, and its eval lands after the mop, so the MODULE has to be here
// for the macro to be live.
static char const src_mods[] =
#ifdef BAKER_RUNE
"(module 'rune "
#include "rune.h"
")"
#else
#include "bao.h"
#endif
;
int main(void) {
  sh_puts("\n; love/mps2 baker -- baking the corpus\n");
  freelist = (struct mem*) POOL;
  freelist->next = NULL;
  freelist->len = POOL_BYTES / sizeof(uintptr_t);
  struct ai *g = ai_ini();          // NO ai_defn: a port nif in the book would
                                    // ride into the image as a dead absolute
  if (ai_ok(g)) ai_core_of(g)->budget = POOL_BYTES / sizeof(ai_word) / 4;
  struct ai *r = ai_egg_(g,
#include "egg.h"
    ,
#include "p1.h"
    ,
#include "prel.h"
    " "
#include "ev.h"
    ,
#include "post.h"
    );
  r = ai_evals_(r, src_mods);
  r = ai_evals_(r,
#ifdef BAKER_RUNE
    // the PLAYDATE corpus: rune (registered module) + the cas workbench, no
    // bao -- the device has no shell, the crank is the interface. cas's
    // crank/pushed/cur_set refs stay symbolic (unbound here); the device
    // defn's them post-wake and the book resolves them live.
    "(use 'rune)"
    " "
#include "cas.h"
#else
    "(use 'bao)"
#endif
    "(: _ (putc 10) _ (puts \"; corpus baked -- dumping\") _ (putc 10) 0)");
  if (!ai_ok(r)) {
    if (ai_code_of(r) == ai_status_scare) ai_scare_face_(r);
    m7_exit(3); }
  struct img_bad bad = { {0}, 0 };
  struct ai_image_guard gd = { img_reject_all, &bad };   // ANY kept absolute refuses the dump
  uintptr_t len = 0;
  void *img = ai_image_save(r, &len, &gd);
  if (!img) {
    sh_puts("; dump REFUSED -- absolutes in the heap (off, val, ap):\n");
    for (uintptr_t i = 0; i < bad.n; i++) {
      sh_hex(bad.q[3 * i]); sh_putc(' ');
      sh_hex(bad.q[3 * i + 1]); sh_putc(' ');
      sh_hex(bad.q[3 * i + 2]); sh_putc('\n'); }
    m7_exit(4); }
  // ⚠ THE BAKED RUNTIME GOES BEFORE THE PROOF DOES, and on 16 MB that is the whole
  // margin: img is g->alloc'd and outlives r, the wake wants a second pool the size of
  // the first, and holding a spent heap through it left the arena 14 KB short of a
  // 6 MB ask with 10.9 MB free but in four pieces.
  ai_fin(r);
  // round-trip PROOF before the file exists: wake the buffer we just dumped
  // and run a law through the woken heap. (same-binary wake -- the cross-
  // binary truth is the teensy's -- but it catches every codec desync here.)
  struct ai *g2 = ai_image_load(img, len);
  if (!g2) { sh_puts("; round-trip load FAILED\n"); m7_exit(7); }
  struct ai *r2 = ai_evals_(g2,
    "(: _ (? ((3 2) = 8) (puts \"; round-trip ok\") (puts \"; ROUND-TRIP BROKEN\"))"
    "   _ (putc 10) 0)");
  if (!ai_ok(r2)) { sh_puts("; round-trip eval FAILED\n"); m7_exit(8); }
#ifdef BAKER_RUNE
  static const char impath[] = "out/mps2/love-pd.img";
#else
  static const char impath[] = "out/mps2/love.img";
#endif
  uintptr_t o[3] = { (uintptr_t) impath, 5, sizeof impath - 1 };
  intptr_t fd = (intptr_t) sh_call(SH_OPEN, (uintptr_t) o);
  if (fd < 0) { sh_puts("; SYS_OPEN failed: "); sh_hex((uintptr_t) fd);
                sh_puts(" len "); sh_hex(len); sh_putc('\n'); m7_exit(5); }
  uintptr_t w[3] = { (uintptr_t) fd, (uintptr_t) img, len };
  if (sh_call(SH_WRITE, (uintptr_t) w)) m7_exit(6);
  uintptr_t c[1] = { (uintptr_t) fd };
  sh_call(SH_CLOSE, (uintptr_t) c);
  sh_puts("; image dumped\n");
  m7_exit(0);
  return 0; }
#else

// --- entry ----------------------------------------------------------------
// start.S enabled the FPU; qemu loaded .data/.bss straight into RAM. Bake the
// egg (compile the compiler with the C evaluator, recompile it with itself),
// then the driver tail: assert a few spec laws over the hatched image and
// exit with the verdict. 42 = the egg hatched and the laws hold on the M7.
int main(void) {
  uart_init();
  for (char const *s = "\n; love/mps2 -- baking the egg on the M7\n"; *s; s++)
    sh_putc(*s);
  freelist = (struct mem*) POOL;
  freelist->next = NULL;
  freelist->len = POOL_BYTES / sizeof(uintptr_t);
  struct ai *g = ai_defn(ai_ini(), defs, countof(defs), 0);
  if (ai_ok(g)) ai_core_of(g)->budget = POOL_BYTES / sizeof(ai_word) / 4;
  struct ai *r = ai_egg_(g,
#include "egg.h"
    ,
#include "p1.h"
    ,
#include "prel.h"
    " "
#include "ev.h"
    ,
#include "post.h"
    );
  r = ai_evals_(r,
#include "bao.h"
    // the driver tail: application-as-power, currying through map, the net
    // measure, and the hatched ev -- each a spec.l law, alive on the M7.
    "(: ok (&& ((3 2) = 8)"
    "      (&& ('(2 3 4) = (map (+ 1) '(1 2 3)))"
    "      (&& (6 = $'(1 2 3))"                    // $ GLUED: spaced it is the apply operator
    "      (&& (lit? ev)"
    "          ((2 3 4) = 262144)))))"
    "   _ (putc 10) _ (puts \"; the egg hatched -- love on the M7\") _ (putc 10)"
    "   (m7exit (? ok 42 1)))");
  if (ai_code_of(r) == ai_status_scare) ai_scare_face_(r);
  ai_fin(r);
  m7_exit(2);                                // fell out of the driver: loud
  return 0; }
#endif
#endif
