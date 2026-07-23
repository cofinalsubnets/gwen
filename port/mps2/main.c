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
#include "../../love.h"

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
static int uart_getc(void) {
  while (!uart_rx_ready()) ;
  return (int) (UREG(0x00) & 0xff); }

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
bool ai_ready(int fd) { return fd ? 1 : uart_rx_ready(); }

void ai_wait_fds(int const *fds, int n, uintptr_t ms) {
  if (n <= 0) { ai_sleep(ms); return; }
  uintptr_t start = ai_clock();
  for (;;) {
    for (int i = 0; i < n; i++) if (ai_ready(fds[i])) return;
    if (ms && ai_clock() - start >= ms) return; } }

// --- port vtable ----------------------------------------------------------
// Console bytes in from UART0 (pollable, never EOF -- a live wire), out
// through semihosting. The eof_seen machinery stays for the vt's shape but
// nothing latches it.
static struct ai *fd_getc(struct ai *g) {
  struct ai *fc = ai_core_of(g);
  struct ai_io *i = fc->io;
  if (getcharm(i->ungetc_buf) != EOF) {
    fc->b = getcharm(i->ungetc_buf);
    i->ungetc_buf = putcharm(EOF);
    return g; }
  fc->b = uart_getc();
  return g; }

static struct ai *fd_ungetc(struct ai *g, int c) {
  struct ai *fc = ai_core_of(g);
  struct ai_io *i = fc->io;
  i->ungetc_buf = putcharm(c);
  i->eof_seen = putcharm(false);
  return fc->b = c, g; }

static struct ai *fd_eof(struct ai *g) {
  struct ai *fc = ai_core_of(g);
  struct ai_io *i = fc->io;
  return fc->b = (getcharm(i->ungetc_buf) == EOF) && getcharm(i->eof_seen), g; }

static struct ai *fd_putc(struct ai *g, int c) {
  sh_putc(c);
  return g; }

static struct ai *fd_flush(struct ai *g) { return g; }

struct ai_io ai_stdin  = { .ap = lvm_port_io, .fd = putcharm(0), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
struct ai_io ai_stdout = { .ap = lvm_port_io, .fd = putcharm(1), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
struct ai_io ai_stderr = { .ap = lvm_port_io, .fd = putcharm(1), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
struct ai_port_vt const ai_fd_port_vt = { fd_getc, fd_ungetc, fd_eof, fd_putc, fd_flush, NULL, NULL };

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
  struct ai *g = ai_defn(ai_ini(), defs, countof(defs));
  if (ai_ok(g)) ai_core_of(g)->budget = POOL_BYTES / sizeof(ai_word) / 4;
  struct ai *r = ai_evals_(g, "("
#include "egg.h"
    ai_egg_pre
#include "prel.h"
    " "
#include "ev.h"
    ai_egg_post
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
