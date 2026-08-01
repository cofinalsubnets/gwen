// Raspberry Pi Pico (RP2040) frontend for gwen lisp -- bare metal, no SDK.
//
// gwen's frontend contract (gwen.h): the host defines g_clock, the
// g_stdin/g_stdout ports, the g_fd_port_vt vtable, and the cooperative-wait
// hooks. Here the console is UART0 on GPIO0(TX)/GPIO1(RX) at 115200 8N1 -- the
// same PL011 the aarch64 kernel console drives -- reachable over a USB-serial
// adapter. The arch backend (rp2040.c) owns the boot chain, clocks, UART, and
// timer; this file is just the gwen glue plus a few GPIO nifs. The REPL line
// editor in repl.g drives the console exactly as it drives the kernel's.
#include "../../gwen.h"
#include "rp2040.h"

#ifndef EOF
#define EOF (-1)
#endif

// --- cooperative waits ----------------------------------------------------
// The host backs these with poll(2); we have only the free-running timer and
// a polled UART, so spin against a g_clock() deadline (ticks are ms; ticks==0
// means wait forever). No IRQs are enabled, so there is nothing to WFE on --
// a tight poll keeps (key)/timed sleeps re-checking readiness. Same shape as
// the host's poll_wait, minus the kernel.
void g_sleep(uintptr_t ms) {
  if (!ms) { for (;;) __asm volatile("wfe"); }   // infinite: park (reset to exit)
  uintptr_t start = g_clock();
  while (g_clock() - start < ms) __asm volatile("nop"); }

bool g_ready(int fd) { return fd == 0 ? serial_rx_ready() : fd >= 0; }

void g_wait_fds(int const *fds, int n, uintptr_t ms) {
  if (n <= 0) { g_sleep(ms); return; }
  if (n > G_WAIT_FDS_MAX) __builtin_trap();
  uintptr_t start = g_clock();
  for (;;) {
    for (int i = 0; i < n; i++) if (g_ready(fds[i])) return;
    if (ms && g_clock() - start >= ms) return;
    __asm volatile("nop"); } }

// --- port vtable ----------------------------------------------------------
// Both ports ride UART0; the fd is nominal (>= 0 so the dispatcher routes
// here). Serial never reaches EOF, so the dispatcher's eof_seen latch never
// trips.
static struct g *fd_getc(struct g *g) {
  struct g *fc = g_core_of(g);
  struct g_io *i = fc->io;
  if (getcharm(i->ungetc_buf) != EOF) {
    fc->b = getcharm(i->ungetc_buf);
    i->ungetc_buf = putcharm(EOF);
    return g; }
  fc->b = serial_getc();
  return g; }

static struct g *fd_ungetc(struct g *g, int c) {
  struct g *fc = g_core_of(g);
  struct g_io *i = fc->io;
  i->ungetc_buf = putcharm(c);
  i->eof_seen = putcharm(false);
  return fc->b = c, g; }

static struct g *fd_eof(struct g *g) {
  struct g *fc = g_core_of(g);
  struct g_io *i = fc->io;
  return fc->b = (getcharm(i->ungetc_buf) == EOF) && getcharm(i->eof_seen), g; }

static struct g *fd_putc(struct g *g, int c) {
  if (c == '\n') serial_putc('\r');     // cook LF -> CRLF for terminals
  serial_putc(c);
  return g; }

static struct g *fd_flush(struct g *g) { return g; }   // UART has no buffer

struct g_io g_stdin  = { g_vm_port_io, putcharm(0), putcharm(EOF), putcharm(false) };
struct g_io g_stdout = { g_vm_port_io, putcharm(1), putcharm(EOF), putcharm(false) };
// No separate error stream; route err to the console too.
struct g_io g_stderr = { g_vm_port_io, putcharm(1), putcharm(EOF), putcharm(false) };
struct g_port_vt const g_fd_port_vt = { fd_getc, fd_ungetc, fd_eof, fd_putc, fd_flush };

// --- GPIO builtins --------------------------------------------------------
// (gpio_init pin)    -- claim a pin for SIO; returns the pin.
// (gpio_dir pin out) -- direction: out non-zero => output; returns out.
// (gpio_put pin val) -- drive an output: val non-zero => high; returns val.
// (gpio_get pin)     -- sample an input; returns 1 (high) or 0 (low).
// zero is putcharm(0), so getcharm(arg) != 0 reads a number or zero correctly.
static g_vm(g_gpio_init) {
  gpio_init(getcharm(Sp[0]));           // leaves Sp[0] (the pin) as the result
  Ip += 1;
  return Continue(); }

static g_vm(g_gpio_get) {
  Sp[0] = putcharm(gpio_get(getcharm(Sp[0])));
  Ip += 1;
  return Continue(); }

static g_vm(g_gpio_dir) {
  unsigned pin = getcharm(Sp[0]);
  int out = getcharm(Sp[1]) != 0;
  gpio_set_dir(pin, out);
  Sp[1] = putcharm(out);
  Sp += 1;
  Ip += 1;
  return Continue(); }

static g_vm(g_gpio_put) {
  unsigned pin = getcharm(Sp[0]);
  int val = getcharm(Sp[1]) != 0;
  gpio_put(pin, val);
  Sp[1] = putcharm(val);
  Sp += 1;
  Ip += 1;
  return Continue(); }

// 1-arg nifs run their thunk directly; 2-arg nifs build a 2-slot frame with
// g_vm_cur first (mirrors the host's nif_open shape).
static union u const
  nif_gpio_init[] = {{g_gpio_init}, {g_vm_ret0}},
  nif_gpio_get[]  = {{g_gpio_get}, {g_vm_ret0}},
  nif_gpio_dir[]  = {{g_vm_cur}, {.x = putcharm(2)}, {g_gpio_dir}, {g_vm_ret0}},
  nif_gpio_put[]  = {{g_vm_cur}, {.x = putcharm(2)}, {g_gpio_put}, {g_vm_ret0}};

static struct g_def defs[] = {
  {"gpio_init", (intptr_t) nif_gpio_init},
  {"gpio_dir",  (intptr_t) nif_gpio_dir},
  {"gpio_put",  (intptr_t) nif_gpio_put},
  {"gpio_get",  (intptr_t) nif_gpio_get}, };

// --- entry ----------------------------------------------------------------
// reset_handler (rp2040.c) has set up clocks before calling us. The static
// pool is the whole gwen heap (RP2040 has 264 KB SRAM and no malloc); the
// rest of SRAM above it is the C stack. The bootstrap egg compiles the gwen
// compiler with the C evaluator, recompiles it with itself, installs it, then
// we run the REPL -- identical to host/free, just smaller. The pool is sized
// as large as fits under the stack; if the self-hosting double-bake ever OOMs
// on real silicon, shrink it or trim the egg (this is a RAM-fit knob, not a
// build concern).
static uint8_t pool[232 * (1 << 10)];

int main(void) {
  serial_init();
  struct g *g = g_defn(g_ini_s(pool, sizeof pool), defs, LEN(defs));
  g = g_evals_(g, "("
#include "egg.h"
    G_EGG_PRE
#include "prel.h"
    " "
#include "ev.h"
    G_EGG_POST
#include "repl.h"
    "(repl 0 0)");
  // The REPL only returns on a fatal error. Idle low-power afterward.
  (void) g;
  for (;;) __asm volatile("wfe"); }
