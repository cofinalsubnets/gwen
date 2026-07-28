// Teensy 4.1 (i.MX RT1062) frontend for love -- bare metal, no Teensyduino.
//
// love's frontend contract (love.h): the host defines ai_clock, the
// ai_stdin/ai_stdout ports, the ai_fd_port_vt vtable, and the cooperative-wait
// hooks. Here the console is LPUART6 on pin0(RX)/pin1(TX) at 115200 8N1,
// reachable over a 3.3 V USB-serial adapter -- the analogue of the rp2040
// port's UART0 console (USB CDC is a TODO, see README). The arch backend
// (teensy41.c) owns the FlexSPI boot image, startup, clocks, LPUART, GPT
// timer, and GPIO; this file is just the love glue plus a few GPIO nifs. The
// shell line editor (love/bao.l, the baked shell core) drives the console
// exactly as it drives the kernel's.
#include "../../love.h"
#include "teensy41.h"
#include "psram.h"

#ifndef EOF
#define EOF (-1)
#endif

// --- cooperative waits ----------------------------------------------------
// The host backs these with poll(2); we have only the free-running GPT timer
// and a polled LPUART, so spin against a ai_clock() deadline (ticks are ms;
// ticks==0 means wait forever). No IRQs are enabled, so there is nothing to
// WFE on -- a tight poll keeps (key)/timed sleeps re-checking readiness. Same
// shape as the host's poll_wait, minus the kernel.
void ai_sleep(uintptr_t ms) {
  if (!ms) { for (;;) if (serial_rx_ready()) return; }   // wait forever -- but wake on input
  uintptr_t start = ai_clock();
  while (ai_clock() - start < ms) ; }

// the readiness law (host/main.c, inle's kmain.c): a NEGATIVE fd is ALWAYS
// ready -- a string port waits on nothing external, and answering "not ready"
// parks its task on a wait no scheduler can satisfy (lvm_sound's park law
// spins sound -> yield -> sound forever: the Enter-key freeze that walled
// first-silicon interactive). fd 0 is the honest poll; other fds are nominal.
bool ai_ready(int fd) { return fd ? 1 : serial_rx_ready(); }

void ai_wait_fds(int const *fds, int n, uintptr_t ms) {
  if (n <= 0) { ai_sleep(ms); return; }
  if (n > ai_wait_fds_max) __builtin_trap();
  uintptr_t start = ai_clock();
  for (;;) {
    for (int i = 0; i < n; i++) if (ai_ready(fds[i])) return;
    if (ms && ai_clock() - start >= ms) return; } }

// --- port vtable ----------------------------------------------------------
// Both ports ride LPUART6; the fd is nominal (>= 0 so the dispatcher routes
// here). Serial never reaches EOF, so the dispatcher's eof_seen latch never
// trips.
static struct ai *fd_getc(struct ai *g) {
  struct ai *fc = ai_core_of(g);
  struct ai_io *i = fc->io;
  if (getcharm(i->ungetc_buf) != EOF) {
    fc->b = getcharm(i->ungetc_buf);
    i->ungetc_buf = putcharm(EOF);
    return g; }
  fc->b = serial_getc();
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
  if (c == '\n') serial_putc('\r');     // cook LF -> CRLF for terminals
  serial_putc(c);
  return g; }

static struct ai *fd_flush(struct ai *g) { return g; }   // LPUART has no buffer here

struct ai_io ai_stdin  = { .ap = lvm_port_io, .fd = putcharm(0), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
struct ai_io ai_stdout = { .ap = lvm_port_io, .fd = putcharm(1), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
// No separate error stream; route err to the console too.
struct ai_io ai_stderr = { .ap = lvm_port_io, .fd = putcharm(1), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false) };
struct ai_port_vt const ai_fd_port_vt = { fd_getc, fd_ungetc, fd_eof, fd_putc, fd_flush, NULL, NULL };  // no bulk lanes: per-byte fallback

// --- GPIO builtins --------------------------------------------------------
// (gpio_init pin)    -- claim a GPIO2 bit (pin 13 also gets its pad muxed); returns the pin.
// (gpio_dir pin out) -- direction: out non-nil => output; returns out.
// (gpio_put pin val) -- drive an output: val non-nil => high; returns val.
// (gpio_get pin)     -- sample an input; returns 1 (high) or 0 (low).
// nil is putcharm(0), so getcharm(arg) != 0 reads a number or nil correctly.
static lvm(ai_gpio_init) {
  gpio_init(getcharm(Sp[0]));           // leaves Sp[0] (the pin) as the result
  Ip += 1;
  return Continue(); }

static lvm(ai_gpio_get) {
  Sp[0] = putcharm(gpio_get(getcharm(Sp[0])));
  Ip += 1;
  return Continue(); }

static lvm(ai_gpio_dir) {
  unsigned pin = getcharm(Sp[0]);
  int out = getcharm(Sp[1]) != 0;
  gpio_set_dir(pin, out);
  Sp[1] = putcharm(out);
  Sp += 1;
  Ip += 1;
  return Continue(); }

static lvm(ai_gpio_put) {
  unsigned pin = getcharm(Sp[0]);
  int val = getcharm(Sp[1]) != 0;
  gpio_put(pin, val);
  Sp[1] = putcharm(val);
  Sp += 1;
  Ip += 1;
  return Continue(); }

// 1-arg nifs run their thunk directly; 2-arg nifs build a 2-slot frame with
// lvm_cur first (mirrors the host's nif_open shape).
static union u const
  nif_gpio_init[] = {{ai_gpio_init}, {lvm_ret0}},
  nif_gpio_get[]  = {{ai_gpio_get}, {lvm_ret0}},
  nif_gpio_dir[]  = {{lvm_cur}, {.x = putcharm(2)}, {ai_gpio_dir}, {lvm_ret0}},
  nif_gpio_put[]  = {{lvm_cur}, {.x = putcharm(2)}, {ai_gpio_put}, {lvm_ret0}};

// the baked heap image, embedded by ld -b binary (see the Makefile + .image
// in teensy41.lds). FILE scope: mooncc emits no relocation for a block-scope
// extern array (the address materializes as garbage -- silicon-diagnosed).
extern const char _binary_love_img_start[], _binary_love_img_end[];

static struct ai_def defs[] = {
  {"gpio_init", (intptr_t) nif_gpio_init},
  {"gpio_dir",  (intptr_t) nif_gpio_dir},
  {"gpio_put",  (intptr_t) nif_gpio_put},
  {"gpio_get",  (intptr_t) nif_gpio_get}, };

// --- the arena ------------------------------------------------------------
// The generational collector is the ONLY collector, and it draws its pools
// through g->alloc, whose default rides malloc/free (ai_ini). So the frontend
// supplies them: a first-fit free list over a static arena in OCRAM2 (the inle
// kernel's kmallocw/kfree, shrunk to one region), with the C stack above it
// under __stack_top__. Lengths are in words, header included.
static struct mem {
  struct mem *next;
  uintptr_t len;
  uintptr_t _[];
} *freelist;

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
// cstartup (teensy41.c) has set up the FPU, .data/.bss, VTOR, clocks, and the
// console before calling us. The bootstrap egg compiles the love compiler with
// the C evaluator, recompiles it with itself, installs it, then we run the
// shell -- identical to host/free, just smaller. The arena of FIRST resort is
// the 16 MB external PSRAM (psram.c brings up FlexSPI2; pattern-tested clean
// on this board 2026-07-22) -- the same arena size the qemu-M7 port bakes in;
// the old 384 KB OCRAM2 pool starved the bake (the first-silicon blocker: a
// live set past the budget faults INSIDE the collector -- solid LED, then the
// bootloader chip's 7-blink as the locked core trips its supervision). The
// OCRAM pool stays as the no-PSRAM fallback.
static uintptr_t pool[384 * (1 << 10) / sizeof(uintptr_t)];   // word-typed: naturally aligned (mooncc parses no post-declarator attribute)

int main(void) {
  // first light: the LED comes on before any love runs, so a board with no
  // serial adapter still shows the boot image + crt0 + clocks worked. The
  // gpio_* layer indexes GPIO2 BITS: the pin-13 LED is GPIO2_IO03 = LED_BIT.
  gpio_init(LED_BIT); gpio_set_dir(LED_BIT, 1); gpio_put(LED_BIT, 1);
  // a raw banner straight to the LPUART: proves the console path (mux, baud,
  // adapter wiring) the moment the board resets, before any love runs.
  for (char const *s = "\r\n; love/teensy41\r\n"; *s; s++)
    serial_putc(*s);
  // self-reported core clock: derive MHz from the LIVE mux state (not from
  // what clocks_init intended) -- pll1 path only; anything else prints the
  // rom-path face so a silently-failed retune is visible on every boot.
  { uint32_t mhz = 0;
    if (!(REG(CCM_CBCDR) & CBCDR_PERIPH_CLK_SEL)
        && (REG(CCM_CBCMR) & CBCMR_PRE_PERIPH_MASK) == CBCMR_PRE_PERIPH_PLL1
        && !(REG(CCM_CCSR) & CCSR_PLL1_SW_CLK_SEL))
      mhz = 24u * (REG(CCM_ANALOG_PLL_ARM) & 0x7Fu) / 2u
          / ((REG(CCM_CACRR) & 7u) + 1u)
          / (((REG(CCM_CBCDR) >> 10) & 7u) + 1u);
    for (char const *s = "; core "; *s; s++) serial_putc(*s);
    if (mhz) { char b[8]; int n = 0;
      do { b[n++] = '0' + mhz % 10u; mhz /= 10u; } while (mhz);
      while (n) serial_putc(b[--n]);
      for (char const *s = " MHz\r\n"; *s; s++) serial_putc(*s); }
    else for (char const *s = "on the ROM path\r\n"; *s; s++) serial_putc(*s); }
  uint32_t psram_mb = psram_init();
  { char const *s = psram_mb ? "; psram arena up\r\n" : "; NO psram -- ocram fallback\r\n";
    for (; *s; s++) serial_putc(*s); }
  uintptr_t arena_words;
  if (psram_mb) {
    // 64 B shy of the region edge: a one-past read at a block boundary must
    // stay inside the region (the mps2 port's bus-fault lesson)
    freelist = (struct mem*) 0x70000000u;
    arena_words = ((psram_mb << 20) - 64u) / sizeof(uintptr_t); }
  else {
    freelist = (struct mem*) pool;
    arena_words = sizeof pool / sizeof(uintptr_t); }
  freelist->next = NULL;
  freelist->len = arena_words;
  // WAKE-FIRST: the flash carries a heap image (qemu-baked at build time by
  // port/mps2's baker -- fully symbolic, so this differently-linked binary
  // may wake it). A good image skips the ~55 s on-device bake; any problem
  // answers NULL and the egg lane below bakes from source as always.
  struct ai *g = ai_image_load(_binary_love_img_start,
                               (uintptr_t)(_binary_love_img_end - _binary_love_img_start));
  int woke = g != NULL;
  { char const *s = woke ? "; image awake\r\n" : "; no image -- baking the egg\r\n";
    for (; *s; s++) serial_putc(*s); }
  if (!woke) g = ai_ini();
  g = ai_defn(g, defs, countof(defs));
  // BOUND the collector to the arena (the Appel knob -- gen_please, love.c):
  // 2*minor + 2*major carve out of the free list, and a major resize holds old
  // and new at once, so an unbounded budget OOMs inside the collector. A
  // quarter of the arena leaves the double-buffered resize and free-list
  // fragmentation their room.
  if (ai_ok(g)) ai_core_of(g)->budget = arena_words / 4;
  // The LED is the status channel while the console has no adapter: solid on
  // = still baking/waking, OFF = the shell is at its prompt, fast blink
  // (below) = fatal. 3 is LED_BIT (GPIO2_IO03 = pin 13). the tail runs AFTER
  // the bake/wake, so its `puts` marks the exact moment love is up.
#define TE_TAIL(banner) \
    "(: _ (gpio_init 3) _ (gpio_dir 3 1) _ (gpio_put 3 0)" \
    "    _ (putc 10) _ (puts \"" banner "\") _ (putc 10) ((from 'bao 'shell) 0))"
  if (!woke) {
    // the on-device egg bake: bao is a MODULE (no brackets of its own) --
    // register the source, load it by name inside the boot form. a woken
    // image (the mps2 baker's) carries the registration already.
    static char const src_bao[] =
#include "bao.h"
    ;
    g = ai_lib_(g, "bao", src_bao);
    g = ai_evals_(g, "("
#include "egg.h"
    ai_egg_pre
#include "prel.h"
    " "
#include "ev.h"
    ai_egg_post
    "(use 'bao) 0)"); }
  // THE SESSION: a fresh writable layer, C-side -- the shell's defglobs land
  // here, never in the base (bakes carry none; every boot or wake pushes its own).
  g = ai_layer_(g);
  struct ai *r = ai_evals_(g, woke ? TE_TAIL("; image hatched -- shell up")
                                   : TE_TAIL("; egg hatched -- shell up"));
  // The shell only returns on a fatal error: honest face, then blink it out.
  if (ai_code_of(r) == ai_status_scare) ai_scare_face_(r);
  ai_fin(r);
  for (;;) {
    gpio_put(LED_BIT, 1); ai_sleep(120);
    gpio_put(LED_BIT, 0); ai_sleep(120); } }
