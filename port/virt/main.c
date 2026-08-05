// qemu -M virt (riscv64) frontend for love -- the SIM port, and the first
// port with NO foreign toolchain anywhere: love.c + the am math floor + libc
// + this file all compile `mooncc -t riscv64`, start.o is laid straight from
// holo IR (mkstart.l), and OUR linker binds the ELF (-Ttext puts the first
// text byte exactly at 0x80000000, where the board's mask ROM jumps -- see
// the Makefile). The "metal" is all plain MMIO: console on the ns16550 at
// 0x10000000, the clock off the CLINT's mtime, and exit through the sifive
// test finisher -- no semihosting, no traps, no asm here at all. The boot
// bakes the egg from source on the emulated hart -- the self-hosting
// double-bake under emulation -- then the driver tail asserts a few spec
// laws and exits through vexit, so `make test_virt` sees 42 (98 = a trap,
// reported by start.o's mtvec tail through fault_report below).
#include "../../love.h"

#ifndef EOF
#define EOF (-1)
#endif

// --- the metal ------------------------------------------------------------
// ns16550: THR/RBR at +0, LSR at +5 (bit 5 = THR empty, bit 0 = data ready).
#define UART ((volatile uint8_t *) 0x10000000u)
static void v_putc(char c) {
  while (!(UART[5] & 0x20)) ;
  UART[0] = (uint8_t) c; }
static int uart_rx_ready(void) { return UART[5] & 1; }

// the CLINT's mtime, a free-running 10 MHz counter -> ms.
#define MTIME (*(volatile uint64_t *) 0x0200BFF8u)
uintptr_t ai_clock(void) { return (uintptr_t) (MTIME / 10000u); }

// the sifive test finisher: FINISHER_FAIL (0x3333) carries the exit status in
// its high half, so qemu leaves with exactly `code` -- the gate's whole wire.
#define TESTDEV (*(volatile uint32_t *) 0x100000u)
static void v_exit(uintptr_t code) {
  TESTDEV = ((uint32_t) code << 16) | 0x3333u;
  for (;;) ; }

static void v_puts(char const *s) { while (*s) v_putc(*s++); }
static void v_hex(uintptr_t v) {
  int i;
  for (i = 60; i >= 0; i -= 4) v_putc("0123456789abcdef"[(v >> i) & 15]); }

// any machine trap vectors here (start.o's mtvec tail hands over mcause/mepc):
// name the cause, then exit 98 -- loud and greppable where the bare hart would
// sit in a trap loop.
void fault_report(uintptr_t cause, uintptr_t epc) {
  v_puts("\n; trap cause=");
  v_hex(cause);
  v_puts(" epc=");
  v_hex(epc);
  v_putc('\n');
  v_exit(98); }

// --- cooperative waits ----------------------------------------------------
// The teensy shapes: no IRQs, so spin against an ai_clock deadline (ms;
// 0 means forever). Under qemu the spin costs nothing real.
void ai_sleep(uintptr_t ms) {
  if (!ms) { for (;;) if (uart_rx_ready()) return; }   // wait forever -- but wake on input
  uintptr_t start = ai_clock();
  while (ai_clock() - start < ms) ; }

// the readiness law (host/main.c, inle's kmain.c): a NEGATIVE fd is ALWAYS
// ready -- a string port waits on nothing external, and answering "not ready"
// parks its task on a wait no scheduler can satisfy. fd 0 is the honest poll;
// others nominal.
bool ai_ready(int fd, int events) { return fd || events != ai_wait_in ? 1 : uart_rx_ready(); }

void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) {
  if (n <= 0) { ai_sleep(ms); return; }
  uintptr_t start = ai_clock();
  for (;;) {
    for (int i = 0; i < n; i++) if (ai_ready(fds[i].fd, fds[i].events)) return;
    if (ms && ai_clock() - start >= ms) return; } }

// --- port vtable ----------------------------------------------------------
// Console bytes in and out through the ns16550 (pollable, never EOF -- a live
// wire, so a dry read is 0 and never -1). This used to spin in uart_getc until
// a byte arrived, which stopped the vm rather than the reading task.
static intptr_t fd_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  (void) g;
  uintptr_t k = 0;
  while (k < n && uart_rx_ready()) dst[k++] = UART[0];
  return (intptr_t) k; }

static intptr_t fd_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  (void) fp;
  for (uintptr_t k = 0; k < n; k++) v_putc((char) src[k]);
  return (intptr_t) n; }

static struct ai *fd_flush(struct ai *g) { return g; }

struct ai_fio ai_stdin  = { { .ap = lvm_port_io, .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(0) };
struct ai_fio ai_stdout = { { .ap = lvm_port_io, .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(1) };
struct ai_fio ai_stderr = { { .ap = lvm_port_io, .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(1) };
struct ai_port_vt const ai_fd_port_vt = { fd_flush, fd_writen, fd_readn, NULL };

// --- the exit builtin -----------------------------------------------------
// (vexit code) -- leave the machine through the test finisher with `code` as
// the qemu exit status. The driver tail's last word.
static lvm(ai_vexit) {
  v_exit(getcharm(Sp[0]));
  return Continue(); }                       // unreached

static union u const nif_vexit[] = {{ai_vexit}, {lvm_ret0}};
static struct ai_def defs[] = { {"vexit", (intptr_t) nif_vexit} };

// --- the arena ------------------------------------------------------------
// The teensy first-fit free list, fed 64 MB of virt's DRAM by address -- the
// ox64's PSRAM budget, so what fits here fits the board. The image sits at
// the bottom of DRAM (0x80000000); the C stack tops out at the pool's base
// and grows away from it.
static struct mem {
  struct mem *next;
  uintptr_t len;
  uintptr_t _[];
} *freelist;

#define POOL ((uint8_t*) 0x82000000u)
#define POOL_BYTES ((64u << 20) - 64)   // 64B short: a one-past read at a block
                                        // boundary stays inside the region

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
// start.o set sp, opened the FPU (mstatus.FS) and pointed mtvec at the fault
// tail; qemu loaded .data straight into RAM and fresh RAM is zero (.bss
// included). Bake the egg (compile the compiler with the C evaluator,
// recompile it with itself), then the driver tail: assert a few spec laws
// over the hatched image and exit with the verdict. 42 = the egg hatched and
// the laws hold on the hart.
int main(void) {
  v_puts("\n; love/virt -- baking the egg on the hart\n");
  freelist = (struct mem*) POOL;
  freelist->next = NULL;
  freelist->len = POOL_BYTES / sizeof(uintptr_t);
  struct ai *g = ai_defn(ai_ini(), defs, countof(defs));
  if (ai_ok(g)) ai_core_of(g)->budget = POOL_BYTES / sizeof(ai_word) / 4;
  struct ai *r = ai_egg_(g,
#include "egg.h"
    ,
#include "p1.h"
    ,
#include "prel.h"
    " "
#include "ev.h"
    );
  r = ai_evals_(r,
#include "bao.h"
    // the driver tail: application-as-power, currying through map, the net
    // measure, and the hatched ev -- each a spec.l law, alive on the hart.
    "(: ok (&& ((3 2) = 8)"
    "      (&& ('(2 3 4) = (map (+ 1) '(1 2 3)))"
    "      (&& (6 = $'(1 2 3))"                    // $ GLUED: spaced it is the apply operator
    "      (&& (lit? ev)"
    "          ((2 3 4) = 262144)))))"
    "   _ (putc 10) _ (puts \"; the egg hatched -- love on the hart\") _ (putc 10)"
    "   (vexit (? ok 42 1)))");
  if (ai_code_of(r) == ai_status_scare) ai_scare_face_(r);
  ai_fin(r);
  v_exit(2);                                 // fell out of the driver: loud
  return 0; }
