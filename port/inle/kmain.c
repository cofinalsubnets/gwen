#include "limine.h"
#include "k.h"
#include "love.h"
#include "quay.h"
#include "asmops.h"                    // the privileged instructions, both spellings
#include <stdarg.h>
#include <limits.h>

uint64_t kticks;
// Higher-half direct map offset: physical address P is reachable at
// khhdm + P, copied out of Limine's HHDM response. Set before archinit,
// so arch code can use it for MMIO.
uintptr_t khhdm;

static struct mem {
  struct mem *next;
  uintptr_t len;
  uintptr_t _[];
} *kmem;

// total free RAM linked into kmem, in words -- summed in meminit, used to bound the generational
// collector (g->budget) so its two growing pools stay within the device's RAM. See kmain.
static uintptr_t kram_words;

struct cb *kcb;

static struct {
  volatile uint32_t *_;
  uint16_t width, height, pitch; } kfb;

// keyboard input. kb_int (interrupt context) decodes scancodes and
// enqueues input bytes -- arrow/Delete keys as the ANSI escape sequences
// the line editor decodes; k_getc and the (key) builtin drain the queue.
// g holds the live modifier flags.
static struct { uint8_t g, q[16], qh, qt; } kkb;
// enqueue one input byte (drop if full). non-static: the COM1 serial
// RX ap (k_uart, in x86_64/arch.c) feeds this same queue.
void kq(uint8_t b) {
  uint8_t n = (kkb.qt + 1) & 15;
  if (n != kkb.qh) kkb.q[kkb.qt] = b, kkb.qt = n; }
static int kqpop(void) {                   // dequeue one byte, -1 if empty
  if (kkb.qh == kkb.qt) return -1;
  int b = kkb.q[kkb.qh];
  return kkb.qh = (kkb.qh + 1) & 15, b; }

static uint32_t palette[256];
static struct font
 kfont = { .glyphs = (uint8_t*) moderndos_8x16, .w = 8, .h = 16, },
 *fonts[16] = { &kfont };

static void palette_init(void) {
  static const uint32_t base[16] = {              // 0..15: the standard 16
    0x000000, 0x800000, 0x008000, 0x808000,
    0x000080, 0x800080, 0x008080, 0xc0c0c0,
    0x808080, 0xff0000, 0x00ff00, 0xffff00,
    0x0000ff, 0xff00ff, 0x00ffff, 0xffffff };
  static const uint8_t cube[6] = { 0, 95, 135, 175, 215, 255 };  // xterm levels

  for (int i = 0; i < 16; i++) palette[i] = base[i];
  for (int i = 0; i < 216; i++) {                  // 16..231: 6x6x6 cube
    int r = i / 36, g = i / 6 % 6, b = i % 6;
    palette[16 + i] = cube[r] << 16 | cube[g] << 8 | cube[b]; }
  for (int i = 0; i < 24; i++) {                   // 232..255: grey ramp
    uint32_t v = 8 + 10 * i;
    palette[232 + i] = v << 16 | v << 8 | v; } }


void k_reset(void), archinit(void), fbdraw(void), serial_init(void), serial_putc(int),
     k_fault_trigger(intptr_t n);
#ifdef K_TEST
void k_qemu_exit(int);
#endif

#include "quay.h"
#include <stdarg.h>
__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
#define _L __attribute__((used, section(".limine_requests"))) static volatile
_L LIMINE_BASE_REVISION(3);
_L struct limine_memmap_request memmap_req = { .id = LIMINE_MEMMAP_REQUEST, .revision = 0 };
_L struct limine_hhdm_request hhdm_req = { .id = LIMINE_HHDM_REQUEST, .revision = 0 };
_L struct limine_framebuffer_request fb_req = { .id = LIMINE_FRAMEBUFFER_REQUEST, .revision = 0 };
_L struct limine_date_at_boot_request date_req = { .id = LIMINE_DATE_AT_BOOT_REQUEST, .revision = 0 };
_L struct limine_executable_address_request addr_req = { .id = LIMINE_EXECUTABLE_ADDRESS_REQUEST, .revision = 0 };
_L struct limine_efi_system_table_request systbl_req = { .id = LIMINE_EFI_SYSTEM_TABLE_REQUEST, .revision = 0 };
_L struct limine_executable_cmdline_request cmdline_req = { .id = LIMINE_EXECUTABLE_CMDLINE_REQUEST, .revision = 0 };
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;

// kboot -- populated by limine_to_kboot() at the top of kmain.
struct k_boot kboot;
static void limine_to_kboot(void) {
  if (hhdm_req.response) kboot.hhdm = hhdm_req.response->offset;
  if (memmap_req.response) {
    struct limine_memmap_entry **rr = memmap_req.response->entries;
    uintptr_t n = memmap_req.response->entry_count;
    for (uintptr_t i = 0; i < n && kboot.ram_n < k_boot_ram_max; i++)
      if (rr[i]->type == 0)
        kboot.ram[kboot.ram_n].base = rr[i]->base,
        kboot.ram[kboot.ram_n].len  = rr[i]->length,
        kboot.ram_n++; }
  if (fb_req.response && fb_req.response->framebuffer_count) {
    struct limine_framebuffer *g = fb_req.response->framebuffers[0];
    kboot.fb.base     = g->address;
    kboot.fb.w        = g->width;
    kboot.fb.h        = g->height;
    kboot.fb.pitch_px = g->pitch >> 2;
    kboot.has_fb      = true; } }

#define kb_code_lshift 0x2a
#define kb_code_rshift 0x36
#define kb_code_extend 0xe0
#define kb_code_delete 0x53
#define kb_code_ctl 0x1d
#define kb_code_alt 0x38
#define kb_flag_rshift 1
#define kb_flag_lshift 2
#define kb_flag_rctl   4
#define kb_flag_lctl   8
#define kb_flag_ralt   16
#define kb_flag_lalt   32
#define kb_flag_extend 128
#define kb_flag_alt (kb_flag_lalt|kb_flag_ralt)
#define kb_flag_ctl (kb_flag_lctl|kb_flag_rctl)
#define kb_flag_shift (kb_flag_lshift|kb_flag_rshift)

// --- vfs-shaped source table ----------------------------------------------
// k_sources[] holds per-fd vtables. The kernel's ai_fd_port_vt is a thin
// shim that routes each call through k_sources[fd]. NULL slots mean
// "no method"; the dispatcher skips them (writes discard, reads return
// EOF, ready returns false). Per-byte methods today -- P3b/later will
// add bulk read/write when ramfs/files start needing them. `state` is
// per-instance scratch (ramfs uses it for the buffer pointer; statics
// like keyboard/serial leave it null).
#define k_sources_max 32

struct k_source {
  int  (*getc)(int fd);                 // returns 0..255, -1 = EOF / no data
  void (*putc)(int fd, int c);
  void (*flush)(int fd);
  bool (*ready)(int fd);                // non-blocking probe
  void (*close)(int fd);                // release per-fd state
  void *state;
};

// Slot 0: PS/2 keyboard. Blocks until a byte is queued, pumping the
// framebuffer in between so the cursor keeps blinking. No EOF on bare
// metal -- the kb queue is endless -- so the dispatcher's eof_seen
// latch never trips for slot 0.
static int kb_getc(int fd) {
  (void) fd;
  int b;
  while ((b = kqpop()) < 0) fbdraw(), k_wait();
  return b; }
static bool kb_ready(int fd) { (void) fd; return kkb.qh != kkb.qt; }

// Slot 1: serial console. Output goes to the framebuffer when one is
// present and is always mirrored to COM1. Flush triggers a frame draw.
static void serial_putc1(int fd, int c) {
  (void) fd;
  if (kcb) cb_putc(kcb, c);
  serial_putc(c); }
static void serial_flush(int fd) { (void) fd; fbdraw(); }

static struct k_source k_sources[k_sources_max] = {
  [0] = { .getc = kb_getc,      .ready = kb_ready    },
  [1] = { .putc = serial_putc1, .flush = serial_flush },
};

// Generic kernel dispatchers. ungetc/eof touch only header fields so
// they're identical across sources; getc/putc/flush route through
// k_sources[fd]. Bounds-checks and NULL-guards keep misuse from
// crashing (read-from-output-fd returns EOF; write-to-input-fd discards).
static struct ai *fd_getc(struct ai *g) {
  struct ai *fc = ai_core_of(g);
  struct ai_io *i = g->io;
  if (getcharm(i->ungetc_buf) != EOF) {
    fc->b = getcharm(i->ungetc_buf);
    i->ungetc_buf = putcharm(EOF);
    return g; }
  int fd = getcharm(i->fd);
  int c = -1;
  if (fd >= 0 && fd < k_sources_max && k_sources[fd].getc)
    c = k_sources[fd].getc(fd);
  if (c < 0) { i->eof_seen = putcharm(true); fc->b = EOF; }
  else fc->b = c;
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
  int fd = getcharm(g->io->fd);
  if (fd >= 0 && fd < k_sources_max && k_sources[fd].putc)
    k_sources[fd].putc(fd, c);
  return g; }
static struct ai *fd_flush(struct ai *g) {
  int fd = getcharm(g->io->fd);
  if (fd >= 0 && fd < k_sources_max && k_sources[fd].flush)
    k_sources[fd].flush(fd);
  return g; }

struct ai_io ai_stdin = { .ap = lvm_port_io,
                        .fd = putcharm(0), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false), };
struct ai_io ai_stdout = { .ap = lvm_port_io,
                         .fd = putcharm(1), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false), };
// No separate error stream; route err to the same fd as out (the console).
struct ai_io ai_stderr = { .ap = lvm_port_io,
                         .fd = putcharm(1), .ungetc_buf = putcharm(EOF), .eof_seen = putcharm(false), };

struct ai_port_vt const ai_fd_port_vt = { fd_getc, fd_ungetc, fd_eof, fd_putc, fd_flush, NULL, NULL };  // bulk lanes: the promised P3b, when ramfs/files need them

// Override the weak g.c default; route close through k_sources[fd].
// Statics (stdin/stdout) have NULL close -- nothing to release.
void ai_fd_close(int fd) {
  if (fd >= 0 && fd < k_sources_max && k_sources[fd].close)
    k_sources[fd].close(fd); }

bool ai_ready(int fd) {
  if (fd < 0) return true;
  if (fd >= k_sources_max || !k_sources[fd].ready) return false;
  return k_sources[fd].ready(fd); }

// Multi-source wait. ticks=0 means infinite. Future: program a one-shot
// timer at the deadline instead of waking every tick.
void ai_wait_fds(int const *fds, int n, uintptr_t ticks) {
  if (n <= 0) { ai_sleep(ticks); return; }
  if (n > ai_wait_fds_max) __builtin_trap();
  uintptr_t deadline = kticks + ticks;
  for (;;) {
    if (ticks && kticks >= deadline) return;
    for (int i = 0; i < n; i++) if (ai_ready(fds[i])) return;
    k_wait(); } }
uintptr_t ai_clock(void) { return kticks; }

// Pure time-wait. ticks=0 means infinite (caller is expected to chain with an
// input wait via ai_in->wait, so this should only be hit when no I/O is intended).
void ai_sleep(uintptr_t ticks) {
  uintptr_t deadline = kticks + ticks;
  for (;;) {
    if (ticks && kticks >= deadline) break;
    k_wait(); } }

static const uint8_t
  kb2ascii[] = {
     0,  27, '1',  '2', '3', '4', '5', '6',
   '7', '8', '9',  '0', '-', '=',   8,   9,
   'q', 'w', 'e',  'r', 't', 'y', 'u', 'i',
   'o', 'p', '[',  ']',  10,   0, 'a', 's',
   'd', 'f', 'g',  'h', 'j', 'k', 'l', ';',
  '\'', '`',   0, '\\', 'z', 'x', 'c', 'v',
   'b', 'n', 'm',  ',', '.', '/',   0, '*',
     0, ' ' },
  shift_kb2ascii[] = {
     0,  27, '!',  '@', '#', '$', '%', '^',
   '&', '*', '(',  ')', '_', '+',   8,   9,
   'Q', 'W', 'E',  'R', 'T', 'Y', 'U', 'I',
   'O', 'P', '{',  '}',  10,   0, 'A', 'S',
   'D', 'F', 'G',  'H', 'J', 'K', 'L', ':',
   '"', '~',   0,  '|', 'Z', 'X', 'C', 'V',
   'B', 'N', 'M',  '<', '>', '?',   0, '*',
     0, ' ' };

_Static_assert(countof(kb2ascii) == countof(shift_kb2ascii));

#define kb_code_left 75
#define kb_code_right 77
#define kb_code_up 72
#define kb_code_down 80
#define kb_code_home 71
#define kb_code_end 79
// decode a PS/2 scancode (interrupt context) and enqueue input bytes.
// arrows, Home, End, and Delete become the ANSI escape sequences the
// line editor decodes; with Ctrl held, Home / End emit the modified
// CSI form (`ESC [ 1 ; 5 H/F`) that the editor reads as buffer top /
// buffer end. Ctrl+letter becomes the matching control byte (so
// Ctrl-A/E reach the editor as home/end, Ctrl-D as quit).
void kb_int(const uint8_t code) {
  if (code == kb_code_extend) { kkb.g |= kb_flag_extend; return; }
  bool ext = kkb.g & kb_flag_extend, up = code & 128;
  uint8_t sc = code & 127;
  kkb.g &= ~kb_flag_extend;
  if (ext) switch (sc) {
    case kb_code_ctl: kkb.g = up ? kkb.g & ~kb_flag_rctl : kkb.g | kb_flag_rctl; return;
    case kb_code_alt: kkb.g = up ? kkb.g & ~kb_flag_ralt : kkb.g | kb_flag_ralt; return;
    case kb_code_delete:
      if (up) return;
      if (kkb.g & kb_flag_ctl && kkb.g & kb_flag_alt) k_reset();
      kq(27), kq('['), kq('3'), kq('~'); return;       // Delete -> CSI 3 ~
    case kb_code_left:  if (!up) kq(27), kq('['), kq('D'); return;
    case kb_code_right: if (!up) kq(27), kq('['), kq('C'); return;
    case kb_code_up:    if (!up) kq(27), kq('['), kq('A'); return;
    case kb_code_down:  if (!up) kq(27), kq('['), kq('B'); return;
    case kb_code_home:
      if (up) return;
      if (kkb.g & kb_flag_ctl) kq(27), kq('['), kq('1'), kq(';'), kq('5'), kq('H');
      else kq(27), kq('['), kq('H');
      return;
    case kb_code_end:
      if (up) return;
      if (kkb.g & kb_flag_ctl) kq(27), kq('['), kq('1'), kq(';'), kq('5'), kq('F');
      else kq(27), kq('['), kq('F');
      return;
    default: return; }
  switch (sc) {
    case kb_code_lshift: kkb.g = up ? kkb.g & ~kb_flag_lshift : kkb.g | kb_flag_lshift; return;
    case kb_code_rshift: kkb.g = up ? kkb.g & ~kb_flag_rshift : kkb.g | kb_flag_rshift; return;
    case kb_code_ctl:    kkb.g = up ? kkb.g & ~kb_flag_lctl : kkb.g | kb_flag_lctl; return;
    case kb_code_alt:    kkb.g = up ? kkb.g & ~kb_flag_lalt : kkb.g | kb_flag_lalt; return;
    default:
      if (up || sc >= countof(kb2ascii)) return;
      uint8_t a = (kkb.g & kb_flag_shift ? shift_kb2ascii : kb2ascii)[sc];
      if (a && kkb.g & kb_flag_ctl && (a | 32) >= 'a' && (a | 32) <= 'z') a &= 31;
      if (a) kq(a);
      return; } }


static ai_inline struct mem *after(struct mem *r) {
  return (struct mem*) ((uintptr_t*) r + r->len); }

static void *kmallocw(uintptr_t n) {
  if (!n) return NULL;
  void *p = NULL;
  struct mem *r = NULL, *t;
  while (kmem && kmem->len < n + 2 * Width(struct mem))
    t = kmem,
    kmem = t->next,
    t->next = r,
    r = t;
  if (kmem)
    kmem->len -= n + Width(struct mem),
    t = after(kmem),
    t->len = Width(struct mem) + n,
    p = t->_;
  while (r)
    t = r,
    r = t->next,
    t->next = kmem,
    kmem = t;
  return p; }

static void kfree(void *p) {
  if (!p) return;
  struct mem *m = (struct mem*)p - 1, *r = NULL, *t;
  while (kmem && kmem < m)
    t = kmem,
    kmem = t->next,
    t->next = r,
    r = t;
  for (;; m = r, r = r->next) {
    if (kmem != after(m)) m->next = kmem;
    else m->len += kmem->len,
         m->next = kmem->next;
    kmem = m;
    if (!r) return; } }

void *malloc(size_t n) { return kmallocw(b2w(n)); }
void free(void *x) { return kfree(x); }

static lvm(ai_kreset) { return k_reset(), g; }

void fbdraw(void) {
  if (!kcb) return;                    // serial-only: no framebuffer console
  for (uint16_t i = 0, rows = kcb->rows; i < rows; i++)
    for (uint16_t j = 0, cols = kcb->cols; j < cols; j++) {
      uint32_t const
       pos = (uint32_t) i * cols + j,
       _g = kcb->cb[pos];
      struct font *ff = fonts[cb_font(_g)];
      uint8_t const
       face = cb_face(_g),
       g = _g,
       *bmp = ff->glyphs + ff->h * (g == '\n' ? 0 : g);
      bool invert = kcb->flag & cb_show && kcb->wpos == pos && kticks & 64;
      uint8_t fgx = cb_fg(_g);
      if (face & cb_bold && fgx < 8) fgx += 8;      // bold as the bright half
      uint32_t fg = palette[fgx], bg = palette[cb_bg(_g)];
      if (face & cb_rev) fg ^= bg, bg ^= fg, fg ^= bg;
      if (invert) fg ^= bg, bg ^= fg, fg ^= bg;
      uintptr_t y = (uintptr_t) i * ff->h, x = (uintptr_t) j * ff->w;
      for (uint8_t r = 0; r < ff->h; r++) {
        bool ul = face & cb_under && r == ff->h - 1u;  // underline: the last scanline
        for (uint8_t o = bmp[r], c = ff->w; c--; o >>= 1)
          kfb._[(y + r) * kfb.pitch + x + c] = ul || o & 1 ? fg : bg; } } }

static lvm(draw) {
  fbdraw();
  k_wait();
  Ip += 1;
  return Continue(); }


static lvm(key) {
 int b = kqpop();
 Sp[0] = putcharm(b < 0 ? 0 : b);
 Ip += 1;
 return Continue(); }

static lvm(color) {
 uint8_t fg = getcharm(*Sp++), bg = getcharm(*Sp++);
 if (kcb) {
  cb_attr(kcb, fg, bg, 0);
  for (uint32_t i = 0, j = kcb->rows * kcb->cols; i < j; i++)
   kcb->cb[i] = cb_cell(cb_ch(kcb->cb[i]), fg, bg, 0); }
 return Ip += 1, Continue(); }

// (fault n) -- deliberately raise a CPU exception to exercise the
// ap in arch.c. k_fault_trigger (in each arch's arch.c) maps n
// to a concrete fault: the cases mirror x86_64 vector numbers, and the
// per-arch implementation picks the analogous fault for that target.
// the ap reports and halts, so k_fault_trigger does not return;
// the post-call statements are reachable only if the fault did not fire.
static lvm(lvm_fault) {
  k_fault_trigger(getcharm(Sp[0]));
  Ip += 1;
  return Continue(); }

#ifdef K_TEST
// (exit code) -- quit qemu; the test corpus calls it on completion / failure.
static lvm(lvm_kexit) { k_qemu_exit(getcharm(Sp[0])); Ip += 1; return Continue(); }
#endif



static union u
  nif_reset[] = {{ai_kreset}},
  nif_draw[] = {{draw}, {lvm_ret0}},
  nif_key[] = {{key}, {lvm_ret0}},
  nif_color[] = {{lvm_cur}, {.x = putcharm(2)}, {color}, {lvm_ret0}},
#ifdef K_TEST
  nif_exit[] = {{lvm_kexit}, {lvm_ret0}},
#endif
  nif_fault[] = {{lvm_fault}, {lvm_ret0}};

// Reads the bootloader-populated kboot struct (Limine or UEFI) and
// links every reported free range into the kernel free list. The
// chained-into-kmem order matches the previous Limine-walk order:
// entries are pushed in array order, so kmem ends up pointing at the
// last entry, with earlier entries linked through ->next.
static bool meminit(void) {
  if (!kboot.ram_n) return false;
  for (uint32_t i = 0; i < kboot.ram_n; i++) {
    struct mem *m = (struct mem*) (kboot.hhdm + kboot.ram[i].base);
    m->len = kboot.ram[i].len / sizeof(uintptr_t);
    kram_words += m->len;
    m->next = kmem;
    kmem = m; }
  return true; }

static bool fbinit(void) {
  if (!kboot.has_fb) return false;
  kfb._      = kboot.fb.base;
  kfb.width  = kboot.fb.w;
  kfb.height = kboot.fb.h;
  kfb.pitch  = kboot.fb.pitch_px;
  return true; }

static bool cbinit(void) {
  const uintptr_t rows = kfb.height / kfont.h,
                  cols = kfb.width / kfont.w;
  if (!(kcb = malloc(sizeof(struct cb) + rows * cols * sizeof(uint32_t)))) return false;
  cb_open(kcb, rows, cols);
  kcb->flag |= cb_lnm;  // the kernel console's discipline: a bare \n is a newline
  cb_attr(kcb, 47, 56, 0);
  cb_fill(kcb, 0);
  return true; }

static struct ai_def defs[] = {
  {"reset", (intptr_t) nif_reset},
  {"draw", (intptr_t) nif_draw},
  {"key", (intptr_t) nif_key},
  {"fault", (intptr_t) nif_fault},
#ifdef K_TEST
  {"exit", (intptr_t) nif_exit},
#endif
  {"color", (intptr_t) nif_color} };

#ifdef K_TEST
// The whole test corpus, baked VERBATIM to a C string literal by tools/lcatv.l
// (Makefile out/lib/ktests.h). Bound to the global `tests` and run through ev at boot.
static char const ktests[] =
#include "ktests.h"
;
#endif

void kmain(void) {
#if defined(__x86_64__)
 // Enable x87/SSE before ANY other C runs -- a compiler vectorizes freely on
 // x86_64 (even the struct copies in limine_to_kboot below compile to movups),
 // and that #UDs into a triple fault with no output while SSE is masked. This
 // is the single SSE-enable point; archinit no longer repeats it.
 k_sse_enable();
#endif
 // Copy the requested Limine responses into kboot before anything else
 // reads it.
 limine_to_kboot();
 khhdm = kboot.hhdm;
 archinit();
 serial_init();
 // the heap (meminit) is the only hard requirement. the framebuffer
 // console is optional: when fbinit/cbinit fail -- no Limine
 // framebuffer, or the console buffer won't allocate -- kcb stays null
 // and the kernel runs headless on the serial console alone.
 if (meminit()) {
  if (fbinit() && cbinit()) palette_init();
  struct ai *g = ai_defn(ai_ini(), defs, countof(defs));
  // BOUND the generational collector to the device's RAM (the Appel knob): without it the nursery's
  // copy-overhead resizer grows unbounded and gen_major's worst-case (all-survive) sizing then asks
  // kmallocw for a contiguous block bigger than physical RAM -> OOM. An eighth of free RAM leaves ample
  // headroom for the major's double-buffered resize, the kernel free list, and kmallocw fragmentation.
  // (The host runs g->budget == 0 / unbounded -- it has virtual memory and a fragmentation-proof malloc.)
  if (ai_ok(g)) ai_core_of(g)->budget = kram_words / 8;
#ifdef K_TEST
  // bind the baked corpus to the global `tests`; below it is read form-by-form
  // and run through ev at boot (no console), then qemu is quit.
  g = ai_strof(g, ktests);
  struct ai_def td[] = {{"tests", ai_pop1(g)}};
  g = ai_defn(g, td, countof(td));
#endif
  // the module sources, name-keyed (see host/main.c): registered in the source
  // library, loaded by `use` in the boot text below -- one layer per load, leave
  // registers, the splice serves the bare names (the console editor reads bao's).
  static char const src_uu[] =
#include "uu.h"
  ;
  static char const src_bao[] =
#include "bao.h"
  ;
  g = ai_lib_(g, "uu", src_uu);
  g = ai_lib_(g, "bao", src_bao);
#ifdef K_TEST
  static char const src_coin[] =
#include "coin.h"
  ;
  static char const src_rng[] =
#include "rng.h"
  ;
  static char const src_q[] =
#include "q.h"
  ;
  static char const src_kanren[] =
#include "kanren.h"
  ;
  g = ai_lib_(g, "coin", src_coin);
  g = ai_lib_(g, "rng", src_rng);
  g = ai_lib_(g, "q", src_q);
  g = ai_lib_(g, "kanren", src_kanren);
#endif
  // load the prel, then run the l read-eval-print loop. its line
  // editor (in love/bao.l, the baked shell core) drives the console; PS/2 keyboard
  // and serial input both arrive as ANSI escape sequences the l edev decodes.
  struct ai *r = ai_evals_(g, "("
#include "egg.h"
 ai_egg_pre
#include "prel.h"
 " "
#include "ev.h"
 ai_egg_post
 "(use 'uu) (: uu (from 'uu))"                         // the uu kernel: the corpus's uu files drive it through the
 "(use 'bao)"                                          //   one-name `uu` surface on this target too
#ifdef K_TEST
 "(use 'coin)"                                         // the optional library layers, test build ONLY: the corpus asserts on
 "(use 'rng)"                                          //   coin, rng, q and kanren, a booting kernel wants none of them -- so
 "(use 'q)"                                            //   the shipped image carries no ring/monoid, no random stream, no
 "(use 'kanren)"                                       //   rationals and no unifier (~65K of heap for the last two alone)
#endif
  );
  // THE SESSION: a fresh writable layer, C-side (the host's run_program shape) --
  // the shell's defglobs (and the corpus stream's) land here, never in the base.
  r = ai_layer_(r);
#ifdef K_TEST
  // test build: drink the baked `tests` string (string -> charlist -> tap port)
  // through reads (love/bao.l) -- the same stream shell as the host's stdin runner.
  // zz-fin.l prints the summary and (exit 1)s on failure. (`tap` builds the port;
  // `sip` is the verb that draws ONE unit -- see the vessel frame in love/prel.l.)
  r = ai_evals_(r, "(reads (tap ((: (g i) (? (< i (tally tests)) (link (peep tests i 0) (g (+ 1 i))))) 0)))");
#else
  r = ai_evals_(r, "((from 'bao 'shell) 0)");
#endif
  // a terminal scare gets the honest face on the serial console before reset
  if (ai_code_of(r) == ai_status_scare) ai_scare_face_(r);
  ai_fin(r); }
#ifdef K_TEST
 k_qemu_exit(0);   // corpus done with no failures -> quit qemu (exit 0)
#endif
 k_reset(); }
