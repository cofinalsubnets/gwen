#include "limine.h"
#include "k.h"
#include "love.h"
#include "quay.h"
#include "asmops.h"                    // the privileged instructions, both spellings
#include <stdarg.h>
#include <limits.h>
#include <string.h>

uint64_t kticks;
// the timer runs at 100 Hz on both arches (mkvec.l's PIT divisor, aarch64's
// cntfrq/100), so one tick is this many milliseconds -- the granularity every
// deadline below rounds up to.
#define k_tick_ms 10
static uintptr_t k_ticks_for(uintptr_t ms) { return (ms + k_tick_ms - 1) / k_tick_ms; }
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
// the line editor decodes; kb_readn and the (key) builtin drain the queue.
// g holds the live modifier flags.
static struct { uint8_t g, q[16], qh, qt; uint16_t lost; } kkb;
// enqueue one input byte. non-static: the COM1 serial RX ap (k_uart, in
// x86_64/arch.c) feeds this same queue.
// ⚠ A DROPPED KEYSTROKE SAYS SO. an interrupt cannot wait, so the ring must be
// bounded and a fast paste can outrun it -- but a byte vanishing in SILENCE is
// the one input failure a user cannot diagnose, and no size makes it diagnosable.
// so the drop is counted and serial_flush says how many fell (below). the count
// SATURATES rather than wrapping: "65535" understates, 0 would lie.
void kq(uint8_t b) {
  uint8_t n = (kkb.qt + 1) & 15;
  if (n != kkb.qh) kkb.q[kkb.qt] = b, kkb.qt = n;
  else if (kkb.lost != (uint16_t) -1) kkb.lost++; }
static int kqpop(void) {                   // dequeue one byte, -1 if empty
  if (kkb.qh == kkb.qt) return -1;
  int b = kkb.q[kkb.qh];
  return kkb.qh = (kkb.qh + 1) & 15, b; }

// the xterm-256 palette rides .rodata, laid by quay.l through clay -- the SAME table
// host/cb.c reads, so a cell means the same pixels in a window and on the framebuffer
// by construction rather than by two copies of the recipe agreeing.
#include "xterm256.h"
#define palette xterm256
static struct font
 kfont = { .glyphs = (uint8_t*) moderndos_8x16, .w = 8, .h = 16, },
 *fonts[16] = { &kfont };



void k_reset(void), archinit(void), fbdraw(void), serial_init(void), serial_putc(int),
     k_fault_trigger(intptr_t n);
uint64_t k_rtc(void);                  // the machine's own clock, unix seconds (0 = none)
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
  if (date_req.response && date_req.response->timestamp > 0)
    kboot.date = (uint64_t) date_req.response->timestamp;
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
// the end, ready returns false). Both directions can be bulk; a row carrying no
// writen is written a byte at a time instead, which is all a console can take
// either way. `state` is per-instance scratch (a ramfs fd holds its handle
// there; statics like keyboard/serial leave it null).
//
// ⚠ THE TABLE GROWS; IT DOES NOT CAP. it was a `k_source[32]` with five `fd <
// k_sources_max` bounds checks around it -- unreachable while nothing wrote it,
// and the rung-6 sweep left it as a rule in prose (doc/io.md) rather than a fix.
// this is the fix: k_source_open is the ONE door in, and it grows the table
// through g->alloc, which down here lands in the KERNEL'S OWN HEAP. the bug a
// ceiling would have shipped is worse than the host's was: not a hang but a
// silent refusal to open the 33rd thing.
// ⚠ inle DEFINES the malloc family (below the allocator) rather than importing
// one, so a bare malloc() here would be ours and would work -- and would read
// exactly like the libc call nothing in this tree is allowed to make. the door
// is g->alloc everywhere it can be reached; kmallocw where g does not exist yet.
void *malloc(size_t n);
void free(void *x);

struct k_source {
  // the read door (love.h's readn contract, one fd deeper): >0 = bytes,
  // 0 = nothing waiting, -1 = end. it USED to be a per-byte getc answering
  // "-1 = EOF / no data" -- one sentinel, two meanings -- and the keyboard paid
  // for it by spinning the whole vm on an empty queue.
  intptr_t (*readn)(int fd, unsigned char *dst, uintptr_t n);
  // the bulk write door, the same contract mirrored: >0 = bytes taken, 0 = busy,
  // -1 = gone. a row carrying one is asked instead of putc -- which is how the
  // ramfs REFUSES an allocation it could not get, where a void putc could only
  // drop the byte in silence.
  intptr_t (*writen)(int fd, unsigned char const *src, uintptr_t n);
  void (*putc)(int fd, int c);
  void (*flush)(int fd);
  bool (*ready)(int fd);                // non-blocking probe
  void (*close)(int fd);                // release per-fd state
  void *state;
};

// Slot 0: PS/2 keyboard. Drains what the interrupt queued and answers 0 when
// there is nothing -- never the end, because the kb queue is endless on bare
// metal. It used to SPIN here (`while ((b = kqpop()) < 0) fbdraw(), k_wait();`),
// computing this same answer and throwing it away; the scheduler owns that wait
// now.
static intptr_t kb_readn(int fd, unsigned char *dst, uintptr_t n) {
  (void) fd;
  uintptr_t k = 0;
  for (int b; k < n && (b = kqpop()) >= 0; ) dst[k++] = (unsigned char) b;
  return (intptr_t) k; }
static bool kb_ready(int fd) { (void) fd; return kkb.qh != kkb.qt; }

// Slot 1: serial console. Output goes to the framebuffer when one is
// present and is always mirrored to COM1. Flush triggers a frame draw.
static void serial_putc1(int fd, int c) {
  (void) fd;
  if (kcb) cb_putc(kcb, c);
  serial_putc(c); }
// the loud edge for kq's drops: the console is about to be shown, so say what
// the keyboard ring could not hold before the frame goes up.
static void serial_flush(int fd) {
  (void) fd;
  if (kkb.lost) {
    char d[6];
    int i = 0;
    unsigned v = kkb.lost;
    kkb.lost = 0;
    for (char const *s = "\n; input lost: "; *s; s++) serial_putc1(1, *s);
    do d[i++] = (char) ('0' + v % 10); while ((v /= 10));
    while (i) serial_putc1(1, d[--i]);
    for (char const *s = " bytes\n"; *s; s++) serial_putc1(1, *s); }
  fbdraw(); }

// ⚠ THE BOOT ROWS ARE STATIC ON PURPOSE, and must stay that way: the console is
// how the kernel says anything at all -- including that an allocation failed --
// so it cannot itself be the first thing that needs one. everything past them is
// heap.
static struct k_source k_boot[] = {
  [0] = { .readn = kb_readn,    .ready = kb_ready    },
  [1] = { .putc = serial_putc1, .flush = serial_flush },
};
static struct k_source *k_sources = k_boot;
static int k_sources_n = (int) countof(k_boot);

// the row for fd, or NULL -- the ONE bounds check in the file, so no dispatcher
// carries a limit of its own.
static ai_inline struct k_source *k_source(int fd) {
  return fd >= 0 && fd < k_sources_n ? &k_sources[fd] : NULL; }

// THE DOOR IN: answer fd's row, making room for it first. Doubling from the boot
// rows, copying, and freeing the old table unless it is the static one -- there
// is no realloc down here. -> NULL when there is no memory, which is a REFUSAL
// the caller must read; nothing is ever silently dropped, which is the whole
// difference between this and the ceiling it replaces.
// the ramfs is the caller: every open file is a row past the boot two, so the grow
// branch runs on the first one (test/kernel/ramfs.l).
struct k_source *k_source_open(struct ai *g, int fd) {
  if (fd < 0) return NULL;
  if (fd >= k_sources_n) {
    int m = k_sources_n;
    while (m <= fd) m *= 2;
    struct k_source *t = g->alloc(g, NULL, (size_t) m * sizeof *t);
    if (!t) return NULL;
    for (int i = 0; i < m; i++)
      t[i] = i < k_sources_n ? k_sources[i] : (struct k_source) {0};
    if (k_sources != k_boot) g->alloc(g, k_sources, 0);
    k_sources = t, k_sources_n = m; }
  return &k_sources[fd]; }

// Generic kernel dispatchers: readn/putc/flush route through k_sources[fd].
// The NULL-guards keep misuse from crashing (read-from-output-fd reads the end;
// write-to-input-fd discards).
static intptr_t fd_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  int fd = (int) ai_io_fd(g->io);
  struct k_source *s = k_source(fd);
  if (!s || !s->readn) return -1;
  return s->readn(fd, dst, n); }
static intptr_t fd_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  int fd = (int) ai_io_fd((*fp)->io);
  struct k_source *s = k_source(fd);
  if (!s) return (intptr_t) n;
  if (s->writen) return s->writen(fd, src, n);
  if (!s->putc) return (intptr_t) n;
  for (uintptr_t k = 0; k < n; k++) s->putc(fd, src[k]);
  return (intptr_t) n; }
static struct ai *fd_flush(struct ai *g) {
  int fd = (int) ai_io_fd(g->io);
  struct k_source *s = k_source(fd);
  if (s && s->flush) s->flush(fd);
  return g; }

struct ai_fio ai_stdin = { { .ap = lvm_port_io,
                        .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(0) };
struct ai_fio ai_stdout = { { .ap = lvm_port_io,
                         .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(1) };
// No separate error stream; route err to the same fd as out (the console).
struct ai_fio ai_stderr = { { .ap = lvm_port_io,
                         .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(1) };

struct ai_port_vt const ai_fd_port_vt = { fd_flush, fd_writen, fd_readn, NULL };

// Override the weak g.c default; route close through k_sources[fd].
// Statics (stdin/stdout) have NULL close -- nothing to release.
void ai_fd_close(int fd) {
  struct k_source *s = k_source(fd);
  if (s && s->close) s->close(fd); }

// the kernel has no write-direction probe: a k_source that can take a byte can
// always take one, so an OUT park is ready by definition.
bool ai_ready(int fd, int events) {
  if (fd < 0) return true;
  if (events != ai_wait_in) return true;
  struct k_source *s = k_source(fd);
  return s && s->ready && s->ready(fd); }

// Multi-source wait. ticks=0 means infinite. Future: program a one-shot
// timer at the deadline instead of waking every tick.
// ⚠ RECORD WHICH SOURCE ANSWERED, don't just return on the first: the scheduler
// reads `revents` back and skips re-asking about every fd it names (love.h). A
// sweep of the whole block costs one flag read per source and saves the scheduler
// a walk of the ring per parked task.
void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) {
  if (n <= 0) { ai_sleep(ms); return; }
  uintptr_t deadline = kticks + k_ticks_for(ms);
  for (;;) {
    int any = 0;
    for (int i = 0; i < n; i++) {
      int r = ai_ready(fds[i].fd, fds[i].events);
      fds[i].revents = r ? fds[i].events : 0;
      any |= r; }
    if (any || (ms && kticks >= deadline)) return;
    k_wait(); } }

// ⚠ MILLISECONDS SINCE THE EPOCH, the host's scale exactly (its ai_clock is
// CLOCK_REALTIME in ms) -- one scale for the scheduler's deadlines, for (clock t),
// and for every mtime. This used to answer kticks: an uptime in TENTHS OF A SECOND
// wearing the millisecond name, which made (rest 30) a third of a second and every
// date a fiction. The date rides kboot (limine's, or the machine's RTC); when
// nobody knew it, this degrades to milliseconds since boot and says so by reading
// as 1970.
uintptr_t ai_clock(void) { return (uintptr_t) (kboot.date * 1000 + kticks * k_tick_ms); }

// Pure time-wait. ms=0 means infinite (caller is expected to chain with an
// input wait via ai_in->wait, so this should only be hit when no I/O is intended).
void ai_sleep(uintptr_t ms) {
  uintptr_t deadline = kticks + k_ticks_for(ms);
  for (;;) {
    if (ms && kticks >= deadline) break;
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

// --- the ramfs: the baked tree, and the copies writes make -----------------
// The initrd is .rodata. tools/lcatfs.l bakes one {path, bytes, len} row per file
// (out/lib/kfs.h) the way lcatv bakes the test corpus, and reads come straight off
// it; the FIRST write copies that blob into the kernel heap and the entry reads
// from the copy ever after. So a file nobody writes costs a row and not one word
// of the bounded heap -- bake generously, copy lazily -- and two opens of one path
// see each other's writes, because the copy is per FILE and never per fd.
//
// ⚠ kmallocw/kfree, not g->alloc: a vt method is handed an fd and nothing else, so
// g is out of reach at the door that grows a file. On this seat they are the same
// heap (g->alloc is love.c's ai_libc_alloc -> malloc -> kmallocw, defined above),
// which is why cbinit already names it directly for the same reason.
// ⚠ ms is the SOURCE's mtime, baked: the initrd carries no directory, so the date a
// file was last written on the machine that built it exists nowhere else.
struct k_file { char const *path, *bytes; uintptr_t len, ms; };
static struct k_file const kfiles[] = {
#include "kfs.h"
};

// the mutable half, one slot per baked row. ⚠ `own` is the presence bit and has to
// be one: a file written and then emptied is {NULL, 0}, which is what a file still
// in .rodata looks like too, so the flag is the only thing that says which blob to
// read -- the tree's presence law wearing its C face.
static struct { unsigned char *bytes; uintptr_t len, cap, ms; bool own; }
  kfsw[countof(kfiles)];

// one open file: which row, where in it, and whether writes are allowed. rides the
// k_source row's `state`; the close door frees it.
struct k_fh { int i; uintptr_t pos; bool w; };

static intptr_t ram_readn(int fd, unsigned char *dst, uintptr_t n);

// ⚠ the handle behind an fd, and NOTHING for a row that is not the ramfs's: `state`
// is per-instance scratch of whatever kind its row's methods please, so the read
// door is what says it means a file handle. lseek reaches fds it did not open.
static ai_inline struct k_fh *k_fh(int fd) {
  struct k_source *s = k_source(fd);
  return s && s->readn == ram_readn ? s->state : NULL; }

// what row i reads as: the heap copy once there is one, the .rodata blob until then.
static unsigned char const *k_blob(int i, uintptr_t *len) {
  if (kfsw[i].own) return *len = kfsw[i].len, kfsw[i].bytes;
  return *len = kfiles[i].len, (unsigned char const*) kfiles[i].bytes; }

// the same question for the date: the write's stamp once there is a copy, the
// bake's until then.
static uintptr_t k_mtime(int i) { return kfsw[i].own ? kfsw[i].ms : kfiles[i].ms; }

// path -> row. LINEAR and unapologetic: the tree is a few dozen rows in .rodata,
// and a hash would cost a table the boot has to build before it can open the file
// that would have justified it.
static int k_find(char const *p, uintptr_t n) {
  for (int i = 0; i < (int) countof(kfiles); i++)
    if (strlen(kfiles[i].path) == n && !memcmp(kfiles[i].path, p, n)) return i;
  return -1; }

// make room for `need` bytes in row i's heap copy, bringing the .rodata blob across
// on the first write. -> false is a REFUSAL the caller must read and say; nothing
// is ever dropped quietly.
static bool k_fit(int i, uintptr_t need) {
  if (!kfsw[i].own) {
    uintptr_t n = kfiles[i].len, cap = n > need ? n : need;
    unsigned char *p = cap ? kmallocw(b2w(cap)) : NULL;
    if (cap && !p) return false;
    if (n) memcpy(p, kfiles[i].bytes, n);
    kfsw[i].bytes = p, kfsw[i].len = n, kfsw[i].cap = cap, kfsw[i].own = true;
    kfsw[i].ms = kfiles[i].ms;        // the copy inherits the bake's date; the write stamps it
    return true; }
  if (kfsw[i].cap >= need) return true;
  uintptr_t cap = kfsw[i].cap ? kfsw[i].cap : 64;
  while (cap < need) cap *= 2;
  unsigned char *p = kmallocw(b2w(cap));
  if (!p) return false;
  if (kfsw[i].len) memcpy(p, kfsw[i].bytes, kfsw[i].len);
  kfree(kfsw[i].bytes);
  kfsw[i].bytes = p, kfsw[i].cap = cap;
  return true; }

static intptr_t ram_readn(int fd, unsigned char *dst, uintptr_t n) {
  struct k_fh *h = k_fh(fd);
  if (!h) return -1;
  uintptr_t len;
  unsigned char const *p = k_blob(h->i, &len);
  // ⚠ the end, never 0: a file does not grow under its reader, so "nothing waiting"
  // would park the scheduler on a source that will never speak (doc/io.md).
  if (h->pos >= len) return -1;
  uintptr_t k = len - h->pos;
  if (k > n) k = n;
  memcpy(dst, p + h->pos, k);
  h->pos += k;
  return (intptr_t) k; }

static intptr_t ram_writen(int fd, unsigned char const *src, uintptr_t n) {
  struct k_fh *h = k_fh(fd);
  if (!h || !h->w) return -1;                  // read-only: gone, not silently taken
  if (!n) return 0;
  if (!k_fit(h->i, h->pos + n)) return -1;
  // a gap (a truncate under an append fd) reads as zeros, never as the bytes the
  // last tenant of that block left there.
  if (h->pos > kfsw[h->i].len)
    memset(kfsw[h->i].bytes + kfsw[h->i].len, 0, h->pos - kfsw[h->i].len);
  memcpy(kfsw[h->i].bytes + h->pos, src, n);
  h->pos += n;
  if (h->pos > kfsw[h->i].len) kfsw[h->i].len = h->pos;
  kfsw[h->i].ms = ai_clock();
  return (intptr_t) n; }

static bool ram_ready(int fd) { (void) fd; return true; }

static void ram_close(int fd) {
  struct k_source *s = k_source(fd);
  if (!s) return;
  kfree(s->state);
  *s = (struct k_source) {0}; }               // and the row is free again

// the lowest free row at or past the boot two -- POSIX's rule, which scripts lean
// on. A row is free when it carries no method at all, which is what k_source_open
// zeroes a fresh one to and what ram_close puts one back to.
static int k_fd_free(void) {
  for (int i = (int) countof(k_boot); i < k_sources_n; i++) {
    struct k_source *s = &k_sources[i];
    if (!s->readn && !s->writen && !s->putc && !s->flush && !s->ready && !s->close)
      return i; }
  return k_sources_n; }

// open a baked path -> its fd, or -1. m is r read, w truncate, a append -- the one
// door under both `open` (which reads it off a mode string) and `openfd` (off the
// charm host/posix.c spells 0/1/2). ⚠ NO CREATE: a path that is not baked answers
// -1 even for w, which is absence and not divergence -- the writable tree
// (mkdir/unlink/create) is rung 2.
static ai_noinline int k_ramopen(struct ai *g, struct ai_str *pv, char m) {
  if (m != 'r' && m != 'w' && m != 'a') return -1;
  int i = k_find(pv->bytes, pv->len);
  if (i < 0) return -1;
  int fd = k_fd_free();
  struct k_fh *h = kmallocw(b2w(sizeof *h));
  if (!h) return -1;
  struct k_source *s = k_source_open(g, fd);   // the grow door; -> NULL is no memory
  if (!s) return kfree(h), -1;
  // ⚠ the truncate lands LAST, past every way this can still fail: an open that
  // refuses must leave the file exactly as it found it.
  uintptr_t len = 0;
  if (m == 'w') kfsw[i].own = true, kfsw[i].len = 0, kfsw[i].ms = ai_clock();
  if (m == 'a') k_blob(i, &len);
  *h = (struct k_fh) { .i = i, .pos = len, .w = m != 'r' };
  *s = (struct k_source) { .readn = ram_readn, .writen = ram_writen,
                           .ready = ram_ready, .close = ram_close, .state = h };
  return fd; }

// (open path mode) -- host/main.c's lvm_open for the ramfs door: a heap port
// (closed on GC) or the zero point on any failure. The kernel links no host/*.c,
// so the shape is written fresh rather than shared -- doc/posix.md's conventions
// exactly, since kore reads these and a wrong one is silent.
static lvm(lvm_open) {
  if (!ai_strp(Sp[0]) || !ai_strp(Sp[1])) goto fail;
  struct ai_str *mv = (struct ai_str*) Sp[1];
  int fd = mv->len ? k_ramopen(g, (struct ai_str*) Sp[0], mv->bytes[0]) : -1;
  if (fd < 0) goto fail;
  Pack(g);
  struct ai *r = ai_io_alloc(g, fd);
  if (!ai_ok(r)) { ai_fd_close(fd); goto fail; }
  g = r;
  Unpack(g);
  // stack: [port, path, mode, ..] -> [port, ..]
  Sp[2] = Sp[0];
  Sp += 2;
  Ip += 1;
  ai_musttail return Continue();
 fail:
  Sp[1] = ZeroPoint;
  Sp += 1;
  Ip += 1;
  ai_musttail return Continue(); }

// (close p) -- flush, release the row, and HAND THE PORT THE CLOSED VT, so every
// later read/write/flush finds the door that does nothing and the finalizer, which
// asks the vt for an fd, skips. Answers (). No-op on a non-port.
static lvm(lvm_close) {
  if ((Sp[0] & 1) == 0 && ((union u*) Sp[0])->ap == lvm_port_io) {
    struct ai_io *io = (struct ai_io*) Sp[0];
    intptr_t fd = ai_io_fd(io);
    if (fd >= 0) {
      g->io = io;
      Pack(g);
      g = ai_io_wflush(g, io);        // buffered bytes land before the row dies
      if (!ai_ok(g)) return ghelp(g);
      // the device would not take the whole run: PARK and come back. nothing has
      // been mutated yet -- the row is live and Ip unadvanced -- so the re-run is
      // this same close from the top.
      if (ai_io_wpending(g, (struct ai_io*) g->sp[0])) {
        Unpack(g);
        g->next_wake_at = ai_clock() + 1;
        ai_musttail return Ap(lvm_yield_sw, g); }
      Unpack(g);
      ai_fd_close((int) fd);
      ((struct ai_io*) Sp[0])->vt = &ai_closed_vt; } }   // ⚠ re-read: wflush may collect
  Sp[0] = ZeroPoint;
  Ip += 1;
  ai_musttail return Continue(); }

// --- the file nifs: stat, readdir, lseek, openfd, fdclose -------------------
// doc/posix.md's conventions exactly, because kore reads these shapes and a wrong
// one is silent. ⚠ THE TREE IS FLAT: the initrd holds "lib/json.l" and no row for
// "lib", so a DIRECTORY here is a PREFIX that some path lies under, and its entries
// are the distinct next components of those paths. Nothing is stored for one, and
// nothing can be: rung 2's writable tree is what gives a directory an existence of
// its own.
#define k_mode_file 0100644            // (& mode 61440) = 32768: a regular file
#define k_mode_dir  0040755            //                = 16384: a directory

// the prefix a path names, its trailing slashes cut. "" and "." are both the root,
// as they are on the host, where they name the cwd.
static uintptr_t k_dirlen(struct ai_str *pv) {
  uintptr_t n = pv->len;
  while (n && pv->bytes[n - 1] == '/') n--;
  return n == 1 && pv->bytes[0] == '.' ? 0 : n; }

// row i's entry name under a prefix of pn bytes -- NULL when the row does not lie
// under it. A row deeper than one level answers its next COMPONENT, so a
// subdirectory is named by the paths inside it and by nothing else.
static char const *k_entry(int i, char const *p, uintptr_t pn, uintptr_t *len) {
  char const *q = kfiles[i].path;
  uintptr_t ql = strlen(q);
  if (pn) {
    if (ql <= pn + 1 || memcmp(q, p, pn) || q[pn] != '/') return NULL;
    q += pn + 1, ql -= pn + 1; }
  uintptr_t k = 0;
  while (k < ql && q[k] != '/') k++;
  return *len = k, q; }

// is this prefix a directory, and how new is it? -> its newest child's date, which
// is the only date a synthesized directory can honestly wear.
static bool k_dirstat(char const *p, uintptr_t pn, uintptr_t *ms) {
  bool any = false;
  *ms = 0;
  for (int i = 0; i < (int) countof(kfiles); i++) {
    uintptr_t k;
    if (!k_entry(i, p, pn, &k)) continue;
    any = true;
    if (k_mtime(i) > *ms) *ms = k_mtime(i); }
  return any; }

// (stat path) -> (size mtime-ms mode ns) | (). ⚠ ns is the ms date times a million,
// not a finer reading of it: this clock's last hand IS the millisecond (a 100 Hz
// tick over the boot date), and digits it does not have would be the wrong honesty.
ai_noinline static struct ai *k_stat(struct ai *g) {
  if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
  struct ai_str *pv = (struct ai_str*) g->sp[0];
  int i = k_find(pv->bytes, pv->len);
  uintptr_t size = 0, ms = 0, mode = k_mode_file;
  if (i >= 0) k_blob(i, &size), ms = k_mtime(i);
  else if (k_dirstat(pv->bytes, k_dirlen(pv), &ms)) mode = k_mode_dir;
  else return g->sp[0] = ZeroPoint, g;          // absent -> the real ()
  if (!ai_ok(g = ai_have(g, 4 * Width(struct ai_chain)))) return g;
  struct ai_chain *c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm((intptr_t) (ms * 1000000)), ZeroPoint);
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) mode), word(c));
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) ms), word(c));
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) size), word(c));
  return g->sp[0] = word(c), g; }
static lvm(lvm_stat) {
  Pack(g); g = k_stat(g);
  if (!ai_ok(g)) return ghelp(g);
  Unpack(g);
  ai_musttail return Next(1); }

// (readdir path) -> the entry names, one string each, or () -- for a path that is
// no directory as much as for one that is missing, which is the host's answer too.
// NO order promised (row order); "." and ".." are not entries here, since a flat
// tree has no link to hold them.
ai_noinline static struct ai *k_readdir(struct ai *g) {
  // ⚠ the prefix is COPIED out: every ai_have below may collect, and the string it
  // came from is a heap object that moves. The rows are .rodata and never do, which
  // is why the entries themselves are read in place.
  char pb[256], nb[128];
  if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
  struct ai_str *pv = (struct ai_str*) g->sp[0];
  uintptr_t pn = k_dirlen(pv);
  if (pn >= sizeof pb) return g->sp[0] = ZeroPoint, g;   // longer than any row: absent
  memcpy(pb, pv->bytes, pn);
  uintptr_t junk;
  if (!k_dirstat(pb, pn, &junk)) return g->sp[0] = ZeroPoint, g;
  g->sp[0] = ZeroPoint;                                 // the accumulator, over the path
  for (int i = 0; i < (int) countof(kfiles); i++) {
    uintptr_t k;
    char const *e = k_entry(i, pb, pn, &k);
    if (!e) continue;
    bool seen = false;                                  // one name per entry, not per row
    for (int j = 0; j < i && !seen; j++) {
      uintptr_t k2;
      char const *e2 = k_entry(j, pb, pn, &k2);
      seen = e2 && k2 == k && !memcmp(e, e2, k); }
    if (seen || k >= sizeof nb) continue;
    memcpy(nb, e, k), nb[k] = 0;                        // ai_strof's door is a C string
    if (!ai_ok(g = ai_strof(g, nb))) return g;          // pushes: name over acc
    if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
    struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                   g->sp[0], g->sp[1]); // slots re-read post-GC
    g->sp[1] = word(w);
    g->sp += 1; }                                       // pop the name
  return g; }
static lvm(lvm_readdir) {
  Pack(g); g = k_readdir(g);
  if (!ai_ok(g)) return ghelp(g);
  Unpack(g);
  ai_musttail return Next(1); }

// (lseek fd off whence) -> the new offset | -1. RAW fds, openfd's lane and never a
// port's -- a port buffers, and a seek under the buffer desyncs it. whence: 0 SET,
// 1 CUR, 2 END. ⚠ PAST THE END IS LEGAL and lands there; a write from that offset
// leaves a gap that reads as zeros (ram_writen), which is the hole POSIX promises.
static lvm(lvm_lseek) {
  intptr_t r = -1;
  struct k_fh *h = (Sp[0] & 1) ? k_fh((int) getcharm(Sp[0])) : NULL;
  if (h && (Sp[1] & 1)) {
    uintptr_t len;
    int wh = (Sp[2] & 1) ? (int) getcharm(Sp[2]) : 0;
    k_blob(h->i, &len);
    intptr_t at = getcharm(Sp[1])
                + (wh == 1 ? (intptr_t) h->pos : wh == 2 ? (intptr_t) len : 0);
    if (at >= 0) h->pos = (uintptr_t) at, r = at; }
  Sp[2] = putcharm(r);
  Sp += 2; ai_musttail return Next(1); }

// (openfd path mode) -> a RAW fd | -1. mode 0 read, 1 write+truncate, 2 append, the
// charm host/posix.c spells. ⚠ the failure is a bare -1 where the host answers
// -errno: down here the only failure IS absence, and there is no errno table to
// name it with -- the sign is what every caller reads either way.
static lvm(lvm_openfd) {
  intptr_t m = (Sp[1] & 1) ? getcharm(Sp[1]) : 0;
  Sp[1] = putcharm(!ai_strp(Sp[0]) ? -1
                   : k_ramopen(g, (struct ai_str*) Sp[0],
                               m == 1 ? 'w' : m == 2 ? 'a' : 'r'));
  Sp += 1; ai_musttail return Next(1); }

// (fdclose fd) -> (). openfd's other half. A row nobody opened is already closed,
// which is why this cannot fail and answers the zero point either way.
static lvm(lvm_fdclose) {
  if (Sp[0] & 1) ai_fd_close((int) getcharm(Sp[0]));
  Sp[0] = ZeroPoint;
  ai_musttail return Next(1); }

static lvm(ai_kreset) { return k_reset(), g; }

// paint ONE console row. `cur` is the cursor's cell (~0u when it is hidden) and
// `blink` its phase -- both passed IN, never read here: kticks is bumped by the timer
// ISR, so re-reading it per row could paint one row lit and the next one dark.
static void fbrow(uint16_t i, uint32_t cur, bool blink) {
  for (uint16_t j = 0, cols = kcb->cols; j < cols; j++) {
    uint32_t const
     pos = (uint32_t) i * cols + j,
     _g = kcb->cb[pos];
    struct font *ff = fonts[cb_font(_g)];
    uint8_t const
     face = cb_face(_g),
     g = _g,
     *bmp = ff->glyphs + ff->h * (g == '\n' ? 0 : g);
    bool invert = pos == cur && blink;
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

// the cursor as last PAINTED. quay marks the row of every grid WRITE, and the cursor
// is not one: cb_cur moves wpos in silence and the blink is a function of the clock.
// So the renderer owns the cursor, or the block stays where it last was.
static uint32_t fbcur = ~0u;
static bool fbblink;

// repaint what MOVED. quay marks each written row in cb->dmg and the contract is "a
// renderer reads-and-clears" (quay.h) -- so read it. This is called from serial_flush,
// and love flushes per WRITE, so painting the whole screen here cost a full-screen
// blit per character printed: on the door that hands over a framebuffer the corpus
// ran 3x slower than on the one that does not (measured 180s vs 61s under qemu, and
// on metal every one of those cells is a write over the PCI bus).
void fbdraw(void) {
  if (!kcb) return;                    // serial-only: no framebuffer console
  uint16_t const rows = kcb->rows, cols = kcb->cols;
  bool const blink = (kticks & 64) != 0;
  uint32_t const cur = kcb->flag & cb_show ? kcb->wpos : ~0u;
  // a hidden cursor's row is ~0u, which no row index equals, so it matches nothing.
  uint32_t const was = fbcur == ~0u ? ~0u : fbcur / cols,
                 now = cur == ~0u ? ~0u : cur / cols;
  bool const moved = cur != fbcur || blink != fbblink;
  for (uint16_t i = 0; i < rows; i++) {
    uint32_t const r = i > 255 ? 255 : i;   // quay's fold: bit 255 stands for 255-and-past
    if (kcb->dmg[r >> 5] >> (r & 31) & 1 || (moved && (i == was || i == now)))
      fbrow(i, cur, blink); }
  for (int k = 0; k < 8; k++) kcb->dmg[k] = 0;
  fbcur = cur, fbblink = blink; }

static lvm(draw) {
  fbdraw();
  k_wait();
  Ip += 1;
  ai_musttail return Continue(); }


static lvm(key) {
 int b = kqpop();
 Sp[0] = putcharm(b < 0 ? 0 : b);
 Ip += 1;
 ai_musttail return Continue(); }

static lvm(color) {
 uint8_t fg = getcharm(*Sp++), bg = getcharm(*Sp++);
 if (kcb) {
  cb_attr(kcb, fg, bg, 0);
  for (uint32_t i = 0, j = kcb->rows * kcb->cols; i < j; i++)
   kcb->cb[i] = cb_cell(cb_ch(kcb->cb[i]), fg, bg, 0); }
 ai_musttail return Next(1); }

// (fault n) -- deliberately raise a CPU exception to exercise the
// ap in arch.c. k_fault_trigger (in each arch's arch.c) maps n
// to a concrete fault: the cases mirror x86_64 vector numbers, and the
// per-arch implementation picks the analogous fault for that target.
// the ap reports and halts, so k_fault_trigger does not return;
// the post-call statements are reachable only if the fault did not fire.
static lvm(lvm_fault) {
  k_fault_trigger(getcharm(Sp[0]));
  Ip += 1;
  ai_musttail return Continue(); }

#ifdef K_TEST
// (exit code) -- quit qemu; the test corpus calls it on completion / failure.
static lvm(lvm_kexit) { k_qemu_exit(getcharm(Sp[0])); Ip += 1; ai_musttail return Continue(); }
#endif



static union u
  nif_reset[] = {{ai_kreset}},
  nif_draw[] = {{draw}, {lvm_ret0}},
  nif_key[] = {{key}, {lvm_ret0}},
  nif_color[] = {{lvm_cur}, {.x = putcharm(2)}, {color}, {lvm_ret0}},
  nif_open[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_open}, {lvm_ret0}},
  nif_close[] = {{lvm_close}, {lvm_ret0}},
  nif_stat[] = {{lvm_stat}, {lvm_ret0}},
  nif_readdir[] = {{lvm_readdir}, {lvm_ret0}},
  nif_lseek[] = {{lvm_cur}, {.x = putcharm(3)}, {lvm_lseek}, {lvm_ret0}},
  nif_openfd[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_openfd}, {lvm_ret0}},
  nif_fdclose[] = {{lvm_fdclose}, {lvm_ret0}},
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
  // ⚠ kmallocw, not g->alloc: kmain runs cbinit BEFORE ai_ini, because the console
  // is how a failure in ai_ini would be said. no g exists yet, so this names the
  // kernel heap directly rather than wearing malloc's face.
  if (!(kcb = kmallocw(b2w(sizeof(struct cb) + rows * cols * sizeof(uint32_t))))) return false;
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
  // the ramfs door. ⚠ `open`'s PRESENCE is what lights up prel's module walk
  // (love/prel.l's fsopen, by peep) and salt's config read -- both are gated on
  // the name being in the book, so this row is the whole wiring.
  {"open", (intptr_t) nif_open},
  {"close", (intptr_t) nif_close},
  // the rest of the read surface (rung 1). ⚠ these wear the HOST'S names and the
  // host's shapes on purpose: kore reads (size mtime mode ns) and a list of entry
  // strings, and a divergence here would be silent where an absence is loud.
  {"stat", (intptr_t) nif_stat},
  {"readdir", (intptr_t) nif_readdir},
  {"lseek", (intptr_t) nif_lseek},
  {"openfd", (intptr_t) nif_openfd},
  {"fdclose", (intptr_t) nif_fdclose},
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

// the module sources, name-keyed (see host/main.c): the source library `use` reads
// in the boot text below -- one layer per load, leave registers, the splice serves
// the bare names (the console editor reads bao's). .rodata: a source the kernel
// never loads costs a row and not one word of its bounded heap.
static char const src_uu[] =
#include "uu.h"
;
static char const src_bao[] =
#include "bao.h"
;
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
#endif
static struct ai_lib const libs[] = {
  {"uu", src_uu}, {"bao", src_bao},
#ifdef K_TEST
  {"coin", src_coin}, {"rng", src_rng}, {"q", src_q}, {"kanren", src_kanren},
#endif
  {NULL, NULL} };
struct ai_lib const *ai_libs(void) { return libs; }

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
 // the wall date, in the one order that can answer on every door: limine's if it
 // was asked and answered, else the machine's RTC -- which archinit has just made
 // reachable (the aarch64 read is device memory, and mmio_map lays it).
 if (!kboot.date) kboot.date = k_rtc();
 serial_init();
 // the heap (meminit) is the only hard requirement. the framebuffer
 // console is optional: when fbinit/cbinit fail -- no Limine
 // framebuffer, or the console buffer won't allocate -- kcb stays null
 // and the kernel runs headless on the serial console alone.
 if (meminit()) {
  if (fbinit()) cbinit();        // the framebuffer console; the palette is a table now
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
  // load the prel, then run the l read-eval-print loop. its line
  // editor (in love/bao.l, the baked shell core) drives the console; PS/2 keyboard
  // and serial input both arrive as ANSI escape sequences the l edev decodes.
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
