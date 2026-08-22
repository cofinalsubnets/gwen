#include "k.h"
#include "love.h"
#include "quay.h"
#include "asmops.h"                    // the privileged instructions, both spellings
#include <stdarg.h>
#include <limits.h>
#include <string.h>
#include <errno.h>      // the E numbers only (the host's, linux's) -- no errno variable down here

uint64_t kticks;
// the timer runs at 100 Hz on both arches (mkvec.l's PIT divisor, aarch64's
// cntfrq/100), so one tick is this many milliseconds -- the granularity every
// deadline below rounds up to.
#define k_tick_ms 10
static uintptr_t k_ticks_for(uintptr_t ms) { return (ms + k_tick_ms - 1) / k_tick_ms; }
// Higher-half direct map offset: physical address P is reachable at
// khhdm + P, taken from kboot's hhdm. Set before archinit,
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

// the console's font. the palette that goes with it lives in crew/quay/paint.c,
// which is the one place a cell becomes pixels.
static struct font const kfont = { .glyphs = (uint8_t*) moderndos_8x16, .w = 8, .h = 16 };



void k_reset(void), archinit(void), fbdraw(void), serial_init(void), serial_putc(int),
     k_fault_trigger(intptr_t n);
uint64_t k_rtc(void);                  // the machine's own clock, unix seconds (0 = none)
#ifdef K_TEST
void k_qemu_exit(int);
#endif

#include "quay.h"
#include <stdarg.h>
// kboot -- the machine as the door found it, filled BEFORE kmain reads it:
// pvh_to_kboot off qemu's hvm_start_info, the UEFI loader off the firmware
// memmap and GOP, the aarch64 stub off the DTB. nothing below asks which door
// answered, which is the whole point of the struct.
struct k_boot kboot;

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
// this is the fix: k_source_open is the ONE door in, and it grows the table in
// the KERNEL'S OWN HEAP. the bug a ceiling would have shipped is worse than the
// host's was: not a hang but a silent refusal to open the 33rd thing.
// ⚠ inle DEFINES the malloc family (below the allocator) rather than importing
// one, so a bare malloc() here would be ours and would work -- and would read
// exactly like the libc call nothing in this tree is allowed to make. the door
// is g->alloc everywhere it can be reached; kmallocw where g cannot be.
// ⚠ THIS DOOR IS ONE OF THOSE: it takes no g, because free/sys.c's open must
// reach it and a syscall arrives with none. Same heap either way (g->alloc is
// love.c's ai_libc_alloc -> malloc -> kmallocw), so what this buys is one
// allocator for one table rather than two that must agree.
void *malloc(size_t n);
void free(void *x);
static void *kmallocw(uintptr_t n);
static void kfree(void *p);

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
// heap. err carries its own fd (rung 4): the seat remap below tells 1 from 2, so
// a pipeline stage's out can ride a pipe while its scare face stays on the console
// -- the rows are twins, the NUMBERS are the distinction.
static struct k_source k_boot[] = {
  [0] = { .readn = kb_readn,    .ready = kb_ready    },
  [1] = { .putc = serial_putc1, .flush = serial_flush },
  [2] = { .putc = serial_putc1, .flush = serial_flush },
};
static struct k_source *k_sources = k_boot;
static int k_sources_n = (int) countof(k_boot);

// the row for fd, or NULL -- the ONE bounds check in the file, so no dispatcher
// carries a limit of its own.
static ai_inline struct k_source *k_source(int fd) {
  return fd >= 0 && fd < k_sources_n ? &k_sources[fd] : NULL; }

// ⚠ IN RANGE IS NOT OPEN, and a syscall face is the caller that has to care: a
// closed row is ZEROED where it stands (ram_close, pipe_rclose, pipe_wclose),
// never removed, so k_source keeps answering it. Carrying any method at all is
// what live means -- k_fd_free's rule, read the other way round.
static ai_inline bool k_row_live(int fd) {
  struct k_source const *s = k_source(fd);
  return s && (s->readn || s->writen || s->putc || s->flush || s->ready || s->close); }

// THE DOOR IN: answer fd's row, making room for it first. Doubling from the boot
// rows, copying, and freeing the old table unless it is the static one -- there
// is no realloc down here. -> NULL when there is no memory, which is a REFUSAL
// the caller must read; nothing is ever silently dropped, which is the whole
// difference between this and the ceiling it replaces.
// the ramfs is the caller: every open file is a row past the boot two, so the grow
// branch runs on the first one (test/kernel/ramfs.l).
struct k_source *k_source_open(int fd) {
  if (fd < 0) return NULL;
  if (fd >= k_sources_n) {
    int m = k_sources_n;
    while (m <= fd) m *= 2;
    struct k_source *t = kmallocw(b2w((uintptr_t) m * sizeof *t));
    if (!t) return NULL;
    for (int i = 0; i < m; i++)
      t[i] = i < k_sources_n ? k_sources[i] : (struct k_source) {0};
    if (k_sources != k_boot) kfree(k_sources);
    k_sources = t, k_sources_n = m; }
  return &k_sources[fd]; }

// --- rung 4: the seat table -- a process task's stdio, keyed by pid ------------
// the love machine's dup2-in-the-child: compiled code FOLDS the global in/out/err
// ports at its own compile (one book, one fold), so a pipeline stage cannot be
// redirected by any rebind -- the remap has to live UNDER the port, at the fd
// door. a seat maps the running task's fds 0/1/2 to real rows; every dispatcher
// below reads it through k_fd_eff. slot -1 is pass-through, -2 is seated CLOSED
// (an fdmap's () entry: reads answer the end, writes fall away).
// ⚠ THE SEAT IS THE PORT LAYER'S, AND ONLY ITS: k_fd_eff is reached from
// fd_readn, fd_writen, ai_fd_close and k_procseat -- never from a nif, which is
// why k_fdopen takes the fd it was handed. So an fd spelled in love is an
// absolute row, and free/sys.c's syscall door is seat-blind by the same law.
struct k_seat { intptr_t pid; int fd[3]; };
static struct k_seat *k_seats;
static int k_seats_n;

// the running task's pid: the run ring's head IS the running task (love.c's law),
// its pid at node[2]. the main task wears the zero point there, and reads as 0 --
// which no spawned pid can be (the mint stream pre-increments), so 0 = unseated.
static ai_inline intptr_t k_cur_pid(struct ai *g) {
  union u *t = ai_core_of(g)->tasks;
  return t && (t[2].x & 1) ? getcharm(t[2].x) : 0; }

static struct k_seat *k_seat_find(intptr_t pid) {
  for (int i = 0; i < k_seats_n; i++)
    if (k_seats[i].pid == pid) return &k_seats[i];
  return NULL; }

// a free slot, growing the table (kmallocw -- the table must outlive any one g
// frame, and the grow law is k_source_open's: double, copy, never cap).
static struct k_seat *k_seat_slot(void) {
  struct k_seat *s = k_seat_find(0);
  if (s) return s;
  int m = k_seats_n ? k_seats_n * 2 : 4;
  struct k_seat *t = kmallocw(b2w((uintptr_t) m * sizeof *t));
  if (!t) return NULL;
  for (int i = 0; i < m; i++)
    t[i] = i < k_seats_n ? k_seats[i] : (struct k_seat) { 0, {-1, -1, -1} };
  kfree(k_seats);
  k_seats = t;
  s = &t[k_seats_n];
  k_seats_n = m;
  return s; }

// the running task's EFFECTIVE fd: 0/1/2 through its seat, everything else as
// spelled. -1 out of a seated-closed slot reads as no row at all (k_source(-1)
// is NULL), which is the end for a reader and the void for a writer.
static int k_fd_eff(struct ai *g, int fd) {
  if (fd < 0 || fd > 2 || !k_seats_n) return fd;
  intptr_t pid = k_cur_pid(g);
  struct k_seat *s = pid ? k_seat_find(pid) : NULL;
  if (!s || s->fd[fd] == -1) return fd;
  return s->fd[fd] == -2 ? -1 : s->fd[fd]; }

// Generic kernel dispatchers: readn/putc/flush route through k_sources[fd],
// the fd first read through the running task's seat (rung 4). The NULL-guards
// keep misuse from crashing (read-from-output-fd reads the end;
// write-to-input-fd discards).
// the row-level motions, on an ALREADY-RESOLVED fd. The port dispatchers below
// resolve through the running task's seat first; free/sys.c's syscall door does
// not, which is the one difference between the two callers.
static intptr_t k_row_read(int fd, unsigned char *dst, uintptr_t n) {
  struct k_source *s = k_source(fd);
  if (!s || !s->readn) return -1;
  return s->readn(fd, dst, n); }
static intptr_t k_row_write(int fd, unsigned char const *src, uintptr_t n) {
  struct k_source *s = k_source(fd);
  if (!s) return (intptr_t) n;
  if (s->writen) return s->writen(fd, src, n);
  if (!s->putc) return (intptr_t) n;
  for (uintptr_t k = 0; k < n; k++) s->putc(fd, src[k]);
  return (intptr_t) n; }
static intptr_t fd_readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  return k_row_read(k_fd_eff(g, (int) ai_io_fd(g->io)), dst, n); }
static intptr_t fd_writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  return k_row_write(k_fd_eff(*fp, (int) ai_io_fd((*fp)->io)), src, n); }

// free/sys.c's door: the POSIX shapes over the same rows. ⚠ the port layer says
// END with -1 and read(2) says it with 0, so the ends are translated here rather
// than in the syscall table, where every future row would have to remember.
long k_fd_write(int fd, void const *b, long n) {
  if (n < 0) return -22;                                 // EINVAL
  return (long) k_row_write(fd, (unsigned char const *) b, (uintptr_t) n); }
long k_fd_read(int fd, void *b, long n) {
  if (n < 0) return -22;
  intptr_t r = k_row_read(fd, (unsigned char *) b, (uintptr_t) n);
  return r < 0 ? 0 : (long) r; }
long k_fd_close(int fd) {
  if (!k_row_live(fd)) return -9;                        // EBADF
  ai_fd_close(fd);
  return 0; }
static struct ai *fd_flush(struct ai *g) {
  int fd = k_fd_eff(g, (int) ai_io_fd(g->io));
  struct k_source *s = k_source(fd);
  if (s && s->flush) s->flush(fd);
  return g; }

struct ai_fio ai_stdin = { { .ap = lvm_port_io,
                        .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(0) };
struct ai_fio ai_stdout = { { .ap = lvm_port_io,
                         .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(1) };
struct ai_fio ai_stderr = { { .ap = lvm_port_io,
                         .vt = &ai_fd_port_vt, .ungetc_buf = putcharm(EOF) }, .fd = putcharm(2) };

struct ai_port_vt const ai_fd_port_vt = { fd_flush, fd_writen, fd_readn, NULL };

// Override the weak g.c default; route close through k_sources[fd].
// Statics (stdin/stdout) have NULL close -- nothing to release.
void ai_fd_close(int fd) {
  struct k_source *s = k_source(fd);
  if (s && s->close) s->close(fd); }

// the kernel has no write-direction probe: a k_source that can take a byte can
// always take one, so an OUT park is ready by definition.
// ⚠ a SEATED reader parks wearing its PORT's fd (love.c records ai_io_fd, which
// for the folded stdin is 0), and by wake time the asker is not the running task
// -- so a query on 0 sweeps every seat's read slot and takes the false wake: the
// woken reader re-asks through its own seat and re-parks. seats are pipeline
// stages, a handful; the spurious wake costs one re-read.
bool ai_ready(int fd, int events) {
  if (fd < 0) return true;
  if (events != ai_wait_in) return true;
  struct k_source *s = k_source(fd);
  if (s && s->ready && s->ready(fd)) return true;
  if (fd == 0)
    for (int i = 0; i < k_seats_n; i++)
      if (k_seats[i].pid && k_seats[i].fd[0] >= 0) {
        struct k_source *t = k_source(k_seats[i].fd[0]);
        if (t && t->ready && t->ready(k_seats[i].fd[0])) return true; }
  return false; }

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
// date a fiction. The date rides kboot (the door's, or the machine's RTC); when
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

_Static_assert(countof(kb2ascii) == countof(shift_kb2ascii), "one scancode table, two faces");

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

// the tree itself (rung 2): a table of ENTRIES in the kernel heap, one per baked
// row at first touch, growing as create and mkdir add paths the bake never knew.
// ⚠ `own` is the presence bit and has to be one: a file written and then emptied
// is {NULL, 0}, which is what one still in .rodata looks like too, so the flag is
// the only thing that says which blob to read -- the tree's presence law wearing
// its C face. `heap` is the same bit for the path (create and rename spell names
// .rodata never held); a NULL path is a retired slot the next create may take.
struct k_ent {
  char const *path;                 // the canonical key
  int bake;                         // the kfiles row backing reads until the first write; -1 none
  unsigned char *bytes;
  uintptr_t len, cap, ms, mode;     // mode is the permission bits; stat lays the kind over them
  int refs;                         // open fds; an unlinked entry frees at the last close
  bool own, heap, dir, live;
};
static struct k_ent *k_ents;
static int k_ents_n, k_ents_cap;

// lay the table on first use: every baked row, live, reading off .rodata -- plus
// tmp, the scratch a POSIX machine promises and no initrd carries. idempotent, and
// a refusal leaves the console standing (the caller answers absence or ENOMEM).
static bool k_fs_init(void) {
  if (k_ents) return true;
  int n = (int) countof(kfiles), cap = n + 8;
  struct k_ent *t = kmallocw(b2w((uintptr_t) cap * sizeof *t));
  if (!t) return false;
  for (int i = 0; i < n; i++)
    t[i] = (struct k_ent) { .path = kfiles[i].path, .bake = i,
                            .ms = kfiles[i].ms, .mode = 0644, .live = true };
  t[n] = (struct k_ent) { .path = "tmp", .bake = -1, .ms = ai_clock(),
                          .mode = 0755, .own = true, .dir = true, .live = true };
  k_ents = t, k_ents_n = n + 1, k_ents_cap = cap;
  return true; }

// the cwd, a kernel string -- canonical ("" is the root), what k_canon resolves
// every relative path against. chdir writes it; cwd wears the leading slash.
static char k_cwd[256];
static uintptr_t k_cwd_n;

// resolve a path against the cwd into out (cap 256): absolute starts at the root,
// "." holds, ".." pops, doubled and trailing slashes fall away. -> the canonical
// length (0 is the root), or -1 for one longer than any entry could carry.
static intptr_t k_canon(char const *p, uintptr_t pn, char *out) {
  uintptr_t n = 0;
  if (!(pn && p[0] == '/')) memcpy(out, k_cwd, n = k_cwd_n);
  for (uintptr_t i = 0; i < pn;) {
    while (i < pn && p[i] == '/') i++;
    uintptr_t j = i;
    while (j < pn && p[j] != '/') j++;
    uintptr_t k = j - i;
    if (!k) break;
    if (k == 1 && p[i] == '.') { i = j; continue; }
    if (k == 2 && p[i] == '.' && p[i + 1] == '.') {
      while (n && out[n - 1] != '/') n--;
      if (n) n--;
      i = j;
      continue; }
    if (n + k + 2 > 256) return -1;
    if (n) out[n++] = '/';
    memcpy(out + n, p + i, k), n += k;
    i = j; }
  return (intptr_t) n; }

// one open file: which entry, where in it, and whether writes are allowed. rides
// the k_source row's `state`; the close door frees it.
struct k_fh { int i; uintptr_t pos; bool w; };

static intptr_t ram_readn(int fd, unsigned char *dst, uintptr_t n);

// ⚠ the handle behind an fd, and NOTHING for a row that is not the ramfs's: `state`
// is per-instance scratch of whatever kind its row's methods please, so the read
// door is what says it means a file handle. lseek reaches fds it did not open.
static ai_inline struct k_fh *k_fh(int fd) {
  struct k_source *s = k_source(fd);
  return s && s->readn == ram_readn ? s->state : NULL; }

// what entry i reads as: the heap copy once there is one, the baked blob until then.
static unsigned char const *k_blob(int i, uintptr_t *len) {
  struct k_ent const *e = &k_ents[i];
  if (e->own) return *len = e->len, e->bytes;
  return *len = kfiles[e->bake].len, (unsigned char const*) kfiles[e->bake].bytes; }

// free/sys.c's seek. ⚠ it answers an ERRNO where lvm_lseek answers a bare -1:
// down here a caller can tell "no such fd" from "this row does not seek", which
// the love door could not, having no errno table to name it with. whence 0/1/2
// is SEEK_SET/CUR/END -- what the love door already meant by them.
long k_fd_lseek(int fd, long off, int whence) {
  if (!k_row_live(fd)) return -9;                        // EBADF
  struct k_fh *h = k_fh(fd);
  if (!h) return -29;                                    // ESPIPE: a console or a pipe
  if (whence < 0 || whence > 2) return -22;              // EINVAL
  uintptr_t len;
  k_blob(h->i, &len);
  intptr_t at = off + (whence == 1 ? (intptr_t) h->pos
                     : whence == 2 ? (intptr_t) len : 0);
  if (at < 0) return -22;
  return (long) (h->pos = (uintptr_t) at); }

// canonical path -> its live entry. LINEAR and unapologetic: the tree is a few
// dozen entries, and a hash would cost a table the boot has to build before it can
// open the file that would have justified it.
static int k_find(char const *p, uintptr_t n) {
  for (int i = 0; i < k_ents_n; i++)
    if (k_ents[i].live && k_ents[i].path
        && strlen(k_ents[i].path) == n && !memcmp(k_ents[i].path, p, n)) return i;
  return -1; }

// a slot for a fresh entry: a retired one first, else the table doubles. -1 is a
// refusal the caller reads.
static int k_ent_slot(void) {
  for (int i = 0; i < k_ents_n; i++) if (!k_ents[i].path) return i;
  if (k_ents_n == k_ents_cap) {
    int cap = k_ents_cap * 2;
    struct k_ent *t = kmallocw(b2w((uintptr_t) cap * sizeof *t));
    if (!t) return -1;
    memcpy(t, k_ents, (uintptr_t) k_ents_n * sizeof *t);
    kfree(k_ents);
    k_ents = t, k_ents_cap = cap; }
  return k_ents_n++; }

static char *k_strdup(char const *p, uintptr_t n) {
  char *q = kmallocw(b2w(n + 1));
  if (q) memcpy(q, p, n), q[n] = 0;
  return q; }

// free a dead, unheld entry's storage and retire the slot. unlink and the last
// close both land here, so an open fd keeps its file until it lets go -- POSIX's
// rule, and the one that keeps a live handle off freed bytes.
static void k_ent_gc(int i) {
  struct k_ent *e = &k_ents[i];
  if (e->live || e->refs || !e->path) return;
  if (e->own) kfree(e->bytes);
  if (e->heap) kfree((void*) e->path);
  *e = (struct k_ent) {0}; }

// a fresh live entry at canonical path p -- rung 2's create. the caller has
// already asked k_parent_ok; -1 is memory refusing.
static int k_create(char const *p, uintptr_t n, bool dir, uintptr_t mode) {
  char *q = k_strdup(p, n);
  if (!q) return -1;
  int i = k_ent_slot();
  if (i < 0) return kfree(q), -1;
  k_ents[i] = (struct k_ent) { .path = q, .bake = -1, .ms = ai_clock(),
                               .mode = mode, .own = true, .heap = true,
                               .dir = dir, .live = true };
  return i; }

// ⚠ A DIRECTORY CAN BE A PREFIX: the initrd is flat ("lib/json.l" and no row for
// "lib"), so a name baked paths lie under is a directory with no entry of its own
// -- synthesized, 0755, wearing its newest child's date. mkdir is what gives one
// an entry (and an emptiness) of its own.

// entry i's name under a prefix of pn bytes -- NULL when it does not lie under it.
// An entry deeper than one level answers its next COMPONENT, so a subdirectory is
// named by the paths inside it as much as by any entry of its own.
static char const *k_entry(int i, char const *p, uintptr_t pn, uintptr_t *len) {
  if (!k_ents[i].live || !k_ents[i].path) return NULL;
  char const *q = k_ents[i].path;
  uintptr_t ql = strlen(q);
  if (pn) {
    if (ql <= pn + 1 || memcmp(q, p, pn) || q[pn] != '/') return NULL;
    q += pn + 1, ql -= pn + 1; }
  uintptr_t k = 0;
  while (k < ql && q[k] != '/') k++;
  return *len = k, q; }

// anything live under the prefix? -> and the newest date beneath it, the only
// date a synthesized directory can honestly wear.
static bool k_kids(char const *p, uintptr_t pn, uintptr_t *ms) {
  bool any = false;
  *ms = 0;
  for (int i = 0; i < k_ents_n; i++) {
    uintptr_t k;
    if (!k_entry(i, p, pn, &k)) continue;
    any = true;
    if (k_ents[i].ms > *ms) *ms = k_ents[i].ms; }
  return any; }

// is the canonical path a directory: the root always, an entry that says so, or a
// prefix something lives under.
static bool k_dirp(char const *p, uintptr_t pn) {
  if (!pn) return true;
  int i = k_find(p, pn);
  if (i >= 0) return k_ents[i].dir;
  uintptr_t junk;
  return k_kids(p, pn, &junk); }

// the parent a path wants to land in: 0 when it is a directory, else the errno
// the host would say (a hole ENOENT, a file in the way ENOTDIR).
static int k_parent_ok(char const *p, uintptr_t n) {
  uintptr_t dn = n;
  while (dn && p[dn - 1] != '/') dn--;
  if (dn) dn--;
  if (!dn) return 0;
  int i = k_find(p, dn);
  if (i >= 0) return k_ents[i].dir ? 0 : -ENOTDIR;
  uintptr_t junk;
  return k_kids(p, dn, &junk) ? 0 : -ENOENT; }

// make room for `need` bytes in entry i's heap copy, bringing the baked blob
// across on the first write. -> false is a REFUSAL the caller must read and say;
// nothing is ever dropped quietly.
static bool k_fit(int i, uintptr_t need) {
  struct k_ent *e = &k_ents[i];
  if (!e->own) {
    uintptr_t n = kfiles[e->bake].len, cap = n > need ? n : need;
    unsigned char *p = cap ? kmallocw(b2w(cap)) : NULL;
    if (cap && !p) return false;
    if (n) memcpy(p, kfiles[e->bake].bytes, n);
    e->bytes = p, e->len = n, e->cap = cap, e->own = true;
    return true; }
  if (e->cap >= need) return true;
  uintptr_t cap = e->cap ? e->cap : 64;
  while (cap < need) cap *= 2;
  unsigned char *p = kmallocw(b2w(cap));
  if (!p) return false;
  if (e->len) memcpy(p, e->bytes, e->len);
  kfree(e->bytes);
  e->bytes = p, e->cap = cap;
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
  struct k_ent *e = &k_ents[h->i];
  // a gap (a truncate under an append fd) reads as zeros, never as the bytes the
  // last tenant of that block left there.
  if (h->pos > e->len)
    memset(e->bytes + e->len, 0, h->pos - e->len);
  memcpy(e->bytes + h->pos, src, n);
  h->pos += n;
  if (h->pos > e->len) e->len = h->pos;
  e->ms = ai_clock();
  return (intptr_t) n; }

static bool ram_ready(int fd) { (void) fd; return true; }

static void ram_close(int fd) {
  struct k_source *s = k_source(fd);
  if (!s) return;
  struct k_fh *h = s->state;
  if (h && k_ents[h->i].refs) k_ents[h->i].refs--, k_ent_gc(h->i);
  kfree(s->state);
  *s = (struct k_source) {0}; }               // and the row is free again

// the lowest free row at or past the boot two -- POSIX's rule, which scripts lean
// on. A row is free when it carries no method at all, which is what k_source_open
// zeroes a fresh one to and what ram_close puts one back to.
static int k_fd_free(void) {
  for (int i = (int) countof(k_boot); i < k_sources_n; i++)
    if (!k_row_live(i)) return i;
  return k_sources_n; }

// open a path -> its fd, or -1. m is r read, w truncate, a append -- the one door
// under both `open` (which reads it off a mode string) and `openfd` (off the charm
// host/posix.c spells 0/1/2). w and a CREATE an absent path whose parent is a
// directory (rung 2); for r absence stays absence. a directory does not open --
// readdir is its read door.
// the PATH FACE: an fd, or a NEGATIVE errno. ⚠ the love doors above answer a
// bare -1 / () for every failure alike and always did -- down here the reasons
// are distinct and free/sys.c's openat needs them, so they are told apart HERE
// and flattened in the marshaling, never the other way round.
static ai_noinline int k_fs_open(char const *p, uintptr_t pn, char m) {
  if (m != 'r' && m != 'w' && m != 'a') return -EINVAL;
  if (!k_fs_init()) return -ENOMEM;
  char cp[256];
  intptr_t cn = k_canon(p, pn, cp);
  if (cn < 0) return -ENAMETOOLONG;
  if (!cn) return -EISDIR;                       // the root is a directory
  int i = k_find(cp, (uintptr_t) cn);
  if (i >= 0 && k_ents[i].dir) return -EISDIR;
  bool made = false;
  if (i < 0) {
    if (m == 'r') return -ENOENT;
    int e = k_parent_ok(cp, (uintptr_t) cn);
    if (e) return e;
    if ((i = k_create(cp, (uintptr_t) cn, false, 0644)) < 0) return -ENOMEM;
    made = true; }
  int fd = k_fd_free();
  struct k_fh *h = kmallocw(b2w(sizeof *h));
  struct k_source *s = h ? k_source_open(fd) : NULL;   // the grow door; NULL is no memory
  if (!s) {
    kfree(h);
    // an open that refuses must leave the tree exactly as it found it -- a file
    // this call minted leaves with it.
    if (made) k_ents[i].live = false, k_ent_gc(i);
    return -ENOMEM; }
  // ⚠ the truncate lands LAST, past every way this can still fail (same law).
  uintptr_t len = 0;
  struct k_ent *e = &k_ents[i];
  if (m == 'w') e->own = true, e->len = 0, e->ms = ai_clock();
  if (m == 'a') k_blob(i, &len);
  e->refs++;
  *h = (struct k_fh) { .i = i, .pos = len, .w = m != 'r' };
  *s = (struct k_source) { .readn = ram_readn, .writen = ram_writen,
                           .ready = ram_ready, .close = ram_close, .state = h };
  return fd; }
// the love face: every refusal flattens to -1, which is what both doors above
// have always answered and what kore reads.
static int k_ramopen(struct ai_str *pv, char m) {
  int fd = k_fs_open(pv->bytes, pv->len, m);
  return fd < 0 ? -1 : fd; }

// (open path mode) -- host/main.c's lvm_open for the ramfs door: a heap port
// (closed on GC) or the zero point on any failure. The kernel links no host/*.c,
// so the shape is written fresh rather than shared -- doc/posix.md's conventions
// exactly, since kore reads these and a wrong one is silent.
static lvm(lvm_open) {
  if (!ai_strp(Sp[0]) || !ai_strp(Sp[1])) goto fail;
  struct ai_str *mv = (struct ai_str*) Sp[1];
  int fd = mv->len ? k_ramopen((struct ai_str*) Sp[0], mv->bytes[0]) : -1;
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
      if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
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
// one is silent.
#define k_mode_file 0100000            // (& mode 61440) = 32768: a regular file
#define k_mode_dir  0040000            //                = 16384: a directory

// (stat path) -> (size mtime-ms mode ns) | (). ⚠ ns is the ms date times a million,
// not a finer reading of it: this clock's last hand IS the millisecond (a 100 Hz
// tick over the boot date), and digits it does not have would be the wrong honesty.
// what the ramfs KNOWS about a path, and nothing it would have to invent. A
// struct stat's ino, nlink, uid and dev have no answer down here, so filling
// them is free/sys.c's fabrication to make in the open -- not this face's to
// bury, where nobody would ever see what inle had decided an inode is.
struct k_st { uintptr_t size, ms, mode; };

// -> 0, or -ENOENT for a path that is not there. ⚠ a SYNTHESIZED directory --
// a prefix that has children but no entry of its own, and the root -- answers
// like any other, because the initrd carries no directories and so most of the
// tree is synthesized. That case is also why this fills a struct rather than
// handing back an entry index: it has no row to point at.
ai_noinline static int k_fs_stat(char const *p, uintptr_t pn, struct k_st *st) {
  char cp[256];
  intptr_t cn;
  if (!k_fs_init()) return -ENOMEM;
  if ((cn = k_canon(p, pn, cp)) < 0) return -ENOENT;
  int i = k_find(cp, (uintptr_t) cn);
  uintptr_t kid;
  *st = (struct k_st) { 0, 0, 0 };
  if (i >= 0 && !k_ents[i].dir)
    k_blob(i, &st->size), st->ms = k_ents[i].ms,
    st->mode = k_mode_file | k_ents[i].mode;
  else if (i >= 0) {
    st->mode = k_mode_dir | k_ents[i].mode;     // an explicit directory: its own date,
    st->ms = k_ents[i].ms;                      // or its newest child's if newer
    if (k_kids(cp, (uintptr_t) cn, &kid) && kid > st->ms) st->ms = kid; }
  else if (k_kids(cp, (uintptr_t) cn, &st->ms) || !cn) st->mode = k_mode_dir | 0755;
  else return -ENOENT;
  return 0; }

// the love face: (size mtime mode ns), kore's shape. ⚠ every refusal is the
// same () here -- absence and misuse alike -- which is the host's answer too.
ai_noinline static struct ai *k_stat(struct ai *g) {
  struct k_st st;
  if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
  struct ai_str *pv = (struct ai_str*) g->sp[0];
  if (k_fs_stat(pv->bytes, pv->len, &st)) return g->sp[0] = ZeroPoint, g;
  if (!ai_ok(g = ai_have(g, 4 * Width(struct ai_chain)))) return g;
  struct ai_chain *c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm((intptr_t) (st.ms * 1000000)), ZeroPoint);
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) st.mode), word(c));
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) st.ms), word(c));
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) st.size), word(c));
  return g->sp[0] = word(c), g; }
static lvm(lvm_stat) {
  Pack(g); g = k_stat(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }

// (readdir path) -> the entry names, one string each, or () -- for a path that is
// no directory as much as for one that is missing, which is the host's answer too
// (an EMPTY directory answers the same (), told from absence by stat). NO order
// promised (entry order); "." and ".." are not entries here, since this tree has
// no link to hold them.
ai_noinline static struct ai *k_readdir(struct ai *g) {
  // ⚠ the prefix is COPIED out (the canon buffer): every ai_have below may
  // collect, and the string it came from is a heap object that moves. Entry paths
  // live in .rodata or the kernel heap and never do, which is why the names
  // themselves are read in place.
  char cp[256], nb[128];
  if (!ai_strp(g->sp[0])) return g->sp[0] = ZeroPoint, g;
  struct ai_str *pv = (struct ai_str*) g->sp[0];
  intptr_t cn;
  if (!k_fs_init() || (cn = k_canon(pv->bytes, pv->len, cp)) < 0)
    return g->sp[0] = ZeroPoint, g;
  if (!k_dirp(cp, (uintptr_t) cn)) return g->sp[0] = ZeroPoint, g;
  g->sp[0] = ZeroPoint;                                 // the accumulator, over the path
  for (int i = 0; i < k_ents_n; i++) {
    uintptr_t k;
    char const *e = k_entry(i, cp, (uintptr_t) cn, &k);
    if (!e) continue;
    bool seen = false;                                  // one name per entry, not per path
    for (int j = 0; j < i && !seen; j++) {
      uintptr_t k2;
      char const *e2 = k_entry(j, cp, (uintptr_t) cn, &k2);
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
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
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
                   : k_ramopen((struct ai_str*) Sp[0],
                               m == 1 ? 'w' : m == 2 ? 'a' : 'r'));
  Sp += 1; ai_musttail return Next(1); }

// (fdclose fd) -> (). openfd's other half. A row nobody opened is already closed,
// which is why this cannot fail and answers the zero point either way.
static lvm(lvm_fdclose) {
  if (Sp[0] & 1) ai_fd_close((int) getcharm(Sp[0]));
  Sp[0] = ZeroPoint;
  ai_musttail return Next(1); }

// --- rung 4: pipes, and the fd plumbing over them ---------------------------
// a pipe is a k_source PAIR over one byte queue in the kernel heap: the read end
// answers 0 while a writer is open and -1 when the last one closes -- exactly
// what the scheduler parks on. each end counts its holders (dup and the seat
// below make aliases), and the queue frees when both counts reach zero.
// ⚠ the queue GROWS rather than refusing at a cap: the writer's lane is the
// static port's unbuffered zputc, whose contract on a busy answer is one retry
// and then a DROPPED byte -- a bounded ring here would shed bytes in silence
// under exactly the load it exists for. the price rides the same open question
// as the ramfs's memory ceiling (doc/inle.md).
struct k_pipe { unsigned char *buf; uintptr_t cap, rp, wp; int rrefs, wrefs; };

static struct k_pipe *k_pipe_of(int fd) {
  struct k_source *s = k_source(fd);
  return s ? s->state : NULL; }

static intptr_t pipe_readn(int fd, unsigned char *dst, uintptr_t n) {
  struct k_pipe *p = k_pipe_of(fd);
  if (!p) return -1;
  uintptr_t a = p->wp - p->rp;
  if (!a) return p->wrefs ? 0 : -1;             // quiet with a writer: park; else the end
  if (a > n) a = n;
  memcpy(dst, p->buf + p->rp, a);
  p->rp += a;
  if (p->rp == p->wp) p->rp = p->wp = 0;        // drained: the queue restarts at the front
  return (intptr_t) a; }

static intptr_t pipe_writen(int fd, unsigned char const *src, uintptr_t n) {
  struct k_pipe *p = k_pipe_of(fd);
  // ⚠ no readers is GONE, not busy -- but with no SIGPIPE on this machine the
  // writer only learns if it looks: the run is dropped, as the host drops one
  // on EPIPE (io_wdrain's k < 0 lane). a `yes` into a dead pipe spins.
  if (!p || !p->rrefs) return -1;
  if (p->wp + n > p->cap) {
    if (p->rp) {                                // compact before growing
      memmove(p->buf, p->buf + p->rp, p->wp - p->rp);
      p->wp -= p->rp, p->rp = 0; }
    if (p->wp + n > p->cap) {
      uintptr_t cap = p->cap ? p->cap : 4096;
      while (cap < p->wp + n) cap *= 2;
      unsigned char *b = kmallocw(b2w(cap));
      if (!b) return -1;                        // memory refusing, said out loud
      if (p->wp) memcpy(b, p->buf, p->wp);
      kfree(p->buf);
      p->buf = b, p->cap = cap; } }
  memcpy(p->buf + p->wp, src, n);
  p->wp += n;
  return (intptr_t) n; }

static bool pipe_rready(int fd) {
  struct k_pipe *p = k_pipe_of(fd);
  return p && (p->wp > p->rp || !p->wrefs); }   // bytes, or the end -- both wake a reader

static void pipe_free(struct k_pipe *p) {
  if (p->rrefs || p->wrefs) return;
  kfree(p->buf);
  kfree(p); }
static void pipe_rclose(int fd) {
  struct k_pipe *p = k_pipe_of(fd);
  struct k_source *s = k_source(fd);
  if (s) *s = (struct k_source) {0};
  if (p) p->rrefs--, pipe_free(p); }
static void pipe_wclose(int fd) {
  struct k_pipe *p = k_pipe_of(fd);
  struct k_source *s = k_source(fd);
  if (s) *s = (struct k_source) {0};
  if (p) p->wrefs--, pipe_free(p); }

// a duped row that carries no state of its own (a console twin) still owes
// k_fd_free a way back to empty.
static void k_row_zero(int fd) {
  struct k_source *s = k_source(fd);
  if (s) *s = (struct k_source) {0}; }

// clone src's row into a fresh fd -- POSIX dup as a ROW ALIAS. a pipe end shares
// the queue and bumps its side's count; a ramfs fd clones the handle (⚠ the
// offset then DIVERGES where POSIX shares it -- the shell's save/restore dance
// never seeks, and a shared-offset handle costs a refcounted box nothing asks
// for yet); a boot twin gets k_row_zero so its close frees the row.
ai_noinline static int k_dup_row(struct ai *g, int src) {
  struct k_source *s = k_source(src);
  if (!s || !(s->readn || s->writen || s->putc)) return -1;
  int fd = k_fd_free();
  struct k_fh *h = NULL;
  if (s->readn == ram_readn) {
    struct k_fh *o = s->state;
    if (!o || !(h = kmallocw(b2w(sizeof *h)))) return -1;
    *h = *o; }
  struct k_source *t = k_source_open(fd);
  if (!t) { kfree(h); return -1; }
  s = k_source(src);                            // the grow may have moved the table
  *t = *s;
  if (h) t->state = h, k_ents[h->i].refs++;
  else if (s->readn == pipe_readn) ((struct k_pipe*) s->state)->rrefs++;
  else if (s->writen == pipe_writen) ((struct k_pipe*) s->state)->wrefs++;
  else if (!t->close) t->close = k_row_zero;
  return fd; }

// (pipe _) -> (rfd . wfd) | a NEGATIVE errno -- host/posix.c's shape exactly.
// the body rides an ai_noinline helper (k_stat's model) so the wrapper stays a
// pure tail jump.
ai_noinline static struct ai *k_pipe_new(struct ai *g) {
  if (!ai_ok(g = ai_have(g, Width(struct ai_chain)))) return g;
  struct k_pipe *p = kmallocw(b2w(sizeof *p));
  if (!p) return g->sp[0] = putcharm(-ENOMEM), g;
  *p = (struct k_pipe) { .rrefs = 1, .wrefs = 1 };
  int rfd = k_fd_free();
  struct k_source *rs = k_source_open(rfd);
  if (rs) *rs = (struct k_source) { .readn = pipe_readn, .ready = pipe_rready,
                                    .close = pipe_rclose, .state = p };
  int wfd = rs ? k_fd_free() : -1;
  struct k_source *ws = rs ? k_source_open(wfd) : NULL;
  if (!ws) {
    if (rs) k_row_zero(rfd);
    kfree(p);
    return g->sp[0] = putcharm(-ENOMEM), g; }
  *ws = (struct k_source) { .writen = pipe_writen, .close = pipe_wclose, .state = p };
  struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm(rfd), putcharm(wfd));
  return g->sp[0] = word(w), g; }
static lvm(lvm_pipe) {
  Pack(g); g = k_pipe_new(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }

// (fdopen fd) -> a PORT over a raw fd | () -- pipe/openfd's other half, so love
// reads and writes its own plumbing. the port's GC finalizer owns the fd from
// here: hand it over, don't fdclose it too (host/posix.c's law).
ai_noinline static struct ai *k_fdopen(struct ai *g) {
  ai_word a = g->sp[0];
  int fd = (a & 1) ? (int) getcharm(a) : -1;
  struct k_source *s = k_source(fd);
  if (!s || !(s->readn || s->writen || s->putc)) return g->sp[0] = ZeroPoint, g;
  struct ai *r = ai_io_alloc(g, fd);
  if (!ai_ok(r)) return g->sp[0] = ZeroPoint, g;   // OOM -> the zero point (host's face)
  g = r;
  return g->sp[1] = g->sp[0], g->sp += 1, g; }     // port over the fd arg
static lvm(lvm_fdopen) {
  Pack(g); g = k_fdopen(g);
  Unpack(g);                                       // every failure folds to (), no ghelp
  ai_musttail return Next(1); }

// (dup fd) -> a fresh row aliasing fd | -errno.  (dup2 src dst) -> () | errno |
// EINVAL: dst's old row closes first, then src's alias lands IN that slot.
ai_noinline static ai_word k_dup(struct ai *g, ai_word w) {
  int fd = (w & 1) ? (int) getcharm(w) : -1;
  int r = fd < 0 ? -1 : k_dup_row(g, fd);
  return putcharm(r < 0 ? -EBADF : r); }
static lvm(lvm_dup) {
  Sp[0] = k_dup(g, Sp[0]);
  ai_musttail return Next(1); }

ai_noinline static ai_word k_dup2(struct ai *g, ai_word sw, ai_word dw) {
  if (!(sw & 1) || !(dw & 1)) return putcharm(EINVAL);
  int src = (int) getcharm(sw), dst = (int) getcharm(dw);
  if (src == dst) return ZeroPoint;
  int nfd = k_dup_row(g, src);                  // the alias first: src == a dying dst's twin survives
  if (nfd < 0) return putcharm(EBADF);
  struct k_source *d = k_source_open(dst);
  if (!d) { ai_fd_close(nfd); k_row_zero(nfd); return putcharm(ENOMEM); }
  if (d->close) d->close(dst);
  d = k_source(dst);                            // close zeroes through the live table
  struct k_source *n = k_source(nfd);
  *d = *n;
  *n = (struct k_source) {0};                   // the temp row retires; its state moved whole
  return ZeroPoint; }
static lvm(lvm_dup2) {
  Sp[1] = k_dup2(g, Sp[0], Sp[1]);
  Sp += 1; ai_musttail return Next(1); }

// (getpid _) -> the running task's pid, a charm; the main task reads 0.
static lvm(lvm_getpid) {
  Sp[0] = putcharm(k_cur_pid(g));
  ai_musttail return Next(1); }

// (procseat pid f0 f1 f2) -> () | ENOMEM. the spawn shim's registration, called
// in the PARENT right after twirl -- which does not switch tasks, so the seat is
// in place before the child's first read. each fi: an fd >= 0 is DUPED into the
// seat (fork's fd-copy made explicit, so the parent may fdclose its own end);
// -1 inherits the parent's effective fd (duped when the parent is itself seated);
// -2 seats closed (an fdmap's () entry). quit is the door that takes it down.
ai_noinline static ai_word k_procseat(struct ai *g, ai_word pw,
                                      ai_word w0, ai_word w1, ai_word w2) {
  ai_word ws[3] = { w0, w1, w2 };
  intptr_t pid = (pw & 1) ? getcharm(pw) : 0;
  if (!pid) return putcharm(EINVAL);
  struct k_seat *s = k_seat_slot();
  if (!s) return putcharm(ENOMEM);
  *s = (struct k_seat) { pid, {-1, -1, -1} };
  for (int i = 0; i < 3; i++) {
    intptr_t f = (ws[i] & 1) ? getcharm(ws[i]) : -1;
    if (f == -2) { s->fd[i] = -2; continue; }
    if (f == -1) f = i;                         // absent: inherit this slot
    if (f >= 0 && f <= 2) {                     // a console-numbered fd means the
      f = k_fd_eff(g, (int) f);                 // PARENT's view of it (2>&1 under
      if (f < 0) { s->fd[i] = -2; continue; }   // a seat follows the seat)
      if (f == i) continue; }                   // the identity seat is no seat
    int d = k_dup_row(g, (int) f);
    s = k_seat_find(pid);                       // the dup may have grown tables
    if (d < 0) { s->fd[i] = -2; continue; }     // a dead fd seats closed, not silent
    s->fd[i] = d; }
  return ZeroPoint; }
static lvm(lvm_procseat) {
  Sp[3] = k_procseat(g, Sp[0], Sp[1], Sp[2], Sp[3]);
  Sp += 3; ai_musttail return Next(1); }

// --- rung 5: the disk -- the block door love's filesystem (lib/fat.l) rides.
// the driver is free/blk.c (virtio-blk, polled, synchronous); DMA rides
// the love string's own bytes -- heap memory, and nothing allocates between
// post and completion, so the collector cannot move the buffer under the device.
// (disk _)         -> the sector count, 0 when no disk: presence by the green.
// (disk-read l n)  -> a string of n*512 bytes off sector l | ().
// (disk-write l s) -> the sectors written | () (s must be whole sectors).
void k_blk_init(void *dma);
uint64_t k_blk_sectors(void);
int k_blk_rw(uint64_t lba, uint32_t n, void *buf, int wr);

static lvm(lvm_disk) {
  Sp[0] = putcharm((intptr_t) k_blk_sectors());
  ai_musttail return Next(1); }

ai_noinline static struct ai *k_disk_read(struct ai *g) {
  ai_word lw = g->sp[0], nw = g->sp[1];
  intptr_t lba = (lw & 1) ? getcharm(lw) : -1,
           n   = (nw & 1) ? getcharm(nw) : -1;
  if (lba < 0 || n <= 0 || n > 1 << 24) return g->sp[1] = ZeroPoint, g->sp += 1, g;
  if (!ai_ok(g = str0(g, (uintptr_t) n * 512))) return g;   // OOM: the wrapper ghelps
  if (k_blk_rw((uint64_t) lba, (uint32_t) n, txt(g->sp[0]), 0) < 0)
    g->sp[0] = ZeroPoint;
  return g->sp[2] = g->sp[0], g->sp += 2, g; }
static lvm(lvm_disk_read) {
  Pack(g); g = k_disk_read(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }

ai_noinline static ai_word k_disk_write(ai_word lw, ai_word sw) {
  intptr_t lba = (lw & 1) ? getcharm(lw) : -1;
  if (lba < 0 || !ai_strp(sw)) return ZeroPoint;
  struct ai_str *s = (struct ai_str*) sw;
  if (!s->len || s->len % 512) return ZeroPoint;
  if (k_blk_rw((uint64_t) lba, (uint32_t) (s->len / 512), s->bytes, 1) < 0)
    return ZeroPoint;
  return putcharm((intptr_t) (s->len / 512)); }
static lvm(lvm_disk_write) {
  Sp[1] = k_disk_write(Sp[0], Sp[1]);
  Sp += 1; ai_musttail return Next(1); }

// --- the SVM spike (x86_64 only; free/x86_64/svm.c). (svm ())
// is the capability and (svm-run ()) runs one guest, answering (exitcode rax
// rip) or (). Nothing else in the kernel asks for a guest yet: the whole job of
// these two rows is to prove that a guest can run and that the exit lands back
// in ordinary C.
#if defined(__x86_64__)
uintptr_t k_svm_need(void);
bool k_svm_ok(void);
int k_svm_spike(void *mem, uint64_t *code, uint64_t *rax, uint64_t *rip);

static lvm(lvm_svm) {
  Sp[0] = k_svm_ok() ? putcharm(1) : ZeroPoint;
  ai_musttail return Next(1); }

ai_noinline static struct ai *k_svm_run(struct ai *g) {
  uint64_t code = 0, rax = 0, rip = 0;
  if (!k_svm_ok()) return g->sp[0] = ZeroPoint, g;
  // the spike's pages ride a love string's own bytes -- blk.c's trick for DMA,
  // and safe for the same reason: nothing allocates between the carve and the
  // vmrun, so the collector cannot move the VMCB out from under the CPU. It
  // also keeps the machine from spending a page on a guest nobody asked for.
  if (!ai_ok(g = str0(g, k_svm_need()))) return g;      // OOM: the wrapper ghelps
  if (k_svm_spike(txt(g->sp[0]), &code, &rax, &rip) < 0)
    return g->sp[1] = ZeroPoint, g->sp += 1, g;
  if (!ai_ok(g = ai_have(g, 3 * Width(struct ai_chain)))) return g;
  struct ai_chain *c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm((intptr_t) rip), ZeroPoint);
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) rax), word(c));
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) code), word(c));
  return g->sp[1] = word(c), g->sp += 1, g; }
static lvm(lvm_svm_run) {
  Pack(g); g = k_svm_run(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }

// ..and its Intel twin (free/x86_64/vmx.c). (vmx-run ())
// answers FOUR numbers where the SVM door answers three: the last is the
// VM-instruction error, which is the only thing a refused entry has to say and
// is worth carrying out to where a human reads it.
uintptr_t k_vmx_need(void);
bool k_vmx_ok(void);
int k_vmx_spike(void *mem, uint64_t *reason, uint64_t *rax, uint64_t *rip,
                uint64_t *err);

static lvm(lvm_vmx) {
  Sp[0] = k_vmx_ok() ? putcharm(1) : ZeroPoint;
  ai_musttail return Next(1); }

ai_noinline static struct ai *k_vmx_run(struct ai *g) {
  uint64_t reason = 0, rax = 0, rip = 0, err = 0;
  if (!k_vmx_ok()) return g->sp[0] = ZeroPoint, g;
  if (!ai_ok(g = str0(g, k_vmx_need()))) return g;       // OOM: the wrapper ghelps
  if (k_vmx_spike(txt(g->sp[0]), &reason, &rax, &rip, &err) < 0)
    return g->sp[1] = ZeroPoint, g->sp += 1, g;
  if (!ai_ok(g = ai_have(g, 4 * Width(struct ai_chain)))) return g;
  struct ai_chain *c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                 putcharm((intptr_t) err), ZeroPoint);
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) rip), word(c));
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) rax), word(c));
  c = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                putcharm((intptr_t) reason), word(c));
  return g->sp[1] = word(c), g->sp += 1, g; }
static lvm(lvm_vmx_run) {
  Pack(g); g = k_vmx_run(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }
#endif

// --- rung 2: the writable tree -- mkdir, rmdir, unlink, rename, chdir/cwd,
// chmod, utime. doc/posix.md's conventions exactly: an effect answers () | a
// POSITIVE errno (the host's numbers -- kore reads them back, and mv's EXDEV
// lane proves a shape can matter) | EINVAL on misuse; chdir wears the host's
// negative lane; cwd answers the string | (). The environment is not here: a
// tablet in the boot text (kmain, below), as doc/inle.md says.

// --- the PATH FACES ------------------------------------------------------
// k_fs_* take (bytes, len) and answer 0 or a NEGATIVE errno, as k_fd_* and
// k_parent_ok do -- ONE sign for every C face in this kernel, and it is the
// one __ai_sys owes its caller (impl.h's er() reads an error as
// (unsigned long) r > (unsigned long) -4096), so free/sys.c hands these answers
// straight out with no flip anywhere. The love conventions are the k_* wrappers'
// business: positive for most doors, negative for chdir, () for absence.
// ⚠ do not "restore" the positive lane -- it was a fossil of the love bodies
// these were lifted out of, and a flip per path is a sign to get wrong per path.
// ⚠ ai_noinline is load-bearing here, not decoration: cp[256] living in an
// lvm's own frame would block its musttail.
ai_noinline static int k_fs_mkdir(char const *p, uintptr_t pn, uintptr_t mode) {
  char cp[256];
  intptr_t cn;
  if (!k_fs_init()) return -ENOMEM;
  if ((cn = k_canon(p, pn, cp)) < 0) return -ENAMETOOLONG;
  uintptr_t junk;
  if (!cn || k_find(cp, (uintptr_t) cn) >= 0 || k_kids(cp, (uintptr_t) cn, &junk))
    return -EEXIST;
  int e = k_parent_ok(cp, (uintptr_t) cn);
  if (e) return e;
  return k_create(cp, (uintptr_t) cn, true, mode) < 0 ? -ENOMEM : 0; }
static ai_word k_mkdir(ai_word pw, ai_word mw) {
  if (!ai_strp(pw)) return putcharm(EINVAL);
  struct ai_str *pv = (struct ai_str*) pw;
  int e = k_fs_mkdir(pv->bytes, pv->len,
                     (mw & 1) ? (uintptr_t) getcharm(mw) & 07777 : 0755);
  return e ? putcharm(-e) : ZeroPoint; }
static lvm(lvm_mkdir) {
  Sp[1] = k_mkdir(Sp[0], Sp[1]);
  Sp += 1; ai_musttail return Next(1); }

ai_noinline static int k_fs_rmdir(char const *p, uintptr_t pn) {
  char cp[256];
  intptr_t cn;
  if (!k_fs_init()) return -ENOMEM;
  if ((cn = k_canon(p, pn, cp)) < 0) return -ENOENT;
  if (!cn) return -EBUSY;                         // the root stays
  int i = k_find(cp, (uintptr_t) cn);
  if (i >= 0 && !k_ents[i].dir) return -ENOTDIR;
  uintptr_t junk;
  if (k_kids(cp, (uintptr_t) cn, &junk)) return -ENOTEMPTY;
  if (i < 0) return -ENOENT;
  k_ents[i].live = false;
  k_ent_gc(i);
  return 0; }
static ai_word k_rmdir(ai_word pw) {
  if (!ai_strp(pw)) return putcharm(EINVAL);
  struct ai_str *pv = (struct ai_str*) pw;
  int e = k_fs_rmdir(pv->bytes, pv->len);
  return e ? putcharm(-e) : ZeroPoint; }
static lvm(lvm_rmdir) { Sp[0] = k_rmdir(Sp[0]); ai_musttail return Next(1); }

ai_noinline static int k_fs_unlink(char const *p, uintptr_t pn) {
  char cp[256];
  intptr_t cn;
  if (!k_fs_init()) return -ENOMEM;
  if ((cn = k_canon(p, pn, cp)) < 0) return -ENOENT;
  int i = cn ? k_find(cp, (uintptr_t) cn) : -1;
  if (i < 0) return k_dirp(cp, (uintptr_t) cn) ? -EISDIR : -ENOENT;
  if (k_ents[i].dir) return -EISDIR;
  k_ents[i].live = false;                        // an open fd keeps the bytes; the
  k_ent_gc(i);                                   // last close frees them
  return 0; }
static ai_word k_unlink(ai_word pw) {
  if (!ai_strp(pw)) return putcharm(EINVAL);
  struct ai_str *pv = (struct ai_str*) pw;
  int e = k_fs_unlink(pv->bytes, pv->len);
  return e ? putcharm(-e) : ZeroPoint; }
static lvm(lvm_unlink) { Sp[0] = k_unlink(Sp[0]); ai_musttail return Next(1); }

// (rename old new): a file moves whole, a target file unlinked under it; a
// directory carries everything beneath it -- every live path at or under the
// prefix respelled, the copies staged FIRST so a refusal leaves the tree whole.
struct k_ren { struct k_ren *next; int i; char *q; };
ai_noinline static int k_fs_rename(char const *o, uintptr_t olen,
                                   char const *n, uintptr_t nlen) {
  char op[256], np[256];
  intptr_t on, nn;
  if (!k_fs_init()) return -ENOMEM;
  if ((on = k_canon(o, olen, op)) < 0
   || (nn = k_canon(n, nlen, np)) < 0) return -ENAMETOOLONG;
  if (!on) return -EBUSY;                         // the root does not move
  if (on == nn && !memcmp(op, np, (uintptr_t) on)) return 0;           // itself: done
  if (!nn) return -EEXIST;                        // onto the root
  int e = k_parent_ok(np, (uintptr_t) nn);
  if (e) return e;
  int si = k_find(op, (uintptr_t) on), di = k_find(np, (uintptr_t) nn);
  uintptr_t junk;
  bool sdir = si >= 0 ? k_ents[si].dir : k_kids(op, (uintptr_t) on, &junk);
  if (si < 0 && !sdir) return -ENOENT;
  if (!sdir) {
    if (di >= 0 ? k_ents[di].dir : k_kids(np, (uintptr_t) nn, &junk))
      return -EISDIR;                             // a file does not land on a directory
    char *q = k_strdup(np, (uintptr_t) nn);
    if (!q) return -ENOMEM;
    if (di >= 0) k_ents[di].live = false, k_ent_gc(di);
    if (k_ents[si].heap) kfree((void*) k_ents[si].path);
    k_ents[si].path = q, k_ents[si].heap = true;
    return 0; }
  // the directory lane
  if ((uintptr_t) nn > (uintptr_t) on && !memcmp(np, op, (uintptr_t) on) && np[on] == '/')
    return -EINVAL;                               // never into itself
  if (di >= 0 || k_kids(np, (uintptr_t) nn, &junk)) return -EEXIST;
  struct k_ren *st = NULL;
  for (int i = 0; i < k_ents_n; i++) {
    struct k_ent const *t = &k_ents[i];
    if (!t->live || !t->path) continue;
    uintptr_t tl = strlen(t->path);
    if (tl < (uintptr_t) on || memcmp(t->path, op, (uintptr_t) on)
        || (tl > (uintptr_t) on && t->path[on] != '/')) continue;
    char *q = kmallocw(b2w((uintptr_t) nn + tl - (uintptr_t) on + 1));
    struct k_ren *r = q ? kmallocw(b2w(sizeof *r)) : NULL;
    if (!r) {                                    // roll the staging back whole
      kfree(q);
      while (st) { struct k_ren *x = st; st = st->next; kfree(x->q), kfree(x); }
      return -ENOMEM; }
    memcpy(q, np, (uintptr_t) nn);
    memcpy(q + nn, t->path + on, tl - (uintptr_t) on);
    q[(uintptr_t) nn + tl - (uintptr_t) on] = 0;
    *r = (struct k_ren) { st, i, q };
    st = r; }
  while (st) {
    struct k_ent *t = &k_ents[st->i];
    if (t->heap) kfree((void*) t->path);
    t->path = st->q, t->heap = true;
    struct k_ren *x = st;
    st = st->next;
    kfree(x); }
  return 0; }
static ai_word k_rename(ai_word ow, ai_word nw) {
  if (!ai_strp(ow) || !ai_strp(nw)) return putcharm(EINVAL);
  struct ai_str *ov = (struct ai_str*) ow, *nv = (struct ai_str*) nw;
  int e = k_fs_rename(ov->bytes, ov->len, nv->bytes, nv->len);
  return e ? putcharm(-e) : ZeroPoint; }
static lvm(lvm_rename) {
  Sp[1] = k_rename(Sp[0], Sp[1]);
  Sp += 1; ai_musttail return Next(1); }

ai_noinline static int k_fs_chdir(char const *p, uintptr_t pn) {
  char cp[256];
  intptr_t cn;
  if (!k_fs_init()) return -ENOMEM;
  if ((cn = k_canon(p, pn, cp)) < 0) return -ENAMETOOLONG;
  if (cn) {
    int i = k_find(cp, (uintptr_t) cn);
    if (i >= 0 && !k_ents[i].dir) return -ENOTDIR;
    uintptr_t junk;
    if (i < 0 && !k_kids(cp, (uintptr_t) cn, &junk)) return -ENOENT; }
  memcpy(k_cwd, cp, (uintptr_t) cn), k_cwd_n = (uintptr_t) cn;
  return 0; }
// ⚠ chdir's LOVE face answers a NEGATIVE errno where its six siblings answer a
// positive one -- and so does the host's (host/posix.c's host_chdir, likewise
// putcharm(-errno)), which is what makes it a shape rather than a slip. The
// twins agree; doc/posix.md's "effect ops answer a POSITIVE errno" simply does
// not cover this one. So this is the wrapper that does NOT flip: the face below
// already answers negative, like every other C face here.
static ai_word k_chdir(ai_word pw) {
  if (!ai_strp(pw)) return putcharm(-1);
  struct ai_str *pv = (struct ai_str*) pw;
  int e = k_fs_chdir(pv->bytes, pv->len);
  return e ? putcharm(e) : ZeroPoint; }
static lvm(lvm_chdir) { Sp[0] = k_chdir(Sp[0]); ai_musttail return Next(1); }

// (cwd _) -> the seat as an absolute string -- the host's shape, for the prompt.
// the PATH FACE: the seat into a caller's buffer, 0 ok or -ERANGE -- getcwd(2)'s
// own refusal, and the one thing the love door never had to say (its buffer is
// sized to the seat by construction).
static int k_fs_getcwd(char *b, uintptr_t n) {
  if (n < k_cwd_n + 2) return -ERANGE;            // '/' + the seat + the NUL
  b[0] = '/';
  memcpy(b + 1, k_cwd, k_cwd_n);
  b[1 + k_cwd_n] = 0;
  return 0; }
ai_noinline static struct ai *k_cwd_read(struct ai *g) {
  char b[258];
  k_fs_getcwd(b, sizeof b);                      // sizeof b > k_cwd's ceiling: cannot refuse
  if (!ai_ok(g = ai_strof(g, b))) return g;
  return g->sp[1] = g->sp[0], g->sp += 1, g; }  // cwd string over the dummy arg
static lvm(lvm_cwd) {
  Pack(g); g = k_cwd_read(g);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }

// the two attribute writers land on the ENTRY, so a synthesized (prefix)
// directory takes either as a no-op: it has no row to keep bits on, and its date
// is its children's. absence stays loud.
ai_noinline static int k_fs_chmod(char const *p, uintptr_t pn, uintptr_t mode) {
  char cp[256];
  intptr_t cn;
  if (!k_fs_init()) return -ENOMEM;
  if ((cn = k_canon(p, pn, cp)) < 0) return -ENOENT;
  int i = k_find(cp, (uintptr_t) cn);
  if (i < 0) return k_dirp(cp, (uintptr_t) cn) ? 0 : -ENOENT;
  k_ents[i].mode = mode & 07777;
  return 0; }
static ai_word k_chmod(ai_word pw, ai_word mw) {
  if (!ai_strp(pw) || !(mw & 1)) return putcharm(EINVAL);
  struct ai_str *pv = (struct ai_str*) pw;
  int e = k_fs_chmod(pv->bytes, pv->len, (uintptr_t) getcharm(mw));
  return e ? putcharm(-e) : ZeroPoint; }
static lvm(lvm_chmod) {
  Sp[1] = k_chmod(Sp[0], Sp[1]);
  Sp += 1; ai_musttail return Next(1); }

ai_noinline static int k_fs_utime(char const *p, uintptr_t pn, uintptr_t ms) {
  char cp[256];
  intptr_t cn;
  if (!k_fs_init()) return -ENOMEM;
  if ((cn = k_canon(p, pn, cp)) < 0) return -ENOENT;
  int i = k_find(cp, (uintptr_t) cn);
  if (i < 0) return k_dirp(cp, (uintptr_t) cn) ? 0 : -ENOENT;
  k_ents[i].ms = ms;
  return 0; }
static ai_word k_utime(ai_word pw, ai_word mw) {
  if (!ai_strp(pw)) return putcharm(EINVAL);
  struct ai_str *pv = (struct ai_str*) pw;
  int e = k_fs_utime(pv->bytes, pv->len,
                     (mw & 1) ? (uintptr_t) getcharm(mw) : ai_clock());
  return e ? putcharm(-e) : ZeroPoint; }
static lvm(lvm_utime) {
  Sp[1] = k_utime(Sp[0], Sp[1]);
  Sp += 1; ai_musttail return Next(1); }

static lvm(ai_kreset) { return k_reset(), g; }

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
  // the paper is minted per FRAME, never per row: kticks is bumped by the timer ISR,
  // so re-reading the blink phase mid-frame could paint one row lit and the next dark.
  struct cb_paper const paper = { kfb._, kfb.pitch, kfb.width, kfb.height };
  for (uint16_t i = 0; i < rows; i++) {
    uint32_t const r = i > 255 ? 255 : i;   // quay's fold: bit 255 stands for 255-and-past
    if (kcb->dmg[r >> 5] >> (r & 31) & 1 || (moved && (i == was || i == now)))
      cb_paint(&paper, kcb, &kfont, i, 0, 0, blink ? cur : ~0u); }
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

// (syswrite fd str) -> the count landed, or -1 on a non-string. THE SYSCALL
// SEAM'S ONE GATE: it calls nolibc's write(), which is sc3(NR_write, ..) into
// free/sys.c, which is the row -- so a green test/kernel/sys.l says that whole
// path is live and no other test in the tree can say it. K_TEST only: the
// shipped kernel has no reason to spell a syscall in love.
extern long write(int, void const *, long);
ai_noinline static ai_word k_syswrite(ai_word fw, ai_word sw) {
  if (!ai_strp(sw)) return putcharm(-1);
  struct ai_str *pv = (struct ai_str*) sw;
  return putcharm(write((int) getcharm(fw), pv->bytes, (long) pv->len)); }
static lvm(lvm_syswrite) {
  Sp[1] = k_syswrite(Sp[0], Sp[1]);
  Sp += 1; ai_musttail return Next(1); }

// (syscall "name" a b c) -> the raw answer, errno NEGATIVE as the door gives
// it; () for a name no row answers to. The instrument for every row whose
// arguments are numbers, so the next one costs a test and not a nif -- and it
// takes the NAME because the numbers are arch-keyed and free/sys.c is the only
// file that may spell them. ⚠ it reaches __ai_sys DIRECTLY, under nolibc: what
// it gates is the dispatch and the k_fd_* faces, which is where the rows are
// written. syswrite proves the nolibc half once, so the composition is said.
extern long __ai_sys(long, long, long, long, long, long, long);
extern long k_sys_nr(char const *nm, long n);
ai_noinline static ai_word k_syscall(ai_word nw, ai_word aw, ai_word bw, ai_word cw) {
  if (!ai_strp(nw)) return ZeroPoint;
  struct ai_str *pv = (struct ai_str*) nw;
  long nr = k_sys_nr(pv->bytes, (long) pv->len);
  if (nr < 0) return ZeroPoint;
  ai_word ws[3] = { aw, bw, cw };
  long v[3];
  for (int i = 0; i < 3; i++) v[i] = (ws[i] & 1) ? (long) getcharm(ws[i]) : 0;
  return putcharm(__ai_sys(nr, v[0], v[1], v[2], 0, 0, 0)); }
static lvm(lvm_syscall) {
  Sp[3] = k_syscall(Sp[0], Sp[1], Sp[2], Sp[3]);
  Sp += 3; ai_musttail return Next(1); }
#endif

// (quit code) -- the exit door, and since rung 4 the door with two rooms behind
// it. a SEATED task (a spawned process) quits as _exit: its seated fds close --
// the write end's close is the downstream reader's EOF -- the seat retires, and
// the TASK lands dormant with the code as its retval, which is what `wait`
// (catch) answers. every program exit funnels here: the shim's wrapper quits the
// main's answer, its help quits a scare, and a folded (quit 0) inside a kore
// main was always going to arrive on its own feet.
// unseated, the exit is the MACHINE's, as rung 3 laid it: reset on the shipped
// kernel. the TEST kernel answers the code instead (kore0.l's identity pin, one
// door deeper) -- a reset would eat the corpus summary, and `exit` stays the
// one way out of qemu.
static union u const k_exit_body[] = { {lvm_task_exit} };
// the seated half: close the seat's fds (the write end's close is the reader's
// EOF), retire the slot, clear the yield intentions. -> nonzero when a seat was
// there, so the wrapper knows which room it is in.
ai_noinline static int k_seat_exit(struct ai *g) {
  intptr_t pid = k_cur_pid(g);
  struct k_seat *s = pid ? k_seat_find(pid) : NULL;
  if (!s) return 0;
  for (int i = 0; i < 3; i++) if (s->fd[i] >= 0) ai_fd_close(s->fd[i]);
  s->pid = 0;                                   // the slot is free for the next spawn
  g->next_wake_at = 0;                          // a stale intention would gate the park
  g->next_wait_fd = -1;
  return 1; }
static lvm(lvm_quit) {
  if (k_seat_exit(g)) {
    // the love-machine _exit: the stack becomes just [code] and Ip a task-exit
    // cell, exactly the shape lvm_task_exit leaves -- catch reads node[7], donep
    // and scoop read the saved ap. the frame below Sp is abandoned whole.
    ai_word code = (Sp[0] & 1) ? Sp[0] : putcharm(0);
    Sp = (ai_word*) g + g->len - 1;
    Sp[0] = code;
    Ip = (union u*) k_exit_body;
    ai_musttail return Ap(lvm_task_exit, g); }
#ifdef K_TEST
  ai_musttail return Next(1);                   // unseated on the test kernel: the identity
#else
  k_reset(); Ip += 1; ai_musttail return Continue();
#endif
}



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
  nif_mkdir[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_mkdir}, {lvm_ret0}},
  nif_rmdir[] = {{lvm_rmdir}, {lvm_ret0}},
  nif_unlink[] = {{lvm_unlink}, {lvm_ret0}},
  nif_rename[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_rename}, {lvm_ret0}},
  nif_chdir[] = {{lvm_chdir}, {lvm_ret0}},
  nif_cwd[] = {{lvm_cwd}, {lvm_ret0}},
  nif_chmod[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_chmod}, {lvm_ret0}},
  nif_utime[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_utime}, {lvm_ret0}},
  nif_pipe[] = {{lvm_pipe}, {lvm_ret0}},
  nif_fdopen[] = {{lvm_fdopen}, {lvm_ret0}},
  nif_dup[] = {{lvm_dup}, {lvm_ret0}},
  nif_dup2[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_dup2}, {lvm_ret0}},
  nif_getpid[] = {{lvm_getpid}, {lvm_ret0}},
  nif_procseat[] = {{lvm_cur}, {.x = putcharm(4)}, {lvm_procseat}, {lvm_ret0}},
  nif_disk[] = {{lvm_disk}, {lvm_ret0}},
  nif_disk_read[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_disk_read}, {lvm_ret0}},
  nif_disk_write[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_disk_write}, {lvm_ret0}},
#if defined(__x86_64__)
  nif_svm[] = {{lvm_svm}, {lvm_ret0}},
  nif_svm_run[] = {{lvm_svm_run}, {lvm_ret0}},
  nif_vmx[] = {{lvm_vmx}, {lvm_ret0}},
  nif_vmx_run[] = {{lvm_vmx_run}, {lvm_ret0}},
#endif
  nif_quit[] = {{lvm_quit}, {lvm_ret0}},
#ifdef K_TEST
  nif_exit[] = {{lvm_kexit}, {lvm_ret0}},
  nif_syswrite[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_syswrite}, {lvm_ret0}},
  nif_syscall[] = {{lvm_cur}, {.x = putcharm(4)}, {lvm_syscall}, {lvm_ret0}},
#endif
  nif_fault[] = {{lvm_fault}, {lvm_ret0}};

// Reads the door-populated kboot struct and
// links every reported free range into the kernel free list. The
// chained-into-kmem order matches the memmap walk order:
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

// the kernel's nifs ride the ai_nifs section, the same one AiNif lands a host
// app's in -- so this table is drained by the __start_/__stop_ bracket and named
// by nothing. `used` is what keeps it, and the whole array in one blob is the
// AiModNifs shape rather than a row at a time. What it buys: a host/<app>.c
// dropped into the kernel build registers itself with no edit here, and the
// image's host slice (core/love.c) indexes a kernel nif the way it does a host
// one. ⚠ the slice is INDEXED BY POSITION, so this order is part of an image's
// contract -- append, do not insert.
static struct ai_def const __attribute__((section("ai_nifs"), used)) defs[] = {
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
  // the writable tree (rung 2), the host's names and shapes again
  {"mkdir", (intptr_t) nif_mkdir},
  {"rmdir", (intptr_t) nif_rmdir},
  {"unlink", (intptr_t) nif_unlink},
  {"rename", (intptr_t) nif_rename},
  {"chdir", (intptr_t) nif_chdir},
  {"cwd", (intptr_t) nif_cwd},
  {"chmod", (intptr_t) nif_chmod},
  {"utime", (intptr_t) nif_utime},
  // rung 4: the pipe pair, the fd plumbing, and the process seat under the
  // spawn shim (the boot text below). host names, host shapes, as ever.
  {"pipe", (intptr_t) nif_pipe},
  {"fdopen", (intptr_t) nif_fdopen},
  {"dup", (intptr_t) nif_dup},
  {"dup2", (intptr_t) nif_dup2},
  {"getpid", (intptr_t) nif_getpid},
  {"procseat", (intptr_t) nif_procseat},
  // rung 5: the disk -- the raw block door lib/fat.l's filesystem rides. these
  // three are OURS (no host twin: the host has no raw disk), so the shapes are
  // love's -- absence and refusal answer (), presence is the green sector count.
  {"disk", (intptr_t) nif_disk},
  {"disk-read", (intptr_t) nif_disk_read},
  {"disk-write", (intptr_t) nif_disk_write},
  // ⚠ x86_64 only, so a love-side reader must ask (member? 'svm (names ()))
  // before it asks (svm ()) -- on the aarch64 seat the nom is not in the book
  // at all, and reading it is a missing condition rather than an absence.
#if defined(__x86_64__)
  {"svm", (intptr_t) nif_svm},
  {"svm-run", (intptr_t) nif_svm_run},
  {"vmx", (intptr_t) nif_vmx},
  {"vmx-run", (intptr_t) nif_vmx_run},
#endif
  // quit is seat-aware now (rung 4): a spawned task's exit is the TASK's, so
  // the row is owed on BOTH kernels. unseated it resets the shipped machine;
  // the TEST kernel's unseated arm answers the code instead (kore0.l's identity
  // pin, one door deeper), so a failing assert still cannot eat the summary,
  // and `exit` stays qemu's one door out.
  {"quit", (intptr_t) nif_quit},
#ifdef K_TEST
  {"exit", (intptr_t) nif_exit},
  {"syswrite", (intptr_t) nif_syswrite},
  {"syscall", (intptr_t) nif_syscall},
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
// verbs: the verb REGISTRY (love/verbs.l). the kernel wants one for the plainest
// reason -- its userland IS a verb table, and the boot cmdline's program seat reads
// it. cook.l and lush ride the cat and seat themselves through the same door.
static char const src_verbs[] =
#include "verbs.h"
 ;
static char const src_pat[] =
#include "pat.h"
 ;
static char const src_uu[] =
#include "uu.h"
;
static char const src_bao[] =
#include "bao.h"
;
#ifndef K_TEST
// the shipped kernel's userland (rung 3): holo -- the arch-neutral core plus the
// NATIVE backend, host/main.c's shape -- because the kore cat's asbook.l opens
// with (use 'holo); then the whole $(korefiles) cat, evaled at boot through the
// stream shell. (the TEST kernel skips both: its corpus bakes the kore subset it
// drives, and the full cat would only slow every gate boot.)
static char const src_holo[] =
#include "holo.h"
#if defined(__x86_64__)
#include "x64.h"
#elif defined(__aarch64__)
#include "arm64.h"
#endif
;
static char const src_kore[] =
#include "korecat.h"
;
// peg: cook.l (in the cat) opens with (use 'peg)
static char const src_peg[] =
#include "peg.h"
;
#endif
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
  {"verbs", src_verbs}, {"pat", src_pat}, {"uu", src_uu}, {"bao", src_bao},
#ifdef K_TEST
  {"coin", src_coin}, {"rng", src_rng}, {"q", src_q}, {"kanren", src_kanren},
#else
  {"holo", src_holo}, {"peg", src_peg},
#endif
  {NULL, NULL} };
struct ai_lib const *ai_libs(void) { return libs; }

void kmain(void) {
#if defined(__x86_64__)
 // Enable x87/SSE before ANY other C runs -- a compiler vectorizes freely on
 // x86_64 (even a struct copy compiles to movups), and that #UDs into a triple
 // fault with no output while SSE is masked. This is the single SSE-enable
 // point; archinit no longer repeats it.
 k_sse_enable();
#endif
 khhdm = kboot.hhdm;
 archinit();
 // the wall date, in the one order that can answer on every door: whatever the
 // door left in kboot if it had one, else the machine's RTC -- which archinit has
 // just made reachable (the aarch64 read is device memory, and mmio_map lays it).
 if (!kboot.date) kboot.date = k_rtc();
 serial_init();
 // the heap (meminit) is the only hard requirement. the framebuffer
 // console is optional: when fbinit/cbinit fail -- the door handed over no
 // framebuffer, or the console buffer won't allocate -- kcb stays null
 // and the kernel runs headless on the serial console alone.
 if (meminit()) {
  if (fbinit()) cbinit();        // the framebuffer console; the palette is a table now
  // the disk (rung 5): probe the bus, and hand the driver its one DMA block --
  // kmallocw memory, so pa = va - khhdm holds for everything the device reads.
  k_blk_init(kmallocw(b2w(352)));
  struct ai *g = ai_defn(ai_ini(), __start_ai_nifs,
                         (uintptr_t)(__stop_ai_nifs - __start_ai_nifs), 0);
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
  g = ai_defn(g, td, countof(td), 0);
#else
  // the kore cat, bound whole (rung 3); the session below drinks it through a tap.
  g = ai_strof(g, src_kore);
  struct ai_def kd[] = {{"korecat", ai_pop1(g)}};
  g = ai_defn(g, kd, countof(kd), 0);
#endif
  // the boot cmdline, raw; the boot text below splits it into the argv shape.
  g = ai_strof(g, kboot.cmdline);
  struct ai_def bd[] = {{"bootline", ai_pop1(g)}};
  g = ai_defn(g, bd, countof(bd), 0);
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
 ,
#include "pat.h"                                     // ⚠ pat RIDES THE POST TEXT: post is written in @, and a
    " "                                              //   macro reaches a reader only once it is in the book
#include "post.h"
 );
  r = ai_evals_(r,
 "(use 'verbs)" // the registry FIRST: the cat's apps pin their own names at load
 "(use 'pat)"   // ⚠ pat BEFORE uu: uu.l is written in @, and a macro reaches a reader
 "(use 'uu) (: uu (from 'uu))"                         // the uu kernel: the corpus's uu files drive it through the
 "(use 'bao)"                                          //   one-name `uu` surface on this target too
 // the environment (rung 2): a TABLET, the pairs on slot 0, closures over it
 // wearing the host's names and shapes -- getenv the value | () absent/misused,
 // setenv () | EINVAL misuse (a non-string value UNSETS, the absence lane),
 // environ the raw "NAME=value" strings.
 "(: envt (tablet 0)"
 "   (envget l n) (? (two? l) (? (= n (cap (cap l))) (cup (cap l)) (envget (cup l) n)) ())"
 "   (envcut l n) (? (two? l) (? (= n (cap (cap l))) (envcut (cup l) n)"
 "                              (link (cap l) (envcut (cup l) n))) ())"
 "   (getenv n) (? (string? n) (envget (peep envt 0 ()) n) ())"
 "   (setenv n v) (? (string? n)"
 "                   (: c (envcut (peep envt 0 ()) n)"
 "                      _ (pin envt 0 (? (string? v) (link (link n v) c) c)) ())"
 "                   22)"
 "   (environ u) (map (\\ e (+ (cap e) (+ \"=\" (cup e)))) (peep envt 0 ())))"
 // the command line (rung 3): `bootargv` = (word..) off the raw boot line, split
 // quote-aware (-append 'sh -c \"cd lib; pwd\"' must reach the shell as one command).
 // ⚠ `cmdline` stays SEATLESS until the cat is in: a member's seat fires as its own
 // file is read, and lush sits mid-cat -- it would take the machine with kore's
 // applets still unread. the boot dispatch at the foot wears the real line.
 "(: bootargv"
 "     (: (kw i w s acc) (? (<= (tally bootline) i) (rev (? (tally w) (link w acc) acc))"
 "                          (: c (bootline i)"
 // ⚠ a char joins a string as a STRING OF ONE: (+ w c) on mixed bands
 // degenerates to w alone, so a bare charm would drop every word's letters.
 "                             (? s (? (= c s) (kw (+ i 1) w 0 acc) (kw (+ i 1) (+ w (string c)) s acc))"
 "                                (= c 32) (kw (+ i 1) \"\" 0 (? (tally w) (link w acc) acc))"
 "                                (|| (= c 34) (= c 39)) (kw (+ i 1) w c acc)"
 "                                (kw (+ i 1) (+ w (string c)) 0 acc))))"
 "        (kw 0 \"\" 0 ()))"
 "   cmdline (link \"love\" ())"
 "   argv cmdline)"
 // rung 4: spawn/wait as a love-side shim over the core task ops. a process on
 // this machine IS a task: k-prog maps argv onto a love main -- a VERB off the
 // registry (kore, sh, every applet the cat pinned), a tool's own <name>-main
 // where nothing registered one, or a .l
 // path off the ramfs, evaled form by form (⚠ no fresh layer from here: its
 // defglobs land in the session, the shim's honest divergence) -- and k-spawn1
 // twirls it under a help that quits any scare (the wait-side face of a died
 // child), then seats the pid's stdio (procseat, in the PARENT: twirl does not
 // switch, so the seat is laid before the child's first read). every exit
 // funnels through the seat-aware quit; wait is catch, the pid is the task pid.
 // pg/fg/closes are accepted and ignored: no process groups, no ^Z, and the
 // seat dups its own ends so there is nothing for a child to leak.
 "(: (k-bn p) (: n (tally p)"
 "     (go i r) (? (< i n) (go (+ i 1) (? (= (p i) 47) (+ i 1) r)) (snip p r n))"
 "     (go 0 0))"
 "   (k-run-file p) (\\ as (: q (open p \"r\")"
 "     (? (port? q)"
 "        (: t (slurp q) _ (close q)"
 "           (go cl) (: r (sound cl) (? (two? r) (: _ (ev (cap r)) (go (cup r))) 0))"
 "           (go t))"
 "        127)))"
 "   (k-tool nm as) (? (member? nm (names ())) (link (ev nm) as) ())"
 // the registry is the PATH on this machine: every app pins its own names into
 // (from 'verbs 'tab), and `word` applies the shadow rules -- a slashed word or
 // a .l name is a file and never a verb, which is what leaves the two lanes
 // below reachable. a verb takes the args AFTER its name, kore's convention.
 "   (k-prog argv) (: a0 (cap argv) b (k-bn a0) as (cup argv)"
 "     v ((from 'verbs 'word) a0)"
 "     (? !(nil? v) (link v as)"
 "        (: k (k-tool (intern (+ b \"-main\")) as)"
 "           (? (two? k) k"
 // `kore TOOL ..` where no dispatcher registered one -- the test kernel's seat,
 // which bakes the applet files and not kore.l
 "              (&& (= b \"kore\") (two? as))"
 "                (k-tool (intern (+ (cap as) \"-main\")) (cup as))"
 "              (two? (stat a0)) (link (k-run-file a0) as)"
 "              ()))))"
 "   (k-spawn1 argv f0 f1 f2) (: pr (k-prog argv)"
 // ⚠ the help is the seat's exit door too: a kore main leaves deep by scaring 'leave
 // with its status (crew/kore/core.l), and taking that as a plain scare would flatten
 // every usage code to 1. every other condition is the died-child face.
 "     p (twirl (\\ _ (: _ (hear (\\ a b (? (id? a 'leave) (quit b)"
 "                                        (: _ (say err \";; \") _ (print err a)"
 "                                           _ (say err \" \") _ (print err b)"
 "                                           _ (put err 10) (quit 1)))))"
 "                     r (? (two? pr) ((cap pr) (cup pr))"
 "                          (: _ (say err (+ (cap argv) \": not found\"))"
 "                             _ (put err 10) 127))"
 "                     (quit (? (charm? r) r 0))))"
 "              0)"
 "     _ (procseat p f0 f1 f2)"
 "     p)"
 "   (k-fdw x) (? (charm? x) (? (< x 0) (- 0 1) x) (- 0 1))"
 "   (spawn argv) (k-spawn1 argv (- 0 1) (- 0 1) (- 0 1))"
 "   (spawnio argv i o e cl pg fg) (k-spawn1 argv (k-fdw i) (k-fdw o) (k-fdw e))"
 "   (spawnmap argv fdm cl pg fg)"
 "     ((: (go m a b c)"
 "          (? (! (two? m)) (k-spawn1 argv a b c)"
 "             (: e (cap m) cf (cap e) sf (cup e)"
 "                v (? (charm? sf)"
 "                     (? (&& (<= 0 sf) (< sf 3))"
 "                        (: w (? (= sf 0) a (= sf 1) b c) (? (< w 0) sf w))"
 "                        sf)"
 "                     (- 0 2))"
 "                (? (= cf 0) (go (cup m) v b c)"
 "                   (= cf 1) (go (cup m) a v c)"
 "                   (= cf 2) (go (cup m) a b v)"
 "                   (go (cup m) a b c))))"
 "        go)"
 "      fdm (- 0 1) (- 0 1) (- 0 1))"
 "   (wait p) (catch p))"
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
  // rung 3: the userland. first test/00-init.l's move, for the same reason it
  // makes it: an unbound mention raises missing at every define that names one,
  // and bao's file-help now folds a REAL quit -- one absent nif in the cat and
  // the machine resets at load. so pin a no-op fallback for whichever host nifs
  // the cat mentions and this seat lacks (self-retiring: a rung that lands the
  // real nif takes its name off this list by existing). raw answers () -- the
  // console is always a raw tty; signal accepts and ignores, there are no
  // signals on this machine -- lush's interactive entry rides both.
  r = ai_evals_(r,
   "(: (raw m) () (signal n h) ())"
   "(map (\\ n (? (member? n (names ())) () (ev `(': `(n 'x) ()))))"
   "     '(symlink hardlink readlink spawn spawnmap fork exec herald wait still"
   "       getpid getuid seal ttyfg glean pipe fdopen dup dup2 connect listen"
   "       accept udp-bind udp-send udp-recv hark winsize))");
  // then the kore cat through the stream shell, quietly: the line is seatless
  // here, so every member's own seat sits out and the whole userland lands.
  r = ai_evals_(r, "(reads (tap ((: (g i) (? (< i (tally korecat)) (link (peep korecat i 0) (g (+ 1 i))))) 0)))");
  // now the line wears its real shape and the program word dispatches off the
  // registry -- spawn's own door. a seated program quits with its status (the
  // reset door); an empty line falls to the console shell, the toolbox warm.
  r = ai_evals_(r, "(: cmdline (link \"love\" bootargv) argv cmdline)");
  r = ai_evals_(r,
   "(? (two? bootargv)"
   "   (: _ (hear (\\ a b (? (id? a 'leave) (quit b)"
   "                        (: _ (say err \";; \") _ (print err a) _ (say err \" \") _ (print err b)"
   "                           _ (put err 10) (quit 1)))))"
   "      pr (k-prog bootargv)"
   "      r (? (two? pr) ((cap pr) (cup pr))"
   "           (: _ (say err (+ (cap bootargv) \": not found\")) _ (put err 10) 127))"
   "      (quit (? (charm? r) r 0)))"
   "   0)");
  r = ai_evals_(r, "((from 'bao 'shell) 0)");
#endif
  // a terminal scare gets the honest face on the serial console before reset
  if (ai_code_of(r) == ai_status_scare) ai_scare_face_(r);
  ai_fin(r); }
#ifdef K_TEST
 k_qemu_exit(0);   // corpus done with no failures -> quit qemu (exit 0)
#endif
 k_reset(); }
