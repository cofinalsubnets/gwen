#include "limine/limine.h"
#include "g.h"
#include <stdarg.h>
#include <limits.h>
void k_reset(void), arch_init(void);
uint8_t key_get(void);

static g_inline void k_stop(void) { asm volatile (
#if defined (__x86_64__)
  "hlt"
#elif defined (__aarch64__) || defined (__riscv)
  "wfi"
#elif defined (__loongarch64)
  "idle 0"
#endif
  ); }

typedef struct g_fb32 {
  volatile uint32_t *_;
  uint16_t width, height, pitch, cur_x, cur_y; } g_fb32;

typedef struct g_cb {
  struct { uint8_t c0, c1, rgb[2][3]; } *cb;
  uint16_t rows, cols, cx, cy; } g_cb;

void
  *malloc(size_t),
  free(void*),
  g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px),
  g_fb32_ini(g_fb32*, uint32_t *_, size_t width, size_t height, size_t pitch),
  g_fb32_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint8_t *bmp, uint32_t fg, uint32_t bg);
void
  g_fb32_msg(g_fb32 *fb, size_t row, size_t col, const char *msg, uint32_t fg, uint32_t bg),
  k_cur_msg(const char *msg, uint32_t fg, uint32_t bg);

extern struct k {
  uint64_t ticks;
  g_cb cb;
  g_fb32 fb;
  struct mem {
    uintptr_t len;
    struct mem *next;
    uint8_t _[];
  } *free, *used;
  struct g *g;
  struct { uint8_t k, f; } kb; } K;
void k_logf(const char*, ...), k_vlogf(const char*, va_list), k_log_char(char);
#include "font.h"
#include "cb.h"
#include <stdarg.h>
__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
#define _L __attribute__((used, section(".limine_requests"))) static volatile
_L LIMINE_BASE_REVISION(3);
_L struct limine_memmap_request memmap_req = { .id = LIMINE_MEMMAP_REQUEST, .revision = 0 };
_L struct limine_hhdm_request hhdm_req = { .id = LIMINE_HHDM_REQUEST, .revision = 0 };
_L struct limine_framebuffer_request fb_req = { .id = LIMINE_FRAMEBUFFER_REQUEST, .revision = 0 };
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;
int rand(void);
void srand(unsigned int);

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
#define NROWS 30
#define NCOLS 50
static struct {
  uint8_t rows, cols, row, col, rr, rc, flag, flag2, cb[NROWS * NCOLS];
} _kcb = { NROWS, NCOLS, 0, 0, 0, 0, 0, 0, {0}};
#define kcb ((struct cb*)&_kcb)


void g_stdout_putc(int c) { cb_put_char(kcb, c); }

int g_stdin_getc(void) { return cb_getc(kcb); }
int g_stdin_ungetc(int c) { return cb_ungetc(kcb, c); }
int g_stdin_eof(void) { return cb_eof(kcb); }
uintptr_t g_clock(void) { return K.ticks; }

static void draw_char_buffer(g_fb32 *fb, struct cb *c);

uint8_t key_get(void) {
  uint8_t r = K.kb.k;
  K.kb.k = 0;
  return r; }

static void key_put(uint8_t c) {
  static const uint8_t
    kb2ascii[128] = {
       0,  27, '1',  '2', '3', '4', '5', '6',
     '7', '8', '9',  '0', '-', '=',   8,   9,
     'q', 'w', 'e',  'r', 't', 'y', 'u', 'i',
     'o', 'p', '[',  ']',  10,   0, 'a', 's',
     'd', 'f', 'g',  'h', 'j', 'k', 'l', ';',
    '\'', '`',   0, '\\', 'z', 'x', 'c', 'v',
     'b', 'n', 'm',  ',', '.', '/',   0, '*',
       0, ' ' },
    shift_kb2ascii[128] = {
       0,  27, '!',  '@', '#', '$', '%', '^',
     '&', '*', '(',  ')', '_', '+',   8,   9,
     'Q', 'W', 'E',  'R', 'T', 'Y', 'U', 'I',
     'O', 'P', '{',  '}',  10,   0, 'A', 'S',
     'D', 'F', 'G',  'H', 'J', 'K', 'L', ':',
     '"', '~',   0,  '|', 'Z', 'X', 'C', 'V',
     'B', 'N', 'M',  '<', '>', '?',   0, '*',
       0, ' ' };
  if (K.kb.k == 0) K.kb.k = 
    (K.kb.f & (kb_flag_lshift | kb_flag_rshift) ? shift_kb2ascii : kb2ascii)[c]; }

void kb_int(uint8_t code) {
  if (code == kb_code_extend) {
    K.kb.f |= kb_flag_extend;
    return; }
  if (K.kb.f & kb_flag_extend) {
    K.kb.f &= ~kb_flag_extend;
    if (code < 128) switch (code) {
      case kb_code_alt: K.kb.f |= kb_flag_ralt; break;
      case kb_code_ctl: K.kb.f |= kb_flag_rctl; break;
      case kb_code_delete:
        if (K.kb.f & kb_flag_ctl & kb_flag_alt) k_reset();
        break;
      default: }
    else switch (code - 128) {
      case kb_code_alt: K.kb.f &= ~kb_flag_ralt; break;
      case kb_code_ctl: K.kb.f &= ~kb_flag_rctl; break;
      default: } }
  else {
    if (code < 128) switch (code) {
      case kb_code_lshift: K.kb.f |= kb_flag_lshift; break;
      case kb_code_rshift: K.kb.f |= kb_flag_rshift; break;
      case kb_code_alt:    K.kb.f |= kb_flag_lalt;   break;
      case kb_code_ctl:    K.kb.f |= kb_flag_lctl;   break;
      default: key_put(code); }
    else switch (code - 128) {
      case kb_code_lshift: K.kb.f &= ~kb_flag_lshift; break;
      case kb_code_rshift: K.kb.f &= ~kb_flag_rshift; break;
      case kb_code_alt:    K.kb.f &= ~kb_flag_lalt;   break;
      case kb_code_ctl:    K.kb.f &= ~kb_flag_lctl;   break;
      default: } } }
void g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px) {
  fb->_[row * fb->pitch / 4 + col] = px; }

// add a scale parameter to this
void g_fb32_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint8_t *bmp, uint32_t fg, uint32_t bg) {
  for (int r = 0; r < 8; r++) {
    uint8_t o = bmp[r];
    for (int c = 0; c < 8; c++, o <<= 1) {
      fb->_[(row + r) * fb->pitch / 4 + col + c] = o & 128 ? fg : bg; } } }

void g_fb32_char(g_fb32 *fb, size_t row, size_t col, uint8_t c, uint32_t fg, uint32_t bg) {
  g_fb32_bmp_8x8(fb, row, col, cga_8x8[c], fg, bg); }

#define show_cursor 1
#define blue 0xff
#define green 0xff00
#define red 0xff0000
#define cyan (blue|green)
#define yellow (red|green)
#define white (yellow|blue)
#define magenta (red|blue)
#define black 0
#define console_bg 0x4b4f58
#define console_fg 0xe9edf0
#define console_cur 0x6ba7a2
#define console_sel 0xc3e4e0
static void draw_char_buffer(g_fb32 *fb, struct cb *c) {
  uint8_t rows = c->rows, cols = c->cols,
          raddr = cols * c->rr + c->rc,
          waddr = cols * c->row + c->col;

  for (int32_t i = 0; i < rows; i++)
    for (int32_t j = 0; j < cols; j++) {
      uint8_t g = c->cb[i * cols + j];
      uint32_t fg = console_fg, bg = console_bg;
      uint8_t addr = i * cols + j;
      if (raddr <= addr && addr < waddr)
        fg = console_sel;
      if ((c->flag & show_cursor) && i == c->row && j == c->col)
        fg = bg, bg = console_cur;
      g_fb32_char(fb, i*8, j*8, g, fg, bg); } }

void g_fb32_msg(g_fb32 *fb, size_t row, size_t col, const char *msg, uint32_t fg, uint32_t bg) {
  size_t h = fb->height, w = fb->width;
  for (; *msg; msg++) {
    int c = *msg;
    g_fb32_char(fb, row, col, c, fg, bg);
    col += 8;
    if (col >= w)
      col %= w,
      row += 8,
      row %= h; } }

void k_cur_msg(const char *msg, uint32_t fg, uint32_t bg) {
  size_t h = K.fb.height / 8, w = K.fb.width / 8;
  for (; *msg; msg++) {
    int c = *msg;
    if (c == '\n') K.fb.cur_x = 0, K.fb.cur_y = (K.fb.cur_y + 1) % h;
    else {
      g_fb32_bmp_8x8(&K.fb, K.fb.cur_y * 8, K.fb.cur_x * 8, cga_8x8[c], fg, bg);
      K.fb.cur_x += 1;
      if (K.fb.cur_x >= w)
        K.fb.cur_x %= w,
        K.fb.cur_y += 1,
        K.fb.cur_y %= h; } } }

static void cputs(const char *s, uint32_t fg) {
  k_cur_msg(s, fg, 0); }

static void k_puts(const char *s) {
  cputs(s, console_fg); }

static void cputc(int c, uint32_t fg) {
  char s[2] = { c, 0 };
  cputs(s, fg); }

void cputn(uintptr_t n, uintptr_t base, uint32_t fg) {
  uintptr_t q = n / base, r = n % base;
  if (q) cputn(q, base, fg);
  cputc(g_digits[r], fg); }

static void k_putn(uintptr_t n, uintptr_t base) {
  cputn(n, base, console_fg); }


void g_fb32_ini(g_fb32 *b, uint32_t *_, size_t width, size_t height, size_t pitch) {
  b->_ = _;
  b->width = width;
  b->height = height;
  b->pitch = pitch;
  b->cur_x = b->cur_y = 0; }

static struct mem *kgetmem(uint64_t hhdm, uint64_t n, struct limine_memmap_entry **rr, uint64_t *nbs, uint64_t *nrs) {
  if (!n) return NULL;
  struct limine_memmap_entry *r = *rr;
  struct mem *next = kgetmem(hhdm, n-1, rr+1, nbs, nrs);
  if (r && r->type == 0 && r->length >= sizeof(struct mem) && r->base + r->length - 1 < 1l<<32) {
    struct mem *m = (struct mem*) (hhdm + r->base);
    *nrs += 1;
    *nbs += m->len = r->length;
    m->next = next;
    return m; }
  return next; }

static void fb_init(struct k*k) {
  // framebuffer init
  if (!fb_req.response || !fb_req.response->framebuffer_count)
    for (;;) k_stop();
  struct limine_framebuffer *fb0 = fb_req.response->framebuffers[0];
  g_fb32_ini(&k->fb, fb0->address, fb0->width, fb0->height, fb0->pitch);
  intptr_t x0 = k->fb.width / 2,
           y0 = k->fb.height / 2,
           rad = MIN(x0, y0);
  for (uint32_t color = 0xff0000, step = color / rad; rad; color -= step, rad--)
    for (intptr_t x = x0 - rad; x <= x0 + rad; x++)
      for (intptr_t y = y0 - rad; y <= y0 + rad; y++) {
        intptr_t dx = x - x0, dy = y - y0;
        if (dx * dx + dy * dy < rad * rad)
          g_fb32_px(&k->fb, y, x, color * (uintptr_t) k); }
  cputn(k->fb.width, 10, cyan);
  cputs("x", cyan);
  cputn(k->fb.height, 10, cyan);
  k_puts(" ");
  cputn(k->fb.width>>3, 10, cyan);
  cputs("x", cyan);
  cputn(k->fb.height>>3, 10, cyan);
  k_puts("\n"); }

static void mem_init(struct k*k) {
  if (!memmap_req.response || !hhdm_req.response) for (;;) k_stop();
  struct limine_memmap_entry **rr = memmap_req.response->entries;
  uint64_t hhdm = hhdm_req.response->offset,
           n = memmap_req.response->entry_count,
           nrs = 0,
           nbs = 0;
  k->free = kgetmem(hhdm, n, rr, &nbs, &nrs);
  k->used = NULL;
  if (nbs >= 1<<22)
    cputn(nbs>>20, 10, magenta), cputs("MiB in ", magenta);
  else if (nbs >= 1<<16)
    cputn(nbs>>10, 10, magenta), cputs("KiB in ", magenta);
  else
    k_putn(nbs, 10), cputs("B in ", magenta);
  cputn(nrs, 10, magenta), cputs(" regions\n", magenta); }

static void lisp_init(struct k*k) {
  static intptr_t g_static_pool[1<<23];
  struct g *f = g_ini_static(sizeof(g_static_pool), g_static_pool);
  cputs("\x01 lisp init -", yellow);
  uintptr_t t0 = K.ticks, t1;
  cputn(t0, 10, yellow);
  cputs(" + ", yellow);
  static const char b[] =
#include "boot.h"
  ;
  f = g_pop(g_evals(f, b), 1);
  cputn(t1 = K.ticks, 10, yellow);
  cputs(" = ", yellow);
  cputn(t1 - t0, 10, yellow);
  cputs(" ticks \x01\n", yellow);
//  f = g_vec0(g_pop(f, 1), g_vt_u8, 1, (uintptr_t) sizeof(struct cb) + NROWS * NCOLS);
  if (!f || !g_ok(f)) for (;;) k_stop();
  f->u[1] = (intptr_t) k;
  struct cb *c = kcb;
  c->rows = NROWS, c->cols = NCOLS;
  k->g = f; }

static void k_evals(const char *s) {
  K.g = g_pop(g_evals(K.g, s), 1); }

struct k K;
void kmain(void) {
  arch_init(); // arch specific init
  fb_init(&K);
  mem_init(&K);
  lisp_init(&K);

  srand(K.ticks);
  kcb->flag |= show_cursor;
  // main loop
  for (;;) {
    cb_fill(kcb, 0);
    cb_cur(kcb, 0, 0);
    k_evals(
      "(puts\"\x02 gwen lisp \")(putn(clock 0)10)(puts\"\n\")"
      "(: i(sysinfo 0)f(A i)pool(AB i)len(A(BB i))allocd(AB(BB i))stackd(A(BB(BB i)))"
     " (,"
      "(puts\"@\")(putn f 16)"
      "(puts\"\n@\")(putn pool 16)" 
      "(puts\"\n#\")(putn len 10)"
      "(puts\".\")(putn stackd 10)"
      "(puts\".\")(putn allocd 10)"
      "(puts\"\n\")"
      ")"
      ")"
    );
    draw_char_buffer(&K.fb, kcb);
    k_stop(); } }
