#include "limine/limine.h"
#include "g.h"
#include <stdarg.h>
#include <limits.h>
void kreset(void), kinit(void);
static uint8_t getkey(void);

static g_inline void kstop(void) { asm volatile (
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

static void g_fb32_cur_msg(g_fb32 *fb, const char *msg, uint32_t fg, uint32_t bg);
void
  *malloc(size_t),
  free(void*);
static void
  g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px),
  g_fb32_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint8_t *bmp, uint32_t fg, uint32_t bg);
void
  k_cur_msg(const char *msg, uint32_t fg, uint32_t bg);

extern struct k {
  uint64_t ticks;
  g_cb cb;
  g_fb32 fb;
  struct mem {
    uintptr_t len;
    struct mem *next;
    uintptr_t _[];
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
#define NROWS 64
#define NCOLS 64
#define gcb(f) ((struct cb*)g_str_txt((f)->u[0]))
#define kcb gcb(K.g)


void g_stdout_putc(struct g*f, int c) { cb_put_char(gcb(f), c); }

int g_stdin_getc(struct g*f) { return cb_getc(gcb(f)); }
int g_stdin_ungetc(struct g*f, int c) { return cb_ungetc(gcb(f), c); }
int g_stdin_eof(struct g*f) { return cb_eof(gcb(f)); }
uintptr_t g_clock(void) { return K.ticks; }

static void cputs(const char *s, uint32_t fg) {
  g_fb32_cur_msg(&K.fb, s, fg, 0); }

static void cputc(int c, uint32_t fg) {
  char s[2] = { c, 0 };
  cputs(s, fg); }

static void cputn(uintptr_t n, uintptr_t base, uint32_t fg) {
  uintptr_t q = n / base, r = n % base;
  if (q) cputn(q, base, fg);
  cputc(g_digits[r], fg); }

#define show_cursor 1
#define blue 0xff
#define green 0xff00
#define red 0xff0000
#define cyan (blue|green)
#define yellow (red|green)
#define white (yellow|blue)
#define magenta (red|blue)
#define black 0
#define console_bg black
#define console_fg 0xe9edf0
#define console_cur 0x6ba7a2
#define console_sel 0xc3e4e0
#define font_width 8
#define font_height font_width

uint8_t getkey(void) {
  uint8_t r = K.kb.k;
  K.kb.k = 0;
  return r; }

static void key_ext(uint8_t c) {
  struct cb *cb = kcb;
  uint16_t r = cb->rpos,
           w = cb->wpos,
           cs = cb->cols;
  switch (c) {
    case 75: w -= 1; break;
    case 77: w += 1; break;
    case 72: w -= cs; break;
    case 80: w += cs; break; }
  w %= cb->rows * cs;
  w = MAX(w, r);
  cb->wpos = w; }

static void key_put(uint8_t c) {
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
  if (K.kb.k == 0) {
    const uint8_t *map = (K.kb.f & (kb_flag_lshift | kb_flag_rshift) ? shift_kb2ascii : kb2ascii);
    c = c >= LEN(kb2ascii) ? c : map[c];
    K.kb.k = c; } }

void kb_int(const uint8_t code) {
  if (code == kb_code_extend) {
    K.kb.f |= kb_flag_extend;
    return; }
  uint32_t color = green;
  cputs("       ", color);
  K.fb.cur_x = 0;
  if (K.kb.f & kb_flag_extend) {
    K.kb.f &= ~kb_flag_extend;
    color = red;
    if (code < 128) switch (code) {
      case kb_code_alt: K.kb.f |= kb_flag_ralt; break;
      case kb_code_ctl: K.kb.f |= kb_flag_rctl; break;
      case kb_code_delete:
        if (K.kb.f & kb_flag_ctl & kb_flag_alt) kreset();
        break;
      default: key_ext(code); }
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
      default: } }
  cputn(code, 10, color); }
static g_inline void g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px) {
  fb->_[row * fb->pitch / 4 + col] = px; }

// add a scale parameter to this
static void g_fb32_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint8_t *bmp, uint32_t fg, uint32_t bg) {
  for (int r = 0; r < 8; r++) {
    uint8_t o = bmp[r];
    for (int c = 0; c < 8; c++, o <<= 1) {
      fb->_[(row + r) * fb->pitch / 4 + col + c] = o & 128 ? fg : bg; } } }

static void g_fb32_cur_msg(g_fb32 *fb, const char *msg, uint32_t fg, uint32_t bg) {
  size_t h = fb->height / 8, w = fb->width / 8;
  for (int c; (c = *msg++);)
    if (c == '\n') fb->cur_x = 0, fb->cur_y = (fb->cur_y + 1) % h;
    else {
      g_fb32_bmp_8x8(fb, fb->cur_y * 8, fb->cur_x * 8, cga_8x8[c], fg, bg);
      K.fb.cur_x += 1;
      if (fb->cur_x >= w)
        fb->cur_x %= w,
        fb->cur_y += 1,
        fb->cur_y %= h; } }
#define px_color cyan
#define reg_color magenta
#define mem_color yellow

static void fbinit(void) {
  // framebuffer init
  if (!fb_req.response || !fb_req.response->framebuffer_count)
    for (;;) kstop(); // we have no way to indicate this so just stop
  struct limine_framebuffer *fb0 = fb_req.response->framebuffers[0];
  K.fb._ = fb0->address;
  K.fb.width = fb0->width;
  K.fb.height = fb0->height;
  K.fb.pitch = fb0->pitch;
  K.fb.cur_x = K.fb.cur_y = 0;
  intptr_t x0 = K.fb.width / 2,
           y0 = K.fb.height / 2,
           rad = MIN(x0, y0);
  for (uint32_t color = 0xff0000, step = color / rad; rad; color -= step, rad--)
    for (intptr_t x = x0 - rad; x <= x0 + rad; x++)
      for (intptr_t y = y0 - rad; y <= y0 + rad; y++) {
        intptr_t dx = x - x0, dy = y - y0;
        if (dx * dx + dy * dy < rad * rad)
          g_fb32_px(&K.fb, y, x, color * (uintptr_t) &K); }
  static const char g_display_name[] = "GwempleOS ";
  for (uint32_t i = 0, lim = LEN(g_display_name), c; i < lim; i++) {
    c = i % 3;
    c = c == 0 ? cyan : c == 1 ? magenta : yellow;
    cputc(g_display_name[i], c); }
  cputn(K.fb.width, 10, px_color);
  cputs("x", cyan);
  cputn(K.fb.height, 10, px_color);
  cputs(" ", cyan); }

static void meminit(void) {
  if (!memmap_req.response || !hhdm_req.response) for (;;) kstop();
  struct limine_memmap_entry **rr = memmap_req.response->entries;
  uintptr_t hhdm = hhdm_req.response->offset,
           n = memmap_req.response->entry_count,
           nrs = 0,
           nbs = 0;
  K.free = K.used = NULL;
  for (uintptr_t i = n; i; i--) {
    struct limine_memmap_entry *r = rr[i];
    if (!r || // null entry
        r->type != 0 || // type != 0 (usable)
        r->length < sizeof(struct mem) || r->base + r->length - 1 >= 1l<<32)
      continue;
    struct mem *m = (struct mem*) (hhdm + r->base);
    nrs += 1;
    nbs += r->length;
    m->len = r->length / sizeof(uintptr_t);
    m->next = K.free;
    K.free = m; }

  cputn(nrs, 10, reg_color), cputs(" ", reg_color);
  if (nbs >= 1<<22)
    cputn(nbs>>20, 10, mem_color), cputs("MiB ", mem_color);
  else if (nbs >= 1<<16)
    cputn(nbs>>10, 10, mem_color), cputs("KiB ", mem_color);
  else
    cputn(nbs, 10, mem_color), cputs("B ", mem_color); }

static g_inline struct mem *after(struct mem *r) {
  return (struct mem*) ((uintptr_t*) r + r->len); }

static void *kmallocw(uintptr_t n) {
  void *p = NULL;
  if (n) {
    struct mem *r = NULL, *t;
    while (K.free && K.free->len < n + 2 * Width(struct mem))
      t = K.free,
      K.free = t->next,
      t->next = r,
      r = t;
    if (K.free)
      K.free->len -= n + Width(struct mem),
      //t = (struct mem*) (K.free->_ + K.free->len),
      t = after(K.free),
      t->len = Width(struct mem) + n,
      t->next = NULL, // not using this yet
      p = t->_;
    while (r)
      t = r,
      r = t->next,
      t->next = K.free,
      K.free = t; }
  return p; }

static void kfree(void *p) {
  if (!p) return;
  struct mem *m = (struct mem*)p - 1, *r = NULL, *t;
  while (K.free && K.free < m)
    t = K.free,
    K.free = t->next,
    t->next = r,
    r = t;
  for (m->next = K.free, K.free = m; r; t = r, r = t->next, t->next = K.free, K.free = t)
    if (K.free->next == after(K.free))
      K.free->len += K.free->next->len,
      K.free->next = K.free->next->next; }

void *malloc(size_t n) { return kmallocw(b2w(n)); }
void free(void *x) { return kfree(x); }

static void kdrawfb(void) {
  K.fb.cur_x = 0;
  K.fb.cur_y = 1;
  uint32_t color = g_ok(K.g) ? white : red;
  cputs("\03 ", red), cputn(K.ticks, 10, color);
  if (color == red) for (cputs(" # g is not ok # code ", red), cputn(g_code_of(K.g), 10, red);; kstop());
  cputs("\n@", white), cputn((uintptr_t) K.g, 16, white);
  cputs("\n@", white), cputn((uintptr_t) K.g->pool, 16, white);
  cputs("\n#", white), cputn(K.g->len, 10, white);
  cputs(".", white), cputn((intptr_t*)K.g + K.g->len - K.g->sp, 10, white);
  cputs(".", white), cputn(K.g->hp - (intptr_t*)K.g, 10, white);
  cputs("    \n", white);

  struct cb *c = gcb(K.g);
  uint32_t
    rows = c->rows, cols = c->cols,
    p_width = cols * font_width,
    p_height = rows * font_height,
    voff = (K.fb.height - p_height) / 2,
    hoff = (K.fb.width - p_width) / 2;
  for (uint8_t i = 0; i < rows; i++)
    for (uint8_t j = 0; j < cols; j++) {
      uint16_t addr = i * cols + j;
      uint8_t g = c->cb[addr];
      uint32_t fg = console_fg, bg = console_bg;
      if (c->rpos <= addr && addr < c->wpos) fg = console_sel;
      if ((c->flag & show_cursor) && c->wpos == addr && K.ticks & 64)
        fg = bg, bg = console_cur;
      g_fb32_bmp_8x8(&K.fb, voff + i*8, hoff + j*8, cga_8x8[g == '\n' ? 0 : g], fg, bg); } }

static g_vm(g_reset) {
  kreset();
  return f; }
static union x bif_reset[] = {{g_reset}};

static void ksetrpos(void) {
  if (g_ok(K.g)) {
    struct cb *c = kcb;
    c->rpos = c->wpos; } }

#define ps1 "(puts\"  ; \")"
static void ginit(void) {
  struct g *f = g_ini();
  K.g = f = g_def(f, "reset", (intptr_t) bif_reset);
  K.g = f = g_vec0(f, g_vt_u8, 1, (uintptr_t) sizeof(struct cb) + NROWS * NCOLS);
  if (g_ok(f)) {
    
    f->u[0] = g_pop1(f);
    struct cb *c = gcb(f);
    c->rows = NROWS, c->cols = NCOLS;
    c->flag |= show_cursor;
    K.g = g_pop(g_evals(f,
      "(: t0(clock 0))"
      "(puts\"\x02 \")"
#include "boot.h"
      "(putn(clock 0)10)"
      "(puts\"\n\")"
      ps1
    ), 1);
    ksetrpos(); } }

struct k K;
static void kreadg(void) {
  uint8_t c = getkey();
  if (c) g_stdout_putc(K.g, c);
  if (c == '\n') {
    uintptr_t w0 = kcb->wpos, n = 0;
    while (g_ok(K.g = g_read1(K.g)) && kcb->rpos < w0) {
      K.g = g_eval(K.g);
      K.g = g_write1(K.g);
      n += 1;
      g_stdout_putc(K.g, '\n'); }
    if (g_code_of(K.g) == g_status_eof) K.g = g_core_of(K.g);
    K.g = g_pop(g_evals(K.g, ps1), 1 + n);
    ksetrpos(); } }

void kmain(void) { for (fbinit(), meminit(), kinit(), ginit();; kdrawfb(), kreadg(), kstop()); }
