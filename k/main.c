#include "k.h"
#include "font.h"
#include <stdarg.h>

void g_printf(struct g_out*, const char *fmt, ...) {
  va_list xs;
  va_start(xs, fmt);
  k_vlogf(fmt, xs);
  va_end(xs); }
void g_putc(struct g_out*, int c) {
  k_log_char(c); }

int g_getc(struct g_in*) { return 0; }
int g_ungetc(struct g_in*, int c) { return c; }
int g_eof(struct g_in*) { return 1; }
uintptr_t g_clock(void) { return K.ticks; }

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

void g_fb32_draw_cb(g_fb32 *fb, g_cb *cb) {
}

void g_fb32_set_cursor(g_fb32 *fb, size_t x, size_t y) {
  fb->cur_x = x % fb->width;
  fb->cur_y = y % fb->height; }

void g_fb32_ini(g_fb32 *b, uint32_t *_, size_t width, size_t height, size_t pitch) {
  b->_ = _;
  b->width = width;
  b->height = height;
  b->pitch = pitch;
  b->cur_x = b->cur_y = 0; }

static void g_fb32_log_n_r(g_fb32 *fb, uintptr_t n, uintptr_t base) {
  if (n) g_fb32_log_n(fb, n, base); }

void g_fb32_log_n(g_fb32 *fb, uintptr_t n, uintptr_t base) {
  uintptr_t dig = n % base;
  g_fb32_log_n_r(fb, n / base, base);
  const char d[2] = {digits[dig], 0};
  g_fb32_log(fb, d); }

void g_fb32_log(g_fb32 *fb, const char *msg) {
  g_fb32_log_c(fb, msg, 0xffeeddcc, 0); }
void g_fb32_log_char_c(g_fb32 *fb, char c, uint32_t fg, uint32_t bg) {
  char s[2] = {c, 0};
  g_fb32_log_c(fb, s, fg, bg); }
void g_fb32_log_char(g_fb32 *fb, char c) {
  g_fb32_log_char_c(fb, c, 0xffeeddcc, 0); }
void 
  k_log(const char *msg),
  k_log_c(const char *msg, uint32_t fg, uint32_t bg),
  k_log_n(uintptr_t n, uintptr_t base),
  k_logf(const char *fmt, ...),
  k_log_char(char);

void k_log_c(const char *msg, uint32_t fg, uint32_t bg) {
  k_cur_msg(msg, fg, bg); }

void k_log_char(char c) {
  char s[2] = {c, 0};
  k_log(s); }

void k_log(const char *msg) {
  k_log_c(msg, 0xffeeddcc, 0); }

static void k_log_n_r(uintptr_t n, uintptr_t base) {
  if (n) k_log_n(n, base); }

void k_log_n(uintptr_t n, uintptr_t base) {
  uintptr_t dig = n % base;
  k_log_n_r(n / base, base);
  const char d[2] = {digits[dig], 0};
  k_log(d); }


void k_logf(const char *fmt, ...) {
  va_list xs;
  va_start(xs, fmt);
  k_vlogf(fmt, xs);
  va_end(xs); }

void k_vlogf(const char *fmt, va_list xs) {
  while (*fmt) {
    char c = *fmt++;
    if (c != '%') k_log_char(c);
    else re:
      switch (c = *fmt++) {
      case 0: return;
      default: k_log_char(c); break;
      case 'l': goto re;
      case 'd': k_log_n(va_arg(xs, uintptr_t), 10); break;
      case 'x': k_log_n(va_arg(xs, uintptr_t), 16); break;
      case 'o': k_log_n(va_arg(xs, uintptr_t), 8); break;
      case 'b': k_log_n(va_arg(xs, uintptr_t), 2); break; } } }

#define g_static_size (1<<20)

__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;
#define _L __attribute__((used, section(".limine_requests"))) static volatile
_L LIMINE_BASE_REVISION(3);
_L struct limine_memmap_request memmap_req = { .id = LIMINE_MEMMAP_REQUEST, .revision = 0 };
_L struct limine_hhdm_request hhdm_req = { .id = LIMINE_HHDM_REQUEST, .revision = 0 };
_L struct limine_framebuffer_request fb_req = { .id = LIMINE_FRAMEBUFFER_REQUEST, .revision = 0 };
__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;


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


struct k K;
void kmain(void) {
  arch_init(); // arch specific init

  // framebuffer init
  if (!fb_req.response || !fb_req.response->framebuffer_count) for (;;) k_stop();
  struct limine_framebuffer *fb0 = fb_req.response->framebuffers[0];
  g_fb32_ini(&K.fb, fb0->address, fb0->width, fb0->height, fb0->pitch);
  intptr_t x0 = K.fb.width / 2,
           y0 = K.fb.height / 2,
           rad = MIN(x0, y0);
  for (uint32_t color = 0xff0000, step = color / rad; rad; color -= step, rad--)
    for (intptr_t x = x0 - rad; x <= x0 + rad; x++)
      for (intptr_t y = y0 - rad; y <= y0 + rad; y++) {
        intptr_t dx = x - x0, dy = y - y0;
        if (dx * dx + dy * dy < rad * rad)
          g_fb32_px(&K.fb, y, x, color * (uintptr_t) &K); }
  k_logf("%dx%dp %dx%dc\n", K.fb.width, K.fb.height, K.fb.width >> 3, K.fb.height >> 3);

  // mem init
  if (!memmap_req.response || !hhdm_req.response) for (;;) k_stop();
  struct limine_memmap_entry **rr = memmap_req.response->entries;
  uint64_t hhdm = hhdm_req.response->offset,
           n = memmap_req.response->entry_count,
           nrs = 0,
           nbs = 0;
  K.free = kgetmem(hhdm, n, rr, &nbs, &nrs);
  K.used = NULL;
  k_logf("%dK %dR\n", nbs >> 10, nrs);

  // lisp init
  static intptr_t g_static_pool[g_static_size];
  struct g *f = K.f = g_gc(g_pop(g_evals(g_ini_static(sizeof(g_static_pool), g_static_pool),
#include "boot.h"
  ), 1));
  k_logf("f@0x%x\n", (uintptr_t) f);
  if (f && g_ok(f)) k_logf(
    " pool=0x%x\n len=%d\n ip=0x%x\n allocd=%d\n stackd=%d\n",
    f->pool, f->len, f->ip, (uintptr_t) (f->hp - f->end), (intptr_t*) f + f->len - f->sp);

  // main loop
  for (k_logf("%d ticks ok.\n", K.ticks);; k_stop())
    k_log_char(key_get()); }
