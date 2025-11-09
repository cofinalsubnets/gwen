#include "limine/limine.h"
#include "g.h"
#include "font.h"
#include <stdarg.h>
#include <limits.h>

struct k {
  uint64_t ticks;
  struct mem {
    uintptr_t len;
    struct mem *next;
    uintptr_t _[];
  } *free;
  struct g *g;
  struct cb *cb;
  struct g_fb32 {
    volatile uint32_t *_;
    uint16_t width, height, pitch;
  } *fb;
  struct { uint8_t k, f; } kb;
} K;
void kreset(void), kinit(void);

static g_inline void kwait(void) { asm volatile (
#if defined (__x86_64__)
  "hlt"
#elif defined (__aarch64__) || defined (__riscv)
  "wfi"
#elif defined (__loongarch64)
  "idle 0"
#endif
  ); }

void k_logf(const char*, ...), k_vlogf(const char*, va_list), k_log_char(char);
#include "cb.h"
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
void g_stdout_putc(struct g*f, int c) { cb_putc(K.cb, c); }
int g_stdin_getc(struct g*f) { return cb_getc(K.cb); }
int g_stdin_ungetc(struct g*f, int c) { return cb_ungetc(K.cb, c); }
int g_stdin_eof(struct g*f) { return cb_eof(K.cb); }
uintptr_t g_clock(void) { return K.ticks; }

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
#define font_x 8
#define font_y 8

static void key_ext(uint8_t c) {
  struct cb *cb = K.cb;
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
  if (code == kb_code_extend) return (void) (K.kb.f |= kb_flag_extend);
  if (K.kb.f & kb_flag_extend) {
    K.kb.f &= ~kb_flag_extend;
    if (code < 128) switch (code) {
      case kb_code_alt: K.kb.f |= kb_flag_ralt; return;
      case kb_code_ctl: K.kb.f |= kb_flag_rctl; return;
      case kb_code_delete:
        if (K.kb.f & kb_flag_ctl & kb_flag_alt) kreset();
        return;
      default: return key_ext(code); }
    else switch (code - 128) {
      case kb_code_alt: K.kb.f &= ~kb_flag_ralt; return;
      case kb_code_ctl: K.kb.f &= ~kb_flag_rctl; return;
      default: return; } }
  else {
    if (code < 128) switch (code) {
      case kb_code_lshift: K.kb.f |= kb_flag_lshift; return;
      case kb_code_rshift: K.kb.f |= kb_flag_rshift; return;
      case kb_code_alt:    K.kb.f |= kb_flag_lalt;   return;
      case kb_code_ctl:    K.kb.f |= kb_flag_lctl;   return;
      default: return key_put(code); }
    else switch (code - 128) {
      case kb_code_lshift: K.kb.f &= ~kb_flag_lshift; return;
      case kb_code_rshift: K.kb.f &= ~kb_flag_rshift; return;
      case kb_code_alt:    K.kb.f &= ~kb_flag_lalt;   return;
      case kb_code_ctl:    K.kb.f &= ~kb_flag_lctl;   return;
      default: return; } } }

#define px_color cyan
#define reg_color magenta
#define mem_color yellow

static void fbinit(void) {
  K.fb = NULL;
  if (fb_req.response &&
      fb_req.response->framebuffer_count &&
      (K.fb = malloc(sizeof(struct g_fb32)))) {
    struct limine_framebuffer *lfb = fb_req.response->framebuffers[0];
    fb->_ = lfb->address;
    fb->width = lfb->width;
    fb->height = lfb->height;
    fb->pitch = lfb->pitch; } }

static void meminit(void) {
  K.free = NULL;
  if (memmap_req.response && hhdm_req.response) {
    struct limine_memmap_entry **rr = memmap_req.response->entries;
    uintptr_t hhdm = hhdm_req.response->offset,
             n = memmap_req.response->entry_count;
    for (uintptr_t i = n; i; i--) {
      struct limine_memmap_entry *r = rr[i];
      if (!r || r->type != 0) continue;
      struct mem *m = (struct mem*) (hhdm + r->base);
      m->len = r->length / sizeof(uintptr_t);
      m->next = K.free;
      K.free = m; } } }

static g_inline struct mem *after(struct mem *r) {
  return (struct mem*) ((uintptr_t*) r + r->len); }

static void *kmallocw(uintptr_t n) {
  if (!n) return NULL;
  void *p = NULL;
  struct mem *r = NULL, *t;
  while (K.free && K.free->len < n + 2 * Width(struct mem))
    t = K.free,
    K.free = t->next,
    t->next = r,
    r = t;
  if (K.free)
    K.free->len -= n + Width(struct mem),
    t = after(K.free),
    t->len = Width(struct mem) + n,
    p = t->_;
  while (r)
    t = r,
    r = t->next,
    t->next = K.free,
    K.free = t;
  return p; }

static void kfree(void *p) {
  if (!p) return;
  struct mem *m = (struct mem*)p - 1, *r = NULL, *t;
  while (K.free && K.free < m)
    t = K.free,
    K.free = t->next,
    t->next = r,
    r = t;
  for (;; m = r, r = r->next) {
    if (K.free != after(m)) m->next = K.free;
    else m->len += K.free->len,
         m->next = K.free->next;
    K.free = m;
    if (!r) return; } }

void *malloc(size_t n) { return kmallocw(b2w(n)); }
void free(void *x) { return kfree(x); }

static void kdraw(void) {
  struct cb *c = K.cb;
  uint8_t rows = c->rows, cols = c->cols;
  for (uint8_t i = 0; i < rows; i++)
    for (uint8_t j = 0; j < cols; j++) {
      uint16_t pos = i * cols + j;
      uint8_t g = c->cb[pos], *bmp = cga_8x8[g == '\n' ? 0 : g];
      bool select = c->rpos <= pos && pos < c->wpos,
           invert = (c->flag & show_cursor) && c->wpos == pos && K.ticks & 64;
      uint32_t fg = select ? console_sel : console_fg, bg = console_bg;
      if (invert) fg ^= bg, bg ^= fg, fg ^= bg;
      size_t y = i * font_y, x = j * font_x;
      for (uint8_t r = 0; r < font_y; r++)
        for (uint8_t o = bmp[r], c = font_x; c--;)
          K.fb->_[(y + r) * K.fb->pitch / 4 + (x + c)] = o & 128 >> c ? fg : bg; } }

static void cbinit(void) {
  const uintptr_t rows = K.fb->height / font_y,
                  cols = K.fb->width / font_x;
  struct cb *cb = malloc(sizeof(struct cb) + rows * cols);
  if (!cb) kreset();
  cb->rows = rows, cb->cols = cols;
  cb->rpos = cb->wpos = 0;
  cb->flag = show_cursor;
  memset(cb->cb, 0, rows * cols);
  K.cb = cb; }

static g_vm(g_reset) { return kreset(), f; }
static union x bif_reset[] = {{g_reset}};

static g_inline void k_eval(const char*s) {
  if (g_ok(K.g = g_evals_(K.g, s))) K.cb->rpos = K.cb->wpos; }

#define prompt "  ; "
static void ginit(void) {
  K.g = g_def(g_ini(), "reset", (intptr_t) bif_reset);
  k_eval(
    "(puts\"\x02 \")"
#include "boot.h"
    "(:(kreads _)(:(rr x)(: r(read x)(? r(X(A r)(rr x))))(each(rr 0)(\\ r(,(.(ev'ev r))(putc 10))))))"
    "(putn(clock 0)10)(puts\"\n"prompt"\")"); }

static void kread(void) {
  uint8_t c = K.kb.k;
  K.kb.k = 0;
  if (c) g_stdout_putc(K.g, c);
  if (c == '\n') k_eval("(kreads 0)(puts\""prompt"\")"); }

void kmain(void) {
  kinit();
  meminit();
  fbinit();
  cbinit();
  ginit();
  for (;;)
    kdraw(),
    kread(),
    kwait(); }
