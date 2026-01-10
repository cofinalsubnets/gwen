#include "limine/limine.h"
#include "g.h"
#include "font.h"
#include <stdarg.h>
#include <limits.h>

uint64_t kticks;

static struct mem {
  struct mem *next;
  uintptr_t len;
  uintptr_t _[];
} *kmem;

static struct cb *kcb;

static struct {
  volatile uint32_t *_;
  uint16_t width, height, pitch; } kfb;

static struct { uint8_t k, f; } kkb;

void k_reset(void);
bool archinit(void);

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
void g_stdout_putc(struct g*f, int c) { cb_putc(kcb, c); }
int gputc(struct g*f, int c) { return cb_putc(kcb, c), c; }
int ggetc(struct g*f) { return cb_getc(kcb); }
int gungetc(struct g*f, int c) { return cb_ungetc(kcb, c); }
int geof(struct g*f) { return cb_eof(kcb); }
uintptr_t g_clock(void) { return kticks; }

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

_Static_assert(LEN(kb2ascii) == LEN(shift_kb2ascii));

#define kb_code_left 75
#define kb_code_right 77
#define kb_code_up 72
#define kb_code_down 80
void kb_int(const uint8_t code) {
  if (code == kb_code_extend) kkb.f |= kb_flag_extend;
  else if (kkb.f & kb_flag_extend) {
    kkb.f &= ~kb_flag_extend;
    uint16_t r = kcb->rpos,
             w = kcb->wpos,
             cs = kcb->cols;
    if (code < 128) switch (code) {
      case kb_code_alt: kkb.f |= kb_flag_ralt; return;
      case kb_code_ctl: kkb.f |= kb_flag_rctl; return;
      case kb_code_delete:
        if (kkb.f & kb_flag_ctl & kb_flag_alt) k_reset();
        return;
      case kb_code_left: w -= 1; goto move_cursor;
      case kb_code_right: w += 1; goto move_cursor;
      case kb_code_up: w -= cs; goto move_cursor;
      case kb_code_down: w += cs; 
      move_cursor:
        w %= kcb->rows * cs;
        w = MAX(w, r);
        kcb->wpos = w;
      default: return; }
    else switch (code - 128) {
      case kb_code_alt: kkb.f &= ~kb_flag_ralt; return;
      case kb_code_ctl: kkb.f &= ~kb_flag_rctl; return;
      default: return; } }
  else {
    if (code < 128) switch (code) {
      case kb_code_lshift: kkb.f |= kb_flag_lshift; return;
      case kb_code_rshift: kkb.f |= kb_flag_rshift; return;
      case kb_code_alt:    kkb.f |= kb_flag_lalt;   return;
      case kb_code_ctl:    kkb.f |= kb_flag_lctl;   return;
      default:
        if (!kkb.k) kkb.k = code >= LEN(kb2ascii) ? code :
          (kkb.f & (kb_flag_lshift | kb_flag_rshift) ? shift_kb2ascii : kb2ascii)[code];
        return; }
    else switch (code - 128) {
      case kb_code_lshift: kkb.f &= ~kb_flag_lshift; return;
      case kb_code_rshift: kkb.f &= ~kb_flag_rshift; return;
      case kb_code_alt:    kkb.f &= ~kb_flag_lalt;   return;
      case kb_code_ctl:    kkb.f &= ~kb_flag_lctl;   return;
      default: return; } } }

#define px_color cyan
#define reg_color magenta
#define mem_color yellow

static g_inline struct mem *after(struct mem *r) {
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

static g_vm(g_kreset) { return k_reset(), f; }

#define font_x 8
#define font_y 8
static g_vm(draw) {
  // draw framebuffer
  for (uint8_t i = 0, rows = kcb->rows; i < rows; i++)
    for (uint8_t j = 0, cols = kcb->cols; j < cols; j++) {
      uint16_t pos = i * cols + j;
      uint8_t g = kcb->cb[pos], *bmp = cga_8x8[g == '\n' ? 0 : g];
      bool select = kcb->rpos <= pos && pos < kcb->wpos,
           invert = kcb->flag & show_cursor && kcb->wpos == pos && kticks & 64;
      uint32_t fg = select ? console_sel : console_fg, bg = console_bg;
      if (invert) fg ^= bg, bg ^= fg, fg ^= bg;
      uintptr_t y = i * font_y, x = j * font_x;
      for (uint8_t r = 0; r < font_y; r++)
        for (uint8_t o = bmp[r], c = font_x; c--; o >>= 1)
          kfb._[(y + r) * kfb.pitch + x + c] = o & 1 ? fg : bg; }
  Ip += 1;
  return Continue(); }

static g_vm(wait) {
  kwait();
  Ip += 1;
  return Continue(); }

static g_vm(key) {
    Sp[0] = g_putnum(kkb.k);
    kkb.k = 0;
    Ip += 1;
    return Continue(); }

int gflush(struct g*f) {
 kcb->rpos = kcb->wpos;
 return 0; }


static union u
  bif_reset[] = {{g_kreset}},
  bif_draw[] = {{draw}, {g_vm_ret0}},
  bif_key[] = {{key}, {g_vm_ret0}},
  bif_wait[] = {{wait}, {g_vm_ret0}};

static bool meminit(void) {
  if (!memmap_req.response || !hhdm_req.response) return false;
  struct mem *m;
  struct limine_memmap_entry *r, **rr = memmap_req.response->entries;
  uintptr_t hhdm = hhdm_req.response->offset,
            n = memmap_req.response->entry_count;
  while (n--) if ((r = rr[n])->type == 0)
    m = (struct mem*) (hhdm + r->base),
    m->len = r->length / sizeof(uintptr_t),
    m->next = kmem,
    kmem = m;
  return true; }

static bool fbinit(void) {
  if (!fb_req.response || !fb_req.response->framebuffer_count) return false;
  struct limine_framebuffer *f = fb_req.response->framebuffers[0];
  kfb._ = f->address;
  kfb.width = f->width;
  kfb.height = f->height;
  kfb.pitch = f->pitch >> 2;
  return true; }

static bool cbinit(void) {
  const uintptr_t rows = kfb.height / font_y,
                  cols = kfb.width / font_x;
  kcb = malloc(sizeof(struct cb) + rows * cols);
  if (!kcb) return false;
  kcb->rows = rows;
  kcb->cols = cols;
  kcb->rpos = kcb->wpos = 0;
  kcb->flag = show_cursor;
  cb_fill(kcb, 0);
  return true; }

static struct g_def defs[] = {
  {"reset", (intptr_t) bif_reset},
  {"draw", (intptr_t) bif_draw},
  {"key", (intptr_t) bif_key},
  {"wait", (intptr_t) bif_wait},
  {0}, };

void kmain(void) {
  if (archinit() && meminit() && fbinit() && cbinit())
    g_evals_(g_defs(g_ini(), defs),
#include "boot.h"
      "(:(ps1 _)(puts\" ;; \")"
        "(rs x)(? x(X(A x)(rs(read 0))))"
        "(ep x)(,(.(ev x))(putc 10))"
        "(go k)(go(key(wait(draw(,(? k(putc k))(?(= k 10)(ps1(each(rs(read 0))ep))))))))"
       "(go(key(ps1(,(putn(clock(puts\"\x02 \"))10)(putc 10))))))");
   // */

  k_reset(); }
