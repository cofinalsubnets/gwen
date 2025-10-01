#include "i.h"
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <limine.h>

__attribute__((used, section(".limine_requests_start")))
static volatile LIMINE_REQUESTS_START_MARKER;

__attribute__((used, section(".limine_requests")))
static volatile LIMINE_BASE_REVISION(3);

__attribute__((used, section(".limine_requests")))
static volatile struct limine_framebuffer_request framebuffer_request = {
    .id = LIMINE_FRAMEBUFFER_REQUEST,
    .revision = 0 };

__attribute__((used, section(".limine_requests")))
static volatile struct limine_memmap_request memmap_req = {
  .id = LIMINE_MEMMAP_REQUEST,
  .revision = 0 };

__attribute__((used, section(".limine_requests")))
static volatile struct limine_stack_size_request stack_req = {
  .id = LIMINE_STACK_SIZE_REQUEST,
  .revision = 0,
  .stack_size = 1<<23, };

__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;

static const char boot[] =
#include "boot.h"
;
#include "font.h"

#define g_static_n  (1<<20)
static g_word g_static_pool[2][g_static_n];

static void *g_static_kmalloc(g_core *f, size_t n) {
  return n == g_static_n ? g_static_pool : NULL; }
static void g_static_kfree(g_core *f, void *_) {}

typedef struct g_fb32 {
  size_t width, height, pitch, cur_x, cur_y;
  volatile uint32_t *_; } g_fb32;

static g_fb32 k_fb;



// frame buffer
static volatile uint32_t *g_fb;
static size_t g_fb_width, g_fb_height, g_fb_pitch;

static void g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px) {
  fb->_[row * fb->pitch / 4 + col] = px; }

static void g_fb32_px_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint32_t px, uint8_t *bmp) {
  for (int r = 0; r < 8; r++) {
    uint8_t o = bmp[r];
    for (int c = 0; c < 8; c++, o <<= 1) {
      fb->_[(row + r) * fb->pitch / 4 + col + c] = o & 128 ? px : 0; } } }

static void g_fb32_px_msg(g_fb32 *fb, size_t row, size_t col, uint32_t px, const char *msg) {
  size_t h = fb->height, w = fb->width;
  for (; *msg; msg++) {
    g_fb32_px_bmp_8x8(&k_fb, row, col, 0x2aeb8f1e, cga_8x8[*msg]);
    col += 8;
    if (col >= w)
      col %= w,
      row += 8,
      row %= h; } }

// The following will be our kernel's entry point.
// If renaming kmain() to something else, make sure to change the
// linker script accordingly.
void kmain(void) {
  g_core *f = g_ini_m(g_static_kmalloc, g_static_kfree);
  f = g_evals(f, boot);
  //
  // Ensure the bootloader actually understands our base revision (see spec).
  if (LIMINE_BASE_REVISION_SUPPORTED &&
      framebuffer_request.response   &&
      framebuffer_request.response->framebuffer_count) {
    struct limine_framebuffer *fb = framebuffer_request.response->framebuffers[0];
    k_fb._ = fb->address;
    k_fb.width = fb->width;
    k_fb.height = fb->height;
    k_fb.pitch = fb->pitch;
    k_fb.cur_x = k_fb.cur_y = 0;

    // Note: we assume the framebuffer model is RGB with 32-bit pixels.
    for (size_t i = 0; i < 100; i++)
      g_fb32_px(&k_fb, i, 200 + i, 0x78349aed);

    size_t y0 = k_fb.height / 8,
           x0 = k_fb.width / 4;
    uint32_t colors[] = {0x12e4c932, 0xcd8237fa, 0x5d8e412a, 0x48d03aa2};
    for (int i = 4, j = 128; i > 0; i--, j /= 2)
      for (int x = x0 - j / 2; x < x0 + j / 2; x++)
        for (int y = y0 - j / 2; y < y0 + j / 2; y++)
          g_fb32_px(&k_fb, x, y, colors[i-1]);

    g_fb32_px_msg(&k_fb, 16, 16, 0x2aeb8f1e, "hello world how are you this beautiful tuesday evening");

    int row = 32, col = 16;

    size_t h = k_fb.height, w = k_fb.width;
    if (memmap_req.response) {
      for (int i = 0; i < memmap_req.response->entry_count; i++, row += 8, row %= h, col = 16)
        for (size_t len = memmap_req.response->entries[i]->length; len; len >>= 1) {
          g_fb32_px_bmp_8x8(&k_fb, row, col, 0xffffffff, cga_8x8['#']);
          col += 8;
          if (col >= w)
            col %= w,
            row += 8,
            row %= h; } } }

  g_fin(f);
#if defined (__x86_64__)
#define STOP "hlt"
#elif defined (__aarch64__) || defined (__riscv)
#define STOP "wfi"
#elif defined (__loongarch64)
#define STOP "idle 0"
#endif
  for (;;) asm (STOP); }
