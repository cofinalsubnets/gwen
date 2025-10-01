#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <limine.h>
#include "k.h"
static const char boot[] =
#include "boot.h"
;

#define g_static_size  (1<<23)
static g_word g_static_pool[2][g_static_size];
static void *g_static_kmalloc(g_core *f, size_t n) {
  return f == NULL ? g_static_pool : NULL; }
static void g_static_kfree(g_core *f, void *_) {}

// frame buffer

void kmain(void) {
  g_core *f = g_ini_m_n(g_static_kmalloc, g_static_kfree, g_static_size);
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
      for (size_t x = x0 - j / 2; x < x0 + j / 2; x++)
        for (size_t y = y0 - j / 2; y < y0 + j / 2; y++)
          g_fb32_px(&k_fb, x, y, colors[i-1]);

    g_fb32_px_msg(&k_fb, 16, 16, 0x2aeb8f1e, "hello world how are you this beautiful tuesday evening");

    int row = 32, col = 16;
    size_t h = k_fb.height, w = k_fb.width;
    if (memmap_req.response) {
      for (int i = 0; i < memmap_req.response->entry_count; i++, row += 8, row %= h, col = 16)
        for (size_t len = memmap_req.response->entries[i]->length; len; len >>= 1) {
          g_fb32_px_msg(&k_fb, row, col, 0xffffffff, "#");
          col += 8;
          if (col >= w)
            col %= w,
            row += 8,
            row %= h; } }
 
    k_log("\nhello world again");
    k_log("...\n");
    k_log("\n....... \n");
    k_log_n(57, 10);
    k_log("\n0x"), k_log_n(0xbeab, 16);
    k_log("\n0o"), k_log_n(0xbeab, 8);
    k_log("\n0b"), k_log_n(0xbeab, 2);
    f = please(f, 1);
    k_log("\n\nf@"), k_log_n((uintptr_t) f, 16);
    k_log("\n  len="), k_log_n(f->len, 10);
    k_log("\n  allocd="), k_log_n(f->hp - f->end, 10);
    k_log("\n  stackd="), k_log_n((g_word*) f + f->len - f->sp, 10);
  }

  g_fin(f);
#if defined (__x86_64__)
#define STOP "hlt"
#elif defined (__aarch64__) || defined (__riscv)
#define STOP "wfi"
#elif defined (__loongarch64)
#define STOP "idle 0"
#endif
  for (;;) asm (STOP); }
