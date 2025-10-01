#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
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

void k_ini(void) {
  g_core *f = g_ini_m_n(g_static_kmalloc, g_static_kfree, g_static_size);
  f = g_evals_(f, boot);
  if (!limine_ok() || !g_ok(f)) goto k_fin;
  k_fb_ini();

  // Note: we assume the framebuffer model is RGB with 32-bit pixels.
  for (size_t i = 0; i < 100; i++)
    g_fb32_px(&k_fb, i, 200 + i, 0x78349aed);
  size_t y0 = k_fb.height / 4, x0 = k_fb.width / 4;
  uint32_t colors[] = {0x12e4c932, 0xcd8237fa, 0x5d8e412a, 0x48d03aa2};
  for (int i = 4, j = 128; i > 0; i--, j /= 2)
    for (size_t x = x0 - j / 2; x < x0 + j / 2; x++)
      for (size_t y = y0 - j / 2; y < y0 + j / 2; y++)
        g_fb32_px(&k_fb, x, y, colors[i-1]);

  g_fb32_px_msg(&k_fb, 16, 16, 0x2aeb8f1e, "hello world how are you this beautiful tuesday evening");

  idt_ini();
  asm ("lidt %0" : :"m"(idtr));
//  asm ("int $0");

  k_log("\nhello world again...\n\nmemmap:");
  for (size_t i = 0; i < memmap_req.response->entry_count; i++)
    k_log("\n "), k_log_n(memmap_req.response->entries[i]->type, 10),
    k_log(" 0x"), k_log_n(memmap_req.response->entries[i]->base, 16),
    k_log(" 0x"), k_log_n(memmap_req.response->entries[i]->length, 16);

  k_log("\n\npre gc");
  k_dbg(f);
  f = please(f, 1);
  k_log("\n\npost gc");
  k_dbg(f); 
  g_fin(f);
k_fin:
  for (;;) asm (
#if defined (__x86_64__)
"hlt"
#elif defined (__aarch64__) || defined (__riscv)
"wfi"
#elif defined (__loongarch64)
"idle 0"
#endif
); }
