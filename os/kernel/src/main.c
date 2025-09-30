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

__attribute__((used, section(".limine_requests_end")))
static volatile LIMINE_REQUESTS_END_MARKER;

static const char boot[] =
#include "boot.h"
;


#define g_static_n  (1<<20)
static g_word g_static_pool[2][g_static_n];

static void *g_static_kmalloc(g_core *f, size_t n) {
  return n == g_static_n ? g_static_pool : NULL; }
static void g_static_kfree(g_core *f, void *_) {}

// The following will be our kernel's entry point.
// If renaming kmain() to something else, make sure to change the
// linker script accordingly.
void kmain(void) {
  // Ensure the bootloader actually understands our base revision (see spec).
  if (LIMINE_BASE_REVISION_SUPPORTED &&
      framebuffer_request.response   &&
      framebuffer_request.response->framebuffer_count) {

    g_core *f = g_ini_m(g_static_kmalloc, g_static_kfree);
  //  f = g_evals(f, boot);
    struct limine_framebuffer *fb = framebuffer_request.response->framebuffers[0];

    // Note: we assume the framebuffer model is RGB with 32-bit pixels.
    for (size_t i = 0; i < 100; i++) {
      volatile uint32_t *fb_ptr = fb->address;
      fb_ptr[i * (fb->pitch / 4) + i] = 0xffffff; } }

#if defined (__x86_64__)
#define STOP "hlt"
#elif defined (__aarch64__) || defined (__riscv)
#define STOP "wfi"
#elif defined (__loongarch64)
#define STOP "idle 0"
#endif
  for (;;) asm (STOP); }
