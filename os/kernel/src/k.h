#ifndef _g_k_h
#define _g_k_h
#include <limine.h>
#include "i.h"
#include "libc_internal.h"

extern volatile struct limine_framebuffer_request framebuffer_request;
extern volatile struct limine_memmap_request memmap_req;
extern volatile struct limine_stack_size_request stack_req;
extern uint64_t limine_base_revision[];

typedef struct g_fb32 {
  size_t width, height, pitch, cur_x, cur_y;
  volatile uint32_t *_; } g_fb32;

extern g_fb32 k_fb;
void
  g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px),
  g_fb32_px_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint32_t px, uint8_t *bmp),
  g_fb32_px_msg(g_fb32 *fb, size_t row, size_t col, uint32_t px, const char *msg),
  g_fb32_cur_px_msg(g_fb32 *fb, uint32_t px, const char *msg),
  k_log(const char*),
  k_log_n(uintptr_t n, uintptr_t base),
  k_set_cursor(size_t, size_t)
  ;
#endif
