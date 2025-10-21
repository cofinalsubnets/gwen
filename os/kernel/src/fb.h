#ifndef _g_k_fb_h
#define _g_k_fb_h
#include <stdint.h>
#include <stddef.h>
#include <limine.h>

typedef struct g_fb32 {
  size_t width, height, pitch, cur_x, cur_y;
  volatile uint32_t *_; } g_fb32;

typedef struct g_cb32 {
  uintptr_t rows, cols, cx, cy, px, py;
  uint8_t *cb;
  g_fb32 fb; } g_cb32;

void
  g_fb32_log(g_fb32*, const char*),
  g_fb32_log_c(g_fb32*, const char *msg, uint32_t, uint32_t),
  g_fb32_log_char(g_fb32*, char),
  g_fb32_log_char_c(g_fb32*, char, uint32_t, uint32_t),
  g_fb32_log_n(g_fb32*, uintptr_t, uintptr_t),
  g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px),
  g_fb32_ini(g_fb32*, uint32_t *_, size_t width, size_t height, size_t pitch),
  g_fb32_set_cursor(g_fb32*, size_t, size_t),
  g_fb32_test_pattern(g_fb32*),
  g_fb32_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint8_t *bmp, uint32_t fg, uint32_t bg),
  g_fb32_msg(g_fb32 *fb, size_t row, size_t col, const char *msg, uint32_t fg, uint32_t bg),
  g_fb32_cur_msg(g_fb32 *fb, const char *msg, uint32_t fg, uint32_t bg);

extern volatile struct limine_framebuffer_request framebuffer_request;
#endif
