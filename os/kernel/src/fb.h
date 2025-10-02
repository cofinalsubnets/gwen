#ifndef _g_k_fb_h
#define _g_k_fb_h

typedef struct g_fb32 {
  size_t width, height, pitch, cur_x, cur_y;
  volatile uint32_t *_; } g_fb32;

void
  g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px),
  g_fb32_px_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint32_t px, uint8_t *bmp),
  g_fb32_px_msg(g_fb32 *fb, size_t row, size_t col, uint32_t px, const char *msg),
  g_fb32_cur_px_msg(g_fb32 *fb, uint32_t px, const char *msg),
  g_fb32_ini(g_fb32*),
  g_fb32_set_cursor(g_fb32*, size_t, size_t),
  g_fb32_test_pattern(g_fb32*);

extern g_fb32 k_fb;
#endif
