#include <stdint.h>
#include <stddef.h>

extern volatile struct limine_framebuffer_request framebuffer_request;
typedef struct g_fb32 {
  size_t width, height, pitch, cur_x, cur_y;
  volatile uint32_t *_; } g_fb32;

void
  g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px),
  g_fb32_ini(g_fb32*),
  g_fb32_set_cursor(g_fb32*, size_t, size_t),
  g_fb32_test_pattern(g_fb32*);

// add bg color to these...
void
  g_fb32_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint8_t *bmp, uint32_t fg, uint32_t bg),
  g_fb32_msg(g_fb32 *fb, size_t row, size_t col, const char *msg, uint32_t fg, uint32_t bg),
  g_fb32_cur_msg(g_fb32 *fb, uint32_t px, const char *msg);

extern g_fb32 k_fb;
