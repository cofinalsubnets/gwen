#include "fb.h"
#include "font.h"
#include "k.h"
#include "limine.h"

void g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px) {
  fb->_[row * fb->pitch / 4 + col] = px; }

// add a scale parameter to this
void g_fb32_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint8_t *bmp, uint32_t fg, uint32_t bg) {
  for (int r = 0; r < 8; r++) {
    uint8_t o = bmp[r];
    for (int c = 0; c < 8; c++, o <<= 1) {
      fb->_[(row + r) * fb->pitch / 4 + col + c] = o & 128 ? fg : bg; } } }

void g_fb32_char(g_fb32 *fb, size_t row, size_t col, uint8_t c, uint32_t fg, uint32_t bg) {
  g_fb32_bmp_8x8(fb, row, col, cga_8x8[c], fg, bg); }

void g_fb32_msg(g_fb32 *fb, size_t row, size_t col, const char *msg, uint32_t fg, uint32_t bg) {
  size_t h = fb->height, w = fb->width;
  for (; *msg; msg++) {
    int c = *msg;
    g_fb32_char(fb, row, col, c, fg, bg);
    col += 8;
    if (col >= w)
      col %= w,
      row += 8,
      row %= h; } }

void k_cur_msg(const char *msg, uint32_t fg, uint32_t bg) {
  size_t h = K.fb.height / 8, w = K.fb.width / 8;
  for (; *msg; msg++) {
    int c = *msg;
    if (c == '\n') K.fb.cur_x = 0, K.fb.cur_y = (K.fb.cur_y + 1) % h;
    else {
      g_fb32_bmp_8x8(&K.fb, K.fb.cur_y * 8, K.fb.cur_x * 8, cga_8x8[c], fg, bg);
      K.fb.cur_x += 1;
      if (K.fb.cur_x >= w)
        K.fb.cur_x %= w,
        K.fb.cur_y += 1,
        K.fb.cur_y %= h; } } }

void g_fb32_draw_cb(g_fb32 *fb, g_cb *cb) {
}

void g_fb32_set_cursor(g_fb32 *fb, size_t x, size_t y) {
  fb->cur_x = x % fb->width;
  fb->cur_y = y % fb->height; }

void g_fb32_ini(g_fb32 *b, uint32_t *_, size_t width, size_t height, size_t pitch) {
  b->_ = _;
  b->width = width;
  b->height = height;
  b->pitch = pitch;
  b->cur_x = b->cur_y = 0; }

static void g_fb32_log_n_r(g_fb32 *fb, uintptr_t n, uintptr_t base) {
  if (n) g_fb32_log_n(fb, n, base); }

void g_fb32_log_n(g_fb32 *fb, uintptr_t n, uintptr_t base) {
  uintptr_t dig = n % base;
  g_fb32_log_n_r(fb, n / base, base);
  const char d[2] = {digits[dig], 0};
  g_fb32_log(fb, d); }

void g_fb32_log(g_fb32 *fb, const char *msg) {
  g_fb32_log_c(fb, msg, 0xffeeddcc, 0); }
void g_fb32_log_char_c(g_fb32 *fb, char c, uint32_t fg, uint32_t bg) {
  char s[2] = {c, 0};
  g_fb32_log_c(fb, s, fg, bg); }
void g_fb32_log_char(g_fb32 *fb, char c) {
  g_fb32_log_char_c(fb, c, 0xffeeddcc, 0); }

