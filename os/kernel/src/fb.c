#include "k.h"
#include "font.h"

g_fb32 k_fb;

void g_fb32_px(g_fb32 *fb, size_t row, size_t col, uint32_t px) {
  fb->_[row * fb->pitch / 4 + col] = px; }

void g_fb32_px_bmp_8x8(g_fb32 *fb, size_t row, size_t col, uint32_t px, uint8_t *bmp) {
  for (int r = 0; r < 8; r++) {
    uint8_t o = bmp[r];
    for (int c = 0; c < 8; c++, o <<= 1) {
      fb->_[(row + r) * fb->pitch / 4 + col + c] = o & 128 ? px : 0; } } }

void g_fb32_px_msg(g_fb32 *fb, size_t row, size_t col, uint32_t px, const char *msg) {
  size_t h = fb->height, w = fb->width;
  for (; *msg; msg++) {
    int c = *msg;
    g_fb32_px_bmp_8x8(&k_fb, row, col, px, cga_8x8[c]);
    col += 8;
    if (col >= w)
      col %= w,
      row += 8,
      row %= h; } }

void g_fb32_cur_px_msg(g_fb32 *fb, uint32_t px, const char *msg) {
  size_t h = fb->height / 8, w = fb->width / 8;
  for (; *msg; msg++) {
    int c = *msg;
    if (c == '\n') fb->cur_x = 0, fb->cur_y = fb->cur_y + 1 % h;
    else {
      g_fb32_px_bmp_8x8(fb, fb->cur_y * 8, fb->cur_x * 8, px, cga_8x8[c]);
      fb->cur_x += 1;
      if (fb->cur_x >= w)
        fb->cur_x %= w,
        fb->cur_y += 1,
        fb->cur_y %= h; } } }

void k_log(const char *msg) {
  g_fb32_cur_px_msg(&k_fb, 0xffeeddcc, msg); }

static void k_log_n_r(uintptr_t n, uintptr_t base) { if (n) k_log_n(n, base); }
void k_log_n(uintptr_t n, uintptr_t base) {
  uintptr_t dig = n % base;
  k_log_n_r(n / base, base);
  const char d[2] = {digits[dig], 0};
  k_log(d); }

void g_fb32_set_cursor(g_fb32 *fb, size_t x, size_t y) {
  fb->cur_x = x % fb->width;
  fb->cur_y = y % fb->height; }

void g_fb32_ini(g_fb32 *b) {
  struct limine_framebuffer *fb = framebuffer_request.response->framebuffers[0];
  b->_ = fb->address;
  b->width = fb->width;
  b->height = fb->height;
  b->pitch = fb->pitch;
  b->cur_x = b->cur_y = 0; }

void  k_dbg(g_core *f) {
  k_log("\nf@0x"), k_log_n((uintptr_t) f, 16);
  if (f && g_ok(f)) {
    k_log("\n  ip=0x"), k_log_n((uintptr_t) f->ip, 16);
    k_log("\n  len="), k_log_n(f->len, 10);
    k_log("\n  allocd="), k_log_n(f->hp - f->end, 10);
    k_log("\n  stackd="), k_log_n((g_word*) f + f->len - f->sp, 10); } }

void g_fb32_test_pattern(g_fb32 *fb) {
  // Note: we assume the framebuffer model is RGB with 32-bit pixels.
  for (size_t i = 0; i < 100; i++)
    g_fb32_px(fb, i, 200 + i, 0x78349aed);
  size_t y0 = k_fb.height / 4, x0 = k_fb.width / 4;
  uint32_t colors[] = {0x12e4c932, 0xcd8237fa, 0x5d8e412a, 0x48d03aa2};
  for (int i = 4, j = 128; i > 0; i--, j /= 2)
    for (size_t x = x0 - j / 2; x < x0 + j / 2; x++)
      for (size_t y = y0 - j / 2; y < y0 + j / 2; y++)
        g_fb32_px(fb, x, y, colors[i-1]); }
