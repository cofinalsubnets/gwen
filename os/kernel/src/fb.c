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

void k_set_cursor(size_t x, size_t y) {
  k_fb.cur_x = x % k_fb.width;
  k_fb.cur_y = y % k_fb.height; }

void k_fb_ini(void) {
  struct limine_framebuffer *fb = framebuffer_request.response->framebuffers[0];
  k_fb._ = fb->address;
  k_fb.width = fb->width;
  k_fb.height = fb->height;
  k_fb.pitch = fb->pitch;
  k_fb.cur_x = k_fb.cur_y = 0; }

void  k_dbg(g_core *f) {
  k_log("\nf@0x"), k_log_n((uintptr_t) f, 16);
  if (f && g_ok(f)) {
    k_log("\n  ip=0x"), k_log_n((uintptr_t) f->ip, 16);
    k_log("\n  len="), k_log_n(f->len, 10);
    k_log("\n  allocd="), k_log_n(f->hp - f->end, 10);
    k_log("\n  stackd="), k_log_n((g_word*) f + f->len - f->sp, 10); } }
