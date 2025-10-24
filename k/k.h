#ifndef _g_k_h
#define _g_k_h
#include "limine/limine.h"
#include "g.h"
#include <stdarg.h>
extern const char digits[];
void k_reset(void), arch_init(void);
uint8_t key_get(void);

static Inline void k_stop(void) { asm volatile (
#if defined (__x86_64__)
  "hlt"
#elif defined (__aarch64__) || defined (__riscv)
  "wfi"
#elif defined (__loongarch64)
  "idle 0"
#endif
  ); }

typedef struct g_fb32 {
  volatile uint32_t *_;
  uint16_t width, height, pitch, cur_x, cur_y; } g_fb32;

typedef struct g_cb {
  struct { uint8_t c0, c1, rgb[2][3]; } *cb;
  uint16_t rows, cols, cx, cy; } g_cb;

void
  *malloc(size_t),
  free(void*),
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
  k_cur_msg(const char *msg, uint32_t fg, uint32_t bg);

extern struct k {
  uint64_t ticks;
  g_cb cb;
  g_fb32 fb;
  struct mem {
    uintptr_t len;
    struct mem *next;
    uint8_t _[];
  } *free, *used;
  struct g *f;
  struct { uint8_t k, f; } kb; } K;
void k_logf(const char*, ...), k_vlogf(const char*, va_list), k_log_char(char);
#endif
