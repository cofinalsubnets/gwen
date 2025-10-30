#include "g.h"
#include "cb.h"

void cb_fill(struct cb *c, uint8_t _) {
  uint32_t rows = c->rows, cols = c->cols;
  for (uint32_t i = 0, j = c->rows * c->cols; i < j; i++)
    c->cb[i] = _; }
void cb_cur(struct cb *c, uint32_t row, uint32_t col) {
  c->wpos = (row * c->cols + col) % (c->rows * c->cols); }

void cb_log_n(struct cb *c, uintptr_t n, uintptr_t base) {
  uintptr_t q = n / base, r = n % base;
  if (q) cb_log_n(c, q, base);
  cb_put_char(c, g_digits[r]); }

void cb_vlogf(struct cb *cb, const char *fmt, va_list xs) {
  while (*fmt) {
    char c = *fmt++;
    if (c != '%') cb_put_char(cb, c);
    else re:
      switch (c = *fmt++) {
      case 0: return;
      default: cb_put_char(cb, c); break;
      case 'l': goto re;
      case 'd': cb_log_n(cb, va_arg(xs, uintptr_t), 10); break;
      case 'x': cb_log_n(cb, va_arg(xs, uintptr_t), 16); break;
      case 'o': cb_log_n(cb, va_arg(xs, uintptr_t), 8); break;
      case 'b': cb_log_n(cb, va_arg(xs, uintptr_t), 2); break; } } }


static void cb_line_feed(struct cb *c) {
  uintptr_t p = c->wpos + c->cols;
  p -= p % c->cols;
  p %= c->rows * c->cols;
  c->wpos = p; }

void cb_put_char(struct cb *c, char i) {
  if (i == '\n') return cb_line_feed(c);
  c->cb[c->wpos] = i;
  if (++c->wpos == c->cols * c->rows) c->wpos = 0; }

void cb_log(struct cb *c, const char *msg) {
  while (*msg) cb_put_char(c, *msg++); }

int cb_ungetc(struct cb *c, int i) {
  if (c->rpos-- == 0)
    c->rpos = c->cols * c->rows - 1;
  return c->cb[c->rpos] = i; }
int cb_eof(struct cb *c) {
  return c->rpos == c->wpos; }
int cb_getc(struct cb *c) {
  if (c->rpos == c->wpos) return -1;
  int i = c->cb[c->rpos];
  if (++c->rpos == c->cols * c->rows) c->rpos = 0;
  return i; }


