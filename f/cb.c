#include "g.h"
#include "cb.h"

void cb_fill(struct cb *c, uint8_t _) {
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
  uintptr_t rs = c->rows, cs = c->cols,
            p = 1 + c->wpos / cs;
  c->wpos = cs * (p == rs ? 0 : p); }

void cb_put_char(struct cb *c, char i) {
  if (i == '\b') {
    if (c->wpos != c->rpos) c->wpos--;
    return; }
  c->cb[c->wpos] = i;
  if (i == '\n') return cb_line_feed(c);
  if (++c->wpos == c->cols * c->rows) c->wpos = 0; }

void cb_log(struct cb *c, const char *msg) {
  while (*msg) cb_put_char(c, *msg++); }

int cb_ungetc(struct cb *c, int i) {
  uint16_t r = c->rpos;
  r = r > 0 ? r - 1 : c->cols * c->rows - 1;
  return r == c->wpos ? -1 : (c->cb[c->rpos = r] = i); }

int cb_eof(struct cb *c) {
  return c->rpos == c->wpos; }

int cb_getc(struct cb *c) {
  if (c->rpos == c->wpos) return -1;
  int i = c->cb[c->rpos];
  if (++c->rpos == c->cols * c->rows) c->rpos = 0;
  return i; }


