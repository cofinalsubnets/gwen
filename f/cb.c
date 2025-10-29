#include "g.h"
#include "cb.h"

void cb_fill(struct cb *c, uint8_t _) {
  uint32_t rows = c->rows, cols = c->cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      c->cb[i * cols + j] = _; }
void cb_cur(struct cb *c, uint32_t row, uint32_t col) {
  c->row = row % c->rows, c->col = col % c->cols; }
void cb_mv_cur(struct cb *c, int32_t dr, int32_t dc) {
  cb_cur(c, dr + c->row, dc + c->col); }

void cb_log_n(struct cb *c, uintptr_t n, uintptr_t base) {
  static const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  uintptr_t d = n % base;
  if (n / base) cb_log_n(c, n / base, base);
  cb_put_char(c, digits[d]); }

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
  c->col = 0;
  if (++c->row == c->rows) c->row = 0; }
#define tab_width 8
void cb_put_char(struct cb *c, char i) {
  if (i == '\n') return cb_line_feed(c);
  c->cb[c->row * c->cols + c->col] = i;
  if (++c->col == c->cols) cb_line_feed(c); }

void cb_log(struct cb *c, const char *msg) {
  while (*msg) cb_put_char(c, *msg++); }

int cb_ungetc(struct cb *c, int i) {
  int row = c->rr, col = c->rc;
  if (c->rc-- == 0) {
    c->rc = c->cols - 1;
    if (c->rr-- == 0) c->rr = c->rows - 1; }
  c->cb[c->rr * c->cols + c->rc] = i;
  return i; }
int cb_eof(struct cb *c) {
  return c->row == c->rr && c->col == c->rc; }
int cb_getc(struct cb *c) {
  if (cb_eof(c)) return -1;
  int i = c->cb[c->rr * c->cols + c->rc];
  if (++c->rc == c->cols) {
    c->rc = 0;
    if (++c->rr == c->rows) c->rr = 0; }
  return i; }


