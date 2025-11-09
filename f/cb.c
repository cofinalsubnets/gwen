#include "g.h"
#include "cb.h"

void cb_fill(struct cb *c, uint8_t _) {
  for (uint32_t i = 0, j = c->rows * c->cols; i < j; i++)
    c->cb[i] = _; }

void cb_cur(struct cb *c, uint32_t row, uint32_t col) {
  c->wpos = (row * c->cols + col) % (c->rows * c->cols); }

static void cb_line_feed(struct cb *c) {
  uintptr_t rs = c->rows, cs = c->cols,
            p = 1 + c->wpos / cs;
  c->wpos = cs * (p == rs ? 0 : p); }

void cb_putc(struct cb *c, char i) {
  if (i == '\b') {
    if (c->wpos != c->rpos) c->wpos--;
    return; }
  c->cb[c->wpos] = i;
  if (i == '\n') return cb_line_feed(c);
  if (++c->wpos == c->cols * c->rows) c->wpos = 0; }

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
