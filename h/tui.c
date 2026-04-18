#include <ncurses.h>
#include "../g/g.h"
#include "../f/cb.h"
#include <stdlib.h>
#include <time.h>

g_noinline uintptr_t g_clock(void) {
  struct timespec ts;
  return clock_gettime(CLOCK_REALTIME, &ts) ? (uintptr_t)-1
       : (uintptr_t)(ts.tv_sec * 1000 + ts.tv_nsec / 1000000); }

static struct cb *tcb;
static bool teof;

static void cb_render(void) {
  struct cb *c = tcb;
  uint16_t n = c->rows * c->cols;
  for (uint16_t pos = 0; pos < n; pos++) {
    uint8_t ch = c->cb[pos];
    bool sel = c->rpos <= c->wpos
      ? (pos >= c->rpos && pos < c->wpos)
      : (pos >= c->rpos || pos < c->wpos);
    if (sel) attron(A_REVERSE);
    mvaddch(pos / c->cols, pos % c->cols, (ch && ch != '\n') ? ch : ' ');
    if (sel) attroff(A_REVERSE); }
  move(c->wpos / c->cols, c->wpos % c->cols);
  refresh(); }

int gputc(struct g *f, int c) {
  cb_putc(tcb, c);
  cb_render();
  return c; }

int ggetc(struct g *f) {
  int c;
  if ((c = cb_getc(tcb)) != -1) return c;
  uint16_t start = tcb->wpos;
  for (;;) {
    c = getch();
    if (c == ERR) continue;
    switch (c) {
      case 4:  // ctrl-d
        teof = true;
        cb_render();
        return -1;
      case KEY_BACKSPACE: case 127: case '\b':
        if (tcb->wpos != start) cb_putc(tcb, '\b');
        cb_render();
        break;
      case '\r': c = '\n'; // fall through
      default:
        if ((c >= ' ' && c < 256) || c == '\n' || c == '\t') {
          cb_putc(tcb, (char)c);
          cb_render();
          if (c == '\n') return cb_getc(tcb); }
        break; } } }

int gungetc(struct g *f, int c) {
  return cb_ungetc(tcb, c); }

int geof(struct g *f) {
  return teof && cb_eof(tcb); }

int gflush(struct g *f) {
  tcb->rpos = tcb->wpos;
  return refresh(), 0; }

int main(int argc, char const **argv) {
  initscr();
  raw();
  noecho();
  keypad(stdscr, TRUE);

  uint8_t rows = (uint8_t)(LINES < 255 ? LINES : 255);
  uint8_t cols = (uint8_t)(COLS  < 255 ? COLS  : 255);
  tcb = malloc(sizeof(struct cb) + (size_t)rows * cols);
  tcb->rows = rows;
  tcb->cols = cols;
  tcb->rpos = tcb->wpos = 0;
  tcb->flag = 0;
  cb_fill(tcb, 0);

  struct g *f;
  for (f = g_ini(); *argv; f = g_strof(f, *argv++));
  for (f = g_push(f, 1, g_nil); argc--; f = gxr(f));
  if (g_ok(f)) {
    struct g_def d[] = {{"argv", g_pop1(f)}, {0}};
    static char const p[] =
      #include "boot.h"
      "(:(g e)(: r(read e)(?(= e r)0(: _(ev'ev r)(g e))))(g(sym 0)))";
    f = g_evals_(g_defs(f, d), p); }

  endwin();
  free(tcb);
  return g_code_of(f); }
