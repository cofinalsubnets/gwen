#include "i.h"
#include "sys.h"

void *g_malloc(g_core*f, size_t n) {
  return Pd->system->realloc(NULL, n); }

void g_free(g_core*f, void*x) {
  Pd->system->realloc(x, 0); }

NoInline uintptr_t g_clock(void) {
  return Pd->system->getCurrentTimeMilliseconds(); }

Vm(g_fps) {
  float fps = Pd->display->getFPS();
  int i = (int) fps;
  Sp[0] = putnum(i);
  Ip += 1;
  return Continue(); }

static NoInline int gbs(void) {
  PDButtons a=0, b=0, c=0;
  Pd->system->getButtonState(&a, &b, &c);
  return a; }
Vm(g_buttons) {
  Sp[0] = putnum(gbs());
  Ip += 1;
  return Continue(); }

Vm(g_cursor_h) {
  int n = getnum(Sp[0]);
  Col += n;
  while (Col >= COLS) Col -= COLS;
  while (Col < 0) Col += COLS;
  Ip += 1;
  return Continue(); }
Vm(g_cursor_v) {
  int n = getnum(Sp[0]);
  Row += n;
  while (Row >= ROWS) Row -= ROWS;
  while (Row < 0) Row += ROWS;
  Ip += 1;
  return Continue(); }
Vm(curc) {
  glyph_buffer[Row][Col] = getnum(Sp[0]);
  Ip += 1;
  return Continue(); }

Vm(theta) {
  float t = Pd->system->getCrankChange();
  int delta = (int) (256 * t / 360.0f);
  Sp[0] = putnum(delta);
  Ip += 1;
  return Continue(); }

Vm(g_get_glyph) {
  Sp[0] = putnum(glyph_buffer[Row][Col]);
  Ip += 1;
  return Continue(); }
Vm(g_put_glyph) {
  glyph_buffer[Row][Col] = getnum(Sp[0]);
  Ip += 1;
  return Continue(); }

Vm(g_clear) {
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++)
      glyph_buffer[i][j] = 0;
  Ip += 1;
  return Continue(); }
