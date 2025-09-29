#include "i.h"
Vm(prc) {
  g_word w = *Sp;
  g_fputc(getnum(w), g_stdout);
  Ip += 1;
  return Continue(); }

Vm(dot) {
  transmit(f, g_stdout, Sp[0]);
  Ip += 1;
  return Continue(); }

void transmit(g_core *f, g_file out, g_word x) {
  if (nump(x)) g_fprintf(out, "%ld", (long) getnum(x));
  else if (datp(x)) typ(x)->em(f, out, x);
  else g_fprintf(out, "#%lx", (long) x); }

g_core *g_write1(g_core *f) { return transmit(f, g_stdout, f->sp[0]), f; }
