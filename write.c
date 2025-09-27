#include "i.h"
Vm(prc) {
  g_word w = *Sp;
  putc(getnum(w), stdout);
  Ip += 1;
  return Continue(); }

Vm(dot) {
  transmit(f, stdout, Sp[0]);
  Ip += 1;
  return Continue(); }

void transmit(g_core *f, FILE* out, g_word x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (datp(x)) typ(x)->em(f, out, x);
  else fprintf(out, "#%lx", (long) x); }

g_core *g_write1(g_core *f) { return transmit(f, stdout, f->sp[0]), f; }
