#include "i.h"

Vm(prc)     { Word w = *Sp; putc(getnum(w), stdout);     return op(1, w); }
Vm(display) { Word w = *Sp; transmit(f, stdout, w); return op(1, w); }
Vm(error) { exit(1); }

void transmit(Core *f, PFile* out, Word x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (ptr(x)->ap == data) dtyp(x)->emit(f, out, x);
  else fprintf(out, "#%lx", (long) x); }

void p_write1f(Core *f, PFile *out) {
  transmit(f, out, f->sp[0]); }
