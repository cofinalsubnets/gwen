#include "../impl.h"

/* ---- sockets ---- */
long sendfile(int out, int in, long *off, unsigned long n) {
  return er(sc4(NR_sendfile, out, in, (long) off, (long) n)); }
