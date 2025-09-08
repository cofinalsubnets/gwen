#include "i.h"

NoInline long g_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }

void *g_malloc_libc(g_core *f, size_t n) { return malloc(n); }
void g_free_libc(g_core *f, void *p) { return free(p); }

Vm(sysclock) { return Sp[0] = putnum(g_clock()), Ip += 1, Continue(); }

Vm(p_isatty) { return
  Sp[0] = isatty(getnum(Sp[0])) ? putnum(-1) : nil, Ip += 1, Continue(); }
