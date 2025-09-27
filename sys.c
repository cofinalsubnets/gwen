#include "i.h"
#include <time.h>
#include <unistd.h>

NoInline uintptr_t g_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }

Vm(sysclock) {
  Sp[0] = putnum(g_clock());
  Ip += 1;
  return Continue(); }

Vm(p_isatty) {
  Sp[0] = isatty(getnum(Sp[0])) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }
