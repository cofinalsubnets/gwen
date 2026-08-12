#include "../impl.h"

static __exitfn __atex[32];
static int __natex;
int atexit(void (*f)(void)) {
  if (__natex >= 32) return -1;
  __atex[__natex++] = f;
  return 0; }
void exit(int c) {
  while (__natex > 0) __atex[--__natex]();
  fflush(0);
  _exit(c); }
