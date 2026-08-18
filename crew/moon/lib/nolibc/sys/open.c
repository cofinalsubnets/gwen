#include "../impl.h"

int open(char const *p, int fl, ...) {
  va_list ap; va_start(ap, fl);
  int mode = va_arg(ap, int);
  va_end(ap);
  if (__ai_osv == 2) fl = (int) __ai_ofb(fl);
  return (int) er(sc4(NR_openat, AT_FDCWD, (long) p, fl, mode)); }
