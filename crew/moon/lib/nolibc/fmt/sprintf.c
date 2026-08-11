#include "../impl.h"

int sprintf(char *p, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int r = vsprintf(p, fmt, ap);
  va_end(ap); return r; }
