#include "../impl.h"

int asprintf(char **out, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int r = vasprintf(out, fmt, ap);
  va_end(ap); return r; }
