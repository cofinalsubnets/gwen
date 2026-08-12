#include "../impl.h"

int vsprintf(char *p, char const *fmt, va_list ap) {
  return vsnprintf(p, (size_t) -1, fmt, ap); }
