#include "../impl.h"

void rewind(FILE *f) {
  if (fseek(f, 0, 0) == 0) f->err = 0; }
