#include "../impl.h"

int closedir(DIR *d) {
  int r = close(d->fd);
  free(d);
  return r; }
