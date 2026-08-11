#include "../impl.h"

char *fgets(char *buf, int n, FILE *f) {
  int i = 0;
  while (i < n - 1) {
    char c;
    long k = read(f->fd, &c, 1);
    if (k <= 0) { if (k == 0) f->eof = 1; if (i == 0) return 0; break; }
    buf[i++] = c;
    if (c == 10) break; }
  buf[i] = 0;
  return buf; }
