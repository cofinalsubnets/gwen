#include "../impl.h"

struct dirent *readdir(DIR *d) {
  if (d->pos >= d->len) {
    long n = sc3(NR_getdents64, d->fd, (long) d->buf, sizeof d->buf);
    if (n <= 0) { if (n < 0) __errno_v = (int) -n; return 0; }
    d->len = (int) n; d->pos = 0; }
  struct dirent *e = (struct dirent *) (d->buf + d->pos);
  d->pos += e->d_reclen;
  return e; }
