#include "../impl.h"
/* absent on freebsd until rung 3 supplies the body -- an empty member defines
   nothing, so a consumer reads "undefined reference", the honest sentence */
#if !defined(__FreeBSD__)
struct dirent *readdir(DIR *d) {
  if (d->pos >= d->len) {
    long n = sc3(NR_getdents64, d->fd, (long) d->buf, sizeof d->buf);
    if (n <= 0) { if (n < 0) __errno_v = (int) -n; return 0; }
    d->len = (int) n; d->pos = 0; }
  struct dirent *e = (struct dirent *) (d->buf + d->pos);
  d->pos += e->d_reclen;
  return e; }
#endif
