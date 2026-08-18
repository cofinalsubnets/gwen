#include "../impl.h"
struct dirent *readdir(DIR *d) {
  if (d->pos >= d->len) {
    /* one call, both kernels: the 4th arg is freebsd's basep (NULL); linux
     * getdents64 takes three and never reads the register */
    long n = sc4(NR_getdents64, d->fd, (long) d->buf, sizeof d->buf, 0);
    if (n <= 0) { if (n < 0) __errno_v = (int) -n; return 0; }
    d->len = (int) n; d->pos = 0; }
  if (__ai_osv == 2) {
    /* freebsd's record is another shape (namlen, pads): repack into ours */
    struct __fb_dirent *fe = (struct __fb_dirent *) (d->buf + d->pos);
    unsigned l = fe->d_namlen;
    if (l > 255) l = 255;
    d->pos += fe->d_reclen;
    d->ent.d_ino = fe->d_ino;
    d->ent.d_off = fe->d_off;
    d->ent.d_reclen = sizeof d->ent;
    d->ent.d_type = fe->d_type;
    memcpy(d->ent.d_name, fe->d_name, l);
    d->ent.d_name[l] = 0;
    return &d->ent; }
  struct dirent *e = (struct dirent *) (d->buf + d->pos);
  d->pos += e->d_reclen;
  return e; }
