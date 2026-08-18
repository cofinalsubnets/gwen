#include "../impl.h"

/* the freebsd branch fills the ino64 twin and translates -- shared with
 * stat.c/lstat.c through __ai_fbstat (this member is their floor). */
void __ai_fbstat(struct __fb_stat const *f, struct stat *st) {
  memset(st, 0, sizeof *st);
  st->st_dev = f->st_dev;
  st->st_ino = f->st_ino;
  st->st_nlink = f->st_nlink;
  st->st_mode = f->st_mode;
  st->st_uid = f->st_uid;
  st->st_gid = f->st_gid;
  st->st_rdev = f->st_rdev;
  st->st_size = f->st_size;
  st->st_blksize = f->st_blksize;
  st->st_blocks = f->st_blocks;
  st->st_atim = f->st_atim;
  st->st_mtim = f->st_mtim;
  st->st_ctim = f->st_ctim; }

int fstat(int fd, struct stat *st) {
  if (__ai_osv == 2) {
    struct __fb_stat f;
    long r = er(sc2(NR_fstat, fd, (long) &f));
    if (r >= 0) __ai_fbstat(&f, st);
    return (int) r; }
  return (int) er(sc2(NR_fstat, fd, (long) st)); }
