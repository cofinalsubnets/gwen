#include "../impl.h"

int fcntl(int fd, int cmd, ...) {
  va_list ap; va_start(ap, cmd);
  long arg = va_arg(ap, long);
  va_end(ap);
  if (__ai_osv == 2) {
    /* DUPFD/GETFD/SETFD/GETFL/SETFL share their numbers; the rest translate.
     * SETFL's status flags and GETFL's answer wear the other kernel's bits;
     * the record-lock trio takes freebsd's numbers and its field order. */
    if (cmd == F_SETFL) arg = __ai_ofb(arg);
    else if (cmd == F_DUPFD_CLOEXEC) cmd = 17;
    else if (cmd == F_GETLK || cmd == F_SETLK || cmd == F_SETLKW) {
      struct flock *l = (struct flock *) arg;
      struct { long start, len; int pid; short type, whence; int sysid; } kf;
      kf.start = l->l_start; kf.len = l->l_len; kf.pid = l->l_pid;
      kf.type = (short) (l->l_type == F_RDLCK ? 1 : l->l_type == F_WRLCK ? 3 : 2);
      kf.whence = l->l_whence; kf.sysid = 0;
      long r = er(sc3(NR_fcntl, fd, cmd + 6, (long) &kf));   /* 5/6/7 -> 11/12/13 */
      if (r >= 0 && cmd == F_GETLK) {
        l->l_start = kf.start; l->l_len = kf.len; l->l_pid = kf.pid;
        l->l_whence = kf.whence;
        l->l_type = (short) (kf.type == 1 ? F_RDLCK : kf.type == 3 ? F_WRLCK : F_UNLCK); }
      return (int) r; }
    long r = er(sc3(NR_fcntl, fd, cmd, arg));
    if (r >= 0 && cmd == F_GETFL) r = __ai_ocan(r);
    return (int) r; }
  return (int) er(sc3(NR_fcntl, fd, cmd, arg)); }
