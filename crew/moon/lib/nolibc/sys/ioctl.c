#include "../impl.h"

int ioctl(int fd, unsigned long req, ...) {
  va_list ap; va_start(ap, req);
  long arg = va_arg(ap, long);
  va_end(ap);
  if (__ai_osv >= 2) {
    /* the encodings part wholesale; the requests whose PAYLOAD agrees
     * translate here, and an unknown one refuses loudly rather than firing
     * a foreign encoding at the kernel. termios rides its own members
     * (another struct, not just another number). */
    unsigned long fq;
    switch (req) {
      case 21523: fq = 0x40087468UL; break;   /* TIOCGWINSZ: struct winsize agrees */
      case 21524: fq = 0x80087467UL; break;   /* TIOCSWINSZ */
      case 21518: fq = 0x20007461UL; break;   /* TIOCSCTTY: void */
      case 21519: fq = 0x40047477UL; break;   /* TIOCGPGRP: int */
      case 21520: fq = 0x80047476UL; break;   /* TIOCSPGRP: int */
      case 21531: fq = 0x4004667fUL; break;   /* FIONREAD: int */
      default: __errno_v = ENOSYS; return -1; }
    return (int) er(fb3(NR_fb_ioctl, fd, (long) fq, arg)); }
  return (int) er(sc3(NR_ioctl, fd, (long) req, arg)); }
