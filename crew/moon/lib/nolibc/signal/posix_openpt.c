#include "../impl.h"

/* ---- the pty quartet ---- */
#if defined(__FreeBSD__)
int posix_openpt(int fl) { return (int) er(sc1(NR_posix_openpt, fl)); }   /* a real syscall here; no /dev/ptmx */
#else
int posix_openpt(int fl) { return open("/dev/ptmx", fl, 0); }
#endif
