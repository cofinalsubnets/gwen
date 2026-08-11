/* crew/moon/lib/nolibc/impl.h -- the shared head of every nolibc member.
 * The members are pulled BY NEED (crew/moon/moon.l's runtime table globs this
 * directory the way it globs lib/math/), so a love that asks for no calendar
 * links no calendar. That is the whole reason this is a directory and not the
 * one file it used to be. ⚠ a member reaching another member's file-scope
 * static is what splitting costs: use the public spelling (errno, not
 * __errno_v), or move the state here. */
#ifndef AI_NOLIBC_IMPL_H
#define AI_NOLIBC_IMPL_H
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <setjmp.h>
#include <poll.h>
#include <sys/select.h>
#include <locale.h>
#include <time.h>
#include <sys/time.h>
#include <utime.h>
#include <dirent.h>
#include <termios.h>
#include <netdb.h>
#include <pwd.h>
#include <grp.h>
#include <link.h>
#include <sched.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <sys/mount.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <sys/signalfd.h>

#include <netinet/in.h>

extern long __ai_sys(long n, long a, long b, long c, long d, long e, long f);
extern void __ai_sigret(void);
extern int main(int, char**);

/* FILE: stdio.h hands out an opaque pointer, so a member touching a stream
 * (popen's pid, ferror's err) needs the layout here. */
struct _IO_FILE {
  int fd;
  int wr;                                    /* open for writing */
  int line;                                  /* flush on newline */
  int err;
  int eof;                                   /* a read hit end-of-file (feof) */
  int heap;                                  /* buf and FILE both off malloc (fopen) */
  int pid;                                   /* popen's child, for pclose's wait */
  int un;                                    /* ungetc's pushback byte + 1 (0 = none) */
  int len, cap;
  unsigned char *buf;
};
#endif
