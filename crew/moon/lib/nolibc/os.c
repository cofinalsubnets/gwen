/* crew/moon/lib/nolibc/os.c -- the kernel under one binary (seed-universal
 * rungs UV1-UV2). one build runs linux, freebsd and netbsd: __ai_osdetect asks
 * the kernel which it is (once, at entry or lazily under __ai_call), and
 * numbers, errnos, signals, masks and flag words translate through the tables
 * here. x64 and arm64 carry them; riscv takes the identity stubs below and
 * answers whichever kernel -os named. */
#include "impl.h"

long __ai_osv;         /* 0 unprobed; 1 linux; 2 freebsd; 3 netbsd; -1 inle,
                        * written at its entry: we ARE the kernel, nothing to probe */

/* the inle door's default, for a link that carries no src/sys.c: refuse, and
 * name the protocol. a negative osv is only ever written by inle's own entry,
 * so a hosted binary never takes __ai_call's arm and this body is dead weight
 * the dead-static sweep cannot drop -- one line, kept for the symbol. */
__attribute__((weak))
long __ai_inle(long n, long a, long b, long c, long d, long e, long f) {
  (void) n, (void) a, (void) b, (void) c, (void) d, (void) e, (void) f;
  return -38; }

long __ai_osdetect(void) {
#ifndef AiOsTranslate
  /* no tables on this arch: the kernel is whichever one the build was compiled
   * for, and nothing at runtime can contradict it. ⚠ READ OFF -os, never
   * assumed -- linux is where we started, not a default, and a build naming a
   * kernel this arch has no tail for owes a diagnostic and not another
   * kernel's numbers. */
# if defined(__linux__)
  return 1;
# elif defined(__FreeBSD__)
#  error "nolibc: -os freebsd wants the translation tables, and this arch has no machine tail for them"
# elif defined(__NetBSD__)
#  error "nolibc: -os netbsd wants the translation tables, and this arch has no machine tail for them"
# else
#  error "nolibc: no OS predefine -- -os named a kernel os.c cannot speak for"
# endif
#else
  /* 20 is getpid on both BSDs and writev on linux: writev(-1, NULL, 0) is
   * -EBADF, a pid is positive, and no kernel is disturbed by asking. a
   * positive answer says BSD; kern.ostype's first byte parts the two
   * (__sysctl is 202 and {CTL_KERN, KERN_OSTYPE} is {1, 1} on both).
   * ⚠ on aarch64 the two doors are mksys leaves, not __ai_sys: netbsd there
   * SIGSYSes the register form, so the question has to be asked in the svc
   * IMMEDIATE the kernel being asked about reads. 20 is epoll_create1 on
   * linux/arm64, which refuses -EINVAL -- the same negative the writev door
   * gives. ⚠ and that immediate is ILLEGAL on freebsd/arm64, whose svc handler
   * signals SIGILL/ILL_ILLOPN for any but zero -- asking blind kills the
   * process before it can hear an answer. So freebsd is already OUT by the
   * time this runs: crt0 parts it from the rest by the entry protocol alone
   * and hands 2 down, and this branch only ever asks linux from netbsd. */
# if defined(__aarch64__)
  long r = __ai_nbp20(-1);
  if (r <= 0) return 1;
  { int mib[2] = {1, 1};
    char b[16] = {0};
    unsigned long len = sizeof b;
    __ai_nbp202((long) mib, 2, (long) b, (long) &len, 0, 0);
    return b[0] == 'N' ? 3 : 2; }
# else
  long r = __ai_sys(20, -1, 0, 0, 0, 0, 0);
  if (r <= 0) return 1;
  { int mib[2] = {1, 1};
    char b[16] = {0};
    unsigned long len = sizeof b;
    __ai_sys(202, (long) mib, 2, (long) b, (long) &len, 0, 0);
    return b[0] == 'N' ? 3 : 2; }
# endif
#endif
}

#ifndef AiOsTranslate
long __ai_nrfb(long n) { return n; }      /* no second kernel on this arch */
long __ai_errfb(long e) { return e; }
long __ai_sigfb(long s) { return s; }
long __ai_sigcan(long s) { return s; }
unsigned long __ai_maskfb(unsigned long m) { return m; }
unsigned long __ai_maskcan(unsigned long m) { return m; }
long __ai_ofb(long f) { return f; }
long __ai_ocan(long f) { return f; }
long __ai_mapfb(long f) { return f; }
long __ai_safb(long f) { return f; }
long __ai_sacan(long f) { return f; }
void __ai_tiofb(struct termios const *t, struct __fb_termios *f) { (void) t; (void) f; }
void __ai_tiocan(struct __fb_termios const *f, struct termios *t) { (void) f; (void) t; }
long __ai_affb(long a) { return a; }
long __ai_afcan(long a) { return a; }
long __ai_sotype(long t) { return t; }
long __ai_msgfb(long f) { return f; }
long __ai_msgcan(long f) { return f; }
int __ai_sofb(long *lv, long *op) { (void) lv; (void) op; return 0; }
unsigned int __ai_sain(void const *a, unsigned int n, void *out) { memcpy(out, a, n); return n; }
void __ai_saout(void *a, unsigned int n) { (void) a; (void) n; }
#else
/* canonical (linux x86_64) -> {freebsd, netbsd}, sorted by canonical. a row
 * rides here only when the members speak the call correctly on that kernel --
 * same shape, or a body whose BSD branch builds the BSD shape and hands the
 * number through this door; -1 in a column refuses that kernel (ENOSYS,
 * loudly). unmapped stay unmapped: mount, sendfile, and linux's own
 * mechanisms (clone, dup3, signalfd4, memfd_create, unshare). netbsd's
 * classic band matches freebsd number for number; its versioned calls
 * (__fstat50 440, __getdents30 390, __wait450 449 ..) part company, and the
 * pad-carrying classics (lseek, pread, pwrite, ftruncate, mmap) keep their
 * numbers while the members slide the args. */
static short const os_nr[][3] = {
  {NR_read,          NR_fb_read,          3},
  {NR_write,         NR_fb_write,         4},
  {NR_close,         NR_fb_close,         6},
  {NR_fstat,         NR_fb_fstat,       440},   /* the member fills the OS twin */
  {NR_lseek,         NR_fb_lseek,       199},   /* nb: (fd PAD off whence) */
  {NR_mmap,          NR_fb_mmap,        197},   /* nb: 7 args, pos on the stack */
  {NR_mprotect,      NR_fb_mprotect,     74},
  {NR_munmap,        NR_fb_munmap,       73},
  {NR_rt_sigaction,  NR_fb_rt_sigaction, -1},   /* the member builds the OS shape; nb rides __sigaction_sigtramp */
  {NR_rt_sigprocmask, NR_fb_rt_sigprocmask, 293},/* ..the 16-byte set + how+1, both BSDs */
  {NR_ioctl,         NR_fb_ioctl,        54},   /* the member translates requests */
  {NR_pread64,       NR_fb_pread64,     173},   /* nb: (fd buf n PAD off) */
  {NR_pwrite64,      NR_fb_pwrite64,    174},
  {NR_madvise,       NR_fb_madvise,      75},
  {NR_nanosleep,     NR_fb_nanosleep,   430},
  {NR_getpid,        NR_fb_getpid,       20},
  {NR_socket,        NR_fb_socket,      394},
  {NR_connect,       NR_fb_connect,      98},
  {NR_accept,        NR_fb_accept,       30},
  {NR_sendto,        NR_fb_sendto,      133},
  {NR_recvfrom,      NR_fb_recvfrom,     29},
  {NR_sendmsg,       NR_fb_sendmsg,      28},
  {NR_recvmsg,       NR_fb_recvmsg,      27},
  {NR_shutdown,      NR_fb_shutdown,    134},
  {NR_bind,          NR_fb_bind,        104},
  {NR_listen,        NR_fb_listen,      106},
  {NR_getsockname,   NR_fb_getsockname,  32},
  {NR_getpeername,   NR_fb_getpeername,  31},
  {NR_setsockopt,    NR_fb_setsockopt,  105},
  {NR_getsockopt,    NR_fb_getsockopt,  118},
  {57,               NR_fb_fork,          2},   /* fork: linux's fork.c rides clone (56,
                                                 * unmapped); a BSD branch calls 57 */
  {NR_execve,        NR_fb_execve,       59},
  {60,               NR_fb_exit,          1},   /* exit and exit_group are one act here */
  {NR_wait4,         NR_fb_wait4,       449},
  {NR_kill,          NR_fb_kill,         37},
  {NR_fcntl,         NR_fb_fcntl,        92},
  {NR_fsync,         NR_fb_fsync,        95},
  {NR_fdatasync,     NR_fb_fdatasync,   241},
  {NR_ftruncate,     NR_fb_ftruncate,   201},   /* nb: (fd PAD len) */
  {NR_getcwd,        NR_fb_getcwd,      296},   /* __getcwd fills and answers 0; all faces fill */
  {NR_chdir,         NR_fb_chdir,        12},
  {NR_fchmod,        NR_fb_fchmod,      124},
  {NR_fchown,        NR_fb_fchown,      123},
  {NR_umask,         NR_fb_umask,        60},
  {NR_getuid,        NR_fb_getuid,       24},
  {NR_getgid,        NR_fb_getgid,       47},
  {NR_setuid,        NR_fb_setuid,       23},
  {NR_setgid,        NR_fb_setgid,      181},
  {NR_geteuid,       NR_fb_geteuid,      25},
  {NR_setpgid,       NR_fb_setpgid,      82},
  {NR_setsid,        NR_fb_setsid,      147},
  {NR_setgroups,     NR_fb_setgroups,    80},
  {NR_getpgid,       NR_fb_getpgid,     207},
  {NR_chroot,        NR_fb_chroot,       61},
  {NR_getdents64,    NR_fb_getdirentries, 390}, /* the member repacks the record */
  {NR_clock_gettime, NR_fb_clock_gettime, 427}, /* the member translates the id */
  {NR_exit_group,    NR_fb_exit,          1},
  {NR_openat,        NR_fb_openat,      468},
  {NR_mkdirat,       NR_fb_mkdirat,     461},
  {NR_mknodat,       NR_fb_mknodat,     460},
  {NR_fchownat,      NR_fb_fchownat,    464},
  {NR_newfstatat,    NR_fb_newfstatat,  466},   /* the member fills the OS twin */
  {NR_unlinkat,      NR_fb_unlinkat,    471},
  {NR_renameat,      NR_fb_renameat,    458},
  {NR_linkat,        NR_fb_linkat,      457},
  {NR_symlinkat,     NR_fb_symlinkat,   470},
  {NR_readlinkat,    NR_fb_readlinkat,  469},
  {NR_fchmodat,      NR_fb_fchmodat,    463},
  {NR_faccessat,     NR_fb_faccessat,   462},
  {NR_pselect6,      NR_fb_pselect6,    436},   /* the member hands a bare sigset* 6th */
  {NR_ppoll,         NR_fb_ppoll,       437},   /* __pollts50: same first four args */
  {NR_utimensat,     NR_fb_utimensat,   467},
  {NR_pipe2,         NR_fb_pipe2,       453},
};

/* ⚠ A PLAIN SCAN, and it must be: the rows are keyed by OUR NR_*, which impl.h
 * defines per arch, so the written order is ascending on x86_64 (read 0, write
 * 1, close 3 ..) and is NOT on the asm-generic arches (read 63, write 64, close
 * 57 ..). An early exit on a passed key would answer ENOSYS to almost every
 * call the moment this lane opens on arm64. 71 rows, and only on a BSD. */
long __ai_nrfb(long n) {
  int col = __ai_osv == 3 ? 2 : 1;
  for (unsigned i = 0; i < sizeof os_nr / sizeof *os_nr; i++)
    if (os_nr[i][0] == n) return os_nr[i][col];
  return -1; }

/* freebsd errno -> canonical, indexed by freebsd's value (ELAST 97). rows
 * with no linux concept (the rpc/auth/capsicum family) keep their raw value:
 * our errno.h names none of them, so nothing upstairs can misread one. */
static unsigned char const os_err[] = {
  0,   1,   2,   3,   4,   5,   6,   7,   8,   9,     /* 0..9 as linux */
  10,  35,  12,  13,  14,  15,  16,  17,  18,  19,    /* 11 EDEADLK */
  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,
  30,  31,  32,  33,  34,  11,  115, 114, 88,  89,    /* 35 EAGAIN, 36 EINPROGRESS,
                                                       * 37 EALREADY, 38 ENOTSOCK,
                                                       * 39 EDESTADDRREQ */
  90,  91,  92,  93,  94,  95,  96,  97,  98,  99,    /* the socket band, shifted */
  100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
  110, 111, 40,  36,  112, 113, 39,  67,  87,  122,   /* 62 ELOOP, 63 ENAMETOOLONG,
                                                       * 66 ENOTEMPTY, 68 EUSERS,
                                                       * 69 EDQUOT */
  116, 66,  72,  73,  74,  75,  76,  37,  38,  79,    /* 70 ESTALE, 71 EREMOTE,
                                                       * 77 ENOLCK, 78 ENOSYS */
  80,  81,  43,  42,  75,  125, 84,  87,  88,  74,    /* 82 EIDRM, 83 ENOMSG,
                                                       * 84 EOVERFLOW, 85 ECANCELED,
                                                       * 86 EILSEQ, 89 EBADMSG */
  72,  67,  71,  93,  94,  131, 130, 97,              /* 90 EMULTIHOP, 91 ENOLINK,
                                                       * 92 EPROTO, 95 ENOTRECOVERABLE,
                                                       * 96 EOWNERDEAD */
};

/* ..and netbsd rewrites only the tail: 85..98 in its own order (ENOATTR
 * keeps its raw 93 -- our errno.h names no such thing). below 85 the two
 * BSDs agree to the number. */
static unsigned char const os_err_nb[] = {
  84, 95, 125, 74, 61, 63, 60, 62, 93, 72, 67, 71, 130, 131 };
long __ai_errfb(long e) {
  if (__ai_osv == 3 && e >= 85 && e <= 98) return os_err_nb[e - 85];
  return (e > 0 && e < (long) (sizeof os_err)) ? os_err[e] : e; }

/* the signal permutation, canonical <-> freebsd. same through 6, 8..9, 11,
 * 13..15, 21..22, 24..28; the parted ones swap in pairs; -1 = no twin
 * (STKFLT and PWR have no freebsd number; EMT and INFO no canonical one, and
 * ride raw -- nothing upstairs names them). ⚠ the overlap is adversarial:
 * freebsd 17 IS canonical SIGCHLD's number and means SIGSTOP there -- a lost
 * translation stops a child where it meant to reap it. */
static signed char const os_sigfb[32] = {
   0,  1,  2,  3,  4,  5,  6, 10,  8,  9, 30, 11, 31, 13, 14, 15,
  -1, 20, 19, 17, 18, 21, 22, 16, 24, 25, 26, 27, 28, 23, -1, 12 };
static signed char const os_sigcan[32] = {
   0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  7, 11, 31, 13, 14, 15,
  23, 19, 20, 18, 17, 21, 22, 29, 24, 25, 26, 27, 28, 29, 10, 12 };
long __ai_sigfb(long s) { return (s >= 0 && s < 32) ? os_sigfb[s] : -1; }
long __ai_sigcan(long s) { return (s >= 0 && s < 32) ? os_sigcan[s] : s; }

/* a mask, bit (sig-1), both spellings in the low word (signals 1..31); the
 * canonical rt band above 31 has no freebsd twin and drops. */
unsigned long __ai_maskfb(unsigned long m) {
  unsigned long o = 0;
  for (int s = 1; s < 32; s++)
    if (m & (1UL << (s - 1))) { long t = os_sigfb[s]; if (t > 0) o |= 1UL << (t - 1); }
  return o; }
unsigned long __ai_maskcan(unsigned long m) {
  unsigned long o = 0;
  for (int s = 1; s < 32; s++)
    if (m & (1UL << (s - 1))) { long t = os_sigcan[s]; if (t > 0) o |= 1UL << (t - 1); }
  return o; }

/* open flags: the access mode rides; the named bits translate; the rest drop.
 * both BSDs (stable/14, netbsd-10 sys/fcntl.h): NONBLOCK 4, APPEND 8,
 * NOFOLLOW 0x100, CREAT 0x200, TRUNC 0x400, EXCL 0x800, NOCTTY 0x8000 --
 * only DIRECTORY and CLOEXEC part company (fb 0x20000/0x100000, nb
 * 0x200000/0x400000). */
long __ai_ofb(long f) {
  long o = f & 3, nb = __ai_osv == 3;
  if (f & O_CREAT)     o |= 0x200;
  if (f & O_EXCL)      o |= 0x800;
  if (f & O_NOCTTY)    o |= 0x8000;
  if (f & O_TRUNC)     o |= 0x400;
  if (f & O_APPEND)    o |= 8;
  if (f & O_NONBLOCK)  o |= 4;
  if (f & O_DIRECTORY) o |= nb ? 0x200000 : 0x20000;
  if (f & O_NOFOLLOW)  o |= 0x100;
  if (f & O_CLOEXEC)   o |= nb ? 0x400000 : 0x100000;
  return o; }
long __ai_ocan(long f) {
  long o = f & 3, nb = __ai_osv == 3;
  if (f & 0x200)    o |= O_CREAT;
  if (f & 0x800)    o |= O_EXCL;
  if (f & 0x8000)   o |= O_NOCTTY;
  if (f & 0x400)    o |= O_TRUNC;
  if (f & 8)        o |= O_APPEND;
  if (f & 4)        o |= O_NONBLOCK;
  if (f & (nb ? 0x200000 : 0x20000))  o |= O_DIRECTORY;
  if (f & 0x100)    o |= O_NOFOLLOW;
  if (f & (nb ? 0x400000 : 0x100000)) o |= O_CLOEXEC;
  return o; }

/* mmap flags: SHARED 1 / PRIVATE 2 / FIXED 0x10 agree; ANON moves to 0x1000;
 * POPULATE and the other linux hints have no twin and drop. */
long __ai_mapfb(long f) {
  long o = f & 0x13;
  if (f & MAP_ANON) o |= 0x1000;
  return o; }

/* sa_flags (stable/14: ONSTACK 1, RESTART 2, RESETHAND 4, NOCLDSTOP 8,
 * NODEFER 16, NOCLDWAIT 32, SIGINFO 64) */
long __ai_safb(long f) {
  long o = 0;
  if (f & SA_ONSTACK)   o |= 1;
  if (f & SA_RESTART)   o |= 2;
  if (f & SA_RESETHAND) o |= 4;
  if (f & SA_NOCLDSTOP) o |= 8;
  if (f & SA_NODEFER)   o |= 16;
  if (f & SA_NOCLDWAIT) o |= 32;
  if (f & SA_SIGINFO)   o |= 64;
  return o; }
long __ai_sacan(long f) {
  long o = 0;
  if (f & 1)  o |= SA_ONSTACK;
  if (f & 2)  o |= SA_RESTART;
  if (f & 4)  o |= SA_RESETHAND;
  if (f & 8)  o |= SA_NOCLDSTOP;
  if (f & 16) o |= SA_NODEFER;
  if (f & 32) o |= SA_NOCLDWAIT;
  if (f & 64) o |= SA_SIGINFO;
  return o; }

/* termios, whole (stable/14 sys/termios.h vs linux's asm-generic). each row
 * is {canonical bit, freebsd bit}; a bit in neither table DROPS on the way
 * through -- the exotic locals do not round-trip, the named surface does.
 * cc[] moves by the index row (VMIN/VTIME are counts and ride the same map);
 * the speed words copy verbatim (freebsd speaks plain baud, nothing here
 * reads them, and a round trip through one kernel preserves them). */
static unsigned int const os_tio_i[][2] = {   /* c_iflag */
  {1, 1}, {2, 2}, {4, 4}, {8, 8}, {16, 16}, {32, 32}, {64, 64}, {128, 128},
  {256, 256}, {0x400, 0x200} /* IXON */, {0x1000, 0x400} /* IXOFF */,
  {0x800, 0x800}, {0x2000, 0x2000} };
static unsigned int const os_tio_o[][2] = {   /* c_oflag */
  {1, 1} /* OPOST */, {4, 2} /* ONLCR */ };
static unsigned int const os_tio_c[][2] = {   /* c_cflag: freebsd is linux<<4 */
  {0x10, 0x100}, {0x20, 0x200} /* CSIZE */, {0x40, 0x400} /* CSTOPB */,
  {0x80, 0x800} /* CREAD */, {0x100, 0x1000} /* PARENB */,
  {0x200, 0x2000} /* PARODD */, {0x400, 0x4000} /* HUPCL */,
  {0x800, 0x8000} /* CLOCAL */, {0x80000000, 0x30000} /* CRTSCTS */ };
static unsigned int const os_tio_l[][2] = {   /* c_lflag */
  {1, 0x80} /* ISIG */, {2, 0x100} /* ICANON */, {8, 8} /* ECHO */,
  {0x10, 2} /* ECHOE */, {0x20, 4} /* ECHOK */, {0x40, 0x10} /* ECHONL */,
  {0x80, 0x80000000} /* NOFLSH */, {0x100, 0x400000} /* TOSTOP */,
  {0x200, 0x40} /* ECHOCTL */, {0x400, 0x20} /* ECHOPRT */,
  {0x800, 1} /* ECHOKE */, {0x1000, 0x800000} /* FLUSHO */,
  {0x4000, 0x20000000} /* PENDIN */, {0x8000, 0x400} /* IEXTEN */ };
static signed char const os_tio_cc[17] = {    /* canonical index -> freebsd's */
  8, 9, 3, 5, 0, 17, 16, -1, 12, 13, 10, 1, 6, 15, 4, 14, 2 };
static unsigned int os_tiow(unsigned int v, unsigned int const (*row)[2], int n, int can) {
  unsigned int o = 0;
  for (int i = 0; i < n; i++)
    if (v & row[i][can ? 1 : 0]) o |= row[i][can ? 0 : 1];
  return o; }
void __ai_tiofb(struct termios const *t, struct __fb_termios *f) {
  memset(f, 0, sizeof *f);
  f->c_iflag = os_tiow(t->c_iflag, os_tio_i, sizeof os_tio_i / 8, 0);
  f->c_oflag = os_tiow(t->c_oflag, os_tio_o, sizeof os_tio_o / 8, 0);
  f->c_cflag = os_tiow(t->c_cflag, os_tio_c, sizeof os_tio_c / 8, 0);
  f->c_lflag = os_tiow(t->c_lflag, os_tio_l, sizeof os_tio_l / 8, 0);
  for (int i = 0; i < 17; i++)
    if (os_tio_cc[i] >= 0) f->c_cc[(int) os_tio_cc[i]] = t->c_cc[i];
  /* netbsd spells CRTSCTS as the low bit alone; the high one is foreign there */
  if (__ai_osv == 3 && (f->c_cflag & 0x30000)) f->c_cflag = (f->c_cflag & ~0x30000u) | 0x10000;
  f->c_ispeed = t->c_ispeed; f->c_ospeed = t->c_ospeed; }
void __ai_tiocan(struct __fb_termios const *f, struct termios *t) {
  memset(t, 0, sizeof *t);
  t->c_iflag = os_tiow(f->c_iflag, os_tio_i, sizeof os_tio_i / 8, 1);
  t->c_oflag = os_tiow(f->c_oflag, os_tio_o, sizeof os_tio_o / 8, 1);
  t->c_cflag = os_tiow(f->c_cflag, os_tio_c, sizeof os_tio_c / 8, 1);
  t->c_lflag = os_tiow(f->c_lflag, os_tio_l, sizeof os_tio_l / 8, 1);
  for (int i = 0; i < 17; i++)
    if (os_tio_cc[i] >= 0) t->c_cc[i] = f->c_cc[(int) os_tio_cc[i]];
  t->c_ispeed = f->c_ispeed; t->c_ospeed = f->c_ospeed; }

/* the socket family: unix and inet agree, inet6 moves (10 -> 28). the BSD
 * sockaddr fronts a length byte where linux's 16-bit family sits: __ai_sain
 * rebuilds the head into the caller's scratch, __ai_saout folds a kernel-
 * filled head back in place. freebsd's copyin rewrites sa_len from the
 * syscall's namelen, so the length byte is set only for form. */
long __ai_affb(long a) { return a == 10 ? (__ai_osv == 3 ? 24 : 28) : a; }
long __ai_afcan(long a) { return (a == 28 || a == 24) ? 10 : a; }
unsigned int __ai_sain(void const *a, unsigned int n, void *out) {
  unsigned char const *s = a;
  unsigned char *b = out;
  memcpy(out, a, n);
  if (n >= 2) {
    long fam = s[0] | s[1] << 8;
    /* a unix name is as long as its path: linux's 110-byte struct rides whole,
     * freebsd refuses anything past its own 106 -- so the length follows the
     * string, the portable spelling both kernels take. */
    if (fam == 1 && n > 2) {
      unsigned int k = 2;
      while (k < n && b[k]) k++;
      n = k + (k < n); }
    b[0] = (unsigned char) n;
    b[1] = (unsigned char) __ai_affb(fam); }
  return n; }
void __ai_saout(void *a, unsigned int n) {
  unsigned char *b = a;
  if (n >= 2) {
    unsigned int fam = (unsigned int) __ai_afcan(b[1]);
    b[0] = (unsigned char) (fam & 255);
    b[1] = (unsigned char) (fam >> 8); } }
/* socket type: STREAM/DGRAM agree; the two flag bits move high
 * (stable/14: SOCK_CLOEXEC 0x10000000, SOCK_NONBLOCK 0x20000000) */
long __ai_sotype(long t) {
  long o = t & 0xff;
  if (t & 0x800)   o |= 0x20000000;
  if (t & 0x80000) o |= 0x10000000;
  return o; }
/* send/recv flags: the named bits translate, the rest drop */
long __ai_msgfb(long f) {
  long o = f & 3;                       /* OOB 1, PEEK 2 agree */
  if (f & 0x40)   o |= 0x80;            /* MSG_DONTWAIT */
  if (f & 0x100)  o |= 0x40;            /* MSG_WAITALL */
  if (f & 0x4000) o |= __ai_osv == 3 ? 0x400 : 0x20000;   /* MSG_NOSIGNAL */
  return o; }
/* recvmsg's msg_flags come back the other way */
long __ai_msgcan(long f) {
  long o = f & 1;                       /* MSG_OOB agrees */
  if (f & 0x8)  o |= 0x80;              /* MSG_EOR */
  if (f & 0x10) o |= 0x20;              /* MSG_TRUNC */
  if (f & 0x20) o |= 0x8;               /* MSG_CTRUNC */
  return o; }
/* sockopt: SOL_SOCKET moves whole (1 -> 0xffff) and its names permute; the
 * IPPROTO_* levels ride (TCP_NODELAY 1 = 1). only what sys/socket.h spells
 * has a row; an unmapped name refuses loudly upstream, never a silently
 * different option. 0 ok, -1 unknown. */
int __ai_sofb(long *lv, long *op) {
  if (*lv != 1) return 0;
  *lv = 0xffff;
  switch (*op) {
    case 2: *op = 4; return 0;          /* SO_REUSEADDR */
    case 4: *op = 0x1007; return 0;     /* SO_ERROR */
    case 9: *op = 8; return 0; }        /* SO_KEEPALIVE */
  return -1; }
#endif
