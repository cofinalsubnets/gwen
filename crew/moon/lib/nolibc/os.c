/* crew/moon/lib/nolibc/os.c -- the kernel under one binary (seed-universal
 * rungs UV1-UV2). one x86_64 build runs linux and freebsd: __ai_osdetect asks
 * the kernel which it is (once, at entry or lazily under __ai_call), and
 * numbers, errnos, signals, masks and flag words translate through the
 * tables here. the arm lanes have one kernel today: identity stubs. */
#include "impl.h"

long __ai_osv;                    /* 0 unprobed; 1 linux; 2 freebsd */

long __ai_osdetect(void) {
#if defined(__aarch64__) || defined(__riscv)
  return 1;                       /* one kernel per arch today */
#else
  /* 20 is getpid on freebsd and writev on linux: writev(-1, NULL, 0) is
   * -EBADF, a pid is positive, and neither kernel is disturbed by asking. */
  long r = __ai_sys(20, -1, 0, 0, 0, 0, 0);
  return r > 0 ? 2 : 1;
#endif
}

#if defined(__aarch64__) || defined(__riscv)
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
#else
/* canonical (linux x86_64) -> freebsd, sorted by canonical. a pair rides
 * here only when the members speak the call correctly on both kernels --
 * same shape, or a body whose freebsd branch builds the freebsd shape and
 * hands the number through this door. a call neither mapped nor branched
 * answers ENOSYS loudly (mount, sendfile, and linux's own mechanisms:
 * clone, dup3, signalfd4, memfd_create, unshare). */
static short const os_nr[][2] = {
  {NR_read,          NR_fb_read},
  {NR_write,         NR_fb_write},
  {NR_close,         NR_fb_close},
  {NR_fstat,         NR_fb_fstat},        /* the member fills __fb_stat */
  {NR_lseek,         NR_fb_lseek},
  {NR_mmap,          NR_fb_mmap},
  {NR_mprotect,      NR_fb_mprotect},
  {NR_munmap,        NR_fb_munmap},
  {NR_rt_sigaction,  NR_fb_rt_sigaction},   /* the member builds __fb_sigact */
  {NR_rt_sigprocmask, NR_fb_rt_sigprocmask},/* ..and the 16-byte set + how+1 */
  {NR_ioctl,         NR_fb_ioctl},          /* the member translates requests */
  {NR_pread64,       NR_fb_pread64},
  {NR_pwrite64,      NR_fb_pwrite64},
  {NR_madvise,       NR_fb_madvise},
  {NR_nanosleep,     NR_fb_nanosleep},
  {NR_getpid,        NR_fb_getpid},
  {NR_socket,        NR_fb_socket},
  {NR_connect,       NR_fb_connect},
  {NR_accept,        NR_fb_accept},
  {NR_sendto,        NR_fb_sendto},
  {NR_recvfrom,      NR_fb_recvfrom},
  {NR_sendmsg,       NR_fb_sendmsg},
  {NR_recvmsg,       NR_fb_recvmsg},
  {NR_shutdown,      NR_fb_shutdown},
  {NR_bind,          NR_fb_bind},
  {NR_listen,        NR_fb_listen},
  {NR_getsockname,   NR_fb_getsockname},
  {NR_getpeername,   NR_fb_getpeername},
  {NR_setsockopt,    NR_fb_setsockopt},
  {NR_getsockopt,    NR_fb_getsockopt},
  {57,               NR_fb_fork},    /* fork: linux's fork.c rides clone (56,
                                      * unmapped); a freebsd branch calls 57 */
  {NR_execve,        NR_fb_execve},
  {60,               NR_fb_exit},    /* exit and exit_group are one act here */
  {NR_wait4,         NR_fb_wait4},
  {NR_kill,          NR_fb_kill},
  {NR_fcntl,         NR_fb_fcntl},
  {NR_fsync,         NR_fb_fsync},
  {NR_fdatasync,     NR_fb_fdatasync},
  {NR_ftruncate,     NR_fb_ftruncate},
  {NR_getcwd,        NR_fb_getcwd},         /* __getcwd fills and answers 0; both faces fill */
  {NR_chdir,         NR_fb_chdir},
  {NR_fchmod,        NR_fb_fchmod},
  {NR_fchown,        NR_fb_fchown},
  {NR_umask,         NR_fb_umask},
  {NR_getuid,        NR_fb_getuid},
  {NR_getgid,        NR_fb_getgid},
  {NR_setuid,        NR_fb_setuid},
  {NR_setgid,        NR_fb_setgid},
  {NR_geteuid,       NR_fb_geteuid},
  {NR_setpgid,       NR_fb_setpgid},
  {NR_setsid,        NR_fb_setsid},
  {NR_setgroups,     NR_fb_setgroups},
  {NR_getpgid,       NR_fb_getpgid},
  {NR_chroot,        NR_fb_chroot},
  {NR_getdents64,    NR_fb_getdirentries},  /* the member repacks the record; basep rides arg 4 */
  {NR_clock_gettime, NR_fb_clock_gettime},
  {NR_exit_group,    NR_fb_exit},
  {NR_openat,        NR_fb_openat},
  {NR_mkdirat,       NR_fb_mkdirat},
  {NR_mknodat,       NR_fb_mknodat},
  {NR_fchownat,      NR_fb_fchownat},
  {NR_newfstatat,    NR_fb_newfstatat},     /* the member fills __fb_stat */
  {NR_unlinkat,      NR_fb_unlinkat},
  {NR_renameat,      NR_fb_renameat},
  {NR_linkat,        NR_fb_linkat},
  {NR_symlinkat,     NR_fb_symlinkat},
  {NR_readlinkat,    NR_fb_readlinkat},
  {NR_fchmodat,      NR_fb_fchmodat},
  {NR_faccessat,     NR_fb_faccessat},
  {NR_pselect6,      NR_fb_pselect6},       /* the member hands a bare sigset* 6th */
  {NR_ppoll,         NR_fb_ppoll},
  {NR_utimensat,     NR_fb_utimensat},
  {NR_pipe2,         NR_fb_pipe2},
};

long __ai_nrfb(long n) {
  for (unsigned i = 0; i < sizeof os_nr / sizeof *os_nr; i++) {
    if (os_nr[i][0] == n) return os_nr[i][1];
    if (os_nr[i][0] > n) break; }
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

long __ai_errfb(long e) {
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
 * freebsd (stable/14 sys/fcntl.h): NONBLOCK 4, APPEND 8, NOFOLLOW 0x100,
 * CREAT 0x200, TRUNC 0x400, EXCL 0x800, NOCTTY 0x8000, DIRECTORY 0x20000,
 * CLOEXEC 0x100000. */
long __ai_ofb(long f) {
  long o = f & 3;
  if (f & O_CREAT)     o |= 0x200;
  if (f & O_EXCL)      o |= 0x800;
  if (f & O_NOCTTY)    o |= 0x8000;
  if (f & O_TRUNC)     o |= 0x400;
  if (f & O_APPEND)    o |= 8;
  if (f & O_NONBLOCK)  o |= 4;
  if (f & O_DIRECTORY) o |= 0x20000;
  if (f & O_NOFOLLOW)  o |= 0x100;
  if (f & O_CLOEXEC)   o |= 0x100000;
  return o; }
long __ai_ocan(long f) {
  long o = f & 3;
  if (f & 0x200)    o |= O_CREAT;
  if (f & 0x800)    o |= O_EXCL;
  if (f & 0x8000)   o |= O_NOCTTY;
  if (f & 0x400)    o |= O_TRUNC;
  if (f & 8)        o |= O_APPEND;
  if (f & 4)        o |= O_NONBLOCK;
  if (f & 0x20000)  o |= O_DIRECTORY;
  if (f & 0x100)    o |= O_NOFOLLOW;
  if (f & 0x100000) o |= O_CLOEXEC;
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
#endif
