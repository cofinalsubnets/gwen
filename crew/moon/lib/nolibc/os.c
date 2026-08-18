/* crew/moon/lib/nolibc/os.c -- the kernel under one binary (seed-universal
 * rung UV1). one x86_64 build runs linux and freebsd: __ai_osdetect asks the
 * kernel which it is (once, at entry or lazily under __ai_call), and the
 * canonical lane's numbers and errnos translate through the tables here.
 * the arm lanes and the -os freebsd lane have one kernel each: stubs. */
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

#if defined(__aarch64__) || defined(__riscv) || defined(__FreeBSD__)
long __ai_nrfb(long n) { return n; }      /* no second kernel on this lane */
long __ai_errfb(long e) { return e; }
#else
/* canonical (linux x86_64) -> freebsd, sorted by canonical. a pair rides
 * here ONLY when the call's shape agrees on both kernels; a call whose
 * geometry diverges (another struct, another arg order) is left OUT so the
 * canonical lane answers ENOSYS loudly instead of calling it wrong --
 * UV2 lands each runtime body, then its pair:
 *   fstat 5 / newfstatat 262 (another struct stat), rt_sigaction 13 (no
 *   restorer, another ksigaction), rt_sigprocmask 14 (16-byte set, no size),
 *   ioctl 16 (another encoding), sendfile 40 / pselect6 270 / mount 165
 *   (another signature), getcwd 79 (__getcwd fills, answers 0),
 *   getdents64 217 (getdirentries, another record).
 * ⚠ mapped is not yet TRANSLATED: open/mmap/fcntl flag values, sockaddr's
 * sa_len, clockids and the diverging signal numbers still speak linux --
 * each owes UV2 a value row before the whole love can ride this lane. */
static short const os_nr[][2] = {
  {NR_read,          NR_fb_read},
  {NR_write,         NR_fb_write},
  {NR_close,         NR_fb_close},
  {NR_lseek,         NR_fb_lseek},
  {NR_mmap,          NR_fb_mmap},
  {NR_mprotect,      NR_fb_mprotect},
  {NR_munmap,        NR_fb_munmap},
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
  {NR_clock_gettime, NR_fb_clock_gettime},
  {NR_exit_group,    NR_fb_exit},
  {NR_openat,        NR_fb_openat},
  {NR_mkdirat,       NR_fb_mkdirat},
  {NR_mknodat,       NR_fb_mknodat},
  {NR_fchownat,      NR_fb_fchownat},
  {NR_unlinkat,      NR_fb_unlinkat},
  {NR_renameat,      NR_fb_renameat},
  {NR_linkat,        NR_fb_linkat},
  {NR_symlinkat,     NR_fb_symlinkat},
  {NR_readlinkat,    NR_fb_readlinkat},
  {NR_fchmodat,      NR_fb_fchmodat},
  {NR_faccessat,     NR_fb_faccessat},
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
#endif
