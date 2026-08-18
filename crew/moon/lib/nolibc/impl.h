/* crew/moon/lib/nolibc/impl.h -- the shared head of every nolibc member.
 * The members are pulled BY NEED (crew/moon/moon.l's runtime table globs this
 * directory the way it globs lib/math/), so a love that asks for no calendar
 * links no calendar. That is the whole reason this is a directory and not the
 * one file it used to be. ⚠ a member reaching another member's file-scope
 * static is what splitting costs: use the public spelling (errno, not
 * __errno_v), or move the state here. */
#ifndef AiNolibcImplH
#define AiNolibcImplH
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

/* ---- the syscall numbers. freebsd's table first, UNCONDITIONAL and named
 * NR_fb_*: it is machine-independent (stable/14 sys/sys/syscall.h) and feeds
 * two lanes -- the -os freebsd compile aliases NR_* to it below, and os.c's
 * runtime map pairs it with the canonical numbers so ONE binary answers both
 * kernels. ⚠ a canonical name ABSENT here is a MECHANISM that differs, not a
 * number we lack -- the member that wants it owes a freebsd body
 * (doc/plan/seed-universal.md rungs 4 / UV2). ---- */
#define NR_fb_read            3
#define NR_fb_write           4
#define NR_fb_close           6
#define NR_fb_fstat         551   /* ino64; ⚠ another struct stat (rung 3) */
#define NR_fb_lseek         478
#define NR_fb_nanosleep     240
#define NR_fb_mmap          477
#define NR_fb_mprotect       74
#define NR_fb_munmap         73
#define NR_fb_madvise        75
#define NR_fb_rt_sigaction  416   /* sigaction; ⚠ no restorer, another ksigaction (rung 3) */
#define NR_fb_rt_sigprocmask 340  /* sigprocmask; ⚠ 16-byte set, no size arg (rung 3) */
#define NR_fb_ioctl          54   /* ⚠ the numbers it takes are another encoding (rung 3) */
#define NR_fb_pread64       475
#define NR_fb_pwrite64      476
#define NR_fb_getpid         20
#define NR_fb_setuid         23
#define NR_fb_setgid        181
#define NR_fb_setgroups      80
#define NR_fb_geteuid        25
#define NR_fb_sendfile      393   /* ⚠ another signature (rung 3) */
#define NR_fb_pselect6      522   /* pselect; ⚠ the 6th arg is a plain sigset* (rung 3) */
#define NR_fb_socket         97
#define NR_fb_connect        98
#define NR_fb_accept         30
#define NR_fb_sendto        133
#define NR_fb_getsockname    32
#define NR_fb_getpeername    31
#define NR_fb_recvfrom       29
#define NR_fb_sendmsg        28
#define NR_fb_recvmsg        27
#define NR_fb_shutdown      134
#define NR_fb_bind          104
#define NR_fb_listen        106
#define NR_fb_getsockopt    118
#define NR_fb_setsockopt    105
#define NR_fb_fork            2   /* fork(2) is real here; fork.c forks on the OS */
#define NR_fb_execve         59
#define NR_fb_wait4           7
#define NR_fb_kill           37
#define NR_fb_fcntl          92
#define NR_fb_fsync          95
#define NR_fb_fdatasync     550
#define NR_fb_fchown        123
#define NR_fb_ftruncate     480
#define NR_fb_getcwd        326   /* __getcwd */
#define NR_fb_chdir          12
#define NR_fb_chroot         61
#define NR_fb_fchmod        124
#define NR_fb_umask          60
#define NR_fb_getuid         24
#define NR_fb_getgid         47
#define NR_fb_setpgid        82
#define NR_fb_setsid        147
#define NR_fb_getpgid       207
#define NR_fb_mount          21   /* ⚠ another signature -- (type dir flags data) (rung 3) */
#define NR_fb_getdirentries 554   /* ino64; the record IS the freebsd dirent (readdir.c) */
#define NR_fb___sysctl      202   /* sysctl(3)'s door (selfpath's KERN_PROC_PATHNAME) */
#define NR_fb_posix_openpt  504   /* a real syscall here; linux opens /dev/ptmx */
#define NR_fb_clock_gettime 232
#define NR_fb_exit            1   /* exit: one thread here, so one exit is the whole act */
#define NR_fb_openat        499
#define NR_fb_mkdirat       496
#define NR_fb_mknodat       559
#define NR_fb_fchownat      491
#define NR_fb_faccessat     489
#define NR_fb_newfstatat    552   /* fstatat; ⚠ another struct stat (rung 3) */
#define NR_fb_unlinkat      503
#define NR_fb_renameat      501
#define NR_fb_linkat        495
#define NR_fb_symlinkat     502
#define NR_fb_readlinkat    500
#define NR_fb_fchmodat      490
#define NR_fb_ppoll         545   /* 4 args; our trailing sigsetsize rides an ignored register */
/*      unshare: none -- linux's; the nif above it is already #else'd out */
#define NR_fb_utimensat     547
/*      signalfd4: none -- kqueue 362 / kevent 560, EVFILT_SIGNAL (rung 4) */
/*      dup3: none -- fcntl F_DUP2FD_CLOEXEC (dup2.c, rung 3) */
#define NR_fb_pipe2         542
/*      memfd_create: none -- shm_open2 571 + SHM_ANON (rung 3) */

/* ---- the NR_* the members say: linux's, the CANONICAL numbers -- one body
 * per member, and a freebsd runtime translates through os.c's map (a member's
 * freebsd branch reaches an unmappable call by NR_fb_* through fb0..fb6
 * below). linux's one arch gate: riscv64 shares aarch64's asm-generic table
 * verbatim. ---- */
#if defined(__aarch64__) || defined(__riscv)
#define NR_getcwd          17
#define NR_dup3            24
#define NR_fcntl           25
#define NR_ioctl           29
#define NR_mkdirat         34
#define NR_mknodat         33
#define NR_unlinkat        35
#define NR_nanosleep      101
#define NR_setgid         144
#define NR_setgroups      159
#define NR_setuid         146
#define NR_geteuid        175
#define NR_symlinkat       36
#define NR_linkat          37
#define NR_renameat        38
#define NR_mount           40
#define NR_ftruncate       46
#define NR_chdir           49
#define NR_chroot          51
#define NR_fchmod          52
#define NR_fchmodat        53
#define NR_fchownat        54
#define NR_faccessat       48
#define NR_openat          56
#define NR_close           57
#define NR_pipe2           59
#define NR_getdents64      61
#define NR_lseek           62
#define NR_fchown          55
#define NR_read            63
#define NR_write           64
#define NR_pread64         67
#define NR_pwrite64        68
#define NR_ppoll           73
#define NR_signalfd4       74
#define NR_readlinkat      78
#define NR_newfstatat      79
#define NR_fstat           80
#define NR_fsync           82
#define NR_fdatasync       83
#define NR_utimensat       88
#define NR_exit_group      94
#define NR_unshare         97
#define NR_clock_gettime  113
#define NR_kill           129
#define NR_rt_sigaction   134
#define NR_rt_sigprocmask 135
#define NR_setpgid        154
#define NR_getpgid        155
#define NR_setsid         157
#define NR_umask          166
#define NR_getpid         172
#define NR_getuid         174
#define NR_getgid         176
#define NR_sendfile        71
#define NR_pselect6        72
#define NR_socket         198
#define NR_bind           200
#define NR_getsockname    204
#define NR_getpeername    205
#define NR_listen         201
#define NR_accept         202
#define NR_connect        203
#define NR_sendto         206
#define NR_recvfrom       207
#define NR_getsockopt     209
#define NR_setsockopt     208
#define NR_shutdown       210
#define NR_sendmsg        211
#define NR_recvmsg        212
#define NR_munmap         215
#define NR_clone          220
#define NR_execve         221
#define NR_mmap           222
#define NR_mprotect       226
#define NR_madvise        233
#define NR_wait4          260
#define NR_memfd_create   279
#else
#define NR_read             0
#define NR_write            1
#define NR_close            3
#define NR_fstat            5
#define NR_lseek            8
#define NR_nanosleep       35
#define NR_mmap             9
#define NR_mprotect        10
#define NR_munmap          11
#define NR_madvise         28
#define NR_rt_sigaction    13
#define NR_rt_sigprocmask  14
#define NR_ioctl           16
#define NR_pread64         17
#define NR_pwrite64        18
#define NR_getpid          39
#define NR_setuid         105
#define NR_setgid         106
#define NR_setgroups      116
#define NR_geteuid        107
#define NR_sendfile        40
#define NR_pselect6       270
#define NR_socket          41
#define NR_connect         42
#define NR_accept          43
#define NR_sendto          44
#define NR_getsockname     51
#define NR_getpeername     52
#define NR_recvfrom        45
#define NR_sendmsg         46
#define NR_recvmsg         47
#define NR_shutdown        48
#define NR_bind            49
#define NR_listen          50
#define NR_getsockopt      55
#define NR_setsockopt      54
#define NR_clone           56
#define NR_execve          59
#define NR_wait4           61
#define NR_kill            62
#define NR_fcntl           72
#define NR_fsync           74
#define NR_fdatasync       75
#define NR_fchown          93
#define NR_ftruncate       77
#define NR_getcwd          79
#define NR_chdir           80
#define NR_chroot         161
#define NR_fchmod          91
#define NR_umask           95
#define NR_getuid         102
#define NR_getgid         104
#define NR_setpgid        109
#define NR_setsid         112
#define NR_getpgid        121
#define NR_mount          165
#define NR_getdents64     217
#define NR_clock_gettime  228
#define NR_exit_group     231
#define NR_openat         257
#define NR_mkdirat        258
#define NR_mknodat        259
#define NR_fchownat       260
#define NR_faccessat      269
#define NR_newfstatat     262
#define NR_unlinkat       263
#define NR_renameat       264
#define NR_linkat         265
#define NR_symlinkat      266
#define NR_readlinkat     267
#define NR_fchmodat       268
#define NR_ppoll          271
#define NR_unshare        272
#define NR_utimensat      280
#define NR_signalfd4      289
#define NR_dup3           292
#define NR_pipe2          293
#define NR_memfd_create   319
#endif


/* errno is ONE object; every member only reads it, so it rides extern here and is
 * defined once beside __errno_location. */
extern int __errno_v;
static long er(long r) {
  if ((unsigned long) r > (unsigned long) -4096L) { __errno_v = (int) -r; return -1; }
  return r; }

/* ---- one syscall door under one binary (seed-universal rungs UV1-UV2).
 * __ai_sys is the raw OS-blind tail (sys.o): CF cleared going in, and a carry
 * answer -- freebsd's error convention -- comes back parked BELOW linux's
 * band as -(errno+4096), so the kernels' answers cannot collide. __ai_osv is
 * the kernel under us (os.c probes it once: 1 linux, 2 freebsd); __ai_call
 * translates numbers by os.c's map and errnos by its row. every member wears
 * ONE body: the canonical (linux-valued) face, with a freebsd branch on
 * __ai_osv where the shapes part -- fb0..fb6 reach the calls the map cannot
 * carry, by their NR_fb_* number, errno translated the same. */
extern long __ai_osv;
extern long __ai_osdetect(void);
extern long __ai_nrfb(long n);
extern long __ai_errfb(long e);
extern long __ai_sigfb(long sig);
extern long __ai_sigcan(long sig);
extern unsigned long __ai_maskfb(unsigned long m);
extern unsigned long __ai_maskcan(unsigned long m);
extern long __ai_ofb(long fl);
extern long __ai_ocan(long fl);
extern long __ai_mapfb(long fl);
extern long __ai_safb(long fl);
extern long __ai_sacan(long fl);
static long __ai_call(long n, long a, long b, long c, long d, long e, long f) {
  long v = __ai_osv;
  if (!v) v = __ai_osv = __ai_osdetect();
  if (v == 2) {
    n = __ai_nrfb(n);
    if (n < 0) return -38; }                          /* ENOSYS, canonically */
  long r = __ai_sys(n, a, b, c, d, e, f);
  return r < -4096L ? -__ai_errfb(-r - 4096) : r; }
static long __ai_fb(long n, long a, long b, long c, long d, long e, long f) {
  long r = __ai_sys(n, a, b, c, d, e, f);
  return r < -4096L ? -__ai_errfb(-r - 4096) : r; }

static long sc0(long n) { return __ai_call(n, 0, 0, 0, 0, 0, 0); }
static long sc1(long n, long a) { return __ai_call(n, a, 0, 0, 0, 0, 0); }
static long sc2(long n, long a, long b) { return __ai_call(n, a, b, 0, 0, 0, 0); }
static long sc3(long n, long a, long b, long c) { return __ai_call(n, a, b, c, 0, 0, 0); }
static long sc4(long n, long a, long b, long c, long d) { return __ai_call(n, a, b, c, d, 0, 0); }
static long sc5(long n, long a, long b, long c, long d, long e) { return __ai_call(n, a, b, c, d, e, 0); }
static long sc6(long n, long a, long b, long c, long d, long e, long f) { return __ai_call(n, a, b, c, d, e, f); }
static long fb1(long n, long a) { return __ai_fb(n, a, 0, 0, 0, 0, 0); }
static long fb2(long n, long a, long b) { return __ai_fb(n, a, b, 0, 0, 0, 0); }
static long fb3(long n, long a, long b, long c) { return __ai_fb(n, a, b, c, 0, 0, 0); }
static long fb6(long n, long a, long b, long c, long d, long e, long f) { return __ai_fb(n, a, b, c, d, e, f); }
/* ⚠ er, sc0..sc6 and fb1..fb6 are static IN A HEADER on purpose: a member inlines
 * the ones it uses and the dead-static sweep drops the bodies it did not need, so
 * the rest cost nothing. Before that sweep this shape would have been duplication
 * in every TU. */

/* the shapes the public headers keep opaque, and the fmt members' limb geometry. */
typedef void (*__exitfn)(void);
typedef struct __mhdr { struct __mhdr *next; size_t size; } __mhdr;   /* size in units */
typedef struct __ablk { struct __ablk *next; char *mark; } __ablk;
struct __sctx { char *p; size_t n, at; };
struct __ksigaction { void *h; unsigned long flags; void *restorer; unsigned long mask; };
/* `ent` is the freebsd repack slot: getdirentries' record is another shape,
 * so readdir translates the current one here and hands this out instead. */
struct __dirstream { int fd; int pos; int len; struct dirent ent; char buf[4096]; };

/* ---- freebsd's kernel shapes, the twins a member's freebsd branch fills and
 * translates (stable/14; the canonical faces live in the public headers). ---- */
struct __fb_stat {                    /* ino64, 224 bytes; mode is 16-BIT */
  unsigned long st_dev;
  unsigned long st_ino;
  unsigned long st_nlink;
  unsigned short st_mode;
  short         st_bsdflags;
  unsigned int  st_uid;
  unsigned int  st_gid;
  int           __pad0;
  unsigned long st_rdev;
  struct timespec st_atim;
  struct timespec st_mtim;
  struct timespec st_ctim;
  struct timespec st_birthtim;
  long          st_size;
  long          st_blocks;
  int           st_blksize;
  unsigned int  st_flags;
  unsigned long st_gen;
  unsigned long st_filerev;
  unsigned long __spare[9];
};
struct __fb_dirent {                  /* the ino64 record getdirentries fills */
  unsigned long  d_ino;
  long           d_off;
  unsigned short d_reclen;
  unsigned char  d_type;
  unsigned char  __pad0;
  unsigned short d_namlen;
  unsigned short __pad1;
  char           d_name[256];
};
struct __fb_sigact { void *h; int flags; unsigned int mask[4]; };   /* sigaction(416): no restorer */
struct __fb_termios {                 /* 44 bytes: 4 flag words, 20 chars, 2 speeds, no c_line */
  unsigned int c_iflag, c_oflag, c_cflag, c_lflag;
  unsigned char c_cc[20];
  unsigned int c_ispeed, c_ospeed;
};
extern void __ai_fbstat(struct __fb_stat const *f, struct stat *st);   /* fstat.c's, shared by the stat trio */
extern void __ai_tiofb(struct termios const *t, struct __fb_termios *f);   /* os.c's termios rows */
extern void __ai_tiocan(struct __fb_termios const *f, struct termios *t);
#define FfLeft 1
#define FfZero 2
#define FfAlt  4
#define FfPlus 8
#define FfSpc  16
#define BdB 1000000000UL
#define BdI 36                        /* integer limbs: 324 digits >= 309 */
#define BdF 121                       /* fraction limbs: 1089 digits >= 1074 */
#define BdN (BdI + BdF)
#define BdD (BdN * 9)                /* every digit index the array holds */
#define BdU (BdI * 9 - 1)            /* the index of the units place */

extern char **environ;
extern char const *__ai_progname;

/* helpers a sibling member calls or passes as a pointer: one definition, named here. */
void __fmtnum(void (*put)(void *, int), void *ctx, unsigned long v, unsigned base,
              int neg, int width, int fl, int up);
int __fmtsgn(int neg, int fl);
void __pad(void (*put)(void *, int), void *ctx, int n, int ch);
void __femit(void *ctx, int c);
void __semit(void *ctx, int c);

/* the three standard streams' storage: core.c's entry wires stdout's buffer, and
 * the stream table is named by whoever opens one. */
extern FILE __stdf[3];
extern unsigned char __obuf[8192];
#endif
