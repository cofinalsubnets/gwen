/* crew/moon/lib/nolibc.c -- the raw-syscall libc under the gcc-free love (rung 4).
 * Everything the host objects ask of glibc, answered straight off the Linux
 * syscall table through __ai_sys (crew/moon/lib/mksys.l lays that leaf,
 * with __sigsetjmp/siglongjmp/__ai_sigret beside it; crew/moon/lib/math/
 * carries the math floor, crew/moon/lib/math/am.c -- ours). One file, mooncc-compiled, our own linker binds it:
 *   mooncc love.o (host objects) nolibc.o (math objects) sys.o -o love
 * Three arches, one body: every call below speaks the modern forms both tables
 * carry (openat / newfstatat / ppoll / pipe2 / dup3 / clone / the *at file
 * ops) -- the asm-generic table (aarch64 + riscv64, one shared block) dropped
 * the legacy names outright, so the NR block under this comment is the only
 * thing that gates. The shapes
 * MATCH crew/moon/include/: struct stat and dirent are the kernel layouts
 * verbatim (stat.h arch-gates the struct), termios rides TCGETS raw the way
 * musl does, and only sigaction needs a real translation (glibc's 152-byte
 * struct to the kernel's -- x86-64 supplies a restorer, aarch64's kernel
 * lays its own vdso return trampoline). Single-threaded by design, like
 * love itself: errno is one int, no locks anywhere. */
#include <stddef.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <setjmp.h>
#include <poll.h>
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

extern long __ai_sys(long n, long a, long b, long c, long d, long e, long f);
extern void __ai_sigret(void);
extern int main(int, char**);

/* ---- the syscall numbers, the one arch gate (riscv64 shares aarch64's
 * asm-generic table verbatim -- one flag, two arches) ---- */
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
#define NR_setuid         146
#define NR_geteuid        175
#define NR_symlinkat       36
#define NR_linkat          37
#define NR_renameat        38
#define NR_mount           40
#define NR_ftruncate       46
#define NR_chdir           49
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
#define NR_socket         198
#define NR_bind           200
#define NR_listen         201
#define NR_accept         202
#define NR_connect        203
#define NR_sendto         206
#define NR_recvfrom       207
#define NR_setsockopt     208
#define NR_shutdown       210
#define NR_sendmsg        211
#define NR_recvmsg        212
#define NR_munmap         215
#define NR_clone          220
#define NR_execve         221
#define NR_mmap           222
#define NR_mprotect       226
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
#define NR_rt_sigaction    13
#define NR_rt_sigprocmask  14
#define NR_ioctl           16
#define NR_pread64         17
#define NR_pwrite64        18
#define NR_getpid          39
#define NR_setuid         105
#define NR_setgid         106
#define NR_geteuid        107
#define NR_socket          41
#define NR_connect         42
#define NR_accept          43
#define NR_sendto          44
#define NR_recvfrom        45
#define NR_sendmsg         46
#define NR_recvmsg         47
#define NR_shutdown        48
#define NR_bind            49
#define NR_listen          50
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

char **environ;
static long *__auxv;

/* ---- errno: one int (love is single-threaded), kernel -errno unwrapped ---- */
static int __errno_v;
int *__errno_location(void) { return &__errno_v; }
static long er(long r) {
  if ((unsigned long) r > (unsigned long) -4096L) { __errno_v = (int) -r; return -1; }
  return r; }

static long sc0(long n) { return __ai_sys(n, 0, 0, 0, 0, 0, 0); }
static long sc1(long n, long a) { return __ai_sys(n, a, 0, 0, 0, 0, 0); }
static long sc2(long n, long a, long b) { return __ai_sys(n, a, b, 0, 0, 0, 0); }
static long sc3(long n, long a, long b, long c) { return __ai_sys(n, a, b, c, 0, 0, 0); }
static long sc4(long n, long a, long b, long c, long d) { return __ai_sys(n, a, b, c, d, 0, 0); }
static long sc5(long n, long a, long b, long c, long d, long e) { return __ai_sys(n, a, b, c, d, e, 0); }
static long sc6(long n, long a, long b, long c, long d, long e, long f) { return __ai_sys(n, a, b, c, d, e, f); }

/* ---- memory/string: word-wide where the pointers agree (the GC image and
 * string lanes move real volume through these) ---- */
void *memcpy(void *d, void const *s, size_t n) {
  unsigned char *dp = d; unsigned char const *sp = s;
  if ((((unsigned long) dp | (unsigned long) sp) & 7) == 0)
    while (n >= 8) { *(unsigned long *) dp = *(unsigned long const *) sp; dp += 8; sp += 8; n -= 8; }
  while (n--) *dp++ = *sp++;
  return d; }
void *memmove(void *d, void const *s, size_t n) {
  unsigned char *dp = d; unsigned char const *sp = s;
  if (dp == sp || n == 0) return d;
  if (dp < sp) return memcpy(d, s, n);
  dp += n; sp += n;
  while (n--) *--dp = *--sp;
  return d; }
void *memset(void *d, int c, size_t n) {
  unsigned char *dp = d;
  unsigned char b = (unsigned char) c;
  unsigned long w = b; w |= w << 8; w |= w << 16; w |= w << 32;
  if (((unsigned long) dp & 7) == 0)
    while (n >= 8) { *(unsigned long *) dp = w; dp += 8; n -= 8; }
  while (n--) *dp++ = b;
  return d; }
int memcmp(void const *a, void const *b, size_t n) {
  unsigned char const *x = a, *y = b;
  while (n--) { if (*x != *y) return (int) *x - (int) *y; x++; y++; }
  return 0; }
size_t strlen(char const *s) { size_t n = 0; while (*s++) n++; return n; }
int strcmp(char const *a, char const *b) {
  while (*a && *a == *b) { a++; b++; }
  return (int) (unsigned char) *a - (int) (unsigned char) *b; }
int strncmp(char const *a, char const *b, size_t n) {
  while (n && *a && *a == *b) { a++; b++; n--; }
  return n ? (int) (unsigned char) *a - (int) (unsigned char) *b : 0; }
char *strcpy(char *d, char const *s) { char *r = d; while ((*d++ = *s++)) ; return r; }
char *strncpy(char *d, char const *s, size_t n) {
  char *r = d;
  while (n && *s) { *d++ = *s++; n--; }
  while (n) { *d++ = 0; n--; }
  return r; }
char *strcat(char *d, char const *s) {
  char *r = d;
  while (*d) d++;
  while ((*d++ = *s++)) ;
  return r; }
char *strrchr(char const *s, int c) {
  char const *last = 0;
  do { if (*s == (char) c) last = s; } while (*s++);
  return (char *) last; }
size_t strspn(char const *s, char const *set) {
  size_t n = 0;
  for (; s[n]; n++) { char const *p = set; while (*p && *p != s[n]) p++; if (!*p) break; }
  return n; }
size_t strcspn(char const *s, char const *set) {
  size_t n = 0;
  for (; s[n]; n++) { char const *p = set; while (*p && *p != s[n]) p++; if (*p) break; }
  return n; }
int isupper(int c) { return c >= 65 && c <= 90; }
int islower(int c) { return c >= 97 && c <= 122; }
int isalpha(int c) { return (c >= 65 && c <= 90) || (c >= 97 && c <= 122); }
int isdigit(int c) { return c >= 48 && c <= 57; }
int isalnum(int c) { return isalpha(c) || isdigit(c); }
int isxdigit(int c) { return isdigit(c) || (c >= 65 && c <= 70) || (c >= 97 && c <= 102); }
int isspace(int c) { return c == 32 || (c >= 9 && c <= 13); }
int isprint(int c) { return c >= 32 && c < 127; }
int iscntrl(int c) { return (c >= 0 && c < 32) || c == 127; }
int ispunct(int c) { return isprint(c) && c != 32 && !isalnum(c); }
int tolower(int c) { return (c >= 65 && c <= 90) ? c + 32 : c; }
int toupper(int c) { return (c >= 97 && c <= 122) ? c - 32 : c; }
int isgraph(int c) { return c > 32 && c < 127; }
int isblank(int c) { return c == 32 || c == 9; }
/* the two GNU string extensions the headers name: memcpy answering its END, and
 * memchr with no bound (the caller warrants the byte is there) */
void *mempcpy(void *d, void const *s, size_t n) { return (char *) memcpy(d, s, n) + n; }
void *rawmemchr(void const *p, int c) {
  unsigned char const *q = p;
  while (*q != (unsigned char) c) q++;
  return (void *) q; }
char *strchr(char const *s, int c) { for (;; s++) { if (*s == (char) c) return (char *) s; if (!*s) return 0; } }
/* the classic table (errno 1..34), the range real packages print; past it the
 * number speaks for itself. the texts are the canonical POSIX ones -- m4's
 * check suite string-compares "No such file or directory". */
static char const *const __errs[] = { 0,
  "Operation not permitted", "No such file or directory", "No such process",
  "Interrupted system call", "Input/output error", "No such device or address",
  "Argument list too long", "Exec format error", "Bad file descriptor",
  "No child processes", "Resource temporarily unavailable", "Cannot allocate memory",
  "Permission denied", "Bad address", "Block device required",
  "Device or resource busy", "File exists", "Invalid cross-device link",
  "No such device", "Not a directory", "Is a directory",
  "Invalid argument", "Too many open files in system", "Too many open files",
  "Inappropriate ioctl for device", "Text file busy", "File too large",
  "No space left on device", "Illegal seek", "Read-only file system",
  "Too many links", "Broken pipe", "Numerical argument out of domain",
  "Numerical result out of range" };
char *strerror(int e) {
  if (e > 0 && e <= 34) return (char *) __errs[e];
  static char b[24]; snprintf(b, sizeof b, "error %d", e); return b; }
int strcasecmp(char const *a, char const *b) {
  while (*a && tolower((unsigned char) *a) == tolower((unsigned char) *b)) { a++; b++; }
  return tolower((unsigned char) *a) - tolower((unsigned char) *b); }
int strncasecmp(char const *a, char const *b, size_t n) {
  while (n && *a && tolower((unsigned char) *a) == tolower((unsigned char) *b)) { a++; b++; n--; }
  return n ? tolower((unsigned char) *a) - tolower((unsigned char) *b) : 0; }
int strcoll(char const *a, char const *b) { return strcmp(a, b); }   /* the "C" locale IS strcmp */
char *strstr(char const *h, char const *n) {
  size_t nl = strlen(n);
  if (!nl) return (char *) h;
  for (; *h; h++)
    if (*h == *n && !strncmp(h, n, nl)) return (char *) h;
  return 0; }
char *strpbrk(char const *s, char const *set) {
  for (; *s; s++) if (strchr(set, *s)) return (char *) s;
  return 0; }
char *strtok(char *s, char const *sep) {
  static char *nxt;
  if (!s) s = nxt;
  if (!s) return 0;
  s += strspn(s, sep);
  if (!*s) { nxt = 0; return 0; }
  char *e = s + strcspn(s, sep);
  if (*e) { *e = 0; nxt = e + 1; } else nxt = 0;
  return s; }
char *strncat(char *d, char const *s, size_t n) {
  char *p = d + strlen(d);
  while (n-- && *s) *p++ = *s++;
  *p = 0;
  return d; }
char *strdup(char const *s) {
  size_t n = strlen(s) + 1;
  char *d = malloc(n);
  if (d) memcpy(d, s, n);
  return d; }
void *memchr(void const *p, int c, size_t n) {
  unsigned char const *s = p;
  for (; n; n--, s++) if (*s == (unsigned char) c) return (void *) s;
  return 0; }

/* ---- the plain syscall tail: one line each ---- */
long read(int fd, void *b, long n) { return er(sc3(NR_read, fd, (long) b, n)); }
long write(int fd, void const *b, long n) { return er(sc3(NR_write, fd, (long) b, n)); }
int close(int fd) { return (int) er(sc1(NR_close, fd)); }
long lseek(int fd, long off, int wh) { return er(sc3(NR_lseek, fd, off, wh)); }
int open(char const *p, int fl, ...) {
  va_list ap; va_start(ap, fl);
  int mode = va_arg(ap, int);
  va_end(ap);
  return (int) er(sc4(NR_openat, AT_FDCWD, (long) p, fl, mode)); }
int creat(char const *p, unsigned int mode) { return open(p, O_WRONLY | O_CREAT | O_TRUNC, (int) mode); }
int fcntl(int fd, int cmd, ...) {
  va_list ap; va_start(ap, cmd);
  long arg = va_arg(ap, long);
  va_end(ap);
  return (int) er(sc3(NR_fcntl, fd, cmd, arg)); }
int ioctl(int fd, unsigned long req, ...) {
  va_list ap; va_start(ap, req);
  long arg = va_arg(ap, long);
  va_end(ap);
  return (int) er(sc3(NR_ioctl, fd, (long) req, arg)); }
int stat(char const *p, struct stat *st) { return (int) er(sc4(NR_newfstatat, AT_FDCWD, (long) p, (long) st, 0)); }
int fstat(int fd, struct stat *st) { return (int) er(sc2(NR_fstat, fd, (long) st)); }
int lstat(char const *p, struct stat *st) { return (int) er(sc4(NR_newfstatat, AT_FDCWD, (long) p, (long) st, 256)); }   /* AT_SYMLINK_NOFOLLOW */
int poll(struct pollfd *fds, nfds_t n, int ms) {
  struct timespec ts;
  ts.tv_sec = ms / 1000;
  ts.tv_nsec = (long) (ms % 1000) * 1000000;
  return (int) er(sc5(NR_ppoll, (long) fds, (long) n, ms < 0 ? 0 : (long) &ts, 0, 8)); }
int pipe(int *fds) { return (int) er(sc2(NR_pipe2, (long) fds, 0)); }
int dup2(int a, int b) {
  if (a == b) return fcntl(a, F_GETFD, 0) < 0 ? -1 : b;   /* dup3 refuses a==b; dup2 answers b if a lives */
  return (int) er(sc3(NR_dup3, a, b, 0)); }
int getpid(void) { return (int) sc0(NR_getpid); }
unsigned int getuid(void) { return (unsigned int) sc0(NR_getuid); }
unsigned int getgid(void) { return (unsigned int) sc0(NR_getgid); }
int fork(void) { return (int) er(sc5(NR_clone, 17, 0, 0, 0, 0)); }   /* clone(SIGCHLD): the fork nobody dropped */
int waitpid(int pid, int *st, int opt) { return (int) er(sc4(NR_wait4, pid, (long) st, opt, 0)); }
int kill(pid_t pid, int sig) { return (int) er(sc2(NR_kill, pid, sig)); }
int raise(int sig) { return kill(getpid(), sig); }
int fsync(int fd) { return (int) er(sc1(NR_fsync, fd)); }
int fdatasync(int fd) { return (int) er(sc1(NR_fdatasync, fd)); }
int fchown(int fd, unsigned int u, unsigned int g) { return (int) er(sc3(NR_fchown, fd, u, g)); }
int getpagesize(void) { return 4096; }
int ftruncate(int fd, long n) { return (int) er(sc2(NR_ftruncate, fd, n)); }
char *getcwd(char *b, unsigned long n) {
  long r = sc2(NR_getcwd, (long) b, (long) n);
  if (r < 0) { __errno_v = (int) -r; return 0; }
  return b; }
int chdir(char const *p) { return (int) er(sc1(NR_chdir, (long) p)); }
int rename(char const *a, char const *b) { return (int) er(sc4(NR_renameat, AT_FDCWD, (long) a, AT_FDCWD, (long) b)); }
int mkdir(char const *p, unsigned int m) { return (int) er(sc3(NR_mkdirat, AT_FDCWD, (long) p, m)); }
int rmdir(char const *p) { return (int) er(sc3(NR_unlinkat, AT_FDCWD, (long) p, 512)); }   /* AT_REMOVEDIR */
int link(char const *a, char const *b) { return (int) er(sc5(NR_linkat, AT_FDCWD, (long) a, AT_FDCWD, (long) b, 0)); }
int unlink(char const *p) { return (int) er(sc3(NR_unlinkat, AT_FDCWD, (long) p, 0)); }
int symlink(char const *a, char const *b) { return (int) er(sc3(NR_symlinkat, (long) a, AT_FDCWD, (long) b)); }
long readlink(char const *p, char *b, unsigned long n) { return er(sc4(NR_readlinkat, AT_FDCWD, (long) p, (long) b, (long) n)); }
int chmod(char const *p, unsigned int m) { return (int) er(sc4(NR_fchmodat, AT_FDCWD, (long) p, m, 0)); }
int fchmod(int fd, unsigned int m) { return (int) er(sc2(NR_fchmod, fd, m)); }
int chown(char const *p, unsigned int u, unsigned int g) { return (int) er(sc5(NR_fchownat, AT_FDCWD, (long) p, u, g, 0)); }
int lchown(char const *p, unsigned int u, unsigned int g) { return (int) er(sc5(NR_fchownat, AT_FDCWD, (long) p, u, g, 256)); }   /* AT_SYMLINK_NOFOLLOW */
int access(char const *p, int m) { return (int) er(sc4(NR_faccessat, AT_FDCWD, (long) p, m, 0)); }
int dup(int fd) { return (int) er(sc3(NR_fcntl, fd, 0, 0)); }                          /* F_DUPFD */
unsigned int geteuid(void) { return (unsigned int) sc0(NR_geteuid); }
int setuid(unsigned int u) { return (int) er(sc1(NR_setuid, u)); }
int setgid(unsigned int g) { return (int) er(sc1(NR_setgid, g)); }
int mknod(char const *p, unsigned int mode, unsigned long dev) { return (int) er(sc4(NR_mknodat, AT_FDCWD, (long) p, mode, (long) dev)); }
int mkfifo(char const *p, unsigned int mode) { return mknod(p, mode | 4096U, 0); }     /* S_IFIFO = 010000 */
int wait(int *st) { return waitpid(-1, st, 0); }
int usleep(unsigned int us) {
  struct timespec ts;
  ts.tv_sec = us / 1000000;
  ts.tv_nsec = (long) (us % 1000000) * 1000;
  return (int) er(sc2(NR_nanosleep, (long) &ts, 0)); }
int nanosleep(struct timespec const *req, struct timespec *rem) {
  return (int) er(sc2(NR_nanosleep, (long) req, (long) rem)); }
time_t time(time_t *t) {
  struct timespec ts;
  clock_gettime(0, &ts);                       /* CLOCK_REALTIME */
  if (t) *t = ts.tv_sec;
  return ts.tv_sec; }
unsigned int umask(unsigned int m) { return (unsigned int) sc1(NR_umask, m); }
int setpgid(pid_t p, pid_t g) { return (int) er(sc2(NR_setpgid, p, g)); }
int getpgrp(void) { return (int) sc1(NR_getpgid, 0); }
int setsid(void) { return (int) er(sc0(NR_setsid)); }
int mount(char const *src, char const *tgt, char const *ty, unsigned long fl, void const *d) {
  return (int) er(sc5(NR_mount, (long) src, (long) tgt, (long) ty, (long) fl, (long) d)); }
int unshare(int fl) { return (int) er(sc1(NR_unshare, fl)); }
int clock_gettime(int ck, struct timespec *ts) { return (int) er(sc2(NR_clock_gettime, ck, (long) ts)); }
long clock(void) {                                 /* CLOCKS_PER_SEC is 1e6; clock 2 = CLOCK_PROCESS_CPUTIME_ID */
  struct timespec ts;
  if (clock_gettime(2, &ts) < 0) return -1;
  return ts.tv_sec * 1000000 + ts.tv_nsec / 1000; }
long pwrite(int fd, void const *b, unsigned long n, long off) { return er(sc4(NR_pwrite64, fd, (long) b, (long) n, off)); }
long pread(int fd, void *b, unsigned long n, long off) { return er(sc4(NR_pread64, fd, (long) b, (long) n, off)); }
int memfd_create(char const *name, unsigned int fl) { return (int) er(sc2(NR_memfd_create, (long) name, fl)); }
int gettimeofday(struct timeval *tv, void *tz) {
  struct timespec ts;
  (void) tz;
  if (clock_gettime(0, &ts) < 0) return -1;        /* CLOCK_REALTIME */
  tv->tv_sec = ts.tv_sec;
  tv->tv_usec = ts.tv_nsec / 1000;
  return 0; }
int utimes(char const *p, struct timeval const *tv) {
  struct timespec ts[2];
  if (!tv) return utimensat(AT_FDCWD, p, 0, 0);
  ts[0].tv_sec = tv[0].tv_sec; ts[0].tv_nsec = tv[0].tv_usec * 1000;
  ts[1].tv_sec = tv[1].tv_sec; ts[1].tv_nsec = tv[1].tv_usec * 1000;
  return utimensat(AT_FDCWD, p, ts, 0); }
int utimensat(int dfd, char const *p, struct timespec const *ts, int fl) {
  return (int) er(sc4(NR_utimensat, dfd, (long) p, (long) ts, fl)); }
int utime(char const *path, struct utimbuf const *t) {
  if (!t) return utimensat(AT_FDCWD, path, 0, 0);
  struct timespec ts[2];
  ts[0].tv_sec = t->actime;  ts[0].tv_nsec = 0;
  ts[1].tv_sec = t->modtime; ts[1].tv_nsec = 0;
  return utimensat(AT_FDCWD, path, ts, 0); }

/* ---- the calendar: no timezone database, so localtime IS gmtime (UTC). the
 * civil-from-days is Hinnant's exact integer algorithm (1970-01-01 = Thursday,
 * wday 4). asctime lays glibc's fixed 26-byte "Www Mmm dd hh:mm:ss yyyy\n". ---- */
struct tm *gmtime(time_t const *tp) {
  static struct tm tm;
  long t = *tp;
  long days = t / 86400, secs = t % 86400;
  if (secs < 0) { secs += 86400; days -= 1; }
  tm.tm_hour = (int) (secs / 3600);
  tm.tm_min = (int) (secs % 3600 / 60);
  tm.tm_sec = (int) (secs % 60);
  tm.tm_wday = (int) (((days % 7) + 4 + 7) % 7);
  long z = days + 719468;
  long era = (z >= 0 ? z : z - 146096) / 146097;
  long doe = z - era * 146097;
  long yoe = (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365;
  long y = yoe + era * 400;
  long doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
  long mp = (5 * doy + 2) / 153;
  long d = doy - (153 * mp + 2) / 5 + 1;
  long m = mp < 10 ? mp + 3 : mp - 9;
  y += (m <= 2);
  tm.tm_year = (int) (y - 1900);
  tm.tm_mon = (int) (m - 1);
  tm.tm_mday = (int) d;
  tm.tm_yday = 0;
  tm.tm_isdst = 0;
  tm.tm_gmtoff = 0;
  tm.tm_zone = "UTC";
  return &tm; }
struct tm *localtime(time_t const *tp) { return gmtime(tp); }
static void __d2(char *p, int v) { p[0] = (char) (48 + v / 10 % 10); p[1] = (char) (48 + v % 10); }
double difftime(time_t a, time_t b) { return (double) (a - b); }
time_t mktime(struct tm *tm) {                     /* the exact inverse of gmtime (UTC -- no tz, like localtime) */
  long y = tm->tm_year + 1900, m = tm->tm_mon + 1, d = tm->tm_mday;
  y -= m <= 2;
  long era = (y >= 0 ? y : y - 399) / 400;
  long yoe = y - era * 400;
  long doy = (153 * (m + (m > 2 ? -3 : 9)) + 2) / 5 + d - 1;   /* Hinnant days-from-civil */
  long doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
  long days = era * 146097 + doe - 719468;
  return days * 86400 + tm->tm_hour * 3600L + tm->tm_min * 60L + tm->tm_sec; }
char *asctime(struct tm const *tm) {
  static char b[26];
  static char const *wd = "SunMonTueWedThuFriSat";
  static char const *mo = "JanFebMarAprMayJunJulAugSepOctNovDec";
  int i;
  int w = tm->tm_wday, mn = tm->tm_mon, y = tm->tm_year + 1900;
  if (w < 0 || w > 6) w = 0;
  if (mn < 0 || mn > 11) mn = 0;
  for (i = 0; i < 3; i++) b[i] = wd[w * 3 + i];
  b[3] = ' ';
  for (i = 0; i < 3; i++) b[4 + i] = mo[mn * 3 + i];
  b[7] = ' ';
  __d2(b + 8, tm->tm_mday); if (b[8] == '0') b[8] = ' ';
  b[10] = ' ';
  __d2(b + 11, tm->tm_hour); b[13] = ':';
  __d2(b + 14, tm->tm_min);  b[16] = ':';
  __d2(b + 17, tm->tm_sec);  b[19] = ' ';
  __d2(b + 20, y / 100); __d2(b + 22, y % 100);
  b[24] = 10; b[25] = 0;
  return b; }
char *ctime(time_t const *tp) { return asctime(gmtime(tp)); }
/* strftime: the everyday conversions (lua's os.date; %c is the asctime lay).
 * unknown specifiers echo literally; answers 0 when the buffer runs out. */
size_t strftime(char *s, size_t max, char const *fmt, struct tm const *tm) {
  static char const *wdl = "Sunday\0   Monday\0   Tuesday\0  Wednesday\0Thursday\0 Friday\0   Saturday";
  static char const *mol = "January\0  February\0 March\0    April\0    May\0      June\0     "
                           "July\0     August\0   September\0October\0  November\0 December";
  size_t n = 0;
  char b[26];
  for (; *fmt; fmt++) {
    char const *p = 0;
    int v = -1, w = 2;
    if (*fmt != '%') { if (n + 1 >= max) return 0; s[n++] = *fmt; continue; }
    fmt++;
    switch (*fmt) {
      case 'Y': v = tm->tm_year + 1900; w = 4; break;
      case 'y': v = (tm->tm_year + 1900) % 100; break;
      case 'm': v = tm->tm_mon + 1; break;
      case 'd': v = tm->tm_mday; break;
      case 'H': v = tm->tm_hour; break;
      case 'M': v = tm->tm_min; break;
      case 'S': v = tm->tm_sec; break;
      case 'j': v = tm->tm_yday + 1; w = 3; break;
      case 'p': p = tm->tm_hour < 12 ? "AM" : "PM"; break;
      case 'a': memcpy(b, wdl + tm->tm_wday * 10, 3); b[3] = 0; p = b; break;
      case 'A': p = wdl + tm->tm_wday * 10; break;
      case 'b': memcpy(b, mol + tm->tm_mon * 10, 3); b[3] = 0; p = b; break;
      case 'B': p = mol + tm->tm_mon * 10; break;
      case 'c': { char *a = asctime(tm); memcpy(b, a, 24); b[24] = 0; p = b; break; }
      case 'x': { snprintf(b, sizeof b, "%02d/%02d/%02d", tm->tm_mon + 1, tm->tm_mday, (tm->tm_year + 1900) % 100); p = b; break; }
      case 'X': { snprintf(b, sizeof b, "%02d:%02d:%02d", tm->tm_hour, tm->tm_min, tm->tm_sec); p = b; break; }
      case '%': p = "%"; break;
      default:  b[0] = '%'; b[1] = *fmt; b[2] = 0; p = b; break; }
    if (v >= 0) { snprintf(b, sizeof b, w == 4 ? "%04d" : w == 3 ? "%03d" : "%02d", v); p = b; }
    if (p) { size_t l = strlen(p); if (n + l >= max) return 0; memcpy(s + n, p, l); n += l; } }
  if (n >= max) return 0;
  s[n] = 0;
  return n; }
void *mmap(void *a, long n, int prot, int fl, int fd, long off) {
  long r = sc6(NR_mmap, (long) a, n, prot, fl, fd, off);
  if ((unsigned long) r > (unsigned long) -4096L) { __errno_v = (int) -r; return (void *) -1; }
  return (void *) r; }
int munmap(void *a, long n) { return (int) er(sc2(NR_munmap, (long) a, n)); }
int mprotect(void *a, long n, int prot) { return (int) er(sc3(NR_mprotect, (long) a, n, prot)); }
long sysconf(int name) {
  if (name == _SC_PAGESIZE) return 4096;
  __errno_v = EINVAL; return -1; }

/* ---- exit: the atexit chain, the stdio flush, then exit_group ---- */
typedef void (*__exitfn)(void);
static __exitfn __atex[32];
static int __natex;
int atexit(void (*f)(void)) {
  if (__natex >= 32) return -1;
  __atex[__natex++] = f;
  return 0; }
void _exit(int c) { for (;;) sc1(NR_exit_group, c); }
void exit(int c) {
  while (__natex > 0) __atex[--__natex]();
  fflush(0);
  _exit(c); }
void abort(void) { raise(SIGABRT); _exit(127); }

/* ---- execvp: execve + the PATH walk ---- */
int execv(char const *p, char *const *av) {
  return (int) er(sc3(NR_execve, (long) p, (long) av, (long) environ)); }
int execvp(char const *f, char *const *av) {
  char const *s = f;
  while (*s) { if (*s == '/') return execv(f, av); s++; }
  char const *path = getenv("PATH");
  if (!path) path = "/usr/local/bin:/usr/bin:/bin";
  char buf[4096];
  size_t fn = strlen(f);
  while (*path) {
    size_t i = 0;
    while (path[i] && path[i] != ':') i++;
    if (i + 1 + fn + 1 < sizeof buf) {
      memcpy(buf, path, i);
      buf[i] = '/';
      memcpy(buf + i + 1, f, fn + 1);
      execv(buf, av); }                     /* returns only on failure; keep walking */
    path += i;
    if (*path == ':') path++; }
  __errno_v = ENOENT;
  return -1; }

/* ---- malloc: K&R's first-fit free list over 1MB mmap arenas. 16-byte units,
 * 16-byte alignment, coalescing free. love mallocs pools (big, rare) and codec /
 * line buffers (small, freed) -- this shape covers both without ceremony. ---- */
typedef struct __mhdr { struct __mhdr *next; size_t size; } __mhdr;   /* size in units */
static __mhdr __mbase;
static __mhdr *__mfree;
void free(void *p) {
  if (!p) return;
  __mhdr *b = (__mhdr *) p - 1, *q = __mfree;
  for (; !(b > q && b < q->next); q = q->next)
    if (q >= q->next && (b > q || b < q->next)) break;   /* at the arena's wrap point */
  if (b + b->size == q->next) { b->size += q->next->size; b->next = q->next->next; }
  else b->next = q->next;
  if (q + q->size == b) { q->size += b->size; q->next = b->next; }
  else q->next = b;
  __mfree = q; }
static __mhdr *__mcore(size_t nu) {
  size_t need = (nu + 1) * sizeof(__mhdr);
  size_t len = need < (1UL << 20) ? (1UL << 20) : ((need + 4095UL) & ~4095UL);
  void *m = mmap(0, (long) len, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (m == (void *) -1) return 0;
  __mhdr *u = m;
  u->size = len / sizeof(__mhdr);
  free((void *) (u + 1));
  return __mfree; }
void *malloc(size_t n) {
  size_t nu = (n + sizeof(__mhdr) - 1) / sizeof(__mhdr) + 1;
  __mhdr *prev = __mfree;
  if (!prev) { __mbase.next = __mfree = prev = &__mbase; __mbase.size = 0; }
  for (__mhdr *q = prev->next; ; prev = q, q = q->next) {
    if (q->size >= nu) {
      if (q->size == nu) prev->next = q->next;
      else { q->size -= nu; q += q->size; q->size = nu; }
      __mfree = prev;
      return (void *) (q + 1); }
    if (q == __mfree)
      if (!(q = __mcore(nu))) { __errno_v = ENOMEM; return 0; } } }

void *calloc(size_t n, size_t sz) {
  size_t t = n * sz;
  void *p = malloc(t);
  if (p) memset(p, 0, t);
  return p; }
void *realloc(void *p, size_t n) {
  if (!p) return malloc(n);
  if (n == 0) { free(p); return 0; }
  size_t old = (((__mhdr *) p - 1)->size - 1) * sizeof(__mhdr);   /* payload bytes */
  if (old >= n) return p;
  void *q = malloc(n);
  if (!q) return 0;
  memcpy(q, p, old);
  free(p);
  return q; }
/* alloca: no native / __builtin form, so gnulib's C_ALLOCA scheme by hand --
 * malloc-backed, reclaimed by stack depth. both arches grow DOWN, so a frame
 * that has returned sits at a HIGHER address than the current probe; on each
 * call we free every block whose mark sits BELOW `here` (its frame unwound
 * past). blocks from the same or an ancestor frame (mark >= here) stay.
 * leak-free without a stack-direction probe. */
typedef struct __ablk { struct __ablk *next; char *mark; } __ablk;
static __ablk *__ahead;
void *alloca(size_t n) {
  char here;
  while (__ahead && __ahead->mark < &here) { __ablk *d = __ahead; __ahead = d->next; free(d); }
  if (n == 0) return 0;                         /* alloca(0): reclaim only */
  __ablk *b = malloc(sizeof(__ablk) + n);
  if (!b) return 0;
  b->mark = &here;
  b->next = __ahead;
  __ahead = b;
  return (void *) (b + 1); }
int atoi(char const *s) {
  int sign = 1, v = 0;
  while (*s == ' ' || *s == 9) s++;
  if (*s == '-') { sign = -1; s++; } else if (*s == '+') s++;
  while (*s >= '0' && *s <= '9') { v = v * 10 + (*s - '0'); s++; }
  return sign * v; }
long atol(char const *s) {
  long sign = 1, v = 0;
  while (*s == ' ' || *s == 9) s++;
  if (*s == '-') { sign = -1; s++; } else if (*s == '+') s++;
  while (*s >= '0' && *s <= '9') { v = v * 10 + (*s - '0'); s++; }
  return sign * v; }
int abs(int v) { return v < 0 ? -v : v; }
long labs(long v) { return v < 0 ? -v : v; }
/* n*sz with the multiply CHECKED -- the whole reason the call exists, and a
 * plain realloc(p, n*sz) would be the overflow it was invented to stop */
void *reallocarray(void *p, size_t n, size_t sz) {
  if (sz && n > (size_t) -1 / sz) { __errno_v = ENOMEM; return 0; }
  return realloc(p, n * sz); }
/* argv[0], stashed by __ai_start below -- gnulib's progname module reaches for
 * this and would otherwise die at the link */
char const *__ai_progname = "";
const char *getprogname(void) { return __ai_progname; }

/* ---- env ---- */
char *getenv(char const *k) {
  size_t n = strlen(k);
  if (!environ) return 0;
  for (char **e = environ; *e; e++)
    if (memcmp(*e, k, n) == 0 && (*e)[n] == '=') return *e + n + 1;
  return 0; }
static int __env_ours;                       /* the array itself came off our malloc */
int setenv(char const *k, char const *v, int ov) {
  size_t kn = strlen(k), vn = strlen(v);
  char *kv = malloc(kn + 1 + vn + 1);
  if (!kv) return -1;
  memcpy(kv, k, kn); kv[kn] = '=';
  memcpy(kv + kn + 1, v, vn + 1);
  size_t cnt = 0;
  if (environ)
    for (char **e = environ; *e; e++, cnt++)
      if (memcmp(*e, k, kn) == 0 && (*e)[kn] == '=') {
        if (!ov) { free(kv); return 0; }
        *e = kv;                             /* the old string may be the kernel's; leak it */
        return 0; }
  char **ne = malloc((cnt + 2) * sizeof(char *));
  if (!ne) { free(kv); return -1; }
  for (size_t i = 0; i < cnt; i++) ne[i] = environ[i];
  ne[cnt] = kv;
  ne[cnt + 1] = 0;
  if (__env_ours) free(environ);
  environ = ne;
  __env_ours = 1;
  return 0; }
int unsetenv(char const *k) {
  size_t kn = strlen(k);
  if (!environ) return 0;
  char **w = environ;
  for (char **e = environ; *e; e++)
    if (!(memcmp(*e, k, kn) == 0 && (*e)[kn] == '=')) *w++ = *e;
  *w = 0;
  return 0; }

/* ---- stdio: FILE is a fd plus (for write streams) a flush buffer. stdout is
 * the one hot stream -- love's fd_putc sends EVERY output byte through fputc, so
 * it buffers 8KB (line-flushed on a tty, glibc's shape); stderr never buffers;
 * fopen'd streams buffer 4KB. reads are unbuffered (image.c freads whole
 * files), which keeps fseek/ftell honest as plain lseek. ---- */
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
static unsigned char __obuf[8192];
static FILE __stdf[3];
FILE *stdin = &__stdf[0], *stdout = &__stdf[1], *stderr = &__stdf[2];

static long __wall(int fd, unsigned char const *p, long n) {
  long i = 0;
  while (i < n) {
    long k = write(fd, p + i, n - i);
    if (k < 0) { if (__errno_v == EINTR) continue; return -1; }
    i += k; }
  return i; }
static int __fdrain(FILE *f) {
  if (f->len == 0) return 0;
  long n = f->len;
  f->len = 0;
  if (__wall(f->fd, f->buf, n) < 0) { f->err = 1; return EOF; }
  return 0; }
int fflush(FILE *f) {
  if (!f) {
    int r = __fdrain(stdout);
    return __fdrain(stderr) || r ? EOF : 0; }
  return __fdrain(f); }
void setbuf(FILE *f, char *buf) {          /* NULL = unbuffered (m4 -e); else a BUFSIZ block */
  __fdrain(f);
  if (buf) { f->buf = (unsigned char *) buf; f->cap = 8192; f->line = 0; }
  else f->cap = 0; }
int setvbuf(FILE *f, char *buf, int mode, size_t size) {
  __fdrain(f);
  if (mode == _IONBF) { f->cap = 0; f->line = 0; return 0; }
  if (buf && size) { f->buf = (unsigned char *) buf; f->cap = (int) size; }
  f->line = mode == _IOLBF;
  return 0; }
int fputc(int c, FILE *f) {
  unsigned char b = (unsigned char) c;
  if (!f->cap) { if (__wall(f->fd, &b, 1) < 0) { f->err = 1; return EOF; } return b; }
  f->buf[f->len++] = b;
  if (f->len == f->cap || (f->line && b == 10))
    if (__fdrain(f)) return EOF;
  return b; }
size_t fwrite(void const *p, size_t sz, size_t n, FILE *f) {
  size_t total = sz * n;
  if (total == 0) return 0;
  if (!f->cap || total >= (size_t) f->cap) {
    if (__fdrain(f)) return 0;
    if (__wall(f->fd, p, (long) total) < 0) { f->err = 1; return 0; }
    return n; }
  if ((size_t) (f->cap - f->len) < total && __fdrain(f)) return 0;
  memcpy(f->buf + f->len, p, total);
  f->len += (int) total;
  if (f->line) { unsigned char const *q = p; for (size_t i = 0; i < total; i++) if (q[i] == 10) { __fdrain(f); break; } }
  return n; }
size_t fread(void *p, size_t sz, size_t n, FILE *f) {
  size_t total = sz * n, got = 0;
  unsigned char *d = p;
  while (got < total) {
    long k = read(f->fd, d + got, (long) (total - got));
    if (k < 0) { if (__errno_v == EINTR) continue; f->err = 1; break; }
    if (k == 0) { f->eof = 1; break; }
    got += (size_t) k; }
  return sz ? got / sz : 0; }
FILE *fopen(char const *path, char const *mode) {
  int fl = O_RDONLY, wr = 0;
  if (mode[0] == 'w') { fl = O_WRONLY | O_CREAT | O_TRUNC; wr = 1; }
  else if (mode[0] == 'a') { fl = O_WRONLY | O_CREAT | O_APPEND; wr = 1; }
  for (char const *m = mode + 1; *m; m++)
    if (*m == '+') { fl = (fl & ~3) | O_RDWR; wr = 1; }
  int fd = open(path, fl, 438);              /* 0666, the umask trims it */
  if (fd < 0) return 0;
  FILE *f = malloc(sizeof(FILE) + 4096);
  if (!f) { close(fd); return 0; }
  memset(f, 0, sizeof(FILE));
  f->fd = fd;
  f->wr = wr;
  f->heap = 1;
  if (wr) { f->buf = (unsigned char *) (f + 1); f->cap = 4096; }
  return f; }
int fclose(FILE *f) {
  int r = __fdrain(f);
  if (close(f->fd) < 0) r = EOF;
  if (f->heap) free(f);
  return r; }
FILE *freopen(char const *path, char const *mode, FILE *f) {
  __fdrain(f);                                     /* the stream KEEPS its FILE (and its buffer) -- only the fd turns over */
  close(f->fd);
  int fl = O_RDONLY, wr = 0;
  if (mode[0] == 'w') { fl = O_WRONLY | O_CREAT | O_TRUNC; wr = 1; }
  else if (mode[0] == 'a') { fl = O_WRONLY | O_CREAT | O_APPEND; wr = 1; }
  for (char const *m = mode + 1; *m; m++)
    if (*m == '+') { fl = (fl & ~3) | O_RDWR; wr = 1; }
  int fd = open(path, fl, 438);
  if (fd < 0) return 0;
  f->fd = fd; f->wr = wr; f->err = 0; f->eof = 0; f->un = 0; f->len = 0; f->pid = 0;
  return f; }
int fseek(FILE *f, long off, int wh) {
  if (__fdrain(f)) return -1;
  f->un = 0;                                 /* ISO: a seek discards the pushback */
  f->eof = 0;                                /* and clears the end-of-file flag */
  return lseek(f->fd, off, wh) < 0 ? -1 : 0; }
void rewind(FILE *f) {
  if (fseek(f, 0, 0) == 0) f->err = 0; }
long ftell(FILE *f) {
  if (__fdrain(f)) return -1;
  return lseek(f->fd, 0, SEEK_CUR); }
int fileno(FILE *f) { return f->fd; }

/* ---- the one formatter under fprintf and snprintf: %s %c %d %u %x %o %p and
 * the float lanes %f %e %g %a, with l/z widths and the %[-0+ #]W.P flags.
 * sink+ctx so neither caller stages a bound buffer. */
static void __femit(void *ctx, int c) { fputc(c, (FILE *) ctx); }
struct __sctx { char *p; size_t n, at; };
static void __semit(void *ctx, int c) {
  struct __sctx *s = ctx;
  if (s->at + 1 < s->n) s->p[s->at] = (char) c;
  s->at++; }
static void __pad(void (*put)(void *, int), void *ctx, int n, int ch) { while (n-- > 0) put(ctx, ch); }
/* the conversion flags, gathered off the % once so neither field function
 * takes a parameter per flag. */
#define FF_LEFT 1
#define FF_ZERO 2
#define FF_ALT  4
#define FF_PLUS 8
#define FF_SPC  16
/* the sign a value wears: '-' when it is negative, else whatever + or space
 * asks for, else none at all. */
static int __fmtsgn(int neg, int fl) {
  return neg ? '-' : (fl & FF_PLUS) ? '+' : (fl & FF_SPC) ? ' ' : 0; }
/* one integer field with %[-0+ #]WIDTH: digits reversed into tmp, then sign +
 * base prefix + pad (zeros hug the digits, spaces sit outside) + digits, or
 * left-justified. */
static void __fmtnum(void (*put)(void *, int), void *ctx, unsigned long v, unsigned base,
                     int neg, int width, int fl, int up) {
  char tmp[24];
  int nd = 0;
  int a = up ? 55 : 87;
  do { unsigned d = (unsigned) (v % base); tmp[nd++] = (char) (d < 10 ? 48 + d : a + d); v /= base; } while (v);
  int sgn = __fmtsgn(neg, fl);
  /* # asks the base to show itself: 0x on hex, a leading 0 on octal -- and on
     neither when the digits already start with a zero, which is the whole of
     the zero case. */
  char const *pre = "";
  if ((fl & FF_ALT) && tmp[nd - 1] != '0') pre = base == 16 ? (up ? "0X" : "0x") : base == 8 ? "0" : "";
  int np = 0;
  while (pre[np]) np++;
  int len = nd + np + (sgn ? 1 : 0);
  int pad = width > len ? width - len : 0;
  if (!(fl & (FF_LEFT | FF_ZERO))) __pad(put, ctx, pad, 32);
  if (sgn) put(ctx, sgn);
  for (int i = 0; i < np; i++) put(ctx, pre[i]);
  if (!(fl & FF_LEFT) && (fl & FF_ZERO)) __pad(put, ctx, pad, 48);
  while (nd) put(ctx, tmp[--nd]);
  if (fl & FF_LEFT) __pad(put, ctx, pad, 32); }
/* ---- the exact decimal of a double --------------------------------------
 *
 * a double is m * 2^e with m a 53-bit INTEGER, so its decimal form is finite:
 * at most 309 digits before the point and 1074 after. bd[] holds every one of
 * them, nine to a limb in base 1e9, reached by doubling or halving the
 * mantissa -- so rounding and emission read EXACT digits and the answer is
 * byte-equal to glibc, which is what test_libc compares.
 *
 * ⚠ the lane this replaced turned digits out of the double itself, normalising
 * by repeated `/= 10`. that spends a rounding per decade, and the error lands
 * exactly where a long precision asks to read: %.17g of 1e300 came back wrong
 * from its 16th digit, %.20f of 0.1 answered twenty zeros where the value
 * carries ...00555, and a TIE could not be broken at all -- the residue that
 * decides it had already been rounded away, so %.0f of 2.5 said 3 where every
 * conforming printf says 2. an approximate converter cannot be gated against
 * an exact one; that is the whole reason this is a bignum and not a patch.
 */
#define BD_B 1000000000UL
#define BD_I 36                        /* integer limbs: 324 digits >= 309 */
#define BD_F 121                       /* fraction limbs: 1089 digits >= 1074 */
#define BD_N (BD_I + BD_F)
#define BD_D (BD_N * 9)                /* every digit index the array holds */
#define BD_U (BD_I * 9 - 1)            /* the index of the units place */

static unsigned long const bd_p10[9] = {
  1UL, 10UL, 100UL, 1000UL, 10000UL, 100000UL, 1000000UL, 10000000UL, 100000000UL };

/* the digit at index k -- BD_U is the units place, larger is further right.
 * OUTSIDE the array every digit is a zero, which is what lets an arbitrary
 * precision print with no buffer and no cap. */
static unsigned bd_dig(unsigned long const *d, int k) {
  if (k < 0 || k >= BD_D) return 0;
  return (unsigned) (d[k / 9] / bd_p10[8 - k % 9] % 10UL); }
/* the live range shrinks toward index 0 as the value grows and toward BD_N as
 * it shrinks, so both walks carry their bound rather than sweeping the array:
 * a shift is O(digits that exist), not O(1413). */
static int bd_mul2(unsigned long *d, int lo, int hi, int s) {
  unsigned long carry = 0;
  int i;
  for (i = hi; i >= 0; i--) {
    if (i < lo && !carry) break;
    unsigned long cur = (d[i] << s) + carry;
    d[i] = cur % BD_B;
    carry = cur / BD_B; }
  return i + 1 < lo ? i + 1 : lo; }
static int bd_div2(unsigned long *d, int lo, int hi, int s) {
  unsigned long rem = 0, m = 1UL << s;
  int i;
  for (i = lo; i < BD_N; i++) {
    if (i > hi && !rem) break;
    unsigned long cur = rem * BD_B + d[i];
    d[i] = cur / m;
    rem = cur % m; }
  return i - 1 > hi ? i - 1 : hi; }
/* zero every digit after index k */
static void bd_trunc(unsigned long *d, int k) {
  int i;
  if (k >= BD_D) return;
  if (k < 0) i = 0;
  else { d[k / 9] -= d[k / 9] % bd_p10[8 - k % 9]; i = k / 9 + 1; }
  for (; i < BD_N; i++) d[i] = 0; }
/* add one at digit index k, carrying toward index 0. the array has 324 integer
 * digits against a largest double of 309, so the carry always lands. */
static int bd_inc(unsigned long *d, int k, int lo) {
  int i = k / 9;
  unsigned long add = bd_p10[8 - k % 9];
  while (i >= 0) {
    d[i] += add;
    if (d[i] < BD_B) break;
    d[i] -= BD_B; add = 1; i--; }
  return i >= 0 && i < lo ? i : lo; }
/* round to keep digits through index k. the value is exact, so a TIE is a real
 * tie and breaks to even -- %.0f of 2.5 is 2 and of 3.5 is 4. */
static int bd_round(unsigned long *d, int k, int lo) {
  unsigned n = bd_dig(d, k + 1);
  int up = n > 5;
  if (n == 5) {
    int j = k + 2, any = 0, lim = (j + 8) / 9 * 9;
    for (; j < lim && j < BD_D; j++) if (bd_dig(d, j)) { any = 1; break; }
    if (!any) for (j = lim / 9; j < BD_N; j++) if (d[j]) { any = 1; break; }
    up = any || (bd_dig(d, k) & 1); }
  bd_trunc(d, k);
  return up ? bd_inc(d, k, lo) : lo; }
/* the index of the most significant digit that is not a zero, or the units
 * place when the value is zero (which reads 0 and dates the exponent at 0). */
static int bd_msd(unsigned long const *d, int lo) {
  for (int i = lo; i < BD_N; i++)
    if (d[i]) { int k = i * 9; while (!bd_dig(d, k)) k++; return k; }
  return BD_U; }

/* the float lanes: %f %e %g %a and their upper-case twins. */
static void __fmtflo(void (*put)(void *, int), void *ctx, double v, int conv,
                     int prec, int width, int fl) {
  unsigned long bits;
  memcpy(&bits, &v, sizeof bits);
  int neg = (int) (bits >> 63);            /* ⚠ from the SIGN BIT, not v < 0:
                                              -0.0 is not less than zero, and
                                              printf must still say -0 */
  int be = (int) ((bits >> 52) & 0x7ffUL);
  unsigned long man = bits & 0xfffffffffffffUL;
  int up = conv == 'E' || conv == 'G' || conv == 'F' || conv == 'A';
  if (up) conv += 32;
  int sgn = __fmtsgn(neg, fl);

  /* --- infinity and not-a-number: three letters, and the zero flag does not
     reach them -- a wide %08.1f of -inf pads with spaces. */
  if (be == 0x7ff) {
    char const *w = man ? "nan" : "inf";
    int len = 3 + (sgn ? 1 : 0);
    int pad = width > len ? width - len : 0;
    if (!(fl & FF_LEFT)) __pad(put, ctx, pad, 32);
    if (sgn) put(ctx, sgn);
    for (int i = 0; i < 3; i++) put(ctx, up ? w[i] - 32 : w[i]);
    if (fl & FF_LEFT) __pad(put, ctx, pad, 32);
    return; }

  /* --- %a: the bits themselves, four to a hex digit, so only the rounding at
     a short precision needs any care. a subnormal keeps the -1022 exponent and
     shows a leading 0 rather than renormalising, and so does a mantissa that
     rounds up past f -- 0.999999 at %.1a is 0x2.0p-1, not 0x1.0p+0. */
  if (conv == 'a') {
    int lead = be ? 1 : 0;
    int xe = be ? be - 1023 : man ? -1022 : 0;
    int nd = 13;
    if (prec >= 0 && prec < 13) {
      nd = prec;
      unsigned g = (unsigned) ((man >> (48 - 4 * nd)) & 0xfUL);
      unsigned long rest = man & ((1UL << (48 - 4 * nd)) - 1);
      int odd = nd ? (int) ((man >> (52 - 4 * nd)) & 1UL) : lead & 1;
      int rup = g > 8 || (g == 8 && (rest || odd));
      man &= ~((1UL << (52 - 4 * nd)) - 1);
      if (rup) { man += 1UL << (52 - 4 * nd);
                 if (man >> 52) { man &= 0xfffffffffffffUL; lead++; } } }
    else if (prec >= 0) nd = prec;                    /* past 13, all zeros */
    else while (nd && !((man >> (52 - 4 * nd)) & 0xfUL)) nd--;
    int pt = nd > 0 || (fl & FF_ALT);
    int ax = xe < 0 ? -xe : xe;
    int nx = 1; for (int t = ax; t >= 10; t /= 10) nx++;
    int len = (sgn ? 1 : 0) + 2 + 1 + (pt ? 1 + nd : 0) + 2 + nx;
    int pad = width > len ? width - len : 0;
    if (!(fl & (FF_LEFT | FF_ZERO))) __pad(put, ctx, pad, 32);
    if (sgn) put(ctx, sgn);
    put(ctx, '0'); put(ctx, up ? 'X' : 'x');
    if (!(fl & FF_LEFT) && (fl & FF_ZERO)) __pad(put, ctx, pad, 48);
    put(ctx, (char) ('0' + lead));
    if (pt) {
      put(ctx, '.');
      for (int j = 0; j < nd; j++) {
        unsigned h = j < 13 ? (unsigned) ((man >> (48 - 4 * j)) & 0xfUL) : 0;
        put(ctx, h < 10 ? '0' + h : (up ? 55 : 87) + h); } }
    put(ctx, up ? 'P' : 'p');
    put(ctx, xe < 0 ? '-' : '+');
    for (int t = nx; t; t--) { int q = ax; for (int u = 1; u < t; u++) q /= 10; put(ctx, '0' + q % 10); }
    if (fl & FF_LEFT) __pad(put, ctx, pad, 32);
    return; }

  /* --- the decimal lanes, off the exact digits. */
  unsigned long bd[BD_N];
  for (int i = 0; i < BD_N; i++) bd[i] = 0;
  unsigned long m = be ? man | 0x10000000000000UL : man;
  int e2 = be ? be - 1075 : -1074;               /* the value is m * 2^e2 */
  bd[BD_I - 1] = m % BD_B;
  bd[BD_I - 2] = m / BD_B;                       /* m < 2^53, so two limbs */
  int lo = BD_I - 2, hi = BD_I - 1;
  for (int s = e2; s > 0; ) { int k = s > 30 ? 30 : s; lo = bd_mul2(bd, lo, hi, k); s -= k; }
  for (int s = -e2; s > 0; ) { int k = s > 30 ? 30 : s; hi = bd_div2(bd, lo, hi, k); s -= k; }

  int msd = bd_msd(bd, lo);
  int fprec, cut, strip = 0;
  if (conv == 'g') {
    int p = prec < 0 ? 6 : prec ? prec : 1;
    lo = bd_round(bd, msd + p - 1, lo);
    msd = bd_msd(bd, lo);                        /* 999 -> 1000 moves it */
    int dexp = BD_U - msd;
    conv = (dexp < -4 || dexp >= p) ? 'e' : 'f';
    fprec = conv == 'e' ? p - 1 : p - 1 - dexp;
    if (fprec < 0) fprec = 0;
    strip = !(fl & FF_ALT); }
  else {
    fprec = prec < 0 ? 6 : prec;
    cut = conv == 'e' ? msd + fprec : BD_U + fprec;
    lo = bd_round(bd, cut, lo);
    msd = bd_msd(bd, lo); }

  /* the digits to show: one at msd for %e, or the whole integer part for %f --
     and when the value has none, the units place, which reads the 0 that C
     asks for. */
  int i0 = conv == 'e' ? msd : msd < BD_U ? msd : BD_U;
  int i1 = conv == 'e' ? msd : BD_U;
  if (strip) while (fprec && !bd_dig(bd, i1 + fprec)) fprec--;
  int pt = fprec > 0 || (fl & FF_ALT);
  int xe = BD_U - msd, ax = xe < 0 ? -xe : xe, nx = 1;
  for (int t = ax; t >= 10; t /= 10) nx++;
  if (nx < 2) nx = 2;                            /* the exponent shows two */

  int len = (sgn ? 1 : 0) + (i1 - i0 + 1) + (pt ? 1 + fprec : 0)
          + (conv == 'e' ? 2 + nx : 0);
  int pad = width > len ? width - len : 0;
  if (!(fl & (FF_LEFT | FF_ZERO))) __pad(put, ctx, pad, 32);
  if (sgn) put(ctx, sgn);
  if (!(fl & FF_LEFT) && (fl & FF_ZERO)) __pad(put, ctx, pad, 48);
  for (int k = i0; k <= i1; k++) put(ctx, '0' + bd_dig(bd, k));
  if (pt) { put(ctx, '.');
            for (int j = 1; j <= fprec; j++) put(ctx, '0' + bd_dig(bd, i1 + j)); }
  if (conv == 'e') {
    put(ctx, up ? 'E' : 'e');
    put(ctx, xe < 0 ? '-' : '+');
    for (int t = nx; t; t--) { int q = ax; for (int u = 1; u < t; u++) q /= 10; put(ctx, '0' + q % 10); } }
  if (fl & FF_LEFT) __pad(put, ctx, pad, 32); }
static void __fmt(void (*put)(void *, int), void *ctx, char const *fmt, va_list ap) {
  for (; *fmt; fmt++) {
    if (*fmt != '%') { put(ctx, *fmt); continue; }
    fmt++;
    int fl = 0, width = 0, prec = -1, wide = 0;
    for (; ; fmt++) {                        /* flags: all five of them act */
      if (*fmt == '-') fl |= FF_LEFT;
      else if (*fmt == '0') fl |= FF_ZERO;
      else if (*fmt == '+') fl |= FF_PLUS;
      else if (*fmt == ' ') fl |= FF_SPC;
      else if (*fmt == '#') fl |= FF_ALT;
      else break; }
    while (*fmt >= '0' && *fmt <= '9') { width = width * 10 + (*fmt - 48); fmt++; }
    if (*fmt == '.') { fmt++; prec = 0; while (*fmt >= '0' && *fmt <= '9') { prec = prec * 10 + (*fmt - 48); fmt++; } }
    while (*fmt == 'l' || *fmt == 'z' || *fmt == 'h') { if (*fmt != 'h') wide = 1; fmt++; }
    if (fl & FF_LEFT) fl &= ~FF_ZERO;
    if (fl & FF_PLUS) fl &= ~FF_SPC;         /* + outranks the space */
    if (*fmt == 's') {
      char const *s = va_arg(ap, char const *);
      if (!s) s = "(null)";
      int len = 0;
      while (s[len] && (prec < 0 || len < prec)) len++;
      int pad = width > len ? width - len : 0;
      if (!(fl & FF_LEFT)) __pad(put, ctx, pad, 32);
      for (int i = 0; i < len; i++) put(ctx, s[i]);
      if (fl & FF_LEFT) __pad(put, ctx, pad, 32); }
    else if (*fmt == 'c') {
      int pad = width > 1 ? width - 1 : 0;
      if (!(fl & FF_LEFT)) __pad(put, ctx, pad, 32);
      put(ctx, va_arg(ap, int));
      if (fl & FF_LEFT) __pad(put, ctx, pad, 32); }
    else if (*fmt == 'd' || *fmt == 'i') {
      long v = wide ? va_arg(ap, long) : (long) va_arg(ap, int);
      unsigned long u = (unsigned long) v;
      int neg = v < 0;
      if (neg) u = 0UL - u;
      __fmtnum(put, ctx, u, 10, neg, width, fl, 0); }
    else if (*fmt == 'u')
      __fmtnum(put, ctx, wide ? va_arg(ap, unsigned long) : (unsigned long) va_arg(ap, unsigned int), 10, 0, width, fl, 0);
    else if (*fmt == 'x' || *fmt == 'X')
      __fmtnum(put, ctx, wide ? va_arg(ap, unsigned long) : (unsigned long) va_arg(ap, unsigned int), 16, 0, width, fl, *fmt == 'X');
    else if (*fmt == 'o')
      __fmtnum(put, ctx, wide ? va_arg(ap, unsigned long) : (unsigned long) va_arg(ap, unsigned int), 8, 0, width, fl, 0);
    else if (*fmt == 'f' || *fmt == 'F' || *fmt == 'e' || *fmt == 'E' || *fmt == 'g' || *fmt == 'G'
             || *fmt == 'a' || *fmt == 'A')
      __fmtflo(put, ctx, va_arg(ap, double), *fmt, prec, width, fl);
    else if (*fmt == 'p') { put(ctx, 48); put(ctx, 120); __fmtnum(put, ctx, (unsigned long) va_arg(ap, void *), 16, 0, 0, 0, 0); }
    else if (*fmt == '%') put(ctx, 37);
    else { put(ctx, 37); if (*fmt) put(ctx, *fmt); else fmt--; } }
}
int fprintf(FILE *f, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  __fmt(__femit, f, fmt, ap);
  va_end(ap);
  return 0; }
int snprintf(char *p, size_t n, char const *fmt, ...) {
  struct __sctx s;
  s.p = p; s.n = n; s.at = 0;
  va_list ap; va_start(ap, fmt);
  __fmt(__semit, &s, fmt, ap);
  va_end(ap);
  if (n) p[s.at < n ? s.at : n - 1] = 0;
  return (int) s.at; }
int printf(char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  __fmt(__femit, stdout, fmt, ap);
  va_end(ap);
  return 0; }
/* the v-variants: __fmt already threads a va_list, so these just forward it. */
int vfprintf(FILE *f, char const *fmt, va_list ap) {
  __fmt(__femit, f, fmt, ap); return 0; }
int vprintf(char const *fmt, va_list ap) {
  __fmt(__femit, stdout, fmt, ap); return 0; }
int vsnprintf(char *p, size_t n, char const *fmt, va_list ap) {
  struct __sctx s; s.p = p; s.n = n; s.at = 0;
  __fmt(__semit, &s, fmt, ap);
  if (n) p[s.at < n ? s.at : n - 1] = 0;
  return (int) s.at; }
int vsprintf(char *p, char const *fmt, va_list ap) {
  return vsnprintf(p, (size_t) -1, fmt, ap); }
int sprintf(char *p, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int r = vsprintf(p, fmt, ap);
  va_end(ap); return r; }
int putc(int c, FILE *f) { return fputc(c, f); }
void perror(char const *s) {
  int e = __errno_v;
  if (s && *s) fprintf(stderr, "%s: ", s);
  fprintf(stderr, "errno %d\n", e); }
char *fgets(char *buf, int n, FILE *f) {
  int i = 0;
  while (i < n - 1) {
    char c;
    long k = read(f->fd, &c, 1);
    if (k <= 0) { if (k == 0) f->eof = 1; if (i == 0) return 0; break; }
    buf[i++] = c;
    if (c == 10) break; }
  buf[i] = 0;
  return buf; }

/* ---- signals: glibc's 152-byte sigaction folded onto the kernel's 32-byte
 * one. BOTH arches carry the restorer slot (aarch64 is the odd asm-generic
 * arch that kept SA_RESTORER in its uapi) -- but only x86-64 needs it filled
 * (sys.o's __ai_sigret); aarch64 leaves flag+slot zero and the kernel lays
 * its vdso return trampoline. ---- */
struct __ksigaction { void *h; unsigned long flags; void *restorer; unsigned long mask; };
int sigemptyset(sigset_t *s) { memset(s, 0, sizeof *s); return 0; }
int sigaddset(sigset_t *s, int n) {
  s->__v[(n - 1) / 64] |= 1UL << ((unsigned) (n - 1) % 64);
  return 0; }
int sigaction(int sig, struct sigaction const *a, struct sigaction *old) {
  struct __ksigaction ka, ko;
  memset(&ko, 0, sizeof ko);
  if (a) {
    ka.h = (void *) a->sa_handler;
#if defined(__aarch64__) || defined(__riscv)
    ka.flags = (unsigned long) (unsigned int) a->sa_flags;
    ka.restorer = 0;
#else
    ka.flags = (unsigned long) (unsigned int) a->sa_flags | 67108864UL;   /* SA_RESTORER */
    ka.restorer = (void *) __ai_sigret;
#endif
    ka.mask = (unsigned long) a->sa_mask.__v[0]; }
  long r = sc4(NR_rt_sigaction, sig, a ? (long) &ka : 0, old ? (long) &ko : 0, 8);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (old) {
    memset(old, 0, sizeof *old);
    old->sa_handler = (void (*)(int)) ko.h;
    old->sa_flags = (int) ko.flags;
    old->sa_mask.__v[0] = (long) ko.mask; }
  return 0; }
void *signal(int sig, void *h) {
  struct sigaction sa, old;
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = (void (*)(int)) h;
  sa.sa_flags = 268435456;                   /* SA_RESTART: glibc signal() semantics */
  if (sigaction(sig, &sa, &old)) return (void *) -1;
  return (void *) old.sa_handler; }
int sigprocmask(int how, sigset_t const *s, sigset_t *o) {
  unsigned long ks = s ? (unsigned long) s->__v[0] : 0, ko = 0;
  long r = sc4(NR_rt_sigprocmask, how, s ? (long) &ks : 0, o ? (long) &ko : 0, 8);
  if (r < 0) { __errno_v = (int) -r; return -1; }
  if (o) { memset(o, 0, sizeof *o); o->__v[0] = (long) ko; }
  return 0; }
int signalfd(int fd, sigset_t const *m, int fl) {
  unsigned long km = (unsigned long) m->__v[0];
  return (int) er(sc4(NR_signalfd4, fd, (long) &km, 8, fl)); }

/* ---- the terminal ---- */
int tcgetattr(int fd, struct termios *t) { return ioctl(fd, 21505, t); }            /* TCGETS */
int tcsetattr(int fd, int act, struct termios const *t) {
  if (act < 0 || act > 2) { __errno_v = EINVAL; return -1; }
  return ioctl(fd, (unsigned long) (21506 + act), t); }                             /* TCSETS/W/F */
int tcsetpgrp(int fd, pid_t pg) { int p = (int) pg; return ioctl(fd, 21520, &p); }  /* TIOCSPGRP */
int isatty(int fd) {
  struct termios t;
  return tcgetattr(fd, &t) == 0; }

/* ---- the pty quartet ---- */
int posix_openpt(int fl) { return open("/dev/ptmx", fl, 0); }
int grantpt(int fd) { (void) fd; return 0; }                    /* devpts grants at open */
int unlockpt(int fd) { int z = 0; return ioctl(fd, 1074025521UL, &z); }   /* TIOCSPTLCK */
char *ptsname(int fd) {
  static char nb[32];
  int n = 0;
  if (ioctl(fd, 2147767344UL, &n) < 0) return 0;                /* TIOCGPTN */
  snprintf(nb, sizeof nb, "/dev/pts/%d", n);
  return nb; }

/* ---- dirent over getdents64: the kernel record IS our struct dirent ---- */
struct __dirstream { int fd; int pos; int len; char buf[4096]; };
DIR *opendir(char const *p) {
  int fd = open(p, O_RDONLY | O_DIRECTORY | O_CLOEXEC, 0);
  if (fd < 0) return 0;
  DIR *d = malloc(sizeof(DIR));
  if (!d) { close(fd); return 0; }
  d->fd = fd; d->pos = 0; d->len = 0;
  return d; }
struct dirent *readdir(DIR *d) {
  if (d->pos >= d->len) {
    long n = sc3(NR_getdents64, d->fd, (long) d->buf, sizeof d->buf);
    if (n <= 0) { if (n < 0) __errno_v = (int) -n; return 0; }
    d->len = (int) n; d->pos = 0; }
  struct dirent *e = (struct dirent *) (d->buf + d->pos);
  d->pos += e->d_reclen;
  return e; }
int closedir(DIR *d) {
  int r = close(d->fd);
  free(d);
  return r; }

/* ---- byte order ---- */
unsigned short htons(unsigned short v) { return (unsigned short) ((v << 8) | (v >> 8)); }
unsigned short ntohs(unsigned short v) { return htons(v); }
unsigned int htonl(unsigned int v) {
  return (v << 24) | ((v & 65280U) << 8) | ((v >> 8) & 65280U) | (v >> 24); }
unsigned int ntohl(unsigned int v) { return htonl(v); }

/* ---- getaddrinfo, the numeric slice: dotted-quad IPv4 + localhost + a decimal
 * port -- exactly what the host seam speaks (host/sock.c resolves numbers; DNS
 * stays a post-rung nicety). one malloc'd block carries result + address. ---- */
struct __sain { unsigned short fam; unsigned short port; unsigned int addr; char pad[8]; };   /* sockaddr_in, 16 bytes */
struct __gai { struct addrinfo ai; struct __sain sa; };
static int __quad(char const *s, unsigned int *out) {
  unsigned int a = 0;
  for (int i = 0; i < 4; i++) {
    unsigned int b = 0, any = 0;
    while (*s >= 48 && *s <= 57) { b = b * 10 + (unsigned) (*s++ - 48); any = 1; if (b > 255) return -1; }
    if (!any) return -1;
    a = (a << 8) | b;
    if (i < 3 && *s++ != 46) return -1; }
  if (*s) return -1;
  *out = a;
  return 0; }
/* ---- the NAME half: /etc/hosts, then a UDP A query to /etc/resolv.conf's
 * nameservers -- the smallest resolver that keeps `connect host port` (ain)
 * and seed's http pull real on the raw default binary. IPv4 A records only,
 * first answer wins; 2 tries x ~2.5s per nameserver, up to 3 nameservers,
 * 127.0.0.1 when resolv.conf names none (musl's fallback). all addresses
 * move in HOST order here; getaddrinfo's htonl is the one wire flip. ---- */
static int __hline(char *ln, char const *host, unsigned int *out) {
  char *p = ln;
  while (*p == ' ' || *p == 9) p++;
  if (*p == '#' || !*p) return -1;
  char *a = p;
  while (*p && *p != ' ' && *p != 9) p++;
  if (!*p) return -1;
  *p++ = 0;
  unsigned int addr;
  if (__quad(a, &addr) < 0) return -1;       /* an IPv6 line falls out here */
  for (;;) {
    while (*p == ' ' || *p == 9) p++;
    if (!*p || *p == '#') return -1;
    char *n = p;
    while (*p && *p != ' ' && *p != 9) p++;
    int end = !*p;
    *p = 0;
    if (!strcmp(n, host)) { *out = addr; return 0; }
    if (end) return -1;
    p++; } }
static int __lines(char const *path, char const *host, unsigned int *out,
                   int (*one)(char*, char const*, unsigned int*)) {
  int fd = open(path, 0);
  if (fd < 0) return -1;
  char buf[1024], ln[512];
  long n; int li = 0, hit = -1;
  while (hit < 0 && (n = read(fd, buf, sizeof buf)) > 0)
    for (long i = 0; i < n; i++) {
      if (buf[i] != '\n') { if (li < (int) sizeof ln - 1) ln[li++] = buf[i]; continue; }
      ln[li] = 0, li = 0;
      if (one(ln, host, out) == 0) { hit = 0; break; } }
  if (hit < 0 && li) { ln[li] = 0; if (one(ln, host, out) == 0) hit = 0; }
  close(fd);
  return hit; }
/* a resolv.conf "nameserver A.B.C.D" line; host carries a (char) slot index */
static int __rline(char *ln, char const *slot, unsigned int *out) {
  char *p = ln;
  while (*p == ' ' || *p == 9) p++;
  if (strncmp(p, "nameserver", 10) != 0) return -1;
  p += 10;
  if (*p != ' ' && *p != 9) return -1;
  while (*p == ' ' || *p == 9) p++;
  char *a = p;
  while (*p && *p != ' ' && *p != 9 && *p != '#') p++;
  *p = 0;
  unsigned int addr;
  if (__quad(a, &addr) < 0) return -1;
  out[(int) *slot] = addr;
  return ++*(char*) slot >= 3 ? 0 : -1; }   /* keep scanning until 3 or eof */
static int __dnskip(unsigned char const *r, long rn, int pos) {
  while (pos < rn) {
    int l = r[pos];
    if (!l) return pos + 1;
    if ((l & 192) == 192) return pos + 2;    /* a compression pointer ends the name */
    pos += l + 1; }
  return -1; }
static int __dnsq(unsigned int ns, char const *host, unsigned int *out) {
  unsigned char q[300]; int qn = 12;
  struct timespec ts;
  clock_gettime(1, &ts);                     /* CLOCK_MONOTONIC seeds the id */
  unsigned short id = (unsigned short) (ts.tv_nsec ^ (ts.tv_nsec >> 16));
  memset(q, 0, 12);
  q[0] = (unsigned char) (id >> 8), q[1] = (unsigned char) id;
  q[2] = 1;                                  /* RD */
  q[5] = 1;                                  /* one question */
  for (char const *p = host; *p; ) {         /* labels */
    char const *d = p;
    while (*d && *d != '.') d++;
    long l = d - p;
    if (l < 1 || l > 63 || qn + l + 2 > (int) sizeof q - 5) return -1;
    q[qn++] = (unsigned char) l;
    memcpy(q + qn, p, (size_t) l), qn += (int) l;
    p = *d ? d + 1 : d; }
  q[qn++] = 0;
  q[qn++] = 0, q[qn++] = 1;                  /* QTYPE A */
  q[qn++] = 0, q[qn++] = 1;                  /* QCLASS IN */
  int fd = socket(AF_INET, SOCK_DGRAM, 0);
  if (fd < 0) return -1;
  struct timeval tv = {2, 500000};
  setsockopt(fd, SOL_SOCKET, 20, &tv, sizeof tv);   /* SO_RCVTIMEO */
  struct __sain sa = {AF_INET, htons(53), htonl(ns), {0}};
  int got = -1;
  for (int try = 0; got < 0 && try < 2; try++) {
    if (sendto(fd, q, (unsigned long) qn, 0, (struct sockaddr *) &sa, 16) < 0) break;
    unsigned char r[512];
    long rn = recvfrom(fd, r, sizeof r, 0, 0, 0);
    if (rn < 12 || r[0] != q[0] || r[1] != q[1]) continue;
    if ((r[3] & 15) != 0) break;             /* NXDOMAIN &c: this server answered no */
    int an = (r[6] << 8) | r[7];
    int pos = __dnskip(r, rn, 12);           /* the echoed question */
    if (pos < 0) break;
    pos += 4;
    while (an-- > 0 && pos < rn) {           /* first A record wins (CNAMEs ride ahead of it) */
      pos = __dnskip(r, rn, pos);
      if (pos < 0 || pos + 10 > rn) break;
      int ty = (r[pos] << 8) | r[pos + 1], cl = (r[pos + 2] << 8) | r[pos + 3];
      int rdl = (r[pos + 8] << 8) | r[pos + 9];
      pos += 10;
      if (pos + rdl > rn) break;
      if (ty == 1 && cl == 1 && rdl == 4) {
        *out = ((unsigned int) r[pos] << 24) | ((unsigned int) r[pos + 1] << 16)
             | ((unsigned int) r[pos + 2] << 8) | r[pos + 3];
        got = 0;
        break; }
      pos += rdl; } }
  close(fd);
  return got; }
static int __dnslook(char const *host, unsigned int *out) {
  unsigned int ns[3] = {0, 0, 0};
  char slot = 0;
  __lines("/etc/resolv.conf", &slot, ns, __rline);
  if (!slot) ns[0] = 2130706433U, slot = 1;  /* no nameserver line: 127.0.0.1 */
  for (int i = 0; i < slot; i++)
    if (__dnsq(ns[i], host, out) == 0) return 0;
  return -1; }
int getaddrinfo(char const *host, char const *serv, struct addrinfo const *hints, struct addrinfo **res) {
  unsigned int a4 = 2130706433U;             /* 127.0.0.1 */
  if (host) {
    if (strcmp(host, "localhost") != 0 && __quad(host, &a4) < 0
        && __lines("/etc/hosts", host, &a4, __hline) < 0
        && __dnslook(host, &a4) < 0) return -2;                               /* EAI_NONAME */
  } else if (hints && (hints->ai_flags & AI_PASSIVE)) a4 = 0;                 /* INADDR_ANY */
  unsigned int port = 0;
  if (serv) {
    char const *s = serv;
    if (!*s) return -2;
    while (*s >= 48 && *s <= 57) port = port * 10 + (unsigned) (*s++ - 48);
    if (*s || port > 65535) return -2; }
  struct __gai *g = malloc(sizeof(struct __gai));
  if (!g) return -10;                        /* EAI_MEMORY */
  memset(g, 0, sizeof *g);
  g->sa.fam = AF_INET;
  g->sa.port = htons((unsigned short) port);
  g->sa.addr = htonl(a4);
  g->ai.ai_family = AF_INET;
  g->ai.ai_socktype = hints && hints->ai_socktype ? hints->ai_socktype : SOCK_STREAM;
  g->ai.ai_protocol = hints ? hints->ai_protocol : 0;
  g->ai.ai_addrlen = 16;
  g->ai.ai_addr = (struct sockaddr *) &g->sa;
  *res = &g->ai;
  return 0; }
void freeaddrinfo(struct addrinfo *r) { free(r); }

/* ---- sockets ---- */
int socket(int d, int t, int p) { return (int) er(sc3(NR_socket, d, t, p)); }
int connect(int fd, struct sockaddr const *a, socklen_t n) { return (int) er(sc3(NR_connect, fd, (long) a, n)); }
int accept(int fd, struct sockaddr *a, socklen_t *n) { return (int) er(sc3(NR_accept, fd, (long) a, (long) n)); }
long sendto(int fd, void const *b, unsigned long n, int fl, struct sockaddr const *a, socklen_t an) {
  return er(sc6(NR_sendto, fd, (long) b, (long) n, fl, (long) a, an)); }
long recvfrom(int fd, void *b, unsigned long n, int fl, struct sockaddr *a, socklen_t *an) {
  return er(sc6(NR_recvfrom, fd, (long) b, (long) n, fl, (long) a, (long) an)); }
long sendmsg(int fd, struct msghdr const *m, int fl) { return er(sc3(NR_sendmsg, fd, (long) m, fl)); }
long recvmsg(int fd, struct msghdr *m, int fl) { return er(sc3(NR_recvmsg, fd, (long) m, fl)); }
int shutdown(int fd, int how) { return (int) er(sc2(NR_shutdown, fd, how)); }
int bind(int fd, struct sockaddr const *a, socklen_t n) { return (int) er(sc3(NR_bind, fd, (long) a, n)); }
int listen(int fd, int bl) { return (int) er(sc2(NR_listen, fd, bl)); }
int setsockopt(int fd, int lv, int op, void const *v, socklen_t n) {
  return (int) er(sc5(NR_setsockopt, fd, lv, op, (long) v, n)); }

/* ---- strtol / strtod: the reader's number path. the bodies keep libc/str.c's
 * exact semantics, SATURATION INCLUDED (the kernel corpus runs them; the naive
 * strtod measured corpus-green against glibc's in the rung-4 differential). ---- */
static int __digval(int c) {
  if (c >= 48 && c <= 57) return c - 48;
  if (c >= 97 && c <= 122) return c - 87;
  if (c >= 65 && c <= 90) return c - 55;
  return 99; }
long strtol(char const *s, char **endptr, int base) {
  char const *p = s;
  int sign = 1;
  while (*p == 32 || (*p >= 9 && *p <= 13)) p++;
  if (*p == '-') { sign = -1; p++; }
  else if (*p == '+') p++;
  if (*p == '0') {
    ++p;
    if ((base == 0 || base == 16) && (*p == 'x' || *p == 'X')) {
      base = 16;
      ++p;
      if (__digval(*p) >= base) p -= 2; }
    else if (base == 0) { base = 8; --p; }
    else --p; }
  else if (!base) base = 10;
  if (base < 2 || base > 36) return 0;
  /* OVERFLOW SATURATES -- it does not wrap. the standard says so, and every
   * strtol we sit beside (glibc, musl, newlib) does it; the accumulator used to
   * wrap, which is how ONE source text came to read as two different numbers
   * depending on which libc the binary carried. saturating also leaves the
   * caller a signal (the limit value, and ERANGE) where wrapping leaves a
   * plausible lie. the accumulation runs UNSIGNED so LONG_MIN's magnitude is
   * reachable without signed overflow on the way. */
  unsigned long lim = sign < 0 ? (unsigned long) LONG_MAX + 1UL : (unsigned long) LONG_MAX,
                cut = lim / (unsigned long) base, cutd = lim % (unsigned long) base, rc = 0;
  int any = 0, over = 0;
  for (int d; (d = __digval(*p)) < base; p++) {
    any = 1;
    if (over || rc > cut || (rc == cut && (unsigned long) d > cutd)) over = 1;
    else rc = rc * (unsigned long) base + (unsigned long) d; }
  if (endptr) *endptr = (char *) (any ? p : s);
  if (!any) return 0;
  if (over) { __errno_v = ERANGE; return sign < 0 ? LONG_MIN : LONG_MAX; }
  return (long) (sign < 0 ? 0UL - rc : rc); }
double atof(char const *s) { return strtod(s, 0); }
/* the libc math faces over the am floor (am.c's seven transcendentals ride
 * m_am.o in every ladder link); the rest are exact derivations. tan and the
 * arc trio are DERIVED (a few ulp looser than a dedicated kernel) -- enough
 * for the ladder; a consumer that measures gets its own am kernel. */
double am_sqrt(double), am_exp(double), am_log(double);
double am_sin(double), am_cos(double), am_atan2(double, double), am_pow(double, double);
double sqrt(double x) { return am_sqrt(x); }
double exp(double x) { return am_exp(x); }
double log(double x) { return am_log(x); }
double sin(double x) { return am_sin(x); }
double cos(double x) { return am_cos(x); }
double tan(double x) { return am_sin(x) / am_cos(x); }
double pow(double x, double y) { return am_pow(x, y); }
double atan2(double y, double x) { return am_atan2(y, x); }
double atan(double x) { return am_atan2(x, 1.0); }
double asin(double x) { return am_atan2(x, am_sqrt(1.0 - x * x)); }
double acos(double x) { return am_atan2(am_sqrt(1.0 - x * x), x); }
double log2(double x) { return am_log(x) * 1.4426950408889634; }
double log10(double x) { return am_log(x) * 0.4342944819032518; }
double sinh(double x) { double e = am_exp(x); return (e - 1.0 / e) / 2.0; }
double cosh(double x) { double e = am_exp(x); return (e + 1.0 / e) / 2.0; }
double tanh(double x) { double e = am_exp(2.0 * x); return (e - 1.0) / (e + 1.0); }
double fabs(double x) { return x <= 0 ? 0.0 - x : x; }
static double __trunc9(double x) {                 /* |x| < 2^52 assumed */
  double t = (double) (long) x;
  return t; }
double floor(double x) {
  if (x != x || x >= 9007199254740992.0 || x <= -9007199254740992.0) return x;
  double t = __trunc9(x);
  return t > x ? t - 1.0 : t; }
double ceil(double x) {
  if (x != x || x >= 9007199254740992.0 || x <= -9007199254740992.0) return x;
  double t = __trunc9(x);
  return t < x ? t + 1.0 : t; }
double fmod(double x, double y) {
  if (y == 0.0 || x != x || y != y) return 0.0 / 0.0;
  double q = x / y;
  if (q >= 9007199254740992.0 || q <= -9007199254740992.0) return 0.0;   /* quotient past exact-int: stance */
  double r = x - __trunc9(q) * y;
  return r; }
/* frexp/ldexp: exact exponent surgery on the IEEE bits (no math floor needed) */
double frexp(double x, int *e) {
  union { double d; unsigned long u; } b;
  b.d = x;
  int ex = (int) ((b.u >> 52) & 2047);
  *e = 0;
  if (ex == 2047 || x == 0) return x;              /* inf/nan/0 ride through, *e 0 */
  if (ex == 0) {                                   /* denormal: normalize by 2^64 first */
    b.d = x * 18446744073709551616.0;
    ex = (int) ((b.u >> 52) & 2047) - 64; }
  *e = ex - 1022;
  b.u = (b.u & 0x800ffffffffffffful) | 0x3fe0000000000000ul;
  return b.d; }
static double __e2d(int n) {                       /* 2^n for normal n */
  union { double d; unsigned long u; } b;
  b.u = ((unsigned long) (n + 1023)) << 52;
  return b.d; }
double ldexp(double x, int n) {                    /* x * 2^n, clamped through the rim in steps */
  if (n > 1023) { x *= __e2d(1023); n -= 1023;
    if (n > 1023) { x *= __e2d(1023); n -= 1023; if (n > 1023) n = 1023; } }
  else if (n < -1022) { x *= __e2d(-969); n += 969;
    if (n < -1022) { x *= __e2d(-969); n += 969; if (n < -1022) n = -1022; } }
  return x * __e2d(n); }
/* the math floor's exact reader (crew/moon/lib/math/am.c -- linked wherever
   nolibc is: the raw love build and the whole moon userland): correctly
   rounded, so read(show x) = x holds off-glibc too. The naive accumulator
   that lived here parsed "0.3" one ulp off -- masked until love's printer
   went shortest-roundtrip, then loud in test_raw. */
double am_strtod(char const *, char **);
/* ⚠ THE LIBC FACE IS NOT am_strtod's FACE, and the wrapper is where they part:
 * am_strtod is love's float reader, and the reader hands it a whole TOKEN, so
 * it skips no leading space. C's strtod owes that, and owes endptr = the
 * ORIGINAL nptr when nothing converts. doing it here keeps am.c exactly what
 * love wants -- correctly rounded and nothing else. found by test/libc/num.c.
 * ⚠ the SIGN of a zero needs nothing: am_strtod gets -0.0 right on its own.
 * it did not while mooncc lowered -d as 0.0 - d (crew/moon/gen.l), and a
 * wrapper that "fixed" it here would now flip the sign BACK, since -0.0 == 0.0
 * tests true. */
double strtod(char const *s, char **end) {
  char const *p = s;
  while (*p == 32 || (*p >= 9 && *p <= 13)) p++;
  char *e = (char *) p;
  double v = am_strtod(p, &e);
  if (e == p) { if (end) *end = (char *) s; return 0.0; }   /* no conversion: the ORIGINAL s */
  if (end) *end = e;
  return v; }
/* the unsigned twin: strtol's digit walk, saturating at ULONG_MAX the same way,
 * with the ONE wrap the standard does ask for -- a leading minus negates the
 * magnitude modulo 2^64 rather than refusing. */
static unsigned long __strtoux(char const *s, char **endptr, int base) {
  char const *p = s;
  int neg = 0;
  while (*p == 32 || (*p >= 9 && *p <= 13)) p++;
  if (*p == '-') { neg = 1; p++; } else if (*p == '+') p++;
  if (*p == '0') {
    ++p;
    if ((base == 0 || base == 16) && (*p == 'x' || *p == 'X')) { base = 16; ++p; if (__digval(*p) >= base) p -= 2; }
    else if (base == 0) { base = 8; --p; }
    else --p; }
  else if (!base) base = 10;
  if (base < 2 || base > 36) return 0;
  unsigned long cut = ULONG_MAX / (unsigned long) base, cutd = ULONG_MAX % (unsigned long) base, rc = 0;
  int any = 0, over = 0;
  for (int d; (d = __digval(*p)) < base; p++) {
    any = 1;
    if (over || rc > cut || (rc == cut && (unsigned long) d > cutd)) over = 1;
    else rc = rc * (unsigned long) base + (unsigned long) d; }
  if (endptr) *endptr = (char *) (any ? p : s);
  if (!any) return 0;
  if (over) { __errno_v = ERANGE; return ULONG_MAX; }
  return neg ? 0UL - rc : rc; }
long strtoll(char const *s, char **endptr, int base) { return strtol(s, endptr, base); }
unsigned long strtoul(char const *s, char **endptr, int base) { return __strtoux(s, endptr, base); }
unsigned long strtoull(char const *s, char **endptr, int base) { return __strtoux(s, endptr, base); }
unsigned long strtoumax(char const *s, char **endptr, int base) { return __strtoux(s, endptr, base); }

/* qsort: shellsort (Ciura-ish 3x gaps would be nicer, but n/2 halving is small
 * and tar's arrays are short). in-place byte swap of size-sz elements. */
void qsort(void *base, size_t n, size_t sz, int (*cmp)(void const *, void const *)) {
  char *a = base;
  for (size_t gap = n / 2; gap > 0; gap /= 2)
    for (size_t i = gap; i < n; i++)
      for (size_t j = i; j >= gap && cmp(a + (j - gap) * sz, a + j * sz) > 0; j -= gap) {
        char *x = a + (j - gap) * sz, *y = a + j * sz;
        for (size_t k = 0; k < sz; k++) { char t = x[k]; x[k] = y[k]; y[k] = t; } } }
/* qsort's twin, and the headers already named it: a plain binary search over
 * the half-open span, answering the ELEMENT or null. */
void *bsearch(void const *key, void const *base, size_t n, size_t sz,
              int (*cmp)(void const *, void const *)) {
  char const *a = base;
  size_t lo = 0, hi = n;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    int r = cmp(key, a + mid * sz);
    if (r == 0) return (void *) (a + mid * sz);
    if (r < 0) hi = mid; else lo = mid + 1; }
  return 0; }

/* exec's variadic pair: gather (arg0, .., NULL) off the stack, then execv[p]. */
int execl(char const *p, char const *a0, ...) {
  char *av[256]; int n = 0;
  va_list ap; va_start(ap, a0);
  av[n++] = (char *) a0;
  while (n < 255 && (av[n] = va_arg(ap, char *))) n++;
  av[n] = 0;
  va_end(ap);
  return execv(p, av); }
int execlp(char const *f, char const *a0, ...) {
  char *av[256]; int n = 0;
  va_list ap; va_start(ap, a0);
  av[n++] = (char *) a0;
  while (n < 255 && (av[n] = va_arg(ap, char *))) n++;
  av[n] = 0;
  va_end(ap);
  return execvp(f, av); }
/* system: fork, /bin/sh -c, wait. no signal juggling (love is single-threaded). */
int system(char const *cmd) {
  if (!cmd) return 1;                          /* a shell is available */
  int pid = fork();
  if (pid < 0) return -1;
  if (pid == 0) {
    char *av[4]; av[0] = "sh"; av[1] = "-c"; av[2] = (char *) cmd; av[3] = 0;
    execv("/bin/sh", av);
    _exit(127); }
  int st = 0;
  while (waitpid(pid, &st, 0) < 0) if (__errno_v != EINTR) return -1;
  return st; }
/* popen: system's shape with a pipe spliced onto the child's stdout ("r") or
 * stdin ("w"); the child pid rides the FILE for pclose's wait (m4 esyscmd). */
FILE *popen(char const *cmd, char const *mode) {
  int fds[2];
  int rd = mode[0] == 'r';
  if (pipe(fds) < 0) return 0;
  int pid = fork();
  if (pid < 0) { close(fds[0]); close(fds[1]); return 0; }
  if (pid == 0) {
    dup2(rd ? fds[1] : fds[0], rd ? 1 : 0);
    close(fds[0]); close(fds[1]);
    char *av[4]; av[0] = "sh"; av[1] = "-c"; av[2] = (char *) cmd; av[3] = 0;
    execv("/bin/sh", av);
    _exit(127); }
  close(rd ? fds[1] : fds[0]);
  FILE *f = malloc(sizeof(FILE) + 4096);
  if (!f) { close(rd ? fds[0] : fds[1]); return 0; }
  memset(f, 0, sizeof(FILE));
  f->fd = rd ? fds[0] : fds[1];
  f->wr = !rd;
  f->heap = 1;
  f->pid = pid;
  if (!rd) { f->buf = (unsigned char *) (f + 1); f->cap = 4096; }
  return f; }
int pclose(FILE *f) {
  int pid = f->pid, st = 0;
  int r = fclose(f);
  if (r == EOF) return -1;
  while (waitpid(pid, &st, 0) < 0) if (__errno_v != EINTR) return -1;
  return st; }
/* tmpfile: a mktemp'd /tmp file opened w+ and unlinked at once, so it lives
 * exactly as long as the FILE (m4's diversions write it, rewind, read back). */
FILE *tmpfile(void) {
  char buf[16];
  strcpy(buf, "/tmp/aiXXXXXX");
  mktemp(buf);
  if (!buf[0]) return 0;
  FILE *f = fopen(buf, "w+");
  if (f) unlink(buf);
  return f; }
int remove(char const *p) {                        /* the ISO face: unlink, a directory falls to rmdir */
  int r = unlink(p);
  return r == 0 ? 0 : rmdir(p); }
char *tmpnam(char *s) {                            /* the ISO face over mktemp (lua's os.tmpname) */
  static char b[20];
  if (!s) s = b;
  strcpy(s, "/tmp/aiXXXXXX");
  mktemp(s);
  return s[0] ? s : 0; }
/* mktemp: fill the trailing XXXXXX from the pid and bump until the name is
 * free (racy by design -- the caller opens it; m4's diversion files). */
char *mktemp(char *tmpl) {
  size_t n = strlen(tmpl);
  if (n < 6 || strcmp(tmpl + n - 6, "XXXXXX")) { tmpl[0] = 0; return tmpl; }
  char *x = tmpl + n - 6;
  unsigned long v = (unsigned long) getpid();
  for (int k = 0; k < 100; k++, v += 7777) {
    unsigned long w = v;
    for (int i = 0; i < 6; i++) { x[i] = 'a' + w % 26; w /= 26; }
    if (access(tmpl, 0) < 0) return tmpl; }
  tmpl[0] = 0;
  return tmpl; }
/* one fixed "C" locale, so setlocale just answers its name. */
char *setlocale(int cat, char const *loc) { (void) cat; (void) loc; return (char *) "C"; }
struct lconv *localeconv(void) {                   /* the C locale's table: "." and empties */
  static struct lconv c = { (char *) ".", (char *) "", (char *) "",
    (char *) "", (char *) "", (char *) "", (char *) "", (char *) "",
    (char *) "", (char *) "", 127, 127, 127, 127, 127, 127, 127, 127 };
  return &c; }

/* getc/fputs/ferror over the unbuffered read streams; fscanf reads char-by-char
 * (no ungetc, so it consumes the field terminator -- tar's lone use is "%d"). */
int getc(FILE *f) {
  unsigned char c;
  if (f->un) { int r = f->un - 1; f->un = 0; return r; }
  long k = read(f->fd, &c, 1);
  if (k <= 0) { if (k < 0) f->err = 1; else f->eof = 1; return EOF; }
  return c; }
int ungetc(int c, FILE *f) {
  if (c == EOF || f->un) return EOF;
  f->un = (c & 255) + 1;
  f->eof = 0;
  return c & 255; }
int fputs(char const *s, FILE *f) { size_t n = strlen(s); return fwrite(s, 1, n, f) == n ? 0 : EOF; }
/* ---- the stdout/stdin shorthands and the odds and ends the HEADERS already
 * promised. every one of these was declared in crew/moon/include/ with no body
 * anywhere, so a program calling it compiled and then died at the LINK under
 * CC=mooncc while building fine against glibc -- gnulib's progname module
 * reaches getprogname exactly that way. test/libc/'s header-completeness phase
 * (test/gate/libc.sh) is what found them and is what keeps the promise honest
 * from here: a name the headers declare must have a definition. ---- */
int putchar(int c) { return fputc(c, stdout); }
int puts(char const *s) { return fputs(s, stdout) == EOF || fputc('\n', stdout) == EOF ? EOF : 0; }
int fgetc(FILE *f) { return getc(f); }
int getchar(void) { return getc(stdin); }
int ferror(FILE *f) { return f->err; }
int feof(FILE *f) { return f->eof; }
void clearerr(FILE *f) { f->err = 0; f->eof = 0; }
/* sscanf, the string twin, %d only (m4 builtin.c's lone use: a divert number). */
int sscanf(char const *s, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int got = 0;
  for (; *fmt; fmt++) {
    if (*fmt == '%' && fmt[1] == 'd') {
      char *e;
      long v = strtol(s, &e, 10);
      if (e == s) break;
      *va_arg(ap, int *) = (int) v;
      s = e; fmt++; got++; }
    else if (*fmt == ' ') { while (*s == ' ' || (*s >= 9 && *s <= 13)) s++; }
    else { if (*s != *fmt) break; s++; } }
  va_end(ap);
  return got; }
static int __vfscanf(FILE *f, char const *fmt, va_list ap) {
  int got = 0, c;
  for (; *fmt; fmt++) {
    if (*fmt == '%') {
      fmt++;
      if (*fmt == 'd' || *fmt == 'u' || *fmt == 'x' || *fmt == 's')
        do { c = getc(f); } while (c == 32 || (c >= 9 && c <= 13));
      if (*fmt == 'd' || *fmt == 'u' || *fmt == 'x') {
        int base = *fmt == 'x' ? 16 : 10, sign = 1, any = 0, d;
        if (*fmt == 'd' && (c == '-' || c == '+')) { if (c == '-') sign = -1; c = getc(f); }
        long v = 0;
        while ((d = __digval(c)) < base) { v = v * base + d; any = 1; c = getc(f); }
        if (!any) break;
        *va_arg(ap, int *) = (int) (sign * v);
        got++; }
      else if (*fmt == 's') {
        char *out = va_arg(ap, char *); int i = 0;
        while (c != EOF && !(c == 32 || (c >= 9 && c <= 13))) { out[i++] = (char) c; c = getc(f); }
        out[i] = 0; got++; }
      else if (*fmt == 'c') { c = getc(f); if (c == EOF) break; *va_arg(ap, char *) = (char) c; got++; } }
    else if (*fmt == 32 || (*fmt >= 9 && *fmt <= 13)) ;   /* fmt whitespace: no peek, skip */
    else { c = getc(f); if (c != (unsigned char) *fmt) break; } }
  return got; }
/* one body, three faces -- fscanf and scanf differ only in which stream */
int fscanf(FILE *f, char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int r = __vfscanf(f, fmt, ap);
  va_end(ap);
  return r; }
int scanf(char const *fmt, ...) {
  va_list ap; va_start(ap, fmt);
  int r = __vfscanf(stdin, fmt, ap);
  va_end(ap);
  return r; }

/* no name database yet: every passwd/group lookup misses, so tar prints numeric
 * owner/group (its own fallback). a real /etc/passwd walk is a later rung. */
struct passwd *getpwuid(uid_t u) { (void) u; return 0; }
struct passwd *getpwnam(char const *n) { (void) n; return 0; }
struct group *getgrgid(gid_t g) { (void) g; return 0; }
struct group *getgrnam(char const *n) { (void) n; return 0; }
void setgrent(void) { }

/* ---- the load bias: 0 for a fixed-base ET_EXEC, the ASLR slide for a -pie
 * ET_DYN. AT_PHDR is the runtime address of the program headers, which sit at
 * file offset 64 inside the p_offset==0 PT_LOAD, so bias = AT_PHDR - 64 - that
 * segment's link-time p_vaddr (0x400000 for EXEC -> 0; 0 for PIE -> the slide). ---- */
static unsigned long __ai_bias(void) {
  unsigned long phdr = 0; Elf64_Half phnum = 0;
  for (long *a = __auxv; a && a[0]; a += 2) {
    if (a[0] == 3) phdr = (unsigned long) a[1];        /* AT_PHDR */
    if (a[0] == 5) phnum = (Elf64_Half) a[1]; }        /* AT_PHNUM */
  if (!phdr) return 0;
  Elf64_Phdr const *ph = (Elf64_Phdr const *) phdr;
  for (Elf64_Half i = 0; i < phnum; i++)
    if (ph[i].p_type == PT_LOAD && ph[i].p_offset == 0)
      return phdr - 64 - (unsigned long) ph[i].p_vaddr;
  return 0; }

/* ---- -pie self-relocation. The linker (crew/holo/link.l) laid the exe at base 0
 * and left every abs64 data pointer holding its base-0 offset, plus a table of
 * those sites bracketed by __start_/__stop_ai_rela. Add the real load base to each
 * -- the whole of static-PIE relocation, no dynamic loader. Must run before any
 * such pointer is dereferenced (top of __ai_start). An ET_EXEC binary links an
 * EMPTY table (start == stop), so this is a no-op there. ---- */
extern long __start_ai_rela[], __stop_ai_rela[];
static void __ai_reloc(void) {
  unsigned long bias = __ai_bias();
  for (long *p = __start_ai_rela; p < __stop_ai_rela; p++)
    *(unsigned long *) (bias + (unsigned long) *p) += bias; }

/* ---- dl_iterate_phdr off the auxv (AT_PHDR/AT_PHNUM): one callback covers "the
 * main program", carrying the real load bias so image.c's bake walk bounds the
 * in-binary pointers correctly under -pie (0 for a fixed-base ET_EXEC). ---- */
int dl_iterate_phdr(int (*cb)(struct dl_phdr_info *, unsigned long, void *), void *data) {
  unsigned long phdr = 0, phnum = 0;
  for (long *a = __auxv; a && a[0]; a += 2) {
    if (a[0] == 3) phdr = (unsigned long) a[1];        /* AT_PHDR */
    if (a[0] == 5) phnum = (unsigned long) a[1]; }     /* AT_PHNUM */
  if (!phdr) return 0;
  struct dl_phdr_info in;
  memset(&in, 0, sizeof in);
  in.dlpi_addr = __ai_bias();
  in.dlpi_name = "";
  in.dlpi_phdr = (Elf64_Phdr const *) phdr;
  in.dlpi_phnum = (Elf64_Half) phnum;
  return cb(&in, sizeof in, data); }

/* ---- the entry: crt0 hands us the OS stack pointer (argc at [sp]); unpack
 * argv/envp/auxv, arm stdio, run main, exit with its answer. this STRONG
 * definition overrides crt0's weak call-main tail (the linker's weak machinery
 * is the whole switch -- no flags anywhere). ---- */
void __ai_start(long *sp) {
  long argc = sp[0];
  char **argv = (char **) (sp + 1);
  char **e = argv + argc + 1;
  if (argc > 0 && argv[0]) __ai_progname = argv[0];   /* getprogname's answer */
  environ = e;
  while (*e) e++;
  __auxv = (long *) (e + 1);
  __ai_reloc();                 /* -pie: slide abs64 data pointers before any is used (no-op for ET_EXEC) */
  stdout->fd = 1;
  stdout->wr = 1;
  stdout->buf = __obuf;
  stdout->cap = sizeof __obuf;
  stdout->line = isatty(1);
  stderr->fd = 2;
  stderr->wr = 1;
  exit(main((int) argc, argv)); }
