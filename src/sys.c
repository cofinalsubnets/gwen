// src/sys.c -- inle's syscall door, and the whole of it. nolibc's 76 sys/*
// members reach one seam (impl.h's sc0..sc6 -> __ai_call, no inline asm
// anywhere in that C), and __ai_call parts its callers by __ai_osv: a hosted
// kernel takes the mksys.l lay that issues `syscall` or `svc`, and a negative
// osv -- written at kmain, we ARE the kernel -- takes __ai_inle, this file's C
// answer. That one arm is what lets nolibc, and every lane written against it,
// stand on this kernel instead of a hosted one.
//
// The numbers are LINUX'S, per arch, straight off impl.h's NR_* -- the tree
// carries those tables for x86_64 and aarch64 already, and inle owes no
// compatibility to anyone, so taking them costs nothing and translates nothing:
// a negative osv reads as linux at every member's own branch.
//
// ⚠ AN UNMAPPED NUMBER ANSWERS -ENOSYS, and that is the refusal protocol, not a
// gap to be ashamed of -- the same one mount and unshare wear off linux. A lane
// asks its libc what it carries, never which kernel it is standing on.
#include "../crew/moon/lib/nolibc/impl.h"
#include <stdint.h>

// the seat is nolibc's core.c now (plan C2: the fused link carries it whole --
// errno, the streams, the mmap-arena malloc). what a hosted __ai_start would
// arm, the kernel arms here instead: an environment that starts empty, and the
// std streams write-through on fds 1 and 2 with no buffer (cap 0, so every
// byte goes straight through write -- ⚠ SEAT-BLIND: a seated task's C-level
// printf reaches the console where its port reaches the pipe; love writes
// through ports, which seat). kmain calls this right after it writes the osv.
static char *k_env0[] = { 0 };
void k_seat_init(void) {
  environ = k_env0;
  stdout->fd = 1; stdout->wr = 1;
  stderr->fd = 2; stderr->wr = 1; }

// the kernel side (kmain.c): a raw fd through the k_sources row, no port above
// it -- and SEAT-BLIND, which is the law and not a gap. The seat is a property
// of the PORT layer: k_fd_eff is called from k_port_readn, k_port_writen, k_row_close
// and k_procseat, and from nowhere else, so an fd spelled in love is already an
// absolute row and only a port's own fd is ever remapped. A syscall sits under
// the port by construction, exactly as on a real kernel, where the number the
// trap carries is already the calling process's own.
// ⚠ THE DIVERGENCE THIS BUYS, named so it is not rediscovered as a bug: a
// SEATED task spelling `write(1, ..)` reaches row 1, where POSIX would reach
// whatever its parent seated. Nothing does -- love's stdio goes through the
// folded ports, and src/posix.c touches an implicit fd at three terminal-
// control calls and no data I/O at all. Closing it means per-task row tables
// (a real fd table), not an ambient g: `g` MOVES under collection, so the
// running task cannot be cached, only threaded.
extern long k_fd_write(int fd, void const *b, long n);
extern long k_fd_read(int fd, void *b, long n);
extern long k_fd_close(int fd);
extern long k_fd_lseek(int fd, long off, int whence);
// the kernel free list (kmain.c), the page supply under the mmap arm below
extern void *kmallocw(uintptr_t n);
extern void kfree(void *p);

// the ramfs path faces (kmain.c), each 0 or a negative errno. k_st is what the
// ramfs KNOWS about a path; a struct stat's ino/nlink/uid/dev have no answer
// down there, so the fabrication is made HERE, in the stat arm, where it shows.
struct k_st { uintptr_t size, ms, mode; };
extern int k_fs_open(char const *p, uintptr_t pn, char m);
extern int k_fs_stat(char const *p, uintptr_t pn, struct k_st *st);
extern int k_fs_mkdir(char const *p, uintptr_t pn, uintptr_t mode);
extern int k_fs_rmdir(char const *p, uintptr_t pn);
extern int k_fs_unlink(char const *p, uintptr_t pn);
extern int k_fs_rename(char const *o, uintptr_t on, char const *n, uintptr_t nn);
extern int k_fs_chdir(char const *p, uintptr_t pn);
extern int k_fs_getcwd(char *b, uintptr_t n);
extern int k_fs_chmod(char const *p, uintptr_t pn, uintptr_t mode);
extern int k_fs_utime(char const *p, uintptr_t pn, uintptr_t ms);
extern uintptr_t k_clock_ms(void);     // ms since the epoch, the kernel's one scale

// the fd faces (kmain.c), g-free by construction: rows, pipe queues and the
// dents cursor are kernel memory, and g only ever entered their old shapes to
// build love answers. what g truly knows -- WHICH TASK RUNS -- never crosses
// this seam: the syscall door is the PROCESS's (inle is one process, tasks its
// threads), and per-task questions stay with the nifs, where g is threaded.
extern long k_fd_pipe(int fds[2]);
extern long k_fd_dup(int src, int at);
extern long k_fd_dup3(int src, int dst);
extern long k_fd_stat(int fd, struct k_st *st);
extern long k_fd_dents(int fd, void *buf, long cap);
extern long k_fs_opendir(char const *p, uintptr_t pn);

// a dirfd is honored as AT_FDCWD only: the ramfs has one cwd, and an absolute
// path ignores its dirfd by POSIX's own rule. any other seat refuses loudly.
static long at_ok(long dfd, char const *p) {
  if (!p) return -EFAULT;
  return (dfd == AT_FDCWD || p[0] == '/') ? 0 : -ENOTSUP; }

static long k_openat(long dfd, char const *p, long fl, long mode) {
  (void) mode;                                  // the ramfs mints its own (0644)
  long r = at_ok(dfd, p);
  if (r) return r;
  long acc = fl & 3;
  if (fl & O_DIRECTORY) {                       // opendir's lane, read-only by nature
    if (acc != O_RDONLY) return -EINVAL;
    return k_fs_opendir(p, strlen(p)); }
  char m = acc == O_RDONLY ? 'r'
         : acc != O_WRONLY ? 0
         : (fl & O_APPEND) ? 'a' : 'w';
  if (!m) return -EINVAL;                       // the ramfs has no O_RDWR door
  r = k_fs_open(p, strlen(p), m);
  // POSIX opens a directory read-only; the face keeps its 'r' misses cheap
  // (one k_find), so the directory answer is assembled on this slow path --
  // -EISDIR for an explicit entry, a stat for a synthesized one.
  if (r == -EISDIR && m == 'r') return k_fs_opendir(p, strlen(p));
  if (r == -ENOENT && m == 'r') {
    struct k_st t;
    if (!k_fs_stat(p, strlen(p), &t) && (t.mode & 040000))
      return k_fs_opendir(p, strlen(p)); }
  return r; }

// what the ramfs knows lands as it is; ino/nlink/uid/dev are invented here,
// where the invention shows. fstat and newfstatat share the one shape.
static void stat_fab(struct stat *st, struct k_st const *t) {
  *st = (struct stat) {0};
  st->st_mode = (unsigned) t->mode;
  st->st_nlink = 1;
  st->st_size = (long) t->size;
  st->st_blksize = 4096;
  st->st_blocks = (long) ((t->size + 511) / 512);
  st->st_mtim.tv_sec = (long) (t->ms / 1000);
  st->st_mtim.tv_nsec = (long) (t->ms % 1000) * 1000000;
  st->st_atim = st->st_ctim = st->st_mtim; }

static long k_statat(long dfd, char const *p, struct stat *st, long fl) {
  (void) fl;                                    // no symlinks to not-follow
  long r = at_ok(dfd, p);
  if (r || !st) return r ? r : -EFAULT;
  struct k_st t;
  if ((r = k_fs_stat(p, strlen(p), &t))) return r;
  stat_fab(st, &t);
  return 0; }

static long k_utimeat(long dfd, char const *p, struct timespec const *ts, long fl) {
  (void) fl;
  long r = at_ok(dfd, p);
  if (r) return r;
  uintptr_t ms;
  if (!ts || ts[1].tv_nsec == UTIME_NOW) ms = k_clock_ms();
  else if (ts[1].tv_nsec == UTIME_OMIT) return 0;   // nothing asked of the mtime
  else ms = (uintptr_t) ts[1].tv_sec * 1000 + (uintptr_t) ts[1].tv_nsec / 1000000;
  return k_fs_utime(p, strlen(p), ms); }

#ifdef K_TEST
// the test instrument's door (kmain.c's `syscall` nif): a row's NAME to its
// number. It lives here because the numbers are impl.h's and ARCH-KEYED --
// close is 3 on x86_64 and 57 on aarch64 -- so a love test that spelled one
// would pass on the seat it was written on and mean nothing on the other.
long k_sys_nr(char const *nm, long n) {
  struct { char const *n; long nr; } const t[] = {
    {"read", NR_read}, {"write", NR_write},
    {"close", NR_close}, {"lseek", NR_lseek},
    {"openat", NR_openat}, {"newfstatat", NR_newfstatat},
    {"mkdirat", NR_mkdirat}, {"unlinkat", NR_unlinkat},
    {"renameat", NR_renameat}, {"chdir", NR_chdir},
    {"getcwd", NR_getcwd}, {"fchmodat", NR_fchmodat},
    {"utimensat", NR_utimensat},
    {"pipe2", NR_pipe2}, {"dup3", NR_dup3}, {"fcntl", NR_fcntl},
    {"fstat", NR_fstat}, {"getdents64", NR_getdents64},
    {"getpid", NR_getpid}, {"clock_gettime", NR_clock_gettime} };
  for (unsigned i = 0; i < sizeof t / sizeof *t; i++)
    if ((long) strlen(t[i].n) == n && !memcmp(t[i].n, nm, (unsigned long) n))
      return t[i].nr;
  return -1; }
#endif

long __ai_inle(long n, long a, long b, long c, long d, long e, long f) {
  (void) e, (void) f;
  long r;
  switch (n) {
    case NR_write: return k_fd_write((int) a, (void const *) b, c);
    case NR_read:  return k_fd_read((int) a, (void *) b, c);
    case NR_close: return k_fd_close((int) a);
    case NR_lseek: return k_fd_lseek((int) a, b, (int) c);
    case NR_openat:     return k_openat(a, (char const *) b, c, d);
    case NR_newfstatat: return k_statat(a, (char const *) b, (struct stat *) c, d);
    case NR_utimensat:  return k_utimeat(a, (char const *) b, (struct timespec const *) c, d);
    case NR_mkdirat:
      if ((r = at_ok(a, (char const *) b))) return r;
      return k_fs_mkdir((char const *) b, strlen((char const *) b), (uintptr_t) c & 07777);
    case NR_unlinkat:
      if ((r = at_ok(a, (char const *) b))) return r;
      return (c & AT_REMOVEDIR)
        ? k_fs_rmdir((char const *) b, strlen((char const *) b))
        : k_fs_unlink((char const *) b, strlen((char const *) b));
    case NR_renameat:
      if ((r = at_ok(a, (char const *) b)) || (r = at_ok(c, (char const *) d))) return r;
      return k_fs_rename((char const *) b, strlen((char const *) b),
                         (char const *) d, strlen((char const *) d));
    case NR_chdir:
      if (!a) return -EFAULT;
      return k_fs_chdir((char const *) a, strlen((char const *) a));
    case NR_getcwd:
      if (!a) return -EFAULT;
      if ((r = k_fs_getcwd((char *) a, (uintptr_t) b))) return r;
      return (long) strlen((char *) a) + 1;     // getcwd(2)'s answer: the length, NUL counted
    case NR_fchmodat:
      if ((r = at_ok(a, (char const *) b))) return r;
      return k_fs_chmod((char const *) b, strlen((char const *) b), (uintptr_t) c);
    case NR_pipe2:
      if (!a) return -EFAULT;
      if (b) return -EINVAL;                    // no close-on-exec where nothing execs
      return k_fd_pipe((int *) a);
    case NR_dup3:
      if (c) return -EINVAL;
      return k_fd_dup3((int) a, (int) b);
    case NR_fcntl:
      switch (b) {
        case F_DUPFD: return k_fd_dup((int) a, (int) c);
        case F_GETFD: case F_SETFD: case F_GETFL: {   // flag words this seat does not keep
          struct k_st t;
          return k_fd_stat((int) a, &t) ? -EBADF : 0; }
        default: return -EINVAL; }
    case NR_fstat: {
      if (!b) return -EFAULT;
      struct k_st t;
      if ((r = k_fd_stat((int) a, &t))) return r;
      stat_fab((struct stat *) b, &t);
      return 0; }
    case NR_getdents64:
      if (!b) return -EFAULT;
      return k_fd_dents((int) a, (void *) b, c);
    // the allocator's page door: nolibc's malloc runs its mmap arenas on this
    // kernel as on any other -- kmallocw supplies the pages. anonymous and
    // kernel-placed only; the true base rides the word below the aligned
    // block, where munmap's arm reads it back. zeroed, because MAP_ANONYMOUS
    // promises zeroed pages and calloc's direct lane trusts exactly that.
    case NR_mmap: {
      if (a || (int) e != -1 || b <= 0) return -38;         // ENOSYS: not our shape
      void *m = kmallocw(((uintptr_t) b + 2 * 4096) / sizeof(uintptr_t));
      if (!m) return -12;                                   // ENOMEM
      uintptr_t p = ((uintptr_t) m + sizeof(void *) + 4095) & ~4095ull;
      ((void **) p)[-1] = m;
      memset((void *) p, 0, (size_t) b);
      return (long) p; }
    case NR_munmap:
      if (!a || (a & 4095)) return -22;                     // EINVAL
      kfree(((void **) a)[-1]);
      return 0;
    // the map is not flat: mkboot.l puts NX on the hhdm, and these are 2 MiB entries the
    // identity window shares, so nothing here can lift it off one page. R/W is already
    // what every heap page is -- answer that, and REFUSE the exec ask rather than
    // returning 0 to a caller whose next move is to jump into what it just protected.
    case NR_mprotect: return (c & 4) ? -EACCES : 0;
    // one clock, the wall: ai_clock's body is clock_gettime now (src/seat.c),
    // so this arm is where the kernel's scale becomes a timespec.
    case NR_clock_gettime: {
      if (a) return -EINVAL;                    // CLOCK_REALTIME only
      if (!b) return -EFAULT;
      struct timespec *ts = (struct timespec *) b;
      uintptr_t ms = k_clock_ms();
      ts->tv_sec = (long) (ms / 1000);
      ts->tv_nsec = (long) (ms % 1000) * 1000000;
      return 0; }
    // inle is ONE process and love tasks are its threads, so getpid answers
    // the machine's constant -- what every thread of a process reads. the
    // TASK pid is love's question, and its nif keeps g, where the answer is.
    case NR_getpid: return 1;
    default:       return -38; } }                       // ENOSYS, canonically
