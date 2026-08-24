// free/sys.c -- inle's syscall door, and the whole of it. nolibc's 76 sys/*
// members reach __ai_sys and nothing else (impl.h's sc0..sc6 -> __ai_call, no
// inline asm anywhere in that C), so answering it here is what lets that libc --
// and every lane written against it -- stand on this kernel instead of a hosted
// one. On a hosted seat the same symbol is a mksys.l lay that issues `syscall`
// or `svc`; here it is a C function, because the kernel it would have called is
// this file's other side.
//
// The numbers are LINUX'S, per arch, straight off impl.h's NR_* -- the tree
// carries those tables for x86_64 and aarch64 already, and inle owes no
// compatibility to anyone, so taking them costs nothing and translates nothing.
// os.c hands __ai_call a 1 under __inle__: the no-translation lane riscv takes.
//
// ⚠ AN UNMAPPED NUMBER ANSWERS -ENOSYS, and that is the refusal protocol, not a
// gap to be ashamed of -- the same one mount and unshare wear off linux. A lane
// asks its libc what it carries, never which kernel it is standing on.
#include "../crew/moon/lib/nolibc/impl.h"
#include <stdint.h>

// errno lives here rather than by linking nolibc's core.c: that member also
// carries malloc, environ, stdio and the process entry, every one of which this
// kernel already owns. Its other halves belong to a seat, and inle is the seat.
int __errno_v;
int *__errno_location(void) { return &__errno_v; }

// the kernel side (kmain.c): a raw fd through the k_sources row, no port above
// it -- and SEAT-BLIND, which is the law and not a gap. The seat is a property
// of the PORT layer: k_fd_eff is called from fd_readn, fd_writen, ai_fd_close
// and k_procseat, and from nowhere else, so an fd spelled in love is already an
// absolute row and only a port's own fd is ever remapped. A syscall sits under
// the port by construction, exactly as on a real kernel, where the number the
// trap carries is already the calling process's own.
// ⚠ THE DIVERGENCE THIS BUYS, named so it is not rediscovered as a bug: a
// SEATED task spelling `write(1, ..)` reaches row 1, where POSIX would reach
// whatever its parent seated. Nothing does -- love's stdio goes through the
// folded ports, and host/posix.c touches an implicit fd at three terminal-
// control calls and no data I/O at all. Closing it means per-task row tables
// (a real fd table), not an ambient g: `g` MOVES under collection, so the
// running task cannot be cached, only threaded.
extern long k_fd_write(int fd, void const *b, long n);
extern long k_fd_read(int fd, void *b, long n);
extern long k_fd_close(int fd);
extern long k_fd_lseek(int fd, long off, int whence);

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
extern uintptr_t ai_clock(void);

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
  char m = acc == O_RDONLY ? 'r'
         : acc != O_WRONLY ? 0
         : (fl & O_APPEND) ? 'a' : 'w';
  if (!m) return -EINVAL;                       // the ramfs has no O_RDWR door
  r = k_fs_open(p, strlen(p), m);
  if (r == -ENOENT && m == 'r') {               // the face keeps its 'r' misses cheap, so a
    struct k_st t;                              // synthesized directory is told apart here
    if (!k_fs_stat(p, strlen(p), &t) && (t.mode & 040000)) return -EISDIR; }
  return r; }

static long k_statat(long dfd, char const *p, struct stat *st, long fl) {
  (void) fl;                                    // no symlinks to not-follow
  long r = at_ok(dfd, p);
  if (r || !st) return r ? r : -EFAULT;
  struct k_st t;
  if ((r = k_fs_stat(p, strlen(p), &t))) return r;
  *st = (struct stat) {0};
  st->st_mode = (unsigned) t.mode;
  st->st_nlink = 1;
  st->st_size = (long) t.size;
  st->st_blksize = 4096;
  st->st_blocks = (long) ((t.size + 511) / 512);
  st->st_mtim.tv_sec = (long) (t.ms / 1000);
  st->st_mtim.tv_nsec = (long) (t.ms % 1000) * 1000000;
  st->st_atim = st->st_ctim = st->st_mtim;
  return 0; }

static long k_utimeat(long dfd, char const *p, struct timespec const *ts, long fl) {
  (void) fl;
  long r = at_ok(dfd, p);
  if (r) return r;
  uintptr_t ms;
  if (!ts || ts[1].tv_nsec == UTIME_NOW) ms = ai_clock();
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
    {"utimensat", NR_utimensat} };
  for (unsigned i = 0; i < sizeof t / sizeof *t; i++)
    if ((long) strlen(t[i].n) == n && !memcmp(t[i].n, nm, (unsigned long) n))
      return t[i].nr;
  return -1; }
#endif

long __ai_sys(long n, long a, long b, long c, long d, long e, long f) {
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
    default:       return -38; } }                       // ENOSYS, canonically
