#ifndef _AI_FCNTL_H
#define _AI_FCNTL_H
/* Linux x86-64 values (octal in the kernel; spelled decimal here); freebsd's
 * fork on the OS first -- one table, every arch (stable/14 sys/fcntl.h) */
#define O_RDONLY         0
#define O_WRONLY         1
#define O_RDWR           2
#if defined(__FreeBSD__)
#define O_CREAT        512
#define O_EXCL        2048
#define O_NOCTTY     32768
#define O_TRUNC       1024
#define O_APPEND         8
#define O_NONBLOCK       4
#define O_DIRECTORY 131072
#define O_NOFOLLOW     256
#define O_CLOEXEC  1048576
#else
#define O_CREAT         64
#define O_EXCL         128
#define O_NOCTTY       256
#define O_TRUNC        512
#define O_APPEND      1024
#define O_NONBLOCK    2048
/* ⚠ THE ARM FAMILY IS THE EXCEPTION HERE, and riscv64 is NOT in it. arm64 kept
 * 32-bit ARM's values for these two; riscv64 takes the genuine asm-generic
 * ones, which are x86-64's. the first cut of this gated on
 * `__aarch64__ || __riscv` and called 040000 "asm-generic" -- it is arm's. on
 * riscv64 that made O_DIRECTORY mean O_DIRECT and O_NOFOLLOW mean O_LARGEFILE:
 * opendir answered EINVAL (direct I/O on a directory), and the symlink guard
 * was silently absent, which is the quieter half. arm64 masked it by being
 * right, so it took a riscv64 package build to surface. */
#if defined(__aarch64__) || defined(__arm__)
#define O_DIRECTORY  16384   /* arm/arm64 040000 */
#define O_NOFOLLOW   32768   /* arm/arm64 0100000 */
#else
#define O_DIRECTORY  65536   /* x86-64, riscv64, asm-generic 0200000 */
#define O_NOFOLLOW  131072   /* x86-64, riscv64, asm-generic 0400000 */
#endif
#define O_CLOEXEC   524288
#endif
#define O_SEARCH   O_RDONLY   /* Linux has no O_SEARCH; gnulib's own fallback (fcntl.in.h) */
#define O_BINARY         0    /* no text/binary distinction on Linux (a DOS-ism; 0 = no-op) */
#define O_TEXT           0
#define AT_FDCWD      (-100)   /* both kernels' spelling */
#if defined(__FreeBSD__)
#define AT_SYMLINK_NOFOLLOW 512
#define AT_REMOVEDIR       2048
#else
#define AT_SYMLINK_NOFOLLOW 256
#define AT_REMOVEDIR        512
#endif
#define F_DUPFD          0
#define F_GETFD          1
#define F_SETFD          2
#define F_GETFL          3
#define F_SETFL          4
#define FD_CLOEXEC       1
#if defined(__FreeBSD__)
#define F_DUPFD_CLOEXEC 17
#define F_DUP2FD        10
#define F_DUP2FD_CLOEXEC 18
/* POSIX record locks, freebsd's numbers and freebsd's field order */
#define F_GETLK         11
#define F_SETLK         12
#define F_SETLKW        13
#define F_RDLCK          1
#define F_WRLCK          3
#define F_UNLCK          2
struct flock {
  long  l_start;
  long  l_len;
  int   l_pid;
  short l_type;
  short l_whence;
  int   l_sysid;
};
#else
#define F_DUPFD_CLOEXEC 1030   /* F_LINUX_SPECIFIC_BASE (1024) + 6 */
/* POSIX record locks (sqlite's whole locking story rides these) */
#define F_GETLK          5
#define F_SETLK          6
#define F_SETLKW         7
#define F_RDLCK          0
#define F_WRLCK          1
#define F_UNLCK          2
struct flock {
  short l_type;
  short l_whence;
  long  l_start;
  long  l_len;
  int   l_pid;
};
#endif
int open(char const*, int, ...);
int openat(int, char const*, int, ...);
int creat(char const*, unsigned int);
int fcntl(int, int, ...);
#endif
