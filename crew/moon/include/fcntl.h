#ifndef _AI_FCNTL_H
#define _AI_FCNTL_H
/* Linux x86-64 values (octal in the kernel; spelled decimal here) */
#define O_RDONLY         0
#define O_WRONLY         1
#define O_RDWR           2
#define O_CREAT         64
#define O_EXCL         128
#define O_NOCTTY       256
#define O_TRUNC        512
#define O_APPEND      1024
#define O_NONBLOCK    2048
#if defined(__aarch64__) || defined(__riscv)
#define O_DIRECTORY  16384   /* asm-generic 040000 */
#define O_NOFOLLOW   32768   /* asm-generic 0100000 */
#else
#define O_DIRECTORY  65536   /* x86 0200000 */
#define O_NOFOLLOW  131072   /* x86 0400000 */
#endif
#define O_CLOEXEC   524288
#define O_SEARCH   O_RDONLY   /* Linux has no O_SEARCH; gnulib's own fallback (fcntl.in.h) */
#define O_BINARY         0    /* no text/binary distinction on Linux (a DOS-ism; 0 = no-op) */
#define O_TEXT           0
#define AT_FDCWD      (-100)
#define F_DUPFD          0
#define F_GETFD          1
#define F_SETFD          2
#define F_GETFL          3
#define F_SETFL          4
#define F_DUPFD_CLOEXEC 1030   /* F_LINUX_SPECIFIC_BASE (1024) + 6 */
#define FD_CLOEXEC       1
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
int open(char const*, int, ...);
int openat(int, char const*, int, ...);
int creat(char const*, unsigned int);
int fcntl(int, int, ...);
#endif
