#ifndef _AI_SYS_STAT_H
#define _AI_SYS_STAT_H
#include <time.h>       /* struct timespec */
#include <sys/types.h>  /* mode_t etc -- POSIX says stat.h provides them (busybox leans on it) */
#ifdef __aarch64__
/* the asm-generic kernel struct stat (aarch64): 128 bytes, st_mode before
 * st_nlink and both 32-bit -- what newfstatat fills verbatim */
struct stat {
  unsigned long st_dev;
  unsigned long st_ino;
  unsigned int  st_mode;
  unsigned int  st_nlink;
  unsigned int  st_uid;
  unsigned int  st_gid;
  unsigned long st_rdev;
  unsigned long __pad1;
  long          st_size;
  int           st_blksize;
  int           __pad2;
  long          st_blocks;
  struct timespec st_atim;
  struct timespec st_mtim;
  struct timespec st_ctim;
  unsigned int __unused4;
  unsigned int __unused5;
};
#else
/* glibc x86-64 struct stat: 144 bytes, st_nlink BEFORE st_mode (the classic) */
struct stat {
  unsigned long st_dev;
  unsigned long st_ino;
  unsigned long st_nlink;
  unsigned int  st_mode;
  unsigned int  st_uid;
  unsigned int  st_gid;
  int           __pad0;
  unsigned long st_rdev;
  long          st_size;
  long          st_blksize;
  long          st_blocks;
  struct timespec st_atim;
  struct timespec st_mtim;
  struct timespec st_ctim;
  long __reserved[3];
};
#endif
#define S_IFMT   61440
#define S_IFSOCK 49152
#define S_IFLNK  40960
#define S_IFREG  32768
#define S_IFBLK  24576
#define S_IFDIR  16384
#define S_IFCHR   8192
#define S_IFIFO   4096
#define S_ISREG(m)  (((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m)  (((m) & S_IFMT) == S_IFDIR)
#define S_ISLNK(m)  (((m) & S_IFMT) == S_IFLNK)
#define S_ISCHR(m)  (((m) & S_IFMT) == S_IFCHR)
#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#define UTIME_NOW  1073741823
#define UTIME_OMIT 1073741822
/* permission bits (decimal for the octal values, matching this file's style) */
#define S_ISUID 2048
#define S_ISGID 1024
#define S_ISVTX 512
#define S_IRWXU 448
#define S_IRUSR 256
#define S_IWUSR 128
#define S_IXUSR 64
#define S_IRWXG 56
#define S_IRGRP 32
#define S_IWGRP 16
#define S_IXGRP 8
#define S_IRWXO 7
#define S_IROTH 4
#define S_IWOTH 2
#define S_IXOTH 1
/* the common (non-POSIX) all-user combos gnulib/coreutils expect */
#define S_IRWXUGO (S_IRWXU | S_IRWXG | S_IRWXO)
#define S_IXUGO   (S_IXUSR | S_IXGRP | S_IXOTH)
/* the st_Xtime compatibility aliases glibc supplies (third-party C reads these) */
#define st_atime st_atim.tv_sec
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec
int stat(char const*, struct stat*);
int fstat(int, struct stat*);
int lstat(char const*, struct stat*);
int mkdir(char const*, unsigned int);
int chmod(char const*, unsigned int);
int fchmod(int, unsigned int);
unsigned int umask(unsigned int);
int utimensat(int, char const*, struct timespec const*, int);
int futimens(int, struct timespec const*);
int mkfifo(char const*, unsigned int);
int mknod(char const*, unsigned int, unsigned long);
#endif
