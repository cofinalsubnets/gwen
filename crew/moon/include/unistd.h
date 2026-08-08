#ifndef _AI_UNISTD_H
#define _AI_UNISTD_H
typedef long ssize_t;
#include <sys/types.h>
long read(int, void*, long);
long write(int, void const*, long);
int close(int);
long lseek(int, long, int);
int open(char const*, int, ...);   /* fcntl.h's shape, repeated for the lone-include habit */
long sysconf(int);
const char *getprogname(void);   /* mirrors stdlib.h; gnulib's progname reaches either */
#define _SC_PAGESIZE 30
#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2
/* the POSIX tail the host seam rides (rung 2) */
int  unlink(char const*);
int  rmdir(char const*);
int  link(char const*, char const*);
int  symlink(char const*, char const*);
long readlink(char const*, char*, unsigned long);
int  chown(char const*, unsigned int, unsigned int);
int  fchown(int, unsigned int, unsigned int);
int  lchown(char const*, unsigned int, unsigned int);
int  setuid(unsigned int);
int  setgid(unsigned int);
int  seteuid(unsigned int);
int  setegid(unsigned int);
int  chdir(char const*);
int  chroot(char const*);
char *getcwd(char*, unsigned long);
int  access(char const*, int);
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4
int  dup(int);
int  dup2(int, int);
int  pipe(int*);
int  fork(void);
int  execvp(char const*, char *const*);
int  execv(char const*, char *const*);
int  execve(char const*, char *const*, char *const*);
int  execl(char const*, char const*, ...);
int  execlp(char const*, char const*, ...);
int  execle(char const*, char const*, ...);
void _exit(int);
unsigned int getuid(void);
unsigned int geteuid(void);
unsigned int getgid(void);
int  getpid(void);
int  setsid(void);
int  isatty(int);
int  ftruncate(int, long);
int  truncate(char const*, long);
long pathconf(char const*, int);
long fpathconf(int, int);
int  fsync(int);
int  fdatasync(int);
int  unlinkat(int, char const*, int);
unsigned int sleep(unsigned int);
int  usleep(unsigned int);
long pread(int, void*, unsigned long, long);
long pwrite(int, void const*, unsigned long, long);
int  getpgrp(void);
int  setpgid(pid_t, pid_t);
int  tcsetpgrp(int, pid_t);
#endif
