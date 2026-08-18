#ifndef _AI_SIGNAL_H
#define _AI_SIGNAL_H
typedef int sig_atomic_t;
#include <sys/types.h>
typedef struct { long __v[16]; } sigset_t;   /* 128 bytes, glibc-sized */
struct sigaction {
  void (*sa_handler)(int);
  sigset_t sa_mask;
  int sa_flags;
  void (*sa_restorer)(void);
};
#define SIG_DFL ((void(*)(int))0)
#define SIG_IGN ((void(*)(int))1)
#define SIG_ERR ((void(*)(int))-1)
#define SIGINT   2
#define SIGILL   4
#define SIGABRT  6
#define SIGFPE   8
#define SIGSEGV 11
#define SIGBUS   7
/* sa_flags, Linux's values (the same on every arch we speak) */
#define SA_NOCLDSTOP 1
#define SA_NOCLDWAIT 2
#define SA_SIGINFO   4
#define SA_ONSTACK   0x08000000
#define SA_RESTART   0x10000000
#define SA_NODEFER 1073741824
#define SA_RESETHAND 0x80000000
void *signal(int, void*);        /* returns the old handler; love.c ignores it */
int raise(int);
int sigaction(int, struct sigaction const*, struct sigaction*);
int sigemptyset(sigset_t*);
/* the host seam's tail (rung 2) */
#define SIGHUP   1
#define SIGQUIT  3
#define SIGKILL  9
#define SIGPIPE 13
#define SIGALRM 14
#define SIGTERM 15
#define SIGTTIN 21
#define SIGTTOU 22
#define SIGWINCH 28
#define SIGUSR1 10
#define SIGUSR2 12
#define SIGCHLD 17
#define SIGCONT 18
#define SIGSTOP 19
#define SIGTSTP 20
#define SIG_BLOCK   0
#define SIG_UNBLOCK 1
#define SIG_SETMASK 2
int kill(pid_t, int);
int sigaddset(sigset_t*, int);
int sigismember(sigset_t const*, int);
int sigprocmask(int, sigset_t const*, sigset_t*);
#endif
