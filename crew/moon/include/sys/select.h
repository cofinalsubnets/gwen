#ifndef _AI_SYS_SELECT_H
#define _AI_SYS_SELECT_H
#include <sys/types.h>
#include <sys/time.h>
/* glibc's layout exactly: 1024 bits as 16 longs, so a set built here is the one
 * the kernel reads and a mooncc object interoperates with a glibc-built one. */
#define FD_SETSIZE 1024
#define __NFDBITS (8 * (int) sizeof(unsigned long))
typedef struct { unsigned long fds_bits[FD_SETSIZE / (8 * sizeof(unsigned long))]; } fd_set;
#define __FDELT(d)  ((d) / __NFDBITS)
#define __FDMASK(d) (1UL << ((d) % __NFDBITS))
#define FD_ZERO(s) do { unsigned long *__b = (s)->fds_bits; int __i; \
  for (__i = 0; __i < FD_SETSIZE / __NFDBITS; __i++) __b[__i] = 0; } while (0)
#define FD_SET(d, s)   ((s)->fds_bits[__FDELT(d)] |= __FDMASK(d))
#define FD_CLR(d, s)   ((s)->fds_bits[__FDELT(d)] &= ~__FDMASK(d))
#define FD_ISSET(d, s) (((s)->fds_bits[__FDELT(d)] & __FDMASK(d)) != 0)
int select(int, fd_set*, fd_set*, fd_set*, struct timeval*);
#endif
