#ifndef _AI_SYS_TIME_H
#define _AI_SYS_TIME_H
#include <time.h>   /* struct timespec, time_t */
struct timeval { long tv_sec; long tv_usec; };
int gettimeofday(struct timeval*, void*);
int utimes(char const*, struct timeval const*);
#endif
