#ifndef _AI_TIME_H
#define _AI_TIME_H
typedef long time_t;
typedef long clock_t;
#define CLOCKS_PER_SEC 1000000
struct timespec { long tv_sec; long tv_nsec; };
/* glibc layout -- a hosted (cc-built) object links libc's localtime, so the
   field order + the two GNU extensions (tm_gmtoff/tm_zone) must match. */
struct tm { int tm_sec; int tm_min; int tm_hour; int tm_mday; int tm_mon;
            int tm_year; int tm_wday; int tm_yday; int tm_isdst;
            long tm_gmtoff; char const* tm_zone; };
/* tzset + its three globals: no timezone database here, so the zone is always
 * UTC and tzset is the no-op that says so. declared because a program calls it
 * before localtime and reads tzname for a log line. */
void tzset(void);
extern char *tzname[2];
extern long timezone;
extern int daylight;
struct tm* localtime(time_t const*);
struct tm* gmtime(time_t const*);
struct tm* localtime_r(time_t const*, struct tm*);
struct tm* gmtime_r(time_t const*, struct tm*);
time_t     mktime(struct tm*);
char*      ctime(time_t const*);
char*      asctime(struct tm const*);
double     difftime(time_t, time_t);
unsigned long strftime(char*, unsigned long, char const*, struct tm const*);
#define CLOCK_REALTIME  0
#define CLOCK_MONOTONIC 1
int clock_gettime(int, struct timespec*);
int nanosleep(struct timespec const*, struct timespec*);
time_t time(time_t*);
clock_t clock(void);
#endif
