#include "../impl.h"

int clock_gettime(int ck, struct timespec *ts) {
  if (__ai_osv == 2) {
    /* the clockids part: REALTIME 0 agrees; MONOTONIC is 4 there (1 is
     * CLOCK_VIRTUAL), PROCESS_CPUTIME_ID 15, THREAD_CPUTIME_ID 14. */
    if (ck == 1) ck = 4;
    else if (ck == 2) ck = 15;
    else if (ck == 3) ck = 14; }
  return (int) er(sc2(NR_clock_gettime, ck, (long) ts)); }
