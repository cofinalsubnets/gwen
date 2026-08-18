#include "../impl.h"

int kill(pid_t pid, int sig) {
  if (__ai_osv >= 2) {
    long fs = __ai_sigfb(sig);
    if (fs < 0) { __errno_v = EINVAL; return -1; }
    return (int) er(sc2(NR_kill, pid, fs)); }
  return (int) er(sc2(NR_kill, pid, sig)); }
