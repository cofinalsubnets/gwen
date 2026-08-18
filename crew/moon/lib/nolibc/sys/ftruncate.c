#include "../impl.h"

int ftruncate(int fd, long n) {
  if (__ai_osv == 3) return (int) er(sc3(NR_ftruncate, fd, 0, n));   /* the classic PAD */
  return (int) er(sc2(NR_ftruncate, fd, n)); }
