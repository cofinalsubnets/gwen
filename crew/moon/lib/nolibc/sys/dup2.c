#include "../impl.h"
int dup2(int a, int b) {
  if (__ai_osv == 2) return (int) er(fb3(NR_fb_fcntl, a, 10, b));   /* F_DUP2FD is dup2 whole */
  if (a == b) return fcntl(a, F_GETFD, 0) < 0 ? -1 : b;   /* dup3 refuses a==b; dup2 answers b if a lives */
  return (int) er(sc3(NR_dup3, a, b, 0)); }
