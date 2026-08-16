#include "../impl.h"
/* absent on freebsd until rung 3 supplies the body -- an empty member defines
   nothing, so a consumer reads "undefined reference", the honest sentence */
#if !defined(__FreeBSD__)
int dup2(int a, int b) {
  if (a == b) return fcntl(a, F_GETFD, 0) < 0 ? -1 : b;   /* dup3 refuses a==b; dup2 answers b if a lives */
  return (int) er(sc3(NR_dup3, a, b, 0)); }
#endif
