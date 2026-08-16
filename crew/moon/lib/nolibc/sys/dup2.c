#include "../impl.h"
#if defined(__FreeBSD__)
int dup2(int a, int b) { return fcntl(a, F_DUP2FD, b); }   /* no dup3 syscall; F_DUP2FD is dup2 whole */
#else
int dup2(int a, int b) {
  if (a == b) return fcntl(a, F_GETFD, 0) < 0 ? -1 : b;   /* dup3 refuses a==b; dup2 answers b if a lives */
  return (int) er(sc3(NR_dup3, a, b, 0)); }
#endif
