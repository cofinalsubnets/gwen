#include "../impl.h"

/* ---- execvp: execve + the PATH walk ---- */
int execv(char const *p, char *const *av) {
  return (int) er(sc3(NR_execve, (long) p, (long) av, (long) environ)); }
