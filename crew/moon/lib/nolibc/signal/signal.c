#include "../impl.h"

void *signal(int sig, void *h) {
  struct sigaction sa, old;
  memset(&sa, 0, sizeof sa);
  sa.sa_handler = (void (*)(int)) h;
  sa.sa_flags = 268435456;                   /* SA_RESTART: glibc signal() semantics */
  if (sigaction(sig, &sa, &old)) return (void *) -1;
  return (void *) old.sa_handler; }
