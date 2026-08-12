#include "../impl.h"

static unsigned long __rnd = 1;
static unsigned long __rstep(void) {
  return __rnd = __rnd * 6364136223846793005UL + 1442695040888963407UL; }
int rand(void) { return (int) ((__rstep() >> 49) & 32767); }
void srand(unsigned int s) { __rnd = s; }
long random(void) { return (long) ((__rstep() >> 33) & 2147483647); }
void srandom(unsigned int s) { __rnd = s; }
