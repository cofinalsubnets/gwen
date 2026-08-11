#include "../impl.h"

char *strpbrk(char const *s, char const *set) {
  for (; *s; s++) if (strchr(set, *s)) return (char *) s;
  return 0; }
