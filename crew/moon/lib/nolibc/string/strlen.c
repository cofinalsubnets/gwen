#include "../impl.h"

size_t strlen(char const *s) { size_t n = 0; while (*s++) n++; return n; }
