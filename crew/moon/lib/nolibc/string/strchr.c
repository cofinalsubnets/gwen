#include "../impl.h"

char *strchr(char const *s, int c) { for (;; s++) { if (*s == (char) c) return (char *) s; if (!*s) return 0; } }
