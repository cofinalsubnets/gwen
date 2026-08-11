#include "../impl.h"

char *strcpy(char *d, char const *s) { char *r = d; while ((*d++ = *s++)) ; return r; }
