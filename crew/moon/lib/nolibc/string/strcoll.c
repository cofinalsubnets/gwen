#include "../impl.h"

int strcoll(char const *a, char const *b) { return strcmp(a, b); }   /* the "C" locale IS strcmp */
