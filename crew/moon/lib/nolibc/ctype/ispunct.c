#include "../impl.h"

int ispunct(int c) { return isprint(c) && c != 32 && !isalnum(c); }
