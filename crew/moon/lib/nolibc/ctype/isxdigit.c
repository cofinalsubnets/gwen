#include "../impl.h"

int isxdigit(int c) { return isdigit(c) || (c >= 65 && c <= 70) || (c >= 97 && c <= 102); }
