#include "../impl.h"

/* ---- byte order ---- */
unsigned short htons(unsigned short v) { return (unsigned short) ((v << 8) | (v >> 8)); }
