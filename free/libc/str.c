#include <stddef.h>

// The freestanding string floor, and by now it is one function: love.c's reader
// reads every integer base itself (ai_big_read_dec / _hex / _oct) and takes its
// floats straight from am_strtod, so the strtol/strtod/ctype half this file used
// to carry has no callers left in the kernel's link set. What remains is what a
// compiler SYNTHESIZES -- strlen here, the mem* next door -- which is why these
// cannot go the same way however few calls the source shows.
size_t strlen(char const *c) {
  size_t len = 0;
  while (*c++) len++;
  return len; }
