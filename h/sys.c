#include "g.h"
#include <time.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>
// noinline because it leaks a stack address
g_noinline uintptr_t g_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }
void g_printf(struct g_out*, const char *fmt, ...) {
  va_list xs;
  va_start(xs, fmt);
  vfprintf(stdout, fmt, xs);
  va_end(xs); }
void g_putc(struct g_out*, int c) { putchar(c); }
int g_getc(struct g_in*) { return getc(stdin); }
int g_ungetc(struct g_in*, int c) { return ungetc(c, stdin); }
int g_eof(struct g_in*) { return feof(stdin); }
