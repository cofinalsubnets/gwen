#include <stdio.h>
#include <sys/time.h>
#include "g.h"

g_noinline uintptr_t g_clock(void) {
  struct timeval tp;
  gettimeofday(&tp, NULL);
  return 1ul * tp.tv_sec + tp.tv_usec; }
int gputc(struct g*, int c) { return putc(c, stdout); }
int ggetc(struct g*) { return getc(stdin); }
int gungetc(struct g*, int c) { return ungetc(c, stdin); }
int geof(struct g*) { return feof(stdin); }
int gflush(struct g*f) { return fflush(stdout); }

void app_main(void)
{
  struct g *f = g_ini();
  putc(g_code_of(f), stdout);
}
