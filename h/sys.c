#include "i.h"
#include <time.h>
#include <string.h>
NoInline uintptr_t g_sys_clock(void) {
  struct timespec ts;
  int s = clock_gettime(CLOCK_REALTIME, &ts);
  return s ? -1 : ts.tv_sec  * 1000 + ts.tv_nsec / 1000000; }
int p_file_getc(struct g_input *i) {
  return getc(((file_input*) i)->file); }
int p_file_ungetc(struct g_input *i, int c) {
  return ungetc(c, ((file_input*) i)->file); }
int p_file_eof(struct g_input *i) {
  return feof(((file_input*) i)->file); }
NoInline struct g *g_read1f(struct g *f, g_file i) {
  file_input fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  return g_read1i(f, (input*) &fi); }
struct g *g_read1(struct g *f) { return g_read1f(f, g_stdin); }
