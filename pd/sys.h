#ifndef _g_pd_sys_h
#define _g_pd_sys_h
#include "pd_api.h"
#include <stdlib.h>
#include "font.h"
typedef int g_file;
extern PlaydateAPI *Pd;

void
  g_fb_clear(void),
  g_fb_putc(char),
  g_fb_puts(const char*),
  g_fb_set_cursor(int, int);

#define g_stdin  0
#define g_stdout 1
#define g_stderr 2
#define g_fprintf(_, ...) Pd->system->logToConsole(__VA_ARGS__)
#define g_fputc(c, _) g_fb_putc(c)
#endif
