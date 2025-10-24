#ifndef _g_pd_sys_h
#define _g_pd_sys_h
#include "pd_api.h"
#include <stdlib.h>
#include "font.h"
typedef int g_file;
#define g_stdin  0
#define g_stdout 1
#define g_stderr 2
#define g_fprintf(_, ...) Pd->system->logToConsole(__VA_ARGS__)
#define g_fputc(c, _) g_fb_putc(c)
extern PlaydateAPI *Pd;

#define ROWS 30
#define COLS 50

extern uint8_t gb[ROWS][COLS];
extern int Row, Col;
void
  g_fb_clear(void),
  g_fb_putc(char),
  g_fb_puts(const char*),
  g_fb_set_cursor(int, int);

int g_fb_row(void), g_fb_col(void);

g_vm_t g_buttons,
       g_cursor_h,
       g_cursor_v,
       theta,
       g_get_glyph,
       g_put_glyph,
       g_fps,
       g_clear;
#endif
