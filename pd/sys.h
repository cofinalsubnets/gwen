#include "pd_api.h"

extern PlaydateAPI *Pd;

#define ROWS 30
#define COLS 50

extern uint8_t glyph_buffer[ROWS][COLS];
extern int Row, Col;
void
  g_fb_clear(void),
  g_fb_putc(char),
  g_fb_puts(const char*),
  g_fb_set_cursor(int, int);

int g_fb_row(void), g_fb_col(void);
