#include <stdio.h>
#include <stdlib.h>
#define g_target g_target_pd
#include "i.h"
#include "font.h"

static const char boot[] =
#include "../src/boot.h"
;

g_core *G = NULL;
PlaydateAPI *Pd;

uint8_t glyph_buffer[ROWS][COLS];

static void draw_glyph_buffer(void);

static int g_update(void *userdata) {
	Pd = userdata;
  G = g_evals_(G, "(update 0)");
  draw_glyph_buffer();
	return 1; }

void g_dbg(g_core*f) {
  if (!g_ok(f) || !f)
    Pd->system->logToConsole("f@%lx\n", f);
  else Pd->system->logToConsole(
      "f@%lx\n"
      " pool@%lx\n"
      " len=%ld\n"
      " allocd=%ld\n"
      " height=%ld\n"
      ,
      f,
      f->pool,
      f->len,
      f->hp - f->end,
      (g_word*) f + f->len - f->sp
      ); }

static Vm(fb_get) {
  size_t row = getnum(Sp[0]),
         col = getnum(Sp[1]);
  *++Sp = putnum(glyph_buffer[row % ROWS][col % COLS]);
  Ip += 1;
  return Continue(); }

static Vm(fb_put) {
  size_t row = getnum(Sp[0]),
         col = getnum(Sp[1]);
  char b = getnum(Sp[2]);
  glyph_buffer[row % ROWS][col % COLS] = b;
  Sp += 2;
  Ip += 1;
  return Continue(); }

static const union g_cell
  bif_cur_h[] = { {g_cursor_h}, {ret0}},
  bif_cur_v[] = { {g_cursor_v}, {ret0}},
  bif_theta[] = { {theta}, {ret0}},
  bif_get_glyph[] = {{g_get_glyph}, {ret0}},
  bif_put_glyph[] = {{g_put_glyph}, {ret0}},
  bif_clear[] = {{g_clear}, {ret0}},
  bif_buttons[] = {{g_buttons}, {ret0}},
  bif_fps[] = {{g_fps}, {ret0}},
  bif_fb_get[] = { {curry}, {.x = putnum(2)}, {fb_get}, {ret0}},
  bif_fb_put[] = { {curry}, {.x = putnum(3)}, {fb_put}, {ret0}};

static PDMenuItem *screen_menu_item;
#define g_screen_draw 0

static void g_screen_cb(void *ud) {
  int screen = Pd->system->getMenuItemValue(screen_menu_item);
  Pd->system->logToConsole("screen=%d", screen); }

void *gg_malloc(g_core*f, size_t n) {
  return Pd->system->realloc(NULL, n); }

void gg_free(g_core*f, void*x) {
  Pd->system->realloc(x, 0); }

static struct {
  const char *n;
  const g_cell *v;
} defs[] = {
  {"cursor_h", bif_cur_h},
  {"cursor_v", bif_cur_v},
  {"get_angle", bif_theta},
  {"get_glyph", bif_get_glyph},
  {"put_glyph", bif_put_glyph},
  {"clear", bif_clear},
  {"fb_put", bif_fb_put},
  {"fb_get", bif_fb_get},
  {"get_fps", bif_fps},
  {"get_buttons", bif_buttons},
};

static g_core *g_pd_init(void) {
  g_core *f;
  f = g_ini_m(gg_malloc, gg_free);
  g_dbg(f);
#define LEN(x) (sizeof(x)/sizeof(*x))
  for (int i = 0; i < LEN(defs); i++)
    f = g_define(g_push(f, 1, defs[i].v), defs[i].n);
  g_dbg(f);

//  f = g_evals(f, boot);

  const char prog[] = "(,"
   "(fb_put 1 1 99)"
   "(fb_put 2 2 99)"
   "(fb_put 3 3 99)"
   "(fb_put 4 4 99)"
   "(: (puts s) (: (css m n) (? (< m n) (, (putc (sget s m)) (css (+ 1 m) n))) (css 0 (slen s))))"
   "(. (clock 0)))"
   "(:(event ev arg)(,(.'got_event)(. ev)(. arg)))"
   "(: (update _) (: fps (get_fps 0) (,"
   "(fb_put 29 49 (+ 48(% fps 10)))"
   "(fb_put 29 48 (+ 48(% (/ fps 10) 10)))"
   "(: bs (get_buttons 0)" 
   "(, (? (& 1 bs) (cursor_h -1))"
      "(? (& 2 bs) (cursor_h 1))"
      "(? (& 4 bs) (cursor_v -1))"
      "(? (& 8 bs) (cursor_v 1))"
      "(? (& 32 bs) (clear 0))"
      "(put_glyph (+ (? (& bs 16) 1) (+ (get_angle 0) (get_glyph 0))))"
   ")"
   "))))"
   ")";
  f = g_evals(f, prog);
  g_dbg(f);
  return f; }

static void g_boot_cb(void *u) {
  G = g_evals_(G, boot); }

static void g_reset_cb(void *id) {
  g_fb_clear();
  g_fin(G);
  G = g_pd_init(); }


int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  Pd = pd;
  g_core *f;
  switch (event) {
    case kEventInit:
      g_reset_cb(NULL);
      pd->system->setUpdateCallback(g_update, pd);
      g_fb_set_cursor(6, 0);
      g_fb_puts("fb write");
      static const char *options[] = { "draw", "life", "shell"};
      screen_menu_item = pd->system->addOptionsMenuItem("screen", options, 3, g_screen_cb, NULL);
      pd->system->addMenuItem("reset", g_reset_cb, NULL);
      pd->system->addMenuItem("boot", g_boot_cb, NULL);
      break;
    default:
      /*
      f = G;
      f = g_evals(f, "event");
      f = g_push(f, 3, nil, putnum(arg), putnum(event));
      f = g_cons_r(f);
      f = g_cons_r(f);
      f = g_cons_r(f);
      f = g_cons_r(f);
      f = g_eval_(f);
      */
      break; }

	return 0; }

int Row = 0, Col = 0;


void g_fb_set_cursor(int row, int col) {
  Row = row % ROWS;
  Col = col % COLS; }

int g_fb_row(void) { return Row; }
int g_fb_col(void) { return Col; }

void g_fb_putc(char c) {
  if (c == '\n') Row += 1, Row %= ROWS, Col = 0;
  else glyph_buffer[Row][Col] = c,
    Col += 1, Col %= COLS,
    Row = Col ? Row : (Row + 1 % ROWS); }

void g_fb_puts(const char *s) {
  for (int i; (i = *s++); g_fb_putc(i)); }

void g_fb_clear(void) {
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++)
      glyph_buffer[i][j] = 0;
  g_fb_set_cursor(0, 0); }

static void draw_glyph_buffer(void) {
  uint8_t *frame = Pd->graphics->getFrame();
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++) {
      uint8_t *glyph = cga_8x8[glyph_buffer[i][j]];
      for (int k = 0; k < 8; k++)
        frame[52 * (8 * i + k) + j] = i == Row && j == Col ? ~glyph[k] : glyph[k]; }
  Pd->graphics->markUpdatedRows(0, LCD_ROWS); }
