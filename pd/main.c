#include <stdio.h>
#include <stdlib.h>
#define g_target g_target_pd
#include "i.h"
#include "font.h"

static bool show_cursor;
static const char boot[] =
#include "boot.h"
;

g_core *G = NULL;
PlaydateAPI *Pd;

uint8_t gb[ROWS][COLS];
static void random_life(void);

static void draw_gb(void);

#define draw_mode 0
#define synth_mode 1
#define life_mode 2
#define shell_mode 3
#define live_char 0xb2
#define dead_char 0xb0

static int mode = draw_mode;
static int g_update(void*), synth_update(void*);
static int life_update(void *userdata);
static int update(void *userdata) {
  static int (*update_modes[])(void*) = {
    g_update,
    synth_update,
    life_update,
    g_update, };
  int r = update_modes[mode](userdata);
  draw_gb();
  return r; }

static int g_update(void *userdata) {
	Pd = userdata;
  G = g_evals_(G, "(update 0)");
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
  *++Sp = putnum(gb[row % ROWS][col % COLS]);
  Ip += 1;
  return Continue(); }

static Vm(fb_put) {
  size_t row = getnum(Sp[0]),
         col = getnum(Sp[1]);
  char b = getnum(Sp[2]);
  gb[row % ROWS][col % COLS] = b;
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

static void enter_mode(int m);
static void g_screen_cb(void *ud) {
  int m = Pd->system->getMenuItemValue(screen_menu_item);
  Pd->system->logToConsole("mode=%d", m);
  enter_mode(m); }

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
  f = g_ini_dynamic(gg_malloc, gg_free);
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
  G = g_pd_init();
  mode = 0;
}

PDSynth *synth;
PlaydateAPI *Pd;
static SoundWaveform waveforms[] = {
  kWaveformSquare,
  kWaveformTriangle,
  kWaveformSine,
  kWaveformNoise,
  kWaveformSawtooth,
  kWaveformPOPhase,
  kWaveformPODigital,
  kWaveformPOVosim,
};
static int active_waveform = 0;
static float freq = 1000, freq_max = 20000, freq_min = 20, vol = 0.5;
static PDSynthSignal *signal;
static float constant_signal(void *u, int* i, float *f) { return *(float*)u; }

static void leave_mode(int m) {
  switch (m) {
    case synth_mode:
      Pd->sound->synth->freeSynth(synth);
    default: } }

static void random_life(void) {
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++)
      gb[i][j] = rand() & 1 ? live_char : dead_char; }

static void enter_mode(int m) {
  if (m != mode) {
    leave_mode(mode);
    switch (mode = m) {
      case draw_mode: show_cursor = true;
      default: g_fb_clear(); break;
      case life_mode:
        show_cursor = false;
        random_life();
        break;
      case synth_mode:
        show_cursor = false;
        synth = Pd->sound->synth->newSynth();
        Pd->sound->synth->setWaveform(synth, waveforms[active_waveform]); } } }


int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  Pd = pd;
  g_core *f;
  switch (event) {
    case kEventInit:
      G = g_pd_init();
      enter_mode(mode);
      pd->system->setUpdateCallback(update, pd);
      static const char *options[] = { "draw", "synth", "life", "shell"};
      screen_menu_item = pd->system->addOptionsMenuItem("screen", options, LEN(options), g_screen_cb, NULL);
      pd->system->addMenuItem("reset", g_reset_cb, NULL);
      pd->system->addMenuItem("boot", g_boot_cb, NULL);
      break;
    default: }

	return 0; }

int Row = 0, Col = 0;


void g_fb_set_cursor(int row, int col) {
  Row = row % ROWS;
  Col = col % COLS; }

int g_fb_row(void) { return Row; }
int g_fb_col(void) { return Col; }

void g_fb_putc(char c) {
  if (c == '\n') Row += 1, Row %= ROWS, Col = 0;
  else gb[Row][Col] = c,
    Col += 1, Col %= COLS,
    Row = Col ? Row : (Row + 1 % ROWS); }

void g_fb_puts(const char *s) {
  for (int i; (i = *s++); g_fb_putc(i)); }

void g_fb_clear(void) {
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++)
      gb[i][j] = 0;
  g_fb_set_cursor(0, 0); }

static void draw_gb(void) {
  uint8_t *frame = Pd->graphics->getFrame();
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++) {
      uint8_t *glyph = cga_8x8[gb[i][j]];
      for (int k = 0; k < 8; k++)
        frame[52 * (8 * i + k) + j] = show_cursor && i == Row && j == Col ? ~glyph[k] : glyph[k]; }
  Pd->graphics->markUpdatedRows(0, LCD_ROWS); }

static void shift_waveform(int n) {
  active_waveform += n;
  active_waveform %= LEN(waveforms);
  Pd->sound->synth->setWaveform(synth, waveforms[active_waveform]); }

static int life_update(void *userdata) {
  PDButtons current, pushed, released;
  Pd->system->getButtonState(&current, &pushed, &released);
  if (pushed & (kButtonA|kButtonB)) random_life();
  uint8_t db[ROWS][COLS];
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++) {
      int i_1 = i-1<0?ROWS-1:i-1,
          i1  = i+1==ROWS?0:i+1,
          j_1 = j-1<0?COLS-1:j-1,
          j1  = j+1==COLS?0:j+1,
          n = (gb[i_1][j_1] == live_char ? 1 : 0)
            + (gb[i_1][j] == live_char ? 1 : 0)
            + (gb[i_1][j1] == live_char ? 1 : 0)
            + (gb[i][j_1] == live_char ? 1 : 0)
            + (gb[i][j1] == live_char ? 1 : 0)
            + (gb[i1][j_1] == live_char ? 1 : 0)
            + (gb[i1][j] == live_char ? 1 : 0)
            + (gb[i1][j1] == live_char ? 1 : 0);
      db[i][j] = n == 3 || (n == 2 && gb[i][j] == live_char) ? live_char : dead_char; }
  memcpy(gb, db, sizeof(db));
  return 1; }

static int synth_update(void* userdata) {
	PlaydateAPI* pd = userdata;
  pd->sound->synth->setVolume(synth, vol, vol);
  pd->sound->synth->playNote(synth, freq, 100, -1, 0);

  freq += freq * pd->system->getCrankChange() / 360.0f;

  PDButtons current, pushed, released;
  pd->system->getButtonState(&current, &pushed, &released);

  if (pushed & kButtonUp) vol += 0.1f;
  if (pushed & kButtonDown) vol -= 0.1f;
  vol = vol > 1 ? 1 : vol;
  vol = vol < 0 ? 0 : vol;


  if (pushed & kButtonLeft) shift_waveform(-1);
  if (pushed & kButtonRight) shift_waveform(1);

	return 1; }
