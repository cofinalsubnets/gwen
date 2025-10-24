#include <stdio.h>
#include <stdlib.h>
#include "g.h"
#include <stdlib.h>
#include "pd_api.h"
#include "font.h"
extern PlaydateAPI *Pd;

void
  g_fb_clear(void),
  g_fb_putc(char),
  g_fb_puts(const char*),
  g_fb_set_cursor(int, int);

static g_vm_t g_buttons, g_cursor_h, g_cursor_v, theta, g_get_glyph, g_put_glyph, g_fps, g_clear;
#define NROWS 30
#define NCOLS 50
struct cb {
  uint32_t rows, cols, row, col;
  uint8_t cb[NROWS][NCOLS];
} BB = { NROWS, NCOLS, 0, 0, };
#define ROWS (BB.rows)
#define COLS (BB.cols)
#define Row (BB.row)
#define Col (BB.col)
static void random_life(struct cb*);

static bool show_cursor;
static const char boot[] =
#include "boot.h"
;

struct g *G = NULL;
PlaydateAPI *Pd;


static void draw_gb(struct cb*);

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
  draw_gb(&BB);
  return r; }

static int g_update(void *userdata) {
	Pd = userdata;
  G = g_pop(g_evals(G, "(update 0)"), 1);
	return 1; }

void g_dbg(struct g *f) {
  if (!g_ok(f) || !f) return Pd->system->logToConsole("f@%lx\n", f);
  intptr_t
    allocd = f->hp - f->sp,
    height = (intptr_t*) f + f->len - f->sp;
  Pd->system->logToConsole("f@%lx\n pool@%lx\n len=%ld\n allocd=%ld\n height=%ld\n",
                            f,   f->pool,   f->len,      allocd,      height); }

static Vm(fb_get) {
  size_t row = g_getnum(Sp[0]),
         col = g_getnum(Sp[1]);
  *++Sp = g_putnum(BB.cb[row % BB.rows][col % BB.cols]);
  Ip += 1;
  return Continue(); }

static Vm(fb_put) {
  size_t row = g_getnum(Sp[0]),
         col = g_getnum(Sp[1]);
  char b = g_getnum(Sp[2]);
  BB.cb[row % BB.rows][col % BB.cols] = b;
  Sp += 2;
  Ip += 1;
  return Continue(); }

static union x
  bif_cur_h[] = { {g_cursor_h}, {ret0}},
  bif_cur_v[] = { {g_cursor_v}, {ret0}},
  bif_theta[] = { {theta}, {ret0}},
  bif_get_glyph[] = {{g_get_glyph}, {ret0}},
  bif_put_glyph[] = {{g_put_glyph}, {ret0}},
  bif_clear[] = {{g_clear}, {ret0}},
  bif_buttons[] = {{g_buttons}, {ret0}},
  bif_fps[] = {{g_fps}, {ret0}},
  bif_fb_get[] = { {curry}, {.x = g_putnum(2)}, {fb_get}, {ret0}},
  bif_fb_put[] = { {curry}, {.x = g_putnum(3)}, {fb_put}, {ret0}};

static PDMenuItem *screen_menu_item;

static void enter_mode(int m);
static void g_screen_cb(void *ud) {
  int m = Pd->system->getMenuItemValue(screen_menu_item);
  Pd->system->logToConsole("mode=%d", m);
  enter_mode(m); }

void *gg_malloc(struct g *f, size_t n) {
  return Pd->system->realloc(NULL, n); }

void gg_free(struct g *f, void*x) {
  Pd->system->realloc(x, 0); }

static struct g_def defs[] = {
  {"cursor_h", bif_cur_h},
  {"cursor_v", bif_cur_v},
  {"get_angle", bif_theta},
  {"get_glyph", bif_get_glyph},
  {"put_glyph", bif_put_glyph},
  {"clear", bif_clear},
  {"fb_put", bif_fb_put},
  {"fb_get", bif_fb_get},
  {"get_fps", bif_fps},
  {"get_buttons", bif_buttons}, };

static struct g *g_pd_init(void) {
  struct g *f;
  f = g_ini_dynamic(gg_malloc, gg_free);
  g_dbg(f);
  f = g_defines(f, LEN(defs), defs);
  g_dbg(f);

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

// this works in the simulator but currently either crashes or times out on real systems
static void g_boot_cb(void *u) { G = g_pop(g_evals(G, boot), 1); }

static void cb_clear(struct cb *c) {
  uint32_t rows = c->rows, cols = c->cols;
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++)
      c->cb[i][j] = 0; }

void g_fb_clear(void) { cb_clear(&BB); }
static void g_reset_cb(void *id) {
  cb_clear(&BB);
  g_fin(G);
  G = g_pd_init();
  mode = 0; }

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
  kWaveformPOVosim, };
static int active_waveform = 0;
static float freq = 1000, freq_max = 20000, freq_min = 20, vol = 0.5;
static PDSynthSignal *signal;
static float constant_signal(void *u, int* i, float *f) { return *(float*)u; }

static void leave_mode(int m) {
  switch (m) {
    case synth_mode:
      Pd->sound->synth->freeSynth(synth);
    default: } }

static void random_life(struct cb*c) {
  uint32_t rows = c->rows, cols = c->cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      c->cb[i][j] = rand() & 1 ? live_char : dead_char; }

static void enter_mode(int m) {
  if (m != mode) {
    leave_mode(mode);
    switch (mode = m) {
      case draw_mode: show_cursor = true;
      default: cb_clear(&BB); break;
      case life_mode:
        show_cursor = false;
        random_life(&BB);
        break;
      case synth_mode:
        show_cursor = false;
        synth = Pd->sound->synth->newSynth();
        Pd->sound->synth->setWaveform(synth, waveforms[active_waveform]); } } }

void g_printf(struct g_out *o, const char*fmt, ...) {
}
void g_putc(struct g_out *o, int c) {
}
int g_getc(struct g_in *) { return 0; }
int g_ungetc(struct g_in*, int c) { return c; }
int g_eof(struct g_in*) { return 1; }

int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  Pd = pd;
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

void cb_putc(struct cb *cb, char c) {
  uintptr_t rows = cb->rows, cols = cb->cols;
  if (c == '\n') cb->row += 1, cb->row %= rows, cb->col = 0;
  else cb->cb[cb->row][cb->col] = c,
    cb->col += 1, cb->col %= cols,
    cb->row = cb->col ? cb->row : (cb->row + 1 % rows); }
void cb_puts(struct cb *cb, const char *s) { for (int i; (i = *s++); cb_putc(cb, i)); }
void g_fb_putc(char c) { cb_putc(&BB, c); }
void g_fb_puts(const char *s) { cb_puts(&BB, s); }

static void draw_gb(struct cb *c) {
  uint8_t *frame = Pd->graphics->getFrame();
  uint32_t rows = c->rows, cols = c->cols;
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++) {
      uint8_t *glyph = cga_8x8[c->cb[i][j]];
      for (int k = 0; k < 8; k++)
        frame[52 * (8 * i + k) + j] = show_cursor && i == c->row && j == c->col ? ~glyph[k] : glyph[k]; }
  Pd->graphics->markUpdatedRows(0, LCD_ROWS); }

static void shift_waveform(int n) {
  active_waveform += n;
  active_waveform %= LEN(waveforms);
  Pd->sound->synth->setWaveform(synth, waveforms[active_waveform]); }

static int life_update(void *userdata) {
  PDButtons current, pushed, released;
  Pd->system->getButtonState(&current, &pushed, &released);
  if (pushed & (kButtonA|kButtonB)) random_life(&BB);
  uint8_t db[NROWS][NCOLS];
  for (int i = 0; i < ROWS; i++)
    for (int j = 0; j < COLS; j++) {
      int i_1 = i-1<0?ROWS-1:i-1,
          i1  = i+1==ROWS?0:i+1,
          j_1 = j-1<0?COLS-1:j-1,
          j1  = j+1==COLS?0:j+1,
          n = (BB.cb[i_1][j_1] == live_char ? 1 : 0)
            + (BB.cb[i_1][j] == live_char ? 1 : 0)
            + (BB.cb[i_1][j1] == live_char ? 1 : 0)
            + (BB.cb[i][j_1] == live_char ? 1 : 0)
            + (BB.cb[i][j1] == live_char ? 1 : 0)
            + (BB.cb[i1][j_1] == live_char ? 1 : 0)
            + (BB.cb[i1][j] == live_char ? 1 : 0)
            + (BB.cb[i1][j1] == live_char ? 1 : 0);
      db[i][j] = n == 3 || (n == 2 && BB.cb[i][j] == live_char) ? live_char : dead_char; }
  memcpy(BB.cb, db, sizeof(db));
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

#include <time.h>
#include <unistd.h>
NoInline uintptr_t g_clock(void) {
  return Pd->system->getCurrentTimeMilliseconds(); }

Vm(g_fps) {
  float fps = Pd->display->getFPS();
  int i = (int) fps;
  Sp[0] = g_putnum(i);
  Ip += 1;
  return Continue(); }

static NoInline int gbs(void) {
  PDButtons a=0, b=0, c=0;
  Pd->system->getButtonState(&a, &b, &c);
  return a; }
Vm(g_buttons) { return
  Sp[0] = g_putnum(gbs()),
  Ip += 1,
  Continue(); }

Vm(g_cursor_h) {
  int n = g_getnum(Sp[0]);
  Col += n;
  while (Col >= COLS) Col -= COLS;
  while (Col < 0) Col += COLS;
  Ip += 1;
  return Continue(); }
Vm(g_cursor_v) {
  int n = g_getnum(Sp[0]);
  Row += n;
  while (Row >= ROWS) Row -= ROWS;
  while (Row < 0) Row += ROWS;
  Ip += 1;
  return Continue(); }

Vm(theta) {
  float t = Pd->system->getCrankChange();
  int delta = (int) (256 * t / 360.0f);
  Sp[0] = g_putnum(delta);
  Ip += 1;
  return Continue(); }

void cb_put_glyph(struct cb *cb, uint8_t c) { cb->cb[cb->row][cb->col] = c; }
uint8_t cb_get_glyph(struct cb *cb) { return cb->cb[cb->row][cb->col]; }
Vm(g_get_glyph) { return Sp[0] = g_putnum(cb_get_glyph(&BB)), Ip += 1, Continue(); }
Vm(g_put_glyph) { return cb_put_glyph(&BB, g_getnum(Sp[0])), Ip += 1, Continue(); }
Vm(g_clear) { return cb_clear(&BB), Ip += 1, Continue(); }
