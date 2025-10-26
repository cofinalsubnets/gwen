#include <stdio.h>
#include <stdlib.h>
#include "g.h"
#include <stdlib.h>
#include "pd_api.h"
#include "font.h"
#define Sp f->sp
#define Hp f->hp
#define Ip f->ip
#define NROWS 30
#define NCOLS 50

static const SoundWaveform waveforms[] = {
  kWaveformSquare, kWaveformTriangle, kWaveformSine, kWaveformNoise,
  kWaveformSawtooth, kWaveformPOPhase, kWaveformPODigital, kWaveformPOVosim, };

static int g_update(void*), g_synth_update(void*), life_update(void*);
static void g_synth_ini(void), draw_gb(void), g_draw_ini(void), g_synth_ini(void), g_life_ini(void);
static void g_nop(void) {}

enum g_mode {
  g_mode_synth,
  g_mode_life,
  g_mode_draw,
  N_modes };

static const struct {
  void (*ini)(void);
  int (*update)(void*);
  void (*fin)(void);
} modes[N_modes] = {
  { g_synth_ini, g_synth_update, g_nop },
  { g_life_ini, life_update, g_nop },
  { g_draw_ini, g_update, g_nop }, };


#define g_default_mode g_mode_synth
static uint8_t __cb[NROWS][NCOLS];
static struct K {
  struct cb {
    uint32_t rows, cols, row, col;
    uint8_t *cb;
    bool show_cursor; } c;
  PlaydateAPI *pd;
  struct g *g;
  PDSynth *synth;
  PDMenuItem *mode_menu_item;
  enum g_mode mode;
  int active_waveform, synth_mode;
  float synth_time, freq, freq_max, freq_min, vol;
  PDButtons current, pushed, released;
} K = { {NROWS, NCOLS, 0, 0, (uint8_t*) __cb[0]} };

static void cb_line_feed(struct cb *c) {
  c->col = 0;
  c->row++;
  if (c->row == c->rows) c->row = 0; }

static void cb_put_char(struct cb *c, char i) {
  c->cb[c->row * c->cols + c->col] = i;
  c->col++;
  if (c->col == c->cols) cb_line_feed(c); }

static void put_char(char i) {
  return cb_put_char(&K.c, i); }

void cb_log_n(struct cb *c, uintptr_t n, uintptr_t base) {
  static const char digits[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  uintptr_t d = n % base;
  if (n) cb_log_n(c, n / base, base);
  cb_put_char(c, digits[d]); }

void cb_vlogf(struct cb *cb, const char *fmt, va_list xs) {
  while (*fmt) {
    char c = *fmt++;
    if (c != '%') cb_put_char(cb, c);
    else re:
      switch (c = *fmt++) {
      case 0: return;
      default: cb_put_char(cb, c); break;
      case 'l': goto re;
      case 'd': cb_log_n(cb, va_arg(xs, uintptr_t), 10); break;
      case 'x': cb_log_n(cb, va_arg(xs, uintptr_t), 16); break;
      case 'o': cb_log_n(cb, va_arg(xs, uintptr_t), 8); break;
      case 'b': cb_log_n(cb, va_arg(xs, uintptr_t), 2); break; } } }

void k_logf(const char *fmt, ...) {
  va_list xs;
  va_start(xs, fmt);
  cb_vlogf(&K.c, fmt, xs);
  va_end(xs); }

static void cb_fill(struct cb*, uint8_t);

static void get_buttons(void) {
  K.pd->system->getButtonState(&K.current, &K.pushed, &K.released); }

void
  g_fb_set_cursor(int, int);

static g_vm_t g_buttons, g_cursor_h, g_cursor_v, theta, g_get_glyph, g_put_glyph, g_fps, g_clear;
static void random_life(void);

static const char boot[] =
#include "boot.h"
;


#define live_char 0xb2
#define dead_char 0xb0
static void g_draw_ini(void) {
  K.c.show_cursor = true;
  K.c.row = K.c.col = 0;
  cb_fill(&K.c, 0); }

static void draw_char_buffer(struct cb *c, uint8_t font[][8], uint8_t *frame) {
  uint32_t rows = c->rows, cols = c->cols;
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++)
      for (uint8_t k = 0, *glyph = font[c->cb[i * cols + j]], g; k < 8; k++)
        frame[52 * (8 * i + k) + j] = c->show_cursor && i == c->row && j == c->col ? ~glyph[k] : glyph[k]; }

static int update(void *u) {
  // run the update function for the current mode
  int r = modes[K.mode].update(u);
  draw_char_buffer(&K.c, cga_8x8, K.pd->graphics->getFrame());
  K.pd->graphics->markUpdatedRows(0, LCD_ROWS);
  return r; }


static int g_update(void *userdata) {
  K.g = g_pop(g_evals(K.g, "(update 0)"), 1);
	return 1; }

void g_dbg(struct g *f) {
  if (!g_ok(f) || !f) return K.pd->system->logToConsole("f@%lx\n", f);
  intptr_t
    allocd = f->hp - f->sp,
    height = (intptr_t*) f + f->len - f->sp;
  K.pd->system->logToConsole("f@%lx\n pool@%lx\n len=%ld\n allocd=%ld\n height=%ld\n",
                            f,   f->pool,   f->len,      allocd,      height); }

static g_vm(fb_get) {
  size_t row = g_getnum(Sp[0]),
         col = g_getnum(Sp[1]);
  *++Sp = g_putnum(K.c.cb[(row % K.c.rows) * K.c.cols + (col % K.c.cols)]);
  Ip += 1;
  return Continue(); }

static g_vm(fb_put) {
  size_t row = g_getnum(Sp[0]),
         col = g_getnum(Sp[1]);
  char b = g_getnum(Sp[2]);
  K.c.cb[(row % K.c.rows) * K.c.cols + (col % K.c.cols)] = b;
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


static void *gg_malloc(struct g *f, size_t n) { return
  K.pd->system->realloc(NULL, n); }
static void gg_free(struct g *f, void*x) {
  K.pd->system->realloc(x, 0); }

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

const char g_init_prog[] = "(,"
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

static void cb_fill(struct cb *c, uint8_t _) {
  uint32_t rows = c->rows, cols = c->cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      c->cb[i * cols + j] = _; }

static void g_set_mode(void *ud) {
  int m = K.pd->system->getMenuItemValue(K.mode_menu_item);
  if (m != K.mode) modes[K.mode].fin(), modes[K.mode = m].ini(); }
static void reset(PlaydateAPI *pd);
static void g_reset_cb(void*_) { reset(K.pd); }
static void g_boot_cb(void *u) { K.g = g_pop(g_evals(K.g, boot), 1); }
static void reset(PlaydateAPI *pd) {
  K.pd = pd;
  modes[K.mode].fin();
  pd->sound->synth->freeSynth(K.synth);
  K.synth = pd->sound->synth->newSynth();
  pd->system->removeAllMenuItems();
  static const char *options[] = { [g_mode_synth] = "timer", [g_mode_life] = "life", [g_mode_draw] = "draw" };
  K.mode_menu_item = pd->system->addOptionsMenuItem("mode", options, LEN(options), g_set_mode, NULL);
  pd->system->addMenuItem("reset", g_reset_cb, NULL);
  pd->system->addMenuItem("boot", g_boot_cb, NULL);
  K.c.row = K.c.col = 0;
  cb_fill(&K.c, 0);
  g_fin(K.g);
  struct g *f;
  f = g_ini_dynamic(gg_malloc, gg_free);
  f = g_defns(f, LEN(defs), defs);
  f = g_evals(f, g_init_prog);
  K.g = f;
  modes[K.mode = 0].ini();
  pd->system->setUpdateCallback(update, pd); }

static void cb_cur(struct cb *c, uint32_t row, uint32_t col) {
  c->row = row % c->rows, c->col = col % c->cols; }
static void cb_mv_cur(struct cb *c, uint32_t dr, uint32_t dc) {
  cb_cur(c, c->row + dr, c->col + dc); }

static void k_clear(void) {
  K.c.row = K.c.col = 0;
  cb_fill(&K.c, 0); }


static void random_life(void) {
  uint32_t rows = K.c.rows, cols = K.c.cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      K.c.cb[i* cols + j] = rand() & 1 ? live_char : dead_char; }

static void g_life_ini(void) {
  K.c.show_cursor = false;
  random_life(); }

// g_life_fin = g_nop

static void g_synth_ini(void) {
  K.c.show_cursor = false;
  K.active_waveform = 0;
  K.freq = 1000;
  K.freq_max = 20000;
  K.freq_min = 20;
  K.vol = 0.5;
  K.synth_mode = 1;
  K.synth_time = 1;
  K.pd->sound->synth->setWaveform(K.synth, waveforms[K.active_waveform]);
  cb_cur(&K.c, 0, 0);
  cb_fill(&K.c, dead_char); }
void g_printf(struct g_out *o, const char*fmt, ...) { }
void g_putc(struct g_out *o, int c) { }
int g_getc(struct g_in *) { return 0; }
int g_ungetc(struct g_in*, int c) { return c; }
int g_eof(struct g_in*) { return 1; }


int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  switch (event) {
    case kEventInit: reset(pd);
    default: }
	return 0; }

static void shift_waveform(int n) {
  K.active_waveform += n;
  K.active_waveform %= LEN(waveforms);
  K.pd->sound->synth->setWaveform(K.synth, waveforms[K.active_waveform]); }


static int life_update(void *userdata) {
  get_buttons();
  if (K.pushed & (kButtonA | kButtonB)) random_life();
  uint8_t db[NROWS][NCOLS];
  for (int i = 0; i < NROWS; i++)
    for (int j = 0; j < NCOLS; j++) {
      int i_1 = i-1<0?NROWS-1:i-1,
          i1  = i+1==NROWS?0:i+1,
          j_1 = j-1<0?NCOLS-1:j-1,
          j1  = j+1==NCOLS?0:j+1,
          n = (K.c.cb[i_1 * NCOLS + j_1] == live_char ? 1 : 0)
            + (K.c.cb[i_1 * NCOLS + j] == live_char ? 1 : 0)
            + (K.c.cb[i_1 * NCOLS + j1] == live_char ? 1 : 0)
            + (K.c.cb[i * NCOLS + j_1] == live_char ? 1 : 0)
            + (K.c.cb[i * NCOLS + j1] == live_char ? 1 : 0)
            + (K.c.cb[i1*NCOLS+j_1] == live_char ? 1 : 0)
            + (K.c.cb[i1*NCOLS+j] == live_char ? 1 : 0)
            + (K.c.cb[i1*NCOLS+j1] == live_char ? 1 : 0);
      db[i][j] = n == 3 || (n == 2 && K.c.cb[i*NCOLS+j] == live_char) ? live_char : dead_char; }
  memcpy(K.c.cb, db, sizeof(db));
  return 1; }


static int g_synth_update(void* userdata) {
  get_buttons();
  switch (K.synth_mode) {
    case 0:
      if (K.pushed & (kButtonA | kButtonB)) K.synth_time = 1, K.synth_mode = 1;
      else {
        K.pd->sound->synth->setVolume(K.synth, K.vol, K.vol);
        K.pd->sound->synth->playNote(K.synth, K.freq, 100, 1.0f/20, 0);
        K.freq *= 1 + K.pd->system->getCrankChange() / 360.0f;
        if (K.pushed & kButtonLeft) shift_waveform(-1);
        if (K.pushed & kButtonRight) shift_waveform(1); }
      return 1;
    case 1:
      if (K.pushed & (kButtonA | kButtonB)) K.synth_mode = 2;
      else {
        K.synth_time *= (1 + 11 * K.pd->system->getCrankChange() / 360.0f);
        if (K.synth_time < 0) K.synth_time = 0;
        if (K.synth_time > NROWS * NCOLS) K.synth_time = NROWS * NCOLS;
        cb_cur(&K.c, 0, 0);
        cb_fill(&K.c, dead_char);
        for (int i = (int) K.synth_time; i; i--) put_char(live_char); }
      return 1;
    case 2:
      if (K.pushed & (kButtonA | kButtonB)) K.synth_mode = 1;
      else {
        K.synth_time -= 1.0f/30;
        if (K.synth_time <= 0) K.synth_mode = 0;
        cb_cur(&K.c, 0, 0);
        cb_fill(&K.c, dead_char);
        for (int i = (int) K.synth_time; i; i--) put_char(live_char); }
    default:
      return 1; } }

#include <time.h>
#include <unistd.h>
g_noinline uintptr_t g_clock(void) {
  return K.pd->system->getCurrentTimeMilliseconds(); }

g_vm(g_fps) {
  float fps = K.pd->display->getFPS();
  int i = (int) fps;
  Sp[0] = g_putnum(i);
  Ip += 1;
  return Continue(); }
g_vm(g_buttons) { return
  get_buttons(),
  Sp[0] = g_putnum(K.current),
  Ip += 1,
  Continue(); }
g_vm(g_cursor_h) {
  int n = g_getnum(Sp[0]);
  K.c.col += n;
  K.c.col %= NCOLS;
  Ip += 1;
  return Continue(); }
g_vm(g_cursor_v) {
  int n = g_getnum(Sp[0]);
  K.c.row += n;
  K.c.row %= NROWS;
  Ip += 1;
  return Continue(); }

g_vm(theta) {
  float t = K.pd->system->getCrankChange();
  int delta = (int) (256 * t / 360.0f);
  Sp[0] = g_putnum(delta);
  Ip += 1;
  return Continue(); }

static void cb_put_glyph(struct cb *cb, uint8_t c) {
  cb->cb[cb->row*cb->cols+cb->col] = c; }
static uint8_t cb_get_glyph(struct cb *cb) {
  return cb->cb[cb->row*cb->cols+cb->col]; }
g_vm(g_get_glyph) { return Sp[0] = g_putnum(cb_get_glyph(&K.c)), Ip += 1, Continue(); }
g_vm(g_put_glyph) { return cb_put_glyph(&K.c, g_getnum(Sp[0])), Ip += 1, Continue(); }
g_vm(g_clear) { return k_clear(), Ip += 1, Continue(); }
