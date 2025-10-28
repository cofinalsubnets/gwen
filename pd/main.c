#include <stdio.h>
#include <stdlib.h>
#include "g.h"
#include <stdlib.h>
#include <stdarg.h>
#include "pd_api.h"
#include "font.h"
#include "cb.h"
#include <time.h>
#include <unistd.h>
#define Sp f->sp
#define Hp f->hp
#define Ip f->ip
#define NROWS 30
#define NCOLS 50
#define cb_size_bytes(r, c) (5 * sizeof(intptr_t) + (r) * (c))
#define show_cursor 1
#define kpd (K.pd)
#define kgl (K.g)
#define base_ref(n) topref(kgl, n)
#define bcb ((struct cb*)g_str_txt((intptr_t)(struct g_vec*)base_ref(0)))
#define kcb bcb
#define cb_slot(f) (f)->v[7]
#define _jcb(f) ((struct cb*)cb_slot(f))
#define jcb _jcb(kgl)

struct mode {
  void (*ini)(void),
       (*update)(void),
       (*fin)(void);
  struct mode *prev, *next;
  intptr_t data[]; };
static struct mode
  _synth, _log, _life;
static void g_log_update(void), g_synth_update(void), g_life_update(void);
static void g_synth_ini(void), draw_gb(void), g_log_ini(void), g_synth_ini(void), g_life_ini(void);
static void g_nop(void) {}
static struct  mode
 _life = { g_life_ini, g_life_update, g_nop, &_log, &_synth},
 _synth = { g_synth_ini, g_synth_update, g_nop, &_life, &_log },
 _log = { g_log_ini, g_log_update, g_nop, &_synth, &_life};

struct mode *K_mode = &_life;
enum g_mode {
  g_mode_draw,
  g_mode_life,
  g_mode_synth,
  N_modes };
static struct K {
  PlaydateAPI *pd;
  struct g *g;
  PDSynth *synth;
  PDMenuItem *mode_menu_item;
  enum g_mode mode;
  int active_waveform, synth_mode;
  float synth_time, freq, freq_max, freq_min, vol;
  struct {
    PDButtons current, pushed, released; } b;
} K;

g_noinline uintptr_t g_clock(void) {
  return kpd->system->getCurrentTimeMilliseconds(); }



static int k_update(void *_);
static void reset(PlaydateAPI *pd);
int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  switch (event) {
    case kEventInit:
      pd->system->setUpdateCallback(k_update, pd);
      reset(pd);
    default: }
	return 0; }

static void get_buttons(void);
static void get_buttons(void) {
  kpd->system->getButtonState(&K.b.current, &K.b.pushed, &K.b.released); }

static void draw_char_buffer(struct cb *c, uint8_t font[][8], uint8_t *frame);
static int k_update(void *_pd) {

  get_buttons();
  if (!(K.b.pushed & (kButtonUp | kButtonDown))) K_mode->update();
  else {
    if (K.b.pushed & kButtonUp)
      K_mode->fin(), K_mode = K_mode->next, K_mode->ini();
    else
      K_mode->fin(), K_mode = K_mode->prev, K_mode->ini(); }

  return
    draw_char_buffer(kcb, cga_8x8, kpd->graphics->getFrame()),
    kpd->graphics->markUpdatedRows(0, LCD_ROWS),
    1; }

static void draw_char_buffer(struct cb *c, uint8_t font[][8], uint8_t *frame) {
  uint32_t rows = c->rows, cols = c->cols;
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++)
      for (uint8_t k = 0, *glyph = font[c->cb[i * cols + j]], g; k < 8; k++)
        frame[52 * (8 * i + k) + j] = (c->flag & show_cursor) && i == c->row && j == c->col ? ~glyph[k] : glyph[k]; }



static const SoundWaveform waveforms[] = {
  kWaveformSquare, kWaveformTriangle, kWaveformSine, kWaveformNoise,
  kWaveformSawtooth, kWaveformPOPhase, kWaveformPODigital, kWaveformPOVosim, };




void
  g_fb_set_cursor(int, int);

static g_vm_t g_buttons, g_cursor_h, g_cursor_v, theta, g_get_glyph, g_put_glyph, g_fps, g_clear;
static void random_life(void);




struct g*g_stdout_putc(struct g*f, struct g_out *o, int c) { return
  kgl = f,
  cb_put_char(kcb, c),
  f; }


static void g_log_update(void) {
  cb_fill(kcb, 0);
  cb_cur(kcb, 0, 0);
  kgl = g_pop(g_evals(kgl,
    "(: (AB x) (A (B x)) (BB x) (B (B x))"
       "i (sysinfo 0) f (A i) pool (AB i) len (A (BB i)) allocd (AB (BB i)) stackd (A (BB (BB i)))"
   " (,"
    "(puts \"\x01 gwen lisp\n\nf@\")"
    "(putn f 16)"
    "(puts\"\n pool=\") (putn pool 16)" 
    "(puts\"\n len=\") (putn len 10)"
    "(puts\"\n allocd=\") (putn allocd 10)"
    "(puts\"\n stackd=\") (putn stackd 10)"
    "(puts \"\n\ncrank: \") (putn (crank_angle 0) 10) (puts \"\xf8\")"
    "(puts \"\nbuttons: \") (putn (get_buttons 0) 2)"
    ")"
    ")"
    ), 1); }


static g_vm(crank_angle) {
  int d = kpd->system->isCrankDocked();
  float a = kpd->system->getCrankAngle();
  Sp[0] = d ? g_nil : g_putnum((int)a%360);
  Ip += 1;
  return Continue(); }
static g_vm(fb_get) {
  size_t row = g_getnum(Sp[0]),
         col = g_getnum(Sp[1]);
  *++Sp = g_putnum(kcb->cb[(row % kcb->rows) * kcb->cols + (col % kcb->cols)]);
  Ip += 1;
  return Continue(); }

static g_vm(fb_put) {
  size_t row = g_getnum(Sp[0]),
         col = g_getnum(Sp[1]);
  char b = g_getnum(Sp[2]);
  kcb->cb[(row % kcb->rows) * kcb->cols + (col % kcb->cols)] = b;
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
  bif_crank_angle[] = {{crank_angle}, {ret0}},
  bif_fb_get[] = { {curry}, {.x = g_putnum(2)}, {fb_get}, {ret0}},
  bif_fb_put[] = { {curry}, {.x = g_putnum(3)}, {fb_put}, {ret0}};


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
  {"crank_angle", bif_crank_angle},
  {"get_buttons", bif_buttons}, };

const char g_init_prog[] = "(,"
 "(: (puts s) (: (css m n) (? (< m n) (, (putc (sget s m)) (css (+ 1 m) n))) (css 0 (slen s))))"
 "(:(event ev arg)(,(.'got_event)(. ev)(. arg)))"
 "(: (update _) (: fps (get_fps 0) (,"
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

static struct g *ggvprintf(struct g*f, struct g_out*o, const char *fmt, va_list xs) {
  while (*fmt) {
    char c = *fmt++;
    if (c != '%') f = g_putc(f, o, c);
    else re:
      switch (c = *fmt++) {
      case 0: return f;
      default: f = g_putc(f, o, c); break;
      case 'l': goto re;
      case 'd': f = g_putn(f, o, va_arg(xs, uintptr_t), 10); break;
      case 'x': f = g_putn(f, o, va_arg(xs, uintptr_t), 16); break;
      case 'o': f = g_putn(f, o, va_arg(xs, uintptr_t), 8); break;
      case 'b': f = g_putn(f, o, va_arg(xs, uintptr_t), 2); break; } }
  return f; }
struct g*gg_printf(struct g*f, struct g_out*o, const char*fmt, ...) {
  va_list xs;
  va_start(xs, fmt);
  f = ggvprintf(f, o, fmt, xs);
  va_end(xs);
  return f; }
static void g_log_ini(void) { kcb->flag |= show_cursor; }
static void reset(PlaydateAPI *pd) {
  kpd = pd;
  K_mode->fin();
  pd->sound->synth->freeSynth(K.synth);
  K.synth = pd->sound->synth->newSynth();
  pd->system->removeAllMenuItems();

  static intptr_t g_static_pool[1<<20];
  struct g*f = g_ini_static(sizeof(g_static_pool), g_static_pool);
  f = g_defns(f, LEN(defs), defs);
  f = g_pop(g_evals(f, g_init_prog), 1);
  f = g_vec0(f, g_vt_u8, 1, (uintptr_t) sizeof(struct cb) + NROWS * NCOLS);
  //if (g_ok(f)) cb_slot(f) = *f->sp++;

/*
  f = g_pop(g_evals(f, 
        "(: t0 (clock 0))"
#include "boot.h"
        "(: t1 (clock 0))"
        ), 1); // boot XXX crashes real playdate
*/
  kgl = f;
  kcb->rows = NROWS, kcb->cols = NCOLS;

  (K_mode = &_log)->ini(); }



#define live_char 0xb2
#define dead_char 0xb0
static void random_life(void) {
  uint32_t rows = kcb->rows, cols = kcb->cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      kcb->cb[i* cols + j] = rand() & 1 ? live_char : dead_char; }

static void g_life_ini(void) {
  kcb->flag &= ~show_cursor;
  random_life(); }

// g_life_fin = g_nop

static void g_synth_ini(void) {
  kcb->flag &= ~show_cursor;
  K.active_waveform = 0;
  K.freq = 1000;
  K.freq_max = 20000;
  K.freq_min = 20;
  K.vol = 0.5;
  K.synth_mode = 1;
  K.synth_time = 1;
  kpd->sound->synth->setWaveform(K.synth, waveforms[K.active_waveform]);
  cb_cur(kcb, 0, 0);
  cb_fill(kcb, 0); }
int g_getc(struct g_in *) { return 0; }
int g_ungetc(struct g_in*, int c) { return c; }
int g_eof(struct g_in*) { return 1; }


static void shift_waveform(int n) {
  K.active_waveform += n;
  K.active_waveform %= LEN(waveforms);
  kpd->sound->synth->setWaveform(K.synth, waveforms[K.active_waveform]); }


static void g_life_update(void) {
  if (K.b.pushed & (kButtonA | kButtonB)) random_life();
  uint8_t db[NROWS][NCOLS];
  for (int i = 0; i < NROWS; i++)
    for (int j = 0; j < NCOLS; j++) {
      int i_1 = i-1<0?NROWS-1:i-1,
          i1  = i+1==NROWS?0:i+1,
          j_1 = j-1<0?NCOLS-1:j-1,
          j1  = j+1==NCOLS?0:j+1,
          n = (kcb->cb[i_1 * NCOLS + j_1] == live_char ? 1 : 0)
            + (kcb->cb[i_1 * NCOLS + j] == live_char ? 1 : 0)
            + (kcb->cb[i_1 * NCOLS + j1] == live_char ? 1 : 0)
            + (kcb->cb[i * NCOLS + j_1] == live_char ? 1 : 0)
            + (kcb->cb[i * NCOLS + j1] == live_char ? 1 : 0)
            + (kcb->cb[i1*NCOLS+j_1] == live_char ? 1 : 0)
            + (kcb->cb[i1*NCOLS+j] == live_char ? 1 : 0)
            + (kcb->cb[i1*NCOLS+j1] == live_char ? 1 : 0);
      db[i][j] = n == 3 || (n == 2 && kcb->cb[i*NCOLS+j] == live_char) ? live_char : dead_char; }
  memcpy(kcb->cb, db, sizeof(db)); }


static void g_synth_update(void) {
  const int c = 0xfe - 0x21 + 1;
  switch (K.synth_mode) {
    case 0:
      if (K.b.pushed & (kButtonA | kButtonB)) K.synth_time = 1, K.synth_mode = 1;
      else {
        kpd->sound->synth->setVolume(K.synth, K.vol, K.vol);
        kpd->sound->synth->playNote(K.synth, K.freq, 100, 1.0f/20, 0);
        K.freq *= 1 + kpd->system->getCrankChange() / 360.0f;
        if (K.b.pushed & kButtonLeft) shift_waveform(-1);
        if (K.b.pushed & kButtonRight) shift_waveform(1); }
      return;
    case 1:
      if (K.b.pushed & (kButtonA | kButtonB)) K.synth_mode = 2;
      else {
        K.synth_time *= (1 + 11 * kpd->system->getCrankChange() / 360.0f);
        if (K.synth_time < 0.1f) K.synth_time = 0.1f;
        if (K.synth_time > NROWS * NCOLS) K.synth_time = NROWS * NCOLS;
        cb_cur(kcb, 0, 0);
        cb_fill(kcb, 0);
        for (int i = (int) K.synth_time; i; i--)
          cb_put_char(kcb, 0x20 + c - (i % c)); }
      return;
    case 2:
      if (K.b.pushed & (kButtonA | kButtonB)) K.synth_mode = 1;
      else {
        kpd->system->setAutoLockDisabled(1); 
        K.synth_time -= 1.0f/30;
        if (K.synth_time <= 0) K.synth_mode = 0;
        cb_cur(kcb, 0, 0);
        cb_fill(kcb, 0);
        for (int i = (int) K.synth_time; i; i--)
          cb_put_char(kcb, 0x20 + c - (i % c));
      }
    default:
      return; } }


g_vm(g_fps) {
  float fps = kpd->display->getFPS();
  int i = (int) fps;
  Sp[0] = g_putnum(i);
  Ip += 1;
  return Continue(); }
g_vm(g_buttons) { return
  Sp[0] = g_putnum(K.b.current),
  Ip += 1,
  Continue(); }
g_vm(g_cursor_h) {
  intptr_t n = g_getnum(Sp[0]);
  cb_mv_cur(kcb, 0, n);
  Ip += 1;
  return Continue(); }
g_vm(g_cursor_v) {
  intptr_t n = g_getnum(Sp[0]);
  cb_mv_cur(kcb, n, 0);
  Ip += 1;
  return Continue(); }

g_vm(theta) {
  float t = kpd->system->getCrankChange();
  int delta = (int) (256 * t / 360.0f);
  Sp[0] = g_putnum(delta);
  Ip += 1;
  return Continue(); }

static void cb_put_glyph(struct cb *cb, uint8_t c) {
  cb->cb[cb->row*cb->cols+cb->col] = c; }
static uint8_t cb_get_glyph(struct cb *cb) {
  return cb->cb[cb->row*cb->cols+cb->col]; }
g_vm(g_get_glyph) { return Sp[0] = g_putnum(cb_get_glyph(kcb)), Ip += 1, Continue(); }
g_vm(g_put_glyph) { return cb_put_glyph(kcb, g_getnum(Sp[0])), Ip += 1, Continue(); }
g_vm(g_clear) {
  cb_cur(kcb, 0, 0);
  cb_fill(kcb, 0);
  return Ip += 1, Continue(); }
