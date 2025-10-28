#include "pd_api.h"
#include "g.h"
#include "font.h"
#include "cb.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <unistd.h>
#define gk(g) ((struct k*)(g)->u[1])
#define kcb(k) gcb((k)->g)
#define gcb(f) ((struct cb*)g_str_txt((f)->u[0]))
#define NROWS 30
#define NCOLS 50
#define show_cursor_flag 1
#define mode(k) ((void*)k->mode)
struct k {
  struct g *g;
  PlaydateAPI *pd;
  struct { PDButtons current, pushed, released; } b;
  struct mode {
    struct mode *prev, *next;
    void (*ini)(struct k*),
         (*update)(struct k*),
         (*fin)(struct k*);
    intptr_t data[];
  } *mode; };

static g_inline void kbuttons(struct k*k) {
  k->pd->system->getButtonState(&k->b.current, &k->b.pushed, &k->b.released); }

static int k_update(void *_k) {
  struct k *k = _k;
  k->pd->system->setAutoLockDisabled(0); 
  kbuttons(k);
  if (k->b.pushed & (kButtonUp | kButtonDown))
    k->mode->fin(k),
    k->mode = k->b.pushed & kButtonUp ? k->mode->next : k->mode->prev,
    k->mode->ini(k);
  k->mode->update(k);
  // draw the screen
  struct cb *c = kcb(k);
  uint8_t *frame = k->pd->graphics->getFrame();
  uint32_t rows = c->rows, cols = c->cols;
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++)
      for (uint8_t b = 0, *glyph = cga_8x8[c->cb[i * cols + j]], g; b < 8; b++)
        frame[52 * (8 * i + b) + j] = (c->flag & show_cursor_flag) && i == c->row && j == c->col ? ~glyph[b] : glyph[b];
  k->pd->graphics->markUpdatedRows(0, LCD_ROWS);
  return 1; }

static unsigned int (*clockfp)(void);
g_noinline uintptr_t g_clock(void) { return clockfp ? clockfp() : 0; }
static g_vm_t crank_angle, g_buttons, ls_root;
static union x
  bif_ls_root[] = {{ls_root}, {ret0}},
  bif_buttons[] = {{g_buttons}, {ret0}},
  bif_crank_angle[] = {{crank_angle}, {ret0}};
static struct g_def defs[] = {
  {"ls_root", bif_ls_root},
  {"crank_angle", bif_crank_angle},
  {"get_buttons", bif_buttons}, };


struct g*g_stdout_putc(struct g*f, struct g_out *o, int c) {
  return cb_put_char(gcb(f), c), f; }


static void g_nop(struct k*) {}
static void
  g_log_update(struct k*),
  g_synth_update(struct k*),
  g_life_update(struct k*),
  g_synth_ini(struct k*),
  g_log_ini(struct k*),
  g_life_ini(struct k*);
struct synth_mode {
  struct mode mode;
  PDSynth *synth;
  int active_waveform, submode;
  float synth_time, freq, freq_max, freq_min, vol;
};
static struct mode _life, _log;
static struct synth_mode
  _synth = { { &_life, &_log, g_synth_ini, g_synth_update, g_nop, }, };
static struct  mode
  _life  = { &_log, (void*) &_synth, g_life_ini, g_life_update, g_nop, },
  _log  = { (void*) &_synth, &_life, g_log_ini, g_log_update, g_nop, };

static void k_boot(struct k*);
int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  static struct k K;
  struct k *k = &K;
  switch (event) {
    case kEventInit:
      clockfp = pd->system->getCurrentTimeMilliseconds;
      k->pd = pd;
      k->mode = &_log;
      _synth.synth = pd->sound->synth->newSynth();
      static intptr_t g_static_pool[1<<20];
      struct g *f = g_ini_static(sizeof(g_static_pool), g_static_pool);
      f = g_defns(f, LEN(defs), defs);
      k->g = g_pop(g_evals(f, "(: t0(clock 0))"), 1);
      //k_boot(k);
      f = g_pop(g_evals(k->g, "(: t1(clock 0))"), 1);
      f = g_vec0(f, g_vt_u8, 1, (uintptr_t) sizeof(struct cb) + NROWS * NCOLS);
      if (g_ok(f)) {
        f->u[0] = g_pop1(f);
        f->u[1] = (intptr_t) k;
        struct cb *c = gcb(f);
        c->rows = NROWS, c->cols = NCOLS;
        k->g = f;
        k->mode->ini(k);
        pd->system->setUpdateCallback(k_update, k); }
    default: return 0; } }

static void k_eval(struct k*k, const char *s) {
  k->g = g_pop(g_evals(k->g, s), 1); }

static void g_log_update(struct k*k) {
  struct cb *c = kcb(k);
  cb_fill(c, 0);
  cb_cur(c, 0, 0);
  k_eval(k,
    "(: (AB x) (A (B x)) (BB x) (B (B x))"
       "i (sysinfo 0) f (A i) pool (AB i) len (A (BB i)) allocd (AB (BB i)) stackd (A (BB (BB i)))"
   " (,"
    "(puts \"\x01 gwen lisp up \") (putn (- (clock 0) t0) 10) (puts \" ticks (\")"
    "(putn (- t1 t0) 10) (puts \" boot)\n\nf@\")"
    "(putn f 16)"
    "(puts\"\n pool=\") (putn pool 16)" 
    "(puts\"\n len=\") (putn len 10)"
    "(puts\"\n allocd=\") (putn allocd 10)"
    "(puts\"\n stackd=\") (putn stackd 10)"
    "(puts \"\n\ncrank: \") (putn (crank_angle 0) 10) (puts \"\xf8\")"
    "(puts \"\nbuttons: \") (putn (get_buttons 0) 2)))"
    
    "(puts \"\n\n\") (. (ls_root 0))"
    ); }


static g_vm(crank_angle) {
  int d = gk(f)->pd->system->isCrankDocked();
  float a = gk(f)->pd->system->getCrankAngle();
  Sp[0] = d ? g_nil : g_putnum((int)a%360);
  Ip += 1;
  return Continue(); }
static void g_log_ini(struct k*k) { kcb(k)->flag |= show_cursor_flag; }
static struct g*g_boot(struct g*f);
/*
static void k_boot(struct k*k) {
  k_eval(k,
#include "boot.h"
    ); }
    */



#define live_char 0xb2
#define dead_char 0xb0
static void random_life(struct k*k) {
  struct cb *c = kcb(k);
  uint32_t rows = c->rows, cols = c->cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      c->cb[i* cols + j] = rand() & 1 ? live_char : dead_char; }

static void g_life_ini(struct k*k) {
  kcb(k)->flag &= ~show_cursor_flag;
  random_life(k); }

// g_life_fin = g_nop


static void g_synth_ini(struct k*k) {
  struct synth_mode *m = mode(k);
  m->active_waveform = 0;
  m->freq = 1000;
  m->freq_max = 20000;
  m->freq_min = 20;
  m->vol = 0.5;
  m->submode = 1;
  m->synth_time = 1;
  struct cb *c = kcb(k);
  c->flag &= ~show_cursor_flag;
  cb_cur(c, 0, 0);
  cb_fill(c, 0); }


static void g_life_update(struct k*k) {
  if (k->b.pushed & (kButtonA | kButtonB)) random_life(k);
  struct cb *c = kcb(k);
  uint8_t db[NROWS][NCOLS];
  for (int i = 0; i < NROWS; i++)
    for (int j = 0; j < NCOLS; j++) {
      int i_1 = i-1<0?NROWS-1:i-1,
          i1  = i+1==NROWS?0:i+1,
          j_1 = j-1<0?NCOLS-1:j-1,
          j1  = j+1==NCOLS?0:j+1,
          n = (c->cb[i_1 * NCOLS + j_1] == live_char ? 1 : 0)
            + (c->cb[i_1 * NCOLS + j] == live_char ? 1 : 0)
            + (c->cb[i_1 * NCOLS + j1] == live_char ? 1 : 0)
            + (c->cb[i * NCOLS + j_1] == live_char ? 1 : 0)
            + (c->cb[i * NCOLS + j1] == live_char ? 1 : 0)
            + (c->cb[i1*NCOLS+j_1] == live_char ? 1 : 0)
            + (c->cb[i1*NCOLS+j] == live_char ? 1 : 0)
            + (c->cb[i1*NCOLS+j1] == live_char ? 1 : 0);
      db[i][j] = n == 3 || (n == 2 && c->cb[i*NCOLS+j] == live_char) ? live_char : dead_char; }
  memcpy(c->cb, db, sizeof(db)); }

static void g_synth_update(struct k*k) {
  static const SoundWaveform synth_waveforms[] = {
    kWaveformSquare, kWaveformTriangle, kWaveformSine, kWaveformNoise,
    kWaveformSawtooth, kWaveformPOPhase, kWaveformPODigital, kWaveformPOVosim, };
  const int c = 0xfe - 0x21 + 1;
  struct cb*cb = kcb(k);
  struct synth_mode *m = mode(k);
  switch (m->submode) {
    case 0:
      if (k->b.pushed & (kButtonA | kButtonB)) m->synth_time = 1, m->submode = 1;
      else {
        k->pd->sound->synth->setVolume(m->synth, m->vol, m->vol);
        k->pd->sound->synth->playNote(m->synth, m->freq, 100, 1.0f/20, 0);
        m->freq *= 1 + k->pd->system->getCrankChange() / 360.0f;
        if (k->b.pushed & (kButtonLeft | kButtonRight)) {
          uintptr_t i = m->active_waveform + (k->b.pushed & kButtonLeft ? -1 : 1);
          if (i > LEN(synth_waveforms)) i %= LEN(synth_waveforms);
          m->active_waveform = i;
          k->pd->sound->synth->setWaveform(m->synth, synth_waveforms[i]); } }
      return;
    case 1:
      if (k->b.pushed & (kButtonA | kButtonB)) m->submode = 2;
      else {
        m->synth_time *= (1 + 11 * k->pd->system->getCrankChange() / 360.0f);
        if (m->synth_time < 0.1f) m->synth_time = 0.1f;
        if (m->synth_time > NROWS * NCOLS) m->synth_time = NROWS * NCOLS;
        cb_cur(cb, 0, 0);
        cb_fill(cb, 0);
        for (int i = (int) m->synth_time; i; i--)
          cb_put_char(cb, 0x20 + c - (i % c)); }
      return;
    case 2:
      if (k->b.pushed & (kButtonA | kButtonB)) m->submode = 1;
      else {
        k->pd->system->setAutoLockDisabled(1); 
        m->synth_time -= 1.0f/30;
        if (m->synth_time <= 0) m->submode = 0;
        else {
          cb_cur(cb, 0, 0);
          cb_fill(cb, 0);
          for (int i = (int) m->synth_time; i; i--)
            cb_put_char(cb, 0x20 + c - (i % c)); } }
    default:
      return; } }

static g_vm(g_buttons) { return
  Sp[0] = g_putnum(gk(f)->b.current),
  Ip += 1,
  Continue(); }

static void ls_cb(const char *p, void *_k) {
  struct k*k = _k;
  k->g = g_cons_l(g_strof(k->g, p)); }

static g_vm(ls_root) {
  f = g_push(f, 1, g_nil);
  if (g_ok(f)) {
    struct k*k = gk(f);
    k->g = f;
    k->pd->file->listfiles("/", ls_cb, k, 0);
    f = k->g; }
  if (!g_ok(f)) return f;
  return f->sp[1] = f->sp[0], f->sp++, f->ip++, Continue(); }

int g_getc(struct g_in *) { return 0; }
int g_ungetc(struct g_in*, int c) { return c; }
int g_eof(struct g_in*) { return 1; }
