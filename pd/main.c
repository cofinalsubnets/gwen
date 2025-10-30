#include "pd_api.h"
#include "g.h"
#include "font.h"
#include "cb.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <time.h>
#include <unistd.h>
#define gcb(g) ((struct cb*)g_str_txt((g)->u[0]))
#define kcb gcb(K.g)
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
    void (*ini)(void), (*update)(void), (*fin)(void);
    intptr_t data[];
  } *mode; };
static struct k K;

static int k_update(void *_) {
  K.pd->system->setAutoLockDisabled(0); 
  K.pd->system->getButtonState(&K.b.current, &K.b.pushed, &K.b.released);
  if (K.b.pushed & (kButtonUp | kButtonDown))
    K.mode->fin(),
    K.mode = K.b.pushed & kButtonUp ? K.mode->next : K.mode->prev,
    K.mode->ini();
  K.mode->update();
  // draw the screen
  struct cb *c = kcb;
  uint8_t *frame = K.pd->graphics->getFrame();
  uint32_t rows = c->rows, cols = c->cols;
  for (int i = 0; i < rows; i++)
    for (int j = 0; j < cols; j++) {
      uint16_t off = i * cols + j;
      for (uint8_t b = 0, *glyph = cga_8x8[c->cb[off]], g; b < 8; b++)
        frame[52 * (8 * i + b) + j] = (c->flag & show_cursor_flag) && off == c->wpos && K.g == K.g->pool ? ~glyph[b] : glyph[b];
    }
  K.pd->graphics->markUpdatedRows(0, LCD_ROWS);
  return 1; }

static unsigned int (*clockfp)(void);
g_noinline uintptr_t g_clock(void) { return clockfp ? clockfp() : 0; }
static g_vm_t crank_angle, g_buttons, ls_root, cur_row, cur_col, cur_put, cur_set;
static union x
  bif_ls_root[] = {{ls_root}, {ret0}},
  bif_buttons[] = {{g_buttons}, {ret0}},
  bif_cur_row[] = {{cur_row}, {ret0}},
  bif_cur_col[] = {{cur_col}, {ret0}},
  bif_cur_put[] = {{cur_put}, {ret0}},
  bif_cur_set[] = {{curry}, {.x=g_putnum(2)}, {cur_set}, {ret0}},
  bif_crank_angle[] = {{crank_angle}, {ret0}};
static struct g_def defs[] = {
  {"cur_row", (intptr_t) bif_cur_row},
  {"cur_col", (intptr_t) bif_cur_col},
  {"cur_set", (intptr_t) bif_cur_set},
  {"cur_put", (intptr_t) bif_cur_put},
  {"ls_root", (intptr_t) bif_ls_root},
  {"crank_angle", (intptr_t) bif_crank_angle},
  {"get_buttons", (intptr_t) bif_buttons}, };




static void g_nop(void) {}
static void
  g_log_update(void),
  g_synth_update(void),
  g_life_update(void),
  g_synth_ini(void),
  g_log_ini(void),
  g_life_ini(void);
struct synth_mode {
  struct mode mode;
  PDSynth *synth;
  int active_waveform, submode;
  float synth_time, freq;
};
static struct mode _life, _log;
static struct synth_mode
  _synth = { { &_life, &_log, g_synth_ini, g_synth_update, g_nop, }, NULL, 0, 1, 1, 237 };
static struct  mode
  _life  = { &_log, (void*) &_synth, g_life_ini, g_life_update, g_nop, },
  _log  = { (void*) &_synth, &_life, g_log_ini, g_log_update, g_nop, };

static void gg_eval(const char *s) {
  struct g *f = g_reads(K.g, s);
  K.g = f = g_ana(f, g_yield);
  while (g_ok(f)) K.g = f = f->ip->ap(f);
  f = g_code_of(f) == g_status_eof ? g_core_of(f) : f;
  K.g = g_pop(f, 1); }

static void k_boot(void);
int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  switch (event) {
    case kEventInit:
      clockfp = pd->system->getCurrentTimeMilliseconds;
      K.pd = pd;
      K.mode = &_log;
      _synth.synth = pd->sound->synth->newSynth();
      static intptr_t g_static_pool[1<<20];
      struct g *f = g_ini_static(sizeof(g_static_pool), g_static_pool);
      f = g_defs(f, LEN(defs), defs);
      f = g_vec0(f, g_vt_u8, 1, (uintptr_t) sizeof(struct cb) + NROWS * NCOLS);
      if (g_ok(f)) {
        f->u[0] = g_pop1(f);
        struct cb *c = gcb(f);
        c->rows = NROWS, c->cols = NCOLS;
        K.g = f;
        K.mode->ini();
        pd->system->setUpdateCallback(k_update, NULL); }
    default: return 0; } }

static void k_eval(const char *s) { K.g = g_pop(g_evals(K.g, s), 1); }
static void g_log_update(void) {
  struct cb *c = kcb;
  cb_cur(c, 0, 0);
  cb_fill(c, 0);
  k_eval(
    "(: i(sysinfo 0)f(A i)pool(A(B i))len(A(B(B i)))allocd(A(B(B(B i))))stackd(A(B(B(B(B i)))))"
    "(,"
    "(puts\"\x03 \")(putn(clock 0)10)"
    "(puts\"\n\n@\")""(putn pool 16)" 
    "(puts\"\n@\")""(putn f 16)"
    "(puts\"\n#\")"
    "(putn len 10)"
    "(puts\".\")"
    "(putn stackd 10)"
    "(puts\".\")""(putn allocd 10)))"
    "(puts\"\n\ncrank: \")(putn(crank_angle 0)10)(puts\"\xf8\")"
    "(puts\"\nbuttons: \")(putn(get_buttons 0)2)"
    "(puts\"\n\nroot folder contents:\n\")(.(ls_root 0))"
    "(: (cputc r1 c1 c) (: r0 (cur_row 0) c0 (cur_col 0) (, (cur_set r1 c1) (cur_put c) (cur_set r0 c0))))"
    "(: r0 (cur_row 0) c0 (cur_col 0) (, (cur_set 0 44) (puts\"life \x18\") (cur_set 29 44) (puts\"time \x19\") (cur_set r0 c0)))"
                             
    ); }


static g_vm(crank_angle) {
  int d = K.pd->system->isCrankDocked();
  float a = K.pd->system->getCrankAngle();
  Sp[0] = d ? g_nil : g_putnum((int)a%360);
  Ip += 1;
  return Continue(); }
static void g_log_ini(void) { kcb->flag |= show_cursor_flag; }
static struct g*g_boot(struct g*f);
/*
static void k_boot(void) {
  k_eval(
#include "boot.h"
    ); }
    */



#define live_char 0xdb
#define dead_char 0x00
static void random_life(void) {
  struct cb *c = kcb;
  uint32_t rows = c->rows, cols = c->cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      c->cb[i* cols + j] = rand() & 1 ? live_char : dead_char; }

static void g_life_ini(void) {
  kcb->flag &= ~show_cursor_flag;
  random_life(); }

static const SoundWaveform synth_waveforms[] = {
  kWaveformSine,
  kWaveformSquare,
  kWaveformNoise,
  //kWaveformTriangle,
  //kWaveformSawtooth,
};
static float square_wave(float), tri_wave(float), sine_wave(float), noise_wave(float),
             saw_wave(float);

static float square_wave(float x) {
  int i = (int) x;
  return (float) (i & 1); }
static float sine_wave(float x) {
  return (sinf(x) + 1.0f) / 2.0f; }
#include <limits.h>
static float noise_wave(float x) {
  int r = rand();
  return (float) r / (float) RAND_MAX; }

#include <math.h>
static void draw_wave(void) {
  struct synth_mode *m = (void*) K.mode;
  static float (*w)(float);
  switch (synth_waveforms[m->active_waveform % LEN(synth_waveforms)]) {
    case kWaveformSquare: w = square_wave; break;
    case kWaveformSine: w = sine_wave; break;
    default: w = noise_wave; }
  cb_fill(kcb, 0);
  uintptr_t off = g_clock() >> 5;
  for (int i = 0; i < NCOLS; i++) {
    float x = -0.5f + (float) i / NCOLS,
          y = w(x*m->freq + off);
    int r = y * NROWS;
    if (r >= NROWS) r = NROWS - 1;
    kcb->cb[i + r * NCOLS] = 0x7; } }


static void g_synth_ini(void) {
  struct synth_mode *m = (void*) K.mode;
  m->submode = 1;
  struct cb *c = kcb;
  c->flag &= ~show_cursor_flag;
  cb_cur(c, 0, 0);
  cb_fill(c, 0); }


static void g_life_update(void) {
  if (K.b.pushed & (kButtonA | kButtonB)) random_life();
  struct cb *c = kcb;
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

static void g_synth_update(void) {
  struct cb*cb = kcb;
  struct synth_mode *m = (void*) K.mode;
  switch (m->submode) {
    case 0:
      if (K.b.pushed & (kButtonA | kButtonB)) m->synth_time = 1, m->submode = 1;
      else {
        K.pd->sound->synth->playNote(m->synth, m->freq, 100, 1.0f/20, 0);
        m->freq *= 1 + K.pd->system->getCrankChange() / 360.0f;
        if (K.b.pushed & (kButtonLeft | kButtonRight)) {

          uintptr_t i = m->active_waveform + (K.b.pushed & kButtonLeft ? -1 : 1),
                    l = LEN(synth_waveforms);
          i = i == l ? 0 : i > l ? l - 1 : i;
          m->active_waveform = i;
          K.pd->sound->synth->setWaveform(m->synth, synth_waveforms[i]); }
        draw_wave(); }
      return;
    case 1:
      if (K.b.pushed & (kButtonA | kButtonB)) m->submode = 2;
      else {
        m->synth_time *= (1 + 11 * K.pd->system->getCrankChange() / 360.0f);
        if (m->synth_time < 0.1f) m->synth_time = 0.1f;
        if (m->synth_time > NROWS * NCOLS) m->synth_time = NROWS * NCOLS;
        cb_cur(cb, 0, 0);
        cb_fill(cb, 0);
        for (int i = (int) m->synth_time; i; i--)
          cb_put_char(cb, 0x9); }
      return;
    case 2:
      if (K.b.pushed & (kButtonA | kButtonB)) m->submode = 1;
      else {
        K.pd->system->setAutoLockDisabled(1); 
        m->synth_time -= 1.0f/30;
        if (m->synth_time <= 0) m->submode = 0;
        else {
          cb_cur(cb, 0, 0);
          cb_fill(cb, 0);
          for (int i = (int) m->synth_time; i; i--)
            cb_put_char(cb, 0x9); } }
    default:
      return; } }

static g_vm(g_buttons) { return
  Sp[0] = g_putnum(K.b.current),
  Ip += 1,
  Continue(); }

static void ls_cb(const char *p, void *_) {
  K.g = g_cons_l(g_strof(K.g, p)); }

static g_vm(ls_root) {
  f = g_push(f, 1, g_nil);
  if (g_ok(f)) {
    K.g = f;
    K.pd->file->listfiles("/", ls_cb, NULL, 0);
    f = K.g; }
  if (!g_ok(f)) return f;
  return f->sp[1] = f->sp[0], f->sp++, f->ip++, Continue(); }

static g_vm(cur_row) { return Sp[0] = g_putnum(gcb(f)->wpos / gcb(f)->cols), Ip++, Continue(); }
static g_vm(cur_col) { return Sp[0] = g_putnum(gcb(f)->wpos % gcb(f)->cols), Ip++, Continue(); }
static g_vm(cur_set) {
  uintptr_t r = g_getnum(Sp[0]), c = g_getnum(Sp[1]);
  struct cb *cb = gcb(f);
  cb_cur(cb, r, c);
  Sp += 1;
  Ip += 1;
  return Continue(); }
static g_vm(cur_put) {
  struct cb *cb = gcb(f);
  cb->cb[cb->wpos] = g_getnum(Sp[0]);
  Ip += 1;
  return Continue(); }

void g_stdout_putc(struct g*f, int c) { cb_put_char(gcb(f), c); }
int g_stdin_getc(struct g*f) {
  struct cb *cb = gcb(f);
  return cb_getc(cb); }
int g_stdin_ungetc(struct g*f, int c) { return cb_ungetc(gcb(f), c); }
int g_stdin_eof(struct g*f) { return cb_eof(gcb(f)); }
