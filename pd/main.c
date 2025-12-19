#include "pd_api.h"
#include "g.h"
#include "font.h"
#include "cb.h"
#include <limits.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#define NROWS 30
#define NCOLS 50
#define kcb (&K.cb)
#define show_cursor_flag 1
#define mode(k) ((void*)k->mode)
static struct k {
  struct g *g;
  PlaydateAPI *pd;
  struct { PDButtons current, pushed, released; } b;
  struct mode {
    struct mode *prev, *next;
    void (*ini)(void), (*update)(void), (*fin)(void);
    intptr_t data[];
  } *mode;
  union {
    struct cb cb;
    uint8_t cb_bytes[sizeof(struct cb) + NROWS * NCOLS]; };
} K;

static int k_update(void *_) {
  K.pd->system->setAutoLockDisabled(0); 
  K.pd->system->getButtonState(&K.b.current, &K.b.pushed, &K.b.released);
  if (K.b.pushed & (kButtonUp | kButtonDown))
    K.mode->fin(),
    K.mode = K.b.pushed & kButtonUp ? K.mode->next : K.mode->prev,
    K.mode->ini();
  K.mode->update();
  // draw the screen
  uint8_t *frame = K.pd->graphics->getFrame();
  for (uint8_t i = 0, rows = K.cb.rows; i < rows; i++)
    for (uint8_t j = 0, cols = K.cb.cols; j < cols; j++) {
      uint16_t pos = i * cols + j;
      uint8_t g = K.cb.cb[pos], *bmp = cga_8x8[g == '\n' ? 0 : g];
      bool invert = (K.cb.flag & show_cursor_flag) && pos == K.cb.wpos && g_clock() & 512;
      for (uint8_t b = 0; b < 8; b++)
        frame[52 * (8 * i + b) + j] = invert ? ~bmp[b] : bmp[b]; }
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
  bif_cur_set[] = {{curry}, {.x=gputnum(2)}, {cur_set}, {ret0}},
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
  float synth_time, set_time, freq;
};
static struct mode _life, _log;
static struct synth_mode
  _synth = { { &_life, &_log, g_synth_ini, g_synth_update, g_nop, }, NULL, 0, 1, 1, 1, 237 };
static struct  mode
  _life  = { &_log, (void*) &_synth, g_life_ini, g_life_update, g_nop, },
  _log  = { (void*) &_synth, &_life, g_log_ini, g_log_update, g_nop, };

static void *g_pd_malloc(struct g*, uintptr_t n) {
  return K.pd->system->realloc(NULL, n); }

static void g_pd_free(struct g*, void*x) {
  K.pd->system->realloc(x, 0); }

static void k_boot(void);
int eventHandler(PlaydateAPI* pd, PDSystemEvent event, uint32_t arg) {
  switch (event) {
    case kEventInit:
      clockfp = pd->system->getCurrentTimeMilliseconds;
      kcb->rows = NROWS, kcb->cols = NCOLS;
      K.pd = pd;
      K.mode = &_log;
      _synth.synth = pd->sound->synth->newSynth();
      K.g = ginid(g_pd_malloc, g_pd_free);
      K.g = gdefs(K.g, LEN(defs), defs);
      if (gokp(K.g))
        K.mode->ini(),
        pd->system->setUpdateCallback(k_update, NULL);
    default: return 0; } }

static void g_log_update(void) {
  cb_cur(kcb, 0, 0);
  cb_fill(kcb, 0);
  K.g = gevals_(K.g,
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
  Sp[0] = d ? g_nil : gputnum((int)a%360);
  Ip += 1;
  return Continue(); }

static void g_log_ini(void) { kcb->flag |= show_cursor_flag; }
static struct g*g_boot(struct g*f);



#define live_char 0xdb
#define dead_char 0x00
static void random_life(void) {
  uint32_t rows = kcb->rows, cols = kcb->cols;
  for (uint32_t i = 0; i < rows; i++)
    for (uint32_t j = 0; j < cols; j++)
      kcb->cb[i* cols + j] = rand() & 1 ? live_char : dead_char; }

static void g_life_ini(void) {
  kcb->flag &= ~show_cursor_flag;
  random_life(); }

static const SoundWaveform synth_waveforms[] = {
  kWaveformSine, kWaveformSquare, kWaveformNoise, };
static float square_wave(float), tri_wave(float), sine_wave(float), noise_wave(float),
             saw_wave(float);

static float square_wave(float x) {
  int i = (int) x;
  return (float) (i & 1); }
static float sine_wave(float x) {
  return (sinf(x) + 1.0f) / 2.0f; }
static float noise_wave(float x) {
  int r = rand();
  return (float) r / (float) RAND_MAX; }

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
  kcb->flag &= ~show_cursor_flag;
  cb_cur(kcb, 0, 0);
  cb_fill(kcb, 0); }


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
      if (K.b.pushed & (kButtonA | kButtonB)) m->synth_time = m->set_time, m->submode = 1;
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
      if (K.b.pushed & (kButtonA | kButtonB)) m->set_time = m->synth_time, m->submode = 2;
      else {
        m->synth_time *= (1 + 11 * K.pd->system->getCrankChange() / 360.0f);
        if (m->synth_time < 0.1f) m->synth_time = 0.1f;
        if (m->synth_time > NROWS * NCOLS) m->synth_time = NROWS * NCOLS;
        cb_cur(cb, 0, 0);
        cb_fill(cb, 0);
        for (int i = (int) m->synth_time; i; i--)
          cb_putc(cb, 0x9); }
      return;
    case 2:
      if (K.b.pushed & (kButtonA | kButtonB)) m->submode = 1;
      else {
        m->synth_time -= 1.0f/30;
        if (m->synth_time <= 0) m->submode = 0;
        else {
          K.pd->system->setAutoLockDisabled(1); 
          cb_cur(cb, 0, 0);
          cb_fill(cb, 0);
          for (int i = (int) m->synth_time; i; i--)
            cb_putc(cb, 0x9); } }
    default:
      return; } }

static g_vm(g_buttons) { return
  Sp[0] = gputnum(K.b.current),
  Ip += 1,
  Continue(); }

static void ls_cb(const char *p, void *_) {
  K.g = gconsl(gstrof(K.g, p)); }

static g_vm(ls_root) {
  f = gpush(f, 1, g_nil);
  if (gokp(f)) {
    K.g = f;
    K.pd->file->listfiles("/", ls_cb, NULL, 0);
    f = K.g; }
  if (!g_ok(f)) return f;
  return f->sp[1] = f->sp[0], f->sp++, f->ip++, Continue(); }

static g_vm(cur_row) { return Sp[0] = gputnum(kcb->wpos / kcb->cols), Ip++, Continue(); }
static g_vm(cur_col) { return Sp[0] = gputnum(kcb->wpos % kcb->cols), Ip++, Continue(); }
static g_vm(cur_set) {
  uintptr_t r = ggetnum(Sp[0]), c = ggetnum(Sp[1]);
  cb_cur(kcb, r, c);
  Sp += 1;
  Ip += 1;
  return Continue(); }
static g_vm(cur_put) {
  kcb->cb[kcb->wpos] = ggetnum(Sp[0]);
  Ip += 1;
  return Continue(); }

void g_stdout_putc(struct g*f, int c) { cb_putc(kcb, c); }
int g_stdin_getc(struct g*f) { return cb_getc(kcb); }
int g_stdin_ungetc(struct g*f, int c) { return cb_ungetc(kcb, c); }
int g_stdin_eof(struct g*f) { return cb_eof(kcb); }
