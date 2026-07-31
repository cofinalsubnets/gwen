// Playdate frontend for love -- the rune CAS workbench riding the crank.
//
// love's frontend contract (love.h): the host defines ai_clock, the
// ai_stdin/ai_stdout ports, the ai_fd_port_vt vtable, and the cooperative-wait
// hooks. Here the console is a quay cb (50x30 cells of the 8x8 CGA font)
// blitted to the 1-bit LCD each frame; stdout/stderr both land there, so the
// prel's puts IS the screen and a scare face is visible. The heap rides the
// SDK realloc through ai_ini_m. The bootstrap egg compiles the love compiler
// with the C evaluator, recompiles it with itself, installs it, then
// crew/rune/ bakes in as a registered module and cas.l (this folder) drives
// the demo: the C update just clears the console, fires (cas ()), and blits.
//
// This file is mooncc-compiled on device (-t thumb2sp: the STM32F746's FPU is
// single-precision, so every f64 op softens to __aeabi_* -- the same libgcc
// helpers Panic's own toolchain leans on). The SDK lives behind pdglue.c's
// word-only surface; nothing here sees pd_api.h, a float ABI, or a variadic.
#include "../../love.h"
#include "quay.h"
#include "pdglue.h"

#ifndef EOF
#define EOF (-1)
#endif

#define NROWS 30
#define NCOLS 50
#define kcb (&K.cb)
static struct k {
  struct ai *g;
  int dead;                    // a scare froze the session; keep blitting it
  union {
    struct cb cb;
    uint8_t cb_bytes[sizeof(struct cb) + sizeof(uint32_t) * NROWS * NCOLS]; };
} K;

// --- clock + cooperative waits ---------------------------------------------
uintptr_t ai_clock(void) { return pdg_ms(); }
void ai_sleep(uintptr_t ms) {
  uintptr_t start = ai_clock();
  if (ms) while (ai_clock() - start < ms) ;
}
// the readiness law (host/main.c, the teensy's Enter-freeze lesson): a
// NEGATIVE fd is ALWAYS ready -- a string port waits on nothing external.
// fd 0 (the console buffer) answers instantly too (data or EOF), so every
// fd is honestly ready here.
bool ai_ready(int fd) { (void) fd; return 1; }
void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ms) { ai_sleep(ms); }

// --- port vtable: both ports ride the console buffer -----------------------
// cb_getc answers instantly, so a dry buffer IS the end here (-1, never 0) --
// which is why ai_ready above can say yes to every fd and mean it.
static intptr_t _readn(struct ai *g, unsigned char *dst, uintptr_t n) {
  (void) g;
  uintptr_t k = 0;
  for (int c; k < n && (c = cb_getc(kcb)) != EOF; ) dst[k++] = (unsigned char) c;
  return k ? (intptr_t) k : -1; }
static intptr_t _writen(struct ai **fp, unsigned char const *src, uintptr_t n) {
  (void) fp;
  for (uintptr_t k = 0; k < n; k++) cb_putc(kcb, src[k]);
  return (intptr_t) n; }
static struct ai *_flush(struct ai *g) { return g; }

struct ai_io ai_stdin  = { .ap = lvm_port_io, .fd = putcharm(0), .ungetc_buf = putcharm(EOF) };
struct ai_io ai_stdout = { .ap = lvm_port_io, .fd = putcharm(1), .ungetc_buf = putcharm(EOF) };
// No separate error stream on the device; the scare face lands on the LCD too.
struct ai_io ai_stderr = { .ap = lvm_port_io, .fd = putcharm(1), .ungetc_buf = putcharm(EOF) };
struct ai_port_vt const ai_fd_port_vt = { _flush, _writen, _readn };

// --- the playdate nifs ------------------------------------------------------
// (crank ())     -- the crank angle 0..359, or () docked
// (pushed ())    -- this frame's fresh button bits (left 1 right 2 up 4
//                   down 8 B 16 A 32)
// (cur_set r c)  -- seat the console's write cursor
static lvm(ai_crank) {
  Sp[0] = pdg_crank_docked() ? ai_nil : putcharm(pdg_crank_deg());
  Ip += 1;
  return Continue(); }
static lvm(ai_pushed) {
  Sp[0] = putcharm(pdg_pushed());
  Ip += 1;
  return Continue(); }
static lvm(ai_cur_set) {
  cb_cur(kcb, getcharm(Sp[0]), getcharm(Sp[1]));
  Sp += 1;
  Ip += 1;
  return Continue(); }

static union u const
  nif_crank[]   = {{ai_crank}, {lvm_ret0}},
  nif_pushed[]  = {{ai_pushed}, {lvm_ret0}},
  nif_cur_set[] = {{lvm_cur}, {.x = putcharm(2)}, {ai_cur_set}, {lvm_ret0}};
static struct ai_def defs[] = {
  {"crank",   (intptr_t) nif_crank},
  {"pushed",  (intptr_t) nif_pushed},
  {"cur_set", (intptr_t) nif_cur_set} };

// --- the frame --------------------------------------------------------------
static void blit(void) {
  uint8_t *frame = pdg_frame();
  for (uint32_t i = 0; i < NROWS; i++)
    for (uint32_t j = 0; j < NCOLS; j++) {
      uint8_t ch = cb_ch(K.cb.cb[i * NCOLS + j]);
      uint8_t const *bmp = cga_8x8[ch == '\n' ? 0 : ch];
      for (uint32_t b = 0; b < 8; b++)
        frame[PDG_ROWSIZE * (8 * i + b) + j] = bmp[b]; }
  pdg_mark_updated(); }

static int k_update(void *_) {
  pdg_poll_buttons();
  if (!K.dead) {
    cb_cur(kcb, 0, 0);
    cb_fill(kcb, 0);
    K.g = ai_evals_(K.g, "(cas ())");
    if (!ai_ok(K.g)) {
      // honest face: the condition prints to the console, and the screen
      // freezes on it (reset the device to go again)
      if (ai_code_of(K.g) == ai_status_scare) ai_scare_face_(K.g);
      K.dead = 1; } }
  blit();
  return 1; }

#if TARGET_PLAYDATE
// newlib syscall stubs: love.c's strtod/strtol pull gdtoa, whose error paths
// reach abort and the stdio buffer setup; none of them can actually run here,
// the linker just wants the names.
int _write(int fd, const void *b, size_t n) { return n; }
void _exit(int c) { for (;;) ; }
int _fstat(int fd, void *st) { return -1; }
int _isatty(int fd) { return 0; }
int _kill(int pid, int sig) { return -1; }
int _getpid(void) { return 1; }
int _close(int fd) { return -1; }
long _lseek(int fd, long o, int w) { return -1; }
int _read(int fd, void *b, size_t n) { return 0; }
void *_sbrk(intptr_t n) { return (void *) -1; }
#endif

// --- entry -------------------------------------------------------------------
static void *pd_alloc(struct ai *g, void *p, size_t n) {
  return n ? pdg_realloc(NULL, n) : (pdg_realloc(p, 0), NULL); }

void love_init(void) {
  pdg_log("love: init");
  cb_open(kcb, NROWS, NCOLS);
  kcb->flag |= cb_lnm | cb_wrap;   // console discipline + autowrap the long forms
  // WAKE-FIRST: the pdx bundles the qemu-baked heap image (love-pd.img --
  // egg + rune + cas, wake-checked at build time). A good wake skips the
  // minutes-long on-device bake the OS watchdog would kill anyway; any
  // problem answers NULL and the egg lane below bakes from source (the
  // 64-bit simulator refuses the 32-bit image this way BY DESIGN).
  struct ai *g0 = NULL;
  { enum { imgcap = 2u << 20 };
    void *ib = pdg_realloc(NULL, imgcap);
    int n = ib ? pdg_file_read("love-pd.img", ib, imgcap) : -1;
    if (n > 0) g0 = ai_image_load_m(ib, (uintptr_t) n, pd_alloc);
    if (ib) pdg_realloc(ib, 0); }
  int woke = g0 != NULL;
  pdg_log(woke ? "love: image awake" : "love: no image -- baking the egg");
  for (char const *s = woke ? "; love/playdate -- image awake"
                            : "; love/playdate -- baking the egg"; *s; s++)
    cb_putc(kcb, *s);
  blit();
  struct ai *g = ai_defn(woke ? g0 : ai_ini_m(pd_alloc), defs, countof(defs));
  pdg_log(ai_ok(g) ? "love: core up" : "love: core FAILED");
  // bound the collector to a QUARTER of the device's 16 MB (the Appel knob,
  // teensy's law): a major resize holds old and new pools at once, so the
  // transient peak is double the budget -- 8 MB here, and the simulator
  // emulates the device heap exactly (a budget of half OOMed it).
  if (ai_ok(g)) ai_core_of(g)->budget = (4u << 20) / sizeof(ai_word);
  if (woke) {
    K.g = ai_layer_(g);          // the waker opens its own session (the bake carries none)
    pdg_log("love: woke -- workbench up");
    if (ai_ok(K.g)) pdg_set_update(k_update);
    return; }
  static char const src_q[] =
#include "q.h"
  ;
  static char const src_kanren[] =
#include "kanren.h"
  ;
  static char const src_rune[] =
#include "rune.h"
  ;
  g = ai_lib_(g, "q", src_q);                          // q, kanren and rune are MODULES: registered here, loaded
  g = ai_lib_(g, "kanren", src_kanren);                //   by name below -- q is rune's coefficient field, kanren
  g = ai_lib_(g, "rune", src_rune);                    //   its matcher's unifier (subst through the registry,
  K.g = ai_egg_(g,                                     //   unify/ufail?/var down the splice)
#include "egg.h"
    ,
#include "p1.h"
    ,
#include "prel.h"
    ,
#include "ev.h"
    );
  K.g = ai_evals_(K.g,
    "(use 'q)"
    " "
    "(use 'kanren)"
    " "
    "(use 'rune)"
    " "
#include "cas.h"
    "0)");
  // THE SESSION: the crank's evals defglob here, never in the base
  K.g = ai_layer_(K.g);
  pdg_log(ai_ok(K.g) ? "love: boot eval ok" : "love: boot eval FAILED");
  if (ai_ok(K.g))
    pdg_set_update(k_update);
  else if (ai_code_of(K.g) == ai_status_scare)
    ai_scare_face_(K.g),          // the condition lands on the cb...
    blit();                       // ...and freezes on the LCD
}
