// free/doom.c -- doom on inle: the four doors doomgeneric asks a platform for,
// answered off the kernel's own framebuffer, scancode tap and clock, plus the
// nif that starts it.
//
//   (doom)        run it. answers when the game quits -- which on this machine
//                 means doom's exit() reached (quit), so it resets.
//
// OPT-IN and not in any default build: `make run DOOM=1` wants the vendored
// source at dl/doomgeneric and the IWAD at dl/doom1.wad (free/kernel.mk's doom
// lane says how). This file is the tree's own 0BSD glue; what it includes is
// not, so nothing here builds unless someone put that source there on purpose.
//
// ⚠ the WAD is a BAKED FILE, not a disk one: k_baked hands the ramfs a row for
// it (kmain.c's hook), so doom's own fopen/fread reach it through free/sys.c
// with no filesystem mounted anywhere.
#include "love.h"
#include <stdint.h>
#include <string.h>
#include "doomgeneric.h"
#include "doomkeys.h"

// the kernel's doors (kmain.c)
bool k_fb(volatile uint32_t **p, int *w, int *h, int *pitch);
void k_scan_arm(int on);
int k_scan_pop(void);
uintptr_t k_clock_ms(void);
void k_sleep(uintptr_t ms);

// the IWAD, laid into .rodata by tools/mkblob.l
extern unsigned char const doom_wad[];
extern uintptr_t const doom_wad_len;

struct k_file { char const *path, *bytes; uintptr_t len, ms; };
// the strong definition of kmain.c's weak hook: one baked row, the IWAD. the
// mtime is 0 -- the blob carries none, and a date invented here would be a lie
// the corpus's stat laws could read.
int k_baked(struct k_file *rows, int cap) {
  if (rows && cap > 0)
    rows[0] = (struct k_file) { "doom1.wad", (char const *) doom_wad, doom_wad_len, 0 };
  return 1; }

// --- the four doors -------------------------------------------------------

static uintptr_t dg_epoch;

void DG_Init(void) {
  dg_epoch = k_clock_ms();
  k_scan_arm(1); }

// blit the 640x400 frame into the middle of whatever the door handed over. no
// scaling: a GOP mode smaller than the frame simply shows the part that fits,
// which is honest where a stretch would hide the mode.
void DG_DrawFrame(void) {
  volatile uint32_t *fb;
  int w, h, pitch;
  if (!k_fb(&fb, &w, &h, &pitch)) return;
  int cw = w < DOOMGENERIC_RESX ? w : DOOMGENERIC_RESX,
      ch = h < DOOMGENERIC_RESY ? h : DOOMGENERIC_RESY,
      ox = (w - cw) / 2, oy = (h - ch) / 2;
  for (int y = 0; y < ch; y++) {
    volatile uint32_t *d = fb + (uintptr_t) (y + oy) * pitch + ox;
    uint32_t const *s = DG_ScreenBuffer + (uintptr_t) y * DOOMGENERIC_RESX;
    for (int x = 0; x < cw; x++) d[x] = s[x]; } }

void DG_SleepMs(uint32_t ms) { k_sleep(ms); }

uint32_t DG_GetTicksMs(void) { return (uint32_t) (k_clock_ms() - dg_epoch); }

void DG_SetWindowTitle(char const *t) { (void) t; }

// PS/2 set 1, make codes; an extended key wears 0x100 (k_scan_pop's fold). the
// letters and digits come off a table because a switch of forty rows is worse.
static char const sc_ascii[] =
  "\0\0" "1234567890-=" "\0\0" "qwertyuiop[]" "\0\0" "asdfghjkl;'`"
  "\0" "\\" "zxcvbnm,./";
static unsigned char sc_key(int sc) {
  switch (sc) {
    case 0x01: return KEY_ESCAPE;
    case 0x0e: return KEY_BACKSPACE;
    case 0x0f: return KEY_TAB;
    case 0x1c: case 0x11c: return KEY_ENTER;
    case 0x1d: case 0x11d: return KEY_FIRE;          // either ctrl fires
    case 0x2a: case 0x36: return KEY_RSHIFT;
    case 0x38: case 0x138: return KEY_LALT;
    case 0x39: return KEY_USE;                       // space
    case 0x3a: return KEY_CAPSLOCK;
    // the arrow cluster and the keypad share a code, the cluster's extended:
    // doom reads them alike (doomkeys.h's KEYP_8 IS KEY_UPARROW), so both go
    // to the same key rather than one of them going nowhere.
    case 0x48: case 0x148: return KEY_UPARROW;
    case 0x50: case 0x150: return KEY_DOWNARROW;
    case 0x4b: case 0x14b: return KEY_LEFTARROW;
    case 0x4d: case 0x14d: return KEY_RIGHTARROW;
    case 0x47: case 0x147: return KEY_HOME;
    case 0x4f: case 0x14f: return KEY_END;
    case 0x49: case 0x149: return KEY_PGUP;
    case 0x51: case 0x151: return KEY_PGDN;
    case 0x52: case 0x152: return KEY_INS;
    case 0x53: case 0x153: return KEY_DEL;
    default: break; }
  if (sc >= 0x3b && sc <= 0x44) return (unsigned char) (0x80 + sc);   // F1..F10
  if (sc > 0 && sc < (int) sizeof sc_ascii - 1) return (unsigned char) sc_ascii[sc];
  return 0; }

int DG_GetKey(int *pressed, unsigned char *key) {
  for (;;) {
    int c = k_scan_pop();
    if (c < 0) return 0;
    unsigned char k = sc_key((c & ~0x80) | (c & 0x100));
    if (!k) continue;
    return *pressed = !(c & 0x80), *key = k, 1; } }

// --- the nif --------------------------------------------------------------

static char *dg_argv[] = { (char *) "doom", (char *) "-iwad", (char *) "doom1.wad", 0 };

static void doom_run(void) {
  doomgeneric_Create(3, dg_argv);
  for (;;) doomgeneric_Tick(); }

static lvm(lvm_doom) {
  doom_run();
  Ip += 1;
  ai_musttail return Continue(); }

static union u const nif_doom[] = {{lvm_doom}, {lvm_ret0}};
AiNif("doom", nif_doom);
