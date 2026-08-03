// host/cb.c -- quay's console-screen nifs: the terminal emulator's engine.
// crew/quay's struct cb (the VT parser + cell grid, the same C the kernel
// console runs) hosted INSIDE a cask, so the love side owns allocation and
// lifetime (the GC moves and reclaims the screen like any value) and the C
// side stays a pure byte machine re-derived from the cask on every call.
// Self-contained host nif file: auto-globbed + AI_NIF-registered, no
// love.c / main.c edit; quay.c rides along by unity include (it is not
// otherwise linked into the host binary -- only the kernel ports carry it).
//
//   (screen b rows cols) -> b    open a cb over cask b (cb_open)
//                         | n    b not a cask: the byte count a (cask n) needs,
//                                so the ctor is (screen (cask (screen () r c)) r c)
//                         | ()   misuse: cask too small, or silly geometry
//   (scribe scr x)       -> scr  feed x through the VT parser: a byte charm,
//                                or every byte of a string/cask; () misuse
//   (glass scr i)        -> cell the packed 32-bit cell at index i, or ()
//                                (cb_ch/fg/bg/font/face live in the packing)
//   (gaze scr k)         -> n    a field by key: 0 cursor, 1 rows, 2 cols,
//                                3 flag, 4 top, 5 bot; () misuse
//   (reply scr)          -> (b ..) drain the reply queue (DSR/DA answers ride
//                                home to the pty master) as byte charms; () quiet
#include "love.h"
#include <unistd.h>   // read (swig)
#include <fcntl.h>    // O_NONBLOCK (swig)
#include <errno.h>
#include <string.h>   // memcpy (swig's rbuf drain)
#include "../crew/quay/quay.c"
#include "../crew/quay/moderndos_8x16.c"   // the builtin glyphs (host links no font objects)
#include "../crew/quay/cga_8x8.c"

// Re-derive the struct cb from a cask arg, or 0 if it isn't one / doesn't
// hold a sane screen. The cask is OPEN DATA -- the love side can pin any byte
// of it -- so every entry clamps the header fields the C loops trust: a
// scribbled screen may paint garbage, never read or write out of bounds.
static struct cb *scr_ok(ai_word x) {
 if (x & 1 || ((union u*) x)->ap != lvm_buf) return 0;
 struct ai_str *s = ((struct ai_buf*) x)->str;
 if (s->len < sizeof(struct cb)) return 0;
 struct cb *c = (struct cb*) s->bytes;
 uintptr_t n = (uintptr_t) c->rows * c->cols;
 if (!c->rows || !c->cols || sizeof(struct cb) + n * 4 > s->len) return 0;
 if (c->wpos >= n) c->wpos = 0;
 if (c->rpos >= n) c->rpos = 0;
 if (c->spos >= n) c->spos = 0;
 if (c->bot >= c->rows) c->bot = (uint16_t) (c->rows - 1u);
 if (c->top > c->bot) c->top = 0;
 if (c->esc > 8) c->esc = 0;
 if (c->pn > 8) c->pn = 8;
 if (c->on > cb_outn) c->on = 0;
 if (c->un > 3) c->un = 0;
 if (c->ol > 6) c->ol = 6;
 return c; }

// (screen b rows cols): open a screen over cask b. A non-cask b answers the
// byte count the cask needs -- the size protocol that keeps sizeof(struct cb)
// out of the surface. Geometry is capped well under the u32 cell indices.
static lvm(lvm_screen) {
 ai_word b = Sp[0];
 intptr_t r = (Sp[1] & 1) ? getcharm(Sp[1]) : 0,
           k = (Sp[2] & 1) ? getcharm(Sp[2]) : 0;
 ai_word out = ZeroPoint;
 if (r >= 1 && k >= 1 && r <= 65535 && k <= 65535
      && (uintptr_t) r * (uintptr_t) k <= (uintptr_t) 1 << 22) {
  uintptr_t need = sizeof(struct cb) + (uintptr_t) r * (uintptr_t) k * 4;
  if ((b & 1) || ((union u*) b)->ap != lvm_buf) out = putcharm(need);
  else {
   struct ai_str *s = ((struct ai_buf*) b)->str;
   if (s->len >= need) {
    cb_open((struct cb*) s->bytes, (uint16_t) r, (uint16_t) k);
    out = b; } } }
 Sp[2] = out;
 Sp += 2; Ip += 1; ai_musttail return Continue(); }

// (scribe scr x): the feed. A charm is one byte; a string or cask pours every
// byte through cb_putc (the hot path: one nif call per pty read). Returns the
// screen back so feeds chain. No allocation, so the pointer holds throughout.
static lvm(lvm_scribe) {
 struct cb *c = scr_ok(Sp[0]);
 ai_word x = Sp[1], out = ZeroPoint;
 if (c) {
  out = Sp[0];
  if (x & 1) cb_putc(c, (char) (getcharm(x) & 0xff));
  else if (ai_strp(x)) {
   struct ai_str *s = (struct ai_str*) x;
   for (uintptr_t i = 0; i < s->len; i++) cb_putc(c, s->bytes[i]); }
  else if (((union u*) x)->ap == lvm_buf) {
   struct ai_str *s = ((struct ai_buf*) x)->str;
   for (uintptr_t i = 0; i < s->len; i++) cb_putc(c, s->bytes[i]); }
  else out = ZeroPoint; }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

// (glass scr i): look through to one packed cell.
static lvm(lvm_glass) {
 struct cb *c = scr_ok(Sp[0]);
 ai_word out = ZeroPoint;
 if (c && (Sp[1] & 1)) {
  uintptr_t i = (uintptr_t) getcharm(Sp[1]);
  if (i < (uintptr_t) c->rows * c->cols) out = putcharm(c->cb[i]); }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

// (gaze scr k): one header field by key -- no allocation, so a render loop
// polls the cursor for free. 0 cursor, 1 rows, 2 cols, 3 flag, 4 top, 5 bot.
static lvm(lvm_gaze) {
 struct cb *c = scr_ok(Sp[0]);
 ai_word out = ZeroPoint;
 if (c && (Sp[1] & 1)) switch (getcharm(Sp[1])) {
  case 0: out = putcharm(c->wpos); break;
  case 1: out = putcharm(c->rows); break;
  case 2: out = putcharm(c->cols); break;
  case 3: out = putcharm(c->flag); break;
  case 4: out = putcharm(c->top);  break;
  case 5: out = putcharm(c->bot);  break;
  default: break; }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

// the xterm-256 palette, laid once: 16 classics + the 6x6x6 cube + greys.
// same recipe as the kernel's fbdraw palette -- a cell means the same
// pixels on the framebuffer and in a window.
static uint32_t xpal[256];
static void xpal_ini(void) {
 static const uint32_t base[16] = {
  0x000000, 0x800000, 0x008000, 0x808000,
    0x000080, 0x800080, 0x008080, 0xc0c0c0,
    0x808080, 0xff0000, 0x00ff00, 0xffff00,
    0x0000ff, 0xff00ff, 0x00ffff, 0xffffff };
 static const uint8_t cube[6] = { 0, 95, 135, 175, 215, 255 };
 for (int i = 0; i < 16; i++) xpal[i] = base[i];
 for (int i = 0; i < 216; i++) {
  int r = i / 36, g_ = i / 6 % 6, b_ = i % 6;
  xpal[16 + i] = (uint32_t) cube[r] << 16 | (uint32_t) cube[g_] << 8 | cube[b_]; }
 for (int i = 0; i < 24; i++) {
  uint32_t v = 8 + 10u * i;
  xpal[232 + i] = v << 16 | v << 8 | v; } }

// a FONT ATLAS: a cask of [w u8][h u8][0 u16] then 256 glyphs, h scanlines
// each, ceil(w/8) bytes per scanline, MSB the leftmost pixel -- the PSF
// discipline, so console fonts pour straight in. the builtins bake into
// the same shape via (font b k). glyphs up to 16x32.
struct cb_atlas { uint8_t const *g; intptr_t w, h, bpr; };
static int atlas_ok(ai_word x, struct cb_atlas *a) {
 if (x & 1 || ((union u*) x)->ap != lvm_buf) return 0;
 struct ai_str *s = ((struct ai_buf*) x)->str;
 if (s->len < 4) return 0;
 intptr_t w = (uint8_t) s->bytes[0], h = (uint8_t) s->bytes[1];
 intptr_t bpr = (w + 7) / 8;
 if (w < 1 || w > 16 || h < 1 || h > 32) return 0;
 if (s->len < 4 + (uintptr_t) (256 * h * bpr)) return 0;
 a->g = (uint8_t const*) s->bytes + 4, a->w = w, a->h = h, a->bpr = bpr;
 return 1; }

// the pixel core: one cell into a 32bpp little-endian framebuffer through
// an atlas, faces rendered like the kernel's fbdraw (bright bold, swapped
// reverse, underline on the last scanline). caller bounds.
static void cb_px1(uint8_t *base, intptr_t w, uint32_t cell, intptr_t x, intptr_t y,
                   struct cb_atlas const *a) {
 uint8_t g_ = cb_ch(cell), face = cb_face(cell), fgx = cb_fg(cell);
 if (face & cb_bold && fgx < 8) fgx = (uint8_t) (fgx + 8);
 uint32_t fg = xpal[fgx], bg = xpal[cb_bg(cell)];
 if (face & cb_rev) { uint32_t t_ = fg; fg = bg, bg = t_; }
 uint8_t const *bmp = a->g + (intptr_t) g_ * a->h * a->bpr;
 for (intptr_t r = 0; r < a->h; r++) {
  int ul = face & cb_under && r == a->h - 1;
  uint8_t *row = base + ((uintptr_t) (y + r) * (uintptr_t) w + (uintptr_t) x) * 4;
  uint32_t o = bmp[r * a->bpr];
  if (a->bpr > 1) o = o << 8 | bmp[r * a->bpr + 1];
  for (intptr_t k = a->w; k--;) {
   uint32_t px = ul || o >> (a->bpr * 8 - 1 - k) & 1 ? fg : bg;
   row[k * 4] = (uint8_t) px;
   row[k * 4 + 1] = (uint8_t) (px >> 8);
   row[k * 4 + 2] = (uint8_t) (px >> 16);
   row[k * 4 + 3] = 0; } } }

// the builtin faces as a static atlas each, laid on first use
static uint8_t cb_bi0[4 + 256 * 16], cb_bi1[4 + 256 * 8];
static void cb_bi_ini(void) {
 if (cb_bi0[0]) return;
 cb_bi0[0] = 8, cb_bi0[1] = 16;
 for (int i = 0; i < 256 * 16; i++) cb_bi0[4 + i] = moderndos_8x16[i / 16][i % 16];
 cb_bi1[0] = 8, cb_bi1[1] = 8;
 for (int i = 0; i < 256 * 8; i++) cb_bi1[4 + i] = cga_8x8[i / 8][i % 8]; }
// resolve a font arg: a valid atlas cask, or anything else -> builtin 0
static void atlas_of(ai_word x, struct cb_atlas *a) {
 if (atlas_ok(x, a)) return;
 cb_bi_ini();
 a->g = cb_bi0 + 4, a->w = 8, a->h = 16, a->bpr = 1; }

// (font b k): bake builtin k (0 moderndos 8x16, 1 cga 8x8) into cask b as
// an atlas; a non-cask b answers the byte count -- the screen size protocol.
static lvm(lvm_font) {
 ai_word b = Sp[0], out = ZeroPoint;
 intptr_t k = (Sp[1] & 1) ? getcharm(Sp[1]) : -1;
 if (k == 0 || k == 1) {
  uintptr_t need = k == 0 ? sizeof cb_bi0 : sizeof cb_bi1;
  if ((b & 1) || ((union u*) b)->ap != lvm_buf) out = putcharm(need);
  else {
   struct ai_str *s = ((struct ai_buf*) b)->str;
   if (s->len >= need) {
    cb_bi_ini();
    uint8_t const *src = k == 0 ? cb_bi0 : cb_bi1;
    for (uintptr_t i = 0; i < need; i++) s->bytes[i] = (char) src[i];
    out = b; } } }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

// (blit fb wpx cell x y): one cell, bounds-checked; misuse is nothing.
static lvm(lvm_blit) {
 ai_word fb = Sp[0], out = ZeroPoint;
 intptr_t w = (Sp[1] & 1) ? getcharm(Sp[1]) : -1,
           cl = (Sp[2] & 1) ? getcharm(Sp[2]) : -1,
           x = (Sp[3] & 1) ? getcharm(Sp[3]) : -1,
           y = (Sp[4] & 1) ? getcharm(Sp[4]) : -1;
 if (!(fb & 1) && ((union u*) fb)->ap == lvm_buf
      && w > 0 && cl >= 0 && x >= 0 && y >= 0 && x + 8 <= w) {
  struct ai_str *s = ((struct ai_buf*) fb)->str;
  if ((uintptr_t) (y + 16) * (uintptr_t) w * 4 <= s->len) {
   if (!xpal[255]) xpal_ini();
   struct cb_atlas a;
   atlas_of(0, &a);
   cb_px1((uint8_t*) s->bytes, w, (uint32_t) cl, x, y, &a);
   out = fb; } }
 Sp[4] = out;
 Sp += 4; Ip += 1; ai_musttail return Continue(); }

// (blitrow fb wpx scr row curpos): a whole grid row in one call -- the
// painter's hot lane (a keystroke repaints one row, a scroll a bandful,
// and the loop stays in C either way). curpos names the cursor's cell,
// worn in reverse; a non-charm curpos means no cursor on this row.
static lvm(lvm_blitrow) {
 ai_word fb = Sp[0], out = ZeroPoint;
 intptr_t w = (Sp[1] & 1) ? getcharm(Sp[1]) : -1,
           row = (Sp[3] & 1) ? getcharm(Sp[3]) : -1,
           cur = (Sp[4] & 1) ? getcharm(Sp[4]) : -1;
 struct cb *c = scr_ok(Sp[2]);
 struct cb_atlas a;
 atlas_of(Sp[5], &a);
 if (c && !(fb & 1) && ((union u*) fb)->ap == lvm_buf
      && w > 0 && row >= 0 && row < (intptr_t) c->rows) {
  struct ai_str *s = ((struct ai_buf*) fb)->str;
  intptr_t cols = c->cols;
  if (cols * a.w > w) cols = w / a.w;
  if ((uintptr_t) ((row + 1) * a.h) * (uintptr_t) w * 4 <= s->len) {
   if (!xpal[255]) xpal_ini();
   for (intptr_t q = 0; q < cols; q++) {
    uint32_t cell = c->cb[(uintptr_t) row * c->cols + (uintptr_t) q];
    if ((intptr_t) ((uintptr_t) row * c->cols + (uintptr_t) q) == cur)
          cell ^= (uint32_t) cb_rev << 28;
    cb_px1((uint8_t*) s->bytes, w, cell, q * a.w, row * a.h, &a); }
   out = fb; } }
 Sp[5] = out;
 Sp += 5; Ip += 1; ai_musttail return Continue(); }

// (wet scr k): dirty-row bits for rows 32k..32k+31, read-and-cleared --
// the renderer's shopping list. bit 255 stands for row 255 and past.
static lvm(lvm_damage) {
 struct cb *c = scr_ok(Sp[0]);
 ai_word out = ZeroPoint;
 if (c && (Sp[1] & 1)) {
  intptr_t k = getcharm(Sp[1]);
  if (k >= 0 && k < 8) {
   out = putcharm(c->dmg[k]);
   c->dmg[k] = 0; } }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

// gush is GONE: say rides the port vt's writen lane now (love.c lvm_fputs), one
// write(2) stroke for any string/cask at any port -- the fallback dance died
// with it. swig stays: the CHUNK lane a per-byte see can't be.

// (swig port b): drink whatever the fd has waiting into
// cask b, WITHOUT blocking (the caller parks on `see` for the first byte;
// swig drains the rest of the gulp). n bytes read; 0 = nothing waiting or
// eof (the next see tells those apart); a negative charm = -errno / misuse.
static lvm(lvm_swig) {
 ai_word p = Sp[0], x = Sp[1];
 ai_word out = putcharm(-1);
 if (!(p & 1) && ((union u*) p)->ap == lvm_port_io
      && !(x & 1) && ((union u*) x)->ap == lvm_buf) {
  struct ai_io *io = (struct ai_io*) p;
  intptr_t fd = getcharm(io->fd);
  struct ai_str *s = ((struct ai_buf*) x)->str;
    // the port's OWN pending run comes first: a buffered see may have gulped
    // ahead of us, and reading the fd past it would scramble the byte order
  if (s->len && ai_io_pending(g, io)) {
   uintptr_t k = ai_io_read_drain(g, io, (unsigned char*) s->bytes, s->len);
   Sp[1] = putcharm((intptr_t) k);
   Sp += 1; Ip += 1; ai_musttail return Continue(); }
  if (fd >= 0 && s->len) {
   int fl = fcntl((int) fd, F_GETFL);
   fcntl((int) fd, F_SETFL, fl | O_NONBLOCK);
   ssize_t k = read((int) fd, s->bytes, s->len);
   fcntl((int) fd, F_SETFL, fl);
   out = k > 0 ? putcharm(k)
          : k == 0 ? putcharm(0)
          : (errno == EAGAIN || errno == EWOULDBLOCK) ? putcharm(0)
          : putcharm(-errno); } }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

// (unfold g): a cp437 glyph byte's unicode codepoint, 0 when it has none --
// the outward half of the utf-8 fold, for a painter re-emitting the grid
// to a utf-8 terminal (berth caches these per glyph).
static lvm(lvm_unfold) {
 intptr_t g_ = (Sp[0] & 1) ? getcharm(Sp[0]) : -1;
 Sp[0] = (g_ >= 0 && g_ < 256) ? putcharm(cb_unfold((uint8_t) g_)) : ZeroPoint;
 Ip += 1; ai_musttail return Continue(); }

// Workhorse for (reply scr), called with g Packed and the screen at sp[0].
// Drains the queue into a stack buffer FIRST (ai_have may move the cask),
// then builds the byte list tail-first. Returns a not-ok g only on OOM.
ai_noinline static struct ai *host_reply(struct ai *g) {
 struct cb *c = scr_ok(g->sp[0]);
 uint8_t buf[cb_outn];
 int n = c ? cb_reply(c, buf) : 0;
 if (!n) { g->sp[0] = ZeroPoint; return g; }
 if (!ai_ok(g = ai_have(g, (uintptr_t) n * Width(struct ai_chain)))) return g;
 ai_word tail = ZeroPoint;
 for (int i = n; i-- > 0;) {
  struct ai_chain *w = ini_chain((struct ai_chain*) bump(g, Width(struct ai_chain)),
                                   putcharm(buf[i]), tail);
  tail = word(w); }
 g->sp[0] = tail;
 return g; }

// (reply scr): what the terminal wants to say back (DSR position reports, DA).
// The host app forwards these bytes to the pty master; the kernel never asks.
static lvm(lvm_reply) {
 Pack(g);
 g = host_reply(g);
 if (!ai_ok(g)) return ghelp(g);
 Unpack(g);
 Ip += 1; ai_musttail return Continue(); }

static union u const
  nif_screen[] = {{lvm_cur}, {.x = putcharm(3)}, {lvm_screen}, {lvm_ret0}},
  nif_scribe[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_scribe}, {lvm_ret0}},
  nif_glass[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_glass},  {lvm_ret0}},
  nif_gaze[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_gaze},   {lvm_ret0}},
  nif_reply[]  = {{lvm_reply}, {lvm_ret0}},
  nif_unfold[] = {{lvm_unfold}, {lvm_ret0}},
  nif_blit[]   = {{lvm_cur}, {.x = putcharm(5)}, {lvm_blit}, {lvm_ret0}},
  nif_blitrow[] = {{lvm_cur}, {.x = putcharm(6)}, {lvm_blitrow}, {lvm_ret0}},
  nif_font[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_font}, {lvm_ret0}},
  nif_damage[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_damage}, {lvm_ret0}},
  nif_swig[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_swig}, {lvm_ret0}};
AI_NIF("screen", nif_screen);
AI_NIF("scribe", nif_scribe);
AI_NIF("glass", nif_glass);
AI_NIF("gaze", nif_gaze);
AI_NIF("reply", nif_reply);
AI_NIF("unfold", nif_unfold);
AI_NIF("blit", nif_blit);
AI_NIF("blitrow", nif_blitrow);
AI_NIF("font", nif_font);
AI_NIF("wet", nif_damage);
AI_NIF("swig", nif_swig);
