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

// Re-derive the struct cb from a cask arg, or 0 if it isn't one / doesn't
// hold a sane screen. The cask is OPEN DATA -- the love side can pin any byte
// of it -- so every entry clamps the header fields the C loops trust: a
// scribbled screen may paint garbage, never read or write out of bounds.
static struct cb *scr_ok(ai_word x) {
 if (x & 1 || ((union u*) x)->ap != lvm_cask) return 0;
 struct ai_str *s = ((struct ai_cask*) x)->str;
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
  if ((b & 1) || ((union u*) b)->ap != lvm_cask) out = putcharm(need);
  else {
   struct ai_str *s = ((struct ai_cask*) b)->str;
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
  else if (((union u*) x)->ap == lvm_cask) {
   struct ai_str *s = ((struct ai_cask*) x)->str;
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
      && !(x & 1) && ((union u*) x)->ap == lvm_cask) {
  struct ai_io *io = (struct ai_io*) p;
  intptr_t fd = ai_io_fd(io);
  struct ai_str *s = ((struct ai_cask*) x)->str;
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
  nif_damage[] = {{lvm_cur}, {.x = putcharm(2)}, {lvm_damage}, {lvm_ret0}},
  nif_swig[]   = {{lvm_cur}, {.x = putcharm(2)}, {lvm_swig}, {lvm_ret0}};
AI_NIF("screen", nif_screen);
AI_NIF("scribe", nif_scribe);
AI_NIF("glass", nif_glass);
AI_NIF("gaze", nif_gaze);
AI_NIF("reply", nif_reply);
AI_NIF("unfold", nif_unfold);
AI_NIF("wet", nif_damage);
AI_NIF("swig", nif_swig);
