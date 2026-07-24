#ifndef _g_cb_h
#define _g_cb_h
#include <stdint.h>
#include <stdarg.h>

// console buffer. each cell is a packed 32-bit value:
//   bits  0..7   character code
//   bits  8..15  foreground colour index
//   bits 16..23  background colour index
//   bits 24..27  font index
//   bits 28..31  face bits (bold / underline / reverse)
// the colour, font, and face fields are plain indices -- cb only stores
// them; the renderer (fbdraw) maps colour indices to pixels, font
// indices to glyph sets, and faces to whatever it can afford. cb_putc
// stamps each cell with the current pen (cur_fg/cur_bg/cur_font), which
// cb_attr sets; a bare character therefore lands with every index 0.
#define cb_cell(ch, fg, bg, fn) ( (uint32_t)(uint8_t)(ch)        \
                                | (uint32_t)(uint8_t)(fg) <<  8  \
                                | (uint32_t)(uint8_t)(bg) << 16  \
                                | (uint32_t)(uint8_t)(fn) << 24 )
#define cb_ch(c)   ((uint8_t)(c))
#define cb_fg(c)   ((uint8_t)((c) >>  8))
#define cb_bg(c)   ((uint8_t)((c) >> 16))
#define cb_font(c) ((uint8_t)(((c) >> 24) & 15))
#define cb_face(c) ((uint8_t)((c) >> 28))

enum {              // face bits, the high nibble of the font byte
  cb_bold = 1, cb_under = 2, cb_rev = 4 };

enum {              // flag bits: the console's modes
  cb_show   = 1,    // cursor visible (DECTCEM ?25) -- renderers read it
  cb_wrap   = 2,    // autowrap (DECAWM ?7)
  cb_lnm    = 4,    // LNM (20): \n implies \r -- the kernel console's discipline
  cb_origin = 8,    // DECOM (?6): rows address relative to the scroll region
  cb_pend   = 16,   // wrap pending: a glyph landed on the last column
  cb_priv   = 32,   // parser transient: the CSI had a DEC '?'/'='/'<' marker
  cb_junk   = 64,   // parser transient: the CSI had intermediates we don't speak
  cb_gt     = 128 };// parser transient: the CSI had the '>' marker (secondary DA)

enum { cb_outn = 64 };  // the reply queue's capacity (cb_reply's buffer size)

struct cb {
  uint32_t rpos, wpos, spos;  // read cursor (the input ring), write cursor, saved cursor
  uint16_t rows, cols, flag, arg;  // arg: the CSI parameter being collected
  uint8_t cur_fg, cur_bg, cur_font, esc;  // the pen; esc: escape-parser state
  uint16_t pv[8]; uint8_t pn;  // pv/pn: collected CSI parameters
  uint8_t def_fg, def_bg, spen[3];  // the reset pen; the saved pen (DECSC)
  uint16_t top, bot;  // the scroll region, inclusive rows
  uint8_t out[cb_outn], on;  // the reply queue (DSR/DA answers ride home here)
  uint32_t ucp; uint8_t un;  // utf-8 in flight: the codepoint, continuations to come
  uint8_t ob[6], ol;  // an OSC body's head: enough to recognize the colour asks
  uint32_t dmg[8];  // dirty rows, one bit each (row 255 stands for 255-and-past);
                    // every grid write marks, a renderer reads-and-clears --
                    // repainting only what moved is what keeps a wide window quick
  uint32_t cb[]; };

void
  cb_open(struct cb*, uint16_t rows, uint16_t cols),
  cb_clear(struct cb*),
  cb_putc(struct cb*, char),
  cb_stamp(struct cb*, uint8_t),
  cb_fill(struct cb*, uint8_t),
  cb_attr(struct cb*, uint8_t fg, uint8_t bg, uint8_t font),
  cb_cur(struct cb*, uint32_t row, uint32_t col);
int
  cb_getc(struct cb*),
  cb_ungetc(struct cb*, int),
  cb_eof(struct cb*),
  cb_reply(struct cb*, uint8_t*);  // drain the reply queue; buf holds cb_outn
uint32_t cb_unfold(uint8_t);       // a glyph byte's codepoint (0 = none)

struct font { uint8_t const *glyphs, w, h; };
extern uint8_t const cga_8x8[256][8], moderndos_8x16[256][16];
#endif
