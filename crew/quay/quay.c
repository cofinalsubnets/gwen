#include "love.h"
#include "quay.h"

// the blank a clear or scroll leaves behind: char 0 in the current pen,
// so erased ground keeps the program's background (BCE).
static uint32_t cb_blank(struct cb *c) {
  return cb_cell(0, c->cur_fg, c->cur_bg, c->cur_font); }

// mark rows r1..r2 dirty (inclusive; rows past 255 fold onto bit 255).
static void cb_dirt(struct cb *c, uint32_t r1, uint32_t r2) {
  if (r1 > 255) r1 = 255;
  if (r2 > 255) r2 = 255;
  for (uint32_t r = r1; r <= r2; r++) c->dmg[r >> 5] |= (uint32_t) 1 << (r & 31); }

void cb_fill(struct cb *c, uint8_t _) {
  uint32_t cell = cb_cell(_, c->cur_fg, c->cur_bg, c->cur_font);
  for (uint32_t i = 0, j = (uint32_t) c->rows * c->cols; i < j; i++)
    c->cb[i] = cell;
  cb_dirt(c, 0, c->rows - 1u); }

void cb_clear(struct cb *c) { cb_fill(c, 0); }

void cb_cur(struct cb *c, uint32_t row, uint32_t col) {
  c->wpos = (row * c->cols + col) % ((uint32_t) c->rows * c->cols); }

void cb_attr(struct cb *c, uint8_t fg, uint8_t bg, uint8_t font) {
  c->cur_fg = c->def_fg = fg, c->cur_bg = c->def_bg = bg, c->cur_font = font; }

// stamp a raw glyph with the current pen and step the cursor, no
// interpretation -- the escape-free lane for callers (the playdate's
// progress fill) that mean every byte as a picture.
void cb_stamp(struct cb *c, uint8_t i) {
  uint32_t r = c->wpos / c->cols;
  c->cb[c->wpos] = cb_cell(i, c->cur_fg, c->cur_bg, c->cur_font);
  cb_dirt(c, r, r);
  if (++c->wpos == (uint32_t) c->rows * c->cols) c->wpos = 0; }

void cb_open(struct cb *c, uint16_t rows, uint16_t cols) {
  c->rpos = c->wpos = c->spos = 0;
  c->rows = rows, c->cols = cols;
  c->flag = cb_show | cb_wrap;
  c->arg = 0, c->esc = 0, c->pn = 0, c->on = 0;
  c->ucp = 0, c->un = 0, c->ol = 0;
  for (int k = 0; k < 8; k++) c->dmg[k] = 0;
  c->cur_fg = c->def_fg = c->spen[0] = 7;
  c->cur_bg = c->def_bg = c->spen[1] = 0;
  c->cur_font = c->spen[2] = 0;
  c->top = 0, c->bot = rows - 1u;
  cb_clear(c); }

// scroll rows [t,b] up/down by n, blanking what opens. the trailing
// ring reader and the saved cursor RIDE ALONG when their cells move --
// the old scroll left them pointing at shifted ground.
static void cb_ride(struct cb *c, uint32_t *p, uint32_t t, uint32_t b, int dn, uint32_t n) {
  uint32_t r = *p / c->cols, k = n * c->cols;
  if (r < t || r > b) return;
  if (dn < 0) { if (r >= t + n) *p -= k; }
  else        { if (r + n <= b) *p += k; } }

static void cb_scup(struct cb *c, uint32_t t, uint32_t b, uint32_t n) {
  if (!n) return;
  if (n > b - t + 1u) n = b - t + 1u;
  uint32_t cs = c->cols, e = cb_blank(c);
  for (uint32_t i = t * cs, j = (b + 1u - n) * cs; i < j; i++) c->cb[i] = c->cb[i + n * cs];
  for (uint32_t i = (b + 1u - n) * cs, j = (b + 1u) * cs; i < j; i++) c->cb[i] = e;
  cb_dirt(c, t, b);
  cb_ride(c, &c->rpos, t, b, -1, n), cb_ride(c, &c->spos, t, b, -1, n); }

static void cb_scdn(struct cb *c, uint32_t t, uint32_t b, uint32_t n) {
  if (!n) return;
  if (n > b - t + 1u) n = b - t + 1u;
  uint32_t cs = c->cols, e = cb_blank(c);
  for (uint32_t i = (b + 1u) * cs; i-- > (t + n) * cs;) c->cb[i] = c->cb[i - n * cs];
  for (uint32_t i = t * cs, j = (t + n) * cs; i < j; i++) c->cb[i] = e;
  cb_dirt(c, t, b);
  cb_ride(c, &c->rpos, t, b, +1, n), cb_ride(c, &c->spos, t, b, +1, n); }

// index: down one row, scrolling at the region's bottom margin. a
// cursor below the region (possible after DECSTBM) steps to the screen
// edge and stops.
static void cb_ind(struct cb *c) {
  uint32_t r = c->wpos / c->cols;
  c->flag &= (uint16_t) ~cb_pend;
  if (r == c->bot) cb_scup(c, c->top, c->bot, 1);
  else if (r + 1u < c->rows) c->wpos += c->cols; }

static void cb_ri(struct cb *c) {  // reverse index: the mirror
  uint32_t r = c->wpos / c->cols;
  c->flag &= (uint16_t) ~cb_pend;
  if (r == c->top) cb_scdn(c, c->top, c->bot, 1);
  else if (r) c->wpos -= c->cols; }

// absolute cursor addressing; origin mode reads rows against the
// scroll region. every deliberate move drops a pending wrap.
static void cb_goto(struct cb *c, uint32_t r, uint32_t col) {
  uint32_t lo = 0, hi = c->rows - 1u;
  if (c->flag & cb_origin) r += c->top, lo = c->top, hi = c->bot;
  if (r < lo) r = lo;
  if (r > hi) r = hi;
  if (col >= c->cols) col = c->cols - 1u;
  c->wpos = r * c->cols + col, c->flag &= (uint16_t) ~cb_pend; }

// the reply queue: answers (DSR, DA) ride home to whoever feeds the
// parser; the host drains them to the pty master, the kernel may not
// bother. a full queue drops the tail -- a late report beats a torn one.
static void cb_say(struct cb *c, char const *s) {
  while (*s && c->on < cb_outn) c->out[c->on++] = (uint8_t) *s++; }

static void cb_sayn(struct cb *c, uint32_t n) {
  char b[8]; int i = 8;
  do b[--i] = (char) ('0' + n % 10u), n /= 10u; while (n);
  while (i < 8 && c->on < cb_outn) c->out[c->on++] = (uint8_t) b[i++]; }

int cb_reply(struct cb *c, uint8_t *buf) {
  int n = c->on;
  for (int i = 0; i < n; i++) buf[i] = c->out[i];
  return c->on = 0, n; }

// the OSC asks worth answering: colour queries (ESC]10;? fg, ESC]11;? bg)
// -- zsh and friends probe the background at line-editor startup and WAIT;
// silence costs the user a second of buffered keystrokes. the answers are
// the default pen's truth on every renderer: c0c0c0 on black.
static void cb_oscq(struct cb *c) {
  if (c->ol == 4 && c->ob[0] == '1' && c->ob[2] == ';' && c->ob[3] == '?') {
    if (c->ob[1] == '0') cb_say(c, "\033]10;rgb:c0c0/c0c0/c0c0\a");
    else if (c->ob[1] == '1') cb_say(c, "\033]11;rgb:0000/0000/0000\a"); } }

// C0 controls, live in ground state and mid-CSI alike. \n implies \r
// only under LNM (the kernel console's discipline); a raw feed keeps
// the column, which is what a pty's ONLCR-translated stream expects.
static void cb_ctl(struct cb *c, uint8_t i) {
  uint32_t cs = c->cols, col = c->wpos % cs;
  switch (i) {
   case '\r': c->wpos -= col, c->flag &= (uint16_t) ~cb_pend; return;
   case '\b': if (col && c->wpos != c->rpos) c->wpos--;
              c->flag &= (uint16_t) ~cb_pend; return;
   case '\n': case 11: case 12:  // LF VT FF
    if (c->flag & cb_lnm) c->wpos -= col;
    c->flag &= (uint16_t) ~cb_pend;
    return cb_ind(c);
   case '\t': { uint32_t nx = (col / 8u + 1u) * 8u;
    if (nx > cs - 1u) nx = cs - 1u;
    c->wpos += nx - col; return; }
   default: return; } }  // BEL and the rest of C0: swallowed whole

// a printing glyph. a pending wrap fires FIRST (deferred autowrap: the
// glyph that landed on the last column left the cursor there; the next
// one carries it to a fresh line), then the stamp, then the step -- a
// stamp on the last column pends rather than moving, or overwrites in
// place with autowrap off.
static void cb_glyph(struct cb *c, uint8_t i) {
  uint32_t cs = c->cols;
  if (c->flag & cb_pend)
    c->flag &= (uint16_t) ~cb_pend, c->wpos -= c->wpos % cs, cb_ind(c);
  c->cb[c->wpos] = cb_cell(i, c->cur_fg, c->cur_bg, c->cur_font);
  cb_dirt(c, c->wpos / cs, c->wpos / cs);
  if (c->wpos % cs == cs - 1u) { if (c->flag & cb_wrap) c->flag |= cb_pend; }
  else c->wpos++; }

// RIS: everything back to the floor -- pens, faces, region, modes,
// cursor, ground. LNM survives: the newline discipline belongs to the
// console (the kernel set it at boot), not to the program resetting.
static void cb_ris(struct cb *c) {
  uint16_t lnm = c->flag & cb_lnm;
  c->cur_fg = c->def_fg, c->cur_bg = c->def_bg, c->cur_font = 0;
  c->top = 0, c->bot = c->rows - 1u;
  c->flag = (uint16_t) (cb_show | cb_wrap | lnm);
  c->rpos = c->wpos = c->spos = 0;
  c->esc = 0, c->pn = 0, c->arg = 0, c->on = 0, c->un = 0, c->ol = 0;
  cb_clear(c); }

static void cb_save(struct cb *c) {  // DECSC: cursor + pen
  c->spos = c->wpos;
  c->spen[0] = c->cur_fg, c->spen[1] = c->cur_bg, c->spen[2] = c->cur_font; }

static void cb_restore(struct cb *c) {  // DECRC
  c->wpos = c->spos, c->flag &= (uint16_t) ~cb_pend;
  c->cur_fg = c->spen[0], c->cur_bg = c->spen[1], c->cur_font = c->spen[2]; }

// SGR: the pen. colours by index (8 + bright 8 + 256), faces in the
// font byte's high nibble; 38;2 truecolour quantizes onto the 6x6x6
// cube rather than lying about a palette cb doesn't carry.
static void cb_sgr(struct cb *c) {
  for (uint8_t k = 0; k < c->pn; k++) {
    uint16_t p = c->pv[k];
    if (p == 0) c->cur_fg = c->def_fg, c->cur_bg = c->def_bg, c->cur_font &= 15;
    else if (p == 1) c->cur_font |= cb_bold << 4;
    else if (p == 4) c->cur_font |= cb_under << 4;
    else if (p == 7) c->cur_font |= cb_rev << 4;
    else if (p == 22) c->cur_font &= (uint8_t) ~(cb_bold << 4);
    else if (p == 24) c->cur_font &= (uint8_t) ~(cb_under << 4);
    else if (p == 27) c->cur_font &= (uint8_t) ~(cb_rev << 4);
    else if (p >= 30 && p <= 37) c->cur_fg = (uint8_t) (p - 30);
    else if (p == 39) c->cur_fg = c->def_fg;
    else if (p >= 40 && p <= 47) c->cur_bg = (uint8_t) (p - 40);
    else if (p == 49) c->cur_bg = c->def_bg;
    else if (p >= 90 && p <= 97) c->cur_fg = (uint8_t) (p - 90 + 8);
    else if (p >= 100 && p <= 107) c->cur_bg = (uint8_t) (p - 100 + 8);
    else if ((p == 38 || p == 48) && k + 2 < c->pn && c->pv[k + 1] == 5) {
      uint8_t v = (uint8_t) c->pv[k + 2];
      if (p == 38) c->cur_fg = v; else c->cur_bg = v;
      k += 2; }
    else if ((p == 38 || p == 48) && k + 4 < c->pn && c->pv[k + 1] == 2) {
      uint8_t v = (uint8_t) (16 + 36 * (c->pv[k + 2] / 51) + 6 * (c->pv[k + 3] / 51)
                                + c->pv[k + 4] / 51);
      if (p == 38) c->cur_fg = v; else c->cur_bg = v;
      k += 4; } } }

// DEC private / ANSI modes (CSI ? .. h/l and CSI .. h/l). the alternate
// screen (47/1047/1049) is save-and-clear / clear-and-restore over the
// ONE grid cb carries -- a full-screen program looks right; the ground
// it painted over is gone, the honest price of one buffer.
static void cb_mode(struct cb *c, int priv, int on) {
  for (uint8_t k = 0; k < c->pn; k++) {
    uint16_t p = c->pv[k];
    if (!priv) {
      if (p == 20) c->flag = on ? c->flag | cb_lnm : c->flag & (uint16_t) ~cb_lnm; }
    else if (p == 7) c->flag = on ? c->flag | cb_wrap : c->flag & (uint16_t) ~cb_wrap;
    else if (p == 25) c->flag = on ? c->flag | cb_show : c->flag & (uint16_t) ~cb_show;
    else if (p == 6) {
      c->flag = on ? c->flag | cb_origin : c->flag & (uint16_t) ~cb_origin;
      cb_goto(c, 0, 0); }
    else if (p == 47 || p == 1047 || p == 1049) {
      if (on) cb_save(c), cb_clear(c), c->wpos = 0, c->flag &= (uint16_t) ~cb_pend;
      else cb_clear(c), cb_restore(c); } } }

// the CSI dispatch, one final byte at a time. n/m: the first two
// parameters with their traditional default of 1.
static void cb_csi(struct cb *c, uint8_t i) {
  uint32_t n = c->pv[0] ? c->pv[0] : 1, m = c->pn > 1 && c->pv[1] ? c->pv[1] : 1;
  uint32_t cs = c->cols, r = c->wpos / cs, col = c->wpos % cs;
  uint32_t rb = r * cs, re = rb + cs;  // this row's span
  int priv = c->flag & cb_priv, gt = c->flag & cb_gt;
  c->flag &= (uint16_t) ~(cb_priv | cb_gt);
  switch (i) {
   case 'A': { uint32_t lo = r >= c->top ? c->top : 0;
    cb_cur(c, r > lo + n ? r - n : lo, col), c->flag &= (uint16_t) ~cb_pend; return; }
   case 'B': case 'e': { uint32_t hi = r <= c->bot ? c->bot : c->rows - 1u;
    cb_cur(c, r + n < hi ? r + n : hi, col), c->flag &= (uint16_t) ~cb_pend; return; }
   case 'C': c->wpos = col + n < cs ? c->wpos + n : re - 1u;
    c->flag &= (uint16_t) ~cb_pend; return;
   case 'D': c->wpos = col > n ? c->wpos - n : rb;
    c->flag &= (uint16_t) ~cb_pend; return;
   case 'E': { uint32_t hi = r <= c->bot ? c->bot : c->rows - 1u;
    cb_cur(c, r + n < hi ? r + n : hi, 0), c->flag &= (uint16_t) ~cb_pend; return; }
   case 'F': { uint32_t lo = r >= c->top ? c->top : 0;
    cb_cur(c, r > lo + n ? r - n : lo, 0), c->flag &= (uint16_t) ~cb_pend; return; }
   case 'G': case '`': c->wpos = rb + (n - 1u < cs ? n - 1u : cs - 1u);
    c->flag &= (uint16_t) ~cb_pend; return;
   case 'd': return cb_goto(c, n - 1u, col);
   case 'H': case 'f': return cb_goto(c, n - 1u, m - 1u);
   case 'J': { uint32_t e = cb_blank(c), all = (uint32_t) c->rows * cs;
    uint32_t lo = c->pv[0] == 1 ? 0 : c->wpos, hi = c->pv[0] == 1 ? c->wpos + 1u : all;
    if (c->pv[0] >= 2) lo = 0, hi = all;
    for (uint32_t p = lo; p < hi; p++) c->cb[p] = e;
    if (hi > lo) cb_dirt(c, lo / cs, (hi - 1u) / cs);
    return; }
   case 'K': { uint32_t e = cb_blank(c);
    uint32_t lo = c->pv[0] == 1 ? rb : c->wpos, hi = c->pv[0] == 1 ? c->wpos + 1u : re;
    if (c->pv[0] >= 2) lo = rb, hi = re;
    for (uint32_t p = lo; p < hi; p++) c->cb[p] = e;
    cb_dirt(c, r, r);
    return; }
   case 'L': if (r >= c->top && r <= c->bot) cb_scdn(c, r, c->bot, n); return;
   case 'M': if (r >= c->top && r <= c->bot) cb_scup(c, r, c->bot, n); return;
   case '@': { if (n > cs - col) n = cs - col;
    uint32_t e = cb_blank(c);
    for (uint32_t p = re; p-- > c->wpos + n;) c->cb[p] = c->cb[p - n];
    for (uint32_t p = c->wpos, j = c->wpos + n; p < j; p++) c->cb[p] = e;
    cb_dirt(c, r, r);
    return; }
   case 'P': { if (n > cs - col) n = cs - col;
    uint32_t e = cb_blank(c);
    for (uint32_t p = c->wpos; p < re - n; p++) c->cb[p] = c->cb[p + n];
    for (uint32_t p = re - n; p < re; p++) c->cb[p] = e;
    cb_dirt(c, r, r);
    return; }
   case 'X': { if (n > cs - col) n = cs - col;
    uint32_t e = cb_blank(c);
    for (uint32_t p = c->wpos, j = c->wpos + n; p < j; p++) c->cb[p] = e;
    cb_dirt(c, r, r);
    return; }
   case 'S': return cb_scup(c, c->top, c->bot, n);
   case 'T': return cb_scdn(c, c->top, c->bot, n);
   case 'r': if (!priv) {
     uint32_t t = c->pv[0] ? c->pv[0] : 1, b = c->pn > 1 && c->pv[1] ? c->pv[1] : c->rows;
     if (t < b && b <= c->rows) c->top = (uint16_t) (t - 1u), c->bot = (uint16_t) (b - 1u), cb_goto(c, 0, 0); }
    return;
   case 'm': return cb_sgr(c);
   case 'h': return cb_mode(c, priv, 1);
   case 'l': return cb_mode(c, priv, 0);
   case 'n':
    if (c->pv[0] == 6) cb_say(c, "\033["), cb_sayn(c, r + 1u), cb_say(c, ";"),
                       cb_sayn(c, col + 1u), cb_say(c, "R");
    else if (c->pv[0] == 5) cb_say(c, "\033[0n");
    return;
   case 'c':                                // DA: a VT102, honestly; >c the secondary ask
    return cb_say(c, gt ? "\033[>0;0;0c" : "\033[?6c");
   case 's': return cb_save(c);
   case 'u': return cb_restore(c);
   default: return; } }  // anything else: politely nothing

// cb_put1 interprets a working VT subset, one folded byte at a time: C0
// controls (with LNM ruling \n), ESC 7/8/D/E/M/c/#8, CSI cursor addressing
// (A-H, f, G, d, E, F), erase (J/K 0-2, X), edit (@ P L M), scroll (S T,
// DECSTBM r), SGR colours + faces, DEC modes (autowrap, cursor, origin,
// the one-grid alternate screen), and DSR/DA replies via the reply queue.
// OSC/DCS bodies are swallowed whole; charset designators too. anything
// printable is stamped as a glyph with the current pen.
static void cb_put1(struct cb *c, uint8_t i) {
  switch (c->esc) {
   case 1:                                  // after ESC
    c->esc = 0;
    switch (i) {
     case '[': c->esc = 2, c->arg = 0, c->pn = 0;
      c->flag &= (uint16_t) ~(cb_priv | cb_junk | cb_gt); return;
     case ']': c->esc = 7, c->ol = 0; return;       // OSC: capture the head (colour asks answer)
     case 'P': case '^': case '_': c->esc = 3; return;  // DCS/PM/APC: swallow
     case '(': case ')': case '*': case '+': c->esc = 4; return;  // charset designator
     case '#': c->esc = 6; return;
     case '7': return cb_save(c);           // DECSC
     case '8': return cb_restore(c);        // DECRC
     case 'D': return cb_ind(c);            // IND
     case 'E': c->wpos -= c->wpos % c->cols; return cb_ind(c);  // NEL
     case 'M': return cb_ri(c);             // RI
     case 'c': return cb_ris(c);            // RIS
     case 'Z': return cb_say(c, "\033[?6c");
     default: return; }                     // '=' '>' and friends: nothing to keep
   case 2:                                  // within CSI ESC [ ...
    if (i == 27) { c->esc = 1; return; }    // a fresh ESC abandons the sequence
    if (i == 127) return;                   // DEL: nothing, anywhere
    if (i < ' ') return cb_ctl(c, i);       // C0 controls run even mid-sequence
    if (i >= '0' && i <= '9') {
      if (c->arg < 6553) c->arg = (uint16_t) (c->arg * 10 + (i - '0'));
      return; }
    if (i == ';' || i == ':') {              // ':' -- colon-form SGR subparameters
      if (c->pn < 8) c->pv[c->pn++] = c->arg;
      c->arg = 0;
      return; }
    if (i == '>') { c->flag |= cb_gt; return; }     // the secondary-DA marker
    if (i == '?' || i == '=' || i == '<') { c->flag |= cb_priv; return; }
    if (i <= '/') { c->flag |= cb_junk; return; }  // intermediates we don't speak
    if (c->pn < 8) c->pv[c->pn++] = c->arg;        // the final parameter
    c->esc = 0;
    if (c->flag & cb_junk) { c->flag &= (uint16_t) ~(cb_junk | cb_priv | cb_gt); return; }
    return cb_csi(c, i);
   case 3:                                  // a DCS/PM/APC body on its way to ST
    if (i == 7) c->esc = 0;
    else if (i == 27) c->esc = 5;
    return;
   case 5: c->esc = 0; return;              // the byte after ESC ends it (ST's backslash)
   case 7:                                  // an OSC body: head captured for the colour asks
    if (i == 7) { c->esc = 0; return cb_oscq(c); }
    if (i == 27) { c->esc = 8; return; }
    if (c->ol < 5) c->ob[c->ol++] = i; else c->ol = 6;
    return;
   case 8:                                  // OSC's ST tail
    c->esc = 0;
    if (i == '\\') return cb_oscq(c);
    return;
   case 4: c->esc = 0; return;              // the designated charset: discarded
   case 6:                                  // ESC # ...
    if (i == '8') {                         // DECALN: the E screen, region home
      c->top = 0, c->bot = c->rows - 1u, c->wpos = 0, c->flag &= (uint16_t) ~cb_pend;
      cb_fill(c, 'E'); }
    c->esc = 0;
    return;
   default:
    if (i == 27) { c->esc = 1; return; }    // ESC: begin a sequence
    if (i == 127) return;                   // DEL: nothing, anywhere
    if (i < ' ') return cb_ctl(c, i);
    return cb_glyph(c, i); } }

// the utf-8 fold: what codepoints land on which cp437 glyph -- the whole
// classic page (accents, box drawing, blocks, greek, math) plus aliases
// that MEAN one of ours (rounded and heavy box corners onto the singles,
// typographer's dashes and quotes onto the plain ones). anything else,
// astral planes included, wears the ■.
static const uint16_t cb_umap[][2] = {
    {0x263a,0x01},{0x263b,0x02},{0x2665,0x03},{0x2666,0x04},{0x2663,0x05},{0x2660,0x06},
    {0x2022,0x07},{0x25d8,0x08},{0x25cb,0x09},{0x25d9,0x0a},{0x2642,0x0b},{0x2640,0x0c},
    {0x266a,0x0d},{0x266b,0x0e},{0x263c,0x0f},{0x25ba,0x10},{0x25c4,0x11},{0x2195,0x12},
    {0x203c,0x13},{0x00b6,0x14},{0x00a7,0x15},{0x25ac,0x16},{0x21a8,0x17},{0x2191,0x18},
    {0x2193,0x19},{0x2192,0x1a},{0x2190,0x1b},{0x221f,0x1c},{0x2194,0x1d},{0x25b2,0x1e},
    {0x25bc,0x1f},{0x2302,0x7f},
    {0x00c7,0x80},{0x00fc,0x81},{0x00e9,0x82},{0x00e2,0x83},{0x00e4,0x84},{0x00e0,0x85},
    {0x00e5,0x86},{0x00e7,0x87},{0x00ea,0x88},{0x00eb,0x89},{0x00e8,0x8a},{0x00ef,0x8b},
    {0x00ee,0x8c},{0x00ec,0x8d},{0x00c4,0x8e},{0x00c5,0x8f},{0x00c9,0x90},{0x00e6,0x91},
    {0x00c6,0x92},{0x00f4,0x93},{0x00f6,0x94},{0x00f2,0x95},{0x00fb,0x96},{0x00f9,0x97},
    {0x00ff,0x98},{0x00d6,0x99},{0x00dc,0x9a},{0x00a2,0x9b},{0x00a3,0x9c},{0x00a5,0x9d},
    {0x20a7,0x9e},{0x0192,0x9f},
    {0x00e1,0xa0},{0x00ed,0xa1},{0x00f3,0xa2},{0x00fa,0xa3},{0x00f1,0xa4},{0x00d1,0xa5},
    {0x00aa,0xa6},{0x00ba,0xa7},{0x00bf,0xa8},{0x2310,0xa9},{0x00ac,0xaa},{0x00bd,0xab},
    {0x00bc,0xac},{0x00a1,0xad},{0x00ab,0xae},{0x00bb,0xaf},
    {0x2591,0xb0},{0x2592,0xb1},{0x2593,0xb2},{0x2502,0xb3},{0x2524,0xb4},{0x2561,0xb5},
    {0x2562,0xb6},{0x2556,0xb7},{0x2555,0xb8},{0x2563,0xb9},{0x2551,0xba},{0x2557,0xbb},
    {0x255d,0xbc},{0x255c,0xbd},{0x255b,0xbe},{0x2510,0xbf},{0x2514,0xc0},{0x2534,0xc1},
    {0x252c,0xc2},{0x251c,0xc3},{0x2500,0xc4},{0x253c,0xc5},{0x255e,0xc6},{0x255f,0xc7},
    {0x255a,0xc8},{0x2554,0xc9},{0x2569,0xca},{0x2566,0xcb},{0x2560,0xcc},{0x2550,0xcd},
    {0x256c,0xce},{0x2567,0xcf},{0x2568,0xd0},{0x2564,0xd1},{0x2565,0xd2},{0x2559,0xd3},
    {0x2558,0xd4},{0x2552,0xd5},{0x2553,0xd6},{0x256b,0xd7},{0x256a,0xd8},{0x2518,0xd9},
    {0x250c,0xda},{0x2588,0xdb},{0x2584,0xdc},{0x258c,0xdd},{0x2590,0xde},{0x2580,0xdf},
    {0x03b1,0xe0},{0x00df,0xe1},{0x0393,0xe2},{0x03c0,0xe3},{0x03a3,0xe4},{0x03c3,0xe5},
    {0x00b5,0xe6},{0x03c4,0xe7},{0x03a6,0xe8},{0x0398,0xe9},{0x03a9,0xea},{0x03b4,0xeb},
    {0x221e,0xec},{0x03c6,0xed},{0x03b5,0xee},{0x2229,0xef},{0x2261,0xf0},{0x00b1,0xf1},
    {0x2265,0xf2},{0x2264,0xf3},{0x2320,0xf4},{0x2321,0xf5},{0x00f7,0xf6},{0x2248,0xf7},
    {0x00b0,0xf8},{0x2219,0xf9},{0x00b7,0xfa},{0x221a,0xfb},{0x207f,0xfc},{0x00b2,0xfd},
    {0x25a0,0xfe},{0x00a0,0xff},
    // the aliases: things that MEAN one of ours
    {0x256d,0xda},{0x256e,0xbf},{0x256f,0xd9},{0x2570,0xc0},                // rounded corners
    {0x2501,0xc4},{0x2503,0xb3},{0x250f,0xda},{0x2513,0xbf},{0x2517,0xc0}, // heavy lines
    {0x251b,0xd9},{0x2523,0xc3},{0x252b,0xb4},{0x2533,0xc2},{0x253b,0xc1},{0x254b,0xc5},
    {0x2571,'/'},{0x2572,'\\'},{0x25cf,0x07},{0x25aa,0xfe},
    {0x2581,0xdc},{0x2582,0xdc},{0x2583,0xdc},{0x2585,0xdb},{0x2586,0xdb},{0x2587,0xdb},
    {0x2013,'-'},{0x2014,'-'},{0x2018,'\''},{0x2019,'\''},{0x201c,'"'},{0x201d,'"'},
    {0x2026,'.'} };

static uint8_t cb_fold(uint32_t cp) {
  if (cp > 0xffff) return 0xfe;
  for (uintptr_t k = 0; k < sizeof cb_umap / sizeof *cb_umap; k++)
    if (cb_umap[k][0] == cp) return (uint8_t) cb_umap[k][1];
  return 0xfe; }

// the reverse fold, for a painter that re-emits the grid to a utf-8
// terminal: a glyph byte's codepoint. the canonical rows come before the
// aliases, so the first hit is the true face; 0 = no meaning (ascii and
// the blank never ask).
uint32_t cb_unfold(uint8_t g) {
  for (uintptr_t k = 0; k < sizeof cb_umap / sizeof *cb_umap; k++)
    if (cb_umap[k][1] == g) return cb_umap[k][0];
  return 0; }

// cb_putc: the byte door. utf-8 folds here -- a lead opens a sequence
// (state rides in ucp/un), continuations accumulate, the finished
// codepoint folds onto its cp437 glyph and goes through cb_put1 like any
// byte. a torn sequence lands a ■ and the tearing byte rides on; raw
// cp437 art goes through cb_stamp, which never interprets.
void cb_putc(struct cb *c, char _i) {
  uint8_t i = (uint8_t) _i;
  if (c->un) {                                   // continuations expected
    if (i >= 0x80 && i < 0xc0) {
      c->ucp = c->ucp << 6 | (i & 0x3fu);
      if (--c->un) return;
      return cb_put1(c, cb_fold(c->ucp)); }      // complete: fold and deliver
    c->un = 0;                                   // torn: a ■, and i rides on
    cb_put1(c, 0xfe); }
  if (i >= 0x80) {
    if (i >= 0xc2 && i < 0xe0) { c->un = 1, c->ucp = i & 0x1fu; return; }
    if (i >= 0xe0 && i < 0xf0) { c->un = 2, c->ucp = i & 0x0fu; return; }
    if (i >= 0xf0 && i < 0xf5) { c->un = 3, c->ucp = i & 0x07u; return; }
    i = 0xfe; }                                  // stray continuation / bad lead
  cb_put1(c, i); }

int cb_ungetc(struct cb *c, int i) {
  uint32_t r = c->rpos;
  r = r > 0 ? r - 1 : (uint32_t) c->rows * c->cols - 1;
  if (r == c->wpos) return -1;
  c->rpos = r;
  // rewind one cell and replace its char, keeping the cell's colour/font
  c->cb[r] = (c->cb[r] & ~(uint32_t) 0xff) | (uint8_t) i;
  cb_dirt(c, r / c->cols, r / c->cols);
  return i; }

int cb_eof(struct cb *c) {
  return c->rpos == c->wpos; }

int cb_getc(struct cb *c) {
  if (c->rpos == c->wpos) return -1;
  int i = cb_ch(c->cb[c->rpos]);
  if (++c->rpos == (uint32_t) c->rows * c->cols) c->rpos = 0;
  return i; }
