#include "i.h"
////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(PCore *f, PFile *i) {
  for (int c;;) switch (c = getc(i)) {
    default: return c;
    case '#': case ';': while (!feof(i) && (c = getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static PStatus reads(PCore*, PFile*), read_str_lit(PCore*, PFile*),
  read_atom(PCore*, PFile*);

static PStatus enquote(PCore *f) {
  PPair *w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (PWord) w;
  PSymbol* y = literal_symbol(f, "`");
  if (!y) return Oom;
  w = pairof(f, (PWord) y, f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = (PWord) w;
  return Ok; }


PStatus p_read1f(PCore *f, PFile* i) {
  int c = read_char(f, i);
  switch (c) {
    case EOF: return Eof;
    case '\'': return (c = p_read1f(f, i)) == Ok ? enquote(f) : c;
    case '(': return reads(f, i);
    case ')': return pushs(f, 1, nil) ? Ok : Oom;
    case '"': return read_str_lit(f, i);
    default: return ungetc(c, i), read_atom(f, i); } }

static PStatus reads(PCore *f, PFile* i) {
  PWord c = read_char(f, i);
  if (c == EOF || c == ')') return pushs(f, 1, nil) ? Ok : Oom;
  ungetc(c, i);
  if ((c = p_read1f(f, i)) != Ok) return c;
  if ((c = reads(f, i)) != Ok) return c;
  if (!(c = (PWord) pairof(f, f->sp[1], f->sp[0]))) return Oom;
  *++f->sp = c;
  return Ok; }

// create and grow buffers for reading
static PString* bnew(PCore *f) {
  PString*s = cells(f, Width(PString) + 1);
  return s ? ini_str(s, sizeof(PWord)) : s; }

static PString* bgrow(PCore *f, PString* s) {
  PString* t; uintptr_t len = s->len;
  avec(f, s, t = cells(f, Width(PString) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

static PStatus read_str_lit(PCore *f, PFile* i) {
  PString* b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(PWord); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = fgetc(i)) {
      case '\\': c = fgetc(i); if (c == EOF)
      case '"': case EOF: goto out;
      default: b->text[n++] = c; } out:
  if (!b) return Oom;
  b->len = n;
  return pushs(f, 1, b) ? Ok : Oom; }

static PStatus read_atom(PCore *f, PFile* i) {
  PString* b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(PWord); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = getc(i)) {
      case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
      case '(': case ')': case '"': case '\'': case EOF: ungetc(c, i); goto out;
      default: b->text[n++] = c; } out:
  if (!b) return Oom;
  b->len = n, b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
  char *e; long j = strtol(b->text, &e, 0);
  PWord x = *e == 0 ? putnum(j) : (PWord) intern(f, b);
  return !x || !pushs(f, 1, x) ? Oom : Ok; }

// end of parser
