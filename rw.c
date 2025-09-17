#include "i.h"

typedef struct g_input {
  int (*getc)(struct g_input*),
      (*ungetc)(struct g_input*, int),
      (*eof)(struct g_input*); } input;

static Inline int p_in_getc(input *i) { return i->getc(i); }
static Inline int p_in_ungetc(input *i, int c) { return i->ungetc(i, c); }
static Inline int p_in_eof(input *i) { return i->eof(i); }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(core *f, input *i) {
  for (int c;;) switch (c = p_in_getc(i)) {
    default: return c;
    case '#': case ';': while (!p_in_eof(i) && (c = p_in_getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static g_core
  *g_readsi(core*, input*),
  *g_read1i(g_core*, input*),
  *read_string(core*, input*, char),
  *read_atom(core*, input*);

static g_core *g_read1i(core *f, input* i) {
  if (!g_ok(f)) return f;
  int c = read_char(f, i);
  switch (c) {
    case EOF:  return encode(f, Eof);
    case '\'': return f = g_push(f, 1, f->quote),
                      f = g_read1i(f, i),
                      f = g_push(f, 1, nil),
                      f = g_cons_r(f),
                      g_cons_r(f);
    case '(':  return g_readsi(f, i);
    case ')':  return g_push(f, 1, nil);
    case '"':  return read_string(f, i, '"');
    default:   return p_in_ungetc(i, c),
                      read_atom(f, i); } }

static g_core *g_readsi(core *f, input* i) {
  if (!g_ok(f)) return f;
  word c = read_char(f, i);
  if (c == EOF || c == ')') return g_push(f, 1, nil);
  return p_in_ungetc(i, c),
         f = g_read1i(f, i),
         f = g_readsi(f, i),
         g_cons_r(f); }

static g_core *g_buf_new(g_core *f) {
  f = g_cells(f, Width(string) + 1);
  if (g_ok(f)) {
    string *o = (string*) f->sp[0];
    ini_str(o, sizeof(word)); }
  return f; }

static g_core *g_buf_grow(g_core *f) {
  size_t len = str(f->sp[0])->len,
         req = Width(string) + 2 * b2w(len);
  f = g_have(f, req);
  if (g_ok(f)) {
    string *o = (string*) f->hp;
    f->hp += req;
    ini_str(o, 2 * len);
    memcpy(o->text, str(f->sp[0])->text, len);
    f->sp[0] = (word) o; }
  return f; }

static Inline g_core *read_string(core *f, input* i, char delim) {
  int c;
  size_t n = 0, lim = sizeof(word);
  for (f = g_buf_new(f); g_ok(f); f = g_buf_grow(f), lim *= 2)
    for (string *b = str(f->sp[0]); n < lim;)
      if ((c = p_in_getc(i)) == EOF ||
           c == delim ||
           (c == '\\' && (c = p_in_getc(i)) == EOF))
        goto out;
      else b->text[n++] = c;
out:
  if (g_ok(f)) str(f->sp[0])->len = n;
  return f; }

static Inline g_core *read_atom(core *f, input *i) {
  int c;
  size_t n = 0, lim = sizeof(word);
  for (f = g_buf_new(f); g_ok(f); f = g_buf_grow(f), lim *= 2)
    for (string *b = str(f->sp[0]); n < lim;) switch (c = p_in_getc(i)) {
      case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
      case '(': case ')': case '"': case '\'': case EOF: p_in_ungetc(i, c); goto out;
      default: b->text[n++] = c; } out:
  if (!g_ok(f)) return f;
  string *buf = str(f->sp[0]);
  buf->len = n, buf->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
  char *e;
  long j = strtol(buf->text, &e, 0);
  if (*e == 0) return f->sp[0] = putnum(j), f;
  return g_intern(f); }

typedef struct file_input { input in; FILE *file; } file_input;
static int p_file_getc(input *i) { return getc(((file_input*) i)->file); }
static int p_file_ungetc(input *i, int c) { return ungetc(c, ((file_input*) i)->file); }
static int p_file_eof(input *i) { return feof(((file_input*) i)->file); }

NoInline g_core *g_read1f(core *f, FILE* i) {
  file_input fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  return g_read1i(f, (input*) &fi); }

Vm(read0) {
  Pack(f);
  f = g_read1f(f, stdin);
  if (code_of(f) == Eof) { // no error but end of file
    f = core_of(f);
    Unpack(f);
    Sp[0] = nil;
    Ip += 1;
    return Continue(); }
  f = g_push(f, 1, nil);
  f = g_cons_r(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Sp[1] = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }

NoInline g_core *g_readsip(core *f, string *s) {
  char n[256]; // :)
  memcpy(n, s->text, s->len);
  n[s->len] = 0;
  FILE *i = fopen(n, "r");
  if (!i) return g_push(f, 1, nil);
  file_input fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  f = g_readsi(f, (input*) &fi);
  fclose(i);
  return f; }

Vm(readf) {
  string *s = str(Sp[0]);
  if (!strp(Sp[0]) || s->len > 255) return
    Sp[0] = nil,
    Ip += 1,
    Continue();
  Pack(f);
  f = g_readsip(f, s);
  if (!g_ok(f)) return f;
  return Unpack(f),
         Sp[1] = Sp[0],
         Sp += 1,
         Ip += 1,
         Continue(); }

typedef struct text_input { input in; const char *text; int i; } text_input;
static int p_text_getc(input *i) {
  text_input *t = ((text_input*) i);
  char c = t->text[t->i];
  if (c) t->i++;
  return c; }

static int p_text_ungetc(input *i, int _) {
  text_input *t = ((text_input*) i);
  int idx = t->i;
  idx = idx ? idx - 1 : idx;
  t->i = idx;
  return t->text[idx]; }

static int p_text_eof(input *i) {
  text_input *t = (text_input*) i;
  return !t->text[t->i]; }

g_core *g_readcs(g_core *f, const char *cs) {
  text_input t = {{p_text_getc, p_text_ungetc, p_text_eof}, cs, 0};
  return g_read1i(f, (input*) &t); }

void transmit(core *f, FILE* out, word x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (datp(x)) typ(x)->em(f, out, x);
  else fprintf(out, "#%lx", (long) x); }

void g_writef(g_core *f, FILE *o) { transmit(f, o, f->sp[0]); }
