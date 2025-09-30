#include "i.h"

typedef struct text_input {
  g_input in;
  const char *text;
  int i;
} text_input;

static int p_text_getc(g_input *i) {
  text_input *t = ((text_input*) i);
  char c = t->text[t->i];
  if (c) t->i++;
  return c ? c : EOF; }

static int p_text_ungetc(g_input *i, int _) {
  text_input *t = ((text_input*) i);
  int idx = t->i;
  idx = idx ? idx - 1 : idx;
  t->i = idx;
  return t->text[idx]; }

static int p_text_eof(g_input *i) {
  text_input *t = (text_input*) i;
  return !t->text[t->i]; }


NoInline g_core *g_read1s(g_core *f, const char *cs) {
  f = g_readss(f, cs);
  if (g_ok(f)) f->sp[0] = A(f->sp[0]);
  return f; }

NoInline g_core *g_readss(g_core *f, const char *cs) {
  text_input t = {{p_text_getc, p_text_ungetc, p_text_eof}, cs, 0};
  f = g_readsi(f, (input*) &t);
  return f; }



////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(input *i) {
  for (int c;;) switch (c = p_in_getc(i)) {
    default: return c;
    case '#': case ';': while (!p_in_eof(i) && (c = p_in_getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }
static g_core *g_buf_new(g_core *f) {
  f = g_cells(f, Width(string) + 1);
  if (g_ok(f)) {
    string *o = (string*) f->sp[0];
    ini_str(o, sizeof(g_word)); }
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
    f->sp[0] = (g_word) o; }
  return f; }


g_core *g_readsi(g_core *f, input* i) {
  intptr_t n = 0;
  for (int c; g_ok(f); n++) {
    c = read_char(i);
    if (c == EOF || c == ')') break;
    p_in_ungetc(i, c);
    f = g_read1i(f, i); }
  for (f = g_push(f, 1, nil); n--; f = g_cons_r(f));
  return f; }


g_core *g_read1i(g_core *f, input* i) {
  if (!g_ok(f)) return f;
  int c = read_char(i);
  size_t n = 0;
  switch (c) {
    case '(':  return g_readsi(f, i);
    case ')':  return g_push(f, 1, nil);
    case EOF:  return encode(f, g_status_eof);
    case '\'': return g_cons_r(g_cons_l(g_read1i(g_push(f, 2, nil, f->quote), i)));
    case '"':  
      f = g_buf_new(f);
      for (size_t lim = sizeof(g_word); g_ok(f); f = g_buf_grow(f), lim *= 2)
        for (string *b = str(f->sp[0]); n < lim; b->text[n++] = c)
          if ((c = p_in_getc(i)) == EOF || c == '"' ||
               (c == '\\' && (c = p_in_getc(i)) == EOF))
            return b->len = n, f;
      return f;
    default:
      p_in_ungetc(i, c);
      f = g_buf_new(f);
      for (size_t lim = sizeof(g_word); g_ok(f); f = g_buf_grow(f), lim *= 2)
        for (string *b = str(f->sp[0]); n < lim; b->text[n++] = c)
          switch (c = p_in_getc(i)) {
            default: continue;
            case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
            case '(': case ')': case '"': case '\'': case EOF:
              p_in_ungetc(i, c);
              b->len = n;
              b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
              char *e;
              long j = strtol(b->text, &e, 0);
              if (*e == 0) f->sp[0] = putnum(j);
              else f = g_intern(f);
              return f; }
      return f; } }
