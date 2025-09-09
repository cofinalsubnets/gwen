#include "i.h"
#define p_getc(i) getc(i)
#define p_ungetc(i, c) ungetc(c, i)
#define p_eof(i) feof(i)
#define fi(i) ((file_input*)(i))

static int p_in_getc(input *i) { return i->getc(i); }
static int p_in_ungetc(input *i, int c) { return i->ungetc(i, c); }
static int p_in_eof(input *i) { return i->eof(i); }

static int p_file_getc(input *i) { return getc(fi(i)->file); }
static int p_file_ungetc(input *i, int c) { return ungetc(c, fi(i)->file); }
static int p_file_eof(input *i) { return feof(fi(i)->file); }

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
*reads(core*, input*),
  *p_read1(g_core*, input*),
  *read_string(core*, input*, char),
  *read_atom(core*, input*);


static Inline g_core *rquote(core *f, input *i) {
  f = p_read1(f, i);
  f = pushc(f, 1, nil);
  f = g_cons_stack(f, 1, 0);
  f = pushc(f, 1, f->quote);
  return g_cons_stack(f, 0, 1); }

static g_core *p_read1(core *f, input* i) {
  if (!g_ok(f)) return f;
  int c = read_char(f, i);
  switch (c) {
    case EOF:  return encode(f, Eof);
    case '\'': return rquote(f, i);
    case '(':  return reads(f, i);
    case ')':  return pushc(f, 1, nil);
    case '"':  return read_string(f, i, '"');
    default:   return p_in_ungetc(i, c),
                      read_atom(f, i); } }

static g_core *reads(core *f, input* i) {
  if (!g_ok(f)) return f;
  word c = read_char(f, i);
  if (c == EOF || c == ')') return pushc(f, 1, nil);
  p_in_ungetc(i, c);
  f = p_read1(f, i);
  f = reads(f, i);
  return g_cons_stack(f, 1, 0); }

// create and grow buffers for reading
static string *bnew(core *f) {
  string *s = cells(f, Width(string) + 1);
  return s ? ini_str(s, sizeof(word)) : s; }

static string *bgrow(core *f, string *s) {
  string *t; uintptr_t len = s->len;
  avec(f, s, t = cells(f, Width(string) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

static Inline g_core *read_string(core *f, input* i, char delim) {
  string *b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim)
      if ((c = p_in_getc(i)) == EOF ||
           c == delim ||
           (c == '\\' && (c = p_in_getc(i)) == EOF))
        goto out;
      else b->text[n++] = c;
out:
  if (!b) return encode(f, Oom);
  b->len = n;
  return pushc(f, 1, b); }

static Inline g_core *read_atom(core *f, input *i) {
  string *b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = p_in_getc(i)) {
      case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
      case '(': case ')': case '"': case '\'': case EOF: p_in_ungetc(i, c); goto out;
      default: b->text[n++] = c; } out:
  if (!b) return encode(f, Oom);
  b->len = n, b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
  char *e; long j = strtol(b->text, &e, 0);
  word x = *e == 0 ? putnum(j) : W(intern(f, b));
  if (!x) return encode(f, Oom);
  return pushc(f, 1, x); }

#define file_input(f) ((input*)&(file_input){{p_file_getc, p_file_ungetc, p_file_eof}, f})
static NoInline g_core *p_read1f(core *f, FILE* i) {
  input *fi = file_input(i);
  return p_read1(f, fi); }

Vm(read0) {
  Pack(f);
  f = p_read1f(f, stdin);
  int s = code_of(f);
  if (s == Eof) return // no error but end of file
    f = core_of(f),
    Unpack(f),
    Sp[0] = nil,
    Ip++,
    Continue();

  if (s != Ok) return s; // or was there an error?
  // no error and got a value on stack
  // make a list of it
  pair *p = pairof(f, f->sp[0], nil);
  if (!p) return Oom; // ...
  Unpack(f);
  Sp += 1;
  Ip += 1;
  Sp[0] = W(p);
  return Continue(); }

static NoInline g_core *p_readsp(core *f, string *s) {
  char n[265]; // :)
  memcpy(n, s->text, s->len);
  n[s->len] = 0;
  FILE *i = fopen(n, "r");
  if (!i) return pushc(f, 1, nil);
  input *fi = file_input(i);
  f = reads(f, fi);
  fclose(i);
  return f; }

Vm(readf) {
  string *s = str(Sp[0]);
  if (!strp(Sp[0]) || s->len > 255) return Sp[0] = nil, Ip += 1, Continue();
  Pack(f);
  f = p_readsp(f, s);
  if (!g_ok(f)) return code_of(f);
  Unpack(f);
  Sp[1] = Sp[0];
  Sp++;
  Ip++;
  return Continue(); }

#define ti(i) ((text_input*)(i))
static int p_text_getc(input *i) {
  text_input *t = ti(i);
  char c = t->text[t->i];
  if (c) t->i++;
  return c; }

static int p_text_ungetc(input *i, int _) {
  text_input *t = ti(i);
  int idx = t->i;
  return idx = idx ? idx - 1 : idx, t->i = idx, t->text[idx]; }

static int p_text_eof(input *i) { return !ti(i)->text[ti(i)->i]; }

g_core* p_readcs(g_core *f, const char *cs) {
  input *i = ((input*)&(text_input){{p_text_getc, p_text_ungetc, p_text_eof}, cs, 0});
  return p_read1(f, i); }

void transmit(core *f, FILE* out, word x) {
  if (nump(x)) fprintf(out, "%ld", (long) getnum(x));
  else if (datp(x)) typof(x)->em(f, out, x);
  else fprintf(out, "#%lx", (long) x); }
Vm(prc) { word w = *Sp; return putc(getnum(w), stdout), Ip++, Continue(); }
Vm(dot) { return transmit(f, stdout, Sp[0]), Ip++, Continue(); }
