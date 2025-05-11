#include "i.h"

typedef struct In {
  int (*getc)(struct In*),
      (*ungetc)(struct In*, int),
      (*eof)(struct In*);
} In;

typedef struct FileIn {
  struct In in;
  FILE *file;
} FileIn;

typedef struct TextIn {
  struct In in;
  const char *text;
  int i;
} TextIn;

#define p_getc(i) getc(i)
#define p_ungetc(i, c) ungetc(c, i)
#define p_eof(i) feof(i)
#define fi(i) ((FileIn*)(i))

static int p_in_getc(In *i) { return i->getc(i); }
static int p_in_ungetc(In *i, int c) { return i->ungetc(i, c); }
static int p_in_eof(In *i) { return i->eof(i); }

static int p_file_getc(In *i) {
  return getc(fi(i)->file); }
static int p_file_ungetc(In *i, int c) {
  return ungetc(c, fi(i)->file); }
static int p_file_eof(In *i) {
  return feof(fi(i)->file); }

#define ti(i) ((TextIn*)(i))
static int p_text_getc(In *i) {
  TextIn *t = ti(i);
  char c = t->text[t->i];
  if (c) t->i++;
  return c; }

static int p_text_ungetc(In *i, int _) {
  TextIn *t = ti(i);
  int idx = t->i;
  idx = idx ? idx - 1 : idx;
  t->i = idx;
  return t->text[idx]; }

static int p_text_eof(In *i) {
  return !ti(i)->text[ti(i)->i]; }

////
/// " the parser "
//
//
// get the next significant character from the stream
static int read_char(Core *f, In *i) {
  for (int c;;) switch (c = p_in_getc(i)) {
    default: return c;
    case '#': case ';': while (!p_in_eof(i) && (c = p_in_getc(i)) != '\n' && c != '\r');
    case ' ': case '\t': case '\n': case '\r': case '\f': continue; } }

static int
  reads(Core*, In*),
  read_string(Core*, In*, char),
  read_atom(Core*, In*);

static Status enquote(Core *f) {
  Pair *w = pairof(f, f->sp[0], nil);
  if (!w) return Oom;
  f->sp[0] = (Word) w;
  w = pairof(f, (Word) f->vars.quote, f->sp[0]);
  if (!w) return Oom;
  f->sp[0] = (Word) w;
  return Ok; }

static int p_read1(Core*, In*);

Status p_read1f(Core *f, FILE* i) {
  FileIn fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  return p_read1(f, (In*) &fi); }

Status p_read1t(Core *f, const char *t) {
  TextIn ti = {{p_text_getc, p_text_ungetc, p_text_eof}, t, 0};
  return p_read1(f, (In*) &ti); }

static int p_read1(Core *f, In* i) {
  int c = read_char(f, i);
  switch (c) {
    case EOF: return Eof;
    case '\'': return (c = p_read1(f, i)) == Ok ? enquote(f) : c;
    case '(': return reads(f, i);
    case ')': return pushs(f, 1, nil) ? Ok : Oom;
    case '"': return read_string(f, i, '"');
    default: return p_in_ungetc(i, c),
                    read_atom(f, i); } }

static int reads(Core *f, In* i) {
  Word c = read_char(f, i);
  if (c == EOF || c == ')') return pushs(f, 1, nil) ? Ok : Oom;
  p_in_ungetc(i, c);
  int s = p_read1(f, i);
  s = s == Ok ? reads(f, i) : s;
  return s != Ok ? s :
    !(c = Z(pairof(f, f->sp[1], f->sp[0]))) ? Oom :
    (*++f->sp = c, Ok); }

// create and grow buffers for reading
static String *bnew(Core *f) {
  String *s = cells(f, Width(String) + 1);
  return s ? ini_str(s, sizeof(Word)) : s; }

static String *bgrow(Core *f, String *s) {
  String *t; uintptr_t len = s->len;
  avec(f, s, t = cells(f, Width(String) + 2 * b2w(len)));
  if (t) memcpy(ini_str(t, 2 * len)->text, s->text, len);
  return t; }

static int read_string(Core *f, In* i, char delim) {
  String* b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(Word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim)
      if ((c = p_in_getc(i)) == EOF ||
           c == delim ||
           (c == '\\' && (c = p_in_getc(i)) == EOF))
        goto out;
      else b->text[n++] = c;
out:
  if (!b) return Oom;
  b->len = n;
  return pushs(f, 1, b) ? Ok : Oom; }

#define alpha_c "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
#define digit_c "0123456789"
#define whitespace_c " \n\t\r\f"
#define delim_c "(){}[]"
#define math_c "-+*/%^<>&|"
#define sign_c "-+"
#define quote_c "\"'`"
#define punct_c ",:.?!"
#define comment_c ";#"

static int read_atom(Core *f, In *i) {
  String *b = bnew(f);
  int c; size_t n = 0;
  for (size_t lim = sizeof(Word); b; b = bgrow(f, b), lim *= 2)
    while (n < lim) switch (c = p_in_getc(i)) {
      case ' ': case '\n': case '\t': case '\r': case '\f': case ';': case '#':
      case '(': case ')': case '"': case '\'': case EOF: p_in_ungetc(i, c); goto out;
      default: b->text[n++] = c; } out:
  if (!b) return Oom;
  b->len = n, b->text[n] = 0; // zero terminate for strtol ; n < lim so this is safe
  char *e; long j = strtol(b->text, &e, 0);
  Word x = *e == 0 ? putnum(j) : Z(intern(f, b));
  return !x || !pushs(f, 1, x) ? Oom : Ok; }

Vm(read0) {
  Pack(f);
  Status s = p_read1f(f, stdin);
  if (s != Eof) {
    s = s == Ok ? p_cons(f) : s;
    if (s != Ok) return s; }
  Unpack(f);
  return op(1, *Sp); }

Vm(readf) {
  if (!strp(Sp[0])) return op(1, nil);
  String *s = (String*)Sp[0];
  if (s->len > 255) return op(1, nil);
  char n[265];
  memcpy(n, s->text, s->len);
  n[s->len] = 0;
  FILE *i = fopen(n, "r");
  if (!i) return op(1, nil);
  Pack(f);
  FileIn fi = {{p_file_getc, p_file_ungetc, p_file_eof}, i};
  Status t = reads(f, (In*)&fi);
  fclose(i);
  return t == Ok ? (Unpack(f), op(2, *Sp)) : t; }
