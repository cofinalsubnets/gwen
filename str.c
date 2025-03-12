#include "i.h"

Vm(slen) {
  Word x = Sp[0];
  return op(1, strp(x) ? putnum(((String*)x)->len) : nil); }

Vm(ssub) {
  Cell* r = (Cell*) Sp[3];
  if (!strp(Sp[0])) Sp[3] = nil;
  else {
    String *s = (String*) Sp[0];
    intptr_t i = nump(Sp[1]) ? getnum(Sp[1]) : 0,
             j = nump(Sp[2]) ? getnum(Sp[2]) : 0;
    i = max(i, 0), i = min(i, s->len);
    j = max(j, i), j = min(j, s->len);
    if (i == j) Sp[3] = nil;
    else {
      size_t req = Width(String) + b2w(j - i);
      Have(req);
      String* t = ini_str((String*) Hp, j - i);
      Hp += req;
      memcpy(t->text, s->text + i, j - i);
      Sp[3] = (Word) t; } }
  Ip = r, Sp += 3;
  return Continue(); }

Vm(sget) {
  Cell *r = (Cell*) Sp[2];
  if (!strp(Sp[0])) Sp[2] = nil;
  else {
    String*s = (String*) Sp[0];
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[2] = putnum(s->text[i]); }
  Ip = r, Sp += 2;
  return Continue(); }

Vm(scat) {
  Word a = Sp[0], b = Sp[1];
  if (!strp(a)) return op(2, b);
  if (!strp(b)) return op(2, a);
  String *x = (String*) a, *y = (String*) b;
  size_t len = x->len + y->len,
         req = Width(String) + b2w(len);
  Have(req);
  String*z = ini_str((String*) Hp, len);
  Hp += req;
  memcpy(z->text, x->text, x->len);
  memcpy(z->text + x->len, y->text, y->len);
  return op(2, (Word) z); }

Vm(stringp) { return op(1, strp(Sp[0]) ? putnum(-1) : nil); }
String* ini_str(String *s, uintptr_t len) {
  return s->ap = data,
         s->typ = &string_type,
         s->len = len,
         s; }

static Word copy_string(Core* v, Word x, Word *p0, Word *t0) {
  String* src = (String*) x;
  size_t len = sizeof(String) + src->len;
  return (Word) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void walk_string(Core* f, Word x, Word *p0, Word *t0) {
  f->cp += Width(String) + b2w(((String*) x)->len); }

static void print_string(Core* v, PFile *o, Word _) {
  String* s = (String*) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static Word hash_string(Core* v, Word _) {
  String *s = (String*) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(Word),
         bytes = s->len % sizeof(Word);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static bool string_equal(Core *f, Word x, Word y) {
  String*a = (String*) x, *b = (String*) y;
  return a->len == b->len && 0 == strncmp(a->text, b->text, a->len); }

Type
  string_type = { .hash = hash_string, .copy = copy_string, .evac = walk_string, .emit = print_string, .equal = string_equal, };
