#include "i.h"

Vm(slen) {
  PWord x = Sp[0];
  return op(1, strp(x) ? putnum(((PString*)x)->len) : nil); }

Vm(ssub) {
  PCell* r = (PCell*) Sp[3];
  if (!strp(Sp[0])) Sp[3] = nil;
  else {
    PString *s = (PString*) Sp[0];
    intptr_t i = nump(Sp[1]) ? getnum(Sp[1]) : 0,
             j = nump(Sp[2]) ? getnum(Sp[2]) : 0;
    i = max(i, 0), i = min(i, s->len);
    j = max(j, i), j = min(j, s->len);
    if (i == j) Sp[3] = nil;
    else {
      size_t req = Width(PString) + b2w(j - i);
      Have(req);
      PString* t = ini_str((PString*) Hp, j - i);
      Hp += req;
      memcpy(t->text, s->text + i, j - i);
      Sp[3] = (PWord) t; } }
  Ip = r, Sp += 3;
  return Continue(); }

Vm(sget) {
  PCell *r = (PCell*) Sp[2];
  if (!strp(Sp[0])) Sp[2] = nil;
  else {
    PString*s = (PString*) Sp[0];
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[2] = putnum(s->text[i]); }
  Ip = r, Sp += 2;
  return Continue(); }

Vm(scat) {
  PWord a = Sp[0], b = Sp[1];
  if (!strp(a)) return op(2, b);
  if (!strp(b)) return op(2, a);
  PString *x = (PString*) a, *y = (PString*) b;
  size_t len = x->len + y->len,
         req = Width(PString) + b2w(len);
  Have(req);
  PString*z = ini_str((PString*) Hp, len);
  Hp += req;
  memcpy(z->text, x->text, x->len);
  memcpy(z->text + x->len, y->text, y->len);
  return op(2, (PWord) z); }
static PWord copy_string(PCore* v, PWord x, PWord *p0, PWord *t0) {
  PString* src = (PString*) x;
  size_t len = sizeof(PString) + src->len;
  return (PWord) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void walk_string(PCore* v, PWord x, PWord *p0, PWord *t0, PHeap *cp) {
  *cp += Width(PString) + b2w(((PString*) x)->len); }

static void print_string(PCore* v, PFile *o, PWord _) {
  PString* s = (PString*) _;
  size_t len = s->len;
  const char *text = s->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static PWord hash_string(PCore* v, PWord _) {
  PString *s = (PString*) _;
  uintptr_t h = 1;
  size_t words = s->len / sizeof(PWord),
         bytes = s->len % sizeof(PWord);
  const char *bs = s->text + s->len - bytes;
  while (bytes--) h = mix * (h ^ (mix * bs[bytes]));
  const intptr_t *ws = (intptr_t*) s->text;
  while (words--) h = mix * (h ^ (mix * ws[words]));
  return h; }

static bool string_equal(PCore *f, PWord x, PWord y) {
  PString*a = (PString*) x, *b = (PString*) y;
  return a->len == b->len && 0 == strncmp(a->text, b->text, a->len); }

PType
  string_type = { .hash = hash_string, .copy = copy_string, .evac = walk_string, .emit = print_string, .equal = string_equal, };
