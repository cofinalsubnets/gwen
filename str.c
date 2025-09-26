#include "i.h"
#define max(a, b) ((a)>(b)?(a):(b))

g_core *g_strof(g_core *f, const char *cs) {
  size_t bytes = strlen(cs),
         words = b2w(bytes),
         req = Width(string) + words;
  f = g_cells(f, req);
  if (g_ok(f)) {
    g_string *o = str(f->sp[0]);
    ini_str(o, bytes);
    memcpy(o->text, cs, bytes); }
  return f; }

static uintptr_t xx_str(g_core *v, word _);
static bool eq_str(g_core *f, word x, word y);
static void em_str(g_core* v, FILE *o, word _);
static void wk_str(g_core* f, word x, word *p0, word *t0);
static word cp_str(g_core* v, word x, word *p0, word *t0);

g_type
  str_type = { .xx = xx_str, .cp = cp_str, .wk = wk_str, .eq = eq_str, .em = em_str, };

static word cp_str(g_core* v, word x, word *p0, word *t0) {
  string *src = str(x);
  size_t len = sizeof(string) + src->len;
  return (word) (src->ap = memcpy(bump(v, b2w(len)), src, len)); }

static void wk_str(g_core* f, word x, word *p0, word *t0) {
  f->cp += Width(string) + b2w(str(x)->len); }

static void em_str(g_core* v, FILE *o, word _) {
  size_t len = str(_)->len;
  const char *text = str(_)->text;
  putc('"', o);
  for (char c; len--; putc(c, o))
    if ((c = *text++) == '\\' || c == '"') putc('\\', o);
  putc('"', o); }

static uintptr_t xx_str(g_core *v, word _) {
  uintptr_t len = str(_)->len, h = 2166136261;
  unsigned char *bs = (unsigned char*) str(_)->text;
  while (len--) h ^= *bs++, h *= 16777619;
  return h; }

static bool eq_str(g_core *f, word x, word y) {
  string *a = str(x), *b = str(y);
  return a->len == b->len &&
    0 == strncmp(a->text, b->text, a->len); }

Vm(slen) {
  Sp[0] = strp(Sp[0]) ? putnum(str(Sp[0])->len) : nil;
  Ip += 1;
  return Continue(); }

Vm(ssub) {
  if (!strp(Sp[0])) Sp[2] = nil;
  else {
    string *s = str(Sp[0]);
    intptr_t i = nump(Sp[1]) ? getnum(Sp[1]) : 0,
             j = nump(Sp[2]) ? getnum(Sp[2]) : 0;
    i = 0 > i ? 0 : i > s->len ? s->len : i;
    j = i > j ? i : j > s->len ? s->len : j;
    if (i == j) Sp[2] = nil;
    else {
      size_t req = Width(string) + b2w(j - i);
      Have(req);
      string* t = str(Hp);
      Hp += req;
      ini_str(t, j - i);
      memcpy(t->text, s->text + i, j - i);
      Sp[2] = (word) t; } }
  return Ip += 1,
         Sp += 2,
         Continue(); }

Vm(sget) {
  if (!strp(Sp[0])) Sp[1] = nil;
  else {
    string *s = str(Sp[0]);
    size_t i = min(s->len - 1, getnum(Sp[1]));
    i = max(i, 0);
    Sp[1] = putnum(s->text[i]); }
  return Ip += 1,
         Sp += 1,
         Continue(); }

Vm(scat) {
  word a = Sp[0];
  if (!strp(a)) return Sp += 1,
                       Ip += 1,
                       Continue();
  word b = Sp[1];
  if (!strp(b)) return Sp[1] = a,
                       Sp += 1,
                       Ip += 1,
                       Continue();

  string *x = str(a), *y = str(b);
  size_t len = x->len + y->len,
         req = Width(string) + b2w(len);
  Have(req);
  string *z = str(Hp);
  return Hp += req,
         ini_str(z, len),
         memcpy(z->text, x->text, x->len),
         memcpy(z->text + x->len, y->text, y->len),
         Sp[1] = word(z),
         Ip += 1,
         Continue(); }

Vm(stringp) {
  Sp[0] = strp(Sp[0]) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }
