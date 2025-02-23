#include "i.h"

static PSymbol* intern_r(PCore*, PString*, PSymbol**);

static PWord hash_symbol(PCore *v, PWord _) {
  return ((PSymbol*) _)->code; }
static PWord copy_symbol(PCore *f, PWord x, PWord *p0, PWord *t0) {
  PSymbol *src = (PSymbol*) x,
             *dst = src->nom ?
               intern_r(f, (PString*) cp(f, (PWord) src->nom, p0, t0), &f->symbols) :
               ini_anon(bump(f, Width(PSymbol) - 2), src->code);
  return (PWord) (src->ap = (PVm*) dst); }
static void walk_symbol(PCore *f, PWord x, PWord *p0, PWord *t0, PHeap *cp) {
  *cp += Width(PSymbol) - (((PSymbol*)x)->nom ? 0 : 2); }

static void print_symbol(PCore *f, PFile *o, PWord x) {
  PString* s = ((PSymbol*) x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }


static PSymbol* intern_r(PCore *v, PString* b, PSymbol* *y) {
  PSymbol* z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(PSymbol)), b, hash(v, putnum(hash(v, (PWord) b))));
  PString* a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_r(v, b, i < 0 ? &z->l : &z->r); }

PSymbol* intern(PCore *f, PString* b) {
  if (avail(f) < Width(PSymbol)) {
    bool ok;
    avec(f, b, ok = p_please(f, Width(PSymbol)));
    if (!ok) return 0; }
  return intern_r(f, b, &f->symbols); }


PSymbol* literal_symbol(PCore *f, const char *nom) {
  size_t len = strlen(nom);
  PString* o = cells(f, Width(PString) + b2w(len));
  if (!o) return 0;
  memcpy(o->text, nom, len);
  return intern(f, ini_str(o, len)); }

Vm(gensym) {
  const int req = Width(PSymbol) - 2;
  Have(req);
  PSymbol* y = (PSymbol*) Hp;
  Hp += req;
  return op(1, (PWord) ini_anon(y, rand())); }


PType symbol_type = {
  .hash = hash_symbol,
  .copy = copy_symbol,
  .evac = walk_symbol,
  .equal = not_equal,
  .emit = print_symbol,
};
