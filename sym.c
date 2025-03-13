#include "i.h"

static Symbol *intern_r(Core*, String*, Symbol**);
Vm(symbolp) { return op(1, symp(Sp[0]) ? putnum(-1) : nil); }

Symbol *ini_sym(Symbol *y, String *nom, uintptr_t code) {
  return y->ap = data,
         y->typ = &symbol_type,
         y->nom = nom,
         y->code = code,
         y->l = y->r = 0,
         y; }

static Inline Symbol *ini_anon(Symbol *y, Word code) {
  return y->ap = data,
         y->typ = &symbol_type,
         y->nom = 0,
         y->code = code,
         y; }

static Word hash_symbol(Core *v, Word _) {
  return ((Symbol*) _)->code; }

static Word copy_symbol(Core *f, Word x, Word *p0, Word *t0) {
  Symbol *src = (Symbol*) x,
         *dst = src->nom ?
           intern_r(f, (String*) cp(f, (Word) src->nom, p0, t0), &f->symbols) :
           ini_anon(bump(f, Width(Symbol) - 2), src->code);
  return (Word) (src->ap = (Vm*) dst); }

static void walk_symbol(Core *f, Word x, Word *p0, Word *t0) {
  f->cp += Width(Symbol) - (((Symbol*)x)->nom ? 0 : 2); }

static void print_symbol(Core *f, PFile *o, Word x) {
  String* s = ((Symbol*) x)->nom;
  if (s) for (int i = 0; i < s->len; putc(s->text[i++], o));
  else fprintf(o, "#sym@%lx", (long) x); }

static Symbol *intern_r(Core *v, String *b, Symbol **y) {
  Symbol *z = *y;
  if (!z) return *y =
    ini_sym(bump(v, Width(Symbol)), b, hash(v, putnum(hash(v, (Word) b))));
  String *a = z->nom;
  int i = a->len < b->len ? -1 :
          a->len > b->len ? 1 :
          strncmp(a->text, b->text, a->len);
  return i == 0 ? z : intern_r(v, b, i < 0 ? &z->l : &z->r); }

Symbol* intern(Core *f, String* b) {
  if (avail(f) < Width(Symbol)) {
    bool ok;
    avec(f, b, ok = p_please(f, Width(Symbol)));
    if (!ok) return 0; }
  return intern_r(f, b, &f->symbols); }


Symbol* literal_symbol(Core *f, const char *nom) {
  size_t len = strlen(nom);
  String *o = cells(f, Width(String) + b2w(len));
  if (!o) return 0;
  memcpy(o->text, nom, len);
  return intern(f, ini_str(o, len)); }

static Vm(symm) {
  Have(Width(Symbol));
  Pack(f);
  Symbol *y = intern_r(f, (String*) f->sp[0], &f->symbols);
  Unpack(f);
  return op(1, Z(y)); }

Vm(gensym) {
  if (strp(Sp[0])) return Jump(symm);
  const int req = Width(Symbol) - 2;
  Have(req);
  Symbol* y = (Symbol*) Hp;
  Hp += req;
  return op(1, (Word) ini_anon(y, rand())); }


Vm(symnom) {
  Word y = *Sp;
  y = symp(y) && ((Symbol*)y)->nom ? Z(((Symbol*)y)->nom) : nil;
  return op(1, y); }

Type symbol_type = {
  .hash = hash_symbol,
  .copy = copy_symbol,
  .evac = walk_symbol,
  .equal = not_equal,
  .emit = print_symbol,
};
