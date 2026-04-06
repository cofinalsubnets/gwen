#include "i.h"

g_vm(g_vm_gensym) {
 if (strp(Sp[0])) return Ap(g_vm_nomsym, f);
 uintptr_t const req = Width(struct g_atom) - 2;
 Have(req);
 struct g_atom *y = (struct g_atom*) Hp;
 Hp += req;
 ini_anon(y, g_clock());
 Sp[0] = word(y);
 Ip += 1;
 return Continue(); }

g_vm(g_vm_symnom) {
 intptr_t y = Sp[0];
 y = symp(y) && sym(y)->nom ? word(sym(y)->nom) : g_nil;
 Sp[0] = y;
 Ip += 1;
 return Continue(); }

op11(g_vm_symp, symp(Sp[0]) ? gputnum(-1) : g_nil)
struct g *g_intern(struct g*f) {
 f = g_have(f, Width(struct g_atom));
 if (g_ok(f)) f->sp[0] = (g_word) g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols);
 return f; }

g_noinline struct g_atom *g_intern_r(struct g *v, struct g_vec *b, struct g_atom **y) {
 struct g_atom *z = *y;
 if (!z) return // found an empty spot, insert new symbol
  z = bump(v, Width(struct g_atom)),
  z->ap = g_vm_data,
  z->typ = sym_class,
  z->nom = b,
  z->code = g_hash(v, gputnum(g_hash(v, (intptr_t) b))),
  z->l = z->r = 0,
  *y = z;
 struct g_vec *a = z->nom;
 int i = len(a) < len(b) ? -1 :
         len(a) > len(b) ? 1 :
         memcmp(txt(a), txt(b), len(a));
 return i == 0 ? z :
  g_intern_r(v, b, i < 0 ? &z->l : &z->r); }

g_vm(g_vm_nomsym) {
 Have(Width(struct g_atom));
 struct g_atom *y;
 Pack(f);
 y = g_intern_r(f, (struct g_vec*) f->sp[0], &f->symbols),
 Unpack(f);
 Sp[0] = word(y);
 Ip += 1;
 return Continue(); }
