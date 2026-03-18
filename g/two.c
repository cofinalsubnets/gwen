#include "i.h"

op11(g_vm_car, twop(Sp[0]) ? A(Sp[0]) : Sp[0])
op11(g_vm_cdr, twop(Sp[0]) ? B(Sp[0]) : g_nil)
op11(g_vm_twop, twop(Sp[0]) ? gputnum(-1) : g_nil)
g_vm(g_vm_cons) {
 Have(Width(struct g_pair));
 struct g_pair *w = (struct g_pair*) Hp;
 Hp += Width(struct g_pair);
 ini_two(w, Sp[0], Sp[1]);
 *++Sp = word(w);
 Ip++;
 return Continue(); }
void ini_two(struct g_pair *w, intptr_t a, intptr_t b) {
  w->ap = g_vm_data; w->typ = two_class; w->a = a; w->b = b; }

g_noinline struct g *gxl(struct g *f) {
 f = g_have(f, Width(struct g_pair));
 if (g_ok(f)) {
  struct g_pair *p = bump(f, Width(struct g_pair));
  ini_two(p, f->sp[0], f->sp[1]);
  *++f->sp = (intptr_t) p; }
 return f; }

struct g *gxr(struct g *f) {
 g_word x = f->sp[0];
 f->sp[0] = f->sp[1];
 f->sp[1] = x;
 return gxl(f); }
