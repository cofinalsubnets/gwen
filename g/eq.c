#include "i.h"

static g_noinline bool eql_cont(struct g *f, intptr_t a, intptr_t b) {
 if (!(even(a | b) && cell(a)->ap == g_vm_data && cell(b)->ap == g_vm_data && typ(a) == typ(b))) return false;
 intptr_t t = typ(a);
 // FIXME could overflow the stack -- use off pool for this
 if (t == two_q) return eql(f, A(a), A(b)) && eql(f, B(a), B(b));
 if (t == vec_q) return 0 == memcmp(vec(a), vec(b), g_vec_bytes(vec(a)));
 return false; }

g_inline bool eql(struct g *f, intptr_t a, intptr_t b) {
 return a == b || eql_cont(f, a, b); }
