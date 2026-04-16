#include "i.h"

// this is called to check for equivalence when a != b
g_noinline bool eqv(struct g *f, word a, word b) {
 if (0 == ((a | b) & 1) && datp(a) && datp(b) && typ(a) == typ(b)) switch (typ(a)) {
  case two_q: return eql(f, A(a), A(b)) && eql(f, B(a), B(b));
  case vec_q: {
   size_t la = g_vec_bytes(vec(a)), lb = g_vec_bytes(vec(b));
   return la == lb && memcmp(vec(a), vec(b), la) == 0; } }
 return false; }
