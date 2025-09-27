#include "i.h"

static NoInline bool eql_neq(g_core *f, g_word a, g_word b) {
  return celp(a | b) &&
         cell(a)->ap == data &&
         cell(b)->ap == data &&
         typ(a) == typ(b) &&
         typ(a)->eq(f, a, b); }

// default equality method for things that are only equal to themselves
bool neql(g_core *f, g_word a, g_word b) { return false; }

bool eql(g_core *f, g_word a, g_word b) {
  return a == b || eql_neq(f, a, b); }
