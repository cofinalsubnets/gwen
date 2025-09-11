#include "i.h"

// default equality method for things that are only equal to themselves
bool neql(core *f, word a, word b) { return false; }

static NoInline bool eql_neq(core *f, word a, word b) {
  return celp(a | b) &&
         cell(a)->ap == data &&
         cell(b)->ap == data &&
         typ(a) == typ(b) &&
         typ(a)->eq(f, a, b); }

bool eql(core *f, word a, word b) {
  return a == b || eql_neq(f, a, b); }

