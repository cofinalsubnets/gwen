#include "i.h"

// default equality method for things that are only equal to themselves
bool neql(core *f, word a, word b) { return false; }

static NoInline bool eql_neq(core *f, word a, word b) {
  return homp(a | b) &&
         R(a)->ap == data &&
         R(b)->ap == data &&
         typof(a) == typof(b) &&
         typof(a)->eq(f, a, b); }

bool eql(core *f, word a, word b) {
  return a == b || eql_neq(f, a, b); }

