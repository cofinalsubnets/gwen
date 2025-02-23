#include "i.h"
bool eql(PCore *f, PWord a, PWord b) {
  if (a == b) return true;
  if (nump(a | b) ||
      ptr(a)->ap != data ||
      ptr(b)->ap != data ||
      dtyp(a) != dtyp(b)) return false;
  return dtyp(a)->equal(f, a, b); }

bool not_equal(PCore *f, PWord a, PWord b) { return false; }
