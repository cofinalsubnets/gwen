#include "i.h"

// general equality test
bool eql(Core *f, Word a, Word b) {
  // everything equals itself
  if (a == b) return true;
  
  // that's the only way unless a and b are both data
  // with the same type
  if (nump(a | b) ||
      ptr(a)->ap != data ||
      ptr(b)->ap != data ||
      dtyp(a) != dtyp(b)) return false;

  // in this case call the type's equality method
  return dtyp(a)->equal(f, a, b); }

bool not_equal(Core *f, Word a, Word b) { return false; }
