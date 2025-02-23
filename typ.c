// type predicates and introspection
#include "i.h"
Vm(pairp) { return op(1, twop(Sp[0]) ? putnum(-1) : nil); }
Vm(fixnump) { return op(1, nump(Sp[0]) ? putnum(-1) : nil); }
Vm(stringp) { return op(1, strp(Sp[0]) ? putnum(-1) : nil); }
Vm(symbolp) { return op(1, symp(Sp[0]) ? putnum(-1) : nil); }

