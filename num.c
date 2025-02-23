#include "i.h"
Vm(add) { return op(2, putnum(getnum(Sp[0])+getnum(Sp[1]))); }
Vm(sub) { return op(2, putnum(getnum(Sp[0])-getnum(Sp[1]))); }
Vm(mul) { return op(2, putnum(getnum(Sp[0])*getnum(Sp[1]))); }
Vm(quot) { return op(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])/getnum(Sp[1]))); }
Vm(rem) { return op(2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])%getnum(Sp[1]))); }
Vm(eq) { return op(2, eql(f, Sp[0], Sp[1]) ? putnum(-1) : nil); }
Vm(lt) { return op(2, Sp[0] < Sp[1] ? putnum(-1) : nil); }
Vm(le) { return op(2, Sp[0] <= Sp[1] ? putnum(-1) : nil); }
Vm(gt) { return op(2, Sp[0] > Sp[1] ? putnum(-1) : nil); }
Vm(ge) { return op(2, Sp[0] >= Sp[1] ? putnum(-1) : nil);}
Vm(bnot) { return op(1, ~Sp[0] | 1); }
Vm(rng) { return op(1, putnum(rand())); }
