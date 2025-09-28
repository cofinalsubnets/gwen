#include "i.h"

#define op(nom, n, x) Vm(nom) { g_word _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
op(add, 2, (Sp[0]|1) + (Sp[1]&~1))
op(sub, 2, (Sp[0]|1) - (Sp[1]&~1))
op(mul, 2, putnum(getnum(Sp[0])*getnum(Sp[1])))
op(quot, 2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])/getnum(Sp[1])))
op(rem, 2, nilp(Sp[1]) ? nil : putnum(getnum(Sp[0])%getnum(Sp[1])))
op(eq, 2, eql(f, Sp[0], Sp[1]) ? putnum(-1) : nil)
op(lt, 2, Sp[0] < Sp[1] ? putnum(-1) : nil)
op(le, 2, Sp[0] <= Sp[1] ? putnum(-1) : nil)
op(gt, 2, Sp[0] > Sp[1] ? putnum(-1) : nil)
op(ge, 2, Sp[0] >= Sp[1] ? putnum(-1) : nil)
op(bnot, 1, ~Sp[0] | 1)
op(band, 2, (Sp[0] & Sp[1]) | 1)
op(bor, 2, (Sp[0] | Sp[1]) | 1)
op(bxor, 2, (Sp[0] ^ Sp[1]) | 1)
op(rng, 1, putnum(rand()))
op(fixnump, 1, nump(Sp[0]) ? putnum(-1) : nil)

