#include "i.h"
Vm(data) {
  word x = W(Ip);
  return Ip = R(Sp[1]),
         Sp[1] = x,
         Sp += 1,
         Continue(); }

Vm(uncurry) {
  Have1();
  return *--Sp = Ip[1].x,
         Ip = Ip[2].m,
         Continue(); }

Vm(jump) {
  return Ip = Ip[1].m,
         Continue(); }

Vm(cond) { return Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2, Continue(); }
// load instructions
//
// push an immediate value
Vm(imm) { Have1(); return Sp -= 1, Sp[0] = Ip[1].x, Ip += 2, Continue(); }

// push a value from the stack
Vm(ref) { Have1(); return
  Sp[-1] = Sp[getnum(Ip[1].x)], Sp -= 1, Ip += 2, Continue(); }

// call and return
// apply function to one argument
Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  cell *k = R(Sp[1]); return Sp[1] = W(Ip + 1), Ip = k, Continue(); }

// tail call
Vm(tap) {
  word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return Ip = R(Sp[1]), Sp[1] = j, Sp += 1, Continue();
  return Ip = R(j), Sp[0] = x, Continue(); }

// apply to multiple arguments
Vm(apn) {
  size_t n = getnum(Ip[1].x);
  cell *ra = Ip + 2; // return address
  // this instruction is only emitted when the callee is known to be a function
  // so putting a value off the stack into Ip is safe. the +2 is cause we leave
  // the currying instruction in there... should be skipped in compiler instead FIXME
  Ip = R(Sp[n]) + 2;
  Sp[n] = W(ra); // store return address
  return Continue(); }

// tail call
Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = R(Sp[n]) + 2;
  word *o = Sp;
  for (Sp += r + 1; n--; Sp[n] = o[n]);
  return Continue(); }

// return
Vm(ret) {
  word n = getnum(Ip[1].x) + 1;
  return Ip = R(Sp[n]), Sp[n] = Sp[0], Sp += n, Continue(); }
Vm(ret0) { return Ip = R(Sp[1]), Sp[1] = Sp[0], Sp += 1, Continue(); }

// currying
Vm(curry) {
  cell *k = R(Hp), *j = k;
  size_t S = 3 + Width(struct tag),
         n = getnum(Ip[1].x);
  if (n == 2) { Have(S); }
  else { S += 2; Have(S); j += 2, k[0].ap = curry, k[1].x = putnum(n - 1); }
  j[0].ap = uncurry, j[1].x = *Sp++, j[2].m = Ip + 2, j[3].x = 0, j[4].m = k;
  return Hp += S, Ip = R(*Sp), Sp[0] = W(k), Continue(); }


#define op(nom, n, x) Vm(nom) { word _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
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
Vm(symbolp) { return Sp[0] = symp(Sp[0]) ? putnum(-1) : nil, Ip++, Continue(); }
Vm(nullp) { return Sp[0] = nilp(Sp[0]) ? putnum(-1) : nil, Ip++, Continue(); }


Vm(yieldi) {
  Ip = Ip[1].m;
  Pack(f);
  return YieldStatus; }

Vm(seek) {
  Sp[1] = W(((cell*) Sp[1]) + getnum(Sp[0]));
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(peek) {
  Sp[0] = R(Sp[0])->x;
  Ip += 1;
  return Continue(); }

Vm(poke) {
  R(Sp[1])->x = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  cell *k = R(Hp);
  struct tag *t = (struct tag*) (k + n);
  Hp += n + Width(struct tag);
  t->null = NULL;
  t->head = k;
  memset(k, -1, n * sizeof(word));
  Sp[0] = (word) k;
  Ip += 1;
  return Continue(); }

Vm(trim) {
  cell *k = (cell*) Sp[0];
  ttag(k)->head = k;
  Ip += 1;
  return Continue(); }
