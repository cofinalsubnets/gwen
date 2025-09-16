#include "i.h"

NoInline Vm(ev0) {
  Ip += 1;
  Pack(f);
  return g_run(g_ana(f, jump)); }

Vm(defglob) {
  Have(3);
  Sp -= 3;
  Sp[0] = (g_word) f->dict;
  Sp[1] = Ip[1].x;
  Sp[2] = Sp[3];
  Pack(f);
  f = g_hash_put(f);
  if (!g_ok(f)) return f;
  Unpack(f);
  Sp += 1;
  Ip += 2;
  return Continue(); }

Vm(drop1) {
  Ip += 1;
  Sp += 1;
  return Continue(); }

Vm(free_variable) {
  Ip[0].ap = imm;
  Ip[1].x = g_hash_get(f, Ip[1].x, f->dict, Ip[1].x);
  return Continue(); }

Vm(late_bind) {
  word ref = Ip[1].x, lfd = ref;
  ref = AB(lfd);
  Ip[0].ap = imm;
  Ip[1].x = ref;
  return Continue(); }

Vm(data) {
  word x = W(Ip);
  Sp += 1;
  Ip = cell(Sp[0]);
  Sp[0] = x;
  return Continue(); }

Vm(uncurry) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

Vm(jump) {
  Ip = Ip[1].m;
  return Continue(); }

Vm(cond) {
  Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2;
  return Continue(); }
// load instructions
//
// push an immediate value
Vm(imm) {
  Have1();
  Sp -= 1;
  Sp[0] = Ip[1].x;
  Ip += 2;
  return Continue(); }

// push a value from the stack
Vm(ref) {
  Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp -= 1;
  Ip += 2;
  return Continue(); }

// call and return
// apply function to one argument
Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  cell *k = cell(Sp[1]);
  Sp[1] = word(Ip + 1);
  Ip = k;
  return Continue(); }

Vm(apl) {
  if (nump(Sp[0])) return
    Sp[1] = Sp[0],
    Sp++,
    Ip++,
    Continue();
  cell *k = cell(Sp[0]);
  Sp[0] = Sp[1];
  Sp[1] = word(Ip + 1);
  Ip = k;
  return Continue(); }


// tail call
Vm(tap) {
  word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) {
    Sp += 1;
    Ip = cell(Sp[0]);
    Sp[0] = j;
    return Continue(); }
  Ip = cell(j);
  Sp[0] = x;
  return Continue(); }

// apply to multiple arguments
Vm(apn) {
  size_t n = getnum(Ip[1].x);
  cell *ra = Ip + 2; // return address
  // this instruction is only emitted when the callee is known to be a function
  // so putting a value off the stack into Ip is safe. the +2 is cause we leave
  // the currying instruction in there... should be skipped in compiler instead FIXME
  Ip = cell(Sp[n]) + 2;
  Sp[n] = W(ra); // store return address
  return Continue(); }

// tail call
Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = cell(Sp[n]) + 2;
  word *o = Sp;
  for (Sp += r + 1; n--; Sp[n] = o[n]);
  return Continue(); }

// return
Vm(ret) {
  word n = getnum(Ip[1].x) + 1;
  Ip = cell(Sp[n]);
  Sp[n] = Sp[0];
  Sp += n;
  return Continue(); }
Vm(ret0) {
  Ip = cell(Sp[1]);
  Sp[1] = Sp[0];
  Sp += 1;
  return Continue(); }

// currying
Vm(curry) {
  cell *k = cell(Hp), *j = k;
  size_t S = 3 + Width(struct tag),
         n = getnum(Ip[1].x);
  if (n == 2) { Have(S); }
  else { S += 2; Have(S); j += 2, k[0].ap = curry, k[1].x = putnum(n - 1); }
  j[0].ap = uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k;
  Hp += S;
  Ip = cell(*Sp);
  Sp[0] = word(k);
  return Continue(); }


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

Vm(symbolp) {
  Sp[0] = symp(Sp[0]) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }

Vm(nullp) {
  Sp[0] = nilp(Sp[0]) ? putnum(-1) : nil;
  Ip += 1;
  return Continue(); }


Vm(g_yield) {
  Ip = Ip[1].m;
  Pack(f);
  return f; }

Vm(seek) {
  Sp[1] = W(((cell*) Sp[1]) + getnum(Sp[0]));
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(peek) {
  Sp[0] = cell(Sp[0])->x;
  Ip += 1;
  return Continue(); }

Vm(poke) {
  cell(Sp[1])->x = Sp[0];
  Sp += 1;
  Ip += 1;
  return Continue(); }

Vm(thda) {
  size_t n = getnum(Sp[0]);
  Have(n + Width(struct tag));
  cell *k = cell(Hp);
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

Vm(prc) {
  word w = *Sp;
  putc(getnum(w), stdout);
  Ip += 1;
  return Continue(); }

Vm(dot) {
  transmit(f, stdout, Sp[0]);
  Ip += 1;
  return Continue(); }
