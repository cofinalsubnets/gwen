#include "i.h"
Vm(defglob) {
  Have(3);
  Sp -= 3;
  g_table *t = f->dict;
  g_word k = Ip[1].x,
         v = Sp[3];
  Sp[0] = k;
  Sp[1] = v;
  Sp[2] = (g_word) t;
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
  g_word y = Ip[1].x,
         v = g_hash_get(f, y, f->dict, y); // see if it's defined now...
  Ip[0].ap = imm;
  Ip[1].x = v;
  return Continue(); }

Vm(late_bind) {
  g_word v = AB(Ip[1].x);
  Ip[0].ap = imm;
  Ip[1].x = v;
  return Continue(); }

Vm(data) {
  return Ap(typ(Ip)->ap, f); }

Vm(self) {
  g_word x = word(Ip);
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
  g_cell *k = cell(Sp[1]);
  Sp[1] = word(Ip + 1);
  Ip = k;
  return Continue(); }

// tail call
Vm(tap) {
  g_word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return
    Sp += 1,
    Ip = cell(Sp[0]),
    Sp[0] = j,
    Continue();
  return
    Ip = cell(j),
    Sp[0] = x,
    Continue(); }

// apply to multiple arguments
Vm(apn) {
  size_t n = getnum(Ip[1].x);
  g_cell *ra = Ip + 2; // return address
  // this instruction is only emitted when the callee is known to be a function
  // so putting a value off the stack into Ip is safe. the +2 is cause we leave
  // the currying instruction in there... should be skipped in compiler instead FIXME
  Ip = cell(Sp[n]);
  Ip += 2;
  Sp[n] = word(ra); // store return address
  return Continue(); }

// tail call
Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = cell(Sp[n]) + 2;
  g_word *o = Sp;
  for (Sp += r + 1; n--; Sp[n] = o[n]);
  return Continue(); }

// return
Vm(ret) {
  g_word n = getnum(Ip[1].x) + 1;
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
  g_cell *k = cell(Hp), *j = k;
  size_t S = 3 + Width(struct g_tag),
         n = getnum(Ip[1].x);

  if (n == 2) Have(S);
  else {
    S += 2;
    Have(S);
    j += 2;
    k[0].ap = curry;
    k[1].x = putnum(n - 1); }

  j[0].ap = uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k;
  Hp += S;
  Ip = cell(*Sp);
  Sp[0] = word(k);
  return Continue(); }

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
  return encode(f, YieldStatus); }
