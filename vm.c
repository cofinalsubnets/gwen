#include "i.h"

Vm(defmacro) {
  Pack(f);
  if (!table_set(f, f->macro, Sp[0], Sp[1])) return Oom;
  Unpack(f);
  return op(2, Sp[1]); }

Vm(data) {
  Word _ = (Word) Ip;
  return op(1, _); }

static Vm(pushk_jump) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

// branch instructions

Vm(jump) {
  Ip = Ip[1].m;
  return Continue(); }

Vm(cond) {
  Ip = nilp(*Sp) ? Ip[1].m : Ip + 2,
  Sp++;
  return Continue(); }

// load instructions
//
// push an immediate value
Vm(imm) {
  Have1();
  *--Sp = Ip[1].x;
  Ip += 2;
  return Continue(); }

// push a value from the stack
Vm(dup) {
  Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp--;
  Ip += 2;
  return Continue(); }

// call and return
// apply function to one argument
Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  Cell *k = R(Sp[1]);
  Sp[1] = Z(Ip + 1);
  Ip = k;
  return Continue(); }

// tail call
Vm(tap) {
  Word x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return op(1, j);
  Ip = R(j);
  *Sp = x;
  return Continue(); }

// apply to multiple arguments
Vm(apn) {
  size_t n = getnum(Ip[1].x);
  Cell *ra = Ip + 2; // return address
  Ip = R(Sp[n]) + 2; // this instruction is only emitted when the callee is known to be a function
  Sp[n] = Z(ra); // store return address
  return Continue(); }

// tail call
Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = R(Sp[n]) + 2;
  Stack o = Sp;
  for (Sp += r + 1; n--; Sp[n] = o[n]);
  return Continue(); }

// return
Vm(ret) {
  Word n = getnum(Ip[1].x) + 1;
  return op(n, *Sp); }

// exit vm and return to C
Vm(yield) { return Pack(f), YieldStatus; }

// currying
Vm(curry) {
  Cell *k = R(Hp), *j = k;
  size_t S = 3 + Width(struct tag),
         n = getnum(Ip[1].x);

  if (n == 2) { Have(S); }
  else {
    S += 2, j += 2;
    Have(S);
    k[0].ap = curry, k[1].x = putnum(n - 1); }

  j[0].ap = pushk_jump, j[1].x = *Sp++, j[2].m = Ip + 2;
  j[3].x = 0, j[4].m = k;

  return
    Hp += S,
    Ip = R(*Sp),
    *Sp = Z(k),
    Continue(); }
