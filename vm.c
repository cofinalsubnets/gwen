#include "i.h"


Vm(defmacro) {
  Pack(f);
  if (!table_set(f, f->macro, Sp[0], Sp[1])) return Oom;
  Unpack(f);
  return op(2, Sp[1]); }

Vm(data) {
  PWord this = (PWord) Ip;
  return op(1, this); }

static Vm(pushk_jump) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

static Vm(curry2) {
  const size_t S = 3 + Width(struct tag);
  Have(S);
  PCell *k = (PCell*) Hp;
  Hp += S;
  k[0].ap = pushk_jump, k[1].x = *Sp++, k[2].m = Ip + 2;
  k[3].x = 0,   k[4].m = k;
  Ip = (PCell*) *Sp;
  *Sp = (PWord) k;
  return Continue(); }

Vm(jump) {
  Ip = Ip[1].m;
  return Continue(); }

Vm(pushk) {
  Have1();
  *--Sp = Ip[1].x;
  Ip += 2;
  return Continue(); }

Vm(cond) {
  Ip = nilp(*Sp) ? Ip[1].m : Ip + 2,
  Sp++;
  return Continue(); }

Vm(pushp) {
  Have1();
  Sp[-1] = Sp[getnum(Ip[1].x)];
  Sp--;
  Ip += 2;
  return Continue(); }
Vm(curry) {
  size_t n = getnum(Ip[1].x);
  if (n == 2) return Jump(curry2);
  const size_t S = 5 + Width(struct tag);
  Have(S);
  PCell *k = (PCell*) Hp;
  Hp += S;
  k[0].ap = curry, k[1].x = putnum(n - 1);
  k[2].ap = pushk_jump,  k[3].x = *Sp++, k[4].m = Ip + 2;
  k[5].x = 0,    k[6].m = k;
  Ip = (PCell*) *Sp;
  *Sp = (PWord) k;
  return Continue(); }


Vm(ret) {
  PWord n = getnum(Ip[1].x) + 1;
  return op(n, *Sp); }

Vm(yield) { return Pack(f), YieldStatus; }

Vm(ap) {
  if (nump(Sp[1])) return Ip++, Sp++, Continue();
  PCell *k = (PCell*) Sp[1];
  Sp[1] = (PWord) (Ip + 1);
  Ip = k;
  return Continue(); }

Vm(apn) {
  size_t n = getnum(Ip[1].x);
  PCell *ra = Ip + 2; // return address
  Ip = ((PCell*) Sp[n]) + 2; // only used by let form so will not be num
  Sp[n] = (PWord) ra; // store return address
  return Continue(); }

Vm(tap) {
  PWord x = Sp[0], j = Sp[1];
  Sp += getnum(Ip[1].x) + 1;
  if (nump(j)) return op(1, j);
  Ip = (PCell*) j;
  *Sp = x;
  return Continue(); }

Vm(tapn) {
  size_t n = getnum(Ip[1].x),
         r = getnum(Ip[2].x);
  Ip = ((PCell*) Sp[n]) + 2;
  PStack osp = Sp;
  Sp += r + 1;
  while (n--) Sp[n] = osp[n];
  return Continue(); }
