#include "i.h"
g_vm(g_vm_info) {
 size_t const req = 4 * Width(struct g_pair);
 Have(req);
 struct g_pair *si = (struct g_pair*) Hp;
 Hp += req;
 Sp[0] = word(si);
 ini_two(si, gputnum(f), word(si + 1));
 ini_two(si + 1, gputnum(f->len), word(si + 2));
 ini_two(si + 2, gputnum(Hp - ptr(f)), word(si + 3));
 ini_two(si + 3, gputnum(ptr(f) + f->len - Sp), g_nil);
 Ip += 1;
 return Continue(); }

op11(g_vm_clock, gputnum(g_clock()))
static g_vm(g_vm_uncurry) {
  Have1();
  *--Sp = Ip[1].x;
  Ip = Ip[2].m;
  return Continue(); }

g_vm(g_vm_curry) {
 union u *k = (union u*) Hp, *j = k;
 uintptr_t n = ggetnum(Ip[1].x);
 size_t S = 3 + Width(struct g_tag);

 if (n == 2) {
  Have(S);
  j[0].ap = g_vm_uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k; }

 else {
  S += 2;
  Have(S);
  j += 2;
  k[0].ap = g_vm_curry;
  k[1].x = gputnum(n - 1);
  j[0].ap = g_vm_uncurry;
  j[1].x = *Sp++;
  j[2].m = Ip + 2;
  j[3].x = 0;
  j[4].m = k; }

 Hp += S;
 Ip = cell(*Sp);
 Sp[0] = word(k);
 return Continue(); }

g_vm(g_vm_jump) {
 Ip = Ip[1].m;
 return Continue(); }

g_vm(g_vm_cond) {
 Ip = nilp(*Sp++) ? Ip[1].m : Ip + 2;
 return Continue(); }

// load instructions
//
g_vm(g_vm_quote) {
 Have1();
 Sp -= 1;
 Sp[0] = Ip[1].x;
 Ip += 2;
 return Continue(); }

g_vm(g_vm_eval) {
 Ip++;
 Pack(f);
 f = g_c0(f, g_vm_jump);
 return g_ok(f) ? (Unpack(f), Continue()) : f; }

g_vm(g_vm_data) {
 intptr_t x = word(Ip);
 Sp += 1;
 Ip = cell(Sp[0]);
 Sp[0] = x;
 return Continue(); }


// push a value from the stack
g_vm(g_vm_arg) {
 Have1();
 Sp[-1] = Sp[ggetnum(Ip[1].x)];
 Sp -= 1;
 Ip += 2;
 return Continue(); }

// call and return
// apply function to one argument
g_vm(g_vm_ap) {
 union u *k;
 if (odd(Sp[1])) Ip++, Sp++;
 else k = cell(Sp[1]), Sp[1] = word(Ip + 1), Ip = k;
 return Continue(); }

// tail call
g_vm(g_vm_tap) {
 intptr_t x = Sp[0], j = Sp[1];
 Sp += ggetnum(Ip[1].x) + 1;
 if (even(j)) Ip = cell(j), Sp[0] = x;
 else Sp += 1, Ip = cell(Sp[0]), Sp[0] = j;
 return Continue(); }

// apply to multiple arguments
g_vm(g_vm_apn) {
 size_t n = ggetnum(Ip[1].x);
 union u*ra = Ip + 2; // return address
 // this instruction is only emitted when the callee is known to be a function
 // so putting a value off the stack into Ip is safe. the +2 is cause we leave
 // the currying instruction in there... should be skipped in compiler instead FIXME
 Ip = cell(Sp[n]) + 2;
 Sp[n] = word(ra); // store return address
 return Continue(); }

// tail call
g_vm(g_vm_tapn) {
 size_t n = ggetnum(Ip[1].x),
        r = ggetnum(Ip[2].x);
 Ip = cell(Sp[n]) + 2;
 g_num *o = Sp;
 for (Sp += r + 1; n--; Sp[n] = o[n]);
 return Continue(); }

// return
g_vm(g_vm_ret) {
 g_num n = ggetnum(Ip[1].x) + 1;
 Ip = cell(Sp[n]);
 Sp[n] = Sp[0];
 Sp += n;
 return Continue(); }

g_vm(g_vm_ret0) { return
 Ip = cell(Sp[1]),
 Sp[1] = Sp[0],
 Sp += 1,
 Continue(); }

