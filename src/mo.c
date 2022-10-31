#include "la.h"

// function functions
//
// functions are laid out in memory like this
//
// *|*|*|*|*|*|?|0|^
// * = function pointer or inline value
// ? = function name / metadata (optional)
// 0 = null
// ^ = pointer to head of function
//
// this way we can support internal pointers for branch
// destinations, return addresses, etc, while letting
// the garbage collector always find the head.
//
// two easy potential optimizations are:
// - add a tail pointer to the start of the function,
//   so GC can find the head quickly (since often we
//   won't have an internal pointer)
// - tag the tail/head pointers instead of using a null
//   sentinel (but then the C compiler would need to
//   align functions)

// allocate a thread
mo mkmo(la v, size_t n) {
  mo k = cells(v, n+2);
  if (k) G(k+n) = 0, G(k+n+1) = (vm*) k;
  return k; }

// get the tag at the end of a function
mo button(mo k) { while (G(k)) k = F(k); return k; }

// try to get the name of a function
ob hnom(la v, ob x) {
  vm *k = G(x);
  if (k == setclo || k == genclo0 || k == genclo1)
    return hnom(v, (ob) G(FF(x)));
  x = ((ob*) button((mo) x))[-1];
  return livep(v, x) ? x : nil; }

// instructions for the internal compiler
//
// initialize a function
Vm(hom_u) {
  ArityCheck(1);
  Check(nump(fp->argv[0]));
  size_t len = getnum(fp->argv[0]);
  Have(len);
  ob *k = setw(hp, nil, len);
  hp += len + 2;
  k[len] = 0;
  k[len+1] = (ob) k;
  return ApC(ret, (ob) (k + len)); }

// trim a function after writing out code
Vm(hfin_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(homp(xp) && G(xp) != disp);
  GF(button((mo) xp)) = (vm*) xp;
  return ApC(ret, xp); }

// emit code
Vm(emi_u) {
  ArityCheck(2);
  Check(nump(fp->argv[0]));
  Check(homp(fp->argv[1]));
  mo k = (mo) fp->argv[1] - 1;
  G(k) = (vm*) getnum(fp->argv[0]);
  return ApC(ret, (ob) k); }

// frameless version
Vm(emi) {
  mo k = (mo) *sp++ - 1;
  G(k) = (vm*) getnum(xp);
  return ApN(1, (ob) k); }

// emit data
Vm(emx_u) {
  ArityCheck(2);
  Check(homp(fp->argv[1]));
  mo k = (mo) fp->argv[1] - 1;
  G(k) = (vm*) fp->argv[0];
  return ApC(ret, (ob) k); }

// frameless
Vm(emx) {
  mo k = (mo) *sp++ - 1;
  G(k) = (vm*) xp;
  return ApN(1, (ob) k); }

// read an instruction from a thread (as a fixnum)
Vm(peeki_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(homp(xp));
  return ApC(ret, putnum(G(xp))); }

// read data from a thread (be sure it's really data!)
Vm(peekx_u) {
  ArityCheck(1);
  xp = fp->argv[0];
  Check(homp(xp));
  return ApC(ret, (ob) G(xp)); }

// thread pointer arithmetic -- not bounds checked!
Vm(seek_u) {
  ArityCheck(2);
  Check(homp(fp->argv[0]));
  Check(nump(fp->argv[1]));
  ip = (mo) fp->argv[0];
  xp = getnum(fp->argv[1]);
  return ApC(ret, (ob) (ip + xp)); }

// dispatch a data thread
// TODO maybe we could do this with closures instead?
Vm(disp) {
  return ApC(((mtbl) GF(ip))->does, xp); }

// closure functions
//
// pop some things off the stack into an array.
Vm(take) {
  ob *t, n = getnum((ob) GF(ip));
  Have(n + 2);
  t = cpyw(hp, sp, n);
  hp += n + 2;
  t[n] = 0;
  t[n+1] = (ob) t;
  return ApC(ret, (ob) t); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
Vm(genclo0) {
  ob *ec = (ob*) GF(ip), arg = ec[0];
  size_t adic = nilp(arg) ? 0 : getnum(G(arg));
  Have(Width(sf) + adic + 1);
  ob loc = ec[1];
  fr subd = fp;
  G(ip) = genclo1;
  sp = cpyw(sp - adic, (ob*) arg + 1, adic);
  ec = (ob*) GF(ip);
  sp = (ob*) (fp = (fr) sp - 1);
  fp->retp = ip;
  fp->subd = subd;
  fp->argc = adic;
  fp->clos = ec[2];
  if (!nilp(loc)) *--sp = loc;
  return ApY(ec[3], xp); }

// finalize function instance closure
Vm(genclo1) { return
  G(ip) = setclo,
  GF(ip) = (vm*) xp,
  ApY(ip, xp); }

// set the closure for this frame
Vm(setclo) { return
  fp->clos = (ob) GF(ip),
  ApY(G(FF(ip)), xp); }

// the next few functions create and store
// lexical environments.
// FIXME magic numbers
static Vm(enclose) {
  size_t m = fp->argc, n = m + (m ? 14 : 11);
  Have(n);
  ob x = (ob) GF(ip),
     arg = nil,
     *block = hp;
  hp += n;
  if (m) {
    n -= 11;
    arg = (ob) block;
    block += n;
    block[-2] = 0;
    block[-1] = arg;
    n -= 3;
    ((ob*) arg)[0] = putnum(n);
    while (n--) ((ob*) arg)[n+1] = fp->argv[n]; }

  ob *t = (ob*) block, // compiler thread closure array
     *at = t + 6, // compiler thread
     // TODO get closure out of stack frame; configure via xp
     loc = nilp(xp) ? xp : (ob) Locs,
     clo = fp->clos;

  t[0] = arg;
  t[1] = loc;
  t[2] = clo;
  t[3] = B(x);
  t[4] = 0;
  t[5] = (ob) t;

  at[0] = (ob) genclo0;
  at[1] = (ob) t;
  at[2] = A(x);
  at[3] = 0;
  at[4] = (ob) at;

  return ApN(2, (ob) at); }

// FIXME these pass the locals array to encl in xp
// this is a confusing optimization
Vm(encl1) { return ApC(enclose, putnum(1)); }
Vm(encl0) { return ApC(enclose, nil); }
