#include "lisa.h"
#include "vm.h"

// " the virtual machine "
// It's a stack machine with one free register that runs on
// top of the C compiler's calling convention. This lets us
// keep the main state variables in CPU registers without
// needing to write any platform specific code, as long as
// the compiler passes enough arguments in registers and
// optimizes tail calls.

// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function

// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.

// calling and returning
Vm(ap_u) {
  ArityCheck(2);
  TypeCheck(Argv[0], Hom);
  ip = (mo) Argv[0];
  ob x = Argv[1];
  size_t adic = llen(x);
  Have(adic);
  ob off = fp->subd, rp = fp->retp;
  sp = Argv + getnum(Argc) - adic;
  for (size_t j = 0; j < adic; sp[j++] = A(x), x = B(x));
  fp = (fr) sp - 1,
  sp = (ob*) fp;
  fp->retp = rp;
  fp->argc = putnum(adic);
  fp->subd = off;
  fp->clos = nil;
  return ApY(ip, nil); }

Vm(vararg) {
  intptr_t reqd = getnum((ob) ip[1].ll),
           vdic = getnum(Argc) - reqd;
  ArityCheck(reqd);
  // in this case we need to add another argument
  // slot to hold the nil.
  if (!vdic) {
    Have1();
    cpyw((ob*)fp - 1, fp, Width(fr) + getnum(Argc));
    fp = (fr) ((ob*) fp - 1);
    sp = (ob*) fp;
    Argc += sizeof(ob);
    Argv[reqd] = nil;
    return ApN(2, xp); }
  // in this case we just keep the existing slots.
  // the path is knowable at compile time in many cases
  // so maybe vararg should be two or more different
  // functions.
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (size_t i = vdic; i--;
    t[i].a = fp->argv[reqd + i],
    t[i].b = puttwo(t+i+1));
  return
    t[vdic-1].b = nil,
    fp->argv[reqd] = puttwo(t),
    ApN(2, xp); }


// return from a function
Vm(ret) {
  ip = (mo) fp->retp;
  sp = (ob*) ((ob) fp->argv + fp->argc - Num);
  fp = (fr) ((ob) sp + fp->subd - Num);
  return ApY(ip, xp); }

// "inner" function call
Vm(call) {
  intptr_t adic = getnum((ob) ip[1].ll),
           off = (ob*) fp - (sp + adic);
  Have(Width(fr));
  fp = (fr) sp - 1;
  sp = (ob*) fp;
  fp->retp = (ob) (ip + 2);
  fp->subd = putnum(off);
  fp->clos = nil;
  fp->argc = putnum(adic);
  return ApY(xp, nil); }

// tail call
Vm(rec) {
  ip = (mo) ip[1].ll;
  if (Argc == (ob) ip) {
    cpyw(Argv, sp, getnum((ob) ip));
    sp = (ob*) fp;
    return ApY(xp, nil); }
  v->xp = fp->subd;
  v->ip = (mo) fp->retp; // save return info
  fp = (fr) (Argv + getnum(Argc - (ob) ip)),
  rcpyw(fp, sp, getnum((ob) ip)); // copy from high to low
  sp = (ob*) (--fp);
  fp->retp = (ob) v->ip;
  fp->argc = (ob) ip;
  fp->subd = v->xp;
  fp->clos = nil;
  return ApY(xp, nil); }

Vm(disp) { return ApY(((mtbl) GF(ip))->does, xp); }

////
/// Load Instructions
//

// constants
Vm(one) { return ApN(1, putnum(1)); }
Vm(zero) { return ApN(1, putnum(0)); }
// immediate value from thread
Vm(imm) { return xp = (ob) ip[1].ll, ApN(2, xp); }

// indexed references
#define Ref(b) (*(ob*)((ob)(b)+(ob)ip[1].ll-Num))
// pointer arithmetic works because fixnums are premultiplied by W

// function arguments
Vm(arg) { return xp = Ref(Argv), ApN(2, xp); }
Vm(arg0) { return ApN(1, Argv[0]); }
Vm(arg1) { return ApN(1, Argv[1]); }

// local variables
Vm(loc) { return xp = Ref(Locs), ApN(2, xp); }
Vm(loc0) { return ApN(1, Locs[0]); }
Vm(loc1) { return ApN(1, Locs[1]); }

// closure variables
Vm(clo) { return xp = Ref(Clos), ApN(2, xp); }
Vm(clo0) { return ApN(1, Clos[0]); }
Vm(clo1) { return ApN(1, Clos[1]); }

////
/// Store Instructions
// // stack push
Vm(push) {
  Have1();
  *--sp = xp;
  return ApN(1, xp); }

// dup top of stack
Vm(dupl) {
  Have1();
  --sp;
  sp[0] = sp[1];
  return ApN(1, xp); }

// set a local variable
Vm(loc_) { return Ref(Locs) = xp, ApN(2, xp); }

// set a module variable
Vm(tbind) {
  ob a = (ob) ip[1].ll;
  Pack();
  v->xp = tbl_set(v, A(a), B(a), xp);
  Unpack();
  return xp ? ApN(2, xp) : 0; }

// allocate local variable array
Vm(locals) {
  ob *t = hp, n = getnum((ob) ip[1].ll);
  Have(n + 2);
  hp += n + 2;
  setw(t, nil, n);
  t[n] = 0;
  *--sp = t[n+1] = (ob) t;
  return ApN(2, xp); }

// late binding
// long b/c it does the "static" type and arity checks
// that would have been done by the compiler if the function
// had been bound early.
Vm(latebind) {
  ob w = (ob) ip[1].ll, d = AB(w), typ = A(w) >> TagBits;
  xp = BB(w);
  w = tbl_get(v, d, xp);
  if (!w) return ApC(nom_err, xp);
  if (typ != sizeof(ob)) TypeCheck(w, typ);
  xp = w;
  // omit the arity check if possible
  if ((ip[2].ll == call || ip[2].ll == rec) && // xp will be a hom
      ptr(xp)[0] == (ob) arity &&
      ptr(ip)[3] >= ptr(xp)[1])
    xp = (ob) (ptr(xp) + 2);
  ip[0].ll = imm,
  ip[1].ll = (vm*) xp;
  return ApN(2, xp); }

// this is used to create closures.
Vm(take) {
  ob *t, n = getnum((ob) ip[1].ll);
  Have(n + 2);
  t = hp;
  hp += n + 2;
  cpyw(t, sp, n);
  sp += n;
  t[n] = 0;
  t[n+1] = (ob) t;
  return ApC(ret, (ob) t); }

Vm(clos) {
  fp->clos = (ob) ip[1].ll;
  return ApY((ob) ip[2].ll, xp); }

// finalize function instance closure
Vm(clos1) {
  ip->ll = clos;
  ip[1].ll = (vm*) xp;
  return ApY(ip, xp); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
Vm(clos0) {
  ob *ec = (ob*) ip[1].ll,
     arg = ec[0],
     loc = ec[1];
  size_t adic = nilp(arg) ? 0 : getnum(*(ob*)arg),
         req = Width(fr) + adic + 1;
  Have(req);

  intptr_t off = (ob*) fp - sp;
  ip->ll = clos1;
  sp -= adic;
  cpyw(sp, (ob*) arg + 1, adic);
  ec = (ob*) ip[1].ll;
  fp = (fr) sp - 1;
  sp = (ob*) fp;
  fp->retp = (ob) ip;
  fp->subd = putnum(off);
  fp->argc = putnum(adic);
  fp->clos = ec[2];
  if (!nilp(loc)) *--sp = loc;

  return ApY(ec[3], xp); }

// the next few functions create and store
// lexical environments.
// FIXME magic numbers
static Vm(encl) {
  intptr_t m = getnum(Argc),
           n = m + (m ? 14 : 11);
  Have(n);
  ob x = (ob) ip[1].ll,
     arg = nil,
     *block = hp;
  hp += n;
  if (m) {
    n -= 11;
    arg = (ob) block;
    block += n;
    for (ptr(arg)[n-2] = 0,
         ptr(arg)[n-1] = (ob) arg,
         n -= 3,
         ptr(arg)[0] = putnum(n);
         n--;
         ptr(arg)[n+1] = Argv[n]); }

  ob *t = (ob*) block, // compiler thread closure array
     *at = t + 6; // compiler thread

  t[0] = arg;
  t[1] = xp; // Locs or nil
  t[2] = fp->clos;
  t[3] = B(x);
  t[4] = 0;
  t[5] = (ob) t;

  at[0] = (ob) clos0;
  at[1] = (ob) t;
  at[2] = A(x);
  at[3] = 0;
  at[4] = (ob) at;

  return ApN(2, (ob) at); }

Vm(encll) { return ApC(encl, (ob) Locs); }
Vm(encln) { return ApC(encl, nil); }

////
/// Branch Instructions
//
// unconditional jump
Vm(jump) { return ApY((ob) ip[1].ll, xp); }

#include <string.h>
// isolate the more complicated logic from the simple
// pointer comparisons so eql() doesn't touch the stack
// unless it has to
static NoInline bool eql_two(two a, two b) {
  return eql(a->a, b->a) ? eql(a->b, b->b) : false; }
static NoInline bool eql_str(str a, str b) {
  return a->len == b->len && 0 == scmp(a->text, b->text); }
static NoInline bool eql_(ob a, ob b) {
  switch (TypeOf(a)) {
    case Two: return eql_two(gettwo(a), gettwo(b));
    case Str: return eql_str(getstr(a), getstr(b));
    default: return false; } }

bool eql(ob a, ob b) {
  return a == b ? true : TypeOf(a) != TypeOf(b) ? false : eql_(a, b); }

// XXX FIXME returning xp is wrong now that 0 = nil
Vm(lt) {
  xp = *sp++ < xp ? xp : nil;
  return ApN(1, xp); }
Vm(lteq) {
  xp = *sp++ <= xp ? xp : nil;
  return ApN(1, xp); }
Vm(eq) {
  xp = eql(xp, *sp++) ? T : nil;
  return ApN(1, xp); }
Vm(gteq) {
  xp = *sp++ >= xp ? xp : nil;
  return ApN(1, xp); }
Vm(gt) {
  xp = *sp++ > xp ? xp : nil;
  return ApN(1, xp); }

// FIXME remove macros
#define LT(a,b) (a<b)
#define LE(a,b) (a<=b)
#define GE(a,b) (a>=b)
#define GT(a,b) (a>b)
#define cmp(op, n) Vm(n##_u) {\
  ob n = getnum(Argc), *xs = Argv, m, *l;\
  switch (n) {\
    case 0: return ApC(ret, nil);\
    default: for (l = xs + n - 1, m = *xs; xs < l; m= *++xs)\
               if (!op(m, xs[1])) return ApC(ret, nil);\
    case 1: return ApC(ret, T); } }
cmp(LT, lt) cmp(LE, lteq) cmp(GE, gteq) cmp(GT, gt) cmp(eql, eq)

// type predicates
#define Tp(t)\
  Vm(t##pp) { return ApN(1, (t##p(xp)?T:nil)); }\
  Vm(t##p_u) {\
    for (ob *xs = Argv, *l = xs + getnum(Argc); xs < l;)\
      if (!t##p(*xs++)) return ApC(ret, nil);\
    return ApC(ret, T); }
Tp(num) Tp(hom) Tp(two) Tp(sym) Tp(str) Tp(tbl) Tp(nil)

// conditional jumps
//
// args: test, yes addr, yes val, no addr, no val
#define Br(nom, test, a, x, b, y) Vm(nom) {\
  return ApY((test) ? (ob) a(ip) : (ob) b(ip), x); }
// combined test/branch instructions
Br(branch, xp != nil, GF, xp, FF, xp)
Br(barnch, xp != nil, FF, xp, GF, xp)

Br(breq, eql(*sp++, xp), GF, T, FF, nil)
Br(brne, eql(*sp++, xp), FF, T, GF, nil)

Br(brlt,    *sp++ <  xp, GF, xp, FF, nil)
Br(brlt2,   *sp++ <  xp, FF, xp, GF, nil)
Br(brlteq,  *sp++ <= xp, GF, xp, FF, nil)
Br(brlteq2, *sp++ <= xp, FF, xp, GF, nil)
Br(brgt,    *sp++ >  xp, GF, xp, FF, nil)
Br(brgt2,   *sp++ >  xp, FF, xp, GF, nil)
Br(brgteq,  *sp++ >= xp, GF, xp, FF, nil)
// brgteq2 is brlt
//
// type/arity checking
Vm(idZ) { return nump(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(idH) { return homp(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(idT) { return tblp(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(id2) { return twop(xp) ? ApN(1, xp) : ApC(dom_err, xp); }
Vm(arity) {
  ob reqd = (ob) ip[1].ll;
  return fp->argc >= reqd ? ApN(2, xp) : ApC(ary_err, reqd); }

// errors
Vm(dom_err) { return Pack(), nope(v, "is undefined"); }
Vm(oom_err) { return Pack(), nope(v, "oom with %d words", v->len); }
Vm(ary_err) { return Pack(), nope(v, "takes %d parameters", getnum(xp)); }
Vm(nom_err) {
  xp = getsym(xp)->nom;
  return Pack(),
    nope(v, "referenced free variable `%s'", nilp(xp) ? 0 : getstr(xp)->text); }