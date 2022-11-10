#include "la.h"
#include "two.h"
#include "mo.h"
#include "tbl.h"
#include "vm.h"
#include "ns.h"
#include <string.h>

////
/// Load Instructions
//

// immediate values
Vm(imm) { return ApN(2, (ob) GF(ip)); }
Vm(imm0) { return ApN(1, putnum(0)); }
Vm(imm1) { return ApN(1, putnum(1)); }
Vm(imm2) { return ApN(1, putnum(2)); }
Vm(immn1) { return ApN(1, putnum(-1)); }

// function arguments
Vm(argn) { return ApN(2, fp->argv[getnum(GF(ip))]); }
Vm(arg0) { return ApN(1, fp->argv[0]); }
Vm(arg1) { return ApN(1, fp->argv[1]); }
Vm(arg2) { return ApN(1, fp->argv[2]); }
Vm(arg3) { return ApN(1, fp->argv[3]); }

// the first two stack slots under the current frame
// may hold extra call data.
#define Slot1 ((ob**)fp)[-1]
#define Slot2 ((ob**)fp)[-2]

// local variables
Vm(sl1n) { return ApN(2, Slot1[getnum(GF(ip))]); }
Vm(sl10) { return ApN(1, Slot1[0]); }
Vm(sl11) { return ApN(1, Slot1[1]); }
Vm(sl12) { return ApN(1, Slot1[2]); }
Vm(sl13) { return ApN(1, Slot1[3]); }

// closure variables
Vm(clon) { return ApN(2, fp->clos[getnum(GF(ip))]); }
Vm(clo0) { return ApN(1, fp->clos[0]); }
Vm(clo1) { return ApN(1, fp->clos[1]); }
Vm(clo2) { return ApN(1, fp->clos[2]); }
Vm(clo3) { return ApN(1, fp->clos[3]); }

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
Vm(defsl1) { return
  Slot1[getnum(GF(ip))] = xp,
  ApN(2, xp); }

// set a module variable
Vm(deftop) {
  bool _;
  CallOut(_ = tbl_set(v, (tbl) A(GF(ip)), B(GF(ip)), xp));
  return _ ? ApN(2, xp) : ApC(xoom, xp); }

// allocate local variable array
Vm(setloc) {
  size_t n = getnum((ob) GF(ip));
  // + 1 for the stack slot
  Have(n + wsizeof(struct tag) + 1);
  mo t = memset(ini_mo(hp, n), -1, sizeof(ob) * n);
  hp += n + wsizeof(struct tag);
  *--sp = (ob) t;
  return ApN(2, xp); }

// late binding
// TODO dynamic type checking here
Vm(late) {
  ob w = (ob) GF(ip), d = A(w);
  xp = B(w);
  w = ns_seek(v, (tbl) d, xp); // FIXME call name resolve procedure
  if (!w) return ApC(xnom, xp);
  xp = w;
  // omit the arity check if possible
  vm *n = G(FF(ip));
  if ((n == call || n == rec) && // xp will be a hom
      G(xp) == arity &&
      (ob) GF(FF(ip)) >= (ob) GF(xp))
    xp = (ob) FF(ip);
  G(ip) = imm;
  GF(ip) = (vm*) xp;
  return ApN(2, xp); }

// varargs
static NoInline Vm(varg0) {
  Have1();
  fp = memmove((ob*) fp - 1, fp, sizeof(struct sf) + sizeof(ob) * fp->argc);
  sp = (ob*) fp;
  fp->argv[fp->argc++] = nil;
  return ApN(2, xp); }

Vm(varg) {
  size_t reqd = getnum((ob) GF(ip));
  if (reqd == fp->argc) return ApC(varg0, xp);
  if (reqd > fp->argc) return ApC(xary, putnum(reqd));
  size_t vdic = fp->argc - reqd;
  // in this case we need to add another argument
  // slot to hold the nil.
  // in this case we just keep the existing slots.
  Have(wsizeof(struct two) * vdic);
  two t = (two) hp;
  hp += wsizeof(struct two) * vdic;
  for (size_t i = vdic; i--;
    ini_two(t + i, fp->argv[reqd + i], (ob) (t + i + 1)));
  t[vdic-1].b = nil;
  fp->argv[reqd] = (ob) t;
  return ApN(2, xp); }
