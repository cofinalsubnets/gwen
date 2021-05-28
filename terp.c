#include "lips.h"
#include "terp.h"
// " the virtual machine "
// it's a stack machine with one free register that's
// implemented on top of the C compiler's calling convention.
// this allows us to keep the most important state variables
// in CPU registers at all times while the interpreter is
// running, without any platform-specific code.

// " the interpreter "
// is all the functions of type terp:
#define Vm(n,...) NoInline obj \
  n(lips v, obj ip, mem fp, mem sp, mem hp, obj xp, ##__VA_ARGS__)
// the arguments to a terp function collectively represent the
// runtime state, and the  return value is the result of the
// program. there are six arguments because that's the number
// that the prevalent unix calling convention on AMD64 (System
// V ABI) says should be passed in registers; that's the only
// reason why there aren't more. but it's not too bad; the six
// arguments are:
// - v  : vm instance pointer ; most lips functions take this as the first argument
// - ip : instruction pointer ; the current vm instruction ; function pointer pointer
// - fp : frame pointer       ; current function context
// - sp : stack pointer       ; data/call stack
// - hp : heap pointer        ; the next free heap location
// - xp : return value        ; general-purpose register

// when the interpreter isn't running, the state variables that
// would normally be in registers are stored in slots on the
// vm structure. however while the interpreter is running it
// uses these struct slots to pass and return extra values
// without using the stack. so the interpreter has to restore
// the current values in the vm struct before it makes any
// "external" function calls.
#define Pack() (Ip=ip,Sp=sp,Hp=hp,Fp=fp,Xp=xp)
#define Unpack() (fp=Fp,hp=Hp,sp=Sp,ip=Ip,xp=Xp)
#define CallC(...)(Pack(),(__VA_ARGS__),Unpack())

// the frame structure holds the current function context.
typedef struct fr { obj clos, retp, subd, argc, argv[]; } *fr;
#define ff(x)((fr)(x))
#define Clos ff(fp)->clos
#define Retp ff(fp)->retp
#define Subd ff(fp)->subd
#define Argc ff(fp)->argc
#define Argv ff(fp)->argv
// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer.
#define Locs fp[-1]
// if a function has locals, this will have been initialized by the
// by the time they are referred to. the wrinkle in the representation
// gives a small but significant benefit to general function call
// performance and should be extended to the closure pointer, which is often
// nil.

// the return value of a terp function is usually a call
// to another terp function.
#define Jump(f,...) return (f)(v,ip,fp,sp,hp,xp,##__VA_ARGS__)
#define Cont(n, x) return ip+=w2b(n), xp=x, G(ip)(v,ip,fp,sp,hp,xp)
#define Ap(f,x) return ip=f,G(ip)(v,ip,fp,sp,hp,x)
#define Go(f,x) return f(v,ip,fp,sp,hp,x)
#define N(n) ip+=w2b(n);Ap(ip, xp)
// the C compiler has to optimize tail calls in terp functions
// or the stack will grow every time an instruction happens!

// a special case is when garbage collection is necessary.
// this occurs near the beginning of a function. if enough
// memory is not available the interpret jumps to a specific
// terp function
static Vm(gc) { i64 n = Xp; CallC(reqsp(v, n)); N(0); }
// that stores the state and calls the garbage collector;
// afterwards it jumps back to the instruction that called it.
// therefore anything before the Have() macro will be executed
// twice if garbage collection happens! there should be no side
// effects before Have() or similar.
#define avail (sp-hp)
#define Have(n) if (avail < n) Jump((Xp=n,gc))
#define Have1() if (hp == sp) Jump((Xp=1,gc)) // common case, faster comparison

// the interpreter takes a very basic approach to error
// handling: something is wrong? jump to nope().
static Vm(nope);
#define TyCh(x,t) if(kind((x)-(t)))Jump(nope) // type check
#define ArCh(n) if(Pn(n)>Argc)Jump(nope)

// " virtual machine instructions "
//
// load instructions
Vm(imm) { xp = (obj) GF(ip); N(2); }
// common constants
Vm(unit) { xp = nil;   N(1); }
Vm(one)  { xp = Pn(1); N(1); }
Vm(zero) { xp = Pn(0); N(1); }

// indexed load instructions
// this pointer arithmetic works because fixnums are
// premultiplied by W
#define fast_idx(b) (*(Z*)((Z)(b)+(Z)GF(ip)-Num))
Vm(arg)  { xp = fast_idx(Argv);     N(2); }
Vm(arg0) { xp = Argv[0];            N(1); }
Vm(arg1) { xp = Argv[1];            N(1); }
Vm(loc)  { xp = fast_idx(AR(Locs)); N(2); }
Vm(loc0) { xp = AR(Locs)[0];        N(1); }
Vm(loc1) { xp = AR(Locs)[1];        N(1); }
Vm(clo)  { xp = fast_idx(AR(Clos)); N(2); }
Vm(clo0) { xp = AR(Clos)[0];        N(1); }
Vm(clo1) { xp = AR(Clos)[1];        N(1); }

// store instructions
Vm(push) { Have1(); *--sp = xp; N(1); } // stack push
Vm(loc_) { fast_idx(AR(Locs)) = xp; N(2); } // set a local variable
Vm(tbind) { CallC(tblset(v, Dict, Ob GF(ip), xp)); N(2); }

// initialize local variable slots
Vm(locals) {
 i64 n = Gn(GF(ip));
 Have(n + 2);
 vec t = (Ve) hp;
 fill(t->xs, nil, t->len = n);
 hp += n + 1;
 *--sp = puttup(t);
 N(2); }

// late bind
// this function is a lil complicated, because it incorporates
// the "static" type and arity checking that would have been
// done by the compiler if the function had been bound early.
Vm(lbind) {
 obj w = Ob GF(ip),
     d = XY(w), y = X(w);
 if (!(w = tblget(v, d, xp = YY(w)))) Jump(nope); // free variable
 xp = w;
 if (Gn(y) != 8) TyCh(xp, Gn(y)); // do the type check
 terp *q = G(FF(ip)); // omit the arity check if possible
 if (q == call || q == rec) {
  obj aa = (obj) GF(FF(ip));
  if (G(xp) == arity && aa >= (obj) GF(xp))
   xp += W2; }
 G(ip) = imm;
 GF(ip) = (T) xp;
 N(2); }

// control flow instructions
// return to C
Vm(yield) { Pack(); return xp; }

// conditional jumps
Vm(branch) { Ap(xp == nil ? (obj) FF(ip) : (obj) GF(ip), xp); }
Vm(barnch) { Ap(xp == nil ? (obj) GF(ip) : (obj) FF(ip), xp); }
// relational jumps
Vm(brlt)   { Ap(*sp++ <  xp    ? (obj) GF(ip) : (obj) FF(ip), xp); }
Vm(brgteq) { Ap(*sp++ <  xp    ? (obj) FF(ip) : (obj) GF(ip), xp); }
Vm(brlteq) { Ap(*sp++ <= xp    ? (obj) GF(ip) : (obj) FF(ip), xp); }
Vm(brgt)   { Ap(*sp++ <= xp    ? (obj) FF(ip) : (obj) GF(ip), xp); }
Vm(breq)   { Ap(eql(*sp++, xp) ? (obj) GF(ip) : (obj) FF(ip), xp); }
Vm(brne)   { Ap(eql(*sp++, xp) ? (obj) FF(ip) : (obj) GF(ip), xp); }

// unconditional jumps
Vm(jump) { Ap((obj) GF(ip), xp); }
Vm(clos) { Clos = (obj) GF(ip); Ap((obj) G(FF(ip)), xp); }

// return from a function
Vm(ret) {
 ip = Retp;
 sp = (M) ((Z) Argv + Argc - Num);
 fp = (M) ((Z)   sp + Subd - Num);
 N(0); }

// regular function call
Vm(call) {
#define fwds (sizeof(struct fr)/W)
 Have(fwds);
 obj adic = (obj) GF(ip);
 i64 off = fp - (M) ((Z) sp + adic - Num);
 fp = sp -= fwds;
 Retp = Ph(ip+W2);
 Subd = Pn(off);
 Clos = nil;
 Argc = adic;
 Ap(xp, nil); }

// general tail call
Vm(rec) {
 i64 adic = Gn(GF(ip));
 if (Argc == (obj) GF(ip)) {
  for (mem p = Argv; adic--; *p++ = *sp++);
  sp = fp;
  Ap(xp, nil); }

 obj off = Subd, rp = Retp; // save return info
 mem src = sp + adic;
 // overwrite current frame with new frame
 sp = Argv + Gn(Argc);
 // important to copy in reverse order since they
 // may overlap
 for (i64 i = adic; i--; *--sp = *--src);
 fp = sp -= Size(fr);
 Retp = rp;
 Argc = Pn(adic);
 Subd = off;
 Clos = nil;
 Ap(xp, nil); }

// type/arity checking
#define arn(n) if(n>Argc)Jump(nope)
#define tcn(k) {if(kind(xp-k))Jump(nope);}
Vm(idZ) { tcn(Num); N(1); }
Vm(id2) { tcn(Two); N(1); }
Vm(idH) { tcn(Hom); N(1); }
Vm(idT) { tcn(Tbl); N(1); }
Vm(arity) { arn(Ob GF(ip)); N(2); }

// continuations
//
// this is a simple but expensive way of doing continuations.
// it would be more memory efficient to do a copy-on-write
// kind of thing where the stack is only copied if the function
// returns. a spaghetti stack would be another option but it
// would be slower. faster continuations at the cost of slower
// function calls seems like a bad deal given the relative
// frequency of the two.
Vm(ccc_u) {
 obj x;
 ArCh(1);
 TyCh(x = Argv[0], Hom);
 // we need space for:
 // the entire stack
 // the frame offset
 // the length (to put it all in a tuple)
 // the continuation thread (4 words)
 i64 ht = Pool + Len - sp;
 Have(ht + 6);
 vec t = (vec) hp;
 hp += ht + 2;
 t->len = ht + 1;
 t->xs[0] = Pn(fp - sp);
 wcpy(t->xs+1, sp, ht);
 hom c = (hom) hp;
 hp += 4;
 c[0] = cont;
 c[1] = (T) puttup(t);
 c[2] = NULL;
 c[3] = (T) c;
 Argv[0] = Ph(c);
 Ap(x, nil); }

// call a continuation
Vm(cont) {
 vec t = gettup(GF(ip));
 Have(t->len - 1);
 xp = Gn(Argc) == 0 ? nil : *Argv;
 i64 off = Gn(t->xs[0]);
 sp = Pool + Len - (t->len - 1);
 fp = sp + off;
 wcpy(sp, t->xs+1, t->len-1);
 Jump(ret); }

Vm(ap_u) {
 ArCh(2);
 obj x = Argv[0], y = Argv[1];
 TyCh(x, Hom);
 i64 adic = llen(y);
 Have(adic);
 obj off = Subd, rp = Retp;
 sp = Argv + Gn(Argc) - adic;
 for (Z j = 0; j < adic; y = Y(y)) sp[j++] = X(y);
 fp = sp -= Size(fr);
 Retp = rp;
 Argc = Pn(adic);
 Subd = off;
 Clos = nil;
 Ap(x, nil); }

// instructions used by the compiler
Vm(hom_u) {
 obj x;
 ArCh(1);
 TyCh(x = *Argv, Num);
 i64 len = Gn(x) + 2;
 Have(len);
 hom h = (hom) hp;
 hp += len;
 fill((M) h, nil, len);
 h[len-1] = (T) h;
 h[len-2] = NULL;
 Go(ret, Ph(h+len-2)); }

Vm(tset) {
 obj x = *sp++, y = *sp++;
 CallC(x = tblset(v, xp, x, y));
 Ap(ip+W, x); }

Vm(emx) {
 obj h = *sp++ - W;
 G(h) = (T) xp;
 Ap(ip+W, h); }

Vm(emi) {
 obj h = *sp++ - W;
 G(h) = (terp*) Gn(xp);
 Ap(ip+W, h); }

Vm(emx_u) {
 ArCh(2);
 TyCh(Argv[1], Hom);
 obj h = Argv[1] - W;
 G(h) = (T) Argv[0];
 Go(ret, h); }

Vm(emi_u) {
 ArCh(2);
 TyCh(Argv[0], Num);
 TyCh(Argv[1], Hom);
 obj h = Argv[1] - W;
 G(h) = (terp*) Gn(Argv[0]);
 Go(ret, h); }

Vm(hom_geti_u) {
 ArCh(1);
 TyCh(Argv[0], Hom);
 Go(ret, Pn(G(Argv[0]))); }

Vm(hom_getx_u) {
 ArCh(1);
 TyCh(Argv[0], Hom);
 Go(ret, (obj) G(Argv[0])); }

Vm(hom_seek_u) {
 ArCh(2);
 TyCh(Argv[0], Hom);
 TyCh(Argv[1], Num);
 Go(ret, Ph(Gh(Argv[0])+Gn(Argv[1]))); }

// hash tables
Vm(tblg) {
 ArCh(2);
 TyCh(Argv[0], Tbl);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? xp : nil); }

Vm(tget) {
 xp = tblget(v, xp, *sp++);
 Ap(ip+W, xp ? xp : nil); }

#define ok Pn(1)
Vm(thas)  { xp = tblget(v, xp, *sp++) ? ok : nil; N(1); }
Vm(tlen)  { xp = putnum(gettbl(xp)->len); N(1); }
Vm(tkeys) { O x; CallC(x = tblkeys(v, xp)); xp = x; N(1); }

Vm(tblc) {
 ArCh(2);
 TyCh(Argv[0], Tbl);
 xp = tblget(v, Argv[0], Argv[1]);
 Go(ret, xp ? Pn(0) : nil); }

static obj tblss(lips v, i64 i, i64 l) {
 mem fp = Fp;
 return i > l-2 ? Argv[i-1] :
  (tblset(v, Xp, Argv[i], Argv[i+1]),
   tblss(v, i+2, l)); }

Vm(tbls) {
 obj x = nil;
 ArCh(1);
 TyCh(xp = *Argv, Tbl);
 CallC(x = tblss(v, 1, Gn(Argc)));
 Go(ret, x); }

Vm(tblmk) {
 CallC(Xp = table(v), tblss(v, 0, Gn(Argc)));
 Go(ret, Xp); }

Vm(tbld) {
 obj x = nil;
 ArCh(2);
 TyCh(Argv[0], Tbl);
 CallC(x = tbldel(v, Argv[0], Argv[1]));
 Go(ret, x); }

Vm(tblks) {
 ArCh(1);
 TyCh(Argv[0], Tbl);
 obj x;
 CallC(x = tblkeys(v, Argv[0]));
 Go(ret, x); }

Vm(tbll) {
 ArCh(1);
 TyCh(Argv[0], Tbl);
 Go(ret, Pn(gettbl(*Argv)->len)); }

// string instructions
Vm(strl) {
 ArCh(1);
 TyCh(*Argv, Oct);
 Go(ret, Pn(getoct(*Argv)->len-1)); }

Vm(strg) {
 ArCh(2);
 TyCh(Argv[0], Oct);
 TyCh(Argv[1], Num);
 Go(ret, Gn(Argv[1]) < getoct(Argv[0])->len-1 ?
  Pn(getoct(Argv[0])->text[Gn(Argv[1])]) :
  nil); }

Vm(strc) {
 i64 l = Gn(Argc), sum = 0, i = 0;
 while (i < l) {
  obj x = Argv[i++];
  TyCh(x, Oct);
  sum += getoct(x)->len - 1; }
 i64 words = b2w(sum+1) + 1;
 Have(words);
 str d = (str) hp;
 hp += words;
 d->len = sum + 1;
 d->text[sum] = 0;
 while (i) {
  str x = getoct(Argv[--i]);
  sum -= x->len - 1;
  bcpy(d->text+sum, x->text, x->len - 1); }
 Go(ret, putoct(d)); }

#define min(a,b)(a<b?a:b)
#define max(a,b)(a>b?a:b)
Vm(strs) {
 ArCh(3);
 TyCh(Argv[0], Oct);
 TyCh(Argv[1], Num);
 TyCh(Argv[2], Num);
 str src = getoct(Argv[0]);
 i64 lb = Gn(Argv[1]), ub = Gn(Argv[2]);
 lb = max(lb, 0);
 ub = max(min(ub, src->len-1), lb);
 i64 words = 1 + b2w(ub - lb + 1);
 Have(words);
 str dst = (str) hp;
 hp += words;
 dst->len = ub - lb + 1;
 dst->text[ub - lb] = 0;
 bcpy(dst->text, src->text + lb, ub - lb);
 Go(ret, putoct(dst)); }

Vm(strmk) {
 i64 i, l = Gn(Argc)+1, size = 1 + b2w(l);
 Have(size);
 str s = (oct) hp;
 hp += size;
 for (i = 0; i < l-1; i++) {
  obj x = Argv[i];
  TyCh(x, Num);
  if (x == Pn(0)) break;
  s->text[i] = Gn(x); }
 s->text[i] = 0;
 s->len = i+1;
 Go(ret, putoct(s)); }

Vm(vararg) {
 i64 reqd = Gn(GF(ip)),
     vdic = Gn(Argc) - reqd;
 ArCh(reqd);
 // in this case we need to add another argument
 // slot to hold the nil.
 if (!vdic) {
  Have1();
  sp = --fp;
  for (Z i = 0; i < Size(fr) + reqd; i++)
   fp[i] = fp[i+1];
  Argc += W;
  Argv[reqd] = nil; }
 // in this case we just keep the existing slots.
 // the path is knowable at compile time in many cases
 // so maybe vararg should be two or more different
 // functions.
 else {
  Have(2 * vdic);
  two t = (two) hp;
  hp += 2 * vdic;
  for (Z i = vdic; i--;)
   t[i].x = Argv[reqd + i],
   t[i].y = puttwo(t+i+1);
  t[vdic-1].y = nil;
  Argv[reqd] = puttwo(t); }
 N(2); }

// the next few functions create and store
// lexical environments.
static Vm(encl) {
 i64 n = Xp;
 obj x = Ob GF(ip);
 mem block = hp;
 hp += n;
 obj arg = nil; // optional argument array
 if (n > 11) {
  n -= 12;
  vec t = (vec) block;
  block += 1 + n;
  t->len = n;
  while (n--) t->xs[n] = Argv[n];
  arg = puttup(t); }

 vec t = (vec) block; // compiler thread closure array (1 length 5 elements)
 hom at = (hom) (block+6); // compiler thread (1 instruction 2 data 2 tag)

 t->len = 5; // initialize alpha closure
 t->xs[0] = arg;
 t->xs[1] = xp; // Locs or nil
 t->xs[2] = Clos;
 t->xs[3] = Y(x);
 t->xs[4] = Ph(at);

 at[0] = pc0;
 at[1] = (T) puttup(t);
 at[2] = (T) X(x);
 at[3] = 0;
 at[4] = (T) at;

 Ap(ip+W2, Ph(at)); }

Vm(prencl) {
 i64 n = Gn(Argc);
 n += n ? 12 : 11;
 Have(n);
 Xp = n;
 Jump(encl); }

Vm(encll) { Go(prencl, Locs); }
Vm(encln) { Go(prencl, nil); }

// this function is run the first time a user
// function with a closure is called. its
// purpose is to reconstruct the enclosing
// environment and call the closure constructor
// thread generated by the compiler. afterwards
// it overwrites itself with a special jump
// instruction that sets the closure and enters
// the function.
Vm(pc0) {
 obj ec = Ob GF(ip),
     arg = AR(ec)[0],
     loc = AR(ec)[1];
 i64 adic = nilp(arg) ? 0 : AL(arg);
 Have(Size(fr) + adic + 1);
 i64 off = (M) fp - sp;
 G(ip) = pc1;
 sp -= adic;
 for (Z z = adic; z--; sp[z] = AR(arg)[z]);
 ec = Ob GF(ip);
 fp = sp -= Size(fr);
 Retp = ip;
 Subd = Pn(off);
 Argc = Pn(adic);
 Clos = AR(ec)[2];
 if (!nilp(loc)) *--sp = loc;
 ip = AR(ec)[3];
 N(0); }

// finalize function instance closure
Vm(pc1) {
 G(ip) = clos;
 GF(ip) = (T) xp;
 N(0); }

// this is used to create closures.
Vm(take) {
 Z n = Gn(Ob GF(ip));
 Have(n + 1);
 vec t = (vec) hp;
 hp += n + 1;
 t->len = n;
 wcpy(t->xs, sp, n);
 sp += n;
 Go(ret, puttup(t)); }

// print to console
Vm(em_u) {
 i64 l = Gn(Argc), i;
 if (l) {
  for (i = 0; i < l - 1; i++)
   emsep(v, Argv[i], stdout, ' ');
  emit(v, xp = Argv[i], stdout); }
 fputc('\n', stdout);
 Jump(ret); }

// pairs
Vm(cons) {
 Have1(); hp[0] = xp, hp[1] = *sp++;
 xp = puttwo(hp); hp += 2; N(1); }
Vm(car) { Ap(ip+W, X(xp)); }
Vm(cdr) { Ap(ip+W, Y(xp)); }

Vm(cons_u) {
 i64 aa = Gn(Argc);
 if (!aa) Jump(nope);
 Have(2); hp[0] = Argv[0], hp[1] = aa == 1 ? nil : Argv[1];
 xp = puttwo(hp), hp += 2; Jump(ret); }
Vm(car_u) { ArCh(1); TyCh(*Argv, Two); Go(ret, X(*Argv)); }
Vm(cdr_u) { ArCh(1); TyCh(*Argv, Two); Go(ret, Y(*Argv)); }

// arithmetic
Vm(neg) { Ap(ip+W, Pn(-Gn(xp))); }
Vm(add) { xp = xp + *sp++ - Num; N(1); }
Vm(sub) { xp = *sp++ - xp + Num; N(1); }
Vm(mul) { xp = Pn(Gn(xp) * Gn(*sp++)); N(1); }
Vm(dqv) {
 if (xp == Pn(0)) Jump(nope);
 xp = Pn(Gn(*sp++) / Gn(xp));
 N(1); }
Vm(mod) {
 if (xp == Pn(0)) Jump(nope);
 xp = Pn(Gn(*sp++) % Gn(xp));
 N(1); }

#define mm_u(_c,_v,_z,op){\
 O x,m=_z,*xs=_v,*l=xs+_c;\
 if (_c) for(;xs<l;m=m op Gn(x)){\
  x = *xs++; TyCh(x, Num);}\
 Go(ret, Pn(m));}
#define mm_u0(_c,_v,_z,op){\
 O x,m=_z,*xs=_v,*l=xs+_c;\
 if (_c) for(;xs<l;m=m op Gn(x)){\
  x = *xs++; TyCh(x, Num);\
  if (x == Pn(0)) Jump(nope);}\
 Go(ret, Pn(m));}

Vm(add_u) {
 mm_u(Gn(Argc), Argv, 0, +); }
Vm(mul_u) {
 mm_u(Gn(Argc), Argv, 1, *); }
Vm(sub_u) {
 i64 i = Gn(Argc);
 if (i == 0) Go(ret, Pn(0));
 TyCh(*Argv, Num);
 if (i == 1) Go(ret, Pn(-Gn(*Argv)));
 mm_u(i-1,Argv+1,Gn(Argv[0]),-); }

Vm(div_u) {
 Z i = Gn(Argc);
 if (i == 0) Go(ret, Pn(1));
 TyCh(*Argv, Num);
 mm_u0(i-1,Argv+1,Gn(*Argv),/); }
Vm(mod_u) {
 Z i = Gn(Argc);
 if (i == 0) Go(ret, Pn(1));
 TyCh(*Argv, Num);
 mm_u0(i-1,Argv+1,Gn(*Argv),%); }

#define Tf(x) ((x)?ok:nil)
// type predicates
Vm(numpp) { Ap(ip+W, Tf(nump(xp))); }
Vm(hompp) { Ap(ip+W, Tf(homp(xp))); }
Vm(twopp) { Ap(ip+W, Tf(twop(xp))); }
Vm(sympp) { Ap(ip+W, Tf(symp(xp))); }
Vm(strpp) { Ap(ip+W, Tf(octp(xp))); }
Vm(tblpp) { Ap(ip+W, Tf(tblp(xp))); }
Vm(nilpp) { Ap(ip+W, Tf(nilp(xp))); }
Vm(vecpp) { Ap(ip+W, Tf(tupp(xp))); }


// pairs are immutable, so we can take this opportunity to
// deduplicate them.
static int twoeq(O a, O b) {
 if (!eql(X(a), X(b))) return false;
 X(a) = X(b);
 if (!eql(Y(a), Y(b))) return false;
 Y(a) = Y(b);
 return true; }

static int streq(obj a, obj b) {
 str o = getoct(a), m = getoct(b);
 if (o->len != m->len) return false;
 for (Z i = 0; i < o->len; i++)
  if (o->text[i] != m->text[i]) return false;
 return true; }

int eql(obj a, obj b) {
 if (a == b)             return true;
 if (kind(a) != kind(b)) return false;
 switch (kind(a)) {
  case Two: return twoeq(a, b);
  case Oct: return streq(a, b);
  default:  return false; } }

Vm(lt)    { xp = *sp++ <  xp ? xp : nil; N(1); }
Vm(lteq)  { xp = *sp++ <= xp ? xp : nil; N(1); }
Vm(gteq)  { xp = *sp++ >= xp ? xp : nil; N(1); }
Vm(gt)    { xp = *sp++ >  xp ? xp : nil; N(1); }
// there should be a separate instruction for simple equality.
Vm(eq)    { xp = eql(xp, *sp++) ? ok : nil; N(1); }

#define ord_w(r){\
 obj n=Gn(Argc),*xs=Argv,m,*l;\
 switch (n){\
  case 0: no: Go(ret, nil);\
  case 1: break;\
  default: for (l=xs+n-1,m=*xs;xs<l;m=*++xs)\
           if(!(m r xs[1])) goto no;}\
 Go(ret, ok);}

#define ord_wv(r){\
 obj n=Gn(Argc),*xs=Argv,m,*l;\
 switch(n) {\
  case 0: Go(ret, nil);\
  case 1: break;\
  default: for (l=xs+n-1,m=*xs;xs<l;m=*++xs)\
           if (!(r(m,xs[1]))) Go(ret, nil);}\
 Go(ret, ok);}

#define ord_v(r) Go(ret, ord_u(Gn(Argc), Argv, r))

Vm(lt_u)   { ord_w(<); }
Vm(lteq_u) { ord_w(<=); }
Vm(eq_u)   { ord_wv(eql); }
Vm(gteq_u) { ord_w(>=); }
Vm(gt_u)   { ord_w(>); }

#define typpp(t) {\
 for (obj *xs = Argv, *l=xs+Gn(Argc);xs<l;)\
  if (kind(*xs++)!=t) Go(ret, nil);\
 Go(ret, ok); }
Vm(nump_u) { typpp(Num); }
Vm(homp_u) { typpp(Hom); }
Vm(strp_u) { typpp(Oct); }
Vm(tblp_u) { typpp(Tbl); }
Vm(twop_u) { typpp(Two); }
Vm(symp_u) { typpp(Sym); }
Vm(nilp_u) { typpp(Nil); }
Vm(vecp_u) { typpp(Tup); }

// stack manipulation
Vm(tuck) { Have1(); sp--, sp[0] = sp[1], sp[1] = xp; N(1); }
Vm(drop) { sp++; N(1); }
Vm(dupl) { Have1(); --sp; sp[0] = sp[1]; N(1); }

// errors
Vm(fail) { Jump(nope); }
Vm(zzz) { exit(EXIT_SUCCESS); }
Vm(gsym_u) {
 Have(sizeof(struct sym)/W);
 sym y = (sym) hp;
 hp += Size(sym);
 y->nom = y->l = y->r = nil;
 y->code = v->count++ * mix;
 Go(ret, putsym(y)); }

Vm(hfin_u) {
 ArCh(1);
 TyCh(*Argv, Hom);
 obj a = *Argv;
 GF(button(Gh(a))) = (terp*) a;
 Go(ret, a); }

Vm(ev_u) {
 ArCh(1);
 obj x;
 CallC(x = compile(v, *Argv),
       x = G(x)(v, x, Fp, Sp, Hp, nil));
 Go(ret, x); }

Vm(rnd_u) { Go(ret, Pn(rand())); }

// this is for runtime errors from the interpreter, it prints
// a backtrace and everything.
static Inline u0 perrarg(lips v, M fp) {
 i64 i = 0, argc = fp == Pool + Len ? 0 : Gn(Argc);
 if (argc) for (fputc(' ', stderr);;fputc(' ', stderr)) {
  obj x = Argv[i++];
  emit(v, x, stderr);
  if (i == argc) break; }
 fputc(')', stderr); }

static Vm(nope) {
 fputs("# (", stderr), emit(v, Ph(ip), stderr),
 perrarg(v, fp);
 fputs(" does not exist\n", stderr);
 for (;;) {
  ip = Retp, fp += Size(fr) + Gn(Argc) + Gn(Subd);
  if (button(Gh(ip))[-1] == yield) break;
  fputs("#  in ", stderr), emsep(v, Ph(ip), stderr, '\n'); }
 return Hp = hp, restart(v); }

obj restart(lips v) {
 Fp = Sp = Pool + Len;
 Xp = Ip = nil;
 v->mem_root = NULL;
 longjmp(v->restart, 1); }