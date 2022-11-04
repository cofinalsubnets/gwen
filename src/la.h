#include "lisa.h"
#include <stdlib.h>

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

// thanks !!

typedef la_ob ob;
typedef struct mo *mo; // procedures
typedef struct sf *fr, *sf; // stack frame
#define Vm(n, ...) ob n(la v, ob xp, mo ip, ob *hp, ob *sp, sf fp)
typedef Vm(vm);

// struct needed for type indirection
// around vm function pointer arrays
struct mo { vm *ap; };
// every dynamically allocated thread ends
// with a tag pointing back to its head
typedef struct tag {
  void *null; // always null
  struct mo
    *self, // pointer to head of thread
    end[]; // first address after thread
} *tag;

// stack frame
struct sf {
  ob *clos; // closure pointer FIXME
  // keep this on the stack outside the
  // frame so we don't waste space for
  // functions w/o closures.
  mo retp; // thread return address
  sf subd; // stack frame of caller
  size_t argc; // argument count
  ob argv[]; };

// static method table for built-in types
typedef const struct mtbl {
  vm *does;
  bool (*equi)(la, ob, ob);
  intptr_t (*hash)(la, ob);
  long (*emit)(la, FILE*, ob);
  ob (*evac)(la, ob, ob*, ob*);
  void (*walk)(la, ob, ob*, ob*);
} *mtbl;

// pairs
typedef struct two {
  vm *disp; mtbl mtbl;
  ob a, b; } *two;

// strings
typedef struct str {
  vm *disp; mtbl mtbl;
  size_t len;
  char text[]; } *str;

// symbols
// FIXME this is a silly way to store internal symbols
// - it's slower than a hash table
// - anonymous symbols waste 2 words
typedef struct sym {
  vm *disp; mtbl mtbl;
  str nom;
  intptr_t code;
  struct sym *l, *r; } *sym;

// hash tables
typedef struct tbl {
  vm *disp; mtbl mtbl;
  size_t len, cap;
  ob *tab; } *tbl;

// grammar symbols
enum lex { Def, Cond, Lamb, Quote, Seq, Splat, Eval, LexN };

// linked list for gc protection
typedef struct keep { void **it; struct keep *et; } *keep;

struct la {
  // vm state
  mo ip;
  sf fp;
  ob xp, *hp, *sp;

  tbl topl; // global scope
  sym syms; // symbol table
  ob lex[LexN]; // lexicon
  intptr_t rand;

  // gc state
  size_t len;
  ob *pool;
  keep safe;
  union { uintptr_t t0; ob *cp; }; };

bool please(la, size_t); // ask GC for available memory
void *bump(la, size_t), // allocate memory unchecked
     *cells(la, size_t); // allocate memory checked
#define Gc(n) ob n(la v, ob x, ob *pool0, ob *top0)
Gc(cp); // copy something; used by type-specific copying functions


// pairs
two pair(la, ob, ob) NoInline;
size_t llen(ob);

ob ns_tbl(la),
   ns_get(la, ob),
   ns_set(la, ob, ob);

// hash tables
intptr_t
  hash(la, ob),
  hx_mo(la, mo);
tbl table(la);
ob tbl_set(la, tbl, ob, ob),
   tbl_get(la, tbl, ob);

// string & symbol constructors
sym symof(la, str);
str strof(la, const char*);

// output functions:
// like la_tx, they return the number
// of bytes written or a negative number
// on error.
long
  fputstr(FILE*, str),  // like fputs
  tx_mo(la, FILE*, mo); // show a function

// functions
tag button(mo); // get tag at end
mo mkmo(la, size_t); // allocate a thread

bool  pushs(la, ...); // push args onto stack
ob tupl(la, ...); // collect args into tuple (data thread)
#define Push(...) pushs(v, __VA_ARGS__, (ob)0)
#define Tupl(...) tupl(v, __VA_ARGS__, (ob)0)

bool
  eq_not(la, ob, ob), // always returns false
  primp(mo), // is it a primitive function? FIXME hide this
  eql(la, ob, ob); // object equality

// linear congruential pseudorandom number generator
intptr_t lcprng(intptr_t);

// word-sized memset/memcpy analogs
void *setw(void*, intptr_t, size_t),
     *cpyw(void*, const void*, size_t);

// error functions
// print an error with backtrace
void errp(la, const char*, ...) NoInline;
// panic with message
ob nope(la, const char*, ...) NoInline;

#define nil putnum(0)
#define F(_) ((mo)(_)+1)
#define G(_) ((mo)(_))->ap
#define FF(x) F(F(x))
#define GF(x) G(F(x))
#define A(o) ((two)(o))->a
#define B(o) ((two)(o))->b
#define AA(o) A(A(o))
#define AB(o) A(B(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define Avail (v->sp - v->hp)
#define mm(r) ((v->safe = &((struct keep){(void**)(r), v->safe})))
#define um (v->safe = v->safe->et)
#define with(y,...) (mm(&(y)), (__VA_ARGS__), um)
#define Width(t) b2w(sizeof(struct t))

#define T putnum(-1)

#define getnum(_) ((ob)(_)>>1)
#define putnum(_) (((ob)(_)<<1)|1)

struct prim { vm *ap; const char *nom; };
extern const int64_t mix;
extern const struct prim prims[];
extern const struct mtbl mtbl_two, mtbl_str, mtbl_tbl, mtbl_sym;

static Inline bool nilp(ob _) { return _ == nil; }
static Inline bool nump(ob _) { return _ & 1; }
static Inline bool homp(ob _) { return !nump(_); }
static Inline bool tblp(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_tbl; }
static Inline bool strp(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_str; }
static Inline bool twop(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_two; }
static Inline bool symp(ob _) { return homp(_) && GF(_) == (vm*) &mtbl_sym; }

static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(ob), r = b % sizeof(ob);
  return r ? q + 1 : q; }

// this can give a false positive if x is a fixnum
static Inline bool livep(la v, ob x) {
  return (ob*) x >= v->pool && (ob*) x < v->pool + v->len; }

_Static_assert(-1 == -1 >> 1, "signed >>");
_Static_assert(sizeof(void*) == sizeof(size_t), "size_t matches pointer size");

static Inline size_t ror(size_t x, size_t n) {
  return (x<<((8*sizeof(size_t))-n))|(x>>n); }

#define ninl(x, ...) vm x NoInline;

// these are vm functions used by C but not lisp.
#define cfns(_)\
  _(gc) _(xdom) _(xoom) _(xary)\
  _(setclo) _(genclo0) _(genclo1)\
  _(ap_nop) _(yield)
cfns(ninl)
#undef cfns

// used by the compiler but not exposed as primitives
#define i_internals(_)\
 _(call) _(ret) _(rec) _(jump) _(varg) _(disp)\
 _(arity) _(idZ) _(idH) _(id2) _(idT)\
 _(imm) _(zero) _(one)\
 _(argn) _(arg0) _(arg1) _(arg2) _(arg3)\
 _(clon) _(clo0) _(clo1) _(clo2) _(clo3)\
 _(locn) _(loc0) _(loc1) _(loc2) _(loc3)\
 _(deftop) _(late)\
 _(setloc) _(defloc)\
 _(take) _(encl1) _(encl0)\
 _(twopp) _(numpp) _(nilpp) _(strpp)\
 _(tblpp) _(sympp) _(hompp)\
 _(add) _(sub) _(mul) _(dqv) _(mod) _(neg)\
 _(sar) _(sal) _(band) _(bor) _(bxor)\
 _(lt) _(lteq) _(eq) _(gteq) _(gt)\
 _(tget) _(tset) _(thas) _(tlen)\
 _(cons) _(car) _(cdr)\
 _(emi) _(emx)\
 _(br1) _(br0) _(bre) _(brn)\
 _(brl) _(brle) _(brge) _(brl2)\
 _(brle2) _(brg2) _(brg)\
 _(push) _(dupl)\

i_internals(ninl)

// primitive functions
// FIXME due to a hack ev must be the first item in this list
#define i_primitives(_)\
 _(ev_u, "ev") _(ap_u, "ap")\
  \
 _(nump_u, "nump") _(rnd_u, "rand")\
 _(add_u, "+") _(sub_u, "-") _(mul_u, "*")\
 _(div_u, "/") _(mod_u, "%")\
 _(sar_u, ">>") _(sal_u, "<<")\
 _(band_u, "&") _(bnot_u, "!") _(bor_u, "|") _(bxor_u, "^")\
  \
 _(twop_u, "twop") _(cons_u, "X") _(car_u, "A") _(cdr_u, "B")\
  \
 _(hom_u, "hom") _(homp_u, "homp")\
 _(emi_u, "emi") _(emx_u, "emx")\
 _(peeki_u, "peeki") _(peekx_u, "peekx")\
 _(seek_u, "seek") _(hfin_u, "hfin")\
  \
 _(tbl_u, "tbl") _(tblp_u, "tblp") _(tlen_u, "tlen")\
 _(tget_u, "tget") _(thas_u, "thas") _(tset_u, "tset")\
 _(tdel_u, "tdel") _(tkeys_u, "tkeys")\
  \
 _(str_u, "str") _(strp_u, "strp") _(slen_u, "slen")\
 _(ssub_u, "ssub") _(scat_u, "scat")\
 _(sget_u, "schr")\
  \
 _(sym_u, "sym") _(symp_u, "symp") _(ynom_u, "ynom")\
  \
 _(rx_u, "rx") _(show_u, ".") _(putc_u, "putc")\
  \
 _(gettime, "time")\
  \
 _(eq_u, "=") _(lt_u, "<") _(lteq_u, "<=")\
 _(gteq_u, ">=") _(gt_u, ">") _(nilp_u, "nilp")\
  \
 _(xdom, "fail")

i_primitives(ninl)
#undef ninl

// " the interpreter "
// the arguments to a terp function collectively represent the
// runtime state, and the  return value is the result of the
// program. there are six arguments because that's the number
// that the prevalent unix calling convention on AMD64 (System
// V ABI) says should be passed in registers; that's the only
// reason why there aren't more. but it's not too bad; the six
// arguments are:
// - v  : vm instance pointer ; most functions take this as the first argument
// - ip : instruction pointer ; the current vm instruction ; function pointer pointer
// - fp : frame pointer       ; current function context
// - sp : stack pointer       ; data/call stack
// - hp : heap pointer        ; the next free heap location
// - xp : return value        ; general-purpose register

// when the interpreter isn't running, the state variables that
// would normally be in registers are stored in slots on the
// vm structure. phowever while the interpreter is running it
// uses these struct slots to pass and return extra values
// without using the stack. so the interpreter has to restore
// the current values in the vm struct before it makes any
// "external" function calls.
#define Pack() (v->ip=ip,v->sp=sp,v->hp=hp,v->fp=fp,v->xp=xp)
#define Unpack() (fp=v->fp,hp=v->hp,sp=v->sp,ip=v->ip,xp=v->xp)
#define CallOut(...) (Pack(), __VA_ARGS__, Unpack())

// the pointer to the local variables array isn't in the frame struct. it
// isn't present for all functions, but if it is it's in the word of memory
// immediately preceding the frame pointer. if a function has
// locals, this will have been initialized before they are
// referenced.

#define ApN(n, x) (xp = (x), ip += (n), ApC(G(ip), xp))
#define ApC(f, x) (f)(v, (x), ip, hp, sp, fp)
#define ApY(f, x) (ip = (mo) (f), ApC(G(ip), (x)))

#define ArityCheck(n) if (n > fp->argc) return ApC(xary, putnum(n))
#define Check(_) if (!(_)) return ApC(xdom, xp)
#define Have(n) if (sp - hp < n) return (v->xp = n, ApC(gc, xp))
// sp is at least hp so this is a safe check for 1 word
#define Have1() if (sp == hp) return (v->xp = 1, ApC(gc, xp))

// FIXME isolate these if possible?
static Inline two ini_two(void *_, ob a, ob b) {
  two w = _;
  w->disp = disp, w->mtbl = &mtbl_two, w->a = a, w->b = b;
  return w; }
static Inline str ini_str(void *_, size_t len) {
  str s = _;
  s->disp = disp, s->mtbl = &mtbl_str, s->len = len;
  return s; }

static Inline mo ini_mo(void *_, size_t len) {
  mo k = _;
  tag t = (tag) (k + len);
  t->null = NULL, t->self = k;
  return k; }
