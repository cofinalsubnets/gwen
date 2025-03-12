#ifndef _p_i_h
#define _p_i_h
#include "p.h"

#include <stdarg.h> // this is a freestanding header

// these are not freestanding headers
#include <time.h>
#include <string.h>
#include <stdlib.h>

// theres a big benefit in speed from tail call optimization but not all platforms support it
#ifndef TCO
#define TCO 1 // on by default
#endif

#if TCO
#define Vm(n, ...) Status n(Core *f, Thread* Ip, Heap Hp, Stack Sp, ##__VA_ARGS__)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#define YieldStatus PStatusOk
#define Continue() Ip->ap(f, Ip, Hp, Sp)
#define Jump(v) v(f, Ip, Hp, Sp)
#define Have(n) if (Sp - Hp < n) return gc(f, Ip, Hp, Sp, n)
#define Have1() if (Sp == Hp) return gc(f, Ip, Hp, Sp, 1)
#else
#define Vm(n, ...) Status n(Core *f, ##__VA_ARGS__)
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#define YieldStatus PStatusEof
#define Continue() Ok
#define Jump(v) v(f)
#define Have(n) if (Sp - Hp < n) return gc(f, n)
#define Have1() if (Sp == Hp) return gc(f, 1)
#endif


// thanks !!
typedef intptr_t Word, *Heap, *Stack;
typedef PCore Core;
typedef PStatus Status;
typedef struct Type Type;
typedef union Cell Cell, Thread;
typedef Vm(Vm);
typedef Vm PVm;
typedef struct Pair {
  Vm *ap; Type *typ;
  Word a, b;
} Pair;
union Cell {
  Vm *ap;
  Word x;
  Cell *m;
  Type *typ;
  Pair *w;
};


// primitive type method tables
typedef struct Type {
  Word (*copy)(Core*, Word, Word*, Word*); // for gc
  void (*evac)(Core*, Word, Word*, Word*); // for gc
  bool (*equal)(Core*, Word, Word);        // check equality with another object of same type
  void (*emit)(Core*, p_file, Word);       // print it // replace this with stringify...
  Word (*hash)(Core*, Word);               // hash it
} Type;



typedef struct String {
  Vm *ap; Type *typ;
  uintptr_t len;
  char text[];
} String;

typedef struct Symbol {
  Vm *ap; Type *typ;
  String *nom;
  Word code;
  struct Symbol *l, *r;
} Symbol;

typedef struct TableEntry {
  Word key, val;
  struct TableEntry *next;
} TableEntry;

typedef struct Table {
  Vm *ap; Type *typ;
  uintptr_t len, cap;
  TableEntry **tab;
} Table;

typedef struct Mm {
  Word *addr;
  struct Mm *next;
} Mm, Mm;

// runtime core data structure -- 1 core = 1 thread of execution
struct PCore {
  // vm registers
  Cell *ip; // instruction pointer
  Word *hp, // heap pointer
       *sp; // stack pointer

  Symbol *symbols; // interned symbol tree
  // environment // TODO clarify what this is exactly
  Table *dict,  // global environment
        *macro; // compiler macros
  // memory management
  uintptr_t len;
  Word *pool, *loop;
  Mm *safe;
  union { uintptr_t t0;  // end time of last gc
          Heap cp; }; }; // gc copy pointer

struct tag { Cell *null, *head, end[]; } *ttag(Cell*);

Cell
  *mo_ini(Thread*, uintptr_t),
  *trim_thread(Thread*),
  *mo_n(Core*, size_t);
Pair
  *ini_pair(Pair*, Word, Word),
  *pairof(Core*, Word, Word);
Table
  *ini_table(Table*, uintptr_t, uintptr_t, TableEntry**),
  *new_table(Core*),
  *table_set(Core*, Table*, Word, Word);
Symbol
  *ini_sym(Symbol*, String*, uintptr_t),
  *literal_symbol(Core*, const char*),
  *intern(Core*, String*);
String *ini_str(String*, uintptr_t);
void
  *bump(Core*, uintptr_t),
  *cells(Core*, uintptr_t),
  transmit(Core*, PFile*, Word);
bool
  not_equal(Core*, Word, Word),
  p_please(Core*, uintptr_t),
  eql(Core*, Word, Word);
Word
  table_get(Core*, Table*, Word, Word),
  pushs(Core*, uintptr_t, ...),
  hash(Core*, Word),
  cp(Core*, Word, Word*, Word*); // for recursive use by evac functions
Vm(gc, uintptr_t s);
Vm display, bnot, rng, data,
   defmacro,
   ret, ap, apn, tap, tapn,
   jump, cond, dup, imm, yield,
   gensym, ev0, pairp, fixnump, symbolp, stringp,
   ssub, sget, slen, scat, prc, error, cons, car, cdr,
   lt, le, eq, gt, ge, tset, tget, tdel, tnew, tkeys, tlen,
   seek, peek, poke, trim, thda, add, sub, mul, quot, rem,
   curry;

extern Type pair_type, string_type, symbol_type, table_type;

#define Oom PStatusOom
#define Ok PStatusOk
#define Eof PStatusEof

#define Width(_) b2w(sizeof(_))
#define avail(f) (f->sp-f->hp)
#define getnum(_) ((Word)(_)>>1)
#define putnum(_) (((Word)(_)<<1)|1)
#define nil putnum(0)
#define MM(f,r) ((f->safe=&((Mm){(Word*)(r),f->safe})))
#define UM(f) (f->safe=f->safe->next)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define A(o) ((Pair*)(o))->a
#define B(o) ((Pair*)(o))->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define BBA(o) B(B(A(o)))
#define Z(_) ((Word)(_))
#define R(_) ((Cell*)(_))
#define nilp(_) (Z(_)==nil)
#define nump(_) (Z(_)&1)
#define thdp(_) (!nump(_))
#define datp(_) (R(_)->ap==data)
#define dtyp(_) R(_)[1].typ
#define max(a, b) ((a)>(b)?(a):(b))
#define min(a, b) ((a)<(b)?(a):(b))
#define mix ((uintptr_t)2708237354241864315)

#define homp thdp
#define ptr R

#define pop1(f) (*(f)->sp++)

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))

static Inline bool strp(Word _) { return homp(_) && dtyp(_) == &string_type; }
static Inline bool twop(Word _) { return homp(_) && dtyp(_) == &pair_type; }
static Inline bool tblp(Word _) { return homp(_) && dtyp(_) == &table_type; }
static Inline bool symp(Word _) { return homp(_) && dtyp(_) == &symbol_type; }

// align bytes up to the nearest word
static Inline size_t b2w(size_t b) {
  size_t q = b / sizeof(Word), r = b % sizeof(Word);
  return q + (r ? 1 : 0); }

#define bounded(a, b, c) (Z(a)<=Z(b)&&Z(b)<Z(c))
#define op(n, x) (Ip = (Cell*) Sp[n], Sp[n] = (x), Sp += n, Continue())

_Static_assert(-1 >> 1 == -1, "support sign extended shift");
_Static_assert(sizeof(Cell*) == sizeof(Cell), "cell is 1 word wide");

#endif
