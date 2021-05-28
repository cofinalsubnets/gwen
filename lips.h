#ifndef LIPS_H
#define LIPS_H
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdbool.h>
#include <setjmp.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

// thanks !!
//
// first of all C makes you type waaaaaay too much
#define In inline __attribute__((always_inline))
#define Nin __attribute__((noinline))
#define Inline In
#define NoInline Nin

typedef intptr_t O, obj, i64, Z, *M, *mem;
typedef uintptr_t N, u64;
typedef void _, u0;
typedef char Ch;
typedef FILE *Io;
#define Ob (O)
#define non (Ob 0)
#define nil (Ob -1)
#define W sizeof(obj) // pointer arithmetic unit
#define W2 (2*W)

// more fundamental data types
typedef struct two {
  obj x, y; } *Tw, *two; // pairs
typedef struct tup {
  i64 len;
  obj xs[]; } *Ve, *tup, *vec; // vectors
typedef struct oct {
  i64 len;
  char text[]; } *By, *oct, *str; // byte arrays
typedef struct sym {
  obj nom, code, l, r; } *Sy, *sym; // symbols
typedef struct tble {
  obj key, val;
  struct tble *next; } *tble; // tables
typedef struct tbl {
  u64 len, cap;
  tble *tab; } *Ht, *tbl;

enum tag { // the 3 ls bits of each pointer are a type tag
 Hom = 0, Num = 1, Two = 2, Tup = 3,
 Oct = 4, Tbl = 5, Sym = 6, Nil = 7 };

enum globl { // indices into a table of global constants
 Def, Cond, Lamb, Quote, Seq,
 Splat, Topl, Macs, Eval, Apply, NGlobs };

// a linked list of stack addresses containing live values
// that need to be preserved by garbage collection.
typedef struct root { mem one; struct root *next; } *Mp, *root;

// this structure is responsible for holding runtime state.
// most functions take a pointer to it as the first argument.
typedef struct lips {
 obj ip, xp, *fp, *hp, *sp; // vm state variables
 obj syms, glob; // symbols and globals
 root mem_root; // gc protection list
 i64 t0, count, mem_len, *mem_pool; // memory data
 obj (*c_ret)(struct lips *, obj, obj);
 jmp_buf restart; // top level restart
} *lips;

// this is the type of interpreter functions
typedef obj terp(lips, obj, mem, mem, mem, obj);
typedef terp *T, **H, **hom; // code pointer ; the internal function type

lips
 initialize(void),
 bootstrap(lips),
 finalize(lips);

u0
 bcpy(u0*, const u0*, u64),
 wcpy(u0*, const u0*, u64),
 fill(u0*, obj, u64),
 emit(lips, obj, FILE*),
 script(lips, FILE*),
 errp(lips, const char*, ...),
 emsep(lips, obj, FILE*, char),
 reqsp(lips, u64);

obj
 err(lips, obj, const char*, ...),
 linitp(lips, obj, mem),
 snoc(lips, obj, obj),
 sskc(lips, mem, obj),
 restart(lips),
 homnom(lips, obj),
 pair(lips, obj, obj),
 parse(lips, FILE*),
 intern(lips, obj),
 eval(lips, obj),
 compile(lips, obj),
 table(lips),
 tblset(lips, obj, obj, obj),
 tblget(lips, obj, obj),
 tbldel(lips, obj, obj),
 tblkeys(lips, obj),
 string(lips, const char*);

i64 idx(obj, obj),
    llen(obj);
int eql(O, O);

const char* tnom(enum tag);

#define kind(x) ((x)&7)
#define Gh(x) ((hom)((x)))
#define Ph(x) ((obj)(x))
#define Gn getnum
#define Pn putnum
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) ((obj)(x)+Two)
#define getnum(n) ((i64)(n)>>3)
#define putnum(n) (((obj)(n)<<3)+Num)
#define getsym(x) ((sym)((O)(x)-Sym))
#define putsym(x) ((obj)(x)+Sym)
#define gettup(x) ((tup)((x)-Tup))
#define puttup(x) ((obj)(x)+Tup)
#define getoct(x) ((oct)((O)(x)-Oct))
#define putoct(x) ((obj)(x)+Oct)
#define gettbl(x) ((tbl)((O)(x)-Tbl))
#define puttbl(x) ((obj)(x)+Tbl)
#define homp(x) (kind(x)==Hom)
#define octp(x) (kind(x)==Oct)
#define nump(x) (kind(x)==Num)
#define twop(x) (kind(x)==Two)
#define symp(x) (kind(x)==Sym)
#define tupp(x) (kind(x)==Tup)
#define tblp(x) (kind(x)==Tbl)
#define nilp(x) ((x)==nil)
#define X(o) gettwo(o)->x
#define Y(o) gettwo(o)->y
#define XX(x) X(X(x))
#define XY(x) X(Y(x))
#define YX(x) Y(X(x))
#define YY(x) Y(Y(x))
#define F(x) ((hom)(x)+1)
#define G(x) (*(hom)(x))
#define FF(x) F(F(x))
#define FG(x) F(G(x))
#define GF(x) G(F(x))
#define GG(x) G(G(x))
#define chars(x) getoct(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((Safe=&((struct root){(r),Safe})))
#define um (Safe=Safe->next)
#define AR(x) gettup(x)->xs
#define AL(x) gettup(x)->len
#define Mm(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define b2w(n)((n)/W+((n)%W&&1))
#define w2b(n) ((n)*W)
#define Size(t) (sizeof(struct t)/W)
#define Ip v->ip
#define Fp v->fp
#define Hp v->hp
#define Sp v->sp
#define Safe v->mem_root
#define Xp v->xp
#define Pool v->mem_pool
#define Len v->mem_len
#define Dict Top
#define Syms (v->syms)
#define Glob v->glob
#define If AR(Glob)[Cond]
#define De AR(Glob)[Def]
#define La AR(Glob)[Lamb]
#define Qt AR(Glob)[Quote]
#define Se AR(Glob)[Seq]
#define Va AR(Glob)[Splat]
#define Top AR(Glob)[Topl]
#define Mac AR(Glob)[Macs]
#define Eva AR(Glob)[Eval]
#define App AR(Glob)[Apply]
#define Avail (Sp-Hp)

#define mix ((u64)2708237354241864315)

static Inline hom button(hom h) {
 while (*h) h++;
 return h; }

static Inline u0* bump(lips v, u64 n) {
 u0* x;
 return x = v->hp, v->hp += n, x; }

static Inline u0* cells(lips v, u64 n) {
 return Avail < n ? reqsp(v, n):0, bump(v, n); }

static Inline i64 hbi(u64 cap, u64 co) { return co % cap; }

static Inline tble hb(obj t, u64 code) {
 return gettbl(t)->tab[hbi(gettbl(t)->cap, code)]; }

_Static_assert(
 W >= 8,
 "pointers are smaller than 64 bits");

_Static_assert(
 -9 == (((-9)<<12)>>12),
 "opposite bit-shifts on a negative integer "
 "yield a nonidentical result");

#endif
