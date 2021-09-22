#ifndef _lips_h
#define _lips_h
#include <stdint.h>
#include <stdarg.h>
#include <stdio.h>
#include <setjmp.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdnoreturn.h>

#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define zero64 ((u64)0)
#define word64 8
#define rotr64(x, n) (((x)<<(64-(n)))|((x)>>(n)))
#define BWDQ(_) _(8) _(16) _(32) _(64)

typedef void u0;
#define I(n) \
 typedef int##n##_t i##n;\
 typedef uint##n##_t u##n;
BWDQ(I)
#undef I

// ASCII case folding
#define coff ('a'-'A')
static Inline char cmin(char c) {
 return c >= 'A' && c <= 'Z' ? c + coff : c; }
static Inline char cmaj(char c) {
 return c >= 'a' && c <= 'z' ? c - coff : c; }
#undef coff

// linear congruential pseudorandom number generator
// the multiplier comes from "Computationally Easy, Spectrally
// Good Multipliers for Congruential Pseudorandom Number
// Generators" by Steele & Vigna
#define LCPRNG(s) (((s) * 0xaf251af3b0f025b5ll + 1) >> 8)
static Inline i64 lcprng(i64 *s) { return *s = LCPRNG(*s); }

// functions for null-terminated byte strings
static Inline i64 scmp(const char *a, const char *b) {
 for (;;a++, b++) if (!(*a && *a == *b)) return *a - *b; }
static Inline u64 slen(const char *s) {
 for (u64 l = 0;;l++) if (!*s++) return l; }
static Inline i64 sidx(const char *s, char c) {
 for (i64 i = 0; *s; s++, i++) if (*s == c) return i;
 return -1; }

// mem{set,cpy,mov} analogs are defined for
// 8, 16, 32 and 64 bit items
#define memn(n)\
 static Inline u0 set##n(u0*_d,u##n i,u64 l) {\
  for(u##n*d=_d;l--;*d++=i); }\
 static Inline u0 cpy##n(u0*_d,const u0*_s, u64 l) {\
  u##n*d=_d; const u##n*s=_s;\
  while (l--) *d++=*s++; }\
 static Inline u0 cpy##n##r(u0*_d,const u0*_s, u64 l) {\
  u##n*d=_d; const u##n*s=_s;\
  while (l--) d[l]=s[l]; }\
 static Inline u0 mov##n(u0*_d,const u0*_s, u64 l) {\
  if (_d<_s) cpy##n(_d, _s, l);\
  else if (_d>_s) cpy##n##r(_d, _s, l); }
BWDQ(memn)
#undef memn
#undef BWDQ
// thanks !!
typedef i64 num, obj, *mem;
typedef struct two { obj x, y; } *two; // pairs
typedef struct tup { u64 len; obj xs[]; } *tup, *vec; // vectors
typedef struct str { u64 len; char text[]; } *str; // byte arrays
typedef struct sym { obj nom, code, l, r; } *sym; // symbols
typedef struct ent { obj key, val; struct ent *next; } *ent; // tables
typedef struct tbl { u64 len, cap; ent *tab; } *tbl;

enum tag { // the 3 ls bits of each pointer are a type tag
 Hom = 0, Num = 1, Two = 2, Vec = 3,
 Str = 4, Tbl = 5, Sym = 6, Nil = 7 };

enum globl { // indices into a table of global constants
 Def, Cond, Lamb, Quote, Seq, Splat,
 Topl, Macs, Eval, Apply, NGlobs };

// a linked list of stack addresses containing live values
// that need to be preserved by garbage collection.
typedef struct root { mem one; struct root *next; } *root;

// this structure is responsible for holding runtime state.
// most functions take a pointer to it as the first argument.
typedef struct lips {
 obj ip, xp, *fp, *hp, *sp, // vm state variables
     syms, glob[NGlobs]; // symbols and globals
 root root; // gc protection list
 i64 t0, seed, count, len, *pool; // memory data
 jmp_buf *restart; // top level restart
} *lips;

// this is the type of interpreter functions
typedef obj terp(lips, obj, mem, mem, mem, obj);
typedef terp **hom; // code pointer ; the internal function type

u0
 reqsp(lips, u64),
 defprim(lips, const char *, terp*) NoInline,
 emit(lips, obj, FILE*),
 errp_(lips, obj, char*, ...),
 errp(lips, char*, ...),
 emsep(lips, obj, FILE*, char);

obj
 sskc(lips, mem, obj),
 restart(lips),
 homnom(lips, obj),
 pair(lips, obj, obj),
 parse(lips, FILE*),
 intern(lips, obj),
 eval(lips, obj),
 table(lips),
 tblset(lips, obj, obj, obj),
 tblget(lips, obj, obj),
 tbldel(lips, obj, obj),
 string(lips, const char*);

u64 llen(obj) NoInline;
int repl(lips v, FILE *in, FILE *out);
bool eql(obj, obj);

// a packed array of 4-byte strings.
extern const uint32_t *tnoms;

#define non ((obj)0)
#define nil (~non)
#define W (sizeof(obj))
#define W2 (W*2)
#define tnom(t) ((char*)(tnoms+(t)))
#define Gh(x) gethom(x)
#define H(x) gethom(x)
#define H_(x) puthom(x)
#define Ph(x) puthom(x)
#define Gn getnum
#define Pn putnum
#define N(x) getnum(x)
#define N_(x) putnum(x)
#define kind(x) ((x)&7)
#define gethom(x) ((hom)((x)-Hom))
#define puthom(x) ((obj)((x)+Hom))
#define gettwo(x) ((two)((x)-Two))
#define puttwo(x) ((obj)(x)+Two)
#define getnum(n) ((i64)(n)>>3)
#define putnum(n) (((obj)(n)<<3)+Num)
#define getsym(x) ((sym)((obj)(x)-Sym))
#define putsym(x) ((obj)(x)+Sym)
#define getvec(x) ((vec)((x)-Vec))
#define putvec(x) ((obj)(x)+Vec)
#define getstr(x) ((str)((obj)(x)-Str))
#define putstr(x) ((obj)(x)+Str)
#define gettbl(x) ((tbl)((obj)(x)-Tbl))
#define puttbl(x) ((obj)(x)+Tbl)
#define homp(x) (kind(x)==Hom)
#define strp(x) (kind(x)==Str)
#define nump(x) (kind(x)==Num)
#define twop(x) (kind(x)==Two)
#define symp(x) (kind(x)==Sym)
#define vecp(x) (kind(x)==Vec)
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
#define chars(x) getstr(x)->text
#define symnom(y) chars(getsym(y)->nom)
#define mm(r) ((Safe=&((struct root){(r),Safe})))
#define um (Safe=Safe->next)
#define AR(x) getvec(x)->xs
#define AL(x) getvec(x)->len
#define with(y,...) (mm(&(y)),(__VA_ARGS__),um)
#define w2b(n) ((n)*W)
#define Size(t) (sizeof(struct t)/W)
#define Ip v->ip
#define Fp v->fp
#define Hp v->hp
#define Sp v->sp
#define Safe v->root
#define Xp v->xp
#define Pool v->pool
#define Len v->len
#define Syms (v->syms)
#define Glob v->glob
#define If Glob[Cond]
#define De Glob[Def]
#define La Glob[Lamb]
#define Qt Glob[Quote]
#define Se Glob[Seq]
#define Va Glob[Splat]
#define Top Glob[Topl]
#define Mac Glob[Macs]
#define Eva Glob[Eval]
#define App Glob[Apply]
#define Avail (Sp-Hp)

#define mix ((u64)2708237354241864315)
#define interns(v,c) intern(v,string(v,c))
#define SI static Inline
SI hom button(hom h) { while (*h) h++; return h; }
SI u0* bump(lips v, u64 n) { u0* x = v->hp; return v->hp += n, x; }
SI u0* cells(lips v, u64 n) { if (Avail < n) reqsp(v, n); return bump(v, n); }
SI u64 b2w(u64 b) { return b / W + (b % W && 1); }
#undef SI

_Static_assert(
 sizeof(intptr_t) == sizeof(int64_t),
 "pointers are not 64 bits");

_Static_assert(
 -1l == ((-1l<<8)>>8),
 "opposite bit-shifts on a negative integer yield a different result");
#endif
