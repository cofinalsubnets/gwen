#ifndef _g_i_h
#define _g_i_h
#include "g.h"
_Static_assert(sizeof(union u) == sizeof(intptr_t));
_Static_assert(-1 >> 1 == -1, "sign extended shift");
struct g_pair { g_vm_t *ap; uintptr_t typ; intptr_t a, b; };
enum q { two_q, vec_q, sym_q, tbl_q, };
#define two_class two_q
#define vec_class vec_q
#define sym_class sym_q
#define tbl_class tbl_q
typedef g_num num, word;
enum g_vec_type {
 g_vect_u8,  g_vect_i8,
 g_vect_u16, g_vect_i16,
 g_vect_u32, g_vect_i32,
 g_vect_u64, g_vect_i64,
 g_vect_f8,  g_vect_f16,
 g_vect_f32, g_vect_f64, };
struct g
 *g_please(struct g*, uintptr_t),
 *g_have(struct g*, intptr_t),
 *g_tput(struct g *f),
 *g_tnew(struct g*),
 *g_intern(struct g*),
 *g_reads(struct g*, struct g_in*),
 *g_read1(struct g*f, struct g_in* i);
g_vm(g_vm_gc, uintptr_t);
g_vm_t
 g_vm_data,  g_vm_putn,   g_vm_nomsym, g_vm_info, g_vm_dot,    g_vm_clock,
 g_vm_nilp,  g_vm_symnom, g_vm_read,   g_vm_putc, g_vm_gensym, g_vm_twop,
 g_vm_nump,  g_vm_symp,   g_vm_strp,   g_vm_tabp, g_vm_band,   g_vm_bor,
 g_vm_bxor,  g_vm_bsr,    g_vm_bsl,    g_vm_bnot, g_vm_ssub,   g_vm_sget,
 g_vm_slen,  g_vm_scat,   g_vm_cons,   g_vm_car,  g_vm_cdr,    g_vm_puts,
 g_vm_getc,  g_vm_lt,     g_vm_le,     g_vm_eq,   g_vm_gt,     g_vm_ge,
 g_vm_tset,  g_vm_tget,   g_vm_tdel,   g_vm_tnew, g_vm_tkeys,  g_vm_tlen,
 g_vm_seek,  g_vm_peek,   g_vm_poke,   g_vm_trim, g_vm_thda,   g_vm_add,
 g_vm_sub,   g_vm_mul,    g_vm_quot,   g_vm_rem,  g_vm_arg,    g_vm_drop1,
 g_vm_quote, g_vm_freev,  g_vm_eval,   g_vm_cond, g_vm_jump,   g_vm_defglob,
 g_vm_ap,    g_vm_tap,    g_vm_apn,    g_vm_tapn, g_vm_ret,    g_vm_lazyb;
struct g_atom *g_intern_r(struct g*, struct g_vec*, struct g_atom **y);
static g_inline struct g_tag { union u *null, *head, end[]; }
 *ttag(union u *k) {
 while (k->x) k++;
 return (struct g_tag*) k; }
static g_inline union u *clip(union u *k) { return ttag(k)->head = k; }
bool eql(struct g*, intptr_t, intptr_t);
uintptr_t g_hash(struct g*, g_num), g_vec_bytes(struct g_vec*);
int
 memcmp(void const*, void const*, size_t),
 g_putn(struct g *f, struct g_out *o, intptr_t n, uintptr_t base);
void
 ini_vec(struct g_vec*, uintptr_t, uintptr_t, ...),
 *malloc(size_t),
 free(void*),
 *memcpy(void*restrict, void const*restrict, size_t),
 *memset(void*, int, size_t);
intptr_t g_tget(struct g*,intptr_t, struct g_tab*,intptr_t);

#define dict_of(_) (_)->dict
#define nilp(_) (word(_)==g_nil)
#define A(o) two(o)->a
#define B(o) two(o)->b
#define AB(o) A(B(o))
#define AA(o) A(A(o))
#define BA(o) B(A(o))
#define BB(o) B(B(o))
#define push1(f, _) (*--(f)->sp=(_))
#define vlen(_)((struct g_vec*)(_))->shape[0]
#define vtxt(_) ((char*)(((struct g_vec*)(_))->shape+1))
#define len(_) vlen(_)
#define txt(_) vtxt(_)
#define avail(f) ((f->sp-f->hp))
#define nom(_) sym(_)
#define ptr(_) ((num*)(_))
#define num(_) ((num)(_))
#define word(_) num(_)
#define datp(_) (cell(_)->ap==g_vm_data)
#define avec(f, y, ...) (MM(f,&(y)),(__VA_ARGS__),UM(f))
#define MM(f,r) ((f->root=&((struct g_root){(g_num*)(r),f->root})))
#define UM(f) (f->root=f->root->next)
#define mix ((uintptr_t)2708237354241864315)

#define odd(_) ((uintptr_t)(_)&1)
#define even(_) !odd(_)
#define typ(_) cell(_)[1].typ
#define cell(_) ((union u*)(_))

#define g_vect_char g_vect_u8
#define avail(f) ((f->sp-f->hp))
#ifndef EOF
#define EOF -1
#endif
#define Have1() if (Sp == Hp) return Ap(g_vm_gc, f, 1)
#define Have(n) if (Sp < Hp + n) return Ap(g_vm_gc, f, n)
#if g_tco
#define g_status_yield g_status_ok
#else
#define g_status_yield g_status_eof
#endif
#define g_pop1(f) (*(f)->sp++)
#define str_type_width (Width(struct g_vec) + 1)

#define opf(nom, op) g_vm(nom) {\
 intptr_t a = ggetnum(Sp[0]), b = ggetnum(Sp[1]);\
 *++Sp = gputnum(a op b);\
 return Ip++, Continue(); }
#define op0f(nom, op) g_vm(nom) {\
 intptr_t a = ggetnum(Sp[0]), b = ggetnum(Sp[1]);\
 *++Sp = b == 0 ? g_nil : gputnum(a op b);\
 return Ip++, Continue(); }
#define op(nom, n, x) g_vm(nom) { intptr_t _ = (x); *(Sp += n-1) = _; Ip++; return Continue(); }
#define op1(nom, i, x) g_vm(nom) { Sp[0] = (x); Ip += i; return Continue(); }
#define op11(nom, x) op1(nom, 1, x)

#define nil g_nil
#define pop1 g_pop1
#define getnum g_getnum
#define putnum g_putnum
#define g_strp strp

static g_inline struct g_vec *vec(g_num n) { return (struct g_vec*) n; }
static g_inline struct g_tab *tbl(g_num n) { return (struct g_tab*) n; }
static g_inline struct g_pair *two(g_num n) { return (struct g_pair*) n; }
static g_inline struct g_atom *sym(g_num n) { return (struct g_atom*) n; }
static g_inline bool twop(g_num _) { return even(_) && typ(_) == two_q; }
static g_inline bool tabp(g_num _) { return even(_) && typ(_) == tbl_q; }
static g_inline bool symp(g_num _) { return even(_) && typ(_) == sym_q; }
static g_inline bool nump(g_num _) { return odd(_); }
static g_inline bool vec_strp(struct g_vec *s) { return
 s->type == g_vect_char && s->rank == 1; }
static g_inline bool strp(g_num _) { return
 even(_) && typ(_) == vec_q && vec_strp((struct g_vec*)_); }
static g_inline struct g *encode(struct g*f, enum g_status s) { return
 (struct g*) ((uintptr_t) f | s); }

static g_inline void *bump(struct g *f, uintptr_t n) {
 void *x = f->hp;
 f->hp += n;
 return x; }

static g_inline void ini_anon(struct g_atom *y, uintptr_t code) {
 y->ap = g_vm_data; y->typ = sym_q; y->nom = 0; y->code = code; }
static g_inline void ini_str(struct g_vec *s, uintptr_t len) {
 ini_vec((struct g_vec*) s, g_vect_char, 1, len); }
static g_inline void ini_tab(struct g_tab *t, uintptr_t len, uintptr_t cap, struct g_kvs**tab) {
 t->ap = g_vm_data; t->typ = tbl_q; t->len = len; t->cap = cap; t->tab = tab; }
static g_inline void ini_two(struct g_pair *w, intptr_t a, intptr_t b) {
 w->ap = g_vm_data; w->typ = two_q; w->a = a; w->b = b; }

extern struct g_in g_stdin;
extern struct g_out g_stdout;
#endif
