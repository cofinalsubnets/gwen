#ifndef _g_h
#define _g_h
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#ifndef g_tco
#define g_tco 1
#endif
#if g_tco
#define g_vm(n, ...) struct g *n(struct g *f, union x *Ip, intptr_t *Hp, intptr_t *Sp, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Continue() Ap(Ip->ap, f)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#else
#define g_vm(n, ...) struct g *n(struct g *f, ##__VA_ARGS__)
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Continue() f
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#endif
#define g_core_of(f) ((struct g*)((intptr_t)(f)&~(sizeof(intptr_t)-1)))
#define g_code_of(f) ((enum g_status)((intptr_t)(f)&(sizeof(intptr_t)-1)))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#define g_putnum(_) (((intptr_t)(_)<<1)|1)
#define g_getnum(_) ((intptr_t)(_)>>1)
#define g_nil g_putnum(0)
#define g_pop1(f) (*(f)->sp++)
#define LEN(_) (sizeof(_)/sizeof(*_))
#define MIN(_,__) ((_)<(__)?(_):(__))
#define MAX(_,__) ((_)>(__)?(_):(__))
// thanks !!
union x;
typedef g_vm(g_vm_t);
struct g {
  struct g *pool;                        // 5 lower core address
  uintptr_t len;                         // 4 length of core data
  union x {
    g_vm_t *ap;
    intptr_t x;
    union x *m;
    intptr_t typ; } *ip;                 // 0
  intptr_t *hp,                          // 1
           *sp;                          // 2
  struct g_atom {
    g_vm_t *ap;
    intptr_t typ;
    struct g_vec {
      g_vm_t *ap;
      intptr_t typ;
      uintptr_t type, rank, shape[]; } *nom;
    uintptr_t code;
    struct g_atom *l, *r;
  } *symbols;                            // 3 
  struct g_mem_root {
    intptr_t *ptr;
    struct g_mem_root *next; } *safe;    // 6
  union { uintptr_t t0; intptr_t *cp; }; // 7 copy pointer / timestamp
  void *(*malloc)(struct g*, size_t),    // 8
       (*free)(struct g*, void*);        // 9
#define g_nvars 16
  union {
    intptr_t v[g_nvars];
    struct {
      struct g_table {
        g_vm_t *ap;
        intptr_t typ;
        uintptr_t len, cap;
        struct g_entry {
          intptr_t key, val;
          struct g_entry *next;
        } **tab;
      } *dict,
        *macro;
      struct g_atom *quote,
                    *begin,
                    *let,
                    *cond,
                    *lambda;
      intptr_t u[]; }; };
  intptr_t end[]; };                     // 20 end of struct == initial heap pointer

_Static_assert(sizeof(struct g) == (10 + g_nvars) * sizeof(intptr_t));

struct g_in {
  int (*getc)(struct g*, struct g_in*),
      (*ungetc)(struct g*, struct g_in*, int),
      (*eof)(struct g*, struct g_in*);
};
struct g_out {
  struct g *(*putc)(struct g*, struct g_out*, int);
};

struct g_def { const char *n; intptr_t x; };

struct g
  *g_ini(void),
  *g_ini_static(uintptr_t, void*),
  *g_ini_dynamic(void *(*)(struct g*, uintptr_t), void (*)(struct g*, void*));

enum g_vec_type {
  g_vt_u8,  g_vt_i8,
  g_vt_u16, g_vt_i16,
  g_vt_u32, g_vt_i32,
  g_vt_u64, g_vt_i64,
  g_vt_f8,  g_vt_f16,
  g_vt_f32, g_vt_f64, };

enum g_status {
  g_status_ok  = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_fin(struct g*);


uintptr_t g_clock(void); // used by garbage collector
int g_stdin_getc(struct g*),
    g_stdin_ungetc(struct g*, int),
    g_stdin_eof(struct g*),
    strncmp(const char*, const char*, size_t),
    memcmp(const void*, const void*, size_t);
void g_stdout_putc(struct g*, int c);
void
     *malloc(size_t), free(void*),
     *memcpy(void *restrict, const void*restrict, size_t),
     *memset(void*, int, size_t);
long strtol(const char*restrict, char**restrict, int);
size_t strlen(const char*);

char *g_str_txt(intptr_t);
g_vm_t ret0, curry, g_yield;

bool g_twop(intptr_t), g_strp(intptr_t), g_tblp(intptr_t), g_symp(intptr_t);

#define g_vt_char g_vt_i8
uintptr_t g_fixed_size(enum g_vec_type),
          g_str_len(intptr_t),
          vector_total_bytes(struct g_vec *);

struct g
  *g_printf(struct g*, struct g_out*, const char*, ...),
  *g_putc(struct g*, struct g_out*, int),
  *g_putn(struct g*, struct g_out*, uintptr_t, uintptr_t);
struct g
  *g_ana(struct g*, g_vm_t),
  *g_write1(struct g*),
  *g_eval(struct g*),
  *g_read1i(struct g*f, struct g_in*),
  *g_readsi(struct g*, struct g_in*),
  *g_reads(struct g*, const char*),
  *g_def(struct g*, const char*, intptr_t),
  *g_defs(struct g*, uintptr_t, struct g_def*),
  *g_push(struct g*, uintptr_t, ...),
  *g_pop(struct g*, uintptr_t),
  *g_strof(struct g*, const char*),
  *g_vec0(struct g*f, uintptr_t type, uintptr_t rank, ...),
  *g_cons_l(struct g*),
  *g_cons_r(struct g*);

extern struct g_in g_stdin;
extern struct g_out g_stdout;
#define g_inline inline __attribute__((always_inline))
#define g_noinline __attribute__((noinline))
static g_inline intptr_t *topof(struct g*f) { return (intptr_t*) f + f->len; }
static g_inline intptr_t topref(struct g*f, uintptr_t n) { return topof(f)[-1-n]; }
static g_inline struct g *g_enlist(struct g*f) { return g_cons_r(g_push(f, 1, g_nil)); }
static g_inline struct g *g_quote(struct g*f) { return
  f = g_enlist(f),
  g_ok(f) ? g_cons_l(g_push(f, 1, f->quote)) : f; }
static g_inline struct g *g_evals(struct g *f, const char *s) { return
  f = g_eval(g_reads(f, "((:(e a b)(? b(e(ev'ev(A b))(B b))a)e)0)")),
  f = g_ok(f) ? g_push(f, 3, g_nil, f->quote, g_nil) : f,
  g_eval(g_cons_r(g_cons_l(g_cons_r(g_cons_l(g_reads(f, s)))))); }
static g_inline struct g *g_read1(struct g *f) { return g_read1i(f, &g_stdin); }
#define g_log1(f) (g_write1(f),g_putc(f, f->out, '\n'))
#define g_digits "0123456789abcdefghijklmnopqrstuvwxyz"
#endif
