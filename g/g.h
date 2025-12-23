#ifndef _g_h
#define _g_h
// thanks !!
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#define Width(_) b2w(sizeof(_))
#define g_core_of(f) ((struct g*)((intptr_t)(f)&~(sizeof(intptr_t)-1)))
#define g_code_of(f) ((enum g_status)((intptr_t)(f)&(sizeof(intptr_t)-1)))
#define gokp(f) (g_code_of(f) == g_status_ok)
#define g_ok gokp
#define gputnum(_) (((intptr_t)(_)<<1)|1)
#define ggetnum(_) ((intptr_t)(_)>>1)
#define gnil gputnum(0)
#define g_putnum gputnum
#define g_getnum ggetnum
#define g_nil gnil
#define LEN(_) (sizeof(_)/sizeof(*_))
#define MIN(p,q) ((p)<(q)?(p):(q))
#define MAX(p,q) ((p)>(q)?(p):(q))

#ifndef g_tco
#define g_tco 1
#endif
#ifndef g_float
#define g_float 0
#endif

typedef intptr_t g_num, g_word;
struct g;
union u;

#if g_tco
#define g_vm(n, ...) struct g *n(struct g *f, union u *Ip, g_num *Hp, g_num *Sp, ##__VA_ARGS__)
#else
#define g_vm(n, ...) struct g *n(struct g *f, ##__VA_ARGS__)
#endif
typedef g_vm(g_vm_t);
enum g_status {
  g_status_ok  = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3, };

struct g {
  union u {
    g_vm_t *ap;
    g_word x;
    union u *m;
    g_word typ; } *ip;
  g_word *hp, *sp;
  struct g_atom {
    g_vm_t *ap;
    intptr_t typ;
    struct g_vec {
      g_vm_t *ap;
      uintptr_t typ, type, rank, shape[]; } *nom;
    uintptr_t code;
    struct g_atom *l, *r; } *symbols;    // 3 
  uintptr_t len;                         // 4 length of core data
  struct g *pool;                        // 5 lower core address
  struct g_root {
    intptr_t *ptr;
    struct g_root *next; } *root;    // 6
  union { uintptr_t t0; intptr_t *cp; }; // 7 copy pointer / timestamp
  void *(*malloc)(struct g*, size_t),    // 8
       (*free)(struct g*, void*);        // 9
  union {
#define g_nvars 16
    intptr_t v[g_nvars];
    struct {
      struct g_table { g_vm_t *ap; intptr_t typ; uintptr_t len, cap;
        struct g_entry { intptr_t key, val; struct g_entry *next; } **tab;
      } *dict,
        *macro;
      struct g_atom
        *quote,
        *begin,
        *let,
        *cond,
        *lambda;
      intptr_t u[]; }; };
  intptr_t end[]; };

struct g_in {
  int (*getc)(struct g*, struct g_in*),
      (*ungetc)(struct g*, struct g_in*, int),
      (*eof)(struct g*, struct g_in*); };

struct g_out {
  struct g *(*putc)(struct g*, struct g_out*, int); };

struct g_def { char const *n; intptr_t x; };

enum g_status gfin(struct g*);

#define g_vt_char g_vt_i8
uintptr_t
  g_clock(void); // used by garbage collector
int
  g_stdin_getc(struct g*),
  g_stdin_ungetc(struct g*, int),
  g_stdin_eof(struct g*),
  strncmp(char const*, char const*, size_t),
  memcmp(void const*, void const*, size_t);
void
  g_stdout_putc(struct g*, int c),
  *malloc(size_t), free(void*),
  *memcpy(void*restrict, void const*restrict, size_t),
  *memset(void*, int, size_t);
long strtol(char const*restrict, char**restrict, int);
size_t strlen(char const*);
g_vm_t ret0, curry;
struct g
  *gini(void),
  *ginis(uintptr_t, void*),
  *ginid(void *(*)(struct g*, uintptr_t), void (*)(struct g*, void*)),
  *gfprintf(struct g*, struct g_out*, const char*, ...),
  *gputn(struct g*, struct g_out*, intptr_t, uintptr_t),
  *geval(struct g*),
  *gevals(struct g*, const char*),
  *gfread1(struct g*, struct g_in*),
  *greadsi(struct g*, struct g_in*),
  *gfwrite1(struct g*, struct g_out*),
  *gdef1(struct g*, const char*),
  *gdef(struct g*, const char*, g_word),
  *gdefs(struct g*, uintptr_t, struct g_def*),
  *gpush(struct g*, uintptr_t, ...),
  *gstrof(struct g*, const char*),
  *gwrite1(struct g*),
  *gread1(struct g*),
  *gconsl(struct g*),
  *gconsr(struct g*);

#define g_inline inline __attribute__((always_inline))
#define g_noinline __attribute__((noinline))
#define g_log1(f) (g_write1(f),gputc(f, f->out, '\n'))
#define g_digits "0123456789abcdefghijklmnopqrstuvwxyz"
static g_inline struct g *gpop(struct g *f, uintptr_t m) {
  if (g_ok(f)) f->sp += m;
  return f; }
static g_inline struct g *geval_(struct g*f) { return gpop(geval(f), 1); }
static g_inline struct g *gevals_(struct g*f, const char*s) { return gpop(gevals(f, s), 1); }
static g_inline size_t b2w(size_t b) {
  size_t q = b / sizeof(g_word), r = b % sizeof(g_word);
  return q + (r ? 1 : 0); }
static g_inline struct g*gputc(struct g*f, struct g_out *o, int c) { return o->putc(f, o, c); }
static g_inline int ggetc(struct g*f, struct g_in *i) { return i->getc(f, i); }
static g_inline int gungetc(struct g*f, struct g_in *i, int c) { return i->ungetc(f, i, c); }
static g_inline int geof(struct g*f, struct g_in *i) { return i->eof(f, i); }

#if g_f64
#define g_gnum(_) (_)
#define g_pnum(_) (_)
#else
static g_inline g_num g_gnum(g_num n) {
  return n >> 1; }
static g_inline g_num g_pnum(g_num n) {
  return (n << 1) | 1; }
static g_inline g_num *g_ptr(g_num n) {
  return (g_num*) n; }
#endif

#if g_tco
#define Ap(g, f, ...) g(f, Ip, Hp, Sp, ##__VA_ARGS__)
#define Continue() Ap(Ip->ap, f)
#define Pack(f) (f->ip = Ip, f->hp = Hp, f->sp = Sp)
#define Unpack(f) (Ip = f->ip, Hp = f->hp, Sp = f->sp)
#else
#define Ap(g, f, ...) g(f, ##__VA_ARGS__)
#define Continue() f
#define Hp f->hp
#define Sp f->sp
#define Ip f->ip
#define Pack(f) ((void)0)
#define Unpack(f) ((void)0)
#endif

#endif
