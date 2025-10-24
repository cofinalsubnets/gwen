// thanks !!
#ifndef _g_i_h
#define _g_i_h
#include "g.h"
typedef intptr_t g_word;
#include <stdbool.h>
#define Inline inline __attribute__((always_inline))
#define NoInline __attribute__((noinline))
#define LEN(_) (sizeof(_)/sizeof(*_))
#define MIN(_,__) ((_)<(__)?(_):(__))
#define MAX(_,__) ((_)>(__)?(_):(__))
typedef Vm(g_vm_t);
union x { g_vm_t *ap; intptr_t x; union x *m; uintptr_t typ; };
enum g_var {
  g_var_dict,
  g_var_mac,
  g_var_qt,
  g_var_do,
  g_var_de,
  g_var_if,
  g_var_la,
  g_var_N, };
struct g {
  intptr_t *hp, *sp; // heap and stack pointers
  union x *ip;
  union {
    intptr_t vars[g_var_N];
    struct {
      struct g_table *dict, *macro;
      struct g_symbol *quote, *begin, *let, *cond, *lambda; }; };
  struct g_symbol *symbols; // symbol tree
  // memory pool size
  uintptr_t len;
  intptr_t *pool; // lower core address
  struct g_mem_root *safe;
  union { // gc state
    uintptr_t t0; // end of last gc timestamp
    intptr_t *cp; }; // copy pointer
  void *(*malloc)(struct g*, size_t),
        (*free)(struct g*, void*);
  intptr_t end[]; }; // end of struct == initial heap pointer for this core
                     //
typedef struct g_input {
  int (*getc)(struct g_input*),
      (*ungetc)(struct g_input*, int),
      (*eof)(struct g_input*);
} g_input, input;


typedef struct g_string {
  g_vm_t *ap;
  uintptr_t typ;
  uintptr_t len;
  char text[];
} g_string, string;

#include "sys.h"
enum g_ty { g_ty_two, g_ty_str, g_ty_sym, g_ty_tbl, };
uintptr_t
  g_sys_clock(void);
struct g
  *g_read1i(g_core*, g_input*),
  *g_readsi(g_core*, input*);
g_vm_t
  prc, ret0, curry,
  data, dot, self;

#define getnum g_getnum
#define nil g_nil
#define str(_) ((struct g_string*)(_))
#define pop1(f) (*(f)->sp++)

bool twop(intptr_t), strp(intptr_t), tblp(intptr_t), symp(intptr_t);
void *malloc(size_t), free(void*),
     *memcpy(void *restrict, const void*restrict, size_t),
     *memset(void*, int, size_t);
long strtol(const char*restrict, char**restrict, int);
size_t strlen(const char*);
int strncmp(const char*, const char*, size_t);
typedef g_vm_t g_vm;
#endif
