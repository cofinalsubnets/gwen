#ifndef gw_h
#define gw_h
#include <stdint.h>
#include <stddef.h>

#define g_core_of(f) ((struct g_core*)((g_word)(f)&~3))
#define g_code_of(f) ((enum g_status)((g_word)(f)&3))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#define g_nil 1

typedef intptr_t g_word;
typedef union g_cell {
  struct g_core *(*ap)(struct g_core*, union g_cell*, g_word*, g_word*);
  g_word x;
  union g_cell *m;
  struct g_type *typ;
} g_cell;
_Static_assert(sizeof(g_cell) == sizeof(g_word));

typedef struct g_core g_core,
  *g_vm(g_core*, g_cell*, g_word*, g_word*);
enum g_status {
  g_status_ok  = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_fin(g_core*);

g_core
  *g_ini(void),
  *g_read1s(g_core*, const char*),
  *g_eval(g_core*),
  *g_apply(g_core*),
  *g_read1(g_core*),
  *g_write1(g_core*),
  *g_push(g_core*, uintptr_t, ...),
  *g_pop(g_core*, uintptr_t),
  *g_strof(g_core*, const char*),
  *g_symof(g_core*, const char*),
  *g_cons_l(g_core*),
  *g_cons_r(g_core*);
#endif
