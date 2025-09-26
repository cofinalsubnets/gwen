#ifndef gw_h
#define gw_h
#include <stdint.h>
#include <stddef.h>

typedef intptr_t g_word;
typedef struct g_core g_core;
typedef union g_cell g_cell;
typedef struct g_type g_type;
typedef g_core *g_vm(g_core*, g_cell*, g_word*, g_word*);
union g_cell { g_vm *ap; g_word x; g_cell *m; g_type *typ; };

typedef enum g_status {
  g_status_ok = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_status;


g_core
  *g_ini(void),
  *g_strof(g_core*, const char*),
  *g_symof(g_core*, const char*),
  *g_read1(g_core*),
  *g_read1s(g_core*, const char*),
  *g_tbl(g_core*),
  *g_run(g_core*),
  *g_push(g_core*, uintptr_t, ...),
  *g_pop(g_core*, uintptr_t),
  *g_cons_l(g_core*),
  *g_cons_r(g_core*),
  *g_eval(g_core*);

uintptr_t g_height(g_core*);
enum g_status
  g_fin(g_core*);

void
  g_dbg(g_core*, const char*),
  g_write1(g_core*);

#define g_nil 1
#define g_core_of(f) ((g_core*)((g_word)(f)&~3))
#define g_code_of(f) ((g_status)((g_word)(f)&3))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#endif
