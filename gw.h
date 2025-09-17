#ifndef gw_h
#define gw_h
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>

typedef intptr_t g_word;
typedef struct g_core g_core;
typedef union g_cell g_cell;
typedef g_core *g_vm(g_core*, g_cell*, g_word*, g_word*);

typedef enum g_status {
  g_status_ok = 0,
  g_status_oom = 1,
  g_status_err = 2,
  g_status_eof = 3,
} g_status;

enum g_var {
  g_var_ip,
  g_var_dict,
  g_var_mac,
  g_var_ev,
  g_var_qt,
  g_var_do,
  g_var_de,
  g_var_if,
  g_var_la,
  g_var_N, };

g_core
  *g_ini_m(void *(*)(g_core*, size_t), void (*)(g_core*, void*)),
  *g_strof(g_core*, const char*),
  *g_read1f(g_core*, FILE*),
  *g_readcs(g_core*, const char*),
  *g_run(g_core*),
  *g_push(g_core*, uintptr_t, ...),
  *g_pop(g_core*, uintptr_t),
  *g_cons_l(g_core*),
  *g_cons_r(g_core*),
  *g_eval(g_core*)
  ;

enum g_status
  g_fin(g_core*);
void
  *g_malloc(g_core*, size_t),
  g_free(g_core*, void*),
  g_writef(g_core*, FILE*)
  ;

g_word
  g_var(g_core*, enum g_var);

#define g_nil 1
#define g_ini() g_ini_m(g_malloc, g_free)
#define g_core_of(f) ((g_core*)((g_word)(f)&~3))
#define g_code_of(f) ((g_status)((g_word)(f)&3))
#define g_ok(f) (g_code_of(f) == g_status_ok)
#endif
