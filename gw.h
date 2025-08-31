#ifndef gw_h
#define gw_h
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct g_core g_core;
typedef enum g_status {
  g_status_ok,
  g_status_oom,
  g_status_eof,
} g_status;

g_core *g_ini(void),
       *g_run(g_core*, const char*, const char**);
void g_fin(g_core*);

typedef intptr_t g_word;
#define core_of(f) ((g_core*)((g_word)(f)&~3))
#define code_of(f) ((g_status)((g_word)(f)&3))
#define encode(f, s) ((g_core*)((g_word)(f)|s))
#define g_ok(f) (code_of(f) == g_status_ok)

typedef enum g_type {
  g_type_fixnum,
  g_type_symbol,
  g_type_pair,
  g_type_string,
  g_type_table,
  g_type_cell,
} g_type;

typedef struct g_string g_string;
typedef struct g_pair g_pair;
typedef struct g_symbol g_symbol;
typedef struct g_table g_table;
typedef union g_cell g_cell;
g_type g_type_of(g_word);
char *g_gs2cs(g_string*);
g_core *g_cs2gs(char*);

#endif
