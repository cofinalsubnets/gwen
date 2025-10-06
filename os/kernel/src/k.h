#ifndef _g_k_h
#define _g_k_h
#include <limine.h>
#include "i.h"
#include "libc_internal.h"


bool
  boot_ok(void);

void
  k_log(const char*),
  k_log_n(uintptr_t n, uintptr_t base),
  k_dbg(g_core*),
  k_fin(void),
  k_reset(void)
  ;

void
  k_log(const char*),
  k_log_n(uintptr_t n, uintptr_t base),
  k_dbg(g_core*);

void k_init(void);

#endif
