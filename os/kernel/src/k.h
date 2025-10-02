#ifndef _g_k_h
#define _g_k_h
#include <limine.h>
#include "i.h"
#include "libc_internal.h"

extern volatile struct limine_framebuffer_request framebuffer_request;
extern volatile struct limine_memmap_request memmap_req;
extern volatile struct limine_stack_size_request stack_req;

bool
  boot_ok(void);

void
  k_log(const char*),
  k_log_n(uintptr_t n, uintptr_t base),
  k_dbg(g_core*),
  k_fin(void)
  ;

void idt_ini(void);

#endif
