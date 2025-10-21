#ifndef _g_k_log_h
#define _g_k_log_h
#include "k.h"
extern g_fb32 k_fb;
void 
  k_log(const char *msg),
  k_log_c(const char *msg, uint32_t fg, uint32_t bg),
  k_log_n(uintptr_t n, uintptr_t base),
  k_log_char(char);
#endif
