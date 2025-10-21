#include "k.h"
#include "log.h"
#include <stdarg.h>

void k_log_c(const char *msg, uint32_t fg, uint32_t bg) {
  k_cur_msg(msg, fg, bg); }

void k_log_char(char c) {
  char s[2] = {c, 0};
  k_log(s); }

void k_log(const char *msg) {
  k_log_c(msg, 0xffeeddcc, 0); }

static void k_log_n_r(uintptr_t n, uintptr_t base) {
  if (n) k_log_n(n, base); }

void k_log_n(uintptr_t n, uintptr_t base) {
  uintptr_t dig = n % base;
  k_log_n_r(n / base, base);
  const char d[2] = {digits[dig], 0};
  k_log(d); }

void k_logf(const char *fmt, ...) {
  va_list xs;
  va_start(xs, fmt);
  while (*fmt) {
    char c = *fmt++;
    if (c != '%') k_log_char(c);
    else re:
      switch (c = *fmt++) {
      case 0: return;
      default: k_log_char(c); break;
      case 'l': goto re;
      case 'd': k_log_n(va_arg(xs, uintptr_t), 10); break;
      case 'x': k_log_n(va_arg(xs, uintptr_t), 16); break;
      case 'o': k_log_n(va_arg(xs, uintptr_t), 8); break;
      case 'b': k_log_n(va_arg(xs, uintptr_t), 2); break; } } }
