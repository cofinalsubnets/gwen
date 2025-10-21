#include "k.h"
#include "log.h"

void k_log_c(const char *msg, uint32_t fg, uint32_t bg) {
  g_fb32_cur_msg(&k_fb, msg, fg, bg); }

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
