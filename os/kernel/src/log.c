#include "k.h"
#include "fb.h"

void  k_dbg(g_core *f) {
  k_log("\nf@0x"), k_log_n((uintptr_t) f, 16);
  if (f && g_ok(f)) {
    k_log("\n  ip=0x"), k_log_n((uintptr_t) f->ip, 16);
    k_log("\n  len="), k_log_n(f->len, 10);
    k_log("\n  allocd="), k_log_n(f->hp - f->end, 10);
    k_log("\n  stackd="), k_log_n((g_word*) f + f->len - f->sp, 10); } }

void k_log(const char *msg) {
  g_fb32_cur_px_msg(&k_fb, 0xffeeddcc, msg); }

static void k_log_n_r(uintptr_t n, uintptr_t base) { if (n) k_log_n(n, base); }
void k_log_n(uintptr_t n, uintptr_t base) {
  uintptr_t dig = n % base;
  k_log_n_r(n / base, base);
  const char d[2] = {digits[dig], 0};
  k_log(d); }
