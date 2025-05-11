#include "i.h"

static NoInline Word pushsr(Core *f, uintptr_t m, uintptr_t n, va_list xs) {
  if (!n) return p_please(f, m) ? m : n;
  Word x = va_arg(xs, Word), y;
  avec(f, x, y = pushsr(f, m, n - 1, xs));
  return y ? *--f->sp = x : y; }

static NoInline Word vpushs(Core *f, uintptr_t m, va_list xs) {
  Word n, r = 0;
  if (avail(f) < m) r = pushsr(f, m, m, xs);
  else for (n = 0, f->sp -= m; n < m; f->sp[n++] = r = va_arg(xs, Word));
  return r; }

Word pushs(Core *f, uintptr_t m, ...) {
  va_list xs; va_start(xs, m);
  Word r = vpushs(f, m, xs);
  va_end(xs);
  return r; }

size_t p_drop(Core *f, size_t n) {
  size_t h = p_height(f);
  n = min(n, h);
  f->sp += n;
  return n; }

size_t p_height(Core *f) { return f->pool + f->len - f->sp; }
