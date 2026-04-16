#include "i.h"
op11(g_vm_strp, strp(Sp[0]) ? gputnum(-1) : g_nil)

g_vm(g_vm_ssub) {
 if (!strp(Sp[0])) Sp[2] = g_nil;
 else {
  struct g_vec*s = (struct g_vec*) Sp[0], *t;
  intptr_t i = odd(Sp[1]) ? ggetnum(Sp[1]) : 0,
           j = odd(Sp[2]) ? ggetnum(Sp[2]) : 0;
  i = MAX(i, 0);
  i = MIN(i, (word) len(s));
  j = MAX(j, i);
  j = MIN(j, (word) len(s));
  if (i == j) Sp[2] = g_nil;
  else {
   size_t req = str_type_width + b2w(j - i);
   Have(req);
   t = (struct g_vec*) Hp;
   Hp += req;
   ini_str(t, j - i);
   memcpy(txt(t), txt(s) + i, j - i);
   Sp[2] = (word) t; } }
 return Ip += 1, Sp += 2, Continue(); }

g_vm(g_vm_scat) {
 intptr_t a = Sp[0], b = Sp[1];
 if (!strp(a)) Sp += 1;
 else if (!strp(b)) Sp[1] = a, Sp += 1;
 else {
  struct g_vec *x = vec(a), *y = vec(b), *z;
  uintptr_t
   len = len(x) + len(y),
   req = str_type_width + b2w(len);
  Have(req);
  z = (struct g_vec*) Hp;
  Hp += req;
  ini_str(z, len);
  memcpy(txt(z), txt(x), len(x));
  memcpy(txt(z) + len(x), txt(y), len(y));
  Sp[1] = word(z); }
 return Ip++, Continue(); }

static size_t const vt_size[] = { [g_vect_u8]  = 1, };

uintptr_t g_vec_bytes(struct g_vec *v) {
 uintptr_t len = vt_size[v->type],
           rank = v->rank,
           *shape = v->shape;
 while (rank--) len *= *shape++;
 return sizeof(struct g_vec) + v->rank * sizeof(word) + len; }

static void ini_vecv(struct g_vec *v, uintptr_t type, uintptr_t rank, va_list xs) {
 uintptr_t *shape = v->shape;
 v->ap = g_vm_data;
 v->typ = vec_q;
 v->type = type;
 v->rank = rank;
 while (rank--) *shape++ = va_arg(xs, uintptr_t); }

void ini_vec(struct g_vec *v, uintptr_t type, uintptr_t rank, ...) {
 va_list xs;
 va_start(xs, rank);
 ini_vecv(v, type, rank, xs);
 va_end(xs); }

static struct g *vec0(struct g*f, uintptr_t type, uintptr_t rank, ...) {
 uintptr_t len = vt_size[type];
 va_list xs;
 va_start(xs, rank);
 for (uintptr_t i = rank; i--; len *= va_arg(xs, uintptr_t));
 va_end(xs);
 uintptr_t nbytes = sizeof(struct g_vec) + rank * sizeof(word) + len,
           ncells = b2w(nbytes);
 f = g_have(f, ncells + 1);
 if (g_ok(f)) {
  struct g_vec *v = bump(f, ncells);
  *--f->sp = word(v);
  va_start(xs, rank);
  ini_vecv(v, type, rank, xs);
  memset(v->shape + rank, 0, len);
  va_end(xs); }
 return f; }

struct g *g_strof(struct g *f, char const *cs) {
 uintptr_t len = 0;
 for (char const *ks = cs; *ks++; len++);
 f = vec0(f, g_vect_char, 1, len);
 if (g_ok(f)) memcpy(txt(f->sp[0]), cs, len);
 return f; }
