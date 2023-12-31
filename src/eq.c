#include "i.h"

Vm(Xp) {
  ip = (thread) sp[1];
  sp[1] = twop(sp[0]) ? putnum(-1) : nil;
  return ip->ap(f, ip, hp, sp + 1); }

Vm(Np) {
  ip = (thread) sp[1];
  sp[1] = nump(sp[0]) ? putnum(-1) : nil;
  return ip->ap(f, ip, hp, sp + 1); }

Vm(Sp) {
  ip = (thread) sp[1];
  sp[1] = strp(sp[0]) ? putnum(-1) : nil;
  return ip->ap(f, ip, hp, sp + 1); }

typedef bool eqr(state, word, word);
static eqr eq_str, eq_two;
static eqr *eqs[] = { [Pair] = eq_two, [String] = eq_str, };

bool eql(state f, word a, word b) {
  if (a == b) return true;
  if (nump(a | b) || ptr(a)->ap != data) return false;
  return eqs[ptr(a)[1].x](f, a, b); }

// FIXME can overflow the stack
static bool eq_two(state f, word x, word y) {
  if (!htwop(ptr(y))) return false;
  return eql(f, A(x), A(y)) && eql(f, B(x), B(y)); }

static bool eq_str(state f, word x, word y) {
  if (!hstrp((thread) y)) return false;
  string a = (string) x, b = (string) y;
  if (a->len != b->len) return false;
  return 0 == strncmp(a->text, b->text, a->len); }
