#include "lips.h"
#include "terp.h"
#include "mem.h"
#include "write.h"
#include "hom.h"

// errors
Vm(fail) {
  return Pack(), errp(v, "fail"), panic(v); }

Vm(type_error) {
  enum tag exp = v->xp, act = kind(xp);
  Pack();
  errp(v, "wrong type : %s for %s", tnom(act), tnom(exp));
  return panic(v); }

Vm(oob_error) {
  i64 a = v->xp, b = v->ip;
  return Pack(), errp(v, "oob : %d >= %d", a, b), panic(v); }

Vm(ary_error) {
  i64 a = N(Argc), b = v->xp;
  return Pack(), errp(v, arity_err_msg, a, b), panic(v); }

Vm(div_error) { return Pack(), errp(v, "/ 0"), panic(v); }

// type/arity checking
#define DTc(n, t) Vm(n) {\
  if (kind(xp-t)==0) Next(1);\
  v->xp = t; Jump(type_error); }
DTc(idZ, Num) DTc(idH, Hom) DTc(idT, Tbl) DTc(id2, Two)
Vm(arity) {
  obj reqd = (obj) H(ip)[1];
  if (reqd <= Argc) Next(2);
  else Jump((v->xp = N(reqd), ary_error)); }

obj panic(lips v) {
  v->fp = v->sp = v->pool + v->len;
  v->xp = v->ip = nil;
  return 0; }

static NoInline u0 show_call(lips v, obj ip, mem fp) {
  fputc('(', stderr);
  emit(v, ip, stderr);
  mem top = v->pool + v->len;
  for (i64 i = 0, argc = fp == top ? 0 : N(Argc); i < argc;)
    fputc(' ', stderr), emit(v, Argv[i++], stderr);
  fputc(')', stderr); }

u0 errp(lips v, const char *msg, ...) {
  obj ip = v->ip, *fp = v->fp;
  va_list xs; va_start(xs, msg);
  fputs("# ", stderr), show_call(v, ip, fp);
  fputs(" : ", stderr), vfprintf(stderr, msg, xs);
  fputc('\n', stderr), va_end(xs);
  // print backtrace
  for (mem top = v->pool + v->len;;) {
    ip = Retp, fp += Width(frame) + N(Argc) + N(Subr);
    if (fp == top) return;
    fputs("#  in ", stderr);
    show_call(v, ip, fp);
    fputc('\n', stderr); } }