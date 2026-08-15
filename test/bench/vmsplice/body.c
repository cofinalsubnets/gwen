// a composed body meant to be INSTALLED in a live love via `nif`.
//
// self-contained on purpose: charm arithmetic only, so the .text carries no
// relocation -- no lvm_chain, no ai_mint_zero, no lvm_gc. That is the whole
// difference between a body you can lift out of a .o and one you cannot.
//
// the nif contract (love.c's lvm_nif, arity 1): the cell is
//   [header, src, CODE, interp, lvm_ret, ret n=1]
// the body is entered at value[0] with Ip there, and the glaze's own epilogue
// (love/glaze/emit.l's loopepi) spells the exit as: store acc to Sp[0], Ip += 16
// bytes (2 words), load Ip[0], jump -- i.e. tail to value[2] = lvm_ret.
#include "love.h"

#define BUMP() v = putcharm(getcharm(v) + 1);
#define X4(m)  m m m m
#define X16(m) X4(m) X4(m) X4(m) X4(m)
#define X64(m) X16(m) X16(m) X16(m) X16(m)

// 64 spliced bumps, then the one dispatch
struct ai *jitbody(struct ai *restrict g, union u *Ip, ai_word *Hp, ai_word *restrict Sp) {
  intptr_t v = Sp[0];
  X64(BUMP())
  Sp[0] = v;
  Ip += 2;                       // value[0] -> value[2] == lvm_ret
  ai_musttail return Continue(); }
