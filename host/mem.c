// host/mem.c -- 8-byte word slots on a cask, low 4 bytes live: the flat solver's
// state (crew/sat/flat.l), where the byte-at-a-time accessors cost 4 dispatches
// per read. Host-only, auto-globbed + registered under MODULE 'mem (no
// love.c/love.h/main.c edit; nothing on the bare book).
//
//   ((from 'mem 'peepw) c i)   -> the word at slot i, low 4 bytes | () misuse
//   ((from 'mem 'pinw) c i v)  -> lay v zero-extended into slot i -> c | () misuse
//
// value ops, so absence/misuse answers (); flat.l binds getw/putw to them when
// the module is registered, the byte path staying the portable twin
// (freestanding targets have no host glob).
#include "love.h"
#include <string.h>
#include <stdint.h>

static lvm(lvm_peepw) {
 ai_word c = Sp[0], out = ZeroPoint;
 if (!(c & 1) && ((union u*) c)->ap == lvm_cask && (Sp[1] & 1)) {
  intptr_t i = getcharm(Sp[1]);
  struct ai_str *s = ((struct ai_cask*) c)->str;
  if (i >= 0 && (uintptr_t) (i + 1) * 8 <= s->len) {
   uint64_t w;
   memcpy(&w, s->bytes + 8 * i, 8);
   out = putcharm((intptr_t) (w & 0xffffffffu)); } }
 Sp[1] = out;
 Sp += 1; Ip += 1; ai_musttail return Continue(); }

static lvm(lvm_pinw) {
 ai_word c = Sp[0], out = ZeroPoint;
 if (!(c & 1) && ((union u*) c)->ap == lvm_cask && (Sp[1] & 1) && (Sp[2] & 1)) {
  intptr_t i = getcharm(Sp[1]);
  uint64_t v = (uint64_t) getcharm(Sp[2]) & 0xffffffffu;
  struct ai_str *s = ((struct ai_cask*) c)->str;
  if (i >= 0 && (uintptr_t) (i + 1) * 8 <= s->len) {
   memcpy(s->bytes + 8 * i, &v, 8);
   out = c; } }
 Sp[2] = out;
 Sp += 2; Ip += 1; ai_musttail return Continue(); }

static union u const
  nif_peepw[]  = {{lvm_cur}, {.x = putcharm(2)}, {lvm_peepw}, {lvm_ret0}},
  nif_pinw[]   = {{lvm_cur}, {.x = putcharm(3)}, {lvm_pinw},  {lvm_ret0}};
AiModNif("mem", "peepw", nif_peepw);
AiModNif("mem", "pinw",  nif_pinw);
