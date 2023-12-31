#include "i.h"
void l_fin(state f) {
#ifndef _gwen_mem_static
  if (f) free(f->pool < f->loop ? f->pool : f->loop),
         f->pool = f->loop = NULL;
#endif
}

#define binop() {cur}, {.x = putnum(2)}
static union cell
  p_print[] = {{print}},
  p_not[] = {{not}},
  p_cons[] = {binop(), {xons}},
  p_car[] = {{car}},
  p_cdr[] = {{cdr}},
  p_le[] = {binop(), {le}},
  p_lt[] = {binop(), {lt}},
  p_eq[] = {binop(), {eq}},
  p_ge[] = {binop(), {ge}},
  p_gt[] = {binop(), {gt}},
  p_slen[] = {{slen}},
  p_sget[] = {binop(), {sget}},
  p_ssub[] = {{cur}, {.x = putnum(3)}, {ssub}},
  p_2p[] = {{Xp}},
  p_np[] = {{Np}},
  p_sp[] = {{Sp}},
  p_mbind[] = {binop(), {mbind}},
//  p_p[] = { {pr}},
//  p_pp[] = { {ppr} },
//  p_sp[] = { {spr} },
//  p_psp[] = { {pspr} },
  p_pc[] = {{prc}},
  p_quot[] = {binop(), {quot}},
  p_rem[] = {binop(), {rem}},
  p_mul[] = {binop(), {mul}},
  p_sub[] = {binop(), {sub}},
  p_add[] = {binop(), {add}};

static status l_ini_dict(state f) {
  static struct { const char *n; word x; } ini_dict[] = {
    { "+", (word) p_add },
    { "-", (word) p_sub },
    { "*", (word) p_mul },
    { "/", (word) p_quot },
    { "%", (word) p_rem },
    { "=", (word) p_eq },
    { "<", (word) p_lt },
    { "<=", (word) p_le },
    { ">", (word) p_gt },
    { ">=", (word) p_ge },
    { ".", (word) p_print },
    { "~", (word) p_not },
    { "X", (word) p_cons },
    { "A", (word) p_car },
    { "B", (word) p_cdr },
    { "sget", (word) p_sget},
    { "ssub", (word) p_ssub},
    { "slen", (word) p_slen},
    { "s?", (word) p_sp},
    { "n?", (word) p_np},
    { "X?", (word) p_2p},
    { "::", (word) p_mbind},
//    { "p", (word) p_p },
    { "pc", (word) p_pc },
//    { "pp", (word) p_pp },
//    { "sp", (word) p_sp },
//    { "psp", (word) p_psp },
  };
  for (int i = 0; i < sizeof(ini_dict)/sizeof(*ini_dict); i++) {
    string s = strof(f, ini_dict[i].n);
    pair w = s ? cons(f, (word) s, ini_dict[i].x) : 0,
         x = w ? cons(f, (word) w, f->dict) : 0;
    if (!(f->dict = (word) x)) return Oom; }
  return Ok; }

status l_ini(state f) {
  memset(f, 0, sizeof(struct gwen));
#ifdef _gwen_mem_static
  const size_t len = _gwen_mem_static;
  static word _pool[2 * _gwen_mem_static];
  word *pool = _pool;
#else
  const size_t len = 1;
  word *pool = malloc(2 * len * sizeof(word));
  if (!pool) return Oom;
  f->t0 = clock();
#endif
  f->len = len;
  f->pool = f->hp = pool;
  f->loop = f->sp = pool + len;
  f->dict = f->macro = nil;
  status s = l_ini_dict(f);
  if (s != Ok) l_fin(f);
  return s; }
