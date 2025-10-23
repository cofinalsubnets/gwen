#include "i.h"

g_cp_t *t_cp[] = {
  [g_ty_two] = cp_two,
  [g_ty_sym] = cp_sym,
  [g_ty_str] = cp_str,
  [g_ty_tbl] = cp_tbl, };
g_wk_t *t_wk[] = {
  [g_ty_two] = wk_two,
  [g_ty_sym] = wk_sym,
  [g_ty_tbl] = wk_tbl,
  [g_ty_str] = wk_str, };
g_xx_t *t_xx[] = {
  [g_ty_two] = xx_two,
  [g_ty_sym] = xx_sym,
  [g_ty_str] = xx_str,
  [g_ty_tbl] = xx_tbl, };
g_em_t *t_em[] = {
  [g_ty_two] = em_two,
  [g_ty_tbl] = em_tbl,
  [g_ty_sym] = em_sym,
  [g_ty_str] = em_str, };
g_vm *t_ap[] = {
  [g_ty_two] = self,
  [g_ty_tbl] = self,
  [g_ty_sym] = self,
  [g_ty_str] = self, };
g_id_t *t_id[] = {
  [g_ty_two] = eq_two,
  [g_ty_sym] = neql,
  [g_ty_tbl] = neql,
  [g_ty_str] = eq_str, };

void ini_pair(g_pair *w, g_word a, g_word b) {
  w->ap = data;
  w->typ = g_ty_two;
  w->a = a;
  w->b = b; }

void ini_table(g_table *t, uintptr_t len, uintptr_t cap, struct entry**tab) {
  t->ap = data;
  t->typ = g_ty_tbl;
  t->len = len;
  t->cap = cap;
  t->tab = tab; }

void ini_str(g_string *s, uintptr_t len) {
  s->ap = data;
  s->typ = g_ty_str;
  s->len = len; }

void ini_sym(symbol *y, string *nom, uintptr_t code) {
  y->ap = data;
  y->typ = g_ty_sym;
  y->nom = nom;
  y->code = code;
  y->l = y->r = 0; }

void ini_anon(symbol *y, uintptr_t code) {
  y->ap = data;
  y->typ = g_ty_sym;
  y->nom = 0;
  y->code = code; }

bool twop(g_word _) { return celp(_) && typ(_) == g_ty_two; }
bool strp(g_word _) { return celp(_) && typ(_) == g_ty_str; }
bool tblp(g_word _) { return celp(_) && typ(_) == g_ty_tbl; }
bool symp(g_word _) { return celp(_) && typ(_) == g_ty_sym; }

static NoInline bool eql_neq(g_core *f, g_word a, g_word b) {
  return celp(a | b) &&
         cell(a)->ap == data &&
         cell(b)->ap == data &&
         typ(a) == typ(b) &&
         t_id[typ(a)](f, a, b); }

// default equality method for things that are only equal to themselves
bool neql(g_core *f, g_word a, g_word b) { return false; }

bool eql(g_core *f, g_word a, g_word b) {
  return a == b || eql_neq(f, a, b); }
