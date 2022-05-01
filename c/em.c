#include "em.h"
#include <time.h>
#include <stdlib.h>

// initialization helpers
static NoInline bool inst(em v, const char *a, ll *b) {
  ob z;
  bind(z, interns(v, a));
  return !!tbl_set(v, v->glob[Topl], z, putnum((ob) b)); }

static NoInline bool prim(em v, const char *a, ll *i) {
  ob nom;
  yo prim;
  bind(nom, pair(v, interns(v, a), nil));
  with(nom, prim = cells(v, 4));
  bind(prim, prim);
  prim[0].ll = i;
  prim[1].ll = (ll*) nom;
  prim[2].ll = NULL;
  prim[3].ll = (ll*) prim;
  return !!tbl_set(v, v->glob[Topl], A(nom), (ob) prim); }

// lips destructor
void fin(em v) { if (v) free(v->pool), free(v); }

// lips constructor
em ini(void) {
  em v;
  ob _;
  bind(v, malloc(sizeof(struct em)));

  v->rand = lcprng(v->t0 = clock());
  v->len = 1;
  v->pool = NULL;
  v->mm = NULL;
  v->fp = (fr) (v->hp = v->sp = (void*) sizeof (void*));
  v->ip = (yo) (v->xp = v->syms = nil);
  setptr(v->glob, nil, NGlobs);

#define Bind(x) if(!(x))goto fail
  Bind(v->glob[Topl] = table(v));
  Bind(v->glob[Macs] = table(v));
#define register_inst(a, b)if(b){Bind(prim(v,b,a));}else{Bind(inst(v, "i-"#a,a));}
  insts(register_inst)
#define bsym(i,s) Bind(v->glob[i]=interns(v,s))
  bsym(Eval, "ev");
  bsym(Apply, "ap");
  bsym(Def, ":");
  bsym(Cond, "?");
  bsym(Lamb, "\\");
  bsym(Quote, "`");
  bsym(Seq, ",");
  bsym(Splat, ".");
#define def(s, x) Bind(_=interns(v,s));Bind(tbl_set(v,v->glob[Topl],_,x))
  def("_ns", v->glob[Topl]);
  def("_macros", v->glob[Macs]);
  return v; fail:
  return fin(v), NULL; }
