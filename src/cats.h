// cats.h -- the baked source both frontends warm from. src/main.c (hosted) and
// src/kmain.c (inle) run the same egg and register the same modules, and the artifact
// carries BOTH of them -- so these are one definition (src/cats.c) rather than a static
// apiece, which is the whole prel said twice in every link.
// ⚠ not love0's: it LAYS these headers, and its own boot rides the 0.h twins.
#ifndef AI_CATS_H
#define AI_CATS_H

// the egg's four texts, in ai_egg_'s own argument order -- ai_cat_prel carries ev's
// half spliced after prel's, which is the corpus that call wants.
extern char const ai_cat_egg[], ai_cat_p1[], ai_cat_prel[], ai_cat_post[];

// every module this build carries, as (module 'nm ..) source: eval it once and each
// later `use` is a pure splice.
extern char const ai_cat_mods[];

#endif
