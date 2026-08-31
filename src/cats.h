// cats.h -- the baked source both frontends warm from. src/main.c (hosted) and
// src/kmain.c (inle) run the same egg and register the same modules, and the artifact
// carries BOTH of them -- so these are one definition (src/cats.c) rather than a static
// apiece, which is the whole prel said twice in every link.
// ⚠ not love0's: it LAYS these headers, and its own boot rides the 0.h twins.
#ifndef AI_CATS_H
#define AI_CATS_H

// the texts ride DEFLATED, so they are reached through calls rather than named: the
// blobs and the inflate are src/cats.c's alone, and a frontend asks for the effect.
// warm the egg from its four texts -- prel carries ev's half spliced after its own.
struct ai *ai_cats_egg(struct ai *g);

// register every module this build carries: eval them once and each later `use` is a
// pure splice.
struct ai *ai_cats_mods(struct ai *g);

#endif
