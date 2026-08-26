// cats.c -- the baked source, one copy for the whole link: the egg's four texts and the
// module registry. src/main.c and src/kmain.c both warm from these; see src/cats.h.
#include "love.h"
#include "cats.h"

char const ai_cat_egg[] =
#include "egg.h"
 , ai_cat_p1[] =
#include "p1.h"
 , ai_cat_prel[] =
#include "prel.h"
 " "
#include "ev.h"
 , ai_cat_post[] =
#include "post.h"
 ;

// ONE registry, both frontends. the order is the dependency order: overlay's body reads
// (from 'kanren ..) as it registers.
// K_TEST drops holo and its backend, and only those -- nothing its corpus runs opens with
// (use 'holo), where the kore cat's asbook.l does. that pie is unbaked by construction, so
// it takes the egg warm on every gate boot under TCG, and holo+x64 registers ~1.5G
// instructions against kanren's 302M, overlay's 128M, peg's 80M.
// the shipped kernel takes the whole set. it wakes the artifact's image, so this text is
// its fallback lane and the wake is what a box gets -- and `from` on an unregistered
// module answers () rather than scaring, so a short registry is a silent wrong binding.
char const ai_cat_mods[] =
#include "coin.h"
#include "rng.h"
#include "q.h"
#include "glob.h"
#include "kanren.h"
#include "overlay.h"
#include "uu.h"
#ifndef K_TEST
#include "holo.h"
#if defined(__x86_64__)
#include "amd64.h"
#elif defined(__aarch64__)
#include "arm64.h"
#elif defined(__riscv)
#include "rv64.h"
#endif
#endif
#include "bao.h"
#include "verbs.h"
#include "re.h"
#include "peg.h"
 ;
