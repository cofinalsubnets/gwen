// love.c -- g, stack, gc, sys, str, sym, chain, tray. one translation unit of the runtime;
// the shared layouts and the cross-TU seam are src/love_int.h.
#include "love_int.h"
struct ai_chain;
// this file's own, forward-declared so order within it does not matter.
static bool ai_major_cell(struct ai *g, word *c);
static bool gen_remembered(struct ai *g, word obj);
static char *add_emit(struct ai *g, char *w, word x);
static int stringrank(struct ai *g, word x);
static intptr_t seq_byte(word x);
static lvm(_lvm_help_scare);
static lvm(_lvm_yield_c);
static struct ai *ai_ini_0(struct ai*g, uintptr_t len0, void *(*al)(struct ai*, void*, size_t));
static struct ai *ai_modtab(struct ai *g, char const *mod);
static struct ai_tag *ttag2(struct ai *g, struct ai_gcx *X, union u *k);
static uintptr_t stringlen(struct ai *g, word x);
static void evac_big(struct ai *g, struct ai_gcx *X);
static void evac_chain(struct ai *g, struct ai_gcx *X);
static void evac_data(struct ai *g, struct ai_gcx *X);
static void evac_gem(struct ai *g, struct ai_gcx *X);
static void evac_nom(struct ai *g, struct ai_gcx *X);
static void evac_str(struct ai *g, struct ai_gcx *X);
static void evac_sun(struct ai *g, struct ai_gcx *X);
static void evac_sym(struct ai *g, struct ai_gcx *X);
static void evac_thread(struct ai *g, struct ai_gcx *X);
static void evac_twin(struct ai *g, struct ai_gcx *X);
static void gen_fz_relocate(struct ai *g);
static void gen_minor(struct ai *g);
static void gen_remember(struct ai *g, word obj);
static void gen_scan_inplace(struct ai *g, struct ai_gcx *X, word obj);
static void major_run_finalizers(struct ai *g, struct ai_gcx *X);
static word copy_big(struct ai *g, struct ai_big *src);
static word copy_chain(struct ai *g, struct ai_chain *src);
static word copy_data(struct ai *g, union u *src);
static word copy_gem(struct ai *g, struct ai_gem *src);
static word copy_nom(struct ai *g, struct ai_nom *src);
static word copy_str(struct ai *g, struct ai_str *src);
static word copy_sun(struct ai *g, struct ai_sun *src);
static word copy_sym(struct ai *g, struct ai_mint *src);
static word copy_thread(struct ai *g, struct ai_gcx *X, union u *src);
static word copy_tray(struct ai *g, struct ai_tray *src);
static word copy_twin(struct ai *g, struct ai_twin *src);
static word major_symbols_rebuild(struct ai *g, struct ai_gcx *X, word om);
// the build's version string, generated into out/lib/love_version.h and surfaced
// as `love-version`. -DAiVersion wins (love0 pins "bootstrap" so a new commit never
// relinks the bootstrap); -DAiHaveVersionH says the header exists -- mooncc has
// no __has_include, so the probe alone is not enough.
#ifndef AiVersion
# if defined(AiHaveVersionH) || (defined(__has_include) && __has_include("love_version.h"))
#  include "love_version.h"
# endif
#endif
#ifndef AiVersion
#define AiVersion "unknown"
#endif
const word ai_map_gap_cell = 0;
const struct ai_str0 ai_str_empty = { .ap = lvm_str, .len = 0 };
const struct ai_mint ai_mint_zero = { .ap = lvm_sym, .code = 0 };
// ============================================================================
// g
// ============================================================================
enum ai_status ai_fin(struct ai *g) {
 enum ai_status s = ai_code_of(g);
 if ((g = ai_core_of(g))) {
   for (struct ai_fz *fz = g->fz; fz; fz->fn(fz->p), fz = fz->next); // run finalizers
   // the rem set and the major pool are ai_ini_0's own g->alloc calls, not room inside
   // the nursery -- a frontend that exits never misses them, one that fins to make room
   // for the next runtime gets nothing back without this.
   if (g->rem) g->alloc(g, g->rem, 0);
   if (g->major_pool) g->alloc(g, g->major_pool, 0);
   g->alloc(g, g, 0); }                       // ..the pool is g, so it goes last
 return s; }

// the module lane's target: find-or-make mod's tablet on the registry and push it
// where the book map would sit. over a woken image the found tablet takes the re-pin.
static struct ai *ai_modtab(struct ai *g, char const *mod) {
 if (!ai_ok(g)) return g;
 struct ai *c = ai_core_of(g);
 if (!ai_ok(g = intern(ai_strof(g, mod)))) return g;   // [modnom ..]
 c = ai_core_of(g);
 word m = ai_mapget(c, zero, c->sp[0], c->mods);
 if (m != zero) { c->sp[0] = m; return g; }             // [tablet ..]
 if (!ai_ok(g = map_new(g))) return g;                  // a fresh module: [tablet modnom ..]
 c = ai_core_of(g);
 g = ai_push(g, 3, c->sp[1], c->sp[0], c->mods);        // (key val coll) for mapput
 if (!ai_ok(g = ai_mapput(g))) return g;                // [mods tablet modnom ..]
 c = ai_core_of(g);
 c->sp[2] = c->sp[1], c->sp += 2;                       // [tablet ..]
 return g; }

// every .x here must be immortal -- a nif address, a fixnum, an out-of-pool
// constant. C cannot re-root what it holds in an array, and no ordering fixes it;
// a value that moves arrives on the stack instead (ai_defv).
// mod non-NULL binds the whole table under that module instead of the book.
struct ai *ai_defn(struct ai*g, struct ai_def const*defs, uintptr_t n, char const *mod) {
 for (g = mod ? ai_modtab(g, mod) : ai_push(g, 1, A(ai_core_of(g)->book)); n--;
  g = ai_mapput(intern(ai_strof(ai_push(g, 1, defs[n].x), defs[n].n))));
 ai_core_of(g)->sp++;
 return g; }

// ai_defn's twin for a value that moves: it rides g->sp[0], where the collector
// updates it, and is left there (a second name binds the same one; callers pop).
// the sp[1] re-read happens after the book push, so a collection inside it is accounted for.
struct ai *ai_defv(struct ai *g, char const *nm) {
 if (!ai_ok(g)) return g;
 g = ai_push(g, 1, A(ai_core_of(g)->book));           // [book, value, ..]
 if (!ai_ok(g)) return g;
 g = ai_mapput(intern(ai_strof(ai_push(g, 1, ai_core_of(g)->sp[1]), nm)));
 if (ai_ok(g)) ai_core_of(g)->sp++;                   // [value, ..]
 return g; }

// the nif + instruction registry: one `union u` table, a nif's little stream being a
// run inside it, then def1 -- the name -> value table ai_defn reads into the book,
// carrying each run's address. both are laid from the one roster in nifs.l -- edit
// that, not nifs.h; make relays it and test_clay diffs.
#include "nifs.h"

static lvm(_lvm_yield_c) { return Pack(g), g; }
union u const yield_c[] = { {_lvm_yield_c} };

// lvm_help: the default help ap -- re-encode the raised status, yield to C.
// _lvm_help_scare sits outside lvm_* on purpose: the one designed `ret`
// (vmret sounds lvm_* only), reached by tail call. the status rides the core's b,
// like every other thing an op needs beyond the stack.
static lvm(_lvm_help_scare) { return Pack(g), encode(g, (enum ai_status) g->b); }
lvm(lvm_help) {
 struct ai *c = ai_core_of(g);
 c->b = (ai_word) ai_code_of(g);
 return Ap(_lvm_help_scare, c); }

// reverse-lookup a nif value -> its source name or NULL (the printer renders nifs by name)
char const *ai_nif_name(intptr_t x) {
 for (uintptr_t i = 0; i < countof(def1); i++) if (def1[i].x == x) return def1[i].n;
 return 0; }

static struct ai *ai_ini_0(struct ai*g, uintptr_t len0, void *(*al)(struct ai*, void*, size_t)) {
 memset(g, 0, sizeof(struct ai));      // the core needs no leading ap: () is the const ZeroPoint, never (word)g
 g->len = len0, g->alloc = al;
 g->scare_a = g->scare_b = zero;        // v0..end is GC-walked: raw 0 is not a value
 g->hot_read = g->hot_numap = g->hot_stack = g->hot_compose = g->hot_opfix = g->hot_show = zero;   // unsealed: hot_hook traps until (seal-hook) fills them
 g->hp = g->end, g->sp = (word*) g + len0, g->ip = (union u*) yield_c;
 // the rem set + major pool ride g->alloc: a frontend that cannot supply them cannot run
 g->major_len = ai_major0;
 g->rem = g->alloc(g, NULL, AiRemCap * sizeof(word));
 g->major_pool = g->rem ? g->alloc(g, NULL, 2 * g->major_len * sizeof(word)) : NULL;
 if (!g->major_pool) { if (g->rem) g->alloc(g, g->rem, 0); return encode(g, ai_status_scare); }
 g->major_base = g->major_hp = g->major_pool, g->budget = ai_budget;
 g->minor0 = ai_minor0, g->major0 = ai_major0, g->ratio = ai_gc_ratio;   // the live knobs; `tune` moves them
 g->next_wait_events = ai_wait_in;
 // book + macro maps (lookup-lambdas) then the main task thread.
 if (ai_ok(g = map_new(g)) && ai_ok(g = map_new(g)) && ai_ok(g = ai_have(g, 9))) {
  union u *M = bump(g, 9);            // sp[0]=macro, sp[1]=book (no GC since ai_have)
  M[0].m = M;
  M[1].x = zero;   // sentinel; replaced on first yield
  M[2].x = zero;   // main pid
  M[3].x = zero;   // wake_at: zero means "always runnable"
  M[4].x = putcharm(-1);  // wait_fd: -1 = not waiting on I/O (slot value -1, non-zero)
  M[5].x = putcharm(ai_wait_in);   // wait_events: the read direction, the default
  M[6].x = zero;   // help: nothing heard until the first (hear f)
  M[7].x = zero;   // stdio: the console until the first (wear l)
  g->tasks = tagthread(M, 8);
  g->parked = NULL;   // nothing is fd-parked before the first task ever parks
  // book[zero] = macro (the macro table -- no separate field). both are on the
  // stack; push the zero key so (sp2,sp1,sp0)=(book,macro,zero) for ai_mapput.
  g = ai_push(g, 1, zero);
  g = ai_mapput(g);                     // -> sp[0] = book
  g->book = g->sp[0];                  // henceforth GC-forwarded via the v0..end loop
  // the abyss: g->book holds a chain of books, walked head-first (bookget) --
  // one link today (orth, the boot book); a later layer prepends and shadows.
  // the l-level `book` global stays the orth map (def0 pins A(g->book)).
  if (ai_ok(g = ai_have(g, Width(struct ai_chain)))) {
   struct ai_chain *ly = (void*) bump(g, Width(struct ai_chain));
   ini_chain(ly, g->sp[0], ZeroPoint);
   g->book = (word) ly; }
  g = ai_pop(g, 1);
  // the weak intern map (string -> the canonical atom), created before the
  // first intern (the def tables just below). it lives outside the traced
  // v0 region: a collection clones it untraced and sweeps it at the fixpoint.
  g = map_new(g);
  if (ai_ok(g)) g->symbols = ai_pop1(g);
  if (ai_ok(g = map_new(g))) g->mods = ai_pop1(g);   // the registry, before the first ai_modtab
  struct ai_def def0[] = {
   {"book", A(g->book)},   // the l-level book = the orth map (the chain stays C-side; `books` reads it)
   {"in", (word) &ai_stdin},
   {"out", (word) &ai_stdout},
   {"err", (word) &ai_stderr},
   // the two doors prel builds (tap and jug), so it can stamp the kind it means;
   // mopped at birth like every other raw pointer the compiler folds (love/egg.l)
   {"ci-vt", (word) &ai_ci_vt},
   {"to-vt", (word) &ai_to_vt},
   // max-charm/min-charm: this build's fixnum bounds, exposed so width-specific
   // tests gate on the real boundary (it differs on 32- vs 64-bit ports).
   {"max-charm", putcharm((ai_word)((uintptr_t)-1 >> 2))},
   {"min-charm", putcharm(-(ai_word)((uintptr_t)-1 >> 2) - 1)},
   // love-tco: glazed code continues by tail-jump, which only the threaded build
   // honors -- auto.l reads this and keeps the interpreter on a trampoline build
   {"love-tco", putcharm(ai_tco)}, };
  g = ai_defn(g, def0, countof(def0), 0);
  g = ai_defn(g, def1, countof(def1), 0);
  if (ai_ok(g = ai_strof(g, AiVersion)))            // a live string: off the stack, never an ai_def
   g = ai_pop(ai_defv(g, "love-version"), 1);
  // `love-arch`: the host CPU the glaze emits for, and the assembler target every backend
  // is registered under. A NOM, in the prel's canonical spelling (love/prel.l's arch-canon)
  // -- so a reader compares it against 'amd64 rather than interning a string first, and
  // there is one word for this machine across holo, moon, kore and the seed.
#if defined(__x86_64__)
  #define AiArch "amd64"
#elif defined(__aarch64__)
  #define AiArch "arm64"
#elif defined(__riscv)
  #define AiArch "rv64"
#else
  #define AiArch "other"
#endif
  if (ai_ok(g = intern(ai_strof(g, AiArch))))
   g = ai_pop(ai_defv(g, "love-arch"), 1);
  // the 'missing tag needs nothing here (the raise sites mint it); the reader owns
  // no operator tables -- book['operators] is seeded by the prel and factored at compile time
 }
 return g; }

struct ai *ai_ini_m(void *(*al)(struct ai*, void*, size_t)) {
 uintptr_t const len0 = ai_minor0;   // initial minor pool; grows on demand (gen_grow)
 struct ai *g = al(NULL, NULL, 2 * len0 * sizeof(word));
 return g == NULL ? encode(g, ai_status_scare) : ai_ini_0(g, len0, al); }

void *ai_libc_alloc(struct ai*g, void *p, size_t n) { (void) g; return n ? malloc(n) : (free(p), NULL); }
struct ai *ai_ini(void) { return ai_ini_m(ai_libc_alloc); }

// ============================================================================
// stack
// ============================================================================
struct ai *ai_pushr(struct ai *g, uintptr_t m, uintptr_t n, va_list xs) {
 if (n == m) return ai_please(g, m);
 word x = va_arg(xs, word);
 mm(g, &x);
 g = ai_pushr(g, m, n + 1, xs);
 um(g);
 if (ai_ok(g)) *--g->sp = x;
 return g; }

struct ai *ai_push(struct ai *g, uintptr_t m, ...) {
 if (!ai_ok(g)) return g;
 va_list xs;
 va_start(xs, m);
 uintptr_t n = 0;
 if (avail(g) < m) g = ai_pushr(g, m, n, xs);
 else for (g->sp -= m; n < m; g->sp[n++] = va_arg(xs, word));
 va_end(xs);
 return g; }

struct ai *gxl(struct ai *g) {
 if (ai_ok(g = ai_have(g, Width(struct ai_chain)))) {
  struct ai_chain *p = bump(g, Width(struct ai_chain));
  ini_chain(p, g->sp[0], g->sp[1]);
  *++g->sp = (word) p; }
 return g; }

struct ai *gxr(struct ai *g) {
 if (ai_ok(g = ai_have(g, Width(struct ai_chain)))) {
  struct ai_chain *p = bump(g, Width(struct ai_chain));
  ini_chain(p, g->sp[1], g->sp[0]);
  *++g->sp = (word) p; }
 return g; }

// ============================================================================
// gc
// ============================================================================
lvm(lvm_gc) {
 uintptr_t n = (uintptr_t) g->b;                // Have's ask, left in the scratch slot
 Pack(g);
 if (!ai_ok(g = ai_please(g, n))) return Ap(_lvm_ghelp, g);
 return Resume(); }

word gcp(struct ai*, struct ai_gcx*, word);

static ai_inline void evac_chain(struct ai *g, struct ai_gcx *X) {
 struct ai_chain *w = (struct ai_chain*) X->cp;
 X->cp += Width(struct ai_chain);
 w->a = gcp(g, X, w->a);
 w->b = gcp(g, X, w->b); }

void evac_tray(struct ai *g, struct ai_gcx *X) {
 struct ai_tray *v = tray(X->cp);
 X->cp += b2w(ai_tray_bytes(v));
 if (v->type != ai_O) return;                 // numeric trays are GC leaves (flat payload)
 word *e = (word*) tray_data(v);              // object tray: forward each live element word
 uintptr_t n = tray_nelem(v);
 while (n--) e[n] = gcp(g, X, e[n]); }

static ai_inline void evac_str(struct ai *g, struct ai_gcx *X) {
 X->cp += str_width(str(X->cp)->len); }

static ai_inline void evac_big(struct ai *g, struct ai_gcx *X) {
 X->cp += b2w(ai_big_bytes(big(X->cp))); }

// the lean boxes are flat GC leaves
static ai_inline void evac_gem(struct ai *g, struct ai_gcx *X) {
 X->cp += gem_req; }

static ai_inline void evac_sun(struct ai *g, struct ai_gcx *X) {
 X->cp += sun_req; }

static ai_inline void evac_twin(struct ai *g, struct ai_gcx *X) {
 X->cp += twin_req; }

static ai_inline void evac_sym(struct ai *g, struct ai_gcx *X) {
 X->cp += Width(struct ai_mint); }              // uniform 2 words; copy_sym forwards the serial

static ai_inline void evac_nom(struct ai *g, struct ai_gcx *X) {
 struct ai_nom *w = (struct ai_nom*) X->cp;
 X->cp += Width(struct ai_nom);                 // 4 words; forward the name string (serial + dig are scalars)
 w->name = gcp(g, X, w->name); }

static ai_inline void evac_thread(struct ai *g, struct ai_gcx *X) {
  // tagl ends the thread regardless of scan space, so a young-pointing terminator is never gcp'd as a field
  for (X->cp += 1; !tagl(g, X, X->cp[-1]); X->cp[-1] = gcp(g, X, X->cp[-1]), X->cp++); }

static ai_inline void evac_data(struct ai *g, struct ai_gcx *X) {
  switch (typ(X->cp)) {
   case DTray: return evac_tray(g, X);
   case DMint: return evac_sym(g, X);
   case DNom: return evac_nom(g, X);
   case DChain: return evac_chain(g, X);
   case DString: return evac_str(g, X);
   case DBig: return evac_big(g, X);
   case DGem: return evac_gem(g, X);
   case DSun: return evac_sun(g, X);
   case DTwin: return evac_twin(g, X); }
  __builtin_trap(); }                            // a hot outside enum d: the object is not what its ap says

// ===== generational write barrier =====
// a minor scavenges only [minor, hp) and finds old->young edges through the rem
// set: every edge execution mints (a map pin, a store) goes through gen_wb, so a
// minor under a complete set is sound (test/proof/rocq/gc.v barrier_sound).
// the one escape is overflow (rem_miss): a dropped entry forces the next collection
// major, which traces from roots and needs no rem set.
// young?: the address is the generation (no age bits) -- in [end, hp).
bool ai_young(struct ai *g, word p) {
 return lamp(p) && ptr(p) >= (word*) g->end && ptr(p) < g->hp; }
static bool gen_remembered(struct ai *g, word obj) {
 for (uintptr_t i = 0; i < g->rem_n; i++) if (g->rem[i] == obj) return true;
 return false; }
static void gen_remember(struct ai *g, word obj) {
 if (g->rem_n && g->rem[g->rem_n - 1] == obj) return;          // hot path: same map as last pin
 if (gen_remembered(g, obj)) return;                           // deduped: the set stays small (book + a few)
 if (g->rem_n < AiRemCap) g->rem[g->rem_n++] = obj;            // full: the miss forces a major (roots-only trace, no rem set), so a dropped entry can't orphan a young edge
 else g->rem_miss++;
 if (g->rem_n > g->rem_hi) g->rem_hi = g->rem_n; }
// an old `src` gains a young `p` -> remember src. maps and reader spines are the
// only in-place mutations, so this is the whole hot-path barrier.
void gen_wb(struct ai *g, word src, word p) {
 if (lamp(src) && ai_young(g, p) && !ai_young(g, src)) gen_remember(g, src); }
static ai_inline bool ai_major_cell(struct ai *g, word *c) {       // a tenured cell: inside the major pool
 return (ai_word*) c >= g->major_base && (ai_word*) c < g->major_hp; }
// the cell barrier (c0's emit, ev's poke): remember the smallest scannable unit around a
// young-into-tenured store. the cell sits in a tagged span -- a thread, a scope -- and
// never in a chain's field, which has no terminator for the remembered walk to stop at.
// there is no door for a cons: nothing patches one, so nothing needs to say it does.
// masks g.
void gen_wb_cell(struct ai *g, void *cl, word v) {
 g = ai_core_of(g);
 if (ai_young(g, v) && ai_major_cell(g, cl)) gen_remember(g, (word) cl); }
// gen_scan_inplace: a tenured object pointing into the young set stays put, but its
// young fields must promote -- gcp each outgoing pointer in place. evac_* without
// the relocation; a thread's terminator sits in the major to-space.
static void gen_scan_inplace(struct ai *g, struct ai_gcx *X, word obj) {
 union u *p = cell(obj);
 if (datp(obj)) switch (typ(obj)) {
  case DChain: { struct ai_chain *w = two(obj);
                 w->a = gcp(g, X, w->a), w->b = gcp(g, X, w->b); break; }
  case DTray:   { struct ai_tray *v = tray(p); if (v->type == ai_O) { word *e = (word*) tray_data(v);
                 for (uintptr_t i = 0, ne = tray_nelem(v); i < ne; i++) e[i] = gcp(g, X, e[i]); } break; }
  case DNom:   { nom(p)->name = gcp(g, X, nom(p)->name); break; }
  default: break;                                  // DMint/DString/DBig/DGem/DSun/DTwin: pointer-free leaves
 } else { for (union u *q = p; !tagl(g, X, q->x); q++) q->x = gcp(g, X, q->x); } }   // a thread: every word to the tag terminator (tagl: head in any live pool)
          // including word0 -- a normal thread's ap is out-of-pool (gcp no-op) but a task-ring node's
          // word0 is its `next` pointer, the very old->young edge the rem set exists to chase.

// relocate finalizer nodes out of the dead minor into the major. a minor never
// runs a finalizer; that waits for a major's compact.
static void gen_fz_relocate(struct ai *g) {
 struct ai_fz **link = &g->fz;
 for (struct ai_fz *fz = *link; fz; ) {
  struct ai_fz *next = fz->next;
  if ((word*) fz >= (word*) g->end && (word*) fz < g->hp) {   // node was in the minor -> relocate
   struct ai_fz *nn = bump(g, Width(struct ai_fz));           // gc_gen set -> major
   nn->p = fz->p, nn->fn = fz->fn, nn->next = next;
   *link = nn, link = &nn->next;
  } else link = &fz->next;
  fz = next; } }

// the weak-table sweep + finalizer pass of a major's compact: symbols_rebuild /
// run_finalizers, but bumping into the major to-space and testing survival against X's
static word major_symbols_rebuild(struct ai *g, struct ai_gcx *X, word om) {
 if (!om) return 0;
 uintptr_t cap = map_cap(om), mask = cap - 1, n = 0;
 union u *b = map_fill_back(bump(g, 4 + 2 * cap), cap), *hd = bump(g, 3);
 hd[0].ap = lvm_map_lookup, hd[1].x = (word) b, tagthread(hd, 2);
 word *os = map_slots(om), *ns = &b[3].x;
 word const *lo = X->to_lo, *hi = X->to_hi;
 for (uintptr_t j = 0; j < cap; j++) {
  word k = os[2 * j];
  if (k == map_gap) continue;
  word e = os[2 * j + 1];
  word fwd = cell(e)->x;                        // the atom's first word: its forward, if it survived
  if (!(lamp(fwd) && lo <= ptr(fwd) && ptr(fwd) < hi)) continue;
  word nk = nom(fwd)->name;
  uintptr_t i = hash(g, nk) & mask;
  while (ns[2 * i] != map_gap) i = (i + 1) & mask;
  ns[2 * i] = nk, ns[2 * i + 1] = fwd, n++; }
 b[1].x = putcharm(n);
 return (word) hd; }
static void major_run_finalizers(struct ai *g, struct ai_gcx *X) {
 struct ai_fz *new_fz = NULL;
 for (struct ai_fz *fz = g->fz; fz; fz = fz->next) {
  word fwd = fz->p->x;
  if (lamp(fwd) && X->to_lo <= ptr(fwd) && ptr(fwd) < X->to_hi) {
   struct ai_fz *nn = bump(g, Width(struct ai_fz));
   nn->p = cell(fwd), nn->fn = fz->fn, nn->next = new_fz, new_fz = nn;
  } else fz->fn(fz->p); }
 g->fz = new_fz; }

// AiGcStress's two numbers: an even poison, so a stale read faults at an address
// a backtrace can name; and how often a forced collection is a major (gen_please).
#define ai_gc_poison ((word) 0xd0d0d0d0d0d0d0d0ULL)
#define ai_gc_stress_major 32

// the minor: evacuate [end, hp) into the major active half, reset hp = end. the
// cheney scan starts at the append point, walking only fresh survivors; X.fwd
// tells a forward made this collection from a pointer to a pre-existing major object.
static void gen_minor(struct ai *g) {
 struct ai_gcx X = { .p0 = (word const*) g->end, .t0 = g->hp,    // minor from-range
                     .to_lo = g->major_base, .to_hi = g->major_base + g->major_len,
                     .fwd = g->major_hp, .cp = g->major_hp };
 g->gc_gen = true;
 g->ip = cell(gcp(g, &X, word(g->ip)));
 g->tasks = cell(gcp(g, &X, word(g->tasks)));
 if (g->parked) g->parked = cell(gcp(g, &X, word(g->parked)));   // the parked ring is its own root
 for (word i = 0; i < g->end - &g->v0; i++) (&g->v0)[i] = gcp(g, &X, (&g->v0)[i]);   // core vars
 for (word *s = g->sp; s < topof(g); s++) *s = gcp(g, &X, *s);                       // stack
 for (struct ai_r *r = g->root; r; r = r->n) *r->x = gcp(g, &X, *r->x);              // C roots
 // the weak intern map is its own field, not a root: promote its structure by hand
 // (entries stay weak -- a major drops dead atoms). young header: gcp it; tenured:
 // scan its possibly-young backing in place.
 if (g->symbols) {
  if (ai_young(g, g->symbols)) g->symbols = gcp(g, &X, g->symbols);
  else gen_scan_inplace(g, &X, g->symbols), gen_scan_inplace(g, &X, map_back(g->symbols));
 }
 for (uintptr_t i = 0; i < g->rem_n; i++) gen_scan_inplace(g, &X, g->rem[i]);        // major->young edges
 for (struct ai_fz *fz = g->fz; fz; fz = fz->next) fz->p = cell(gcp(g, &X, word(fz->p)));
 while (X.cp < g->major_hp) (datp(X.cp) ? evac_data : evac_thread)(g, &X);
#ifdef AiGcCheck
 // the fixpoint is a fixpoint (gc.v drain_second_pass_copies_nothing): re-drive the
 // whole scan; every gcp must be an identity. if major_hp moves, the first pass lost
 // a reachable object -- trap at the collection that lost it. (make test_gcheck)
 { word *hp1 = g->major_hp;
  X.cp = X.fwd;
  g->ip = cell(gcp(g, &X, word(g->ip)));
  g->tasks = cell(gcp(g, &X, word(g->tasks)));
  if (g->parked) g->parked = cell(gcp(g, &X, word(g->parked)));
  for (word i = 0; i < g->end - &g->v0; i++) (&g->v0)[i] = gcp(g, &X, (&g->v0)[i]);
  for (word *s = g->sp; s < topof(g); s++) *s = gcp(g, &X, *s);
  for (struct ai_r *r = g->root; r; r = r->n) *r->x = gcp(g, &X, *r->x);
  if (g->symbols) {
   if (ai_young(g, g->symbols)) g->symbols = gcp(g, &X, g->symbols);
   else gen_scan_inplace(g, &X, g->symbols), gen_scan_inplace(g, &X, map_back(g->symbols));
  }
  for (uintptr_t i = 0; i < g->rem_n; i++) gen_scan_inplace(g, &X, g->rem[i]);
  for (struct ai_fz *fz = g->fz; fz; fz = fz->next) fz->p = cell(gcp(g, &X, word(fz->p)));
  while (X.cp < g->major_hp) (datp(X.cp) ? evac_data : evac_thread)(g, &X);
  if (g->major_hp != hp1) __builtin_trap(); }
#endif
 if (g->fz) gen_fz_relocate(g);
 g->hp = g->end;                                              // minor emptied
#ifdef AiGcStress
 // poison the vacated nursery, or the stress build is half a detector: a stale
 // local otherwise reads a forwarding pointer that still looks live. last thing
 // here -- gen_fz_relocate is the from-space's last reader.
 for (word *p = (word*) X.p0; p < (word*) X.t0; p++) *p = ai_gc_poison;
#endif
 g->gc_gen = false; }

// the major: one cheney pass from the real roots over both from-spaces into the
// spare half -- reachability, never a linear sweep, which is why a rem-set overflow
// forces one. then rebuild the intern map, run finalizers, flip, reset the minor.
// req0 is the allocation that could not be served; *tight answers whether the pool got the
// size it asked for, which only gen_please can act on.
struct ai *gen_major(struct ai *g, uintptr_t req0, bool *tight) {
 struct ai_gcx X = { .p0 = g->major_base, .t0 = g->major_hp };   // from-range 1: major active
 // size the to-space for the worst case: all of major-active and all of the minor survive
 uintptr_t used = (uintptr_t)(g->major_hp - g->major_base), young = (uintptr_t)(g->hp - (word*) g->end);
 uintptr_t need = used + young;
 // grow/shrink by a whole step (= ai_major0): one step at a time prevents thrash, and
 // snapping down reclaims floated dead promotions. headroom is 25% or a whole nursery
 // plus the pending request, whichever is larger -- the second is gen_please's forcing
 // test verbatim, and a pool sized under it leaves that test true after the major it
 // just forced, so every later collection is a major too.
 uintptr_t slack = (uintptr_t) g->len + req0 + 16, head = need >> 2;
 uintptr_t step = g->major0, want = need + (head > slack ? head : slack) + 16;
 uintptr_t to_len = ((want + step - 1) / step) * step;
 if (to_len < step) to_len = step;
 uintptr_t free_len = to_len;                                   // the size asked for, before any clamp
 uintptr_t need_step = ((need + step - 1) / step) * step;       // the tight size: smallest step-multiple holding `need`
 if (need_step < step) need_step = step;
 // budget cap: keep the major pair within its share, but never below need_step (the
 // to-space must hold the worst-case promotion); too small falls through to the oom path
 if (g->budget) {
  uintptr_t cap = g->budget > 2 * (uintptr_t) g->len ? (g->budget - 2 * (uintptr_t) g->len) / 2 : 0;
  if (to_len > cap) to_len = cap > need_step ? (cap / step) * step : need_step; }
 word *spare = (g->major_base == g->major_pool) ? g->major_pool + g->major_len : g->major_pool;  // the same-size other half
 word *to, *resized = 0;
 if (to_len != g->major_len) {                                 // a different-size pair: alloc it, free the old
  resized = g->alloc(g, NULL, 2 * to_len * sizeof(word));
  if (!resized && to_len > need_step)                          // the headroom alloc failed: retry at the tight size
   to_len = need_step, resized = (need_step == g->major_len) ? 0 : g->alloc(g, NULL, 2 * need_step * sizeof(word));
  if (resized) to = resized;
  else if (need <= g->major_len) to_len = g->major_len, to = spare;   // alloc failed, but the existing spare half holds the live set
  else return g->gc_gen = false, encode(g, ai_status_scare);         // true oom: compacting would overflow the spare -> clean scare, no corruption
 } else to = spare;
 if (tight) *tight = to_len < free_len;   // denied: the budget cap, or the bigger alloc failed
 g->gc_gen = true;
 g->major_hp = to, X.cp = to;
 X.to_lo = to, X.to_hi = to + to_len, X.fwd = to;           // fresh to-space: every copy is a forward
 X.f2lo = (word const*) g->end, X.f2hi = g->hp;             // from-range 2: the minor (promote young in the same pass)
 g->ip = cell(gcp(g, &X, word(g->ip)));
 g->tasks = cell(gcp(g, &X, word(g->tasks)));
 if (g->parked) g->parked = cell(gcp(g, &X, word(g->parked)));   // the parked ring is its own root
 for (word i = 0; i < g->end - &g->v0; i++) (&g->v0)[i] = gcp(g, &X, (&g->v0)[i]);
 for (word *s = g->sp; s < topof(g); s++) *s = gcp(g, &X, *s);
 for (struct ai_r *r = g->root; r; r = r->n) *r->x = gcp(g, &X, *r->x);
 word om = g->symbols; g->symbols = 0;                       // weak: rebuilt after the fixpoint
 while (X.cp < g->major_hp) (datp(X.cp) ? evac_data : evac_thread)(g, &X);
 g->symbols = major_symbols_rebuild(g, &X, om);
 major_run_finalizers(g, &X);
 if (resized) g->alloc(g, g->major_pool, 0), g->major_pool = resized, g->major_len = to_len;
 g->major_base = to;                                           // flip: active = the to-space
 g->hp = g->end;                                             // the minor's young was promoted: reset it
#ifdef AiGcStress
 // poison the promoted young, like the minor. the old major half does not: poisoning it
 // costs ten minutes on a 24-second lane, and a cheney copy already left a forwarding
 // pointer in word0 (the ap), which faults on dispatch.
 for (word *p = (word*) g->end; p < (word*) g->end + young; p++) *p = ai_gc_poison;
#endif
 // the rem set dies with the half it named: a major promotes every survivor, so there is
 // no old->young edge left to remember and every address in it points into a half about
 // to be reused. cleared here rather than in gen_please alone, because a major can be
 // called directly -- the image dump compacts before it serializes.
 g->rem_n = 0, g->rem_miss = 0;
 return g->gc_gen = false, g; }

// resize the minor pool, decoupled from the major. called right after a collection,
// so the minor is empty: only the core + stack move; the major + intern map ride
// through untouched (() is ZeroPoint, so nothing points at the moving core).
struct ai *gen_grow(struct ai *g, uintptr_t len1) {
 struct ai *h = g->alloc(g, NULL, len1 * 2 * sizeof(word));
 if (!h) return encode(g, ai_status_scare);
 memcpy(h, g, sizeof(struct ai));
 h->len = len1;
 word const *sp0 = g->sp;
 struct ai_gcx X = { .p0 = ptr(g), .t0 = ptr(g) + g->len,      // the whole old pool is the from-space
                     .to_lo = ptr(h), .to_hi = ptr(h) + len1, .fwd = ptr(h), .cp = h->end };
 word sh = X.t0 - sp0;
 h->sp = ptr(h) + len1 - sh;
 h->hp = h->end;                             // core moves to h; no (word)g root to forward (() is the const ZeroPoint)
 h->gc_gen = false;
 h->ip = cell(gcp(h, &X, word(h->ip)));
 h->tasks = cell(gcp(h, &X, word(h->tasks)));
 if (h->parked) h->parked = cell(gcp(h, &X, word(h->parked)));
 // h->symbols + the major were memcpy'd and live outside [p0,t0): untouched, not rebuilt
 for (word i = 0; i < h->end - &h->v0; i++) (&h->v0)[i] = gcp(h, &X, (&h->v0)[i]);   // core vars
 for (word n = 0; n < sh; n++) h->sp[n] = gcp(h, &X, sp0[n]);                        // stack
 for (struct ai_r *s = h->root; s; s = s->n) *s->x = gcp(h, &X, *s->x);              // C roots
 while (X.cp < h->hp) (datp(X.cp) ? evac_data : evac_thread)(h, &X);                 // heap empty -> ~nothing
 h->n_resize += 1;
 if (h->len > h->max_len) h->max_len = h->len;
 g->alloc(g, g, 0);                          // free the old main pool
 return h; }

// the GC entry: a minor unless the rem set overflowed or the major lacks headroom --
// then a major. afterwards size the minor by appel's rule against the budget.
struct ai *gen_please(struct ai *g, uintptr_t req0) {
 uintptr_t seen_young = (uintptr_t)(g->hp - g->end);
 uintptr_t major_free = (uintptr_t)((g->major_base + g->major_len) - g->major_hp);
 g->since_major += seen_young;                                  // young allocated (∝ scanned) since the last major
 // a major: forced by rem-set overflow, by the major lacking room for a worst-case
 // promotion, or by the amortization rule -- live set + 4 minor-pools allocated since
 // the last one -- so floating dead tenured objects sweep and the pool can shrink.
 bool major = g->rem_miss
   || major_free < (uintptr_t) g->len + req0 + 16
   || g->since_major > g->major_live0 + 4 * (uintptr_t) g->len;
#ifdef AiGcStress
 // a minor is not enough: stress-collecting tenures everything almost at once,
 // and a minor never moves the tenured -- the detector answered green on its own
 // control. every-collection-major cost a 458 s boot, so a major rides every Nth
 // collection instead: deterministic, off g->n_gc. coverage is the stated trade.
 major = major || g->n_gc % ai_gc_stress_major == 0;
#endif
 word *before = g->major_hp;
 bool tight = false;
 if (major) {
  if (!ai_ok(g = gen_major(g, req0, &tight))) return g;     // a true oom mid-major (compacting would overflow the spare): propagate the scare
  g->n_gc += 1;
  g->since_major = 0, g->major_live0 = (uintptr_t)(g->major_hp - g->major_base);   // reset the amortization window
#ifdef AiGcCheck
  // the forcing test above must read false after the major it forced, unless the sizer was
  // denied the room -- there thrash beats dying. still true on a pool that got what it asked
  // for is the two disagreeing over one number, and the collector has latched into permanent
  // majors: correct, quadratic, and no gate can see it. (make test_gcheck)
  if (!tight && (uintptr_t)((g->major_base + g->major_len) - g->major_hp) < (uintptr_t) g->len + req0 + 16)
   __builtin_trap();
#endif
 } else gen_minor(g), g->n_gc += 1, g->n_minor += 1;
 uintptr_t copied = major ? (uintptr_t)(g->major_hp - g->major_base) : (uintptr_t)(g->major_hp - before);
 g->n_seen += seen_young;
 g->n_evac += copied;
 if (major) { if (copied > g->major_hi) g->major_hi = copied; }
 else if (copied > g->minor_hi) g->minor_hi = copied;
 g->rem_n = 0, g->rem_miss = 0;
 { uintptr_t e = (uintptr_t)(g->major_hp - g->major_base); if (e > g->max_heap) g->max_heap = e; }
 // minor resize, deterministic (words copied / words allocated -- no wall clock, so
 // the schedule is reproducible): keep the copy overhead inside a band, accumulated
 // over a sliding window; ai_budget caps the footprint by appel's rule.
 uintptr_t const ratio = g->ratio;              // target band: grow above 1/ratio overhead, shrink below 1/(4*ratio)
#ifdef AiGcStress
 // the band is meaningless on a forced schedule (`allocated` ~0 doubles the nursery
 // every collection), but the hard floor stays: it guarantees the pending allocation
 // fits. it must also come back down -- a nursery parked at its high-water stands above
 // the major's spare, and `major_free < g->len` then forces a major every collection,
 // generational in name only. shrink on 4x hysteresis, so a resize is not per-pass.
 { uintptr_t used0 = g->len - avail(g), req = req0 + used0 + (used0 >> 2);
   uintptr_t want = req < g->minor0 ? g->minor0 : req;
   if (req > (uintptr_t) g->len) return gen_grow(g, req);        // the floor still wins
   if ((uintptr_t) g->len > 4 * want) return gen_grow(g, want);
   return g; }
#endif
 g->win_alloc += seen_young, g->win_copied += copied;
 uintptr_t used = g->len - avail(g), req = req0 + used + (used >> 2), len1 = g->len, arena = len1;
 // resize stickiness: act only on two consecutive same-way windows (lean tracks the
 // streak; in-band ends it). a resize is the costliest single act -- fresh pool, full
 // copy, every page refaulted -- so a spike self-corrects and only a real ramp confirms
 // next collection. first-verdict-with-reset taxes ramps ~4%, and excluding majors from
 // the window costs +70% wall.
 if (g->win_copied * ratio > g->win_alloc) {                   // overhead > 1/ratio: nursery too small
  if ((g->lean = g->lean > 0 ? g->lean + 1 : 1) >= 2) {        // confirmed: grow
   uintptr_t wa = g->win_alloc | 1;                            // grow until the projected overhead lands in band (| 1: guarantee progress)
   while (g->win_copied * ratio > wa) arena <<= 1, wa <<= 1;    // (doubling the pool ~doubles alloc-between-GCs)
   g->lean = 0, g->win_alloc = g->win_copied = 0; }
 } else if (g->win_copied * (ratio * 4) < g->win_alloc) {       // overhead < 1/(4*ratio): oversized
  if ((g->lean = g->lean < 0 ? g->lean - 1 : -1) <= -2)
   arena = len1 >> 1, g->lean = 0, g->win_alloc = g->win_copied = 0;   // shrink one step (gentle -- multi-step collapses on a lucky GC)
 } else if (g->win_alloc > 8 * len1) g->win_alloc = g->win_copied = 0, g->lean = 0;   // in band: cap the window; the streak dies
 if (g->budget) {
  // appel cap, reserving room for the major that must hold the worst-case promotion
  // (live + this whole nursery): the nursery gets ~(budget - 2*live)/4
  uintptr_t lv = 2 * g->major_live0, room = g->budget > lv ? (g->budget - lv) / 4 : 0;
  if (arena > room) arena = room; }
 // the pool was denied room for a worst-case promotion of this nursery, so the nursery is
 // what gives. the floors below still win: under real pressure thrash beats failing the
 // request that asked for the collection.
 if (tight) {
  uintptr_t fr = (uintptr_t)((g->major_base + g->major_len) - g->major_hp);
  uintptr_t fit = fr > req0 + 16 ? fr - req0 - 16 : 0;
  if (arena > fit) arena = fit; }
 if (arena < g->minor0) arena = g->minor0;                     // floor
 if (arena < req) arena = req;                                 // hard floor: hold the pending allocation
 return arena == len1 ? g : gen_grow(g, arena); }

ai_noinline struct ai *ai_please(struct ai *g, uintptr_t req0) {
 return gen_please(g, req0); }   // generational only: a minor (or major) into the major pool that ai_ini_0 guarantees

static ai_inline word copy_chain(struct ai *g, struct ai_chain *src) {
 struct ai_chain *dst = bump(g, Width(struct ai_chain));
 ini_chain(dst, src->a, src->b);
 src->ap = (lvm_t*) dst;
 return word(dst); }

static ai_inline word copy_tray(struct ai *g, struct ai_tray *src) {
 uintptr_t bytes = ai_tray_bytes(src);
 struct ai_tray *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

static ai_inline word copy_str(struct ai *g, struct ai_str *src) {
 uintptr_t w = str_width(src->len);
 struct ai_str *dst = bump(g, w);
 src->ap = memcpy(dst, src, w * sizeof(word));   // whole words: the zeroed tail rides
 return word(dst); }

// bignums and the lean boxes are flat: one memcpy, like strings
static ai_inline word copy_big(struct ai *g, struct ai_big *src) {
 uintptr_t bytes = ai_big_bytes(src);
 struct ai_big *dst = bump(g, b2w(bytes));
 src->ap = memcpy(dst, src, bytes);
 return word(dst); }

static ai_inline word copy_gem(struct ai *g, struct ai_gem *src) {
 struct ai_gem *dst = bump(g, gem_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_gem));
 return word(dst); }

static ai_inline word copy_sun(struct ai *g, struct ai_sun *src) {
 struct ai_sun *dst = bump(g, sun_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_sun));
 return word(dst); }

static ai_inline word copy_twin(struct ai *g, struct ai_twin *src) {
 struct ai_twin *dst = bump(g, twin_req);
 src->ap = memcpy(dst, src, sizeof(struct ai_twin));
 return word(dst); }

// atoms copy like any object; interning maintenance is the post-fixpoint table sweep's
static ai_inline word copy_sym(struct ai *g, struct ai_mint *src) {
 struct ai_mint *dst = bump(g, Width(struct ai_mint));
 ini_missing(dst, src->code);                     // just the serial rides
 return word(src->ap = (lvm_t*) dst); }

static ai_inline word copy_nom(struct ai *g, struct ai_nom *src) {
 struct ai_nom *dst = bump(g, Width(struct ai_nom));
 // shallow: evac_nom forwards the name later (Cheney)
 ini_nom(dst, src->name, src->code, src->dig);    // name copied raw, serial + dig ride
 return word(src->ap = (lvm_t*) dst); }

static ai_inline word copy_data(struct ai *g, union u *src) {
 switch (typ(src)) {
  case DChain: return copy_chain(g, two(src));
  case DTray: return copy_tray(g, tray(src));
  case DMint: return copy_sym(g, sym(src));
  case DNom: return copy_nom(g, nom(src));
  case DString: return copy_str(g, str(src));
  case DBig: return copy_big(g, big(src));
  case DGem: return copy_gem(g, (struct ai_gem*) src);
  case DSun: return copy_sun(g, (struct ai_sun*) src);
  case DTwin: return copy_twin(g, (struct ai_twin*) src); }
 __builtin_trap(); }

static ai_inline struct ai_tag *ttag2(struct ai *g, struct ai_gcx *X, union u *k) {
 while (!tagl(g, X, k->x)) k++;                              // tagl: terminator head in any live pool
 return (struct ai_tag*) k; }

static ai_inline word copy_thread(struct ai *g, struct ai_gcx *X, union u *src) {
 // it's a thread, find the end to find the head
 struct ai_tag *t = ttag2(g, X, src);
 union u *ini = tag_head(t), *d = bump(g, t->end - ini), *dst = d;
 // copy each content word to dest and leave a forwarding pointer behind,
 // stopping at the terminator; then rewrite it as the new tagged head
 for (union u *s = ini; !tagl(g, X, s->x); s->x = (word) d, d++, s++) d->x = s->x;
 return (word) (tagthread(dst, d - dst) + (src - ini)); }

ai_noinline intptr_t gcp(struct ai *g, struct ai_gcx *X, word x) {
 // a number stays; else x must sit in a from-space range this pass traces (a major traces two)
 if (charmp(x)) return x;
 if (!(ptr(x) >= X->p0 && ptr(x) < X->t0)
  && !(X->f2lo && ptr(x) >= X->f2lo && ptr(x) < X->f2hi)) return x;
 union u *src = cell(x);
 x = src->x; // get its contents
 // if it contains a pointer to the new space then return the pointer (already forwarded)
 word const *flo = X->fwd, *fhi = X->to_hi;   // forwarding window of this collection (major/spare/new pool)
 return lamp(x) && flo <= ptr(x) && ptr(x) < fhi ? x :
        in_data((void*) x) ? copy_data(g, src) :
                             copy_thread(g, X, src); }

// ============================================================================
// sys
// ============================================================================
op11(lvm_clock, putcharm(ai_clock() - (charmp(Sp[0]) ? getcharm(Sp[0]) : 0)))

// the fine clock: monotonic ns for differences ((nclock t) is ns minus t); clock
// stays at ms, the scheduler's scale (ns wraps 32 bits every 4.3s). weak default
// degrades to ms*1e6; hosts override with a real ns source.
__attribute__((weak)) intptr_t ai_nclock(void) {
 return (intptr_t) (ai_clock() * 1000000u); }
op11(lvm_nclock, putcharm(ai_nclock() - (charmp(Sp[0]) ? getcharm(Sp[0]) : 0)))

// (please x): a collection on demand -- () a minor, a positive charm a major;
// answers the new n_gc (the real-time lever). a forced collection observes and
// never steers: the resize window is zeroed for the call and put back, so a probe
// forcing minors can't talk the nursery into doubling (the pause gauge's first
// draft ran the pool to oom@8GB through exactly that feedback).
lvm(lvm_please) {
 word n = Sp[0];
 Pack(g);
 if (charmp(n) && getcharm(n) > 0)
  g->since_major = g->major_live0 + 4 * (uintptr_t) g->len + 1;
 uintptr_t wa = g->win_alloc, wc = g->win_copied;
 int8_t ln = g->lean;
 g->win_alloc = g->win_copied = 0, g->lean = 0;
 if (!ai_ok(g = ai_please(g, 0))) ai_musttail return Ap(_lvm_ghelp, g);
 g->win_alloc = wa, g->win_copied = wc, g->lean = ln;
 Unpack(g);
 Sp[0] = putcharm((intptr_t) g->n_gc);
 Ip += 1; ai_musttail return Continue(); }

// (gauge 0) -> a rank-1 Z array of VM stats (full machine words, not 62-bit fixnums):
//   [0] len       pool size (words)
//   [1] heap      words used from base (core + live heap)
//   [2] stack     stack height (words)
//   [3] n_gc      collections so far
//   [4] max_len   peak pool size (words)
//   [5] max_heap  peak live heap after a collection (words)
//   [6] n_seen    Σ heap occupancy entering each collection (scanned = live + dead, words)
//   [7] n_evac    Σ heap survivors copied out each collection (live, words)
//   [8] old       the tenured set: words live in the major pool
//   [9] rem_miss  rem-set entries dropped on overflow since the last collection (a miss forces a major; ~always 0)
//  [10] rem_hi    peak remembered-set size (distinct old objects with a young field)
//  [11] n_minor   minor collections so far (majors = n_gc - n_minor)
//  [12] major_cap the major pool's reserved footprint: 2*major_len words (both halves), 0 if non-gen
//  [13] n_resize  pool reallocations so far (the pool-cliff tell)
//  [14] minor_hi  peak words one minor copied -- the pause gauge (a copying
//  [15] major_hi  peak words one major copied    collection's pause is its copy volume)
// derive: mortality = (n_seen - n_evac)/n_seen ; copy-amp = n_evac/max_heap
lvm(lvm_gauge) {
 enum { N = 16 };
 uintptr_t const bytes = sizeof(struct ai_tray) + 1 * sizeof(word) + N * ai_T[ai_Z];
 Have(b2w(bytes));
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ai_Z, 1);
 v->shape[0] = N;
 tray_put_int(v, 0, (intptr_t) g->len);
 tray_put_int(v, 1, (intptr_t) (Hp - ptr(g)));
 tray_put_int(v, 2, (intptr_t) (ptr(g) + g->len - Sp));
 tray_put_int(v, 3, (intptr_t) g->n_gc);
 tray_put_int(v, 4, (intptr_t) g->max_len);
 tray_put_int(v, 5, (intptr_t) g->max_heap);
 tray_put_int(v, 6, (intptr_t) g->n_seen);
 tray_put_int(v, 7, (intptr_t) g->n_evac);
 tray_put_int(v, 9, (intptr_t) g->rem_miss);
 tray_put_int(v, 10, (intptr_t) g->rem_hi);
 tray_put_int(v, 11, (intptr_t) g->n_minor);
 tray_put_int(v, 8, (intptr_t) (g->major_hp - g->major_base));            // words live in the major pool
 tray_put_int(v, 12, (intptr_t) (g->major_pool ? 2 * g->major_len : 0));  // major pool capacity (both halves), words
 tray_put_int(v, 13, (intptr_t) g->n_resize);
 tray_put_int(v, 14, (intptr_t) g->minor_hi);
 tray_put_int(v, 15, (intptr_t) g->major_hi);
 ai_musttail return Answer(word(v)); }

// (tune v) -> the four live GC knobs as a rank-1 Z array, in words:
//   [0] budget  total footprint cap (2*minor + 2*major); 0 = unbounded (appel's rule)
//   [1] minor0  the nursery floor every resize clamps up to
//   [2] major0  the major pool's grow/shrink step (never 0: it divides)
//   [3] ratio   copy-overhead setpoint -- hold copied/allocated inside [1/(4*ratio), 1/ratio]
// (tune ()) reads; a rank-1 4-array writes and answers what it replaced, so a probe
// can put the knobs back. seeded at ai_ini from ai_minor0/ai_major0/ai_gc_ratio.
// a knob lands at the next collection -- tightening budget frees nothing until then,
// so pair it with (please 1). a wrong shape is a silent no-op answering the current
// knobs (pin's misuse convention). these are untraced scalars ahead of v0, so a bake
// does not carry them: a woken image tunes again (host's LOVE_BUDGET_MB does exactly that).
lvm(lvm_tune) {
 enum { N = 4 };
 uintptr_t const bytes = sizeof(struct ai_tray) + 1 * sizeof(word) + N * ai_T[ai_Z];
 Have(b2w(bytes));
 word x = Sp[0];                             // read post-Have: a collection forwards the operand
 struct ai_tray *v = (struct ai_tray*) Hp;
 Hp += b2w(bytes);
 ini_tray(v, ai_Z, 1);
 v->shape[0] = N;
 tray_put_int(v, 0, (intptr_t) g->budget);
 tray_put_int(v, 1, (intptr_t) g->minor0);
 tray_put_int(v, 2, (intptr_t) g->major0);
 tray_put_int(v, 3, (intptr_t) g->ratio);
 if (galaxyp(x) && tray(x)->rank == 1 && tray(x)->shape[0] == N) {
  struct ai_tray *w = tray(x);
  intptr_t b = tray_get_int(w, 0), mi = tray_get_int(w, 1),
           ma = tray_get_int(w, 2), ra = tray_get_int(w, 3);
  g->budget = b > 0 ? (uintptr_t) b : 0;     // <= 0 is the unbounded spelling, not a refusal
  if (mi > 0) g->minor0 = (uintptr_t) mi;    // a 0 floor would let the nursery vanish
  if (ma > 0) g->major0 = (uintptr_t) ma;    // the step divides
  if (ra > 0) g->ratio = (uintptr_t) ra; }   // 0 would never grow and always shrink
 ai_musttail return Answer(word(v)); }

// (apof x): x's kind pointer (cell[0]) as a fixnum, 0 for a fixnum/immediate. the string-lane glaze
// reads the kind of a reference string at codegen time and emits a `cmp [s], kind; jne deopt` type guard.
lvm(lvm_apof) {
 word x = Sp[0];
 Sp[0] = putcharm(lamp(x) ? (uintptr_t) cell(x)->ap : 0);
 Ip += 1;
 ai_musttail return Continue(); }

// default fd-keyed waits, conservative (all fds always-ready; multi-source wait
// collapses to sleep) so non-multitasking frontends link without impls
__attribute__((weak)) bool ai_ready(int fd, int events) { (void) fd, (void) events; return true; }
__attribute__((weak)) void ai_wait_fds(struct ai_wait_fd *fds, int n, uintptr_t ticks) {
  (void) fds; (void) n; ai_sleep(ticks); }
// the default authoritative readiness sweep: ask one at a time but fill every
// slot, so "none ready" never reads as "nobody answered"; hosts replace the loop
// with one poll(2)
__attribute__((weak)) void ai_ready_fds(struct ai_wait_fd *fds, int n) {
  for (int i = 0; i < n; i++)
    fds[i].revents = ai_ready(fds[i].fd, fds[i].events) ? fds[i].events : 0; }

__attribute__((weak)) void ai_fd_close(int fd) { (void) fd; }   // host overrides with close(2)
// default sleep is busy wait
__attribute__((weak)) ai_noinline void ai_sleep(uintptr_t ticks) {
  for (ticks += ai_clock(); ai_clock() < ticks;); }

// (cue? p): would `see` answer without parking? the dual of the park law -- all
// three terms (pushback, buffered run, fd), or a port with bytes in hand reads
// "not ready". it asks will you answer, not is there data: a hung-up fd reads
// ready and the see answers -1. a non-port asks about stdin (the bare (cue? 0)).
lvm(lvm_key) {
 Sp[0] = io_route(g, iop(Sp[0]) ? Sp[0] : (word) &ai_stdin);   // the bare (cue? 0) asks about stdin, so it routes too
 struct ai_io *i = (struct ai_io*) Sp[0];
 Sp[0] = (getcharm(i->ungetc_buf) != EOF || bio_rpending(bio_of(g, i))
          || ai_ready((int) ai_io_fd(i), ai_wait_in)) ? putcharm(1) : zero;
 Ip += 1;
 ai_musttail return Continue(); }

// ============================================================================
// str
// ============================================================================
struct ai *str0(struct ai *g, uintptr_t len) {
 if (!len) { if (ai_ok(g = ai_have(g, 1))) *--g->sp = EmptyString; return g; } // never alloc empty
 uintptr_t req = str_width(len);
 if (ai_ok(g = ai_have(g, req + 1)))
  *--g->sp = word(ini_str(bump(g, req), len));
 return g; }

struct ai *ai_strof(struct ai *g, char const *cs) {
 uintptr_t len = strlen(cs);
 if (ai_ok(g = str0(g, len))) memcpy(txt(g->sp[0]), cs, len);
 return g; }

op11(lvm_strp, strp(Sp[0]) ? putcharm(1) : zero)
// a cask snips as the string of its bytes, the way `pour` already reads one: swig
// fills a buffer and the caller wants the prefix it filled, and (string b) first
// copies the whole buffer to take a corner of it -- 64K a read for a 12-byte file.
lvm(lvm_snip) {
 if (!strp(Sp[0]) && !caskp(Sp[0])) Sp[2] = zero;
 else {
  struct ai_str *s = bytes_of(Sp[0]), *t;
  intptr_t i = oddp(Sp[1]) ? getcharm(Sp[1]) : 0,
           j = oddp(Sp[2]) ? getcharm(Sp[2]) : 0;
  i = max(i, 0), i = min(i, (word) len(s));
  j = max(j, i), j = min(j, (word) len(s));
  // an empty range (i == j) answers a string, the closest form of nothing for this
  // kind, not the bare floor (fixnum 0) -- and the empty string, never a fresh one.
  // no 0-length string is ever allocated (str0 holds the same line), which is what
  // lets two empties be id?-equal wherever they were built.
  if (j == i) Sp[2] = EmptyString;
  else {
   size_t req = str_width(j - i);
   Have(req);
   s = bytes_of(Sp[0]);                          // re-read post-Have (GC may have moved it)
   t = str(Hp);
   Hp += req;
   ini_str(t, j - i);
   memcpy(txt(t), txt(s) + i, j - i);
   Sp[2] = (word) t; } }
 ai_musttail return Nextp(1, 2); }


// applying a cask behaves as 0 (yields 1); byte-identical to lvm_port_io, kept
// distinct by ai_noicf so caskp and iop never collide
lvm(lvm_cask) {
 Ip = cell(*++Sp); *Sp = ZeroPoint; ai_musttail return Continue(); }
// (cask n) — a zeroed n-byte mutable cask; (cask charlist) — one holding those
// bytes (the bulk way in). n<=0 -> EmptyString, so no empty cask object exists.
// two heap objects under one Have, so no GC sees a half-built cask.
lvm(lvm_casknew) {
 bool listp = chainp(Sp[0]);
 intptr_t n = charmp(Sp[0]) ? getcharm(Sp[0]) : listp ? (intptr_t) llen(Sp[0]) : 0;
 if (n <= 0) ai_musttail return Answer(EmptyString);   // no empty cask: it is ""
 uintptr_t sreq = str_width(n),
           breq = Width(struct ai_cask) + Width(struct ai_tag);
 Have(sreq + breq);
 struct ai_str *s = ini_str(str(Hp), n);
 Hp += sreq;
 if (listp) {                                                // the charlist lane, mirroring lvm_string's
  word y = Sp[0];                                            // re-read post-Have, like the cask lane there
  for (uintptr_t i = 0; i < (uintptr_t) n; y = B(y)) txt(s)[i++] = (char) getcharm(A(y)); }
 else memset(txt(s), 0, n);
 union u *k = (union u*) Hp;
 Hp += breq;
 cask(k)->ap = lvm_cask;
 cask(k)->str = s;
 tagthread(k, Width(struct ai_cask));
 ai_musttail return Answer(word(k)); }

// the W^X code arena: hosted, the malloc heap is NX, so `nat` copies emitted
// bytes into a W^X mapping -- mmap RW, write, mprotect R+X, never write again;
// the code address lives outside the GC pool and nat_unmap frees it when the
// native closure dies. on inle -- one hosted-compiled binary, so the question
// is asked at RUN TIME, a negative __ai_osv -- and on a freestanding seat,
// RAM is executable and a heap copy runs, with no finalizer owed.
#if __STDC_HOSTED__
// which kernel underneath: nolibc's os.c defines it (0 unprobed; 1..3 the
// hosted kernels; negative = we ARE the kernel). weak for seats with no
// nolibc aboard (love0 under a foreign libc, wasm), where zero reads as
// hosted -- which such a seat is.
__attribute__((weak)) long __ai_osv;
#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif
size_t code_maplen(size_t codelen) {   // round the arena up to a page; glibc's sysconf is a cached auxv load, not a syscall
 long q = sysconf(_SC_PAGESIZE); size_t ps = q > 0 ? (size_t) q : 4096, need = sizeof(struct ai_str) + codelen + 1;
 return (need + ps - 1) & ~(ps - 1); }
#endif

// ============================================================================
// sym
// ============================================================================
// (intern s) -> the interned symbol named by string s; identity on any other arg.
// the empty spelling names nothing: (intern "") is ().
lvm(lvm_intern) {
 if (strp(Sp[0])) {
  if (Sp[0] == EmptyString) ai_musttail return Answer(ZeroPoint);  // (intern "") -> () (zero-ontology: the empty spelling is the zero point)
  word y;
  Have(intern_reserve(g));
  Pack(g), y = intern_checked(g, str(g->sp[0])), Unpack(g);
  Sp[0] = y; }
 ai_musttail return Next(1); }

// (mint _) -> a fresh nameless point, identity its only property (the arg is
// ignored). `code` gets the mint serial: its hash and its order key, GC-stable.
// mints answer nomp, so they bind as gensyms.
lvm(lvm_mint) {
 Have(Width(struct ai_mint));
 struct ai_mint *y = (struct ai_mint*) Hp;
 Hp += Width(struct ai_mint);                   // mints are uniform: ap, code
 ini_missing(y, ++g->next_serial);
 return
  Sp[0] = word(y),
  Ip += 1,
  Continue(); }

// (nom n) -> a fresh, uninterned named point: a string names it, a symbol lends
// its spelling, anything else falls to a bare mint. two (nom 'x) are distinct --
// the gensym-with-a-name.
lvm(lvm_nomctor) {
 Have(Width(struct ai_nom));                    // >= Width(struct ai_mint), so the bare-mint fallback fits too
 word n = Sp[0];                                // re-read post-GC (the stack is rooted)
 struct ai_str *nm = strp(n) ? str(n) : nom_str(g, n);   // a string is the name; a sym lends its spelling
 if (!nm) ai_musttail return Ap(lvm_mint, g);               // no name -> a bare mint
 struct ai_nom *y = (struct ai_nom*) Hp;
 Hp += Width(struct ai_nom);
 ini_nom(y, word(nm), ++g->next_serial, nom_dig(word(nm)));
 ai_musttail return Answer(word(y)); }

struct ai *intern(struct ai*g) {
 if (!ai_ok(g)) return g;                        // intern_reserve reads g, and ai_have's guard is
                                                 // too late (it is an argument): a caller's scare
                                                 // was dereferenced rather than propagated
 if (ai_ok(g = ai_have(g, intern_reserve(g))))   // atom + (at the load factor) the doubled backing
  g->sp[0] = intern_checked(g, str(g->sp[0]));
 return g; }

// what a fresh intern may bump: the atom, plus (at the load factor) the doubled
// backing. callers reserve this before intern_checked, so the insert never allocates.
uintptr_t intern_reserve(struct ai *g) {
 word m = g->symbols;
 uintptr_t extra = m && (map_len(m) + 1) * 4 >= map_cap(m) * 3 ? 4 + 4 * map_cap(m) : 0;
 return Width(struct ai_nom) + extra; }   // a named symbol is one flat KNom (name + serial)

// probe the weak intern map by string content; a miss mints the canonical KNom
// and inserts it. one canonical nom per spelling. bump-only in here (see intern_reserve).
ai_noinline word intern_checked(struct ai *g, struct ai_str *b) {
 word m = g->symbols;
 bool found;
 uintptr_t i = map_probe(g, m, word(b), &found);
 if (found) return map_slots(m)[2 * i + 1];
 if ((map_len(m) + 1) * 4 >= map_cap(m) * 3) {           // at load: rehash into a doubled backing
  uintptr_t ncap = 2 * map_cap(m), nmask = ncap - 1;
  union u *nb = map_fill_back(bump(g, 4 + 2 * ncap), ncap);
  word *os = map_slots(m), *ns = &nb[3].x;
  uintptr_t ocap = map_cap(m), nlen = 0;
  for (uintptr_t j = 0; j < ocap; j++) {
   word k = os[2 * j];
   if (k == map_gap) continue;
   uintptr_t x = hash(g, k) & nmask;
   while (ns[2 * x] != map_gap) x = (x + 1) & nmask;
   ns[2 * x] = k, ns[2 * x + 1] = os[2 * j + 1], nlen++; }
  nb[1].x = putcharm(nlen);
  cell(m)[1].x = (word) nb;                              // swap backing; header identity stable
  i = map_probe(g, m, word(b), &found); }
 struct ai_nom *y = ini_nom(bump(g, Width(struct ai_nom)), word(b), ++g->next_serial, nom_dig(word(b)));  // the canonical KNom: name + serial + cached spelling hash
 word *slots = map_slots(m);
 slots[2 * i] = word(b), slots[2 * i + 1] = word(y);
 cell(map_back(m))[1].x = putcharm(map_len(m) + 1);
 return word(y); }

// (nom? x): a real point -- a non-() mint or a named nom; () is the one point
// that is not nom?
op11(lvm_nomp, (nomp(Sp[0]) && Sp[0] != ZeroPoint) ? putcharm(1) : zero)
// (name? x): a named point only (KNom) -- a nom with a spelling. name? => nom?; the gap
// nom? \ name? is the anonymous-but-real mints (gensyms).
op11(lvm_namep, namep(Sp[0]) ? putcharm(1) : zero)
// (mint? x): that gap, asked directly -- a bare point, the gensym `nom` hands back.
// mint? and name? partition nom?, and () is in neither. the only way to ask, since
// `string` answers text for every point alike and a mint's spelling is "".
op11(lvm_mintp, (mintp(Sp[0]) && Sp[0] != ZeroPoint) ? putcharm(1) : zero)
op11(lvm_packp, (packp(Sp[0]) || gemp(Sp[0]) || sunp(Sp[0]) || twinp(Sp[0])) ? putcharm(1) : zero)  // the pack family: arrays + the lean gem/sun/twin scalar boxes
op11(lvm_bigp, bigp(Sp[0]) ? putcharm(1) : zero)
op11(lvm_sunp, sunp(Sp[0]) ? putcharm(1) : zero)
op11(lvm_setp, trayp(Sp[0]) ? putcharm(1) : zero)
// (int x): truncate a float scalar to a fixnum; other numbers pass through. used by
// num-ap to get an integer composition count from a non-integer numeral operator.
// int: a gem truncates toward zero, saturating at the charm bounds like the other
// rungs (the bare cast wrapped above 2^62 -- UB read as 0); an exact-ratio coin
// truncates by long division; everything else passes through.
lvm(lvm_intf) {
 if (ai_ratio_exact(g, Sp[0])) { Pack(g); g = ai_ratio_rung(g, 0);
  if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
  ai_musttail return Resume(); }
 if (gemp(Sp[0])) { ai_flo_t v = gem_get(Sp[0]);
  Sp[0] = putcharm(v >= (ai_flo_t) maxcharm ? maxcharm
                 : v <= (ai_flo_t) mincharm ? mincharm
                 : v != v ? 0 : (intptr_t) v); }
 Ip += 1; ai_musttail return Continue(); }

// ============================================================================
// chain
// ============================================================================
op11(lvm_cap, chainp(Sp[0]) ? A(Sp[0]) : Sp[0])
op11(lvm_cup, chainp(Sp[0]) ? B(Sp[0]) : ZeroPoint)   // cup of an atom -> the const () (ZeroPoint), not the moving core (which had serial g->ip, not 0)
op11(lvm_books, g->book)   // the live layer chain (the abyss) -- runtime-internal, mopped at birth; ev.l's gv walks it
op11(lvm_setbooks, (g->book = Sp[0], zero))   // set the layer chain: the scope-layer door (open/use/close ride it); runtime-internal, mopped at birth
op11(lvm_mods, g->mods)   // (mods _): the module registry book; runtime-internal, mopped at birth
// push a fresh writable layer at the head of the book chain -- the runtime's
// enter: the session's scope, every defglob's target
struct ai *ai_layer_(struct ai *g) {
 if (!ai_ok(g)) return g;
 if (!ai_ok(g = map_new(g))) return g;                 // sp[0] = the fresh layer map
 g = gxr(ai_push(g, 1, ai_core_of(g)->book));          // (layer . chain)
 if (!ai_ok(g)) return g;
 ai_core_of(g)->book = *ai_core_of(g)->sp;
 return ai_pop(g, 1); }
// drop the link just below the head -- the runtime's bare leave, the inverse of
// one `use`; nothing below the head is a no-op
struct ai *ai_unsplice_(struct ai *g) {
 if (!ai_ok(g)) return g;
 word bk = ai_core_of(g)->book;
 if (!chainp(B(bk))) return g;
 g = gxl(ai_push(g, 2, A(bk), B(B(bk))));              // (head . below-the-neighbour)
 if (!ai_ok(g)) return g;
 ai_core_of(g)->book = *ai_core_of(g)->sp;
 return ai_pop(g, 1); }

op11(lvm_chainp, (chainp(Sp[0]) && !nomp(Sp[0])) ? putcharm(1) : zero)  // the surface chain?: a real compound list. a named symbol reads (name . mint) but counts as an atom

lvm(lvm_link) {
 Have(Width(struct ai_chain));
 struct ai_chain *w = (struct ai_chain*) Hp;
 Hp += Width(struct ai_chain);
 ini_chain(w, Sp[0], Sp[1]);
 *++Sp = word(w);
 Ip++;
 ai_musttail return Continue(); }

#define avm_slow(op, vop, ovf, fexpr) lvm(lvm_##op##n) { \
 word a = Sp[0], b = Sp[1]; \
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop); ai_musttail return Ap(lvm_vbin, g); } \
 if (twinp(a) || twinp(b)) { g->b = (ai_word) (vop); ai_musttail return Ap(lvm_twin_bin, g); } \
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint); \
 if (gemp(a) || gemp(b)) { word _res; Have(box_req); \
  ai_flo_t ad = toflo(a), bd = toflo(b); \
  emit_gem(_res, fexpr); \
  ai_musttail return Push(_res); } \
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b), t; \
  if (!ovf(av, bv, &t)) { word _res; Have(box_req); emit_int(_res, t); \
   ai_musttail return Push(_res); } } \
 if ((vop) == vop_mul) ai_musttail return Ap(lvm_bmul_start, g); /* O(n^2): run yieldable */ \
 Pack(g); g = ai_big_binop(g, vop); \
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g); \
 ai_musttail return Resume(); }
#define avm_slowdiv(op, vop, c_op, fexpr) lvm(lvm_##op##n) { \
 word a = Sp[0], b = Sp[1]; \
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop); ai_musttail return Ap(lvm_vbin, g); } \
 if (twinp(a) || twinp(b)) { g->b = (ai_word) (vop); ai_musttail return Ap(lvm_twin_bin, g); } \
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint); \
 if (gemp(a) || gemp(b) || b == zero) { word _res; Have(box_req); \
  ai_flo_t ad = toflo(a), bd = toflo(b); \
  emit_gem(_res, fexpr); \
  ai_musttail return Push(_res); } \
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b); \
  if (!(av == INTPTR_MIN && bv == -1)) { word _res; Have(box_req); emit_int(_res, av c_op bv); \
   ai_musttail return Push(_res); } } \
 { g->b = (ai_word) (vop); ai_musttail return Ap(lvm_bdiv_start, g); } }   /* big // and % run yieldable (resumable long division) */
// a bare mint (() too) is not a number, so a numeric lane has nothing to compute with
// and answers (), either side: - / // % & | ^ << >>. the sequence ops keep their own
// band rules and never come here -- () is the unit of + (joining nothing on) and the
// annihilator of * (repeating a sequence an absent number of times). comparisons and
// `=` stay strict.
// the ordered comparisons (< <= > >=) and their total order over all values are
// defined after vcmp_int/vcmp_flo (the per-op helpers they reuse), by lvm_vbin.


avm_slow(add, vop_add, __builtin_add_overflow, ad + bd)
avm_slow(sub, vop_sub, __builtin_sub_overflow, ad - bd)
avm_slow(mul, vop_mul, __builtin_mul_overflow, ad * bd)

avm_slowdiv(fquot, vop_fquot, /, ai_trunc(ad / bd))  // `//` truncating: float operand floors toward zero
avm_slowdiv(rem, vop_rem, %, ai_fmod(ad, bd))    // NaN on bd == 0

// `/` true division: exact integer when b divides a, a float box otherwise
// (the truncating quotient is `//`)
lvm(lvm_quotn) {
 word a = Sp[0], b = Sp[1];
 if (trayp(a) || trayp(b)) { g->b = (ai_word) (vop_quot); ai_musttail return Ap(lvm_vbin, g); }
 if (twinp(a) || twinp(b)) { g->b = (ai_word) (vop_quot); ai_musttail return Ap(lvm_twin_bin, g); }
 if (!isnum(a) || !isnum(b)) ai_musttail return Push(ZeroPoint);
 if (gemp(a) || gemp(b) || b == zero) { word _res; Have(box_req);   // ±inf/NaN on ÷0
  ai_flo_t ad = toflo(a), bd = toflo(b);
  emit_gem(_res, ad / bd);
  ai_musttail return Push(_res); }
 if (!bigp(a) && !bigp(b)) { intptr_t av = toint(a), bv = toint(b);  // bv != 0 (b != zero)
  if (!(av == INTPTR_MIN && bv == -1)) {                            // INT_MIN/-1 is exact but overflows -> bignum lane
   if (av % bv == 0) { word _res; Have(box_req); emit_int(_res, av / bv);
    ai_musttail return Push(_res); }
   word _res; Have(box_req);                                        // inexact -> promote to float
   emit_gem(_res, (ai_flo_t) av / (ai_flo_t) bv);
   ai_musttail return Push(_res); } }
 Pack(g); g = ai_big_quot_true(g);
 if (!ai_ok(g)) ai_musttail return Ap(_lvm_ghelp, g);
 ai_musttail return Resume(); }

// `-`: fixnum fast path, the () unit, then coins (`-` has no kind matrix, so the
// interception lives here), then the numeric slow lane
lvm(lvm_sub) {
 word a = Sp[0], b = Sp[1];
 if (charmp(a) && charmp(b)) { intptr_t t;
  if (!__builtin_sub_overflow((intptr_t) getcharm(a), (intptr_t) getcharm(b), &t) &&
      t >= mincharm && t <= maxcharm)
   ai_musttail return Push(putcharm(t)); }
 avm_unit(a, b);
 if (coinp(a) || coinp(b)) ai_musttail return Ap(lvm_sub_coin, g);
 ai_musttail return Ap(lvm_subn, g); }
// lvm_mul + its kind matrix live after the `+` string lane (they reuse nom_str /
// stringrank for the symbol-repetition case), below.

// `+` on sequences is order-preserving concatenation, a scalar lifting into the
// sequence on the side it appears:
//   str + str  -> byte concat          list + list -> spine append
//   str + list -> (link str list)      list + str  -> (append list (list str))
//   num + str  -> byte at front        num + list  -> (link num list)  (etc.)
// ai_add_lr selects the ordered reading.
// FIXME if we always want to allow commutative reading should this be false?
static const bool ai_add_lr = true;
// the byte law: text + number is one byte, strictly an exact integer 0..255
// (rep-blind: 66.0 is 66); anything else zero. answers the byte or -1.
ai_inline intptr_t seq_byte(word x) {
 if (charmp(x)) { intptr_t v = getcharm(x); return v < 0 || v > 255 ? -1 : v; }
 if (gemp(x)) { ai_flo_t f = gem_get(x);
  if (!(f >= 0 && f <= 255)) return -1;                 // range first (nan fails); cast below is safe
  return f != (ai_flo_t) (intptr_t) f ? -1 : (intptr_t) f; }
 return -1; }
// list lane: at least one operand is a chain (the matrix only routes list-involved
// chains here). list+list -> spine append; elt<->list -> the non-list operand joins
// as a scalar element (front if it is on the left, else appended at the tail).
lvm(lvm_add_seq) {
 // a named symbol is an atom for + (an element to adjoin), so the list tests use
 // chainp; sym + sym/str/num falls through to zero (no symbol string algebra)
 word a = Sp[0], b = Sp[1];
 if (chainp(a) && chainp(b)) {                         // list + list -> append a..b
  uintptr_t n = llen(a); Have(n * Width(struct ai_chain));
  a = Sp[0], b = Sp[1];
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = a; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  (w - 1)->b = b;                                // last cdr -> b
  ai_musttail return Push(word(base)); }
 if (chainp(a) || chainp(b)) {                          // elt <-> list (a bare mint never
  bool front = !ai_add_lr || chainp(b);               // reaches here -- lvm_add's identity early-out caught it)
  word lst = chainp(a) ? a : b, elt = chainp(a) ? b : a;
  if (front) { Sp[0] = elt, Sp[1] = lst; ai_musttail return Ap(lvm_link, g); }  // (link elt list)
  uintptr_t n = llen(lst) + 1; Have(n * Width(struct ai_chain));        // append elt at tail
  lst = chainp(Sp[0]) ? Sp[0] : Sp[1], elt = chainp(Sp[0]) ? Sp[1] : Sp[0];
  struct ai_chain *base = (struct ai_chain*) Hp, *w = base;
  Hp += n * Width(struct ai_chain);
  for (word l = lst; chainp(l); l = B(l), w++) ini_chain(w, A(l), word(w + 1));
  ini_chain(w, elt, ZeroPoint);                     // trailing (elt . ()) -- list terminator (zero-ontology)
  ai_musttail return Push(word(base)); }
 ai_musttail return Push(ZeroPoint); }          // neither is a real list (e.g. sym + sym/str/num): no algebra -> zero

// --- text lane: strings + symbols ---
// the string tower is string (0) < uninterned-sym (1) < named-sym|num (2); mixing
// demotes to the lower rank (min keeps the partner's type). the concat is built
// as one string in operand order, then returned per rank: as-is / fresh mint / interned.
struct ai_str *nom_str(struct ai *g, word x) {   // symbol -> name string, or 0 (a bare mint / the zero point / a non-symbol)
 return namep(x) ? str(nom(x)->name) : 0; }  // a named point (KNom) carries its name; a bare mint is nameless

ai_inline int stringrank(struct ai *g, word x) {    // str 0 / mint 1 / named-sym|num 2
 if (strp(x)) return 0;
 if (namep(x)) return 2;          // a named symbol: result re-interns (the min pulls a string operand to 0 -> demote)
 if (mintp(x)) return 1;          // a bare mint / the zero point: an uninterned (fresh) symbol
 return 2; }                      // a number contributes one byte (rank 2)
ai_inline uintptr_t stringlen(struct ai *g, word x) {  // bytes x contributes to a concat
 if (strp(x)) return len(x);
 if (nomp(x)) { struct ai_str *n = nom_str(g, x); return n ? n->len : 0; }
 return 1; }                                            // number -> one byte
// text concat: a's bytes then b's into a caller-owned allocation. `+`'s string lane and
// the apply lane's juxtaposition both land here, so the two cannot drift apart.
struct ai_str *seq_cat(struct ai *g, void *w, word a, word b);
ai_inline char *add_emit(struct ai *g, char *w, word x) {  // append x's bytes; return advanced w
 if (strp(x)) return (void) memcpy(w, txt(x), len(x)), w + len(x);
 if (nomp(x)) { struct ai_str *n = nom_str(g, x);
  return n ? ((void) memcpy(w, txt(n), n->len), w + n->len) : w; }
 return *w = (char) seq_byte(x), w + 1; }               // number -> one byte (unreachable from + since the
                                                        // degenerate lane; symbol paths never land here)
struct ai_str *seq_cat(struct ai *g, void *w, word a, word b) {
 struct ai_str *z = ini_str(str(w), stringlen(g, a) + stringlen(g, b));
 return add_emit(g, add_emit(g, txt(z), a), b), z; }
lvm(lvm_add_string) {
 word a = Sp[0], b = Sp[1];
 if (trayp(a) || trayp(b)) ai_musttail return Push(ZeroPoint); // array <-> string: undefined
 if ((!strp(a) && !nomp(a) && seq_byte(a) < 0) ||
     (!strp(b) && !nomp(b) && seq_byte(b) < 0)) ai_musttail return Push(ZeroPoint);
 int rank = min(stringrank(g, a), stringrank(g, b));
 uintptr_t n = stringlen(g, a) + stringlen(g, b);
 if (!n) ai_musttail return Push(rank ? ZeroPoint : EmptyString);   // the empty spelling is the zero point (cf. lvm_intern), not the zero charm
 uintptr_t req = str_width(n);
 Have(req);
 a = Sp[0], b = Sp[1];                                  // re-read post-GC
 struct ai_str *z = seq_cat(g, Hp, a, b);                     // a's bytes then b's, in order
 Hp += req;
 *++Sp = word(z);
 return rank == 0 ? (Ip++, Continue())                  // string
      : rank == 1 ? Ap(lvm_mint, g)                  // uninterned symbol (fresh)
                  : Ap(lvm_intern, g); }               // interned symbol
lvm(lvm_0) {                             // unsupported mix (array <-> string)
 ai_musttail return Push(ZeroPoint); }
// the unit lane: a bare mint rides through +/*. the dispatchers early-out a mint
// first, so these cells are belt and braces -- but they say the true thing, so
// the matrix stands correct on its own (mx.v checks the whole square).
lvm(lvm_bin_unit) {
 word a = Sp[0], b = Sp[1];
 if (a == ZeroPoint) ai_musttail return Push(b);
 if (b == ZeroPoint) ai_musttail return Push(a);
 if (mintp(a) && mintp(b)) ai_musttail return Push(a == b ? a : ZeroPoint);
 ai_musttail return Push(mintp(a) ? b : a); }
// the degenerate lane: a mixed pair with no lawful crossing answers the higher
// band's operand whole -- the foreigner arrives as that band's unit, since the
// only hom a group has into a free monoid is trivial. this is what restores +
// associativity (the byte law and the element-adjoin law could not associate).
lvm(lvm_bin_a) { word a = Sp[0]; ai_musttail return Push(a); }
lvm(lvm_bin_b) { word b = Sp[1]; ai_musttail return Push(b); }

// ============================================================================
// tray
// ============================================================================
size_t const ai_T[] = {
 [ai_Z] = Bytes,
 [ai_R] = Bytes,
 [ai_C] = 2 * Bytes,      // complex scalar: (re, im)
 [ai_O] = Bytes, };       // object: one tagged l word per element

uintptr_t ai_tray_bytes(struct ai_tray *v) {
 return sizeof(struct ai_tray) + v->rank * sizeof(word) + ai_T[v->type] * tray_nelem(v); }

// these are love_int.h's data-apply shims: one TU has to hold the body.
#if ai_data_section
// the slot is the kind. each sentinel lays in its own input section love_data.N,
// N its enum d value, on a grain of ai_data_stride -- so a run of one-fn sections
// tiles at exactly that, and ai_typ is one divide on (ap - lvm_sym) with in_data one
// unsigned compare (love.h). ld is told the tiling outright, in scripts mx.l lays
// from the same roster enum d comes from; holo needs no telling -- it lays each
// section on the grain the object declares, which is the same thing.
#define DSENT(nn, name, handler) \
 __attribute__((section("love_data." #nn), used, aligned(ai_data_stride))) \
 lvm(name) { ai_musttail return Ap(handler, g); }
DSENT(0,  lvm_sym,     data_sym_apply)
DSENT(1,  lvm_nom,     data_sym_apply)
DSENT(2,  lvm_sunbox,  data_num_apply)
DSENT(3,  lvm_gembox,  data_num_apply)
DSENT(4,  lvm_twinbox, data_num_apply)
DSENT(5,  lvm_big,     data_num_apply)
DSENT(6,  lvm_tray,    data_num_apply)
DSENT(7,  lvm_str,     data_string_apply)
DSENT(8,  lvm_chain,   data_pair_apply)
#else
lvm(lvm_tray)   { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_big)   { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_str)   { ai_musttail return Ap(data_string_apply, g); }
lvm(lvm_sym)   { ai_musttail return Ap(data_sym_apply, g); }
lvm(lvm_nom)   { ai_musttail return Ap(data_sym_apply, g); }
lvm(lvm_chain) { ai_musttail return Ap(data_pair_apply, g); }
lvm(lvm_gembox)   { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_sunbox)  { ai_musttail return Ap(data_num_apply, g); }
lvm(lvm_twinbox)  { ai_musttail return Ap(data_num_apply, g); }
#endif

// def1 is nifs.h's, and this TU is where that header lands; snap.c walks the
// same table to number the aps it serializes.
struct ai_def const *const ai_def1 = def1;
uintptr_t const ai_def1_n = countof(def1);

