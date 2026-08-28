// map.c -- map, codegen backend. one translation unit of the runtime;
// the shared layouts and the cross-TU seam are src/love_int.h.
#include "love_int.h"
// this file's own, forward-declared so order within it does not matter.
static ai_noinline struct ai *map_grow(struct ai *g);
static ai_noinline uintptr_t hash_two(struct ai *g, word x);
static ai_noinline word ai_mapdel(struct ai *g, word m, word k, word dflt);
// ============================================================================
// map (lookup-lambda backed by an open-addressed thread; see tabp comment)
// ============================================================================
// backing is internal -- only ever reached from a header[1], never applied as a
// l value; its ap answers () like lvm_cask should it ever be applied (it won't).
lvm(lvm_map_data) {
 Ip = cell(*++Sp); *Sp = ZeroPoint; ai_musttail return Continue(); }

// the backing slot of k, or -- if absent -- the first empty slot on its probe
// chain. load is kept < 3/4 so an empty slot always terminates the sound.
uintptr_t map_probe(struct ai *g, word m, word k, bool *found) {
 uintptr_t mask = map_cap(m) - 1, i = hash(g, k) & mask;
 word *s = map_slots(m);
 for (;; i = (i + 1) & mask) {
  word sk = s[2 * i];
  if (sk == map_gap) return *found = false, i;
  if (eql(g, k, sk)) return *found = true, i; } }

word ai_mapget(struct ai *g, word dflt, word k, word m) {
 bool found; uintptr_t i = map_probe(g, m, k, &found);
 return found ? map_slots(m)[2 * i + 1] : dflt; }

// the layered global read: g->book is a chain of books walked head-first. a
// per-layer miss needs its own sentinel -- a stored () must shadow, never fall
// through. the l twin is ev.l's gv; keep them in step.
word bookget(struct ai *g, word dflt, word k) {
 static union u const miss[1];
 for (word c = g->book; chainp(c); c = B(c)) {
  word v = ai_mapget(g, word(miss), k, A(c));
  if (v != word(miss)) return v; }
 return dflt; }

// the layered macro read: each layer's macro table rides its [zero] slot; miss
// answers 0, the no-macro convention
word macroget(struct ai *g, word k) {
 static union u const miss[1];
 for (word c = g->book; chainp(c); c = B(c)) {
  word mt = ai_mapget(g, word(miss), zero, A(c));
  if (mt == word(miss)) continue;
  word v = ai_mapget(g, word(miss), k, mt);
  if (v != word(miss)) return v; }
 return 0; }

// fill an empty cap-slot backing at b (cap a power of two); caller reserves it.
union u *map_fill_back(union u *b, uintptr_t cap) {
 b[0].ap = lvm_map_data, b[1].x = putcharm(0), b[2].x = putcharm(cap);
 for (uintptr_t i = 0; i < cap; i++) b[3 + 2 * i].x = map_gap, b[4 + 2 * i].x = zero;
 return tagthread(b, 3 + 2 * cap); }

// double the backing of the map at sp[2], rehash, swap into header[1]; the
// header never moves, so aliased references stay valid
static ai_noinline struct ai *map_grow(struct ai *g) {
 uintptr_t ncap = 2 * map_cap(g->sp[2]);
 if (!ai_ok(g = ai_have(g, 4 + 2 * ncap))) return g;
 word m = g->sp[2];                                 // re-fetch header after GC
 union u *nb = map_fill_back((union u*) g->hp, ncap);
 g->hp += 4 + 2 * ncap;
 word *os = map_slots(m), *ns = &nb[3].x;
 uintptr_t ocap = map_cap(m), nlen = 0, nmask = ncap - 1;
 for (uintptr_t j = 0; j < ocap; j++) {
  word k = os[2 * j];
  if (k == map_gap) continue;
  uintptr_t i = hash(g, k) & nmask;
  while (ns[2 * i] != map_gap) i = (i + 1) & nmask;
  ns[2 * i] = k, ns[2 * i + 1] = os[2 * j + 1], nlen++; }
 nb[1].x = putcharm(nlen);
 cell(m)[1].x = (word) nb;                         // swap backing; header identity stable
 gen_wb(g, m, (word) nb);                          // barrier: old header now points at the fresh (young) backing
 return g; }

// (put k v map): mutate in place; grow (may GC) on a new key past the load
// factor, re-reading k/v from the stack afterwards. leaves the map at sp[2].
ai_noinline struct ai *ai_mapput(struct ai *g) {
 if (!ai_ok(g)) return g;
 bool found; uintptr_t i = map_probe(g, g->sp[2], g->sp[0], &found);
 if (found) {
  gen_wb(g, map_back(g->sp[2]), g->sp[1]);         // barrier: a young value into an old backing
  return map_slots(g->sp[2])[2 * i + 1] = g->sp[1], g->sp += 2, g; }
 if ((map_len(g->sp[2]) + 1) * 4 >= map_cap(g->sp[2]) * 3) {
  if (!ai_ok(g = map_grow(g))) return g;
  i = map_probe(g, g->sp[2], g->sp[0], &found); }   // re-probe larger backing
 word *s = map_slots(g->sp[2]);
 s[2 * i] = g->sp[0], s[2 * i + 1] = g->sp[1];
 gen_wb(g, map_back(g->sp[2]), g->sp[0]);          // barrier: a young key ...
 gen_wb(g, map_back(g->sp[2]), g->sp[1]);          // ... or young value into an old backing
 cell(map_back(g->sp[2]))[1].x = putcharm(map_len(g->sp[2]) + 1);
 return g->sp += 2, g; }

// ai_mapdel: delete k, backward-shift the probe chain so no tombstone is
// needed; v is the not-found result. no allocation. leaves the map at sp[2].
static ai_noinline word ai_mapdel(struct ai *g, word m, word k, word dflt) {
 bool found; uintptr_t i = map_probe(g, m, k, &found);
 if (!found) return dflt;
 word *s = map_slots(m); uintptr_t mask = map_cap(m) - 1;
 for (uintptr_t j = i;;) {
  j = (j + 1) & mask;
  if (s[2 * j] == map_gap) break;
  uintptr_t h = hash(g, s[2 * j]) & mask;            // ideal slot of the probed key
  bool gap = i <= j ? (h <= i || h > j) : (h <= i && h > j);   // h not in (i, j]
  if (gap) {
   s[2 * i] = s[2 * j], s[2 * i + 1] = s[2 * j + 1];
   gen_wb(g, map_back(m), s[2 * i]), gen_wb(g, map_back(m), s[2 * i + 1]);  // delete shifts a (maybe young) k/v within an old backing
   i = j; } }
 s[2 * i] = map_gap, s[2 * i + 1] = zero;
 cell(map_back(m))[1].x = putcharm(map_len(m) - 1);
 return m; }

// C-callable fresh empty map, pushed on sp[0]. same shape as lvm_tablet.
struct ai *map_new(struct ai *g) {
 uintptr_t cap = map_min_cap, nb = 4 + 2 * cap;
 if (!ai_ok(g = ai_have(g, nb + 3))) return g;
 union u *b = map_fill_back((union u*) g->hp, cap), *h = (union u*) (g->hp + nb);
 h[0].ap = lvm_map_lookup, h[1].x = (word) b, tagthread(h, 2);
 g->hp += nb + 3;
 return ai_push(g, 1, (word) h); }

// (tablet n): a fresh empty map; n is a size hint (presized below the 0.75 load
// factor, so inserting n known keys never rehashes). n<=0 keeps the min capacity.
lvm(lvm_tablet) {
 intptr_t raw = charmp(Sp[0]) ? getcharm(Sp[0]) : 0;          // saturate to a bounded green charm first
 uintptr_t hint = raw <= 0 ? 0 : (uintptr_t) raw > map_hint_max ? map_hint_max : (uintptr_t) raw;
 uintptr_t cap = map_min_cap;
 while (cap * 3 <= hint * 4) cap *= 2;                        // grow to hold `hint` below the 0.75 load factor
 uintptr_t nb = 4 + 2 * cap;
 Have(nb + 3);
 union u *b = map_fill_back((union u*) Hp, cap);
 union u *h = (union u*) (Hp + nb);
 h[0].ap = lvm_map_lookup, h[1].x = (word) b, tagthread(h, 2);
 Sp[0] = (word) h;
 Hp += nb + 3; ai_musttail return Next(1); }

// (m k): map application is lookup, () if absent; unwinds like self-quote
lvm(lvm_map_lookup) {
 word v = ai_mapget(g, ZeroPoint, Sp[0], (word) Ip);   // a map miss answers () (the zero point), not the number 0
 Ip = cell(*++Sp); *Sp = v; ai_musttail return Continue(); }

op11(lvm_tabp, tabp(Sp[0]) ? putcharm(1) : zero)
// (lit? x): the upper segment of the lattice, ai_kind >= KTablet -- tablets and the
// tops above (closures, nifs, cask/port), never the fresh value-data below. a
// coin's die decides (DieHot truthy = lit): lit? is the lattice cut, not storage.
lvm(lvm_litp) {
 word x = Sp[0];
 bool lit = coinp(x) ? !ai_nilp(g, die_get(g, coin_die(x), DieHot))   // a coin: its die decides
                     : ai_kind(x) >= KTablet;                             // else the lattice cut
 Sp[0] = lit ? putcharm(1) : zero;
 ai_musttail return Next(1); }
// (hot? x): an opaque hot handle -- a cask or a port (a task is a fixnum id, not a handle)
op11(lvm_hotp, (caskp(Sp[0]) || iop(Sp[0])) ? putcharm(1) : zero)

// (hash x) -- the general hashing method exposed to l as a fixnum.
op11(lvm_dig, putcharm(hash(g, Sp[0])))

lvm(lvm_peep) {                                // (peep coll key default): collection-first
 word x = Sp[0], k = Sp[1], z = Sp[2], n;
 if (caskp(x)) {                                 // mutable byte string: byte index
  struct ai_str *s = cask(x)->str;
  if (charmp(k) && (n = getcharm(k)) >= 0 && n < (word) len(s))
   z = putcharm((unsigned char) txt(s)[n]); }
 else if (tabp(x)) z = ai_mapget(g, z, k, x);     // map lookup (not a data sentinel)
 else if (lamp(x) && datp(x)) switch (typ(x)) {
  default: break;                               // a bare mint (DMint) is not indexable
  case DGem:                                    // a rank-0 scalar float: a zero key derefs to itself
  case DSun:                                   // ... same for a sun
  case DTwin:                                   // ... and a complex scalar
   if (zerop(k)) z = x;
   break;
  case DTray: {
   // array index: a fixnum (rank-1) or a row-major shape-list (rank-N);
   // out-of-bounds or wrong rank falls through to the default
   struct ai_tray *v = tray(x);
   intptr_t o = tray_off(v, k); uintptr_t off = (uintptr_t) o; bool ok = o >= 0;
   if (ok && v->type == ai_O) z = tray_get_obj(v, off);   // object: the slot is the value
   else if (ok && v->type == ai_C) {                       // packed complex -> a (re,im) box
    Have(twin_req); v = tray(Sp[0]);                      // re-read coll (Sp[0]) post-Have
    ai_flo_t *fp = tray_data(v);
    z = mk_twin(&Hp, fp[2*off], fp[2*off+1]); }
   else if (ok) { word _res; Have(box_req); v = tray(Sp[0]);
    if (v->type >= ai_R) emit_gem(_res, tray_get_flo(v, off));
    else emit_int(_res, tray_get_int(v, off));
    z = _res; }
   break; }
  case DString:
   // byte as its unsigned value 0..255 (txt is signed char[]: cast, or a high byte sign-extends)
   if (charmp(k) && (n = getcharm(k)) >= 0 && n < (word) len(x))
    z = putcharm((unsigned char) txt(x)[n]);
   break;
  case DChain:
   if (charmp(k) && (n = getcharm(k)) >= 0) {
    while (n-- && chainp(x = B(x)));
    if (chainp(x)) z = A(x); } }
 ai_musttail return Answerp(2, z); }

// (pin coll key val): a map or a cask has a cell, so the write is in place and the same
// collection answers; text, a chain and a tray have none, so a fresh one carrying the pin
// answers -- the functional update. (peep (pin c k v) k d) = v wherever the pin lands;
// a rank-0 scalar is the one kind peep reads that pin does not write (there is no cell to
// replace, only the value itself). out-of-range/wrong-kind is a silent no-op answering
// coll, the byte ops' misuse convention.
lvm(lvm_pin) {
 word x = Sp[0], n;                              // coll
 if (tabp(x)) {
  Sp[0] = Sp[1], Sp[1] = Sp[2], Sp[2] = x;       // ai_mapput wants (sp0,sp1,sp2)=(key,val,coll)
  Pack(g);
  if (!ai_ok(g = ai_mapput(g))) ai_musttail return Ap(_lvm_ghelp, g);
  Unpack(g);
  ai_musttail return Next(1); }
 if (caskp(x)) {
  if (charmp(Sp[1]) && charmp(Sp[2]) && (n = getcharm(Sp[1])) >= 0 && n < (word) len(cask(x)->str))
   txt(cask(x)->str)[n] = (char) getcharm(Sp[2]);    // index = key = Sp[1], val = Sp[2]
  ai_musttail return Answerp(2, x); }
 if (lamp(x) && datp(x)) switch (typ(x)) {
  default: break;                                // a mint, a scalar: nothing to pin into
  case DString: {                                // one byte replaced in a fresh text
   if (!charmp(Sp[1]) || !charmp(Sp[2])) break;
   if ((n = getcharm(Sp[1])) < 0 || n >= (word) len(x)) break;
   uintptr_t sz = len(x), req = str_width(sz);
   Have(req);
   struct ai_str *s = ini_str(str(Hp), sz); Hp += req;
   memcpy(s->bytes, txt(Sp[0]), sz);             // re-read coll: the Have may have moved it
   s->bytes[n] = (char) getcharm(Sp[2]);
   ai_musttail return Answerp(2, word(s)); }
  case DChain: {                                 // the prefix copied, the tail shared
   if (!charmp(Sp[1]) || (n = getcharm(Sp[1])) < 0 || n >= (word) llen(x)) break;
   Have((uintptr_t) (n + 1) * Width(struct ai_chain));
   struct ai_chain *w = (struct ai_chain*) Hp, *base = w;
   Hp += (uintptr_t) (n + 1) * Width(struct ai_chain);
   word l = Sp[0];                               // re-read coll post-Have
   for (word i = 0; i < n; i++, w++, l = B(l)) ini_chain(w, A(l), word(w + 1));
   ini_chain(w, Sp[2], B(l));                    // the pinned cell, then the old tail
   ai_musttail return Answerp(2, word(base)); }
  case DTray: {                                  // the whole payload copied, one slot stored
   intptr_t o = tray_off(tray(x), Sp[1]);
   if (o < 0) break;
   uintptr_t req = b2w(ai_tray_bytes(tray(x)));
   Have(req);
   struct ai_tray *v = (struct ai_tray*) Hp; Hp += req;
   memcpy(v, tray(Sp[0]), ai_tray_bytes(tray(Sp[0])));   // re-read coll post-Have
   if (!tray_put(v, (uintptr_t) o, Sp[2])) { Hp -= req; break; }   // a non-number into a numeric tray
   ai_musttail return Answerp(2, word(v)); } }
 ai_musttail return Answerp(2, x); }

// (pull coll key default): remove key from a map, answering its value or default
// (symmetry with peep); a non-map coll yields default
lvm(lvm_pull) {
 word coll = Sp[0], v = Sp[2];                   // default
 if (tabp(coll)) {
  v = ai_mapget(g, Sp[2], Sp[1], coll);           // value, or default if absent
  ai_mapdel(g, coll, Sp[1], Sp[2]); }             // remove in place (no-op if absent)
 ai_musttail return Answerp(2, v); }

lvm(lvm_keys) {
 intptr_t list = ZeroPoint;                         // () terminator / empty-map result (zero-ontology)
 if (tabp(Sp[0])) {
  uintptr_t cap = map_cap(Sp[0]), n = map_len(Sp[0]);
  Have(n * Width(struct ai_chain));
  struct ai_chain *chains = (struct ai_chain*) Hp;
  Hp += n * Width(struct ai_chain);
  word *s = map_slots(Sp[0]);                    // re-read after Have (GC may move the map)
  for (uintptr_t i = cap; i;)
   if (s[2 * --i] != map_gap)
    ini_chain(chains, s[2 * i], list), list = (intptr_t) chains, chains++; }
 Sp[0] = list;
 Ip += 1;
 ai_musttail return Continue(); }

ai_noinline uintptr_t hash_two(struct ai *g, word x) {
 word *base = off_pool(g), *top = base + g->len, *w = base;
 for (uintptr_t h = mix;; x = *--w) {
  while (chainp(x)) {
   if (w == top) __builtin_trap();       // worklist overflow: a cycle
   h = (h ^ mix) * mix;                  // mark a chain node
   *w++ = A(x), x = B(x); }
  h = (h ^ hash(g, x)) * mix;          // x is a leaf: hash won't recur
  if (w == base) return h; } }

// the anchor an out-of-pool ap hashes against: the offset survives a bake/wake
// where the raw address does not (a bake-time bucket index would miss at wake and
// every nif-keyed table would silently read empty).
static const char hash_base[1] = {0};
struct arib; uintptr_t shash(struct ai *g, word x, struct arib *env);  // α-invariant source hash
bool clo_nfhash(struct ai *g, word x, uintptr_t *out);  // partial-app -> capture-substitution normal-form hash (the beta bridge)
uintptr_t hash(struct ai *g, intptr_t x) {
 if (charmp(x)) return rot(x*mix);
 if (!datp(x)) {
   // out-of-pool: offset from hash_base. in-pool: a sourced lambda hashes its
   // \-expr α-invariantly (agreeing with `=`), else by length. all GC-stable.
   if (!in_heap(g, x)) return rot((x - (intptr_t) hash_base) * mix);   // a tenured closure lives in the major pool, still in-heap
   union u *k = cell(x); struct ai_tag *tg = ttag(g, k);
   if (tag_head(tg) < k) return shash(g, k[-1].x, 0);   // no-capture lambda: α-invariant source hash
   uintptr_t nf;                                        // partial-app over a sourced base: hash its capture-substitution
   if (clo_nfhash(g, x, &nf)) return nf;                // normal form, so the beta bridge stays hash-consistent (=-equal -> same hash)
   uintptr_t r = mix;                                   // else (continuation / handle / bif-based partial-app): by object length
   for (union u *y = k; y < (union u*) tg; y++) r ^= r * mix;
   return r; }
 switch (typ(x)) {
   case DChain: return hash_two(g, x);
   case DMint: return sym(x)->code;
   case DNom: return nom(x)->dig;                  // the cached spelling hash -- a serial would key
                                                   // bucket order to intern history (a reproducible-
                                                   // build leak); same-spelled noms collide, `=` separates
   case DTray: {
    uintptr_t len = ai_tray_bytes(tray(x)), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DBig: {
    uintptr_t len = ai_big_bytes(big(x)), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DGem: {                                 // hash the lean box (ap is GC-stable, payload is the value)
    uintptr_t len = gem_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DSun: {                                // same: hash the lean box bytes
    uintptr_t len = sun_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DTwin: {                                // same: hash the lean (ap, re, im) box bytes
    uintptr_t len = twin_req * sizeof(word), h = mix;
    for (uint8_t const *bs = (void*) x; len--; h ^= *bs++, h *= mix);
    return h; }
   case DString: {
    uintptr_t n = len(x), h = mix;
    char const *bs = txt(x);
    while (n--) h ^= (uint8_t) *bs++, h *= mix;
    return h; } }
 __builtin_trap(); }

// ============================================================================
// codegen backend brick 1 -- the native-install seam (provisional; -> `ev`)
// ============================================================================
// the W^X arena finalizer: recover the ai_str base from the code address and
// munmap (a dead native's header is the out-of-pool code addr, a live one's a forward)
#if __STDC_HOSTED__
static void nat_unmap(void *p) {
 char *code = (char*) ((union u*) p)[0].ap;            // header == the W^X code address
 struct ai_str *base = str(code - sizeof(struct ai_str));   // code == s->bytes
 munmap(base, code_maplen(base->len)); }
#endif

// (nif code interp src arity): emitted bytes -> a transparent applicable native
// closure (the lvm ABI: g=rdi Ip=rsi Hp=rdx Sp=rcx). arity 1: a 6-word cell
// entering the native body directly; arity>=2: an 8-word lvm_cur cell (curry to
// saturation). value[-1]=src (=/show-identical to the source), value[1]=interp
// (the deopt fallback, so native is never wrong), lvm_ret at the same offset in
// both, so the emitted body is layout-blind. cell[0] duplicates the code addr:
// run_finalizers' dead/live discriminator. internal: the egg mops it.
// a decline (bad args, no code pages, inle) answers the interp twin itself,
// so every caller transparently falls back to bytecode.
lvm(lvm_nif) {
 word codebuf = Sp[0];                        // Sp[0]=code Sp[1]=interp Sp[2]=src Sp[3]=arity
 intptr_t ar = oddp(Sp[3]) ? getcharm(Sp[3]) : 0;
 if (!(strp(codebuf) || caskp(codebuf)) || ar < 1) ai_musttail return Answerp(3, Sp[1]);
 uintptr_t n = len(bytes_of(codebuf));
 if (n == 0) ai_musttail return Answerp(3, Sp[1]);
#ifdef __wasm__                                // wasm has no executable code pages: a jump to a data address traps.
 ai_musttail return Answerp(3, Sp[1]); //  decline unconditionally -> the interp twin runs (emscripten's mprotect
#endif                                         //  is a no-op returning 0, so the mprotect guard below does not catch this).
#if __STDC_HOSTED__
 // inle declines: its heap rides the NX hhdm window (a wild jump into the
 // heap faults by design), and a heap copy would move under the collector
 // besides. the interp twin runs; a metal nat door would want out-of-pool
 // pages through the low window, which keeps X.
 if (__ai_osv < 0) ai_musttail return Answerp(3, Sp[1]);
 struct ai_str *s;
 {
  Have(9 + Width(struct ai_fz));              // 9 covers both cells (6/8 words) + tag + fz
  size_t maplen = code_maplen(n);
  void *base = mmap(0, maplen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (base == MAP_FAILED) ai_musttail return Answerp(3, Sp[1]);
  s = ini_str(str(base), n);
  memcpy(txt(s), txt(bytes_of(Sp[0])), n);    // reload codebuf: a GC in Have may have moved it
  if (mprotect(base, maplen, PROT_READ | PROT_EXEC))
   { munmap(base, maplen); ai_musttail return Answerp(3, Sp[1]); }
#ifndef __wasm__                               // guarded: emscripten's clang has no clear_cache intrinsic (dead here anyway -- wasm early-declined above)
  __builtin___clear_cache(txt(s), txt(s) + n); // AArch64: the I-cache is not coherent with the freshly
#endif                                         // written D-cache -- flush or it runs stale bytes (no-op on x86)
 }
#else
 Have(str_width(n) + 9);                      // freestanding: RAM is executable, a heap copy runs
 struct ai_str *s = ini_str(str(Hp), n); Hp += str_width(n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);
 __builtin___clear_cache(txt(s), txt(s) + n);  // same I-cache flush on the freestanding (RWX) path
#endif
 union u *k = (union u*) Hp;
 if (ar == 1) {                               // 6-word direct-entry cell (the old nat)
  Hp += 7;
  k[0].ap = (lvm_t*) txt(s);                  // header (== code, out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src   (value[-1], for =/show)
  k[2].ap = (lvm_t*) txt(s);                  // code  (value[0]): the emitted body, the entry
  k[3].x  = Sp[1];                            // interp(value[1]): deopt fallback
  k[4].ap = lvm_ret;                          // value[2]: fast-path return
  k[5].x  = putcharm(0);                      // ret n=1
  tagthread(k, 6);
 } else {                                     // 8-word lvm_cur cell (the old natn)
  Hp += 9;
  k[0].ap = (lvm_t*) txt(s);                  // header (out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src (value[-1])
  k[2].ap = lvm_cur;                          // value[0]: curry to saturation
  k[3].x  = putcharm(ar);
  k[4].ap = (lvm_t*) txt(s);                  // native body (lvm_cur resume Ip+2)
  k[5].x  = Sp[1];                            // interp: deopt fallback
  k[6].ap = lvm_ret;
  k[7].x  = putcharm(ar - 1);                 // ret pops n=arity
  tagthread(k, 8);
 }
#if __STDC_HOSTED__
 struct ai_fz *z = (struct ai_fz*) Hp; Hp += Width(struct ai_fz);
 z->p = k, z->fn = nat_unmap, z->next = g->fz, g->fz = z;
#endif
 ai_musttail return Answerp(3, word(k + 2)); }

lvm(lvm_nifx) {
 word codebuf = Sp[0];                        // Sp[0]=code Sp[1]=interp Sp[2]=src Sp[3]=arity Sp[4]=extras
 intptr_t ar = oddp(Sp[3]) ? getcharm(Sp[3]) : 0;
 if (!(strp(codebuf) || caskp(codebuf)) || ar < 1) ai_musttail return Answerp(4, Sp[1]);
 uintptr_t n = len(bytes_of(codebuf));
 if (n == 0) ai_musttail return Answerp(4, Sp[1]);
#ifdef __wasm__                                // wasm has no executable code pages: a jump to a data address traps.
 ai_musttail return Answerp(4, Sp[1]); //  decline unconditionally -> the interp twin runs (emscripten's mprotect
#endif                                         //  is a no-op returning 0, so the mprotect guard below does not catch this).
#if __STDC_HOSTED__
 if (__ai_osv < 0) ai_musttail return Answerp(4, Sp[1]);   // inle declines: the twin above says why
 struct ai_str *s;
 {
  Have(11 + Width(struct ai_fz));             // 11 covers both cells (7/9 words) + tag + fz
  size_t maplen = code_maplen(n);
  void *base = mmap(0, maplen, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (base == MAP_FAILED) ai_musttail return Answerp(4, Sp[1]);
  s = ini_str(str(base), n);
  memcpy(txt(s), txt(bytes_of(Sp[0])), n);    // reload codebuf: a GC in Have may have moved it
  if (mprotect(base, maplen, PROT_READ | PROT_EXEC))
   { munmap(base, maplen); ai_musttail return Answerp(4, Sp[1]); }
#ifndef __wasm__                               // guarded: emscripten's clang has no clear_cache intrinsic (dead here anyway -- wasm early-declined above)
  __builtin___clear_cache(txt(s), txt(s) + n); // AArch64: the I-cache is not coherent with the freshly
#endif                                         // written D-cache -- flush or it runs stale bytes (no-op on x86)
 }
#else
 Have(str_width(n) + 11);                     // freestanding: RAM is executable, a heap copy runs
 struct ai_str *s = ini_str(str(Hp), n);
 Hp += str_width(n);
 memcpy(txt(s), txt(bytes_of(Sp[0])), n);
 __builtin___clear_cache(txt(s), txt(s) + n);  // same I-cache flush on the freestanding (RWX) path
#endif
 union u *k = (union u*) Hp;
 if (ar == 1) {                               // 7-word direct-entry cell: nif + the extras slot
  Hp += 8;
  k[0].ap = (lvm_t*) txt(s);                  // header (== code, out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src   (value[-1], for =/show)
  k[2].ap = (lvm_t*) txt(s);                  // code  (value[0]): the emitted body, the entry
  k[3].x  = Sp[1];                            // interp(value[1]): deopt fallback
  k[4].ap = lvm_ret;                          // value[2]: fast-path return
  k[5].x  = putcharm(0);                      // ret n=1
  k[6].x  = Sp[4];                            // extras (value[3]+8 = Ip+32): GC-walked, image-encoded
  tagthread(k, 7);
 } else {                                     // 9-word lvm_cur cell: natn + the extras slot
  Hp += 10;
  k[0].ap = (lvm_t*) txt(s);                  // header (out-of-pool): finalizer dead-detect
  k[1].x  = Sp[2];                            // src (value[-1])
  k[2].ap = lvm_cur;                          // value[0]: curry to saturation
  k[3].x  = putcharm(ar);
  k[4].ap = (lvm_t*) txt(s);                  // native body (lvm_cur resume Ip+2)
  k[5].x  = Sp[1];                            // interp: deopt fallback
  k[6].ap = lvm_ret;
  k[7].x  = putcharm(ar - 1);                 // ret pops n=arity
  k[8].x  = Sp[4];                            // extras at Ip+32 from the body entry (same offset as arity-1)
  tagthread(k, 9);
 }
#if __STDC_HOSTED__
 struct ai_fz *z = (struct ai_fz*) Hp; Hp += Width(struct ai_fz);
 z->p = k, z->fn = nat_unmap, z->next = g->fz, g->fz = z;
#endif
 ai_musttail return Answerp(4, word(k + 2)); }


// (pour dst doff src soff n): copy n bytes of string-or-cask src into cask dst,
// clamped to both backings (an out-of-range ask copies less, never tramples); answers dst
lvm(lvm_bcopy) {
 word dst = Sp[0], src = Sp[2];
 if (caskp(dst) && (strp(src) || caskp(src))) {
  struct ai_str *d = cask(dst)->str, *s = bytes_of(src);
  intptr_t doff = getcharm(Sp[1]), soff = getcharm(Sp[3]), n = getcharm(Sp[4]),
           dl = len(d), sl = len(s);
  if (n < 0) n = 0;
  if (doff < 0) doff = 0;
  if (soff < 0) soff = 0;
  if (doff + n > dl) n = dl - doff;
  if (soff + n > sl) n = sl - soff;
  if (n > 0) memmove(txt(d) + doff, txt(s) + soff, n); }
 ai_musttail return Answerp(4, dst); }

// public predicate for frontends that need to check string args
bool ai_strp(ai_word x) { return strp(x); }

