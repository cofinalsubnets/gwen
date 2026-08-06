# the generational collector

love's collector is generational: a cheap **minor** scavenges only fresh allocation and leaves
the tenured set untouched, with the full collection demoted to a rare **major**. It is the only
collector — there is no non-generational fallback and no build flag that turns it off; a
frontend whose allocator cannot supply the major pool does not run.

A runnable love model lives in [`doc/proto/gengc.l`](proto/gengc.l) — a world is
`(nur old rem roots next)`, and its asserts pin the invariants (most pointedly: the write
barrier is *necessary*, assert 3b). `proof/rocq/gc.v` proves the load-bearing one.
[`tools/gcstat.l`](../tools/gcstat.l) measures churn on an `-DAI_STAT` host.

## Why

Allocation-heavy workloads run **85–94% infant mortality**, and a single-generation copying
collector recopies the persistent live set **~150×** over a run — every full collection drags
the same long-lived objects through to-space again.

A copying collector's per-collection cost is proportional to *survivors*, not garbage; so high
mortality is not wasted *copy* work — it is wasted *frequency*. The waste is the recopy of the
tenured set on every cycle, and a nursery removes exactly that: a minor copies only the young
survivors; the old set is never walked. The glaze/deforestation work
([`doc/proto/forest.l`](proto/forest.l)) is the orthogonal attack — it *avoids* allocating the
intermediates; this *reaps* cheaply whatever still gets allocated.

## The insight that makes it small

`gcp` forwards a word **only if it points inside `[p0, t0)`** — the from-space bounds — and
leaves every other word alone. So the minor collector is *already written*: point `p0`/`t0` at
the young range and `gcp` copies young objects, leaves old objects in place (they fall outside
the bounds), and the same Cheney scan (`evac_*`) chases the promoted objects' fields.

**Generations are read from the address.** love objects are header-minimal — a string carries
no tag, its size derives from `len` — so there is no spare bit for an age field, and none is
needed: `ai_young(p)` is the range test `g->minor <= p < g->hp`.

## The two pools

- **the minor pool** is the *main* pool — `[core | heap→ … ←stack]`. Allocations bump `hp`
  unchanged, so there is no allocation-path surgery and the whole main heap is young.
- **the major pool** (`g->major_pool`) is a separate two-space region holding only tenured
  heap.

Everything is born young; an object tenures by surviving its first minor.

**A minor** (`gen_minor`) Cheneys the minor pool `[end, hp)` into the major pool's active half
(appending at `major_hp`), then resets `hp = end`. Its roots are the stack, the C roots
(`g->root`), the core vars, the task ring and the parked ring, the finalizer list, **the
remembered set**, and a hand-written promotion of the weak intern map's structure (entries stay
weak; a young header is `gcp`'d, a tenured one has its possibly-young backing scanned in place).
The scan starts at the append point, so a minor never reads a dead or non-object word.

**A major** (`gen_major`) is one **reachability** Cheney pass from the real roots over BOTH
from-spaces — the major pool's active half and the minor pool (`gcp`'s second range) — into the
major pool's spare half, or a fresh pair sized to *major-live + minor-young*. Then it rebuilds
the intern map, runs finalizers, flips, and resets the minor pool. Reachability rather than a
linear sweep is what lets a rem-set overflow force a major safely: dead objects and their stale
pointers are simply never visited.

On a minor the core and stack stay put; only the minor pool moves. `()` is `ZeroPoint`, an
out-of-pool constant, not the core — which is what lets `gen_grow` move core+stack freely.

## The write barrier

An old→young edge must be remembered or the minor loses the young object. `gen_wb(g, src, p)`
remembers `src` when `src` is old and `p` is young. The mutation sites are enumerable:

- **maps and boxes** — `ai_mapput`, `map_grow` (which swaps the backing, itself an
  old-header→young-backing edge), `ai_mapdel`'s backward shift;
- **the reader's set-tail** — building a list by `B(tail) = newcons`;
- **ev's `poke` and c0's stores** — `gen_wb_cell`/`gen_wb_two` remember the exact mutated cell
  of a **tagged span** (a spin thread, an env) or the mutated cons, both shapes the minor's
  in-place scan re-walks soundly. `poke`'s tagged-span contract lives at `lvm_poke`; ev's
  backpatch sites box their cell (`qsite`) so no poke ever targets a chain field;
- **the task ring** — splicing a young yield/spawn snapshot into an existing node;
- **ports and string sinks** taking a fresh young backing.

The rem set is deduped and small (the book plus a few). The one escape is **overflow**
(`rem_miss`): a dropped entry forces the next collection MAJOR, which traces from roots and
needs no rem set. So a minor only ever runs under a complete rem set — exactly the barrier
`gc.v` proves sound, with no unmodeled leg.

⚠ The premise "maps are the only mutable-pointer structure" holds for **execution** but not for
the **compiler**: runtime pure compute, even heavy map mutation, mints zero unbarriered
old→young edges, while the reader, ev and c0 all mutate old objects in place. Every one of
those routes is precise on the one rem set; there is no coarse dirty flag.

## Scheduling and sizing

`gen_please` picks the collection and resizes the pools.

**A major fires** when the rem set overflowed, when the major pool lacks room for a worst-case
promotion, or by the **amortization rule** — allocation since the last major exceeds
`major_live0 + 4 × minor-pool`. That last one periodically sweeps floating dead tenured objects
(which die in place, invisible to a minor) and lets the pool shrink. Majors stay the minority
(~⅓ of collections).

**The minor pool** resizes on a deterministic controller in WORDS, not wall clock: overhead =
copied/allocated, accumulated over a window, growing above 1/8 and shrinking below 1/32. Words
rather than a time ratio is what makes the schedule reproducible — identical collection count
and pool trajectory run to run, byte-identical with ASLR off — and stops a slow major feeding
back into the nursery.

**The major pool** grows and shrinks by a whole **step** (`ai_major0`), never doubling, so it
tracks the live set both ways instead of sticking at a high-water mark.

**Initial sizes are configurable** like the custom allocator: `ai_minor0` (~128 KB) and
`ai_major0` (~512 KB half). A small target picks small and accepts more collections; a host
picks roomy and boots in a few.

**`ai_budget`** (words; 0 = unbounded; a field, settable at runtime like `g->alloc`) caps the
whole footprint by **Appel's rule** — the nursery gets the free budget after the major pool. A
bounded frontend (the kernel, a microcontroller) sets it to its RAM, else two growing pools
exhaust the allocator. At `ai_budget = 2²¹` (~16 MB) total reservation settles at the budget
exactly.

All gen allocation routes through `g->alloc` (freestanding — no raw malloc), so the collector
activates wherever the frontend's allocator supplies the major pool: host, wasm, and the kernel
all run generational and pass the whole corpus.

## Verification

- **the model** — `doc/proto/gengc.l`, 19 asserts, is the spec the C must meet; 3b reproduces
  the bug a missing barrier *is*.
- **`proof/rocq/gc.v`** (axiom-free, `make test_gc`) lifts that heap/region/rem-set model to
  Rocq and proves `barrier_sound`: under `rem_complete` (every old→young edge remembered) the
  minor's young scan reaches every young object the mutator can reach, so **no live young
  object is lost**. The corollary `minor_loses_only_if_barrier_incomplete` is 3b's converse —
  a young object the minor drops *witnesses* an incomplete barrier. The `drain_*` theorems own
  the copy loop: termination, once-per-object, nothing lost, a true fixpoint.
- **`AI_GC_CHECK` / `make test_gcheck`** instance-checks that fixpoint on every minor: re-drive
  the whole scan, and every `gcp` must be an identity — if `major_hp` moves, the first pass lost
  a reachable object, and it traps at the collection that lost it.
- **`gen_audit`** (under `-DAI_STAT`) checks `rem_complete` empirically: a reachability DFS
  from the GC roots verifying every reachable old object's young fields are remembered.
  Dead/orphaned backings are never visited, so there are no false positives.
- **`AI_GC_STRESS` / `make test_gcstress`** is the mutator's side — see doc/verify.md.

A proof that love.c's *pointer code* (`gcp` bounds, the tagged words, the two-space copy)
REFINES this model is the larger separate effort — the place love's in-tree prover could
eventually earn its keep, the same caveat `love/glaze/README.md` flags for a verified glaze.

## Open

- **Tenure-on-birth.** Nothing is born old today. Born-old interned noms, casks/ports/toasts
  and maps would dissolve rather than handle some minor-GC interactions (the weak intern map's
  hand-written promotion, finalizers, the barrier's "old map" premise) — measure before adding.
- **AArch64 toasts.** A major needs no I-cache work, and a promoted toast's W^X arena finalizer
  follows it via `run_finalizers` at the major; confirm a born-old toast never enters the minor
  pool in the first place.
- **Inter-task.** Tasks share one pool and the remembered set is global. A per-task minor pool
  is tied to the inle/init direction.
- **Large objects.** A big array allocated young then promoted is copied twice (minor pool →
  major at its first minor). A size threshold for born-old large objects is the standard
  mitigation; measure before adding.
