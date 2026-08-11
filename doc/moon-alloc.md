# moon-alloc — the allocator arc

The plan for liveness-driven register allocation in mooncc, and the retirement schedule
for the five mechanisms it replaces. Written 2026-08-11, post slot-repack (6424d05a);
companions: doc/moon-regalloc.md (the catalog and rung ledger — read it first, its
physics price every rung here), doc/moon-diff.md (the three-compiler ledger),
doc/hom.md (the destination-die design phase II leans on). Anchors are into
crew/moon/gen.l; this file states the plan, the code states truth.

## why an arc, not a rung

gen.l carries five parallel mechanisms, each answering the same question — *may this
value sit in a register across this range?* — each with its own license and pricing:

1. **the operand pool** (opool) — statement-scoped staging, reset every statement.
2. **the vmap** — cross-statement write-through residency; flushes at labels and calls,
   the slot stays the source of truth, so a flush forgets rather than spills.
3. **homes and rides** — param seats under the 'shadow/ride policies (the pp pricing),
   int homing under canonical extension, unhome renaming after the fact.
4. **the cs pool** — callee-saved homes for depth>=2 loop locals, borrows, d128 pairs.
5. **the param grants** — pcs (pmin-gated, dirty-load priced via nrac) and swcs
   (shrink-wrap, pminp, dual epilogues).

Plus their supporting cast: the 17-pin rgreset roster, the regen dance (build twice
under policy, deopt, restore snapshots), the choice guard chain, and the recovery
peepholes cleaning slot traffic after the fact. The marginal rung got expensive —
shrink-wrap was the arc's heaviest machinery and landed one grant in love.c — and the
measured remainder (post-repack: ~65-70 KB live shuffle + ~23 KB uncoalesced copies +
~13 KB widening churn of the 162 KB gap vs gcc; dynamically the ~22% slot-mov bucket;
the vmsplice probe's store-then-reload op seam) is exactly what per-class licensing
cannot reach. The allocator is the one lever that deletes machinery instead of adding it.

## the stance

Not a sixth mechanism beside the five — one substrate under them, in two phases:

* **phase I: slots become intervals.** Allocation over slot objects on the final forms,
  POST-CHOICE — the seam repack proved safe (the rankers price param slots by build
  offset, so ir1 reaches them untouched; deopt restores ir1's slot map). Reuses the
  slot map, the backedge-widened windows, the bail discipline.
* **phase II: emission against vregs.** Build emits unbounded temps (the destination
  die's consumer-passing is the mouth this was designed for), linear scan with interval
  splitting assigns the register file, spill placement replaces write-through. Phase I's
  interval engine, richer input.

Every rung ships alone under "pays somewhere, regresses nowhere", and every rung that
changes per-invocation costs owes a corpus A/B before landing — the cold-path lesson
(the regalloc ledger, 2026-08-11) is standing law: static models approved grants the
corpus refuted, twice.

## phase I — slots become intervals (x64 only; arm rides the old path)

* **rung 1, the liveness kit.** Per-fn dataflow over final forms: register liveness off
  rdsp (the oracle cskeep already trusts), slot-granule liveness generalizing deadst's
  marks, blocks + the backedge spans repack computes. No emission change; it pays by
  upgrading deadst from never-read to dead-on-every-path and refining repack's windows
  to per-granule. Gate: moon battery + fixpoint + corpus exact-or-better. The arc's
  foundation and its cheapest falsification.
* **rung 2, slot promotion.** A slot object whose interval fits a register free across
  it (rdsp says no def; no call clobber for caller-saved; cs seats for call-crossing
  intervals, the save/restore pair priced like cssv) is promoted — stores become defs,
  loads become uses, the cell vanishes. stld's window logic made global. Ships by
  interval class: leaf-local first, then call-crossing on cs seats.
* **rung 3, retirement one.** Promotion now does generically what the param grants do
  specially: DELETE pcs, swcs, pmin, pminp, nrac, the dual-epilogue sibs flavor — param
  spill slots promote like any slot. First negative-LOC milestone; the A/B must show
  the granted fns hold their wins.
* **rung 4, coalescing.** Liveness-driven copy elision over final forms (a mov whose
  source dies — the +23 KB reg-reg bucket; the movslq churn dies with it, promoted
  values stay extended). dehusk's window heuristics shrink to the rename sandwich.

## phase II — emission against vregs

* **rung 5.** Build emits vregs (r0 becomes just another one). Linear scan assigns the
  pool + cs file; spill under actual pressure, placed, not written through. The regen
  dance collapses: one build, one assignment — no policy attempts, no deopt snapshots
  beyond bad-shape bail. Only after rung 3 proves the engine on the easier input.
* **rung 6, retirement two.** DELETE the vmap, the shadow/ride policies, unhome, the
  pricing walks, most of the rgreset roster. opool survives only as the register file's
  name; cskeep stays as armor; stage.l re-types the shorter chain.
* **rung 7, the arm lanes.** Retarget the assignment to arm64/thumb2/riscv, kdiff-gated
  per arch, then retire the x64-only guards.

## what stays

cskeep (the callee-saved contract at emission — exactly the armor a new allocator
wants), stage.l's typed pipeline, the destination-die lanes, repack's interval engine
(backedge-widened windows ARE live intervals, one representation change away), the
JOIN-meet lessons the vmap earned, and the recovery passes until each is subsumed.

## gates and risks, named up front

* **fixpoint demands determinism** — tablets iterate in insertion order; no
  address-keyed state anywhere in the allocator.
* **vmret holds the tail-jump law** — intervals live at a sibcall must die before the
  jump; the epilogue-anchored shapes enforce this, keep them anchored.
* **the law corpus is shape-anchored** (the repack rung's accidental preparation): laws
  pin which value rides and how many touches, never a seat.
* **test_libc + the torture differentials** guard the miscompile class a green corpus
  misses; memchr has earned its keep three times.
* **the vmsplice probe is the second client**: the op-boundary seam should close toward
  cc's 1.9x as rungs 2 and 5 land — a gauge the corpus cannot fake.

## expected close

Phase I: roughly half the measured 80-100 KB, love.c codegen 1.63x toward ~1.4x.
Phase II: the rest, toward ~1.2-1.3x — and the net-negative LOC, the five mechanisms
and their pricing walks out. Sequencing binds at two joints only: rung 1 before
everything; rung 5 only after rung 3.
