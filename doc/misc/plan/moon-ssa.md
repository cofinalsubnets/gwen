# the SSA backend — one allocator over exact ranges

Drafted 2026-08-27, chosen (revisable). The measurement that funds it is
doc/misc/moon-gauge.md "the SSA question, measured" (the oracle lives in
doc/misc/proto/ssagap/); the history it answers is doc/moon-regalloc.md.

## why this arc

The residency layer is five mechanisms — alive's wrapsets, lpick's pool homes,
ihset/ride's param seats, the cs grant, repack's hull promotion — each
approximating liveness and value identity at NAME-and-form grain, priced and
debugged separately, interacting. The graveyard is the argument: vmap priced
−2.9% and was deleted, unpriced param homes measured +3.4% the wrong way, parks
drained the pools they lived in, pcs priced −0.4% under another mechanism's veto
and +1.1% without it, and the promotion window is a convex hull widened over
every backedge, so a fifteen-form reload chain inside a big loop inherits the
whole loop's window. The oracle's census of what survives all five in shipped
forms: **1,954 full-word call-free cells (9.9k loop-weighted touches) still ride
the frame**, 254 narrow cells beside them, 1,327 call-crossing cells wearing
blanket wraps.

Each of the five answers a projection of the same abstract question — where is
this value live, and what is it — for one syntactic situation (a param, a
local, a call crossing, a promotable cell), and the graveyard is the cost of
solving projections instead of the question. So the arc's principle: solve the
abstract problem love's codegen is a special case of. Virtual registers, one
def per value, per-value live INTERVALS, one allocator. Exact information
instead of five approximations.
This arc is the BACKEND substrate only — SCCP/GVN/LICM over C semantics stay
out (measured ~1% of forms; the kernel rows are a separate decision, the coda).

**What "SSA" means here**: per-def value chains and interval liveness over the
neutral IR. Phis appear only where a value genuinely merges (a join with two
reaching defs both live out); on this IR most merges are slot-carried already
and the allocator's split/join handles them as interval seams, not as IR nodes.
If a rung can ship with intervals alone, it ships with intervals alone.

## the criterion

The regalloc arc's criterion governs (doc/moon-regalloc.md, START HERE): the
goal is that mooncc becomes a program with more of a sense of what it is doing;
a measurement is a FALSIFIER, not an authorization. This arc is that criterion's
purest case — exact ranges are the program knowing, where the five mechanisms
guessed — so a rung that replaces guessing with knowledge can be worth a small
measured regression, and no win licenses a regression it cannot explain.
Beneath that: every rung is measured same-run A/B at matched
budgets, priced in cycles through DIRECT drivers (ccnif's six rows, kore
sha256sum/sort/base64, the corpus row, the bake) with instructions as the
stable meter — the ±4% layout lottery is real (moon-gauge's attribution
section), so a cross-layout claim needs insns + hot-fn identity + frontend
counters. Compile time is a priced axis, not a free one: the build row
(ccbench) rides every rung's table. The whole residency layer prices at
roughly +16% cycles today (locals +6.3, cs +5.0, pool +4.0, params +1.0) —
each rung that replaces a mechanism must hold or beat that mechanism's share.
law.l goldens pin register identities and WILL churn; that is not breakage —
test_cts, test_libc, the cross batteries and the fixpoint are the behavioural
gates. Every rung's off-switch joins `mcid`'s cache key (the MOON_ABLATE law).

## the ladder

Strangler order: each rung lands as a post-choice pass beside the machinery it
replaces, prices against it, then the old mechanism retires. Nothing flips
until rung 6.

- **rung 0 — the value layer, in-tree and law-bound.** Port the oracle's core
  (CFG, escape census, per-def chains, interval liveness) from
  doc/misc/proto/ssagap/ssagap.py into love — crew/moon/val.l — over final
  forms. Laws pin it on hand shapes; the census re-reports the gauge numbers
  in-tree, DIFFERENTIALLY against the python oracle on the whole corpus (two
  implementations, one answer). No codegen change, no risk; this is the
  substrate every later rung reads. ⚠ determinism is a law here: the fixpoint's
  answer must not depend on tablet iteration order — the seed carries it.
  **LANDED 2026-08-28**: crew/moon/val.l rides the moon module (the bake, the
  law-lane cat); laws pin the hand shapes in law.l; `differ.sh` proved 8,200
  rows byte-identical to the python oracle over 80 TUs, and the in-tree census
  re-reports the gauge numbers exactly (1,954 / 1,327 / 254 / 2). The chains
  layer already refined the census: the cell across-flag is a LINEAR span,
  chain crossing is CFG liveness, and they disagree both ways — 527 call-free
  chains inside across=1 cells, 495 crossing chains inside "call-free" cells.
- **rung 1 — per-def promotion replaces the hull.** In repack's slot: a
  non-escaped cell's store-to-loads chain whose OWN interval is call-free takes
  a free seat over that interval — per-def ranges where the hull widened over
  every backedge. Must strictly subsume today's promotion (verify: every cell
  the hull takes, this takes). The pot, per rung 0's chains: **2,471
  call-free CHAINS** (1,944 in call-free cells + 527 the hull condemns), and
  the 495 crossing chains inside "call-free" cells must NOT promote — the
  linear flag lies both ways. dtb_to_kboot's three-reload pointer is the hand
  check. Hull promotion retires.
  **LANDED 2026-08-28**: repack's promotion is per-def chains (val.l's
  vreach/vgroup/vclive, window-bounded, over repack's own touch record); a
  chain's seat is checked over its live-range SET, so a call in the hull but
  outside the range no longer bars it, and a cell retires only when every
  touch promoted. Subsumption held BY CONSTRUCTION and doubles as the
  compile-time floor: cells the hull can serve promote the old way first
  (whole cell, span claim, no analysis), and only the hull's refusals pay
  for chains — single-def cells skip reaching entirely. `MOON_ABLATE=pdef`
  IS the hull, proven byte-identical to the pre-change compiler over all 80
  TUs. The yield: **−13.9% frame touches** (29,876 → 25,722), −2,210 forms,
  371 fns improved, 0 regressed, 107 frames shrunk (one grew 16 B by parity
  padding); the rung-0 census after: 6,640 rows where 8,200 stood. The
  price (moon-ablate base vs pdef + the counter protocol): **−2.5% corpus
  instructions, −3.4% .text, −1.1% cycles on the direct sha256sum row**;
  corpus cycles read +1.6/+0.1/−0.5 across interleaved samples — the layout
  lottery, not a delta (branch-misses favor per-def). Compile time: **+5.2%
  on the whole-src build row** (+6% on ev.c, the worst TU) — priced, carried,
  and rung 6's payback target.
- **rung 2 — narrow values ride.** The 254 narrow/si cells: intervals carry a
  width (rezx's clean-width lattice is the model), a narrow chain promotes with
  its extension discipline. Extends rung 1's promoter; the si-fed cells stop
  being a disqualifier.
  **LANDED 2026-08-28**: a cell whose every touch shares ONE width at its own
  base (st/ld/ldu/si at 1/2/4/8) rides the same chains. The discipline: a
  store canonicalizes the seat to the chain's first load's extension, a
  matching load is a plain mov, a mismatched one re-extends its dest, an si
  def folds the extension into its li AT COMPILE TIME — and a load whose dest
  IS the seat with a mismatched extension refuses the seat (it would wreck
  it). Full-word si-fed cells promote the same way (si → li). The yield on
  top of rung 1: −1,516 frame touches (total −19.0% vs pre-arc; narrow/si
  touches 4,674 → 3,710); census 6,434 rows. pdef still byte-identical (its
  eligibility is untouched: full-word st/ld, first-touch store). Build row
  +6.7% vs the hull (rung 2 adds ~1.5 points). The combined runtime price
  (moon-ablate, rungs 1+2 vs pdef): −2.4% corpus instructions, −4.1% .text,
  cycles inside the lottery band on BOTH sides now (+0.8 hull-slower here,
  −1.6 there). One fn wobbled +4 touches vs
  rung 1 on greedy seat order — still 18 under its hull count; rung 4's
  allocator is where seat contention gets solved properly, not patched here.
- **rung 3 — spill placement replaces the wraps.** A call-crossing value today
  pays a blanket st/ld around EVERY call in alive's statement-grain wrapset
  (goto reads the whole universe). With intervals: split at calls — register
  between calls, spilled exactly where live-across, cs seats per VALUE where
  crossing is hot. ⚠ the fn-wide all-or-none home law ("per-param verdicts were
  measured BOTH ways and lost") was a NAME-grain artifact — per-value verdicts
  are the sound version of the thing that measurement refused; do not half-adopt
  by keeping name grain anywhere in this rung. alive's livtab, the numbering
  guard, and both wrap pricings retire when this holds. The 1,327 spill-class
  cells are the territory.
  **THE CS HALF LANDED 2026-08-28**: a chain every caller-saved seat refuses
  at its calls takes a CALLEE-saved one — per VALUE, from the pool the fn
  never writes (r11–r14 minus the grant's), freeness taken-only (the caller's
  liveness phantom is met by the pair itself), never in a jmpr fn. repack
  emits the pair itself: one save after the r3 save into a fresh slot below
  the packed high water, one restore at the head of every teardown run —
  including SIB peels, and a fn's own entry label is a sib target too (the
  self-tail-call bug corrupted the caller's file through the re-entered
  saves; ev.c's analyze took the artifact down before the law pinned it).
  The gate is the pair's economics: one save+restore per INVOCATION against
  st+ld per CROSSING, so only compounding crossings repay — a call in the
  range at depth ≥ 2 (crossing weight ≥ 64), value-grain twin of the grant's
  own depth bar; at depth ≤ 1 the corpus priced the pair at +0.8% cycles and
  refused it. Priced flat (−0.1% cycles, +0.0% insns, corpus); fires in 19
  corpus fns (eqv_at's 44 loop wraps for 68 entry/exit pair ops the headline);
  −19.6% frame touches total vs pre-arc; pdef still byte-identical;
  MOON_ABLATE=pdefcs holds the lane shut for pricing.
  STILL OPEN in this rung: split-at-calls placement (caller-saved between
  calls, spilled exactly across) — that is interval splitting, rung 4's
  allocator; and the wraps of GRANT-homed values (a save-shaped cs mover
  never promotes). alive's machinery retires at rung 6, not here.
- **rung 4 — the allocator owns locals; lpick retires.** Interval allocation
  (linear scan with splitting) over pool + cs replaces static-touch picking
  (imsort, navl, rpays 'pool). Prices against locals homes' +6.3% share: hold
  or beat.
- **rung 5 — params join; the ride loop retires.** Arrivals are interval defs
  at entry; ride/shadow/pcs become allocation outcomes. The leaf lane's
  shrink-retry and its guards delete.
- **rung 6 — one build; the regen dance retires.** Emission targets virtual
  registers for homable scalars from the start; slots only for escaped and
  aggregate objects; the allocator assigns everything. deopt, rgon, the
  tick-agreement law between alive and the regen — all delete. This is the
  structural payoff, and compile time should IMPROVE here (no per-fn rebuild),
  paying back the analysis cost of rungs 0–5.
- **rung 7 — the other ISAs.** a64 next (its pool and sweeps differ; the a64
  sweep chain reads the chosen ir). Then the pure upside: rv64 and t32 have
  nhome=0 TODAY — locals in registers for the first time on riscv's t0..t3
  pool; thumb2's pool is empty so cs seats only. ccarch/ccriscv gate each.

## the standing constraints (read before building)

- the staging quad r0–r3 belongs to expression staging through rung 5; the
  allocator's file is pool + cs + unclaimed arrivals (alsafe?'s law).
- fesc, hasasm, vararg, sret bar exactly as today: an escaped cell stays a cell.
- musttail/sibcall: no cs save may span a sib peel; jmpr tails ride the
  epilogue laws (cskeep). The allocator inherits these, never relitigates them.
- a4ize renames r4→fp BY POSITION (the 5th-pointer-param lesson): intervals
  compute pre-a4ize or base-aware, never on spellings.
- stage.l types the pass chain; every new pass takes a stage die.
- the flat.l valve stays the answer for the array-kernel shapes (chacha's
  16-word state) until the coda is its own funded decision.

## refusals carried over

vmap (priced −2.9%, deleted — do not rebuild it as "the value table"); per-name
fn-wide policies past rung 3; any unpriced seat grant (the +3.4% lesson);
16-byte fn alignment (the parity law stands).

## the coda, explicitly out of scope

The mid-level on the same substrate — 64-bit knowns and the fold table
(cfoldir's measured gaps: 284 ALU folds in-domain, all 476 at 64-bit knowns),
then GVN-lite/LICM/SROA if the kernel rows (sha256 3.26×) are ever the target.
Each is a separate priced decision; none blocks this arc.
