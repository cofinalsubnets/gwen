# moon-alloc — the allocator arc

The plan for liveness-driven register allocation in mooncc, and the retirement schedule
for the five mechanisms it replaces. Written 2026-08-11, post slot-repack (6424d05a);
companions: doc/moon-regalloc.md (the catalog and rung ledger — read it first, its
physics price every rung here), doc/moon-diff.md (the three-compiler ledger),
doc/hom.md (the destination-die design phase II leans on). Anchors are into
crew/moon/gen.l; this file states the plan, the code states truth.

⚠ **a landed rung is recorded THERE, not here** — doc/moon-regalloc.md's dated ledger
is the one place a rung's gauge, laws and payload live, and this file only marks which
rung climbed. Same for the mechanisms: its "register story" describes the five, this
file only maps them to the rung that retires each. Two homes for one fact is how a
retirement schedule goes stale.

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

⚠ **the 162 KB is the BOTH-EMIT codegen gap and nothing else** — not the binary ratio,
which the same day's byte levers (the dead-static sweep, nolibc's per-function split;
regalloc's lever 5) took from 1.99× to 1.702× without moving the corpus a digit. Those
removed unreachable bytes, which are not shared symbols.

**Step 0 ran, and it re-priced the arc** (doc/moon-diff.md's re-base fill, HEAD
2fc25890). The byte levers left the codegen row alone as expected — but repack did not:
both-emit **1.63× → 1.56×**, the gap **162 KB → 141,221 B** over 612 shared symbols. So
the buckets above are a LARGER share of a SMALLER whole, and the arc's target moved with
it. The second reading matters more: the static and dynamic ratios, which had converged
at 1.63×, came apart (static 1.56×, dynamic 1.625×) because repack is a size lever —
−4.2% .text against −0.25% dynamic insns. mooncc's remaining excess is now
proportionally hotter than its bytes, which is the strongest argument this page has.

And the shuffle bucket is no longer an estimate. **Frame-relative movs are 132,849 B of
mooncc's love.o against gcc's 27,328 — an excess of 105,521 B, ~75% of the whole codegen
gap** (rung 2's pricing has the three-lane table). The arc is aimed at the right thing
and aimed low; what the same measurement took away is the belief that its early rungs
can reach it.

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

⚠ **rung 1 is the exception and it was measured, not assumed** — its own consumers pay
736 bytes (the probe under rung 1 below), so it ships WITH rung 2 or not at all. A rung
that is infrastructure should be named as such up front; this page did not, and the
probe cost twenty minutes to find out.

## phase I — slots become intervals (x64 only; arm rides the old path)

* **step 0, the re-base.** CLIMBED 2026-08-11 — doc/moon-diff.md's re-base fill. The
  arc's base was three moves stale: both-emit **1.63× → 1.56×**, the gap 162 KB →
  **141,221 B** over 612 shared symbols, and repack (not the byte levers) is what moved
  it. The reading that outlives the number: the static and dynamic ratios, converged at
  1.63× the fill before, came APART (1.56× static, 1.625× dynamic) because repack trades
  −4.2% .text for −0.25% dynamic insns. mooncc's remaining excess is proportionally
  hotter than its bytes, which is this arc's case stated in two numbers.
* **rung 1, the liveness kit.** Not "build a liveness engine" — three approximations of
  liveness already sit in gen.l and do not talk to each other:

  | | has | lacks |
  |---|---|---|
  | `rdsp` (~3664) | the per-form transfer function (reads, defs, pure?), already trusted by cskeep, dehusk, cmpfuse | nothing — it is complete |
  | `deadst` (~4493) | 8-byte-granule marks, object-aware through the slot map | control flow: it asks "read ANYWHERE in the fn?" |
  | `repack` (~4556) | backedge-widened spans to a fixpoint, then textbook linear scan — sort by lo, expire actives, reuse a free list | branches: its span is a convex HULL over touches, not a live range |

  So the rung is: **build the CFG those three are missing and let them share one answer.**
  repack already IS the interval engine and its scan's `labt` already collects labels and
  backedges; what is absent is the block graph and a backward fixpoint. Blocks at labels
  and branch targets, liveness backward over rdsp's reads/defs for registers and over
  deadst's granules for slots, shipped behind exactly two consumers — deadst's drop test
  (never-read becomes dead-on-every-path) and repack's windows (hull becomes live range).
  NO emission change. Gate: moon battery + fixpoint byte-identical + corpus
  exact-or-better.

  ⚠ **THE REPACK CONSUMER IS FALSIFIED, 2026-08-11 — rung 1 cannot pay for itself.**
  The probe, before writing any of it: repack's own placement re-run on the UNWIDENED
  windows, which is the floor precise liveness could ever reach (real liveness sits
  strictly between the hull-widened windows and no widening at all). Over love.c's 263
  repacked fns —

  | | frame bytes |
  |---|---|
  | before repack | 23,664 |
  | repack today | **11,120** (−53.0%) |
  | no-widening floor | 10,384 (−6.6% further) |

  — the whole bracket is **736 bytes across 20 of 263 fns**, and precise liveness would
  capture less than that. The widening looked alarming from the window side (28.3% of
  1,166 objects widened, span mass inflated 1.31×) and costs almost nothing at the seat,
  because an inflated window only hurts where it BLOCKS A POOLING, and mostly it does
  not. The instrument checked out against the landed rung before its verdict was
  believed: median packed frame 40 B, exactly what repack's ledger entry reports.

  What this does NOT falsify: promotion. "Can these two cells share?" tolerates a hull;
  "may this value sit in a register across this range?" does not, and that is rung 2's
  question, not repack's. The correction to this page is the pay-for-itself claim, which
  was wrong: **rung 1 is pure infrastructure and must be priced together with rung 2.**
  Ship them as one rung or gate rung 1 on rung 2's prototype — do not land a liveness kit
  on the strength of its own numbers, because it does not have any. The deadst consumer
  is unmeasured and was never the case for the rung; repack's object map already
  harvested that lever's 11 KB.
* **rung 2, slot promotion.** A slot object whose interval fits a register free across
  it (rdsp says no def; no call clobber for caller-saved; cs seats for call-crossing
  intervals, the save/restore pair priced like cssv) is promoted — stores become defs,
  loads become uses, the cell vanishes. stld's window logic made global. Ships by
  interval class: leaf-local first, then call-crossing on cs seats.

  **PRICED 2026-08-11, and the price named a different first rung.** The prize is bigger
  than this page claimed and the reach is far smaller. Frame-relative movs in love.o,
  counted the same way in all three lanes:

  | | frame movs | bytes | of .text |
  |---|---|---|---|
  | mooncc | 21,964 | 132,849 | 38.3% |
  | gcc | 5,378 | 27,328 | 14.3% |
  | clang | 1,954 | 9,762 | 5.0% |

  The excess over gcc is **105,521 B — about 75% of the whole 141,221 B codegen gap**,
  against the ~65-70 KB the shuffle bucket estimated. But rung 2 as scoped rides repack's
  population, and that population holds almost none of it:

  | repack's verdict | frame movs | share | fns |
  |---|---|---|---|
  | processed | 3,874 | 17.6% | 140 |
  | **barred by the scan** | **18,065** | **82.2%** | 205 |
  | no slots | 25 | 0.1% | 8 |

  **What the bar actually is** (the relaxed-bar probe: the three bails made non-fatal so
  the analysis runs on the refused fns, the rewrite SUPPRESSED for them — love.o came out
  byte-identical to the clean build, which is what makes the census honest):

  | class | fns | objects | slot ld/st | promo call-free | promo call-crossing | frame movs | of all |
  |---|---|---|---|---|---|---|---|
  | processed | 263 | 1,166 | 4,181 | 1,643 | 2,160 | 3,874 | 17.6% |
  | **barred** | **215** | **3,716** | **16,063** | 5,546 | 9,456 | **16,941** | **77.1%** |
  | raw (asm) | 10 | — | — | — | — | 1,124 | 5.1% |

  Relaxing it exposes **3.2× the objects and 3.8× the slot traffic**, and those fns' frames
  would pack 62,832 → 23,440 B (−62.7%, better than the −53.0% repack gets today).

  ⚠ **the bar is `unclaimed`, not `loose`, and THE LANE IS THE spush CELL.** Traced end to
  end, every link measured:

  1. `spush`/`spop` (~1343) reserve a 16-byte cell with their OWN `sub sp sp 16` and store
     through sp. They never call `nslot`, so nothing enters `g 'slots` — by design, they
     are a dynamic push, not a frame slot.
  2. `spmerge` (~4475) folds that sub into the prologue's. **The frame grows 16; the map
     does not.** Measured: the final prologue K exceeds the `frame` build laid in 231 of
     640 fns — by exactly +16 (210 fns) or +32 (21 fns), never anything else.
  3. The cell now lives in the deepest 16/32 bytes and gets addressed off r4.
  4. repack's scan meets an r4 ld/st `obj9` cannot claim and refuses the whole fn.

  The proof of the chain is where the unclaimed touches sit: of 1,890, **1,682 (89%) are
  in the deepest 16 bytes and 208 (11%) in the next 16 — zero elsewhere, zero outside the
  frame.** Exactly the region the frame grew by, and nothing else.

  ⚠ and the map was never stale or short — that was a wrong reading twice over. `mapped +
  8 == g 'off` holds in **640 of 640** fns; `nslot` is the sole writer of both and they
  never diverge. The earlier "every barred fn is short" came from comparing against K,
  which by then included the merged cell, and from an align16 slip on the clean class.

  So the rung before rung 2 was not the liveness kit — it was **one cell joining the slot
  map**. LANDED 2026-08-11 (the regalloc ledger): one foldl in deadcell registers each
  converted depth, and love.o's .text goes 361,873 → **339,492 B (−22,381, −6.2%)** at
  flat insns — encoding, not work: disp8 replaces disp32 once repack can shrink those 215
  frames. All gates green, and the laws re-anchored off offsets onto `sof`/`nldrg`,
  verified in both worlds.

  What that leaves for rung 2 proper: the population is now the whole 478, so promotion
  finally faces the 105 KB prize rather than 18 KB of it. Rung 1 stays infrastructure and
  stays priced with rung 2.

  ⚠ and a correction worth keeping, because it nearly became the plan: an earlier pass of
  this probe blamed a bare `mov` on r4 and put 77% behind it. That instrument skipped
  `push/pop/label` but not the frame-link movs the real scan special-cases, so it counted
  1,083 `mov r4,sp` pairs the scan never bars on. The binary held 96 bare rbp movs against
  its 1,083 — the tell was there and was read as a finding instead of as a bug. Validate a
  probe against the thing it models, not only against its own totals.
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

Re-anchored on step 0's fill (both-emit 1.56×, 141,221 B over 612 symbols; dynamic
1.625× on the corpus). Phase I: roughly half the measured 80-100 KB, codegen 1.56x
toward ~1.35x. Phase II: the rest, toward ~1.15-1.25x — and the net-negative LOC, the
five mechanisms
and their pricing walks out. Sequencing binds at three joints: rung 1 before everything
and PRICED WITH rung 2 (its own consumers pay 736 B — the probe); rung 5 only after
rung 3.
