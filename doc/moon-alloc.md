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
5. **the param grants** — pcs (pmin-gated, dirty-load priced via nrac). Its second flavor
   (swcs: shrink-wrap, pminp, dual epilogues) retired 2026-08-12, rung 3's first half.

Plus their supporting cast: the 17-pin rgreset roster, the regen dance (build twice
under policy, deopt, restore snapshots), the choice guard chain, and the recovery
peepholes cleaning slot traffic after the fact. The marginal rung got expensive —
shrink-wrap was the arc's heaviest machinery and landed one grant in love.c, which rung 3
later priced at 48 bytes and deleted — and the
measured remainder (**79,538 B of frame shuffle, 76.6% of the 103,839 B gap vs gcc** —
the estimate that bucket once carried was ~65-70 KB; plus the uncoalesced copies and the
widening churn; dynamically the ~22% slot-mov bucket; the vmsplice probe's
store-then-reload op seam) is exactly what per-class licensing cannot reach. The
allocator is the one lever that deletes machinery instead of adding it.

⚠ **the gap quoted here is the BOTH-EMIT codegen gap and nothing else** — not the binary
ratio, which the same day's byte levers (the dead-static sweep, nolibc's per-function
split; regalloc's lever 5) took from 1.99× to 1.591× without moving the corpus a digit.
Those removed unreachable bytes, which are not shared symbols.

**Step 0 has run four times and re-priced the arc each time** (doc/moon-diff.md, HEAD
2fc25890, a055d279, 86fc76ff, 5e6ba66c). The byte levers left the codegen row alone as expected;
repack, the spush cell, promotion and the two copy-folding rungs did not: both-emit **1.63× →
1.56× → 1.47× → 1.44× → 1.41×**, the gap **162 KB → 103,839 B** over 612 shared symbols. So the
buckets above are a LARGER share of a much SMALLER whole, and the arc's target has moved four
times under it.

The second reading matters more, and the second run of step 0 is what turned it from an
observation into a trend. The static and dynamic ratios had converged at 1.63×; they came
apart and then kept going:

| | 32b54ab8 | 2fc25890 | a055d279 | 86fc76ff | 5e6ba66c |
|---|---|---|---|---|---|
| static, both-emit codegen | 1.63× | 1.56× | 1.47× | 1.44× | **1.41×** |
| dynamic, corpus insns vs clang | 1.63× | 1.625× | 1.629× | 1.617× | **1.577×** |

Three fills of size levers left the executed stream untouched while `.text` walked 10% down;
**the first allocator rung moved it.** That is this arc's whole case, stated and then
demonstrated: mooncc's remaining excess is not spread over its bytes, it is concentrated in
the code that runs, and no size lever can reach it.

And the shuffle bucket is no longer an estimate. **Frame-relative movs are 106,866 B of
mooncc's love.o against gcc's 27,328 — an excess of 79,538 B, 76.6% of the whole codegen
gap** (rung 2's pricing has the three-lane table). Rungs 1+2 were the first aimed AT it and
brought the share 78.2% → 71.0%; the two copy-folding rungs after them left the traffic dead
flat (19,478 movs → 19,474) while shrinking the gap, so the share went back up to 76.6%. That
is the reading, not a setback: nothing that folds copies reaches this bucket, and phase II is
what it waits for.

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

* **step 0, the re-base.** CLIMBED FOUR TIMES (2026-08-11 twice, 2026-08-12 twice) —
  doc/moon-diff.md's trend table. both-emit **1.63× → 1.56× → 1.47× → 1.44× → 1.41×**, the gap
  162 KB → **103,839 B** over 612 shared symbols, and the dynamic row 1.63× → **1.577×**. The reading that outlives the numbers: the static
  and dynamic ratios converged at 1.63×, came APART under three size levers (1.47× static
  against 1.629× dynamic, the dynamic row unmoved), and closed again the moment an
  allocator rung landed. ⚠ **re-run step 0 after any rung that moves `.text`** — the first
  re-base was stale within four hours and the whole page was quoting it. ⚠ and each run
  pays the mcobj cold-cache tax, because a compiler change rehashes every member: the
  build row is quotable only warm.
* **rungs 1+2 CLIMBED 2026-08-12** (4dd9bc41, the regalloc ledger carries the payload) — shipped
  as one rung, which is what the falsification below asked for. What LANDED is the
  caller-saved interval class only: −2,486 frame movs, −6,315 B `.text`, and the arc's first
  dynamic movement (corpus insns −0.73%, cycles −0.93%). ⚠ **the call-crossing class on cs
  seats is REFUSED, 2026-08-12 — built, measured, and it cannot pay.** A cs seat can never
  equal the store's source (sources are caller-saved), so no mov ever drops and the rewrite is
  a frame touch turned into a reg-reg mov ONE FOR ONE plus the save/restore pair: +1,349 insns,
  exactly the pair count. The lvgp class paid precisely because its seat CAN be the source. No
  pricing gate repairs it, and even under coalescing it stays +1,077 insns for −1,479 B while
  retiring nothing. **So the ~80 KB of frame shuffle is not reachable by seating slot objects
  in callee-saved registers**, and this page's expectation that it was is corrected. What that
  bucket needs is emission against vregs — phase II — where a value is born in its seat rather
  than copied into one. Step 0 RAN against rungs 1+2
  (doc/moon-diff.md, HEAD 86fc76ff): codegen 1.47×→**1.44×**, gap **112,069 B**, and the
  dynamic row moved 1.629→**1.617×** with both natives flat — the arc's first.
* **rung 1, the liveness kit.** Not "build a liveness engine" — three approximations of
  liveness already sit in gen.l and do not talk to each other:

  | | has | lacks |
  |---|---|---|
  | `rdsp` (~3664) | the per-form transfer function (reads, defs, pure?), already trusted by cskeep, copyprop, cmpfuse | nothing — it is complete |
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
  counted the same way in all three lanes (pre-rung, and post-rung after the re-base):

  | | frame movs | bytes, pre | bytes, now | of .text |
  |---|---|---|---|---|
  | mooncc | 21,964 | 132,849 | **120,007** | 36.2% |
  | gcc | 5,378 | 27,328 | 27,328 | 14.3% |
  | clang | 1,954 | 9,762 | 9,762 | 5.0% |

  ⚠ **the mov COUNT is identical across the rung** — 21,964 before and after. The rung
  bought encoding (disp8 for disp32), not work, and this table is that claim from the
  emission side.

  The excess over gcc is now **92,679 B — 78.2% of the whole 118,531 B codegen gap**,
  against the ~65-70 KB the shuffle bucket estimated. The share went UP as the gap shrank.
  But rung 2 as scoped rides repack's population, and pre-rung that population held almost
  none of it:

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

  What that leaves for rung 2 proper — **measured 2026-08-11 after the re-base**, the four
  exits separated over love.c's 640 functions rather than inferred from the pre-rung class
  census:

  | repack's verdict | fns | frame-r4 touches | share |
  |---|---|---|---|
  | **packed** | **403** | **23,830** | **92.9%** |
  | barred by the scan | 10 | 991 | 3.9% |
  | analyzed, sub did not shrink | 84 | 658 | 2.6% |
  | refused early (no slots / no prologue) | 143 | 169 | 0.7% |

  **All ten bars are `raw`** — an inline-asm splice, which repack skips by design and
  always will. Zero `loose`, zero `unclaimed`: the 215-fn barred class is gone, not
  reduced. 487 of 640 functions carrying 95.5% of the frame traffic now reach the
  analysis, so promotion faces the 93 KB prize rather than 18 KB of it. Rung 1 stays
  infrastructure and stays priced with rung 2.

  ⚠ the two censuses count different things and their shares compare only in shape: this
  one counts frame-`r4` IR forms (25,648, `lea` included), the pre-rung one counted
  emitted movs. The *reason* column is what is exact, and it is the column that matters.

  ⚠ and a correction worth keeping, because it nearly became the plan: an earlier pass of
  this probe blamed a bare `mov` on r4 and put 77% behind it. That instrument skipped
  `push/pop/label` but not the frame-link movs the real scan special-cases, so it counted
  1,083 `mov r4,sp` pairs the scan never bars on. The binary held 96 bare rbp movs against
  its 1,083 — the tell was there and was read as a finding instead of as a bug. Validate a
  probe against the thing it models, not only against its own totals.
* **rung 3, retirement one. HALF CLIMBED 2026-08-12** (the regalloc ledger carries the payload) —
  the premise was that promotion does generically what the param grants do specially, so both
  could go. Ablation split them, and the split is the finding: **`swcs` and its cast are DELETED**
  (shrink-wrap, `pminp`, `cgitemx`, `swre`, the region split in `build`, four `g` slots, and the
  dual-epilogue flavor of `sibs` — two parameters threaded through six recursive calls and three
  locals recomputed per form, program-wide, for one function's benefit). It cost **48 bytes** of
  `.text` and nothing dynamic, and it is **gen.l 8,125 → 8,012 (−113)**, the first negative-LOC
  milestone.
  ⚠ **`pcs` is REFUSED and the arc should stop expecting it.** It ablates to **+1,004 B** (+0.31%)
  and **zero** corpus instructions — promotion does not recover those bytes, so rung 3's premise
  is simply false for this half. **The two differ by 42× per line** (0.42 B/line against 17.6),
  which a rung named for a mechanism CLASS could not have shown: price the members, not the class.
  The refusal lifts when promotion covers the bytes, not before. (An interleaved cycles read puts
  `pcs`-off 0.4-2.0% faster in four runs, but with instruction count flat that is layout, and the
  box was carrying two other sessions — recorded, not relied on.)
* **rung 4, coalescing. CLIMBED 2026-08-12, AHEAD OF RUNG 3** (the regalloc ledger carries
  the payload) — a copy whose source was defined by the form before it and dies at the copy
  folds into that def. Corpus insns **−1.76%**, `.text` **−1.83%**, +38 lines; the dynamic
  ratio 1.617→**1.589×**, the largest move on this arc. The ordering changed because the
  refusal below proved it: **nothing that turns memory traffic into copies pays until the
  copies can go**, so coalescing precedes every rung that creates them, not follows.
  **The forward half followed, 2026-08-12** — `dehusk`'s five hand-cut windows (a rename
  sandwich capped at 8 forms, a quiet back-copy capped at 6, an adjacent pair, an alu
  read-through behind a whitelist) are one `copyprop`: a forward walk carrying `reg ->
  source`, killed at each def and each control edge, with `lvout` answering the drop. The
  caps went with them, and the read-position whitelist went too — the renamable slots are
  PROBED off `rdsp` (substitute a stranger, ask whether it reads and does not write), so
  `la`'s symbol operand and every read-modify-write slot decline by construction instead of
  by a roster kept in step by hand. Corpus insns **−0.68%**, cycles **−0.49%**, `.text`
  **−0.67%**, static insns **−0.95%**, reg-reg movs **−6.8%**, compile time flat; +13 lines. The two
  directions COMPOSE and that is where most of it comes from: the forward rename breaks an
  `(add d d b)` fusion, which is exactly what lets `coal` fold the stranded copy back into
  the def — and the def then wears the fused form. `coal` learned the one alias it may
  welcome for this (an alu's FIRST source: `(mov d a; op d b)` is the lowering, so
  `(add r7 r7 8)` IS the fused shape; it is the SECOND source that reads its own wreck).
  ⚠ **this retires a mechanism, not lines** — five windows became two directions over one
  substrate and every cap is gone, but gen.l reads thirteen lines LONGER, and that is not what "the
  allocator deletes machinery" promised. (It also shipped once at 78% of the compiler's speed,
  which no gate noticed and step 0 did; the ledger has that.) The lines are in rung 3, and this rung says
  something about why: `copyprop` moves the param-grant pricing merely by shrinking the IR
  `build` returns (the ledger has the case), so those grants cannot be deleted until their
  pricing lives somewhere a later pass cannot perturb.
  **Step 0 RAN against both copy rungs** (doc/moon-diff.md, HEAD 5e6ba66c): codegen
  1.44×→**1.41×**, the gap 112,069→**103,839 B**, and the dynamic row 1.617→**1.577×** — four
  times rungs 1+2's dynamic move. ⚠ the natives were NOT flat this fill (−0.6 to −0.7%), so the
  ratio is the reading and the interleaved per-rung A/Bs are the attribution. And the frame-mov
  bucket is dead flat across both rungs, which is the honest half: **its share of the gap went
  71.0% → 76.6%** because the gap shrank around it. Nothing that folds copies reaches it.

## phase II — emission against vregs

* **rung 5.** Build emits vregs (r0 becomes just another one). Linear scan assigns the
  pool + cs file; spill under actual pressure, placed, not written through. The regen
  dance collapses: one build, one assignment — no policy attempts, no deopt snapshots
  beyond bad-shape bail. ⚠ its entry condition was "after rung 3 proves the engine on the
  easier input" and that was wrong: rung 3's remaining half is BLOCKED ON rung 5, not the
  reverse (the joint above). Rung 5 inherits `pcs` as a mechanism to retire.
  **The cs file is the first increment** — promotion's roster is caller-saved only, so an
  interval crossing a call cannot hold a register today, and that is both what `pcs` hand-serves
  for params and what leaves the frame-mov bucket untouched by three copy-folding rungs.
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

Re-anchored on step 0's second fill (both-emit 1.47×, 118,531 B over 612 symbols; dynamic
1.629× on the corpus). Phase I: roughly half the measured 93 KB of frame-shuffle, codegen
1.47× toward ~1.30×. Phase II: the rest, toward ~1.15× — and the net-negative LOC, the
five mechanisms and their pricing walks out.

⚠ **the static target is not the one to steer by any more.** Three fills of size levers
moved the static ratio 1.63 → 1.47 and the dynamic ratio not at all. Phase I is the first
rung that should move the DYNAMIC row, and if it lands another −10% `.text` at 1.629×
dynamic, it has not done what this arc exists to do. Quote the corpus A/B first.

Sequencing binds at four joints: rung 1 before everything and PRICED WITH rung 2 (its
own consumers pay 736 B — the probe); **rung 4 before rung 3, not after** — nothing that
turns memory traffic into copies pays until the copies can go, which is what refused the
cs-seat class.

⚠ **the fourth joint was backwards and is corrected 2026-08-12.** It read "rung 5 only after
rung 3". It is the other way for the half of rung 3 that remains: promotion seats from `lvgp`,
the CALLER-saved file, and no call may sit inside a range that takes one — so an interval
crossing a call is unreachable to it by construction, and that is exactly the class `pcs`
serves (`pmin` gates the grant on every path containing a call). `pcs` is retired by the rung
that can hand out CALLEE-saved seats, which is rung 5. The regalloc ledger carries the probe.

⚠ **and a gate this page states but did not apply to itself.** Rungs ship under "pays
somewhere, regresses nowhere", but that is the SHIPPING rule; the arc's own measure is
mechanism retired, and rung 1 is on record as infrastructure priced by what it enables. A
rung aimed at a byte bucket rather than at a named mechanism gets a duplicate rather than a
replacement — the cs-seat class was aimed at the 79,564 B of frame shuffle and arrived as a
second spelling of `pcs`, +109 lines, retiring nothing. Name the mechanism a rung retires
before building it, and gate on whether it comes out.
