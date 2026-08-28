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
  **LANDED 2026-08-27**: gen.l's lalloc replaces lpick's two raw-count walks
  in the SAME channel (the regen delivers, as before) — one hottest-first scan
  over pool + cs, candidates off the locals record, intervals from alive's spa
  (the tick-span leg, pre-laid at rung 3). The capacity win is SHARING: two
  names whose spans are disjoint ride one seat (sound because the kill rides
  alive's record — a span covers its defining statement, so disjoint hulls
  never coexist); lvm_mul_rep seats nine locals where the static pick managed
  four, one save/restore pair per seat however many names ride it. Census over
  the 86-TU dump: −226 static frame touches, −0.56% loop-weighted, 48 fns
  improve / 11 regress (all small). pdef stays byte-identical to the pre-arc
  hull; MOON_ABLATE=alloc IS the rung-3 compiler, byte-identical — the
  regression instrument. law.l pins the sharing shape (two call-crossing
  block locals, one pair; the setenv round-trip pins the knob's face) and
  eight goldens re-subjected (assignment order churned identities only).
  ⚠ THE MEASUREMENT THAT RESHAPED THE RUNG: ablating lhome on the rungs-1-3
  tree still costs +9.3% cycles / +9.5% insns — repack CANNOT recover the
  channel's value post-choice, because a regen home REMOVES its register from
  the staging pool (free fn-wide by construction) where a post-choice pass can
  only harvest what staging left idle. So the channel stays until rung 6; what
  retires is the pick. Splitting is likewise emission-grain (an env binding is
  fn-wide per name) and waits for the one-build.
  ⚠ THE RANK FINDING, measured and REFUSED: pure loop-weighted ranking priced
  WORSE (+0.6% weighted census) — it drains seats to deep-loop SHORT-LIVED
  locals (df_hlens's insertion-sort b, d, p) that repack's chains already
  serve post-choice, starving the long-lived crossing ones (nl, top, ii) only
  this channel can seat. lpick's raw count was accidentally right — raw
  touches correlate with long-livedness — and the accident is now the
  discipline: raw count primary, scalars before a pair on ties (mag_divmod: a
  d128 spending two seats must not win a tie), the loop weight breaks what
  remains, the seq makes it total. The exact version of "value of a home =
  weighted touches MINUS what repack recovers downstream" needs the one-build,
  where there is no downstream.
  The price (moon-ablate, 3 samples, same run): ablating the allocator back to
  lpick reads +0.5% cycles / -0.1% insns / .text byte-equal -- the rung holds
  the locals share and nominally buys half a point of cycles, inside the +-0.7%
  floor; the census meters carry the direction. The build row is flat (clean
  artifact builds 75.5/77.6 s against 75.4/78.3 s). test_slow green through
  the seed fixpoint -- after one false FIXPOINT NOT OK from a torn artifact
  (knob-flavored timing builds left in out/host; the compiler itself proved
  deterministic across processes and across the seed's own binary).
- **rung 5 — params join; the ride loop retires.** Arrivals are interval defs
  at entry; ride/shadow/pcs become allocation outcomes. The leaf lane's
  shrink-retry and its guards delete.
  **LANDED 2026-08-27, reshaped by its own measurement.** The pricing came
  first and falsified the premise: on the rung-4 tree, ablating the
  call-bearing param lanes reads −1.6% cycles at +2.1% insns — and the
  counters say it plainly (the ablated binary carries MORE branch and icache
  misses and still wins: the wrap pair's latency chain at every call costs
  more than the forwarded slot traffic it consolidates, now that the chains
  recover the call-free ranges). pcs alone prices +0.2% — nothing; rung 3's
  per-value cs seats already own the crossing case. So there was nothing left
  to unify INTO the allocator: rung 5 lands as rung 3's promised milestone
  arriving through the params — pp (the fn-wide wrapped homes) and pcs (the
  all-or-none seat grant) are KNOB-ONLY now (MOON_ABLATE=pcell restores them;
  the pdef/pdefcs/alloc worlds keep them for parity), and a param cell stays
  a CELL that repack places per value: caller-saved over call-free ranges, a
  cs seat where crossings compound, the frame exactly across calls. Priced:
  restoring the lanes costs +1.7% cycles / −1.9% insns / −2.0% .text; the
  census: loop-weighted frame traffic −10.2% (the retiring wraps sat IN
  loops — inf_run alone −18.5k weighted), static +1,886 and forms +2.2% (the
  returning slot traffic sits at depth 0–1); the change is BIMODAL (275 fns
  slightly up, 56 down big) — recorded for rung 6's exact placement. Build
  row −2–3%: a fn whose only homes were param lanes skips the regen. Both
  wrap pricings (the flat count and the path price) retire from the default
  path with their lane; alive stays as the interval provider. Laws: the
  pp/pcs faces pinned under the knob (setenv round-trips, x64 and a64), a
  default-face law (the entry spill precedes every ALU op, where the old
  lane computed on the arrivals and spilled at the call), and the leaf-lane
  laws untouched.
  STILL OPEN → rung 6: the ride loop (its blocker is pass-2 staging scratch —
  the shuttle pair is protocol, not allocatable, so propose-verify is forced
  until emission is virtual); ihset and the leaf-lane homes stand (call-free
  fns, no wraps — the lanes the measurement left standing); and the
  bimodality's other half, the df_hlens-shaped fns where a fn-wide home
  beats the chains on staging-busy long ranges.
- **rung 6 — one build; the regen dance retires.** Emission targets virtual
  registers for homable scalars from the start; slots only for escaped and
  aggregate objects; the allocator assigns everything. deopt, rgon, the
  tick-agreement law between alive and the regen — all delete. This is the
  structural payoff, and compile time should IMPROVE here (no per-fn rebuild),
  paying back the analysis cost of rungs 0–5.
  **BUILT 2026-08-27, NOT YET DEFAULT — MOON_ABLATE=uni opts in.** The
  machinery exists end to end and is the leaner equivalent of the letter
  above: not virtualized emission but TODAY'S REGEN BUILD run as the only
  build, fed by alive instead of ir1 (alive already carries liveness, spans,
  crossings, a hard/soft call split, per-name touch counts, and a predicted
  decl sequence — the interval leg grew a rank leg). ubuild/ulloc/upar in
  gen.l: one build per fn, seats from the AST, the bare rebuild only for a
  'bad or a drifted tick/decl guard (measured ZERO fallbacks over ev.c), and
  the knobs keep every old world byte-identical (pdef, alloc, pcell, obuild
  all verified against their instruments). It SELF-HOSTS — the artifact
  builds itself and bakes through the one build — and passes make test and
  the full gcc battery. The measured regen incidence it removes: 90 of 117
  fn compilations on love.c run the dance today (52 of 65 leaf regens have
  AST calls that splice away — pure AST leaf-prediction was refuted first).
  WHAT THE FLIP STILL NEEDS: the AST-side seat calibration does not yet hold
  rung 5's line — best configuration so far reads +5.4% loop-weighted census
  and about +6% corpus instructions (cycles inside the floor), and the misses
  concentrate in fns where the ir1-derived outcome came from a mechanism the
  AST cannot yet see. The paid lessons, each with its fix landed:
  (1) the VLA prep's pointer/size slots live in cgfn's entry frame state — a
  reset before the one build re-deals them (the first self-host segfaulted in
  bake_tail); the one build must NOT reset, and empty grants take the bare ()
  policy so no policy bar fires. (2) a TAIL call never crosses its own args —
  without the carve-out the VM's tail-forward fleet (lvm_ap and kin, the
  corpus's hottest code) lost every ride and the corpus read +27% insns.
  (3) softness is candidacy (inlok? already gates size and shape); the
  branch-splice-shape tightening broke lvm_cur while fixing gcp — the real
  split is FN-LEVEL (leaf-shaped: no hard call anywhere), and one cold hard
  call in an error arm still blunts it (lvm_cur's residual). (4) a fn-wide
  cs grant can STARVE repack's chain lane (inf_run: the param chain lost r14
  and paid 200 slot reloads); hard-crossing params now enter the scan as
  BLOCKERS whose won seats stay unwritten for the chains, and csprd lets
  chains ride already-paired cs regs over dead ranges. (5) the armed shadow
  is the soundness workhorse: every uni pool home wears a reserved,
  untouched-until-wrapped shadow slot, so a spliced call costs nothing and a
  surprise emission wraps correctly — classification is economics, never
  soundness. STILL OPEN: the rank residual (inf_run's deep-loop copy temps,
  lvm_sort, the leaf-shaped bluntness) — the honest instrument for the next
  session is per-symbol dynamic diffing (lvm_cur carried 36% of one gap in a
  single symbol; the static census and the corpus disagree systematically).
  Landed on parity + laws + make test + the gcc battery; test_slow is owed on
  the next quiet tree (a concurrent holo/link refactor rode the shared tree
  at land time and owns the fixpoint until it settles).

  **CALIBRATED 2026-08-27 (the same night): corpus insns −1.0%, cycles −0.4%
  (floor), .text equal — the one build now BEATS the dance on the corpus and
  reads −135 forms on ev.c.** Per-symbol dynamic diffing was the instrument
  that named every mechanism, four fixes: (a) a return's TAIL POSITION is
  what the sibcall pass makes it — the comma spine's last call, each ternary
  arm, a cast's operand (nhcr in alive's ret lane; lvm_cur went from 94
  forms with every param spilled to 75, half the whole dynamic gap in one
  fix). (b) int locals join the universe (lhomable, uni-gated as wide9 — the
  dance's slot walk always homed them; alive's homable-only universe blinded
  ulloc's pool arm to them). (c) ulloc's file keeps ONE grantable seat when
  the reserve of two would starve it — pool2 already lost the param homes,
  and lvm_ret was buying a cs frame in a 10-insn fn for want of r7 (it now
  emits 10 forms against the dance's 11). (d) touch counts key by NAME, so a
  shadowed name's sites conflate — ai_net's four `i`/`s` redeclarations each
  bought an exclusive cs seat off the conflated total; site-averaging over
  sqm restored slots (ai_net +44 → 0). TRIED AND REVERTED: a loop-weighted
  bar on the cs lane (lpick's nested-loop law — refused lvm_yield_sw's good
  seats, +31 forms; the dance's rpays 'bor has no such bar), and quad
  arrivals as cells for repack's chains (the chains do NOT promote them in
  the uni world — the fleet spilled, +194 forms; the phs vacate mov is the
  accepted residual, one entry mov in lvm_tapn/lvm_argcup that coalesce
  removes in lvm_ret but not there). THE FLIP: pays on the corpus and on
  build time (one build for 90/117 fns), regresses nowhere measured —
  proposal is to flip after test_slow clears on a quiet tree (owed for this
  commit AND the landing commit).

  **FLIPPED 2026-08-28: the one build is the x64 DEFAULT (uniw?'s law in
  gen.l); the dance retires behind MOON_ABLATE=obuild (byte-identical, like
  pcell/alloc/pdefcs/pdef before it), and the other ISAs keep the dance
  until rung 7 calibrates them.** Restoring the dance now costs +1.0% corpus
  insns, +0.4% cycles, +0.7% .text. The flip was NOT free — test_moon's laws
  caught three real mechanism gaps the corpus number had hidden, and the
  laws were held, not weakened: (a) INT-PARAM RIDES — the old leaf lane rode
  an int arrival with one entry cvt; upar grew the lane (read at least once,
  file kept two deep after the take, NEVER beside d128 material — the wd9
  flag; the one unguarded attempt reopened mag_mul's inner-loop stack cell
  at +1.07G insns, the exact failure the "mag_mul discipline" comment
  memorializes). (b) QUAD RIDES — a protocol-quiet body (pq9: no call, no
  div/mod, no variable shift, no d128 anywhere) rides r0-r3 arrivals; since
  staging owns the quads, ubuild re-reads the built forms (rdsp defs) and
  one demoted rebuild covers a surprise (the dance's soundness re-read at fn
  grain). (c) escape-strength promotion — a merely &-taken local now
  promotes whole (the address never leaves), so the si-lane laws needed
  subjects whose address DEPARTS through a call. Re-subjected law faces are
  equal or better everywhere (zf: both int params ride and j left its cs
  pair for the pool; yf: two restores where three stood; irB/ipf/ihf/f2
  register-resident). RESIDUALS, recorded not hidden: a fully-folded homed
  local leaves one dead li (no dead-home-def sweep yet); the arg-seat aim
  declines onto an armed home (ca pays two movs, cells still zero); the
  quad-vacate mov survives coalesce in lvm_tapn/lvm_argcup (tail-call fns
  are not protocol-quiet, so their quads vacate).
- **rung 7 — the other ISAs.** a64 next (its pool and sweeps differ; the a64
  sweep chain reads the chosen ir). Then the pure upside: rv64 and t32 have
  nhome=0 TODAY — locals in registers for the first time on riscv's t0..t3
  pool; thumb2's pool is empty so cs seats only. ccarch/ccriscv gate each.

  **OPENED 2026-08-28 (ee160604): MOON_ABLATE=uni opts a remaining ISA into
  the one build; a64 is calibrated and gate-green, NOT yet flipped.** The a64
  work forced the classifier to grow up, and every advance came from chasing
  a real face: (a) a soft site costs its callee's TRANSITIVE hard count
  (nhin, memoized, cycle-floored — lvm_add_string's spliced seq_cat brought
  four memcpys and the armed shadow wrapped four homes at nineteen calls);
  (b) a MUSTTAIL tail rides free where a plain soft tail splices (the
  contract owes the jump vs the inlret door — gcp's plain ret-ternary
  splices both copy fns, the VM fleet's ai_musttail sites never do);
  (c) the fn-level count is a PATH MAXIMUM (the pmax lesson at AST grain: an
  if takes its heavier arm, loop weight multiplies, goto bars) and
  leaf-SHAPED means at most two cold weighted crossings — one depth-0 call
  no longer costs a hot fn its homes (gxr, irB, hdf all compute on arrivals
  and wrap exactly at the call); (d) d128 material refuses every param
  verdict — the spilled params are what FREE the wide-pair park's registers
  (mag_divmod's cells doubled under pointer rides; with wd9 held, w2's inner
  mul went CELL-FREE for the first time, m128r on the park). x64 default:
  −1935 forms vs the dance, corpus insns −0.2% vs obuild (the optimistic
  pre-nhin config read −1.0% — its edge lived in gcp-class fns where
  homed-through-storm won dynamically because per-PATH crossings are rare;
  chasing that residual wants path frequency, not more static counts).
  a64 under the knob: −1391 forms vs the dance, zero genfails over 86 TUs,
  test_ccarm64 153/153, test_cts_arm64 211/220 (the corpus reference),
  moon-tar/gzip-arm64 build+run+roundtrip. arm64check.sh fails on BOTH
  worlds — the local cross-gcc predates musttail (environmental, recorded).
  THE A64 FLIP still wants dynamic pricing on silicon (pi.lan) or an explicit
  census+gates decision like the sweeps arc took; rv64/t32 untouched.

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
