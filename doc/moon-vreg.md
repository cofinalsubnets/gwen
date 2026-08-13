# moon-vreg — rung 5, the emission rewrite

⚠ **Plan. Rungs 5.0 and 5.1a (the shadow) are CLIMBED (2026-08-12); 5.1b onward is not built.** Written out of moon-alloc
rung 5, after the arc refused two increments aimed at the same prize. Companions:
`doc/moon-alloc.md` (the arc and its rungs),
`doc/moon-regalloc.md` (the ledger — every number quoted here has an entry there),
`crew/moon/stage.l` (the pipeline's types, which this rung re-types).

## the one sentence

`cgexpr` stops naming physical registers. It mints a **vreg**, and one pass assigns the machine
file by interval. Everything the arc wants to delete is downstream of that single decision.

## what the flow is today

`cgfn` runs a two-build dance:

1. snapshot the accumulators (`n0`, `strs0`)
2. `sp1` — the spill plan, params onto frame slots
3. **`ir1 (build sp1)`** — the first build, registers assigned as it goes
4. snapshot ir1's accumulators, slot map, locals record
5. analyse ir1: `ride`, `hpool`, `hm`, `alive`, `auniv`, `blscan`
6. **the choice** — price `pp` (the shadow/ride homes), then `pcs` (the param cs grant)
7. if any policy applies, **`regen`** — build a SECOND time under it, `deopt` back to ir1 on `bad`
8. the post-choice chain: `deadlab` → `repack` → `coal` → `unframe` → `jr0` → `unhome` → `cskeep`

Register assignment is smeared across the whole of step 3: the operand pool (`ralloc`/`rfree`)
hands out physical registers during expression lowering, the vmap pins values to physical
registers across statements, homes/rides seat params, the cs pool serves loop locals, and the
param grants patch what is left. Five mechanisms, all answering *"may this value sit in a
register across this range?"*, none of them able to see the others' answers.

⚠ **`build` runs TWICE per function** and every whole-function analysis in the build tail is
therefore paid four times over. That is a compile-time lever this rung collects for free: one
build, one assignment.

## how far back it pulls — the measurement

The coupling was counted before the seam was chosen (`crew/moon/gen.l`, 8,012 lines):

| | count | |
|---|---|---|
| bare `r0` in the file | 938 | 511 of them in lines 1000–3499, the expression codegen |
| `mkv … 'r0` — a lane ANSWERS the accumulator | **133** | the real decision points |
| `tor0` — a value FORCED to the accumulator | **69** | the bridges |
| `(vreg v)` read generically | 106 | already vreg-ready, no edit |
| `(? wnt wnt 'r0)` — honours the destination hint | 11 of 42 `wnt` mentions | the hint exists and is mostly ignored |

**So the rewrite is ~200 decision points, not 938.** The value tuple `(ty reg forms)` already
carries the register, so a consumer reading `(vreg v)` does not care what it is; the 938 is
mostly the FORMS (`['li 'r0 n]`), which follow mechanically once the answer site mints. And
`wnt`, the destination hint, is already threaded through `cgexpr` — it is advisory today, and
this rung is largely the act of making it binding.

⚠ **the accumulator is a PROTOCOL, not just a default.** Some lanes assume their operand is
already in r0 (the immediate lane says so in its own header). Those are the subtle sites: the
fix is to read the operand's answered register rather than assume, which the tuple already
affords. Budget the risk here, not at the answer sites.

**The IR itself needs no change, and that is what makes this tractable.** Register names are
symbols; `rdsp`, the liveness kit and every peephole treat them as opaque noms. `crew/holo/x64.l`
maps a nom through a table at the very last step and scares `x64-crnum` on anything unknown — so
a vreg flows through the whole pipeline untouched and dies LOUDLY at the assembler if it was
never allocated. **"no vreg reaches holo" is a free correctness gate**, not one we have to build.

## how far back it does NOT pull

* **not to the IR.** It is fine. The premature part is naming machine registers in it, not its
  shape. SSA is not indicated and would cost the peephole suite.
* **not to expression lowering.** Lines 1000–3499 are mostly TYPE logic — struct layout, the pair
  and wide kits, complex, VLA, builtins, inline asm. Rewriting them to get at the register naming
  would duplicate thousands of lines that have nothing to do with allocation. A new backend file
  is the wrong shape for the same reason.
* **not to the ABI seams.** Calls, returns, the park, div/rem's rdx:rax, the pair/wide kits, sret,
  varargs and asm constraints keep physical names — as PRE-COLOURED vregs, the standard move. 33
  `argr` sites and a handful of `ip2`/`psc`/`pkr` sites are the whole surface.

## cross-platform, and why it is the DEFAULT here rather than a later rung

⚠ **the non-x64 lanes have essentially no allocator today, and that is the biggest single fact
about this rung.** `(cspool g)` answers `()` for arm64, thumb2 and riscv — no callee-saved pool
at all. `(nhome g)` is 0 for thumb2 and riscv, which is "spill every param" in one line. And
`repack`, `coal`, `unframe`, `cskeep` and `addrfold` each bail on `arm? g`. Those targets are
spill-everything backends. **An allocator that is parametric from the start hands three targets
their first one**, from a far lower base than x64's, where five mechanisms already fight over the
last few per cent.

Three things make parametric the cheap option rather than the expensive one:

* **the target-descriptor work is 19 references and three one-line rosters.** `csregs` (11 refs),
  `lvgp` (5), `lvret` (3), defined at `gen.l:4556-4571`. And the pattern is already everywhere
  else in the file — `(argr g)`, `(hregs g)`, `(nhome g)`, `(cspool g)` all answer per target.
  The liveness kit is the ONE place that hardcoded x64 instead of taking `g`.
* ⚠ **and that hardcoding has already cost a hang.** `coal` rode unguarded on `lvout`'s x64
  rosters for two rungs; on an lr/fp target it was answering a different machine's liveness, and
  it hung riscv's on-hart egg bake the first time something raised the stakes. A parametric kit
  removes the class, not the instance. **This is rung 5.0's real content.**
* **arm64 and riscv are THREE-ADDRESS.** `emit-alu`'s `d==a` fusion constraint — the thing that
  makes x64 coalescing delicate and that the copy-walk rung spent itself on — does not exist
  there. The allocator has strictly more freedom on those targets. x64 is the HARD one, so an
  x64-only design would be tuning against the worst case and then generalising from it.

**So: parametric by construction from 5.0, landed x64-first because that is where the gauges
are** — ccbench, ccsize, the corpus insn/cycle rows and the frame-mov census are all x64
instruments. That is a measurement constraint, not a design one, and the difference matters: the
arm lanes are then a roster and a gate rather than a retrofit.

⚠ **owed: an instrument for the other targets.** `test_ccarm64`/`test_ccriscv`/`test_kdiff` are
CORRECTNESS gates; nothing prices arm codegen the way `bench/ccbench.sh` prices x64. We cannot
claim an arm win without one, and "it must help, they had nothing" is exactly the reasoning this
arc has twice had to retract. Build the gauge before claiming the rung.

## what dies

opool (survives only as the register file's name), the vmap and its array leg, homes/rides and
the `pp` pricing, the cs pool, **`pcs` and `pmin`/`nrac`** (rung 3's refused half — this is the
rung that retires it), the regen dance with its deopt snapshots, `unhome`, most of the `rgreset`
roster, and the pricing walks. That is rung 6 in `doc/moon-alloc.md`; it is listed there and
should be collected HERE, rung by rung, so no rung ships without a mechanism coming out.

## the register models — rung 5.0's input

In gen.l's ABSTRACT names (holo maps them per target at encode). Verified against each backend's
own table, not inferred from the ABI documents.

| | x64 SysV | arm64 AAPCS64 | riscv64 LP64 | thumb2 AAPCS32 |
|---|---|---|---|---|
| args | r6 r5 r2 r1 r7 r8 | r0..r7 | r0..r7 | r0..r3 |
| return | r0 r1 / f0 f1 | r0 r1 / f0 f1 | r0 r1 / f0 f1 | r0 r1 |
| caller-saved (`lvgp`) | r0 r1 r2 r5 r6 r7 r8 r9 r10 | r0..r15 minus reserved | r8..r12 (t0-t4) + args | r0..r3, r12 |
| **callee-saved (`csregs`)** | r3 r4 r11 r12 r13 r14 | **r19..r28 → x19..x28** | **r13 r14 r15 → s2 s3 s1; r19..r26 → s4..s11** | **r4..r11** |
| frame base | r4 (rbp) | fp (x29) | fp (s0/x8) | fp (r11) |
| park (`pkr`) | r3 | r15 | r12 | r12 |

⚠ **the callee-saved column is the whole reason `(cspool g)` answers `()` off x64, and it is NOT
a holo limitation** — `crew/holo/arm64.l:38` already maps `('r19 >< 19) … ('r24 >< 24)`, and
`crew/holo/riscv.l:32` says in its own words that `r19..r26 -> s4..s11` is "the callee-saved bank
(arm64's r19..r28 shape) **for a future glaze**". The registers were mapped in anticipation of
this rung. gen.l simply never offered them, and the ledger has carried "arm64 x19+ cspool (empty
there today)" as a residue since.

⚠ **thumb2 is the one to check twice.** Its abstract file is the IDENTITY map onto hardware
r0..r12, so its callee-saved set r4..r11 OVERLAPS the general pool rather than sitting above it,
where x64/arm64/riscv keep theirs disjoint. A roster that assumes disjointness is a miscompile
there. ⚠ and `fp` is r11 on thumb2, inside that range.

## the rungs

Each ships alone under "pays somewhere, regresses nowhere", each names the mechanism it retires,
and each is revertible. ⚠ this arc has refused two increments already — a rung that cannot state
its mechanism is the refused shape wearing a new hat.

* **rung 5.0 — CLIMBED 2026-08-12** (f06a93ba + 79af7e42; the regalloc ledger carries the payload).
  The target descriptor + pre-colouring, no behaviour change on x64: `lvgp`, `lvret` and
  `csregs` stop being x64 constants and become `(x g)` answers like every other roster in the
  file; the liveness kit takes the machine it is on. Make every ABI-fixed register explicit as a
  constraint rather than a bare literal. **Ships byte-identical on x64 — that IS its gate** — and
  retires the x64-only guards on `coal` (and the class of bug that hung riscv) rather than adding
  another. ⚠ do this FIRST: every rung below stands on the liveness kit, and a kit that lies
  about the machine is the one failure mode this arc has already shipped.
* **rungs 5.1–5.3 COLLAPSE INTO ONE, priced 2026-08-12 — they are not separable.** The plan had
  the pool mint first (5.1), then the answer sites (5.2), then params and the vmap (5.3). One
  probe over love.c killed the split:

  | | |
  |---|---|
  | `ralloc` calls | 22,634 |
  | ...of them DRY, falling back to r0 | **2,618 — 10.4%** |
  | `tor0` bridges emitted (`mov r0, <pool>`) | **5,222** |
  | reg-reg movs surviving into the object | 9,674 |

  **5.1's entire headroom is the 10.4% dry rate** — the pool almost always has a register, so
  minting more of them changes little. The traffic is in the BRIDGES, and a bridge is the
  accumulator protocol, which is 5.2.
  ⚠ **and 5.1 cannot be isolated anyway: `vmpin` accepts only `pool0` members (`gen.l:186`), so a
  `ralloc`'d register is exactly what the vmap pins.** The pool and the vmap share one namespace
  BY CONSTRUCTION; minting a vreg in `ralloc` hands one to the vmap on the next statement. 5.1 and
  5.3 are the same rung wearing two numbers.
  ⚠ nor is there a cheap slice of 5.2: every `tor0` site takes an already-bound value, so making
  the hint binding means threading it back to whichever `cgexpr` produced it — the ~200-site
  refactor, not a bounded step.

  **So it ships as ONE rung: the vreg carrier, binding destinations, and the vmap together.**
  Which is the ledger's rung-1/rung-2 lesson arriving a second time — *"rung 1 is pure
  infrastructure and must be priced together with rung 2. Ship them as one rung or gate rung 1 on
  rung 2's prototype."* Same shape, same answer. It retires the vmap, homes/rides, `pp`, `pcs`,
  `pmin`, `nrac` and the regen dance, and collects the compile-time win when `build` stops running
  twice — a large rung, but the arc has now refused three increments that tried to be smaller
  than the thing they were changing.
### 5.1a — the SHADOW STEP, and it must come first — CLIMBED 2026-08-12

**Mint vregs, then map each one to exactly the register today's discipline would have given it.**
Byte-identical by construction. It changes nothing and proves everything: that a vreg survives
`build`, the peepholes and `holo`, and that every predicate which asks a question ABOUT a
register can still answer.

* **the surface is ~16 predicates and three pin tables**, not the 938 `r0`s. `wr?`, `rfree`,
  `ralloc`'s own armor, `vmpin`/`vapin`'s `pool0` tests (`gen.l:186`, `:257`), `alsafe?` (`:481`),
  the two hint tests (`:2122`, `:5891`), plus `vpin` (18 refs), `rpin` (19) and `csbor` (10).
  Each becomes "resolve the vreg, then ask" — one indirection, no policy.

**Built, and the scope above missed a whole class.** The predicates and pin tables landed as
written (`rp` resolves, every g-table holds physicals only, `vrfix` substitutes as build's
innermost tail link). What the byte gate then caught — a 10-byte `.text` diff in two functions of
love.c, run to ground with a per-site-tagged pool trace over instrumented images — was the
**machine-identity comparisons**:

* ⚠ **a hint honored by ACCIDENT is the discipline, not a coincidence to fix.** An UNHELD hint
  (no `rpin` hold — the decl init, `pk`, the fdd lanes) returns to the pool at a splice body's
  psreset, a later `ralloc` re-mints its physical, and the value comes back wearing the new mint.
  The old code compared physicals, so the accident counted — the honor test skipped the `rfree`,
  the register stayed out, and every downstream alloc in the statement shifted. Same-mint `id?`
  breaks exactly there. `rpeq?` (resolve both sides) is the door, at every hint-honor test AND
  the two-address alias tests (`id? rd rA` guarding the operand free in `immop` and the bin
  register lane ×6, `id? wnt rB`, sub's alias-dst dodge) — ~20 sites beyond the scoped list.
* ⚠ **`psafe?` reads the write set RESOLVED** — a freed mint's store still lands on its physical,
  and a `spare` borrow licensed past it reads garbage. The one form-scan that runs before `vrfix`.
* the `ezd` delivered-seat test (`kls`) resolves too: the pool overlaps the x64 arg file
  (r5 r6 r7 r8), so "the value already sits in its seat" can be an accident of the same kind.
* **the instrument that found them**: bake the two gen.l variants into images
  (`love -l <cat> -e '(bake ..)'`, ~40s), compile love.c with both, then drive `cc-parse` +
  `cgen-obj` through `wake image -e` for the IR of one function (seconds, vs minutes
  interpreted), and tag every `ralloc`/`rfree` call site with its line number for the pool
  trace. The corpus (141 files, test/cc + host) never diverged — only love.c, twice; a corpus
  sweep alone would have called this rung green while the discipline drifted.

Gate at the climb: `love.o` byte-identical on all four targets (x64/arm64/riscv64/thumb2),
141-file corpus byte-identical, `make test` + `test_moon` + `test_fixpoint` green.
* ⚠ **PLACEMENT IS FORCED, and it is not the post-choice chain.** The rewrite is the INNERMOST
  link of the build tail, applied to the assembled prologue+body+epilogue **before `sibcall`** —
  because `sibcall` matches epilogue shapes and the park (`pkr`), and on arm `soften` sits inside
  it doing register ARITHMETIC (`sf-slot` maps r0/r1/r2/r3 to slots, `nxr` answers r0→r1→r2).
  Those reason about the physical file and cannot meet a vreg. So vregs live INSIDE build only,
  and the assembled list — the whole function — is what the assignment sees. That is the property
  the rung wants anyway.
* **make it a stage die, not a convention.** `stage.l` already types the pipeline; give build's
  assembled list its own die (`ir-vreg`) and have the assignment advance it to `gst`. Then **"no
  vreg reaches holo" is a STATIC check** rather than a runtime `x64-crnum` scare — the type
  system catching a mis-ordered pass at the seam, which is exactly what that leg is for.
* **the gate is byte-identical `love.o`**, the same one rung 5.0 shipped under: not "the tests
  pass" but "the compiler did not change its mind". ⚠ compare `.text`/`.data`/`.rodata`
  separately — `.rodata` carries the git hash the build stamps, so whole-file `cmp` always fails.

### 5.1b — the rung proper: assignment, destinations, and the vmap, together

Only once the shadow holds. Intervals over the assembled list; assign from the target's file
(rung 5.0 made the rosters answer); **prefer the copy's own source so the mov drops** — that is
the mechanism `coal` approximates with a one-form lookbehind and the reason cs seats can pay here
and could not in `repack`. Destinations bind (`wnt` stops being advisory), which is what the
5,222 bridges are waiting for. Params arrive as pre-coloured vregs; a cross-statement value is
just a longer interval, and the vmap has nothing left to do.

**Retires:** the vmap and its array leg, homes/rides + `pp`, the cs pool, `pcs`/`pmin`/`nrac`,
the regen dance and its deopt snapshots, `unhome`, most of `rgreset`.

**The internal ladder (steps i–iii CLIMBED 2026-08-12; iv is the remaining rung).** The
substrate was already in the tree: `lvout` is a per-form backward liveness fixpoint over the
real CFG, and a mint is just a nom it tracks — so "intervals over the assembled list" is
liveness read at the vrfix seam, not a new analysis.

* **i — the checker (CLIMBED, instrument-only).** lvtx's blanket cases went mint-aware (a call
  touches the caller-saved PHYSICAL file; a mint crossing one must SHOW as crossing), `lvio`
  now answers live-in and live-out both, and an instrumented image resolved every live set
  through the token map over love.c + the corpus on all four targets. **The verdict: zero
  conflicts, zero entry-live mints, zero mint-mint token collisions — the shadow discipline is
  interval-sound, and every one of the 37,976 flags was a mint over its OWN token**: the
  vmap/rpin/hint machinery continuing a value's life under its bare physical name, by 5.1a's
  design. The pricing prototype then measured the coalescing prize: 1,195 movs droppable under
  full-liveness affinity (of 10,420 single-mint movs; 5,010 conflict-refused — the bridge is
  load-bearing PHYSICS when r0 is busy inside the value's range, so coalescing subsumes
  wnt-threading and the ~200-site refactor dissolves; 4,103 pinned by the mixed-name class),
  **and every single droppable mov had a straight-line span** — no label, branch or call
  inside. That measurement is what makes iii local.
* **ii — DISSOLVED by i's verdict.** The mixed-name flows are not a fixable catalog; they are
  the cross-statement mechanism itself. rassign instead refuses any move whose token is still
  read downstream (the release scan below), and the class converts wholesale in iv.
* **iii — rassign (CLIMBED as `rasg`, gen.l).** No fixpoint rides the build tail (a full lvio
  there measured +92% compile — the +78% ghost): the engine is mention-list scans licensed by
  the straight-line-span fact. ADOPTIONS only — `(mov %m X)` at %m's birth, the call-return
  copy `coal` can never reach (its def is a call; rdsp bars it) — under: the window rule (no
  control/clobber inside the span), tenancy (no bare-X mention but the pair's closing copies,
  no co-tenant mint span on X), the token release scan (a bare-token read past the span before
  a def/call refuses — the vmap continuation), and an X-release scan when %m redefines. Two
  refusal classes were paid for in the build and are load-bearing: **no argument register as a
  target** (the ride analysis prices ir1 as built — a mint moved onto an arrival drops rides
  and the regen takes worse lanes), and **no bridge direction** (moving a death-copy onto r0
  robs stld's forwarding through the staging cell — lvm_band bought back a whole spush pair).
  Yield on love.c: grew 0 / shrank 12 insns, .text −30 B x64 / −12 arm64 / −8 riscv; compile
  time inside bake noise after the candidate pre-filter. Thin by design — the engine is iv's,
  and iv is where its freedom arrives. Gates: test, test_moon (laws + 133-program battery),
  test_fixpoint, test_ccarm64, test_ccriscv, test_slow.
* **iv — the long intervals. iv-a CLIMBED 2026-08-12 (branch only, not merged): the vmap
  holds mints.** An entry carries the NOM THE FORMS USE — a mint where the value arrived
  minted, a physical where it was seeded or seated bare; every register question about an
  entry resolves through `rp`, and a MEET answers the physical (the arms may have re-pinned
  one name on different mints of one machine register, and the join needs the one nom both
  paths defined — the φ dodge). Byte-identical on love.c except three iii adoptions that now
  refuse over their honestly-longer spans (+6 insns, the enabling cost). Residency laws
  unchanged, fixpoint holds. ⚠ it STAYS on the branch: iv-b was to be its payer and iv-b is
  refused (below), so the +6 now waits on the interval allocator that retires the vmap.
* **iv-b — BUILT, MEASURED, REFUSED (2026-08-12). The design was vmcflush optimism + a
  `rseat` link; it was built whole, and the numbers killed it.** What shipped for the
  measurement: vmcflush kept its pool pins across a call and named them on the call's own
  marker form `(cross (rz off ty)..)`; a `rseat` link between body completion and assembly
  settled each one — a free callee-saved seat (re-pinning in `g 'vrt`, its save joining
  `cssv` and its load every epilogue) or the RELOAD REWRITE `ld rz r4 off` where the marker
  sat, priced by a release scan (`needs?`, rel?'s shape) and `pmin`. On love.c: **+545
  instructions, 56 functions worse and 2 better**, and the census says why —
  **1,088 crossings: 630 die unread, 454 want a reload, 4 found a seat.**
  * ⚠ **the lever aimed at the frame bucket and GREW it.** Of the +545, **+498 is frame
    traffic**: loads 9,485 → 9,706, stores 5,919 → 6,068. The reload half worked as designed
    — 454 eager reloads retired ~233 lazy ones — but netting +221 loads, because a lazy load
    only runs on the path that reads, and **630 of the 1,088 crossings are never read again**
    (their pin is pure cost). The +149 stores are the tell: a surviving pin holds one of the
    six pool registers past the call, and the squeeze spills. Optimism trades loads for
    pressure and pressure wins.
  * ⚠ **the flush cannot tell the 454 from the 630** — the read-count lives in the AST and
    the crossing is discovered in the IR, so the only signal that would price this decision
    is the one the site does not have. That is the shape of the refusal, not a missing rule.
  * ⚠ **a seat bought at the call is a copy, which is the refusal this whole rung exists to
    escape.** Seating a crossing mid-function costs one mov + one save + a reload per exit,
    against the k loads it retires — so it needs k ≳ 4, and only a LOOP-crossing value has
    that k. Loop crossings are already seated, by `csbor`/`lomig`. The straight-line
    population is exactly the one where the arithmetic fails, and no supply of seats fixes it
    (the 4-seat x64 file was not the binding constraint; the pricing was).
  * **so iv-b's real lesson is about iv, not about calls**: the vmap must RETIRE, not be
    extended. The physics only change when the value is BORN in the callee-saved register —
    the allocator colouring a whole interval, params pre-coloured on arrival — which is
    iv's own charter ("a cross-statement value is just a longer interval"). Any step that
    keeps the vmap and bolts seats onto its flush is re-deriving the same refusal the ledger
    already records twice.
  * one correctness lesson worth keeping, paid for with a miscompiled `love`: **a slot read
    out of `env` at a call inside an inline splice is the CALLEE's slot.** Scalar vmap names
    compare by content, a spliced parameter `n` shadows the pin's own `n`, and the reload
    took the argument's cell — `p0chars` then looped `n*3` times and hit its own `ud2`.
    `vmget` blinds itself on `g 'inlbody` for precisely this reason; anything reading a slot
    for a pin owes the same blind.

* **rung 5.4, spilling placed.** Today the slot is the source of truth and the register a
  write-through cache; invert it — the register is the truth, a spill is placed under real
  pressure. `repack` shrinks to packing actual spills; `stld`/`deadst` lose their write-through
  assumptions.
* **rung 5.5, the other targets LAND.** Not a port — 5.0 made the allocator parametric and every
  rung since has been written against a descriptor, so this rung is a roster per target, a gauge,
  and the gates: `test_kdiff` per arch (~45s each), `test_ccarm64`/`test_ccriscv`, `test_raw_arm64`
  /`test_raw_riscv` under qemu, `test_virt` on-hart. ⚠ it is also where the biggest number
  probably is, since these targets start from no allocator at all — which is exactly why it needs
  the instrument above and not an argument from first principles.

## iv proper — the interval allocator, and the census that priced it

iv-b's refusal is the whole argument for this shape: every mechanism the arc has tried buys
residency **at the call**, and by then the value already sits in a caller-saved register, so
the purchase is a copy. An interval allocator buys **at birth** — the value is born in the
callee-saved register and there is no copy to fold. That is the only place the physics differ.

**The substrate is already here.** 5.1a's mints + `vrfix` broke the "register known at emission
time" constraint, which was the hard part. `alive` is already an interval analysis: a backward
statement-grain walk over exactly the right universe, loops handled by seeding with everything
the loop reads (no fixpoint), computed once per fn — it just DISCARDS most of its answer, since
`rec` records a statement only when `ncl >= 1`. Recording every statement is one guard removed
from a walk that already computes the numbers. And write-through is a free spill fallback: an
interval that gets no register reads and writes its slot, which is today's code.

* **phase 1 — the named vreg.** A universe local gets a mint at its declaration; reads answer it
  with zero forms, writes define it, the slot store stays. Structural residency instead of a
  memo, so the whole loop-keep apparatus (`lokeep`/`lochk`/`lomig`/`loseed`/`lomt`, the edge
  verification and its retry bars) has no job: a mint spans a back edge by construction where a
  memo had to prove it survived. ⚠ the content-collision trap dies here too — a mint is bound at
  the declaration site, so a spliced callee's `n` gets its own and the miscompile above is
  unspellable.
* **phase 2 — assignment, the prize.** Endpoints for named vregs from alive's statement grain,
  mapped to form indices through the statement tick that already runs in lock-step (the
  numbering guard exists); temporaries keep today's per-statement pool, which is already correct
  and cheap. Linear scan, with call-crossing intervals eligible for the callee-saved file; a
  taken register joins `cssv` + the epilogue pin exactly as the regen's grant does, and `cskeep`
  verifies every exit. Unassigned → slot, i.e. today.
* **phase 3 — retirement, in dependency order.** vmap pins + array leg, then loop keeps + `saro`
  + `csbor`/`csbu`, then homes/rides/`pp`/`pcs` as params become pre-coloured intervals, and the
  regen dance LAST — it exists to price the mechanisms above it, and killing it is what returns
  the compile-time budget (roughly 200 mentions of vmap machinery and 180 of homes/rides/regen).
* **phase 4 — 5.4.** One mechanism instead of five, so write-through can invert: stores placed
  at real spill points and the def-store bucket dies.

⚠ **the compile-time law for this arc**: intervals come from `alive` (once per fn), never from a
form-grain fixpoint in the build tail, which is paid per regen attempt. The +78% that shipped
unwatched and the +92% measured at step i are the same trap twice.

**phase 1 step 1 CLIMBED 2026-08-13 — alive answers the span it already computed.** `rec` recorded
only call-bearing statements; `spn` now folds every statement's live set into a per-name `[lo hi]`
tick extent, answered as alive's sixth element. min/max, so a hole inside costs nothing — but the
extent must be a SUPERSET of the true live range, which is why the control-only lanes (`blk` `brk`
`cont` `goto` `lbl` `sdecl`, the nop default) record too: a `goto` reads the whole universe and a
break's set is its target's, and neither is bounded by the ticks that carry a def or a use. Gate:
`.text` and `.data` byte-identical on all four targets (only `.rodata`'s 6-byte `-dirty` stamp
moves), compile time flat (12.44s vs 12.45s, interleaved ×4 on love.c/x64).

**and the span census REFUSES the packing argument.** Two numbers off the new table, love.c:

| target | universe names | mean max-overlap | ≤4 | ≤7 | ≤10 | median span / fn length |
|---|---|---|---|---|---|---|
| x64 | 3,031 | 4.3 | 64% | 87% | 96% | 0.95 |
| arm64 | 3,050 | 4.3 | 64% | 87% | 96% | 0.95 |
| riscv64 | 1,321 | 1.7 | 87% | 95% | 98% | 0.56 |
| thumb2 | 1,275 | 1.7 | 87% | 96% | 98% | 0.56 |

(1) **The file is not the binding constraint.** On arm64's ten seats, 96% of functions could hold
their ENTIRE universe — every homable param and local at once. A linear-scan simulation that lets
disjoint spans share a register seats only +110 names of 3,050 (+4%) over one-register-per-name;
x64's four seats gain +147 of 3,031 (+7%), riscv +45, thumb2 +94. (2) **and the reason is that the
spans do not end**: the median universe name is live across 95% of its function on x64/arm64, p75
is the whole body. The physics is C's own shape — a param is live from entry by definition, and a
local is declared at the top of its scope — so at the statement grain this universe has no short
ranges to pack. riscv/thumb2 read shorter (median 0.56) only because `nhome` is 0 there and the
universe is locals-only.

⚠ so **phase 2's scan is not the prize on arm64**, and the census that ranked arm64 first ranked it
on supply the allocator turns out not to need. What keeps names out of the file there is `lpick`'s
ADMISSION rule — the `tc <= 2` floor, the `1 + nx9` per-invocation term A-1 added, the nested-loop
`ln` licence, and the zero-crossing bar on the pool lane — not capacity. The interval's useful
content for that rule is not `[lo hi]` but the weighted reads over it, which is a different number
than the one this step built. Price the admission gates before building the scan.

**phase 1 step 2 CLIMBED 2026-08-13 — the nested-loop licence retires; it was a proxy for a term
that now exists.** Tagging every rejection point and counting over love.c: **69% of names reach
neither candidate list** on arm64/riscv, all barred from the POOL lane by `crossing` (correct
physics — pool registers are caller-saved), so the cs lane is the only door. What shuts it:

| denied by | arm64 | share |
|---|---|---|
| `ln < 1` (the nested-loop licence) | 579 | 49% |
| `tc <= 1 + nx9` (A-1's per-invocation term) | 367 | 31% |
| `tc <= 2` | 158 | 13% |
| `lea` (address-taken) | 52 | 4% |

The licence was introduced as a PROXY for "the save/restore pair amortizes", when a tc-only gate
measured +0.2% insns and an any-loop gate turned loads +0.2%. Both predate A-1, which added the
real accounting. With the actual cost charged the proxy charges twice. Four variants against
today, in insns:

| variant | arm64 | riscv64 | x64 |
|---|---|---|---|
| b — drop the licence, keep `ln` as the rank | −1,206 (worst +92) | −944 (+162) | −966 |
| c — licence and rank on ANY-loop touches | −951 (+38) | −409 (+149) | −384 |
| **d — drop the licence, rank by `tc`** | **−1,185 (+92)** | **−1,109 (+136)** | **−1,509** |

**d ships.** Dropping the licence degenerates the RANK — most candidates then score `ln` = 0 and
the pick order is arbitrary — which is why d ranks on total slot touches and beats b on riscv64
and x64 both net and worst-case. −2.0% of love.c's x64 `.text`. ⚠ and the dynamic gate is the one
that mattered, since this rule's whole provenance is static counts lying: **271.87M vs 272.08M
instructions retired on the corpus, −206,000 against ±500 run-to-run noise.** Pays statically and
dynamically, regresses on neither.

⚠ **31 shape anchors in `law.l` broke, and none of them hid a regression** — checked, not assumed:
across all 172 law snippets the change is −17 insns, sp-loads unchanged, sp-stores −2, with two
snippets increasing at all. Per function the loops shorten because a counter or accumulator gains
a cs home it never had — `h3` 18 → 14 insns/iteration, `nrg` 21 → 19, `lo8` 13 → 10. Most anchors
were register renames (`s` and `i` swapping seats). Two were not: **`lo8`'s law asserted something
now false** ("a call AFTER the loop bars the homes, so the counters ride the vmap" — the cs lane
does not care about crossing, the callee preserves the seat), and `nrg`'s `(= 1 (ldsp nrgf 56))`
was still PASSING while counting a cs restore instead of the `x` slot it was written for, the
exact accident `law.l:263` warns about. Both rewritten rather than renamed.

**rung A-2 CLIMBED 2026-08-13 — thumb2's file opens, and the op census was short by one.**
Twelve ops modelled in `rdsp`: the arm32 carry family joins `flagops` (adcs/sbcs/ors set the
flags, adc/sbc READ the carry a preceding adds/subs left — none may be lifted), `(umull dl dh
a b)` is the first form in the table defining TWO registers, `(mla d a b acc)` reads three, and
`udivll`/`uremll` are the first NULLARY ones — the whole 64-step restoring shift-subtract IS
the form, owning the r0..r3 protocol quad while r4..r7 are pushed and popped inside it, so a
seat there survives. Plus a `cspool` row for t32 (r4..r10) and one for **v6m starting at r5**,
which keeps r4 as a bottom frame base planted after the sub. **−14,728 B on love.c/thumb2 (80
fns better, 55 worse, worst +256 in lvm_hush); x64, arm64 and riscv64 BYTE-IDENTICAL** — so
clz and the carry family, which those targets do emit, changed nothing there. lvm_aprod goes
4,634 → 4,201 insns and the byte delta is exactly 4× the insn delta, so every removed form was
a 32-bit wide one.

⚠ **the eleven-op census was wrong, and the method is the lesson.** It was enumerated over
love.c, which never converts a double to an unsigned — so `cvttsd2ui` never appeared, and only
test/thumb2/libd.c found it once the gate was live. Re-swept with `cskeep` printing instead of
scaring over 133 test/cc files plus the thumb corpus on all four targets: clean. **A census
over one program describes that program.**

And **`cskeep` stopped naming targets**: its gate was `arm? g && !(a64? g || rv? g)`, a list
that grew once per rung, and now reads `!(two? (cspool g))` — a target with no file has nothing
to breach. a8 bails for the true reason, t32/v6m are covered automatically, and the next
backend to get a file is covered before anyone writes it.

**phase 1 step 3 REFUSED 2026-08-13 — the loop keeps do not retire, and static codegen said
they did.** With the file real on every target and `lpick`'s admission widened, the obvious next
move was to check whether the homes had absorbed the keeps' job. `lonone` is the existing
keep-nothing knob, so the ablation is one line. **Every static instrument said delete it:**

| target | ablating the loop keeps |
|---|---|
| x64 | −300 insns (43 fns better, 42 worse; worst +310 B in `ana_v`) |
| arm64 | −275 insns (28 better, 14 worse; worst +10) |
| riscv64 | −391 insns (27 better, 15 worse; worst +9) |
| thumb2 | byte-identical — already inert there |

**The dynamic gate refused it by a factor of 600: 272,056,500 instructions retired against
271,872,500, +184,000.** (Baseline reproduced to ±400 before and after the experiment.)

⚠ **the physics, and it is the third firing of this arc's oldest trap.** The keeps remove LOADS
FROM LOOP BODIES. A load deleted from a loop running a thousand times is one byte of text and a
thousand instructions of execution — so text size is not merely a noisy proxy here, it is
STRUCTURALLY BLIND to the thing the mechanism does. The magnitude makes the point: +184,000 is
comparable to the −206,000 that step 2's entire admission fix bought. The keeps are worth about
as much as everything else landed the same day.

⚠ so **phase 3's ordering is wrong as written.** The retirement of the loop keeps cannot come
from deletion — delete-and-measure will look free on every static instrument and be wrong every
time. It has to come from SUBSUMPTION: a name that is loop-kept today must instead take a real
interval seat from the assignment, so the keep has no customer left. Until the assignment can
carry what they carry, the 51 lines are load-bearing and stay.

**and the customer census says what subsumption has to BE** (love.c/x64, 165 distinct kept
names): 59 (36%) are denied a home by the `1 + nx9` term, 49 (30%) by the `tc <= 2` floor, 47
(28%) WERE cs candidates and lost to capacity or rank, 10 (6%) never reached `lpick` — and
**zero are element pins**, so the array leg has no loop-keep customer at all here.

⚠ **the two residencies price differently, and that is why one cannot simply replace the
other.** A home is FUNCTION-scoped on a callee-saved register and costs a save/restore pair per
invocation. A keep is LOOP-scoped on a caller-saved pool register and costs nothing per
invocation — it only has to survive the back edge. So the 108 names the cs gates turn away are
turned away CORRECTLY: a whole-function seat really is a bad deal for a name with few total
touches, and the keep is the right instrument for it. They are not substitutes competing for
one job; they are two prices, and each class picks the cheaper.

So subsumption is not "make these names home". It is **the assignment learning to grant a POOL
register over a LOOP-scoped interval** — reproducing exactly what the keep buys, but decided
once from alive's data instead of by memo plus per-edge verification. What retires then is the
optimism apparatus (`lochk`/`lomiss`/`lobar` and the regen retry attempts, which is also where
the compile time is), not the residency itself. The remaining 47 are the other half — genuine
capacity pressure on a four-register file, which is the interval-sharing case the span census
priced at +147 names.

**and the miss census AIMS it (love.c/x64).** The optimism is right almost always: **81 misses
across 10 functions of ~640**, 34 distinct (fn, name). Two facts make the replacement tractable:

* **every miss is ABSENT-ENTIRELY**, never present-in-another-register (0 of 81). A pinned reg
  leaves the free list, so nothing can steal it — a pin only ever dies by a drop.
* **all 34 trace to a full `vmflush`** (a few also touched by the call or co-tenant paths), and
  the reason is one line: `vmcflush` DEGENERATES to a whole-map flush when a loop has a call but
  `csbor` is empty and `saro` is empty. So the miss condition is exactly *a call inside a loop
  whose keep holds names that neither a cs seat nor the roster covers* — and `lomig`/`loseed`
  already compute both sets. What is missing is that an ENCLOSING keep's names are never
  re-checked against an inner loop's call coverage.

⚠ so the up-front rule is **coverage, not optimism**: keep only what the seat set plus the
roster can carry across every call the loop's subtree reaches. That is statically decidable from
data the build already has, it cannot miss by construction, and `lochk`/`lomiss`/`lobar` and the
four regen retry attempts go with it. The residency — the thing worth +184,000 — is untouched.

⚠ **the array leg is NOT in the way and stays.** Its universe on love.c/x64 is exactly three
functions — `as_big`, `rng_seed_into`, `rng_step` — so it serves the rng/bignum lanes, and
**zero of the 165 loop-keep customers are element pins**. It neither blocks this rung nor
depends on it.

**The census (love.c, all four targets, 2026-08-12)** — demand is call-crossing names and their
loop-weighted reads; supply is the callee-saved file minus frame base, sp and the callr park:

| target | fns w/ crossings | crossing names | weighted reads on them | file | fns where all fit |
|---|---|---|---|---|---|
| x64 | 543 | 2,648 | 102,601 of 106,081 (97%) | 4 | 338 (62%) |
| arm64 | 544 | 2,654 | 104,189 of 108,107 (96%) | 10 | 505 (93%) |
| riscv64 | 301 | 1,095 | 66,688 of 69,504 (96%) | 11 | 286 (95%) |
| thumb2 | 299 | 1,074 | 64,784 of 66,952 (97%) | 7 | 263 (88%) |

Three readings. (1) **Nearly all residency traffic is call-crossing** — ~97% of loop-weighted
reads sit on names that cross at least one call, so this is not a niche class, it is the class.
(2) **The file is entirely idle**: the supply column is the same in EVERY function of a target,
because pass 1 never touches a callee-saved register and `cspool` is empty on every arm and
riscv target — the residency machinery has never offered a seat there at all. (3) **Demand is
identical on x64 and arm64 and the supply is 2.5×**, which decides the order: build phase 2
against **arm64 first**, where 93% of functions can hold every crossing name at once against
x64's 62%, and where nothing competes for the registers.

⚠ riscv64 and thumb2 read low only because `nhome` is 0 there — params are never homed, so their
universes are locals-only. Their demand is understated by exactly the parameters, and phase 3's
pre-coloured arrivals would be the first param residency those backends ever get.

**rung A-0 CLIMBED 2026-08-12 — the file becomes real on arm64.** The census's "build against
arm64" had a prerequisite it did not state: the file there was unusable. `cspool` answered ()
for every arm/riscv target, the a64 prologue never spliced `cssv`, and `cskeep` bailed on
`arm? g` — so the baseline really did carry **zero callee-saved operands**. Wiring it exposed
two latent bugs the absence had hidden: `sibjmp` read the `epi-a64` CONSTANT rather than the
passed-in `ejx`, so a tail call left the callee's seats dirty (18 fns, all caught by cskeep the
moment it could look), and `rdsp` did not model `adds`/`subs` — the arm overflow lane — so it
answered 'bar and every analysis declined those functions (now `flagops`: the aluops shape, but
never pure, since the flags feed the `set vs` behind it). **−941 insns / −3,758 B on
love.c/arm64, 28 fns better and 5 worse, x64 .text byte-identical, test_raw_arm64 green under
qemu.** ⚠ the three consumers of the file do not transfer their x64 pricing: lpick's overflow
is the prize (≈ −755 alone), param homes pay, and the loop borrow is a net loss on a64 (−187
alone, dragging both-on to −236 because `wb` denies the homes their seats) — so a64 does not
take the borrow yet, and that is a verdict on insns, not on a mechanism whose x64 win was
measured in wall clock. Residue: `vbin_fill` takes all ten seats for +166.

**rung A-1 CLIMBED 2026-08-13 — riscv joins, and the overflow learns its exits.** riscv64
needed only cspool + cskeep (its prologue rode A-0's splice, its sibjmp already read `g 'epi`)
and compiled clean — but landed at **−5 insns**, because `vbin_fill` gave back +232 of it. The
cause was the residue A-0 named: **`lpick`'s cs overflow had no per-invocation term**, so
`pick` drained whatever file it was offered — invisible at four seats, ruinous at ten. Giving
it `pcs`'s accounting (a seat costs one save plus one reload per exit, so touches must clear
`1 + nx`) moves arm64 to −798 (worst regression +166 → +119), riscv to **−179**, and x64 — not
gated, and this is the argument for the term — to **−43**. Measured alternatives that lost: an
`ln >= 2` escape readmits the whole pathological set, and a set-level cumulative test matches
the worst case at a slightly better net but costs more machinery. thumb2 stays off until eleven
ops are modelled in `rdsp` (the 64-bit pair lane plus `ors`/`clz`/`cvtui2sd`/`udivll`), which
is its own rung. ⚠ A-2 found it is TWELVE — see below.

⚠ phase 1 alone will likely be FLAT on codegen: it replaces a memo with a structure and keeps
write-through. Under the standing ship gate ("pays somewhere, regresses nowhere") flat does not
land, so phase 1 needs an explicit consolidation gate — flat-or-better codegen, no compile-time
regression, N mechanisms deleted — or it must be bundled with phase 2. Decide that BEFORE
building, not after; iv-b is what the other order looks like.

## why this rung and not another patch

The call-crossing class — **44.5% of the frame bucket**, the largest single lever — was built on
cs seats twice and refused twice. The reason is exact and is the argument for this rung:
`repack` runs post-build, where a store's source is ALREADY an assigned caller-saved register, so
a cs seat can only ever be a COPY of it, one-for-one plus a save and a reload per exit. Under
vreg emission the source IS the vreg the allocator seats — it can BE the callee-saved register
and no copy exists to fold. **The physics change here and nowhere else.**

The other census buckets say what this rung does NOT reach: `shape` (25.1%) is a touch-shape
question — sub-word and mixed-width access — and is an INDEPENDENT lever that needs none of this;
price it separately before spending a rewrite on its account. `noseat-plain` (13.6%) is honest
register pressure and IS this rung's other half, greedy-by-window losing to a real scan.

## traps, each already paid for once

* ⚠ **`unframe` reads the prologue BY POSITION** — `sv3` asks whether form 0 of the body is
  exactly `(st r4 -8 r3)`. Anything spliced there displaces it and kills the caller's rbx, as a
  segfault in `main` AFTER the corpus prints "tests pass".
* ⚠ **`lvuniv` answers `(keys t)` — a LIST.** `peep` on it reads nothing, so every register looks
  free. It cost one miscompile that double-saved r12–r15 over the existing homes.
* ⚠ **a liveness universe must cover the SEATS, not just the traffic** — the comment at
  `gen.l:4581` and the `ai_sleep` bug behind it. A new file of seats means a new universe.
* ⚠ **`emit-alu` is two-address**: `d==a` fuses, `d==b` on a non-commutative op scares
  `alias-dst`. The allocator must coalesce a def with its first source or it buys the mov back —
  which is exactly the composition the copy-walk rung measured.
* ⚠ **the build tail costs double.** Any whole-function fixpoint placed there is paid four times.
  It shipped once at +78% compile time with every gate green, because no gate watches it.
* ⚠ **a codegen rung owes a COMPILE-TIME A/B**, interleaved, not only a codegen one.
* ⚠ **the assignment is a whole-function analysis in the build tail, so it is paid TWICE per
  function** until the regen dance goes — and the build tail's double cost is what shipped a +78%
  compiler once already. Expect the shadow step to READ slow and do not tune it there; the rung
  that deletes the second build is the one that pays it back. Measure compile time at 5.1b, not
  at 5.1a.
* ⚠ **`soften` and `sibcall` do register ARITHMETIC** (`sf-slot`, `nxr`) and match epilogue
  shapes. They are the reason the rewrite cannot drift later in the tail. If a future pass wants
  to move it, that is the constraint to re-check first.
* ⚠ **pressure only rises when destinations BIND.** Today a dry pool falls back to r0 and the
  shuttle absorbs it (10.4% of `ralloc` calls). Binding destinations removes that release valve,
  so 5.1b is where spilling starts to matter — which is why 5.4 exists and why 5.1b should keep
  the fallback until 5.4 lands.
* ⚠ **`vmpin` accepts only `pool0` members** (`gen.l:186`). Any design that keeps the vmap while
  minting vregs has to answer that test, which is the structural reason 5.1–5.3 are one rung.

## gates

`cskeep` is the armor and stays — it verifies the callee-saved contract at emission and will
catch a mis-seated save. `crew/moon/stage.l` re-types the shorter chain (20 sigs today). The
laws in `crew/moon/law.l` are shape-anchored and will need re-anchoring per rung. `test_slow` is
the merge gate; `test_fixpoint` (byte-identical self-rebuild) and `vmret` are the headline
invariants; `test_kdiff` per arch for rung 5.5. ⚠ and the finite-carrier instrument
(`law.l`'s exhaustion, `test/uukindlaw.l`'s method) is what rung 5.2 should reach for rather than
a build-and-see: the carrier is small and the refusals are decidable.
