# moon-vreg — rung 5, the emission rewrite

⚠ **Plan, not built.** Written 2026-08-12 out of moon-alloc rung 5, after the arc refused two
increments aimed at the same prize. Companions: `doc/moon-alloc.md` (the arc and its rungs),
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

## what dies

opool (survives only as the register file's name), the vmap and its array leg, homes/rides and
the `pp` pricing, the cs pool, **`pcs` and `pmin`/`nrac`** (rung 3's refused half — this is the
rung that retires it), the regen dance with its deopt snapshots, `unhome`, most of the `rgreset`
roster, and the pricing walks. That is rung 6 in `doc/moon-alloc.md`; it is listed there and
should be collected HERE, rung by rung, so no rung ships without a mechanism coming out.

## the rungs

Each ships alone under "pays somewhere, regresses nowhere", each names the mechanism it retires,
and each is revertible. ⚠ this arc has refused two increments already — a rung that cannot state
its mechanism is the refused shape wearing a new hat.

* **rung 5.0, pre-colouring — no behaviour change.** Make every ABI-fixed register explicit as a
  constraint rather than a bare literal. Ships byte-identical; that IS its gate.
* **rung 5.1, the pool mints.** `ralloc` hands out fresh vregs instead of `opool` members; a
  linear-scan pass maps them back. Everything else stands. The first real measurement, and the
  smallest thing that can carry the allocator.
* **rung 5.2, the answer sites mint.** The 133 `mkv … 'r0` lanes mint; `tor0` becomes a bridge to
  a constraint. `wnt` becomes binding. ⚠ the protocol sites (an operand assumed in r0) are the
  risk and want the exhaustion instrument, not a build-and-see.
* **rung 5.3, params and the vmap.** Params become vregs with an entry constraint; a cross-
  statement value is just a longer interval. **Retires the vmap, homes/rides, `pp`, `pcs`,
  `pmin`, `nrac`, and the regen dance.** The big negative-LOC rung, and the compile-time one
  (build stops running twice).
* **rung 5.4, spilling placed.** Today the slot is the source of truth and the register a
  write-through cache; invert it — the register is the truth, a spill is placed under real
  pressure. `repack` shrinks to packing actual spills; `stld`/`deadst` lose their write-through
  assumptions.
* **rung 5.5, the arm lanes.** arm64/thumb2/riscv take the same assignment with their own pools,
  kdiff-gated per arch; then the x64-only guards retire.

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

## gates

`cskeep` is the armor and stays — it verifies the callee-saved contract at emission and will
catch a mis-seated save. `crew/moon/stage.l` re-types the shorter chain (20 sigs today). The
laws in `crew/moon/law.l` are shape-anchored and will need re-anchoring per rung. `test_slow` is
the merge gate; `test_fixpoint` (byte-identical self-rebuild) and `vmret` are the headline
invariants; `test_kdiff` per arch for rung 5.5. ⚠ and the finite-carrier instrument
(`law.l`'s exhaustion, `test/uukindlaw.l`'s method) is what rung 5.2 should reach for rather than
a build-and-see: the carrier is small and the refusals are decidable.
