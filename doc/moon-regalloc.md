# moon-regalloc — the register story, and the catalog of the gap

How mooncc decides what lives in a register, what that costs against gcc/clang -O2, and
which levers remain — the **catalog** the differentials built, promoted from session
memory 2026-08-10. Companions: doc/moon-diff.md (the running three-compiler ledger),
doc/moon-c-gaps.md (conformance), doc/moon-diag.md (refusals), doc/hom.md (the
design the destination-die migration wears). Anchors below are into
crew/moon/gen.l; verify before leaning on one — this file states shape and physics, the
code states truth.

## the register story as it stands

The generator's baseline is an accumulator protocol: every expression computes into r0
and carries its type; locals ride the r4 frame. On top of that, residency, in layers —
each one priced, gated, and landed separately:

* **the operand pool** (`opool`, six caller-saved regs) — statement-scoped staging;
  `ralloc`/`rfree`, reset at every statement. A pool value never crosses a call.
* **the vmap** — the cross-statement leg: a universe local's value, once written through
  a pool register, maps name → reg so re-reads are zero forms. Write-through: the slot
  stays the single source of truth, so a flush forgets, never spills. Flushes at every
  label and call emission; if/?: join labels survive by pair-INTERSECTION over arriving
  edges (the forward-JOIN meet).
* **homes and rides** — a param's positional seat (`homefold`), the self-assign ride
  license (a param whose only arrival-defs are self-updates rides its arrival register
  end-to-end), int/uint locals and params admitted under canonical extension (`lhomable?`,
  every def re-narrows via cvt so the home always holds the extended value).
* **the cs pool** (IR r11-r14 → machine r12-r15) — callee-saved homes that ride through
  calls unwrapped, priced by the depth≥2/tc≥3 in-loop license; d128 locals take PAIR
  seats (`'regw`). The whole grant is minted at one door: `seats` answers `(lh ih lp cs)`.
* **splice binds** — cbinl/cginl alias a pinned/homed caller register into an inlinee
  (`alsafe?` the staging law), park or seat other args only when priced (`paid?`:
  loop-weighted reads ≥ 8; `deep?`: two pool seats remain after the take; regen attempts
  only — the `rgon` gate).
* **recovery passes** — addrfold (address arith into addressing modes, ldx/stx/si),
  deadcell (the spush-cell reservation sweep), stld/stldw (store-load residency),
  copyprop (the forward copy walk; coal is its backward twin),
  cmpfuse (`dieb?`, the branching death proof), unframe + jr0/unhome (frame elision and
  home→arrival renames), laylax (branch relaxation, in holo).
* **the armor** — cskeep (the callee-saved contract checked at emission, path-aware,
  dark ops refused), stage.l's typed pipeline sigs, the g-pin record (`fnreset`). A
  homing rung is gated by construction now; disassembly probes are for perf reads.

What there is NOT: liveness-driven global allocation. No live intervals, no coloring or
linear scan, no spill decisions — the frame slot is always live, which is why every def
still stores and why values die at joins the meet can't cross and at every call.

## the gap, measured

| date | vs | wall/cycles | insns | note |
|---|---|---|---|---|
| 2026-07-16 | clang | 1.84× | 3.6× | first systematic differential; IPC 3.5 vs 2.0 |
| 2026-07-17 | clang | 1.52× | 2.57× | six frame/cell levers in two days |
| 2026-07-18 | clang | ~1.32× | — | indexed loads, encoding shave, relaxation. ⚠ corpus+box changed after; not comparable forward |
| 2026-08-02 | gcc | 1.52×→1.34× | 2.78×→ | fresh 3-way re-base, then fleet-slots in one rung |
| 2026-08-03 | gcc | **1.30×** | 1.96× | int128 limbs, inlining honored, non-leaf slotting, cs pool, int homing; IPC 3.07 vs 2.17 |
| 2026-08-10 | clang | **1.18× cycles** | 1.71× | doc/moon-diff.md first fill; IPC 2.98 vs 2.05; shared-.text 1.75× |
| 2026-08-11 | clang | 1.24× cycles | 1.63× | the re-base fill (moon-alloc's step 0): static both-emit **1.56×**, 141,221 B — the static and dynamic ratios came APART, repack being a size lever |
| 2026-08-11 | clang | 1.21× cycles | **1.629×** | step 0 re-run after the spush rung: static both-emit **1.47×**, 118,531 B; binary 1.618×. The dynamic row is UNMOVED across three fills while static walked 1.63→1.47 — every landed lever has been a size lever |

The remaining excess is not idiom-shaped (that catalog closed 2026-08-02: cmp-$0, neg
dances, load-then-cmp all at parity). It is structural: slot traffic. At the fifth
differential ~22% of all static insns were rsp slot movs; only ~15% of reloads were
straight-line (peephole-reachable) — the rest cross branches and calls, which is exactly
the boundary the write-through vmap cannot cross.

## the splice client — what the gap costs a JIT

2026-08-11, `bench/vmsplice/`. A second client for lever 2, pricing it from a direction
the corpus differential cannot reach: a **template JIT over the tail-threaded VM** —
decode a thread into its named handlers (`def1` + `image_ap_index` already do this; the
image codec depends on it), splice their bodies into one function with the dispatch
deleted, compile, install through `nif`.

**It works.** A hand-composed body of 64 spliced op bodies, compiled by mooncc, lifted
out of the `.o` with holo's own `ld-read` and installed with `(from 'glaze 'nif)`,
agrees with its interp twin and runs. mooncc honors `ai_musttail` here, so the emitted
tail is exactly loopepi's shape (`st Sp[0]; ld Ip[2]; Ip += 16; jmp`) — the body drops
into the VM's convention with no shim.

**The tier is real and it is ~4×.** Same 64-op body, ⚠ `LOVE_NO_GLAZE=1` (without it the
"interpreted" twin is silently glaze-compiled and the baseline reads ~9× too fast):

| lane | ns/call (64 ops) | ns/op |
|---|---|---|
| interpreted thread | 248.5 | 3.88 |
| **mooncc-composed native** | **62.8** | **0.98** |
| glaze (auto.l's recognizer) | 26.3 | 0.41 |

So a splice JIT is a *universal* baseline tier and the glaze is a further ~2.4× on the
shapes it recognizes — they compose, they do not compete.

**And the gap is the whole distance between those last two rows.** The same probe source
through both compilers, on a chain walk where nothing folds:

| | dispatched | composed | |
|---|---|---|---|
| cc | 1.59 ns/op | 0.83 | **1.91×** |
| mooncc | 1.84 ns/op | 2.21 | **0.83× — slower than dispatch** |

Insn counts are close (11.1 vs 13.0 per op), so this is not density. It is the op-to-op
seam, one line of disassembly each:

```
cc:      mov %rsi,(%rcx)  /  mov %rsi,%rdx     ; store, and KEEP it in a register
mooncc:  mov %rax,(%rcx)  /  mov (%r11),%rax   ; store, then RELOAD the slot just written
```

Sixty-four store→load round trips on the dependent path. This is lever 2's ~22% bucket
seen from the client side: a splice's every op boundary is exactly the write-through the
allocator leg exists to remove, and closing it is worth ~2.5× **on the spliced body** —
about the distance to the glaze.

Two things the probe settled that the design had worried about:

* **`Have1` is free.** A safepoint in every spliced body cost nothing on either compiler
  (cc 0.825 vs 0.830, mooncc 2.188 vs 2.214 ns/op) — the heap-limit branch predicts.
* **splicing lets the optimizer see ACROSS ops.** cc folded the 64-bump arithmetic body
  to **30 bytes** — one `add` — against mooncc's 1299. Real extra value on top of
  dispatch elimination, and the reason the probe's first version read 60×: the whole
  body had folded into its own answer (see the README's traps).

⚠ what a real splice JIT owes beyond this: the lifted body was relocation-free only
because it touches nothing external. A body that touches chains or can collect pulls in
`lvm_chain`, `lvm_sym`, `ai_please` — references that must bind to the LIVE process's
addresses. `lift.l` refuses such a body rather than lifting one that would jump wherever
it happened to be mapped; binding them is a linking step, and holo's `ld-read` (in the
image since the linker half landed) is the tool for it.

## the physics — what prices a lever here

Learned by measuring, several times each; check a new lever against these before building:

* **This OoO desktop runs mooncc's code at IPC ~3.** An insn-count lever off the
  dependent path buys slack, not time: lea fusion, store-side addrfold copy-prop, and
  cmp-mem fusion all measured non-wins or regressions and were reverted. What pays:
  removed WORK on dependent chains (loads, frames, cells), fetch density with no
  critical-path cost (imm8 encoding, branch relaxation), and killed branches.
* **cmp-mem serializes load→cmp→branch** — the split form lets the OoO engine hoist the
  load. It's a density lever only; this is why real compilers keep them split too.
* **Alignment aliases the BTB.** align-16 measured −4% and was reverted; dense unaligned
  packing wins on this frontend. Corollary: **single-layout wall/cycle deltas under ~4%
  are unresolvable** — bench pad variants (an inert `ai_layout_pad` fn, sweep its size,
  median per side). Five landed rungs had their cycle read overturned by the pad probe.
  Even dyn-insn counts wiggle (±0.05% seen); measure the pad spread before invoking it.
* **The ship gate is "pays somewhere, regresses nowhere."** The host hides removed work
  as slack; mooncc's in-order targets (teensy, rp2040) cannot. A wall-neutral insn cut
  with clean semantics lands.

## the open levers, ranked

1. **Array slots** — a fixed-size local array with constant indices is a register file
   wearing a bad disguise; an element gets no vmap pin, so every touch is load+store.
   Measured 23× on chacha20 vs 1.77× on scalar poly1305 (bench/ccbench.sh carries both
   rows); love.c's z-tray elementwise lane 4.8×. Worth more than every landed rung
   combined on its shape. Watch: chacha and poly should close TOGETHER toward poly's
   ratio. FIRST RUNG LANDED 2026-08-10 (the vmap's array leg, the rung ledger below):
   element pins with write residency — chacha −19% wall, poly −10%, moving together as
   the diagnosis demanded. SECOND RUNG LANDED 2026-08-10 (frame-direct, the ledger):
   the clval fold — a constant index is a static slot, the lea/add/park dance is gone,
   deadst elides write-only element stores in call-free fns, and element fns stopped
   pinning fesc (sibcall/ride/homing open). The store-elision half CLOSED 2026-08-11
   from lever 2's side (the exit meet, and the element arm before it: element pins now
   ride calls on seats and their write-through stores sweep). What remains of this
   lever is float/pair elements riding the old walk (afd's whitelist is gp+ptr) —
   the ranking above is the shape's, not the residue's.
2. **Registers as the source of truth** — MEASURED 2026-08-11, twice. Frame-relative movs
   are **120,007 B** of mooncc's love.o against gcc's 27,328 and clang's 9,762, so the
   excess is **92,679 B, 78.2% of the 118,531 B codegen gap** — this lever is more than
   three quarters of what is left, and its share ROSE as the gap shrank. The ~22%
   static-insn figure below reads 28.5%. ⚠ the first reading of this lever said the arc's
   early rungs could not reach it — repack's scan barred 77.1% of that traffic on an
   UNCLAIMED touch, the lane being the **spush cell** (spush reserves 16 bytes with its own
   sub and never nslots; spmerge folds the sub into the prologue's, so the frame grows and
   the map does not). That rung landed the same day and **the bar is now empty**: 403 of
   640 fns pack, carrying 92.9% of the frame traffic, and every one of the ten remaining
   refusals is a raw inline-asm splice repack skips by design. Zero loose, zero unclaimed.
   doc/moon-alloc.md's rung 2 entry carries both censuses.
   The allocator leg proper: liveness over ir1,
   values surviving labels and calls, spill placement instead of write-through. Kills
   both the def-stores and the post-flush reloads (the ~22% bucket). The vmap, the JOIN
   meet, the cs pool and cskeep are its substrate; this is the step the peepholes cannot
   take (their reach measured exhausted 2026-08-04). THE ARC IS PLANNED: doc/moon-alloc.md
   — two phases (slots-become-intervals post-choice, then vreg emission), seven rungs,
   and the retirement schedule for the five parallel residency mechanisms. Its emission-side half — passing
   the CONSUMER down as a destination die instead of delivering everything to r0 — is
   modeled runnable in doc/proto/dest.l (ev.l's continuation-taking emitter shape worn
   by a register machine; gated by test_doc). All four lanes migrated 2026-08-10 —
   asn/decl, cbranch-compare, arg-seat, bin-value (the rungs below); what remains of
   the die rides the allocator leg (callish sides via cs-borrow parks, deeper arg seats).
   ⚠ this lever has a SECOND client with its own price: a splice JIT over the VM, where
   every op boundary is a write-through and the gap costs ~2.5× on the spliced body —
   "the splice client", above.
   FIRST BOUNDARY CROSSED 2026-08-10 (loop-head survival, the ledger): the map keeps
   across loop heads under optimism-with-verification, writes re-establish in place,
   and chacha20 dropped 74% of its wall — call-free hot loops now run register-resident.
   SECOND BOUNDARY CROSSED 2026-08-10 (the cs borrow, the ledger): a callish loop's
   scalar keeps migrate onto free callee-saved seats and ride THROUGH the calls —
   the call-loop shape drops ~20% wall at flat insns (the slot's store→load chain
   breaks). THE CALLISH BAR LIFTED 2026-08-10 (the aim hold, the ledger): the why was
   the free-list/pin split, and with it fixed a callish rhs aims and pins — `s +=
   f(..)` accumulators now ride seats through their own calls. SEAT EXHAUSTION
   ANSWERED 2026-08-10 (spill-around, the ledger): the surplus pins keep on their
   pool regs and reload per call, priced reads-vs-calls. KEEP DEPTH ANSWERED
   2026-08-11 (entry seeding, the ledger): a keep no longer only preserves — a
   loop's hot unpinned scalars pin by one seed load before the head, so inner
   loops rediscover what an outer keep dropped, and stld+deadst routinely erase
   the seed's memory touch entirely. SPLICE-CROSSING KEEPS ANSWERED 2026-08-11
   (the end-label meet, the ledger): a splice's end label is a forward join, not
   a clobber, so a loop keep now rides an inlined call -- straight-line, branchy
   and early-return inlinee bodies cost the pins nothing, and a callish inlinee's
   real call is spared by the same vmcflush the extern call gets. ELEMENT PINS
   ANSWERED 2026-08-11 (the element arm, the ledger): a constant-indexed element
   of a non-escaping frame array is a static slot, so lomig seats and rosters
   its pin exactly like a scalar's, and a written element re-establishes IN its
   seat. STORE ELISION ANSWERED 2026-08-11 (the exit meet, the ledger): the loop
   exit label is the THIRD join (if-joins, the splice end, now ld) -- kept pins
   ride out of the loop over the meet of the cond edge (kp-verified pairs only)
   and every break edge's snap, post-loop reads become zero forms, and with the
   slots load-free deadst sweeps the write-through stores and entry spills whole.
   What remains of the def-store bucket: the pre-call park pair and rostered
   pins' true reload stores (both priced, both earning their keep). PARAMS
   SEATED, pmin-GATED 2026-08-11 (the callish cs homes, the ledger): a priced
   callish fn whose EVERY path calls homes its params on callee-saved seats,
   wrap-free. SHRINK-WRAP BUILT AND SCOPED 2026-08-11 (the ledger): the wrap
   machinery landed (statement-level split, dual-epilogue sibs, pminp), and
   its A/B ladder settled the fleet question -- pp's per-call wraps are
   COLD-PATH wraps, dynamically right for the early-out profile; the static
   8 KB they represent is already-paid-for at runtime, and beating them needs
   a frequency signal (PGO), not a better static model.
   SHRINK-WRAP RETIRED 2026-08-12 (rung 3's first half, the ledger): it was worth
   48 bytes and one grant, against 113 lines and a per-form cost in `sibs`.
3. **Compare staging want-hints** — HELD for the allocator. Landed for the call-free
   side 2026-08-10: cbranch's left aims at its park before evaluating, so member loads
   deliver and the bridge mov dies. What remains is the callish side (the sp cell across
   a call — a cs-borrow park would need the wrap pricing), and doc/moon-alloc.md's rung 2
   subsumes it: a call-crossing interval on a cs seat is the same value by another name.
   Do not build it separately.
4. Recorded small residues: leaf sp-fn stldw coverage; a non-positional fallback home
   (mag_cmp's g1 loses its seat and stays slotted); cs-borrow park elision (params on
   cs regs landed pmin-gated 2026-08-11, the ledger; the fleet's share went with
   shrink-wrap, retired 2026-08-12 — it needs a frequency signal, not more machinery);
   arm64 x19+ cspool (empty there today); register-binding splice
   depth (map_probe's &-decline); d128 params ("a wide arg: not carried"); variable
   index±k rebase; narrow cmp fusion (632 sites, needs cc-aware licensing); the
   3-address dance emission (encoder territory — the reverted lea-fusion physics, only
   density-neutral shapes need apply).
5. **Bytes that are not codegen** — CLIMBED 2026-08-11, and named here so the headline
   ratio is never read as this catalog's score. Two moves, both in doc/moon-diff.md's
   fills: the dead-static sweep (mooncc laid the out-of-line body of every static it had
   already spliced — 149 bodies, 32,768 B; love's own unreachable text 6.4% → 0.6%,
   under gcc's own 2.6%) and nolibc's split to 185 per-function members (libc 64,110 →
   31,662 B, dead 55.1% → 5.4%, under musl's 6.5% floor — archive granularity, no
   linker gc). Whole binary 1.99× → 1.779×. ⚠ **the corpus did not move** (40.74 → 40.72 G):
   every byte was unreachable, so none of it is the gap levers 1-4 measure. The two
   ratios answer different questions and only the both-emit codegen one prices a rung
   here. Residue: 12 libc functions sharing a file with a live sibling, ~3 KB
   unreachable left in love's own C, and the inlining question the sweep re-opened —
   mooncc's remaining symbol surplus over gcc is now a real inlining difference rather
   than bookkeeping, and has never been sized.

## the laws a new rung must hold

* **unframe is correctness, not optimization** — the glaze ABI rides Ip in rbp, so a
  framed fn that calls into the VM faults. Any op that can base on r4 joins a4base AND
  uoff (and deadst's scans); a raw op bars unframe at every splice site (emit named ops).
* **A new op names itself everywhere**: holo's x64-emitters row + ir-arity row, its ISA
  family in gen.l's tables, and its cs effect (cskeep refuses a dark op).
* **alsafe?** — an alias lives only where staging never writes: x64 all-but-r0-r3;
  a64/t32/rv only pool0 ∪ cspool. The ride license covers defs, not reads.
* **vmap hygiene** — pins are born only from pool regs; no pin is born during call-arg
  staging (the argseal) or inside a splice body; any lane that DISCARDS a cgexpr
  emission must vmflush (cgexpr is not pure under pins).
* **rgon-gate any mid-compile register consumer** — an ir1-visible bind flips
  lpick/payoff rankings unpriced (a fn can lose its tail jump); regen attempts only.
* **pears stops at the first body label** — a park's per-iteration reset wears the
  entry-home mov shape; renaming past a label erases it (the strpbrk miscompile;
  test_libc was the only catch).
* **cu0 stays in assignment order** (cs save offsets ride it); d128 stays out of
  homable?/vmap (the pair rides `'regw` seats only).
* a64 raw splices scratch r2/r3/r9/r15 ONLY — r10-r14 are homes.

## how to measure honestly

* Build twins from the repo root (the seat walk; a scratchpad mooncc can't link), same
  TU set, ccache off. Bench under LOVE_NO_IMAGE=1 on both sides, corpus as a FILE, or
  bake both — never mixed (a fresh worktree's cold image once read as −23% insns).
* After a gen.l edit: `make out/host/mooncc.image` FIRST (make test does not rebake it),
  then rebuild the moon objects; a bake that crashes leaves a poisoned image — restore
  love from a known-good copy and bake by hand.
* Interleave slot-alternating rounds; byte-cmp the binaries before believing a delta on
  a semantic no-op (⚠ a bake is never byte-stable and the embedded love-version hash
  differs across HEADs — compare offset sets or the .o). Real text is `objdump -h`
  .text, never size(1).
* ⚠ **a microbenchmark whose ops are constant on their input measures folding, not the
  op.** bench/vmsplice's first version read 60× at 0.023 ns/op — under a cycle, the body
  gone. Two tells, both cheap: a sub-cycle per-op cost, and a `.text` far smaller than
  the source implies (30 bytes for 64 ops). Reach every operand through a `volatile`
  seed and real heap data, and read the emitted size before the timing.
* Probe the verdict chain before theorizing: the sed-probe on a gen.l copy + the mooncc
  cat answers in minutes what disassembly matrices can't; the framed-set diff (objdump
  push-rbp per symbol, comm vs a twin) finds frame regressions; dump pre-fold IR via the
  in-process cc-parse/cgen-obj recipe. A probe's INPUT gets validated before its verdict
  is believed.

## the rungs, dated (git log is the full story; these are the shas)

2026-07-16 · vmap+addrfold pair 3110edcd (the pair is the lever; each alone loses) ·
branch fusion 1ea516b3 · leaf-frame elision 4ea1ba3c.
2026-07-17 · reservation-orphan sweep bc6da67d (the arc's biggest: cycles −8.5%) · park
fold 7153eaad · live-park conversion a84f5976 · call-riding elision 3c20777e · store-park
eaa142b0 · float branch fusion c6af55ce · indexed loads f83b9d7c · dest≠base widening
d3b5eddd · tbr lane 9b2e0621 · jr0+unhome 81f74713 · rbp-free frames 2bb44f5d · encoding
shave 20105c14.
2026-07-18 · branch relaxation 97dcd369 (+ the wake-shim restructure 38e7f8ad).
2026-08-02 · fleet-slots e2d5989b (self −11.4%) · ride license b025327f · member-store
fold 38f51cff · cmp load-op fusion ae392a47 · negative disps e1a6bf99 · __int128 limbs
6ea9223d · always_inline honored ae6a611a.
2026-08-03 · non-leaf slotting 4e05fcc3 · cs pool e339995b (sort −41% cycles) · musttail
sweep 33360636 · slot-load elision c6dad445 · d128 pairs a5b5bb21 · leaf cs pool 26515d3b ·
canonical int homing 62c179f5 · d128 cs pairs 74707fce · shuttle skip af72212d.
2026-08-04 · JOIN meets + splice binds 4266830e 5aa36096 (+ the wrs? fix 3226863a) ·
mov-husk fold 6793a12b · alu read-through 3d11770c · rename sandwich d836b230 · priced
cginl park 591593fc (payload: the pears miscompile fix) · body-decl binds 1317b30e ·
priced int-param homes 33af3413 · cskeep b1ac6f03 · stage sigs ba5af92c · seat table
6c6cbcb9 · g-pin record bdc7e886.
2026-08-10 · the destination die's asn/decl lane: constant statement stores and slotted
inits deliver as store-immediates, a homed init lands in its home with no r0 bridge
(dyn insns −0.72% exact/disjoint, .text −0.69%; the payload: unhome's pre-entry rename
hole — H read before its (mov H A) pair carries an arrival — fixed and law-pinned;
test_libc's memchr differential was the catch, the third time that gate earned its keep).
2026-08-10 · the destination die's compare lane: cbranch's left pre-aims at its park
(pin the want before it evaluates; vpark recognizes the delivered value), so a member
compare loads straight into the pool register — lvm_eq's per-member bridge mov gone
(dyn insns −1.50% exact/disjoint 53.140G→52.344G, .text −4096B/−0.69%, cycles −1.9%
disjoint but under the layout floor; a call on either side bars the aim, both faces
law-pinned and falsified).
2026-08-10 · the destination die's arg-seat lane: the register group emits right-to-left,
so the FIRST cell arg evaluates last and nothing after it writes its seat — it aims
there and an honored load skips the whole push/pop cell (the frame drops too when the
staging was its only use). dies? learned the call door alongside (a call reads only the
argument file; a tail target is a function symbol, labels dot-interned), so the address
walk's r0 husks fold. Modest by design — unframe's adjacent st/ld→mov fold already
served the common shape — dyn insns −0.07% exact/disjoint 48.815G→48.781G, .text
size-neutral. dir only; a HOME seat keeps the cell (it spills after the staging evals,
and an ezm may read it); a zero-form delivery (riding home, vmap pin) keeps the cell
(its value predates the staging). All three guards law-pinned, two falsified by flip.
2026-08-10 · the destination die's bin value lane (the fourth, closing the emission
side): a LOAD-shaped left (dot/moor/deref) aims at its park before it evaluates, vpark
sees a delivered value, and the combine runs 3-address off the park — the bridge mov
gone. ONE aim per spine: the first build aimed at every level, drained the pool
top-down, and pass 1's wide roster handed out param homes — the wraps law caught it
(b spilled in a fn owed zero wraps); a var/num left re-aims free via cgleaf and a
nested-bin left already rides a pool register, so load shapes are the whole win
profile. A call on either side bars it; a constant right rides immop unchanged.
dyn insns −0.04% exact/disjoint 48.778G→48.760G, .text size-neutral. Law-pinned
(delivered park + 3-address combine + both bars), two flips falsified.
2026-08-10 · array slots, first rung (lever 1: the vmap's ARRAY LEG): a once-declared
1-D gp-scalar array whose every appearance is x[e] gets element pins under minted
content-compared keys ("x[3]"). READS pin (want-aimed load into a pool register, the
next read zero forms); WRITES take residency too (the rhs aims at a fresh pool
register, cvt IN PLACE — canonical like an int home's def — store from it, re-pin:
the read-after-write is zero forms) or, unhonored, drop; a variable-index write (or
post/rmw/fill) drops the whole array by key prefix. The ESCAPE gate is the soundness
anchor: any bare x is decay and excludes the array — so no pointer can alias an
eligible one and no pointer analysis exists; the non-retaining mem trio
(memcpy/memset/memcmp) is licensed by name (cc_block's fill is THE idiom; the call's
own vmflush retires every pin anyway). chacha −19% wall (5.40→4.38s), poly −10%
(1.81→1.63s), moving together per the gauge; corpus dyn insns a wash (love.c's hot
arrays are heap trays through pointers — ineligible by design); .text size-neutral.
Five law shapes (multi-read pins, write residency, unhonored drop, variable-index
drop-all, the escape gate), two flips falsified, and a six-shape torture C
differential vs clang. The catch worth keeping: the first probe's chacha number
barely moved — the state array was ESCAPED by its own memcpy fill, and the licence
list is what unlocked the motivating shape.
2026-08-10 · array slots, second rung (the FRAME-DIRECT element lvalue, the clval
fold lever 1 named): a constant-indexed frame-array element is a STATIC slot
(off + k*elsize), so afd answers the folded offset and three lanes stop computing
addresses — clval's deref answers ONE lean (consumed like a scalar's, so element
reads stop pinning fesc), the asn arm stores st r4 off' straight (no park, no
shuttle — even past a call on the right; the write residency rides unchanged), and
post gets the store-direct twin (direct ld/add/st). Escaping arrays fold too (the
address is the same address; no vuarr gate) — only pair/d128/float elements keep
the walk. Payload one: with no lea/lean left in such fns, deadst's whole-fn gate
OPENS and a write-only slot's store drops — the store-elision closure need arrives
free in the call-free case. Payload two: fesc unpoisoned unlocks leaf ride/sibcall/
homing for element fns — which surfaced that the regen was never licensed by the
leg itself (it rode nr>0 or fesc side effects): the array universe now licenses
its own regen (two? au on both gates; q0, the no-param law fn, is the witness —
without it the vmap never arms there). Corpus verdict, measured honestly: love.c +
host compile BYTE-IDENTICAL (the cipher stores were already collapsed by
pins+addrfold once the rhs went zero-form) — the pay is long/callish-rhs stores,
escaped arrays, no-param fns, the elision, and the structural unlock; regression
zero by construction. Laws: no-lea + elided-store + kept-store on f, q0's
self-licensed pins; three flips falsified; the torture differential grew five
shapes (callish rhs, escaped, narrow cvt, au license, ptr elements).
2026-08-10 · loop-head survival (the vmap's first boundary): at a loop head the pinned
map KEEPS, under OPTIMISM WITH VERIFICATION — every arriving edge (back jmp, continues,
the cont/cond label fall-throughs, breaks vs the enclosing keeps) checks map ⊇ keep; a
miss records (headlab nm) and the regen retries with it barred, so only a zero-miss
build ships. Writes re-establish IN the pinned register: the store hints aim at the
existing pin (honored free), unhonored int writes FORCE (store + one mov + re-pin;
dead if never re-read), and ++/-- steps in place (the kept post twin). A loop whose
subtree carries a call/asm/goto/label/switch/case keeps nothing (statically, zero
cost); a variable-index element write drops mid-loop and the check bars it. With
deadst, a call-free loop fn goes fully register-resident, frame gone (lp1/lp2/lo6).
chacha20 −24.5% dyn insns, −74% wall (4.74s→1.25s) — the QR state words and counter
ride pins across iterations; poly1305 flat as the gauge demands (already homed).
THE EXCAVATION (the keeps forced two latent bugs into the light): (1) vmdrop filtered
by id? while scalar map keys are parse-minted per-site strings — the drop was INERT
across statements always; harmless while every keep died at the next label, fatal once
maps survived loops (p0chars's strlen kept a counter whose ++ never retired the pin).
Now content-=, like the array leg always was. (2) the post twins read the raw map
ungated — an inlinee's i++ would find the CALLER's pin under the colliding name and
step the caller's register; now vmon?/argseal-gated. And ONE bar with an open why:
pinning a CALLISH write's result (a class the shipped compiler never pinned — always
unhonored+freed) miscompiles the context-threading shape (g = ai_push(g ..) in
love.c's analyzer); barred that day, EXCAVATED AND LIFTED 2026-08-10 (the aim hold,
below). Laws: lp1/lp2 (registerized loops), lp3 (call refusal),
lo6 (nested), lo7 (the bar engages — sharp against a disabled check), lo8 (force +
kept-post under a post-loop call); four flips falsified; five loop shapes in the
torture differential; fixpoint byte-identical, fuzz, kernel, full battery green.
2026-08-10 · the cs borrow (the vmap's second boundary — values across CALLS): a loop
whose subtree carries calls (and nothing darker: asm/goto/lbl/switch still refuse)
keeps its SCALAR pins by migrating them onto free callee-saved seats at loop entry
(`lomig`: one mov per pin, laid before the head label), and the call flush learns to
spare them (`vmcflush`: pool pins die, borrowed seats ride — the callee preserves
them). The grant is the regen's: a prescan (`blscan`) licenses the offer of every
seat the cs homes left, the attempt loop saves/restores them exactly like cs homes
(same cssv/epi forms, so cskeep accepts by construction), and a zero-miss build that
took fewer seats than offered rebuilds once on the used set — no idle saves ship.
Soundness is UNCHANGED machinery: the same lochk edges verify map ⊇ keep, a lane
that clobbers or full-flushes (splices do) just misses and bars — which is also why
an INLINED call still refuses the keep (lp3's law, reworded) while an extern one
keeps (lp4). Writes re-establish through `vmrepin` (the force movs into the seat);
the targeted-arg lane serves a kept name from its seat (`mov rT seat` for `ld` —
sound because a sibling's call spares cs and staging never allocates one). Gauge:
the two-TU call-loop shape −19.6% wall at FLAT insns — the win is the broken
store→load chain through the slot, not count; the corpus and spec.l flat; ql's
depth-1 counter now rides a borrowed seat with its slot stores swept. Laws: lp4
(seat-served arg, in-place step, cond on the seat, one post-loop slot read), ql's
borrow save/restore pair; three flips falsified (no spare, no grant, no arg ride).
The callish-write pin bar stayed one more rung (its why then still open); s in
`s += f(i)` refused by the bar — until the aim hold (the next entry) lifted it. Ten call-crossing
torture shapes (fn-pointer calls, nested, break/continue, global-writing callee,
address-taken, seat pressure, goto refusal) agree with gcc; fixpoint byte-identical,
fuzz, kernel, full battery green.
2026-08-10 · the aim hold (the callish why, found — the bar lifts): the ana_d
miscompile was never about pinning call results — it was THE FREE-LIST/PIN SPLIT.
'pool and 'vpin agree only at psreset boundaries; the callish aim opened a window
between them: hint = the existing pin's register, the rhs's call flush unpins it
(vmcflush), a SPLICE body's own statements then psreset with vpin empty — handing
the aim register to the free pool — and the force re-pins it with 'pool never
re-filtered. A statement boundary heals the split (the next psreset re-excludes
vpin), which is why plain statements never reproduced: the reachable shape was
ana_d's COMMA CHAINS, where leg N+1's ralloc double-books the register leg N still
pins — the spliced ai_ok's AND landed ON g's pin (`and r7 r7 7`) and pop1 walked
g&7. Twenty lines reproduce it; the fix is three-fold and principled: (1) the aim
HOLD — the hint rides 'rpin across the rhs, the splice binds' own discipline, so no
reset can free it and no ralloc can take it (load-bearing: eviction alone cannot
stop a mid-rhs claimant living past the force); (2) the pin doors evict their
register from 'pool outright — "a pinned reg is never on the free list" was the
comment's claim, now it is the code's, continuously; (3) ralloc SCARES on handing
out a vpin/rpin member — the whole class is loud forever. Both bars lifted (scalar
+ element): callish writes aim and pin, `s += ext(a[i]) + n` keeps its accumulator
on a borrowed seat THROUGH the call (zpf's new law), lp4's s re-establishes in its
seat with even the pre-call park reading it. Laws: the aim-hold shape (the spliced
AND must not land on the pin, pop reads through the LIVE pin) — the plain lift
reds it; calltort t11 (the ana_d shape, runnable); corpus flat, all gates green
(battery, fixpoint byte-identical, fuzz, kernel). The excavation cost one more
taint lesson: `make out/host/mooncc.image` relinks love through the CURRENT gen.l,
so the "reproducer" first crashed in the instrument, not the subject — hand-bake
on a saved-healthy binary before trusting any compiler-under-test.
2026-08-10 · spill-around (the keep past seat exhaustion): when a callish loop holds
more scalar pins than the borrow grant has seats, the surplus no longer drops — a pin
still worth its reloads stays on its POOL register, rostered (g 'saro), and every call
in the loop reloads it from its slot (`vmcflush` keeps the pin and ANSWERS the reload
forms; the call lanes lay them after the clean, beside the home relods). Write-through
makes the spill half free — the slot already holds the value at every call — so the
whole cost is one ld per call per rostered pin, and the license prices exactly that:
reads in the loop subtree >= its calls (`nreads`, an asn's own lhs excluded, vs `ncls`,
a nested loop's calls ×8), so a written-only pin or one read less often than the loop
calls refuses and no dead reload ever ships. Soundness is again UNCHANGED machinery:
the reload re-establishes the exact pair the keep claims, lochk verifies the same
edges, a splice's full flush still misses and bars. The roster nests (saved/restored
per loop, enclosing entries ride inner keeps as-is) and dies with its loop. Two
knock-ons: `skiprel` learned to peel roster reloads after a ret-position call
(pool-reg lds only — the first draft peeled cspool too and ATE THE EPILOGUE'S OWN cs
reloads, refusing every marked musttail; epim? verification keeps the loose peel
honest); and `vmset` became a pin door (its regs leave the pool) — the riscv64 love.c
build proved the armor's worth: a rostered pool pin dropped by a splice, freed at
psreset, then re-asserted by the step label's vmset split the free list from the pin
set, and ralloc SCARED where pre-armor it would have silently double-booked. Gauge: the two-reads-per-var seat-exhausted call loop −5.0% insns (2/iter),
cycles flat-to-better (the host hides removed work as slack — the ship gate's
wall-neutral-insn-cut clause); the one-read shape byte-identical (the license refuses
the wash), corpus flat. The nested shape shows the standing conservatism: a pin the
OUTER loop drops is dead to the inner keep — outer rostering is correctly refused by
the ×8, but the inner loop can't rescue what vmset already killed (the keep-depth
residue, recorded). Laws: spl (four seats then the post-call reload into the pin, the
in-loop use reads the register, two slot reads total) reds on the license flip; nrg
(write-only x refuses: no dead reload). Eight spill-around torture shapes (two calls,
nesting, continue/break, cond calls, tail-from-loop, callish for-step, do-while)
agree with gcc at -O0/-O1; battery, fixpoint byte-identical, fuzz green. And a
process lesson beside the taint one: the shell's cwd silently reset to the POST tree
mid-session, so a "falsification flip" and a law run quietly read the pre-rung
sources and answered plausibly — absolute paths for every gate and probe in a
worktree session, and treat a flip that agrees too easily as a tree check first.
2026-08-11 · entry seeding (keep depth — the keep learns to CREATE): a keep only ever
preserved existing pins, so a hot scalar arriving at a loop unpinned — an outer keep
dropped it, or its reads sit mid-expression and never pinned, or a pre-loop call
killed it — stayed a per-use load forever. Now the loop entry SEEDS it: `loseed`
walks the subtree once (`rdscan`, a tablet of interned read counts — the first draft's
per-candidate string-compare walks were quadratic and crawled on love.c), and each
unpinned vuniv scalar with reads takes one slot load laid beside the seat movs:
callish loops seat first then the roster's own pricing, clean loops pin the pool.
The free file is pool0 minus the KEEP's regs — not g 'pool, whose dying-pin regs
wait for psreset; the seed's vmpin evicts the corpse (one reg, one name) and the
seed load lays after the movs that read it. One reg stays for the body's aim. The
same lochk edges verify; a clobber misses and bars a seed like any entry — and the
one genuine trap was the CAP'S CONTRACT: the keep-nothing lonone attempt "cannot
miss", but a seed that ignored lonone still could, so no bar ever accumulated and
the attempt loop spun forever (vbin_fill's twenty splicey loops; the fix is one
gate: lonone seeds nothing). The peepholes compose: stld turns a seed whose store
is still downstream into the arrival mov and deadst sweeps the spill, so a seeded
param or fresh local often never touches memory at all (ql's n, lp4's n, cln's b).
Gauge: the seat-covered call-loop fn −10.3% insns, the two-reads shape −7.9%, the
nested main-level shape +0.015% insns for −1.1% cycles (roster reloads trade 1:1
with use loads there; the win is the broken chain), cbm and the corpus flat. Laws:
ql (n's seat by seed, both conds register-register), irB/irBa (the call-killed
param re-enters by seed, x64 + arm64), lp4/zpf/spl re-truthed with their seeds —
the seed-off flip reds them; five-shape seed torture (trip-0, nested rediscovery,
call-killed, seeded-then-written, continue/break) agrees with gcc; battery,
fixpoint byte-identical, fuzz, virt-on-hart all green.
2026-08-11 · splice-crossing keeps (the end-label meet): a loop keep survived a real
call (seats ride, rostered pins reload) but died at an INLINED one -- cginl ended
every splice with an unconditional vmflush at its end label, so inlining a call, an
improvement, cost the loop its register residency (the old lp3 refusal). The flush
was over-conservative: the end label is a forward JOIN whose arriving edges are all
known (the body's fall-through plus one jmp per mid-body return), no pin is ever
BORN inside a splice (vmon? is false while inlining), and no store in the body can
alias a pinned slot (vuniv already excludes address-taken names) -- so every edge
map descends from the entry map by deaths only and the meet holds. The machinery is
the if/else join's, worn by the splice: the ret door snapshots the map on each jmp
edge into inlret (g 'inlsnp, saved/restored per splice like inlret itself), and
cginl replaces the end flush with vmset over the vmeet of the fall-through (gated by
vmout?) and every snap; a DECLINE still flushes (the discarded-emission hygiene
law). What rides: a call-free inlinee costs the pins nothing at all; an early-return
body meets its edges (a pin killed on ONE path dies -- the intersection is exactly
the soundness); a callish inlinee's real call is spared by the same vmcflush as an
extern call (seats ride, outer rostered pins reload inside the splice); an inlinee
carrying its own LOOP still refuses honestly -- the inner head flush empties every
edge map, which is why lp3's law survives with its why reworded. Gauge: the
call-free-inlinee loop −4.3% insns, the callish-inlinee loop −21.7% insns (i/s/a/n
all seat through the inlined wrapper: cmp register-register, the arg served from
the seat, the step in place -- ~4 loads + 2 stores per iteration down to the s park
pair), cycles flat on the host (removed work hides as slack -- the ship gate's
wall-neutral-insn-cut clause), the loop-inlinee wash count-identical, cbm twins
byte-identical, corpus flat. The aim-hold law re-truthed: g in the ana_d shape now
rides a borrowed seat through calls AND splices (the and-not-on-pin claim held, on
new registers). Laws: slk (seat mov + seat read past a call-free splice, one
post-loop slot read), erk (the same through two ret edges), wlk (callish inlinee:
seated cond, seat-served arg, in-place step) -- the meet-to-flush flip reds them
and un-seats the aim-hold law's g; ten splice-crossing torture shapes (one-sided
call, param-writing, depth-3 nesting, loop-carrying, continue/break, fesc,
seeded-then-written) agree with gcc -O0/-O1 across two arg sets; battery (now 132),
fixpoint byte-identical, fuzz green. And an excavation beside the rung: the bench
harness found the weak crt0 tail passing the RAW SP to an arg-taking main in a
libc-free link (argc read stack noise -- nondeterministic segfaults on the shipped
compiler; nolibc's strong __ai_start had hidden it everywhere else). Fixed in its
own commit (fdbd7aba): the weak tail unpacks argc/argv on all three arches, and
test/cc/131-argv.c pins it in the battery.
2026-08-11 · element pins across calls (the element arm): a constant-indexed element
pin ("x[3]", the vmap's array leg) died at every call -- lomig's walk refused any
name outside vuniv, so a callish loop reloaded its hot elements per use while seats
sat free. But a vuarr array is local and non-escaping, so an element's slot is a
STATIC frame offset recoverable from the minted key itself (`aeoff`: prefix-match
the array over vuarr, sound the digits, afd's own width gate) -- and with the slot
static, the element arm in lomig seats and rosters exactly like a scalar's: free cs
seats first, then the spill-around license priced by a per-key read counter
(`nrdse`, an asn's own element lhs excluded), the reload one direct ld. vmcflush,
lochk, lobar and the regen ride unchanged -- a variable-index write still drops the
whole array and the edge check bars it (lo7's law untouched). One knock-on: the
frame-direct element write re-pinned through vapin, which refuses non-pool
registers, so a written element on a SEAT lost its keep -- `vaepin` (the
borrow-aware re-pin: csbor → vmbpin, else vapin) closes it, and epw's law pins the
write-through + re-establish-in-seat pair. The peepholes compose as with seeds: a
seat-riding element whose slot is never re-read loses the slot entirely (deadst
sweeps the dead write-through store -- epc's x[1] never touches memory). Gauge: the
two-element call loop −9.5% insns and −14.5% CYCLES (the element loads leave the
loop's dependency chain -- a genuine wall win, not just count), the variable-index
wash insn-flat, cbm twins byte-identical, corpus flat. Laws: epc (both elements
seated, n spills around, x[0] one post-loop read, x[1] memory-free) and epw (the
mid-loop element write stores through and re-establishes in its seat) -- the
element-arm-off flip reds them; eight element torture shapes (written, variable-
index barred, six-element seat exhaustion, element RMW, nested, continue/break,
element-through-splice) agree with gcc -O0/-O1 across two arg sets; battery 132,
fixpoint byte-identical, fast gate flat.
2026-08-11 · store elision across calls (the exit meet -- the loop exit is the third
join): every keep died at ld, so a kept var paid a post-loop reload AND kept its
write-through stores alive (the slot's loads made them undead to deadst) -- the
count's whole residue on call-loop shapes. Now ld MEETS its arriving edges: the cond's
false branch (g 'brkm-free) and this loop's breaks (each break snaps its line map onto
g 'brkm, a stack in lock-step with g 'brk; switches push/pop in lock-step and their
snaps are discarded). The surviving pins make post-loop reads zero forms; with the
slot load-free, deadst sweeps the write-through stores AND the entry spill -- lp4's
i and s never touch memory at all now, and the loop runs register-pure but for the
park pair. TWO SOUNDNESS HOLES found and closed on the way, the second latent in the
tree: (1) the cond edge must carry kp-VERIFIED pairs only (vmeet with kp, all three
lanes) -- a short-circuit cond exits to ld from EVERY false leg, and a pin born in a
later leg never ran on an earlier leg's edge; kills are monotone along the cond, so
kp survivors hold on all. (2) `pex`, the &-taken prepass, never descended into an
element in HEAD position -- every AST node heads with a tag symbol so it never
mattered, but an init list's first element is an EXPRESSION in head position, and
`&lam` as a compound literal's first initializer (love.c's mm() root shape, exactly)
escaped the scan: mm-rooted locals stayed in vuniv, pinnable, and the collector's
root rewrite made any kept register STALE across a GC-ing call. Latent since the cs
borrow made pins call-crossing; the exit meet's wider survival let ana_d's closure
fixpoint hit it (an unbounded shash recursion on the corrupt analysis chain, at egg
boot). One line closes it (pex walks two? heads), and test/cc/132-clitaddr.c pins the
shape in the battery -- the callee writes through the registered root, so gcc is the
oracle. Gauge: the lp4 call-loop shape −12.5% insns and −16.6% CYCLES, the
locals+elements shape −9.1% insns, cbm −16.3% cycles at FLAT insns (the inner loop's
slot store→load dependence chain broke -- wall, not count), fast gate flat. Laws:
lp4 re-truthed (s+i ride out, `(= 4 ldc/stc)` -- park pair + cs saves are the WHOLE
frame traffic), zpf (the return reads the seat, s's slot gone), spl/nrg/epc/epw/
slk/erk/wlk re-truthed the same way -- the meet-off flip reds them; ten exit-shape
tortures (breaks with divergent pin states, short-circuit while/do conds, for(;;),
nested, post-loop calls, the ana_d fixpoint skeleton) agree with gcc -O0/-O1 across
two arg sets; battery 133, fixpoint byte-identical, fuzz green. And a process lesson
paid for twice: the make lane compiles through mooncc0.image (love0-baked from the
TREE'S gen.l) -- swapping out/host/mooncc.image changes nothing it builds, so a
variant test that leans on `make` is testing the tree, not the variant; hand-baked
images + hand-linked hybrids (moon0 -pie love.o host_*.o ...) are the honest bisect,
and a boot probe that "works" right after a cp of a known-good binary is testing
the cp.
2026-08-11 · params on cs seats, pmin-gated (the callish cs homes — the residue
list's "lvm_eq a/b"): a priced callish fn's homable params take callee-saved seats
instead of wrapped hregs — the per-call wrap pair retires for one prologue save + a
reload per exit (the cssv/epi machinery unchanged, unframe and cskeep ride as-is),
the home stays off g 'homes (no wrap, no shadow slot), and the granted params'
homeregs REJOIN the regen's pool. Fn-wide grant or none; seats from what lpick left;
never beside the loop borrow (a keep's claim wins the seat); x64 by construction
(a64's cspool is empty). The pricing took three cuts, each caught by the per-symbol
grower audit: raw slot reads (nr) overcount — stld/dehusk erase pre-call reads free,
so lvm_eval paid 12 movs to save nothing; reads-after-a-call still overcount — a
redefining store re-arms stld (g = c0(..) feeds the loads behind it); the honest
static signal is the DIRTY LOAD — a call dirties every param slot, a store cleans
its own, and the first dirty load per window is the reload no recovery pass erases.
Priced nh·(1+nrets)+em < 2·ndirty. THEN THE DYNAMIC VERDICT REVERSED THE HEADLINE:
the static grant read love.c −1,574 insns / .text −8 KB (lvm_eq 841→586,
lvm_add_string's wrap quads gone), but the corpus A/B — same merged tree, same
corpus, pre-rung vs rung binaries — measured **+2.7% dynamic insns** (40.95→42.06 G;
cycles −0.9%, under the layout floor). The pmax lesson wearing the params face: the
save/reload is a PER-INVOCATION cost and the wraps were a PER-CALL cost, so every
early-out fast path — the dispatch fleet's whole hot profile, lvm_eq's fixnum lane
first — pays the prologue and earns nothing. The landed gate is `pmin`, pmax's twin
(the forward-path call MINIMUM): a grant needs every path through the fn to call.
What survives is small and true — 13 symbols, −1,020 B, ai_ini_0 −736 the biggest
(straight-line call-dense init, the exact shape) — and dynamically EXACT: corpus
insns 40.95 G to the third digit, cycles −0.9%. The machinery (grant plumbing,
dirty-load pricing, pmin) is the substrate; the fleet's 8 KB waits on SHRINK-WRAP —
saves at the callish region's head instead of the prologue, so a fast path never
pays — which is this leg's next boundary.
2026-08-11 · shrink-wrap (the cs grant's second flavor, for the early-out fns pmin
declines): the wrap moves off the prologue — region 1 (the call-free statement
prefix) runs the pre-rung emission unchanged, params in slots and the PLAIN
epilogue; at the split build lays a minted label + seat saves + slot→seat loads and
flips the epilogue pin; region 2 rides the seats wrap-free. The machinery: build
splits the statement list (cgitemx threads region-1 decls' env across), sibs
matches tails against TWO epilogues picked by which side of the wrap label the
candidate sits (sibjmp emits the one that matched — a region-1 tail reloading
unsaved seats would corrupt the caller), goto/lbl bars the grant (a cross-region
jump skips or re-runs the wrap), and cskeep verifies unchanged — its dirty-set
fixpoint was built for path-dependent saves. The recovery passes then finish the
job unasked: stld folds the wrap's slot loads to arrival movs and deadst sweeps
the entry stores, so the probe's fast path is byte-identical to pre-rung. THE
EMPIRICAL LADDER is the story: contending pp for the dispatch fleet measured
+1.0% dynamic insns (fat grant: +2.7%) — lvm_eq alone +0.55 G — because pp's
per-call wraps are COLD-PATH wraps: the hot early-out path keeps its registers
and the wrap movs execute only where calls do. Every static model tried (dirty
loads, wrap-benefit, region-1 read debit) approved grants the corpus refuted;
the pp fleet's 8 KB of wrap bytes is dynamically already-paid-for, and only a
frequency signal (PGO) can beat a cold-path wrap. The landed scope: !pp, every
call-carrying path ≥ 2 calls (pminp, pmin's positive twin), dirty-load priced —
one grant in love.c (ai_big_quot_true) plus the ez cs arm (a seat param arg is
targetable unconditionally: staging never writes callee-saved and no sibling's
call clobbers it), net −39 static insns, corpus insns EXACT to four digits
(40.95 G), cycles flat. Pays a little, regresses nowhere; the machinery is the
substrate the frequency-driven grant will ride. Residues: sk=0 shapes (cmp3 —
first statement callish, needs a form-level wrap point), big_addsub-shaped
declines (call-carrying paths whose dirty loads are thin), and the fleet itself
(waits on PGO).
2026-08-11 · SLOT REPACK + object-precise deadst (the frame lever the fifth
diff fill named: nslot never hands a cell twice, so a frame is the COUNT of its
temporaries — 27% of love.c's fns carried >128 B frames and 11 K stack movs paid
disp32; deadst's whole-fn lea bar hid 11 KB of dead stores, ALL of them in the 160
lea-carrying fns). Two pieces on one substrate, the slot map: nslot registers every
cell in g 'slots ((off n)..), deadst's lea arm marks the OBJECT under the lea live
(the granule refinement its own ⚠ said would lie — the map is what makes it honest;
an unclaimed lea still bars whole), and repack — POST-CHOICE, between deadlab and
unframe — packs objects whose live windows never meet into shared cells and shrinks
the sub. Windows are form-index spans widened over every backedge to a fixpoint
(linear-scan's loop extension); an address-taken object relocates whole (the lea
moves with it) but lives to the last form and only 8-byte cells pool (wider cells
place fresh, 16-parity held); bars mirror unframe's law (raw, loose r4, unclaimed
touch, -8 pinned); ships only when the sub shrinks. THE PLACEMENT is the payload
lesson: the first build ran repack inside the per-attempt pipeline and the corpus
laws caught ql's cs lane wrapping — the rankers price param slots BY BUILD OFFSET
(nreads/nrac/pslots), so a packed ir1 misprices every grant. Post-choice, with
deopt restoring ir1's slot map beside the accumulators, the rankers read unpacked
ir and the winner packs once. .text 585,440→560,864 (−24.6 KB, −4.2%), the movq
$imm fold-residue stores −6.4 KB, dead stores −4.6 KB, median frame 72→40 B,
disp32 stack movs 11,055→9,711; corpus dyn insns −0.25% (40.746→40.646 G), boot
−0.60%, corpus green through the packed binary, fixpoint + vmret + the moon
battery green. The law churn was the offsets: save/restore laws re-anchored by
SHAPE (cspair?/spillrd?/wrapon? — the offset binds within the pair, the seat is
repack's to choose), array laws by the lea base (aeb). Residues: 16-byte cells
never pool (parity insurance), sk-anchored fns keep old layout on any bail, and
slot canonicalization now makes link-time ICF worth re-measuring (~0.5 KB today
because offsets de-canonicalized identical bodies).
2026-08-11 · THE spush CELL JOINS THE SLOT MAP (the rung the arc's pricing named): deadcell
already CONVERTED a live spush cell into a frame slot -- offset (F+8)+8·rank, the prologue
sub grown to F2, the st/ld re-based onto r4 -- and never told `g 'slots`. So every pass
reading the OBJECT map met an r4 touch nothing claimed: repack's scan bars whole on one,
and it was barring **215 of 478 fns holding 77.1% of love.c's frame traffic** (the census:
3,716 objects and 16,063 slot ld/st out of reach, their frames packable 62,832→23,440 B).
One foldl registering `[(0-o) 8]` per converted depth closes it. love.o .text
361,873→**339,492 B (−22,381, −6.2%)** at FLAT insns (77,106→77,109) — the win is
ENCODING, disp8 replacing disp32 once repack can shrink those frames; frame-mov bytes
132,849→120,007. Gates: test_slow (seven zz-fin lines), test_fixpoint byte-identical,
vmret 307 ret-free, test_raw/test_drv/test_libc/test_kore green. ⚠ THE LAW CHURN was the
lesson: fifteen laws spelled a frame OFFSET and repack re-chose every number, so they red
together — and wlkf's `(= 1 (ldsp wlkf 8))` kept PASSING while counting a cs restore in
place of the park it named, which is the failure mode that matters. Re-anchored on `sof`/
`nldrg` (a slot named by the register that owns it) and verified in BOTH worlds — the
laws now pass pre-rung and post-rung, which is what says they describe residency and not
layout. The two-world run caught one bad anchor: hf's park is r0's slot, not r6's, and
they coincide only after this rung pools them.
2026-08-11 · THE DEAD-STATIC SWEEP a1e12f40 (bytes, not codegen — lever 5): gen.l
spliced a small static into every call site and then emitted the out-of-line body
anyway, named by nothing. A mark from roots over the emitted forms, in gfns before
the per-function units concatenate, sweeps the statics nothing reaches. ⚠ the ROOTS
are the whole safety argument and a missed one is a jump into the heap, not a bigger
binary: every exported fn, every alias target, every section-named fn (xfns, placed
for their ADDRESS), and every nom the DATA lane names — love's kind-indexed tables
reach copy_data and the collector by address and never by call. The reference
relation over-approximates on purpose (an opcode counts); only a static is ever a
candidate, and lnames sheds the swept ones so no LOCAL FUNC symbol names a body that
no longer rides. 149 bodies, 32,768 B: .text 556,768→524,000, love's own unreachable
6.4%→0.6% against gcc's own 2.6% and clang's 1.7% — under the natives, which is the
row that says complete rather than lucky. Corpus unmoved (40.72 G): every removed
byte was unreachable, so this buys size and invocation speed and NOT the gap levers
1-4 measure. test_fixpoint byte-identical (the compiler sweeps itself and still
reproduces), vmret 307 ret-free, test_raw/test_drv/test_libc/test_slow green.
2026-08-11 · STEP 0 RE-RUN, and the bar census that closes the spush rung (HEAD a055d279,
no code — measurement only): the re-base fill was four hours old and one rung stale, so
every number the arc was steering by moved. Both-emit codegen **1.56×→1.47×** (gap
141,221→**118,531 B**), binary 1.702→**1.618×**; love.o's .text fell 22,381 B over the
same pair of trees and the shared-symbol gap fell 22,690 — they agree to 309 B, which is
what says the move is the rung and not the fourteen post commits the merge carried.
Frame movs **21,964 before and after**, 132,849→120,007 B: the count is identical, so the
rung bought encoding and the emission side confirms it. The excess over gcc is 92,679 B,
**78.2% of the gap, up from 75%** — the share rose because the gap shrank faster than the
traffic. The census, four exits separated over love.c's 640 fns (temporary probe,
reverted): packed 403/23,830 touches/**92.9%**, barred 10/991, no-shrink 84/658, early
143/169 — and **all ten bars are `raw`**, an inline-asm splice repack skips by design.
Zero loose, zero unclaimed: the 215-fn barred class is GONE, not reduced. love.c through
mooncc 8.40/8.42/8.50 s against the 8.8 s baseline, so the per-depth foldl costs nothing.
⚠ THE READING THAT OUTLIVES ALL OF IT: dynamic 1.63/1.625/**1.629** across three fills
while static walked 1.63→1.47. Every lever this tree has landed is a size lever, and the
executed stream has not moved. The allocator leg is the first rung owed a corpus A/B as
its headline, not a .text delta.
2026-08-12 · RUNGS 1+2, THE LIVENESS KIT AND SLOT PROMOTION 4dd9bc41 (shipped together, rung 1
having been measured at 736 B alone): rdsp was already the per-form transfer function and the
missing half was the GRAPH -- per form, not per block, successors being the jmp/br target plus
the fallthrough, so there are no blocks to build and the fixpoint is repack's own widen1 shape.
An op neither rdsp nor the rosters name answers 'bar and REFUSES the fn. On it, promotion: an
object whose every touch is a full-word ld/st at its own base, never addressed, written before
read, takes a caller-saved register when one is free across its widened window -- free meaning
live-out nowhere in the range and defined nowhere in it, which a call fails by construction. It
rides repack's analysis whole (object map, backedge widening, bail discipline). love.o .text
339,492→**333,177** (−6,315, −1.86%); frame movs 21,964→**19,478** (−2,486, −11.3%), their bytes
120,007→106,892. ⚠ static insns move by TWENTY-NINE: the trade is a memory mov for a register mov
at equal count, so this is the first rung in the arc whose case is DYNAMIC -- corpus insns
**−0.73%**, cycles **−0.93%**, the first movement in that row across four fills. love.c 8.42→9.90 s
(the fixpoint's cost, inside the 20 s budget). THREE BUGS, each now a comment: a cs save is
`(st r4 slot r9)` and its restore `(ld r9 r4 slot)` -- the exact shape of a promotable temporary,
and only the register it moves tells them apart (cskeep caught it at compile, its armor works); a
sibcall is a jmp to no LOCAL label, and read as a plain transfer it has no successor, so liveness
called the whole argument file dead at the one place it is most alive; and the universe was built
from the traffic rdsp names while a SEAT is chosen from the caller-saved file, so a register the
fn never otherwise mentions was absent from a call's clobber set and read as free ACROSS the call
-- ai_sleep promoted into rdx over its one call and that answer survived cskeep, the laws and
every gate but running. A liveness universe must cover the seats, not just the traffic. The kit
carries its own unit laws (join, backedge, call, sibcall, refusal, universe), hand-laid rather
than compiled. Two older laws re-anchored off the mechanism onto the invariant (`ldinto`,
`copyof?`): ci's indirect call now stages through r0 with no frame at all rather than two spush
cells, and wv's written param takes a register instead of a spill. Gates: test_slow (seven
zz-fin lines), test_moon, test_fixpoint byte-identical, vmret,
test_raw/drv/libc/kore/clay.
2026-08-12 · STEP 0 AGAINST RUNGS 1+2 (measurement only, two ccbench passes): both-emit codegen
1.47×→**1.44×** (gap 118,531→**112,069 B**), binary 1.618→**1.591×**; love.o .text fell 6,315 and
the shared-symbol gap 6,462 -- agreeing to 147 B, so the move is this rung alone. THE ROW THAT
MATTERS: corpus insns 42.291→**41.984 G** with BOTH natives flat to 0.05%, so the dynamic ratio
moved 1.629→**1.617×** -- the first movement in that row after three fills of size levers left it
untouched. Frame-mov excess over gcc 92,679→79,564 B, **71.0% of the gap, DOWN from 78.2%**: the
share had risen at every prior fill because the gap shrank faster than the traffic, and this is
the first lever aimed at the traffic itself. poly1305 PASSED gcc and reproduces (1.04× → 0.98×
and 0.99× over two passes, gcc's own stable to 0.3%) while chacha holds at 3.5-3.6× -- the pair's
designed reading firing exactly as specified, poly being scalar locals and chacha array slots.
Warm build 15.4 s (flat vs 15.1), love.c single TU 8.42→9.90 s. ⚠ TWO INSTRUMENT LESSONS: the
mcobj cache is not "paid once per tree" -- 45.2 s cold and 15.4 s warm on the SAME tree, because
a rung that changes the compiler changes every member hash, so every codegen fill pays it; and
clang's chacha is NOT the box anchor the previous fill called it (198.1 twice was coincidence,
169.9/182.7 the next day), so that fill's "1220→1075 is a real move" is corrected -- two agreeing
samples are not a control.
2026-08-12 · COALESCING (rung 4, pulled AHEAD of rung 3) — a copy whose source was defined by
the form before it and dies at the copy is a def that named the wrong register: the def takes the
destination and the mov never lands. The liveness kit's second consumer, and the first to ask a
question the window heuristics cannot — not "does this value die soon" but "does it die HERE".
+38 lines. **corpus insns 41.983→41.244 G, −1.76%** (cycles flat, within noise), love.o .text
333,177→**327,064 B (−1.83%)**, insns 77,080→**75,100 (−2.57%)**, reg-reg movs 12,353→10,391.
Against clang the dynamic ratio goes 1.617→**1.589×** — more than twice rungs 1+2's move, and the
largest on this arc.

⚠ WHAT THIS RUNG COST TO FIND, because the route matters more than the result. It was reached by
building the CALL-CROSSING CLASS on cs seats (rung 2's open half) and measuring that it CANNOT
PAY. A cs seat can never equal the store's source — sources are caller-saved — so no mov ever
drops, and the rewrite is a frame touch turned into a reg-reg mov ONE FOR ONE plus the
save/restore pair: measured +3,073 movs against −1,722 frame movs, +1,349 insns, exactly the pair
count. The lvgp class paid precisely because its seat CAN be the source. No pricing gate repairs
that; a gate tight enough to be safe admits nothing. Even with coalescing on top, cs seats stay
+1,077 insns for −1,479 B and retire no mechanism. **The class is refused**, and the plan's
ordering with it: rung 4 comes BEFORE rung 3, because nothing that turns memory into copies pays
until the copies can go.

Three traps the attempt paid for. **unframe reads the prologue BY POSITION** — its `sv3` asks
whether form 4 is exactly `(st r4 -8 r3)` and drops that save with the frame — so saves inserted
at index 4 displaced it, leaving the rbx save standing while its restores went, and the caller's
rbx died at the tail; it presented as a segfault in `main` with rbp holding a love fixnum, AFTER
the corpus had run and printed `tests pass`. **A name-keyed bisect over one TU's functions is not
a bisect**: the switch matched by name across every TU while the name list came from love.c
alone, so musl and host/*.c promotions were never disabled and three "culprits" were artifacts.
What actually cracked it was emitting the pair while rewriting NO touch — still crashed, which
proved the fault was in emission rather than in the promoted value and pointed straight at
unframe. **The emitter's own `alias-dst` guard** caught the coalescer renaming a destination onto
one of the def's sources; the rename must be a stranger to the def on both sides.

Laws: six hand-laid shapes for the fold and its five refusals (source still live, def reads its
own dest, dest aliases a source, a label between, the prologue). One older law re-anchored off
the mechanism onto the invariant — ci's indirect call is pinned on "two loads, neither onto an
arg seat" rather than on counting r0's, because coalescing now gives one argument its scratch at
birth. Gates: test_slow (seven zz-fin lines), test_moon, test_fixpoint byte-identical, vmret,
test_raw/drv/libc/kore/clay.

2026-08-12 · COPY PROPAGATION (the forward half; dehusk retired) — `dehusk`'s five hand-cut
windows onto one law are one pass: a forward walk carrying `reg -> source`, killed at each def and
at each control edge, with `lvout` answering the drop. Gone with them: the rename sandwich's
8-form cap, the quiet back-copy's 6-form cap, the adjacent-pair cases, and `huskrd` — the
whitelist of renamable operand slots. The slots are PROBED off `rdsp` instead: substitute a
stranger at each nom position and ask whether it lands among the reads and not among the defs, so
a read-modify-write slot (the shifts, the unops) and `la`'s SYMBOL operand decline by
construction, and the roster cannot fall out of step with the table it models. What still needs a
reason rdsp cannot carry is a five-name skip list, each entry a contract rather than a dataflow
fact: the address families (addrfold consumes a mov feeding a base under its own license),
`push` (cskeep reads `(push <cs>)` as that register's SAVE), and the variable shifts (holo scares
if the count is not r1). **corpus insns 39.734→39.464 G (−0.68%)**, cycles 14.871→**14.798 G
(−0.49%, minima 14.678→14.575)**, love.o .text 327,064→**324,872 B (−0.67%)**, insns
75,100→**74,390 (−0.95%)**, reg-reg movs 10,634→**9,908 (−6.8%)**; single-TU compile of love.c
13.72→13.99 s (+2%, flat); gen.l 8,112→8,125. ⚠ the dynamic row wants an interleaved run and
several: two builds 10 bytes apart read 0.3% apart on corpus insns, so a single pair is not a
measurement — it is which movs went, not how many.

⚠ THE TWO DIRECTIONS COMPOSE, and that composition is where most of the win is. Forward
propagation alone REGRESSES the corpus (+0.19% insns) while shrinking `.text`, because renaming
`(add r1 r1 imm)`'s source breaks the two-address fusion and the emitter buys the mov straight
back. The fix is not to protect the fusion — measured, and protecting it is 914 B WORSE — it is
to let the break happen and hand the wreck to `coal`, whose backward fold gives the def the
copy's name and rebuilds the fusion on the right register. `coal` needed one relaxation to accept
it: the destination may alias the def's FIRST source, because `(mov d a; op d b)` is the lowering
and `(add r7 r7 8)` IS the fused form — it is the SECOND source that reads its own wreck. So the
sandwich `dehusk` did with an 8-form window now falls out of two local passes with no window at
all.

⚠ AND PLACEMENT IS A REAL CHOICE, not a detail — four were built and measured. The three
POST-CHOICE ones (varying where `coal` sits around `unframe`/`unhome`) all read BETTER on paper:
`.text` 323,976-324,430 B and 74,065-74,238 insns, −0.94%/−1.38% at the best, corpus insns
−0.67%. All three lose on the clock: interleaved cycles put them at **+0.27%, +0.48% and +1.4%**
against **−0.37%** for the build-tail placement. That is the `emit-alu` lesson holding a second
time from the other side — a mov the CPU rename-eliminates costs no cycles, so deleting one LATE
buys instruction count and nothing else, while deleting it EARLY feeds `addrfold`/`cmpfuse`/
`deaddef` a cleaner input, and that is where the clock moves. The control for reading those
numbers was ablating `dehusk` entirely (+0.5% insns → +1.1% cycles) and the build-tail candidate
(−0.20% → −0.37%): both track their instruction counts, the post-choice three do not. ⚠ measure
cycles interleaved and in rounds, and read the MINIMA beside the median; a single pair is layout
lottery on a corpus running at IPC 2.7. (Post-choice also needs `coal`'s prologue floor relaxed —
`(5 < i)` is a proxy for "not the prologue" and there is no prologue left after `unframe`.)

⚠ COPY PROPAGATION MOVES THE PARAM-GRANT PRICING, because `pmin`/`nrac`/`nreads` are all read off
`ir1` — the IR `build` returns — and a cleanup INSIDE build changes what they measure. On one
synthetic shape (law.l's `ci`, an indirect tail call) that flips a grant: the fn takes a cs seat
and grows a frame, 7 forms to 12. Held against the real corpus it is a shape, not a class — **no
function in love.o gains or loses a frame** (545 `sub $N,%rsp` both sides, 331→330 cs-slot
stores, 181 fns shrank against 46 grown) — so it ships with the law re-anchored on the struct
loads rather than on every load. It is also a preview of rung 3: the grants cannot be deleted
until their pricing moves somewhere that a later pass cannot perturb.

Ablation first, as always: `dehusk` at HEAD was worth 0.52% corpus insns and 0.22% `.text` for 79
lines. That number is what made a rewrite the right move rather than a deletion.

⚠⚠ AND THE FIRST SHIP OF THIS RUNG COST 78% OF THE COMPILER'S SPEED, unmeasured. The numbers
above are the SECOND, after step 0 caught it: love.c through mooncc went **13.2 → 23.4 s**
(interleaved medians, 5 rounds) and no gate says a word, because every gate asks whether the
output is right and none asks what it cost to produce. **The build tail costs DOUBLE** — `build`
runs twice per fn, ir1 and the regen — so the two `lvout` fixpoints copyprop put there were paid
four times over, on top of the one `coal` already paid post-choice: five whole-fn fixpoints where
there had been one. Isolated by substitution, one variant per build: the pass with no liveness at
all 12.2 s, plus the drop's lvout 17.1 s, plus coal's 19.6 s. **The forward walk itself is free**
— its per-slot `rdsp` probing, the part that looked expensive, costs nothing measurable.

The fix is placement, and it makes the rung better rather than merely cheaper. The forward walk
stays in the build tail, where its renames reach `addrfold`/`cmpfuse`/`deaddef` and where the
clock actually moves; it asks no liveness, so the double cost is nothing times two. The backward
fold AND the drop both ride `coal`, post-choice, off the one `lvout` that was already being paid
there — `cpdead` is gone as a pass, folded into `coal`'s existing walk. One fixpoint in the whole
pipeline, exactly as before the rung, and the codegen came out BETTER than the version that cost
78%: −0.67% `.text` against −0.53%, −0.95% static insns against −0.73%, −0.68% corpus insns
against −0.20%.

Two things that fusion turned up, both of which had been silently costing codegen:

* **the lookbehind must survive a drop.** `coal` holds the previous form to fold the next copy
  into; clearing that hold when a copy DROPS loses exactly the composition this rung is about,
  because the dropped copy is gone from the output and the def behind it becomes adjacent to the
  next one. Caught on `gq`, where `(add r9 r6 1) (mov r0 r9) (mov r10 r9)` needs the dead middle
  gone AND the pair folded, and got only the first.
* **`coal` was never x64-gated, and the drop is what made that fatal.** `lvout`'s universe is
  the x64 files — `lvgp` the caller-saved gp set, `lvret` what an x64 exit owes, `csregs` the
  x64 borrow — so on an lr/fp target its answer is a different machine's liveness. The FOLD had
  been riding that unguarded since the coalescing rung and no gate ever said so; the DROP hung
  riscv's on-hart egg bake (`test_virt`, exit 124, the timeout face) the first time it ran.
  `coal` now takes the `arm? g` bail that `repack` and `cskeep` already take, and x64 is
  byte-identical with or without it. ⚠ **a latent unsoundness only shows when something raises
  the stakes** — the fold's silence for two rungs was luck, not licence.
* ⚠ **and the floor cannot be read off an x64 shape.** An attempt to derive it from `pro4?` (4
  when the x64 prologue is there, 0 otherwise) was measured a wash on x64 — and handed arm and
  riscv, whose prologues are longer and whose `pro4?` is false, a floor of 0. That was the
  proximate cause of the same hang. It stays flat at 5, one rung more conservative than x64
  needs, because prologue ground is per-target and this pass cannot see which target it is on.

⚠ the standing lesson: **a codegen rung owes a COMPILE-TIME A/B, not only a codegen one.** "Speed
is a signal" applies to the compiler as much as to the test suite, nothing in `make test_slow`
watches it, and this one shipped green. Measure the single TU interleaved, both directions, and
put the number in the ledger beside the bytes.

⚠ AND THE GUARDS ARE NOW PROVED, NOT ARGUED. The rename relation is FINITE — 75 op shapes (one
per op `rdsp` knows), at most two read positions each, 15 registers in the gp file — so "can this
pass hand holo a form the assembler refuses?" is DECIDABLE by running the carrier. `crew/moon/law.l`
now exhausts it: **390 forward renames** through the real `cpwalk`, **570 backward folds** through
the real `coal`, every result encoded by the real `holo-bytes`, plus a coverage assert that reads
the op ROSTERS so an op joining one without a shape goes red by name. This is `test/uukindlaw.l`'s
instrument — generate the model from the implementation's own table, then exhaust a finite carrier
— pointed at codegen instead of at the kind lattice. Falsified three ways before being believed:
`vshops` out of `cpskip`, `cpsub`'s alias guard off, `coal`'s alias guard off — each goes red on
the matching assert, and the first also moves the carrier-size count, which is the drift signal
doing its job. It costs nothing: 6.97 s against 7.06 s for the law file without it. ⚠ the two
refusals this pass was built against (`alias-dst`, `shiftv-count-not-r1`) were each found by a
BUILD BREAKING and a guess; a complete proof over the carrier is what replaces that, and it is
available exactly because the carrier is small — reach for exhaustion before reaching for search.

Laws: fourteen shapes over `copyprop` — the sandwich in both directions for `add` and for `sub`,
the chain, the self-mov, the alu read-through, the br fall-through at any distance, the label and
call resets, and the four refusals that carry a reason (`la`'s symbol, the shift's count seat, an
address form, a killed map entry). Nine residency goldens re-anchored: each had pinned the
accumulator's own register in `(add rX rX rY)`, and the seat operand — which is what those laws
are actually about — is unmoved in every one. `dv` re-anchored off "the park reads through" onto
`(div r1 r6 r5)`, since there is no park left at all. Gates: test_slow, test_moon, moon-stage (20
sigs), test_fixpoint byte-identical, vmret (307 lvm_* ret-free), test_raw/drv/libc/kore/clay.

2026-08-12 · SHRINK-WRAP RETIRED (rung 3, the first half) — the arc's heaviest mechanism, priced
by ablation and then deleted. Turning the `swcs` guard off costs **48 bytes** of love.o `.text`
and 12 static insns, and nothing whatever on the corpus. Forty-eight. The plan page had already
written the reason down without pricing it — shrink-wrap "landed one grant in love.c" — and one
grant is what 48 bytes looks like from the emission side.

Gone with it: `pminp` (pmin's call-carrying twin, so the wrap could amortize on the cheapest path
that actually calls), `cgitemx` (a `cgitems` that also answers its final env, which existed solely
so region 1's decls stayed visible to region 2), `swre` (the env rewrite that re-seats the params
across the split), the statement-level region split inside `build`, the wrap label with its
save/reload pair, four `g` slots (`swcs` `swat` `swlab` `epi0`), and — the part that reached
furthest — the DUAL-EPILOGUE flavor of `sibs`: two parameters (`sj`, the split; `sw`, the
crossing flag) threaded through all six recursive calls, with `ejc`/`elc`/`sw2` recomputed at
every form of every function on every target, so that a tail call BEFORE the wrap could take the
plain epilogue and one after it the long one. That cost was paid per-form, program-wide, for one
function's benefit. **gen.l 8,125 → 8,012 (−113)** — the arc's first negative-LOC milestone.
⚠ the deletion is byte-identical to the guard-off ablation (324,920 B / 74,402 insns both ways),
which is the check that says the mechanism came out whole and nothing else came with it.

⚠ AND THE OTHER HALF IS REFUSED, on its own measurement. `pcs` — the pmin-gated cs-seat grant —
ablates to **+1,004 B** of `.text` (+0.31%) and **zero** corpus instructions: 39.466 G with it and
39.466 G without, identical to the digit, against a run-to-run spread of ±5 M that bounds what
this instrument can even see. So promotion does NOT do generically what this grant does
specially, which was rung 3's entire premise. Deleting it regresses the size axis and pays on no
other; under "pays somewhere, regresses nowhere" that is a refusal, and it holds until promotion
covers those bytes.

**The two halves differ by 42× per line and that is the whole finding.** `swcs`: 113 lines for 48
bytes, 0.42 B/line. `pcs`: ~57 lines (`pmin`, `nrac`, the grant block, the `pc0` threading;
`pslots`/`nreads` stay, `pp` reads them) for 1,004 bytes, 17.6 B/line. A rung named for a
mechanism CLASS hid that spread — the two grants were listed in one breath on the plan page and
priced in one breath, and they are not one thing. Price the members, not the class.

⚠ what is NOT settled is the clock. Four interleaved runs put `pcs`-off between 0.4% and 2.0%
FASTER on cycles, medians and minima agreeing in direction every time (base vs both −1.24%/−1.52%;
base vs pcs −0.96%/−1.98%; swcs-deleted vs +pcs-off, 15 rounds, −1.26%/−0.43%). But the
instruction count is flat, so that is a layout reading, not a mechanism reading — and the box
carried two other sessions' gates throughout, with the baseline itself drifting 14.874 → 15.219 G
across three runs. Interleaving defends against drift within a run and nothing defends against a
1% claim built on flat insns. It is recorded, it is not the basis of any decision here, and it is
the one thing that could still overturn the refusal on a quiet machine.

Laws: none added — `law.l` and `stage.l` type `sibcall`, the outer two-argument entry, whose
signature is unchanged; `sibs` is internal, so the musttail contract laws (a marked ret-position
call leaves as a jump, `musttail-not-a-tail`, the whole-fn `musttail-escape` decline) hold as
written and are exactly the laws that cover the deletion. Gates: test_slow, test_moon (its
"guaranteed sibcalls" leg is the contract this touches), test_gen, test_clay, test_drv, vmret
(307 lvm_* ret-free), `make test` host + love0 ×2.

Reverted with verdicts worth keeping: lea fusion c618c3d9, fn alignment 4e8bb80c, E5
read-establishment 132a9599, store-side addrfold copy-prop, cmp-mem (the first build) —
each a physics lesson above.
