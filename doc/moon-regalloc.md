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
  dehusk (mov husks: feed/back/dup/self, the rename sandwich, quiet-window back-copy),
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

The remaining excess is not idiom-shaped (that catalog closed 2026-08-02: cmp-$0, neg
dances, load-then-cmp all at parity). It is structural: slot traffic. At the fifth
differential ~22% of all static insns were rsp slot movs; only ~15% of reloads were
straight-line (peephole-reachable) — the rest cross branches and calls, which is exactly
the boundary the write-through vmap cannot cross.

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
   pinning fesc (sibcall/ride/homing open). What closure still needs: store elision
   ACROSS calls/labels (the allocator leg's liveness), and float/pair elements ride
   the old walk (afd's whitelist is gp+ptr).
2. **Registers as the source of truth** — the allocator leg proper: liveness over ir1,
   values surviving labels and calls, spill placement instead of write-through. Kills
   both the def-stores and the post-flush reloads (the ~22% bucket). The vmap, the JOIN
   meet, the cs pool and cskeep are its substrate; this is the step the peepholes cannot
   take (their reach measured exhausted 2026-08-04). Its emission-side half — passing
   the CONSUMER down as a destination die instead of delivering everything to r0 — is
   modeled runnable in doc/proto/dest.l (ev.l's continuation-taking emitter shape worn
   by a register machine; gated by test_doc). All four lanes migrated 2026-08-10 —
   asn/decl, cbranch-compare, arg-seat, bin-value (the rungs below); what remains of
   the die rides the allocator leg (callish sides via cs-borrow parks, deeper arg seats).
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
3. **Compare staging want-hints** — landed for the call-free side 2026-08-10: cbranch's
   left aims at its park before evaluating, so member loads deliver and the bridge mov
   dies. What remains is the callish side (the sp cell across a call — a cs-borrow park
   would need the wrap pricing) — or the allocator leg subsumes it.
4. Recorded small residues: leaf sp-fn stldw coverage; a non-positional fallback home
   (mag_cmp's g1 loses its seat and stays slotted); cs-borrow park elision (params on
   cs regs landed pmin-gated 2026-08-11, the ledger; the fleet's share waits on
   shrink-wrap); arm64 x19+ cspool (empty there today); register-binding splice
   depth (map_probe's &-decline); d128 params ("a wide arg: not carried"); variable
   index±k rebase; narrow cmp fusion (632 sites, needs cc-aware licensing); the
   3-address dance emission (encoder territory — the reverted lea-fusion physics, only
   density-neutral shapes need apply).

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
Reverted with verdicts worth keeping: lea fusion c618c3d9, fn alignment 4e8bb80c, E5
read-establishment 132a9599, store-side addrfold copy-prop, cmp-mem (the first build) —
each a physics lesson above.
