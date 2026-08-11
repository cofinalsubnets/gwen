# moon-hom — the shape migration ladder

doc/hom.md is the design said once; this page is the plan for making gen.l *wear* it —
not just the contract (the four die lanes landed, doc/moon-regalloc.md lever 2) but the
shape c0 and ev were built to. The runnable model of the target is doc/proto/dest.l's
`ck`. Status: chosen (revisable); rungs land bottom-up, each gated.

## the target shape, three deltas

gen.l today: `(cgexpr g env e)` answers a value tuple (ty, THE REGISTER IT LANDED IN,
forms); the die rides as `g 'want`, a one-shot advisory hint consumed at the top node;
recovery passes (addrfold, cmpfuse, dehusk, sibcall) claw back the bridges the r0
protocol lays. The hom shape for a register machine is dest.l's `ck` — forward
emission, the continuation in hand *defunctionalized* — never ev's backward thread
(that shape belongs to the stack machine, which has one object). The deltas:

1. **the die is a parameter, not state.** `w` rides the call, so it cannot go stale
   across a flush or splice — the aim-hold hazard class is a state artifact.
2. **the value tuple loses its register column.** `ck` answers forms only; there is
   no "where did it land" to report, because the consumer said where. Every `vreg`
   read is the bridge protocol made visible.
3. **the scratch pool is threaded, not a free-list.** a subexpression writes its own
   die and scratches strictly below it in the pool; one allocation discipline where
   today the free-list/pin split interacts (the aim hold's own postmortem).
   (answered NO at rung 4 -- the split died by the ledger fix, and the shape is
   blocked while dies are partial; read the rung.)

## the ladder

* **rung 0 — thread `w`, dead.** every `(cgexpr g env e)` becomes `(cgexpr g env e w)`;
  every call site passes `()` (the degenerate die IS today's protocol); nothing reads
  it. ⚠ in a curried language a missed site silently answers a closure — the census
  must be exhaustive before the edit. gate: test_fixpoint **byte-identical** — the
  plumbing proven inert before any semantics move.
* **rung 1 — retire the `want` state.** the pin sites pass their hint as `w` instead;
  cgexpr reads the parameter; the pin/clear dance deletes. ⚠ the aim HOLD (rpin) stays:
  it protects the hint register from ralloc across the rhs — free-list state, rung 4's
  business, not the hint's. emission byte-identical (same values, same order).
* **rung 2 — promote advisory to total, one die arm at a time.** the ablation
  (love.c, static insns) ranked the recovery passes first: addrfold still earned
  10,691 insns (10.4%), cmpfuse 1,255, dehusk 939. and it sharpened the frame:
  **cmpfuse and dehusk are LIVENESS-shaped recoveries, not destination-shaped** —
  their licenses are death proofs (dieb?, the strand condition), knowledge an
  emitter doesn't have at birth. the die fixes destination knowledge only, so those
  two passes STAY; the retirement targets are addrfold's cascades and the sibcall
  rewrite (which needs a fesc prescan, not a die — deferred).
  * **2a — the mem READ face (landed 2026-08-11).** clval answers a symbolic
    (base, offset) face (mkm/vm?/vmb/vmo); a member access FOLDS into the face at
    compile time; loadval/lvload lay ONE fused form; matv is the degenerate bridge
    (the address on r0, exactly what clval used to lay) at the six unconverted
    lanes (general asn store, ++/--, rmw, cxmat, cgsfill, asm ostores); the & lane
    lays its own ESCAPING lea off the face. p->b.y is one load at birth. love.c:
    −160 insns, −770 text bytes; addrfold's earnings 10,691 → 7,135 (a third moved
    to construction). gates: battery, cts (0 wrong), libc, ccarm64 129/129,
    ccriscv 128/128, fixpoint, fast gate.
  * **2b — the mem STORE face (landed 2026-08-11).** smaller than planned, because
    two doors already stood open: parse's calm? already desugars every static `op=`
    to a plain asn, and the frame-direct fd lanes already lay (st r4 off v). the rung
    is one new door — `sfd`, the STATIC face of a store target: a deref-const element
    (afd) or a pure var/dot chain over a frame local whose clval face is (r4, off)
    with no emission — wired into the asn and post fd lanes. s.a.z += 3 is ld/add/st
    on the slot, no lean, no park; the load-free slot then feeds the deadst cascade.
    love.c −2 insns (its style is pointer-heavy — the shape lives in struct-local
    code, where the probe shows 3+ forms saved per store). ⚠ lesson: c0 is
    single-pass — sfd's forward call to clval loaded under host-ev and broke the
    love0 bake (the miss surfaces as an unrelated later nom); definition order is
    the fix, and the love0 load is the honest gate for it.
  * **2c — the face keeps the pointer's register (landed 2026-08-11).** the ablation
    reclassified the residue first: the dominant class was not leax/ldx but 2a's own
    tor0 — the deref face bridged every pointer to r0 and addrfold folded the mov
    back into the load. now the face carries the pointer's OWN register (a home, an
    arg seat, a park) and the consumer both lays the fused access and FREES the base
    (rfree is pin-guarded, so r0/r4/homes are safe no-ops; matv, loadval, lvload,
    the & lane, szof and the dot fm-bad path each free). p->f off a homed param is
    (ld W r6 off), no bridge; an arg cell died with it (f(p->tag, p->size) is now
    cell-free — the law re-truthed to the tighter shape, ci's indirect-call cells
    honestly stand). love.c −114 insns; addrfold's earnings 7,135 → **5,047**
    (construction owns 53% of the rung-2 baseline). remaining residue: the
    store-immediate folds through computed addresses, the true leax/ldx indexed
    lanes, and spill-position folds.
  * **2d — the value-join meets (landed 2026-08-11).** the ?: value lane already met;
    the rung closed the four remaining expression-level flush joins — the &&/|| value
    lanes and cbranch's two internal mixed-polarity joins — with the exit meet's own
    rule: kills are monotone along a short-circuit cond, so the join map is
    vmeet(before, after). the payoff is the KEEP interaction: a && value inside a
    loop used to flush the map at its labels, miss lochk at the back edge, and bar
    the loop's keeps — now the pins ride through and the keep holds (the probe shows
    s and i pinned across the join, zero post-join reloads). love.c +55 static insns
    but −616 text bytes — memory operands became register reads plus seat movs, the
    keeps-engaging signature. reification itself stays (a value consumer needs the
    bit); the va_arg walk and discard-hygiene flushes stay (different licenses).
* **rung 3 — the shuttle (landed 2026-08-11).** the sp cell became the LAST resort:
  the word/float binop and compare shuttles now stage through a three-tier read —
  a left already RIDING a live register (a pin, a home) is read through it at the
  combine (zero forms, rd9's "an operand that already sits somewhere"), else the
  r1 hold, else a POOL park; the cell only when the pool is dry or the right's
  forms genuinely bar. two findings shaped it:
  * **decide from the FORMS, not the AST.** `callish?` barred every pool path for
    a right side containing a call — but an inline-spliced call (love.h's b2w in
    `Have(box_req)`, on every VM op) leaves call-free straight-line forms. psafe?
    scans the emitted forms: labels and internal branches pass (nlab-fresh, no
    entry from outside), a real call or a write of the candidate bars. the same
    scan licenses riding a pin across a splice — which also dropped the dead
    bridge mov the old cell recovery left behind.
  * **a park is a BORROW, not an allocation** (spare): its span closes inside its
    own staging, so the free list keeps its order — an alloc/free cycle reordered
    it and renamed every seat downstream (pass 1 runs the same code; the roster
    weighs its emission). and **never park what recovery already reads through**:
    the first cut parked floats with a bridge mov pair where the old cell had been
    recovered to ONE mov — movqrx aiming straight at the hold register beat both.
  love.c: −397 insns, −2,700 text bytes, cells 231 → 209 (the rest are divide
  staging, real call crossings, and dry-pool spills — the honest floor). the
  residue: a handful of +2..8-byte functions where a 2-mov park chain stands
  where recovery once made 1 mov. two laws re-truthed (an incidental slot
  offset; erk's seat rename once s rides its pin through the splice).
* **rung 4 — the pool as parameter: answered NO (2026-08-11), and here is why.**
  the rung's motivation was the free-list/pin split (the aim hold's postmortem) --
  and that class died the day before this ladder was written (2026-08-10, the
  ledger fix in doc/moon-regalloc.md): pin doors evict their register from the
  free list CONTINUOUSLY, the aim hold rides rpin, and ralloc scares on any
  pinned member. the invariant threading would grant by construction is already
  the code's, loud forever. what remains would be the shape alone, and the shape
  is blocked: dest.l threads its pool because ck's die is total and the value
  tuple has no register column -- gen.l's dies are partial BY LICENSE (the
  totality section below), so values answer registers upward and the consumer
  frees them; a threaded pool under that protocol means every lane re-deriving
  "free now" per sibling -- the one ledger reimplemented n times, by hand, for
  zero emission delta (allocation order renames seats; dry is dry in any order).
  the tree had already measured "more threading" in miniature and declined
  twice: one aim per spine (an eager aim at every level drained the pool -- the
  wraps law's catch), and cbind parks only into a DEEP pool (the +0.4% L1
  displacement). rung 3's `spare` banks the discipline's real content at the one
  new site class -- a BORROW whose span closes inside its own staging keeps the
  free list's order by construction, no parameter needed. revisit only if die
  totality ever lands (then the column retires and the parameter is the natural
  shape); until then the ledger IS the threaded pool, held by the ledger fix.

each rung past 0 rides the standing ritual: laws re-truthed from measured emissions,
tortures vs gcc, the differential tier, fixpoint as the self-consistency gate.

## totality, the honest risk

the recovery passes cannot retire until the LAST decline is gone — they are the net
under every decline. dest.l's `ck` is total over its model, but gen.l's lanes are
richer: t32 wide pairs (the die needs a pair face or those lanes keep the degenerate
door), floats, complex, builtins, inline asm. some lanes may keep `()` forever, which
caps how much recovery code actually deletes. the ladder is sound anyway: the
degenerate die is the permanent floor, so a lane that never migrates costs nothing.

## the discipline

never the big-bang rewrite into ck's image. lane-by-lane with the degenerate die as
the floor is what landed the first four lanes on a permanently green tree; a rung that
cannot hold its gate rolls back whole. the vmap/keeps layer is orthogonal and composes
(a destination can BE a home — dest.l's not-modeled note); nothing here touches it.
