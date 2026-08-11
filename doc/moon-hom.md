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
  * **2b — the mem STORE face**: promote the matv'd lanes to consume (st b o v)
    directly. **2c — the indexed face**: the leax/ldx lanes (variable-index element
    reads ride rvalue pointer arithmetic, a different provenance than clval).
    together they hold addrfold's remaining 7.1k.
  * **2d — ('br lt lf)**: the ?:/&&/|| value lanes stop reifying and their vmflush
    joins close (an emission win, not a pass retirement — cmpfuse stays).
* **rung 3 — the shuttle.** spush2 parks replaced by pool-threaded operand reads
  (dest.l's rd9); the park aims already landed are this rung half-done.
* **rung 4 — the pool as parameter.** ralloc/rfree → the threaded pool with the
  strictly-below discipline. last, because everything above makes it smaller.

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
