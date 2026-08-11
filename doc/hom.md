# hom — the one compiler design under c0, ev, and moon

Three compilers live in this tree: c0 (love.c's bootstrap), ev (love/ev.l, the
self-hosted twin), and moon's gen.l (the C compiler). The first two were built to one
design; the moon arc is teaching the third to wear it. This page is that design said
once, so the seam comments can point somewhere instead of each re-gesturing at it.

## the shape

Hand-wave a category whose objects are "a value, somewhere" — an argument slot, a
register, a stack cell, a label about to be jumped to — and whose morphisms are code:
a run of instructions that takes a value from one somewhere to another. Fix one object
and call it **R: a returned value**. Concretely R is the calling convention's answer —
in the VM, a value placed at top of a stack restored to the position before the call
(love.h's `Answer(v)`: `Sp[0] = v`, continue at `Ip+1`); in moon's ABI, r0.

A compiler in this design never asks "what value does expression e produce" but "what
morphism into R does e become". It works in the codomain of the contravariant functor
Hom(−, R): the *continuation* — the rest of the code between e's value and the
function's answer — is a morphism k into R, and compiling e means **precomposing**
onto k. Contravariance is why the thread builds backward: you hold the consumer in
hand and grow the composite toward its source. Two laws fall out for free:

* **the tail law**: when k is the identity on R (nothing between this value and the
  answer), a call composes to a jump — there is no code to return through. Not an
  optimization; a consequence of asking for a morphism rather than a value.
* **no relocation**: a value is never produced *somewhere* and then moved to where the
  consumer wanted it. The consumer was in the composition from the start.

## worn by the stack machine (c0 and ev)

ev's emitters are curried `(\ k n ..)` — k the continuation emitter, the thread built
backward by prepending (`poke -1`), terminated at Answer. c0 is the same design in C
under its own names: `ana` walks the source pushing `cata` emitters, `pull` pops the
next one and calls it — the continuation literally sits on the stack. The tail law is
`kap` peeping its continuation for `lvm_ret` and emitting `lvm_tap` instead — which is
what `make vmret` then checks in the emitted binary (aps tail-jump, never return).
The seam comment: love/ev.l, "THE EMISSION INTERFACE".

## worn by the register machine (moon)

gen.l grew up on the other protocol: every expression delivers into r0 and the
consumer relocates — Hom(−, r0) with every composite forced through one representative
object, paying a bridge (a mov, an sp cell, a 0/1 reify-and-retest) at each seam. The
recovery passes (addrfold, cmpfuse, dehusk ..) claw the bridges back after the fact;
the residency layers (vmap, homes, cspool) shrink how often a value must travel at all.

The migration passes the consumer down instead, as a **destination die** — the
continuation defunctionalized, which C affords because an expression's consumers form
a small closed set:

    ('reg r)     deliver into r — a home, a cs seat, an argument seat
    ('mem b o)   deliver through a store
    ('br lt lf)  a control destination: compares branch, && threads labels, no 0/1
    'tail        the identity on R — a call here is a sibcall by construction
    ()           the degenerate die: r0, today's protocol unchanged

Defunctionalized rather than closures as in ev, because gen.l must emit *named* IR
forms — the recovery passes and unframe read them, and a dark op is barred. The `()`
die is what makes lane-by-lane migration sound: an unmigrated consumer keeps reading
r0 and never notices. gen.l's `g 'want` hint is the die's advisory shadow (a consumer
pins it, the top node honors or declines, a decline costs one bridge); each migration
rung promotes the hint toward the contract. All four lanes have landed — asn/decl,
the cbranch compare, the call's arg seats, the bin value park (doc/moon-regalloc.md,
lever 2 and the rung ledger) — closing the emission side; what remains of the die
rides the allocator leg. The gap the rungs close is scored in doc/moon-diff.md.

The literature calls the register-machine face *destination-driven code generation*
(Dybvig & Burger, 1990). The runnable model — both protocols over one AST, the
before/after shapes law-pinned — is doc/proto/dest.l, gated by test_doc. The plan for
migrating gen.l's *shape* (die as parameter, no register column, threaded pool) is
doc/moon-hom.md.
