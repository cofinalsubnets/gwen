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

## the hand-wave, tightened

The category has many objects and fixes one; it is *equivalent* to one object, and the
runtime is the witness. Every cell here is one tagged word, so any two somewheres hold
the same thing: a mov carries a value between them and a mov carries it back — the
identity on the value carried, which is the quotient the two laws above are already read
in. So every object is isomorphic to R, the skeleton is the monoid End(R), and Hom(−,R)
is that monoid acting on itself: precomposition IS the multiplication, contravariance the
direction the product grows. The isos are exactly the relocations — which is why *no
relocation* is a discipline and not a theorem. The movs exist; the design declines to pay
them.

love presents that monoid rather than modelling it:

* the **carrier is the hom-set**. `(f) == f`, apply is total over the kind lattice (a
  stuck application reifies as a held redex, never an error), so a morphism is a value and
  there is nothing to encode.
* the **multiplication is `*`** — `(compose f g x) (f (g x))  ; (* f g)` in prel.l, sealed
  as the mul hook. `(f * g) = (compose f g)` on the nose.
* the **identity is `1`, up to behaviour only**: `((1 * f) = f)` is false and
  `(((f * g) * f) = (f * (g * f)))` is false, while each side answers 13 at 5. `=` is alpha
  + structural; the monoid is its quotient by observation, which no binary decides — the
  quotient's formal vocabulary is already in the tree (uu's `eqrel`/`setquot`, test/uu.l),
  and the corpus pins such laws *applied*, the only honest spelling at the binary. (The
  on-the-nose unit is `()`, whose law preempts every lane: `(() * f)` IS f. `1` is the
  composition band's identity, `()` the whole value space's — two monoids, not one.)
* the **laws are theorems**: test/uuhom.l spells `*`'s composition band in uu (`homcomp`,
  prel's argument order) and checks unit and associativity **by idpath** — conv there is
  NbE with pi-eta, so the equality the binary's `=` declines is the checker's own
  definitional one; a differential foot meets the binary's `*` at a numeral, and the
  lemmas export to Rocq + Lean with the uu corpus (test_uugen / test_uulean).
* **the skeleton is worn there too**: test/uuhom.l models an object as a location over one
  state map, `hmov` as the relocation, and proves carry/frame/retract and that skeleton
  composition IS `homcomp` — then a small machine (state = regs, frame, stack cells, a
  concrete sp) denotes dest.l's IR. tools/dest2uu.l lifts dest.l's own law-site emissions,
  cv and ck both, into uu terms (test/uuhomgen.l, committed + drift-gated by
  test_uuhomgen), and test/uuhomlaw.l proves the five die laws OF THE EMISSIONS: the
  one-si/one-alu collapses, zero set forms under ('br), the tail's jmp, the byte-equal
  `()` lane — and the semantic ones **forall machine states** (migration lands the same
  value at the die; the arg seats agree), each by idpath, a neutral state threading the
  balanced shuttle. dest.l's asserts pin the emissions; these are the same laws as
  theorems, re-certified by both foreign kernels.
* **the stack machine wears it too, and the two meet**: doc/proto/spl.l models CALL vs
  SPLICE in ev's emitter shape (one body compiler, three param readers — the convention's
  seats, a binding splice, a substitution splice), with an effect TRACE beside the value —
  the observation R alone cannot carry. tools/spl2uu.l lifts its threads
  (test/uusplgen.l, drift-gated by test_uusplgen); test/uuspllaw.l proves the splice
  LICENSE forall states — splice ≡ call as value and trace, arg effects before body
  effects — and the flip as a theorem pair: substitution under an effectful arg read
  twice keeps the value equal and doubles the trace, the by-name breach an inliner must
  not ship. ev.l's inliner now ships under it: cprop's bspl lane substitutes a trailing
  effectful arg read exactly once (sl-sub-1 — the call vanishes, no let), and binds each
  remaining impure arg once, in call order, where the lane used to decline whole. And
  `sl-cross` is the cross-machine theorem: one source (y+1), the stack machine's splice
  and the register machine's de5bck, one value, forall states of both — "one design, two
  machines" as a single law.
* the **morphisms have canonical names**: `show` into byte strings, `sound`/`ev` back, with
  test/roundtrip.l's `rd`/`rt` and test/fn.l asserting the return — closures, captures and a
  whole composite. A retract, not an iso: comments and whitespace die on the way back, and a
  mint, cask or port has no section at all. The string monoid the names live in is not
  concatenation (`show` is no `+`-hom); it is the reparsable band under
  `s ⊗ t = show (read s * read t)`.

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
recovery passes (addrfold, cmpfuse, copyprop ..) claw the bridges back after the fact;
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
