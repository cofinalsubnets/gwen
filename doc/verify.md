# verification -- the map

what is machine-checked, how each proof reaches the running binary, and where the
seams are. one line of CLAUDE.md orients ("spec.l STATES and PROVES by assertion,
spec.v PROVES by theorem"); this doc is the whole lattice. the deep dives stay where
they live: doc/holo-verify.md (the encoder ladder), doc/gengc.md (the GC model),
doc/uu-universes.md (uu and the two-kernel bridge).

the posture is a lattice of MODELS plus BRIDGES, not one end-to-end theorem: the
laws are proven in Rocq (axiom-free, house rule -- the build log must say "closed
under the global context"), and the implementation is held against them by three
kinds of bridge. closer to seL4's spec-to-model shape than CakeML's compiler
refinement -- and every rung below is wired into `make test_slow`, regenerated on
every run, so none of it can drift quietly.

## the proofs

| file | what it proves | gate |
|---|---|---|
| proof/rocq/spec.v | the core laws, hand-written: church application, net, subst/beta, the order lattice | test_proof |
| proof/rocq/patch.v | seed's patch groupoid -- the commute laws as theorems | test_proof |
| proof/rocq/gc.v | the generational minor is SOUND (a complete rem-set barrier loses no live young), its PAUSE has its shape (work bounded by the nursery alone; survivor set identical under tenure-blind growth), and its COPY LOOP is a terminating, once-per-object, nothing-lost fixpoint (the drain_* theorems; test_gcheck instance-checks the fixpoint on every minor) | test_gc |
| proof/rocq/gen.v | GENERATED from test/spec.l: the corpus asserts as vm_compute theorems over spec.v's own model | test_gen |
| proof/rocq/uugen.v | GENERATED from test/uu.l: terms uu's kernel checked, re-checked by coqc | test_uugen |
| proof/lean/uugen.lean | the SAME uu corpus through Lean 4 -- a second, unrelated kernel | test_uulean |
| proof/rocq/extract.v | the differential oracle's normalizer BUILT ON spec.v's proven subst/shift, extracted to OCaml | test_extract |
| proof/rocq/big.v | the bignum lane's reference: stdlib binary Z + a PROVEN decimal codec (parse_print), extracted; big_drive fuzzes love's reader/limbs/printer against it | test_big |
| proof/rocq/mx.v | GENERATED from THE TABLE (tools/mx.l), the same love datum love.c's mx.h is laid from: the 256-cell tables factor through the derived band quotient, dispatch commutes, the diagonal reads the lattice | test_mx |
| proof/rocq/enc.v encmem.v encli.v | reference x86-64 encoders, decode inverts encode, byte-identical against holo | test_encver |

every .v holds the axiom audit: no Axiom, no Admitted, no classical/funext escape
hatch. the coqc-shaped gates no-op without a Rocq install (like test_kernel without
qemu), so the fast gate stays green anywhere.

## the three bridges

a theorem about a model proves nothing about love.c until something CONNECTS them.
three connection shapes, each drift-proof by construction:

1. **shared source** -- one text, two checkers. tools/spec2coq.l reads test/spec.l
   and emits gen.v: the same assert runs green on the binary AND closes by
   vm_compute in the model, regenerated every run so they cannot diverge (an
   unmodeled assert is a LOUD skip, never a silent one). tools/uu2coq.l /
   uu2lean.l do the same for proof terms: uu's kernel type-checks each, then two
   independent kernels agree (the de Bruijn criterion, diversified). the analysis
   rides this bridge too: kinds.l's join compiles to uu terms (test/uukind.l) so
   the semilattice laws of the kind lattice are proved at corpus time.

2. **extraction** -- the proven definitions BECOME the reference. extract.v builds
   the oracle's normalizer directly on spec.v's subst/shift and extracts to OCaml;
   oracle_drive fuzzes ev against it extensionally (2000/2000). the hand-
   transcribed twin (test/oracle.l) stays in the fast gate; the extracted one is
   the high-assurance form -- the fuzzer's reference IS the theorems' subject.

3. **differential against a proven model** -- enc.v/encmem.v/encli.v prove their
   encoders correct over the finite domain, extract, and check holo's emission
   BYTE-IDENTICAL. holo is validated against a machine-checked oracle, not a
   trusted disassembler (test_holofuzz is the trusted-disassembler rung below it).

## the trusted base, honestly

coqc and lean are trusted (two unrelated kernels, so a bug must strike twice the
same way). the generators (spec2coq.l, uu2coq.l, uu2lean.l) run ON love -- a
circularity, mitigated by keeping them small and auditable and by the second
kernel. the C core and both compilers are UNPROVEN; they are held by the bridges
above plus the corpus on every target. state it this way or not at all.

on the C core specifically (~9k lines), the two levers rank clearly: SHRINKING
is near its floor (what remains -- allocator, collector, dispatch, VM loop,
limbs, reader/printer, c0, nifs -- is what can't leave; the big shrink already
happened a layer down, when moon + holo pushed gcc/glibc/ld out), so the work
is VERIFYING pieces against references, the encoder-ladder shape. the bignum
lane is bridged now (big.v -- it caught abs-of-INTPTR_MIN wrapping on its first
run), the +/* dispatch matrices are owned as data (mx.v -- band factorization,
dispatch commutativity, the diagonal-is-the-lattice reading), and as of clay
rung 2 they are owned in the OTHER direction too: tools/mx.l is the table, the
C (mx.h) and the model are both generated from it, and love.c has its first
generated region, and the GC copy loop has its theorems (gc.v's drain_*:
termination, once-per-object, nothing lost, a true fixpoint -- test_gcheck
re-drives the whole minor scan on a debug build and traps if a second pass
copies a word; the guard is sabotage-proven). floats ROUND-TRIP exactly now:
the printer is shortest-roundtrip (exact Steele & White in love.c) and the
reader is correctly rounded (am_strtod in the math floor, every frontend --
glibc's strtod left the trusted base), spec.l pins the classic faces. the
next such rung is the round-trip on the rest of the data grammar (big.v
covers the decimal integers; strings/symbols/lists remain).

## the mutator's side of the collector -- GCDBG, and the lane that is half built

`test_gcheck` checks the COLLECTOR. The sibling question is the MUTATOR's: a raw C
pointer held across a call that may collect. It is the costliest recurring bug shape
in the C core (`obin_elem` held `g->ip` across `ai_big_binop` for years and surfaced
only when an unrelated 24-byte struct shrink moved allocation), and nothing in the
tree looked for it. `AI_GC_STRESS` is the instrument: `ai_have` -- the tree's own
phrase for *"this call may collect"* -- stops being a maybe and always collects, the
vacated nursery is poisoned so a stale read faults instead of reading plausible data,
and every 32nd collection is a MAJOR so tenured objects move too.

**⚠ the flag knob is `GCDBG`, not `EXTRA_CFLAGS`, and that distinction is a bug fix.**
`EXTRA_CFLAGS` rides `$(ai_cflags)`, which the mooncc recipes do not use -- and the
default `love` has been mooncc-built since self-host rung 2. So
`EXTRA_CFLAGS=-DAI_GC_CHECK` compiled love.c **clean** and ran the corpus on a binary
that had never had the check in it: **`test_gcheck` was answering green on a question
it was not asking, and had been since the mooncc flip.** `GCDBG` reaches both
compilers and, deliberately, NOT `love0` -- love0 is shared and unsuffixed
(`out/host/0`), so a flag that reaches it leaks out of the debug lane's own tree, and
a stress-built love0 segfaults baking `mooncc0.image`.

**What is verified.** The lane detects. The control is `ai_big_binop`'s own re-fetch
(`a = g->sp[0], b = g->sp[1]; // re-fetch (ai_have may have GC'd)`): delete that one
line and the normal build answers a 163-digit bignum sum correctly and passes the
whole corpus -- **latent, exactly as obin_elem was** -- while the stress build
segfaults on the first big + big. Restore it and the stress build is clean again.
It also found a real bug on its first boot: `intern` evaluated `intern_reserve(g)` as
an ARGUMENT to `ai_have`, so a scare arriving from the caller was dereferenced rather
than propagated -- an OOM at startup segfaulted instead of scaring. Fixed.

**⚠ what is NOT verified, and why there is no `test_gcstress` target.** The lane runs
a targeted program; it does not yet run the corpus. A stress build boots (16.9 s) and
then **exits 0 having evaluated nothing at all** -- `-e '(quit 7)'` exits 0. Silent,
green, and wrong, which is the one gate result worth less than none. The suspicion is
the uncommitted heap gap (the argv marshalling lays bytes in it and the gap's contract
is "consumed before anything allocates again", which forcing a collection at every
`ai_have` breaks by construction) -- suspicion, not a finding: it has not been traced.
Until it is, this is an instrument you point at a program by hand, not a gate.

Three measurements worth not re-deriving:

* **a minor is not enough.** Collecting at every `ai_have` tenures everything almost
  at once, so a minor -- which only moves the young -- moves nothing by the time the
  stale local's collection lands. The detector answered green on its own control
  until the periodic major went in.
* **every collection being a major is not shippable**: a bare boot took 458 s (and
  crashed). Every 32nd is 17-24 s. Coverage is the trade and it is stated in the code.
* **poisoning the old major half costs more than ten minutes on that same boot**, and
  is the half least needed: a Cheney copy already overwrites word0 -- the ap -- of
  every source object with the forwarding pointer, which is precisely how obin_elem
  announced itself. Only the vacated young is poisoned.

## the open seams, ranked

one already closed sets the pattern: test/host/gcpause.l MEASURES the minor flat
in the live set (the worst minor bit-identical across 3.7x tenured growth;
gauge[14]/[15] carry the peaks), and gc.v's minor_work_bounded / minor_flat state
the same shape as theorems -- the gauge is the instance-check. the pattern to
repeat: a gate that measures a shape earns a theorem that OWNS the shape.

1. **the evaluator refinement gap.** ev + the glaze touch the proofs only through
   gen.v's asserts and the oracle fuzz. the realistic work is WIDENING both:
   more spec.l sections through spec2coq (the skip list names what's unmodeled;
   two 2026-07-27 widenings took it 114 -> 235 -> 300 of 654 -- first the unit
   lane, text as spelling, word shifts, list functions with their function
   arguments, predicates as charms; then the gaussian twins (exact complex over
   Z, sign and order lex on (re,im)), lambda alpha-equality on de Bruijn terms,
   and id?/unit-form verdicts), richer term generation for the extracted oracle.
   still skipped: floats (honest), the special forms with real bindings, ports/
   casks/macros. a small-step machine that spec.v's semantics refines to is the
   ambitious form.
2. **moon is the largest unproven trusted component** -- it compiles love.c and
   most of a userland now, checked only by the corpus gates. the encoder ladder
   covers holo's emission, not moon's selection or regalloc. the honest near-term
   move is differential (two-binary tree diff, judge.l) aimed at moon-vs-gcc
   output, not a compiler proof.
3. **the encoder ladder's next rungs** (doc/holo-verify.md): indexed addressing,
   sized loads, arm64.
4. **uu whole-corpus integration** -- every corpus file into both export lists;
   mechanical, unfinished.
5. **the mutator's side of the collector** -- `AI_GC_STRESS` detects the class and
   has caught one real bug, but cannot run the corpus yet (above). Finishing it is
   the cheapest remaining rung here: one silent stop to trace, and it becomes a gate.
