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
refinement -- and every rung below is wired into `make test_all`, regenerated on
every run, so none of it can drift quietly.

## the proofs

| file | what it proves | gate |
|---|---|---|
| proof/rocq/spec.v | the core laws, hand-written: church application, net, subst/beta, the order lattice | test_proof |
| proof/rocq/patch.v | seed's patch groupoid -- the commute laws as theorems | test_proof |
| proof/rocq/gc.v | the generational minor is SOUND (a complete rem-set barrier loses no live young) and its PAUSE has its shape (work bounded by the nursery alone; survivor set identical under tenure-blind growth) | test_gc |
| proof/rocq/gen.v | GENERATED from test/spec.l: the corpus asserts as vm_compute theorems over spec.v's own model | test_gen |
| proof/rocq/uugen.v | GENERATED from test/uu.l: terms uu's kernel checked, re-checked by coqc | test_uugen |
| proof/lean/uugen.lean | the SAME uu corpus through Lean 4 -- a second, unrelated kernel | test_uulean |
| proof/rocq/extract.v | the differential oracle's normalizer BUILT ON spec.v's proven subst/shift, extracted to OCaml | test_extract |
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

## the open seams, ranked

one already closed sets the pattern: test/host/gcpause.l MEASURES the minor flat
in the live set (the worst minor bit-identical across 3.7x tenured growth;
gauge[14]/[15] carry the peaks), and gc.v's minor_work_bounded / minor_flat state
the same shape as theorems -- the gauge is the instance-check. the pattern to
repeat: a gate that measures a shape earns a theorem that OWNS the shape.

1. **the evaluator refinement gap.** ev + the glaze touch the proofs only through
   gen.v's asserts and the oracle fuzz. the realistic work is WIDENING both:
   more spec.l sections through spec2coq (the skip list names what's unmodeled),
   richer term generation for the extracted oracle. a small-step machine that
   spec.v's semantics refines to is the ambitious form.
2. **moon is the largest unproven trusted component** -- it compiles love.c and
   most of a userland now, checked only by the corpus gates. the encoder ladder
   covers holo's emission, not moon's selection or regalloc. the honest near-term
   move is differential (two-binary tree diff, judge.l) aimed at moon-vs-gcc
   output, not a compiler proof.
3. **the encoder ladder's next rungs** (doc/holo-verify.md): indexed addressing,
   sized loads, arm64.
4. **uu whole-corpus integration** -- every corpus file into both export lists;
   mechanical, unfinished.
