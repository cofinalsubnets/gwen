# verification — the map

What is machine-checked, how each proof reaches the running binary, and where the seams are.
One line of CLAUDE.md orients ("spec.l STATES and PROVES by assertion, spec.v PROVES by
theorem"); this doc is the whole lattice. The deep dives stay where they live:
doc/holo-verify.md (the encoder ladder), doc/gengc.md (the GC model), doc/uu-universes.md (uu
and the two-kernel bridge).

The posture is a lattice of MODELS plus BRIDGES, not one end-to-end theorem: the laws are proven
in Rocq (axiom-free, house rule — the build log must say "closed under the global context"), and
the implementation is held against them by three kinds of bridge. Closer to seL4's
spec-to-model shape than CakeML's compiler refinement — and every rung below is wired into
`make test_slow`, regenerated on every run, so none of it can drift quietly.

## the proofs

| file | what it proves | gate |
|---|---|---|
| proof/rocq/spec.v | the core laws, hand-written: church application, net, subst/beta, the order lattice, the faces (doc/faces.md) | test_proof |
| proof/rocq/patch.v | seed's patch groupoid — the commute laws as theorems | test_proof |
| proof/rocq/gc.v | the generational minor is SOUND (a complete rem-set barrier loses no live young), its PAUSE has its shape (work bounded by the nursery alone; survivor set identical under tenure-blind growth), and its COPY LOOP is a terminating, once-per-object, nothing-lost fixpoint (the drain_* theorems; test_gcheck instance-checks the fixpoint on every minor) | test_gc |
| proof/rocq/gen.v | GENERATED from test/spec.l: the corpus asserts as vm_compute theorems over spec.v's own model | test_gen |
| proof/rocq/uugen.v | GENERATED from test/uu.l: terms uu's kernel checked, re-checked by coqc | test_uugen |
| proof/lean/uugen.lean | the SAME uu corpus through Lean 4 — a second, unrelated kernel | test_uulean |
| proof/rocq/extract.v | the differential oracle's normalizer BUILT ON spec.v's proven subst/shift, extracted to OCaml | test_extract |
| proof/rocq/big.v | the bignum lane's reference: stdlib binary Z + a PROVEN decimal codec (parse_print), extracted; big_drive fuzzes love's reader/limbs/printer against it | test_big |
| proof/rocq/mx.v | GENERATED from THE TABLE (tools/mx.l), the same love datum love.c's mx.h is laid from: the 256-cell tables factor through the derived band quotient, dispatch commutes, the diagonal reads the lattice | test_mx |
| proof/rocq/enc.v encmem.v encli.v | reference x86-64 encoders, decode inverts encode, byte-identical against holo | test_encver |

Every `.v` holds the axiom audit: no `Axiom`, no `Admitted`, no classical/funext escape hatch.
The coqc-shaped gates no-op without a Rocq install (like test_kernel without qemu), so the fast
gate stays green anywhere.

## the three bridges

A theorem about a model proves nothing about love.c until something CONNECTS them. Three
connection shapes, each drift-proof by construction:

1. **shared source** — one text, two checkers. `tools/spec2coq.l` reads test/spec.l and emits
   gen.v: the same assert runs green on the binary AND closes by `vm_compute` in the model,
   regenerated every run so they cannot diverge (an unmodeled assert is a LOUD skip, never a
   silent one). `tools/uu2coq.l` / `uu2lean.l` do the same for proof terms: uu's kernel
   type-checks each, then two independent kernels agree (the de Bruijn criterion, diversified).
   The analysis rides this bridge too: kinds.l's join compiles to uu terms (test/uukind.l) so
   the semilattice laws of the kind lattice are proved at corpus time.

2. **extraction** — the proven definitions BECOME the reference. extract.v builds the oracle's
   normalizer directly on spec.v's subst/shift and extracts to OCaml; `oracle_drive` fuzzes ev
   against it extensionally. The hand-transcribed twin (test/oracle.l) stays in the fast gate;
   the extracted one is the high-assurance form — the fuzzer's reference IS the theorems'
   subject.

3. **differential against a proven model** — enc.v/encmem.v/encli.v prove their encoders correct
   over the finite domain, extract, and check holo's emission BYTE-IDENTICAL. holo is validated
   against a machine-checked oracle, not a trusted disassembler (test_holofuzz is the
   trusted-disassembler rung below it).

## the trusted base, honestly

coqc and lean are trusted (two unrelated kernels, so a bug must strike twice the same way). The
generators (spec2coq.l, uu2coq.l, uu2lean.l) run ON love — a circularity, mitigated by keeping
them small and auditable and by the second kernel. The C core and both compilers are UNPROVEN;
they are held by the bridges above plus the corpus on every target. **State it this way or not
at all.**

On the C core specifically (~9k lines), the two levers rank clearly. SHRINKING is near its floor
— what remains (allocator, collector, dispatch, VM loop, limbs, reader/printer, c0, nifs) is
what cannot leave, and the big shrink already happened a layer down, when moon + holo pushed
gcc/glibc/ld out. So the work is VERIFYING pieces against references, the encoder-ladder shape:

- the bignum lane is bridged (big.v);
- the `+`/`*` dispatch matrices are owned as data (mx.v — band factorization, dispatch
  commutativity, the diagonal-is-the-lattice reading) and owned in the OTHER direction too:
  `tools/mx.l` is the table, the C (mx.h) and the model are both generated from it, so love.c
  has a generated region;
- the GC copy loop has its theorems (gc.v's `drain_*`), with test_gcheck re-driving the whole
  minor scan on a debug build and trapping if a second pass copies a word — a guard that is
  sabotage-proven;
- floats round-trip exactly: the printer is shortest-roundtrip (exact Steele & White in love.c)
  and the reader is correctly rounded (`am_strtod` in the math floor, every frontend — glibc's
  strtod left the trusted base), with spec.l pinning the classic faces.

The next such rung is the round-trip on the rest of the data grammar — big.v covers the decimal
integers; strings/symbols/lists remain.

## the mutator's side of the collector — `AI_GC_STRESS`

`test_gcheck` checks the COLLECTOR. The sibling question is the MUTATOR's: a raw C pointer held
across a call that may collect. It is the costliest recurring bug shape in the C core, and
nothing else in the tree looks for it.

`AI_GC_STRESS` is the instrument: `ai_have` — the tree's own phrase for *"this call may
collect"* — stops being a maybe and always collects, the vacated nursery is poisoned so a stale
read faults instead of reading plausible data, and every 32nd collection is a MAJOR so tenured
objects move too.

⚠ **The flag knob is `GCDBG`, not `EXTRA_CFLAGS`.** `EXTRA_CFLAGS` rides `$(ai_cflags)`, which
the mooncc recipes do not use — and the default `love` is mooncc-built. So
`EXTRA_CFLAGS=-DAI_GC_CHECK` compiles love.c **clean** and runs the corpus on a binary that never
had the check in it: green on a question it was not asking. `GCDBG` reaches both compilers and,
deliberately, NOT `love0` — love0 is shared and unsuffixed (`out/host/0`), so a flag that reaches
it leaks out of the debug lane's own tree, and a stress-built love0 segfaults baking
`mooncc0.image`.

**The gate is `test_gcstress`, and it rides `test_slow`**: the whole corpus walks the stress
build, a few minutes of wall behind a cold egg boot. Sabotage-proven — delete the `gen_wb` in
`obin_run` and the gate reddens on exactly the array-of-bignums assert that finds it.

⚠ **What it does NOT reach.** `ai_have` is the whole door. `Have(n)` — the op-boundary macro
inside an `lvm_` frame — does not route through it, and does not need to: the op re-runs, so
that lane is safe by construction. But it means the stress lane says nothing about a raw pointer
held across a `Have`, and nothing at all about the frontends' own allocation (host/*.c is
compiled into the stress build, but nothing drives its rarer lanes).

**Three measurements worth not re-deriving:**

* **a minor is not enough.** Collecting at every `ai_have` tenures everything almost at once, so
  a minor — which only moves the young — moves nothing by the time the stale local's collection
  lands. The detector answers green on its own control until the periodic major goes in.
* **every collection being a major is not shippable**: a bare boot took 458 s. Every 32nd is
  17-24 s. Coverage is the trade and it is stated in the code.
* **poisoning the old major half costs more than ten minutes on that same boot**, and is the
  half least needed: a Cheney copy already overwrites word0 — the ap — of every source object
  with the forwarding pointer, which is precisely how a stale pointer announces itself. Only the
  vacated young is poisoned.

**The C shapes it finds, so they are recognized on sight:**

* **a live value handed to a definition helper that allocates.** `ai_defn` interns a name, which
  allocates, so any value it holds goes stale; and it reads its table one entry at a time, so
  even a two-entry call goes stale between its own definitions. ⚠ **There is no ordering that
  fixes this** — rooting up front only moves the hazard into the pushes (`ai_push` collects when
  the stack is short and leaves every entry it has not reached yet exactly as stale, rarely and
  only under memory pressure, which `AI_GC_STRESS` cannot see at all), and reserving the room up
  front collects before the first read. C cannot re-root what it holds in an array. So `ai_defn`
  keeps its real contract — **immortal values only** — and a live value arrives on the STACK
  instead, through `ai_defv`, where the collector finds and updates it.
* **an elementwise loop storing into a promoted array with no write barrier.** A lane that
  allocates per element can tenure the result array mid-loop while the elements it is being
  filled with stay young — an old→young edge nothing remembered. Re-fetching the array keeps the
  STORE landing in the right place; it does not make the stored edge visible. ⚠ This one is
  reachable with no flag at all, at a small `LOVE_BUDGET_MB`.
* **a printer reading through a bare C word.** Emitting a byte grows the port and may collect, so
  every `ioput_*` parks its value on the stack rather than reading fields off a raw local.
* **an allocation guard evaluated as an ARGUMENT.** `intern` computing its reserve inside the
  `ai_have` call dereferenced a scare arriving from the caller rather than propagating it, so an
  OOM at startup segfaulted instead of scaring.

The control for the instrument itself is `ai_big_binop`'s own re-fetch after `ai_have`: delete
that one line and the normal build answers a 163-digit bignum sum correctly and passes the whole
corpus — latent — while the stress build segfaults on the first big + big.

⚠ **The first face of a stale root can be a SILENT STOP, not a crash.** A stress build that
boots and exits 0 having evaluated nothing is the argv lane reading a value that is no longer
there. The way to it was noticing that the STDIN lane worked while every lane reading `argv` did
nothing — and the way *not* to it was a plausible suspicion about the heap gap. **Record a
suspicion as a suspicion**: as a finding it sends the next reader to the wrong file.

## the open seams, ranked

The pattern to repeat, set by the one already closed: **a gate that measures a shape earns a
theorem that OWNS the shape.** test/host/gcpause.l measures the minor flat in the live set (the
worst minor bit-identical across 3.7× tenured growth; `gauge[14]`/`[15]` carry the peaks), and
gc.v's `minor_work_bounded`/`minor_flat` state the same shape as theorems — the gauge is the
instance-check.

1. **the evaluator refinement gap.** ev + the glaze touch the proofs only through gen.v's
   asserts and the oracle fuzz. The realistic work is WIDENING both: more spec.l sections
   through spec2coq (the skip list names what is unmodeled), and richer term generation for the
   extracted oracle. Still skipped: floats (honest), the special forms with real bindings,
   ports/casks/macros. A small-step machine that spec.v's semantics refines to is the ambitious
   form.
2. **moon is the largest unproven trusted component** — it compiles love.c and most of a
   userland, checked only by the corpus gates. The encoder ladder covers holo's emission, not
   moon's selection or regalloc. The honest near-term move is differential (two-binary tree
   diff, `judge.l`) aimed at moon-vs-gcc output, not a compiler proof.
3. **the encoder ladder's next rungs** (doc/holo-verify.md): indexed addressing, sized loads,
   arm64.
4. **uu whole-corpus integration** — every corpus file into both export lists; mechanical,
   unfinished.
