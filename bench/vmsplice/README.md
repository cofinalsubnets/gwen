# vmsplice — a template JIT over the tail-threaded VM

Decode a thread into its named handlers, splice their bodies into one function with the
dispatch deleted, install through `nif`. This started as a probe pricing the shape
(`run.sh`, below); **the JIT itself is now `lib/splice.l`, a module with one door** —
`(use 'splice)` then `(jit f)`, which answers a native closure agreeing with `f`, or `()`
naming the row it could not say. What is left here is the pricing.

**It needs no compiler.** Each op's own machine-form IR is in the binary: `mooncc -fir=lvm_`
writes it to `.rodata` at compile time, `nifs.l` lays the book-name bridge beside it, and holo
assembles the spliced result. No source tree, no object file, no temporary anything.

```sh
make test_splice                   # the gate: jit the samples, differential each against its twin
LOVE_NO_GLAZE=1 out/host/love \
  -l bench/vmsplice/samples.l bench/vmsplice/auto.l    # the same, timed
sh bench/vmsplice/run.sh           # the original probe (splice.c + the hand-made body.c)
```

All need `make host` first. Results and the argument live in **doc/moon-regalloc.md**: "the splice client" for the numbers, "the splice JIT
and the moon arc" for why this and the compiler's residency work are one problem, and THE LADDER
for the shared plan (steps 4 and 7 are the ones this asks for).

## the door — dis → splice → holo → nif, in one process and with no compiler

Every side is ours, so the reflection is direct — no objcopy, no foreign ELF walk, no
`/proc` guessing. `lib/splice.l` carries all of it; the pieces named here are its sections.

* **`dis`** (in `love/ev.l`, not here) — the emission interface's DUAL: a compiled
  thread read back as `(op-nom operand..)` rows, terminated at the ret family. It is
  built pre-egg from `peek` + the book, exactly like `feels`, so the op-word→name table
  survives the birth mop that deletes those noms. The op roster and the operand/opcode
  discrimination are the image codec's own law (`img_encode`). `test/dis.l` gates it.
* **the splicer** — `dis` a closure, then take each row's op IR out of `ai_lvm_ir`, strip the
  fixed dispatch triple off it, rename its internal labels for that copy, and lay the bodies
  end to end. The VM's convention is why nothing needs rewriting: `g=r6, Ip=r5, Hp=r2, Sp=r1`,
  every op takes its argument from `Sp[0]` and leaves its answer there, so two bodies already
  agree about everything. The load family is *said* rather than spliced (three forms), and the
  fld family is **unfused** into a push and an op — the fusion is the VM's, not the meaning's.
  ⚠ **the splice condition is checked, not assumed**: a body qualifies only if it touches no
  frame, never reads `Ip`, and leaves itself nowhere. That last one is the sharp edge — a
  handler's own room guard jumps to `lvm_gc`, and gc RESUMES AT `Ip`, which in a spliced body
  is the nif cell, so a collection halfway through would re-run the ops that already ran. One
  hoisted guard leads instead, deopting to the twin. A row that fails declines by name.
* **the reflow** — the seam, removed where it is two lines instead of an allocator. A handler
  DELIVERS with `(st r1 0 rX)` and the next one OPENS by reading it straight back with
  `(ld rY r1 0)`; laid end to end those are adjacent, one base, one name, nothing between. That
  is `stldp`'s law with none of its aliasing question, because both sides are ours. This is the
  op-boundary reload the whole arc is about.
* **no relocations at all**, because a JIT knows the answers: the one external reference a clean
  handler carries is `(la rX sym)`, and at runtime that symbol's live address is a *number* — so
  it becomes `(li rX addr)`, a movabs, and the body is position-independent. Symbols come from
  our own binary's symtab (`/proc/self/exe`, holo linked it) plus the load bias from the exe's
  own `/proc/self/maps` line (matched by `readlink` path + zero file-offset, never the first
  line — that is often an unrelated anon `r-xp` region). ⚠ **in-process by construction**: a
  separate process has a different ASLR base, so the bytes are valid only in the love that made
  them. That is what a JIT is, and why none of this can be written out for later.
* **the differential** (`test/gate/splice.l`) — the twin is the SAME closure, so a composed
  body is never wrong, only faster: `sl-cross` one level down, one denotation and two
  presentations, checked forall inputs. A disagreement is a mooncc codegen bug, a bad
  extraction or a bad splice, and all three are silent otherwise.

Measured today (LOVE_NO_GLAZE): ~1.2× on the short accessor chains, **1.6× on a 32-op body**,
where a mooncc-compiled composed body of the same closure also reads 1.6× — the splicer matches
the compiler's own output while needing none of it. The probe shows the tier is ~4× on a 64-op
body; the distance from here to there is now the splicer's own business (keeping `Sp[0]` in a
register across a whole segment rather than storing and reloading at each op), not the compiler's.

## the original probe (run.sh)

| | what it answers |
|---|---|
| `splice.c` | composed vs dispatched, built by **both** compilers. cc is what the shape is worth; mooncc is what we get today. The difference is the lever. |
| `body.c` | does a mooncc-compiled composed body actually install and run in a live `love`, and what does it beat? |

## the pieces

The JIT half is `lib/splice.l` (the door), `test/gate/splice.l` (the differential) and
`samples.l` → `auto.l` (the timing) — plus `mooncc -fir=lvm_` and `nifs.l`'s bridge, which
put the IR in the binary in the first place. The probe half:

* **splice.c** — three lanes over one 64-op sequence: a real thread dispatched the
  normal way, the same bodies spliced, and the spliced ones each carrying a `Have1`.
  Both lanes do an identical chain walk, so the pointer-chase latency is common-mode
  and the difference is the dispatch.
* **body.c** — a self-contained composed body, written so its `.text` carries no
  relocation, which keeps the probe a pure dispatch-vs-composed reading.
* **lift.l** — pulls a relocation-free body's bytes out of the `.o` with holo's own
  reader (`ld-read`), writing them as love source pinning `jitcode`. `run.sh` uses it for
  `body.c`; the JIT proper never needs it, since it assembles its own bytes.
* **install.l** — `nif`s the bytes, checks the native agrees with its interp twin on
  five inputs, then times both.

## the traps, all three paid for once

* ⚠ **a folding microbenchmark measures folding.** The first version of `splice.c` used
  `cap;charmp` over a charm — both constant on that input — and reported **60×** at
  0.023 ns/op, under one cycle: the whole body had collapsed into its own answer. Every
  operand now reaches through a `volatile` seed and a real heap chain. Corollary worth
  keeping: splicing *does* let the optimizer see across op boundaries — cc folds the
  64-bump arithmetic body to **30 bytes**, one `add`, against mooncc's 1299 — which is
  real value and exactly why the probe has to be built not to benefit from it.
* ⚠ **`LOVE_NO_GLAZE=1` or the baseline is not the interpreter.** `install.l`'s twin is
  an arithmetic closure, the shape `love/glaze/auto.l` recognizes. A default run times
  the glaze and reads ~9× too fast. `run.sh` prints both rows on purpose: the glaze is
  the specializing tier a splice JIT would sit *under*, not its competition.
* ⚠ **a relocation-free body is a written constraint, not luck.** `lift.l` refuses a
  body carrying one. The moment a body touches chains or can collect it pulls in
  `lvm_chain`, `lvm_sym`, `ai_please` — references that must bind to the **live**
  process's addresses. That is a linking step `lift.l` does not do — and the JIT proper
  never has the problem, because it assembles those references as absolute immediates it
  looks up at splice time.

Two smaller ones, both cheap to re-learn the hard way: `say` is dyadic, so
`(say out "a" b "\n")` prints only `"a"` and swallows the rest — the juxtaposition
wants its own parens, `(say out ("a" b "\n"))`. And never bind a local named `out`:
that is the stdout port, and shadowing it makes every report vanish silently.
