# vmsplice — a template JIT over the tail-threaded VM

Decode a thread into its named handlers, splice their bodies into one function with the
dispatch deleted, compile, install through `nif`. This started as a probe pricing the
shape (`run.sh`, below) and is now an **end-to-end pipeline that runs on live closures**
(`auto.sh`): `dis` reads a compiled closure back to op rows, `compose.l` maps each row to
its op's own C body, mooncc compiles it, `bind.l` links it against the running process,
`check.l` differentials it against the interp twin.

```sh
sh bench/vmsplice/auto.sh          # the pipeline on live closures (samples.l)
sh bench/vmsplice/run.sh           # the original probe (splice.c + the hand-made body.c)
```

Both need `make host` first. Results and the argument live in **doc/moon-regalloc.md**,
"the splice client".

## the pipeline (auto.sh) — dis → compose → mooncc → bind → nif → differential

Every side is ours, so the reflection is direct — no objcopy, no foreign ELF walk, no
`/proc` guessing:

* **`dis`** (in `love/ev.l`, not here) — the emission interface's DUAL: a compiled
  thread read back as `(op-nom operand..)` rows, terminated at the ret family. It is
  built pre-egg from `peek` + the book, exactly like `feels`, so the op-word→name table
  survives the birth mop that deletes those noms. The op roster and the operand/opcode
  discrimination are the image codec's own law (`img_encode`). `test/dis.l` gates it.
* **compose.l** — `dis` a sample, then map each row to its op's C body. The bodies are
  literally macro arguments in `love.c` (`op11(lvm_cup, chainp(Sp[0]) ? B(Sp[0]) :
  ZeroPoint)`), harvested from the source text; a nif name is bridged to its `lvm_` nom
  through `nifs.h`'s array rows. Operands bake as immediates, one room guard leads, the
  glaze's deopt-to-twin and the nif-cell epilogue close it. A row the table cannot say
  DECLINES the sample and names the op — that report is what prices the next extraction
  rung (today `two?`, a non-`op11` nif, is the first uncovered one).
* **bind.l** — rung 2, the relocation step `lift.l` refuses, done IN-PROCESS. mooncc
  emits an external ref as a 7-byte `lea r,[rip+d32]`; `bindcode` flips it to a
  same-length `mov r,[rip+d32]` aimed at an 8-byte cell appended to the blob holding the
  symbol's LIVE address — so every ref is blob-internal and `nif`'s mmap can land the
  bytes anywhere. Symbols come from our own binary's symtab (`/proc/self/exe`, holo
  linked it) plus the load bias from the exe's own `/proc/self/maps` line (matched by
  `readlink` path + zero file-offset, never the first line — that is often an unrelated
  anon `r-xp` region). ⚠ this is a library, not a tool that writes a `.l` for later: a
  separate process has a different ASLR base, so the binding is only valid in the
  process that then nifs it. That is how a real in-process JIT works.
* **check.l** — bind + nif + differential + timing, all in one `love`. The twin is the
  SAME closure (samples.l compiled by ev), so a composed body is never wrong, only
  faster — the differential is `sl-cross` one level down: one denotation, two
  presentations, checked forall inputs.

Measured today (LOVE_NO_GLAZE, short accessor chains): the composed bodies beat their
interp twins ~1.2–1.3×. That is the floor, not the ceiling — the doc's probe shows the
tier is ~4× on a 64-op body, and the whole distance from here to there is mooncc's
op-boundary slot reload (lever 2), which lands with the allocator arc.

## the original probe (run.sh)

| | what it answers |
|---|---|
| `splice.c` | composed vs dispatched, built by **both** compilers. cc is what the shape is worth; mooncc is what we get today. The difference is the lever. |
| `body.c` | does a mooncc-compiled composed body actually install and run in a live `love`, and what does it beat? |

## the pieces

The pipeline half is `samples.l` → `compose.l` → `bind.l` → `check.l`, driven by
`auto.sh` (documented above). The probe half:

* **splice.c** — three lanes over one 64-op sequence: a real thread dispatched the
  normal way, the same bodies spliced, and the spliced ones each carrying a `Have1`.
  Both lanes do an identical chain walk, so the pointer-chase latency is common-mode
  and the difference is the dispatch.
* **body.c** — a self-contained composed body, written so its `.text` carries no
  relocation. `bind.l` lifts that constraint (above); `body.c` keeps it so the probe
  stays a pure dispatch-vs-composed reading.
* **lift.l** — pulls a relocation-free body's bytes out of the `.o` with holo's own
  reader (`ld-read`), writing them as love source pinning `jitcode`. The address-free
  fast path `bind.l` generalizes; `run.sh` still uses it for `body.c`.
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
  process's addresses. That is a linking step nothing here does, and it is the first
  thing a real implementation owes.

Two smaller ones, both cheap to re-learn the hard way: `say` is dyadic, so
`(say out "a" b "\n")` prints only `"a"` and swallows the rest — the juxtaposition
wants its own parens, `(say out ("a" b "\n"))`. And never bind a local named `out`:
that is the stdout port, and shadowing it makes every report vanish silently.
