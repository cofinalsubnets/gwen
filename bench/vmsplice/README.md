# vmsplice — what the slot gap costs a splice JIT

A probe, not a gate. It prices one shape: **a template JIT over the tail-threaded VM** —
decode a thread into its named handlers, splice their bodies into one function with the
dispatch deleted, compile, install through `nif`. The pointer→name map it would need
already exists (`def1` + `image_ap_index`, which the image codec depends on).

```sh
sh bench/vmsplice/run.sh          # from the repo root; needs `make host` first
```

The reading, and why both halves are here:

| | what it answers |
|---|---|
| `splice.c` | composed vs dispatched, built by **both** compilers. cc is what the shape is worth; mooncc is what we get today. The difference is the lever. |
| `body.c` | does a mooncc-compiled composed body actually install and run in a live `love`, and what does it beat? |

Results and the argument live in **doc/moon-regalloc.md**, "the splice client".

## the pieces

* **splice.c** — three lanes over one 64-op sequence: a real thread dispatched the
  normal way, the same bodies spliced, and the spliced ones each carrying a `Have1`.
  Both lanes do an identical chain walk, so the pointer-chase latency is common-mode
  and the difference is the dispatch.
* **body.c** — a self-contained composed body, written so its `.text` carries no
  relocation. That constraint is the point, not a convenience (below).
* **lift.l** — pulls the body's bytes out of the `.o` with holo's own reader
  (`ld-read`), and writes them as love source pinning `jitcode`. No objcopy, no
  foreign ELF walk.
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
