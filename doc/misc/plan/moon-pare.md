# plan: paring gen.l back

`crew/moon/gen.l` went 4,150 → 8,532 lines between 2026-07-19 and 2026-08-16, and the
corpus row did not move across that span (1.21× clang on 2026-08-11, 1.19× on 2026-08-16).
That reads as a month spent for nothing, and it is not what happened — chacha went ~23× →
5.34× over the same period, which the corpus under-weights by construction. What actually
went wrong is smaller and fixable: **the arc was steered by instruction counts on a core
that hides instructions**, and the layer it was tuning turned out to hold two mechanisms
with opposite economics. doc/misc/moon-gauge.md has both measurements — and, revised
2026-08-22, the ROTATE finding with its md5 control, which this ladder now carries as
rung 1 because it contaminates every row the later rungs read.

⚠ **this is not a plan to delete register residency.** Ablated whole it costs +12.5% of
corpus cycles and moves mooncc 1.19× → ~1.34× against clang. The layer earns its place.
What is unjustified is the *lines per cycle bought*, and that is what this pares.

## the criterion (from the regalloc ledger, kept)

> did the program stop guessing something? did a mechanism come out? is performance not
> badly regressed (a floor, not a payment)?

To which this plan adds one term, because it is what went missing: **and was the price read
in cycles, on both ccbench rows.**

⚠ before proposing any mechanism, read the refusals list — priced, closed, do not rebuild.
It is out of the tree with the rest of the archive: `git show 14dc955c~1:doc/moon-regalloc.md`.
One entry there was already built twice.

## the ladder

- **rung 0 — the ablation becomes a knob, and the knob becomes a harness.** Every
  measurement in doc/misc/moon-gauge.md came from hand-editing one binding and rebuilding.
  The timing half arrived 2026-08-22: `make -C bench ccnif` reads the nif floors straight in
  ~20 s and is the per-edit instrument; ccbench stays the per-rung one. Still owed: make
  each mechanism switchable without editing gen.l, and script the pricing — `test_fixpoint`
  for correctness, `perf` cycles for the number, both ccbench rows for the shape. ⚠ the
  read must happen at RUNTIME — a plain value folds eagerly at bake time (gen.l's own first
  binding says so), so the knob would carry the baking session's environment. ⚠ and
  `test_fixpoint` is a by-name gate now (retired from the slow roster), so the harness
  invokes it by name or the correctness leg silently doesn't run.
- **rung 1 — the rotate lands, and the pair stops lying. LANDED 2026-08-22.** The
  recognizer is in gen.l (constant and spliced-variable counts, both directions, both
  widths; `ror4`/`rorv`/`rorv4` joined holo on x64 + arm64), pinned by law, by
  `test/cc/152-rotate.c` on three targets, and the fixpoint. The re-fill answered the
  confound: sha256 4.23× → 2.90×, chacha 5.80× → 3.59×, every rotate-free row inside
  noise — **both hypotheses were true, each owning a row**, and the pair reads array vs
  scalar again. moon-gauge's *the rotate lands* has the tables. Still open from this
  rung's teardown, the named next lever: an inlined body does not constant-propagate,
  so hash.c's rotates ride `%cl` off a frame slot where gcc folds an immediate.
- **rung 2 — price the six separately.** `pool0`, `lhome`/`lpick`, `vuniv`/the vmap,
  `csbor`/the cs seats, the param `homes`, `vrt`/the mints. Today only four points exist
  (A, B, C, D) and they bracket groups, not mechanisms. Publish cycles-bought and lines-owned
  per mechanism. **Nothing is deleted in this rung** — it is the census every later rung
  argues from, and the census is what killed the last two plausible stories.
- **rung 3 — delete what prices at zero.** `pcs` is already measured: +1,004 B of `.text`,
  **zero** corpus instructions, 57 lines the program cannot explain. It was held under the
  old gate ("regresses nowhere", in bytes); under the criterion above a mechanism that buys
  no time and cannot state its own reason is a deletion. ⚠ re-price on today's tree first —
  that ablation is 2026-08-12 and the riscv cs-seat work of 2026-08-21 touched the same
  machinery, and `pcs` threads four sites (the grant, the fnreset capture, the shadow-spill
  read, the accounting the locals wear), so the deletion ships on a fresh number plus
  `test_fixpoint`, not the archived one. Whatever else rung 2 finds at zero joins it.
- **rung 4 — the seven names become one loop.** `lokeep`/`loseed`/`lomig`/`lochk`/`lomiss`/
  `lobar`/`lonone` is seven names for the phases of one mechanism's uncertainty, and the
  ledger already called them its own progress bar: *"if the ladder is working, most of those
  names disappear; if they survive, it is not."* All seven survive. ⚠ the constraint under
  them does not dissolve — the keep decision must precede the emission that determines
  whether it is valid, and `rdsp` cannot supply a parse-tree fact before emission. So this
  rung is not "remove the retries", it is **write the fixpoint down as a fixpoint**: the
  `att` loop already states its convergence in place (bars grow monotonically, only a
  zero-miss build ships, the capped attempt cannot miss) — what is owed is one named loop
  owning the seven channels, a consolidation rather than a discovery. Gate: the names
  shrink to a few, `test_fixpoint` byte-identical, `test_cts` unmoved.
- **rung 5 — the B half, decided on rung 2's evidence.** The operand pool and locals homing
  together buy 4.9% of cycles for 14% of instructions, and they own most of the machinery —
  the free list, the mints and `vrfix`, the parks and aims, `lpick`/`ihset`/`seats`, the
  regen dance. The cs seats buy **more** cycles (6.6%) for 2% of instructions. So the
  cheaper mechanism by instruction count is the better one by time, and the expensive one by
  line count is the worse one by time. Rung 2 says which specific parts of B are which.
  Options, in preference order: shrink B to the parts that price; fold its residency into
  the cs-seat mechanism that already pays; leave it alone with the price written down.
  ⚠ deleting B outright is a 4.9% regression and is not proposed here.
- **rung 6 — give the hot shapes somewhere else to go.** Where C plus residency cannot close
  a gap, `gen.l` should not grow to chase it. `crew/sat/flat.l` is the pattern: hand-written
  kernels in holo's **neutral** IR (one body, five backends — `(assemble <target> ir)`), with
  interpreted twins as both deopt path and differential oracle. This is the release valve
  that keeps rung 5's answer honest — without it, every unclosed shape becomes another
  thousand lines of `gen.l`. ⚠ no shape gets a kernel before rung 1's re-fill is read —
  the biggest hot shapes measured so far may have been the rotate wearing an array costume.

## what not to do

- ⚠ **do not optimise instruction count.** 601,597 icache misses against 34 billion
  instructions; marginal instructions retire at ~5.85 IPC against a 2.74 baseline. On this
  core the counter that is easiest to move is the one that means least.
- ⚠ **do not read one ccbench row.** The corpus average is flattering and the cipher pair
  over-weights array work. Both, every time — and ±4% is the floor on a wall-clock ratio.
- ⚠ **do not carry x86-64 numbers to the MCU targets.** thumb1/thumb2 are M0+ and M7,
  in-order, where instruction count *is* cycle count. `tpool` is already empty there, so
  those targets have run the ablated configuration all along.
- ⚠ **do not read law churn as breakage.** 80–106 of `law.l`'s 866 goldens pin register
  identities and residency counts; any lane change moves them. `test_cts` and
  `test_fixpoint` are the behavioural instruments — both held in all four ablations.
