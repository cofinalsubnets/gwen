# plan: paring gen.l back

`crew/moon/gen.l` went 4,150 → 8,532 lines between 2026-07-19 and 2026-08-16, and the
corpus row did not move across that span (1.21× clang on 2026-08-11, 1.19× on 2026-08-16).
That reads as a month spent for nothing, and it is not what happened — chacha went ~23× →
5.34× over the same period, which the corpus under-weights by construction. What actually
went wrong is smaller and fixable: **the arc was steered by instruction counts on a core
that hides instructions**, and the layer it was tuning turned out to hold two mechanisms
with opposite economics. doc/misc/moon-gauge.md carries the current
measurements (the superseded fills live in its git history); the ROTATE finding is
rung 1 here because it contaminated every row the later rungs read.

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

- **rung 0 — the ablation becomes a knob, and the knob becomes a harness. LANDED
  2026-08-23.** `MOON_ABLATE="tpool,cs"` holds named mechanisms empty — `ralloc tpool cs
  lhome vuniv csbor homes pcs` (gen.l's `ablenv`, read at RUN time per unit; a plain value
  binding folds at bake and would carry the baking session's environment). `tools/
  moon-ablate.sh` is the pricing: each configuration recompiles all of love, closes
  `test_fixpoint` by name, and reads perf cycles + instructions + .text against base. It
  reproduced all four census points (A +12.5%/+4.6%, B +14.3%/+5.3%, C +1.9%/+6.9%,
  D +22.1%/+12.3% insns/cycles), and every fine knob closes its fixpoint and flips real
  bytes. ⚠ the knob is part of the compiler's IDENTITY: `mcid` carries it in the runtime-
  cache key, and the fixpoint said so the hard way (moon-gauge's cache-trap note). ⚠ one
  uncontrolled sample hinted `vuniv` may price at or below zero cycles on today's corpus —
  rung 2 settles that with same-run bases and real samples, not this footnote.
- **rung 1 — the rotate lands, and the pair stops lying. LANDED 2026-08-22.** The
  recognizer is in gen.l (constant and spliced-variable counts, both directions, both
  widths; `ror4`/`rorv`/`rorv4` joined holo on x64 + arm64), pinned by law, by
  `test/cc/152-rotate.c` on three targets, and the fixpoint. The re-fill answered the
  confound: sha256 4.23× → 2.90×, chacha 5.80× → 3.59×, every rotate-free row inside
  noise — **both hypotheses were true, each owning a row**, and the pair reads array vs
  scalar again. The landing fill's tables are in moon-gauge's history (d52aea0a). Still
  open from this rung's teardown, the named next lever: an inlined body does not
  constant-propagate,
  so hash.c's rotates ride `%cl` off a frame slot where gcc folds an immediate.
- **rung 2 — price the six separately. LANDED 2026-08-23**, the pre-cut table is in
  moon-gauge's history (e2cea5c3); the current one is its *what the residency layer is
  worth*. The payers: locals homes +6.3% cycles, the cs grant +5.0%, the operand pool
  +4.0%, param homes +1.0%. At zero or below: `pcs` (−0.4%, the ledger's zero confirmed),
  `csbor` (+0.4%, the floor), and **`vuniv` at −2.9%** — the vmap universe costs cycles,
  and the shape rows agree (ccnif unmoved to the millisecond, chacha +3.6% inside the
  wall floor under the ablation). The 2026-08-10 array-slot story — true when it landed —
  is falsified on today's tree: chacha's wins ride the cs keeps and the rotate now. The
  census killed its third plausible story, which is its job.
- **rung 3 — delete what prices at zero.** `pcs` is already measured: +1,004 B of `.text`,
  **zero** corpus instructions, 57 lines the program cannot explain. It was held under the
  old gate ("regresses nowhere", in bytes); under the criterion above a mechanism that buys
  no time and cannot state its own reason is a deletion. ⚠ re-price on today's tree first —
  that ablation is 2026-08-12 and the riscv cs-seat work of 2026-08-21 touched the same
  machinery, and `pcs` threads four sites (the grant, the fnreset capture, the shadow-spill
  read, the accounting the locals wear), so the deletion ships on a fresh number plus
  `test_fixpoint`, not the archived one. **The vmap complex is CUT, 2026-08-23** —
  gen.l 8,606 → 7,927, the −2% banked (the gate found the
  knob's `au` license still buying pointless regens, and the wall clock lying across
  fills — hold hot functions to instruction identity, not the clock). The re-price after
  it: `cs` HOLDS at +6.0%, and `csbor` read −0.2%. **The csbor complex is CUT,
  2026-08-23** — gen.l 7,927 → 7,879, and the cut binary is byte-identical to the priced
  ablation reference (the borrow's grant had been provably
  empty since the vmap cut; its one live effect was VETOING pcs beside a callish loop,
  which is why the ablation flipped code at the same .text size). And the fresh number
  answered `pcs` the other way: **+1.5% cycles on the cut tree — it PAYS, and it stays.**
  The −0.4%/zero-instructions reading was taken under the veto; the 57 lines had a reason
  the whole time. `cs` holds at +6–7%. **RUNG CLOSED 2026-08-23**: everything that priced
  at zero is deleted, and what remains prices above the floor.
- **rung 4 — the seven names become one loop. CLOSED BY OBSERVATION 2026-08-23.**
  `lokeep`/`loseed`/`lomig`/`lochk`/`lomiss`/`lobar`/`lonone` was seven names for the
  phases of one mechanism's uncertainty, and the ledger already called them its own
  progress bar: *"if the ladder is working, most of those names disappear; if they
  survive, it is not."* Rung 3 answered: the vmap cut took `lokeep`/`lochk`/`lomig`/
  `loseed`, the csbor cut took the `att` grant dance and `lomiss`/`lobar`/`lonone` —
  all seven are gone, `regen` is a straight-line build with one deopt guard, and there
  is no retry loop left to consolidate. The ladder was working.
- **rung 5 — the B half, decided on rung 2's evidence. CLOSED 2026-08-23** (moon-gauge's
  *what the residency layer is worth*). The negotiation settled in preference order: shrinking B
  to the parts that price was performed BY rung 3 (the cuts took whole dependency cones —
  an orphan scan over gen.l's 334 definitions finds zero names without a live caller);
  folding into the cs seats is the ledger's twice-refused retrofit (a cs seat can never
  be the store's source — reachable only under vreg emission, a rebuild this plan is
  not); so the B half stays whole with the price written down — and the price ROSE: on
  the cut tree lhome ablates to +11.4% cycles at +8.1% insns (was +6.3%/+2.9%), tpool
  +11.7%, ralloc +7.4%, cs +8.6%, with homes +0.9% and pcs +1.1% still above the floor.
  The survivors absorbed the deleted mechanisms' work; every knob is a payer now.
  (chosen, revisable — what would reopen it: a survivor pricing at the floor on a future
  same-run census.)
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
