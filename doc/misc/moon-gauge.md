# moon-gauge — what mooncc's codegen is measured against, and what residency is worth

## the target application is love itself

Codegen quality is gauged on **love**: `ccbench`'s corpus row, which builds the host binary
with each compiler and runs the arch-neutral test corpus through it. That workload is
love.c's VM — call- and branch-dense dispatch — and it is what mooncc exists to compile.

The cipher rows (chacha20, poly1305, `bench/ccrypto.l`) are **subsidiary**: a lever gauge,
not a target. chacha indexes a 16-word array in its inner loop and poly keeps five scalar
limbs, so the pair reads whether a gap is array slots or general residency. (It once read a
second signal too — chacha rotates 320 times a block, poly never — until gen.l learned the
rotate idiom, 2026-08-22: `ror4`/`rorv`/`rorv4` in holo's IR on x64 + arm64, sha256
4.23× → 2.90× and chacha 5.80× → 3.59× on landing, every rotate-free row inside noise.
Both hypotheses had been true, each owning a row.) ⚠ do not read the pair to zero:
`src/love.c:6138`'s z-tray comparison is the same array-indexed shape.

⚠ **the corpus average is flattering and the pair exists because of it** (`ccbench.sh`'s
own header says so). Both rows, every time.

The **heavy-nif rows** (inflate, crc32, sha256 — `bench/cnifs.l`) are a third kind: not a
target and not a lever, but *work the tree waits on* — `love source` unpacks through
inflate and checks with crc32, every svalbard id is a sha256. They also span the pair's
shapes: crc32 carries no array across its loop, inflate is branches and a table, sha256
is a 64-word array beside eight scalars.

## the differentials (2026-08-23, x86-64, static musl on the native lanes)

One quiet `make -C bench ccbench` fill, the shipped artifact on the mooncc lane:

| row | mooncc | gcc-musl | clang-musl | /gcc | /clang |
|---|---:|---:|---:|---:|---:|
| build | 18,336.3 ms | 9,970.7 | 6,208.9 | 1.84× | 2.95× |
| corpus | 2,938.8 ms | 2,431.0 | 2,423.3 | **1.21×** | 1.21× |
| chacha20 | 727.4 ms | 289.2 | 187.5 | **2.52×** | 3.88× |
| poly1305 | 1,241.7 ms | 1,304.3 | 778.3 | **0.95×** | 1.60× |
| inflate | 598.8 ms | 358.8 | 296.9 | 1.67× | 2.02× |
| crc32 | 599.2 ms | 394.7 | 454.0 | 1.52× | 1.32× |
| sha256 | 1,009.0 ms | 309.7 | 353.7 | **3.26×** | 2.85× |

**poly1305 beats gcc.** ⚠ and clang is 15% slower than gcc on crc32 while much faster on
poly — two optimizing compilers disagreeing by that much on adjacent rows is the scale of
noise-plus-real-difference to hold in mind before reading a mooncc move of the same size.

## the same floors compiled STRAIGHT (ccnif, 2026-08-23)

`make -C bench ccnif` builds src/hash.c, src/deflate.c, src/inflate.c with every lane
and reads them three ways — answers (a divergence is a miscompile, the only thing in the
script that says a compiler is *wrong*), .text, wall clock. No love runtime, no libc in
the loop; ~20 s, so it is the per-edit instrument where ccbench is the per-rung one.

| ms (median of 5) | mooncc | gcc -O2 | clang -O2 | gcc -O0 |
|---|---:|---:|---:|---:|
| sha256 | 230 | 76 (3.03×) | 84 (2.74×) | 382 (0.60×) |
| md5 | 76 | 45 (1.69×) | 50 (1.52×) | 138 (0.55×) |
| crc32 | 17 | 12 (1.42×) | 13 (1.31×) | 23 (0.74×) |
| cksum | 18 | 13 (1.38×) | 13 (1.38×) | 23 (0.78×) |
| deflate | 200 | 130 (1.54×) | 136 (1.47×) | 322 (0.62×) |
| inflate | 27 | 17 (1.59×) | 16 (1.69×) | 41 (0.66×) |

mooncc beats gcc -O0 on every row. .text whole-file (mooncc/gcc-O2/clang-O2): hash.c
10,326/9,636/8,187 · deflate.c 10,587/9,749/15,259 · inflate.c 11,799/6,821/10,281.

⚠ **only the whole-file .text number is a sound total** — gcc and clang inline statics out
of existence (hash.c is 45 functions under mooncc, 34 under gcc), so summing shared names
charges mooncc for a callee its opposite number paid for inside a caller. The script
prints per-function ratios instead, worst first; `sha_block` stays the widest cell (3.37×
clang's bytes beside the 3.03× clock — the static and dynamic readings name the same
function).

## where the build's ~18 s goes (measured 2026-08-22)

Direct per-step timing, not subtraction: `src/love.c` is 68% of the build, and 88% of
that one compile is codegen (`cgen-obj`) — lex+cpp+parse 10%, object write 2%. The perf
profile of the compile is flat VM dispatch (`lvm_argtwocond` 13%, `lvm_eq` 12%, `lvm_tapn`
8%, then the arg family; gcp 2.2%): no data structure to fix, no collector to tune — it is
gen.l's own love on the interpreter, ~1.6× gcc per unit. ⚠ the linker is 1% of the build
(194 ms vs ld's 38) and is not the problem. ⚠ the splice JIT is not the lever either and
its census said why before the code was cut (`lib/splice.l`, `-fir`, `dis`/`disg`,
removed 2026-08-13): 95.1% of the 12,427 closures reaching the door during a love.c
codegen contain a CALL, and the splicer deletes dispatch *between* the ops of one thread —
there is no machine form for "enter an arbitrary closure". gen.l is calls almost all the
way down; whatever pays its 11.5 s down, it is not that lane.

## what the residency layer is worth (2026-08-23, the cut tree)

`tools/moon-ablate.sh`, whole roster in one run, same-run base, quiet box (floor ±0.7%
cycles). The cost of ABLATING a mechanism is what the mechanism buys:

| ablated | cycles | insns | .text |
|---|---:|---:|---:|
| ralloc (operand pool dry) | +7.4% | +13.8% | +3.7% |
| tpool (pool + homes) | +11.7% | +15.0% | +6.7% |
| cs (callee-saved seats) | +8.6% | +2.8% | +3.0% |
| lhome (locals homes) | +11.4% | +8.1% | +3.7% |
| homes (param homes) | +0.9% | +1.0% | +1.5% |
| pcs (param cs seats) | +1.1% | −0.0% | ±0 |
| **tpool,cs (D: no residency)** | **+17.2%** | **+23.0%** | **+11.9%** |

Every knob pays. The vmap and the cs borrow are not in this table because they priced at
or below zero and were **deleted** (rung 3, commits ee076adc + 8a53b06b — the vmap's
census read −2.9% cycles and the cut banked it; the borrow's grant was provably empty and
its one live effect was vetoing pcs). The survivors then absorbed the deleted mechanisms'
work: lhome's insn price tripled across the cuts (+2.9% → +8.1%) because the homes carry
the loop locals the keeps used to.

⚠ **the two halves have opposite economics, and it is the file's central finding**: the
pool/homes half buys cycles with instruction count (B: +15% insns for its cycles, IPC
rises as they arrive — store-to-load forwarding on L1-hot slot traffic is nearly free on
this core), the cs half buys latency at flat count (C: +2.8% insns for +8.6% cycles —
breaking the store→load chain across calls). An instrument that counts instructions ranks
them backwards; that is how "every landed lever is a size lever" was once recorded as a
disappointment.

Whole layer: **+17.2% corpus cycles** — mooncc's 1.21× corpus row would sit near ~1.42×
without it, so the layer closes about half of the remaining excess against clang. It is
not overhead.

## attribution (2026-08-23, Zen 3, Ryzen 7 5825U) — why instruction count is not the meter

Intel's `--topdown` does not apply; the Zen equivalents, corpus with boot subtracted:

| | base | D (no residency) |
|---|---:|---:|
| instructions | 32.55 G | 40.04 G |
| cycles | 10.91 G | 12.96 G |
| IPC | 2.98 | 3.09 |
| L1-icache misses | 261 K | 483 K |
| iTLB misses | 395 K | 425 K |
| uop queue empty | 1.60 G (14.6%) | 1.26 G (9.7%) |
| **load-queue token stall** | **14.7 M** | **133.4 M** |
| store-queue token stall | 3.0 M | 6.9 M |

The instruction column is near-deterministic (±0.01%) and tempting; the table is why it
misleads: icache and iTLB misses are noise against tens of billions of instructions, the
marginal (ablated-in) instructions retire at far above the baseline IPC, and what moves
when residency leaves is the load-queue stall — memory latency, not issue width. Cycles,
on both ccbench rows, for anything claiming a speed effect.

## the levers, read from today's emission (2026-08-23)

Where the remaining ratios live, from disassembly of the current binaries — each lever
names the evidence that prices it:

1. **inline const-prop — LANDED 2026-08-23 (the CONST bind).** An inlined body used to
   materialize every argument to a frame slot, constants included: sha_block's spliced
   `rr(x, k)` was 7 instructions + 4 frame ops per rotate riding `%cl` where gcc emits
   `ror $6`. A literal arg whose conversion to the param type folds exactly (cnum's
   arithmetic is the slot round-trip's) now substitutes into the body as
   `(cast pty (num v))` — no slot, no forms, every immediate lane reads it. Declines:
   body assigns or shadows the name, asm in the body, `&param` (through clval, the real
   call stands). sha256 230 → 204 ms (3.03× → 2.68× gcc); compression 139 → 121
   insns/iter, all counts immediate; corpus flat. What separates 2.68× from gcc now is
   the VALUE param's slot traffic and the zext chatter — lever 3's territory.
2. **adjacent store→reload, u32 lane — LANDED 2026-08-23.** cc_block stored an element
   and reloaded the same slot on the very next instruction 32 times; the `stld` adjacent
   lanes only matched full-width `st`/`ld`. The narrow pairs forward now, wearing the
   extension the load promised (`zx4`/`sx4` and kin), on any base — every anchor shape
   is a full `ld`, so only the full-width r4 pair stays reserved. A store fed by an
   adjacent `li` declines (that triple is the si fold's). chacha −2–4% cycles at flat
   instructions, interleaved same-run.
3. **u32 zext chatter — LANDED 2026-08-23 (`rezx`).** 96 of cc_block's 619 instructions
   were `mov %eax,%eax` re-asserting a cleanliness the producing op already guaranteed.
   `rezx` (after `copyprop` in the x64 sweep chain, stage-sigged `(a) a`) tracks each
   register's clean width — the zx family and unsigned loads by contract, the 32-bit
   rotates' w-form dests, an li's own value, a mov carrying its source's — and drops a
   zxN over a register already clean to N; any other def dirties, a bar clears. The
   required zexts (after 64-bit ALU) stand. cc_block 619 → 486 insns; sha256
   204 → **195 ms** (2.57× gcc), crc32 1.42× → 1.33×, cksum 1.38× → 1.23×; .text
   −8,192 B. "Free on Zen" was wrong — same-register 32-bit movs are not eliminated.
4. **chacha's residual is the rung-6 shape, not a gen.l rung.** cc_block's 16-word state
   gets zero residency: 269 of 619 instructions touch the frame, every element op a
   load-op-store round trip. The mechanism that chased this (the vmap) priced negative
   and is deleted; the pare plan's answer is the flat.l valve — a hand kernel in holo's
   neutral IR, built on demand, not another thousand lines of gen.l.

## the arm64 lane rides the sweeps (2026-08-23)

The sweep chain reaches arm64 — at the POST-CHOICE seam (before deadlab), not inside
build. The seam is the whole design: the rankers price ir1 as built (nreads/ntouch), and
an in-build sweep starves them — nreads on swept ir1 fell below the home gate and irDa's
params shipped as slot traffic instead of homes, worse than either the old homed or the
new swept shape. Post-choice, every pricing decision is exactly as before and the CHOSEN
ir gets cleaned: pipeline output was the raw forms; it is now the swept forms with homes
intact. On x64 the sweeps stay inside build — that regime is what the whole census
priced, and 153/153 battery programs are byte-identical across this change.

What moved: the passes took the target (`cfoldir g` / `stld g` / `addrfold g`), the frame
base is `(fbase g)` (fp past a4ize — pre-a4ize an r4 is also the 5th argument, so the
base laws only hold after the retarget), the epilogue anchors are `csregs` (`stldkp`),
`deadcell`'s path-exit pops are fp/lr, and the imm-form folds ask `imma` (the fold face
of `immok`: a64 add/sub ±16M, cmp ±4095, logicals bottom-aligned masks, mul never).
`cmpfuse` and the `si` fold stay home — no memory-operand compare and no store-immediate
off x64. The epilogue's `(lea sp fp 0)` is teardown, not an address take (stld/deadst).

Priced by count — on fixed-width arm64 count IS bytes, and on in-order cores it is
close to cycles: the 150-program battery's .text −28,672 B (**−2.76%**), 131/150
programs changed, riscv byte-identical (still unswept — its lane needs its own rezx
producer table: `rorw` sign-extends). Gates: ccarm64 150, cts_arm64 211/220, fixpoint,
test_slow, and a cross-seeded `love-aarch64` bakes and runs `love cc` under qemu.

## the SSA question, measured (2026-08-27)

Would an SSA middle pay? The four bespoke passes -- cfoldir, alive, repack's
promotion, deadst -- were measured against an SSA-grade oracle: dump every TU's
FINAL forms (doc/misc/proto/ssagap/, 82 TUs, 1102 fns, 133k forms), rebuild the
CFG outside the compiler, and run constant/liveness analyses to a real fixpoint.
Whatever the oracle still finds in shipped forms is what the passes missed.
Every count is a floor (unmodeled ops invalidate), and every category was
spot-verified in the forms before it was written down.

| residual fact              | static | loop-wt | in cfoldir's domain | + 64-bit knowns |
|----------------------------|-------:|--------:|--------------------:|----------------:|
| const slot reload          |    215 |   4,632 |                 143 |             179 |
| foldable ALU on knowns     |    476 |   4,886 |                 284 |             476 |
| mov of a known (li-able)   |    410 |   2,468 |                 375 |             405 |
| decidable branch           |      4 |      32 |                   4 |               4 |
| dead store                 |      2 |       9 |                   — |               — |

The reading, and it is not the expected one:

- **deadst and the branch folder are complete.** Two dead stores and four
  decidable branches in the whole corpus. (A first run reported 1,045 dead
  stores; every one but two was an outgoing-arg frame before a call -- check the
  instrument before the compiler.)
- **almost nothing needs SSA.** 100% of the foldable ALU and 99% of the movs
  fall to a LINEAR pass with 64-bit knowns -- cfoldir's own shape. The gaps are
  its fold TABLE (shl/shr/sar/not/neg/ror are blunt: `li 2; sx4; shl 1; or 1`
  ships as four ops where the fixnum tag 5 is one li -- ev.c alone carries 149)
  and its kmax cap (`li 0x7fffffffffffffff; neg; sub 1` materializes LONG_MIN in
  three ops; img_wake reloads a 2^40 constant in a loop cfoldir cannot hold).
  The fixpoint-only residue is ~36 const reloads corpus-wide.
- **the one structural miss is promotion's convex hull.** The mem2reg census
  (non-escaped fns, direct-touch cells only): 1,327 cells live across a call
  (spill class -- cs-seat territory, not headroom), but **1,954 full-word cells
  whose touch span is call-free** still ride the frame (9.9k loop-wt touches;
  dtb_to_kboot stores a pointer once and reloads it three times in fifteen
  straight-line forms), plus 254 narrow/si cells (11.2k loop-wt) the full-word
  law excludes outright. The bar is the window: repack widens the packing hull
  over every backedge, and a call anywhere in the hull fails every seat by
  construction -- so a fifteen-form chain inside a big loop inherits the whole
  loop's window. Per-DEF ranges (each store-to-loads chain its own interval) are
  the SSA idea worth having; nothing else here needs the phi apparatus.

So the priced answer to "generate SSA?": no -- widen cfoldir's fold table and
knowns to 64 bits (linear, in-place, no new pass), and teach promotion per-def
windows instead of the convex hull. Those two levers cover ~97% of what the
oracle can see, and the second is where the loop-weighted mass is.

## how to measure

- `make -C bench ccnif` per gen.l edit (~20 s, no runtime in the loop); one quiet
  `make -C bench ccbench` fill per rung. Both cipher rows and the corpus, never one row.
- ⚠ ±4% is the floor on a ccbench wall-clock ratio, and **cross-fill clocks lie past it**:
  a row moved +12% against the previous day's fill with byte-identical machine code, and
  crc32 (untouched by the change) moved +4.5% the same way. When a cut's rows move, diff
  the FUNCTIONS before believing the clock; instruction identity is the instrument.
- `sh tools/moon-ablate.sh [samples] [conf ..]` prices mechanisms: each configuration
  recompiles all of love under `MOON_ABLATE`, must close `test_fixpoint` (a configuration
  that cannot rebuild itself never reaches the timer), then perf cycles + insns + .text
  against the base row. Corpus by REDIRECT, boot subtracted, medians.
- ⚠ the ±0.7% cycle floor is a **same-run** property. Identical binary pairs read 1.5%
  apart across runs hours apart on a quiet box; same-day is not same-run. The harness's
  whole-roster-in-one-run design is the instrument, not a convenience.
- ⚠ **corpus cycles across DIFFERENT layouts carry a frontend-layout LOTTERY, observed
  to ±4%.** Function entries are 2-aligned by law (parity is the image codec's pointer
  discriminator, and 16-alignment was measured and REVERTED at ~4% slower — gen.l's
  fn-start note), so any size change reshuffles every downstream entry and deals a new
  branch-predictor and op-cache hand: binaries whose hot functions are
  instruction-identical have read +2.4% and +3.7% corpus cycles on branch-misses,
  icache and uop-queue-empty alone. Before believing a cross-binary delta of that
  size, check instructions (near-deterministic), hot-function identity, and the
  frontend counters — and read the rows the change actually touches through a direct
  driver (the ciphers moved −5% cycles in the same build whose corpus read +3.7%).
  Census rows inside the band (homes, pcs) are lottery-sized; the big payers stand.
- ⚠ **a row priced under another mechanism's veto is not that mechanism's price**: pcs
  read −0.4% while the cs borrow's `wb` denied it beside every callish loop, and +1.1%
  once the borrow was cut. When mechanisms gate each other, ablate the gater first.
- ⚠ an ablation is part of the compiler's IDENTITY: `mcid` carries `MOON_ABLATE` in the
  runtime-cache key (the nolibc archive under `out/cache/moon` once served
  base-compiled members into an ablated build — a "broken" fixpoint whose only defect
  was the env-blind key). Any future config knob must join the key the same way.
- ⚠ the mooncc ccbench lane races the ARTIFACT (its baked image keys as `"<baked>"`, so
  the archive cache survives intermediate rebuilds); a stale bake reads as a slow egg
  boot, never a wrong compiler.
- ⚠ `crew/moon/law.l` goldens pin register identities and residency counts; a lane change
  churns them. That is not breakage — `test_cts` and the fixpoint are the behavioural
  instruments.
- Cycles, not instructions, for any speed claim on this box (the attribution above); on
  thumb1/2 and in-order riscv, count is the meter and `tpool` is already empty there.
