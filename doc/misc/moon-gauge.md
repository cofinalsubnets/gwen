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

## the differentials (2026-08-28, x86-64, static musl on the native lanes)

One quiet `make -C bench ccbench` fill, the shipped artifact (the one-build world) on
the mooncc lane. ⚠ absolute ms are not comparable to older fills — the corpus grew with
the merged suite and the inflate workload changed; the RATIOS are the record:

| row | mooncc | gcc-musl | clang-musl | /gcc | /clang |
|---|---:|---:|---:|---:|---:|
| build | 18,003.0 ms | 10,141.6 | 6,416.6 | 1.78× | 2.81× |
| corpus | 5,200.1 ms | 4,434.4 | 4,597.4 | **1.17×** | **1.13×** |
| chacha20 | 685.0 ms | 283.2 | 190.9 | 2.42× | 3.59× |
| poly1305 | 1,153.4 ms | 1,340.5 | 791.8 | **0.86×** | 1.46× |
| inflate | 98.0 ms | 55.2 | 57.2 | 1.78× | 1.71× |
| crc32 | 583.3 ms | 408.9 | 436.0 | 1.43× | 1.34× |
| sha256 | 784.0 ms | 304.4 | 352.4 | 2.58× | 2.22× |

**poly1305 beats gcc by 14% now** (0.95× → 0.86×), the corpus tightened from 1.21× to
1.17×/1.13×, and sha256 came down 3.26× → 2.58× across the SSA arc (the 2026-08-23
fill is this section's git history). ⚠ two optimizing compilers still disagree by ~7%
on adjacent rows (crc32) — that is the scale of noise-plus-real-difference to hold in
mind before reading a mooncc move of the same size.

## the same floors compiled STRAIGHT (ccnif, 2026-08-28)

`make -C bench ccnif` builds src/hash.c, src/deflate.c, src/inflate.c with every lane
and reads them three ways — answers (a divergence is a miscompile, the only thing in the
script that says a compiler is *wrong*), .text, wall clock. No love runtime, no libc in
the loop; ~20 s, so it is the per-edit instrument where ccbench is the per-rung one.

| ms (median of 5) | mooncc | gcc -O2 | clang -O2 | gcc -O0 |
|---|---:|---:|---:|---:|
| sha256 | 224 | 89 (2.52×) | 101 (2.22×) | 447 (0.50×) |
| md5 | 88 | 52 (1.69×) | 57 (1.54×) | 163 (0.54×) |
| crc32 | 18 | 14 (1.29×) | 16 (1.12×) | 27 (0.67×) |
| cksum | 19 | 16 (1.19×) | 16 (1.19×) | 27 (0.70×) |
| deflate | 208 | 153 (1.36×) | 159 (1.31×) | 378 (0.55×) |
| inflate | 29 | 20 (1.45×) | 18 (1.61×) | 47 (0.62×) |

(2026-08-28, the one-build world.) mooncc beats gcc -O0 on every row, and every ratio
tightened across the SSA arc — sha256 3.03× → 2.52×, crc32 1.42× → 1.29×, cksum
1.38× → 1.19×, deflate 1.54× → 1.36× (the 2026-08-23 fill is this section's git
history). .text whole-file (mooncc/gcc-O2/clang-O2): hash.c 9,284/9,636/8,187 ·
deflate.c 9,727/9,749/15,259 · inflate.c 10,605/6,837/10,296 — **mooncc's hash.c and
deflate.c are smaller than gcc -O2's now**.

⚠ **only the whole-file .text number is a sound total** — gcc and clang inline statics out
of existence (hash.c is 45 functions under mooncc, 34 under gcc), so summing shared names
charges mooncc for a callee its opposite number paid for inside a caller. The script
prints per-function ratios instead, worst first; `sha_block` stays the widest cell (3.37×
clang's bytes beside the 3.03× clock — the static and dynamic readings name the same
function).

## where the build's ~18 s goes (measured 2026-08-22, before love.c split into seven TUs — the shape holds, the per-file split is finer now)

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

## what the residency layer is worth (2026-08-28, the one-build world)

`tools/moon-ablate.sh`, whole roster in one run, same-run base, quiet box. The roster is
the three surviving knobs — the fine-grained ones (lhome/homes/pcs and the old worlds)
retired with the dance, and the one build owns their work. The cost of ABLATING a
mechanism is what the mechanism buys:

| ablated | cycles | insns | .text |
|---|---:|---:|---:|
| ralloc (operand pool dry) | +2.7% | +7.2% | +2.0% |
| tpool (pool + all seats) | +1.8% | +14.0% | +6.1% |
| cs (callee-saved grants) | −2.4% | +1.1% | +2.7% |
| **tpool,cs (no residency)** | **+4.7%** | **+20.6%** | **+10.1%** |

⚠ read the INSNS column: each conf is a different binary, so the cycle column wears the
cross-layout lottery (±4% — the how-to-measure section), and cs's −2.4% cycles beside
+1.1% insns is that lottery, not a finding. What the table says against the old world's
(git history; +17.2% cycles / +23.0% insns for the whole layer then): the layer's
instruction price holds (+20.6%) while its measured cycle price fell — the one build's
seats sit where the old world's post-hoc mechanisms had to buy their way back in. The
pre-cut history (vmap and csbor deleted at ≤0, lhome/homes/pcs priced separately) is
this section's git history and the pare plan's rungs 2–5.

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

## landed levers (2026-08-23, kept as method)

Three finds from disassembly-first pricing — each names the evidence that funded it,
and together they are the method the forward path below inherits:

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
4. **chacha's residual is the valve's shape, not a gen.l rung.** cc_block's 16-word state
   gets zero residency: the element ops are load-op-store round trips. The mechanism that
   chased this (the vmap) priced negative and is deleted; the pare plan's answer is the
   flat.l valve — a hand kernel in holo's neutral IR, built on demand, not another
   thousand lines of gen.l.
5. **cfoldir's 64-bit knowns + fold table — LANDED 2026-08-28 (the SSA oracle's coda).**
   A known is the word's signed value now (love's fixnum is exactly s64, so the bit ops
   read straight and the arithmetic wraps), and the table covers what the oracle named:
   the immediate shifts and rotates (`ror4` low-32, `rol` as `ror W-k`), the unops (neg,
   not, the sx/zx family), xor, and a cmp decided by its cc's signedness. The fixnum tag
   `li 2; sx4; shl 1; or 1` is one li; LONG_MIN's three ops are one. The a64 lane prices
   an li by its movz/movk lanes (a fold answers a li up to two lanes, a copy of a known
   becomes its li only at one — x64's reg-reg mov vanishes at rename, the lea lesson, so
   there the mov stands). ⚠ the old arm for the unops and variable shifts invalidated
   r0–r3 instead of the op's own dest: `li r9 5; neg r9; add r0 r9 1` folded to `li r0 6`
   — a latent miscompile, reachable only once a non-r0 register carried a known into a
   unop; the table's exact folds retire it, and `imma` now gates x64's imul on imm32
   (a 2^40 known would have ridden the three-operand form). Priced: ccnif .text hash.c
   −48 B, deflate.c −478 B (−4.9%), inflate.c −54 B, every clock inside the band;
   the arm64 battery 117,627 → 115,160 instructions (**−2.10%**, 132 of 153 programs
   changed). test_cts 212/220 and cts_arm64 211/220 unchanged, fixpoint byte-identical.
6. **int-vacate — LANDED 2026-08-28.** Two verdict arms in `upar`: an int param in a64's
   r4 arrival rides free with its entry cvt, as a pointer does (`a4rider?` — the pool test
   had refused it), and a HOT int param that cannot ride (a quad under a dirty body)
   vacates to its positional seat, `mov` + cvt — hot by the blocker law (loop-weighted
   read count ≥ 64); a cold one keeps its slot, which is what the +3.4% refusal in git
   history was about. mag_mul's inner bound is register-register now: the exact meter
   reads mag_mul 92.16G → 91.60G (−558M) on the arm64check corpus, mag_add tighter by
   the same shape, 48 a64 fns changed; the whole-corpus total is flat (+0.005%) under the
   cross-binary walk drift (how-to-measure). x64: r6/r5/r7/r8 arrivals already rode, so
   the arm fires once in hash.c (+8 B), the corpus reads identical instructions
   (71.238G both worlds) and the x64 battery is byte-identical. Gates at reference.

## the path forward

**Codegen quality**, each lever with the evidence that prices it (largest first is not
the order — cheapest-instrument-first is):

1. **the x64 rung-6 residuals**, recorded in the moon-ssa ledger: the gcp-class
   path-frequency miss (~0.8% insns — the classifier's path maximum is static, and a
   cold-if fn whose hot path never calls still refuses rides), the dead-home-def sweep,
   the arg-seat aim declining onto armed homes, and the quad-vacate mov in tail fns.
2. **the rv64 sweep port** — riscv is the one ELF target whose CHOSEN ir is unswept;
   its rezx wants a producer table of its own (`rorw` SIGN-extends, the a64 table is
   wrong as-is). Priced by the exact meter, which rv64 now has.
3. **the flat.l valve** for the array-kernel shapes (chacha's 16-word state) — the
   standing answer whenever a shape cannot be closed without another thousand lines.

**Internal design**, the questions the cut left open:

- **sweeps inside the arm build?** The post-choice seam was held by the deleted
  rankers; only the fp-settling reason remains. Moving the a64 sweeps in-build would
  simplify the pipeline to ONE sweep seam — priced by the exact meter, expect near-zero.
- **alive's numbering discipline — answered NO (2026-08-28).** A tablet keys
  STRUCTURALLY (`(pin t '(x 1) 7)` is found by a second `'(x 1)`), so the statement node
  cannot key the livtab: two `i++;` in one fn would share a wrapset. `id?` is the identity,
  but an identity-keyed list is O(n) a lookup, n² a fn. The tick and its guard stay.
- **repack's chains beyond x64** — a64/rv64 get tailst's straight-line peel but not the
  per-def promotion; the exact meter prices whether the general chains pay there. ⚠ not a
  flip: repack declines the whole arm family at its head (`arm? g`) because its frame
  model is x64's (`pro4?`, the `sub sp sp K` at form 3) — the price is the port.
- **the wasm relooper** (doc/misc/plan/moon-wasm.md) — its own plan; the one build
  removed nothing it needs and the residency story it must NOT pay for is now one
  mechanism instead of five.

## the arm64 lane rides the sweeps (2026-08-23)

The sweep chain reaches arm64 — at the POST-CHOICE seam (before deadlab), not inside
build. The seam was chosen because the old world's rankers priced ir1 as built and an
in-build sweep starved them; the rankers are deleted now (the cut rung), so the seam is
held by its second reason — the frame base is settled fp there (pre-a4ize an r4 is also
the 5th argument) — and "sweeps inside the arm build" is an OPEN internal-design
question, priced by the exact meter whenever someone wants it. On x64 the sweeps stay
inside build.

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

So the priced answer to "generate SSA?" was no -- widen cfoldir's fold table
and knowns to 64 bits (linear, in-place, no new pass), and teach promotion
per-def windows instead of the convex hull. **The second lever is LANDED**: the
SSA arc's rungs 1-3 gave repack per-def chains (full-word, narrow, and cs
seats), and the arc went on to replace the whole two-build dance with the one
build (doc/misc/plan/moon-ssa.md, closed 2026-08-28). The first lever --
cfoldir's 64-bit knowns and fold table -- landed the same day (landed lever 5).

## the GVN/LICM question, measured (2026-08-28)

The same method as the SSA question, for the two passes that would actually need
phis: `doc/misc/proto/ssagap/gvn.tpl.l` (love, riding val.l's CFG) value-numbers
every fn's FINAL forms to a fixpoint over the CFG (a join keeps only what every edge
agrees on) and counts computations whose value already sits in a register (G), and
walks each back-edge span for loop-invariant computations in SSA's view -- every read
either unwritten in the span or reaching from an invariant def, register reuse
ignored (L). 86 TUs, floors throughout; LINEAR = every label starts empty.

| residual                    | static | loop-wt | linear (in-block) |
|-----------------------------|-------:|--------:|------------------:|
| G load (slot reload, value in a reg) | 1,540 | 15,092 | 995 / 11,796 |
| G alu (repeated ALU/shift/unop)      |   270 |  2,482 | 223 / 2,239 |
| G addr (repeated lea/leax)           |    61 |  3,624 |  54 / 3,596 |
| L alu (invariant ALU in a loop)      |   210 |  5,264 | -- |
| L addr (invariant address)           |    91 |  3,248 | -- |
| L load (invariant load, store-free loop) | 72 | 1,808 | -- |
| L li (a constant materialized in a loop) | 515 | 15,376 | (free on x64: no lever) |

The reading: **the CFG buys little** -- 78% of the redundant loads' loop weight and
~90% of the ALU/addr rows are in-block, a linear pass's territory. The shipped forms
still carry `mov r0 r11; st sp 136 r0; … ld r5 sp 136`, and cfoldir's input for
p0read1 says why (2026-08-28): the inliner materializes a nested splice's params slot
to slot through r0 (`ld r0 [g]; st [g2] r0; … ld r0 [g2]; st [g3] r0`), and cfoldir's
lattice names only constants and copies-of-a-register -- a loaded unknown has no
name, so the record "g2 holds a copy of r0" dies the moment r0 is reused two forms
later. The registers that would make those loads movs (g and d promoted to cs seats
r11/r12 by repack's chains) do not exist yet when cfoldir runs: repack sits
post-choice, cfoldir in the build. So the fix is not a phi and not a wider lattice:
run the copy fold once more AFTER repack (base still r4 there), or let repack's
promotion rewrite the slot copies it just made redundant -- priced by this oracle's G
load row, whole corpus 1,540 static. What genuinely needs
the loop structure is L alu + L addr + L load: ~370 forms, ~10k loop-wt -- twice the
ALU-fold residue that funded lever 5, spread thin (io.c's p0skip, inflate's
inf_run/inf_build, snap's img_hashcons head the lists). Spot-verified in the forms:
`and r10 r6 -8` under a loop head with r6 unwritten in the span; `imul r0 r5 8`
likewise. Neither needs SSA either: a span-local invariance pass over the flat forms
is the same shape as this oracle.

## how to measure

- `make -C bench ccnif` per gen.l edit (~20 s, no runtime in the loop); one quiet
  `make -C bench ccbench` fill per rung. Both cipher rows and the corpus, never one row.
- **the exact meter (a64/rv64)**: a ~40-line qemu TCG plugin (doc/misc/proto/insnpc/)
  counts guest instructions per translation block (inline adds into chunked scoreboards; PC→symbol through nm and
  the PIE bias from `qemu_plugin_entry_code()`), deterministic to ~5ppm — no cycles
  lottery, no sampling skid. `LOVE_NO_IMAGE=1 qemu-<arch> -plugin insnpc.so love <
  corpus.l`, per-world binaries via `MOON_ABLATE=... make xa=<arch> out/x-<arch>/love`
  (⚠ `love seed ARCH` drops the env; ⚠ rm the x-dir between worlds — env changes touch
  no mtimes). It refused the first a64 flip that forms and .text had approved, and its
  per-symbol attribution put the whole regression in two functions. ⚠ pin the TREE
  STATE: a binary built before a merge wears different semantics, and the mask it wears
  is a miscompile's. ⚠ across two BINARIES the corpus walks differently (address-keyed
  tables, unsorted keys): lvm_qa read +2.2G with byte-identical code, so a per-symbol
  delta counts only beside a mnemonic diff of that symbol, and the whole total wears
  the same drift. ⚠ a whole-artifact byte or .text compare can never close across a
  source edit (the carried source moves every address) — diff disassembly MNEMONICS and
  expect only the address-formers to move.
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
- ⚠ holo's static binaries pad .text to the page (the section ends where .rodata's page
  begins), so a battery's .text total moves in 4,096 B steps and reads a 208 B fn as
  −4 KiB or as nothing. Count instructions (`llvm-objdump -d | wc -l`), not section bytes.
- ⚠ `crew/moon/law.l` goldens pin register identities and residency counts; a lane change
  churns them. That is not breakage — `test_cts` and the fixpoint are the behavioural
  instruments.
- Cycles, not instructions, for any speed claim on this box (the attribution above); on
  thumb1/2 and in-order riscv, count is the meter and `tpool` is already empty there.
