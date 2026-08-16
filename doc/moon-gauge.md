# moon-gauge — what mooncc's codegen is measured against, and what residency is worth

## the target application is love itself

Codegen quality is gauged on **love**: `ccbench`'s corpus row, which builds the host binary
with each compiler and runs the arch-neutral test corpus through it. That workload is
love.c's VM — call- and branch-dense dispatch — and it is what mooncc exists to compile.

The cipher rows (chacha20, poly1305, `test/bench/ccrypto.l`) are **subsidiary**. They are a
lever gauge, not a target: chacha indexes a 16-word array in its inner loop and poly keeps
five scalar limbs, so the pair reads whether a gap is array slots or general residency. A
win there is worth having and is not the objective. ⚠ but do not read them to zero either —
`core/love.c:6138`'s z-tray comparison is the same array-indexed shape.

⚠ **the corpus average is flattering and the pair exists because of it** (`ccbench.sh`'s own
header says so). An arc that reports only the corpus row will under-weight array work; an arc
that reports only the pair will over-weight it. Both rows, every time.

## the readings (2026-08-16, x86-64, static musl on both native lanes)

| | mooncc | gcc-musl | clang-musl | mooncc/clang |
|---|---|---|---|---|
| build | 51,766.8 ms | 12,540.4 | 7,611.3 | **6.80×** |
| corpus | 4,574.7 ms | 3,872.0 | 3,847.8 | **1.19×** |
| chacha20 | 1,192.7 ms | 241.6 | 223.2 | **5.34×** |
| poly1305 | 1,754.5 ms | 1,713.4 | 1,053.3 | **1.67×** (1.02× vs gcc) |

⚠ **these are wall-clock and they are not tight.** A second fill 40 minutes later on the same
box read the corpus at 1.14× (mooncc 4,007.1, clang 3,517.4) and the build rows moved 7-10%.
Treat ±4% as the floor on a ccbench ratio and reach for `perf` cycles (±0.72% here) for
anything finer — a 3% ccbench move is not a result.

The corpus row confirms the plateau the regalloc arc last recorded at 1.21× on 2026-08-11.
chacha reads 5.34× against the ~23× in `ccbench.sh`'s header — the array-slot rung of
2026-08-10 claimed a 74% wall drop and this is that claim corroborated by a later run of a
different instrument. poly1305 is at parity with gcc.

## what the residency layer is worth

Four ablations, each one binding in `gen.l`, each rebuilt through `test_fixpoint` (so every
configuration below compiles all of love, links love1, and love1 rebuilds itself
byte-identically). Measured with `perf` on those love1 binaries, median of 3, boot
subtracted. Noise floor on this box: **±0.72% cycles, ±0.01% instructions**.

- **A** — `ralloc` dry always: no operand pool, homes and cs seats intact.
- **B** — `tpool ()`: no operand pool and no locals homing (`pools` filters from `tpool`).
- **C** — `cspool ()`: no callee-saved seats, operand pool and homes intact.
- **D** — B and C together: no register residency at all.

| | corpus insns | corpus cycles | .text |
|---|---|---|---|
| base | — | — | 896,896 |
| A | +12.3% | +4.2% | +2.7% |
| B | +14.0% | +4.9% | +4.6% |
| C | **+2.0%** | **+6.6%** | +1.4% |
| D | +22.0% | **+12.5%** | +6.4% |

⚠ **the two halves have opposite economics, and that is the finding.** B costs 14% of
instructions to buy 4.9% of cycles — an instruction-count lever, and this core hides most of
it (IPC rises 2.71 → 2.97 as the instructions arrive, store-to-load forwarding on L1-hot slot
traffic being nearly free). C costs 2% of instructions to buy 6.6% of cycles — a pure
**latency** lever, breaking the store→load chain across calls, exactly as the cs-borrow rung
claimed when it landed. The cheaper mechanism by instruction count is the more valuable one
by time.

So an instrument that counts instructions ranks these two backwards. That is how "every
landed lever has been a size lever" came to be recorded as a disappointment: the levers that
moved instructions were the ones being counted.

Whole layer: **+12.5% corpus cycles**, taking mooncc from 1.19× to ~1.34× against clang — the
residency layer closes about **44%** of mooncc's excess. It is not overhead. The question it
leaves open is not whether to keep register residency but what fraction of `gen.l`'s size buys
which half.

On the cipher pair the same split reads harder: C alone costs chacha **+16.8%** cycles and D
costs it **+29.2%** — the array leg's keeps ride callee-saved seats, not the operand pool.

## how to measure

- Both `ccbench` rows, never one. `make -C test/bench ccbench` (it refreshes `host`,
  `mooncc` and `mooncc.image` first — a `wake` on an image its binary did not bake fails as
  `love: cannot open mooncc`, not as a version error).
- Cycles, not instructions, for anything claiming a speed effect. Instructions are near
  deterministic here (±0.01%) and make a tempting proxy; §above is why they mislead.
- An ablation is priced through `test_fixpoint`, so a configuration that cannot rebuild
  itself never reaches the timer.
- ⚠ the `crew/moon/law.l` goldens pin register identities and residency counts (80 fail
  under A, 106 under B). A lane change churns them. That is not breakage — `test_cts` and
  the fixpoint are the behavioural instruments, and both held in all four ablations.

## not yet measured

No top-down cycle attribution has ever been run on mooncc's output. Whether the residual
1.19× is frontend-bound (I-cache, where code size *is* cycles and the size levers were
paying invisibly), backend-bound on slot traffic, or bad speculation, is unknown — and it
decides whether the ablations above are read as savings or as losses.
