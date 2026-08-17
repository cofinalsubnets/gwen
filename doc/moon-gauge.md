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

The **heavy-nif rows** (inflate, crc32, sha256 — `test/bench/cnifs.l`, added 2026-08-17) are a
third kind and are read differently again: not a target and not a lever, but *work the tree
waits on*. `love source` unpacks its own tarball through inflate and checks it with crc32,
and every svalbard id is a sha256, so a regression there is a regression a user feels. They
also happen to span the pair's two shapes — crc32 has no branch in its loop, inflate is a bit
reader and a table lookup per symbol, sha256 carries a 64-word array beside eight scalars —
so a lane behind on inflate and level on crc32 is losing to branches, not to loads.

## the readings (2026-08-16, x86-64, static musl on both native lanes)

| | mooncc | gcc-musl | clang-musl | mooncc/clang |
|---|---|---|---|---|
| build | 51,766.8 ms | 12,540.4 | 7,611.3 | **6.80×** |
| corpus | 4,574.7 ms | 3,872.0 | 3,847.8 | **1.19×** |
| chacha20 | 1,192.7 ms | 241.6 | 223.2 | **5.34×** |
| poly1305 | 1,754.5 ms | 1,713.4 | 1,053.3 | **1.67×** (1.02× vs gcc) |

⚠ **THAT BUILD ROW IS NOT COMPARABLE TO THE ONE BELOW** and never measured what it said:
51,766.8 ms carries the one-time nolibc build the next section takes apart. Its mooncc/clang
figure is fiction; the corpus and cipher rows are unaffected.

⚠ **these are wall-clock and they are not tight.** A second fill 40 minutes later on the same
box read the corpus at 1.14× (mooncc 4,007.1, clang 3,517.4) and the build rows moved 7-10%.
Treat ±4% as the floor on a ccbench ratio and reach for `perf` cycles (±0.72% here) for
anything finer — a 3% ccbench move is not a result.

## the heavy-nif rows (2026-08-17, same box, and a corrected build row)

| | mooncc | gcc-musl | clang-musl | mooncc/clang |
|---|---|---|---|---|
| build | 19,202.8 ms | 9,542.0 | 5,729.1 | **3.35×** |
| corpus | 3,249.2 ms | 2,593.0 | 2,506.0 | **1.30×** |
| chacha20 | 926.5 ms | 281.4 | 159.6 | **5.80×** |
| poly1305 | 1,231.1 ms | 1,321.0 | 795.2 | **1.55×** (0.93× vs gcc) |
| inflate | 576.3 ms | 362.5 | 316.2 | **1.82×** |
| crc32 | 649.1 ms | 410.7 | 452.6 | **1.43×** (1.58× vs gcc) |
| sha256 | 1,484.0 ms | 392.9 | 350.8 | **4.23×** |

⚠ **THE BUILD ROW WAS 2.2× TOO HIGH AND THE BENCHMARK CAUSED IT.** It read 41,794 ms twice
running, reproducibly, and it was still wrong: mooncc's link pulls `crew/moon/lib/nolibc/`
member by need and caches the archive under `~/.love/cache/moon` keyed on the compiler,
its stat and its image — and for an image FILE the key carries that file's stat, while
`test/bench/Makefile` rebuilt `out/host/mooncc.image` as a prerequisite of the very
target. So every run missed and paid a one-time **libc build** inside a per-build row,
against gcc and clang linking a musl somebody else compiled. Measured directly: 43,754 ms
after `touch out/host/mooncc.image`, 20,139 ms without. The lane now runs **the artifact**,
whose baked image keys as the word `"<baked>"` and therefore survives every rebuild of the
intermediates (`touch out/host/mooncc.image out/host/love` leaves it at 18,905 ms), plus
one untimed build so the row means one thing. ⚠ a one-line C file does not warm the
archive in its place — a program that needs no member pulls none, and it links in 94 ms.

⚠ **sha256 is the row worth reading, and it was not chosen to prove anything.** It sits at
4.23×, next door to chacha's 5.80×, while crc32 — the same kind of table work with no array
carried across the loop — sits at 1.43×. sha256's inner loop keeps a 64-word message
schedule; that is the array-slot shape appearing in a function picked for being *used*, not
for being diagnostic, which is the strongest corroboration the diagnosis has. inflate at
1.82× is the branchy end and says the branch path is not where mooncc is losing.

## where the build time goes (2026-08-17, the same box)

Once the row is honest, the 19.2 s has an address. Every number below is a direct
measurement, not a subtraction:

| step | ms | of the build |
|---|---:|---:|
| `core/love.c` | 12,587 | 68% |
| `host/*.c` (11 units) | 4,934 | 27% |
| `crew/moon/lib/math/am.c` | 642 | 3% |
| `mksys` (the syscall leaf) | 173 | 1% |
| **link** | **194** | **1%** |

and the one file that is two thirds of it splits again:

| phase of the `love.c` compile | ms | share |
|---|---:|---:|
| lex + cpp + parse | 1,306 | 10% |
| **codegen (`cgen-obj`, `crew/moon/gen.l`)** | **11,509** | **88%** |
| object write (`objsecs`) | 277 | 2% |

**So the gap is `gen.l`, and it is not a hot spot.** `perf` on one `love.c` compile is flat
VM dispatch — `lvm_argtwocond` 13.0%, `lvm_eq` 11.6%, `lvm_tapn` 8.5%, `lvm_cur` 7.1%,
then the `arg*` family — with `gcp` at 2.2% and `map_probe` (tablet hashing) at 1.3%.
There is no data structure to fix and no collector to tune; it is gen.l's own love running
on the interpreter, and per-unit against gcc that is 1.6× on `love.c` and about 2× on the
small ones.

⚠ **the linker is not the problem and it was worth checking**: ours binds the whole set in
194 ms where `ld` does it in 38. 5×, on 1% of the build.

⚠ **the splice JIT is not the lever either, and it is already built.** With love rebuilt
`make moon_fir=-fir` so the binary carries its IR record, `(use 'splice)` ahead of the
mooncc cat, and codegen re-run: **one closure** native-backed over the whole compile, and
11,708 → 11,248 ms, which is noise. Whatever gen.l's hot closures are, the splicer declines
them — that is the thing to look at before anything else in this file gets optimized.

⚠ clang is SLOWER than gcc on crc32 (452.6 against 410.7) and much faster on poly1305. Two
optimizing compilers disagreeing by 11% in opposite directions on adjacent rows is the scale
of noise-plus-real-difference to keep in mind before reading a mooncc move of that size.

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

- Both cipher rows, never one, and the corpus beside them. `make -C test/bench ccbench` (it refreshes `host`,
  `mooncc` and `mooncc.image` first — a `wake` on an image its binary did not bake fails as
  `love: cannot open mooncc`, not as a version error).
- ⚠ `net` is a sum over every phase, so it moved when the three nif rows landed and results
  either side of that do not compare on it. Per-row ratios do.
- Cycles, not instructions, for anything claiming a speed effect. Instructions are near
  deterministic here (±0.01%) and make a tempting proxy; §above is why they mislead.
- An ablation is priced through `test_fixpoint`, so a configuration that cannot rebuild
  itself never reaches the timer.
- ⚠ the `crew/moon/law.l` goldens pin register identities and residency counts (80 fail
  under A, 106 under B). A lane change churns them. That is not breakage — `test_cts` and
  the fixpoint are the behavioural instruments, and both held in all four ablations.

## where the cycles go — the attribution, run 2026-08-16 (Zen 3, Ryzen 7 5825U)

Intel's `--topdown` does not apply here; the Zen equivalents are
`de_dis_uop_queue_empty_di0` (frontend delivered nothing) and the
`de_dis_dispatch_token_stalls*` family (backend resource stalls). Tight event groups, no
multiplexing, corpus with boot subtracted, base against D.

| | base | D (no residency) |
|---|---|---|
| instructions | 34.54 G | 42.14 G |
| cycles | 12.59 G | 13.89 G |
| IPC | 2.74 | 3.03 |
| **L1-icache misses** | **601,597** | **662,371** |
| iTLB misses | 422,093 | 400,462 |
| uop queue empty | 1.762 G (14.0%) | 1.415 G (10.2%) |
| load-queue token stall | 29.9 M | **156.3 M** |
| store-queue token stall | 4.1 M | 15.4 M |
| retire token stall | 395.5 M | 427.8 M |
| branch misses | 49.9 M (1.04%) | 54.4 M (1.13%) |
| L1-dcache load misses | 177.2 M | 180.4 M |

⚠ **the frontend hypothesis is dead, twice over.** 600 thousand icache misses against 34
*billion* instructions is nothing — at 20 cycles apiece it is 0.1% of the run. And frontend
starvation goes DOWN as the code grows (14.0% → 10.2%), because a backend that is stalling
gives the fetcher time to fill the queue. **Code size does not buy cycles on this machine.**
The size levers were not paying invisibly; they were not paying.

Where D's cost actually lands is the load queue — **29.9 M → 156.3 M token stalls, 5.2×** —
which is the slot traffic, exactly where the arc always said the gap was. But the loads all
hit: L1-dcache misses move 1.8% and branch misses 9%, neither material.

⚠ **the marginal instructions execute at ~5.85 IPC** (Δ7.60 G instructions over Δ1.30 G
cycles) against a 2.74 baseline — near Zen 3's dispatch width of 6. That is the whole
mechanism of "instructions are hidden here": store-forwarded, L1-resident slot traffic is
absorbed at the fastest rate this core can absorb anything.

⚠ **and that is a fact about THIS core, not about mooncc.** A wide out-of-order machine
hides instruction count; an in-order one does not. mooncc's thumb1/thumb2 targets are
Cortex-M0+ and M7 — where instructions *are* cycles, and where the operand pool is already
empty (`tpool`). The ablations above price residency on x86-64 and say nothing about what it
is worth on the MCU targets. Do not carry these numbers there.
