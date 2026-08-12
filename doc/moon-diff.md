# moon-diff — mooncc vs gcc/clang, the running differential

The three-compiler race on the love host binary: how fast each compiler *runs* (invocation
speed), how big the code it lays is (.text), and how fast that code *executes* (wall + user
instructions on the test corpus). **One current measurement, then the trend table** — every
fill's headline in a row. Re-measure and REPLACE the current section; add a row.

⚠ this page used to append a full fill per rung and reached eleven of them. They were
dropped 2026-08-12: the cause of a move belongs in doc/moon-regalloc.md's dated ledger, which
kept it, and what a fill was for is the trend, which the table keeps. Don't grow it back.

Three harnesses, all over the binaries `bench/ccbench.sh` leaves in `out/bench/cc/`:
`ccbench.sh` itself for build and corpus wall, `bench/ccsize.sh` for the `.text`
decomposition, `bench/ccdead.py` for how much of each libc is reachable at all
(`make ccbench`, `make ccsize`, `make ccdead`). The insn lanes are `perf stat -e
instructions:u`. **All three lanes are static** — mooncc against its own nolibc, gcc and
clang against musl — because that is the only shape in which the size rows mean
anything. Method and traps at the bottom — reproduce rather than trust.

This ledger is one of five docs that ride together: doc/moon-regalloc.md (the catalog
of the gap and the rung ledger — *why* a row moved), doc/moon-alloc.md (the allocator
arc, whose every rung is priced against this page), doc/hom.md (the design the
destination-die migration wears), doc/proto/dest.l (that design modeled runnable).

⚠ **read the both-emit codegen ratio, not the binary ratio, when pricing a codegen
rung.** The byte levers (the dead-static sweep, nolibc's per-function split) took the binary
from 1.99× to 1.78× without moving the corpus a digit, because a byte that is unreachable
never executes. The headline answers "how big is the artifact"; only the shared-symbol row
answers "how good is the code", and only the corpus answers "how fast".

## the current binary — 2026-08-12, rungs 1+2 (HEAD 86fc76ff)

Step 0 against the allocator arc's first landed rung (4dd9bc41, liveness + slot promotion).
Two full ccbench passes, because the pair rows swung hard in the first.

### .text and the codegen row

| | mooncc | gcc-musl | clang-musl |
|---|---|---|---|
| .text (bytes) | **465,406** | 292,448 | 292,832 |
| love's own C | 434,176 (730 syms) | 252,073 (614) | 252,496 (604) |
| libc under it | 31,230 (145) | 40,375 (229) | 40,336 (230) |

Whole binary **1.591×**. And the row a codegen rung is priced against:

| | syms | mooncc | native | |
|---|---|---|---|---|
| both lanes emit, vs gcc-musl | 612 | 364,030 | 251,961 | **1.44×** — codegen |
| both lanes emit, vs clang-musl | 602 | 364,626 | 252,000 | **1.45×** |
| mooncc-only (the inlining residue) | 118 | 70,146 | — | |

The gap is **112,069 B**. love.o's `.text` fell 6,315 over this rung and the shared-symbol
gap 6,462 — they agree to 147 B, which is what says the move is the rung alone.

### runtime — the corpus, egg-boot subtracted, median of 3

| | mooncc | gcc-musl | clang-musl | mooncc/clang |
|---|---|---|---|---|
| corpus insns (G, user) | **41.984** | 23.923 | 25.965 | **1.617×** |
| corpus cycles (G, user) | 15.710 | 12.839 | 12.956 | 1.213× |

Both natives are flat to 0.05% across this rung while mooncc fell 0.73% — as clean an
attribution as this page gets. ⚠ the cycles ratio did NOT move (1.209 → 1.213): clang's own
cycles fell 1.1% in the same run, so that row is the noisy one and insns is the reading.

### frame movs — lever 2's own gauge

| | insns | frame movs | bytes | of .text |
|---|---|---|---|---|
| mooncc | 77,080 | **19,478** | **106,892** | 32.8% |
| gcc | 45,959 | 5,378 | 27,328 | 14.3% |
| clang | 50,691 | 1,954 | 9,762 | 5.0% |

The excess over gcc is 79,564 B — **71.0% of the gap**, and this is the first fill where that
share FALLS (78.2% → 71.0%). It rose at every earlier fill because the gap shrank faster than
the frame traffic; rungs 1+2 are the first lever aimed at the traffic itself.

### the libc floor

| | syms | shipped | live | dead | |
|---|---|---|---|---|---|
| mooncc | 145 | 31,230 | 29,536 | **1,694** | **5.4%** |
| gcc-musl | 229 | 40,375 | 37,749 | 2,626 | 6.5% |
| clang-musl | 230 | 40,336 | 37,758 | 2,578 | 6.4% |

Under musl's own floor, and 0.77× its shipped size — the per-function nolibc split's result,
unmoved by anything since.

### invocation speed

| | mooncc | gcc-musl | clang-musl |
|---|---|---|---|
| full build + link, warm (s) | **15.4** | 9.0 | 5.4 |
| ..the same, cold tree (s) | 45.2 | 9.0 | 5.3 |
| love.c single TU, median of 3 | 9.90 | — | — |

The whole build is flat against the previous rung even though the single TU went 8.42 → 9.90 s
(+18%) for the liveness fixpoint.

### the pair — two passes (⚠ ratios only; the box moves more than the code)

| | mooncc | gcc-musl | clang-musl | mooncc/gcc |
|---|---|---|---|---|
| chacha20 (ms) | 1019 / 1025 | 281 / 292 | 170 / 183 | 3.63 / 3.51 |
| poly1305 (ms) | 1288 / 1301 | 1318 / 1315 | 786 / 779 | **0.98 / 0.99** |

**poly1305 passed gcc, and it reproduces** — 1.04× before rungs 1+2, 0.98× and 0.99× after,
with gcc's own poly stable to 0.3% across both passes. That is the pair's designed reading
firing exactly as specified: poly keeps its five limbs as scalar LOCALS, promotion is a
scalar-locals lever, and chacha's array slots — lever 1's residue — are untouched at 3.5–3.6×.

## the trend — every fill's headline, oldest first

Prose for each of these lived here until 2026-08-12 and was dropped; **doc/moon-regalloc.md's
dated ledger is where a row's cause lives**, and it is the surviving account. What a fill is
for is the trend, and the trend is this:

| HEAD | what landed | binary | codegen | corpus insns |
|---|---|---|---|---|
| c83b19a8 | the allocator arc (the first hom ladder) | 1.86× | — | 1.71× |
| d83892e3 | the hom ladder | 2.32× | 1.67× | 1.60× |
| 3dd9e1fc | the cs-seat rung, pmin-gated | 2.31× | — | 1.63× |
| b0f92305 | static musl, libc onto the ledger | 1.99× | 1.63× | 1.63× |
| 32b54ab8 | shrink-wrap | 1.99× | 1.63× | 1.63× |
| 6a8f8b68 | nolibc splits by area | — | 1.63× | — |
| fc061fc0 | the dead-static sweep | 1.793× | 1.63× | — |
| a1e12f40 | nolibc, one function to a file | 1.779× | — | — |
| 2fc25890 | re-base: repack + sweep + split | 1.702× | 1.56× | 1.625× |
| a055d279 | the spush cell joins the slot map | 1.618× | 1.47× | 1.629× |
| **86fc76ff** | **rungs 1+2: liveness + promotion** | **1.591×** | **1.44×** | **1.617×** |

Two readings the table carries and no single cell does. **The static and dynamic ratios
converged at 1.63× and then came apart** — four size levers (the sweep, the split, repack, the
spush cell) walked codegen 1.63 → 1.47 while the corpus did not move a digit, because bytes
that are unreachable or merely re-encoded are not bytes that execute. **And they closed again
the moment an allocator rung landed**: rungs 1+2 are the first to move the dynamic row at all.
That is the whole argument of doc/moon-alloc.md, first stated and then demonstrated.

⚠ two instrument lessons the fills paid for, kept because they still bite:

* **the `mcobj` cache is not "paid once per tree".** 45.2 s cold and 15.4 s warm on the SAME
  tree, one run after the other, because a rung that changes the compiler rehashes every
  member. Every codegen fill pays it; only the warm build row is quotable.
* **clang's chacha is not a box anchor.** It read 198.1 twice and the a055d279 fill took that
  coincidence for stability, reading a mooncc move off it; it reads 169.9 and 182.7 the next
  day. That row swings ±8%. Two agreeing samples are not a control — a control is a lane that
  moves less than the effect, over as many passes as the claim needs.

## method

* `cd bench && ./ccbench.sh` builds all three lanes (mooncc's is the `make test_raw`
  sequence: mooncc lays every .o, mksys the syscall leaf, holo links; gcc/clang compile
  the same TUs at the host's real -O2 flags, `CCACHE_DISABLE=1`) and reports build wall.
* runtime lanes run each binary under `LOVE_NO_IMAGE=1` — every lane egg-boots, a level
  field — and subtract the empty-stdin boot (median of 3 for corpus; the hostnif pass
  subtracts 28 boots). insns are `perf stat -e instructions:u`, children included.
* the hostnif roster is `test.mk`'s `hostnif_tests` minus `loader` and `bake`, each file
  as `cat test/00-init.l test/host/<x>.l | <bin>`, gated on exit 0 + `: ok` before timing.
* single-TU invocation: each compiler on love.c alone with its build-lane flags, median
  of 3.
* the native lanes go through the `musl-gcc`/`musl-clang` wrappers with `-static` added
  to `$CFLAGS` — same units, same `-O2` flags otherwise. `CCGLIBC=1` adds the old
  dynamic-glibc lanes back; they read the same on every timed row and differ only on
  size, which is the point.
* `bench/ccsize.sh` (`make ccsize`) is the `.text` decomposition, over whatever binaries
  ccbench left. It sizes symbols by ADDRESS GAP (mooncc's ELF carries no `st_size`, so
  it is the one measure both sides answer — every figure therefore carries inter-fn
  padding, ~1–2%), decides love's C per lane from **that lane's own objects**, and folds
  gcc/clang clone suffixes back into the parent before any set is taken. The two traps
  below are why each of those is spelled out.
* `bench/ccdead.py` (`make ccdead`) is the reachability half: call/jmp/lea edges out of
  `objdump -d`, seeded from `_start` and from every function address found in `.data`,
  `.rodata`, `.data.rel.ro` and `.init_array`. It cannot see an indirect call it has no
  data root for, so it errs toward calling a live function dead — read the native lanes
  as the floor (~6.5%, and mostly real dead code rather than scan error) and only a lane
  well clear of it as a finding. `CCDEAD_V=1` lists each lane's biggest dead symbols.

## ⚠ traps — each one ate a run before it was written down

* **a fresh worktree's first ccbench overcharges the mooncc BUILD row** (2026-08-11, the
  re-base fill): 43.1 s cold against 15.1 s warm on the same tree ten minutes apart —
  185 cold `mcobj` member compiles over a cold tree hash, paid once per tree. It reads
  exactly like a 3× build regression and is not one. The tell is the natives: they sit
  flat across the same pair, so a cost only mooncc pays and only once is caching, not
  codegen. Warm the tree, and never compare this row across worktrees. Nothing else in a
  fill is affected — sizes and insn counts are cache-independent.
* **the corpus arrives by REDIRECT, never down a pipe** — both feed it on stdin and both
  answer 3811 pass, so this is timing hygiene, not correctness: only a seekable fd 0 gets
  a read run, and a pipe drips the 953K bytes one syscall each. That's ~0.5 s of kernel
  time — the same work in all three lanes, so it only dilutes what the table is seeing.
* **a bench link owes `love_data.ld`** — the data sentinels' tiling IS love.h's ai_typ,
  and left to itself ld keeps each `love_data.N` an orphan in first-encountered order.
  gcc emits `love_data.7` first, so lvm_str lands below lvm_sym: `in_data` wraps unsigned
  and every string reads as a closure, hash takes the closure branch, and ttag walks off
  the heap during ai_ini's first intern. clang passed only by emitting in source order.
  Fixed 2026-08-10 — ccbench now links with `$LDFLAGS`, and gcc runs the corpus green.
  The .text rows below predate the fix and are still real: the orphans are their own
  sections either way, so `size -A` reads the same with the script and without.
* **the bench binaries are unbaked** — ccbench links without `image_ldflags`, so there is
  no `.image` section to bake into. Never compare their wall to a shipped love (a baked
  boot is ~33 ms; these boot in ~900). That's also why `bake` is off the roster, and
  `loader` is off because its seat probe writes to `<seat>/../lib` — the bench seat is
  `out/bench/cc/`, not `out/host/`.
* **`sh.l` needs `out/host/lush` built first** (`make out/host/lush`) — it's a dep of the
  make gate, not of `make host`; without it the lane reads as a mooncc failure and isn't.
* **`size -A`, never bare `size`** — Berkeley size(1) lumps .rodata into "text", which
  once mis-read the gap as near-parity. Compare the sections.
* **one lane's symbol roster cannot judge another's** — the mistake that put ~110 KB of
  love code in mooncc's "libc" column for two fills. A symbol missing from the native
  binary is not thereby runtime: gcc inlines 265 of love's statics out of existence, and
  a set difference taken against the *binary* reads every one of them as mooncc-only
  runtime. Take the difference against each lane's own **objects**, and canonicalize
  first — gcc ships `c0_lambda.isra.0` where mooncc ships `c0_lambda`, so 53 symbols
  (7.5 KB) land in the wrong column if the `.isra`/`.part`/`.constprop`/`.cold` suffix
  is not folded back into the parent.
* **shipped is not reachable, and for a libc the two barely relate** — `size -A` and
  ccsize.sh both read mooncc's libc as 1.59× musl's, which invites "mooncc's libc is
  slightly fat". 55% of it is code the binary cannot call, because nolibc.o is a single
  `.text` section and the linker discards by section; on what actually runs it is 0.76×.
  Any claim about a *libc's* size owes the reachability pass, and the natives are the
  control that says the pass is working.
* **a build lane can report `ok` having emitted a DIRECTORY** — `lane` gated on
  `[ -x "$bin" ]`, and a directory passes that. When the musl lanes first landed, the
  object dir was keyed on the binary's name, so `$WORK/love-gcc` was both; the build row
  timed a link that had failed and all four runtime rows read dnf. The gate is `-f` and
  `-x` now, and the objects live under `o-<binname>/`.
* **sub-% deltas are layout luck** — dyn-insn counts carry ±0.05% pad wiggle and wall
  carries BTB-lottery swings measured up to 4%. A ledger row moves when a ratio moves,
  not a third decimal.
* **love.c compiles in under 20 s** — time the TU whenever a pass lands in mooncc.
  Nothing else on this page gates the compiler's own speed.
