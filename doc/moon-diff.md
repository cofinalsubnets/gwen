# moon-diff — mooncc vs gcc/clang, the running differential

A **living ledger** of the three-compiler race on the love host binary: how fast each
compiler *runs* (invocation speed), how big the code it lays is (.text), and how fast that
code *executes* (wall + user instructions on the test corpus and the host-nif suite).
Rows are dated and appended, newest first — trend is the point, not any single cell.

The harness is `bench/ccbench.sh` (build + corpus wall; it leaves the three binaries in
`out/bench/cc/`); the insn and size lanes below are `perf stat -e instructions:u` and
`size -A` over those same binaries. Method and traps at the bottom — reproduce rather
than trust.

This ledger is one of four docs that ride together: doc/moon-regalloc.md (the catalog
of the gap and the rung ledger — *why* a row moved), doc/hom.md (the design the
destination-die migration wears), doc/proto/dest.l (that design modeled runnable).

## 2026-08-11 — after the allocator arc (HEAD c83b19a8)

Same box, same method. Between the fills the vmap crossed every boundary it had
(loop heads, calls, seat exhaustion, entry seeding, splices, element pins, the loop
exit) — doc/moon-regalloc.md's 2026-08-10/11 ledger entries are the why per row.

### invocation speed

| | mooncc | gcc | clang |
|---|---|---|---|
| full build + link (s) | 14.7 | 8.7 | 5.1 |

mooncc's own invocation cost +6% over the first fill (13.9 → 14.7 s): the keeps buy
their soundness with regen attempts (a barred entry rebuilds the fn), and the meet
machinery rides every loop. The price of the rows below.

### text size — `size -A`

| | mooncc | gcc | clang |
|---|---|---|---|
| .text (bytes) | 601,824 | 250,931 | 250,627 |

mooncc +1.4% (cs saves, seat movs, roster reloads lay real bytes), the natives +2.6%
(love.c itself grew); the ratio nudged 2.43× → 2.40×.

### runtime — the corpus, egg-boot subtracted, median of 3

| | mooncc | clang | ratio | gcc |
|---|---|---|---|---|
| corpus insns (G, user) | 43.39 | 25.40 | **1.71×** | 23.33 |
| corpus cycles (G, user) | 15.41 | 12.54 | 1.23× | 12.44 |
| egg boot insns (G) | 9.38 | 5.60 | 1.68× | 5.18 |

The gcc column fills for the first time (the love_data.ld fix held): gcc lays FEWER
corpus instructions than clang here — mooncc/gcc is **1.86×**, the harder number.
**The corpus ratio did not move (1.71× → 1.71×), and that is the honest reading, not
a null result**: the corpus's hot symbols are the VM dispatch lanes (lvm_cur, lvm_qap,
lvm_eq) — tail-threaded musttail chains the keep machinery deliberately never touches
(no loops to keep, scratch dies at every jump). The arc's wins live where loops live:

### the pair that says where the gap is — wall, boot subtracted

| | mooncc | gcc | clang | mooncc/clang |
|---|---|---|---|---|
| chacha20 (ms) | 1059.6 | 279.8 | 165.8 | **6.4×** (was ~23×) |
| poly1305 (ms) | 1424.5 | 1317.9 | 776.9 | 1.83× (gcc 1.08×) |

chacha20 — the array-indexed inner loop, the shape the first fill named as the
23× outlier — dropped to 6.4× vs clang and 3.8× vs gcc: loop-head survival, element
pins across calls and the exit meet are exactly that shape's levers. poly1305 (five
scalar limbs, the shape mooncc already held) sits at 1.08× of gcc. The pair still
reads array-slots-are-the-gap, but the outlier is now a ratio, not a scandal; what
remains of it is the pre-call park pair and the residues doc/moon-regalloc.md lists.



Ryzen 7 5825U, 16 threads, quiet box. gcc 16.1.1, clang 22.1.8, mooncc at HEAD
(warm off `mooncc.image`).

### invocation speed — source to runnable binary, ccache off

| | mooncc | gcc | clang |
|---|---|---|---|
| full build + link (s) | 13.9 | 9.1 | 5.4 |
| love.c alone, median of 3 (s) | 9.0 | 8.1 | 4.9 |

mooncc compiles the 7.8-kloc love.c in 1.8× clang's time and ~1.1× gcc's — a compiler
written in love holding within 2× of the natives at their own -O2 job.

### text size — `size -A`, the section, never size(1)

| | mooncc | gcc | clang |
|---|---|---|---|
| .text (bytes) | 593,632 | 244,499 | 244,531 |
| .rodata (bytes) | 364,544 | 360,464 | 361,008 |

the 2.43× headline decomposes: 666 symbols (~170 KB) are mooncc's **own libc/runtime**
(nolibc, `__fmt*`, `__dnsq`, rbig, the am floor) — the clang lane rides shared glibc,
off its ledger. Over the 610 *shared* C symbols it's 421 KB vs 240 KB — **1.75×** of
genuinely emitted code, .rodata at parity. 1.75× is the codegen number to track; 2.43×
is what ships. (File sizes don't compare: the gcc/clang lanes carry `-g`; mooncc
per-symbol sizes are address-gap derived, so they carry inter-fn padding, ~1–2%.)

### runtime — the corpus, egg-boot subtracted, median of 3

| | mooncc | clang | ratio | gcc |
|---|---|---|---|---|
| corpus insns (G, user) | 43.87 | 25.66 | **1.71×** | dnf |
| corpus wall (ms) | 4706 | 3969 | 1.19× | dnf |
| egg boot insns (G) | 9.56 | 5.57 | 1.72× | dnf |
| egg boot wall (ms) | 962 | 823 | 1.17× | dnf |

gcc's dnf was the missing `love_data.ld` (traps below), fixed the same day — the column
is fillable now and empty only because this fill predates it.

The insn ratio is the codegen differential; the wall ratio is softer because mooncc's
extra instructions are cheap and run at higher IPC — measured this fill: IPC 2.98 vs
2.05, cycles 17.9G vs 15.2G (**1.18×**, and wall tracks cycles). The corpus and the
boot agree at 1.7× — the boot *is* the compiler compiling, so that's one story told
twice. Per-symbol, the profiles are the same roster (the VM dispatch lanes) and the
gap is broad, not one villain: lvm_cur and lvm_qap ~2.6× insns each, lvm_eq ~2.1×.

### runtime — host nifs, 28-file roster, 28 boots subtracted, single pass

| | mooncc | clang | ratio |
|---|---|---|---|
| hostnif insns (G, user) | 729.7 | 677.4 | 1.08× |
| hostnif wall (s) | 94.6 | 90.4 | 1.05× |

A smoke-level differential only: the lane is wait-dominated (pty, net, tasks, timers),
and perf counts the whole process tree — sh.l spawns the *tree's* love + lush, so
children dilute the ratio. The corpus row is the clean number; this row exists to catch
a lane that regresses *disproportionately* (nif-heavy paths, syscall glue), not to be
read as a codegen gap.

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

## ⚠ traps — each one ate a run before it was written down

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
* **sub-% deltas are layout luck** — dyn-insn counts carry ±0.05% pad wiggle and wall
  carries BTB-lottery swings measured up to 4%. A ledger row moves when a ratio moves,
  not a third decimal.
