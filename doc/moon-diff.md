# moon-diff — mooncc vs gcc/clang, the running differential

A **living ledger** of the three-compiler race on the love host binary: how fast each
compiler *runs* (invocation speed), how big the code it lays is (.text), and how fast that
code *executes* (wall + user instructions on the test corpus and the host-nif suite).
Rows are dated and appended, newest first — trend is the point, not any single cell.

The harness is `bench/ccbench.sh` (build + corpus wall; it leaves the three binaries in
`out/bench/cc/`); the insn and size lanes below are `perf stat -e instructions:u` and
`size -A` over those same binaries. Method and traps at the bottom — reproduce rather
than trust.

## 2026-08-10 — first fill (HEAD 9d719a28)

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

* **the corpus goes in as a FILE, never on stdin** — the corpus tests stdin (io.l reads
  it), so a piped run desyncs the reader mid-suite and every lane looks dnf. The
  test.mk:41 discipline applies here too.
* **the gcc runtime column is dnf**: gcc 16.1.1 -O2 builds a love that segfaults at boot,
  before any test. Pre-existing and undiagnosed — nothing in the tree builds with gcc
  (the default is mooncc, `.hostcc` picks clang). Its build-time and .text rows are still
  real; delete this bullet when the crash is found.
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
