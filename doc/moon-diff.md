# moon-diff — mooncc vs gcc/clang, the running differential

A **living ledger** of the three-compiler race on the love host binary: how fast each
compiler *runs* (invocation speed), how big the code it lays is (.text), and how fast that
code *executes* (wall + user instructions on the test corpus and the host-nif suite).
Rows are dated and appended, newest first — trend is the point, not any single cell.

Two harnesses, both over the binaries `bench/ccbench.sh` leaves in `out/bench/cc/`:
`ccbench.sh` itself for build and corpus wall, `bench/ccsize.sh` for the `.text`
decomposition (`make ccbench`, `make ccsize`). The insn lanes are `perf stat -e
instructions:u`. **All three lanes are static** — mooncc against its own nolibc, gcc and
clang against musl — because that is the only shape in which the size rows mean
anything. Method and traps at the bottom — reproduce rather than trust.

This ledger is one of four docs that ride together: doc/moon-regalloc.md (the catalog
of the gap and the rung ledger — *why* a row moved), doc/hom.md (the design the
destination-die migration wears), doc/proto/dest.l (that design modeled runnable).

## 2026-08-11 — static musl, and the libc comes onto the ledger (HEAD c95a3da4)

Every size row before this one raced a static mooncc binary carrying its own nolibc
against *dynamic* gcc/clang binaries whose glibc sat off the ledger entirely — one
number answering two questions. **The native lanes are now `gcc-musl` and `clang-musl`**:
the same translation units through the musl wrappers, linked `-static`. Both passed the
corpus first try, no source change of any kind. (`CCGLIBC=1` puts the old dynamic lanes
back alongside, for continuity with the fills below.)

### .text — `size -A`

| | mooncc | gcc | clang | gcc-musl | clang-musl |
|---|---|---|---|---|---|
| .text (bytes) | 597,728 | 250,931 | 250,531 | 290,960 | 290,576 |
| mooncc ÷ | — | 2.38× | 2.39× | **2.05×** | **2.06×** |

**2.05× is the apples-to-apples headline, not 2.40×** — ~40 KB of the old gap was never
mooncc's code, it was glibc being absent from the file.

musl is the right static target and not just the available one: linked `-static` against
**glibc** the same units lay 774,381 bytes of `.text` (gcc; clang 774,061) in a 2.8 MB
file — 2.66× musl's, and **bigger than mooncc's whole binary**. Whatever else this page
says about mooncc's codegen, its static ELF is 0.77× the size of the one gcc lays when
gcc is held to the same self-sufficiency. A libc that was designed to be linked in is
the honest opponent; glibc was not.

Decomposed, each lane judged against **its own objects** (gcc's `.isra`/`.part` clones
folded back into the parent; sizes address-gap derived, so they carry inter-fn padding):

| | love's own C | libc in .text | libc syms |
|---|---|---|---|
| mooncc | 531,696 | 64,830 | 334 |
| gcc | 250,752 | 1,936 | 10 |
| clang | 250,384 | 1,904 | 10 |
| gcc-musl | 250,793 | 40,375 | 229 |
| clang-musl | 250,448 | 40,336 | 230 |

Two readings, and the second **corrects the fills below**:

* **mooncc's libc is the well-behaved half.** nolibc plus the syscall leaf is 64,830
  bytes against musl's 40,375 linked in — **1.61×**, the narrowest ratio on this page.
* **love's own C is 2.12×, not 1.75×.** The earlier fills read "mooncc's own
  libc/runtime, 666 symbols / ~170 KB" off the roster of symbols the *native binary
  lacked* — but ~110 KB of that roster is love code gcc inlined out of existence, not
  runtime. Against its own objects mooncc's libc is 334 symbols / 65 KB, and the whole
  love-code comparison is 531,696 vs 250,752 = **2.12×**.

That 2.12× splits in two, and only one half is codegen:

| | syms | mooncc | native | |
|---|---|---|---|---|
| both lanes emit | 613 | 421,888 | 250,752 | **1.68×** — codegen |
| mooncc emits, gcc doesn't | 265 | 109,808 | 0 | inlining |

The 1.68× is the differential the regalloc arc moves, and it reads 1.68×/1.69× whichever
libc the native lane rides — as it must, the libc not touching how love.c compiles. The
other 265 are love statics the natives emit no code for at all (`ana_d`, `copy_data`,
`cb_csi`, `rbig`, `obin_run`): **21% of mooncc's love .text is functions gcc inlines**,
a lever the per-symbol number cannot see. No native-only symbols exist — every symbol
gcc emits is in the shared set.

### runtime — musl moves nothing

| | mooncc | gcc | gcc-musl | clang | clang-musl |
|---|---|---|---|---|---|
| corpus insns (G, user) | 42.52 | 23.30 | 23.28 | 25.32 | 25.31 |
| corpus cycles (G, user) | 15.66 | 12.31 | 12.33 | 12.48 | 12.53 |
| egg boot insns (G) | 9.17 | 5.18 | 5.18 | 5.61 | 5.61 |
| build wall (s) | 14.6 | 9.2 | 9.3 | 5.4 | 5.4 |
| chacha20 (ms) | 1083.7 | 274.6 | 280.6 | 187.9 | 166.2 |
| poly1305 (ms) | 1508.5 | 1370.8 | 1367.5 | 823.4 | 842.3 |

The three perf-counted rows are under 0.1% apart across each libc pair: love allocates,
formats and copies through its own floor, so libc barely runs. That is the licence to
move the size lane onto static musl and leave the runtime story alone — the corpus rows
below compare straight across the change. The wall rows are noisier, as wall rows are:
gcc's cipher pair agrees to 2%, clang's spreads 13% on chacha with no counted difference
under it, so read the insn rows and not those two cells.

Behaviour holds where the two libcs actually differ, too — `net`, `tls`, `tlsc`, `pty`
and `fs` off the hostnif roster pass on both static-musl binaries (static musl carries a
working resolver, the thing static glibc will not do). The rest of that roster was not
run on these lanes.

### aside — tcc cannot build love

Probed the same day, since a fourth C compiler would be a fourth column. tcc 0.9.28
compiles `love.c` and, given musl's headers, every other translation unit — but it does
not produce a love, and the reasons are structural rather than a missing flag:

* **nine `__builtin_*` it does not have** — `add`/`sub`/`mul_overflow`, `clzll`, `trap`,
  `inf`, `nanf`, `isinf`, `___clear_cache`. Each becomes an implicit declaration, so
  they link as ordinary calls, and three of them are the trap: an implicit declaration
  returns `int`, so `dv = __builtin_inf()` and love.h's `NAN` would be silently wrong
  rather than loud. A shim cannot fix what the call site already got wrong.
* **no `__int128`** — glibc's `<link.h>` needs it, so `host/image.c` will not preprocess
  at all against the system headers. musl's headers dodge this one.
* **no `musttail`** — love.h's guard names mooncc, clang and gcc≥15, so under tcc
  `ai_musttail` silently expands to nothing and every VM tail becomes a call that
  returns. The tail-threaded VM is the design (`make vmret` exists to hold it), so this
  is not a quality-of-implementation gap; it is the one instrument tcc lacks.

⚠ and the `#else` branch love.h keeps for exactly such a compiler — `ai_tco=0`, the
plain-return interpreter — **does not work under gcc either**: same flags, `-Dai_tco=1`
runs and `-Dai_tco=0` segfaults on `(+ 1 2)`. So there is no fallback shape for tcc to
take even if the builtins were dealt with, and that dead branch is its own bug, not a
finding about tcc.

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

⚠ superseded, same method error as the fill below it — and the 2.40× is against dynamic
binaries; the static-musl section at the top is the comparison.

Why 2.40× — measured per symbol this fill (nm over the pair; the natives are dynamic
against glibc, mooncc a static ELF carrying its own nolibc, so only the 614 shared C
symbols compare): 428 KB vs 246 KB, **1.74×** of genuinely emitted code. The gap is
instruction COUNT, not encoding: lvm_add_string (a 3× representative) lays 1132 insns
vs clang's 396 at the same ~4 bytes/insn. Of its 454 movs, **271 are stack-slot
traffic** (137 reloads + 134 spills; clang: 6) — the write-through discipline in
call-dense, branch-dense dispatch code: pins die at every call (cs seats ride only
loop keeps, and these fns have no loops), and a leaf materialization (the lea of a
type sigil) clobbers the pinned scratch, forcing a reload the next compare. Secondary:
no CSE (the same tag word reloads per test), no tail merge or identical-code folding
(clang folds lvm_chain to a 5-byte alias of a twin; mooncc lays every epilogue in
full), immediates rematerialized per use. The spread is broad — 248 of 614 symbols
above 2×, but 105 at or under parity (−27 KB: clang paying inlining bytes mooncc
doesn't). The size gap and the corpus's flat 1.71× insn ratio are one fact seen
twice: the slot movs are cheap (IPC eats them — cycles sit at 1.23×) but they are
most of the extra instructions and most of the extra bytes.

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

⚠ superseded — the decomposition below reads the "own libc/runtime" set off the symbols
the *native binary* lacked, which files every love static gcc inlined away as mooncc
runtime. See the static-musl section at the top: 65 KB and 2.12×, not 170 KB and 1.75×.

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
* **one lane's symbol roster cannot judge another's** — the mistake that put ~110 KB of
  love code in mooncc's "libc" column for two fills. A symbol missing from the native
  binary is not thereby runtime: gcc inlines 265 of love's statics out of existence, and
  a set difference taken against the *binary* reads every one of them as mooncc-only
  runtime. Take the difference against each lane's own **objects**, and canonicalize
  first — gcc ships `c0_lambda.isra.0` where mooncc ships `c0_lambda`, so 53 symbols
  (7.5 KB) land in the wrong column if the `.isra`/`.part`/`.constprop`/`.cold` suffix
  is not folded back into the parent.
* **a build lane can report `ok` having emitted a DIRECTORY** — `lane` gated on
  `[ -x "$bin" ]`, and a directory passes that. When the musl lanes first landed, the
  object dir was keyed on the binary's name, so `$WORK/love-gcc` was both; the build row
  timed a link that had failed and all four runtime rows read dnf. The gate is `-f` and
  `-x` now, and the objects live under `o-<binname>/`.
* **sub-% deltas are layout luck** — dyn-insn counts carry ±0.05% pad wiggle and wall
  carries BTB-lottery swings measured up to 4%. A ledger row moves when a ratio moves,
  not a third decimal.
