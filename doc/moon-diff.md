# moon-diff — mooncc vs gcc/clang, the running differential

A **living ledger** of the three-compiler race on the love host binary: how fast each
compiler *runs* (invocation speed), how big the code it lays is (.text), and how fast that
code *executes* (wall + user instructions on the test corpus and the host-nif suite).
Rows are dated and appended, newest first — trend is the point, not any single cell.

Three harnesses, all over the binaries `bench/ccbench.sh` leaves in `out/bench/cc/`:
`ccbench.sh` itself for build and corpus wall, `bench/ccsize.sh` for the `.text`
decomposition, `bench/ccdead.py` for how much of each libc is reachable at all
(`make ccbench`, `make ccsize`, `make ccdead`). The insn lanes are `perf stat -e
instructions:u`. **All three lanes are static** — mooncc against its own nolibc, gcc and
clang against musl — because that is the only shape in which the size rows mean
anything. Method and traps at the bottom — reproduce rather than trust.

This ledger is one of four docs that ride together: doc/moon-regalloc.md (the catalog
of the gap and the rung ledger — *why* a row moved), doc/hom.md (the design the
destination-die migration wears), doc/proto/dest.l (that design modeled runnable).

## 2026-08-11 — static musl, and the libc comes onto the ledger (HEAD b0f92305)

Every size row before this one raced a static mooncc binary carrying its own nolibc
against *dynamic* gcc/clang binaries whose glibc sat off the ledger entirely — one
number answering two questions. **The native lanes are now `gcc-musl` and `clang-musl`**:
the same translation units through the musl wrappers, linked `-static`. Both passed the
corpus first try, no source change of any kind. (`CCGLIBC=1` puts the old dynamic lanes
back alongside, and they are kept below because every earlier fill quotes them.)

Measured on the fill below's tree, so the mooncc column is the same pmin-gated compiler
and the dynamic columns reproduce its numbers exactly — the method changed, not the
subject.

### .text — `size -A`

| | mooncc | gcc-musl | clang-musl | gcc (dyn) | clang (dyn) |
|---|---|---|---|---|---|
| .text (bytes) | 581,344 | 291,792 | 291,216 | 251,763 | 251,123 |
| mooncc ÷ | — | **1.99×** | **2.00×** | 2.31× | 2.32× |

**1.99× is the apples-to-apples headline, not 2.31×** — ~40 KB of the old gap was never
mooncc's code, it was glibc being absent from the file. Every fill below quotes the
dynamic column; read them as that column, not this one.

musl is the right static target and not merely the available one: linked `-static`
against **glibc** the same units lay 775,213 bytes of `.text` in a 2.8 MB file — 2.66×
musl's, and **1.33× mooncc's whole binary**. Whatever else this page says about mooncc's
codegen, its static ELF is 0.75× the size of the one gcc lays when gcc is held to the
same self-sufficiency. A libc designed to be linked in is the honest opponent.

Decomposed by `bench/ccsize.sh`, each lane judged against **its own objects** (gcc's
`.isra`/`.part` clones folded back into the parent; sizes address-gap derived, so they
carry inter-fn padding):

| | love's own C | libc in .text | libc syms |
|---|---|---|---|
| mooncc | 515,040 | 64,110 | 334 |
| gcc-musl | 251,625 | 40,375 | 229 |
| clang-musl | 251,056 | 40,368 | 230 |
| gcc (dyn) | 251,584 | 1,936 | 10 |
| clang (dyn) | 250,976 | 1,904 | 10 |

Two readings, and the second **corrects every fill below**:

* **mooncc's libc is the well-behaved half.** nolibc plus the syscall leaf is 64,110
  bytes against musl's 40,375 linked in — **1.59×**, the narrowest ratio on this page,
  and the next section takes that 1.59× apart: almost none of it is code.
* **love's own C is 2.05×, and mooncc's libc is 64 KB, not ~167.** The fills below read
  the "own libc/runtime" set off the roster of symbols the *native binary lacked* — but
  ~106 KB of that roster is love code gcc inlined out of existence, not runtime. Judged
  against its own objects mooncc's libc is 334 symbols / 64 KB, and the whole love-code
  comparison is 515,040 vs 251,625 = **2.05×**. The "~170 KB" and "167 KB" figures below
  are that error, not a measurement that moved.

That 2.05× splits in two, and only one half is codegen:

| | syms | mooncc | native | |
|---|---|---|---|---|
| both lanes emit | 612 | 409,256 | 251,625 | **1.63×** — codegen |
| mooncc emits, gcc doesn't | 265 | 105,784 | 0 | inlining |

The 1.63× is the differential the regalloc and hom arcs move, and it reads 1.63×/1.64×
whichever libc the native lane rides — as it must, the libc not touching how love.c
compiles. The other 265 are love statics the natives emit no code for at all (`ana_d`,
`copy_data`, `cb_csi`, `rbig`, `obin_run`): **21% of mooncc's love .text is functions gcc
inlines**, a lever the per-symbol number cannot see. No native-only symbols exist — every
symbol gcc emits is in the shared set.

### the libc's 1.59× is packaging, and reverses when read live

`bench/ccdead.py` (`make ccdead`) asks the other question: not how many libc bytes a lane
*ships* but how many it can ever *call*. It walks call/jmp/lea targets out of the
disassembly, seeded from `_start` and from every function address sitting in a data
section — a vtable entry is reached no other way.

| | libc syms | shipped | reachable | dead | |
|---|---|---|---|---|---|
| mooncc | 334 | 64,110 | 28,794 | 35,316 | **55.1%** |
| gcc-musl | 229 | 40,375 | 37,749 | 2,626 | 6.5% |
| clang-musl | 230 | 40,368 | 37,758 | 2,610 | 6.5% |
| gcc (dyn, `CCGLIBC=1`) | 10 | 1,936 | 1,808 | 128 | 6.6% |

**The native rows are the control**, and reading all 28 of gcc-musl's is what says the
scan works: hardly any of them are scan misses. `pad` (256 B) has *zero* references
anywhere in the binary — gcc inlined it into `printf_core` and left the out-of-line copy;
`putenv` is dead while `__putenv` is live, the public entry riding along with the
`setenv` its object shares; `umount`/`umount2` ride together the same way. The rest is
crt (`deregister_tm_clones`, `libc_start_init`) and alternates the static link did not
pick (`__simple_malloc`, `static_init_tls`, `static_dl_iterate_phdr`). So 6.5% is not
noise — it is mostly real dead code, and it is what a libc *built* for static linking
still cannot shed. mooncc's 55% is eight times that floor.

The cause is granularity, not code quality. **`nolibc.o` carries one `.text` section of
63,887 bytes**, and a section is the linker's unit of discard — all or nothing. musl
compiles roughly one function per object, so its static link drops what love never calls.
What mooncc therefore ships and cannot reach: `__dnsq` 2,514 · `strftime` 1,634 ·
`asctime` 1,598 · `popen` 1,506 · `gmtime` 1,158 · `__vfscanf` 1,048, then `qsort`,
`system`, `mktemp`, the exec family. `getaddrinfo` and `strtol` are on that list and are
the two worth checking, since love calling either would be a hole in the scan — both
appear in the tree **only in comments**, each marking its own removal (host/sock.c:15,
*"getaddrinfo is what used to make connect the exception, and it is gone"*).

Live against live, the ratio turns over:

| | syms | .text | |
|---|---|---|---|
| mooncc, reachable | 130 | 28,794 | **0.76×** |
| gcc-musl, reachable | 201 | 37,749 | — |

and 97% of that 8,955-byte gap is two clusters:

| | mooncc | gcc-musl | |
|---|---|---|---|
| malloc | 672 | 8,483 | −7,811 |
| printf | 11,122 | 12,029 | −907 |

malloc alone is 87% of it, and it is a fit rather than a win: love brings its own
two-space heap and mallocs pools — big and rare — so nolibc's K&R first-fit over 1 MB
mmap arenas is right-sized where musl's mallocng buys it nothing but `alloc_slot` (2,528)
and the meta machinery. printf is the honest read on the same job done twice: `__fmtflo`
+ `__fmt` + `__fmtnum` against `printf_core` + `pop_arg` + `wcrtomb`, and mooncc's is the
smaller of the two.

**So on the code both lanes actually run, mooncc's libc is not the well-behaved half by
courtesy — it is smaller.** The ~35 KB is 3.5% of the whole binary and is recoverable by
`-ffunction-sections` in mooncc plus `--gc-sections` in holo; neither exists today, and
how hard either is was not costed. ⚠ the scan's error is one-directional — an indirect
call it misses marks a live function dead, never the reverse — so 28,794 is a lower bound
on live and 35,316 an upper bound on dead.

### runtime — musl moves nothing

| | mooncc | gcc-musl | gcc (dyn) | clang-musl | clang (dyn) |
|---|---|---|---|---|---|
| corpus insns (G, user) | 40.71 | 23.08 | 23.08 | 24.97 | 24.98 |
| corpus cycles (G, user) | 15.02 | 11.74 | 11.67 | 12.25 | 12.04 |
| egg boot insns (G) | 8.92 | 5.25 | 5.25 | 5.70 | 5.70 |

Each libc pair agrees to 0.05% on corpus insns, and the boot rows are identical to three
digits: love allocates, formats and copies through its own floor, so libc barely runs.
That is the licence to move the size lane onto static musl and leave the runtime story
alone — the corpus rows below compare straight across the change. Cycles are looser
(clang's pair spreads 1.7%), as cycles are; read the insn rows.

The corpus ratio here is mooncc/clang **1.63×**, the fill below's number on the fill
below's corpus, reached independently — which is the check that matters for a method
change: the size lane moved, the runtime lane did not.

Behaviour holds where the two libcs actually differ, too — `net`, `tls`, `tlsc`, `pty`
and `fs` off the hostnif roster pass on both static-musl binaries (static musl carries a
working resolver, the thing static glibc will not do). The rest of that roster was not
run on these lanes.

### aside — tcc cannot build love

Probed the same day, since a fourth C compiler would be a fourth column. tcc 0.9.28
compiles `love.c` and, given musl's headers, every other translation unit — but it does
not produce a love, and the reasons are structural rather than a missing flag:

* **nine `__builtin_*` it does not have** — `add`/`sub`/`mul_overflow`, `clzll`, `trap`,
  `inf`, `nanf`, `isinf`, `___clear_cache`. As bare implicit declarations three of them
  are silently wrong (an implicit declaration returns `int`, so `dv = __builtin_inf()`
  converts one), though a *prototyped* polyfill header fixes that — and `-include` means
  no source change is needed to supply one.
* **no `__int128`** — glibc's `<link.h>` needs it, so `host/image.c` will not preprocess
  at all against the system headers. musl's headers dodge this one.
* **no `musttail`** — love.h's guard names mooncc, clang and gcc≥15, so under tcc
  `ai_musttail` silently expands to nothing and every VM tail becomes a call that
  returns. The tail-threaded VM is the design (`make vmret` exists to hold it), so this
  is not a quality-of-implementation gap; it is the one instrument tcc lacks.

⚠ and the `#else` branch love.h keeps for exactly such a compiler — `ai_tco=0`, the
plain-return interpreter — **does not work under gcc either**: same flags, `-Dai_tco=1`
runs and `-Dai_tco=0` faults in `ttag` walking off the heap, reached from `eqv` →
`clo_load`. Not the `love_data.ld` trap below — the sentinel order is correct in both
lanes. So there is no fallback shape for tcc to take even if the builtins were dealt
with, and that dead branch is its own bug, not a finding about tcc.

chibicc gets further — with musl's headers and that polyfill it compiles every TU — but
it has no `_Static_assert` at all, and love.c uses C23 labels before declarations.
contrib/chibicc/ carries the one thing that was upstream's own bug.

## 2026-08-11 — after the cs-seat rung, pmin-gated (HEAD 3dd9e1fc)

Third fill today. Between it and the last: the callish cs-homes rung, landed
pmin-GATED after its own A/B — doc/moon-regalloc.md's ledger carries the story (the
fat grant read −1.6% static insns and measured **+2.7% dynamic**; a per-invocation
save cannot buy a per-call saving in early-out code) — and the str-juxt arc moved
love.c/ev/prel and grew the corpus ~70 tests per target. ⚠ So the corpus rows
RE-BASE here: not comparable backward (the 2026-07-18 lesson again); the clean
cross-rung statement is the same-corpus A/B in the ledger, which measured the landed
grant dynamically EXACT (insns flat to the third digit, cycles −0.9%). This fill's
walls ran on a shared-loaded box (load ~2, another session on the tree): absolute
walls +10%, ratios stand, insn lanes unaffected (user-scoped).

### text size — `size -A`

| | mooncc | gcc | clang |
|---|---|---|---|
| .text (bytes) | 581,344 | 251,763 | 251,123 |

2.32× → **2.31×**, shared-symbol 1.67× (flat): str-juxt's love.c growth and the
pmin-gated grant's trim (13 symbols, −1,020 B, ai_ini_0 −736 the biggest) net out.
The unshipped fat grant had read 573,152 / 2.28× / 1.64× — that 8 KB is real and
waits on shrink-wrap (saves at the callish region's head, so a fast path never
pays), the allocator leg's next boundary.

### runtime — the corpus (NEW BASE), egg-boot subtracted, median of 3

| | mooncc | clang | ratio | gcc |
|---|---|---|---|---|
| corpus insns (G, user) | 40.95 | 25.17 | **1.63×** | 23.23 |
| corpus cycles (G, user) | 15.23 | 12.04 | 1.27× | 11.92 |
| egg boot insns (G) | 8.92 | 5.70 | 1.57× | 5.25 |

The 1.60× → 1.63× move against the last fill is the CORPUS moving, not the codegen:
the str-juxt tests lean on the string lanes (mooncc's 2.4–3× band), and the same-
corpus A/B pinned the rung itself at exactly flat. 1.63× is the number to beat on
this corpus.

### the pair — wall, boot subtracted (loaded box: ratios only)

| | mooncc | gcc | clang | mooncc/clang |
|---|---|---|---|---|
| chacha20 (ms) | 1173 | 325 | 199 | 5.9× |
| poly1305 (ms) | 1490 | 1460 | 891 | 1.67× (gcc 1.02×) |

flat, the expected null — the cs-seat rung's levers are call boundaries, not array
slots.

The methodological catch this fill exists to record: the static ledger (insns, .text,
per-symbol growers) APPROVED the fat grant unanimously, and only the corpus row
caught the +2.7% — the differential is the instrument that reads invocation mix,
which no static count sees. A codegen rung that changes per-invocation costs owes a
corpus A/B before it ships.

## 2026-08-11 — after the hom ladder (HEAD d83892e3)

Same box, same method, hours after the previous fill. Between them sits the whole
destination-die migration (doc/moon-hom.md rungs 0–3 and the addrfold residue rungs:
faces at birth, face-direct loads, stores and post tails, the ptr±const fold, the sp
cell demoted to last resort) — ~210 net lines of gen.l. love.c moved by one nif
(lvm_myself), so the native columns are the control: both sat still.

### invocation speed

| | mooncc | gcc | clang |
|---|---|---|---|
| full build + link (s) | 14.2 | 9.0 | 5.4 |

flat — the face machinery reads state clval already had; no new passes, no regen.

### text size — `size -A`

| | mooncc | gcc | clang |
|---|---|---|---|
| .text (bytes) | 581,344 | 250,931 | 250,515 |

mooncc **−20,480 bytes (−3.4%)** against still natives: 2.40× → **2.32×**. Per symbol
(same 604-shared-C-symbol comparison): 411 KB vs 245 KB, 1.74× → **1.67×** of genuinely
emitted code; mooncc's own libc/runtime lane trimmed too (~170 → 167 KB, nolibc is
compiled by the same faces). ⚠ that 167 KB is the mis-attribution the static-musl
section at the top corrects — most of it is love code gcc inlines away, and mooncc's
actual libc is 64 KB. The `.text` rows here stand; the libc split does not. The representative recount: lvm_add_string 1132 → 1086
insns, its rsp-slot movs 271 → 255 (clang: 6) — the write-through discipline at calls
is still most of the remaining gap; the faces removed the *address* traffic (the lea +
seat movs around member access), not the slot traffic.

### runtime — the corpus, egg-boot subtracted, median of 3

| | mooncc | clang | ratio | gcc |
|---|---|---|---|---|
| corpus insns (G, user) | 40.63 | 25.43 | **1.60×** | 23.37 |
| corpus cycles (G, user) | 15.70 | 12.51 | 1.26× | 12.39 |
| egg boot insns (G) | 8.79 | 5.61 | 1.57× | 5.18 |

**The corpus ratio moved for the first time: 1.71× → 1.60×** (mooncc/gcc 1.86× → 1.74×),
natives flat to three digits — the delta is all mooncc. The previous fill's reading
called it: the corpus's hot symbols are the tail-threaded dispatch lanes the keep
machinery never touches, straight-line member-access code — which is exactly the shape
the faces compile. The boot row agrees twice over: mooncc's own boot insns dropped 6.3%
(9.38 → 8.79 G) — the boot IS the compiler compiling, so the arc shrank both the code
it lays and the work of laying it. The two arcs are complements: the allocator arc moved
the loop shapes, the hom arc moved the straight-line ones.

### the pair — wall, boot subtracted

| | mooncc | gcc | clang | mooncc/clang |
|---|---|---|---|---|
| chacha20 (ms) | 1055.6 | 275.4 | 174.7 | 6.0× |
| poly1305 (ms) | 1377.0 | 1316.4 | 805.0 | 1.71× (gcc 1.05×) |

both flat within wall wiggle, and that is the expected null: the pair reads array-slot
residency, and the hom arc's levers are member faces. What remains of chacha's ratio is
the pre-call park pair and doc/moon-regalloc.md's residues, unchanged by this arc.

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
* `bench/ccdead.py` (`make ccdead`) is the reachability half: call/jmp/lea edges out of
  `objdump -d`, seeded from `_start` and from every function address found in `.data`,
  `.rodata`, `.data.rel.ro` and `.init_array`. It cannot see an indirect call it has no
  data root for, so it errs toward calling a live function dead — read the native lanes
  as the floor (~6.5%, and mostly real dead code rather than scan error) and only a lane
  well clear of it as a finding. `CCDEAD_V=1` lists each lane's biggest dead symbols.

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
