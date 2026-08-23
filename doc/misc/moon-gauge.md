# moon-gauge — what mooncc's codegen is measured against, and what residency is worth

## the target application is love itself

Codegen quality is gauged on **love**: `ccbench`'s corpus row, which builds the host binary
with each compiler and runs the arch-neutral test corpus through it. That workload is
love.c's VM — call- and branch-dense dispatch — and it is what mooncc exists to compile.

The cipher rows (chacha20, poly1305, `bench/ccrypto.l`) are **subsidiary**. They are a
lever gauge, not a target: chacha indexes a 16-word array in its inner loop and poly keeps
five scalar limbs, so the pair reads whether a gap is array slots or general residency. A
win there is worth having and is not the objective. ⚠ but do not read them to zero either —
`core/love.c:6138`'s z-tray comparison is the same array-indexed shape.

⚠ **AND THE PAIR WAS CONFOUNDED** — chacha rotates 320 times a block and poly1305 not once,
so the pair separates rotate-heavy from rotate-free just as cleanly as it separates array
from scalar, and mooncc emitted no rotate instruction at all. Resolved 2026-08-22: gen.l
learned the idiom and the re-fill (*the rotate lands*, below) split the two signals — the
pair reads array vs scalar again.

⚠ **the corpus average is flattering and the pair exists because of it** (`ccbench.sh`'s own
header says so). An arc that reports only the corpus row will under-weight array work; an arc
that reports only the pair will over-weight it. Both rows, every time.

The **heavy-nif rows** (inflate, crc32, sha256 — `bench/cnifs.l`, added 2026-08-17) are a
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
`bench/Makefile` rebuilt `out/host/mooncc.image` as a prerequisite of the very
target. So every run missed and paid a one-time **libc build** inside a per-build row,
against gcc and clang linking a musl somebody else compiled. Measured directly: 43,754 ms
after `touch out/host/mooncc.image`, 20,139 ms without. The lane now runs **the artifact**,
whose baked image keys as the word `"<baked>"` and therefore survives every rebuild of the
intermediates (`touch out/host/mooncc.image out/host/love` leaves it at 18,905 ms), plus
one untimed build so the row means one thing. ⚠ a one-line C file does not warm the
archive in its place — a program that needs no member pulls none, and it links in 94 ms.

⚠ **sha256 is the row worth reading, and it was not chosen to prove anything.** It sits at
4.23×, next door to chacha's 5.80×, while crc32 — the same kind of table work with no array
carried across the loop — sits at 1.43×. inflate at 1.82× is the branchy end and says the
branch path is not where mooncc is losing.

### ⚠ and the reason is the ROTATE, not the array — read this before the section above

This file said sha256 corroborated the array-slot diagnosis, on the strength of its 64-word
message schedule. **That was wrong, or at best half true**, and taking the row apart says so:

| | insns/iter | memory ops/iter | |
|---|---:|---:|---|
| schedule loop, mooncc | 144 | 55 | 48 iterations |
| schedule loop, gcc | 36 | 4 | |
| compression loop, mooncc | 209 | 69 | 64 iterations |
| compression loop, gcc | 48 | 3 | |

Dynamically that is **5.06× the instructions** (34.4 G against 6.80 G for the row) at a
*higher* IPC — 5.89 against 5.32 — so nothing is stalling. mooncc simply executes five times
the work, all of it hitting L1.

And here is where it goes. For `rr(e, 6)` gcc emits **one instruction**, `ror $0xb,%r14d`.
mooncc emits sixteen:

    mov  %r10,0x40(%rsp)        ; e, to a frame slot
    movq $0x6,0x48(%rsp)        ; ..and the CONSTANT 6, to another
    mov  0x40(%rsp),%r8d        ; reload e
    movslq 0x48(%rsp),%rcx      ; reload 6
    shr  %cl,%rax               ; a VARIABLE shift by it
    rex mov 0x40(%rsp),%esi     ; reload e again
    mov  $0x20,%r9d
    movslq 0x48(%rsp),%rax      ; reload 6 again
    sub  %rax,%r9               ; 32 - 6
    shl  %cl,%rax  ...  or

Two separable defects, and neither is register pressure: **the rotate idiom
`(x >> n) | (x << (32 - n))` is never recognized**, and **an inlined body does not
constant-propagate** — the literal 6 is materialized to the frame and read back twice, which
is also why the shifts are `%cl` variable forms instead of immediates. Counted over the whole
translation unit, **mooncc emits 0 rotate instructions where gcc emits 6 in `host/hash.c` and
32 in `host/tls.c`.**

sha_block does 4 rotates per schedule iteration and 6 per compression iteration — 576 a
block. At ~15 excess instructions each that is ~8,600 of the ~16,000 excess, so the rotate is
about **half** the gap on its own.

⚠ **THIS CONFOUNDS THE CIPHER PAIR.** chacha does 320 rotates a block; poly1305 does **none**,
and neither does crc32, and neither does inflate. The rows split exactly on rotates —
5.80× and 4.23× with them, 1.55×/1.43×/1.82× without — which the array-slot reading also
fits, because the two rotate-heavy functions happen to be the two carrying arrays. The two
hypotheses are not separated by anything measured so far. The experiment that separates them
is cheap and has not been run: **teach `gen.l` the rotate, re-fill the table.** If chacha and
sha256 fall toward 2× the pair was reading rotates all along.

## the same floors compiled STRAIGHT (2026-08-22, same box)

`bench/ccnif.sh` (`make -C bench ccnif`) builds host/hash.c, host/deflate.c and
host/inflate.c with mooncc, gcc and clang and reads them three ways: the answers, the .text,
and the wall clock. It is not a gate and is not wired into one — it is the instrument to
re-run while working on gen.l.

Where the rows above time these nifs through the SHIPPED BINARY, this times the code and
nothing else: the harnesses include the .c (every entry point in the three files is a
static, so no seam had to be cut into host/ to reach one), stub the six runtime symbols the
love-facing wrappers name, and never enter the lvm. No love runtime, no libc in the loop.
A run is ~20 s against ccbench's minutes, which is what makes it the vehicle for the
ablation the section above leaves open.

| ms, 64 passes, median of 5 | mooncc | gcc -O2 | clang -O2 | mooncc/gcc | mooncc/clang |
|---|---:|---:|---:|---:|---:|
| sha256 | 958 | 215 | 243 | **4.46×** | 3.94× |
| md5 | 256 | 127 | 139 | **2.02×** | 1.84× |
| crc32 | 44 | 31 | 35 | 1.42× | 1.26× |
| cksum | 45 | 33 | 35 | 1.36× | 1.29× |
| deflate | 562 | 366 | 381 | 1.54× | 1.48× |
| inflate | 59 | 37 | 33 | 1.59× | 1.79× |

**It reproduces the shipped-binary reading through a different harness**, which is the first
thing to check of a new instrument: sha256 4.46× against 4.23×, inflate 1.79× against 1.82×,
crc32 1.42× against 1.43×. Nothing in the love runtime was making those rows.

And .text, whole-file and exact (mooncc / gcc -O2 / clang -O2 bytes):

| | mooncc | gcc -O2 | clang -O2 |
|---|---:|---:|---:|
| host/hash.c | 10,832 | 9,636 | 8,187 |
| host/deflate.c | 10,675 | 9,749 | 15,259 |
| host/inflate.c | 11,763 | 6,821 | 10,281 |

⚠ **only the whole-file number is a sound total.** gcc and clang inline statics out of
existence — hash.c is 45 functions under mooncc and 34 under gcc, deflate.c 18 against 9 —
so summing the names two lanes share charges mooncc for a callee its opposite number already
paid for inside a caller. The script prints per-function ratios instead, worst first, and
`sha_block` is the widest cell in the whole table: **1.90× gcc's bytes and 4.30× clang's**,
beside a 4.46×/3.94× clock. The static and dynamic readings name the same function.

### ⚠ md5 is the control the rotate question wanted, and it is cheap to take further

The section above ends on an unrun experiment — teach `gen.l` the rotate, re-fill the table,
and see whether sha256 falls toward 2×. This run adds a datum that bears on it directly.

**md5 rotates as hard as sha-256 does and sits at 2.02×, not 4.46×.** `md5_block` runs 64
rotates a block against `sha_block`'s 576, but the difference that matters is that md5's
amount is a runtime load (`MS[i]` off a table) where sha-256's are literals. Counted over
the object: mooncc emits **0** rotate instructions in host/hash.c, gcc 8 — and only 2 of
gcc's are in `md5_block`, because gcc cannot use a rotate-immediate there either.

So the row where the oracle also gives up the idiom is the row where mooncc's gap collapses
by more than half. That fits "the rotate is the lever" and not "the array is", since md5
carries `m[16]` across its loop exactly as sha-256 carries `w[64]`. ⚠ it is not proof: md5
and sha-256 differ in work per block and in schedule as well as in rotates, and nothing here
counts instructions. The honest reading is that md5 is now the cheapest place to separate
the two hypotheses, and that `ccnif.sh` re-fills the whole table in twenty seconds once
`gen.l` learns the idiom.

## the rotate lands (2026-08-22, the same box) — and the pair was reading BOTH signals

`gen.l` recognizes the idiom now — `(x >> n) | (x << (W - n))` in either order, constant
or variable count, W 32 or 64, the operand unsigned and pure — and emits one rotate where
it emitted up to sixteen instructions: `ror4`/`rorv`/`rorv4` joined holo's neutral IR
(x64 + arm64 carry them; riscv and the thumbs keep their shifts, and the recognizer stays
off there). A spliced variable count rides the register form, a rotl count negated; the
macro spelling `(32 - (16))` folds through `cfold`, so chacha's four ROTL constants land
as rotate-immediates. Pinned by law (the emission per shape, and the near-misses that must
stay shifts), by `test/cc/152-rotate.c` across the gcc differential and both cross
targets, and the fixpoint holds with 45 rotates in love1's own body.

The re-fill, one quiet fill of each instrument (ccbench, then ccnif):

| | mooncc | gcc-musl | clang-musl | mooncc/clang | was (08-17) |
|---|---|---|---|---|---|
| build | 19,292.8 ms | 10,018.8 | 6,104.3 | 3.16× | 3.35× |
| corpus | 3,035.6 ms | 2,439.8 | 2,374.4 | **1.28×** | 1.30× |
| chacha20 | 690.2 ms | 295.5 | 192.5 | **3.59×** | 5.80× |
| poly1305 | 1,154.3 ms | 1,290.9 | 771.2 | 1.50× (**0.89× vs gcc**) | 1.55× |
| inflate | 586.5 ms | 372.8 | 290.2 | 2.02× | 1.82× |
| crc32 | 573.9 ms | 402.5 | 446.3 | 1.29× (1.43× vs gcc) | 1.43× |
| sha256 | 974.3 ms | 320.3 | 335.9 | **2.90×** | 4.23× |

and the straight floors: sha256 4.46× → **3.11×** gcc, md5 2.02× → 1.80×, crc32/cksum/
deflate/inflate unmoved.

**The answer: both hypotheses were true, each owning a row.** The rotate-heavy rows fell
hard (sha256 4.23× → 2.90×, chacha 5.80× → 3.59×) and every rotate-free row sat inside
the noise floor (poly 1.55 → 1.50, crc32-vs-gcc 1.43 → 1.43 exactly, corpus 1.30 → 1.28).
Neither fell TO 2×, so the pair was never reading one signal: the rotate took ~40-45% of
the excess on both rotate rows, and what remains of chacha is the array-slot reading,
intact — its keeps ride cs seats, exactly as the ablation said.

⚠ **the second defect stands and is now the named next lever**: an inlined body still does
not constant-propagate, so all of hash.c's rotates ride `%cl` off a frame-loaded count
(love1: 13 register-count rotates from the splices, 32 immediates from the macro
spellings) where gcc folds `rr(e, 6)` to a rotate-immediate. The count's frame round-trip
is most of what separates sha256's 2.90× from md5's control at 1.58×.

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

### ⚠ the splice JIT is not the lever, and the census says why

⚠ **the code is gone** — `lib/splice.l`, `mooncc -fir`, and `dis`/`disg` were cut once this
census stood. What follows is the measurement that decided it, kept because the idea is worth
revisiting and the ceiling below is what anyone reviving it has to answer.

With love rebuilt `make moon_fir=-fir` so the binary carries its IR record, `(use 'splice)`
ahead of the mooncc cat, and codegen re-run: **one closure** native-backed over the whole
compile, 11,708 → 11,248 ms, which is noise. `LOVE_SPLICE_CENSUS=1` says what happened —
12,427 closures reached the door during one `cgen-obj` of `core/love.c`:

| class | closures | | what would unblock it |
|---|---:|---|---|
| **call** | **11,821** | **95.1%** | nothing — see below |
| branch-plus | 298 | 2.4% | the Ip fold, and then something else |
| other | 214 | 1.7% | — |
| none | 62 | 0.5% | nothing blocks it |
| branch-only | 32 | **0.26%** | the Ip fold, on its own |

and the ranked blockers are the apply family, in order: `lvm_qap` 6,699, `lvm_tap` 6,356,
`lvm_argap` 5,993, `lvm_tapn` 5,426, `lvm_ap` 5,238, `lvm_apn` 4,029, `lvm_quoteap` 3,246 —
interleaved with the *names* being applied (`+` 2,813, `=` 1,932, `><` 1,548, `peep` 1,224,
`pin` 905).

**This is by construction, not by omission.** The splicer deletes the dispatch *between* the
ops of one bytecode thread, pasting each op's machine form out of `.rodata`. An apply leaves
the thread, and there is no machine form for "enter an arbitrary closure", so `lib/splice.l`
says it plainly: *a closure containing a CALL is one no amount of branch or operand work
reaches.* gen.l is a code generator — it is calls almost all the way down.

So the arc's next rung, the Ip fold that dissolves the branch family, converts **32 of 12,427
closures here**. Whatever gen.l's 11.5 s is going to be paid down by, it is not this lane, and
the census is the argument — not a guess about what gen.l looks like.

⚠ **62 closures were blocked by nothing and 1 was installed.** Worth a look before anyone
reads the `none` row as headroom: the blocker walk (`jit-blockers`, over `disg`) and the door
(`jit-1`, over `dis`) are not the same pass, and the other 61 died somewhere after.

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

- For the nif floors alone, `make -C bench ccnif` — the algorithms compiled straight by
  all three compilers, with no love runtime between the timer and the code, and the answers
  diffed across the lanes on the way past (a divergence there is a miscompile, and it is the
  only thing in that script that says a compiler is wrong). Twenty seconds, so it is the one
  to re-run per gen.l edit; ccbench is the one that includes the runtime.
- Both cipher rows, never one, and the corpus beside them. `make -C bench ccbench`
  refreshes `host` and `dist-seed` first: the crew rides love's own layered image now, so
  there is no sibling image to skew, and the mooncc lane races the ARTIFACT — a stale bake
  reads as a slow egg boot, never a wrong compiler.
- ⚠ `net` is a sum over every phase, so it moved when the three nif rows landed and results
  either side of that do not compare on it. Per-row ratios do.
- Cycles, not instructions, for anything claiming a speed effect. Instructions are near
  deterministic here (±0.01%) and make a tempting proxy; §above is why they mislead.
- An ablation is priced through `test_fixpoint`, so a configuration that cannot rebuild
  itself never reaches the timer. `sh tools/moon-ablate.sh [samples] [conf ..]` is the
  whole procedure as a script (the pare plan's rung 0): each configuration recompiles all
  of love under `MOON_ABLATE`, closes the fixpoint, and reads cycles + instructions +
  .text against the base row. It reproduced the hand census (C +1.9%/+6.9%, B
  +14.3%/+5.3% insns/cycles) on 2026-08-22's corpus.
- ⚠ **an ablation is part of the compiler's identity, and the runtime cache said so the
  hard way**: the nolibc archive under `~/.love/cache/moon` is keyed on compiler + image
  + source, and an env-blind key served BASE-compiled `__ai_*` members into an ablated
  love1 while love2 compiled its own fresh — a "broken" fixpoint whose only defect was
  the cache. `mcid` carries `MOON_ABLATE` in the key now (unset stays out, so standing
  keys survive). Any future config knob must join it the same way.
- ⚠ the `crew/moon/law.l` goldens pin register identities and residency counts (80 fail
  under A, 106 under B). A lane change churns them. That is not breakage — `test_cts` and
  the fixpoint are the behavioural instruments, and both held in all four ablations.

## the census (2026-08-23) — every mechanism priced alone, and the vmap reads negative

`tools/moon-ablate.sh 3` over every knob, same-run base, quiet box, 3 samples (floor
±0.7% cycles). The cost of ABLATING a mechanism is what the mechanism buys:

| ablated | cycles | insns | .text | lines owned (approx) |
|---|---|---|---|---|
| ralloc (A) | +4.0% | +12.5% | +4.5% | pool + mints ~89 |
| tpool (B) | +5.9% | +14.3% | +7.5% | (A's machinery + the pool half of the dance) |
| cs (C) | +5.0% | +1.9% | +2.2% | cskeep 85 + the grant lanes |
| tpool,cs (D) | +12.0% | +22.1% | +11.9% | — |
| **lhome** | **+6.3%** | +2.9% | +3.0% | lpick ~120 + alive's share of 245 |
| **vuniv** | **−2.9%** | −0.7% | ±0 | vm* ~127 + auniv 48 + channels |
| csbor | +0.4% | −0.2% | −0.7% | blscan + the borrow half of `att` |
| homes | +1.0% | +1.4% | +1.5% | pricing helpers ~107 |
| pcs | −0.4% | −0.1% | ±0 | ~57 |

(The whole residency layer owns ~1,450–1,550 of gen.l's 8,606 lines, `call-fixed`'s 328
serving the wrap side beside it. Overlaps are not additive — lhome alone reads above
tpool because the knobs are not nested partitions.)

**The payers are locals homes, the cs grant, and the operand pool.** `pcs` confirms the
ledger's zero. `csbor` prices at the floor. And **the vmap universe prices NEGATIVE** —
ablating it takes 2.9% of corpus cycles off — so the shape rows were read before believing
it: ccnif is unmoved to the millisecond (sha256 231→232 ms — the schedule array never
rode the universe), and a full ccbench fill under `MOON_ABLATE=vuniv` moves chacha
+3.6% — inside the ±4% wall floor — while the corpus row gains 5.2%. The row the
mechanism was BUILT for no longer misses it; its wins ride the cs-seat keeps and the
rotate now. The 2026-08-10 array-slot claim was true then and is falsified today —
which is the census doing the one job the criterion gives a number.

⚠ what this does NOT say: nothing here prices the MCU targets (as ever), and the
loop-keep family (`lo*`) serves both the vmap keeps and the cs borrows — a deletion of
the vmap complex has to re-price `cs` after it, since the keeps' zero may be hiding
inside cs's +5.0%. That sequencing is the pare plan's rung 3.

## the vmap cut (2026-08-23) — rung 3's first stroke, and what the gate caught

The complex is out of gen.l: the pins and their 29 flush sites, the array leg (`auniv`,
the element keys), the loop keeps (`lokeep`/`lochk`/`lomig`/`loseed`/`lomt`), the forward
joins and the staging seal — 8,606 → 7,927 lines, plus four law blocks that pinned the
mechanism (−198 in law.l; five neighboring blocks re-pinned to the new emission). The
knob's seams stay for the surviving mechanisms.

The gate: the fixpoint closes, and the cut's base prices as the census's ablation —
corpus instructions match to five digits (32.545 G both), cycles 11.435 G → **11.200 G**,
the −2% the census promised, banked. Byte-identity against the ablated reference was the
first gate and it FAILED at one seam, which is the finding: the knob had left `two? au`
(the array universe, still computed) licensing REGENS whose seat table withheld unhomed
params' home registers — a pointless rebuild whose only trace was a rotated pool pick in
the memcpy-idiom functions. The cut removes the license too, and the hot bodies were held
to the stronger instrument instead: `host_chacha20` and `host_poly1305` are
**instruction-identical** between the reference and the cut.

⚠ **cross-fill wall clocks lied here twice, and the identity check is the instrument.**
chacha read +12% against a fill from the previous day — with byte-equal machine code.
crc32, a control this cut cannot touch, moved +4.5% the same way. A day of thermal drift
is bigger than the ±4% floor; when a cut's rows move, diff the FUNCTIONS before
believing the clock.

The re-price after the cut (the sequencing law): `cs` holds at **+6.0%** cycles — the
keeps' zero was not hiding inside it — and **`csbor` now prices at −0.2% with
base-identical .text**: the borrow license (blscan, `wb`, the `att` grant dance) is dead
weight and its cut is licensed. `pcs` stays on the roster behind it.

⚠ a harness run leaves out/host ABLATED (its last configuration's objects); the seed then
reads FIXPOINT NOT OK against the mixed artifact. `moon-ablate.sh` clears the moon
objects on exit now so the next make rebuilds clean.

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
