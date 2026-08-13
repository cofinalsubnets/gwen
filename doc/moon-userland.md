# moon-userland — growing mooncc by building real packages

Build the conventional GNU/Linux userland (the Linux-From-Scratch package set — bzip2, gzip,
sed, grep, make, bash, coreutils, …) **with mooncc**, in LFS build order, as a conventional
overlay over the love base system. Two payoffs at once:

- each real package drives mooncc through C it has never seen, growing the compiler the same
  way love.c grew it — refuse a construct, add it, move on;
- the result is a familiar POSIX userland whose *entire toolchain underneath* is
  love/mooncc/holo.

The base is the gcc-free, glibc-free static `love` (`make test_raw`) booting as pid 1 on the
Linux kernel via `init/boot.l` + `mk/distro.mk` (`make distro-run`, or `distro-smoke` for the
headless check). The overlay is these packages, laid beside kore under /usr.

⚠ **Skip the two-pass cross-toolchain ritual entirely** — mooncc/holo/nolibc already ARE the
self-hosting toolchain. Link initially with a foreign `ld` + the host libc if a package needs
it, then move it onto nolibc/holo as its libc surface fills in.

## what runs

Six packages build and run: **bzip2 1.0.8**, **gzip 1.2.4**, **tar 1.13**, **m4 1.4**,
**Lua 5.4.7** and **SQLite 3.45.3** (the amalgamation). All six now have a
`make moon-<pkg>[-arm64|-riscv]` target over `tools/moon-<pkg>.sh`, each skipping cleanly
without a source tree or without qemu.

⚠ **A dead gate says nothing, and four of these were dead.** `crew/moon/lib/nolibc.c` became
the directory `crew/moon/lib/nolibc/` when the libc was split into by-need members, and every
harness still compiled the old single file — so `make moon-tar` and its three siblings failed
at the first object rather than running. Nothing noticed, because these targets are opt-in and
no tier runs them. The link owes nolibc's symbols and the driver pulls the members itself, so
the fix is to name **no** nolibc object at all (host/build.mk says the same of `love`). The
lesson is the ordinary one about opt-in gates: **their silence is not a pass**, and the
interval in which one can rot is the interval since somebody last typed its name.

**And a gate that was not running was not catching anything either.** Repairing the four and
running them turned up a real miscompile that had been sitting behind the dead `moon-sqlite`:
`SELECT 4294967296*2` answered `8589934592.0` where every other sqlite in the world answers
the integer `8589934592`. It is fixed — see below — and all six rungs now pass.

Beside them, seven **applications** built 2026-08-08 — not LFS rungs, but the widest sweep of
ordinary third-party C the compiler has met, and the one that found the `deadst` miscompile:

| | |
|---|---|
| **dwm** 6.5, **st** 0.9.3 | build and RUN under Xvfb (dwm tiles a real xterm; st spawns a shell in its pty). X11 clients, so they link foreign — mooncc's holo speaks no `-l`/`-L` and emits no shared objects. |
| **darkhttpd** 1.17 | builds and serves — a 100KB file byte-identical, `Range` exact, 404s, traversal refused. |
| **xlander** (1992) | the real `xlander_19920427.orig` tarball: K&R C from before x86-64 existed, builds and draws under Xvfb (the wireframe lander, the terrain, the instrument panel). ⚠ **its first sweep was run against a tree that held modernized pdxlander sources**, so the row said "builds and draws" a year before it did — an outside package proves nothing until you have looked at the C you fed it. What the real one needed was the block-scope function declaration (git log 2026-08-08): it writes fourteen in the K&R spelling, `void f(), g();`, and any ONE of the four files carrying them was enough to segfault the program. ⚠ it also wants 4.3BSD's `sigmask`/`sigblock`/`sigsetmask`, which glibc still declares and nolibc does not — and our headers win for `<>`, so a `-I` shim supplies them. |
| **PDCLib** | **233/233 files** in its threadless configuration — the whole library compiles on x64, dlmalloc and all — and its own per-function test drivers run with **zero mooncc-only failures** against a gcc-built control. The last refusal fell 2026-08-09 to sizeof-as-size_t (`d19418a9`): `_PDCLIB_internal.h:667` guards the library with `1 / (!!(sizeof(sizeof(int)) == sizeof(size_t)))`, and 4 ≠ 8 refused the divide under every file that reached it; the ladder beneath it landed the same day — `__extension__` (which is what opens `<pthread.h>`), the `__builtin_bswap` trio, the `__sync` spin-lock pair, `__builtin_clz`/`ctz`. With threads on it is 257/258, and the one holdout names a real gap rather than a nit: `functions/_PDCLIB/errno.c`'s `_Thread_local int _PDCLIB_errno`, C11 thread-local storage. arm64/riscv64 dlmalloc still stops at the 80-byte struct return (doc/moon-c-gaps). Its `%f` is what found the duplicated-lvalue bug (doc/moon.md, `calm?`) — and, first, the trap that the control has to be the same library built by gcc (doc/moon-c-gaps). ⚠ its 117 `-D`s must be passed as **quoted** argv entries — 35 of them contain a space (`-D__SIZE_TYPE__=long unsigned int`), and a shell that word-splits them hands the compiler a truncated typedef and a stray `int`. ⚠ it is a cmake package, so three files arrive as templates — `_PDCLIB_config.h` carries a `#cmakedefine`, and the platform errno/fcntl headers `#include "@ERRNO_PATH@/errno.h"` — configure it or resolve those by hand, else the refusal you read is the harness's. |
| **limine** | the host tool builds and runs; the BIOS/UEFI stages are unreachable *by its own configure*, which demands a whole `x86_64-elf` binutils (`ar`/`objcopy`/`ld`) rather than a compiler. |
| **pdxlander** | the simulator half builds and runs in the Playdate simulator; the device half stops at the SDK's `LCDMakeRect` (doc/moon-c-gaps: the t32 16B composite return). |
| **Scheme 48** 1.9.3 | configures, ~20 files compile, stops at `pthread_sigmask`. |

⚠ **Nothing here is wired into a gate.** They are an afternoon's sweep with a scratch harness,
not rungs with oracles — say so before quoting them as coverage. What they left behind is
doc/moon-c-gaps.md's "accepted, and WRONG" section, which is the part worth keeping.

Notes worth not re-deriving:

- **bzip2** is the ideal first package: in the LFS book, ~7.3k lines of plain C89, and **no
  `./configure`**, so it isolates mooncc's C coverage from the shell/autotools bootstrap. Its
  compressed bytes come out byte-identical to a `gcc -O0` build, and it interoperates both ways
  with the system tool.
- **Lua** needs no configure and was the first package where every source file compiled
  unpatched.
- **SQLite** is the widest net the tree has: 256k lines from ONE machine-generated file, so it
  reaches C shapes nobody writes by hand — deep switch ladders, computed unions, 64-bit mixing,
  a whole float formatter of its own. Config is `THREADSAFE=0` + no load-extension, both
  first-class sqlite configurations rather than patches (nolibc carries no pthreads and no
  dlopen).
- **m4** runs its OWN 57-check suite on the cross targets, not a reduced version. `config.h` is
  reused as configure wrote it for the host, which is sound because both targets are
  little-endian LP64 and the two answers that actually differ (`HAVE_EFGCVT`, `USE_STACKOVF`)
  are ones the harness already corrects by hand.
- ⚠ **tar 1.13 needs two things of `./configure` that are about 1999, not about us**: its
  `config.guess`/`config.sub` predate x86-64 (copy the system automake's over), and modern gcc
  makes the implicit-int `main(){return(0);}` of its probes a hard error
  (`CC="gcc -std=gnu89"`). mooncc compiles every actual source either way.
- **gzip 1.2.4** wants one app-side edit and it is a real 64-bit portability bug in *gzip*,
  not a mooncc gap: `gzip.c` calls `ctime` with no declaration in scope, and on x86-64 the
  implicit `int` return truncates the returned pointer. The harness prepends `<time.h>` to a
  copy, leaving the imported tree pristine.

### the three bugs the packages found

All three were silent, all three were found by *running* the program rather than compiling
it, and none was reachable from a single-file test — which is the argument for package rungs
stated once more.

- **gzip: a tentative definition did not complete an earlier `extern T x[];`** (`crew/moon/gen.l`).
  C's composite-type rule (6.2.7) says an array of unknown size and one with a size compose to
  the sized type; gen's tentative rule (6.9.2) kept whichever entry already stood unless the
  newcomer carried an initializer. So `extern char a[];` followed by `char a[1024];` left `a`
  laid in `.bss` **at size zero**, and the next global took the same address. `gzip.h` declares
  `extern char ifname[], ofname[];` and `gzip.c` defines both, so gzip's input and output
  filename buffers were one buffer. It built, linked, ran, printed its version and its
  compilation options, and then opened its *output* name for reading — `open("stdout")` under
  `-c`, `open("V.gz")` otherwise. Five lines reproduce it; `test/cc/134-tentative.c` pins it.
- **bzip2: `fread` ignored the `ungetc` pushback** (`crew/moon/lib/nolibc/`). C says the next
  input of any kind sees an ungetc'd byte. bzip2's `myfeof` is the portable EOF probe —
  `fgetc`, then `ungetc` if that was not EOF — so with `fread` reading *past* the pushback,
  every probe re-served the same stale byte and EOF never arrived: **compressing any non-empty
  file spun forever**, and the first byte of the file was quietly missing besides. `fgets` had
  the same gap and now runs over `getc`. ⚠ the diagnosis came straight from `strace`:
  `read(3,"h",1)` then `read(3,"ello hello\n",5000)=11` — eleven bytes where twelve were
  asked for names the dropped byte and the bad EOF in one line.
- **sqlite: the usual arithmetic conversions read as "either operand is unsigned"**
  (`crew/moon/gen.l`, two places). C 6.3.1.8 is a **rank** rule: where the signed operand's
  rank is strictly greater and its type represents every value of the unsigned one, the
  common type stays **signed** — on LP64 that is `long long` meeting `unsigned int`. `puac`
  in parse.l already spells this correctly, and both offenders were code that did not ask it:
  `ubin` typed every mixed binary result unsigned, and the constant strength-reduction lane
  fired on "either operand unsigned" plus a power-of-two divisor, turning a **signed** divide
  into a **logical** shift. sqlite's `LARGEST_INT64` is `0xffffffff|((i64)0x7fffffff<<32)`,
  so that unsigned-int literal made the whole 64-bit constant unsigned; `sqlite3MulInt64`
  then read `INT64_MIN/2` as positive, every multiply looked like an overflow, and sqlite
  fell back to floating point. ⚠ **the power-of-two divisor is what made it nasty**: `/3u`
  was always right and `/2u` and `/4u` were wrong, so the shape that looks safest is the one
  that broke. Pinned by `test/cc/135-uac.c`.
  - ⚠ two traps in *writing* that test, both caught by running gcc first: the same rank rule
    means `~0u` widens to `+4294967295` rather than sign-extending (so `(i64)-8 & ~0u` is
    `4294967288`, not `-8`), and `return bad` on a bitmask of failures is taken **mod 256** —
    8192 would have exited 0. The status is now just nonzero; the printed line is the
    differential.

## the battery is a differential payload, not a smoke test

Each package's harness computes answers and compares them line by line, because **an exit code
is eight bits and cannot name the query that broke**. SQLite's, for instance, covers 64-bit
integer edges, REAL formatting through sqlite's own printf, string and GLOB/LIKE operators,
aggregates, `GROUP BY … HAVING`, a join, a correlated subquery, a window function, a recursive
CTE, `json_extract`, and then the file-backed lane: journaled transaction, index, close/reopen,
a rollback that must actually roll back, `PRAGMA integrity_check`.

That gives a complete oracle ladder, the same one `test/gate/ccarch.sh` stands on one level
down:

```
gcc + glibc  ──pins──▶  mooncc x86-64  ──pins──▶  mooncc aarch64 / riscv64
```

The x64 lane builds the SAME driver and the SAME sources with the system cc and compares byte
for byte; a cross lane compares against the x64 answers. **Two mooncc builds agreeing proves
less than it looks** — they can share a fault in the shared model — so the gcc leg is what makes
the cross comparison mean anything.

⚠ **A cross lane invites false reds that have nothing to do with the compiler.** m4's
`check-them` is a shell script that execs `m4` off PATH, and a cross binary is not executable
without a binfmt_misc registration, so the harness writes a one-line wrapper onto PATH — and it
must pass **`qemu -0 m4`**, because m4 prints its own `argv[0]` in every error message and two
checks compare stderr against a text that names it.

## why a THIRD target, not a second

**Targets route around faults differently.** Each new backend has paid on its first run:

- **arm64** caught a codegen collision no single-file test could reach. gen speaks `r4` for the
  frame base on every target, and `a4ize` retargets it to `fp` at the end of build **by
  position** — an `r4` in a memory op's BASE slot becomes `fp`, an `r4` anywhere else is left
  alone (it is the 5th argument). On AArch64 gp 4 arrives in x4, so the moment the ride analysis
  let a 5th parameter stay in its arrival register and the body used it as a pointer, its base
  slot read as the frame pointer: `*p`, `p[i]` and `p + i` all addressed the FRAME. `rideset`
  now bars a rider whose arrival register collides with the build-time frame-base spelling —
  the param homes to r14, `a4ize` renames the frame base, and `unhome` (which runs *after*
  `a4ize`) renames r14 back to r4 safely, so the param still rides its arrival register end to
  end. x64 cannot reach this (r4 is rbp, never an argument register), thumb has only four
  argument registers, and riscv has `nhome = 0` so nothing rides. **The shape that reaches it —
  a six-parameter function whose 5th is a pointer — is not one anybody writes into a compiler
  test deliberately.** Pinned by `test/cc/110-param5.c`.
- **riscv64** is the more distant of the two (`nhome = 0`, so the whole ride analysis is
  bypassed) and found two faults arm64 had been silent about:
  - **`O_DIRECTORY` and `O_NOFOLLOW` were wrong on riscv64.** The gate read
    `#if defined(__aarch64__) || defined(__riscv)` and called 040000 "asm-generic" — it is
    **arm's**. arm64 keeps 32-bit ARM's values for these two; riscv64 takes the genuine
    asm-generic ones, which are x86-64's.

    | | O_DIRECTORY | O_NOFOLLOW |
    |---|---|---|
    | arm, arm64 | 040000 | 0100000 |
    | x86-64, riscv64, asm-generic | 0200000 | 0400000 |

    So on riscv64 `O_DIRECTORY` meant **`O_DIRECT`** and `O_NOFOLLOW` meant **`O_LARGEFILE`**:
    `opendir` answered EINVAL and tar could not read a directory at all. The gate is on the ARM
    family now (`__aarch64__ || __arm__`), which is what the difference actually is.
  - **A direct tail call could not reach past ±1MB on riscv64.** A tail call is the one `jmp`
    that leaves its function, and riscv's `jal` spans only ±1MB (`call` never had the problem,
    being already an auipc pair). It took 1.6MB of SQLite in one object to reach the limit, and
    the backend refused loudly (`;; rv-jal-range`) rather than emitting a wrong branch, which is
    the behaviour you want from a range check. The fix borrows from the lane directly below it:
    the *indirect* tail call already parks its target in t4 and uses `jmpr`, which has no range,
    so the direct one loads the address with `la` into that same park **after** the restores.

⚠ **A shared model's fault hides behind whichever lane the other target lacks**, and a 30k-line
package is a much wider net than a test suite someone wrote on purpose. That is the argument for
package-scale differentials over more single-file tests. See doc/mooncc-differentials.

## the source cache

Every package rung is opt-in on an imported source tree. All the harnesses look in the same
places before giving up:

```
$LUASRC / $SQLSRC / $M4SRC / $TARSRC / $GZIPSRC / $BZIP2SRC   (explicit, always wins)
dl/<glob>                              (tree-local; `make distclean` takes it)
$MOONSRC/<glob>                        (the cache — ~/src when MOONSRC is unset)
```

So `make moon-sqlite-arm64` works bare. The globs are versioned (`lua-5.4.*`,
`sqlite-amalgamation-*`, `m4-1.4*`, `tar-1.13*`), so a version bump does not break the search,
and **a missing tree is still a clean SKIP, never a failure** — these stay opt-in, and a green
`make test_slow` says nothing about them. The exact `curl` line for each lives in the header of
its `tools/moon-*.sh`.

⚠ m4 and tar want a `./configure`'d tree (they read `config.h`); Lua, bzip2 and the SQLite
amalgamation want only an extracted one. gzip's configure writes three `-D`s the harness
passes by hand, so an unconfigured tree builds too — the file is kept as the witness that the
tree was prepared, not because it is load-bearing.

## and then we stopped needing them

`lib/gz.l` and `lib/tar.l` are gzip and tar **in love**: the RFC 1952 container with crc32,
DEFLATE as a coder and a decoder, and the ustar archive read and written. `tools/tgz.l` is the
`c`/`x`/`t` door over both, and `make dist-tgz` cuts a release tarball with neither `tar` nor
`gzip` on the box.

Reading gzip's C first is what made that a short job rather than a long one — `deflate.c` and
`trees.c` are the canonical statement of the format, and having a **mooncc-built gzip sitting
right there** meant every stage had an oracle a directory away.

The division of proof is deliberate. `test/host/gz.l` holds what needs nothing outside the
tree; `test/gate/targz.sh` (`make test_gz`) holds what only GNU tar and GNU gzip can say.
⚠ **A round trip through our own pair proves nothing about the format** — a coder and a
decoder written by one hand invert each other happily over a format nobody else speaks, so
the system tools are not a nicety there, they are the entire oracle.

⚠ our coder emits the **fixed** Huffman code only. On this tree's sources that is a shade
behind `gzip -1` and about 24% behind `gzip -9` (137965 → 49241, against 39713 and 47876).
The decoder is complete — stored, fixed and dynamic — because the far side is not ours to
choose. A dynamic coder is the next real win and roughly all of the gap.

## the gnulib layer

`tools/moon-sweep.sh` measures a package's gnulib layer file by file. ⚠ **The denominator is the
point** — gnulib's `lib/` carries every platform's lane, and automake builds a fraction of it on
Linux, so score against `lib/Makefile`'s `*_a_OBJECTS`, never `lib/*.c`.

The work the sweep surfaces is mostly ordinary header completeness — a missing declaration at a
time, provided in `crew/moon/include/` rather than fallen through to `/usr/include`. **The
standing rule: provide the header cross-wise rather than let a partial one fall through**, since
a header we do not carry resolving to glibc's is how a freestanding build takes a hosted
declaration and looks green.

What makes that work expensive is diagnostics, not compilation — see doc/moon-diag.md, which the
sweep motivated and which the sweep is the before-picture for.

⚠ `#include_next` never fires today, because gnulib's `include_next` lives in `.in.h` templates
the Makefile materializes only when config.h says a replacement is needed — and a config.h
describing **glibc** makes gnulib stand aside. It becomes load-bearing the moment a config.h
describes nolibc honestly. doc/moon-diag.md carries the two paths.

## suggested order

bzip2 → gzip → less → m4 → make → sed/grep (gnulib-heavy, harder) → bash → coreutils.

Related: doc/moon.md (the compiler), doc/moon-kernel.md (which is no longer this ladder's far
end — the kernel is built by mooncc already), `init/boot.l` + `mk/distro.mk` (the base).
