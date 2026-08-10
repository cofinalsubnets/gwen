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

Six packages build and run: **bzip2 1.0.8**, **gzip**, **tar 1.13**, **m4 1.4**, **Lua 5.4.7**
and **SQLite 3.45.3** (the amalgamation). Four of them — lua, sqlite, m4, tar — also
**cross-build and run for aarch64 and riscv64**, each with a `make moon-<pkg>[-arm64|-riscv]`
target over `tools/moon-<pkg>.sh`, and each skipping cleanly without qemu.

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
$LUASRC / $SQLSRC / $M4SRC / $TARSRC   (explicit, always wins)
dl/<glob>                              (tree-local; `make distclean` takes it)
$MOONSRC/<glob>                        (the cache — ~/src when MOONSRC is unset)
```

So `make moon-sqlite-arm64` works bare. The globs are versioned (`lua-5.4.*`,
`sqlite-amalgamation-*`, `m4-1.4*`, `tar-1.13*`), so a version bump does not break the search,
and **a missing tree is still a clean SKIP, never a failure** — these stay opt-in, and a green
`make test_slow` says nothing about them. The exact `curl` line for each lives in the header of
its `tools/moon-*.sh`.

⚠ m4 and tar want a `./configure`'d tree (they read `config.h`); Lua and the SQLite
amalgamation want only an extracted one.

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
