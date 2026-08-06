# the two libcs

The tree carries two C libraries of its own. They are not two versions of one thing: one is what
a program needs when there is no OS; the other is the OS, wrapped.

**`libc/` — the freestanding floor.** Two `.c` files, ~45 lines: `mem.c` and `str.c`. ⚠ **FOUR
consumers, and only one goes through `common.mk`.** `c_c = $(wildcard $R/libc/*.c)` serves
`port/inle/kernel.mk` alone; `port/virt`, `port/mps2` and `port/teensy41` each name the files in
their own object lists, two of them through a `%.o: $(R)/libc/%.c` pattern rule that silently
builds only what is listed. So a grep for `c_c` finds a QUARTER of the truth — grep for `libc/`
across every Makefile, and for the basenames too. (`limine.h` sits in the same folder but is a
vendored bootloader header, not ours.)

**`crew/moon/lib/nolibc.c` — a hosted Linux libc.** ~1950 lines over one arch-gated syscall leaf
(`__ai_sys`, laid by `crew/moon/lib/mksys.l`) and the `sc0`..`sc6` wrappers: stdio over a flush
buffer, K&R malloc over mmap arenas, sockets and a resolver, signals folded onto the kernel's
32-byte sigaction, termios, dirent over getdents64, the calendar, `qsort`, the printf family, the
`-pie` self-relocator, and `__ai_start`. It is the userland floor under `CC=mooncc`,
`make test_raw`, the distro binaries and `love up`.

**The OS-dependent half is already the kernel's own.** `port/inle/kmain.c` defines
`malloc`/`free` over `kmallocw`/`kfree`. The kernel never wanted nolibc's allocator, and `libc/`
never offered one.

**The kernel's own C calls almost no libc at all.** Across `kmain.c`, the per-arch
`arch.c`/`pvh.c`/`dtb.c`, `crew/quay/*.c` and the UEFI loader, the whole demand is two `malloc`
calls. Every other reference comes from `love.c` or is SYNTHESIZED by the compiler from a loop
idiom. So the floor exists for the runtime and the code generator, not for the kernel's code.

## the overlap

Five functions — `memcmp` `memcpy` `memmove` `memset` `strlen` — plus `memchr` kept in
`libc/mem.c` with no caller because a compiler may synthesize one. Every one of them is a byte
loop. (The authoritative check is `nm -u` over the built kernel objects, not a guess.)

`love.c`'s reader reads all three integer bases itself
(`ai_big_read_dec`/`_hex`/`_oct`) and takes floats straight from `am_strtod`, which is ours and
lives in `crew/moon/lib/math/am.c` — already the shared kind of file the merge below is about.
That is what removed the freestanding `strtol`, and with it `strtod`, `memchr`, `isspace`,
`tolower`, `toupper`.

**The rule that falls out of the drift these two once carried** (a wrapping `strtol` on one side
against a saturating one on the other, so one source text read as two different numbers): **a
duplicated function is fine while it is a loop, and a liability the moment it has a rule.** Five
byte loops cannot drift in a way a gate would miss — change `memcpy` on either side and
everything fails at once, loudly.

## the differential gate — `make test_libc`

`test/gate/libc.sh` over `test/libc/*.c`, in `test_slow`. One program per family — `mem` `str`
`ctype` `num` `flo` `fmt` `sort` — each built twice, by mooncc (whose implicit link pulls
`nolibc.c`) and by gcc against glibc, run, and the two **outputs** compared.

⚠ **Output, not the exit code the mooncc battery compares.** An exit code is eight bits and says
only THAT something drifted; a diff names the function and the case. That is why this is its own
gate rather than more programs in `test/cc`, which stays all-freestanding.

Three design points make it work:

- **Report what the standard FIXES, not what a library chose.** Comparisons go through `say_c`
  (the sign only — glibc hands back the byte difference, ours -1/0/1, both right); pointers
  through `say_p` (an offset, since addresses differ between builds); a predicate's truth through
  `!!` (glibc's `isalnum` returns the mask bit `8`, ours returns `1`). Each of these produces a
  false failure unnormalized.
- **The reporting side must not use the library under test.** `test/libc/say.h` turns its own
  digits by hand and prints through `putchar`, so a drifted `%d` shows up as `fmt.c`'s payload
  instead of corrupting all seven frames at once.
- **No undefined behaviour in the cases.** An invalid `strtol` base leaves `endptr` untouched in
  BOTH libraries, so a differential on it compares two uninitialized pointers and fails at
  random.

**The headers must not promise what the library cannot deliver.** Every function declared in
`stdio.h`/`stdlib.h`/`string.h`/`ctype.h` is referenced by a generated program; a name with no
body fails the LINK. The list is generated from the headers rather than written down, so it
cannot rot.

⚠ **`(show x)` is not a value test.** love's printer drops a zero's sign, so a `-0.0` fault is
invisible through it. A differential must not look through a normalizing printer.

⚠ **A diagnostic that describes the compiler's internals instead of the program's fault will be
believed, and will send someone hunting.** `gen.l` carries an `'undecl` pin on both the function
and the data path so an undeclared identifier in a static initializer says
`cc: undeclared 'x' in the initializer of 'y'` rather than dumping IR; `test/gate/moon.sh` gates
both directions — the refusal must name it, and a function's address must still image.

⚠ **The tree has no other differential against a second implementation of anything it writes in
C.** The mooncc battery compares CODEGEN against gcc; this is the only thing comparing the
LIBRARY against a libc.

## the merge, unbuilt

The correctness case is spent — what is left is tidiness. Worth doing when something else is
already open in `moon.l`'s member list or in `kernel.mk`. What would revive it is the
freestanding side growing a function **with a policy in it**: a `snprintf` for kernel
diagnostics, a real string surface, a `strtol` coming back for a command line.

### 1. the shared floor becomes one file

**Put it at `crew/moon/lib/pure.c`, not under `libc/`.** The constraint that decides this: mooncc
must find its runtime sources from its *installed seat* (`mhome`, moon.l — it walks `crew/moon/`
then `<exe>/../lib/love/moon/`, and `mk/install.mk` ships `moon_srcs` there), while the kernel
build has the whole tree and can name any path. So the awkward consumer keeps the short path.

**mooncc's side is nearly free.** `moon.l`'s `mems` is already a by-need MULTI-MEMBER list —
`lib/math/*.c` is globbed into it, each member carrying its defined/undefined symbols via
`dusyms`, and the `go` loop pulls only what the link references. Adding a member is a list entry;
globbing `lib/*.c` the way math is globbed is better still.

| file | change |
|---|---|
| `crew/moon/lib/pure.c` | new — the five, moved out of nolibc.c |
| `crew/moon/moon.l` | glob `lib/*.c` into `mems` instead of naming nolibc |
| `port/inle/kernel.mk` | name `crew/moon/lib/pure.c` beside `$(c_c)` |
| `port/virt`, `port/mps2`, `port/teensy41` | their own object lists, by hand — see the ⚠ above |
| `host/build.mk`, `crew/build.mk` | add `pure.o` to the object lists |
| `mk/install.mk` | `moon_srcs` picks it up (glob, or name it) |

⚠ `moon.l`'s home probe reads `lib/nolibc.c` to decide it has found the runtime. Leave nolibc.c
in place and that probe is untouched; if the file ever moves, the probe moves with it.

⚠ `__ai_start` must stay in `nolibc.c`, not in the shared file: `moon.l` defines a WEAK one in
crt0 that nolibc overrides strong, and that weak/strong pair is the whole switch between a bare
link and a hosted one. A freestanding link must NOT drag the strong one in.

### 2. `libc/` empties

Delete `libc/mem.c` and `libc/str.c`; `libc/` is then `limine.h` alone, which is a vendored
header and belongs beside the bootloader glue in `port/inle/`. `common.mk`'s `c_c` retires with
it.

### 3. the errno seam — ONLY if something with an error path joins

The one real design question, and today it does not arise. nolibc reports through `__errno_v`;
freestanding has no errno and never wanted one. The five shared functions cannot fail, so the
seam is unneeded. If a future member CAN fail — a `strtol` coming back, a `snprintf` — resolve it
before writing the member, not after: either a weak `__errno_location` the freestanding side
leaves unimplemented, or the shared body returns the error and each side reports it. **Do not let
a shared file reference `errno` unconditionally.**

## what NOT to do

- **Do not link `nolibc.c` whole into the kernel.** It is ONE translation unit, so pulling
  `strlen` pulls the mmap allocator, `__ai_start`, the signal restorer and every syscall site,
  which in ring 0 fault or go nowhere. It also opens with twenty system headers, and the kernel
  builds `-nostdinc`.
- **Do not tie this to the mooncc kernel migration.** Same translation units either way —
  `libc/*.c` stays in `k_shared_c` on the far side. That migration is about the last C *compiler*
  island, not the libc.
- **Do not move `malloc`/`free`.** The kernel's allocator is the kernel's, by design.
- **Do not delete a `libc/` file on the strength of `make test`** — three of its four consumers
  are named by hand in board Makefiles, and the first gate to notice is `test_virt`, deep in
  `test_slow`.
