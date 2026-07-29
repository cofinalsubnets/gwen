# the two libcs -- THE PLAN

the tree carries two C libraries of its own, `libc/` and
`crew/moon/lib/nolibc.c`, and they overlapped enough to DRIFT: a wrapping
`strtol` on one side against a saturating one on the other made a single
source text read as two different numbers, silently, in the one function the
reader leaned on (see doc/reader.md rung 2). this is the plan for collapsing
the overlap. drafted 2026-07-29.

⚠ **read the verdict at the bottom before starting.** the reader change that
found the drift also DELETED most of the overlap, so the correctness case for
this arc is largely spent. what remains is tidiness, and the ladder is written
so it can sit unbuilt without rotting.

## the inventory (measured, not assumed)

**`libc/` -- the freestanding floor.** two `.c` files, ~45 lines. ⚠ **FOUR
consumers, and only one of them goes through `common.mk`.** `c_c = $(wildcard
$R/libc/*.c)` (common.mk:57) serves `port/inle/kernel.mk:38` alone; `port/virt`,
`port/mps2` and `port/teensy41` each name the files in their own object lists,
two of them through a `%.o: $(R)/libc/%.c` pattern rule that silently builds only
what is listed. so a grep for `c_c` finds a QUARTER of the truth -- grep for
`libc/` across every Makefile, and for the basenames too. (deleting `ctype.c`
looked clean under `make test` and broke `test_virt` three gates into
`test_all`.) (`limine.h` sits in the same folder but is a vendored bootloader
header, not ours.)

**`crew/moon/lib/nolibc.c` -- a hosted Linux libc.** ~1700 lines, **224 raw
syscall sites**: stdio over a flush buffer, K&R malloc over mmap arenas,
sockets and a resolver, signals folded onto the kernel's 32-byte sigaction,
termios, dirent over getdents64, the calendar, `qsort`, the printf family, the
`-pie` self-relocator, and `__ai_start`. it is the userland floor under
`CC=mooncc`, `make test_raw`, the distro binaries and `love up`.

**they are not two versions of one thing.** one is what a program needs when
there is no OS; the other is the OS, wrapped.

**the OS-dependent half is ALREADY the kernel's own.** `port/inle/kmain.c:363`
defines `malloc`/`free` over `kmallocw`/`kfree`. the kernel never wanted
nolibc's allocator, and `libc/` never offered one.

**the kernel's own C calls almost no libc at all.** across `kmain.c`, the
per-arch `arch.c`/`pvh.c`/`dtb.c`, `crew/quay/*.c` and the UEFI loader, the
whole demand is two `malloc` calls. every other reference comes from `love.c`
or is SYNTHESIZED by the compiler from a loop idiom. so the floor exists for
the runtime and the code generator, not for the kernel's code.

## what the strtol change did to the question

`love.c`'s reader now reads all three integer bases itself
(`ai_big_read_dec`/`_hex`/`_oct`) and takes floats straight from `am_strtod`,
which is ours and lives in `crew/moon/lib/math/am.c` -- **already the shared
kind of file this arc is about.** that removed the last caller of the
freestanding `strtol`, and with it `strtod`, `memchr`, `isspace`, `tolower`,
`toupper`. `libc/ctype.c` is gone and `libc/str.c` is one function.

authoritative check, not a guess -- `nm -u` over the built kernel objects:

```
before   memcmp memcpy memmove memset strlen strtol   (+ malloc free, kmain's own)
after    memcmp memcpy memmove memset strlen
```

**so the overlap is now five functions**, plus `memchr` kept in `libc/mem.c`
with no caller because a compiler may synthesize one. every one of them is a
byte loop.

## the rungs

### 0. a differential gate for the shared floor -- SEEDED 2026-07-29

`test/cc/105-strtol.c`. the mooncc battery (`test/gate/moon.sh`) compiles each
`test/cc/*.c` with mooncc AND with gcc and demands the exit codes agree -- and
because mooncc's implicit link pulls `nolibc.c`, a program that CALLS libc turns
that harness into a differential over our libc against glibc. that is the shape
to grow: one program per function family, gcc as the second opinion.

⚠ it is deliberately the battery's only non-freestanding member; say so in any
sibling, or a failure reads as a codegen bug. proved to bite: reverting nolibc's
saturation drops the exit code 17 -> 10.

⚠ **this rung stands alone and is worth having even if the rest is never
built** -- it is the thing that would have caught the original drift.

### 1. the shared floor becomes one file

**put it at `crew/moon/lib/pure.c`, not under `libc/`.** the constraint that
decides this: mooncc must find its runtime sources from its *installed seat*
(`mhome`, moon.l:44 -- it walks `crew/moon/` then
`<exe>/../lib/love/moon/`, and `mk/install.mk:105` ships `moon_srcs` there),
while the kernel build has the whole tree and can name any path. so let the
awkward consumer keep the short path.

**mooncc's side is nearly free.** `moon.l:233`'s `mems` is already a by-need
MULTI-MEMBER list -- `lib/math/*.c` is globbed into it, each member carrying its
defined/undefined symbols via `dusyms`, and the `go` loop pulls only what the
link actually references. adding a member is a list entry; globbing `lib/*.c`
the way math is globbed is better still.

the touch points, all of them:

| file | change |
|---|---|
| `crew/moon/lib/pure.c` | new -- the five, moved out of nolibc.c |
| `crew/moon/moon.l:233` | glob `lib/*.c` into `mems` instead of naming nolibc |
| `port/inle/kernel.mk` | name `crew/moon/lib/pure.c` beside `$(c_c)` |
| `port/virt`, `port/mps2`, `port/teensy41` | their own object lists, by hand -- see the ⚠ above |
| `host/build.mk:218`, `crew/build.mk:167` | add `pure.o` to the object lists |
| `mk/install.mk:105` | `moon_srcs` picks it up (glob, or name it) |

⚠ `moon.l:49`'s home probe reads `lib/nolibc.c` to decide it has found the
runtime. leave nolibc.c in place and that probe is untouched; if the file ever
moves, the probe moves with it.

⚠ and `__ai_start` must stay in `nolibc.c`, not in the shared file: `moon.l:156`
defines a WEAK one in crt0 that nolibc overrides strong, and that weak/strong
pair is the whole switch between a bare link and a hosted one. a freestanding
link must NOT drag the strong one in.

### 2. `libc/` empties

delete `libc/mem.c` and `libc/str.c`; `libc/` is then `limine.h` alone, which is
a vendored header and belongs beside the bootloader glue in `port/inle/` anyway.
`common.mk:57`'s `c_c` retires with it.

### 3. the errno seam -- ONLY if something with an error path joins

the one real design question, and today it does not arise. nolibc reports through
`__errno_v` (`nolibc.c:203`); freestanding has no errno and never wanted one. the
five shared functions cannot fail, so the seam is unneeded. if a future member
CAN fail -- a `strtol` coming back, a `snprintf` -- resolve it before writing the
member, not after: either a weak `__errno_location` the freestanding side leaves
unimplemented, or the shared body returns the error and each side reports it.
**do not let a shared file reference `errno` unconditionally.**

## what NOT to do

- **do not try to link `nolibc.c` whole into the kernel.** it is ONE
  translation unit, so pulling `strlen` pulls the mmap allocator, `__ai_start`,
  the signal restorer and 224 `syscall` instructions that in ring 0 fault or go
  nowhere. it also opens with twenty system headers, and the kernel builds
  `-nostdinc`.
- **do not wait for the mooncc kernel migration to do this.** checked against
  doc/moon-kernel.md: rung 4 turns the `.S` files into holo lays and rung 5
  flips `kernel.mk`'s default compiler. same translation units either way --
  `libc/*.c` stays in `k_shared_c` on the far side. the migration is about the
  last C *compiler* island, not the libc.
- **do not move `malloc`/`free`.** the kernel's allocator is the kernel's, by
  design.

## the verdict

**the correctness case is spent; what is left is tidiness.** the drift that
motivated this arc lived in a 20-line number parser with an overflow rule,
duplicated by hand, with the invariant recorded only in a comment. that
duplicate is deleted. what remains is five byte loops that cannot drift in any
way a gate would miss -- change `memcpy` on either side and everything fails at
once, loudly.

so rung 0 is worth building on its own, and rungs 1-2 are worth doing when
something else is already open in `moon.l`'s member list or in `kernel.mk`.
**what would revive the case** is the freestanding side growing a function with
a policy in it: a `snprintf` for kernel diagnostics, a real string surface, a
`strtol` coming back for a command line. the rule that falls out of the original
bug: **a duplicated function is fine while it is a loop, and a liability the
moment it has a rule.**
