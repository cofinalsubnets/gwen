# plan: one kernel, no test face

**THE CLAIM: K_TEST is not a build flag, it is a second kernel.** Twelve
conditional blocks over 268 lines of C, plus thirteen Makefile sites and a
parallel `out/free/<a>-test/` object tree, and what they buy is a machine that
diverges from the shipped one in exactly the three places the shipped one is
least covered: it never walks the ramfs, never wakes an image, and never warms
an egg from carried source. A green corpus therefore says nothing about any of
them. That is not a theory -- a whole-userland failure lived in the warm path
until 2026-09-01 (see "what this buys"), and no gate could see it.

The whole apparatus exists to hand the kernel a corpus it is already carrying.
`dist_drop` is `bench port wasm`, so `test/` rides the source blob: 394 files,
in the ramfs of every shipped kernel. And `k-prog`'s third lane already evals a
`.l` path off that ramfs. Both facts are measured, on the SHIPPED x86_64 elf:

    -append "test/zz-fin.l"        ;; missing test_get      (read, evaled, right failure)
    kore wc test/spec.l            75679                    (byte-exact off the ramfs)

So the corpus needs no bake, no second odir and no second face. It needs a
driver file and a way to say "run it".

## the map: what each block buys

| block | buys |
|---|---|
| kmain 553, 674 | the `lib/*.l` lcatfs bake, INSTEAD of the ramfs untar (83 lines of gz+ustar+symlink walk the corpus never runs) |
| kmain 1639, 1739, 1816 | the `syswrite` and `syscall` nifs |
| kmain 1822, 1830, 1904 | `ktests[]` vs `src_korelist[]`, and which one binds |
| kmain 1869 | `woke = false` by construction, INSTEAD of the image wake |
| kmain 2040 | `(use 'coin) (use 'rng) (use 'q) (use 'kanren)` -- the corpus asserts on them, a booting kernel wants none |
| kmain 2059 | drink `tests` through `reads`, INSTEAD of the kore cat and the boot cmdline |
| sys.c 144 | `k_sys_nr`, the arch-keyed name->number table `syscall` reads |
| Makefile x13 | `ksuf`, the `-test` odir, `-DK_TEST -Dai_tco=1`, the header swap, the `kt` roster, `ktests.{l,h}`, four gates |

`-Dai_tco=1` is redundant: `src/love.h:39` already defaults it to 1 and the
shipped kernel takes the default.

## the rungs

**Rung 0 -- the exit channel and quit's second room. LANDED 83db7565.**
`k_qemu_exit` was written per arch to carry the corpus verdict out as a process
exit code. Nothing reads it: `tools/ktest.l` decides on the output TEXT and
never touches hark's status half, `kboot.l` is the same shape, and the three
`$?`-after-qemu in the tree are qemu-user gates. It was also miswired --
`test/kernel/kore0.l` pins `(: (quit n) n)` before the cat loads, so zz-fin's
`(quit 1)` was the identity and control fell to `k_qemu_exit(0)`; a failing
corpus asked qemu to exit 0. With it gone the `#ifdef` in the quit door goes
too: unseated is reset on every face, and the corpus answers its own codes
through kore0.l's pin, one door deeper. -41/+4 lines over three files.

**Rung 1 -- close the nif gaps, then delete both instruments.** `syswrite` and
`syscall` are diagnostic nifs whose 190 lines of law mostly do not need them.
The kernel links `src/posix.c` unchanged and overrides none of its file names,
so love's ordinary `open`/`stat`/`readdir`/`lseek` already walk the same chain
the instruments do -- nif -> nolibc -> `__ai_inle` -> `src/sys.c`'s row -- and
`posix.c` already answers `-errno` (line 834), so a normal test can tell ESPIPE
from EBADF today. What it CANNOT reach is a thin surface, not a missing level:

- `openfd` takes a mode index 0..3 (`posix.c:493`), not flags: no `O_DIRECTORY`,
  no bare `O_WRONLY`, no chosen mode bits, no dirfd
- no `fstat` nif -- `stat` takes a path only
- no `fcntl` nif at all
- `dup3`/`pipe2` flag words, `AT_REMOVEDIR`, `UTIME_OMIT`

Widen those, rewrite `test/kernel/sys.l` as ordinary corpus laws that run on the
host AND the kernel, keep the one law with no other observer -- an absent row
swallows its bytes, `(syswrite 4096 s)` answers the count -- and both nifs and
`k_sys_nr` go. Delete the stale comment in sys.l claiming `lvm_lseek` flattens
to -1; it does not, and it is what made this look like a level-cut problem.

**Rung 2 -- the corpus off the ramfs.** A driver file (`test/kernel/all.l`)
reads a roster, slurps each member and drives `reads` -- the same shape kmain
already spells for the kore cat at 2088-2097, about ten lines of love. The
roster becomes a FILE both the host gate and the driver read, which retires the
`kt` Makefile variable, `out/lib/ktests.{l,h}`, `out/lib/kfs.h` and `lcatfs.l`'s
last caller. The gates invoke it as `-append "test/kernel/all.l"`. Rung 1's
laws ride the same file on both seats.

**Rung 3 -- the layers move into the driver.** `(use 'coin) (use 'rng) (use 'q)
(use 'kanren)` leave kmain for `all.l`; they are baked modules, so `use` finds
them. The shipped image stops carrying a ring, a random stream, rationals and a
unifier it never wanted.

**Rung 4 -- delete K_TEST.** What is left is `ksuf`, the `-test` odir, the
header swap, `-DK_TEST`, `tools/ccdb.l:32`, and the `ifndef K_TEST` half of the
`k_pie_in` fork -- which then reads plainly: at the host's own arch project the
shipped binary, everywhere else build the pie. `src/x86_64_asmops.h:11` loses
its last sentence.

## what this buys

Coverage, and the arc has already paid for the claim. `test_vec` is the only
gate in the tree that boots a WARM kernel -- everything else either projects the
host binary, which wakes a baked image, or builds K_TEST, which carries no
`src.o` to warm from. It was one of the 43 orphans no aggregate reached until
148fdc35, and the first thing it found was a warm aarch64 kernel that reaches
the shell, answers status ok and prints nothing at all, at `-m 512M` and no
other size from 256M to 4096M. That is the GC placement lottery `tools/ktest.l`
already documents (a major takes a contiguous 2x pool beside the old one). One
kernel means the corpus runs on the ramfs walk, the wake and the warm path, so
that class of hole has a gate over it.

And it removes the last reason the two arches differ in kind rather than in
machine: x86_64 projects and wakes, aarch64 builds and warms, and after this
both run the same corpus the same way.

## traps this plan already knows

- **the roster is an ordering, not a set.** `mk/common.mk`'s `t` front-loads
  00-init, spec and uu.l explicitly, and a locale `ls` would order `uukind*`
  before `uu.l` and run the laws against an unloaded kernel. A roster file must
  keep the order; globbing the ramfs must not replace it.
- **kore0.l's `quit` pin is load-bearing and positional.** It shadows `quit`
  for the REST of the stream, so files before it (00-init, spec, uu) still meet
  the real door -- which now resets. Ordinary assert failures do not quit; only
  `test-strict` (a missing name) and `test_fin` do.
- **a red gate must stay red.** Deleting a verdict channel is exactly the change
  that turns a gate green. Rung 0's falsifier: append `(assert (= 1 2))` to a
  kernel corpus file, confirm `test_disk` reports "1 failed:" and exits 2,
  revert. Run it again at every rung that touches the failure path.
- **`k_semihost_exit` has a second caller.** `test/gate/asmops.c` uses it for
  the clang/mooncc differential; it stays in the header whatever kmain does.
- **the corpus is bigger than the machine at some sizes.** `ktest.l` asks for
  768M, and `test/gate/vec.sh` had to be raised to match (ed4007f7). A merged
  corpus is not smaller; price the margin before assuming a size.
- **K_TEST also picks the fs SOURCE.** Rung 2 hands the merged kernel a corpus
  whose stat laws (`test/kernel/fs.l`) read mtimes; the lcatfs bake preserved
  real ones and the ustar walk carries the archive's. Check the laws hold on
  tar mtimes before deleting `kfs.h`.
