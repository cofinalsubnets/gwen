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

**Rung 1 -- the raw-fd lane, finished by subtraction.** `syswrite` and `syscall`
exist because the raw-fd lane is half built. `openfd` and `pipe` mint an fd,
`lseek` seeks it (`posix.c:725` names it "the openfd lane -- not ports"),
`close` closes it -- and nothing reads or writes one. Every port nif in io.c
is `if (iop(Sp[0])) { .. }` falling through to a no-op, `(fputs port s)` even
documented "no-op on misuse", so a charm handed to `say` or `see` is silently
ignored. That hole is the whole reason for a second door. Close it under the
names that already exist and both instruments have nowhere left to be special.

- **`fdclose` folds into `close`. LANDED.** They were complementary halves of
  one operation: `lvm_close` tested the port kind and fell through to a bare
  `ZeroPoint` on a charm; `lvm_shutfd` closed a charm and answered `()` for
  anything else. Shutfd's three lines are close's charm arm now -- a nif retired
  and 41 call sites spelled `close`.
- **the io.c port surface takes a charm as an fd** -- `see` `say` `put` `slurp`
  `flush`. Five silent no-ops become operations, and `syswrite` is not needed
  under any name, its one law (an absent row swallows its bytes, so the count
  comes back) being an ordinary count. fds 0, 1 and 2 are not special here: a
  charm that reached `see` by mistake is misuse, and refusing the low three to
  catch it would forbid `say` to stdout by number, which is a thing to want.
- **`stat` takes a charm as an fd**, which is `fstat` without a new name.
  Nothing in the tree asserts anything about `(stat <charm>)`, and the
  contract's `()` for "absence or unreadability" is already the right answer
  for EBADF. Worth doing on its own account: the `fstat` ROW has no love-level
  caller at all -- src/image.c's two are the hosted file-load path and the
  kernel wakes from memory -- so its only exercise anywhere is the instrument
  written to exercise it.
- **`lseek` stops normalizing an unknown whence.** `posix.c:832` reads
  `wh == 1 ? SEEK_CUR : wh == 2 ? SEEK_END : SEEK_SET`, so `(lseek fd 0 7)`
  seeks to 0 and reports success. Pass it through and the row answers EINVAL.

Nothing is added. `syswrite`, `syscall` and `k_sys_nr` go (~90 lines over
kmain.c and sys.c), `fdclose` went, four nifs gain a kind, one loses a bug.

test/kernel/sys.l is then ordinary corpus that runs on the host AND the kernel:
counts and errnos, byte-exact reads off the ramfs, close and its EBADF on a
reclose, pipe roundtrips, partial reads with the remainder waiting,
create/append/extend/unlink, rename, chdir/cwd, chmod, mkdir/rmdir/ENOTEMPTY,
readdir. The struct-layout laws come out better rather than worse: comparing
`(stat fd)` against `(stat path)` for one file catches a wrong offset through
the parse that actually matters, where reading a raw 144-byte cask only asserts
that the offsets are the ones we already wrote down.

What goes with the instruments is one kind of law: the refusal branches nolibc
cannot reach, and raw wire formats. A dirfd that is not AT_FDCWD, an absolute
path ignoring its dirfd, O_RDWR on a ramfs file, `dup3` src == dst, `pipe2` with
a flag word, `fcntl` with a stranger cmd, UTIME_OMIT, getcwd's ERANGE, the
NULL-pointer EFAULT arms, getdents64's 8-aligned record walk. Every ROW under
those keeps coverage through an ordinary nif -- dup issues fcntl, dup2 issues
dup3, pipe issues pipe2, utime issues utimensat, rmdir issues unlinkat with
AT_REMOVEDIR, readdir issues getdents64, stat issues newfstatat -- so what is
lost is the argument values those rows refuse, which are defensive arms
guarding against a caller that does not exist. src/sys.c should say so on them
rather than leave them looking exercised. getpid's row is the one casualty that
does not move: this seat has no getpid nif, so it goes untested.

Two constraints on the rewrite. `fdopen`'s port finalizer owns the fd -- "hand
it over, don't close it too" -- and sys.l double-closes freely today because
`syscall "close"` went around the port. And a port buffers, so a law that
interleaves seeks and reads on one fd cannot use a port for both; that is what
gave lseek a raw lane in the first place.

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
