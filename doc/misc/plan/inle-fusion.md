# plan: one binary, host and free

**THE CLAIM: the tree builds two loves for one machine.** `out/host/love` and
`out/free/love-x86_64.elf` share `core/love.c`, `am.c` and quay -- ~86% of the
kernel's text and 78% of the host's -- and then implement twenty-one of the same
behaviours twice. `open`, `stat`, `readdir`, `lseek`, `mkdir`, `rename`, `pipe`,
`dup`: each is one body in `host/posix.c` and a second in `free/kmain.c`. The
end state is one ELF per ISA that boots on metal or runs hosted, with inle a
third seat beside host and wasm rather than a second application.

⚠ **this is not a code-size reduction, and selling it as one is how it goes
wrong.** Measured: the duplicated marshaling is ~200-350 lines, and the syscall
door that replaces it is ~150-250. What improves is implementations-per-behaviour
(21x2 -> 21x1), so a bug in `stat`'s shape is fixed once -- and nolibc becomes
callable inside the kernel, which is what lets more of the crew run there.

## where it stands

- **rung 0** (`4d0106ed`) -- one nif registration mechanism. `kmain.c`'s `defs[]`
  rides the `ai_nifs` section and the kernel drains `[__start_ai_nifs,
  __stop_ai_nifs)` like `host/main.c:1291` does. Three lines of code; it is the
  gate for everything else, because the image indexes host nifs BY POSITION in
  that section.
- **the syscall seam** (`67ab3584`, `46417cf5`) -- `free/sys.c` answers
  `__ai_sys` in C where a hosted seat has a mksys lay issuing `syscall`/`svc`.
  Four numbers: read, write, close, lseek. Gate `test/kernel/sys.l`.
- **phase A1** (`4433e222`, `37171d37`, `5353380e`) -- the ramfs has a face a
  syscall can call: nine `k_fs_*` taking (bytes, len), with the love marshaling
  split off above. All behaviour-neutral by gate.
- **one sign** -- every C face (`k_fs_*`, `k_fd_*`, `k_parent_ok`) answers 0 or
  a NEGATIVE errno, because that is what `__ai_sys` owes its caller (impl.h's
  `er()` reads an error as `(unsigned long) r > (unsigned long) -4096`), so
  `free/sys.c` forwards their answers untouched. ⚠ the love conventions are the
  `k_*` wrappers' business and did not move: positive for most doors, `()` for
  absence, and chdir's negative lane -- which makes chdir the one wrapper that
  does NOT flip.
- **the pointer instrument** -- core strings now carry a NUL behind their bytes
  (one sizing macro, `str_width`; love semantics untouched), so `(syscall
  "name" a b c d)` marshals by kind: charm = integer, string = its bytes (a
  path as C expects), cask = an output buffer read back with snip, anything
  else -1 before the door. gated through write (string in) and read (cask out)
  over pipes and the ramfs. every remaining A2 number now costs a test, no
  nif, pointer arguments included.

## the numbers that size the rest

| | |
|---|---|
| syscalls `host/posix.c` reaches | **34** (not 78 -- that is all of nolibc) |
| ..answered so far | **19**: read/write/close/lseek; the path family (openat, newfstatat, mkdirat, unlinkat, renameat, chdir, getcwd, fchmodat, utimensat); the fd family (pipe2, dup3, fcntl, fstat, getdents64) + getpid |
| ..that inle simply lacks, and `-ENOSYS` already answers | ~9 (clone, wait4, kill, setpgid, setsid, mount, unshare, madvise, getpgid) |
| `posix.c` changes needed to compile freestanding | **none** -- verified, it builds clean under the kernel's flags today |
| its undefined symbols | 84: 17 love-core (kernel has them), 3 nolibc string (linked), ~64 nolibc members to link |
| boot spent evaluating source | ~2 s (`test_kernel` 10.41 s wall vs its own 7.8 s corpus) |

## the questions that are settled

Read these before re-opening one; each cost more than the code it justifies.

**The seat is the PORT LAYER's, so a syscall is under it** (`cc555499`).
`k_fd_eff` is reached from `fd_readn`, `fd_writen`, `ai_fd_close` and
`k_procseat` and nowhere else; `k_fdopen` takes the fd it was handed. So an fd
spelled in love is an absolute row, and `free/sys.c` is seat-blind by that same
law -- as a syscall is on a real kernel, where the number the trap carries is
already the caller's own. ⚠ the divergence: a SEATED task spelling
`write(1, ..)` reaches row 1 where POSIX would reach what its parent seated.
Nothing does. Closing it means per-task row tables, never an ambient `g` -- `g`
moves under collection.

**The `__ai_sys` collision** -- in one binary both the mksys lay and
`free/sys.c` define it. `core.c`'s `__ai_start(long *sp, long osv)` already
takes the kernel identity FROM THE ENTRY (`__ai_osv = osv ? osv :
__ai_osdetect()`); freebsd/aarch64 depends on it today because it SIGILLs any
non-zero `svc` immediate and cannot be probed blind. So inle becomes another
`__ai_osv` value: `__ai_sys` keeps one definition, `free/sys.c`'s dispatch is
renamed, and `__ai_call` gains one arm on a value it already loads. Sufficient
because `__ai_sys` has exactly three callers -- `__ai_call` (universal),
`__ai_fb` (only v>=2), os.c's probe (only v==0). ⚠ ORDERING: `v >= 2` means "a
BSD, translate", so the inle test must come FIRST. ⚠ on metal `__ai_start` is
not the entry, so metal WRITES `__ai_osv` rather than passing it.

**The aarch64 entry.** x86_64 already carries two entries -- `e_entry` and the
PVH note's, and `free/mkboot.l` says so: "the ELF entry is kmain's; the PVH
entry rides the note". aarch64 has one, and `qemu -kernel` uses it (measured:
`e_entry` 0x40204000 vs load base 0x40200000, and `.boot` at the base is page
tables, so an image-base entry would execute them). Our UEFI loader reads
`e_entry` too but is ours to change. So the conflict is `-kernel` against the
hosted OS loader, and the answer is Linux's own: **one ELF plus a raw Image
projection**, `e_entry = _start` for hosted, the projection entered at byte 0
for `-kernel`. Measured: qemu loads a raw arm64 image at RAM base + 0x80000 and
enters its first byte. Costs laying `a64boot` first (it is 16 KiB in today,
behind the page tables) and either a relink or a 64-byte Image header.

**The image needs no work of its own.** `ai_image_load`'s guard is
`(&ai_image_save - image_immortals) == H.anchor` -- a SAME-BINARY check, and a
gap rather than two addresses precisely so ASLR cannot move it. One binary means
it holds, so the existing `bake -L` produces an image the metal boot wakes. ⚠ do
NOT build the `port/mps2` qemu-BAKER pipeline for this: that exists because the
Playdate genuinely is a different binary.

**Precise below, lossy above, never the reverse.** `k_fs_open` tells six
failures apart where the love doors have always answered a bare `-1`; the
flattening lives in the marshaling, where it is a choice. `k_fs_stat` fills
`struct k_st {size, ms, mode}` and NOT a `struct stat`, because ino/nlink/uid/dev
have no answer in a ramfs and the fabrication belongs where it is visible.

## the phases

**A -- inle runs host code.** The bulk, and it stands alone: even stopping here,
twenty-one behaviours stop existing twice.

- A1 ✅ the path and fd faces.
- A2 ✅ the syscall table: 19 numbers live, the ledger above. the instrument
  takes pointer arguments (strings in, casks out); every number is a dispatch
  arm plus tests. ⚠ `k_fs_open` keeps its 'r' misses ONE k_find deep: they are
  the load path's probe lane, and a k_dirp there ran the corpus 24x slower.
  only a create pays k_dirp; the openat arm assembles the directory answers on
  its own slow path -- and a directory opens READ-ONLY there as a dents row
  (close + cursor, read(2) on it EISDIR), which is getdents64's shape answer.
- **the g question, settled**: g holds exactly one fact the kernel tables do
  not -- WHICH TASK RUNS (`k_cur_pid` reads the run ring's head). the fd faces
  never needed it (k_dup_row took g and never read it), so they split the A1
  way and are g-free. the residue splits by layer: inle is ONE process, love
  tasks its threads -- getpid(2) answers the machine's constant, the task pid
  stays with the nif, where g is threaded. per-task fd tables, if ever, take
  identity as an explicit pid into pid-keyed kernel tables (k_seats' shape),
  never an ambient g.
- A3 -- link the ~64 nolibc members, add `host/posix.c` to the kernel build,
  delete the 21 duplicate nifs. ⚠ the deletion must be in the SAME commit as the
  link, or 21 names are defined twice.

**B -- one "which kernel" flag, while still two binaries.** The de-risking step:
every runtime branch fusion needs becomes live and gated before anything merges.

- B1 -- `__ai_osv` for inle; metal entry writes it; `__ai_call` branches. ⚠
  `-D__inle__` and impl.h's `&& !defined(__inle__)` come back OUT here: fusion
  needs `AiOsTranslate` ON, since one binary must also be able to be freebsd.
  The compile-time guard is a stepping stone, correct only while the kernel is a
  separate link.
- B2 -- one definition each of `ai_clock`, `ai_fd_port_vt`, `ai_stdin/out/err`,
  `ai_libs`, currently in both `kmain.c` and `host/main.c`, branching on that
  same flag.
- Gate: both binaries still build and pass.

**C -- one link.**

- C1 -- reconcile the flag sets. ⚠ **UNEXAMINED, and the largest remaining
  unknown**: the kernel builds `-nostdinc -ffreestanding -fno-PIC
  -ffunction-sections`, the host `-fpic` with a different include set.
- C2 -- one object set, one holo link, dual entry.
- C3 -- aarch64's raw Image projection.
- Gate: one ELF passes the host corpus AND `test_inle`.

**D -- the image.** Nearly free once C lands; kills the ~2 s boot eval and
carries the AOT glaze.

## what is still open

- **C1**, above -- could be trivial or could be the hardest thing here.
- **`getpid` through a syscall** has no `g`, so it cannot know the running task.
  Same shape as the seat divergence, and it wants the same answer.
- **`getdents64`** -- variable-length output into a caller's buffer, the one
  face whose syscall shape and nif shape genuinely differ rather than being
  wrapped differently.
- **No door has booted on metal.** Every loader here is proven against OVMF in
  qemu only.
