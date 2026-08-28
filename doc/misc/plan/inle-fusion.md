# plan: one binary, host and free

**THE CLAIM: the tree builds two loves for one machine.** `out/host/love` and
`out/free/love-x86_64.elf` share `src/love.c`, `am.c` and quay -- ~86% of the
kernel's text and 78% of the host's -- and then implement twenty-one of the same
behaviours twice. `open`, `stat`, `readdir`, `lseek`, `mkdir`, `rename`, `pipe`,
`dup`: each is one body in `src/posix.c` and a second in `src/kmain.c`. The
end state is one ELF per ISA that boots on metal or runs hosted, with inle a
third seat beside host and wasm rather than a second application.

⚠ **this is not a code-size reduction, and selling it as one is how it goes
wrong.** Measured: the duplicated marshaling is ~200-350 lines, and the syscall
door that replaces it is ~150-250. What improves is implementations-per-behaviour
(21x2 -> 21x1), so a bug in `stat`'s shape is fixed once -- and nolibc becomes
callable inside the kernel, which is what lets more of the crew run there.

## where it stands

- **rung 0** (`4d0106ed`) -- one nif registration mechanism. `kmain.c`'s `defs[]`
  rides the `love_nifs` section and the kernel drains `[__start_love_nifs,
  __stop_love_nifs)` like `src/main.c:1291` does. Three lines of code; it is the
  gate for everything else, because the image indexes host nifs BY POSITION in
  that section.
- **the syscall seam** (`67ab3584`, `46417cf5`) -- `src/sys.c` answers
  `__ai_sys` in C where a hosted seat has a mksys lay issuing `syscall`/`svc`.
  Four numbers: read, write, close, lseek. Gate `test/kernel/sys.l`.
- **phase A1** (`4433e222`, `37171d37`, `5353380e`) -- the ramfs has a face a
  syscall can call: nine `k_fs_*` taking (bytes, len), with the love marshaling
  split off above. All behaviour-neutral by gate.
- **one sign** -- every C face (`k_fs_*`, `k_fd_*`, `k_parent_ok`) answers 0 or
  a NEGATIVE errno, because that is what `__ai_sys` owes its caller (impl.h's
  `er()` reads an error as `(unsigned long) r > (unsigned long) -4096`), so
  `src/sys.c` forwards their answers untouched. ⚠ the love conventions are the
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
| syscalls `src/posix.c` reaches | **34** (not 78 -- that is all of nolibc) |
| ..answered so far | **20**: read/write/close/lseek; the path family (openat, newfstatat, mkdirat, unlinkat, renameat, chdir, getcwd, fchmodat, utimensat); the fd family (pipe2, dup3, fcntl, fstat, getdents64); getpid + clock_gettime |
| ..that inle simply lacks, and `-ENOSYS` already answers | ~9 (clone, wait4, kill, setpgid, setsid, mount, unshare, madvise, getpgid) |
| `posix.c` changes needed to compile freestanding | **none** -- verified, it builds clean under the kernel's flags today |
| its undefined symbols | 84: 17 love-core (kernel has them), 3 nolibc string (linked), ~64 nolibc members to link |
| boot spent evaluating source | ~2 s (`test_kernel` 10.41 s wall vs its own 7.8 s corpus) |

## the questions that are settled

Read these before re-opening one; each cost more than the code it justifies.

**The seat is the PORT LAYER's, so a syscall is under it** (`cc555499`).
`k_fd_eff` is reached from `fd_readn`, `fd_writen`, `ai_fd_close` and
`k_procseat` and nowhere else; `k_fdopen` takes the fd it was handed. So an fd
spelled in love is an absolute row, and `src/sys.c` is seat-blind by that same
law -- as a syscall is on a real kernel, where the number the trap carries is
already the caller's own. ⚠ the divergence: a SEATED task spelling
`write(1, ..)` reaches row 1 where POSIX would reach what its parent seated.
Nothing does. Closing it means per-task row tables, never an ambient `g` -- `g`
moves under collection.

**The `__ai_sys` collision** -- in one binary both the mksys lay and
`src/sys.c` define it. `core.c`'s `__ai_start(long *sp, long osv)` already
takes the kernel identity FROM THE ENTRY (`__ai_osv = osv ? osv :
__ai_osdetect()`); freebsd/aarch64 depends on it today because it SIGILLs any
non-zero `svc` immediate and cannot be probed blind. So inle becomes another
`__ai_osv` value: `__ai_sys` keeps one definition, `src/sys.c`'s dispatch is
renamed, and `__ai_call` gains one arm on a value it already loads. Sufficient
because `__ai_sys` has exactly three callers -- `__ai_call` (universal),
`__ai_fb` (only v>=2), os.c's probe (only v==0). ⚠ ORDERING: `v >= 2` means "a
BSD, translate", so the inle test must come FIRST. ⚠ on metal `__ai_start` is
not the entry, so metal WRITES `__ai_osv` rather than passing it.

**The aarch64 entry.** x86_64 already carries two entries -- `e_entry` and the
PVH note's, and `src/mkboot.l` says so: "the ELF entry is kmain's; the PVH
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
- A3 ✅ `src/posix.c` rides the kernel whole: 66 nolibc members named into
  `c_c` (core.c stays out; src/sys.c answers its four seat symbols -- environ,
  the unbuffered std streams, `__ai_sigret`), and kmain shed its SEVENTEEN
  posix twins in the same commit. `open`/`close` stay -- their host twins live
  in main.c, which fuses at C -- and `getpid` stays as the TASK pid. posix's
  spawn family registers and refuses at runtime (fork is -ENOSYS); the boot
  text's task shim shadows those names regardless. ledger: kmain -274, +51
  across the seat and the build. the corpus runs on the adopted nifs, kboot's
  real pipelines included.

**phase A is climbed.** what "kernel" means now: the machine bring-up, the
ramfs faces, the seat/task plumbing, the disk and virt doors -- plus one shared
posix surface it hosts rather than mirrors.

**B -- one "which kernel" flag, while still two binaries.** The de-risking step:
every runtime branch fusion needs becomes live and gated before anything merges.

- B1 ✅ inle is `__ai_osv` -1, written at kmain (metal has no `__ai_start`);
  `-D__inle__` is gone and the kernel compiles nolibc with `AiOsTranslate` ON.
  ⚠ the value is NEGATIVE by necessity: ~25 member sites read `v >= 2` as "a
  BSD" and ~5 read `v < 2` as "speak canonical linux", which inle does -- a
  positive value would take freebsd shapes. `__ai_call`'s first arm takes v<0
  to `__ai_inle` (src/sys.c's renamed dispatch); os.c carries a weak -ENOSYS
  default for links without the door; and the kernel links the REAL mksys tail
  (dead on metal, but it is the fused shape and it answers `__ai_sigret` and
  the netbsd leaves the stubs used to fake).
- B2 ✅ `src/seat.c`, one TU both links carry: `ai_clock` is one
  clock_gettime body (src/sys.c's arm serves it from `k_clock_ms`);
  `ai_fd_port_vt` + the statics exist once, the host bodies branching to
  kmain's exported `k_port_*` lanes on v<0 -- the port protocol keeps busy
  and end distinct, which read(2) cannot carry, so the vt branches ABOVE the
  syscall door; `ai_libs` picks between per-frontend `k_libs`/`host_libs`.
  the mechanism is weak defaults for whichever side a link lacks, plus a weak
  `__ai_osv` for foreign-libc links (love0) that carry no os.c -- zero reads
  hosted, which such a link is. the ports (playdate/mps2/virt/teensy) define
  their own vt and never link seat.c.
- Gate: both binaries build and pass, the whole roster. ⚠ landing B2 tripped
  the KERNEL LANES' MEMORY WALL, not a defect: gen_major grows the pool by
  allocating a new contiguous 2x pair beside the old one, so the ask (~78M at
  today's corpus) must fit a hole the old pair fragments -- a placement
  lottery any image-size change re-rolls (a 5 KB delta lost it on
  test_uefi_arm64, `;; oom@len=16384`, len being the NURSERY size -- the ask
  showed only under a kmallocw-refusal probe). the lanes run 768M now
  (tools/ktest.l, kboot.l); the cure would be a pool pair in two blocks.

**phase B is climbed.** the C2 collision survey (nm over kernel-only vs
host-only TUs): FOUR symbols remain defined on both sides -- `ai_fd_close`,
`ai_ready`, `ai_sleep`, `ai_wait_fds` -- plus the planned pair-ups (`main` vs
`kmain`, and the open/close/getpid nif twins).

**C -- one link.**

- C1 ✅ the flag sets reconcile to ONE LINE: mooncc hears `-c -o -I -D -t -os
  -std= -Ttext/-Tdata -fno-inline -pie -freadme -ffreestanding` and
  tolerates-and-discards the traditional soup (`-g -O -W*` and the `-f`
  family) -- verified by byte-identical objects. even `-ffreestanding` went:
  love.c's one hosted/metal fork (the W^X code arena) branches on `__ai_osv`
  at run time, so the kernel compiles with the host moon lane's exact line,
  and the mmap family refuses -ENOSYS through the door like everything else.
- **the shape, chosen (revisable): PIE + PROJECTIONS.** the artifact stays a
  hosted static pie (ASLR kept -- the binary's one code-reuse mitigation, with
  tls/inflate/the reader as real C surfaces); metal boots a file DERIVED from
  it. the ET_EXEC-everywhere alternative was priced and declined for the ASLR
  loss; the love_rela machinery stays, so it remains reachable.
- C2 ✅ the kernel build IS the host's link: one `mooncc -pie` over one object
  set (main.c and the whole host surface aboard), and `tools/kproject.l`
  PROJECTS it for the doors -- every PT_LOAD re-based at the kernel base, the
  love_rela table applied there (the law `__ai_reloc` runs at a hosted start,
  run ahead of time), boot.o laid and patched below the image (its 32-bit PVH
  stub carries abs32 sites a pie cannot slide -- the one object that stays out
  of the link), `k_image_top` patched into the file where the flat link's
  `kimage_end` stood, symtab slid (the UEFI loader reads `kboot` off it).
  klink.l retired; ldkern stays for the ports. with core.c aboard the LAST
  twins fell: errno and the streams are nolibc's (`k_seat_init` arms what a
  hosted `__ai_start` would), malloc runs its mmap arenas over src/sys.c's
  page arm -- kmallocw supplies pages like any kernel does, zeroed because
  MAP_ANONYMOUS promises that -- and quit/getpid branch to `k_lvm_` twins.
- C3 ✅ absorbed by the projection: the projected ELF's `e_entry` IS `a64boot`,
  and qemu's arm64 `-kernel` reads exactly that -- the raw-Image question was
  an artifact of the one-file-everywhere shape and never arises under
  projection. arm64 came through the same tool with zero arch-specific code.
- Gate ✅ the whole roster on the fused pipeline: every door, both arches,
  kboot's real pipelines on the projected SHIPPED kernel, test_slow + the
  seed fixpoint.

**THE ARTIFACT IS UNIFIED** (8e6f472c): `out/host/love` carries the shipped
kernel -- kmain with the kore cats, the ramfs, the syscall door, the arch
bring-up, the vector lay -- compiled through the MOON LANE (a seed builds the
artifact before any $m exists), and the shipped kernel is the ARTIFACT'S OWN
PROJECTION: test_kboot boots exactly what `make` installs, source blob and
all. the moves it took: the kernel-only nifs ride `ai_knifs`, a bracket
apart, so reset/fault/the virt doors never enter the hosted book; vec.o's
pointer tables lay as .data (under the pie `__ai_reloc` WRITES them hosted --
a read-only segment there is a startup segfault, and the linker has no
relro); the artifact's ramfs dates pin to the dist stamp (lcatfs -s) so the
seed fixpoint stays a function of the tree's bytes, while the test kernel
keeps real dates for the corpus's stat laws. the whole roster + the fixpoint
are green on the fused binary: 15.2 MB baked, of which 8.9 MB is the image
and 3.1 MB the source blob.

**THE INITRD IS THE BLOB, AND THE VERB EXISTS** (1b739c83, 54ce6907). the
2 MB carriage question answered itself: the freight was the kernel's file
trees in PLAIN TEXT beside their compressed twins in ai_srcgz -- so the
shipped kernel inflates the blob it already carries and walks the tar into
the ramfs (symlinks resolved; the K_TEST kernel keeps the lcatfs bake for its
real-mtime stat laws), korecat is catted off the ramfs from a baked roster,
and the baked fused artifact is 13.46 MB -- THE PRE-FUSION SIZE. the whole
tree lands on metal, which is the metal seed's doorstep. and `love kernel
OUT.elf` (crew/source/source.l) emits the boot image ANYWHERE from nothing but what
the binary carries -- the boot stub laid from the blob's mkboot.l over baked
holo, the arch read off its own e_machine -- gated by test_kverb, which
demands BYTE-IDENTITY with make's own projection.

**D -- THE METAL BOOT WAKES THE IMAGE** (ab77f859, 550b15bd). kmain asks
`ai_baked_pick` for the projection's re-based image and wakes it; the egg,
the module warmup and the korecat drink are the fallback lane (an unbaked
cross pie's stub, a torn blob). boot: ~2.2 s -> ~0.26 s under TCG. two seams
the wake found, both fixed at their root:

- a declined `nif` answered ZERO and ala installed it as a closure entry --
  latent everywhere, fatal on the first seat that actually declines. the
  decline now answers the interp twin itself, so the glaze transparently
  interps wherever code pages are refused (inle, wasm, a failed map).
- a baked closure CAPTURES its doors, so the seat text's by-name shadows
  don't reach a woken lush's spawn. the crew now bakes against `seat-doors`
  wrappers (spawn/spawnio/spawnmap/wait read a tablet per call) and the
  wake pins its task shim into the slots. captures heal at one choke point;
  the egg lanes are untouched (their cat captures the live door directly).

still D-adjacent: the arm64 artifact via the cross lane (its kernel projects
from the odir pie and egg-boots); the right inverse (`love hostbin` on metal
-- the projection is bias-invertible, the pie's own header rides the flat
image at bias+0); a metal nat door (out-of-pool pages through the low
window, which keeps X) if the interp twins ever want company.

## what is still open
- **`getpid` through a syscall** has no `g`, so it cannot know the running task.
  Same shape as the seat divergence, and it wants the same answer.
- **the kernel lanes' memory wall** (phase B's gate note) -- 768M is margin,
  not a cure; the cure is gen_major's pool pair in two blocks.
- **No door has booted on metal.** Every loader here is proven against OVMF in
  qemu only.
