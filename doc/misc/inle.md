# inle — the love machine

The plan for turning the bare-metal kernel (`free/`) from a REPL that boots into a machine
that runs the userland we already have: kore, lush, vi, cook, mooncc.

**The decision it rests on: one address space, one `g`, no protection.** Every program here is a
love program in a VM we wrote, not an ELF binary — so a process boundary would buy fault
isolation from bugs in our own C and nothing else. That is a **love machine**, and it should be
called that rather than Unix. What it borrows from Unix is the *surface* — `doc/misc/posix.md`'s L2,
the same nif names the host answers, so the userland moves without an edit.

Where it stops being enough is named at the foot, with what it would cost.

## where inle is today

`kmain.c` boots (two doors: `-kernel`, UEFI), lays a heap over the memory map, draws a
framebuffer console through quay, decodes PS/2 scancodes, and runs `((from 'bao 'shell) 0)`.
Interrupts, the timer, cooperative tasks (`twirl`/`catch`), and fd-parking all work.

It also has a filesystem now — the ramfs over a `.rodata` initrd, with the read surface whole
(`open` `close` `stat` `readdir` `lseek` `openfd` `fdclose`), `use` resolving `lib/<x>.l` off
it, and since rung 2 a **writable tree**: create, `mkdir` `rmdir` `unlink` `rename` `chmod`
`utime`, a cwd (`chdir`/`cwd`), and the environment (`getenv` `setenv` `environ`).
`k_sources[]` grows a row per open file. And it has a wall clock: `ai_clock` is epoch
milliseconds, off the machine's RTC.

Since rung 3 it has its userland: the whole kore cat (the fs and line tools, grep/sed, vi,
lush, cook, the holo toolchain) bakes into the shipped kernel and the boot command line
dispatches it off the verb registry — `-append "kore ls lib"` runs the tool and resets,
`-append "sh"` boots lush, an empty cmdline falls to the console shell with the toolbox
warm. `make run` boots to a lush prompt; the iso's second entry is the console shell.

Since rung 4 it has processes — pipes over kernel-heap queues, `spawn`/`wait` as a love shim
over `twirl`/`catch` (a process IS a task), per-pid stdio seats under the folded ports, and a
seat-aware `quit` — so lush runs real pipelines of kore tools.

Since rung 5 it has the disk: virtio-blk (PCI on x86_64, virtio-mmio on aarch64; polled, one
C file) under three raw nifs, and **FAT32 r/w written in love** (`lib/fat.l`, off the ramfs)
over them — a file written before a reset is there after it, and mtools reads what it writes.

Missing: network.

## the shape it grows into

Three mappings, all of them already half-built:

* **`k_sources[]` is the vfs.** A per-fd vtable (`readn`/`writen`/`putc`/`flush`/`ready`/`close` +
  `state`), and `k_source_open` is the one door in, growing the table through `g->alloc`. Every
  rung below adds rows.
* **ports are the fds.** `ai_io_alloc(g, fd)` is core, not host — it wraps an fd as a port with a
  close finalizer, and read/write come free.
* **tasks are the processes.** `twirl` answers a pid, `catch` waits on one, and the scheduler
  already parks a task on an fd and wakes the ready one (``).

⚠ **The kernel links no `host/*.c` yet, and that is a rung rather than a rule** — `k_shared_c`
is love.c + am.c + quay + libc. The nif MECHANISM is no longer a difference: `kmain.c`'s
`defs[]` rides the `love_nifs` section and the kernel drains `[__start_love_nifs, __stop_love_nifs)`
exactly as `src/main.c` does, so a `host/<app>.c` added to this build registers itself with
no edit. What is still written fresh is the nif BODIES, and `doc/misc/plan/inle-fusion.md` is the
plan for retiring that: `src/sys.c` answers `__ai_sys`, so nolibc — and everything written
against it, `src/posix.c` included — can stand on this kernel instead of a hosted one.

⚠ **The conventions are `doc/misc/posix.md`'s, exactly.** An effect answers `()` | a POSITIVE errno |
EINVAL on misuse; a value answers the value | `()`. `stat` answers `(size mtime-ms mode ns)`.
Divergence here is worse than absence — kore reads these shapes and a wrong one is silent.
⚠ **`chdir` is the exception, on BOTH seats**: it answers a NEGATIVE errno (`putcharm(-e)`, and
`-1` for misuse), here and in `src/posix.c`'s `host_chdir` alike. The twins agree, which is
what matters; the sentence above simply never reached this one. Do not "fix" one of them.

## the ladder

Sizes are one focused person, rough, and they compound: each rung is gated before the next.
Each rung names its own gate at the foot; **`make test_inle` runs all of them** — the corpus,
the disk, the UEFI door, the command line, the aarch64 twin, the clang twin. None of it is on
`test_slow`, so this is the one to type when free/ or the kore cat moves.

### rung 0 — the initrd, and a ramfs behind it  ✅ landed

`tools/lcatfs.l` bakes `lib/*.l` per-file into `.rodata` as `{path, bytes, len}` rows
(`out/lib/kfs.h`) where `lcatv.l` bakes one file into one literal. Reads come straight off the
rows; the first write copies the blob into the kernel heap and the entry reads from the copy ever
after. `open` and `close` land in `defs[]` beside it — the gate needs them, and `open`'s presence
is what lights `use` up (below).

No driver, no PCI, no disk. This is the rung that changes what the machine *is*, and it defers
every hardware question to rung 5.

* ⚠ **The copy is per FILE, never per fd** — two opens of one path must see each other's writes,
  or it is a bundle and not a filesystem. The `k_source` row's `state` holds only the handle
  (which entry, where in it, may it write); the bytes hang off the entry.
* ⚠ **`own` is a presence bit and has to be one.** A file written and then emptied is `{NULL, 0}`,
  which is exactly what one still in `.rodata` looks like — the tree's presence law wearing its
  C face, and the flag is the only thing that says which blob to read.
* ⚠ **The write door is bulk now.** `k_source` grew a `writen` beside `putc`, which the vtable's
  own comment had already invited: a `void putc` can only DROP a byte it has no memory for, where
  `writen` can refuse. A row without one is still written a byte at a time.
* ⚠ **`kmallocw`, not `g->alloc`.** A vt method is handed an fd and nothing else, so `g` is out of
  reach at the door that grows a file. On this seat they are the same heap — `g->alloc` is
  `ai_libc_alloc` → `malloc` → `kmallocw` — which is why `cbinit` already names it directly.
* **`k_source_open`'s grow branch runs now**, on the first file opened; the boot rows stay static
  as `kmain.c`'s law asks, and a failed grow leaves the console standing.
* **Not here: create.** `open path "w"` on an unbaked path refuses, which is absence and not
  divergence; the writable tree is rung 2.
* *gate:* `test/kernel/ramfs.l` (kernel-only, via `kernel.mk`'s `kt`) — open a baked path, read
  it, write it, append, truncate, grow it past the baked blob, put it back, and `use` it.

**The rung-1 payoff came with it, free.** `use` resolves `lib/<x>.l` off the ramfs on the
freestanding kernel with zero prel change, exactly as predicted below: the walk is gated on
`open` being in the book, and it is. The last law in `ramfs.l` proves it — `json` sits in no
baked `ai_libs` table on this seat, only in the initrd.

### rung 1 — the file nifs, and the clock under them  ✅ landed

`stat` `readdir` `lseek`, plus the raw-fd lane `lseek` needs (`openfd` `fdclose`) — `open`,
`close` and the `ai_fd_close` routing came with rung 0, and with them `use` off the ramfs. Every
name and every shape is the host's, because kore reads them and a divergence is silent where an
absence is loud.

* **The wall clock landed with it, and had to.** `ai_clock` answered `kticks` — an uptime in
  TENTHS OF A SECOND wearing the millisecond name, so `(rest 30)` slept a third of a second and
  every date was a fiction. It is milliseconds since the epoch now, the host's scale exactly: the
  100 Hz tick over a boot date, and `ai_sleep`/`ai_wait_fds` convert at their door — so the
  corpus's `(rest 30)` sleeps 30 ms, as it always claimed to.
* ⚠ **The date has to come from the machine, not the door.** No door answers a boot date, so
  the RTC is read directly:
  the mc146818 CMOS walk on x86_64 (BCD, 12-hour and update-in-progress all handled, bounded so
  an absent chip cannot hang the boot), one register of the PL031 on aarch64, which already sits
  inside the 2MiB block `mmio_map` lays for the UART. Both doors prove it in the gate.
* ⚠ **A directory is a PREFIX.** The initrd is flat — a row for `lib/json.l` and none for `lib` —
  so `readdir` answers the distinct next components of every path under a prefix, and `stat` on
  one synthesizes the dir bits and its newest child's date. Nothing is stored for a directory and
  nothing can be; rung 2's writable tree is what gives one an existence of its own.
* **The mtime is baked.** `lcatfs` takes each source's `stat` and lays the ms in the row, since
  the initrd carries no directory and that date exists nowhere else. A write stamps the copy from
  the clock, and `k_mtime` reads the copy once there is one — `k_blob`'s question, asked of the
  date.
* *gate:* `test/kernel/fs.l`, 24 laws — the four stat fields against a read's own byte count, the
  dir bits, absence as the real `()`, the root deduped to one entry, `lseek`'s three whences off
  a raw fd, and the clock read as a 2020s stamp on both arches.
* **Not here, and the plan was wrong about it:** the corpus's `io.l` cannot rejoin yet. Its file
  roundtrip writes `/tmp/l-io-test`, which wants **create** — rung 2's, not this rung's. The rest
  of `io.l` (taps, flows, `sound`) would run today; splitting the file for one law is not worth
  it, so it rejoins with rung 2.

### rung 2 — a writable tree  ✅ landed

**create** came first and pulled the shape with it: the parallel `kfsw[]` array is retired for a
table of **entries** in the kernel heap (`k_ents`, laid at first touch — every baked row plus
`tmp`, the scratch a POSIX machine promises and no initrd carries), growing as `open … "w"`/`"a"`
and `mkdir` add paths the bake never knew. Then the roster: `mkdir` `rmdir` `unlink` `rename`
`chdir` `cwd` `environ` `getenv` `setenv` — **plus `chmod` and `utime`**, which the plan did not
name but the gate's own tools ride (kore's `cp` chmods its copy, `touch` utimes). The cwd is a
kernel string (`""` canonical, worn as `"/"`); the environment is a tablet in the boot text, its
pairs on slot 0 behind the host's three doors.

* **Every path canonicalizes** (`k_canon`) against the cwd before it touches the table: absolute
  off the root, `.` holds, `..` pops, doubled and trailing slashes fall away. `""` and `"."` are
  still both the cwd, now by construction.
* ⚠ **`own` kept its job and grew a sibling.** The entry's `own` is still the bytes' presence
  bit; `heap` is the same bit for the *path* (create and rename spell names `.rodata` never
  held), and a NULL path is a retired slot the next create may take. `refs` counts open fds:
  unlink takes the entry out of the tree at once but the bytes free at the **last close** —
  POSIX's rule, and what keeps a live handle off freed memory.
* **A directory can still be a prefix.** The baked tree stays flat (`lib` has no entry); mkdir is
  what gives one an entry — and an *emptiness* — of its own. `rmdir` refuses ENOTEMPTY while
  anything lies under either kind.
* **rename carries a directory whole**: every live path at or under the prefix is respelled, the
  copies staged before any commit so a refused allocation leaves the tree untouched; a file lane
  replaces a target file under itself; `a` → `a/b` is EINVAL.
* **The errno are the host's, by number** (moon's own `<errno.h>`, no errno variable) — kore
  reads them back, and mv's EXDEV lane proves a shape can matter.
* ⚠ **Compile-time folding bit the gate, instructively.** A kore main folds its `quit` global at
  *its* compile, and `test/00-init.l`'s absent-nif fallback answers `()` — so the identity
  `quit` must be pinned **before** the crew cat loads (`test/kernel/kore0.l`), or every exit
  code reads `()`. Proved on the host, where the folded *real* `quit` exited the process
  mid-corpus.
* ⚠ **The plan was wrong about io.l a second time.** The `/tmp` roundtrip wanted create, as
  said — but the stdin-unget laws read the **corpus itself** off stdin on the host (the reader's
  pushed-back delimiter), and on inle `in` is the keyboard while the corpus rides a baked tap,
  so a bare `(see in)` on a quiet console parks forever. Those two laws gate themselves on the
  seat (`fault`, a nif only the kernel registers); everything else rejoined whole.
* *gate:* `test/kernel/wfs.l` (create/unlink/mkdir/rmdir/rename/chdir/chmod/utime/env, the tree
  restored at the foot); `test/kernel/kore.l` — kore's `ls cp mv rm mkdir touch pwd` driven as
  mains over the crew cat's fs prefix, baked into `kt` just before it; and `test/io.l` back in
  the kernel corpus. 3894 on both arches.

### rung 3 — kore and lush boot  ✅ landed

The whole `$(korefiles)` cat bakes verbatim (`lcatv`, the ktests precedent) into the SHIPPED
kernel and evals at boot through the stream shell — the corpus's own reads-over-a-tap lane,
since that is the one door proven on full-surface text. `holo` and `peg` join the kernel's
`ai_libs` beside uu and bao, because asbook.l wants `holo` and cook.l `peg`. The cmdline
rides `kboot` from the doors that carry one — PVH's `start_info`, the DTB's `/chosen`
`bootargs`; the UEFI loader passes none — and the boot text splits it quote-aware into `bootargv`.

⚠ **The cat loads SEATLESS, and the boot dispatches after it.** A member's own seat fires
as its file is read, and the cat is in dependency order — lush sits mid-cat, so a seat
firing there takes the machine with kore's applets still unread (no `ls`, no pipeline).
So `cmdline` is `("love")` while the cat loads, every seat sits out, and the foot of the
boot pins the real line and hands the program word to `k-prog` — the same registry door
rung 4's `spawn` uses. One dispatch on this machine, and an interactive lush gets the
whole toolbox: `-append "kore ls lib"` runs the tool, `-append "sh"` boots lush,
`-append "vi lib/json.l"` boots the editor, an empty line falls to the console shell.

* ⚠ **There is no shebang lane on inle.** `kore TOOL ARGS` is a love call into the registry
  tablet, not an exec — the multi-call trick is doing all the work, and it is why kore was the
  right thing to build first.
* **`quit` is the reset door.** A kore main's exit IS the machine's: `(quit n)` resets, which
  `-no-reboot` turns into a qemu exit — the gate's whole mechanism. The TEST kernel deliberately
  does not get the row: a failing assert's `(quit 1)` would reset mid-corpus and eat the summary,
  so 00-init.l's no-op pin keeps serving there and `exit` stays its one door out.
* ⚠ **The quit nif re-armed bao's file-help, instructively.** file-help folds `quit` at *bao's*
  define — the wasm note in bao.l, live here: with no quit nif the `(quit 1)` was a no-op and a
  load-scare welped through; with a real one, the FIRST absent-nif mention in the cat (fs.l's
  `hardlink`) printed one `;;` face and reset the machine. The fix is 00-init.l's own move made
  kernel-side: pin a no-op fallback for every host nif the cat mentions and this seat lacks
  (symlink/hardlink/readlink, the spawn family, pipes and fds, sockets, the tty trio), before
  the cat loads. The roster is self-retiring — a rung that lands the real nif takes its name
  off the list by existing. `raw` answers `()` (the console is always a raw tty) and `signal`
  accepts and ignores, which is what lush's interactive entry wants.
* **lush boots**, interactively and with the whole cat behind it: the prompt carries the cwd,
  builtins (cd, pwd, export, read ..) ride the rung-2 tree, and a bare `ls lib | wc -l` is
  rung 4's spawn over the registry — no `/bin`, no PATH, the verb table IS the path.
* *gate:* `make test_kboot` — four boots of the shipped x86_64 kernel through the PVH door,
  each `-append` a real command line: `kore ls lib`, `kore wc lib/json.l` byte-exact against
  the host `wc`, `sh -c "cd lib; pwd"`, and a pipeline. Opt-in (a cold cat eval per boot);
  run it when the kernel or the cat moves. vi is the interactive smoke under `run-*`, and its
  boot is proven headless — `-append "vi lib/json.l"` draws the hued file over serial. The
  aarch64 twin dispatches the same way through its DTB door (spot-proven; the gate lane is
  x86_64's).

### rung 4 — pipes, `spawn`, `wait`  ✅ landed

A pipe is a `k_source` pair over one byte queue in the kernel heap: the read end answers **0**
while a writer is open and **-1** when the last one closes, which is exactly what the scheduler
parks on. `dup`/`dup2` are row aliases; `fdopen` opens love's own end of the plumbing.

`spawn` on inle is a love-side shim (the boot text) over the core task ops: map argv onto a love
main — `kore-main` (or a tool's own `<name>-main` where the dispatcher is not baked), `sh-main`,
or a `.l` path off the ramfs, evaled form by form — and `twirl` it. **The pid IS the task pid**
and `wait` is `catch`.

* ⚠ **`doc/misc/posix.md` says "tasks are not processes — never cross them." On inle they are the same
  thing.** That is not a shortcut, it is the machine's whole character, and this is the one place
  it gets written down. The host keeps both; inle has one, and the shared name means kore's
  `proc.l` and lush's pipelines move unedited.
* ⚠ **Redirection lives UNDER the ports, and had to.** Compiled code FOLDS the global
  `in`/`out`/`err` at its own compile (proved by probe: rebinding moves nothing), so a pipeline
  stage cannot be redirected by any love-level rebind. The remap is the kernel's **seat table**:
  pid-keyed fd 0/1/2 → real rows, read by every fd dispatcher (`k_fd_eff`). `procseat` registers
  it **in the parent right after `twirl`** — which does not switch tasks, so the seat is laid
  before the child's first read — and each seated fd is a **dup**, fork's fd-copy made explicit,
  so the parent may `fdclose` its own pipe ends at once.
* ⚠ **`quit` is the process's exit door, seat-aware.** A seated task's `(quit n)` closes its
  seated fds (the write end's close IS the downstream EOF), retires the seat, and lands the task
  dormant with n as its retval — the love-machine `_exit`, and what `wait` reads. Every program
  exit funnels there: the shim's wrapper quits the main's answer, its help quits a scare, and the
  kore mains' own folded `(quit 0)` arrives on its own feet. Unseated it still resets the shipped
  machine; the TEST kernel's unseated arm answers the code (kore0.l's identity, one door deeper).
  `err` grew its own boot row (fd 2) so a seat can tell a stage's out from its scare face.
* ⚠ **A seated reader parks wearing its port's fd** (the folded stdin is fd 0), and by wake time
  the asker is not the running task — so `ai_ready(0)` sweeps every seat's read slot and takes
  the false wake: the woken reader re-asks through its own seat and re-parks. Seats are pipeline
  stages, a handful; a spurious wake costs one re-read.
* ⚠ **The queue GROWS rather than refusing at a cap.** The writer's lane is the static port's
  unbuffered `zputc`, whose contract on a busy answer is one retry and then a DROPPED byte — a
  bounded ring would shed bytes in silence under exactly the load it exists for. The price rides
  the same open question as the ramfs's memory ceiling (below). And with no SIGPIPE on this
  machine, a write on a widowed pipe answers "gone" and the run drops — a `yes | head` spins.
* Job control degrades honestly: no process groups, no `tcsetpgrp`, so pg/fg arguments are
  accepted and ignored, and `^Z` has nothing to stop. `dup` of a ramfs fd clones the handle, so
  the offset diverges where POSIX shares it (nothing seeks a saved fd yet); `ai_stdin`'s ungetc
  slot is one word all tasks share, so two stages parsing their stdin at once can cross-talk; a
  `freeze`d process task leaks its seat (quit never runs). Known, small, and said here.
* ⚠ **Every twirl must be caught** (CLAUDE.md's corpus law) — doubly here: an orphan stalls the
  kernel runner and the failure reads as a hang.
* *gate:* `test/kernel/pipe.l` — the pair, EOF at the last close, dup/dup2, spawn/wait, the seat
  driven bare — then lush itself: the engine parts bake into the K_TEST corpus (sh0.l pins what
  they mention and the seat lacks) and `test/kernel/sh.l` runs `kore ls lib | kore wc -l` through
  `sh-line`, the tail redirected onto the tree and read back. `test_kboot` grew the same pipeline
  as a fourth boot of the SHIPPED kernel through `sh -c`.

### rung 5 — the disk  ✅ landed

PCI config-space enumeration (CF8/CFC), then **virtio-blk** — modern virtio-pci on x86_64,
virtio-mmio on aarch64 (qemu virt's 32 fixed slots), one split virtqueue, polled, synchronous,
all in `src/blk.c` (~250 lines, the one part that had to be C). Over it three nifs —
`(disk _)` the sector count, `(disk-read l n)`, `(disk-write l s)` — and over those **FAT32
r/w written in love**: `lib/fat.l`, which rides the ramfs into every kernel via the module
walk, zero registration. The fs is device-parameterized (a dev is `(rd wr nsec)`), so the same
module runs on the virtio disk, on a cask in host tests, and on anything else that answers
sectors — mkfs, mount, ls/stat/read/write/mkdir/rm, LFN both directions, presence on the
`(1 _)` wrapper throughout.

* ⚠ **A polled driver must SUPPRESS the completion interrupt** (`VRING_AVAIL_F_NO_INTERRUPT` +
  PCI INTx-disable). Without it the INTx lands on an unhandled vector stub and the machine
  resets in SILENCE, timed exactly like a hang in the poll loop — the rung's one real bug.
* ⚠ **DMA addresses are `va - khhdm`, which holds only for heap memory.** The ring lives in a
  kmallocw block, data rides the love string's own bytes (nothing allocates between post and
  completion, so the collector cannot move the buffer), and image statics are barred — their
  physical address is not `va - khhdm`.
* ⚠ **Every door's map stops at 4 GiB, and OVMF parks 64-bit BARs above it.** The driver skips
  such a disk with a word (the honest fallback); the uefi test lane turns OVMF's 64-bit MMIO
  window off (`-fw_cfg opt/ovmf/X-PciMmio64Mb,string=0`) so its BAR lands reachable. qemu
  virt's virtio-mmio slots default LEGACY; the lanes pass `force-legacy=false` for version 2.
* ⚠ **FAT32 only, by the format's own law:** under 65525 clusters the type flips to FAT16 by
  definition, so `fat-mkfs` refuses a device under ~33 MB — absence, not divergence.
* **The disk does NOT mount under the ramfs paths** (the plan's one dropped clause): `open` is
  a C nif and the fs is love, so a C row cannot call it. The disk speaks through the module's
  own verbs — `(fat-mount (fat-disk ()))` — which is what kore-level tooling can wrap later.
* Metal still wants AHCI or NVMe — the same fs over a driver 3–4× the size, a later rung.
* *gate:* `test/host/fat.l` — the fs laws over a cask dev plus **mtools interop** (mdir/mtype
  read what we format and write; we read what mcopy writes) — `test/kernel/disk.l` (the raw
  door + fat on the real disk, both arches, all three x86 boot doors, guarded on `(disk ())`),
  and `test_disk`: two boots over one fresh image — write, RESET, read back — the second boot
  must print `disk: fat kept across the reset` (in `test_slow`).

### rung 6 — preemption  (~1–2 weeks)

Already fully scoped in ``, down to the field: **the timer must not switch tasks — it
sets a flag the next `YieldCheck` honors.** Switching in the ISR is barred three ways over (the
snapshot allocates, `g` is coherent only at Pack/Unpack, ring mutations are two steps).

⚠ The scheduler change is the small half. Latency is bounded by safepoint *distance*, so the real
work is auditing unbounded primitives and chunking them on the bignum pattern.

## what this is not, and when that stops being enough

No protection, no multi-user, no network, no foreign binaries. Three things would each force the
jump to real processes — page tables, a syscall ABI, a `g` per address space, ELF loading, signal
delivery, fork-with-COW — call it **6–12 months** on top of the ladder above:

* **running foreign binaries.** We can *build* them (mooncc, holo, our own linker), so this is a
  loader question, not a toolchain one.
* **isolation from our own C.** A task that faults in C takes the machine down today, and no
  amount of love-level safety changes that.
* **more than one user.**

If none of those is the goal, rung 6 is the end of the road and the machine is finished.

## the expensive things nobody budgets

* **PS/2 does not exist on modern metal.** `kb_int` decodes scancodes; real keyboards are USB HID,
  and a USB stack is weeks. Legacy emulation covers some machines and no laptop.
* **crash consistency** is a different animal from a filesystem that reads. Rung 5 buys the
  second one.
* **TCP is an arc, not a rung.** virtio-net is a week; the stack above it is months unless ported.
* **a `g` per process** — if real processes ever land, each one is a whole heap. The egg makes the
  boot cheap (0.03 s baked against 1.10 s cold); the memory floor is the open question.

## can it run doom  ✅ yes, opt-in

`make run DOOM=1` boots the machine with doomgeneric linked in and `doom 0` at the console
starts it: the title screen, the menus, and E1M1 on the framebuffer. It is **opt-in and in no
default build** — the source is not ours and not in this tree, so the lane wants
`dl/doomgeneric` and `dl/doom1.wad` and is otherwise absent (test_cts's posture). src/doom.c
is the glue, ~120 lines, and the whole of what it needed:

* **the compiler was the question, and it answered.** mooncc compiles all 83 translation units
  of doomgeneric and links them with our own linker, on the host and into the kernel alike.
  **Nothing foreign is in the build**: 83 DOOM + 27 MOON objects, the WAD blob, the runtime
  slices, the vector lay and the projection — and not one `CC` line in the log. The subtlety
  worth naming is `out/host/love0`, the bootstrap that RUNS mooncc: a bare `make` builds it
  with the ambient cc, because a bare make has no love (Makefile's own note). It is not in the
  artifact — but the claim is only airtight if it need not be, so it was checked:
  `make CC='out/host/love mooncc' love0` builds the bootstrap with our own compiler, and the
  kernel above was then rebuilt from it. gcc is nowhere in that chain.
  Three real bugs came out of the port, all fixed with gates:
  * a **block-scope `extern` declaration did not name the file-scope object** — it bound a
    local and the body read a slot the linker never wrote. doom's `d_net.c` says
    `extern boolean advancedemo;` inside a function, so `if (advancedemo)` tested garbage and
    the demo loop advanced every tic; a started game was cancelled within a tic of starting.
    ⚠ this is the WORST class the tree has: a silent wrong answer over a construct that reads
    like nothing. test/cc/154-blockextern.c.
  * a **float constant through a cast to an integer type** would not fold
    (doc/misc/moon-c-gaps.md; the parse half was answering WRONG, not refusing).
  * nolibc's **printf dropped the precision on `%d`**, so `"%.3d"` of 33 read `33` — which is
    how doom asks for the lump name `STCFN033` (test/libc/fmt.c had precision rows for `%s`
    alone).
* **three doors, and they existed.** `k_fb` hands over the framebuffer whole, `k_scan_arm` /
  `k_scan_pop` are the SCANCODE tap beside the ascii queue (a game wants make and break, where
  the line editor wants a byte), and `k_clock_ms` was already milliseconds.
* **the WAD is a baked file.** `k_baked` is kmain.c's hook for an object that wants a file in
  the tree: tools/mkblob.l lays the 4 MB IWAD into .rodata and doom's own `fopen`/`fseek`/
  `fread` reach it through src/sys.c with nothing mounted. That door is not doom's — it is
  the general one, and this is its first taker.
* ⚠ **the ESP door only.** `qemu -kernel` hands over no framebuffer, so `run-sh` cannot show
  it; `make run DOOM=1` (UEFI) is the lane.
* ⚠ **quitting doom resets the machine**, because doom's exit IS `(quit)` and rung 3 says a
  quit resets. Honest rather than fixed.
* **not finished:** the blit is a per-pixel loop into the GOP framebuffer with no double
  buffer, so a screenshot can catch a frame mid-copy (it costs ~4 ms of a ~260 fps loop, so
  the frame rate is not what wants fixing — the tear is). No sound: there is no audio door on
  this machine at all, and an AC'97 twin of src/blk.c is what one would cost.

## what is cheaper than it looks, and why

Worth stating, because it is the reason this ladder is weeks and not years:

* **there are no `.S` files.** `mkboot.l` lays the bring-up and `mkvec.l` the interrupt tail, so
  new assembly is a lay change in love.
* **the assembler, linker and compiler are ours.** An ELF loader, if it is ever wanted, is reading
  our own writer.
* **there is no global state in C** (CLAUDE.md's hard rule): `g` is a parameter everywhere. Two
  VMs in one image are already legal by construction — most kernels cannot say that.
* **the userland exists.** kore is 45 tools, lush is a real shell, vi edits, cook builds. That is
  the part that usually costs years, and it is behind us.

## open

* **the initrd's shape** — the baked table is what rung 0 built, and it costs a rebuild per
  change. A real archive format we could also write from the host is still open, and costs a
  reader.
* ~~**fd numbering.**~~ Answered: lowest free row at or past the boot three, POSIX's rule, which
  scripts lean on. A row is free when it carries no method at all — what `k_source_open` zeroes a
  fresh one to and what the ramfs close door puts one back to.
* **the ramfs's memory ceiling.** `g->budget` bounds the collector at RAM/8; a ramfs growing
  through the same heap competes with it, and nothing prices that yet. Rung 0 made this real
  rather than hypothetical, and rung 2 widened it: a write or a create is now the thing that can
  take memory the collector was counting on. Rung 4 added the pipe queues, which grow the same
  way (and for a reason — the growth is what keeps the unbuffered write lane from dropping
  bytes).
* ~~**whether lush wants a `/bin` at all**~~ Answered: no. The verb registry is the path —
  `k-prog` resolves a spawned word through `(from 'verbs 'word)`, so every applet the cat
  pinned is a program by name. lush's own in-image lane stays refused here (it asks whether
  PATH's winner is this binary, and there is no PATH), which is what leaves every stage a
  real task.
