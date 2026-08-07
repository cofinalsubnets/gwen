# inle — the love machine

The plan for turning the bare-metal kernel (`port/inle/`) from a REPL that boots into a machine
that runs the userland we already have: kore, lush, vi, cook, mooncc.

**The decision it rests on: one address space, one `g`, no protection.** Every program here is a
love program in a VM we wrote, not an ELF binary — so a process boundary would buy fault
isolation from bugs in our own C and nothing else. That is a **love machine**, and it should be
called that rather than Unix. What it borrows from Unix is the *surface* — `doc/posix.md`'s L2,
the same nif names the host answers, so the userland moves without an edit.

Where it stops being enough is named at the foot, with what it would cost.

## where inle is today

`kmain.c` boots (three doors: `-kernel`, UEFI, limine), lays a heap over the memory map, draws a
framebuffer console through quay, decodes PS/2 scancodes, and runs `((from 'bao 'shell) 0)`.
Interrupts, the timer, cooperative tasks (`twirl`/`catch`), and fd-parking all work.

It also has a filesystem now — the rung-0 ramfs over a `.rodata` initrd, with `open` and `close`
in `defs[]` and `use` resolving `lib/<x>.l` off it. `k_sources[]` grows a row per open file.

Missing: storage (no PCI, no block driver), the rest of the file nifs (`readdir` `stat` `lseek`),
a wall clock, processes, network.

## the shape it grows into

Three mappings, all of them already half-built:

* **`k_sources[]` is the vfs.** A per-fd vtable (`readn`/`writen`/`putc`/`flush`/`ready`/`close` +
  `state`), and `k_source_open` is the one door in, growing the table through `g->alloc`. Every
  rung below adds rows.
* **ports are the fds.** `ai_io_alloc(g, fd)` is core, not host — it wraps an fd as a port with a
  close finalizer, and read/write come free.
* **tasks are the processes.** `twirl` answers a pid, `catch` waits on one, and the scheduler
  already parks a task on an fd and wakes the ready one (`doc/sched.md`).

⚠ **The kernel links no `host/*.c`** — `k_shared_c` is love.c + am.c + quay + libc only. The
`AI_NIF` section glob is the host's trick; a kernel nif is a row in `kmain.c`'s `defs[]` table,
handed to `ai_defn`. Every nif below is written fresh against the vfs, not `#ifdef`'d out of
`host/posix.c`.

⚠ **The conventions are `doc/posix.md`'s, exactly.** An effect answers `()` | a POSITIVE errno |
EINVAL on misuse; a value answers the value | `()`. `stat` answers `(size mtime-ms mode ns)`.
Divergence here is worse than absence — kore reads these shapes and a wrong one is silent.

## the ladder

Sizes are one focused person, rough, and they compound: each rung is gated before the next.

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

### rung 1 — the file nifs, and `use` lights up for free  (~1 week)

`readdir` `stat` `lseek` — `open`, `close` and the `ai_fd_close` routing came with rung 0, and
with them `use` off the ramfs. `salt` (`~/.love/etc/<app>.l`) is presence-gated the same way and
wants only a `HOME` to answer.

* **The wall clock lands here**, because `stat` needs an mtime. `date_at_boot` is *already
  requested* from limine (`kmain.c:78`) and dropped on the floor; wire it plus `kticks` into
  `ai_clock`'s epoch and every mtime and build grade starts meaning something.
* ⚠ `readn`'s contract is the one to honor (`doc/io.md`): **>0 bytes, 0 = nothing waiting, -1 =
  the end.** A file at EOF answers -1; a pipe with a live writer answers 0. Confusing them is
  what makes the scheduler spin.
* *gate:* `test_kernel` per nif; the corpus's `io.l` rejoins the kernel set (`kt` in kernel.mk
  drops it today for exactly this reason).

### rung 2 — a writable tree  (~1 week)

`mkdir` `rmdir` `unlink` `rename` `chdir` `cwd` `environ` `getenv` `setenv`. The cwd is a kernel
string; the environment is a tablet. Nothing hard here — it is the rung that makes the ramfs a
filesystem rather than a read-only bundle.

* *gate:* kore's fs tools (`ls cp mv rm mkdir touch pwd`) smoke on the K_TEST kernel.

### rung 3 — kore and lush boot  (~1 week)

Bake the `$(korefiles)` cat as a rodata source and dispatch off the program seat of `cmdline`,
which is how `out/host/kore` already works. Then lush.

* ⚠ **There is no shebang lane on inle.** `kore TOOL ARGS` is a love call into the registry
  tablet, not an exec — the multi-call trick is doing all the work, and it is why kore was the
  right thing to build first.
* *gate:* `kore ls`, `kore wc`, `vi` on a ramfs file, under `run-*`.

### rung 4 — pipes, `spawn`, `wait`  (~1 week)

A pipe is a `k_source` pair over a ring buffer in the kernel heap: the read end answers **0**
while a writer is open and **-1** when the last one closes, which is exactly what the scheduler
parks on. `dup`/`dup2` are row aliases.

`spawn` on inle is a love-side shim over the core task ops: read the path, load it into a fresh
layer, `twirl` it. **The pid IS the task pid** and `wait` is `catch`.

* ⚠ **`doc/posix.md` says "tasks are not processes — never cross them." On inle they are the same
  thing.** That is not a shortcut, it is the machine's whole character, and this is the one place
  it gets written down. The host keeps both; inle has one, and the shared name means kore's
  `proc.l` and lush's pipelines move unedited.
* ⚠ Job control degrades honestly: no process groups, no `tcsetpgrp`, so `spawnio`'s pg/fg
  arguments are accepted and ignored, and `^Z` has nothing to stop. Say so in the refusal rather
  than pretending a job is backgrounded.
* ⚠ **Every twirl must be caught** (CLAUDE.md's corpus law) — doubly here: an orphan stalls the
  kernel runner and the failure reads as a hang.
* *gate:* a lush pipeline — `kore ls | kore wc -l` — on the K_TEST kernel.

### rung 5 — the disk  (~2–4 weeks)

PCI config-space enumeration, then **virtio-blk** (the cheap real driver; a virtqueue and a
handful of MMIO writes). Then a filesystem over it — **FAT32 r/w**, so the machine can read and
write the same ESP it booted from (today `uefi/loader.c` reads `love.elf` off it through the
firmware's own boot services, and we carry no FAT code of our own). The ramfs stays as the root
and the disk mounts under it, so nothing above this rung changes.

* ⚠ **Write the filesystem in love, over a block port — not in C.** A bug in a love fs is a scare
  on the console; the same bug in kernel C is a triple fault with no output. The driver is the
  only part that must be C, and it is the small part.
* ⚠ Metal wants AHCI or NVMe instead, which is the same vfs with a driver 3–4× the size. Do
  virtio first and let the interface prove itself under qemu.
* *gate:* a `run-*` lane that writes a file, resets the machine, and reads it back.

### rung 6 — preemption  (~1–2 weeks)

Already fully scoped in `doc/sched.md`, down to the field: **the timer must not switch tasks — it
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
* ~~**fd numbering.**~~ Answered: lowest free row at or past the boot two, POSIX's rule, which
  scripts lean on. A row is free when it carries no method at all — what `k_source_open` zeroes a
  fresh one to and what the ramfs close door puts one back to.
* **the ramfs's memory ceiling.** `g->budget` bounds the collector at RAM/8; a ramfs growing
  through the same heap competes with it, and nothing prices that yet. Rung 0 makes this real
  rather than hypothetical: a write is now the one thing that can take memory the collector was
  counting on.
* **whether lush wants a `/bin` at all** on a machine where every program is a registry entry.
