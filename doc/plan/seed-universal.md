# plan: a universal seed binary

One seed that runs on every platform and answers the same fixpoint everywhere —
the cosmocc shape. First, what the tree actually holds: no mention of APE,
cosmopolitan, polyglot or fat binaries anywhere in 4,124 commits; the seed is
**per-ISA by stated design** (doc/dist.md: three seeds, one per arch); Linux is
**compiled in, not detected** (cpp.l predefines `__linux__` unconditionally;
impl.h's 154 syscall numbers fork on arch only, zero OS gates); macOS does not
build at all in any lane (image.c needs `<link.h>` + `dl_iterate_phdr`); and the
fixpoint gate runs on x86-64 only. So this arc is not one claim, it is three,
and they cost wildly different amounts:

- **(a)** one file that *executes* everywhere — the polyglot container trick;
- **(b)** code that *runs* on every OS — per-OS syscalls behind `__ai_sys`;
- **(c)** *identical bytes* whatever machine produced it — the fixpoint claim.

What the tree already has in its favour: the linker is ours (ELF header fully in
hand), a working PE32+ emitter exists, the libc is ours and one-function-per-file
(OS-forking it is mechanical), all OS traffic passes one chokepoint (`__ai_sys`,
bytes laid in one file, mksys.l), static PIE self-relocates on all three arches
with no `ld`, and the seed already carries its own userland (`src-farm`'s verb
symlink farm). The "one downloaded file needs nothing on the box" half of the
cosmocc story is already true — just Linux-and-one-ISA-shaped.

## the walls, named

- **the heap image.** `.love_image` is 7.1 MB of the 11.8 MB artifact,
  arch-stamped, anchor-stamped, meaningful only inside the exact binary that
  baked it — and the bake is a *run* of the binary (dist_cross warms a foreign
  twin under qemu-user). Every universality strategy hits this first: N images,
  an arch-neutral image format, or egg-boot (~230 ms and the glaze bake lost).
- **bake_tail's layout contract.** `.love_image` must end the highest PT_LOAD;
  the bake self-rewrites in place. A polyglot prefix or self-assimilating loader
  perturbs exactly this.
- **macOS.** No Mach-O emitter, image.c is Linux-only, and Apple silicon makes a
  self-patching binary re-sign itself. This is possibly a refusal, not a rung.
- **claim (c) across ISAs has no meaning yet even at home:** nothing anywhere
  compares machine A's artifact to machine B's, and test_fixpoint skips off
  x86-64.

## the linux surface, measured

Smaller than feared. **Already portable:** the scheduler waits on plain poll
(no epoll/futex/timerfd/eventfd anywhere in shipped code); no threads, no
thread-local storage, no vdso use; subprocess is fork+execvp+waitpid; sockets
are BSD sockets; and exactly two C files test `__linux__` — three blocks in
host/posix.c, each with a working `#else` stub. Everything else is Linux by
*content*, not by `#if`. **Mechanical tables:** 77 invoked syscalls per arch,
all behind the one `__ai_sys` trampoline and one decode point (`er()`'s -4096
negative-errno law, impl.h); the O_*/MAP_*/SO_*/SA_* flag values and the errno
tail; signal numbers — ⚠ which leak into love source, lush's job.l and init.l
spell Linux's 17 for SIGCHLD; the ioctl `_IOC` encoding; and the struct layouts
(stat, dirent-as-the-getdents64-record, termios' `c_line`, sockaddr without
`sa_len`, addrinfo's field order). **Structural, each with a named landing:**
signalfd → kqueue's EVFILT_SIGNAL behind the same sigfd/sigtake nif shape
(non-Linux already degrades to inert stubs, so the seam exists); SA_RESTORER/
rt_sigreturn simply drop on BSD (the kernel lays its own trampoline) but
sigsetjmp's inlined mask ABI changes; image.c's bake walk — dl_iterate_phdr
(nolibc already grows its own off auxv) plus a hard `readlink("/proc/self/exe")`
that bypasses the selfpath ladder everyone else uses; splice.l's
/proc/self/maps parse; kore uname's /proc/sys reads (fallbacks exist). pid1,
mount and namespaces stay Linux-only behind their existing ENOSYS stubs — a
distro concern, not the artifact's.

⚠ the failure mode is silence: a lost predefine compiles posix.c's features
OUT, loudly nowhere — the wart cpp.l's old comment recorded. The decoupling
owes a roster gate: on linux, assert the linux features are aboard.

## the decoupling ladder (owed regardless)

- **rung 0 — the OS is a named dimension.** Landed 2026-08-16: the `__linux__`
  family moved from cpp.l's unconditional predefine table to the driver
  (moon.l's osdefs, `-os linux|none`, default linux — same law as the arch
  predefines), and `-ffreestanding` pins `-os none` (a later explicit `-os`
  wins), so the kernel and the boards lose a `__linux__` they never asked for.
- **rung 1 — no stray absolutes.** Landed 2026-08-16: the self-bake reopens the
  binary through the selfpath ladder instead of a bare `/proc/self/exe`
  readlink, and test/host/fs.l probes the roster (the real mount answers the
  call's errno, the stub answers ENOSYS — a lost predefine now fails a gate;
  sigfd's twin assert already lived in test/host/sh.l).
- **rung 2 — nolibc grows the OS axis.** Landed AND GATED 2026-08-16:
  test_freebsd (the FBSD_SSH door, a qemu/KVM FreeBSD 14.4 box) ran a
  mooncc-laid static freebsd/amd64 binary — tables, trampoline, er()'s one
  law, crt0-fbsd, the EI_OSABI brand, sigsetjmp round trip. The ride found two
  real things: freebsd's syscall exit ZEROES scratch registers (linux does
  not — siglongjmp's val/buf moved to callee-saved), and ld-write takes a
  piece list, not a byte string. The shape: impl.h opens on the OS before the
  arch — freebsd's block is one machine-independent table off stable/14's
  syscall.h, a name it does NOT define is a mechanism that differs (clone,
  dup3, getdents64, memfd, signalfd4, unshare), so a member pulled early fails
  by name; mksys-freebsd lays the x64 machine wearing freebsd's kernel (the
  CF+errno answer normalized to -errno in the trampoline, so er() keeps one
  law; sigprocmask 340 with the 16-byte set in buf[8..9]; no restorer — the
  sigret leaf is surface parity only); the driver takes `-os freebsd`
  (predefine `__FreeBSD__=14`) for compiles and REFUSES the link (no crt0, no
  ELF brand — rung 5's).
- **rung 3 — the tables fork.** Landed and gated 2026-08-16, same box: the
  value tables open on the OS ahead of the arch (O_*/AT_*/MAP_*/SA_*, the
  parting signal numbers, the errno tail — the kernels agree through 10 and
  part at 11 — the ino64 stat/dirent shapes, freebsd's flock), and the
  mechanism members got bodies: fork(2) real, dup2 via F_DUP2FD, readdir over
  getdirentries, sigaction translated to the kernel shape (no restorer),
  sigprocmask's 16-byte set, pselect's bare sigset arg, isatty by TIOCGETA.
  getcwd needed nothing — its body only reads the sign. Still absent by `#if`
  (rung 4, gates need a tty and a wire): termios proper, the pty family,
  mount/sendfile, signalfd (kqueue), the socket constants + `sa_len`. The
  signal-number leak into job.l/init.l waits for a love runtime on freebsd
  (rung 5) to mean anything.
- **rung 4 — the mechanisms.** sigfd over EVFILT_SIGNAL; splice's maps read
  over the sysctl; selfpath's OpenBSD gap if anyone cares.
- **rung 5 — the artifact whole.** bake/wake on the foreign OS (the phdr walk
  off auxv, EI_OSABI/.note.ABI-tag), then the fixpoint gate runs there.

## the universality ladder (above it)

- **rung U0 — evidence before architecture.** Landed 2026-08-16, two gates:
  test_fixpoint now runs on any seed arch (the x86-64 guard opens, the mksys
  leaf forks on the host — and the gate had been dark since the core/ move,
  missing -Icore, which is its own argument for U0). test_xfixpoint runs the
  cross-machine claim in effigy: dist_cross's twin objects link love1 (this
  machine's bytes for the other arch), love1 under qemu-user rebuilds itself
  natively and must answer the same bytes — one cmp proves the twin machine
  reproduces this machine's, and that mooncc's output does not depend on the
  arch mooncc runs on. The literal leg rides a real aarch64 box (pi.lan): the
  shipped twin runs `love seed` there and its own sha256 check is the
  two-machine compare — which found the twin was NOT a seed (dist_cross
  linked no source blob; fixed, the twin link now mirrors the native one).
  Still owed: a native riscv64 ride of test_fixpoint.
- **rung U1 — the container.** The polyglot prefix over our own linker: sh +
  ELF is the cheap pair; PE re-uses pe.l's reloc machinery; Mach-O is new and
  waits. The prefix must respect bake_tail or the bake learns to re-lay it.
- **rung U2 — one file, many ISAs.** Three doors, undecided on purpose: a fat
  binary (N .text + N images — the per-arch object separation in crew/build.mk
  exists precisely because objects must not mix); a carried interpreter (the
  seed's real product is the *tree* — a tiny portable interpreter that can run
  `love seed` is a much smaller universality claim than a universal vm binary);
  or the wasm backend ([moon-wasm](moon-wasm.md)) as the one ISA that runs
  everywhere. Decide after U0–U1 and decoupling rung 2 have taught what the
  fixpoint can even mean across ISAs.

## choices (revisable)

- read "every platform" as *every hosted Linux ISA, then one BSD* until someone
  names a platform they actually need; Windows and macOS are out of scope words
  until then.
- read "identical fixpoint" as *any machine reproduces the per-target bytes*
  (rung 0), not *one canonical byte string for all targets* — the strong reading
  waits on the rung-3 decision, and may be declined.
- the per-ISA claim in doc/dist.md stays true until the rung that falsifies it
  lands; this plan does not pre-rewrite the docs.

## difficulty

Highest of the four arcs, and the only one whose full statement may not be worth
its price — it is three arcs wearing one name. But the decoupling ladder is owed
regardless and is bounded by the chokepoint design the tree already got right
(one trampoline, one decode point, one-function-per-file libc, two `#if`
files), U0 is pure gates, and the plan's value is mostly in forcing the
readings apart before anyone spends a month on the wrong one.
