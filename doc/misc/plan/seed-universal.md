# plan: a universal seed binary

**THE INVARIANT: the same byte-identical fixpoint binary, built in every
environment.** Per target, one byte string — whatever machine, arch, or OS did
the building. Every rung below either proves the invariant somewhere new or
removes a reason it could fail; an environment joins the roster only by
producing the same bytes. Standing evidence, 2026-08-16: love-aarch64 built on
bee (x86_64) == built on pi (aarch64), real silicon both ends; the x86_64
mirror leg and the OS dimension climb toward the same bar. What the bar has
extracted so far: the image renames its serials canonical at save (one live
heap, one byte string — a dead task pid and its +1 nom ripple were the two
machines' whole difference), and exec-bound forks drop the heap pools from
the child's inheritance (a swapless box refuses to double-charge a
budget-sized commitment; the seed's own make died ENOMEM before one recipe
ran). Open door: the seed PARENT holds its boot-fat heap (inflate scratch +
budget-grown pools) through the whole build it merely waits on — a small box
under build pressure oom-kills the waiter and the verdict line dies with it
(the build and the fixpoint survive, orphaned). The parent should SHED before
herald: a lean collect + pool release, or the check re-execed thin.

One seed that runs on every platform and answers the same fixpoint everywhere —
the cosmocc shape. First, what the tree actually holds: no mention of APE,
cosmopolitan, polyglot or fat binaries anywhere in 4,124 commits; the seed is
**per-ISA by stated design** (doc/misc/dist.md: three seeds, one per arch); Linux is
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
that bypasses the selfpath ladder everyone else uses; kore uname's /proc/sys reads (fallbacks exist). pid1,
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
- **rung 4 — the mechanisms.** BEGUN 2026-08-17, by need — the whole love was
  compiled -os freebsd and the link named its debts: sysctl (nolibc grew the
  member + header; selfpath's KERN_PROC_PATHNAME reads it), the termios fork
  (freebsd's 44-byte struct, no c_line; ISIG/ICANON/IEXTEN/IXON/VMIN/VTIME
  and the flush/flow selectors part company — termios.h forks on the OS), and
  the tty family's freebsd bodies (TIOCGETA/SETA+act, TIOCSPGRP, posix_openpt
  the real syscall 504, unlockpt a no-op by pts(4), ptsname over FIODGNAME).
  With those, THE WHOLE LOVE LINKS AND RUNS on the 14.4 box — the -e lane,
  say, and the stdin repl answer (test_freebsd's new leg holds it). Still
  owed: sigfd over EVFILT_SIGNAL; the
  socket constants + sa_len (compiled with linux values today — the net lanes
  are UNTESTED on freebsd); memfd/unshare/mount stay linux behind stubs.
  The "-e race" RESOLVED 2026-08-17 — never a race: freebsd's kernel hands
  the arg vector base in %rdi (its own crt1 reads argc at `(%rdi)`) and rsp
  is only aligned NEAR it, sometimes with a pad word below argc, so a crt0
  reading argc off [rsp] saw argc=0 by stack address — love then ran the
  stdin lane and exited 0 at EOF (the piped-repl legs were only accidentally
  deterministic: the stdin lane read the same piped program). crt0-fbsd
  reads %rdi now, and the gate runs -e eight times to hold argv whole on
  every exec.
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
- **rung U1 — the container.** THE RULING (gwen, 2026-08-17): the strong
  reading is chosen. There are not multiple builds of love — no x86_64 binary
  and aarch64 binary. THE artifact carries the text for every platform and the
  OS compatibility layer, and one byte string answers the fixpoint on every
  arch. That was the initial bar for `love seed`; U1 is the container that
  makes the one file execute everywhere. Its own ladder:
  - **U1.0 — evidence before architecture.** Landed 2026-08-17: a one-block
    sh prefix (picks by `uname -m`, dd's the page-aligned member out once,
    execs the cache) + the baked native seed + the x-lane twin = 14.0 MB, and
    the ONE file answered `(quit 7)` and the tower on bee and on pi, scp'd
    as-is. What it taught: the pi pays a 7.6 s egg boot EVERY run (the twin
    ships no image), so U1.2's first-boot bake is load-bearing, not polish;
    the x-lane's sys.o recipe had rotted dark (ambient mksys names, moved to
    module 'moon by the modules arc — fixed, the recipe takes the module
    door); and the halves must share the source blob or the fat pays it
    twice.
  - **U1.1 — the format.** Landed 2026-08-17: tools/fatpack.l lays the
    container (page-aligned members behind a one-block `#!/bin/sh` prefix; the
    kernel execs that from ANY caller, where the APE-style bare-word prefix
    only survives shell/execvp fallback), the cache is content-named under
    ~/.love/fat (the members' sha rides the prefix, so a new fat lands a new
    cache and the adopt holds), the x-lane grew the twin SEED (its own src.o
    + readme — both halves answer the same 3.2 MB blob), and `make dist-fat`
    + `make test_fat` (opt-in: prefix + cache + byte-determinism + the
    foreign member under qemu) hold it. Deferred: the blob rides once per
    half today — the shared-member dedup (~3 MB) waits on a directory walk in
    the src lookup (U1.1b); no env hand-back of the fat path yet (nothing
    reads it until U1.3's re-cut). And the pi named U1.2's price exactly: an
    egg twin serves NO VERBS (`love source` on the pi read a file named
    "source") — verb dispatch is the baked binary's behavior.
  - **U1 PARKED (gwen, 2026-08-17), and why.** The exec question has exactly
    three doors on every ELF OS (Linux and all four BSDs check one e_machine
    before any byte of ours runs; FatELF proposed kernel-side fat in 2009 and
    upstream refused; only Mach-O kernels pick an arch): a #! coat, per-box
    binfmt registration, or explicit lay. The coat's run shapes were explored
    to the end — cache (rejected: nothing touches ~/.love), self-assimilation
    (rejected: a file that overwrites itself is anti-user), and the ephemeral
    hatch (an anonymous inode, execve via /proc/self/fd, no residue) which is
    SOUND but needs the members baked — and duplicating two ~8 MB images
    whose heap is one arch-free program is not acceptable: it makes the
    global fixpoint ugly, and the point of the fixpoint is that it is
    beautiful. So the container waits on either the ARCH-NEUTRAL IMAGE
    (symbolic refsyms over a name-sorted lvm table, per-arch glaze annexes;
    the fat then costs ~1 MB of text per platform over ONE image) — or on
    inle: OUR kernel's exec loader accepts the fat format we actually want
    to write, the door the free kernels refused. fatpack/dist-fat/test_fat
    stay in the tree as the working evidence (opt-in, no default path).
  - **NEXT, ruled: the OS dimension** — claim (b), the decoupling ladder's
    rungs 4-5, toward one text that runs on every kernel of its arch.
- **rung UV — MULTI-OS PER ISA** (ruled on the table, 2026-08-17): one binary
  per arch that runs on linux AND freebsd, then netbsd. The wall that parked
  U1 has no OS twin — proven the day it was ruled: the tree's own love with
  e_ident[EI_OSABI]=9 runs UNCHANGED on linux (the linux loader never reads
  the byte) and the freebsd box EXECS the same bytes (it dies at the first
  linux-numbered syscall — the loader said yes). One byte string satisfies
  both kernels; only the syscall layer is owed. The ladder:
  - **UV1 — one syscall door.** LANDED 2026-08-17. The x64 mksys tail is
    OS-BLIND: CF cleared going in (linux hands it back untouched through
    r11), a carry answer — freebsd's +errno — parked below linux's band as
    -(errno+4096); impl.h's __ai_call unparks, so the tail never learns
    which kernel answered. os.c probes once (syscall 20: getpid on freebsd
    answers a pid, writev(-1,NULL,0) on linux answers -EBADF) and the
    canonical lane translates numbers (NR_fb_* now unconditional in impl.h,
    the -os freebsd lane aliases it, os.c's map pairs it) and errnos (the
    0..97 row). The sigprocmask leaves branch on __ai_osv between 14 and
    340; crt0 serves both kernels (zero rdi says linux); crt0-fbsd and the
    separate freebsd mksys lane RETIRED (mksys-freebsd survives as an
    alias). ⚠ the map's POLICY: a pair rides only when the call's shape
    agrees on both kernels — fstat/sigaction/sigprocmask/ioctl/getdents/
    getcwd/mount/sendfile/pselect6 are OMITTED (ENOSYS, loud) until UV2
    lands each body; mapped-but-untranslated VALUES (open/mmap/fcntl flags,
    clockids, diverging signal numbers, sa_len) are UV2's rows. Gate:
    test_freebsd's UV1 leg — one default-lane binary, branded 9 by dd,
    answers identical text and status on both kernels (argc, sigsetjmp
    round trip, EBADF, kill, exit).
  - **UV2 — the compat members.** LANDED 2026-08-17, and it went FURTHER
    than planned: the freebsd-valued HEADER forks retired entirely (fcntl/
    signal/mman/stat/dirent/termios/errno are canonical, linux-valued, on
    every lane), so each member wears ONE body with a runtime branch on
    __ai_osv — the `-os freebsd` lane collapsed into "canonical + brand".
    What translates: the stat trio and readdir (the __fb_stat/__fb_dirent
    twins; readdir repacks into the DIR's own slot), sigaction (freebsd's
    ksigaction, SA rows, and __ai_sigshim so a handler sees the CANONICAL
    signal number), sigprocmask (how+1, the 16-byte set, bit-by-bit mask
    translation), kill (⚠ freebsd 17 IS canonical SIGCHLD and means
    SIGSTOP there — os.c's permutation, STKFLT/PWR refuse), waitpid (the
    signal inside the status), open/mmap/fcntl (flag rows, record locks
    reordered), clock_gettime (MONOTONIC is 4 there), select's 6th arg,
    termios WHOLE (four flag-word tables + the cc index permutation;
    exotic locals do not round-trip, the named surface does), ioctl (known
    requests translate, unknown refuse loudly), the pty quartet, rmdir/
    utimensat (AT bits, UTIME specials -1/-2), madvise (DONTFORK no-ops
    rather than firing freebsd's PROTECT), sysctl (runtime-guarded, always
    linked — host_selfpath's ladder tries /proc then the sysctl door, so
    the try IS the OS probe; ⚠ the glibc bootstrap lane guards it out,
    glibc dropped the symbol). mount/sendfile and linux's own mechanisms
    stay off the map: ENOSYS, loudly. Gate: test_freebsd's UV2 leg runs
    the ENTIRE rung2 battery from one default-lane binary, byte-identical
    text and status on both kernels. Standing evidence past the gate: the
    tree's own out/host/love, branded by one dd byte, ran the repl and
    `love source` ON THE BOX — the tree laid whole, bin/love copied in by
    the runtime selfpath, and the laid love answered. The socket FAMILY
    LANDED 2026-08-18: sockaddr heads rebuilt through __ai_sain/__ai_saout
    (the BSD length byte where linux's 16-bit family sits; a unix name's
    length follows its path — freebsd refuses linux's whole 110), inet6
    moves 10→28, SOCK_* flag bits move high, SOL_SOCKET moves whole with
    its names permuted (only what sys/socket.h spells has a row), msg
    flags map, sendmsg/recvmsg refuse loudly on fb (msghdr/cmsg layouts
    differ; nothing in the tree speaks them). Gate: the UV-net leg —
    loopback TCP, UDP with the peer's translated head, a unix pair, one
    binary, byte-identical text both kernels. ⚠ still linux-only: sigfd →
    kqueue (signalfd answers ENOSYS on fb; lush's job control leans on
    it). ⚠ a 2 GB box OOM-kills mooncc's love.o compile at the default
    budget (half box RAM in WORDS ≈ 4x RSS): seed runs there want
    LOVE_BUDGET_MB set small.
  - **UV3 — one binary, both boxes.** BEGUN 2026-08-17: every x64 static
    exe is now BORN branded EI_OSABI=9 (the linux loader never reads the
    byte, freebsd's imgact refuses without it) — the on-box seed run forced
    the ruling: the whole build succeeded on freebsd and then its own
    freshly-linked love failed exec, unbranded. out/host/love needs no dd
    anywhere now. LANDED 2026-08-18: the gate's `-os freebsd` build legs
    are DELETED (`-os` stays a mooncc cross dimension for foreign C, and
    the OS left love's artifact space: per-ISA bytes, every kernel); the
    born brand is asserted, not dd'd; and the TROPHY IS GATED — the
    FBSD_SEED=1 leg runs `love seed` on the box and greps its own
    fixpoint-ok (the seed compares the rebuild against the running exe's
    bytes, so the box answering ok IS the tree's bytes; budget-invariance
    makes the box's LOVE_BUDGET_MB=512 economics immaterial). ⚠ small-box
    seed economics: a 2 GB box swap-kills the parent
    waiter + the interpreted holo step even at LOVE_BUDGET_MB=128 — the
    waiter-sheds-its-heap door (top of this doc) is now load-bearing.
    **THE CROSS-KERNEL TROPHY, 2026-08-17**: the seed built the seed ON
    FREEBSD — the whole ladder (ambient cc → love0 → mooncc → every object
    → link → bake) ran on the box from the one binary, and the bytes it
    answered are IDENTICAL to the linux build's at matched budget: linux/
    ext4 and freebsd/ufs produce ONE byte string from one tree. Two finds
    on the way: lush's glob answered raw readdir order (now sorted — the
    posix law, and readdir's order is the FILESYSTEM'S), and the bug this
    hunt isolated — **LOVE_BUDGET_MB leaked into the bake** — is FIXED
    2026-08-17. Three leaks, all of them the collector's TIMING written
    into bytes: the intern map's slot order was its insertion history
    (probing settles collisions by arrival, majors re-arrive in old slot
    order); a layered bake's FROZEN intern backing was abandoned by the
    first post-freeze major mid-mutation, pinning both the moment's slots
    and every since-freeze atom as ballast; and a weak drop + re-intern
    handed a name a fresh serial at a GC-chosen moment. The cures, in the
    codec and the collector: the dump re-inserts the live intern pairs in
    SPELLING order (img_canon_symbols); a freeze moves the intern map off
    the pin the instant it sets one, so the copy it abandons holds
    exactly what the record took (img_rehome_symbols); and the serial
    rename ranks mints in session order then named noms by spelling
    (img_rank_assign) — one shared heapsort (img_sort) serves all three
    orders plus the dictionary. Gated: test_bakerep grew the layered
    egg-bake budget lane (default vs LOVE_BUDGET_MB=128, byte-compare);
    a 64..2048 sweep answered identical.
  - **UV4 — netbsd.** LANDED 2026-08-18 — the one x64 binary answers a THIRD
    kernel. What it took, smaller than feared because the BSDs agree so much:
    ldlink lays the .note.netbsd.ident PT_NOTE + alloc section in every
    hosted x64 link (netbsd's exec REFUSES noteless binaries; foreign
    kernels ignore notes; strip keeps an alloc section); the probe grew a
    second question (sys 20 says BSD, kern.ostype's first byte parts the
    two); os_nr grew a third column (the classic band matches freebsd
    NUMBER FOR NUMBER — only the versioned calls differ: __fstat50 440,
    __getdents30 390, __wait450 449, __sigprocmask14 293 ..); signals, SA
    flags, masks, wait status, tty ioctls and termios (one CRTSCTS bit)
    reuse the freebsd tables VERBATIM; errno parts company only past 84.
    netbsd's own: the classic PAD (lseek/pread/pwrite/ftruncate slide an
    arg; mmap's 7th rides the stack through mksys's __ai_sys7 wide door),
    the stat/dirent/sigaction shapes, O_DIRECTORY/O_CLOEXEC/AF_INET6/
    MSG_NOSIGNAL values, and the userland sigtramp — the kernel calls the
    handler directly and provides NO return path, so sigaction registers
    mksys's __ai_nb_sigtramp (mov r15->rdi; setcontext) via
    __sigaction_sigtramp version 2. the ENTRY needed nothing: rdi arrives 0
    and [rsp] is a long argc, so the dual crt0's linux door already fit.
    Two ambushes: netbsd's AuxInfo is {u32 type, PAD, u64 value} with the
    pad UNZEROED, so auxv a_type reads through its low word everywhere
    (dl_iterate_phdr found no phdrs and the dump's guard refused
    everything); and netbsd loads a PIE near ZERO, so every binary pointer
    sat under the codec's lane floor — a low absolute now passes when the
    wake-safety guard vouches for it (the encoding was anchor-relative all
    along), and only an unaudited dump keeps the floor. Gate:
    test/gate/osbox.sh is ONE script for both boxes (test_freebsd /
    test_netbsd; FBSD_SSH / NBSD_SSH; *_SEED=1 for the trophy leg); the
    conjuring recipe for the netbsd box (the -live.img.gz, QMP send-key
    through the VGA console) rides its header. **THE TROPHY, THIRD KERNEL,
    2026-08-18 (NBSD_SEED=1, gated)**: the seed built the seed ON NETBSD —
    linux/ext4, freebsd/ufs and netbsd/ffs answer ONE byte string from one
    tree. still open on netbsd: the pty quartet (TIOCPTSNAME is another
    shape) and sendmsg/recvmsg (refused loudly, both BSDs).
  - **UV-sig — sigfd rides kqueue. LANDED 2026-08-18.** The last inert
    stub with a live consumer (init's perceive parks on sigfd; lush's job
    control never called it — wait/signal/still, already translated).
    nolibc grows the pair: sys/event.h speaks freebsd's record and
    negative filters as the canon; netbsd repacks to __kevent50's 40
    bytes, filter = -canon - 1 (an involution). EVFILT_SIGNAL is the one
    filter whose ident is a signal number, so it alone rides the
    permutation — canonical numbers both directions; a no-twin signal
    (STKFLT, PWR) refuses EINVAL. posix.c's sigfd falls to a kqueue on
    signalfd's ENOSYS behind the same port; sigtake there names no sender
    (pid 0 — a 'chld consumer loops glean anyway). The load-bearing fact,
    probed on both boxes before a line was written: a BLOCKED signal
    still fires EVFILT_SIGNAL (it hooks the send, before the mask), so
    the sigfd contract — block, queue, take — holds verbatim; and poll
    sees a pending kqueue fd, so the scheduler's await merge just works.
    Gate: osbox.sh's UV-sig leg (signalfd, or its ENOSYS falling to
    kqueue — same text both kernels) and the trophy leg's love-level
    sigkq.l (the pending take AND the parked take, on the box).
  - **UV-far — the last two doors, full parity. LANDED 2026-08-18.**
    sendmsg/recvmsg stop refusing: both BSDs share one msghdr/cmsghdr
    shape, and the probe showed the cmsg DATA offset (16) and CMSG_LEN
    values AGREE with the canon — so the translation rewrites only the
    heads (int-wide lengths, level moved with SOL_SOCKET; under it only
    SCM_RIGHTS is spelled, an unmapped type refuses). control rides a
    256-byte scratch, a bigger one refuses loudly; recvmsg folds the
    name through saout, the flags back through msgcan. And the pty
    quartet answers netbsd: grantpt = TIOCGRANTPT, unlockpt a no-op
    like the other kernels, ptsname = TIOCPTSNAME filling a ptmget
    (2056 bytes, the slave name at 1032) — three kernels, three
    ptsname shapes, one member. Gate: uvnet grew the SCM_RIGHTS round
    (a pipe end crosses the unix pair and still writes) and a UV-pty
    leg (quartet, isatty, tcgetattr, a line through the pair). With
    this every refusal left in the BSD lanes is inherent (STKFLT/PWR
    have no twin; memfd/unshare/mount are linux's own words).
- **rung U2 — the targets DISSOLVE.** Decided 2026-08-16 (chosen, revisable);
  Landed 2026-08-17: ONE binary. The host build is subsumed — out/host/love
  links the source blob + readme and, baked, IS the artifact (`make` in a clean
  tree produces love0 and the seed, nothing else); love-x86_64/love-aarch64
  and the dist_cross twin are gone as products (`dist-seed` is the tree's
  binary; the x-lane objects remain only for test_xfixpoint); `love seed`
  takes no arch; and a git-less tree re-cuts its archive from itself
  (tools/selfpack.l, gated by distboot's binary compare). The seed's bytes
  are the tree's, never the builder's — that is the standing invariant. The
  full statement:
  once the invariant holds per target, love-x86_64 and love-aarch64 stop being
  products — ONE `love`, one byte string, every machine. The invariant is what
  makes this well-defined: each lane's bytes are already machine-independent
  (proven both directions on real silicon), so the union artifact is too — any
  box assembles the same fat file, because every part it packs is the part
  every other box would pack. `mooncc -t` keeps its targets; it is the
  ARTIFACT names that go. Two tensions the design must answer:
  (1) **the self-bake mutates the file** — a universal artifact's distributed
  bytes must stay immutable, so the per-arch images either all ride the file
  (N bakes, qemu for the foreign ones at build time — the current dist_cross
  law generalized), or the bake moves out-of-file (a sidecar under ~/.love,
  first-boot warm) for the universal lane;
  (2) **a builder today needs qemu-user for foreign bakes** — either that
  stays a build-time-only tool (the precedent dist_cross set), or the bake
  becomes an emulation-free function of the tree. The fallback doors if fat
  disappoints: the carried interpreter, or the wasm backend
  ([moon-wasm](moon-wasm.md)) as the one ISA. U1's polyglot container is the
  EXECUTES-everywhere half; this rung is the RUNS-NATIVE-everywhere half.

## choices (revisable)

- read "every platform" as *every hosted Linux ISA, then one BSD* until someone
  names a platform they actually need; Windows and macOS are out of scope words
  until then.
- read "identical fixpoint" as *one canonical byte string for all targets* —
  the strong reading, RULED 2026-08-17 (rung U1's header); the weaker
  per-target reading was the scaffold and U0 retired it.
- the per-ISA claim in doc/misc/dist.md stays true until the rung that falsifies it
  lands; this plan does not pre-rewrite the docs.

## difficulty

Highest of the four arcs, and the only one whose full statement may not be worth
its price — it is three arcs wearing one name. But the decoupling ladder is owed
regardless and is bounded by the chokepoint design the tree already got right
(one trampoline, one decode point, one-function-per-file libc, two `#if`
files), U0 is pure gates, and the plan's value is mostly in forcing the
readings apart before anyone spends a month on the wrong one.
