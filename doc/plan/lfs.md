# plan: how much of Linux From Scratch is climbed

LFS is defined as *building the GNU sources*. We mostly did not climb it — we built a
parallel userland with the same job description. So the honest accounting runs on two
axes, and only one of them is LFS's own ladder:

- **A — native equivalents.** Our code doing the package's job (kore, lush, cook, moon,
  holo, `lib/gz.l`, `lib/tar.l`). This is where nearly all the distance is.
- **B — LFS packages built by our toolchain.** Six, with repeatable harnesses:
  `mk/tools/moon-{bzip2,gzip,lua,m4,sqlite,tar}.sh`, each with x86-64, arm64 and riscv
  lanes (`make moon-tar`, `make moon-tar-arm64`, …). ⚠ every one is **opt-in** — no tier
  runs them, so a green `test_slow` says nothing about them. Run them by name.

## chapters 5–6, the toolchain — climbed, and self-hosting

LFS spends two chapters here and calls it the hard part. It is the part we are done with.

| LFS | ours | state |
| --- | --- | --- |
| binutils | `crew/holo/` — as, ld, ar+ranlib, nm, objcopy | byte-identical smokes vs GNU/llvm |
| gcc | `crew/moon/` — mooncc, C11 freestanding | self-hosting, fixpoint-gated, x64 + arm64 + riscv |
| glibc | `crew/moon/lib/nolibc/` | by-need members, no host libc |
| linux-headers | `crew/moon/include/` | our own minimal set, not the host's |

Plus one rung LFS never attempts: `free/kernel.mk` builds a whole kernel with
`KCC ?= mooncc` and our own linker, on two arches, with nothing foreign left.

**The one structural hole: no C++.** Real gcc and binutils need a C++ compiler to build
themselves, so axis B cannot reach LFS's own chapters 5–6 at any effort short of a C++
front end. That is a different order of work from everything else on this page, and
naming it is most of what this section is for.

## chapters 7–8, the final system

Present natively — roughly **18 of ~85 chapter-8 packages**, several partial:

coreutils (`kore`, 85 tools / 88 names, GNU-byte-identical smokes, `make test_kore`) ·
bash (`lush`) · sed · grep · diffutils (diff, cmp) · make (`cook`) · tar (`lib/tar.l`,
ustar both ways, `love tar`) · gzip (`lib/gz.l`, and `lib/gzcmd.l` wears GNU's flags
as `love gzip` / `gunzip` / `zcat`) · cpio (`lib/cpio.l` newc, `love cpio`) · zlib ·
vim (`crew/vi`) · sysvinit
(`crew/init/boot.l` as `/init`) · openssl-ish (`crew/tls`) · nc (`mk/tools/ain.l`) ·
**patch** (`crew/kore/patch.l`, unified diffs).

The partials, stated: kore has no `df` (nothing here answers `statvfs`, so it wants a
nif and not an afternoon); sed is a deliberate subset (no hold space, no `\n` in
replacements); `expr` has no `-o` output template and `od` takes one `-t` per run; our
DEFLATE lands a few percent above `gzip -9` (it costs every block three ways and writes
the cheapest -- lib/gz.l carries the numbers), and `gzip -d` reads one member per file
where GNU reads a concatenation.

### what is absent, in the order it hurts

- **the ./configure tax** — perl, python, m4, autoconf, automake, libtool, bison, flex,
  gettext, pkg-config. Every real LFS package demands these *before* it compiles a line.
  This, not the compiler, is what axis B actually runs into.
- **bc** — the last of what was "the small four"; awk, find and patch are in.
- **the rest of the shell floor** — less, xz, bzip2, file.
- **the admin layer** — util-linux, shadow, procps, psmisc, e2fsprogs, kmod, iproute2, kbd.
- **docs** — groff, man-db, texinfo, ncurses, readline.

## chapters 9–10 — config partial, kernel imported

lush reads `/etc/profile` and `~/.profile`; libra owns `~/.love/etc`. The kernel is
still the one imported artifact — `mk/distro.mk` says `BZIMAGE ?= /boot/vmlinuz-linux`.
No GRUB.

The image is cut by **kore's `find`, `love cpio` and `love gzip`** — no host tool in the
pipeline — and `make distro-smoke` boots it. The wart this section carried for months
(a distro that exists to prove we need no host, built by one) is gone.

## the number

By chapter-8 package count: **~20%**. By "can the system rebuild itself from source with
nothing foreign underneath": **essentially all of it** — that is `make test_raw` plus the
mooncc fixpoint, and it is green.

The gap between those two numbers is entirely *other people's build systems*.

## the ladder

- **rung 0 — `find` and `awk` — BUILT** (`e015bf61`). Both are
  `crew/kore/` applets on the u-floor, both in `make test_kore`: awk 39 checks
  byte-identical to gawk plus a lawed pure floor, find 18 walks set-identical to the
  system find. awk is a POSIX awk (BEGIN/END, ranges, arrays, user functions with
  array parameters by reference, the builtins, `-F`/`-v`/`-f`) minus getline, output
  pipes and a non-newline RS — the three named in its header as rungs, not oversights.
  find is `-name -path -type -print -prune -exec` with the operators and the depths.
  Two things the work taught, both now comments in the tree: **`nil?` is a truth test,
  not a type one** (it answers 1 for `0` and `""` as readily as for `()`, which silently
  prints awk's `0` as empty), and **an integral double past a charm still owes its
  digits** — `int` overflows to 0 at 1e20, so the exact integer comes back through
  love's bigints, glibc-identical even at 1e23.
- **rung 1 — `patch`, and the coreutils stragglers — BUILT.** Thirteen applets: `patch`
  (`crew/kore/patch.l`, unified diffs, `-pN -R -i -o --dry-run -s`, offsets and rejects),
  `expr` (`crew/kore/expr.l`, its own file because `:` rides the BRE engine), `od paste
  comm join split` in core.l, `stat du chown mktemp` in fs.l, `date id` in proc.l. All in
  `make test_kore`, all GNU-byte-identical where GNU has an opinion, plus laws over the
  pure floors — the calendar, the record floor, the report floor, expr's, patch's.
  Three nifs grew with them (host/posix.c): **`stat`'s tuple gained
  `uid gid nlink blocks ino`** (append-only; the kernel's own stat still answers the
  first four, and the tail is asked by `tally`), a **`lstat`** beside it (du and stat owe
  the link's own blocks, not its target's), **`getgid`**, and `openfd` gained mode 3,
  O_EXCL at 0600, which is what makes `mktemp` a claim rather than a guess.
  Four things the work taught, all now comments in the tree:
  * **`two?` is false for a TEXT**, and the u-floor's option walk asked it of a glued
    value — so `grep -m2` read as a bare `-m` with nothing behind it and took the next
    word, the FILE, for the count. Silently, since the miss had somewhere to go. Fixed in
    `uopts` (ask `tally`), and every option walk written since asks the same way.
  * **the oracle for `patch` is the TREE, not the message.** GNU patch's chatter has moved
    between releases; what it leaves on disk has not.
  * ⚠ **a `< $ho/p.diff` inside a cd'd subshell opens AFTER the cd**, so a relative path
    hands the tool an empty stdin — and both sides then do nothing and match, which reads
    exactly like a pass. The gate spells `$ho` and `$m` absolutely now.
  * **GNU's `--apparent-size` counts a file's `st_size` and a directory's not at all** —
    an empty directory whose st_size is 40 reports 0. Read off the tool, not from the man
    page, and not from one filesystem: btrfs and tmpfs disagree about everything else here.
- **rung 1b — the second coreutils batch — BUILT.** Sixteen applets, every one pure over
  the u-floor and none of them wanting a nif: `tac` (core.l), the column tools `fold`
  `expand` `unexpand` over the shared `ucol`, the encodings `base64` `base32` (one coder,
  two alphabets), `tsort` and `factor`, `realpath` `link` `unlink` (fs.l), `printenv`
  `whoami` `groups` (proc.l) and `arch` `nproc` beside uname. All in `make test_kore`,
  GNU-byte-identical, laws over the new pure floors. `uwords` moved core-ward to u.l,
  where tsort and factor read it too. Three things the work taught, all comments in the
  tree now:
  * **`base64 -w 0` closes nothing** — GNU ends a WRAPPED last line with a newline and
    leaves one long line without one. The obvious coder is a byte too long.
  * **a tab lands only where it saves at least two columns**, so a lone space sitting on
    a tab stop stays a space. The rule unexpand is easiest to get wrong.
  * **the encodings are only smoked by a BINARY file** — text agrees under any bug that
    only mangles the high bit.
  Left deliberately: fmt, pr, csplit, ptx and numfmt (each its own layout language),
  dir/vdir (they are `ls -C`/`ls -l`), shuf (a seed decision first), the sha1/sha512
  family (host/hash.c carries three digests), and who/users/logname (no utmp).
- **rung 1c — gzip's face — BUILT.** `lib/gzcmd.l`: `love gzip`, `love gunzip` and
  `love zcat`, GNU's flag spelling (`-cdfklnNqrtv`, `-1..-9`, `-S SUF`, the long forms)
  over lib/gz.l's two doors, registered as verbs the way `love tar` is. The in-place
  replace carries the mode and the mtime; `-l`'s listing is byte-identical to GNU's,
  ratio and all. Gated in `make test_gz` (test/gate/targz.sh section 4). Three things
  worth knowing:
  * **`-l`'s ratio is the DEFLATE PAYLOAD's**, not the file's — GNU takes the header
    and the trailer out before dividing, and the tenth is ROUNDED where an older gzip
    truncated. A hundred random files were asked which.
  * **the suffix is asked before the bytes are**: a name with nothing to strip is a
    warning (exit 2), so `gunzip *` over a mixed directory walks on. Reading first
    calls every plain file "not in gzip format" instead, which is exit 1 and a stop.
  * **one member per file** — gz-unzip reads the trailer off the tail, so a legal
    `cat a.gz b.gz` is refused whole rather than half-read. That belongs in gz.l when
    something here needs it.
- **rung 2 — cpio, and the distro cuts itself — BUILT.** `lib/cpio.l` is the SVR4 newc
  wire (pack, unpack, scatter) over lib/tar.l's own entries — the walk that fills them
  is about a file and not about a format, which is why the second wire is short — and
  `lib/cpiocmd.l` is `love cpio` (`-o -i -t`, `-H newc`, `-d -u -v`, `-F/-I/-O`,
  `--quiet`, the block count). `mk/distro.mk` now cuts the initramfs with
  **kore's find, our cpio and our gzip**, and `make distro-smoke` boots that image
  under qemu: love is pid 1, /proc is mounted, the kore userland answers. The wart at
  the top of this page is closed. Gated by name in `make test_cpio` (GNU cpio both
  ways, the listings byte-identical, the flags, the image shape). Three things:
  * ⚠ **the stored name loses a leading `./`** — GNU cpio drops it where GNU tar keeps
    it, so two listings of the same tree disagree by two charms until you match it.
  * ⚠ **the mode field is the whole `st_mode`**, type bits and all, where tar keeps the
    kind in a typeflag byte. A newc header carrying permission bits alone extracts as a
    file of mode 0 and no kind, and the kernel's own reader takes it silently.
  * **hard links are not encoded** (newc says them with a shared ino and nlink > 1, the
    body on the last member); we write nlink 1 and a fresh ino, so a tree of hard links
    comes back as copies. Half of that job would be worse than none — a reader that
    believes nlink waits for a body that never comes.
  * ⚠ and the bug the boot found, which the packer had nothing to do with:
    **`lib/dns.l` has to ride into the initramfs**. mk/tools/ain.l is a korefiles member
    and probes for the `dial` nif at load, saying `(use 'dns)` when it is absent — which
    it is in love-raw. With no `/lib/dns.l` that scare takes the whole cat down, and the
    symptom is every applet gone rather than a quiet `nc`.
- **rung 3 — decide about the configure tax.** The genuine fork, and it is a decision,
  not a rung: grow perl/python/autotools, or keep declining them and only ever build
  packages that do not ask. Six packages so far have not asked. That is not an accident
  — the harnesses were chosen for it.
- **rung 4 — C++, or a permanent refusal.** Changes the shape of the whole page. Do not
  start here; rungs 0–2 are worth doing whichever way this goes.

## choices (revisable)

- **native equivalents over ported sources.** A tool we wrote is smoked byte-identical
  against GNU and is ours to keep; a tool we ported carries gnulib. The six package
  harnesses stay what they are — instruments for growing mooncc, not the userland's plan.
- **the ladder is not a race to 85.** Most of chapter 8 is there to build chapter 8.
  A tool earns its rung by unblocking something we actually run, which is why `find`
  and `awk` come first and `groff` may never come at all.
