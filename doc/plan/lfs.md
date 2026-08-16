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

Present natively — roughly **17 of ~85 chapter-8 packages**, several partial:

coreutils (`kore`, 46 tools / 49 names, GNU-byte-identical smokes, `make test_kore`) ·
bash (`lush`) · sed · grep · diffutils (diff, cmp) · make (`cook`) · tar (`lib/tar.l`,
ustar both ways) · gzip (`lib/gz.l`) · zlib · vim (`crew/vi`) · sysvinit
(`crew/init/boot.l` as `/init`) · openssl-ish (`crew/tls`) · nc (`mk/tools/ain.l`).

The partials, stated: kore has no `du df stat chown id date od expr split paste comm
join mktemp`; sed is a deliberate subset (no hold space, no `\n` in replacements); our
DEFLATE is fixed-Huffman only, about 24% behind `gzip -9`.

### what is absent, in the order it hurts

- **the ./configure tax** — perl, python, m4, autoconf, automake, libtool, bison, flex,
  gettext, pkg-config. Every real LFS package demands these *before* it compiles a line.
  This, not the compiler, is what axis B actually runs into.
- **the small four** — awk, find, patch, bc. Individually cheap, and `find`/`awk` are
  what a configure script actually executes.
- **the rest of the shell floor** — less, xz, bzip2, file.
- **the admin layer** — util-linux, shadow, procps, psmisc, e2fsprogs, kmod, iproute2, kbd.
- **docs** — groff, man-db, texinfo, ncurses, readline.

## chapters 9–10 — config partial, kernel imported

lush reads `/etc/profile` and `~/.profile`; libra owns `~/.love/etc`. The kernel is
still the one imported artifact — `mk/distro.mk` says `BZIMAGE ?= /boot/vmlinuz-linux`.
No GRUB.

⚠ and the wart worth naming: `distro-initramfs` cuts its image with the **host's** `find`,
`cpio` and `gzip -9`. The distro that exists to prove we need no host is built by one.

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
- **rung 1 — `patch`, and the coreutils stragglers.** `patch` has a reader already in the
  tree to learn from (`crew/sb/`); `stat du date od expr mktemp` are each an afternoon
  and each unblock a shell script somebody has already written.
- **rung 2 — cpio, and the distro cuts itself.** With `find` landed, replacing the
  host `find | cpio | gzip -9` pipeline closes the wart above. `lib/gz.l` already
  writes the gzip container; cpio's newc format is smaller than ustar.
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
