# dist — what a release is

A release is **two tarballs**, and they differ on the one question that matters to
somebody unpacking one: *do you have a C toolchain?*

| | |
|---|---|
| `love-<ver>.tar.gz` | **lean** — sources only. `make` bootstraps through the machine's own cc. |
| `love-<ver>-<arch>.tar.gz` | **full** — the same tree plus `bin/love`, a baked love for `<arch>`. `make` bootstraps through *that*, and no ambient compiler is called. |

Unpack either, type `make`, and you get **the same binary**. Then `make install`.

That is the whole distribution story. There is no installer, no package manager, no
download-and-then-download-more. The social contract is the one every C project has
used since the 1980s, and the reason to prefer it is not nostalgia — it is that
nobody has to learn anything.

## why the two artifacts can answer the same bytes

Not because we engineered it. Because of the shape the bootstrap already has:

```
local cc  ──builds──▶  love0  ──wakes──▶  mooncc0.image  ──compiles──▶  every shipped object
```

`$(CC)` builds **`love0` and nothing else** (host/build.mk). Every object in the
binary you end up running is mooncc's. The bootstrap compiler is a scaffold that
leaves no trace in the product — so which compiler held the scaffold cannot show in
the result.

That is the same property `test_fixpoint` asserts inside one tree (rebuild the
generation with itself, assert `love1 == love2` to the byte), and whose
diverse-double-compiling leg — a *foreign*-compiled love0 — was audited green on
2026-07-27. `make test_distboot` states it across the two artifacts instead, which
is the form a person downloading them can care about.

⚠ **the full artifact is per-ISA.** "Bootstraps anywhere" means anywhere of that
architecture: x86_64, aarch64, riscv64 are three artifacts. `dist_cross` bakes the
twin, so one x86 laptop can cut the pi's download.

## the recipes

```
make dist-src     # the lean tarball
make dist-full    # the full one for this arch
make dist-rel     # both — the usual cut
make test_distboot
```

The archive is **ours end to end** — `lib/tar.l` writes the ustar, `lib/gz.l` the
DEFLATE — so cutting a release needs neither `tar` nor `gzip` on the box.
⚠ our coder writes the fixed Huffman code only, ~24% above `gzip -9`
(lib/gz.l carries the measured numbers). That is a real cost on a download and the
reason a dynamic coder is the next rung.

**Reproducible by construction.** The pack pins every mtime/uid/gid to `dist_stamp`
(0 by default) and the gzip header's own MTIME is 0, so two cuts of one revision are
the same bytes and "this is that release" is something anyone can check with
`sha256sum`. File modes are *not* pinned — the executable bit is content, and a
binary that unpacks unrunnable is a broken artifact.

## the two traps this design walks into

⚠ **VERSION.** `love_version.h` is generated from `git describe`, and an extracted
tarball has no `.git` — so it fell back to `"unknown"`, and that string is compiled
into `love.o`. The two artifacts would have differed by exactly one word, which is
the sort of thing that makes a headline claim quietly false. The stage writes a
`VERSION` file and `mk/lib.mk` reads it **when there is no VCS directory**, so a
development checkout is unaffected and a stray VERSION can never shadow a real
revision.

⚠ **`CC ?=` cannot express "unless the user chose one".** make defines `CC=cc`
itself, so `?=` never fires and the ambient compiler wins silently. `$(origin CC)`
is the only way to ask whether a *human* set it. The Makefile uses that to let a
bundled `bin/love` be the compiler, and an explicit `CC=` still outranks it — which
is exactly what the lean artifact is for, and what the DDC leg needs.

And the gate earns its keep by **poisoning the compiler**: the full lane builds with
`cc`/`gcc`/`clang` shadowed by scripts that fail loudly. Without that, a passing
build cannot distinguish "the bundled love did the work" from "gcc quietly did it" —
both produce a working binary.

## install

`make install` lays the runnable crew on PATH. The installed tree and its original
tarball live under `~/.love/` (beside `~/.love/etc/`, which is where salt already
reads configuration from) — keeping the archive means there is always a pristine
baseline to re-extract and to diff a local tree against.

⚠ **keep the install-owned copy separate from a development checkout.** If one
directory is both, an install fights your working tree.

Related: `mk/lib.mk` (the version stamp), `crew/build.mk` (the recipes),
`test/gate/distboot.sh` (the claim), doc/moon-userland.md (what builds the packages).
