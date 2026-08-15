# dist — what a release is

A release is **two artifacts**, and they differ on the one question that matters to
somebody who just downloaded one: *do you have a C toolchain?*

| | | |
|---|---|---|
| **source** | `love-<ver>.tar.gz` | sources only. `make` bootstraps through the machine's own cc. |
| **seed** | `love-<arch>` | one executable that carries its own source and **is** its own toolchain. |

With the source tarball: unpack, `make`, `make install` — the social contract every C
project has used since the 1980s, and the reason to prefer it is not nostalgia, it is
that nobody has to learn anything.

With the seed: download one file, and

```sh
./love-x86_64 source     # lays love-<ver>/ with bin/love already inside
cd love-<ver> && make    # calls no ambient compiler at all
```

Both answer **the same binary**. There is no installer, no package manager, and no
download-that-downloads-more.

## why the two can answer the same bytes

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
2026-07-27. `make test_distboot` states it across the artifacts instead, which is the
form a person downloading them can care about.

## the circle

The seed's stronger claim, and the one that took a reproducible bake to make sayable:

```
cut source → bootstrap it on ambient cc → build the seed
   → extract the source back OUT of the seed → rebuild → the same bytes
```

with `cc`/`gcc`/`clang` poisoned for that last leg. The seed carries everything it was
made from, and nothing of the machine that made it. `test_distboot`'s fourth leg is
exactly this; `test_bakerep` guards the reproducible bake underneath it cheaply enough
to ride the slow gate.

⚠ **the archive rides along.** `love source` lays the very bytes the seed carried,
because an extracted tree has no `.git` and cannot re-cut one. Same blob in, same
binary out — reusing them is what makes the rebuild byte-identical rather than merely
equivalent.

⚠ **the seed is per-ISA.** "Bootstraps anywhere" means anywhere of that architecture:
x86_64, aarch64, riscv64 are three seeds. `make xa=<arch> dist_cross` bakes any of them,
so one x86 laptop can cut the pi's download.

**`love seed <arch> [DIR]`** is that door held by the artifact rather than the Makefile —
we are a cross compiler carrying our own source, so a seed can lay a seed for a machine
it is not. ⚠ **no fixpoint there, and none is owed.** The check can hold only where the
output ought to *be* this binary, and a cross lay is the one case the invocation itself
says it cannot; running it would be a claim nothing could satisfy. What is still checked
is the artifact's *shape* — its ELF `e_machine` must be the arch asked for, which catches
a cross build quietly laying the host's. The trust is derived rather than absent: the
binary doing the cross build is the one plain `love seed` proves natively, and the real
check for the output is `love seed` on the machine it is for.

⚠ and the OTHER case the fixpoint cannot hold — a **dirty tree** — still FAILS. Nobody
asked for it, and the megabyte-scale mismatch is the report. The rule is *skip the check
when the invocation named the reason, never when it was discovered.*

⚠ the cross bake needs **qemu-user** for that arch (the twin's own heap is warmed by
running it). A missing one is a loud build failure, not a silently unbaked seed.

## the recipes

```
make dist-source        # the tarball
make dist-seed          # the one-file artifact for this arch
make dist               # both — a release
make xa=aarch64 dist_cross   # a seed for another arch (roster: crew/build.mk)
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

## what the seed retired

There used to be a third artifact: a **full** tarball, `love-<ver>-<arch>.tar.gz`, the
source tree with a baked `bin/love` laid beside it. The seed does that job strictly
better — one file instead of an archive, nothing needed to unpack it, and the same
bytes at the far end — so the full tarball became a second way of saying what the seed
already says, and a third bootstrap to keep honest in every release gate. Retired
2026-08-13.

## the traps this design walks into

⚠ **VERSION.** `love_version.h` is generated from `git describe`, and an extracted
tarball has no `.git` — so it fell back to `"unknown"`, and that string is compiled
into `love.o`. The artifacts would have differed by exactly one word, which is the
sort of thing that makes a headline claim quietly false. The stage writes a `VERSION`
file and `mk/lib.mk` reads it **when there is no VCS directory**, so a development
checkout is unaffected and a stray VERSION can never shadow a real revision.

⚠ **`CC ?=` cannot express "unless the user chose one".** make defines `CC=cc`
itself, so `?=` never fires and the ambient compiler wins silently. `$(origin CC)`
is the only way to ask whether a *human* set it. The Makefile uses that to let a
bundled `bin/love` be the compiler, and an explicit `CC=` still outranks it — which
is exactly what the source artifact is for, and what the DDC leg needs.

⚠ **a release is cut from the INDEX.** `git checkout-index` is the stage, which is
what makes an artifact reproducible from a revision — and it means an uncommitted edit
is *not* in what you just built. The stage says so when the worktree and index
disagree, because a gate run against a stale artifact is silent and looks exactly like
the fix not working.

And the gate earns its keep by **poisoning the compiler**: the seed lane builds with
`cc`/`gcc`/`clang` shadowed by scripts that fail loudly. Without that, a passing build
cannot distinguish "the bundled love did the work" from "gcc quietly did it" — both
produce a working binary.

## install

`make install` lays the runnable crew on PATH. The installed tree and its original
tarball live under `~/.love/` (beside `~/.love/etc/`, which is where salt already
reads configuration from) — keeping the archive means there is always a pristine
baseline to re-extract and to diff a local tree against.

⚠ **keep the install-owned copy separate from a development checkout.** If one
directory is both, an install fights your working tree.

Related: `mk/lib.mk` (the version stamp), `crew/build.mk` (the recipes),
`test/gate/distboot.sh` (the claim) (what builds the packages).
