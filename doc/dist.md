# dist — what a release is

A release is **two artifacts**, and they differ on the one question that matters to
somebody who just downloaded one: *do you have a C toolchain?*

| | | |
|---|---|---|
| **source** | `love-<ver>.tar.gz` | sources only. `make` bootstraps through the machine's own cc. |
| **seed** | `love` | one executable that carries its own source and **is** its own toolchain. |

The seed is not a separate build: it **is** the tree's own `out/host/love`, baked —
the default binary links its source blob and readme, so `make` produces `love0`
(the bootstrap scaffold) and the seed, and nothing else.

With the source tarball: unpack, `make`, `make install` — the social contract every C
project has used since the 1980s, and the reason to prefer it is not nostalgia, it is
that nobody has to learn anything.

With the seed: download one file, and

```sh
./love source            # lays love-<ver>/ with bin/love already inside
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
and a re-cut answers the same bytes: `mk/tools/selfpack.l` is the one cutter in every
world — the tree on disk, sorted, mtimes pinned — so same tree in, same blob out,
same binary out.

⚠ **the seed's bytes are the tree's, not the builder's.** Whatever machine runs
the build, the same tree answers the same bytes (the seed-universal invariant,
proven both directions on real silicon) — so platform-specific builds are
meaningless and the `love-<arch>` names dissolved with rung U2: there is one
binary, `love`. (`mooncc -t` keeps its cross targets — `make test_xfixpoint`
proves the bytes do not depend on the arch mooncc runs on — but no second
artifact is built from them.)

**`love seed [DIR]`** is the circle held by the artifact rather than the
Makefile: lay the source, rebuild, and check the rebuilt seed IS this binary,
byte for byte.

## the recipes

```
make dist-source        # the tarball
make dist-seed          # the seed: out/host/love, baked
make dist               # both — a release
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

⚠ **VERSION.** The checked-in `./VERSION` is the **whole** id: `love_version.h` is
generated from it and nothing else, the tarball is named for it, and no version
control is consulted anywhere — an id with a VCS suffix would make the artifact's
bytes depend on something outside the tree, which is exactly what the seed invariant
forbids. It moves when a release does, by hand.

⚠ **`CC ?=` cannot express "unless the user chose one".** make defines `CC=cc`
itself, so `?=` never fires and the ambient compiler wins silently. `$(origin CC)`
is the only way to ask whether a *human* set it. The Makefile uses that to let a
bundled `bin/love` be the compiler, and an explicit `CC=` still outranks it — which
is exactly what the source artifact is for, and what the DDC leg needs.

⚠ **a release is cut from the TREE.** `selfpack` walks the root and skips only what
is not source (`out bin dl`, everything hidden at the root, and `wasm/love.js`), so
every file on disk — tracked or not — is in the artifact, and the edit you just made
is in what you just built. The cut runs every make and settles on its own stamp (the
sha of the leveled tar, kept beside the archive), so a deleted or renamed file —
which leaves no mtime for make to watch — still re-cuts, and a touch does not.

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
