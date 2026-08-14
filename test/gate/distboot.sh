#!/bin/sh
# test/gate/distboot.sh -- the release claim, stated over the artifacts.
#
# THE CLAIM: take either artifact, type `make`, get the SAME binary. SOURCE bootstraps
# through whatever C compiler the machine has; SEED carries its own source, is its own
# toolchain, and touches no ambient compiler at all. Both answer the same bytes.
#
# ⚠ WHY THAT IS EVEN POSSIBLE, and it is not something we engineered for this gate:
# the local cc builds `love0` and NOTHING else (host/build.mk). Every object in the
# shipped binary is mooncc's, compiled by love0 waking mooncc0.image. The bootstrap
# compiler is a scaffold that leaves no trace in the product -- which is the same
# property test_fixpoint asserts within one tree, and whose DDC leg (a foreign
# love0) was audited 2026-07-27. This gate says it ACROSS the artifacts, which is the
# form a person downloading them can care about.
#
# ⚠ THE ARTIFACTS ARE COMPARED TO EACH OTHER, not to the in-tree binary. That is the
# claim as stated, and it is also the only form that survives a dirty tree: a release
# is cut from the INDEX and the in-tree binary is built from the WORKING TREE, so on
# an uncommitted change those two legitimately differ. The in-tree comparison is made
# too, but only when git says the tree is clean.
#
# ⚠ AND THE SEED LANE POISONS THE COMPILER. A gate that merely observes the build
# succeed cannot tell whether the bundled love did the work or the ambient gcc quietly
# did it: both produce a working binary. So cc/gcc/clang are shadowed by scripts that
# fail loudly, and the build has to come out the far side anyway.
#
# ⚠ THE SEED CARRIES ITS OWN SOURCE. It holds the source tarball in .rodata
# (tools/mksrc.l, host/src.c) and `love source` lays it out with bin/love already
# inside, so one downloaded file needs no tar and no second fetch. "It unpacked
# something" is not the claim -- the tree it lays has to build, compilers poisoned.
#
# ⚠ AND THE CIRCLE IS THE WHOLE CLAIM. The seed rebuilds ITSELF from the source it
# laid, byte for byte -- so it carries everything it was made from and nothing of the
# machine that made it. That leg only became possible once a bake stopped writing the
# baker's ASLR base and hatch time into the image (test_bakerep guards the same law
# cheaply, in the slow gate, so a regression does not wait for a release).
#
# ⚠ THERE WAS A THIRD ARTIFACT, retired 2026-08-13: a FULL tarball, the source tree
# with a baked love in bin/. The seed does that job strictly better -- one file, and
# nothing needed to unpack it -- so its leg here was a third bootstrap proving what
# the seed's already proves.
#
# Two complete bootstraps and a self-rebuild -- minutes, not seconds. Opt-in, by name.
# usage: distboot.sh SOURCE_TGZ SEED_EXE [REFERENCE_LOVE]
set -u

src=$1
seed=$2
ref=${3:-}

[ -f "$src" ] || { echo "distboot: no $src -- run 'make dist'"; exit 1; }
[ -x "$seed" ] || { echo "distboot: no $seed -- run 'make dist'"; exit 1; }
command -v make >/dev/null 2>&1 || { echo "distboot: no make, skipped"; exit 0; }

R=$(pwd)
w=$(mktemp -d)
trap 'rm -rf "$w"' EXIT
fail() { echo "FAIL distboot: $*" >&2; exit 1; }

# our own extractor, so the gate leans on nothing it is not already testing
love=$R/out/host/love
[ -x "$love" ] || fail "no $love"

echo "distboot: two bootstraps and a self-rebuild, this takes a few minutes"

# ---- 1. SOURCE, through the machine's own compiler ---------------------------
mkdir -p "$w/lean"
"$love" "$R/tools/tgz.l" x "$src" "$w/lean" > /dev/null || fail "cannot unpack $src"
lean=$(echo "$w"/lean/love-*/)
[ -d "$lean" ] || fail "the source tarball unpacked no love-<ver>/ directory"
[ -f "$lean/VERSION" ] || fail "the source tarball carries no VERSION (the binary would stamp 'unknown')"
[ ! -e "$lean/.git" ] || fail "the source tarball shipped a .git"
( cd "$lean" && make -j"$(nproc 2>/dev/null || echo 4)" out/host/love ) > "$w/lean.log" 2>&1 \
  || { tail -20 "$w/lean.log"; fail "the source artifact does not build"; }
[ -x "$lean/out/host/love" ] || fail "the source build produced no love"
echo "  OK source: builds through the ambient cc"

# ---- 2. SEED, which needs no tarball at all and no compiler ------------------
mkdir -p "$w/nocc"
for c in cc gcc clang c99 tcc; do
  printf '#!/bin/sh\necho "distboot: the ambient %s was called -- the bundled love should have been the compiler" >&2\nexit 1\n' "$c" > "$w/nocc/$c"
  chmod +x "$w/nocc/$c"
done
# ⚠ run it from a COPY in the scratch dir. `love source` lays its tree beside the
# binary's cwd, and the tree we are testing must not land in the repo.
mkdir -p "$w/self"
cp "$seed" "$w/self/love" || fail "cannot copy $seed"
# ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads. The root Makefile EXPORTS it for the corpus,
# and an egg-booted love has no verb table at all -- `source` then reads as a FILENAME
# and the artifact answers "cannot open source", which looks like a missing verb
# rather than a missing image. The build lanes lead with the same thing for `mooncc`.
( cd "$w/self" && LOVE_NO_IMAGE= ./love source ) > "$w/self.log" 2>&1 \
  || { tail -20 "$w/self.log"; fail "the seed could not lay its source"; }
selfd=$(echo "$w"/self/love-*/)
[ -d "$selfd" ] || fail "'love source' unpacked no love-<ver>/ directory"
[ -f "$selfd/VERSION" ] || fail "the embedded source carries no VERSION"
# ⚠ bin/love is the whole point: without it the unpacked tree falls back to the
# ambient cc and the one-file claim quietly becomes a two-tool one.
[ -x "$selfd/bin/love" ] || fail "'love source' laid no runnable bin/love"
( cd "$selfd" && PATH="$w/nocc:$PATH" make -j"$(nproc 2>/dev/null || echo 4)" out/host/love ) \
  > "$w/selfb.log" 2>&1 \
  || { tail -20 "$w/selfb.log"; fail "the seed-laid tree does not build without an ambient compiler"; }
grep -q "was called" "$w/selfb.log" && { grep "was called" "$w/selfb.log" | head -3; fail "the seed-laid build reached for an ambient compiler"; }
echo "  OK seed: one binary lays its own source and builds it, no tar and no ambient cc"

# ---- 3. THE CIRCLE CLOSES: the artifact rebuilds ITSELF, to the byte ---------
# The chain whole: cut a tarball, bootstrap it, build the artifact, extract the source
# back OUT of the artifact, and rebuild -- and the second artifact is the first one's
# bytes. That is a stronger claim than "it builds": it says the artifact carries
# everything it was made from and nothing about the machine it was made on leaked in.
#
# ⚠ IT NEEDS A REPRODUCIBLE BAKE, and that is the only reason this leg can exist. An
# image used to carry the baker's ASLR base (raw kept absolutes, the header's address
# pair, a dead JIT husk's W^X pointer) and `born`, the hatch duration -- so two bakes of
# one tree differed by 180012 bytes and no artifact could ever equal another.
# ⚠ and the archive rides ALONG: `love source` lays the very bytes it carried, because
# an extracted tree has no .git and cannot re-cut one. Same blob in, same binary out.
( cd "$selfd" && PATH="$w/nocc:$PATH" make -j"$(nproc 2>/dev/null || echo 4)" dist ) \
  > "$w/selfd.log" 2>&1 \
  || { tail -20 "$w/selfd.log"; fail "the seed-laid tree cannot rebuild the artifact"; }
grep -q "was called" "$w/selfd.log" && { grep "was called" "$w/selfd.log" | head -3; fail "the artifact rebuild reached for an ambient compiler"; }
again=$selfd/out/dist/love-$(uname -m)
[ -f "$again" ] || fail "the artifact rebuild produced no $again"
if cmp -s "$seed" "$again"; then
  echo "  OK circle: the artifact rebuilds ITSELF byte-for-byte ($(sha256sum < "$again" | cut -c1-16)..)"
else
  ls -l "$seed" "$again"
  fail "the rebuilt artifact differs from the one that laid its source ($(cmp -l "$seed" "$again" 2>/dev/null | wc -l) bytes)"
fi

# ---- 4. THE CLAIM ------------------------------------------------------------
if cmp -s "$lean/out/host/love" "$selfd/out/host/love"; then
  echo "  OK both artifacts answer the SAME binary ($(wc -c < "$lean/out/host/love") bytes)"
else
  ls -l "$lean/out/host/love" "$selfd/out/host/love"
  fail "source and seed built DIFFERENT binaries -- the release claim is false"
fi

# ..and against the tree they were cut from, when the tree is one thing (see the
# header: a dirty tree legitimately differs, because the tarballs come from the index)
if [ -n "$ref" ] && [ -f "$ref" ]; then
  if [ -z "$(git -C "$R" status --porcelain 2>/dev/null)" ]; then
    cmp -s "$ref" "$lean/out/host/love" \
      && echo "  OK and identical to the in-tree binary" \
      || fail "the artifacts differ from the in-tree binary on a CLEAN tree"
  else
    echo "  (tree is dirty -- skipping the in-tree comparison, see the header)"
  fi
fi

echo "distboot: two artifacts, one love -- and the seed rebuilds itself to the byte -- ok"
