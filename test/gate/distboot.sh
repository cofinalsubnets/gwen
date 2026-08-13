#!/bin/sh
# test/gate/distboot.sh -- the release claim, stated over the artifacts.
#
# THE CLAIM: unpack either tarball, type `make`, get the SAME binary. The lean one
# bootstraps through whatever C compiler the machine has; the full one bootstraps
# through the `love` it ships and touches no ambient compiler at all. Both answer
# the same bytes.
#
# ⚠ WHY THAT IS EVEN POSSIBLE, and it is not something we engineered for this gate:
# the local cc builds `love0` and NOTHING else (host/build.mk). Every object in the
# shipped binary is mooncc's, compiled by love0 waking mooncc0.image. The bootstrap
# compiler is a scaffold that leaves no trace in the product -- which is the same
# property test_fixpoint asserts within one tree, and whose DDC leg (a foreign
# love0) was audited 2026-07-27. This gate says it ACROSS the two artifacts, which
# is the form a person downloading them can care about.
#
# ⚠ THE TWO ARTIFACTS ARE COMPARED TO EACH OTHER, not to the in-tree binary. That is
# the claim as stated, and it is also the only form that survives a dirty tree: the
# tarballs are cut from the INDEX and the in-tree binary is built from the WORKING
# TREE, so on an uncommitted change those two legitimately differ. The in-tree
# comparison is made too, but only when git says the tree is clean.
#
# ⚠ AND THE FULL LANE POISONS THE COMPILER. A gate that merely observes the build
# succeed cannot tell whether the bundled love did the work or the ambient gcc
# quietly did it: both produce a working binary. So cc/gcc/clang are shadowed by
# scripts that fail loudly, and the full build has to come out the far side anyway.
#
# ⚠ AND THE THIRD ARTIFACT CARRIES ITS OWN SOURCE. The self-extracting binary holds
# the lean tarball in .rodata (tools/mksrc.l, host/src.c) and `love source` lays it
# out with bin/love already inside, so one downloaded file needs no tar and no second
# fetch. Its leg is the same shape as the full one -- poison the compilers, build, and
# require the same bytes -- because "it unpacked something" is not the claim.
#
# Three complete bootstraps -- minutes, not seconds. Opt-in, by name.
# usage: distboot.sh SRC_TGZ FULL_TGZ SELF_EXE [REFERENCE_LOVE]
set -u

src=$1
full=$2
selfexe=$3
ref=${4:-}

for t in "$src" "$full"; do
  [ -f "$t" ] || { echo "distboot: no $t -- run 'make dist-rel'"; exit 1; }
done
command -v make >/dev/null 2>&1 || { echo "distboot: no make, skipped"; exit 0; }

R=$(pwd)
w=$(mktemp -d)
trap 'rm -rf "$w"' EXIT
fail() { echo "FAIL distboot: $*" >&2; exit 1; }

# our own extractor, so the gate leans on nothing it is not already testing
love=$R/out/host/love
[ -x "$love" ] || fail "no $love"

echo "distboot: three full bootstraps, this takes a few minutes"

# ---- 1. the LEAN artifact, through the machine's own compiler ----------------
mkdir -p "$w/lean"
"$love" "$R/tools/tgz.l" x "$src" "$w/lean" > /dev/null || fail "cannot unpack $src"
lean=$(echo "$w"/lean/love-*/)
[ -d "$lean" ] || fail "the lean tarball unpacked no love-<ver>/ directory"
[ -f "$lean/VERSION" ] || fail "the lean tarball carries no VERSION (the binary would stamp 'unknown')"
[ ! -e "$lean/.git" ] || fail "the lean tarball shipped a .git"
( cd "$lean" && make -j"$(nproc 2>/dev/null || echo 4)" out/host/love ) > "$w/lean.log" 2>&1 \
  || { tail -20 "$w/lean.log"; fail "the lean artifact does not build"; }
[ -x "$lean/out/host/love" ] || fail "the lean build produced no love"
echo "  OK lean: builds through the ambient cc"

# ---- 2. the FULL artifact, with every ambient compiler POISONED -------------
mkdir -p "$w/full" "$w/nocc"
for c in cc gcc clang c99 tcc; do
  printf '#!/bin/sh\necho "distboot: the ambient %s was called -- the bundled love should have been the compiler" >&2\nexit 1\n' "$c" > "$w/nocc/$c"
  chmod +x "$w/nocc/$c"
done
"$love" "$R/tools/tgz.l" x "$full" "$w/full" > /dev/null || fail "cannot unpack $full"
fulld=$(echo "$w"/full/love-*/)
[ -d "$fulld" ] || fail "the full tarball unpacked no love-<ver>/ directory"
[ -x "$fulld/bin/love" ] || fail "the full tarball ships no runnable bin/love"
( cd "$fulld" && PATH="$w/nocc:$PATH" make -j"$(nproc 2>/dev/null || echo 4)" out/host/love ) > "$w/full.log" 2>&1 \
  || { tail -20 "$w/full.log"; fail "the full artifact does not build without an ambient compiler"; }
[ -x "$fulld/out/host/love" ] || fail "the full build produced no love"
grep -q "was called" "$w/full.log" && { grep "was called" "$w/full.log" | head -3; fail "the full build reached for an ambient compiler"; }
echo "  OK full: builds with cc/gcc/clang poisoned -- the bundled love was the toolchain"

# ---- 3. the SELF-EXTRACTING binary, which needs no tarball at all -------------
# ⚠ run it from a COPY in the scratch dir. `love source` lays its tree beside the
# binary's cwd, and the tree we are testing must not land in the repo.
mkdir -p "$w/self"
cp "$selfexe" "$w/self/love" || fail "cannot copy $selfexe"
# ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads. The root Makefile EXPORTS it for the corpus,
# and an egg-booted love has no verb table at all -- `source` then reads as a FILENAME
# and the artifact answers "cannot open source", which looks like a missing verb
# rather than a missing image. The build lanes lead with the same thing for `mooncc`.
( cd "$w/self" && LOVE_NO_IMAGE= ./love source ) > "$w/self.log" 2>&1 \
  || { tail -20 "$w/self.log"; fail "the self-extracting binary could not lay its source"; }
selfd=$(echo "$w"/self/love-*/)
[ -d "$selfd" ] || fail "'love source' unpacked no love-<ver>/ directory"
[ -f "$selfd/VERSION" ] || fail "the embedded source carries no VERSION"
# ⚠ bin/love is the whole point: without it the unpacked tree falls back to the
# ambient cc and the one-file claim quietly becomes a two-tool one.
[ -x "$selfd/bin/love" ] || fail "'love source' laid no runnable bin/love"
( cd "$selfd" && PATH="$w/nocc:$PATH" make -j"$(nproc 2>/dev/null || echo 4)" out/host/love ) \
  > "$w/selfb.log" 2>&1 \
  || { tail -20 "$w/selfb.log"; fail "the self-extracted tree does not build without an ambient compiler"; }
grep -q "was called" "$w/selfb.log" && { grep "was called" "$w/selfb.log" | head -3; fail "the self-extracted build reached for an ambient compiler"; }
echo "  OK self: one binary lays its own source and builds it, no tar and no ambient cc"

# ---- 4. THE CLAIM ------------------------------------------------------------
if cmp -s "$lean/out/host/love" "$fulld/out/host/love" \
   && cmp -s "$lean/out/host/love" "$selfd/out/host/love"; then
  echo "  OK all three artifacts answer the SAME binary ($(wc -c < "$lean/out/host/love") bytes)"
else
  ls -l "$lean/out/host/love" "$fulld/out/host/love" "$selfd/out/host/love"
  fail "the artifacts built DIFFERENT binaries -- the release claim is false"
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

echo "distboot: lean, full and self-extracting bootstrap to the same love -- ok"
