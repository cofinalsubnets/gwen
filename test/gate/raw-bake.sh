#!/bin/sh
# test/gate/raw-bake.sh -- the WAKE half of the gcc-free image cycle. mooncc could
# never bake before (an EXEC's low load address collides the codec's pointer/fixnum
# index range); a -pie ET_DYN loads high and clears it. Reuses test_raw's objects,
# links them PIE, bakes the post-warm image into the binary's OWN .image (host/image.c
# self-patch), then WAKES that image and runs the corpus over the woken heap. This is
# the ONLY gate that exercises ai_image_load on a mooncc binary -- the seam where an
# odd-addressed lvm_* ap mis-encodes as a fixnum, invisible to every egg-boot gate.
#
# The SPLIT: make owns the dependency graph and the file lists -- its $(filter-out)
# drops glaze.l, because the bake pulls the nif/nifx seam off the book, so (lit? nif)
# cannot hold on a woken heap; the gcc build fails it identically, so it is not a
# mooncc regression. This file owns the PROCEDURE, which is why it is a file: a make
# recipe would carry the same steps \-joined into one line with $$ throughout.
#
# NOT set -e: the corpus run captures $? to report the exit code in its own failure
# message, and errexit would abort before the capture (that is also what rules out
# .SHELLFLAGS := -ec for the tree as a whole -- 96 sites do this).
#
# usage: raw-bake.sh OUTDIR CORPUS.l ..
set -u

ho=$1
shift

arch=$(uname -m)
if [ "$arch" != x86_64 ]; then
  echo "test_raw_bake: x86-64 only, skipped on $arch"
  exit 0
fi

fail() { echo "FAIL test_raw_bake: $*" >&2; exit 1; }

d=$ho/raw
ls "$d"/*.o >/dev/null 2>&1 || fail "no objects in $d -- run test_raw first"

echo "BAKE-WAKE $ho/love-raw-pie"
$ho/mooncc -pie "$d"/*.o -o "$ho/love-raw-pie" || fail "-pie link love-raw-pie"

# bake a COPY, so the un-baked pie binary stays around
cp "$ho/love-raw-pie" "$ho/love-raw-baked"
"$ho/love-raw-baked" --bake >/dev/null 2>&1 \
  || fail "--bake (the mooncc-PIE binary refused to snapshot its own image)"

# the corpus runs CONCATENATED in one global scope
out=$ho/.test_raw_bake.out
# corpus as a FILE, stdin closed -- test_host's reason (test/test.mk): the corpus
# TESTS stdin, and `reads` no longer drains it ahead of the first form.
cat "$@" > "$ho/.corpus-baked.l"
"$ho/love-raw-baked" "$ho/.corpus-baked.l" </dev/null > "$out" 2>&1
s=$?
tail -1 "$out"
[ $s -eq 0 ] && grep -q "tests pass" "$out" \
  || fail "the woken corpus (exit $s) -- ai_image_load desync?"

echo "test_raw_bake: the mooncc-PIE binary bakes its own image and WAKES it -- corpus passes on the woken heap"
