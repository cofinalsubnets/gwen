#!/bin/sh
# test/gate/bakerep.sh -- A BAKE IS A FUNCTION OF THE TREE, not of the machine.
#
# Two bakes of one binary must be the same bytes, so a release can be checked by its
# hash and two people can agree they have the same artifact. Everything that ever broke
# this wrote the BAKER's environment into the image instead of the tree's content:
#
#   the kept absolutes  a binary pointer stored as the address it happened to have,
#                       +delta'd on load -- correct, and it carried the ASLR base
#   the header pair     `anchor`/`refsym`, two ADDRESSES compared only to check their
#                       deltas agreed, which is their GAP being preserved
#   a reverted husk     the glaze redirects a native cell to its interp twin and leaves
#                       the husk as ballast; its W^X pointer rode into the image, from a
#                       mapping whose distance to the binary is randomized
#   `born`              the hatch DURATION, frozen from whichever machine baked
#
# ⚠ SECONDS, AND IT RIDES THE SLOW GATE ON PURPOSE. test_distboot proves the whole
# circle -- the artifact rebuilding itself to the byte -- but it is opt-in and minutes
# long, so a regression here would sit unnoticed until a release. This is the same law
# asked cheaply enough to run every time.
#
# ⚠ the two bakes run at the SAME PATH, one after the other, because a love bakes its
# own path into the heap (`love-image`, and the seat the loader walks from). Two names
# would differ legitimately and say nothing about determinism.
#
# usage: bakerep.sh OUTDIR
set -u

ho=$1
w=$(mktemp -d)
trap 'rm -rf "$w"' EXIT
fail() { echo "FAIL test_bakerep: $*" >&2; exit 1; }

[ -x "$ho/love" ] || fail "no $ho/love"
cp "$ho/love" "$w/seed" || fail "cannot copy $ho/love"

for i in 1 2; do
  cp "$w/seed" "$w/love" || fail "cannot stage bake $i"
  ( cd "$w" && ./love bake ) > "$w/bake$i.log" 2>&1 \
    || { cat "$w/bake$i.log"; fail "bake $i failed"; }
  mv "$w/love" "$w/b$i" || fail "bake $i produced nothing"
done

if ! cmp -s "$w/b1" "$w/b2"; then
  echo "  the two bakes differ in $(cmp -l "$w/b1" "$w/b2" 2>/dev/null | wc -l) bytes" >&2
  off=$(cmp "$w/b1" "$w/b2" 2>&1 | sed 's/.*byte //;s/,.*//')
  echo "  first at byte $off" >&2
  fail "a bake is not reproducible -- something of the MACHINE is in the image"
fi

# ..and the thing still has to WAKE. A bake that is reproducible and dead would pass the
# comparison above and nothing else, which is the failure this line exists to refuse.
# ⚠ GREP, never a whole-output compare: `-e` prints the form's VALUE as well as anything
# it said, so a probe that puts "x" answers `x"x"` and an equality test fails on the echo
# rather than on the answer.
out=$(cd "$w" && env -u LOVE_NO_IMAGE ./b1 -e '(puts (? (3 = 1 + 2) "wake-ok" "wake-bad"))' 2>&1) \
  || fail "the reproducible bake does not run"
case $out in *wake-ok*) ;; *) fail "the reproducible bake woke wrong: [$out]" ;; esac

echo "test_bakerep: two bakes of one binary are the same bytes, and it wakes ($(sha256sum < "$w/b1" | cut -c1-16)..)"
