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
# ⚠ THE PATH IS NOT IN THE IMAGE, though this line once said it was and gave that as the
# reason the two bakes share a path. `love-image` is the literal "<baked>" wherever the
# binary carries its own .image section (host/main.c), and a bake unpins it besides; the
# seat rungs the loader once walked off selfpath retired with the modules arc. Bake one
# binary at two names of different lengths and the bytes agree -- so a second name would
# not "differ legitimately", it would just be a second name. The two below still run at
# one path because that is the question's own shape (bake THIS binary twice), and for no
# reason beyond it.
#
# usage: bakerep.sh OUTDIR
set -u

ho=$(cd "$1" && pwd) || exit 1
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

# ..and the same bytes under a DIFFERENT GC BUDGET, through the crew bake the build
# runs. the collector's timing once chose the intern layout and the serial ranks --
# each budget its own stable fixpoint. the canonical orders make the bake a function
# of the live set alone.
# (cwd stays the TREE, and that one IS load-bearing: the egg boot reads modules through
# cwd lib/. the path both lanes bake at is not -- see above.)
for lane in "b4:LOVE_BUDGET_MB=128" "b3:"; do
  cp "$w/seed" "$w/love" || fail "cannot stage the budget bake"
  env LOVE_NO_IMAGE=1 ${lane#*:} "$w/love" bake -l "$ho/.dist-cat.l" > "$w/bake.log" 2>&1 \
    || { cat "$w/bake.log"; fail "crew bake (${lane#*:}) failed"; }
  mv "$w/love" "$w/${lane%%:*}" || fail "crew bake produced nothing"
done
cmp -s "$w/b3" "$w/b4" \
  || fail "the GC budget is in the image -- a bake must not care when collections fire"

# ..and the thing still has to WAKE. A bake that is reproducible and dead would pass the
# comparison above and nothing else, which is the failure this line exists to refuse.
# ⚠ GREP, never a whole-output compare: `-e` prints the form's VALUE as well as anything
# it said, so a probe that puts "x" answers `x"x"` and an equality test fails on the echo
# rather than on the answer.
out=$(cd "$w" && env -u LOVE_NO_IMAGE ./b1 -e '(puts (? (3 = 1 + 2) "wake-ok" "wake-bad"))' 2>&1) \
  || fail "the reproducible bake does not run"
case $out in *wake-ok*) ;; *) fail "the reproducible bake woke wrong: [$out]" ;; esac

# THE FIRST BOOT (host/main.c): an unbaked binary that carries its source bakes and
# patches ITSELF, serves the invocation it was given -- and the bytes are the same
# bake. flip the image HEADER's magic (its byte string's LAST occurrence: the
# section is laid last, past the immediates in .text that spell the same constant)
# so the load refuses and the binary reads as unbaked, then run one command: the
# banner must show, the command must answer, and the self-patched file must equal
# the explicit bake to the byte -- the header the corruption lived in is exactly
# what the self-bake rewrites. b3 is the yardstick: the first boot IS the crew
# bake off the egg, where b1 re-baked an already-woken image.
cp "$w/b3" "$w/love" || fail "cannot stage the first boot"     # the SAME PATH the bakes ran at (the path law above)
mo=$(grep -abo 'AISNO05' "$w/love" | tail -1 | cut -d: -f1)
[ -n "$mo" ] || fail "no image magic to corrupt"
printf 'x' | dd of="$w/love" bs=1 seek="$mo" conv=notrunc 2>/dev/null \
  || fail "cannot corrupt the image"
out=$(cd "$w" && env -u LOVE_NO_IMAGE -u LOVE_FIRST_BOOT ./love -e '(puts "fb-ok")' 2>&1) \
  || fail "the first boot did not serve its invocation"
case $out in *fb-ok*) ;; *) fail "the first boot ran but the command answered nothing: [$out]" ;; esac
cmp -s "$w/love" "$w/b3" || fail "the first boot baked different bytes than the explicit bake"
rm -f "$w/love"

echo "test_bakerep: two bakes of one binary are the same bytes, it wakes, and a first boot self-bakes to the same bytes ($(sha256sum < "$w/b1" | cut -c1-16)..)"
