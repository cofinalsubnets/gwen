#!/bin/sh
# test/gate/hdiff.sh -- THE FOREIGN-CC DIFFERENTIAL at the host, KCC's twin one level up.
#
# `love` is mooncc-built and holo-linked in the default lane, so gcc and clang no longer
# touch the vm at all -- and what they would catch, nothing else does. Two things only:
#
#   1. it BUILDS and answers, per cc. semantics are the interpreter's and do not change
#      with the compiler, so re-running the whole corpus through each is spent time -- the
#      quick host suite is the whole reading.
#   2. `vmret` is green, per cc. this is the one that earns the lane: ai_musttail is OWED,
#      and a shape that cannot jump must refuse at compile. mooncc's sibcall pass asks only
#      whether a jump is EMITTABLE, so it waved through a 5-arg caller jumping into a 4-arg
#      callee that clang refuses outright (doc/misc/moon-c-gaps.md). vmret cannot catch it
#      either -- it reads the binary mooncc built, sounding our own rule against our own
#      output. A foreign cc compiling the same file is the only instrument that has.
#
# ⚠ ai_tco=1 IS THE POINT. at tco=0 ai_musttail expands to nothing and this proves nothing,
# which is exactly why love0's clang lane never caught any of it.
#
# usage: hdiff.sh CC..
set -u
fail() { echo "FAIL test_hdiff: $*" >&2; exit 1; }

# vmret is what earns this lane, and it is a NO-OP with a message where no
# disassembler is present -- so without one the loop below would build, run the
# suite, and report "tail-jump clean" having read no instruction at all.
command -v objdump > /dev/null 2>&1 || command -v llvm-objdump > /dev/null 2>&1 || {
  echo "test_hdiff: no objdump or llvm-objdump, so vmret reads nothing -- SKIPPED, not passed"
  exit 0; }

# ..and the cc has to be able to HONOUR ai_musttail. love.h refuses to build the
# tail-threaded vm without it, so a cc that lacks it would look like "could not
# build love" -- a failure report for a tree that is fine and a toolchain that is old.
mtc=out/host/.hdiff-musttail.c
mkdir -p out/host
printf '%s\n' \
  '#if !defined(__clang__) && !(defined(__GNUC__) && __GNUC__ >= 15)' \
  '#error no musttail' \
  '#endif' \
  'int main(void) { return 0; }' > "$mtc"

ran=""
for cc in "$@"; do
  command -v "$cc" > /dev/null 2>&1 || { echo "test_hdiff: no $cc, skipped"; continue; }
  "$cc" -c "$mtc" -o "$mtc".o > /dev/null 2>&1 || {
    echo "test_hdiff: $cc has no musttail -- ai_tco=1 is this lane's point, so SKIPPED, not passed"
    continue; }
  echo "  $cc: building love (HCC=1, ai_tco=1)"
  make --no-print-directory HCC=1 CC="$cc" host > /dev/null 2>&1 \
    || fail "$cc could not build love"
  b=out/host-cc/love
  [ -x "$b" ] || fail "$cc laid no $b"
  # it answers: the corpus is the interpreter's business, this is the binary's.
  [ "$("$b" -e '(2 = 1 + 1)' 2>&1)" = 1 ] || fail "the $cc-built love does not answer"
  echo "  $cc: the quick host suite"
  make --no-print-directory HCC=1 CC="$cc" test_host > /dev/null 2>&1 \
    || fail "$cc: test_host"
  echo "  $cc: vmret"
  make --no-print-directory HCC=1 CC="$cc" vmret > /dev/null 2>&1 \
    || fail "$cc: vmret -- an lvm_ kept a ret"
  ran="$ran $cc"
done

rm -f "$mtc" "$mtc".o
# ⚠ the summary names what RAN. a line that says "gcc and clang" after skipping both
# is the same silence this gate exists to end, one level up.
[ -n "$ran" ] || { echo "test_hdiff: no cc here could run this lane -- nothing was proved"; exit 0; }
echo "test_hdiff:$ran each build love, pass the host suite and tail-jump clean"
