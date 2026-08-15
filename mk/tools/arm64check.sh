#!/bin/sh
# arm64check.sh -- the aarch64 EXECUTION validator. Cross-builds `love` for aarch64
# (reusing the native love0-generated headers in out/lib) and RUNS a test corpus
# under qemu-aarch64. apps/asm/asmtest.l proves the arm64 byte ENCODINGS; this proves
# they RUN -- the only trustworthy check for the glaze's second target, since the
# glaze self-test executes the machine code it emits.
#
# Needs qemu-aarch64 (qemu-user) + an aarch64 cross-gcc with a static libc. Point
# AARCH64_CC at it, or let the auto-find try the usual names + the local Nerves
# toolchain. No-ops (exit 0, a note) when either is missing -- like test_kernel /
# test_wasm. love does its own write(2)-based I/O, so it is unaffected by the
# static-glibc printf-vararg quirk seen under qemu.
#
# Args: $1.. = the .l files to run (default: the corpus). The gate is the
# "tests pass" sentinel AND a clean exit (a reader-stop exits 0 without it).
set -e
cd "$(dirname "$0")/.."

QEMU=$(command -v qemu-aarch64 2>/dev/null || true)
GCC="$AARCH64_CC"
[ -z "$GCC" ] && GCC=$(command -v aarch64-linux-gnu-gcc 2>/dev/null || true)
[ -z "$GCC" ] && GCC=$(command -v aarch64-nerves-linux-gnu-gcc 2>/dev/null || true)
[ -z "$GCC" ] && GCC=$(ls /usr/local/data/*/.nerves/artifacts/nerves_toolchain_aarch64*/bin/aarch64-nerves-linux-gnu-gcc 2>/dev/null | head -1)
if [ -z "$QEMU" ] || [ -z "$GCC" ]; then
  echo "test_arm64: skipped (need qemu-aarch64 + an aarch64 cross-gcc; set AARCH64_CC)"; exit 0
fi

# the native headers (egg + lcat libs) must exist; they are arch-neutral source.
test -f out/lib/egg.h || { echo "test_arm64: run 'make host' first (need out/lib/*.h)"; exit 1; }

O=out/arm64; mkdir -p $O; rm -f $O/*.o
# ai_tco=1 (the DEFAULT threaded dispatch): the glaze native code is NOT
# dispatch-independent -- it emits the threaded ABI (Ip/Hp/Sp in x1/x2/x3) and
# its own Continue (advance Ip, br [Ip]). Under tco=0 the native is entered as
# ap(g) with x1/x2/x3 garbage, so the codegen ONLY runs under tco=1. -O2 gives
# the sibling-call optimization the threaded loop relies on.
CF="-std=gnu2x -O2 -Dai_tco=1 -I. -Iout/lib -fomit-frame-pointer -fno-stack-protector -fno-exceptions -w"
echo "AARCH64 cross-build ($GCC)"
# crew/moon/lib/math/am.c is the math floor (fdlibm/-lm retired) -- link it like the host does.
for f in core/love.c host/*.c crew/moon/lib/math/am.c; do
  o=$O/$(basename "$f" .c).o
  $GCC $CF -c "$f" -o "$o"
done
# core/love_data.ld pins the data-sentinel tiling core/love.h's ai_typ reads; without it
# ai_ini traps at boot (SIGTRAP), which is how this lane announced itself.
LD_ARGS="-Wl,-T,core/love_data.ld"
$GCC -static $LD_ARGS -o $O/love $O/*.o 2>/dev/null || $GCC -static $LD_ARGS -o $O/love $O/*.o

# the corpus (or the files named on the command line), under qemu. Mirror mk/common.mk's
# $t EXACTLY: 00-init, spec, then uu.l HOISTED to 3rd (uu's test files uukind/uukindlaw
# assume uu is set up early), then the C-sorted rest with those filtered out. A bare
# `ls` (locale-sorted, uu.l NOT hoisted) loads ~40 files before uu.l -> uukindlaw.l's
# `(member? 'vof (names ()))` fails with `missing defn`. $t is the source of truth.
if [ $# -gt 0 ]; then set -- "$@"; DEFAULT_CORPUS=; else DEFAULT_CORPUS=1
  # test/arm64/*.l (a subfolder, invisible to the non-recursive test/*.l corpus glob) holds aarch64-ONLY
  # execution tests -- they build `... 'arm64` nifs whose code is aarch64 bytes and RUN them, which would
  # fault on the x86 corpus. Appended here so only qemu-aarch64 ever runs them; the gate requires each one's
  # sentinel below so a silent reader-stop can't skip them.
  set -- test/00-init.l test/spec.l test/uu.l $(ls test/*.l | LC_ALL=C sort | grep -vE '00-init|spec\.l|glaze-x86|uu\.l') test/arm64/callout.l
fi
echo "AARCH64 qemu run ($(echo "$@" | wc -w) files)"
# under a CEILING: a wedged emulated corpus is indistinguishable from a slow one, and this
# lane takes ~60 s, so a hang used to sit until someone noticed. 420 s is ktest.l's budget,
# the same shape; exit 124 is the timeout's own and reads as the failure it is.
# the corpus goes in as a FILE with stdin closed, not on a pipe -- test_host's reason
# (test/test.mk): the corpus TESTS stdin (test/io.l's see/unsee roundtrip), and `reads`
# no longer drains stdin ahead of the first form, so a piped corpus has those asserts
# poking the very script they are riding on.
# ⚠ THE RUN GOES IN AN `if`, not `cmd; r=$?`: under `set -e` a failing corpus kills the
# script right here, so the FAIL branch below never speaks and the gate reports Error 1
# with nothing to read. A condition context is the one place a non-zero exit is allowed.
cat "$@" > $O/.corpus.l
if LOVE_NO_IMAGE=1 timeout 420 "$QEMU" $O/love $O/.corpus.l </dev/null > $O/.out 2>&1
  then r=0; else r=$?; fi
tail -1 $O/.out
# the default corpus must print "tests pass" AND each test/arm64/*.l sentinel (explicit-args runs skip the sentinel check)
if [ -n "$DEFAULT_CORPUS" ]; then sent='test/arm64/callout:'; else sent=''; fi
{ [ $r -eq 0 ] && grep -q "tests pass" $O/.out && { [ -z "$sent" ] || grep -q "$sent" $O/.out; }; } || { echo "FAIL arm64 (exit $r)"; tail -20 $O/.out; exit 1; }
echo "test_arm64: ok"
