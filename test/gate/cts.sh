#!/bin/sh
# test/gate/cts.sh -- mooncc against an OUTSIDE corpus: c-testsuite's 220 single-file
# programs, each with the stdout+stderr it must print. Three targets, one procedure
# (ccarch.sh's shape).
#
# WHY THIS AND NOT MORE test/cc FILES. Every file in test/cc/ was written here, and
# nearly all of them were written to pin a fault we had already tripped over -- so the
# battery says what we have already learned and nothing about what we have not. This
# corpus was written by people compiling other compilers, and its FIRST run found nine
# programs mooncc compiles clean and answers wrong, two of them by segfaulting, plus
# nineteen it refuses. None of the nine had a consumer here yet, which is the whole
# point: they were waiting for one.
#
# THE ORACLE IS THE CORPUS'S OWN .expected FILE, not gcc. That is the difference from
# every other cc gate here -- an expected output written by a third party is an
# independent opinion, where gcc is a second one that can share our reading of a
# murky rule. (gcc passes all 220 on this box; the roster below is therefore ours.)
#
# THE FAILURES ARE ROSTERED, NOT SKIPPED, with a cause each -- and the roster is
# double-edged: a program not on it must pass, and a program ON it must still fail.
# So a fix that lands takes its line off this list, and a regression cannot hide in a
# skip. The KIND is asserted too, because the two are not the same news: `refuses` is
# mooncc saying no and naming the file, and costs an afternoon; `wrong` is mooncc
# compiling clean and handing back a wrong answer, and costs a wrong artifact. A
# refusal degrading into a wrong answer must be loud.
#
# Skips whole (exit 0, with the clone line) when the corpus is not here -- it is an
# imported tree like the package rungs, found under dl/ then $MOONSRC (~/src), and
# `make dl/c-testsuite` fetches it. The cross lanes skip without their qemu.
# make owns the dependency graph; this owns the procedure.
# NOT set -e: the checks report their own failures with context.
#
# usage: cts.sh ARCH OUTDIR LOVE     (ARCH: x64 | arm64 | riscv64)
set -u

arch=$1
ho=$2
m=$3

case $arch in
  x64)     name=test_cts       ; tflag=""           ; QEMU= ; qemu= ; pretty=x86-64 ;;
  arm64)   name=test_cts_arm64 ; tflag="-t arm64"   ; qemu=qemu-aarch64 ; pretty=aarch64 ;;
  riscv64) name=test_cts_riscv ; tflag="-t riscv64" ; qemu=qemu-riscv64 ; pretty=riscv64 ;;
  *) echo "cts.sh: unknown target $arch" >&2; exit 1 ;;
esac

# The roster, one line each: <number> <cause>. Every entry below reproduces on all
# three targets, which is itself a finding -- these are front-end and shared-model
# faults, not backend ones, and no target routes around any of them. The two that ARE
# per-target follow the lists.
roster_refuses='
00050 a brace-elided initializer that continues PAST an anonymous union member
00149 the address of a compound literal in a static initializer
00150 the same, with designated initializers nested inside it
00201 a ## paste that MAKES a macro name, which is then invoked with arguments
00204 a register-exhausted SSE-class by-value argument -- five float HFAs (the gp twin landed, this one did not)
00213 a statement expression, ({ ... })
00214 a statement expression under __builtin_expect
00216 designated RANGE initializers -- [1 ... 5] = v
'
# EMPTY, and worth keeping as a list rather than a comment: the day one of these
# comes back it belongs here, and `wrong` is the kind that must stay loud.
roster_wrong=''
# the two per-target lines, both of them lanes x64 has and the others do not
# (doc/misc/moon-c-gaps, "target asymmetries"): a by-value composite in a variadic
# function, and the variable-length array.
if [ "$arch" != x64 ]; then
  roster_refuses="$roster_refuses
00140 no lane for a by-value composite argument on $arch (x64 carries it, named and anonymous)
"
fi
fail() { echo "FAIL $name: $*" >&2; exit 1; }
moonrun() { LOVE_NO_IMAGE= "$m" mooncc "$@"; }
# the roster read two ways: which kind a number is on, and what its cause says
kindof() {
  case "$roster_refuses" in *"
$1 "*) echo refuses; return;; esac
  case "$roster_wrong" in *"
$1 "*) echo wrong; return;; esac
  echo pass
}
causeof() { printf '%s\n%s\n' "$roster_refuses" "$roster_wrong" | grep "^$1 " | cut -d' ' -f2-; }

if [ -n "$qemu" ]; then
  QEMU=$(command -v "$qemu" 2>/dev/null || true)
  [ -n "$QEMU" ] || { echo "$name: skipped (need $qemu)"; exit 0; }
fi

# the corpus, first hit wins: an explicit CTSSRC, then the tree-local dl/, then the
# cache. Answers empty when nothing matches, which the skip below reads.
cts=${CTSSRC:-}
if [ -z "$cts" ]; then
  for c in dl/c-testsuite "${MOONSRC:-$HOME/src}"/c-testsuite; do
    [ -f "$c/tests/single-exec/00001.c" ] && { cts=$c; break; }
  done
fi
if [ -z "$cts" ] || [ ! -f "$cts/tests/single-exec/00001.c" ]; then
  echo "$name: no c-testsuite here (looked in dl and ${MOONSRC:-$HOME/src}) -- skipped."
  echo "          \`make dl/c-testsuite\` fetches it, or set CTSSRC=<a clone>."
  exit 0
fi

d=$ho/cts-$arch
rm -rf "$d"; mkdir -p "$d"

npass=0; nref=0; nwrong=0
for f in "$cts"/tests/single-exec/*.c; do
  b=$(basename "$f" .c)
  want=$(kindof "$b")
  [ -f "$f.expected" ] || fail "$b: the corpus has no .expected for it"

  # ⚠ take the status on its own line: after `if ! cmd`, $? is the `!`, not the cmd.
  moonrun $tflag -o "$d/$b.bin" "$f" > "$d/$b.cclog" 2>&1; st=$?
  if [ $st -ne 0 ]; then
    case $want in
      pass)  fail "$b: mooncc refused a program that must pass -- $(head -1 "$d/$b.cclog")" ;;
      wrong) fail "$b: mooncc refuses a program rostered as compiling and answering WRONG ($(causeof "$b")) -- a gap and a defect are not the same news, so move its line" ;;
    esac
    [ $st -lt 128 ] || fail "$b: mooncc died on a signal ($st) where a refusal was expected"
    grep -q "$f" "$d/$b.cclog" \
      || { cat "$d/$b.cclog" >&2; fail "$b: the refusal does not name the file"; }
    nref=$((nref + 1)); continue
  fi
  [ "$want" != refuses ] \
    || fail "$b: mooncc BUILT a program rostered as refusing ($(causeof "$b")) -- take its line out of this script"

  # in its own subshell: two of the rostered-wrong ones SEGFAULT, and the shell
  # announcing that on stderr would read as the gate itself dying. ⚠ the trailing
  # `exit $?` is load-bearing -- a lone command in a subshell is exec'd into it, so
  # the SIGSEGV lands on the subshell and the parent does the announcing instead.
  # ⚠ AND IN $d, not here: 00154 writes fred.txt beside itself and 00196/00199 read it
  # back, so a run from the tree root litters the tree root (fred.txt was .gitignore'd
  # rather than confined). the subshell's cd keeps the outer paths below unchanged.
  ( cd "$d" && timeout 60 ${QEMU:-} "./$b.bin" > "$b.out" 2>&1; exit $? ) 2>/dev/null; r=$?
  [ $r -ne 124 ] || fail "$b: timed out"
  if [ $r -eq 0 ] && cmp -s "$f.expected" "$d/$b.out"; then
    [ "$want" != wrong ] \
      || fail "$b: now answers right, but is rostered wrong ($(causeof "$b")) -- take its line out of this script"
    npass=$((npass + 1)); continue
  fi

  [ "$want" = wrong ] || {
    echo "--- $b: exit $r, and ours vs the corpus's own .expected ---" >&2
    diff "$f.expected" "$d/$b.out" 2>/dev/null | head -20 >&2
    fail "$b: compiled clean and answered wrong -- a MISCOMPILE, not a gap"; }
  nwrong=$((nwrong + 1))
done

n=$((npass + nref + nwrong))
[ $n -gt 0 ] || fail "no programs ran from $cts"
echo "$name: $npass of $n c-testsuite programs answer exactly what the corpus says on $pretty; $nref refuse cleanly and $nwrong compile clean and answer WRONG, each named in this script and in doc/misc/moon-c-gaps"
