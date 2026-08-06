#!/bin/sh
# test/gate/elf32.sh -- holo's ELF32 executable writer (crew/holo/elf.l elf32-at), judged by
# a real loader. Both thumb backends lay the same program; Linux maps the segment, enters at
# e_entry in Thumb state, and the process must print its line and leave with 42.
#
# NOTHING FOREIGN WRITES A BYTE: no as, no ld, no libc, no arm-none-eabi at all -- qemu-arm
# is an emulated cpu, not a toolchain. That is the point of the lane, and the reason it runs
# on boxes where test_thumb1/2 skip.
#
# usage: elf32.sh OUTDIR
set -u

ho=$1
name=test_elf32
fail() { echo "FAIL $*" >&2; exit 1; }

command -v qemu-arm > /dev/null 2>&1 || { echo "$name: no qemu-arm, skipped"; exit 0; }

echo "ELF32 $ho/elf32"
d=$ho/elf32
mkdir -p "$d"

for tgt in thumb1 thumb2; do
  { cat crew/holo/holo.l "crew/holo/$tgt.l" crew/holo/elf.l test/gate/elf32.l
    echo "(elf32-write '$tgt \"$d/$tgt.elf\")"; } | "$ho/love" || fail "elf32-write $tgt"
  chmod +x "$d/$tgt.elf"
  out=$(qemu-arm "$d/$tgt.elf"); rc=$?
  [ $rc -eq 42 ] || fail "$tgt: exit $rc, want 42 (139 = entered in ARM state, so no thumb bit)"
  case $out in
    *"arm32 linux"*) ;;
    *) fail "$tgt: stdout was '$out'" ;;
  esac
done

echo "$name: thumb1 + thumb2 ELF32 executables loaded, ran and left with 42 -- ok"
