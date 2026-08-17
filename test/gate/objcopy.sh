#!/bin/sh
# test/gate/objcopy.sh -- crew/holo/copy.l against the tool it replaces. objcopy's two
# output formats are a WIRE, not a taste: the .hex a Teensy loader accepts and the .bin
# whose offsets are flash offsets. So the gate is a byte comparison with the real thing
# over every ELF on hand, both formats, and nothing softer.
#
# The fixtures are minted by our OWN linker at addresses chosen to reach the rules a
# board's image happens to miss: a section straddling a 64K line (a data record has to
# stop there, not wrap), and an image climbing past 1 MB (the base record changes AGE
# there -- an 8086 segment below, a linear half above, and they SUM, so the segment has
# to be zeroed on the way through). A binary in /usr/bin brings the third: a dynamic
# exe loads .note / .dynsym / .rela, which are not PROGBITS and are still image bytes.
#
# Skips where no objcopy exists -- there is no oracle then, and the ports' own boot-image
# verifies (test_teensy41 / test_nucleo446 / test_rp2040) still read what we wrote.
#
# usage: objcopy.sh OUTDIR
set -u

ho=$1
name=test_objcopy
oc=`command -v llvm-objcopy 2>/dev/null || command -v objcopy 2>/dev/null || true`
[ -n "$oc" ] || { echo "$name: no objcopy, skipped"; exit 0; }

d=$ho/objcopy
rm -rf "$d"; mkdir -p "$d"
echo "OBJCOPY $d"

{ echo "(use 'holo)"
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/thumb2.l \
      crew/holo/elf.l crew/holo/obj.l crew/holo/link.l crew/holo/copy.l
  echo "((from 'holo 'objcopy) >argv)"; } > "$d/ocopy.l"

# the fixtures, ours end to end: mkfix.l lays one object and links it three ways
{ echo "(use 'holo)"
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/thumb2.l \
      crew/holo/elf.l crew/holo/obj.l crew/holo/link.l test/gate/objcopy.l
  echo "(mkfix \"$d\")"; } | "$ho/love" || { echo "FAIL objcopy fixtures"; exit 1; }

n=0
chk() {
  for f in ihex binary; do
    "$ho/love" "$d/ocopy.l" -O $f "$1" "$d/mine.out" || { echo "FAIL objcopy -O $f $1"; exit 1; }
    "$oc" -O $f "$1" "$d/ref.out" || { echo "FAIL $oc -O $f $1"; exit 1; }
    cmp "$d/mine.out" "$d/ref.out" > /dev/null \
      || { echo "FAIL objcopy -O $f $1 -- differs from $oc"; exit 1; }
    n=`expr $n + 1`
  done
}

for e in "$d"/fix-*.elf; do chk "$e"; done
for e in "$ho/love" /usr/bin/ls /bin/sh out/teensy41/love.elf out/nucleo446/firm.elf \
         out/rp2040/love.elf; do
  [ -f "$e" ] && chk "$e"
done

# and the two refusals, where the oracles part company: gnu objcopy errors on an ELF
# with no section headers, llvm-objcopy writes a ZERO-BYTE image and says nothing. we
# error, and a firmware build that would have shipped empty stops instead.
if [ -f "$ho/elf32/thumb2.elf" ]; then
  "$ho/love" "$d/ocopy.l" -O binary "$ho/elf32/thumb2.elf" "$d/mine.out" 2>/dev/null \
    && { echo "FAIL objcopy took a section-header-less ELF"; exit 1; }
fi
"$ho/love" "$d/ocopy.l" -O binary test/gate/objcopy.sh "$d/mine.out" 2>/dev/null \
  && { echo "FAIL objcopy took a non-ELF"; exit 1; }

echo "$name: $n flattens byte-identical to $oc, and two refusals held"
