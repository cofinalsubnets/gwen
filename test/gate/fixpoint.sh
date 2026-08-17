#!/bin/sh
# test/gate/fixpoint.sh -- the SELF-REGENERATION fixpoint (self-host rung 2).
# The default out/host/love is mooncc-built already (love0 waking mooncc0.image
# compiles every TU, holo links -pie). This gate closes the loop: relink that
# generation as love1, let love1 bake its OWN mooncc image and rebuild every TU
# with itself, link love2 the same way, and assert love1 == love2 TO THE BYTE.
# One diff = the fixpoint + the determinism differential + the trusting-trust
# half in a single compare (the DDC leg proper adds a foreign-compiled love0;
# the 2026-07-27 audit ran that lane green).
#
# make owns the dependency graph (the moon_o objects + mooncc0.image exist);
# this owns the procedure. NOT set -e: the compile loop reports its own file.
#
# usage: fixpoint.sh OUTDIR LOVE0 FIRFLAG OBJ...
# ⚠ FIRFLAG comes FROM make ($(moon_fir)) rather than being written down here. It used to be
# spelled in both places, and the two spellings drifted the moment the shipped build changed
# its mind: love1 carried make's objects and love2 the gate's, ~1.5 MB of record apart, and
# the report was `the fixpoint broke` -- a compiler that agreed on every byte it emitted.
set -u

ho=$1
love0=$2
fir=$3
shift 3
d=$ho/fix
cat=$ho/.mooncc-cat.l

# any arch a seed can be laid for owes this invariant (doc/plan/seed-universal.md
# U0); an arch off the roster skips, it does not fail. the mksys leaf is the
# host's own (the twin roster, crew/build.mk).
case "$(uname -m)" in
  x86_64)  mks=mksys ;;
  aarch64) mks=mksys-arm64 ;;
  riscv64) mks=mksys-riscv ;;
  *) echo "test_fixpoint: no seed for $(uname -m), skipped"; exit 0
esac

fail() { echo "FAIL test_fixpoint: $*" >&2; exit 1; }

mkdir -p "$d"
rm -f "$d"/*.o "$d"/love1 "$d"/love2 "$d"/mooncc1.image

# love1: relink the generation make already compiled (love0's lane, byte-cheap).
# ⚠ the list arrives FROM make ($(moon_o), source-derived) and is never globbed out of
# the odir: a deleted host/*.c leaves its .o sitting there, and a glob relinks the ghost --
# love1 carrying a TU love2 never compiles, which reads as a broken fixpoint.
moon0() { "$love0" wake "$ho/mooncc0.image" mooncc "$@"; }
moon0 -pie "$@" -o "$d/love1" || fail "love1 relink"

echo "FIX  $d/love1 rebuilds itself"

# love1 bakes its own compiler image (anchor-checked to love1)...
LOVE_NO_IMAGE=1 "$d/love1" -l "$cat" -e "(? ((bake \"$d/mooncc1.image\") = 1) (quit 0) (quit 1))" \
  || fail "love1 bakes mooncc1.image"

# ...and rebuilds every TU with it, in the exact order make links them
moon1() { "$d/love1" wake "$d/mooncc1.image" mooncc "$@"; }
# ⚠ core/love.c's flags must MIRROR make's ($(moon_d)/love.o in host/build.mk), not just its
# order: -D AiHaveVersionH is what puts the version id in this TU, and love1 was linked
# from make's object. Drop it here and love2 carries "unknown" -- the compare fails at the
# string, naming a broken fixpoint where the only difference is a build flag. $fir is the
# same trap wearing its second face -- it decides whether ~1.5 MB of IR record rides -- and
# it arrives from make for exactly that reason.
moon1 -D ai_tco=1 -D AiHaveVersionH $fir -I"$ho" -I. -Icore -Iout/lib -c core/love.c "$d/love.o" || fail "love1 mooncc -c core/love.c"
for f in host/*.c; do
  b=$(basename "$f" .c)
  moon1 -D ai_tco=1 $fir -I"$ho" -I. -Icore -Iout/lib -c "$f" "$d/host_$b.o" || fail "love1 mooncc -c $f"
done
# nolibc rides the implicit runtime, as in raw.sh -- pulled member by need.
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  moon1 $fir -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || fail "love1 mooncc -c $f"
done
LOVE_NO_IMAGE=1 "$d/love1" -l "$ho/.mksys-cat.l" -e "((from 'moon '$mks) \"$d/sys.o\")" >/dev/null || fail "love1 mksys"
test -s "$d/sys.o" || fail "love1 mksys laid an empty sys.o"

# love2 takes the SAME list in the SAME order, one directory over -- link order is layout,
# so two globs agreeing by luck is not one list. A name love1 linked and the loops above
# never compiled dies here, at the linker, by name.
o2=; for o in "$@"; do o2="$o2 $d/${o##*/}"; done
moon1 -pie $o2 -o "$d/love2" || fail "love2 link"

cmp "$d/love1" "$d/love2" || fail "love2 differs from love1 -- the fixpoint broke"

echo "test_fixpoint: love0+mooncc -> love1; love1+mooncc -> love2; byte-identical -- the compiler rebuilds itself exactly"
