#!/bin/sh
# ccbench.sh -- the COMPILER shootout (the page's FOURTH table). Builds the love host
# binary with three C compilers and, for each, reports four wall-clock costs:
#   build : compile every C translation unit (src/love.c + host/*.c + the am math floor)
#           and link a working `love` -- source to runnable binary. ⚠ the mooncc lane
#           builds ONCE UNTIMED first; the note above that call says why, and the row read
#           2.2x too high until it did.
#   test  : run the full arch-neutral corpus ($t, the same files test_host/test_raw
#           feed) through the binary that build produced, with egg-boot EXCLUDED
#           (subtracted) so it times the suite executing, not the compiler self-install.
#   chacha / poly1305 : one C function each, same subtraction (bench/ccrypto.l).
#           These are here because the corpus row averages a compiler's work over all
#           of src/love.c, and the average is flattering: mooncc/clang reads ~1.1x there
#           and ~23x on chacha. chacha20 indexes a 16-word state ARRAY in its inner
#           loop, poly1305 keeps five limbs as scalar LOCALS, and mooncc has register
#           residency for the second shape only -- so the PAIR is the reading. Wide
#           chacha beside narrow poly says the gap is array slots; the day they close
#           together is the day that reading was wrong.
#   inflate / crc32 / sha256 : the HEAVY NIFS (bench/cnifs.l), and a different
#           question from the pair above. The ciphers are a lever chosen to name a
#           defect; these three are work the tree waits on -- `love source` unpacks its
#           own tarball through inflate and checks it with crc32, and every svalbard
#           blob id is a sha256. They are also three shapes: inflate is BRANCHY (a bit
#           reader and a table per symbol), crc32 has no branch in its loop at all, and
#           sha256 carries a 64-word array beside eight scalars -- the ciphers' two
#           shapes in one function. A lane behind on inflate and level on crc32 is
#           losing to branches, not to loads.
# The three compilers, ALL THREE STATIC -- that is the whole point of the pairing:
#   mooncc : love's OWN C compiler (crew/moon/), run out of THE SHIPPED ARTIFACT's own
#            `mooncc` verb -- no gcc/glibc/ld anywhere: mooncc lays every .o, mksys emits
#            the syscall leaf, our linker (crew/holo/) binds.
#   gcc-musl / clang-musl : the same translation units at the host's real -O2 cflags,
#            through the musl-gcc/musl-clang wrappers and linked -static. Also
#            egg-boot -- no `bake`, so all three lanes run the identical corpus off
#            the freshly-eval'd egg (a level field).
#
# ⚠ the natives are STATIC MUSL, not the distro's dynamic glibc, and the size rows are
# why: mooncc's binary is a static ELF carrying its own nolibc, so racing it against a
# dynamic binary asks two questions at once and answers neither -- ~40 KB of the gap it
# used to report was glibc being ABSENT from the file. Runtime is untouched by the
# choice (measured: under 0.1% on insns, cycles and boot -- love runs on its own floor,
# so libc barely executes), so the corpus rows compare straight across the change.
# CCGLIBC=1 adds the old dynamic gcc/clang lanes back alongside, for continuity.
#
# Emits "<phase> <compiler> <ms> <note>" lines (the 4-field satrace shape), so
# mkhtml renders it like the SAT table -- one row per phase, columns the compilers,
# net their sum (source to a tested and measured binary). A missing/failed lane
# shows dnf.
#
# Requires `make host` first: the generated out/lib/*.h headers and out/host/love, which
# is also the ARTIFACT (the seed) -- the mooncc lane runs it and not an intermediate;
# see the note on SEED.
# x86-64 only (mooncc's native lane); off x86-64, or with no artifact built, the mooncc
# cells read dnf and gcc/clang are still raced.
#
# ⚠ THE net ROW IS A SUM OVER EVERY PHASE, so adding rows moves it and results either
# side of a row change do not compare. The per-row ratios do.
#
# usage: ./ccbench.sh [timeout-seconds] [samples]
#   build is timed once (a stable multi-second cost, and the artifact is reused);
#   test subtracts two medians of `samples` runs each (corpus, then empty boot), default 3.
# resolve the repo root ABSOLUTELY: the build lanes cd into it to reach the source
# globs (src/love.c, host/*.c, crew/...), so every output/include path below must be absolute.
R=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
TIMEOUT=${1:-180}
SAMPLES=${2:-3}
ho=$R/out/host
WORK=$R/out/bench/cc
rm -rf "$WORK"; mkdir -p "$WORK"

# HONEST build times: a distro often symlinks gcc/cc/clang through ccache, which would
# clock a warm cache HIT (~100ms) instead of the compiler doing its -O2 work -- and
# mooncc has no such cache, so the race would be rigged. CCACHE_DISABLE makes the ccache
# shim pass straight through to the real compiler, every unit compiled for real.
export CCACHE_DISABLE=1

# the corpus, byte-identical to mk/common.mk's `t`: 00-init/spec/uu front-loaded, then
# every other test/*.l in byte order (glaze-x86 excluded -- it needs emit.l ahead and
# runs x86 native under its own guard). The Makefile passes it in $CORPUS; recompute
# it for a standalone run.
CORPUS=${CORPUS:-"$R/test/00-init.l $R/test/spec.l $R/test/uu.l $(ls "$R"/test/*.l 2>/dev/null | grep -vE '/(00-init|spec|glaze-x86|uu)\.l$' | LC_ALL=C sort)"}

# the host's real C flags come from the Makefile ($(ai_cflags)); fall back to a
# matching set (mk/common.mk) for a standalone run.
if [ -z "$LOVE_CFLAGS" ]; then
  LOVE_CFLAGS="-std=c11 -g -O2 -pipe -Wall -Wextra -Werror -Wstrict-prototypes -Wno-unused-parameter -Wmissing-field-initializers -Wno-implicit-fallthrough -falign-functions=16 -fomit-frame-pointer -fno-stack-check -fno-stack-protector -fno-exceptions -fno-asynchronous-unwind-tables"
  LOVE_CFLAGS="$LOVE_CFLAGS -fcf-protection=none -D_POSIX_C_SOURCE=200809L"
fi
# drop -Werror: this table times compile+link, and -Werror is a lint GATE, not a
# codegen or speed factor. Keeping it would bench a compiler's warning set, not its
# throughput -- gcc's -Wall flags a benign construct in src/love.c (-Wmisleading-indentation)
# that clang doesn't, and that shouldn't scratch it from a SPEED race.
CFLAGS="$(printf '%s' "$LOVE_CFLAGS" | sed 's/-Werror//g') -Dai_tco=1 -fpic -I$ho -I$R -I$R/core -I$R/out/lib"
# mk/common.mk's $(data_ld), which a bench link owes exactly as a host link does: the data
# sentinels' tiling IS src/love.h's ai_typ, and ld left to itself keeps each love_data.N an
# orphan in first-encountered order -- gcc emits love_data.7 first, so lvm_str lands
# below lvm_sym and every string reads as a closure.
LDFLAGS="-Wl,-T,$R/src/love_data.ld"

# wall-clock (ms) of a command; echoes just the number. Runs in a subshell so a cd can't leak.
wall() { t0=$(date +%s.%N); ( eval "$1" ) >/dev/null 2>&1; t1=$(date +%s.%N)
         awk -v a="$t0" -v b="$t1" 'BEGIN{printf "%.1f",(b-a)*1000}'; }
# median (ms) of running CMD $SAMPLES times.
med() { i=0; while [ "$i" -lt "$SAMPLES" ]; do wall "$1"; echo; i=$((i+1)); done \
        | sort -n | awk '{v[NR]=$0} END{print v[int((NR+1)/2)]}'; }

# -- gcc / clang: the ordinary lane. Compile the liblove.a translation units (src/love.c +
#    am.c) and the host/*.c glob (main.c carries the egg), then link the objects. --
build_cc() { # $1=compiler $2=binpath $3=extra flags ; objects under $WORK/o-<binname>
  cc=$1; bin=$2; xf=$3; od=$WORK/o-$(basename "$bin")   # o- prefix: $bin itself lives in $WORK
  rm -rf "$od"; mkdir -p "$od/host"
  ( cd "$R" || exit 1
    $cc $CFLAGS $xf -c src/love.c                    -o "$od/love.o" || exit 1
    $cc $CFLAGS $xf -c crew/moon/lib/math/am.c -o "$od/am.o" || exit 1
    for f in host/*.c; do b=$(basename "$f" .c)
      $cc $CFLAGS $xf -c "$f" -o "$od/host/$b.o" || exit 1; done
    $cc $CFLAGS $xf $LDFLAGS -o "$bin" "$od"/love.o "$od"/am.o "$od"/host/*.o ) || return 1
}

# -- mooncc: the WHOLE toolchain in love, verbatim from `make test_raw`. mooncc -c each
#    unit, mksys the syscall leaf, our linker binds. -I$ho picks up the lcat'd headers. --
# ⚠ THE COMPILER IS THE SHIPPED ARTIFACT, and it is not a preference -- it is the only
# spelling of this lane that measures the same thing twice. mooncc's link pulls
# crew/moon/lib/nolibc/ MEMBER BY NEED and caches the archive under ~/.love/cache/moon,
# keyed on the compiler, its stat, AND ITS IMAGE (moon.l's mcrtkey). An image FILE puts
# that file's stat in the key, so while the lane ran out of out/host/mooncc -- whose
# .image this file's own make target rebuilt as a prerequisite -- every run missed and
# paid a one-time libc BUILD inside a per-build row: 43.8 s against 20.1 s warm, 54% of
# the number. A BAKED image keys as the word "<baked>" instead, so the entry survives
# every rebuild of the intermediates (measured then: `touch out/host/mooncc.image
# out/host/love` left it at 18.9 s). The one-binary change has since retired that image
# file, which closes the same hole from the other side -- but the artifact is still what
# this should race, because it is what a user runs. ⚠ a one-line C file does NOT warm the
# archive in its place: a program that needs no member pulls none.
# ⚠ LOVE_NO_IMAGE= (empty = UNSET) leads, the guard against an exported egg: an
# egg-booted love has no verb table, so `mooncc` reads as a FILENAME.
SEED=$R/out/host/love
mc() { env LOVE_NO_IMAGE= "$SEED" mooncc "$@"; }
build_mooncc() { # $1=binpath
  bin=$1; od=$WORK/mooncc; rm -rf "$od"; mkdir -p "$od"
  ( cd "$R" || exit 1
    mc -D ai_tco=1 -Iout/host -I. -Icore -Iout/lib -c src/love.c "$od/love.o" || exit 1
    for f in host/*.c; do b=$(basename "$f" .c)
      mc -D ai_tco=1 -Iout/host -I. -Icore -Iout/lib -c "$f" "$od/$b.o" || exit 1; done
    # no nolibc object: the link owes its symbols and the driver supplies them
    # member by need, so the dead areas never arrive. ⚠ ccsize/ccdead therefore
    # read mooncc's libc off the BINARY's complement, not off a nolibc.o.
    for f in crew/moon/lib/math/*.c; do b=$(basename "$f" .c)
      mc -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$od/m_$b.o" || exit 1; done
    { cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l \
          crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
      echo "((from 'moon 'mksys) \"$od/sys.o\")"; } | env LOVE_NO_IMAGE= "$SEED" || exit 1
    mc "$od"/*.o -o "$bin" ) || return 1
}

# the corpus as ONE file, fed by REDIRECT. It arrives on stdin either way (which keeps
# the one-global-scope property), but a redirect is seekable and a pipe is not, and only
# a seekable fd 0 gets a read run (src/main.c). Piping still costs 953K reads over this
# corpus -- one per byte, which no pipe can be spared -- and syscall time is the SAME work
# in all three lanes: kernel, not codegen, so it only dilutes what this table is seeing.
CORPUS1=$WORK/corpus.l
cat $CORPUS > "$CORPUS1"

# does $1 pass the corpus? (exit 0 AND the zz-fin sentinel). Guards against timing a
# binary that silently reader-stops or crashes mid-corpus.
passes() { out=$(LOVE_NO_IMAGE=1 timeout "$TIMEOUT" "$1" < "$CORPUS1" 2>&1); r=$?
           [ $r -eq 0 ] && printf '%s' "$out" | grep -q "tests pass"; }

# the corpus's OWN run time, boot EXCLUDED. Every fresh binary egg-boots (evals the
# whole compiler out of the egg -- ~seconds, no baked image). A baked fast-wake image
# would skew ONE lane; a level field egg-boots all three and subtracts that fixed cost.
# So subtract it: full = wall(corpus | bin); boot = wall(empty | bin); the suite's
# execution is full - boot. Both egg-boot identically, so the fixed cost cancels and
# what's left is the tests actually running -- the same method for all three compilers.
corpus_ms() { # $1=binpath ; median full, median boot, report max(0, full-boot)
  bin=$1
  full=$(med "LOVE_NO_IMAGE=1 $bin < $CORPUS1")
  boot=$(med "LOVE_NO_IMAGE=1 $bin </dev/null")
  awk -v f="$full" -v b="$boot" 'BEGIN{d=f-b; printf "%.1f", d<0?0:d}'
}

# a workload driver's own run time, boot excluded the same way. The reps are fixed
# in the .l, so every compiler does identical work. dnf if the sentinel never printed
# -- a lane that answered nothing must not be timed as if it were fast.
CRYPTO=$R/bench/ccrypto.l
NIFS=$R/bench/cnifs.l
drv_ms() { # $1=binpath $2=driver-file $3=driver-call $4=sentinel
  bin=$1; df=$2; drv=$3
  { cat "$df"; echo "$drv"; } > "$WORK/drv.run.l"
  out=$(LOVE_NO_IMAGE=1 timeout "$TIMEOUT" "$bin" < "$WORK/drv.run.l" 2>&1) || { echo dnf; return; }
  printf '%s' "$out" | grep -q "$4" || { echo dnf; return; }
  full=$(med "LOVE_NO_IMAGE=1 $bin < $WORK/drv.run.l")
  boot=$(med "LOVE_NO_IMAGE=1 $bin </dev/null")
  awk -v f="$full" -v b="$boot" 'BEGIN{d=f-b; printf "%.1f", d<0?0:d}'
}

# the inflate row's input, laid ONCE by the already-built host love -- lib/gz.l is a
# module and the lane binaries have no module path, so the stream cannot be made where
# it is used. INFN is the inflated size, handed to the nif so it allocates once.
# ⚠ if this fails the inflate row is dnf and the other two are unaffected: a missing
# stream must not read as a compiler that could not build.
INF=$WORK/bench.deflate
INFN=$(cd "$R" && out/host/love bench/ccgen.l src/love.c "$INF" 2>/dev/null)
case $INFN in ''|*[!0-9]*) INFN=0;; esac

# one compiler lane: build (timed once), verify, then time the corpus and the two
# cipher rows (boot excluded from each).
lane() { # $1=label $2=builder-cmd $3=binpath $4=extra cflags (build_cc only)
  lbl=$1; bld=$2; bin=$3
  bt=$(wall "$bld '$bin' '$4'")
  if [ ! -f "$bin" ] || [ ! -x "$bin" ]; then dnf_lane "$lbl"; return; fi
  echo "build $lbl $bt ok"; LIVE=$((LIVE + 1))            # the liveness tally, read at the end
  if passes "$bin"; then echo "test $lbl $(corpus_ms "$bin") ok"
  else echo "test $lbl dnf"; fi
  crow chacha   "$lbl" "$(drv_ms "$bin" "$CRYPTO" '(cc-run ())' 'ccrypto chacha: ok')"
  crow poly1305 "$lbl" "$(drv_ms "$bin" "$CRYPTO" '(po-run ())' 'ccrypto poly1305: ok')"
  if [ "$INFN" -gt 0 ]; then
    crow inflate "$lbl" "$(drv_ms "$bin" "$NIFS" "(inf-run \"$INF\" $INFN 300)" 'cnifs inflate: ok')"
  else echo "inflate $lbl dnf"; fi
  crow crc32    "$lbl" "$(drv_ms "$bin" "$NIFS" '(crc-run ())' 'cnifs crc32: ok')"
  crow sha256   "$lbl" "$(drv_ms "$bin" "$NIFS" '(sha-run ())' 'cnifs sha256: ok')"
}
crow() { case $3 in dnf) echo "$1 $2 dnf";; *) echo "$1 $2 $3 ok";; esac; }
dnf_lane() { for ph in build test chacha poly1305 inflate crc32 sha256; do echo "$ph $1 dnf"; done; }

# ⚠ A LANE THAT CANNOT BUILD REPORTS dnf, WHICH MEANS A BROKEN HARNESS RENDERS AS A
# WELL-FORMED TABLE OF NOTHING. that is not hypothetical: the 2026-08-15 reorg broke the
# root resolution and the -Icore seam, and twelve dnf rows sat in the cached result for a
# day with the corpus reading simply unavailable. one missing compiler is a legitimate
# skip; ZERO lanes is the harness, and it exits 1 below.
LIVE=0

if [ "$(uname -m)" = x86_64 ] && [ -x "$SEED" ]; then
  # ⚠ AND ONE UNTIMED BUILD BEFORE THE TIMED ONE, which the note on SEED explains: the
  # artifact's cache entry survives everything but a NEW ARTIFACT, and after `make dist`
  # the first link builds the runtime. gcc and clang link a musl somebody else compiled,
  # so holding mooncc to the same shape means its libc is built too, not built inside the
  # row. Costs one build on a ten-minute table and makes the row mean one thing.
  build_mooncc "$WORK/love-warm" >/dev/null 2>&1
  lane mooncc build_mooncc "$WORK/love-mooncc"
else
  dnf_lane mooncc                        # x86-64 only, and it needs the artifact built
fi
# the native lanes: static musl. The wrappers hand the compiler musl's headers and crt,
# so the translation units are the identical job -- only the libc differs, and -static
# puts it inside the binary where mooncc's nolibc already is.
for c in gcc clang; do
  if command -v "musl-$c" >/dev/null 2>&1; then
    lane "$c-musl" "build_cc musl-$c" "$WORK/love-$c-musl" -static
  else
    dnf_lane "$c-musl"
  fi
done
# CCGLIBC=1: the old dynamic-glibc lanes, kept for continuity with the fills that
# predate the switch. Not the comparison the size rows want -- see the ⚠ at the top.
if [ -n "$CCGLIBC" ]; then
  for c in gcc clang; do
    if command -v "$c" >/dev/null 2>&1; then
      lane "$c" "build_cc $c" "$WORK/love-$c"
    else
      dnf_lane "$c"
    fi
  done
fi

[ "$LIVE" -gt 0 ] || { echo "ccbench: every lane dnf -- the harness, not the compilers" >&2
                       exit 1; }
