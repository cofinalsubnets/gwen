#!/bin/sh
# ccbench.sh -- the COMPILER shootout (the page's FOURTH table). Builds the love host
# binary with three C compilers and, for each, reports four wall-clock costs:
#   build : compile every C translation unit (love.c + host/*.c + the am math floor)
#           and link a working `love` -- source to runnable binary.
#   test  : run the full arch-neutral corpus ($t, the same files test_host/test_raw
#           feed) through the binary that build produced, with egg-boot EXCLUDED
#           (subtracted) so it times the suite executing, not the compiler self-install.
#   chacha / poly1305 : one C function each, same subtraction (bench/ccrypto.l).
#           These are here because the corpus row averages a compiler's work over all
#           of love.c, and the average is flattering: mooncc/clang reads ~1.1x there
#           and ~23x on chacha. chacha20 indexes a 16-word state ARRAY in its inner
#           loop, poly1305 keeps five limbs as scalar LOCALS, and mooncc has register
#           residency for the second shape only -- so the PAIR is the reading. Wide
#           chacha beside narrow poly says the gap is array slots; the day they close
#           together is the day that reading was wrong.
# The three compilers:
#   mooncc : love's OWN C compiler (crew/moon/), the exact `make test_raw` sequence --
#            no gcc/glibc/ld anywhere: mooncc lays every .o, mksys emits the syscall
#            leaf, our linker (crew/holo/) binds. It egg-boots (no baked image).
#   gcc / clang : the same translation units at the host's real -O2 cflags, linked
#            the ordinary way. Also egg-boot -- no `--bake`, so all three lanes run
#            the identical corpus off the freshly-eval'd egg (a level field).
#
# Emits "<phase> <compiler> <ms> <note>" lines (the 4-field satrace shape), so
# mkhtml renders it like the SAT table -- one row per phase, columns the compilers,
# net their sum (source to a tested and measured binary). A missing/failed lane
# shows dnf.
#
# Requires `make host` first: the generated out/lib/*.h headers and, for the mooncc
# lane, out/host/mooncc(+.image). x86-64 only (mooncc's native lane); off x86-64 it
# prints every row with the mooncc cells dnf and gcc/clang still raced.
#
# usage: ./ccbench.sh [timeout-seconds] [samples]
#   build is timed once (a stable multi-second cost, and the artifact is reused);
#   test subtracts two medians of `samples` runs each (corpus, then empty boot), default 3.
# resolve the repo root ABSOLUTELY: the build lanes cd into it to reach the source
# globs (love.c, host/*.c, crew/...), so every output/include path below must be absolute.
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

# the corpus, byte-identical to common.mk's `t`: 00-init/spec/uu front-loaded, then
# every other test/*.l in byte order (glaze-x86 excluded -- it needs emit.l ahead and
# runs x86 native under its own guard). The Makefile passes it in $CORPUS; recompute
# it for a standalone run.
CORPUS=${CORPUS:-"$R/test/00-init.l $R/test/spec.l $R/test/uu.l $(ls "$R"/test/*.l 2>/dev/null | grep -vE '/(00-init|spec|glaze-x86|uu)\.l$' | LC_ALL=C sort)"}

# the host's real C flags come from the Makefile ($(ai_cflags)); fall back to a
# matching set (common.mk) for a standalone run. -std probe = clang/gcc gnu23 else gnu2x.
if [ -z "$LOVE_CFLAGS" ]; then
  std=$(printf 'int main(void){return 0;}' | cc -std=gnu23 -x c -c -o /dev/null - 2>/dev/null && echo gnu23 || echo gnu2x)
  LOVE_CFLAGS="-std=$std -g -O2 -pipe -Wall -Wextra -Werror -Wstrict-prototypes -Wno-unused-parameter -Wmissing-field-initializers -Wno-implicit-fallthrough -falign-functions=16 -fomit-frame-pointer -fno-stack-check -fno-stack-protector -fno-exceptions -fno-asynchronous-unwind-tables"
  [ "$(uname -s)" = Darwin ] || LOVE_CFLAGS="$LOVE_CFLAGS -fcf-protection=none"
fi
# drop -Werror: this table times compile+link, and -Werror is a lint GATE, not a
# codegen or speed factor. Keeping it would bench a compiler's warning set, not its
# throughput -- gcc's -Wall flags a benign construct in love.c (-Wmisleading-indentation)
# that clang doesn't, and that shouldn't scratch it from a SPEED race.
CFLAGS="$(printf '%s' "$LOVE_CFLAGS" | sed 's/-Werror//g') -Dai_tco=1 -fpic -I$ho -I$R -I$R/out/lib"

# wall-clock (ms) of a command; echoes just the number. Runs in a subshell so a cd can't leak.
wall() { t0=$(date +%s.%N); ( eval "$1" ) >/dev/null 2>&1; t1=$(date +%s.%N)
         awk -v a="$t0" -v b="$t1" 'BEGIN{printf "%.1f",(b-a)*1000}'; }
# median (ms) of running CMD $SAMPLES times.
med() { i=0; while [ "$i" -lt "$SAMPLES" ]; do wall "$1"; echo; i=$((i+1)); done \
        | sort -n | awk '{v[NR]=$0} END{print v[int((NR+1)/2)]}'; }

# -- gcc / clang: the ordinary lane. Compile the liblove.a translation units (love.c +
#    am.c) and the host/*.c glob (main.c carries the egg), then link the objects. --
build_cc() { # $1=compiler $2=binpath ; leaves objects under $WORK/<compiler>
  cc=$1; bin=$2; od=$WORK/$(basename "$cc")
  rm -rf "$od"; mkdir -p "$od/host"
  ( cd "$R" || exit 1
    $cc $CFLAGS -c love.c                    -o "$od/love.o" || exit 1
    $cc $CFLAGS -c crew/moon/lib/math/am.c -o "$od/am.o" || exit 1
    for f in host/*.c; do b=$(basename "$f" .c)
      $cc $CFLAGS -c "$f" -o "$od/host/$b.o" || exit 1; done
    $cc $CFLAGS -o "$bin" "$od"/love.o "$od"/am.o "$od"/host/*.o ) || return 1
}

# -- mooncc: the WHOLE toolchain in love, verbatim from `make test_raw`. mooncc -c each
#    unit, mksys the syscall leaf, our linker binds. -I$ho picks up the lcat'd headers. --
MC="$ho/mooncc"
build_mooncc() { # $1=binpath
  bin=$1; od=$WORK/mooncc; rm -rf "$od"; mkdir -p "$od"
  ( cd "$R" || exit 1
    "$MC" -D ai_tco=1 -Iout/host -I. -Iout/lib -c love.c "$od/love.o" || exit 1
    for f in host/*.c; do b=$(basename "$f" .c)
      "$MC" -D ai_tco=1 -Iout/host -I. -Iout/lib -c "$f" "$od/$b.o" || exit 1; done
    "$MC" -Icrew/moon/include -c crew/moon/lib/nolibc.c "$od/nolibc.o" || exit 1
    for f in crew/moon/lib/math/*.c; do b=$(basename "$f" .c)
      "$MC" -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$od/m_$b.o" || exit 1; done
    { cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l \
          crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
      echo "(mksys \"$od/sys.o\")"; } | out/host/love || exit 1
    "$MC" "$od"/*.o -o "$bin" ) || return 1
}

# the corpus as ONE file, fed by REDIRECT. It arrives on stdin either way (which keeps
# the one-global-scope property), but a redirect is seekable and a pipe is not, and only
# a seekable fd 0 gets a read run (host/main.c). Piping still costs 953K reads over this
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

# a ccrypto.l driver's own run time, boot excluded the same way. The reps are fixed
# in the .l, so every compiler does identical work. dnf if the sentinel never printed
# -- a lane that answered nothing must not be timed as if it were fast.
CRYPTO=$R/bench/ccrypto.l
crypto_ms() { # $1=binpath $2=driver-call $3=sentinel
  bin=$1; drv=$2
  { cat "$CRYPTO"; echo "$drv"; } > "$WORK/ccrypto.run.l"
  out=$(LOVE_NO_IMAGE=1 timeout "$TIMEOUT" "$bin" < "$WORK/ccrypto.run.l" 2>&1) || { echo dnf; return; }
  printf '%s' "$out" | grep -q "$3" || { echo dnf; return; }
  full=$(med "LOVE_NO_IMAGE=1 $bin < $WORK/ccrypto.run.l")
  boot=$(med "LOVE_NO_IMAGE=1 $bin </dev/null")
  awk -v f="$full" -v b="$boot" 'BEGIN{d=f-b; printf "%.1f", d<0?0:d}'
}

# one compiler lane: build (timed once), verify, then time the corpus and the two
# cipher rows (boot excluded from each).
lane() { # $1=label $2=builder-cmd $3=binpath
  lbl=$1; bld=$2; bin=$3
  bt=$(wall "$bld '$bin'")
  if [ ! -x "$bin" ]; then dnf_lane "$lbl"; return; fi
  echo "build $lbl $bt ok"
  if passes "$bin"; then echo "test $lbl $(corpus_ms "$bin") ok"
  else echo "test $lbl dnf"; fi
  crow chacha   "$lbl" "$(crypto_ms "$bin" '(cc-run ())' 'ccrypto chacha: ok')"
  crow poly1305 "$lbl" "$(crypto_ms "$bin" '(po-run ())' 'ccrypto poly1305: ok')"
}
crow() { case $3 in dnf) echo "$1 $2 dnf";; *) echo "$1 $2 $3 ok";; esac; }
dnf_lane() { for ph in build test chacha poly1305; do echo "$ph $1 dnf"; done; }

if [ "$(uname -m)" = x86_64 ] && [ -x "$MC" ]; then
  lane mooncc build_mooncc "$WORK/love-mooncc"
else
  dnf_lane mooncc                                   # mooncc's native lane is x86-64 only
fi
for c in gcc clang; do
  if command -v "$c" >/dev/null 2>&1; then
    lane "$c" "build_cc $c" "$WORK/love-$c"
  else
    dnf_lane "$c"
  fi
done
