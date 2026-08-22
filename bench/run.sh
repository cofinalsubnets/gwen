#!/bin/sh
# run.sh <lang> <benches> [timeout] [skip] [samples] -- run one language across
# the named benches.
#
# Emits one raw "<name> <lang> <reps> <ms> <chk>" result line per bench on stdout
# (the Makefile redirects them into out/bench/<lang>.txt) and a progress tick per
# bench on stderr. Each bench is run `samples` times (default 5); the line whose
# per-iteration time (ms/reps) is the MEDIAN of the samples is the one reported.
# Median -- not min -- is the honest central tendency for a GC'd language: a GC
# pause is intrinsic cost, so min would catch the runs that happened to dodge a
# collection and SYSTEMATICALLY under-report it, biasing toward high-variance
# collectors (the artifact that made a generational build look like a `hash`
# regression). Median is robust to BOTH the GC-lucky lows AND OS-noise highs.
# Each sample runs under `timeout` (default 30s); a bench that times out or
# errors on every sample produces no line, so a language that can't finish a
# workload drops that cell rather than failing the build. `skip` is a
# space-separated list of <lang>:<bench> pairs known to time out; those are
# dropped up front, so the build never pays the timeout wall for them. If the
# interpreter/compiler isn't on PATH the whole language is skipped. BENCH_LANG is
# exported so the shared harnesses (lib/bench.* + lib/Bench.*) print the right
# column label.
lang=$1
benches=$2
to=${3:-30}
skip=" ${4:-} "   # padded so `case` can match whole " lang:bench " words
samples=${5:-5}   # timed windows per bench; the median ms/it window is reported

# per-language file extension, interpreter binary, and run command. the command
# is eval'd with $b bound to the bench name, so the source is benches/$b.$ext.
case $lang in
  # love: the native glaze (love/glaze/{emit,auto}.l) is BAKED into out/host/love and active by default, so the
  # bench just runs through the binary -- no prepend. (`make bench` depends on `host`, so the baked glaze
  # is always fresh.) We do NOT cat the glaze source ahead of the bench: re-loading the glaze on top of
  # the baked glaze is a redundant DOUBLE-LOAD that faults under autospec; the baked binary is the truth.
  # cdcl alone prepends the SAT solver library it needs (not the glaze).
  love)            ext=l;    bin=../out/host/love;  cmd='cat $([ "$b" = cdcl ] && printf "%s " ../crew/sat/sat.l) bench.l benches/$b.l | LOVE_NO_IMAGE=1 ../out/host/love' ;;
  chez)         ext=ss;   bin=chez;       cmd='chez --script benches/$b.ss' ;;
  sbcl)         ext=lisp; bin=sbcl;       cmd='sbcl --script benches/$b.lisp' ;;
  apl)          ext=apl;  bin=dyalogscript; cmd='dyalogscript benches/$b.apl' ;;   # Dyalog APL (~/.local install); the bench ⎕FIXes lib/bench.apl
  elixir)       ext=exs;  bin=elixir;     cmd='elixir benches/$b.exs' ;;
  julia)        ext=jl;   bin=julia;      cmd='julia --startup-file=no benches/$b.jl' ;;
  pypy)         ext=py;   bin=pypy3;      cmd='pypy3 benches/$b.py' ;;
  node)         ext=js;   bin=node;       cmd='node benches/$b.js' ;;
  luajit)       ext=lua;  bin=luajit;     cmd='luajit benches/$b.lua' ;;
  # compiled languages: build to a scratch dir (the compile is NOT timed -- each
  # bench self-times its inner loop), run, clean up. A compile failure produces no
  # output, so run.sh drops the cell exactly as it does for a missing source file.
  go)           ext=go;   bin=go;         cmd='d=$(mktemp -d); cp benches/$b.go "$d/main.go"; cp lib/bench.go "$d/bench.go"; go run "$d/main.go" "$d/bench.go"; rm -rf "$d"' ;;
  rust)         ext=rs;   bin=rustc;      cmd='d=$(mktemp -d); rustc -O -o "$d/b" benches/$b.rs >/dev/null 2>&1 && "$d/b"; rm -rf "$d"' ;;
  java)         ext=java; bin=javac;      cmd='d=$(mktemp -d); javac -d "$d" benches/$b.java lib/Bench.java >/dev/null 2>&1 && java -cp "$d" Main; rm -rf "$d"' ;;
  # lean: compile the shared harness (as module `Bench`, via -R lib) and the
  # bench to C, link into a native executable with leanc (from the toolchain
  # prefix; only `lean` need be on PATH), then run. The build is NOT timed -- the
  # bench self-times its inner loop, like go/rust/java.
  lean)         ext=lean; bin=lean;       cmd='d=$(mktemp -d); lc="$(lean --print-prefix)/bin/leanc"; lean -R lib -o "$d/Bench.olean" -c "$d/Bench.c" lib/Bench.lean >/dev/null 2>&1 && "$lc" -O3 -c -o "$d/Bench.o" "$d/Bench.c" >/dev/null 2>&1 && LEAN_PATH="$d" lean -o "$d/m.olean" -c "$d/m.c" benches/$b.lean >/dev/null 2>&1 && "$lc" -O3 -o "$d/m" "$d/m.c" "$d/Bench.o" >/dev/null 2>&1 && "$d/m"; rm -rf "$d"' ;;
  *) echo "run.sh: unknown language '$lang'" >&2; exit 1 ;;
esac

if ! command -v "$bin" >/dev/null 2>&1; then
  echo "  $lang: $bin not found, skipped" >&2
  exit 0
fi

export BENCH_LANG=$lang b   # $b is read by $cmd inside `sh -c` below
for b in $benches; do
  [ -f "benches/$b.$ext" ] || continue
  case $skip in *" $lang:$b "*)                     # known-timeout pair: don't attempt
    printf '  %-13s %-10s (dropped: known timeout)\n' "$lang" "$b" >&2; continue ;;
  esac
  printf '  %-13s %-10s' "$lang" "$b" >&2          # start the line (no newline)
  acc=''                                            # collect one result line per sample
  i=0
  while [ "$i" -lt "$samples" ]; do
    i=$((i + 1))
    o=$(timeout "$to" sh -c "$cmd" 2>/dev/null)
    [ -n "$o" ] && acc="$acc$o
"
  done
  if [ -n "$acc" ]; then
    # report the sample whose per-iteration time (ms/reps) is the median. keying on
    # ms/reps -- not raw ms -- normalizes the auto-scaled rep count, which can differ
    # between samples near the MIN_MS boundary; the emitted line is one real run, so
    # its reps+ms stay internally consistent for report.awk (which divides ms/reps).
    med=$(printf '%s' "$acc" | awk '
      NF == 5 && $3 + 0 > 0 { line[++n] = $0; key[n] = $4 / $3 }
      END {
        if (!n) exit
        for (i = 1; i <= n; i++) ord[i] = i        # insertion-sort indices by ms/reps (n is tiny)
        for (i = 2; i <= n; i++) {
          v = ord[i]; j = i - 1
          while (j >= 1 && key[ord[j]] > key[v]) { ord[j + 1] = ord[j]; j-- }
          ord[j + 1] = v }
        print line[ord[int((n + 1) / 2)]] }')
    if [ -n "$med" ]; then
      printf '%s\n' "$med"                          # the median result line -> stdout (the .txt)
      printf ' %s ms (median/%s)\n' "$(printf '%s\n' "$med" | awk '{print $4}')" "$samples" >&2
    else
      printf ' (skipped: no valid sample)\n' >&2
    fi
  else
    printf ' (skipped: >%ss or error)\n' "$to" >&2
  fi
done
