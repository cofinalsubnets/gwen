#!/bin/sh
# setup.sh -- the COLD-START row. Median wall-clock to go from SOURCE to a trivial result for each
# language: compiled langs pay their compile (rust/java recompile each run; go hits its build cache
# after the first), interpreted/JIT langs pay process startup + warmup. This is the fixed per-run
# cost the per-iteration table deliberately excludes -- where love's baked-egg image (a tiny native
# binary, no compile, no VM warmup) shows against the JVM/julia startup and the compile-heavy langs.
#
# Emits `setup <lang> 1 <median-ms> 0` lines -- the 5-field bench-stream format -- so mkhtml.sh
# renders it as a row below the net (NORANK: it is not a per-iteration time). A language whose
# toolchain is absent is skipped (a dotted cell), exactly like the main harness.
#
# usage: ./setup.sh [samples]   (default 5; the median of that many cold-ish runs)
R=..
SAMPLES=${1:-5}
T=$R/out/bench/setup-tmp
mkdir -p "$T"

# minimal valid program per language -- does ~nothing, so the time IS the setup.
printf '0\n'                                                    > "$T/t.l"
printf 'package main\nfunc main(){}\n'                          > "$T/t.go"
printf 'fn main(){}\n'                                          > "$T/t.rs"
printf 'public class T{public static void main(String[] a){}}\n'> "$T/T.java"
: > "$T/t.jl"; : > "$T/t.lua"; : > "$T/t.py"; : > "$T/t.js"; : > "$T/t.exs"; : > "$T/t.ss"; : > "$T/t.lisp"

# median (ms) of running CMD $SAMPLES times, each in its own subshell (so a `cd` can't leak).
med() {
  i=0
  while [ "$i" -lt "$SAMPLES" ]; do
    t0=$(date +%s.%N); ( eval "$1" ) >/dev/null 2>&1; t1=$(date +%s.%N)
    awk -v a="$t0" -v b="$t1" 'BEGIN{printf "%.3f\n",(b-a)*1000}'
    i=$((i+1))
  done | sort -n | awk '{v[NR]=$0} END{print v[int((NR+1)/2)]}'
}
emit() { command -v "$2" >/dev/null 2>&1 || return; echo "setup $1 1 $(med "$3") 0"; }

# love measures its REAL cold start: the binary wakes its baked .image section (a precompiled, glaze-baked heap
# snapshot, mmap'd + relocated -- no egg eval), ~4 ms. `unset LOVE_NO_IMAGE` defeats the Makefile-wide
# suppression (the per-iteration harness sets it for determinism; the cold-start row wants the image).
emit love     "$R/out/host/love" "unset LOVE_NO_IMAGE; $R/out/host/love $T/t.l"
emit go     go               "cd $T && go run t.go"
emit rust   rustc            "rustc -O $T/t.rs -o $T/t.rsbin && $T/t.rsbin"
emit java   javac            "cd $T && javac T.java && java T"
emit julia  julia            "julia --startup-file=no $T/t.jl"
emit luajit luajit           "luajit $T/t.lua"
emit pypy   pypy3            "pypy3 $T/t.py"
emit node   node             "node $T/t.js"
emit elixir elixir           "elixir $T/t.exs"
emit chez   chez             "chez --script $T/t.ss"
emit sbcl   sbcl             "sbcl --script $T/t.lisp"
