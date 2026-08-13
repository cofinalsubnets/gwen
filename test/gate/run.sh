#!/bin/sh
# test/gate/run.sh -- the corpus-gate shape, in one place: run a love over some source, keep
# the WHOLE output, and hold it to exit 0 AND every sentinel. A red prints what was said and
# names which half failed -- the exit or the missing line.
#
# usage: <source> | run.sh NAME LOVE SENTINEL[|SENTINEL...]
#        run.sh -a NAME LOVE SENTINEL[|SENTINEL...] FILE...
#
# By default the source arrives on stdin, which is how the corpus keeps its one global scope
# (the caller cats what it wants together). -a hands the FILES to the love as arguments with
# stdin closed instead -- for a gate whose subject must not be cat'd in, and for the lanes
# that test stdin itself.
#
# /warn LOVE is a word LIST, not a path: `$(mw)` is `env -u LOVE_NO_IMAGE out/host/love`, so
# it must go unquoted here. The caller quotes it as one argument; this splits it back.
set -u

a=
[ "$1" = -a ] && { a=1; shift; }
n=$1 love=$2 sen=$3
shift 3

# ⚠ `-l /dev/stdin`, not the bare stdin lane -- the corpus still arrives on the SAME pipe,
# in one stream, in one scope. What changes is which port reads it: a pipe is deliberately
# unbuffered (host/main.c's stdin_take lends a read run only to a SEEKABLE stdin, since you
# cannot put back what you over-read from a pipe), so the reader gulps a piped corpus ONE
# BYTE AT A TIME -- ~1940 instructions a byte against ~175 through an ordinary buffered
# port. Opened by name it gets its own buffer: ~3.4% off each gate, and the ownership
# contract does not apply because a gate hands stdin to nobody afterwards.
# (Strictness is UNCHANGED -- both lanes quit 1 on a scare, a missing name and a failed
# assert alike. Only the read path differs.)
o=out/host/.test_$n.out
if [ -n "$a" ]; then $love "$@" < /dev/null > "$o" 2>&1; else $love -l /dev/stdin > "$o" 2>&1; fi
r=$?
cat "$o"
[ $r -eq 0 ] || { echo "FAIL $n (exit $r)"; exit 1; }

IFS='|'
for s in $sen; do
  grep -q "$s" "$o" || { echo "FAIL $n (exit 0, but no \"$s\")"; exit 1; }
done
