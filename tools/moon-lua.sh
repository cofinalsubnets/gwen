#!/bin/sh
# moon-lua.sh -- build Lua 5.4 with mooncc + nolibc + the holo linker (no
# gcc/glibc/ld) and prove it RUNS: a battery over closures, strings, tables,
# the math floor (am.c under the libc faces), integer/bitwise ops, pcall +
# coroutines (setjmp/longjmp through sys.o's leaves), metatables, gc, os
# time/date (gmtime/mktime/strftime), io, and load. The fifth moon-userland
# rung (doc/moon-userland.md), after bzip2, gzip, tar and m4 -- and the first
# where EVERY package source compiles unpatched (35/35 after the paren-
# declarator + braced-string-literal rungs).
#
# TWO TARGETS, one procedure (raw.sh's shape): `moon-lua.sh` builds the native
# x86-64 lane, `moon-lua.sh arm64` cross-compiles the same sources with
# `mooncc -t arm64` and runs the battery under qemu-aarch64. The cross lane
# SKIPS cleanly without qemu, like test_raw_arm64.
#
# WHY A CROSS LANE. Every package rung here had been x64-only, and a 30k-line
# package is a far wider net than the 110 single-file programs of test/cc: the
# first arm64 run found a miscompile that had survived both, and Lua found it
# in the one way that is hard to notice -- string.match, string.gsub and
# string.find-with-a-pattern all silently returned nil in an interpreter that
# otherwise ran floats, coroutines and its whole battery correctly. The cause
# is test/cc/110-param5.c's law (a 5th pointer parameter riding x4 collided
# with the frame-base spelling); the shape that reaches it is a six-parameter
# function whose 5th is a pointer, which is prepstate in lstrlib.c and is not
# a thing anyone writes into a compiler test on purpose.
#
# Lua's source is the one imported artifact -- and it needs NO configure.
# Point LUASRC at an extracted lua-5.4.x tree; without one the check SKIPS
# (like moon-tar without TARSRC). To make one:
#   curl -O https://www.lua.org/ftp/lua-5.4.7.tar.gz
#   tar xzf lua-5.4.7.tar.gz
#   make moon-lua LUASRC=$PWD/lua-5.4.7
#   make moon-lua-arm64 LUASRC=$PWD/lua-5.4.7
#
# THE SOURCES ARE CACHED, so none of that is needed twice: this looks for
# `lua-5.4.*` under dl/ and then under $MOONSRC -- ~/src when that is unset --
# so a bare `make moon-lua` finds a cached tree with no variable at all. An
# explicit LUASRC= still outranks both, and a missing tree is a clean SKIP
# rather than a failure, so this gate stays opt-in either way.
set -e

target=${1:-x64}
case $target in
  x64)   name=moon-lua       ; tflag=""         ; sub=moonlua
         mksys=mksys       ; backend=""              ; run=""            ; need="" ;;
  arm64) name=moon-lua-arm64 ; tflag="-t arm64" ; sub=moonlua-a64
         mksys=mksys-arm64 ; backend=crew/holo/arm64.l ; run=qemu-aarch64 ; need=qemu-aarch64 ;;
  riscv64) name=moon-lua-riscv ; tflag="-t riscv64" ; sub=moonlua-rv
         mksys=mksys-riscv ; backend=crew/holo/riscv.l ; run=qemu-riscv64 ; need=qemu-riscv64 ;;
  *) echo "moon-lua.sh: unknown target $target (x64 | arm64 | riscv64)" >&2; exit 1 ;;
esac

# where a package's sources may live, first hit wins: the tree-local dl,
# then the cache -- $MOONSRC, or ~/src when that is unset. An explicit *SRC=
# on the make line still outranks both. Answers EMPTY when nothing matches,
# which is what the skip branch below reads, so a missing tree is never an
# error and never a `set -e` abort.
pkgfind() {                        # pkgfind <dir-glob> <witness-file>
  for c in dl/$1 "${MOONSRC:-$HOME/src}"/$1; do
    [ -f "$c/$2" ] && { printf '%s\n' "$c"; return 0; }
  done
  return 0
}

ho=out/host
mc=$ho/mooncc
love=$ho/love

if [ -n "$need" ] && ! command -v "$need" > /dev/null 2>&1; then
  echo "$name: no $need, skipped"
  exit 0
fi
LUASRC=${LUASRC:-$(pkgfind 'lua-5.4.*' src/lua.c)}
if [ -z "$LUASRC" ] || [ ! -f "$LUASRC/src/lua.c" ]; then
  echo "$name: no lua tree found (looked in dl and ${MOONSRC:-$HOME/src}) -- skipped."
  echo "          set LUASRC=<an extracted lua-5.4.x tree> to run (see tools/moon-lua.sh)."
  exit 0
fi
[ -x "$mc" ] || { echo "$name: missing $mc -- run 'make $ho/mooncc'"; exit 1; }

G=$(pwd); MC=$G/$mc
d=$ho/$sub
rm -rf "$d"; mkdir -p "$d"

echo "MOON-LUA  $LUASRC  ($target: mooncc + nolibc + holo, no gcc/glibc/ld)"

objs=""
for f in "$LUASRC"/src/*.c; do
  b=$(basename "$f" .c)
  [ "$b" = luac ] && continue
  $MC $tflag -Icrew/moon/include -I"$LUASRC/src" -c "$f" "$d/$b.o" || { echo "FAIL mooncc -c src/$b.c"; exit 1; }
  objs="$objs $d/$b.o"
done

# the rung-4 libc floor: am math + the syscall leaf (mksys lays sys.o). ⚠ NO nolibc
# object -- the link owes its symbols and the driver's runtime table pulls
# crew/moon/lib/nolibc/ MEMBER BY NEED (host/build.mk says the same of love itself).
# Naming an object would take every member instead.
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  $MC $tflag -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || { echo "FAIL mooncc -c $f"; exit 1; }
done
# sys.o is LAID, not compiled -- and a CROSS lay needs holo's backend loaded first
# (the host bake carries only the native one), exactly as raw.sh does it.
{ if [ -n "$backend" ]; then echo "(use 'holo)"; cat "$backend"; fi
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "($mksys \"$d/sys.o\")"; } | $love || { echo "FAIL $mksys sys.o"; exit 1; }

$MC $tflag $objs "$d"/m_*.o "$d/sys.o" -o "$d/lua" || { echo "FAIL holo link lua"; exit 1; }
echo "  linked $(wc -c < "$d/lua") bytes -> $d/lua"

# ---- prove it runs ----
luabin=$(cd "$d" && pwd)/lua
$run "$luabin" -v >/dev/null 2>&1 || { echo "FAIL lua -v"; exit 1; }

cat > "$d/battery.lua" <<'EOF'
local function fib(n) return n < 2 and n or fib(n-1) + fib(n-2) end
assert(fib(20) == 6765)
assert(("hello"):upper() == "HELLO")
assert(string.format("%d %5.2f %s %x", 42, 3.14159, "ok", 255) == "42  3.14 ok ff")
assert(("a,b,c"):match("([^,]+)") == "a")
-- the pattern matcher in anger: match/gsub/find-with-a-pattern all ran the
-- do_match recursion, and all three silently answered nil on the first arm64
-- build (see the header). plain find does NOT -- it shortcuts to lmemfind --
-- so a pattern with a special character is the one that asks the question.
assert(("hello world"):gsub("o", "0") == "hell0 w0rld")
assert(("key=value"):match("^(%w+)=(%w+)") == "key")
assert(select(2, ("key=value"):match("^(%w+)=(%w+)")) == "value")
assert(("2026-07-31"):find("%d+%-%d+") == 1)
assert(("  trim  "):match("^%s*(.-)%s*$") == "trim")
local t = {5,3,8,1,9,2}; table.sort(t)
assert(table.concat(t, ",") == "1,2,3,5,8,9")
assert(math.abs(math.sin(math.pi)) < 1e-15)
assert(math.sqrt(144) == 12 and math.fmod(7.5, 2) == 1.5)
assert(math.floor(-2.5) == -3 and math.ceil(-2.5) == -2)
assert(7 // 2 == 3 and 5 & 3 == 1 and 1 << 10 == 1024)
local ok, err = pcall(function() error("boom") end)
assert(not ok and err:find("boom"))
local co = coroutine.create(function(a) local b = coroutine.yield(a+1) return b*2 end)
local _, v = coroutine.resume(co, 10); assert(v == 11)
local _, w = coroutine.resume(co, 7); assert(w == 14)
local mt = {__add = function(a,b) return a.v + b.v end}
assert(setmetatable({v=3}, mt) + setmetatable({v=4}, mt) == 7)
collectgarbage("collect")
assert(os.date("!%Y-%m-%d", 86400) == "1970-01-02")
assert(type(os.time()) == "number" and type(os.clock()) == "number")
local f = assert(io.open("moonlua-scratch.txt", "w"))
f:write("line one\nline two\n"); f:close()
local lines = {}
for l in io.lines("moonlua-scratch.txt") do lines[#lines+1] = l end
assert(#lines == 2 and lines[2] == "line two")
os.remove("moonlua-scratch.txt")
assert(tonumber("0x10") == 16 and load("return 6*7")() == 42)
print("battery ok")
EOF
out=$(cd "$d" && $run "$luabin" battery.lua)
[ "$out" = "battery ok" ] || { echo "FAIL lua battery: '$out'"; exit 1; }
echo "  OK closures + strings + patterns + tables + math + pcall/coroutines (setjmp) + metatables + os date/time + io + load"
echo "$name: a runnable Lua $($run "$luabin" -v 2>&1 | cut -d' ' -f2), mooncc-compiled$([ -n "$run" ] && echo " for $target"), no gcc/glibc/ld"
