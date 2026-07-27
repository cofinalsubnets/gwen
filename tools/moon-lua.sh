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
# Lua's source is the one imported artifact -- and it needs NO configure.
# Point LUASRC at an extracted lua-5.4.x tree; without one the check SKIPS
# (like moon-tar without TARSRC). To make one:
#   curl -O https://www.lua.org/ftp/lua-5.4.7.tar.gz
#   tar xzf lua-5.4.7.tar.gz
#   make moon-lua LUASRC=$PWD/lua-5.4.7
set -e

ho=out/host
mc=$ho/mooncc
love=$ho/love

if [ -z "$LUASRC" ] || [ ! -f "$LUASRC/src/lua.c" ]; then
  echo "moon-lua: no lua tree at '$LUASRC' -- skipped."
  echo "          set LUASRC=<an extracted lua-5.4.x tree> to run (see tools/moon-lua.sh)."
  exit 0
fi
[ -x "$mc" ] || { echo "moon-lua: missing $mc -- run 'make $ho/mooncc'"; exit 1; }

G=$(pwd); MC=$G/$mc
d=$ho/moonlua
rm -rf "$d"; mkdir -p "$d"

echo "MOON-LUA  $LUASRC  (mooncc + nolibc + holo, no gcc/glibc/ld)"

objs=""
for f in "$LUASRC"/src/*.c; do
  b=$(basename "$f" .c)
  [ "$b" = luac ] && continue
  $MC -Icrew/moon/include -I"$LUASRC/src" -c "$f" "$d/$b.o" || { echo "FAIL mooncc -c src/$b.c"; exit 1; }
  objs="$objs $d/$b.o"
done

# the rung-4 libc floor: nolibc + am math + the syscall leaf (mksys lays sys.o).
$MC -Icrew/moon/include -c crew/moon/lib/nolibc.c "$d/nolibc.o" || { echo "FAIL mooncc -c nolibc.c"; exit 1; }
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  $MC -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || { echo "FAIL mooncc -c $f"; exit 1; }
done
{ cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "(mksys \"$d/sys.o\")"; } | $love || { echo "FAIL mksys sys.o"; exit 1; }

$MC $objs "$d/nolibc.o" "$d"/m_*.o "$d/sys.o" -o "$d/lua" || { echo "FAIL holo link lua"; exit 1; }
echo "  linked $(wc -c < "$d/lua") bytes -> $d/lua"

# ---- prove it runs ----
luabin=$(cd "$d" && pwd)/lua
"$luabin" -v >/dev/null 2>&1 || { echo "FAIL lua -v"; exit 1; }

cat > "$d/battery.lua" <<'EOF'
local function fib(n) return n < 2 and n or fib(n-1) + fib(n-2) end
assert(fib(20) == 6765)
assert(("hello"):upper() == "HELLO")
assert(string.format("%d %5.2f %s %x", 42, 3.14159, "ok", 255) == "42  3.14 ok ff")
assert(("a,b,c"):match("([^,]+)") == "a")
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
out=$(cd "$d" && "$luabin" battery.lua)
[ "$out" = "battery ok" ] || { echo "FAIL lua battery: '$out'"; exit 1; }
echo "  OK closures + strings + tables + math + pcall/coroutines (setjmp) + metatables + os date/time + io + load"
echo "moon-lua: a runnable Lua $("$luabin" -v 2>&1 | cut -d' ' -f2), mooncc-compiled, no gcc/glibc/ld"
