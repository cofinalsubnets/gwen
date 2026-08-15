#!/bin/sh
# moon-sqlite.sh -- build the SQLite amalgamation with mooncc + nolibc + the
# holo linker (no gcc/glibc/ld) and prove it RUNS: an in-memory battery
# (aggregates, ORDER BY, expressions) and a FILE-BACKED one through the whole
# unix VFS -- journaled transaction, index, close/reopen persistence, prepared
# statements, and PRAGMA integrity_check answering ok. The sixth moon-userland
# rung, after bzip2, gzip, tar, m4 and lua.
#
# TWO TARGETS, one procedure (raw.sh's shape, as moon-lua.sh does it):
# `moon-sqlite.sh` builds the native x86-64 lane, `moon-sqlite.sh arm64`
# cross-compiles the same source with `mooncc -t arm64` and runs the battery
# under qemu-aarch64. The cross lane SKIPS cleanly without qemu.
#
# WHY THE CROSS LANE IS WORTH ITS MINUTE. This is the widest single net the
# tree has: 256k lines from one file, and the amalgamation is machine-built
# from many, so it reaches C shapes nobody writes by hand -- deep switch
# ladders, computed unions, 64-bit mixing, a whole float formatter of its own.
# The precedent is moon-lua-arm64, whose FIRST run found a miscompile that 110
# single-file cc programs and the entire love corpus under mooncc/arm64 had all
# been green over. A package on a cross target is the cheapest coverage here.
#
# The amalgamation is the one imported artifact -- two files, no configure.
# Point SQLSRC at an extracted sqlite-amalgamation-* dir; without one the
# check SKIPS (like moon-lua without LUASRC). To make one:
#   curl -O https://sqlite.org/2024/sqlite-amalgamation-3450300.zip
#   unzip sqlite-amalgamation-3450300.zip
#   make moon-sqlite       SQLSRC=$PWD/sqlite-amalgamation-3450300
#   make moon-sqlite-arm64 SQLSRC=$PWD/sqlite-amalgamation-3450300
#
# THE SOURCES ARE CACHED, so none of that is needed twice: this looks for
# `sqlite-amalgamation-*` under dl/ and then under $MOONSRC -- ~/src when that is unset --
# so a bare `make moon-sqlite` finds a cached tree with no variable at all. An
# explicit SQLSRC= still outranks both, and a missing tree is a clean SKIP
# rather than a failure, so this gate stays opt-in either way.
#
# The config: THREADSAFE=0 (nolibc carries no pthreads) and no load-extension
# (no dlopen) -- both first-class sqlite configurations, not patches.
set -e

target=${1:-x64}
case $target in
  x64)   name=moon-sqlite       ; tflag=""         ; sub=moonsqlite
         mksys=mksys       ; backend=""              ; run=""            ; need="" ;;
  arm64) name=moon-sqlite-arm64 ; tflag="-t arm64" ; sub=moonsqlite-a64
         mksys=mksys-arm64 ; backend=crew/holo/arm64.l ; run=qemu-aarch64 ; need=qemu-aarch64 ;;
  riscv64) name=moon-sqlite-riscv ; tflag="-t riscv64" ; sub=moonsqlite-rv
         mksys=mksys-riscv ; backend=crew/holo/riscv.l ; run=qemu-riscv64 ; need=qemu-riscv64 ;;
  *) echo "moon-sqlite.sh: unknown target $target (x64 | arm64 | riscv64)" >&2; exit 1 ;;
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
SQLSRC=${SQLSRC:-$(pkgfind 'sqlite-amalgamation-*' sqlite3.c)}
if [ -z "$SQLSRC" ] || [ ! -f "$SQLSRC/sqlite3.c" ]; then
  echo "$name: no amalgamation found (looked in dl and ${MOONSRC:-$HOME/src}) -- skipped."
  echo "             set SQLSRC=<an extracted sqlite-amalgamation dir> to run (see mk/tools/moon-sqlite.sh)."
  exit 0
fi
[ -x "$mc" ] || { echo "$name: missing $mc -- run 'make $ho/mooncc'"; exit 1; }

d=$ho/$sub
rm -rf "$d"; mkdir -p "$d"

echo "MOON-SQLITE  $SQLSRC  ($target: mooncc + nolibc + holo, no gcc/glibc/ld)"

$mc $tflag -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION=1 -Icrew/moon/include \
    -c "$SQLSRC/sqlite3.c" "$d/sqlite3.o" || { echo "FAIL mooncc -c sqlite3.c"; exit 1; }
echo "  sqlite3.c -> $(wc -c < "$d/sqlite3.o") bytes of object"

cat > "$d/drv.c" <<'EOF'
/* the battery, and it is a DIFFERENTIAL payload: every line is a computed
 * value printed to stdout, so the arm64 build's output is compared to the
 * x86-64 build's byte for byte rather than merely checked for "ok". a codegen
 * fault then names the query it broke instead of showing up as a bad exit
 * code. nothing here may vary between two correct runs -- no clock, no
 * address, no rowid ordering left to chance. */
#include <stdio.h>
#include "sqlite3.h"
static int cb(void *u, int n, char **v, char **c) {
  (void)u; (void)c;
  for (int i = 0; i < n; i++) printf("%s%s", v[i] ? v[i] : "NULL", i + 1 < n ? "|" : "\n");
  return 0;
}
/* run a statement for its rows, each row tagged so a diff names the case */
static void q(sqlite3 *db, char const *tag, char const *sql) {
  sqlite3_stmt *st;
  if (sqlite3_prepare_v2(db, sql, -1, &st, 0) != SQLITE_OK) {
    printf("%s: PREPARE-FAIL %s\n", tag, sqlite3_errmsg(db)); return; }
  while (sqlite3_step(st) == SQLITE_ROW) {
    printf("%s:", tag);
    for (int i = 0; i < sqlite3_column_count(st); i++) {
      unsigned char const *t = sqlite3_column_text(st, i);
      printf(" %s", t ? (char const *) t : "NULL"); }
    printf("\n"); }
  sqlite3_finalize(st);
}
int main(void) {
  sqlite3 *db; sqlite3_stmt *st;
  if (sqlite3_open(":memory:", &db) != SQLITE_OK) { printf("FAIL open\n"); return 1; }
  if (sqlite3_exec(db,
      "CREATE TABLE t(a INTEGER, b TEXT);"
      "INSERT INTO t VALUES (1,'one'),(2,'two'),(3,'three');"
      "SELECT sum(a), group_concat(b), count(*) FROM t;",
      cb, 0, 0) != SQLITE_OK) { printf("FAIL exec: %s\n", sqlite3_errmsg(db)); return 1; }

  /* --- 64-bit integer edges: the lane a 32-bit slip shows up in first --- */
  q(db, "int.max",  "SELECT 9223372036854775807, -9223372036854775807-1");
  q(db, "int.wrap", "SELECT 4294967295+1, 4294967296*2, 1<<62, -1>>1");
  q(db, "int.div",  "SELECT 7/2, -7/2, 7%3, -7%3, 7.0/2");
  q(db, "int.bit",  "SELECT 255&15, 240|15, ~0, -1>>63, 1<<63");

  /* --- REAL: sqlite carries its OWN printf, so this is a second float
     formatter under mooncc, independent of nolibc's --- */
  q(db, "real.fmt", "SELECT 3.14159265358979, 1e300, 1e-300, 0.1+0.2");
  q(db, "real.rnd", "SELECT round(2.5), round(3.5), round(-2.5), round(1.005,2)");
  q(db, "real.cast","SELECT CAST(3.99 AS INTEGER), CAST(-3.99 AS INTEGER), CAST('12abc' AS INTEGER)");
  q(db, "real.fn",  "SELECT abs(-4.5), max(1.5,2.5), min(1.5,2.5), 2.0*3.5");
  q(db, "real.sci", "SELECT printf('%.17g', 1.0/3), printf('%e', 12345.6789), printf('%.3f', 2.0/3)");

  /* --- strings, collation and the pattern operators --- */
  q(db, "str.fn",   "SELECT upper('abc'), length('hello'), substr('abcdef',2,3), replace('aaa','a','b')");
  q(db, "str.like", "SELECT 'foobar' LIKE 'foo%', 'foobar' GLOB 'f?obar', 'ABC' LIKE 'abc'");
  q(db, "str.trim", "SELECT trim('  x  '), ltrim('xxay','x'), rtrim('yaxx','x'), instr('hello','ll')");
  q(db, "str.hex",  "SELECT hex('AB'), quote('it''s'), char(65,66,67), unicode('A')");

  /* --- aggregates and ordering over a wider table --- */
  sqlite3_exec(db, "CREATE TABLE n(i INTEGER, r REAL, s TEXT);", 0, 0, 0);
  sqlite3_exec(db,
      "INSERT INTO n SELECT value, value*1.5, 'v'||value FROM generate_series(1,50);",
      0, 0, 0);   /* generate_series may be absent; the fallback below covers it */
  { sqlite3_stmt *c; sqlite3_prepare_v2(db, "SELECT count(*) FROM n", -1, &c, 0);
    sqlite3_step(c);
    if (sqlite3_column_int(c, 0) == 0)
      for (int i = 1; i <= 50; i++) {
        char sql[128];
        snprintf(sql, sizeof sql,
                 "INSERT INTO n VALUES(%d,%d.5,'v%d')", i, i, i);
        sqlite3_exec(db, sql, 0, 0, 0); }
    sqlite3_finalize(c); }
  q(db, "agg", "SELECT count(*), sum(i), total(r), avg(i), min(s), max(s) FROM n");
  q(db, "ord", "SELECT group_concat(i) FROM (SELECT i FROM n ORDER BY r DESC LIMIT 8)");
  q(db, "grp", "SELECT i%7, count(*), sum(i) FROM n GROUP BY i%7 HAVING count(*)>6 ORDER BY 1");
  q(db, "join","SELECT count(*) FROM n x JOIN n y ON x.i = y.i+1");
  q(db, "sub", "SELECT i FROM n WHERE i IN (SELECT i FROM n WHERE i%13=0) ORDER BY i");
  q(db, "win", "SELECT i, sum(i) OVER (ORDER BY i ROWS 2 PRECEDING) FROM n WHERE i<6 ORDER BY i");
  q(db, "cte", "WITH RECURSIVE f(a,b) AS (SELECT 0,1 UNION ALL SELECT b,a+b FROM f WHERE b<1000)"
               " SELECT group_concat(a) FROM f");
  q(db, "json","SELECT json_extract('{\"a\":[1,2,{\"b\":7}]}','$.a[2].b'), json_array_length('[1,2,3]')");
  sqlite3_close(db);

  /* --- the file-backed lane: journal, index, close/reopen, integrity --- */
  remove("moonsq.db");
  if (sqlite3_open("moonsq.db", &db) != SQLITE_OK) { printf("FAIL fopen\n"); return 1; }
  if (sqlite3_exec(db,
      "CREATE TABLE kv(k TEXT PRIMARY KEY, v REAL);"
      "BEGIN; INSERT INTO kv VALUES ('pi',3.14159),('e',2.71828),('phi',1.61803); COMMIT;"
      "CREATE INDEX kvi ON kv(v);", 0, 0, 0) != SQLITE_OK) { printf("FAIL write: %s\n", sqlite3_errmsg(db)); return 1; }
  sqlite3_close(db);
  if (sqlite3_open("moonsq.db", &db) != SQLITE_OK) { printf("FAIL reopen\n"); return 1; }
  sqlite3_prepare_v2(db, "SELECT count(*) FROM kv WHERE v > 2", -1, &st, 0);
  if (sqlite3_step(st) != SQLITE_ROW || sqlite3_column_int(st, 0) != 2) { printf("FAIL query\n"); return 1; }
  sqlite3_finalize(st);
  q(db, "kv", "SELECT k, v FROM kv ORDER BY v");
  /* a rollback must actually roll back -- the journal round trip */
  sqlite3_exec(db, "BEGIN; DELETE FROM kv; ROLLBACK;", 0, 0, 0);
  q(db, "kv.rb", "SELECT count(*) FROM kv");
  sqlite3_prepare_v2(db, "PRAGMA integrity_check", -1, &st, 0);
  if (sqlite3_step(st) != SQLITE_ROW) { printf("FAIL check\n"); return 1; }
  printf("integrity=%s\n", sqlite3_column_text(st, 0));
  sqlite3_finalize(st);
  sqlite3_close(db);
  remove("moonsq.db");
  printf("battery ok %s\n", sqlite3_libversion());
  return 0;
}
EOF
$mc $tflag -Icrew/moon/include -I"$SQLSRC" -c "$d/drv.c" "$d/drv.o" || { echo "FAIL mooncc -c drv.c"; exit 1; }

# the rung-4 libc floor: am math + the syscall leaf (mksys lays sys.o). ⚠ NO nolibc
# object -- the link owes its symbols and the driver's runtime table pulls
# crew/moon/lib/nolibc/ MEMBER BY NEED (host/build.mk says the same of love itself).
# Naming an object would take every member instead.
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  $mc $tflag -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || { echo "FAIL mooncc -c $f"; exit 1; }
done
# sys.o is LAID, not compiled -- and a CROSS lay needs holo's backend loaded
# first (the host bake carries only the native one), exactly as raw.sh does it.
{ if [ -n "$backend" ]; then echo "(use 'holo)"; cat "$backend"; fi
  cat crew/kore/text.l crew/kore/u.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "($mksys \"$d/sys.o\")"; } | $love || { echo "FAIL $mksys sys.o"; exit 1; }

$mc $tflag "$d/sqlite3.o" "$d/drv.o" "$d"/m_*.o "$d/sys.o" -o "$d/sq" || { echo "FAIL holo link"; exit 1; }
echo "  linked $(wc -c < "$d/sq") bytes -> $d/sq"

(cd "$d" && $run ./sq) > "$d/out.txt" || { echo "FAIL battery did not run"; cat "$d/out.txt"; exit 1; }
out=$(cat "$d/out.txt")
echo "$out" | grep -q '^6|one,two,three|3$' || { echo "FAIL battery (mem): $out"; exit 1; }
echo "$out" | grep -q '^integrity=ok$' || { echo "FAIL battery (integrity): $out"; exit 1; }
echo "$out" | grep -q '^battery ok' || { echo "FAIL battery: $out"; exit 1; }
echo "$out" | grep -q 'PREPARE-FAIL' && { echo "FAIL battery: a statement would not prepare"; grep PREPARE-FAIL "$d/out.txt"; exit 1; }
echo "  OK $(grep -c . "$d/out.txt") lines: int/real/string/aggregate/window/CTE + journaled txn + rollback + reopen + integrity_check"

# ---- THE GCC LEG. two builds that agree can still share a fault, so the x64
# lane does not rest on mooncc alone: the SAME driver and the SAME amalgamation
# go through the system cc against glibc, and the answers must match. that
# closes the ladder -- gcc pins x64, x64 pins the cross target. skipped
# silently where there is no system cc, like test_libc's second opinion.
if [ "$target" = x64 ]; then
  cc_g=$(command -v gcc || command -v cc || true)
  if [ -n "${cc_g:-}" ]; then
    mkdir -p "$d/g"
    if $cc_g -O0 -w -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION=1 \
        -I"$SQLSRC" "$SQLSRC/sqlite3.c" "$d/drv.c" -o "$d/g/sq" -lm 2> "$d/g/build.log"; then
      (cd "$d/g" && ./sq) > "$d/g/out.txt" 2>&1 || true
      if cmp -s "$d/out.txt" "$d/g/out.txt"; then
        echo "  OK every answer byte-identical to the same source built by $cc_g"
      else
        # ⚠ the LABEL must name the argument order, because the first thing anyone
        # does with this output is decide which side is the bug. `<` is the ORACLE
        # here, not us -- reading it the other way sent one session off explaining
        # why gcc must be wrong. (The tiebreaker when in doubt is a third opinion:
        # ask the system sqlite3 what `SELECT typeof(...)` says.)
        echo "--- < $cc_g (the oracle)   vs   > mooncc (first 20 differing lines) ---" >&2
        diff "$d/g/out.txt" "$d/out.txt" | head -20 >&2
        echo "FAIL $name: mooncc and $cc_g answer differently" >&2
        exit 1
      fi
    else
      echo "  ($cc_g could not build the amalgamation -- second opinion skipped)"
    fi
  fi
fi

# ---- THE CROSS DIFFERENTIAL. x86-64 is the oracle: test_moon pins it against
# gcc, so gcc pins x64 and x64 pins the cross target -- the same ladder
# test/gate/ccarch.sh stands on. comparing OUTPUT rather than an exit code is
# the whole point: eight bits cannot name the query that broke.
ref=$ho/moonsqlite/out.txt
if [ "$target" != x64 ]; then
  if [ ! -f "$ref" ]; then
    echo "  (no x86-64 run to compare against -- run 'make moon-sqlite' first for the differential)"
  elif cmp -s "$ref" "$d/out.txt"; then
    echo "  OK every answer byte-identical to the x86-64 build ($(grep -c . "$ref") lines)"
  else
    echo "--- $target vs x86-64 (first 20 differing lines) ---" >&2
    diff "$ref" "$d/out.txt" | head -20 >&2
    echo "FAIL $name: the cross build answers differently from the x86-64 one" >&2
    exit 1
  fi
fi
echo "$name: a runnable SQLite $(echo "$out" | sed -n 's/^battery ok //p'), mooncc-compiled$([ -n "$run" ] && echo " for $target"), no gcc/glibc/ld"
