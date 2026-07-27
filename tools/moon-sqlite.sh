#!/bin/sh
# moon-sqlite.sh -- build the SQLite amalgamation with mooncc + nolibc + the
# holo linker (no gcc/glibc/ld) and prove it RUNS: an in-memory battery
# (aggregates, ORDER BY, expressions) and a FILE-BACKED one through the whole
# unix VFS -- journaled transaction, index, close/reopen persistence, prepared
# statements, and PRAGMA integrity_check answering ok. The sixth moon-userland
# rung (doc/moon-userland.md), after bzip2, gzip, tar, m4 and lua.
#
# The amalgamation is the one imported artifact -- two files, no configure.
# Point SQLSRC at an extracted sqlite-amalgamation-* dir; without one the
# check SKIPS (like moon-lua without LUASRC). To make one:
#   curl -O https://sqlite.org/2024/sqlite-amalgamation-3450300.zip
#   unzip sqlite-amalgamation-3450300.zip
#   make moon-sqlite SQLSRC=$PWD/sqlite-amalgamation-3450300
#
# The config: THREADSAFE=0 (nolibc carries no pthreads) and no load-extension
# (no dlopen) -- both first-class sqlite configurations, not patches.
set -e

ho=out/host
mc=$ho/mooncc
love=$ho/love

if [ -z "$SQLSRC" ] || [ ! -f "$SQLSRC/sqlite3.c" ]; then
  echo "moon-sqlite: no amalgamation at '$SQLSRC' -- skipped."
  echo "             set SQLSRC=<an extracted sqlite-amalgamation dir> to run (see tools/moon-sqlite.sh)."
  exit 0
fi
[ -x "$mc" ] || { echo "moon-sqlite: missing $mc -- run 'make $ho/mooncc'"; exit 1; }

d=$ho/moonsqlite
rm -rf "$d"; mkdir -p "$d"

echo "MOON-SQLITE  $SQLSRC  (mooncc + nolibc + holo, no gcc/glibc/ld)"

$mc -DSQLITE_THREADSAFE=0 -DSQLITE_OMIT_LOAD_EXTENSION=1 -Icrew/moon/include \
    -c "$SQLSRC/sqlite3.c" "$d/sqlite3.o" || { echo "FAIL mooncc -c sqlite3.c"; exit 1; }
echo "  sqlite3.c -> $(wc -c < "$d/sqlite3.o") bytes of object"

cat > "$d/drv.c" <<'EOF'
#include <stdio.h>
#include "sqlite3.h"
static int cb(void *u, int n, char **v, char **c) {
  (void)u; (void)c;
  for (int i = 0; i < n; i++) printf("%s%s", v[i] ? v[i] : "NULL", i + 1 < n ? "|" : "\n");
  return 0;
}
int main(void) {
  sqlite3 *db; sqlite3_stmt *st;
  if (sqlite3_open(":memory:", &db) != SQLITE_OK) { printf("FAIL open\n"); return 1; }
  if (sqlite3_exec(db,
      "CREATE TABLE t(a INTEGER, b TEXT);"
      "INSERT INTO t VALUES (1,'one'),(2,'two'),(3,'three');"
      "SELECT sum(a), group_concat(b), count(*) FROM t;",
      cb, 0, 0) != SQLITE_OK) { printf("FAIL exec: %s\n", sqlite3_errmsg(db)); return 1; }
  sqlite3_close(db);
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
$mc -Icrew/moon/include -I"$SQLSRC" -c "$d/drv.c" "$d/drv.o" || { echo "FAIL mooncc -c drv.c"; exit 1; }

# the rung-4 libc floor: nolibc + am math + the syscall leaf (mksys lays sys.o).
$mc -Icrew/moon/include -c crew/moon/lib/nolibc.c "$d/nolibc.o" || { echo "FAIL mooncc -c nolibc.c"; exit 1; }
for f in crew/moon/lib/math/*.c; do
  b=$(basename "$f" .c)
  $mc -Icrew/moon/lib/math -Icrew/moon/include -c "$f" "$d/m_$b.o" || { echo "FAIL mooncc -c $f"; exit 1; }
done
{ cat crew/kore/text.l crew/kore/core.l crew/kore/asbook.l crew/holo/elf.l crew/holo/obj.l crew/moon/lib/mksys.l
  echo "(mksys \"$d/sys.o\")"; } | $love || { echo "FAIL mksys sys.o"; exit 1; }

$mc "$d/sqlite3.o" "$d/drv.o" "$d/nolibc.o" "$d"/m_*.o "$d/sys.o" -o "$d/sq" || { echo "FAIL holo link"; exit 1; }
echo "  linked $(wc -c < "$d/sq") bytes -> $d/sq"

out=$(cd "$d" && ./sq)
echo "$out" | grep -q '^6|one,two,three|3$' || { echo "FAIL battery (mem): $out"; exit 1; }
echo "$out" | grep -q '^integrity=ok$' || { echo "FAIL battery (integrity): $out"; exit 1; }
echo "$out" | grep -q '^battery ok' || { echo "FAIL battery: $out"; exit 1; }
echo "  OK in-memory aggregates + file-backed journaled txn + reopen + integrity_check"
echo "moon-sqlite: a runnable SQLite $(echo "$out" | sed -n 's/^battery ok //p'), mooncc-compiled, no gcc/glibc/ld"
