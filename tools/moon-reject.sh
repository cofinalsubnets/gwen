#!/bin/sh
# moon-reject.sh -- the REFUSAL battery: C that gcc rejects, put to mooncc, and
# the two verdicts set side by side. doc/moon-diag.md's "the battery" section is
# this script's output, read once and written down.
#
# moon-sweep.sh measures C that SHOULD compile and doesn't. This measures C that
# should NOT compile and does -- the other failure of a front end, and the one no
# amount of building real packages will ever show you, because real packages
# compile. Programs are written out here rather than kept as a corpus directory
# on purpose: each is two lines, and the point of every one of them is a single
# named C rule, which a file called `s_dupcase.c` states less clearly than the
# table row does.
#
# THREE OUTCOMES per program, and only the third is a finding:
#   both     -- gcc and mooncc both refuse. The diagnostic is printed so it can
#               be read; a refusal that names nothing is a rung, not a red.
#   mooncc   -- mooncc refuses where gcc does not (nothing here does today).
#   ACCEPTED -- gcc refuses and mooncc lays an object. The list that matters.
#
# gcc is the ORACLE and nothing else -- no gcc-compiled artifact is used. Without
# a gcc the script skips, like moon-sweep.sh without a package tree.
set -e

mc=${MOONCC:-out/host/mooncc}
kore=${KORE:-out/host/kore}
gcc=${GCC:-gcc}
d=${TMPDIR:-/tmp}/moon-reject.$$
[ -x "$mc" ] || { echo "moon-reject: no $mc -- run make out/host/mooncc"; exit 0; }
command -v "$gcc" >/dev/null 2>&1 || { echo "moon-reject: no $gcc (the oracle) -- skipping"; exit 0; }
mkdir -p "$d"
trap 'rm -rf "$d"' EXIT

nboth=0; nacc=0; nonly=0
acc=""

# p NAME SOURCE -- one program through both compilers
p() {
  f=$d/$1.c
  printf '%s\n' "$2" > "$f"
  if "$gcc" -c -std=c99 -o "$d/$1.gcc.o" "$f" >"$d/$1.gcc.log" 2>&1; then g=ok; else g=no; fi
  if "$mc" -c -o "$d/$1.moon.o" "$f" >"$d/$1.moon.log" 2>&1; then m=ok; else m=no; fi
  msg=$(head -1 "$d/$1.moon.log" 2>/dev/null)
  if [ "$g" = no ] && [ "$m" = no ]; then
    nboth=$((nboth+1)); printf '  both      %-14s %s\n' "$1" "$msg"
  elif [ "$g" = no ] && [ "$m" = ok ]; then
    nacc=$((nacc+1)); acc="$acc $1"
    gm=$("$gcc" -c -std=c99 -o /dev/null "$f" 2>&1 | grep -m1 -E 'error|warning' | sed 's/.*: //')
    printf '  ACCEPTED  %-14s gcc: %s\n' "$1" "$gm"
  elif [ "$g" = ok ] && [ "$m" = no ]; then
    nonly=$((nonly+1)); printf '  mooncc    %-14s %s\n' "$1" "$msg"
  fi
}

echo "--- lex ---"
p lex_at   'int main(void){ int x = 1 @ 2; return x; }'
p lex_str  'int main(void){ char *s = "unterminated;
 return 0; }'
p lex_chr  "int main(void){ char c = 'a; return c; }"
p lex_cmt  'int main(void){ /* never closed
 return 0; }'
p lex_num  'int main(void){ return 0x; }'

echo "--- preprocessor ---"
p cpp_err   '#error "this platform lacks fsync"
int main(void){ return 0; }'
p cpp_if    '#if 1
int main(void){ return 0; }'
p cpp_endif 'int main(void){ return 0; }
#endif'
p cpp_inc   '#include <nosuchheader.h>
int main(void){ return 0; }'
p cpp_arity '#define F(a,b) ((a)+(b))
int main(void){ return F(1); }'
p cpp_unk   '#frobnicate 3
int main(void){ return 0; }'
p cpp_open  '#define F(a) (a)
int main(void){ return F(1; }'

echo "--- parse ---"
p p_semi  'int main(void){ int x = 1
 return x; }'
p p_brace 'int main(void){ int x = 1; return x;'
p p_paren 'int main(void){ return (1 + 2; }'
p p_junk  'int main(void){ return ; ; ) ; }'
p p_kw    'int main(void){ struct return x; return 0; }'
p p_else  'int main(void){ else return 1; }'

echo "--- names, arity, types ---"
p s_undecl     'int main(void){ return nope; }'
p s_undeclfn   'int main(void){ return frobnicate(1); }'
p s_argc       'int f(int a, int b){ return a+b; }
int main(void){ return f(1); }'
p s_argc2      'int f(int a){ return a; }
int main(void){ return f(1,2,3); }'
p s_callint    'int main(void){ int x = 3; return x(1); }'
p s_rvalue     'int main(void){ 1 = 2; return 0; }'
p s_redef      'int f(void){ return 1; }
int f(void){ return 2; }
int main(void){ return f(); }'
p s_dupvar     'int main(void){ int x = 1; int x = 2; return x; }'
p s_member     'struct s { int a; };
int main(void){ struct s v; v.b = 1; return v.a; }'
p s_deref      'int main(void){ int x = 1; return *x; }'
p s_arrfn      'int main(void){ int x = 1; return x[0]; }'
p s_void       'void f(void){ return 1; }
int main(void){ f(); return 0; }'
p s_incomplete 'struct s;
int main(void){ struct s v; return 0; }'
p s_typemis    'struct s { int a; };
int main(void){ struct s v; int x = v; return x; }'
p s_badcast    'struct s { int a; };
int main(void){ int x = (struct s)1; return x; }'

echo "--- labels: the ones that reach the SYMBOL TABLE ---"
# ⚠ these two are the finding. An unresolved local label does not refuse -- it
# leaves the object carrying an undefined GLOBAL symbol spelled with mooncc's own
# internal name, and only the LINK notices. the symbol table is printed because the
# compiler's exit code says nothing. `kore nm -u` reads it -- our own reader over
# holo's ELF door, so this needs no binutils to say what the object owes.
p s_goto    'int main(void){ goto nowhere; return 0; }'
p s_dupcase 'int main(void){ int x=1; switch(x){ case 1: return 1; case 1: return 2; } return 0; }'
for t in s_goto s_dupcase; do
  [ -f "$d/$t.moon.o" ] || continue
  [ -x "$kore" ] || { echo "    (no $kore -- run make out/host/kore to read the symbol tables)"; break; }
  u=$("$kore" nm -gu "$d/$t.moon.o" 2>/dev/null | awk '{print $2}' | tr '\n' ' ')
  [ -n "$u" ] && printf '    %-12s undefined in the object: %s\n' "$t" "$u"
done
echo "--- and what the LINK says about them ---"
for t in s_goto s_dupcase; do
  printf '    %-12s %s\n' "$t" "$("$mc" -o "$d/$t.exe" "$d/$t.c" 2>&1 | head -1)"
done

echo
echo "--- census ---"
echo "  $nboth refused by both, $nonly refused by mooncc only,"
echo "  $nacc ACCEPTED by mooncc that gcc refuses:$acc"
