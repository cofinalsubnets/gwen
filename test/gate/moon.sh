#!/bin/sh
# test/gate/moon.sh -- mooncc's gate. Two halves: the LAWS (crew/moon/law.l, which
# runs anywhere) and, on x86-64 only, an END-TO-END battery against gcc as the oracle
# -- mooncc compiles a program, gcc compiles the same program, and the two exit codes
# must agree. gcc is never trusted to be right, only to be a second opinion; where a
# check wants an exact value it says so (42, mostly).
#
# make owns the dependency graph; this owns the procedure.
# NOT set -e: nearly every check captures $? to report it in its own message.
#
# usage: moon.sh OUTDIR LOVE LOVE0
set -u

ho=$1
m=$2
love0=$3

fail() { echo "FAIL $*" >&2; exit 1; }
# the compiler under test: the baked mooncc image, woken per invocation
moonrun() { "$m" wake "$ho/mooncc.image" mooncc "$@"; }
# ..and the BOOTSTRAP one, the lane that compiles love.c: love0 waking mooncc0.image
moon0() { "$love0" wake out/host/mooncc0.image mooncc "$@"; }

# ---------------------------------------------------------------- the laws
echo "CC crew/moon/{lex,cpp,parse,gen,law}.l"
out=$ho/.test_moon.out
{ echo "(use 'holo)"
  cat test/00-init.l crew/moon/floor.l crew/moon/lex.l crew/moon/cpp.l crew/moon/parse.l \
      crew/holo/text.l crew/moon/gen.l crew/moon/law.l
} | "$m" > "$out" 2>&1
r=$?
cat "$out"
[ $r -eq 0 ] && grep -q "crew/moon/law:" "$out" || fail "cc laws (exit $r)"

# ----------------------------------------------- the template parser, under love0
# ⚠ holo/text.l reaches the combinators through the bare name `post`, which each
# frontend's boot binds to that module's accessor. a lane that leaves something else
# there curries every combinator into a silent partial: no scare, no wrong answer,
# just every template failing to parse. love0's build-tool lane is the one that
# compiles love.c, and it is the only lane the laws above never walk.
echo "CC crew/holo/text.l (love0 lane)"
"$love0" -l crew/holo/text.l -e '(? (two? (asm-text "li r0, 60")) (quit 0) (quit 1))' </dev/null \
  || fail "asm-text under love0 -- is bare \`post\` the module accessor there?"

# ---------------------------------------------- the pipeline's stage types
# gen.l read as DATA and typed against crew/moon/stage.l's sig table (the
# overlay leg): the post-choice chain composes in exactly one order, and a
# clash names its innermost seam. nothing from stage.l rides any image.
"$m" crew/moon/stage.l || fail "moon-stage (the ;; moon-stage line names the seam)"

arch=$(uname -m)
if [ "$arch" != x86_64 ]; then
  echo "mooncc: cc (laws only -- x86_64 e2e skipped on $arch) ok"
  exit 0
fi

cc_g=$(command -v gcc || command -v cc)

# ------------------------------------------------- return 42, and agree with gcc
printf 'int main() { return 42; }\n' > "$ho/.cc1.c"
moonrun "$ho/.cc1.c" "$ho/.cc1" > /dev/null 2>&1 || fail "mooncc compile"
"$ho/.cc1"; a=$?
$cc_g -O0 -o "$ho/.cc1g" "$ho/.cc1.c" && "$ho/.cc1g"; b=$?
[ $a -eq 42 ] && [ $a -eq $b ] || fail "mooncc vs gcc (ours $a gcc $b)"

cat > "$ho/.cc2.c" <<'EOF'
// c
int f() { return 1; }
int main() { return 7; }
EOF
moonrun "$ho/.cc2.c" "$ho/.cc2" > /dev/null 2>&1 && "$ho/.cc2"; a=$?
$cc_g -O0 -o "$ho/.cc2g" "$ho/.cc2.c" && "$ho/.cc2g"; b=$?
[ $a -eq $b ] || fail "mooncc two-fn vs gcc (ours $a gcc $b)"

# ------------------------------------------------------------- the battery
for f in test/cc/*.c; do
  moonrun "$f" "$ho/.ccb" > /dev/null 2>&1 || fail "mooncc compile $f"
  "$ho/.ccb"; a=$?
  $cc_g -O0 -o "$ho/.ccbg" "$f" && "$ho/.ccbg"; b=$?
  [ $a -eq $b ] || fail "mooncc battery $f (ours $a gcc $b)"
done

# -------------------------------- C11 conditional features (6.10.8.3), per target
# gcc cannot be the oracle here -- it HAS atomics -- so these are ours alone, and
# each row must track the parity table: a claimed absence we do not have sends a
# portable source down a fallback for nothing.
c11feat() {                        # TGT MACRO want(1 present | 0 absent)
  if [ "$3" = 1 ]; then b="#ifndef $2"; else b="#ifdef $2"; fi
  printf '%s\n#error no\n#endif\nint m(void){return 0;}\n' "$b" > "$ho/.feat.c"
  moonrun -c -t "$1" -o /dev/null "$ho/.feat.c" > /dev/null 2>&1 \
    || fail "C11 feature macro $2 on $1 (want present=$3)"
}
for t in x64 arm64 riscv64 thumb2 thumb2sp thumb1; do
  for mac in __STDC_NO_ATOMICS__ __STDC_NO_THREADS__ __STDC_UTF_16__ __STDC_UTF_32__; do
    c11feat "$t" "$mac" 1
  done
  case $t in x64|arm64) c11feat "$t" __STDC_NO_VLA__ 0 ;; *) c11feat "$t" __STDC_NO_VLA__ 1 ;; esac
  case $t in x64)       c11feat "$t" __STDC_NO_COMPLEX__ 0 ;; *) c11feat "$t" __STDC_NO_COMPLEX__ 1 ;; esac
done
# a TU that is ONLY a _Static_assert -- want answers the remainder, which is () at
# EOF, and that read as "no `;` found". a failing assert must still refuse.
printf '_Static_assert(1, "ok");' > "$ho/.feat.c"
moonrun -c -t x64 -o /dev/null "$ho/.feat.c" > /dev/null 2>&1 || fail "a TU of one _Static_assert"
printf '_Static_assert(0, "boom");' > "$ho/.feat.c"
moonrun -c -t x64 -o /dev/null "$ho/.feat.c" > /dev/null 2>&1 && fail "a FAILING lone _Static_assert passed"

# a UCN takes EXACTLY 4 (or 8) hex digits -- a short run must REFUSE, not take what
# it found. test/cc/138 holds the well-formed side; only the refusals live here.
# \134 is the backslash, written in octal so the sequence survives this file.
# ..and C11 6.4.3p2 bars a UCN from naming a BASIC-SET character (under 00A0, bar
# $ @ `), a surrogate, or anything past the last code point -- so A for 'A' is a
# constraint violation, not a long spelling. gcc 13 refuses it; newer ones take C23's
# relaxation, which is why the cross gcc caught this and the host one did not.
for bad in '\134u00E' '\134U0001F60' '\134u' '\134uZZZZ' \
           '\134u0041' '\134u0000' '\134u009F' '\134uD800' '\134uDFFF' '\134U00110000'; do
  printf "char *s = \"$bad\";\nint m(void){return 0;}\n" > "$ho/.feat.c"
  moonrun -c -t x64 -o /dev/null "$ho/.feat.c" > /dev/null 2>&1 \
    && fail "a malformed universal character name was accepted: $bad"
done
for ok in '\134u0024' '\134u0040' '\134u0060' '\134u00A0' '\134U0010FFFF'; do
  printf "char *s = \"$ok\";\nint m(void){return 0;}\n" > "$ho/.feat.c"
  moonrun -c -t x64 -o /dev/null "$ho/.feat.c" > /dev/null 2>&1 \
    || fail "a legal universal character name was refused: $ok"
done

# the freestanding header set is C11 4p6: these two were the ones we did not ship
printf '#include <iso646.h>\n#include <stdalign.h>\nint m(void){return (1 and 2) + alignof(int);}\n' \
  > "$ho/.feat.c"
moonrun -c -t x64 -o /dev/null "$ho/.feat.c" > /dev/null 2>&1 || fail "iso646.h + stdalign.h"
echo "mooncc: C11 conditional features track the parity table on all six targets"

# --------------------------------------------- -fno-inline: real, and neutral
# TWO halves, and both are the point: the flag must BITE (more functions reach
# the object, always_inline included -- the driver's word outranks the source's)
# and it must not CHANGE THE ANSWER. 112 is the inline law's own program, so the
# splice-off run walks the same checks the splice-on run does.
moonrun            test/cc/112-alwaysinline.c "$ho/.ni-on"  > /dev/null 2>&1 || fail "-fno-inline: splice-on compile"
moonrun -fno-inline test/cc/112-alwaysinline.c "$ho/.ni-off" > /dev/null 2>&1 || fail "-fno-inline: splice-off compile"
"$ho/.ni-on";  a=$?
"$ho/.ni-off"; b=$?
[ $a -eq $b ] || fail "-fno-inline changed the answer (on $a, off $b)"
moonrun -c            test/cc/112-alwaysinline.c "$ho/.ni-on.o"  > /dev/null 2>&1 || fail "-fno-inline: -c splice-on"
moonrun -c -fno-inline test/cc/112-alwaysinline.c "$ho/.ni-off.o" > /dev/null 2>&1 || fail "-fno-inline: -c splice-off"
non=$(nm "$ho/.ni-on.o"  | grep -c ' [tT] ')
nof=$(nm "$ho/.ni-off.o" | grep -c ' [tT] ')
[ "$nof" -gt "$non" ] || fail "-fno-inline barred no splice ($non text syms either way)"
echo "mooncc: -fno-inline bars every splice ($non functions emitted, $nof with it) and answers the same"

# ------------------------------------- -fir / -fno-ir: the record, and its set algebra
# The compiler writing down what it built, for whoever opens the binary. The laws are the
# SET ALGEBRA: positives collect (repeated flag == comma list, and they must agree), then
# negatives carve, and with no positive at all the set opens so -fno-ir= alone means
# everything-but. ⚠ -fno-inline rides along on purpose -- these functions are small enough
# that a splice would fold them into main and there would be no record to ask about.
cat > "$ho/.ir.c" <<'EOF'
int aa_one(int x) { return x + 1; }
int aa_two(int x) { return x + 2; }
int bb_one(int x) { return x + 3; }
int main(void) { return aa_one(1) + aa_two(2) + bb_one(3) - 12; }
EOF
irbuild () { o="$1"; shift; moonrun -c -fno-inline "$@" "$ho/.ir.c" "$o" >/dev/null 2>&1 \
             || fail "-fir: compile ($*)"; }
# a record is written as `(name form..)`, so the name followed by a space is the probe
irhas () { grep -qa "($2 " "$1"; }
irwant () { irhas "$1" "$2" || fail "-fir: $3 -- expected $2 in the record"; }
irnot ()  { ! irhas "$1" "$2" || fail "-fir: $3 -- did NOT expect $2 in the record"; }

irbuild "$ho/.ir-a.o" -fir=aa_
irwant "$ho/.ir-a.o" aa_one "one prefix"; irwant "$ho/.ir-a.o" aa_two "one prefix"
irnot  "$ho/.ir-a.o" bb_one "one prefix"
irbuild "$ho/.ir-b.o" -fir=aa_,bb_                    # comma list
irbuild "$ho/.ir-c.o" -fir=aa_ -fir=bb_               # ..and the repeated flag
for n in aa_one aa_two bb_one; do
  irwant "$ho/.ir-b.o" $n "comma list"; irwant "$ho/.ir-c.o" $n "repeated flag"
done
cmp -s "$ho/.ir-b.o" "$ho/.ir-c.o" || fail "-fir: a comma list and a repeated flag differ"
irbuild "$ho/.ir-d.o" -fir=aa_ -fno-ir=aa_t           # positives, then the carve
irwant "$ho/.ir-d.o" aa_one "negative"; irnot "$ho/.ir-d.o" aa_two "negative"
irbuild "$ho/.ir-e.o" -fno-ir=aa_                     # no positive -> everything but
irwant "$ho/.ir-e.o" bb_one "bare negative"; irnot "$ho/.ir-e.o" aa_one "bare negative"
irbuild "$ho/.ir-f.o"                                 # and absent when never asked
nm "$ho/.ir-f.o" 2>/dev/null | grep -q " ai_ir_" && fail "-fir: a record with no flag"
# BARE, and the algebra gives them their meaning: -fir is the empty POSITIVE (every name has
# it as a prefix, so it opens the set from anywhere) and -fno-ir the empty NEGATIVE (and the
# negatives carve after the positives whatever the order, so it wins from either end without
# a precedence rule written anywhere). ⚠ before they were named they matched tol?'s advisory
# -f family and were accepted in SILENCE, answering a binary with no record.
irbuild "$ho/.ir-g.o" -fir
for n in aa_one aa_two bb_one; do irwant "$ho/.ir-g.o" $n "bare -fir is everything"; done
for args in "-fno-ir" "-fir=aa_ -fno-ir" "-fno-ir -fir=aa_"; do
  irbuild "$ho/.ir-h.o" $args
  nm "$ho/.ir-h.o" 2>/dev/null | grep -q " ai_ir_" && fail "-fno-ir did not empty the record ($args)"
done
# ⚠ and the empty set lays NO SYMBOL, not one holding "()": a reader finding the symbol would
# conclude the compiler wrote down that there was nothing, which is a different claim.
irbuild "$ho/.ir-i.o" -fir=zz_no_such_prefix
nm "$ho/.ir-i.o" 2>/dev/null | grep -q " ai_ir_" && fail "-fir: an empty set still laid a symbol"
echo "mooncc: -fir collects, -fno-ir carves, a comma list IS the repeated flag (byte-identical),"
echo "        bare -fir is all and bare -fno-ir is none from either end, and empty lays nothing"

# ------------------------------- the record READS BACK, on every target, from this machine
# ⚠ THIS IS THE CLAIM WORTH GATING: a record is text and an ELF is a table, so reading one
# asks nothing of the machine underneath -- no disassembler, no per-arch mnemonic table, no
# objdump built with the right target list. All six targets, read here on the host, through
# lib/irec.l (which is NOT lib/splice.l on purpose: the hook lights its lane on `from 'splice`,
# and looking at a binary must not start a JIT). ⚠ three of the six lay ELF32, where every
# header offset moves -- thumb2 read as "no record at all" until irec carried both classes.
for t in x64 arm64 riscv64 thumb2 thumb2sp thumb1; do
  moonrun -c -fno-inline -t $t -fir= "$ho/.ir.c" "$ho/.ir-$t.o" >/dev/null 2>&1 \
    || fail "-fir: compile for $t"
  n=$($m tools/ir.l "$ho/.ir-$t.o" aa_one 2>/dev/null | head -1)
  case "$n" in aa_one*) ;; *) fail "-fir: $t record does not read back ($n)" ;; esac
done
echo "mooncc: the record reads back on all six targets from this one machine (ELF32 and ELF64)"

# ------------------------------------------------------- the failure exits
moonrun "$ho/.cc-none.c" "$ho/.ccx" > /dev/null 2>&1; r=$?
[ $r -eq 1 ] || fail "mooncc missing input exit (rc $r)"
printf 'int main() { return 42 }\n' > "$ho/.cc3.c"
moonrun "$ho/.cc3.c" "$ho/.ccx" > /dev/null 2>&1; r=$?
[ $r -eq 1 ] || fail "mooncc parse-error exit (rc $r)"
# a refusal must NAME ITS CAUSE. an undeclared identifier in a static
# initializer used to print the initializer's whole IR ("CGDATA-BAD .."), which
# reads as a codegen gap rather than as a typo -- and was read as one.
cat > "$ho/.cc4.c" <<'EOF'
void *r[] = { (void *) nosuchthing, 0 };
int main(void) { return 0; }
EOF
moonrun "$ho/.cc4.c" "$ho/.ccx" > "$ho/.cc4.out" 2>&1; r=$?
[ $r -eq 1 ] || fail "mooncc undeclared-in-initializer exit (rc $r)"
grep -q "undeclared 'nosuchthing'" "$ho/.cc4.out" \
  || fail "mooncc undeclared-in-initializer must name it: $(head -1 "$ho/.cc4.out")"
# ..and the shape it must NOT refuse: a function's address IS a constant
cat > "$ho/.cc5.c" <<'EOF'
int puts(char const*);
void *const r[] = { (void *) puts, 0 };
int main(void) { return r[0] == 0; }
EOF
moonrun "$ho/.cc5.c" "$ho/.cc5" > /dev/null 2>&1 || fail "mooncc fn address in a static initializer"
"$ho/.cc5"; r=$?
[ $r -eq 0 ] || fail "mooncc fn address in a static initializer ran wrong (rc $r)"

# -------------------------------------------- -c objects, linked by the system ld
cat > "$ho/.olib.c" <<'EOF'
int vals[3] = {10,20,12};
char *tag = "x";
int pick(int i){return vals[i];}
EOF
cat > "$ho/.omain.c" <<'EOF'
extern int vals[];
int pick(int i);
int ext_add(int a,int b);
int main(){return pick(0)+vals[2]+ext_add(15,5);}
EOF
printf 'int ext_add(int a,int b){return a+b;}\n' > "$ho/.oext.c"
moonrun -c "$ho/.olib.c"  "$ho/.olib.o"  > /dev/null 2>&1 || fail "mooncc -c lib"
moonrun -c "$ho/.omain.c" "$ho/.omain.o" > /dev/null 2>&1 || fail "mooncc -c main"
$cc_g -O0 -c -o "$ho/.oext.o" "$ho/.oext.c"
$cc_g -no-pie -o "$ho/.oexe" "$ho/.omain.o" "$ho/.olib.o" "$ho/.oext.o" > /dev/null 2>&1 \
  || fail "ld cc objects"
"$ho/.oexe"; a=$?
$cc_g -O0 -o "$ho/.oexeg" "$ho/.omain.c" "$ho/.olib.c" "$ho/.oext.c" && "$ho/.oexeg"; b=$?
[ $a -eq 42 ] && [ $a -eq $b ] || fail "mooncc -c link+run (ours $a gcc $b)"

# --------------------------------------------------------------- -I / -D / -o
cat > "$ho/.flg.c" <<'EOF'
#include <ans.h>
int main() { return ANS + BONUS; }
EOF
mkdir -p "$ho/.flginc" && printf '#define ANS 30\n' > "$ho/.flginc/ans.h"
moonrun -I "$ho/.flginc" -D BONUS=12 -o "$ho/.flg" "$ho/.flg.c" > /dev/null 2>&1 \
  || fail "mooncc -I/-D/-o"
"$ho/.flg"; a=$?
[ $a -eq 42 ] || fail "mooncc -I/-D/-o run (got $a want 42)"

# ------------------------------------------------------------------ inline asm
printf 'int main() { long v; asm("li %%0, 40" : "=r"(v)); return v + 2; }\n' > "$ho/.casm1.c"
moonrun "$ho/.casm1.c" "$ho/.casm1" > /dev/null 2>&1 || fail "mooncc asm compile"
"$ho/.casm1"; a=$?
[ $a -eq 42 ] || fail "mooncc asm output operand (got $a want 42)"

printf 'int main() { asm volatile("sys" : : "r0"(60), "r6"(42)); return 0; }\n' > "$ho/.casm2.c"
moonrun "$ho/.casm2.c" "$ho/.casm2" > /dev/null 2>&1 || fail "mooncc asm syscall compile"
"$ho/.casm2"; a=$?
[ $a -eq 42 ] || fail "mooncc asm pinned-reg syscall (got $a want 42)"

printf 'int main() { long x = 30; asm("add %%0, %%0, %%1" : "+r"(x) : "r"(12L)); return x; }\n' > "$ho/.casm3.c"
moonrun "$ho/.casm3.c" "$ho/.casm3" > /dev/null 2>&1 || fail "mooncc asm in-out compile"
"$ho/.casm3"; a=$?
[ $a -eq 42 ] || fail "mooncc asm in-out (got $a want 42)"

moonrun -t arm64 -o "$ho/.casm1a" "$ho/.casm1.c" > /dev/null 2>&1 || fail "mooncc asm arm64 compile"

# the same template through the BOOTSTRAP compiler. every check above rides the
# default love, and inline asm is the one feature whose front end (the combinators
# text.l parses templates with) is reached by a bare name each lane binds itself --
# so mooncc0 losing it while mooncc keeps it is a live shape, not a hypothetical.
moon0 -o "$ho/.casm0" "$ho/.casm1.c" > /dev/null 2>&1 || fail "mooncc0 asm compile"
"$ho/.casm0"; a=$?
[ $a -eq 42 ] || fail "mooncc0 inline asm (got $a want 42)"

# ------------------------------------------------------------- multi-input -c
cat > "$ho/.mi1.c" <<'EOF'
int f();
int main() { return f() + 2; }
EOF
printf 'int f() { return 40; }\n' > "$ho/.mi2.c"
# -c with several inputs writes each .o beside its source, so it runs IN $ho
mabs="$PWD/$ho"
( cd "$ho" && "$mabs/love" wake "$mabs/mooncc.image" \
    mooncc -c .mi1.c .mi2.c ) > /dev/null 2>&1 \
  || fail "mooncc multi-input -c"
$cc_g -no-pie -o "$ho/.mi" "$ho/.mi1.o" "$ho/.mi2.o" > /dev/null 2>&1 \
  || fail "ld multi-input objects"
"$ho/.mi"; a=$?
[ $a -eq 42 ] || fail "mooncc multi-input run (got $a want 42)"

# ------------------------------------- SysV varargs, called ACROSS toolchains
cat > "$ho/.valib.c" <<'EOF'
#include <stdarg.h>
int isum(int n,...){va_list ap;va_start(ap,n);long s=0;for(int i=0;i<n;i++)s+=va_arg(ap,int);va_end(ap);return s;}
EOF
cat > "$ho/.vamain.c" <<'EOF'
int isum(int n,...);
int main(){return isum(4,10,11,12,9);}
EOF
moonrun -c "$ho/.valib.c" "$ho/.valib.o" > /dev/null 2>&1 || fail "mooncc -c variadic"
$cc_g -O0 -c -o "$ho/.vamain.o" "$ho/.vamain.c"
$cc_g -no-pie -o "$ho/.vaexe" "$ho/.vamain.o" "$ho/.valib.o" > /dev/null 2>&1 \
  || fail "ld cc-variadic + gmoon-main"
"$ho/.vaexe"; a=$?
[ $a -eq 42 ] || fail "cc-variadic <- gcc-caller (SysV va ABI, got $a want 42)"

# ------------------------------------------------------------- weak overriding
cat > "$ho/.wklib.c" <<'EOF'
__attribute__((weak)) int wpick(void){return 7;}
int main(){return wpick() + 30;}
EOF
printf 'int wpick(void){return 12;}\n' > "$ho/.wkstr.c"
moonrun -c "$ho/.wklib.c" "$ho/.wklib.o" > /dev/null 2>&1 || fail "mooncc -c weak"
$cc_g -no-pie -o "$ho/.wkdef" "$ho/.wklib.o" > /dev/null 2>&1 && "$ho/.wkdef"; a=$?
[ $a -eq 37 ] || fail "weak default (got $a want 37)"
$cc_g -O0 -c -o "$ho/.wkstr.o" "$ho/.wkstr.c"
$cc_g -no-pie -o "$ho/.wkovr" "$ho/.wklib.o" "$ho/.wkstr.o" > /dev/null 2>&1 && "$ho/.wkovr"; a=$?
[ $a -eq 42 ] || fail "weak override (got $a want 42)"

# ------------------------------------------- callee-saved rbx across a cc call
printf 'long bump(long x){long r;__builtin_add_overflow(x,1,&r);return r;}\n' > "$ho/.rblib.c"
cat > "$ho/.rbmain.c" <<'EOF'
long bump(long);
int main(void){volatile long a=0;long s=a;for(int i=42;i--;)s=bump(s);return (int)s;}
EOF
moonrun -c "$ho/.rblib.c" "$ho/.rblib.o" > /dev/null 2>&1 || fail "mooncc -c rbx-callee"
$cc_g -O2 -c -o "$ho/.rbmain.o" "$ho/.rbmain.c"
$cc_g -no-pie -o "$ho/.rbexe" "$ho/.rbmain.o" "$ho/.rblib.o" > /dev/null 2>&1 || fail "ld rbx interop"
"$ho/.rbexe"; a=$?
[ $a -eq 42 ] || fail "callee-saved rbx across cc call (-O2 caller loop bound, got $a want 42)"

# ------------------------------------------------------- guaranteed sibcalls
cat > "$ho/.sib.c" <<'EOF'
static long cd(long n,long a){if(n==0)return a;return cd(n-1,a+1);}
static long tb(long n);
static long ta(long n){if(n==0)return 21;return tb(n-1);}
static long tb(long n){if(n==0)return 22;return ta(n-1);}
static long dp(long(*f)(long,long),long n){return f(n,0);}
int main(void){long a=cd(50000000,0)/2500000;long b=ta(30000000);long c=dp(cd,1000000)/1000000;return (int)(a+b+c);}
EOF
moonrun "$ho/.sib.c" "$ho/.sibx" > /dev/null 2>&1 || fail "mooncc sibcall"
"$ho/.sibx"; a=$?
[ $a -eq 42 ] \
  || fail "sibcall flat recursion (50M-deep self + mutual + fn-ptr, got $a want 42 -- a non-tail call would stack-overflow to 139)"

# --------------------------------------------------- 16-byte stack alignment
printf 'long fork(void);long waitpid(long,int*,long);void _exit(long);long g;long id(long a){return a;}int main(void){g=id(fork());if(g==0)_exit(42);int st;waitpid(-1,&st,0);return ((st&127)==0&&((st>>8)&255)==42)?42:1;}\n' > "$ho/.aln.c"
moonrun -c "$ho/.aln.c" "$ho/.aln.o" > /dev/null 2>&1 || fail "mooncc -c stack-align"
$cc_g -no-pie -o "$ho/.alnx" "$ho/.aln.o" > /dev/null 2>&1 || fail "ld stack-align"
"$ho/.alnx"; a=$?
[ $a -eq 42 ] \
  || fail "16-byte stack alignment ('g=id(fork())' holds a temp across the call; a bare-push spill leaves rsp at 8 mod 16 and glibc fork's child movaps #GPs -- got $a want 42)"

# ------------------------------------------------------- OUR OWN static linker
moonrun -c "$ho/.oext.c" "$ho/.oext2.o" > /dev/null 2>&1 || fail "mooncc -c ext"
moonrun "$ho/.omain.o" "$ho/.olib.o" "$ho/.oext2.o" -o "$ho/.lnk1" > /dev/null 2>&1 \
  || fail "mooncc link .o"
"$ho/.lnk1"; a=$?
[ $a -eq 42 ] || fail "mooncc-linked exe (own static linker, got $a want 42)"

moonrun "$ho/.mi1.c" "$ho/.mi2.c" -o "$ho/.lnk2" > /dev/null 2>&1 || fail "mooncc link multi-.c"
"$ho/.lnk2"; a=$?
[ $a -eq 42 ] || fail "mooncc multi-.c link (got $a want 42)"

moonrun -c "$ho/.wkstr.c" "$ho/.wkstr2.o" > /dev/null 2>&1 || fail "mooncc -c weak-strong"
moonrun "$ho/.wklib.o" -o "$ho/.lnk3" > /dev/null 2>&1 && "$ho/.lnk3"; a=$?
[ $a -eq 37 ] || fail "mooncc-link weak default (got $a want 37)"
moonrun "$ho/.wklib.o" "$ho/.wkstr2.o" -o "$ho/.lnk4" > /dev/null 2>&1 && "$ho/.lnk4"; a=$?
[ $a -eq 42 ] || fail "mooncc-link weak override (got $a want 42)"

# the ai_nifs bracket: two TUs packed into one section, __start_/__stop_ synthesized
cat > "$ho/.nf1.c" <<'EOF'
typedef struct { char *n; long v; } ent;
__attribute__((section("ai_nifs"))) ent e1 = { "a", 30 };
EOF
cat > "$ho/.nf2.c" <<'EOF'
typedef struct { char *n; long v; } ent;
__attribute__((section("ai_nifs"))) ent e2 = { "b", 12 };
extern ent __start_ai_nifs[];
extern ent __stop_ai_nifs[];
int main(){ long s=0; for (ent *p=__start_ai_nifs; p<__stop_ai_nifs; p++) s+=p->v; return (int)s; }
EOF
moonrun "$ho/.nf2.c" "$ho/.nf1.c" -o "$ho/.lnk5" > /dev/null 2>&1 || fail "mooncc link ai_nifs"
"$ho/.lnk5"; a=$?
[ $a -eq 42 ] || fail "ai_nifs bracket walk (two TUs packed + __start_/__stop_ synthesized, got $a want 42)"

# a COMPILER-NAMED section -- one the linker has no word for. it becomes its own
# lane and stays WHOLE: the two TUs' entries land adjacent, in the order given.
cat > "$ho/.mt1.c" <<'EOF'
typedef struct { char *n; long v; } ent;
__attribute__((section("mytab"))) ent e1 = { "a", 30 };
EOF
cat > "$ho/.mt2.c" <<'EOF'
typedef struct { char *n; long v; } ent;
__attribute__((section("mytab"))) ent e2 = { "b", 12 };
extern ent e1;
int main(){ long d=(char*)&e2-(char*)&e1; if (d!=(long)sizeof(ent) && d!=-(long)sizeof(ent)) return 1; return (int)(e1.v+e2.v); }
EOF
moonrun "$ho/.mt2.c" "$ho/.mt1.c" -o "$ho/.lnk6" > /dev/null 2>&1 || fail "mooncc link a named section"
"$ho/.lnk6"; a=$?
[ $a -eq 42 ] || fail "named-section lane (two TUs, contiguous, got $a want 42; 1 = the entries were not adjacent)"

# ..and which known lane it travels with is its FLAGS. a gcc TU says all three at
# once -- the code home has no mooncc twin below, since a section a FUNCTION names
# is executable by construction and never had a flag to get wrong.
cat > "$ho/.hm1.c" <<'EOF'
const long __attribute__((section("rotab"))) c1 = 30;
__attribute__((section("mycode"),noinline)) int f12(int x){ return x + 10; }
EOF
cat > "$ho/.hm2.c" <<'EOF'
extern const long c1;
int f12(int);
__attribute__((section("mytab"))) long w1 = 12;
int main(){ if ((unsigned long)&c1 >= (unsigned long)&w1) return 1; return (int)(c1 + f12(2)); }
EOF
$cc_g -O2 -c -o "$ho/.hm1.o" "$ho/.hm1.c" || fail "gcc -c named sections"
moonrun "$ho/.hm2.c" "$ho/.hm1.o" -o "$ho/.lnk7" > /dev/null 2>&1 || fail "mooncc link named-section homes"
"$ho/.lnk7"; a=$?
[ $a -eq 42 ] || fail "named-section homes (AX->text, A->rodata below data, WA->data; got $a want 42; 1 = rodata did not land below data)"

# ..and mooncc says the read-only one ITSELF: a named section whose every global is
# const and reloc-free comes out ALLOC-only. one holding an ADDRESS stays writable --
# the .rodata rule, and gcc's answer too (.data.rel.ro is the lane gcc renames it to,
# and a section the programmer NAMED cannot be renamed).
# ⚠ r2's const leads the SPECIFIER run: a declarator-side `void *const r2[]` is no
# const object here at all (parse.l's cobj?) and never reaches the relocation guard.
# ⚠ mixtab holds one of each in BOTH orders -- gcc refuses the mix outright ("section
# type conflict"), we take the writable reading, and a walk that stops at the first
# member it likes reads as unanimous from whichever end it starts.
cat > "$ho/.hm3.c" <<'EOF'
int f13(int);
struct ent { void *p; long v; };
const long __attribute__((section("rotab2"))) c2 = 30;
const struct ent __attribute__((section("reltab"), used)) r2[] = { { (void *) f13, 2 } };
__attribute__((section("mytab2"))) long w2 = 12;
const long __attribute__((section("mixtab"), used)) m1 = 5;
long __attribute__((section("mixtab"), used)) m2 = 6;
long __attribute__((section("mixtab2"), used)) m3 = 7;
const long __attribute__((section("mixtab2"), used)) m4 = 8;
__attribute__((noinline)) int f13(int x){ return x + 10; }
int main(){ if (r2[0].p != (void *) f13) return 1;
            return (int)(c2 + w2 + m1 + m2 + m3 + m4 + f13(-36)); }
EOF
moonrun "$ho/.hm3.c" -o "$ho/.lnk8" > /dev/null 2>&1 || fail "mooncc link its own const named section"
"$ho/.lnk8"; a=$?
[ $a -eq 42 ] || fail "mooncc const named section (got $a want 42; 1 = the relocated entry is wrong)"
# ⚠ read the home off nm, not off an address comparison: these order by declaration
# and so agree with the claim whether or not the flags do.
nm "$ho/.lnk8" > "$ho/.hm3.nm" 2>&1 || fail "nm on the named-flags exe"
for s in "R c2" "D r2" "D w2" "D m1" "D m2" "D m3" "D m4"; do
  grep -q " $s\$" "$ho/.hm3.nm" \
    || fail "mooncc named-section flags: '$s' is not where it belongs ($(grep " ${s#* }\$" "$ho/.hm3.nm")) -- R c2 = const and reloc-free goes ALLOC-only, D r2 = one holding an address keeps its write bit, D m1 = one writable member decides for the whole section"
done

# ------------------------------------------------- a FOREIGN gcc .o, linked whole
cat > "$ho/.fgn.c" <<'EOF'
long bigbuf[4096];
int zed;
static const char *const nms[]={"zero","one","two"};
const long tbl[4]={3,5,7,9};
long fill(long n){long i;for(i=0;i<n;i++)bigbuf[i]=i+1;zed=(int)bigbuf[n-1];return bigbuf[0]+bigbuf[n-1];}
const char *nm(int i){return nms[i];}
long tb(int i){return tbl[i];}
EOF
cat > "$ho/.fgnm.c" <<'EOF'
extern long bigbuf[];extern int zed;long fill(long);const char*nm(int);long tb(int);
static int eq(const char*a,const char*b){while(*a&&*a==*b){a++;b++;}return *a==*b;}
int main(void){if(zed||bigbuf[5]||bigbuf[4095])return 1;if(!eq(nm(1),"one"))return 2;if(tb(3)!=9)return 3;long s=fill(9);return (int)(s+zed+23);}
EOF
for mode in -fno-pie -fPIE; do
  $cc_g -O0 $mode -c -o "$ho/.fgn.o" "$ho/.fgn.c"
  moonrun "$ho/.fgnm.c" "$ho/.fgn.o" -o "$ho/.fgnx" > /dev/null 2>&1 \
    || fail "mooncc link foreign .o ($mode)"
  "$ho/.fgnx"; a=$?
  [ $a -eq 42 ] \
    || fail "foreign gcc .o link $mode (.bss arrives zeroed + .rodata/.data.rel.ro + abs32 relocs, got $a want 42)"
done

# ------------------------------------------------------- .comment, the producer
# what wrote the file, in the section every toolchain writes it in -- read back
# with our own ELF walk, so this holds on any machine and needs no readelf.
# the UNION is the point: .fgnx is our link over a gcc object, and it must credit
# gcc for the code gcc compiled rather than claiming the whole binary.
# ⚠ ours is the BASE half of love-version and never the whole id, and that is a law:
# the VCS suffix names the commit that built the COMPILER, so it would make love1 and
# love2 differ and name a broken fixpoint (crew/holo/link.l says it at the door).
# read ./VERSION rather than writing 0.1 down -- a release bump must not fail here.
cmt() { "$m" -l lib/irec.l \
          -e "(: r (irec-secof \"$1\" \".comment\") _ (? (two? r) (puts <r) 0) _ (flush out) (quit 0))"; }
c=$(cmt "$ho/.fgnx" | tr '\0' ' ')
case "$c" in
  *GCC*mooncc*) ;;
  *) fail "mooncc link over a gcc .o must credit both in .comment, got '$c'" ;;
esac
c=$(cmt "$ho/.sibx" | tr '\0' ' ')
[ "$c" = "mooncc $(cat VERSION) " ] \
  || fail "an all-ours link says 'mooncc <base>' in .comment (base only -- the fixpoint law), got '$c'"

# ..and our own binaries carry a symbol table nm and gdb can read
nm "$ho/.fgnx" > "$ho/.fgn.nm" 2>&1 || fail "nm on our exe (no symbol table)"
for s in "T main" "T fill" "B bigbuf" "B zed" "R tbl"; do
  grep -q " $s\$" "$ho/.fgn.nm" \
    || fail "symtab missing '$s' (nm must classify by the section the symbol lives in)"
done

# ------------------------------------------------- const globals reach .rodata
# a `const` in the SPECIFIER run names the object, and an object no relocation
# touches may ride the read-only lane. the two exclusions are the whole rule:
# a const POINTEE leaves the pointer writable, and a const array of ADDRESSES is
# a lane the linker writes (gcc spells that one .data.rel.ro).
cat > "$ho/.ro.c" <<'EOF'
const long rotbl[4] = {3,5,7,9};
const char romsg[] = "const bytes";
const char *mutp = 0;
static void h(void){}
void (*const fnp[1])(void) = { h };
long wtbl[2] = {1,2};
const long rozero[2];
int main(){ return (int)(rotbl[3] + romsg[0] + wtbl[0] + rozero[0]
                         + (mutp!=0) + (fnp[0]!=0)); }
EOF
moonrun "$ho/.ro.c" -o "$ho/.rox" > /dev/null 2>&1 || fail "mooncc const-lane compile"
"$ho/.rox"; a=$?
[ $a -eq 110 ] || fail "const lane semantics (got $a want 110 = 9 + 'c' + 1 + 0 + 0 + 1)"
nm "$ho/.rox" > "$ho/.ro.nm" 2>&1 || fail "nm on the const-lane exe"
for s in "R rotbl" "R romsg" "D mutp" "D fnp" "D wtbl" "B rozero"; do
  grep -q " $s\$" "$ho/.ro.nm" \
    || fail "const lane: '$s' is not where it belongs ($(grep " ${s#* }\$" "$ho/.ro.nm"))"
done

echo "mooncc: cc (laws + return-42 + a $(ls test/cc/*.c | wc -l)-program gcc battery + .o link/interop + -I/-D/-o + multi-input -c + inline asm on both compiler lanes + SysV varargs cross-toolchain + weak override + callee-saved rbx + guaranteed sibcalls + 16-byte stack alignment + our own static linker: multi-.o/.c link, weak strong-over, ai_nifs brackets, named-section lanes + their const/writable flag homes, const globals to .rodata, a FOREIGN gcc .o whole, a symbol table nm/gdb read, a .comment naming every producer) ok"


# ------------------------------------------------ the warm compiler
# moon-run ANSWERS its status (moon-main is the same compile, quitting), so one
# image compiles again after a compile that failed -- the cat is read once and
# every cc after it is free. four compiles in one process: good, a hard error, a
# usage error, then good again; the process must reach the last say, the statuses
# must be 0 1 2 0, and the object laid AFTER the two failures must be byte-identical
# to the same compile run cold. ⚠ a regression to `quit` inside moon-run passes
# every check above this line.
printf 'int wa(int x){return x+1;}\n' > "$ho/.wa.c"
printf 'int wb(void){ return nope; }\n' > "$ho/.wb.c"
moonrun -c "$ho/.wa.c" -o "$ho/.wa-cold.o" > /dev/null 2>&1 || fail "warm: the cold reference compile"
warm=$(printf '(: a (moon-run (list "-c" "%s" "-o" "%s"))
                  b (moon-run (list "-c" "%s" "-o" "/dev/null"))
                  c (moon-run (list "-zzz"))
                  d (moon-run (list "-c" "%s" "-o" "%s"))
                  _ (say out (show a + " " + show b + " " + show c + " " + show d + "\n"))
                  (quit 0))' \
             "$ho/.wa.c" "$ho/.wa-warm.o" "$ho/.wb.c" "$ho/.wa.c" "$ho/.wa-warm.o")
"$m" wake "$ho/mooncc.image" -e "$warm" > "$ho/.warm.out" 2>/dev/null
r=$?
[ $r -eq 0 ] || fail "warm mooncc: the image did not survive a failed compile (exit $r)"
[ "$(tail -1 "$ho/.warm.out")" = "0 1 2 0" ] || fail "warm mooncc statuses: $(tail -1 "$ho/.warm.out")"
cmp -s "$ho/.wa-cold.o" "$ho/.wa-warm.o" \
  || fail "warm mooncc: the object after two failures differs from the cold one"
echo "mooncc: the warm compiler (moon-run answers, the image compiles on past a failure) ok"
