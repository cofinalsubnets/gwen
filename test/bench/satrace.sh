#!/bin/sh
# satrace.sh -- the SAT-solver shootout. Emits raw "<instance> <solver> <ms> <verdict>"
# lines (the analogue of run.sh's per-language stream) for the SECOND table on
# bench.html. Three row families:
#   php5..php8 -- pigeonhole, UNSAT and resolution-hard: exercises conflict learning
#     and the fbva factoring pass (CDCL alone is exponential here, re-encoding isn't).
#   rnd100/rnd150 -- random 3-SAT at the threshold (m = 4.26n, 5 fixed-seed instances
#     summed): raw search with NO factorable structure, the guard against tuning the
#     solver into a pigeonhole specialist. The verdict field is the per-instance
#     signature ("uusus"), so solver agreement is visible in the raw stream.
#   uf100/uuf100/uuf150/uf250/flat100 -- REAL benchmark-library instances (SATLIB,
#     the classic competition-era suite): uniform random 3-SAT at the phase
#     transition, SATISFIABLE (uf) and PROVEN-UNSATISFIABLE (uuf) sets, plus flat
#     graph 3-coloring; a fixed file prefix per row, summed, signature verdicts.
#     Downloaded once into out/bench/satlib/ (rows silently skip if offline).
# The rnd instances are drawn from love's own xoshiro (seed/random, reproducible), and
# the SAME generator text feeds both the DIMACS dump and love's in-process lane, so
# every solver sees identical instances by construction; the SATLIB rows feed love the
# byte-identical files, converted to a formula literal by awk.
#
# love's `fcdcl` (crew/sat/flat.l: flat-arena CDCL + the crew/holo/-emitted native BCP kernel)
# is timed by its OWN clock around the solve call, so the interpreter warmup + the
# self-tests (which would otherwise dominate) are
# excluded -- the honest "solve time". External solvers are timed by process
# wall-clock (their startup is ~ms). A solver not on $PATH is skipped (a dotted
# cell); one that exceeds $TIMEOUT is reported `timeout`.
#
# usage: ./satrace.sh [timeout-seconds]   (default 30)
# NB: no `set -e` -- SAT solvers exit non-zero by convention (minisat: 10=SAT,
# 20=UNSAT), and `timeout` exits 124, all of which are normal control flow here.
R=..
GL=$R/out/host/love
export LOVE_NO_IMAGE=1   # REQUIRED for the flat solver's native BCP kernel: crew/sat/flat.l installs it
                       # through the `nif` seam, which the glazed image mops from the book (the
                       # no-image book keeps it). the old glaze<->sat.l miscompile is gone.
TIMEOUT=${1:-30}
INSTANCES="5 6 7 8"
RNDN="100 150"        # random-3-SAT row sizes; m = round(4.26 n), seeds 1000..1004
RNDK=5
SOLVERS="minisat cadical kissat glucose picosat"   # external; love is special-cased
CNF=$R/out/bench/cnf
mkdir -p "$CNF"

# the ONE generator text (leaks gen2 from a body-less top-level `:`): standard random
# 3-SAT, each literal an independent (var, sign) draw from the explicit-state RNG.
GEN2='(: (gen2 sd n m)
    ((: (go i st acc)
        (? (>= i m) acc
           (: p1 (random st n) v1 (+ 1 (cap p1))
              s1 (random (cup p1) 2)
              p2 (random (cup s1) n) v2 (+ 1 (cap p2))
              s2 (random (cup p2) 2)
              p3 (random (cup s2) n) v3 (+ 1 (cap p3))
              s3 (random (cup p3) 2)
              (go (+ i 1) (cup s3)
                  (link (link (? (= 0 (cap s1)) v1 (- 0 v1))
                        (link (? (= 0 (cap s2)) v2 (- 0 v2))
                        (link (? (= 0 (cap s3)) v3 (- 0 v3)) ()))) acc)))))
     0 (wheel sd) ()))'
rndm() { awk -v n="$1" 'BEGIN{printf "%d", int(n*4.26+0.5)}'; }

# -- generate the DIMACS once (php re-stated here, the textbook encoding, so the
#    files match crew/sat/sat.l's (php h) without loading its self-test). --
gen() {
  cat <<'AI'
(: (neg v) (- 0 v)
   (php h) (: P (+ h 1)
      pc (map (\ p (map (\ k (+ (* p h) (+ k 1))) (jot h))) (jot P))
      hc (foldl (\ a k (cat a (foldl (\ a2 p (cat a2 (foldl (\ a3 q
             (? (< p q) (link (link (neg (+ (* p h) (+ k 1))) (link (neg (+ (* q h) (+ k 1))) ())) a3) a3))
             () (jot P)))) () (jot P)))) () (jot h))
      (cat pc hc))
   (php-vars h) (* (+ h 1) h)
   (lits cl) (foldl (\ a x (+ a (+ (show x) " "))) "" cl)
   (dump h) (: cs (php h)
      _ (puts (+ "p cnf " (+ (show (php-vars h)) (+ " " (+ (show (tally cs)) "\n")))))
      (foldl (\ _ cl (puts (+ (lits cl) "0\n"))) 0 cs)))
AI
  echo "(dump $1)"
}
for h in $INSTANCES; do
  [ -f "$CNF/php$h.cnf" ] || gen "$h" | "$GL" 2>/dev/null > "$CNF/php$h.cnf"
done

# -- the random-3-SAT files, one per (size, seed): the SAME gen2 the love lane runs. --
genrnd() { # $1 = n, $2 = m, $3 = seed
  printf '%s\n' "$GEN2"
  cat <<'AI'
(: (lits cl) (foldl (\ a x (+ a (+ (show x) " "))) "" cl)
   (dumpf n m sd) (: cs (gen2 sd n m)
      _ (puts (+ "p cnf " (+ (show n) (+ " " (+ (show (tally cs)) "\n")))))
      (foldl (\ _ cl (puts (+ (lits cl) "0\n"))) 0 cs))
AI
  echo "   _ (dumpf $1 $2 $3))"
}
for n in $RNDN; do
  m=$(rndm "$n")
  i=0; while [ "$i" -lt "$RNDK" ]; do
    [ -f "$CNF/rnd$n-$i.cnf" ] || genrnd "$n" "$m" $((1000+i)) | "$GL" 2>/dev/null > "$CNF/rnd$n-$i.cnf"
    i=$((i+1))
  done
done

# -- wall-clock a command in ms; echoes "<ms> <exit>" (exit 124 = timed out). --
wall() {
  t0=$(date +%s.%N)
  timeout "$TIMEOUT" "$@" >/dev/null 2>&1; ex=$?
  t1=$(date +%s.%N)
  awk -v a="$t0" -v b="$t1" 'BEGIN{printf "%.3f", (b-a)*1000}'
  echo " $ex"
}

for h in $INSTANCES; do
  # love: its own solve clock (warmup + self-test excluded), under a process timeout.
  # one full throwaway solve first: kernels assemble+cache AND the solver's own lisp
  # (fbva/fmk) JITs, both OUTSIDE the clock, like the interpreter warmup -- the honest
  # "solve time" is the warm one (the rnd lane already warms the same way). (clock 0)
  # is INTEGER milliseconds, and php5/php6 solve sub-ms -- a single solve would round
  # to 0. So clock K solves in one span and report the total; the shell divides for a
  # fractional ms, matching the C solvers' %.3f. Reported as "RESULT <total-ms> <K> <verdict>".
  out=$(printf '(: _ (fcdcl (php %s) (php-vars %s)) K 32 t0 (clock 0) (rep i r) (? (>= i K) r (rep (+ i 1) (fcdcl (php %s) (php-vars %s)))) r (rep 0 ()) tot (- (clock 0) t0) _ (puts (+ "RESULT " (+ (show tot) (+ " " (+ (show K) (+ " " (show r))))))))' "$h" "$h" "$h" "$h" \
        | cat "$R/crew/sat/sat.l" "$R/crew/sat/flat.l" - | timeout "$TIMEOUT" "$GL" 2>/dev/null | grep -a '^RESULT' || true)
  if [ -n "$out" ]; then
    echo "php$h love $(echo "$out" | awk '{printf "%.3f %s", $2/$3, $4}')"
  else
    echo "php$h love timeout dnf"
  fi
  # external solvers: process wall-clock; verdict from the DIMACS SAT/UNSAT line.
  for s in $SOLVERS; do
    command -v "$s" >/dev/null 2>&1 || continue
    res=$(wall "$s" "$CNF/php$h.cnf"); ms=$(echo "$res" | cut -d' ' -f1); ex=$(echo "$res" | cut -d' ' -f2)
    if [ "$ex" = 124 ]; then echo "php$h $s timeout dnf"; else echo "php$h $s $ms unsat"; fi
  done
done

# -- the random rows: a batch of RNDK instances summed; verdict = the signature. --
RNDDRV=$(cat <<'AI'
(: _ (fcdcl (gen2 1000 @N@ @M@) @N@)
   res ((: (go i ms sig)
           (? (>= i @K@) (link ms sig)
              (: f (gen2 (+ 1000 i) @N@ @M@)
                 c0 (clock 0) r (fcdcl f @N@) d (- (clock 0) c0)
                 (go (+ i 1) (+ ms d) (+ sig (? (id? r 'unsat) "u" "s"))))))
        0 0 "")
   _ (puts (+ "RESULT " (+ (show (cap res)) (+ " " (+ (cup res) "\n"))))))
AI
)
for n in $RNDN; do
  m=$(rndm "$n")
  # love: instance 0 solved once first (kernel assembly for the size bucket + warmth),
  # then each solve clocked in-process and summed -- same accounting as the php rows.
  out=$({ printf '%s\n' "$GEN2"
          printf '%s\n' "$RNDDRV" | sed "s/@N@/$n/g; s/@M@/$m/g; s/@K@/$RNDK/g"; } \
        | cat "$R/crew/sat/sat.l" "$R/crew/sat/flat.l" - | timeout "$TIMEOUT" "$GL" 2>/dev/null | grep -a '^RESULT' || true)
  if [ -n "$out" ]; then
    echo "rnd$n love $(echo "$out" | awk '{print $2, $3}')"
  else
    echo "rnd$n love timeout dnf"
  fi
  # external solvers: sum the batch; the signature from the 10/20 exit convention.
  for s in $SOLVERS; do
    command -v "$s" >/dev/null 2>&1 || continue
    tot=0; sig=""; to=0
    i=0; while [ "$i" -lt "$RNDK" ]; do
      res=$(wall "$s" "$CNF/rnd$n-$i.cnf"); ms=$(echo "$res" | cut -d' ' -f1); ex=$(echo "$res" | cut -d' ' -f2)
      if [ "$ex" = 124 ]; then to=1; break; fi
      tot=$(awk -v a="$tot" -v b="$ms" 'BEGIN{printf "%.3f", a+b}')
      case "$ex" in 10) sig="${sig}s";; 20) sig="${sig}u";; *) sig="${sig}?";; esac
      i=$((i+1))
    done
    if [ "$to" = 1 ]; then echo "rnd$n $s timeout dnf"; else echo "rnd$n $s $tot $sig"; fi
  done
done

# -- the SATLIB rows: real benchmark-library instances, downloaded once and cached.
#    Each row = a fixed set of files summed; verdict = the per-file signature.
SLIB=$R/out/bench/satlib
SATLIB_URL="https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT"
# fetch <tarball-subpath> <glob-of-wanted-files> -- extract matching files FLAT into $SLIB
fetch() {
  tb=$(basename "$1")
  mkdir -p "$SLIB/tmp.$$" 2>/dev/null
  [ -f "$SLIB/$tb" ] || curl -s --max-time 120 -o "$SLIB/$tb" "$SATLIB_URL/$1" || { rm -f "$SLIB/$tb"; return 1; }
  tar xzf "$SLIB/$tb" -C "$SLIB/tmp.$$" 2>/dev/null || return 1
  # strip the SATLIB "%"-footer in transit: minisat/cadical REJECT it as a parse
  # error (exit 1/3 in ~3ms -- a fake "solve"; the signature column catches it).
  find "$SLIB/tmp.$$" -name "$2" | while read -r f; do
    awk '/^%/{exit} {print}' "$f" > "$SLIB/$(basename "$f")"
  done
  rm -rf "$SLIB/tmp.$$"
}
# row spec: <row> <tarball-subpath> <file-prefix> <count>. SATLIB names files
# "<prefix>-0<i>.cnf" (a literal 0 separator: -01, -09, -010, -0999); the GCP sets
# use plain "<prefix>-<i>.cnf" -- enumerate both.
slibrow() {
  row=$1; tbp=$2; pre=$3; cnt=$4
  files=""
  i=1; while [ "$i" -le "$cnt" ]; do
    f2="$pre-0$i.cnf"; f1="$pre-$i.cnf"
    if   [ -f "$SLIB/$f2" ]; then files="$files $SLIB/$f2"
    elif [ -f "$SLIB/$f1" ]; then files="$files $SLIB/$f1"
    else fetch "$tbp" "$pre-*.cnf" >/dev/null 2>&1
         if   [ -f "$SLIB/$f2" ]; then files="$files $SLIB/$f2"
         elif [ -f "$SLIB/$f1" ]; then files="$files $SLIB/$f1"
         else return 0; fi   # offline / missing -> skip the whole row silently
    fi
    i=$((i+1))
  done
  # love: embed each file as a formula literal (awk; the % footer ends the body),
  # one process for the row -- warm on the first file, then clock each solve. all
  # forms are BODY-LESS top-level `:` (the leaking idiom); state rides the boxes.
  drv=$(for f in $files; do
    n=$(awk '/^p/{print $3; exit}' "$f")
    lst=$(awk '$1=="%"{exit} /^[cp]/{next} {for(j=1;j<=NF;j++){ if($j=="0"){printf ") "; o=0} else {if(!o){printf "("; o=1}; printf "%s ", $j}}}' "$f")
    printf '(: f (\\ (%s)) _ (? (peep W 0 0) 0 (fcdcl f %s)) t0 (clock 0) r (fcdcl f %s) _ (pin MS 0 (+ (peep MS 0 0) (- (clock 0) t0))) _ (pin SG 0 (+ (peep SG 0 "") (? (id? r (\\ unsat)) "u" "s"))) _ (pin W 0 1))\n' "$lst" "$n" "$n"
  done)
  out=$({ printf '(: MS #0 SG #(0 "") W #0)\n'
          printf '%s\n' "$drv"
          printf '(: _ (puts (+ "RESULT " (+ (show (peep MS 0 0)) (+ " " (+ (peep SG 0 "") "\n"))))))\n'; } \
        | cat "$R/crew/sat/sat.l" "$R/crew/sat/flat.l" - | timeout "$TIMEOUT" "$GL" 2>/dev/null | grep -a '^RESULT' || true)
  if [ -n "$out" ]; then
    echo "$row love $(echo "$out" | awk '{print $2, $3}')"
  else
    echo "$row love timeout dnf"
  fi
  # external solvers: per-file wall clock, summed; signature from the 10/20 exits.
  for s in $SOLVERS; do
    command -v "$s" >/dev/null 2>&1 || continue
    tot=0; sig=""; to=0
    for f in $files; do
      res=$(wall "$s" "$f"); ms=$(echo "$res" | cut -d' ' -f1); ex=$(echo "$res" | cut -d' ' -f2)
      if [ "$ex" = 124 ]; then to=1; break; fi
      tot=$(awk -v a="$tot" -v b="$ms" 'BEGIN{printf "%.3f", a+b}')
      case "$ex" in 10) sig="${sig}s";; 20) sig="${sig}u";; *) sig="${sig}?";; esac
    done
    if [ "$to" = 1 ]; then echo "$row $s timeout dnf"; else echo "$row $s $tot $sig"; fi
  done
}
mkdir -p "$SLIB"
slibrow uf100   "RND3SAT/uf100-430.tar.gz"   uf100   10
slibrow uuf100  "RND3SAT/uuf100-430.tar.gz"  uuf100  10
slibrow uuf150  "RND3SAT/uuf150-645.tar.gz"  uuf150  5
slibrow uf250   "RND3SAT/uf250-1065.tar.gz"  uf250   14   # threshold-SAT is a per-instance lottery: panel width damps it
slibrow flat100 "GCP/flat100-239.tar.gz"     flat100 3
