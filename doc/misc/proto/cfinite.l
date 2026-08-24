; layer-3 spike: the C-FINITE loop-closer (`cfinite`) -- close a CONSTANT-COEFFICIENT LINEAR-STATE
; counted loop to its exact closed form by MATRIX POWER. this is the sibling of close.l: where close
; sums a polynomial body Σ_{i<n} f(i) to an O(1) literal, cfinite closes an n-VARIABLE state loop
; `(\ n ((: (go v..) (? (< i n) (go U..) R)) inits..))` -- the shape loopinfo-n already extracts
; (love/glaze/auto.l:92 hands back exactly (vars updates T R inits)) -- to M^n x0, O(log n) exact.
;
; THE UNIFICATION (the point of the spike). a linear-state update x <- M x is the WHOLE story; the
; two closers are ONE engine dispatched on the spectrum of M:
;   * M UNIPOTENT (every eigenvalue 1, i.e. (M-I) nilpotent) <=> the body is a POLYNOMIAL in the trip
;     count -> close.l's Newton-Gregory path, O(1), a literal. Σ_{i<n} i is the unipotent case.
;   * M GENERAL (a non-trivial eigenvalue) -> no polynomial closed form; close by M^n via REPEATED
;     SQUARING, O(log n) ring ops, EXACT. fib/lucas/pell live here.
; so close.l is the unipotent special case of this; `cf-kind` below reports which branch a loop takes.
;
; EXACT, NOT BINET. the textbook closed form is Binet -- fib(n) = (phi^n - psi^n)/sqrt5 -- which needs
; sqrt5 and float rounding, exactly the unsoundness finite-differences was built to dodge. the sound
; method is the integer COMPANION MATRIX raised by squaring over Z: affine updates `x <- M x + b` are
; HOMOGENIZED by an augmenting `1` coordinate (so [v.. 1] <- M' [v.. 1] is pure-linear), M' is read
; STRUCTURALLY off the update syntax (no sampling -- linearity is a decidable syntactic certificate, so
; nothing a piecewise/longer-period body could fool), and every * / + promotes to bignum on overflow
; just like the rest of ai -- so the closed form is bit-exact INTO the bignum regime (fib(100) below).
;
; SOUNDNESS. `lclin?` accepts ONLY integer-affine expressions over the state vars: a var, an int
; literal, (+ - of two linear forms), or (* with at least one constant side). a var*var, a %, a ?, or a
; reference to anything not a state var (e.g. the bound n) -> NOT linear -> the whole loop DECLINES
; (cf-kind 'decline), the loop is left alone. conservative: it fires only when it can read the matrix.
;
; THE HONEST PAYOFF (read this before wiring it into auto.l). matrix-power is O(log n) but with a d*d
; matmul constant; the existing loopinfo-n glaze is a native O(n) counted loop. crossover is well past
; n~700, but fib OVERFLOWS int64 at n~92 -- so IN THE FIXNUM RANGE the native loop already wins and this
; would only lose. where it WINS is (a) UNIFICATION -- one nilpotency-dispatched engine subsuming the
; polynomial closer -- and (b) the SMART BIGNUM DEOPT: a linear-state loop whose result outgrows int64
; should deopt to THIS (O(log n) fast-doubling on huge numbers), not to the naive O(n) interpreted
; bignum loop it falls back to today. so this is a generality + worst-case-regime win, not a bench cell.
;
; run:    out/host/love doc/misc/proto/cfinite.l                         (the demo; standalone `assert` is
;                                                                lenient -- eyeball the shown forms)
; strict: cat test/00-init.l doc/misc/proto/cfinite.l | out/host/love    (00-init makes `assert` enforce)

(:
 (nul? l) (! (two? l))                                  ; end-of-list = spine test, not the net (the house nil-trap)

 ; --- little list / vector helpers ---------------------------------------------------------------
 (zeros m)      (? (= m 0) () (link 0 (zeros (- m 1))))
 (unitrow m i)  (: (go j) (? (< j m) (link (? (= j i) 1 0) (go (+ j 1))) ()) (go 0))   ; e_i in dim m
 (constrow m k) (cat (zeros (- m 1)) [k])               ; [0..0 k] -- a constant as a row's last coeff
 (idx l v)      (: (go xs i) (? (nul? xs) -1 (id? (cap xs) v) i (go (cup xs) (+ i 1))) (go l 0))
 (vdot a b)      (? (nul? a) 0 (+ (* (cap a) (cap b)) (vdot (cup a) (cup b))))
 (row2 f a b)   (? (nul? a) () (link (f (cap a) (cap b)) (row2 f (cup a) (cup b))))    ; elementwise binop
 (rowscale k r) (map (\ x (* k x)) r)
 (rowconst? r)  (all (\ x (= x 0)) (init r))            ; pure constant: every coeff but the last is 0

 ; --- the LINEAR certificate: e -> its augmented coeff row over [vars.. | 1], or () if NOT affine ----
 ; this is the C-finite analogue of close.l's lcpoly? -- but it READS coefficients straight off the
 ; syntax (exact, no oracle), so a declined form is declined STRUCTURALLY, not by a sample that lies.
 (lclin vars e)
  (: m (+ (tally vars) 1)
     (? (whole? e) (constrow m e)                       ; an int literal -> its last coeff
        (name? e)  (: i (idx vars e) (? (< i 0) () (unitrow m i)))   ; a state var -> e_i ; ELSE (e.g. n) decline
        (&& (two? e) (member? (cap e) '(+ -)) (= 3 (tally e)))
          (: a (lclin vars (caup e)) b (lclin vars (cap (cuup e)))
             (? (|| (nul? a) (nul? b)) () (row2 (? (id? (cap e) '+) (\ x y (+ x y)) (\ x y (- x y))) a b)))
        (&& (two? e) (id? (cap e) '*) (= 3 (tally e)))   ; a product is linear only if a side is constant
          (: a (lclin vars (caup e)) b (lclin vars (cap (cuup e)))
             (? (|| (nul? a) (nul? b)) ()
                (rowconst? a) (rowscale (last a) b)
                (rowconst? b) (rowscale (last b) a)
                ()))                                     ; var*var -> nonlinear -> decline
        ()))

 ; --- lcmat: assemble the AUGMENTED transition matrix M' from (vars updates) -----------------------
 ; rows 0..d-1 are the updates' linear rows; the last row [0..0 1] carries the constant `1` coordinate
 ; forward (homogenizing the affine part). () if ANY update is non-linear.
 (lcmat vars updates)
  (: m    (+ (tally vars) 1)
     rows (map (\ u (lclin vars u)) updates)
     (? (any nul? rows) () (cat rows [(unitrow m (- m 1))])))

 ; --- exact integer matrix algebra -----------------------------------------------------------------
 (ident m)      (: (go i) (? (< i m) (link (unitrow m i) (go (+ i 1))) ()) (go 0))
 (transpose M)  (? (nul? (cap M)) () (link (map cap M) (transpose (map cup M))))
 (matvec M v)   (map (\ r (vdot r v)) M)
 (matmul A B)   (: Bt (transpose B) (map (\ r (map (\ c (vdot r c)) Bt)) A))
 (mpow M n)     (: D (tally M)
                   (go p acc base) (? (= p 0) acc
                                      (: b2 (matmul base base)
                                         (? (= 1 (% p 2)) (go (// p 2) (matmul acc base) b2) (go (// p 2) acc b2))))
                   (go n (ident D) M))                  ; M^n by repeated squaring -- O(log n) matmuls

 ; --- nilpotency: is (M-I) nilpotent (=> M unipotent => the loop is POLYNOMIAL, close.l's lane)? ----
 (msub A B)  (? (nul? A) () (link (row2 (\ x y (- x y)) (cap A) (cap B)) (msub (cup A) (cup B))))
 (mzero? M)  (all (\ r (all (\ x (= x 0)) r)) M)
 (nilp? M)   (: D (tally M) (mzero? (mpow (msub M (ident D)) D)))   ; (M-I)^D = 0  <=>  nilpotent (index <= D)

 ; --- the closer + the dispatch classifier ---------------------------------------------------------
 (augvec inits) (cat inits [1])                         ; x0 = [inits.. 1]
 (cf-closed vars updates inits R n)                     ; the closed form: a number, or () on decline
  (: M (lcmat vars updates) rrow (lclin vars R)
     (? (|| (nul? M) (nul? rrow)) () (vdot rrow (matvec (mpow M n) (augvec inits)))))
 (cf-kind vars updates)                                 ; 'decline | 'poly (unipotent, O(1)) | 'cfinite (O(log n))
  (: M (lcmat vars updates) (? (nul? M) 'decline (nilp? M) 'poly 'cfinite))

 ; --- an INDEPENDENT naive reference: interpret the updates step by step, O(n), no matrix ----------
 (zipenv ks vs) (? (nul? ks) () (link (link (cap ks) (cap vs)) (zipenv (cup ks) (cup vs))))
 (elook env nm) (? (nul? env) 0 (id? (cap (cap env)) nm) (cup (cap env)) (elook (cup env) nm))
 (aev env e)    (? (whole? e) e (name? e) (elook env e)
                   (: op (cap e) a (aev env (caup e)) b (aev env (cap (cuup e)))
                      (? (id? op '+) (+ a b) (id? op '-) (- a b) (* a b))))
 (cf-naive vars updates inits R n)
  (: (go k vals) (? (< k n) (go (+ k 1) (map (\ u (aev (zipenv vars vals) u)) updates)) (aev (zipenv vars vals) R))
     (go 0 inits))

 ; --- the SOURCE recognizer: a loopinfo-n-shaped \-form -> a closed-form closure of n --------------
 ; (mirrors loopinfo-n's field extraction, auto.l:73-92; declines to () if it doesn't fit / isn't linear)
 (cf-form lam)
  (: BODY (caup (cup lam)) S (cap BODY) inits (cup BODY)
     G (caup S) IF (caup (cup S)) vars (cup G)
     THEN (caup (cup IF)) R (caup (cup (cup IF))) updates (cup THEN)
     M (lcmat vars updates) rrow (lclin vars R) x0 (augvec inits)
     (? (|| (nul? M) (nul? rrow)) () (\ n (vdot rrow (matvec (mpow M n) x0)))))

 ; ============================ self-checks ============================
 ; the three C-finite test recurrences, written ALSO as hand loops (the doubly-independent ground
 ; truth -- these touch NONE of the cf machinery), plus their (vars updates inits R) data.
 (fibl   n) (: (go i a b) (? (< i n) (go (+ i 1) b (+ a b)) a) (go 0 0 1))         ; fib:   (a b)<-(b a+b)
 (lucasl n) (: (go i a b) (? (< i n) (go (+ i 1) b (+ a b)) a) (go 0 2 1))         ; lucas: same map, inits (2 1)
 (pelll  n) (: (go i a b) (? (< i n) (go (+ i 1) b (+ (* 2 b) a)) a) (go 0 0 1))   ; pell:  (a b)<-(b 2b+a)
 V    '(i a b)
 fibU '((+ i 1) b (+ a b))
 pelU '((+ i 1) b (+ (* 2 b) a))

 ; the headline: the O(log n) matrix-power closed form == the hand-written O(n) loop, at the bound and
 ; at several n (so it is a genuine identity in n, not one memoized point).
 _ (assert (= (fibl 10) (cf-closed V fibU '(0 0 1) 'a 10)) "fib: M^n closed form == naive loop (n=10)")
 _ (assert (= (fibl 40) (cf-closed V fibU '(0 0 1) 'a 40)) "fib: ... and at another n (a real closed form)")
 _ (assert (= (fibl 92) (cf-closed V fibU '(0 0 1) 'a 92)) "fib: ... at the largest in-int64 n (92)")
 _ (assert (= 832040 (cf-closed V fibU '(0 0 1) 'a 30))    "fib: bench value fib(30)=832040 via matrix power")

 ; the BIGNUM regime: fib(100) outgrows int64; the matrix entries promote and the closed form stays
 ; bit-exact -- this is the regime the smart-deopt would actually serve.
 _ (assert (= (fibl 100) (cf-closed V fibU '(0 0 1) 'a 100)) "fib: matrix-power bignum == naive bignum (n=100)")
 _ (assert (= 354224848179261915075 (cf-closed V fibU '(0 0 1) 'a 100)) "fib: fib(100) exact bignum, O(log n) ops")
 _ (assert (= (fibl 200) (cf-closed V fibU '(0 0 1) 'a 200)) "fib: still exact deep in the bignum regime (n=200)")

 ; lucas + pell: the same engine, different inits / a constant-scaled update (exercises lclin's * lane)
 _ (assert (= (lucasl 40) (cf-closed V fibU '(0 2 1) 'a 40)) "lucas: closed form == loop (same M, inits (2 1))")
 _ (assert (= 11 (cf-closed V fibU '(0 2 1) 'a 5))           "lucas: L(5)=11")
 _ (assert (= (pelll 30) (cf-closed V pelU '(0 0 1) 'a 30))  "pell: closed form == loop ( (a b)<-(b 2b+a) )")
 _ (assert (= 29 (cf-closed V pelU '(0 0 1) 'a 5))           "pell: P(5)=29 (constant-coeff * recognized)")

 ; the generic data path (cf-naive interpreter) agrees with the hand loops -- validates the encoding.
 _ (assert (= (fibl 50) (cf-naive V fibU '(0 0 1) 'a 50))   "encoding: cf-naive interpreter == hand loop (fib)")
 _ (assert (= (pelll 20) (cf-naive V pelU '(0 0 1) 'a 20))  "encoding: cf-naive interpreter == hand loop (pell)")

 ; --- the UNIFICATION: nilpotency dispatch separates the polynomial (O(1)) from the C-finite case ---
 _ (assert (id? 'cfinite (cf-kind V fibU))                  "dispatch: fib -> 'cfinite (M not unipotent)")
 _ (assert (id? 'cfinite (cf-kind V pelU))                  "dispatch: pell -> 'cfinite")
 _ (assert (id? 'poly (cf-kind '(i s) '((+ i 1) (+ s i))))  "dispatch: Σi (s<-s+i) -> 'poly (M unipotent: close.l's lane)")
 ; ... and matrix-power closes the polynomial case too (just O(log n) where close.l is O(1)): Σ_{i<n} i = n(n-1)/2
 _ (assert (= 4950 (cf-closed '(i s) '((+ i 1) (+ s i)) '(0 0) 's 100)) "poly: Σ_{i<100} i = 4950 via M^n (close.l does this O(1))")

 ; soundness: a NON-linear update (a*a) is DECLINED -- the loop is left alone, never mis-closed.
 _ (assert (id? 'decline (cf-kind '(i a) '((+ i 1) (* a a)))) "sound: nonlinear update (a*a) declines, not mis-closed")
 _ (assert (nul? (cf-closed '(i a) '((+ i 1) (* a a)) '(0 1) 'a 5)) "sound: ... cf-closed returns () (the loop stays a loop)")
 ; ... as does an update that references the bound n (not a state var): linear in n, but n isn't in the state.
 _ (assert (id? 'decline (cf-kind '(i a) '((+ i 1) (+ a n)))) "sound: update over the bound n declines (n not a state var)")

 ; the SOURCE recognizer, end to end: the bench's iterative-fib \-form -> a closed-form closure of n.
 fibsrc '(\ n ((: (go i a b) (? (< i n) (go (+ i 1) b (+ a b)) a)) 0 0 1))
 _ (assert (= (fibl 30) ((cf-form fibsrc) 30)) "recognizer: loopinfo-n source form -> closed-form closure, value preserved")
 _ (assert (= (fibl 90) ((cf-form fibsrc) 90)) "recognizer: ... and the closure is a real identity in n")

 _ (. ";; doc/misc/proto/cfinite.l: C-finite loop-closer self-checks pass -- a constant-coefficient\n")
 _ (. ";; linear-state counted loop closes to M^n x0 (exact integer matrix power, O(log n)), bit-exact\n")
 _ (. ";; into the bignum regime. ONE nilpotency-dispatched engine; close.l is the unipotent case.\n")
 _ (. ";; fib(30)  via M^n   -> ") _ (. (show (cf-closed V fibU '(0 0 1) 'a 30)))  _ (. "  (kind ") _ (. (show (cf-kind V fibU)))  _ (. ")\n")
 _ (. ";; fib(100) bignum    -> ") _ (. (show (cf-closed V fibU '(0 0 1) 'a 100))) _ (. "\n")
 _ (. ";; pell(30)           -> ") _ (. (show (cf-closed V pelU '(0 0 1) 'a 30)))  _ (. "  (kind ") _ (. (show (cf-kind V pelU)))  _ (. ")\n")
 _ (. ";; Σ_{i<100} i        -> ") _ (. (show (cf-closed '(i s) '((+ i 1) (+ s i)) '(0 0) 's 100))) _ (. "  (kind ") _ (. (show (cf-kind '(i s) '((+ i 1) (+ s i))))) _ (. " -> close.l's O(1) lane)\n"))
