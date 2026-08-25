; auto -- the transparent auto-hook: rebind `ev` so a QUALIFYING closure compiles to
; NATIVE automatically. No user-facing verb -- you call `ev` (or eval a closure) as
; always; qualifying ones are just faster, and =/show-IDENTICAL to their source.
; Everything else falls through to the base interpreter unchanged.
;
; Shapes that qualify (the emitter's grammar; E := var | int | (OP E E) | (? E E E),
; OP in + - * // % & | ^ < <= > >= =):
;   LEAF1 (\ x E)            -- E over x
;   LEAFN (\ p0 .. p_{n-1} E) -- E over the params, any arity n>=2 (currying via lvm_cur; natn)
;   FLEAF (\ x F)            -- F a FLOAT body (cgf's +-*/ over a fixnum param + consts); the
;         RESULT is a heap-boxed gem (jitfr). Sound ONLY when the param is floated at first use
;         (allsafe) -- else the interpreter keeps an int subexpr exact and native rounds early.
;   LOOP  (\ n ((: (go i a) (? (i < n) (go (i + 1) (a + B)) a)) 0 0))  -- a native counted
;         loop summing B(i) for i<n; B in the same grammar over i. Owning the LOOP is
;         where the big win is (cf. the leaf's ~6%): the back-edge stays native.
;
; ev is 1-arg, so the loader's `(ev 'ev r)` = `((ev 'ev) r)` still works after the
; rebind; njit-loop/njit-loop-n call the captured base-ev -> no jit<->ev re-entry. Load AFTER
; love/glaze/emit.l (uses emit/cgv/ld-x/ld-i/epilogue/loopcode/nat). ⚠ x86-64.
;
; emit.l + this file ARE the `glaze` module (host/main.c registers the pair and says
; (use 'glaze)), so every name here is a member reached by (from 'glaze 'k) and none of
; them touches the book. `ev` and `member?` are the exceptions the tail explains.
(: sfuel (tablet 0))   ; autospec's unfold-fuel -- its OWN global (a sibling-captured value-local in the letrec below misses in the hatched ev; smix re-seeds it per specialization, so the shared tablet is safe)
(: glaze-loaded? (lit? (from 'glaze))   ; RE-LOAD detector, read BEFORE this load defglob's anything: `glaze` in the registry => the glaze is installed (the file is being cat'd on top of the baked glaze). Used at the `ev` rebind below to make a re-load IDEMPOTENT -- keep the baked glaze, don't reinstall -- so `cat auto.l prog | love` just runs prog. (Loading the compiler's own source through itself otherwise recompiles its helpers wrong; tracked separately.) ⚠ the registry, never `book`: a woken image has `book` sealed away, so the old read scared and answered "not loaded" on exactly the binary that carries the glaze.
   base-ev (: G (from 'glaze) (? G (G 'base-ev) ev))   ; the PURE interpreter, captured ONCE: first load (registry miss -> the presence guard's ()) -> ev, the interpreter; a RE-LOAD has ev=auto-ev already, so take the interpreter out of the MODULE book (else base-ev becomes the glaze and autospec's static fold re-enters it)
   arch (intern love-arch)                ; the host CPU as the assembler target ('x64 / 'arm64); glaze loads only on these
   x86? (id? arch 'x64)                 ; gates the still-x86-only lanes (float result, counted loops, float grids)
   ; the emitter's expression grammar over var v: var | int | arith | cmp | (? t a b)
   (exprp e v) (? (e = v)   1
                  (whole? e)  1
                  (two? e)  (: op <e
                               (? (member? op '(+ - * // % & | ^ < <= > >= =)) (exprp <>e v && exprp <>>e v)
                                  (op = '?) ((two? >>>e) && !(two? >>>>e)   ; exactly 3 operands
                                            && (exprp <>e v) && (exprp <>>e v) && (exprp <>>>e v))
                                  0))
                  0)
   ; --- leaf: (\ x E) ---
   (qual lam) ((two? lam) && (<lam = '\) && (two? >lam) && (name? <>lam)                ; (\ v E), v any symbol
               && (two? >>lam) && !(two? >>>lam) && (exprp <>>lam <>lam))
   ; --- arity-N leaf: (\ p0 .. p_{n-1} E), E over the params (any distinct symbols) ---
   (exprn ps e) (? (member? e ps) 1  (whole? e) 1
                   (two? e) (: op <e
                               (? (member? op '(+ - * // % & | ^ < <= > >= =)) (exprn ps <>e && exprn ps <>>e)
                                  (op = '?) ((two? >>>e) && !(two? >>>>e)
                                            && (exprn ps <>e) && (exprn ps <>>e) && (exprn ps <>>>e))
                                  0))
                   0)
   (distinctp l) (? !(two? l) 1  (member? <l >l) 0  (distinctp >l))        ; no repeated param
   (qualn lam) (: ps (? (two? >lam) (init >lam) 0)  body (? (two? >lam) (last >lam) 0)
                  ((two? lam) && (<lam = '\) && (two? >lam)
                   && (all name? ps) && (1 < (tally ps)) && (distinctp ps)                     ; >=2 distinct symbol params
                   && (exprn ps body)))
   ; --- loop: match the tail-recursive counted-sum skeleton, extract (B counter) ---
   (loopinfo lam)
    (: M    <>>lam                                ; lambda body
       S    <M   z1 <>M   z2 <>>M       ; M = (S 0 0)
       G    <>S  IF <>>S                     ; S = (: (go i a) IF)
       GO   <G   I <>G    A <>>G        ; (go i a)
       T    <>IF THEN <>>IF ELS <>>>IF   ; (? T THEN ELS)
       STEP <>THEN ACC <>>THEN               ; THEN = (go STEP ACC)
       B    <>>ACC                                ; ACC = (op a B)
       op   (? (two? ACC) <ACC 0)  cmp (? (two? T) <T 0)   ; the fold op + the loop-test comparison
       ok ((two? lam) && (<lam = '\) && (name? <>lam)                  ; (\ N ..)
           && (two? >>lam) && !(two? >>>lam)           ; one body
           && (two? M) && (z1 = 0) && !(two? >>>M)                  ; (S 0 a0)
           && (two? S) && (<S = ':) && (name? GO) && (name? I) && (name? A) && !(I = A)   ; (: (go i a) IF)
           && (two? IF) && (<IF = '?) && (ELS = A)                            ; (? .. .. a)
           && (two? T) && (member? cmp '(< <= > >= =)) && (<>T = I) && (<>>T = <>lam)  ; (cmp i n)
           && (<THEN = GO)                                              ; (go ..)
           && (two? STEP) && (<STEP = '+) && (<>STEP = I) && (<>>STEP = 1)  ; (i + 1)
           && (two? ACC) && (member? op '(+ *)) && (<>ACC = A) && (exprp B I)       ; (op a B), op in {+,*}, B over i
           && (z2 = (foldinit op)))                                          ; a0 matches the op's identity (0 / 1)
       (? ok [B I op cmp] 0))
   ; --- CALL-OUT loop: loopinfo's shape B, but the per-step term is a CALL to a resolvable global closure
   ; F (a "call-out") rather than arithmetic over i: B = (F ARG), op in {+,*}, ARG an exprp over i (the bare
   ; counter, or a computed expr like (i + 1) / (i * i)). Returns (F op iv ARG): the caller resolves F, bakes
   ; the op-parameterized fold, and compiles ARG natively for the callout arg (emit.l jitcalloutx / caarg).
   ; The leaf/group grammar rejects (F ARG) (F isn't an op / a group name), so this is the only lane that owns
   ; it. SINGLE param (no imps) => F is guaranteed free => a global; the CAPTURED / multi-arg case is
   ; calloutinfo-frame below (F a frame param, read from the [imps args] frame).
   (calloutinfo lam)
    (: M    <>>lam  S <M z1 <>M z2 <>>M
       G    <>S  IF <>>S
       GO   <G   I <>G A <>>G
       T    <>IF THEN <>>IF ELS <>>>IF
       STEP <>THEN ACC <>>THEN
       B    <>>ACC  op (? (two? ACC) <ACC 0) cmp (? (two? T) <T 0)
       F    (? (two? B) <B 0)                                            ; the callout head
       ok ((two? lam) && (<lam = '\) && (name? <>lam)
           && (two? >>lam) && !(two? >>>lam)            ; exactly one param (no imps)
           && (two? M) && (z1 = 0) && (z2 = (foldinit op)) && !(two? >>>M)   ; (S 0 a0) -- a0 the op's identity (0 for +, 1 for *)
           && (two? S) && (<S = ':) && (name? GO) && (name? I) && (name? A) && !(I = A)
           && (two? IF) && (<IF = '?) && (ELS = A)
           && (two? T) && (member? cmp '(< <= > >= =)) && (<>T = I) && (<>>T = <>lam)   ; (cmp i n)
           && (<THEN = GO)
           && (two? STEP) && (<STEP = '+) && (<>STEP = I) && (<>>STEP = 1)   ; (i + 1)
           && (two? ACC) && (member? op '(+ *)) && (<>ACC = A)                 ; (op a B), op in {+,*}
           && (two? B) && (2 = (tally B)) && (name? F) && (exprp <>B I) && !(F = I) && !(F = A) && !(F = <>lam))   ; B = (F ARG), F free, ARG an expr over i
       (? ok [F op I <>B] 0))
   ; --- CAPTURED / multi-arg CALL-OUT: the SAME sum-over-(F i) shape, but F (and the bound N) are FRAME
   ; params -- a captured import `(\ n (sum (f i)))` (ala prepends the imp -> `(\ f n ..)`) or a genuine
   ; multi-arg `(\ f n (sum (f i)))`. The native reads F and N from the [imps args] frame (fxir-style,
   ; F raw / N untagged) instead of baking; deopt goes to value[1]=e, the bytecode twin ala built (which
   ; already captures F). Returns (Fidx Nidx op ARG) -- the frame indices, fold op, and the (computed) arg expr.
   ; Needs >=2 params (a=1 is the global lane).
   (calloutinfo-frame lam)
    (: ps (init >lam)  M (last >lam)  S (? (two? M) <M 0) z1 (? (two? M && two? >M) <>M 1) z2 (? (two? M && two? >M && two? >>M) <>>M 1)
       G    (? (two? S) <>S 0)  IF (? (two? S && two? >S) <>>S 0)
       GO   (? (two? G) <G 0)  I (? (two? G && two? >G) <>G 0) A (? (two? G && two? >G && two? >>G) <>>G 0)
       T    (? (two? IF) <>IF 0)  THEN (? (two? IF && two? >IF) <>>IF 0) ELS (? (two? IF && two? >IF && two? >>IF) <>>>IF 0)
       STEP (? (two? THEN) <>THEN 0)  ACC (? (two? THEN && two? >THEN) <>>THEN 0)
       B    (? (two? ACC && two? >ACC) <>>ACC 0)  op (? (two? ACC) <ACC 0) cmp (? (two? T) <T 0)
       N    (? (two? T) <>>T 0)  F (? (two? B) <B 0)
       ok ((two? lam) && (<lam = '\) && (1 < (tally ps)) && (all name? ps) && (distinctp ps)   ; >=2 frame params
           && (two? M) && (z1 = 0) && (z2 = (foldinit op)) && !(two? >>>M)   ; (S 0 a0) -- a0 the op's identity
           && (two? S) && (<S = ':) && (name? GO) && (name? I) && (name? A) && !(I = A)
           && (two? IF) && (<IF = '?) && (ELS = A)
           && (two? T) && (member? cmp '(< <= > >= =)) && (<>T = I) && (member? N ps) && !(N = I) && !(N = A)   ; bound is a frame param
           && (<THEN = GO)
           && (two? STEP) && (<STEP = '+) && (<>STEP = I) && (<>>STEP = 1)
           && (two? ACC) && (member? op '(+ *)) && (<>ACC = A)                                             ; (op a B), op in {+,*}
           && (two? B) && (2 = (tally B)) && (name? F) && (member? F ps) && (exprp <>B I) && !(F = I) && !(F = A) && !(F = N))   ; B = (F ARG), F a frame param, ARG an expr over i
       (? ok [(posof F ps 0) (posof N ps 0) op <>B] 0))
   (njit-loop lam B cv op cmp interp) (nif (emit (loopcode B cv op cmp arch)) interp lam 1)
   ; --- general N-VARIABLE loop: (\ n ((: (go v0..vk) (? T (go U..) R)) I0..Ik)),
   ; up to 3 loop vars + arbitrary update exprs -- this is the shape iterative fib/tak
   ; take (i,a,b with updates b and a+b), not just the fold skeleton above.
   (loopinfo-n lam)
    (: BODY (? (two? >lam) <>>lam 0)
       S    (? (two? BODY) <BODY 0)  inits (? (two? BODY) >BODY 0)
       G    (? (two? S) <>S 0)
       IF   (? (two? S && two? >S) <>>S 0)
       GO   (? (two? G) <G 0)  vars (? (two? G) >G 0)
       T    (? (two? IF) <>IF 0)
       THEN (? (two? IF && two? >IF) <>>IF 0)
       R    (? (two? IF && two? >>IF) <>>>IF 0)
       updates (? (two? THEN) >THEN 0)
       fr   (<>lam >< vars)                                  ; cgl frame: {n, v0..vk}
       ok ((two? lam) && (<lam = '\) && (name? <>lam) && (two? >>lam) && !(two? >>>lam)
           && (two? BODY) && (two? S) && (<S = ':) && (two? >S) && !(two? >>>S)   ; (: G IF)
           && (two? G) && (name? GO) && (two? vars) && (all name? vars) && (distinctp vars) && !(member? GO vars)
           && ((tally vars) < 4)                                    ; <= 3 loop vars (register limit)
           && (two? IF) && (<IF = '?) && (two? THEN) && (<THEN = GO)   ; (? T (go U..) R)
           && ((tally updates) = (tally vars)) && ((tally inits) = (tally vars))
           && (two? T) && (member? <T '(< <= > >= =)) && (exprn fr T)   ; TEST a comparison over {n,vars}
           && (all (\ u (exprn fr u)) updates) && (exprn fr R)         ; updates + RESULT exprs over {n,vars}
           && (all whole? inits))                                     ; inits are fixnum consts
       (? ok [vars updates T R inits] 0))
   ; --- C-FINITE smart bignum deopt (doc/misc/proto/cfinite.l): a constant-coefficient LINEAR-state
   ;     recurrence with a canonical counter (c: init 0, step (c + 1), test (c < n) -> trips = n) closes
   ;     to its result by EXACT integer MATRIX POWER M^n x0 in O(log n). The native loop above already
   ;     wins IN THE FIXNUM RANGE, so this changes ONLY njit-loop-n's post-overflow path: the deopt
   ;     fallback computes the bignum result by SQUARING M instead of re-running the naive O(n) bignum
   ;     loop. SOUND -- bit-exact vs the loop at every n (the asserts below + the spike), linearity a
   ;     STRUCTURAL certificate (cf-lin reads coeffs off the syntax, no sampling, nothing piecewise can
   ;     fool); ANY non-linear update / non-canonical counter / n-referencing update DECLINES to
   ;     (base-ev lam), the prior behavior. State HOMOGENIZED by a trailing `1` coordinate so an affine
   ;     update reads linear. M' rows 0..d-1 are the updates' coeff rows, the last [0..0 1] keeps the 1.
   (cf-nul? l)     !(two? l)
   (cf-zeros m)    (? (m = 0) () (0 >< cf-zeros (m - 1)))
   (cf-unit m i)   (: (go j) (? (j < m) ((? (j = i) 1 0) >< go (j + 1)) ()) (go 0))      ; e_i in dim m
   (cf-const m k)  ((cf-zeros (m - 1)) + [k])                                             ; [0..0 k]
   (cf-idx l v)    (: (go xs i) (? (cf-nul? xs) -1 (id? <xs v) i (go >xs (i + 1))) (go l 0))
   (cf-nth l i)    (? (i = 0) <l (cf-nth >l (i - 1)))
   (cf-dot a b)    (? (cf-nul? a) 0 ((<a * <b) + (cf-dot >a >b)))
   (cf-rowf f a b) (? (cf-nul? a) () (f <a <b >< cf-rowf f >a >b))  ; elementwise
   (cf-scale k r)  (map (\ x (k * x)) r)
   (cf-const? r)   (all (\ x (x = 0)) (init r))                                              ; pure constant row
   (cf-lin vs e)                                                                            ; e -> augmented coeff row, or () if NOT integer-affine
    (: m ((tally vs) + 1)
       (? (whole? e) (cf-const m e)
          (name? e)  (: i (cf-idx vs e) (? (i < 0) () (cf-unit m i)))                        ; a state var -> e_i ; else (e.g. n) decline
          (two? e && member? <e '(+ -) && 3 = (tally e))
            (: a (cf-lin vs <>e) b (cf-lin vs <>>e)
               (? (cf-nul? a || cf-nul? b) () (cf-rowf (? (id? <e '+) (\ x y (x + y)) (\ x y (x - y))) a b)))
          (two? e && id? <e '* && 3 = (tally e))                                     ; a product is linear only if a side is constant
            (: a (cf-lin vs <>e) b (cf-lin vs <>>e)
               (? (cf-nul? a || cf-nul? b) ()
                  (cf-const? a) (cf-scale (last a) b)
                  (cf-const? b) (cf-scale (last b) a)
                  ()))                                                                       ; var*var -> nonlinear -> decline
          ()))
   (cf-mat vs ups) (: m ((tally vs) + 1) rows (map (\ u (cf-lin vs u)) ups)
                      (? (any cf-nul? rows) () (rows + [(cf-unit m (m - 1))])))           ; () if any update is non-linear
   (cf-id m)       (: (go i) (? (i < m) (cf-unit m i >< go (i + 1)) ()) (go 0))
   (cf-tpose M)    (? (cf-nul? <M) () (map cap M >< cf-tpose (map cup M)))
   (cf-mv M v)     (map (\ r (cf-dot r v)) M)
   (cf-mul A B)    (: Bt (cf-tpose B) (map (\ r (map (\ c (cf-dot r c)) Bt)) A))
   (cf-mpow M n)   (: D (tally M) (go p acc base)
                      (? (p = 0) acc (: b2 (cf-mul base base) (? (1 = (p % 2)) (go (p // 2) (cf-mul acc base) b2) (go (p // 2) acc b2))))
                      (go n (cf-id D) M))                                                    ; M^n by repeated squaring, O(log n) matmuls
   (cf-aug inits)  (inits + [1])                                                          ; x0 = [inits.. 1]
   (cf-canon? lam vs ups test inits)                                                        ; counter c -> EXACTLY n trips (the lambda param)?
    ((two? test) && (id? <test '<) && (3 = (tally test)) && (id? <>>test <>lam)
     && (: c <>test i (cf-idx vs c)
           ((i >= 0) && ((cf-nth inits i) = 0)
            && (: u (cf-nth ups i) (two? u && id? <u '+ && 3 = (tally u) && id? <>u c && <>>u = 1)))))
   (cf-deopt lam vs ups test result inits interp)                                           ; the deopt closure: matrix power if linear+canonical, else `interp` (the caller's deopt target -- (base-ev l) off the ev-rebind, or the bytecode twin `e` on the natjit hook; NEVER (base-ev lam) here, which under the hook would re-enter ala)
    (: M (cf-mat vs ups) rrow (cf-lin vs result)
       (? (cf-canon? lam vs ups test inits && !(cf-nul? M) && !(cf-nul? rrow))
          (\ n (cf-dot rrow (cf-mv (cf-mpow M n) (cf-aug inits))))            ; the matrix-power closure is itself bytecode under the hook -- its body isn't leaf/loop grammar, so no re-entry
          interp))
   (njit-loop-n lam vars updates test result inits interp)
    (nif (emit (loopcode-n <>lam vars updates test result inits arch)) (cf-deopt lam vars updates test result inits interp) lam 1)
   ; --- captured-closure CONSTRUCTOR (\ p (\ q E)): a nest of 1-arg lambdas, E over
   ; {p,q}. Compiled as the flat 2-param native (\ p q E); applying it to p yields a
   ; native CAPTURED closure -- fully transparent (= the interpreter's ((2param) p)
   ; partial form: native captures via partial-apply are =/show-identical to interp).
   ; The constructor itself normalizes to the flat 2-param (a curry-equivalent form).
   (qualc lam) (: p <>lam  inner (? (two? >lam) <>>lam 0)
                  ((two? lam) && (<lam = '\) && (two? >lam) && (name? p)
                   && (two? >>lam) && !(two? >>>lam)               ; (\ p INNER), one body
                   && (two? inner) && (<inner = '\) && (two? >inner) && (name? <>inner) && !(p = <>inner)
                   && (two? >>inner) && !(two? >>>inner)            ; INNER = (\ q E), q != p
                   && (exprn [p <>inner] <>>inner)))
   (flatc lam) (: inner <>>lam ['\ <>lam <>inner <>>inner])   ; (\ p (\ q E)) -> (\ p q E)
   ; --- FLOAT-RESULT leaf (\ x <fexpr>) -> jitfr (emit.l): a body that BOXES a gem, over cgf's +-*/
   ;     grammar (a fixnum param + float/int consts). SOUNDNESS vs the interpreter turns on WHEN the
   ;     param is floated: the native lane cvtsi2sd's it at EVERY use, but the interpreter keeps an
   ;     INTEGER subexpression exact (a bignum, even) until a float forces promotion -- so ((x * 3) + 1.5)
   ;     DIVERGES at x=2^53+1 (native rounds the param early). `fty` types each node F / I / 0(reject);
   ;     `allsafe` demands the param NEVER sit inside an I-typed COMPOUND (it must meet a float at first
   ;     contact), so native's per-use flotation lands on the interpreter's promotion point exactly.
   (fty e v) (? (e = v) 'I  (gem? e) 'F  (whole? e) 'I                                           ; param/int = I, float const = F
                (two? e && 3 = (tally e) && member? <e '(+ - * /))                       ; a binary +-*/ form
                  (: op <e ta (fty <>e v) tb (fty <>>e v)
                     (? (id? ta 0 || id? tb 0) 0                                             ; an operand rejected -> reject
                        (member? op '(+ - *)) (? (id? ta 'F || id? tb 'F) 'F 'I)            ; +-*: float iff an operand is float
                        (id? ta 'F || id? tb 'F) 'F  0))                                     ; /: float-division ok; int/int is value-dependent -> reject
                0)
   (allsafe e v) (? !(occurs? v e) 1  !(two? e) 1  (id? (fty e v) 'I) 0                    ; an I-typed COMPOUND holding the param -> late promotion -> reject
                    (allsafe <>e v && allsafe <>>e v))
   (qualfr lam) ((two? lam) && (<lam = '\) && (two? >lam) && (name? <>lam)
                 && (two? >>lam) && !(two? >>>lam)                         ; (\ v BODY), one param + one body
                 && (: v <>lam body <>>lam
                       (id? (fty body v) 'F && allsafe body v)))                             ; BODY is a float RESULT, param floated everywhere
   ; --- the GRID NEST (general, D-dimensional): a letrec of D counted-loop functions wrapping a
   ; FLOAT RECURRENCE -- the inner def self-recurses with an (&& (it < LIM) (cmp TEST BOUND)) escape
   ; test; each loop def is (? (cnt < N) (self ..outer.. (cnt + 1) ..(acc + INNERCALL)..) acc), the
   ; outermost driven by the trailing (driver 0 ..). autonat reads this STRAIGHT FROM SOURCE, walks
   ; the loop chain to recover D bounds + counters, classifies the recurrence into M invariants /
   ; N float-vars / their updates, and builds the whole-grid native via jitfgridn (emit.l). Then it
   ; rewrites the OUTERMOST loop's body to call the native. The mandelbrot 64x64 grid is the D=2,
   ; M=2, N=2 instance; any D>=1 and M+N<=6 within the xmm budget glaze. Returns the form UNCHANGED
   ; if it doesn't fit, so every other form passes through untouched. ⚠ x86-64 (-> jitfgridn).
   (anat-defs bs) (? (atom? >bs) [() <bs]             ; a : binding list -> (pairs body); pairs = ((nameform . body)..)
                     (: r (anat-defs >>bs)
                        [((<bs >< <>bs) >< <r) <>r]))
   (anat-cond? e) (two? e && id? <e '?)                 ; a (? ..) form?
   (afind pred l) (? (atom? l) () (pred <l) <l (afind pred >l))
   ; the recurrence pair: a (? (&& ..) (SELF ..) ..) whose then-branch self-calls (head = its own name)
   (anat-rec? p) (: nm <<p body >p
                    ((anat-cond? body) && (two? >>body)
                     && (: cond (opd body 0) then (opd body 1)
                           (two? cond && id? <cond '&& && two? then && id? <then nm))))
   ; anat-callee: the next-inner function a loop def calls -- INNERCALL's head in (? (cnt < N)
   ; (self .. (acc + INNERCALL)) acc) -- or () if the pair doesn't match the loop shape. The OUTERMOST
   ; loop is the one no other loop names (the driver reaches it -- maybe wrapped, e.g. (bench .. (g 0 0))).
   (anat-callee q) (: body >q
      (? !(anat-cond? body && 1 < (tally >>body)) ()
         (: then (opd body 1)
            (? !(two? then) ()
               (: accarg (last >then)
                  (? !(two? accarg && id? <accarg '+ && two? >>accarg) ()
                     (: ic <>>accarg (? (two? ic) <ic ()))))))))
   ; loopchain: walk loopnm's pair -- (? (cnt < N) (self ..(acc + INNERCALL)..) acc) -- record
   ; (cnt N rec-args), recursing on INNERCALL's head until it reaches the recurrence recnm. The list
   ; runs OUTER->INNER; () on any structural mismatch (so autonat bails to the unchanged form).
   (loopchain loopnm pairs recnm)
    (: p (afind (\ q (id? <<q loopnm)) pairs)
       (? !(two? p) ()
          (: body >p
             (? !(anat-cond? body && 1 < (tally >>body)) ()
                (: cond (opd body 0) then (opd body 1)
                   (? !(two? cond && id? <cond '< && two? then) ()
                      (: cnt (opd cond 0) bnd (opd cond 1) accarg (last >then)
                         (? !(two? accarg && id? <accarg '+ && two? >>accarg) ()
                            (: ic <>>accarg
                               (? !(two? ic) ()
                                  (id? <ic recnm) [[cnt bnd >ic]]               ; innermost: the recurrence call's args
                                  (: rest (loopchain <ic pairs recnm)                  ; else recurse on the next loop
                                     (? (two? rest) ([cnt bnd ()] >< rest) ()))))))))))))
   ; rebind: rebuild a : binding list, replacing the outermost loop's body with (native 0)
   (anat-rebind bs outernm ng)
    (? (atom? >bs) bs
       (: nm <bs bd <>bs
          (nm >< ((? (two? nm && id? <nm outernm) [ng 0] bd) >< anat-rebind >>bs outernm ng))))
   ; --- a GROUP of mutually-recursive ARITHMETIC functions in a `:` -- each (name p..) BODY with BODY
   ; in the integer fragment `groupok?` accepts: param | int | (op E E) | (cmp E E) | (? E E E) | (g E..)
   ; where g is ANY function OF THE GROUP (self OR sibling) of matching arity. autogroup finds the
   ; maximal CLOSED such subset (a fixpoint: a fn that calls OUT of the fragment -- into link/cap, a float
   ; const, or an interpreted helper -- drops out, taking its callers with it), then rewrites EACH group
   ; binding to its OWN native (jitgroupir, emit.l), so every reference -- from the `:` body OR an
   ; interpreted sibling -- resolves to native, while the calls AMONG the group stay native inside the
   ; buffer. fib/tak are SINGLETON groups; even?/odd? a 2-fn group. `groupok?` is the standalone fragment
   ; predicate (the futamura `mix` pass can reuse it as its "residual is ground arithmetic" stop oracle).
   ; Returns the form UNCHANGED if nothing fits (lists/floats/other recursion pass through). ⚠ x86-64.
   ; --- LAMBDA-LIFTING: fold a NAMED-LET in a fn body into a flat group fn, so a fn whose body is a
   ;     local recursive loop joins the call graph. A body (NL inits..) where NL = (: (g q..) GBODY) is a
   ;     local fn g (its own letrec) applied to inits. Lift g to a top-level fn g'(FV.. q..) where FV =
   ;     the ENCLOSING fn's params that occur free in GBODY -- threading FV through g's self-calls -- and
   ;     replace the body with (g' FV.. inits..). primes' `prime?` and `count` are each such a named-let
   ;     loop, so after lifting the whole trial-division graph (prime? count prime?_go count_go) is a flat
   ;     mutually-recursive group autogroup+jitgroupir glaze. ONE level (whole-body named-let; GBODY flat);
   ;     a non-named-let body passes through untouched, so fib/tak/even-odd are unaffected.
   (occurs? s e) (? (two? e) (foldl (\ a x (a || occurs? s x)) 0 e) (id? e s))                  ; s occurs (syntactically) in e?
   (subst-call g gn fv e) (? (two? e) (? (id? <e g) (gn >< fv + (map (\ x (subst-call g gn fv x)) >e))   ; (g A..) -> (gn fv.. A'..)
                                          (map (\ x (subst-call g gn fv x)) e)) e)
   (named-let? e) ((two? e) && (: nl <e                                                      ; e = ((: (g q..) GBODY) inits..) ?
                      ((two? nl) && (id? <nl ':) && (two? >nl)
                       && (: gnf <>nl
                             ((two? gnf) && (name? <gnf) && (1 < (tally gnf)) && (all name? >gnf) && (distinctp >gnf)
                              && (two? >>nl) && !(two? >>>nl)               ; (: (g q..) GBODY), body-less (one binding)
                              && ((tally >e) = (tally >gnf)))))))                       ; inits arity == g's arity
   (subst-sym nm val e) (? (id? e nm) val (two? e) (map (\ x (subst-sym nm val x)) e) e)            ; symbol nm -> val in e
   ; norm-let: a fn body (: aux.. (g q..) GBODY (g inits..)) -- a let with VALUE aux bindings + ONE local
   ; fn + a CALL result -- normalizes to the named-let ((: (g q..) GBODY[aux inlined]) inits..) lift handles.
   ; strscan's strhash (: n (tally s) (go i h) .. (go 0 0)) is exactly this (n=(tally s) inlined into go).
   (norm-let body)
    (? !(two? body && id? <body ': && !(atom? >body)) body
       (: parsed (anat-defs >body) pairs <parsed result <>parsed
          fnps (filter (\ pr (two? <pr)) pairs) auxps (filter (\ pr !(two? <pr)) pairs)
          (? !(1 = (tally fnps) && two? result) body                                         ; need exactly one local fn + a call result
             (: g <<<fnps q ><<fnps gbody ><fnps
                (? !(id? <result g && (tally >result) = (tally q)) body            ; result must be (g inits..) matching arity
                   (: inl (\ x (foldl (\ b ap (subst-sym <ap >ap b)) x auxps))            ; inline every aux name -> its value
                      ((': >< ((g >< q) >< (inl gbody >< ()))) >< map inl >result)))))))
   (lift-pair p)                                                                                  ; (nameform . body) -> (rewritten-pair newdefs)
    (: nf <p pbody (norm-let >p)
       (? !(two? nf && name? <nf && 1 < (tally nf) && named-let? pbody) [p ()]
          (: F <nf P >nf  nl <pbody inits >pbody
             gnf <>nl g <gnf q >gnf GBODY <>>nl
             gn (intern ((string F) + "_" + (string g)))                                        ; lifted name F_g (unique per enclosing fn)
             fv (filter (\ pp (occurs? pp GBODY && !(member? pp q))) P)                        ; enclosing params free in GBODY
             [(nf >< (gn >< fv + inits))                                                 ; F's body -> (gn fv.. inits..)
               [((gn >< fv + q) >< subst-call g gn fv GBODY)]])))                       ; new def: (gn fv.. q..) . GBODY'
   (unpair ps body) (? (atom? ps) [body] (: pr <ps (<pr >< (>pr >< unpair >ps body))))    ; pairs + body -> a : binding list
   (lift form)                                                                                    ; lambda-lift every named-let fn body in a `:`
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          results (map lift-pair pairs)
          allpairs ((map cap results) + (foldl (\ a r (a + <>r)) () results))            ; rewritten originals ++ all the lifted defs
          (': >< unpair allpairs body)))
   ; --- PARTIAL LIFT (plift): hoist a READ-ONLY loop fn OUT of a fn's nested `:` body, all the way to the
   ;     TOP level, threading its lexical captures (the enclosing fn's params + the nested value-bindings it
   ;     reads) as LEADING params -- so a read loop buried among ALLOCATING siblings surfaces as a top-level
   ;     group candidate while the allocators stay interpreted. The sibling case `lift` can't reach: hash's
   ;     `hash-run` has THREE local fns (ins/scan/bump) sharing the mutable map `h` + a compound `(a + (scan
   ;     0 0))` result, so norm-let/named-let? bail. Read-only = no pin/link/tablet/cask/say (a write can't
   ;     hoist past the interpreted fills it interleaves with); self-recursive, no sibling call (v1). ==
   ;     interp: the lifted fn gets the SAME captures BY VALUE. The lifted `(g .. h ..)` then needs the MAP
   ;     lane (groupok?/jitgroupir) to glaze; until that lands plift is STANDALONE -- OFF the autogroup chain.
   roalloc '(pin pull link tablet hash cask say pour jug tap put puts putc putn putx flush dot see sip slurp sound read reads rand randf twirl catch scare ev)   ; allocating/mutating/IO heads -- a `_`-bound one is an EFFECT that must NOT be dropped or hoisted
   (uses-head? hs e) (? (two? e) (member? <e hs || foldl (\ a x (a || uses-head? hs x)) 0 >e) 0)
   (fnpair? pr) (: nf <pr (two? nf && name? <nf && 1 < (tally nf) && all name? >nf && distinctp >nf))  ; (g q..) define-sugar
   (roloop? pr sibs) (: g <<pr body >pr                                                 ; a hoistable READ-ONLY self-loop
      ((fnpair? pr) && (occurs? g body) && !(uses-head? roalloc body)
       && !(any (\ s (!(id? s g) && occurs? s body)) sibs)))                              ; calls no SIBLING fn
   ; --- WRITE-LOOP (the map UPDATE-in-place lane): hash's `bump` -- (g i) (? COND (: k.. _ (pin H k V) (g STEP)) H)
   ;     -- a read-modify-WRITE self-loop over a captured MAP H whose ONLY effect is the in-place (pin H k V)
   ;     (the key was inserted by an earlier pass, so the pin never grows the table -> allocation-free) and
   ;     whose result is the MAP H. plift hoists it like a roloop (H threaded as a captured param), but the
   ;     hoisted body is THREADED: the (pin H k V) folds into the recursive call's H slot as (mpin H k V)
   ;     (mpin writes AND returns H, like the cask lane threads (pin s i E)), peep->mpeep, the base H -> 0
   ;     (native returns a fixnum, not the pointer the nif would tag). SOUND only when H's result is
   ;     DISCARDED at every call site (we return 0, not H) -- bumpok? enforces it; a missing key DEOPTS.
   (wmaph pr sibs) (: g <<pr body >pr                                                   ; the captured MAP H of an RMW write-loop pr, else ()
      (? !((fnpair? pr) && (occurs? g body) && !(any (\ s (!(id? s g) && occurs? s body)) sibs)
             && (two? body) && (id? <body '?) && (4 = (tally body))) ()
         (: THEN (opd body 1) BASE (opd body 2)
            (? !(name? BASE && two? THEN && id? <THEN ': && !(atom? >THEN)) ()
               (: pp (anat-defs >THEN) binds <pp call <>pp
                  pinpr (afind (\ b (: v >b (two? v && id? <v 'pin && 4 = (tally v) && id? <>v BASE))) binds)
                  (? ((two? pinpr)
                      && (all (\ b (id? b pinpr || !(uses-head? roalloc >b))) binds)   ; the ONLY effect is that one pin
                      && (two? call) && (id? <call g)) BASE ()))))))                            ; tail self-call (H is captured pre-lift; wthread threads it via the recursive call after plift adds it)
   (wthread g gn fv H pr) (: body >pr COND (opd body 0) THEN (opd body 1)                    ; the threaded hoisted body for a write-loop (still peep/pin -> interpretable; mapfix rewrites to mpeep/mpin later)
      pp (anat-defs >THEN) binds <pp call <>pp
      pinpr (afind (\ b (: v >b (two? v && id? <v 'pin && id? <>v H))) binds)
      pv >pinpr K (opd pv 1) V (opd pv 2)
      others (filter (\ b !(id? b pinpr)) binds)
      inl (\ e (foldl (\ acc ob (subst-sym <ob >ob acc)) e others))                    ; inline the pure value-binds (e.g. k)
      nc (map (\ x (? (id? x H) ['pin H (inl K) (inl V)] x)) (subst-call g gn fv call))          ; (gn fv.. STEP) with H's slot threaded as (pin H k v) -- pin writes AND returns H, threading the map
      ['? (subst-call g gn fv COND) nc 0])                                                       ; base -> 0 (the map result is discarded -- bumpok?)
   (other-use? nm self ipairs ibody) (occurs? nm ibody || any (\ pr (!(id? pr self) && occurs? nm >pr)) ipairs)   ; nm used outside its own binding?
   (bumpok? g defpr ipairs ibody)                                                                ; every (g ..) call (besides g's def) is a DISCARDED whole-binding value
    (!(occurs? g ibody)
     && (all (\ pr ((id? pr defpr) || !(occurs? g >pr)
                    || (!(fnpair? pr) && (: v >pr (two? v && id? <v g)) && !(other-use? <pr pr ipairs ibody)))) ipairs))
   (subst-pairs g gn fv pairs) (map (\ pr (<pr >< subst-call g gn fv >pr)) pairs)    ; rewrite (g A..) in every pair's value
   (plift-fn Fpair)                                                                               ; ((F P..) . FBODY) -> `(Fpair' newdefs)
    (: nf <Fpair FBODY >Fpair
       (? !(two? nf && name? <nf && two? FBODY && id? <FBODY ': && !(atom? >FBODY)) [Fpair ()]
          (: F <nf P >nf
             parsed (anat-defs >FBODY) ipairs <parsed ibody <>parsed
             sibnames (map caap (filter fnpair? ipairs))                                           ; all inner fn NAMES
             valnames (map cap (filter (\ pr !(fnpair? pr)) ipairs))                            ; inner value-binding names (bare symbols)
             wlifts (filter (\ pr (name? (wmaph pr sibnames) && bumpok? <<pr pr ipairs ibody)) ipairs)   ; the hoistable WRITE-loops (map RMW, discarded result)
             mapsz (foldl (\ m w (: H (wmaph w sibnames) bd >w                                  ; PRESIZE: each write-loop's map H -> its counted bound N
                            cnd (? (two? bd && id? <bd '?) (opd bd 0) ())
                            N (? (two? cnd && id? <cnd '<) (opd cnd 1) ())                   ; cond (i < N) -> the bound
                            (? (name? H && !(id? N ())) ((H >< N) >< m) m))) () wlifts)       ; so `(tablet 0)` -> `(tablet N)`: the native inserts never grow
             lifts ((filter (\ pr (roloop? pr sibnames)) ipairs) + wlifts)
             (? (atom? lifts) [Fpair ()]
                (: remain (filter (\ pr !(any (\ lp (id? pr lp)) lifts)) ipairs)                ; the NON-lifted inner bindings (by identity)
                   specs (map (\ pr (<<pr                                               ; per lift a spec (g gn fv..): name, hoisted name F_g, captures
                               >< ((intern ((string F) + "_" + (string <<pr)))
                                     >< (filter (\ s (!(id? s '_) && !(member? s ><pr) && occurs? s >pr)) (P + valnames))))) lifts)   ; `_` is the throwaway/effect binding -- the loop body REBINDS it locally (the pin slot), never captures the enclosing one
                   (presz pr) (? (fnpair? pr) pr (: ms (afind (\ x (id? <x <pr)) mapsz) v >pr   ; a map binding `(H (tablet 0))` -> `(H (tablet N))`
                                  (? (two? ms && two? v && id? <v 'tablet) (<pr >< ['tablet >ms]) pr)))    ; presize `(tablet N)` so the N native inserts never grow
                   rwp (map presz (foldl (\ ps sp (subst-pairs <sp <>sp >>sp ps)) remain specs))   ; rewrite (g A..) calls through every spec, then PRESIZE the maps
                   rwb (foldl (\ b sp (subst-call <sp <>sp >>sp b)) ibody specs)       ; ... and in the trailing body
                   newdefs (map (\ sp (: lp (afind (\ pr (id? <<pr <sp)) lifts)            ; the hoisted def: (gn fv.. q..) . body'[g->gn]
                                  wl (any (\ w (id? w lp)) wlifts)                                   ; a WRITE-loop -> threaded body; else the plain read-only rename
                                  ((<>sp >< >>sp + ><lp)
                                     >< (? wl (wthread <sp <>sp >>sp (wmaph lp sibnames) lp)
                                           (subst-call <sp <>sp >>sp >lp))))) specs)
                   [(nf >< (': >< unpair rwp rwb)) newdefs])))))
   (plift form)                                                                                   ; hoist read-only loops out of every top-level fn's nested :
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          results (map plift-fn pairs)
          extradefs (foldl (\ a r (a + <>r)) () results)
          (? (atom? extradefs) form
             (': >< unpair ((map cap results) + extradefs) body))))
   ; --- MAP LANE (mapfix): a group fn's param that is `peep`'d but never `tally`'d is a MAP, not a string
   ;     (strscan's `s` is BOTH peep'd and tally'd; hash's `h` is peep'd, never tally'd). Rewrite that
   ;     param's `(peep h k d)` to `(mpeep h k d)` so the emitter lays an open-addressed map probe (and
   ;     types the param map-kind: raw pointer + a mapkind entry guard). The guard backstops the heuristic
   ;     -- a non-map arg deopts to interp. The string lane is untouched (tally'd peep-params stay peep).
   (puse? e P) (? (two? e) (? (id? <e 'peep && id? <>e P) 1 (foldl (\ a x (a || puse? x P)) 0 >e)) 0)   ; P used as (peep P k d)?
   (tuse? e P) (? (two? e) (? (id? <e 'tally && id? <>e P) 1 (foldl (\ a x (a || tuse? x P)) 0 >e)) 0)   ; P used as (tally P)?
   (peep->mpeep P e) (? (two? e) (? (id? <e 'peep && id? <>e P) ('mpeep >< map (\ x (peep->mpeep P x)) >e) (map (\ x (peep->mpeep P x)) e)) e)
   (pin->mpin P e)   (? (two? e) (? (id? <e 'pin && id? <>e P) ('mpin >< map (\ x (pin->mpin  P x)) >e) (map (\ x (pin->mpin  P x)) e)) e)   ; a map param's (pin P k v) UPDATE -> (mpin P k v); a cask param is pin'd-not-peep'd so never in mps
   (mapfix-def def) (: nf <def body >def
      mps (filter (\ P (puse? body P && !(tuse? body P))) >nf)                              ; this fn's MAP params (peep'd, not tally'd)
      (nf >< foldl (\ b P (pin->mpin P (peep->mpeep P b))) body mps))
   ; --- STRING-BUILDER fusion (castbuild): a string-accumulator LOOP -- (: (g v.. s ..) (? COND (g u.. (s + X) ..) s)
   ;     (g i.. "" ..)) builds a string by repeated append; each `+` copies the whole prefix, so the build is
   ;     O(n^2). Two rewrites: the clean single-byte counted case lowers to a NATIVE cask-fill (cbcask, below);
   ;     the GENERAL case (cbrw, here) threads a `jug` (the memory output PORT -- say into it, O(1)-amortized,
   ;     then ONE final O(n) drain). The jug REPLACES the accumulator IN PLACE as a threaded loop param -- init
   ;     (jug 0) (born ONCE at the call), passed UNCHANGED each step (the jug is mutated, not rebound), with
   ;     (say s (string X)) as the per-step effect; the base `s` becomes (slurp s). Threading the jug as a loop
   ;     var (not an aux let) is what lets it survive autogroup's purity-assuming inliners -- an aux (jug 0)
   ;     binding would be substituted into both use sites and reborn empty each step. (NB: a jug is a PORT, not
   ;     a cask -- a cask is the byte ARRAY the native path fills.) SOUND: (say s (string X)) reproduces +'s
   ;     string coercion EXACTLY (charm->1 byte, string->itself, named sym->spelling), so it equals (s + X)
   ;     wherever the `+` string lane is defined; the only divergence is where (s + X) is nil -- a broken build.
   ;     The guard demands s be a PURE append accumulator: init "", s appearing ONLY as (s + X) (s the LEFT
   ;     operand) in the tail self-call's slot and as the base result -- nowhere else (not the test, not X, not
   ;     a sibling arg). A non-matching loop / form rebuilds unchanged, so the other benches are untouched. This
   ;     is the OUTPUT-side dual of deforestation: there we fuse a consumed pipeline, here a produced string.
   (cbquote? x) (two? x && id? <x '\ && 2 = (tally x))                       ; (\ datum) -- quoted data, don't descend
   (cbsay? x)   (string? x || (two? x && id? <x 'string && 2 = (tally x))) ; already string-valued -> say x as-is
   (cbcall? e g ar) (two? e && id? <e g && (tally >e) = ar)             ; e a self-call (g ..) of arity ar
   (cbset l i v) (? (i = 0) (v >< >l) (<l >< cbset >l (i - 1) v))  ; l with index i replaced by v
   (cbnotelse? s as si) (foldl (\ a j (a && (? (j = si) 1 !(occurs? s (nthl as j))))) 1 (rangeL 0 (tally as)))   ; s in no arg but si
   (cbacc nf)                                                                        ; -> (si X g q COND RECUR recur-is-then?) on a match, else ()
    (? !(named-let? nf) ()
       (: nl <nf inits >nf gnf <>nl g <gnf q >gnf B <>>nl    ; B = the loop body
          (? !(two? B && id? <B '? && 4 = (tally B)) ()                   ; B = (? COND e1 e2)
             (: ar (tally q) c (opd B 0) e1 (opd B 1) e2 (opd B 2)
                rth (cbcall? e1 g ar) r (? rth e1 (cbcall? e2 g ar) e2 ()) base (? rth e2 e1)   ; which branch is the tail self-call; the other is the base
                (? !(two? r && name? base && member? base q) ()                ; need a self-call branch + a param-returning base
                   (: si (posof base q 0) sa (nthl >r si) IN (nthl inits si)
                      (? !(two? sa && id? <sa '+ && 3 = (tally sa) && id? (opd sa 0) base) ()   ; arg_si = (s + X)
                         (: X (opd sa 1)
                            (? (string? IN && 0 = (tally IN) && !(occurs? base c) && !(occurs? base X) && cbnotelse? base >r si)
                               [si X g q c r rth] ())))))))))                        ; init "" + s only in (s + X)@si and the base
   (cbrw nf info)                                                                    ; build the THREADED-jug rewrite: the accumulator param becomes a jug, init (jug 0)
    (: si (nthl info 0) X (nthl info 1) g (nthl info 2) q (nthl info 3) c (nthl info 4) r (nthl info 5) rth (nthl info 6)
       inits >nf  s (nthl q si)                                                 ; s = the accumulator param (now the jug, threaded unchanged)
       app  (? (cbsay? X) ['say s X] ['say s ['string X]])                        ; (say s X) | (say s (string X)) -- mirrors (s + X)
       r2   (': >< ['_ app (g >< cbset >r si s)])                        ; (: _ (say s ..) (g ..s..)) -- pass the jug UNCHANGED
       b2   (? rth ['? c r2 ['slurp s]] ['? c ['slurp s] r2])                    ; keep the original branch orientation; base -> (slurp s)
       (': >< [(g >< q) b2 (g >< cbset inits si '(jug 0))]))                ; (: (g q..) b2 (g ..(jug 0)..)) -- the jug born ONCE at the call
   (cbwalk e) (? (atom? e) e  (cbquote? e) e
                 (: nf (norm-let e) info (cbacc nf) (? (two? info) (cbrw nf info) (map cbwalk e))))
   ; --- the NATIVE CASK variant: a CLEAN single-byte counted builder -- (: (F p..) (: (go i s) (? (i < n)
   ;     (go (i + 1) (s + (string E))) s) (go 0 "")) ..) with i:0-by-1 the counter (cask index), E a CHARM
   ;     expr -- compiles the FILL loop to a native counted byte-store. We can't return the cask from native
   ;     (the epilogue tags rax as a fixnum, mangling the pointer), so we LIFT the loop to a sibling F-fill
   ;     that returns the count (a fixnum, glazeable) and threads the cask as a buf param (pin), while the
   ;     wrapper F holds the cask and converts it: (F p..) (: F-cb (cask n) _ (F-fill p.. 0 F-cb) (string
   ;     F-cb)). F-fill glazes (pin native); F stays interp (cask/string nifs). This is the elixir-class path
   ;     -- the build runs as machine code, one O(n) drain at the end. Falls back to the threaded `jug` (cbwalk)
   ;     for multi-char appends, non-0/1 counters, or the else-orientation.
   (cbbyte? e i) (? (whole? e) 1  (id? e i) 1  (two? e && id? <e 'peep) 1                ; a CHARM expr: const | the counter | a peep byte | arith thereof
                    (two? e && member? <e '(+ - * // % & | ^ << >>) && 3 = (tally e)) (cbbyte? (opd e 0) i && cbbyte? (opd e 1) i) 0)
   (cbcask nlval nf)                                                                              ; -> (wrapper-pair fill-pair) on a clean single-byte counted match, else ()
    (? !(two? nf && name? <nf && 1 < (tally nf)) ()                                    ; nf must be (F p..) so we can sibling the fill
       (: info (cbacc nlval)
          (? !(two? info) ()
             (: si (nthl info 0) X (nthl info 1) g (nthl info 2) q (nthl info 3) cond (nthl info 4) r (nthl info 5) rth (nthl info 6) inits >nlval
                (? !(rth && (two? cond) && (id? <cond '<) && (3 = (tally cond)) && (member? (opd cond 0) q)   ; recur-then; cond = (i < n)
                      && (two? X) && (id? <X 'string) && (2 = (tally X)) && (cbbyte? (opd X 0) (opd cond 0))) ()   ; append = (string E), E a charm in the counter
                   (: i (opd cond 0) n (opd cond 1) ci (posof i q 0) E (opd X 0) rargs >r ia (nthl rargs ci)
                      (? !(0 = (nthl inits ci) && two? ia && id? <ia '+ && 3 = (tally ia) && id? (opd ia 0) i && 1 = (opd ia 1)) ()   ; counter init 0, step (i + 1)
                         (: F <nf Fp >nf s (nthl q si) fillnm (intern ((string F) + "-fill")) cbn (intern ((string F) + "-cb"))
                            fv (filter (\ p (occurs? p cond || occurs? p r)) Fp)               ; enclosing params the loop reads
                            fbody ['? cond (fillnm >< fv + (cbset rargs si ['pin s i E])) i]     ; (? COND (F-fill fv.. (i + 1) (pin s i E)) i)
                            wbody (': >< [cbn ['cask n] '_ (fillnm >< fv + (cbset inits si cbn)) ['string cbn]])
                            [(nf >< wbody) ((fillnm >< fv + q) >< fbody)]))))))))
   (cbcpair p) (: info (cbcask (norm-let >p) <p) (? (two? info) info [(<p >< cbwalk >p)]))    ; cask variant, else keep (jug via cbwalk)
   (cbflat ps) (? (atom? ps) () (: pr <ps (<pr >< (>pr >< cbflat >ps))))   ; pairs -> a binding list, NO trailing body (preserves a body-less `:`)
   (castbuild form)
    (? !(two? form && id? <form ': && !(atom? >form)) (cbwalk form)              ; a `:` group: try the cask variant per binding (adds F-fill siblings); else the in-place walk
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          newpairs (foldl (\ a r (a + r)) () (map cbcpair pairs))
          (? (1 = ((tally >form) % 2)) (': >< unpair newpairs (cbwalk body))             ; ODD count -> has a body
             (': >< cbflat newpairs))))                                                       ; EVEN -> body-less (leaks globals: bench.l!), keep it so
   ; fold-consts: inline a top-level `:`'s WHOLE-constant value bindings (e.g. strscan's `hmod 1000000007`)
   ; into every body and drop them -- so a folded constant doesn't read as an un-glazeable free symbol.
   (fold-consts form)
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          consts (filter (\ pr (name? <pr && whole? >pr)) pairs)
          (? (atom? consts) form
             (: sub (\ e (foldl (\ b pr (subst-sym <pr >pr b)) e consts))
                keep (filter (\ pr !(name? <pr && whole? >pr)) pairs)
                (': >< unpair (map (\ pr (<pr >< sub >pr)) keep) (sub body))))))
   ; --- HOF INLINE (dehof): inline a top-level `:`'s non-recursive lambda-CONSTANT bindings -- a plain-name
   ;     binding to a (\ ..) value, like the closure bench's twice/adder -- into every body, then `dsimp`
   ;     beta-collapses the now-LITERAL curried application. So a higher-order recurrence ((twice (adder i)) i)
   ;     reduces to first-order arithmetic (3i) that lift+autogroup glaze; the inlined lambdas are dropped
   ;     (like fold-consts). dsimp is the same hygienic beta engine deforestation uses; the closed-lambda /
   ;     distinct-loop-var common case is capture-safe. No-op on any `:` with no lambda-const binding.
   ; desugar a binding head so a define-sugar `(f a..) body` is treated the SAME as the explicit
   ; `f (\ a.. body)` -- the surface form must not decide anything (was: `(name? <pr)` on the RAW
   ; head, which rejected define-sugar, making glazeability depend on how a sibling binding was spelled).
   (dehead pr) (: h <pr (? (atom? h) pr (<h >< ('\ >< >h + [>pr]))))    ; (f a..) body -> (f . (\ a.. body)); a plain name binding passes through (dsug is mopped post-egg)
   ; --- HIGHER-ORDER inference (smfix-shaped): a group fn is HIGHER-ORDER iff it RETURNS a function or
   ;     TAKES a function param. dehof inlines exactly the HO fns -- the group lane is FIRST-ORDER (params
   ;     are integer values), so a HOF can ONLY glaze by being inlined + dsimp-collapsed; a first-order fn
   ;     (prime?/count, identity) is left for grpfix to GROUP. A monotone fixpoint over the call graph
   ;     (cf. emit.l's smask string-param inference): func-param(f,k) = param k is APPLIED in operator
   ;     position OR PASSED to a callee's func-param; func-result(f) = body yields a function (a lambda,
   ;     a PARTIAL call, a call to a HO-result fn, a func-typed param, or a ?-branch that does). SOUND:
   ;     marks HO only when PROVABLE -- a missed HOF merely falls to interp (the param fixnum-guard deopts),
   ;     never a wrong answer. Semantic + desugared, so it no longer depends on surface form. Local helpers
   ;     (ho-*) avoid the post-egg-mopped boxfix globals (dsug/lambp/assoc/init).
   (ho-initl l) (? (atom? >l) () (<l >< ho-initl >l))                         ; all but the last
   (ho-lamb? v) (two? v && id? <v '\ && 2 < (tally v))
   (ho-any p l) (foldl (\ a x (a || p x)) 0 l)
   (ho-memb x l) (? (atom? l) 0 (id? <l x) 1 (ho-memb x >l))
   (ho-posn x l i) (? (atom? l) -1 (id? <l x) i (ho-posn x >l (i + 1)))
   (ho-dedup l) (? (atom? l) () (ho-memb <l >l) (ho-dedup >l) (<l >< ho-dedup >l))
   (ho-assq k l) (? (atom? l) () (id? <<l k) <l (ho-assq k >l))                ; entry (k . rest) by car
   (ho-nth l i) (? (i = 0) <l (ho-nth >l (-1 + i)))
   (ho-fpm nm hm) (: e (ho-assq nm hm) (? (two? e) >>e ()))                                  ; nm's func-param mask
   (ho-rf nm hm) (: e (ho-assq nm hm) (? (two? e) <>e 0))                                    ; nm returns a function?
   (ho-ar nm info) (: e (ho-assq nm info) (? (two? e) (tally <>e) -1))                       ; nm's arity, -1 if not a group fn
   (ho-walkfp fp body hm)                                                                         ; func-param mask of fp in body
    (ho-dedup ((: (walk e acc) (? !(two? e) acc
                  (: hd <e a0 (? (ho-memb hd fp) (ho-posn hd fp 0 >< acc) acc)            ; (p ..) -> p applied as a fn
                     gm (ho-fpm hd hm)
                     a1 (? (atom? gm) a0 (foldl (\ ac j (: arg (ho-nth >e j) (? (ho-memb arg fp) (ho-posn arg fp 0 >< ac) ac))) a0 gm))   ; OUR param at a callee's func-pos
                     (foldl (\ ac x (walk x ac)) a1 >e)))) body ()))
   (ho-ret? e fp fpm info hm)                                                                     ; does e yield a function?
    (? (atom? e) (ho-memb e fp && ho-memb (ho-posn e fp 0) fpm)                               ; a func-typed param
       (id? <e '\) (ho-lamb? e)                                                              ; a lambda literal
       (id? <e '?) (ho-any (\ b (ho-ret? b fp fpm info hm)) >e)                          ; a ?-branch that does
       (: hd <e ar (ho-ar hd info) (? (ar < 0) 0 ((tally >e) < ar) 1 (ho-rf hd hm))))    ; non-group->0; PARTIAL->fn; full->g's result
   (ho-fn fp body info hm) (: fpm (ho-walkfp fp body hm) ((? (ho-ret? body fp fpm info hm) 1 0) >< fpm))
   (ho-sz hm) (foldl (\ a e (a + <>e + (tally >>e))) 0 hm)                            ; monotone measure (rflags + |fpmask|)
   (ho-fix info hm) (monofix hm (\ st (map (\ f (<f >< ho-fn (ho-nth f 1) (ho-nth f 2) info st)) info)) ho-sz)   ; higher-order lattice (ascending: rflags+|fpmask|) over the SHARED harness (prel)
   (dehof form)
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          fns (filter (\ p (ho-lamb? >p)) (map dehead pairs))                              ; lambda-valued (desugared) bindings = the group fns
          info (map (\ p [<p (ho-initl >>p) (last >p)]) fns)                 ; (name params body)
          hm (ho-fix info (map (\ f (<f >< (0 >< ()))) info))                            ; the higher-order fixpoint
          ho? (\ nm (: e (ho-assq nm hm) (two? e && (<>e || two? >>e))))             ; returns-fn OR takes-fn -> inline it
          lamc? (\ pr (: nd (dehead pr) (ho-lamb? >nd && !(dcount <nd >nd) && ho? <nd)))   ; a NON-RECURSIVE higher-order const
          lamcs0 (map dehead (filter lamc? pairs))
          ; a lamc NAMED in another lamc's body stays bound: the fold substitutes each value as
          ; written, so a name already spent is never rewritten inside the text replacing a later
          ; one -- inlining both would leave the dropped binding dangling in that text.
          lamcs (filter (\ q !(ho-any (\ r (!(id? <r <q) && occurs? <q >r)) lamcs0)) lamcs0)
          (? (atom? lamcs) form                                                                  ; no HOF const -> leave the form for the other lanes (dsimp mangles string/cask/loop forms)
             (: sub (\ e (dsimp (foldl (\ b pr (subst-sym <pr >pr b)) e lamcs)))         ; inline each HOF lamc, then beta-collapse
                keep (filter (\ pr !(ho-any (\ q (id? <q <(dehead pr))) lamcs)) pairs)         ; non-inlined bindings keep their RAW form (grpfix recognizes define-sugar function heads)
                (': >< unpair (map (\ pr (<pr >< sub >pr)) keep) (sub body))))))
   ; --- CHURCH LOWERING (dechurch): a literal church application (N f x) -- N a small whole numeral,
   ;     f a FUNCTION (a lambda literal or a partial of a dyadic op, e.g. (+ i)) -- is f applied to x
   ;     N times: ((N f) x) = f^N(x). Lower it to the nested application at compile time:
   ;     (2 (+ i) i) -> (i + i + i). Pure arithmetic the group lane glazes -- where church is
   ;     otherwise OPAQUE to the glaze (every application dispatches through the lisp `num-ap` closure,
   ;     so it stays interpreted). EXACT -- this IS love's church semantics: (0 f x)=x (0=const-1->1=id),
   ;     (1 f x)=(f x), and dch-nest builds f^N innermost-out. Disambiguated from POWER by "the operand
   ;     is a function": (2 i)/(2 5)/(2 3 4) carry a NON-function operand (a numeral, or tally /= 2) so
   ;     dch-fn? is false -> left as exponentiation/the tower. A lambda operand is beta-collapsed by
   ;     dsimp; an op-partial needs no beta. Bottom-up; no-op unless the operand is a literal function,
   ;     so a variable count/operand (e.g. a bound `dbl`) still rides num-ap (sound, just interpreted).
   (dch-fn? f) (ho-lamb? f || (two? f && 2 = (tally f) && member? <f '(+ - * // % & | ^)))   ; a lambda, or a dyadic-op partial like (+ i)
   (dch-ap f x) (? (ho-lamb? f) (dsimp [f x]) (f + [x]))                                            ; apply f to x: beta a lambda, extend an op-partial
   (dch-nest f n x) (? (n = 0) x (dch-ap f (dch-nest f (n - 1) x)))                                     ; f^n(x), built innermost-out
   (dch-occs v e) (? (id? e v) 1  (atom? e) 0  (dquote? e) 0                                            ; occurrences of the param in the body (shadow/quote overcount = safe)
                     (foldl (\ s t (s + (dch-occs v t))) 0 e))
   ; the FAT GATE: beta splices the accumulated x at EVERY param occurrence, so a body using its
   ; param k>=2 times residualizes ~k^N nodes -- exponential when the seed does not fold (a captured
   ; outer var blocks every fold). unroll only while k^N stays small; a fat one rides num-ap
   ; (sound, interpreted) exactly like a variable count. op-partials and multi-param lambdas
   ; never duplicate (dsimp betas unary only), so only the unary lambda is measured.
   (dch-fat? f n) ((ho-lamb? f) && (3 = (tally f))
                   && (: k (dch-occs <>f <>>f) (1 < k && 256 < (n k))))
   (dechurch e)
    (? (atom? e) e  (dquote? e) e
       (: y (map dechurch e)                                                                           ; bottom-up: lower nested church first
          (? ((3 = (tally y)) && (whole? <y) && (0 <= <y) && (<y < 64) && (dch-fn? <>y)
              && !(dch-fat? <>y <y))
             (dch-nest <>y <y <>>y)                                       ; (N f x) -> f^N(x), the FLAT surface form
             ((2 = (tally y)) && (two? <y) && (2 = (tally <y)) && (whole? <<y) && (0 <= <<y) && (<<y < 64) && (dch-fn? <><y)
              && !(dch-fat? <><y <<y))
             (dch-nest <><y <<y <>y)                                                ; ((N f) x) -> f^N(x), the CURRIED surface form -- ((2 (+ i)) i) and (2 (+ i) i) are the same value, so compile the same
             y)))
   ; --- LOCAL LET (delet): inline a PURE value-let -- (: n1 v1 .. result), every binding a plain symbol
   ;     (no local fn -> that is norm-let/lift's job), names distinct -- into ONE fragment expression by
   ;     ordered substitution, so a fn that names its intermediates joins the group. The fragment is pure
   ;     only when dup-cheap (dcheap?: arithmetic/cmp/peep/tally, NO call): substitution then preserves
   ;     value AND order. A binding holding a CALL never inlines -- a dup recomputes, a 0-use drop loses
   ;     an effect, and even a single-use move breaks the sequence law when the call effects (a user fn
   ;     hides its effects from any head scan) -- it BAILS to the unchanged `:` (groupok? rejects -> interp,
   ;     sound, no perf cliff).
   ;     delet-body walks the WHOLE body BOTTOM-UP, so a value-let inlines wherever it sits -- at the body
   ;     root OR nested in a sub-expression (e.g. the accumulator arg of a tail call, `(g i (: x v (x + x)))`,
   ;     the natural "bind an intermediate" idiom). Without this the emitter hits a raw `:` in arg position
   ;     and the whole group DEOPTS (slower than interp); with it the loop glazes. Bottom-up is what makes a
   ;     SHADOWING inner let resolve before its outer namesake substitutes. A `\` (quote/lambda) is opaque
   ;     -- never descended; a local-fn `(g q..)` binding is left as-is (defer to norm-let/lift). delet runs
   ;     BEFORE lift: a value-let may WRAP a named-let loop, whose free vars it must fix up first.
   (dcount s e) (? (id? e s) 1 (two? e) (foldl (\ a x (a + (dcount s x))) 0 e) 0)            ; syntactic occurrences of s in e
   (dcheap? e) (? (atom? e) 1  (dquote? e) 0                                                  ; dup-cheap: arith/cmp/bit/peep/tally, NO call
                  (: h <e (? (id? h '? || member? h '(+ - * // % & | ^ < <= > >= = peep tally)) (all dcheap? >e) 0)))
   (delet-go pairs result orig)
    (? (atom? pairs) result
       (: pr <pairs nm <pr val >pr rest >pairs
          uses (foldl (\ a e (a + (dcount nm e))) 0 ((map cup rest) + [result]))          ; uses in subsequent values + body
          (? !(dcheap? val) orig                                                          ; a CALL never inlines: a dup (>1 use) recomputes, a 0-use drop loses an effect, and even a 1-use move breaks the sequence law when the call has effects (a user fn hides them from uses-head?) -> bail to interp
             (delet-go (map (\ p (<p >< subst-sym nm val >p)) rest) (subst-sym nm val result) orig))))
   (delet-body e)                                                                             ; inline value-lets ANYWHERE in a fn body (root OR nested), bottom-up
    (? (atom? e) e
       (id? <e '\) e                                                                     ; a quote / lambda is opaque -- never descend
       !(id? <e ':) (map delet-body e)                                                ; a (?/call/arith) form -> recurse into children to reach a nested let
       (atom? >e) e                                                                      ; a bare (:) -> leave
       (: parsed (anat-defs >e) pairs <parsed result <>parsed names (map cap pairs)
          hasbody (1 = ((tally >e) % 2))                                                  ; ODD count -> a trailing result; EVEN -> body-less (a named-let returning its fn)
          pairs2  (map (\ pr (<pr >< delet-body >pr)) pairs)                     ; bottom-up: inline within each value (incl. a local-fn's BODY) ...
          result2 (delet-body result)                                                         ; ... and within the result
          (? (!hasbody || !(all name? names && distinctp names) || uses-head? roalloc e) ; body-less, a local-fn `(g q..)`, OR an EFFECTFUL let (a `_`-bound `pin`/`say`/.. used 0x must NOT be dropped) -> KEEP the structure (parity-preserving),
               (': >< (? hasbody (unpair pairs2 result2) (cbflat pairs2)))                   ; but surface a value-let nested in any body (else it survives lift -> deopt)
             (delet-go pairs2 result2 (': >< unpair pairs2 result2)))))                    ; a PURE value-let with a body -> inline it (a bail keeps the walked `:`, sound)
   (delet form)                                                                              ; inline value-lets in every fn body of a `:`
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          (': >< unpair (map (\ pr (<pr >< (? (two? <pr) (delet-body >pr) >pr))) pairs) body)))
   ; --- BOOLEANS & CLAMPS (debool): lower &&/||/abs to the `?`-fragment the glaze already compiles --
   ;     exact, value-preserving rewrites (verified): (&& a b)=(? a b 0), (|| a b)=(? a a (? b b 0)),
   ;     (abs x)=(? (x < 0) (0 - x) x). The &&/|| forms bake in the floor-clamp semantics (a false `&&`
   ;     nets 0, like all/any's bits) -- which is exactly why they equal these `?` forms, not the looser
   ;     (? a b a). && never duplicates an operand; ||/abs do, so they reuse delet's `dcheap?` guard -- a
   ;     form that would duplicate a CALL is LEFT un-lowered (groupok? then rejects the head -> interp,
   ;     sound). Bottom-up (operands lowered first), so nested compounds collapse. A no-op on any form
   ;     with none of these heads, and runs INSIDE autogroup -- so autonat (which matches the float-grid
   ;     escape test `(&& (it < L) ..)` on the RAW form, before autogroup) is unaffected. (NOTE: `min`/
   ;     `max` are array REDUCERS, not binary scalar ops -- `(min 7 3)` is `((min 7) 3)` = `(7 3)` = 3^7,
   ;     NOT 3 -- so a binary `(min a b)` is no language feature and is deliberately NOT lowered here.)
   (dband as) (? (atom? as) 1  (atom? >as) <as  ['? <as (dband >as) 0])    ; (&& a R..) = (? a (&& R..) 0)
   (dbor as)  (? (atom? as) 0  !(dcheap? <as) ('|| >< as)                             ; (|| a R..) = (? a a (|| R..)); a dup'd, so guard
                 ['? <as <as (dbor >as)])
   (dbabs x) (? !(dcheap? x) ['abs x] ['? ['< x 0] ['- 0 x] x])                           ; (abs x) = (? (x < 0) (0 - x) x)
   (debool e)                                                                                   ; recursively lower &&/||/abs to ?
    (? (atom? e) e  (dquote? e) e
       (: y (map debool e) h (opword <y) as >y                    ; lower operands first (bottom-up); opword names a glued VALUE head
          (? (id? h '&&) (dband as)  (id? h '||) (dbor as)
             (id? h 'abs && 1 = (tally as)) (dbabs <as)  y)))
   ; --- SPECIALIZATION (autospec): mix (doc/misc/proto/mix.l, the offline partial evaluator) wired in as a
   ;     glaze recognizer -- the 1st Futamura projection COMPOSED with the native lane. an interpreter
   ;     applied to a STATIC program (a const-bound quoted AST) and a DYNAMIC input specializes: the
   ;     dispatch + AST navigation fold away to the program's straight-line arithmetic, which defoliate/
   ;     lift/autogroup then native -- so an interpreter-over-static-data loop runs as COMPILED code,
   ;     capturing the interpretation-overhead prize (interp-spec.l measured ~6.5x) the in-compiler
   ;     unfold missed. `spe` is the proto engine with `defs` (the form's fn defs) threaded as a param,
   ;     not a hardcoded table; a STATIC-driven recursion unrolls (the DESCENT guard in specall bounds
   ;     re-entry, sfuel bounds a runaway), a DYNAMIC-driven one residualizes clean. autospec is a
   ;     NO-OP (id-identical) on any form with no defined-fn call over a static-data binding.
   (sstat v) ['s v]  (sdyn c) ['r c]  (sstat? p) (id? <p 's)  (spval p) <>p
   (sqv v) ['\ v]                                                                 ; quote a static datum -> seen as DATA
   (sreify p) (: v <>p (? (sstat? p && !(constellation? v)) (sqv v) v))  ; non-number static -> quoted
   (sstatval? p v) (sstat? p && v = (spval p))
   (szipenv ps apv) (? (atom? ps) () ([<ps <apv] >< szipenv >ps >apv))
   (slook env k) (? (atom? env) (sdyn k) (id? k <<env) <><env (slook >env k))  ; unbound -> dynamic
   (sdeflook nm defs) (? (atom? defs) () (id? nm <<defs) <defs (sdeflook nm >defs))
   (sprim? h) (member? h '(* - + ! < <= > >= = cap cup caap caup cuap cuup id?))   ; arith + comparisons + static AST navigation
   (ssimp1 h a b)
    (? (id? h '* && sstatval? a 1) b   (id? h '* && sstatval? b 1) a       ; drop the * and + units on a residual
       (id? h '+ && sstatval? a 0) b   (id? h '+ && sstatval? b 0) a
       (sdyn [h (sreify a) (sreify b)]))
   (speprim h apv)
    (? (all sstat? apv) (sstat (base-ev (h >< map sqv (map spval apv))))       ; all static -> FOLD over quoted data
       (2 = (tally apv)) (ssimp1 h <apv <>apv)                           ; binary -> residualize, drop units
       (sdyn (h >< map sreify apv)))
   (spcondm cs env seen defs)
    (? (atom? >cs) (spe <cs env seen defs)                              ; lone else
       (: cv (spe <cs env seen defs)
          (? !(sstat? cv) (sdyn ('? >< sresidcl cv <>cs >>cs env seen defs))  ; dynamic test -> residual tail
             !(spval cv) (spcondm >>cs env seen defs)                     ; static false -> drop clause
             (spe <>cs env seen defs))))                                     ; static true  -> take result
   (sresidcl cv then rest env seen defs)
    (sreify cv >< (sreify (spe then env seen defs) >< sresidtl rest env seen defs))
   (sresidtl cs env seen defs)
    (? (atom? >cs) [(sreify (spe <cs env seen defs))]
       (: cv (spe <cs env seen defs)
          (? !(sstat? cv) (sresidcl cv <>cs >>cs env seen defs)
             !(spval cv) (sresidtl >>cs env seen defs)
             [(sreify (spe <>cs env seen defs))])))
   ; the DESCENT guard: an unfold re-entering a fn along the chain must strictly SHRINK its
   ; static weight (total leaf count of the static args) -- the structural-recursion certificate.
   ; an AST walk (the Futamura prize) shrinks its static datum every level, so it unfolds to the
   ; bottom; a non-descending re-entry residualizes as a plain call (still correct). the old
   ; same-SIGNATURE seen-set couldn't see a CHANGING static ((n - 1) never repeats): a recursion
   ; whose test doesn't fold speculatively unrolled to the fuel floor -- a 100k-deep residual,
   ; a hung compile. `seen` threads the in-progress chain as (nm . weight) pairs.
   (ssz v) (? (two? v) ((ssz <v) + (ssz >v)) 1)                          ; total leaf count of a static datum
   (sw apv) (foldl (\ a p (? (sstat? p) (a + (ssz (spval p))) a)) 0 apv)           ; the call's static weight
   (sanc nm seen) (? (atom? seen) -1 (id? nm <<seen) ><seen (sanc nm >seen))   ; nearest in-progress weight for nm (-1 = none)
   (specall def apv seen defs)                                                    ; unfold a defined-fn call, unless non-descending / out of fuel
    (: nm <def ps <>def bd <>>def sz (sw apv)
       w (sanc nm seen) n (peep sfuel 'n 0)
       (? (0 <= w && w <= sz) (sdyn (nm >< map sreify apv))               ; no strict descent -> residual call
          !n (sdyn (nm >< map sreify apv))                                 ; fuel out -> bail to a residual call (still correct)
          (: _ (pin sfuel 'n (n - 1)) (spe bd (szipenv ps apv) ((nm >< sz) >< seen) defs))))
   (spe x env seen defs)                                                          ; the partial evaluator
    (? (constellation? x) (sstat x)
       (nom? x) (slook env x)
       (atom? x) (sstat x)
       (: h <x a >x
          (? (id? h '\ && atom? >a) (sstat <a)                      ; (\ datum) quote -> a static datum
             (id? h '?) (spcondm a env seen defs)
             (sprim? h) (speprim h (map (\ e (spe e env seen defs)) a))
             (: d (sdeflook h defs)
                (? d (specall d (map (\ e (spe e env seen defs)) a) seen defs)
                     (sdyn (h >< map sreify (map (\ e (spe e env seen defs)) a))))))))
   ; the residual-SIZE gate: spe substitutes a param's residual at EVERY occurrence, so nested
   ; unfolds can share one small DAG whose TREE reading is exponential -- every downstream walker
   ; (the passes, the printer, the compiler) reads trees, so an over-budget residual is a hang,
   ; not a win. stsz walks with a step budget (0 = blown); an over-budget residual DECLINES to
   ; the original call (autospec is a recognizer: bailing is always sound).
   (stsz e n) (? (n < 1) 0 (atom? e) (n - 1) (: m (stsz <e (n - 1)) (? !m 0 (stsz >e m))))
   (smix call statenv defs) (: _ (pin sfuel 'n 100000) r (sreify (spe call statenv () defs))
                               (? (stsz r 20000) r call))   ; specialize one call -> residual source (small ones only)
   ; autospec proper: build defs (fn bindings) + statenv (names bound to a quoted datum), then replace
   ; every call to a defined fn that mentions a static name with its specialized residual (in place).
   (sconst? v) (two? v && id? <v '\ && 2 = (tally v))                      ; a quoted-constant binding value (\ datum)
   (sment? e statn) (? (name? e) (member? e statn)  (two? e) (srefany? e statn)  0)
   (srefany? args statn) (? (atom? args) 0 (sment? <args statn || srefany? >args statn))
   (specall? e defn statn) (two? e && member? <e defn && srefany? >e statn)   ; defined-fn call mentioning a static name
   (swalk e statenv defs defn statn)
    (? (atom? e) e
       (dquote? e) e                                                              ; never descend into a quoted datum
       (specall? e defn statn) (smix e statenv defs)                              ; specialize this call in place
       (map (\ x (swalk x statenv defs defn statn)) e))                           ; walk EVERY element (a list head holds the loop body)
   (autospec form)
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed
          fnps (filter (\ pr (two? <pr)) pairs)
          defs (map (\ pr (<<pr >< (><pr >< (>pr >< ())))) fnps)   ; (name (p..) body)
          statps (filter (\ pr (name? <pr && sconst? >pr)) pairs)
          statenv (map (\ pr [<pr (sstat <>>pr)]) statps)         ; name -> (stat datum)
          (? !(two? defs && two? statenv) form                            ; need a defined fn AND a static-data binding
             (swalk form statenv defs (map cap defs) (map cap statenv)))))
   ; --- DEFORESTATION: fuse a map/filter pipeline under a CONSUMER into ONE pass, then lower it over
   ;     (jot N) to the named-let LOOP `lift` already glazes -- so a combinator pipeline compiles to
   ;     native. The rewrite-level companion to the codegen recognizers; the standalone spec lives in
   ;     doc/misc/proto/forest.l. Consumers glazed: foldl/foldr (a reduction -> a counted loop, UP for foldl,
   ;     DOWN for foldr) and all/any (a short-circuit predicate -> an early-exit loop returning the truth
   ;     BITS 0/1). all/any glaze transparently ONLY because their false value is now the fixnum 0 (the
   ;     green floor) like =/</!, not the zero point `()` -- the integer compiler returns 0, so it matches
   ;     (see love/prel.l: all/any return truth bits). `each` is NOT here: its body is an EFFECT, outside
   ;     the arithmetic fragment. Transducer view of shortcut fusion -- map f: elem|->(f elem); filter p:
   ;     step|->(? (p elem) step skip). `dsimp` then beta-inlines the unary stage lambdas + flattens
   ;     curried operator heads to a FIRST-ORDER step. Step binders are fresh gensym noms (hygienic).
   ;     `defoliate` is a NO-OP (id-identical) on any form with no pipeline -- so non-pipeline forms are byte-unchanged.
   (dquote? x) (two? x && id? <x '\ && 2 = (tally x))                  ; (\ datum) -> a quote, never descend
   (drm ks env) (filter (\ p !(member? <p ks)) env)                    ; drop the shadowed substitutions
   (dcolnms bs) (? (atom? >bs) () (: n <bs (? (nom? n) (n >< dcolnms >>bs) (dcolnms >>bs))))   ; a : binding list's binder noms
   (dsubx env x)                                                              ; capture-avoiding multi-substitution (twin of ev.l's): α-rename BOTH \ and : binders
    (? (nom? x) (: p (assoc x env) (? (two? p) >p x))
       (atom? x) x
       (dquote? x) x
       (id? <x '\) (? (atom? >>x) x
          (: ps (init >x) ren (map (\ p (p >< nom 0)) ps)
             ('\ >< (map cup ren) + [(dsubx (ren + (drm ps env)) (last x))])))
       (id? <x ':) (: nms (dcolnms >x) ren (map (\ p (p >< nom 0)) nms)
          (map (\ e (dsubx (ren + (drm nms env)) e)) x))
       (map (\ e (dsubx env e)) x))
   (dsub old new B) (dsubx [(old >< new)] B)                               ; substitute old->new in B, capture-avoiding
   ; the define-sugar DECLINE guard: a sugared : binder ((g z..) B) is a LIST --
   ; dsubx never renames it -- so a beta whose argument carries a colliding free name would capture.
   ; DECLINE such a beta: the redex stays source -- a missed inline, never a miscompile.
   (dsyms e) (? (nom? e) [e] (atom? e) () (dquote? e) () (foldl (\ a x (a + (dsyms x))) () e))
   (dsugcol bs) (? (atom? >bs) () (: r (dsugcol >>bs) n <bs (? (two? n) (n + r) r)))
   (dsugs e) (? (atom? e) () (dquote? e) ()
                (: r (foldl (\ a x (a + (dsugs x))) () e)
                   (? (id? <e ':) ((dsugcol >e) + r) r)))
   (dbeta? old new B) (: ds (dsugs B)
      (? (atom? ds) 1
         (!(member? old ds) && !(foldl (\ a s (a || member? s ds)) 0 (dsyms new)))))
   (dmap? P)   (two? P && id? <P 'map)
   (dfilt? P)  (two? P && id? <P 'filter)
   (dprod? P)  (dmap? P || dfilt? P)
   (dsfn P)    <>P  (dssrc P) <>>P                              ; (map f P') -> f / P'
   (dpeel B e felse P)                                                        ; thread step body B over (elem e, filter-skip felse) through the chain
    (? (dmap? P)  (dpeel (dsub e [(dforest (dsfn P)) e] B) e felse (dssrc P))    ; e := (f e)
       (dfilt? P) (dpeel ['? [(dforest (dsfn P)) e] B felse] e felse (dssrc P))   ; (? (p e) B skip)
       [B (dforest P)])                                                      ; base list (forest nested pipelines)
   (dfold? x) (member? <x '(foldl foldr) && 4 = (tally x) && (: P <>>>x (dprod? P || djot? P)))   ; (foldl/foldr c z P)
   (dpred? x) (member? <x '(all any) && 3 = (tally x) && (: P <>>x (dprod? P || djot? P)))         ; (all/any q P)
   (dforest x)
    (? (atom? x)   x
       (dquote? x) x
       (dfold? x)  (: tag <x c <>x z <>>x P <>>>x e (nom 'e) a (nom 'a)
                      B0 (? (id? tag 'foldl) [c a e] [c e a])               ; foldl step (c a e); foldr step (c e a)
                      r (dpeel B0 e a P)                                      ; filter-skip = the acc (unchanged)
                      hd (? (id? tag 'foldl) ['\ a e <r] ['\ e a <r])
                      [tag hd (dforest z) <>r])
       (dpred? x)  (: tag <x q <>x P <>>x e (nom 'e) unit (? (id? tag 'all) 1 0)   ; filter-skip = the short-circuit unit
                      r (dpeel [q e] e unit P)
                      [tag ['\ e <r] <>r])
       (<x >< map dforest >x))
   (dspecial? h) (id? h '? || (id? h '\ || id? h ':))
   (dsimp x)                                                                  ; reduce to first-order: beta unary lambdas, flatten curried heads
    (? (atom? x)   x
       (dquote? x) x
       (: y (dsimp <x >< map dsimp >x) h <y as >y
          (? (atom? h) y
             (id? <h '\ && (3 = (tally h) && (1 = (tally as) && dbeta? <>h <as <>>h))) (dsimp (dsub <>h <as <>>h))
             (atom? <h && !(dspecial? <h))                 (dsimp (<h >< >h + as))
             y)))
   ; (jot N) is the LIST source -- the materialized chain (0 .. N-1) foldl/map iterate; the loop never
   ; builds it. (NOT iota -- that is the ARRAY @(0..N-1), which foldl/map (chain ops) do NOT iterate,
   ; so a foldl over iota interprets to z; lowering THAT would be unsound. jot only.)
   (djot? s) (two? s && id? <s 'jot && 2 = (tally s))                 ; (jot N)
   (dstep? f)  (two? f && id? <f '\ && 4 = (tally f))                 ; (\ A E BODY)  -- a 2-param fold step
   (dstep1? f) (two? f && id? <f '\ && 3 = (tally f))                 ; (\ E BODY)    -- a 1-param predicate
   (dlowerfold x)                                                            ; (foldl/foldr STEP Z (jot N)) -> counted loop (up / down)
    (: tag <x STEP <>x Z <>>x SRC <>>>x
       (? !(dstep? STEP && djot? SRC) x
          (: P1 <>STEP P2 <>>STEP BODY <>>>STEP N <>SRC go (nom 'go)
             (? (id? tag 'foldl)                                            ; STEP=(\ a e B): P1=a P2=e ; count e UP 0..N-1
                [[': [go P2 P1] ['? ['< P2 N] [go ['+ P2 1] BODY] P1]] 0 Z]
                [[': [go P1 P2] ['? ['< P1 0] P2 [go ['- P1 1] BODY]]] ['- N 1] Z]))))           ; STEP=(\ e a B): count e DOWN N-1..0
   (dlowerpred x)                                                            ; (all/any PRED (jot N)) -> short-circuit loop over one index
    (: tag <x PRED <>x SRC <>>x
       (? !(dstep1? PRED && djot? SRC) x
          (: E <>PRED BODY <>>PRED N <>SRC go (nom 'go)
             (? (id? tag 'all)                                              ; all: a false body returns 0 early; run-off returns 1
                [[': [go E] ['? ['< E N] ['? BODY [go ['+ E 1]] 0] 1]] 0]
                [[': [go E] ['? ['< E N] ['? BODY 1 [go ['+ E 1]]] 0]] 0]))))           ; any: a true body returns 1 early; run-off 0
   (dlower x)
    (? (atom? x) x (dquote? x) x
       (member? <x '(foldl foldr) && 4 = (tally x)) (dlowerfold (<x >< map dlower >x))
       (member? <x '(all any) && 3 = (tally x))     (dlowerpred (<x >< map dlower >x))
       (<x >< map dlower >x))
   (has-pipe? x) (? (atom? x) 0  (dquote? x) 0  (dprod? x) 1  (dfold? x) 1  (dpred? x) 1  (any has-pipe? >x))
   (defol-body b) (? (has-pipe? b) (dlower (dsimp (dforest b))) b)            ; fuse+lower a pipeline body, else leave it
   (defol-bs bl) (? (atom? >bl) [(defol-body <bl)]                 ; trailing body
                    (<bl >< (defol-body <>bl >< defol-bs >>bl)))   ; (name value ..) -> transform each value
   (defoliate form)
    (? !(has-pipe? form) form                                             ; NO pipeline anywhere -> byte-identical (every legacy form)
       !(two? form && id? <form ': && !(atom? >form)) (dlower (dsimp (dforest form)))
       (': >< defol-bs >form))
   ; --- LOOP-CLOSER (loopclose): collapse a polynomial accumulation `(foldl/foldr (+) z SRC)` to its
   ;     CLOSED FORM in the (possibly symbolic) bound -- work INDEPENDENT of the trip count, the sequel
   ;     to deforestation, a layer above mix (which only unfolds). Runs BEFORE defoliate (it wants the
   ;     raw fold, not the lowered loop); the standalone spec is doc/misc/proto/close.l. ENGINE: finite
   ;     differences in EXACT integers -- the body's forward differences c_k = Δ^k g(0) (Newton-Gregory)
   ;     + the hockey-stick identity Σ_{i<N} C(i,k) = C(N,k+1), so Σ_{i<N} g(i) = Σ_k c_k C(N,k+1),
   ;     emitted as PURE ARITHMETIC (falling factorial / k!), no runtime helper. The body is a BLACK-BOX
   ;     ORACLE we SAMPLE (base-ev, no glaze re-entry) -- a closure-building body closes like a bare line.
   ;     SOUNDNESS: map F / filter P must `qual` (pure arithmetic over one param -> no capture of the
   ;     bound), AND the difference table must DIE (top difference EXACTLY 0 -- `=0`, not `nil?`, which
   ;     also fires on a negative leading term). FILTERS: a period-2 predicate (odd/even) reparametrizes
   ;     to the dense kept index (k=2j+1 / 2j) -- the SCEV-beating `deforest` case; other predicates
   ;     DECLINE to defoliate. Byte-identical on any form with no closeable fold.
   (lcfd ys)    (? (atom? >ys) () (<>ys - <ys >< lcfd >ys))         ; one forward difference
   (lclead ys)  (? (atom? ys) () (<ys >< lclead (lcfd ys)))                          ; leading diagonal c_0..c_d
   (lctopfd ys) (? (atom? >ys) <ys (lctopfd (lcfd ys)))                             ; last entry: Δ^(L-1) g(0)
   (lcsamp g d) (: (go i) (? (i <= d) (g i >< go (i + 1)) ()) (go 0))                   ; (g 0)..(g d)
   (lcdeg g cp) (: (go d) (? (d >= cp) -1 (? (0 = (lctopfd (lcsamp g (d + 1)))) d (go (d + 1)))) (go 0))   ; degree, -1 if not polynomial within cp
   (lcfac r)    (: (go k a) (? (k < 2) a (go (k - 1) (a * k))) (go r 1))                      ; r! (a literal at glaze time)
   (lcsub N j)  (? (j = 0) N ['- N j])                                                       ; (N - j), or N when j=0
   (lcff N j r) (? (j = (r - 1)) (lcsub N j) ['* (lcsub N j) (lcff N (j + 1) r)])            ; falling factorial N(N-1)..(N-r+1) as source
   (lccbn N r)  (? (r = 1) N ['// (lcff N 0 r) (lcfac r)])                                   ; C(N,r) as source (r consecutive ints / r!, exact)
   (lcmul c t)  (? (c = 1) t ['* c t])
   (lcadd a b)  (? (b = 0) a ['+ a b])
   (lcsum cks N) (: (go ks k) (? (atom? ks) 0 (: rest (go >ks (k + 1)) (? (<ks = 0) rest (lcadd (lcmul <ks (lccbn N (k + 1))) rest)))) (go cks 0))   ; Σ_k c_k C(N,k+1), nonzero terms
   (lcfinal z sum) (? (z = 0) sum ['+ z sum])
   (lcemit z g M) (: d (lcdeg g 8) (? (d < 0) () [(lcfinal z (lcsum (lclead (lcsamp g d)) M))]))    ; sample -> degree -> closed source, wrapped; () declines
   (lcjot? s)   (two? s && id? <s 'jot && 2 = (tally s))
   (lcmap? s)   (two? s && id? <s 'map && 3 = (tally s))
   (lcfilt? s)  (two? s && id? <s 'filter && 3 = (tally s))
   (lcplus? f)  (id? f '+ || (two? f && id? <f '+ && 1 = (tally f)))                ; + or (+)
   (lcfold? x)  (two? x && member? <x '(foldl foldr) && 4 = (tally x) && lcplus? (opd x 0))
   (lcsrc s)                                                                                  ; SRC -> (Fsrc Psrc Nexpr) | () ; F/P = () when absent
    (? (lcjot? s)  [() () (opd s 0)]
       (lcfilt? s) (: in (opd s 1) (? (lcjot? in) [() (opd s 0) (opd in 0)] ()))
       (lcmap? s)  (: in (opd s 1) (? (lcjot? in) [(opd s 0) () (opd in 0)]
                                       (lcfilt? in && lcjot? (opd in 1)) [(opd s 0) (opd in 0) (opd (opd in 1) 0)]
                                       ()))
       ())
   ; SOUNDNESS: only close a body that is a PURE POLYNOMIAL (+ - * over the param + int consts) -- a
   ; genuine polynomial, so the difference table dies EXACTLY at its degree (a body with //, %, <, or ?
   ; can fit a low-degree polynomial on the first samples yet break later -- sampling can't prove it).
   (lc1lam? f)   (two? f && id? <f '\ && 3 = (tally f) && name? (opd f 0))               ; (\ v BODY)
   (lcpoly? v e) (? (id? e v) 1 (whole? e) 1
                    (two? e && member? <e '(+ - *) && 3 = (tally e)) (lcpoly? v (opd e 0) && lcpoly? v (opd e 1))
                    0)
   (lcpolyf? f)  (lc1lam? f && lcpoly? (opd f 0) (opd f 1))
   ; and recognize the filter predicate STRUCTURALLY (odd/even over (v % 2)) -- not by sampling, which
   ; a longer-period predicate could fool. canonical forms: (v % 2)/(1 = (v % 2)) odd, (0 = (v % 2)) even.
   (lcmod2? e v) (two? e && id? <e '% && 3 = (tally e) && id? (opd e 0) v && (opd e 1) = 2)
   (lcoddp? f)   (? !(lc1lam? f) ()                                                          ; -> 'odd | 'even | ()
                    (: v (opd f 0) b (opd f 1)
                       (? (lcmod2? b v) 'odd
                          (two? b && id? <b '= && 3 = (tally b))
                             (: a (opd b 0) c (opd b 1)
                                (? (a = 1 && lcmod2? c v) 'odd  (c = 1 && lcmod2? a v) 'odd
                                   (a = 0 && lcmod2? c v) 'even (c = 0 && lcmod2? a v) 'even ()))
                          ())))
   (lcclose1 x)                                                                               ; (foldl/foldr (+) z SRC) -> (closed-source) | () decline
    (: z (opd x 1) p (lcsrc (opd x 2))
       (? (atom? p) ()
          (: Fsrc <p Psrc <>p N <>>p
             Fc (? (id? Fsrc ()) (\ i i) (? (lcpolyf? Fsrc) (base-ev Fsrc) ()))              ; identity if no map; else F must be a pure polynomial
             (? (id? Fc ()) ()
                (id? Psrc ()) (lcemit z Fc N)                                                 ; no filter: g = F over [0,N)
                (: pat (lcoddp? Psrc)                                                         ; else need a recognized odd/even filter
                   (? (id? pat 'odd)  (lcemit z (\ j (Fc (1 + (2 * j)))) ['// N 2])          ; k=2j+1, M=N//2
                      (id? pat 'even) (lcemit z (\ j (Fc (2 * j))) ['// ['+ N 1] 2])        ; k=2j,   M=(N+1)//2
                      ()))))))
   (lcquote? x) (two? x && id? <x '\ && 2 = (tally x))
   (lc:form? x) (two? x && id? <x ': && !(atom? >x))                            ; a (: ..) with bindings
   (lcocc? v e) (? (id? e v) 1 (atom? e) 0 (lcquote? e) 0 (any (\ s (lcocc? v s)) >e))   ; v occurs in e
   ; --- NAMED-LET ACCUMULATOR: a self-recursive `:` binding `(NM i a) (? (i < N) (NM (i + 1) (a + B)) a)`
   ;     with B a pure polynomial in i and N loop-invariant -> the CLOSED sum, O(1). Same finite-diff
   ;     engine as the fold lane (lcsum/lcdeg/lclead), just reached through a `:` binding instead of a
   ;     foldl combinator -- so a hand-written counting accumulator closes like `(foldl (+) 0 (map ..))`.
   ;     g(j)=B over j; Σ_{i<=j<N} g(j) = G[N]-G[i] (G the antidifference), GUARDED by the original test so
   ;     the function stays faithful for i>=N (loop returns acc, sum 0). Restricted: step (i + 1), test `<`,
   ;     op `+` (the additive accumulator); anything else falls through unchanged. B's purity is lcpoly?
   ;     (only i, + - *, int consts), which also rejects any reference to acc or an outer var.
   (lcacc? name val)
    ((two? name) && (3 = (tally name)) && (name? <name) && (name? <>name) && (name? <>>name)
     && !(id? <>name <>>name)                                               ; (NM i a), i /= a
     && (two? val) && (id? <val '?) && (4 = (tally val))                                        ; (? T THEN ELS)
     && (: NM <name I <>name A <>>name
           T (opd val 0) THEN (opd val 1) ELS (opd val 2)
           ((two? T) && (id? <T '<) && (3 = (tally T)) && (id? (opd T 0) I)                     ; T = (i < N)
            && !(lcocc? I (opd T 1)) && !(lcocc? A (opd T 1))                               ; N loop-invariant in i,a
            && (id? ELS A)                                                                      ; base case -> acc
            && (two? THEN) && (id? <THEN NM) && (3 = (tally THEN))                               ; THEN = (NM STEP ACC)
            && (: STEP (opd THEN 0) ACC (opd THEN 1)
                  ((two? STEP) && (id? <STEP '+) && (3 = (tally STEP)) && (id? (opd STEP 0) I) && ((opd STEP 1) = 1)  ; (i + 1)
                   && (two? ACC) && (id? <ACC '+) && (3 = (tally ACC)) && (id? (opd ACC 0) A)       ; (a + B)
                   && (lcpoly? I (opd ACC 1)))))))                                              ; B a pure polynomial in i
   (lcaccsum gfn N i)                                                                          ; G[N]-G[i] source, wrapped ; () declines
    (: d (lcdeg gfn 8) (? (d < 0) () (: cks (lclead (lcsamp gfn d)) [['- (lcsum cks N) (lcsum cks i)]])))
   (lcaccrw name val)                                                                          ; the accumulator binding -> its closed value
    (: I <>name A <>>name T (opd val 0)
       N (opd T 1) B (opd (opd (opd val 1) 1) 1)                                               ; THEN=(NM (i + 1) (a + B)) -> ACC=(a + B) -> B
       cs (lcaccsum (base-ev ['\ I B]) N I)
       (? (atom? cs) val ['+ A ['? T <cs 0]]))                                           ; (a + (? (i < N) (G[N]-G[i]) 0))
   (lc-bs bl) (? (atom? bl) bl  (atom? >bl) [(lcwalk <bl)]                           ; trailing body
                 (<bl >< ((? (lcacc? <bl <>bl) (lcaccrw <bl <>bl) (lcwalk <>bl)) >< lc-bs >>bl)))
   (lcwalk x) (? (atom? x) x (lcquote? x) x
                 (lc:form? x) (': >< lc-bs >x)                                         ; walk `:` bindings (accumulators + recurse)
                 (: c (? (lcfold? x) (lcclose1 x) ())
                    (? (atom? c) (<x >< map lcwalk >x) <c)))               ; () decline -> recurse; (src) -> the residual
   (lc-bs-has? bl) (? (atom? bl) 0  (atom? >bl) (lc-has-acc? <bl)
                      (lcacc? <bl <>bl || (lc-has-acc? <>bl || lc-bs-has? >>bl)))
   (lc-has-acc? x) (? (atom? x) 0 (lcquote? x) 0 (lc:form? x) (lc-bs-has? >x) (any lc-has-acc? >x))
   (lc-has-fold? x) (? (atom? x) 0 (lcquote? x) 0 (lcfold? x) 1 (any lc-has-fold? >x))
   (loopclose form) (? (lc-has-fold? form || lc-has-acc? form) (lcwalk form) form)          ; byte-identical when nothing closeable
   (clook2 nm al) (? (atom? al) -1 (id? nm <<al) <><al (clook2 nm >al))   ; (name arity) alist -> arity (-1 absent)
   (cmlook nm cm) (? (atom? cm) () (id? nm <<cm) ><cm (cmlook nm >cm))          ; (name . chain-param-INDEX-list) -> indices (() absent)
   ; cok? -- e is CHAIN-valued (a raw chain word, NOT an untagged int): a chain param (in cps) or cap/cup
   ; of a chain-valued expr. A chain word MUST NOT reach an arith/cmp operand (the body would mistreat it
   ; as an int -- the <t stores putcharm(v), not v); it may only flow to two?/cap/cup or a callee's
   ; CHAIN slot. ck stays sound because <t/>t feed only the recursive ck (a chain param).
   (cok? cm cands cps ps e) (? (member? e cps) 1
                               (two? e && member? <e '(cap cup) && tally e = 2) (cok? cm cands cps ps <>e)
                               0)
   ; pwcar? -- w is a raw chain-CAR word: <CE/>CE over a chain-valued operand. The one
   ; place such a word may reach a compare: the POINT-= production below (raw word identity).
   (pwcar? cm cands cps ps w) (two? w && member? <w '(cap cup) && 2 = (tally w) && cok? cm cands cps ps <>w)
   (gargok? cm pm cands cps pps ps as i callee) (? (atom? as) 1                                ; call args: slot i chain (per callee's cmap) -> cok?; point (per pm) -> a point param; else int
      ((? (member? i (cmlook callee cm)) (cok? cm cands cps ps <as)
             (member? i (cmlook callee pm)) (member? <as pps)                             ; a point slot takes a point param, nothing else (the guard is at the ENTRY only)
             (groupok? cm pm cands cps pps ps <as))
       && (gargok? cm pm cands cps pps ps >as (i + 1) callee)))
   (groupok? cm pm cands cps pps ps e)                                                        ; INT-valued e over cands; cps/pps = the current fn's chain-/point-param NAMES
    (? (member? e cps) 0                                                                       ; a chain param is NOT int-valued (only valid via cok?)
       (member? e pps) 0                                                                       ; a point param neither (raw word: only the point-= and point slots)
       (member? e ps) 1  (whole? e) 1
       (two? e) (: op <e ar (clook2 op cands)
          (? (-1 < ar)                                  ((tally >e) = ar && gargok? cm pm cands cps pps ps >e 0 op)   ; group call: arity ok, each arg ok for its slot's kind
             ((id? op '=) && ((3 = (tally e))                                              ; POINT-=: (P = <..)/(<.. = P) -- word identity, int 0/1.
              && (: x1 <>e y1 <>>e                                             ;   TOTAL because P is entry-guarded nom|mint and a point equals only
                    ((member? x1 pps && pwcar? cm cands cps ps y1)                     ;   itself, whatever kind the car is (charm keys deopt at the entry --
                     || (member? y1 pps && pwcar? cm cands cps ps x1))))) 1               ;   only interp knows the numeric bridges, (2 = 2.0))
             (member? op '(+ - * // % & | ^ < <= > >= =)) (3 = (tally e) && groupok? cm pm cands cps pps ps <>e && groupok? cm pm cands cps pps ps <>>e)
             (id? op '?)                                  (4 = (tally e) && groupok? cm pm cands cps pps ps <>e && groupok? cm pm cands cps pps ps <>>e && groupok? cm pm cands cps pps ps <>>>e)
             (id? op 'tally)                              (2 = (tally e) && member? <>e ps)                                          ; (tally STR): STR a param (string lane)
             (id? op 'peep)                               (4 = (tally e) && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && whole? <>>>e)   ; (peep STR i d): STR param, i ok, d const
             (id? op 'mpeep)                              (4 = (tally e) && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && whole? <>>>e)   ; (mpeep MAP k d): MAP param, key ok, d const (the map lane -- mapfix rewrote a map-valued peep here)
             (id? op 'mpin)                               (4 = (tally e) && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && groupok? cm pm cands cps pps ps <>>>e)   ; (mpin MAP k v): MAP param, key ok, v ok -- UPDATE-in-place; produces the MAP (threaded into a call's map slot, or a discarded result)
             (id? op 'pin)                                (4 = (tally e) && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && groupok? cm pm cands cps pps ps <>>>e)   ; (pin BUF i V): BUF a raw-buf param, i ok, V ok
             (id? op 'two?)                               (2 = (tally e) && cok? cm cands cps ps <>e)               ; (two? CE): CE chain-valued; result int 0/1 (TOTAL -- a non-chain reads false)
             0))                                                                               ; bare cap/cup are CHAIN-valued -> not admitted in an int position (cok? and the point-= carry them)
       0)
   ; --- STAGE D: vok? -- the VALUE grammar (a heap-allocating fn's body, e.g. tree's `mk`). An expr is
   ;     VALUE-valued if it is: an int const (-> the tagged value) | a param (cggvir re-tags it) | (link V V)
   ;     (a cons; both operands value-ok) | (? INT V V) (int test via groupok?, value branches) | (g E..)
   ;     a call to a VALUE-typed candidate (vcands; args int/chain-ok via gargok?, as for an int call).
   ;     CROSS-CALL SOUNDNESS by candidate-set SEPARATION: vok?'s calls resolve only in `vcands`,
   ;     groupok?'s only in `icands` -- so an int fn can't call a value fn (not in its set) and vice
   ;     versa, with NO change to groupok?. The value fns glaze via cggvir + the value-epilogue (emit.l).
   (vok? cm pm icands vcands cps pps ps e)
    (? (whole? e) 1
       (member? e cps) 0  (member? e pps) 0                                                   ; raw-word params (chain/point) are NOT values: cggvir would re-tag them
       (member? e ps) 1
       (two? e) (: op <e ar (clook2 op vcands)
          (? (-1 < ar)         ((tally >e) = ar && gargok? cm pm icands cps pps ps >e 0 op)   ; a VALUE-group call (target in vcands)
             (id? op 'link)    (3 = (tally e) && vok? cm pm icands vcands cps pps ps <>e && vok? cm pm icands vcands cps pps ps <>>e)
             (id? op '?)       (4 = (tally e) && groupok? cm pm icands cps pps ps <>e && vok? cm pm icands vcands cps pps ps <>>e && vok? cm pm icands vcands cps pps ps <>>>e)
             0))
       0)
   (chps body ps) (filter (\ k (cuse? body (nthl ps k))) (rangeL 0 (tally ps)))                ; chain-param INDICES: params used in cap/cup/two? (cuse? from emit.l)
   (grpok-1 fns cm pm icands vcands vnames c) (: p (afind (\ q (id? <<q <c)) fns)
                                cps (map (\ k (nthl ><p k)) (cmlook <c cm))      ; this fn's chain-param NAMES (its index list -> names)
                                pps (map (\ k (nthl ><p k)) (cmlook <c pm))      ; ... and its point-param NAMES (same alist shape)
                                (? (member? <c vnames) (vok? cm pm icands vcands cps pps ><p >p)   ; a VALUE fn -> value grammar
                                   (groupok? cm pm icands cps pps ><p >p)))      ; an INT fn -> int grammar (over INT candidates only)
   ; the group-membership fixpoint as a DESCENDING projection of the shared `monofix` (prel): each step
   ; drops candidates that fail the int/value grammar, recomputing the int/value split from the surviving
   ; set; tally (shrinking) is the measure. Converges when a pass drops nothing -- the closed glazeable subset.
   (gfix fns cm pm vnames cands)
    (monofix cands
       (\ st (: icands (filter (\ c !(member? <c vnames)) st)
                vcands (filter (\ c (member? <c vnames)) st)
                (filter (\ c (grpok-1 fns cm pm icands vcands vnames c)) st)))
       tally)
   (grpfix fns cands) (: cm (map (\ f (<<f >< chps >f ><f)) fns)   ; (name . chain-param-index-list) for every candidate fn
                         pm (map (\ f (: fps ><f cix (chps >f fps)              ; (name . point-param-index-list): point-compared ((P = <..)), never chain-used
                                         (<<f >< filter (\ k (nuse? >f (nthl fps k) && !(member? k cix))) (rangeL 0 (tally fps))))) fns)
                         vnames (map (\ f <<f) (filter (\ f (hasl >f)) fns))    ; the VALUE-typed candidate names (body conses)
                         (gfix fns cm pm vnames cands))
   (rewrite-bindings bl gnames gdefs) (? (atom? >bl) bl                                  ; the trailing body form -> keep
      (: nf <bl val <>bl  hit (two? nf && member? <nf gnames)
         ((? hit <nf nf)                                                            ; drop the (name p..) define-sugar -> a PLAIN binding
            >< ((? hit (? (jgir-ok? gdefs) (jitgroupir gdefs <nf arch) val) val)   ; jgir-ok? -> the neutral-IR group lane (no hand offsets, both targets); else interp (jgir-supported spans the recognizer grammar, so this fallback is a soundness backstop, not a live lane)
                  >< (rewrite-bindings >>bl gnames gdefs)))))
   ; memo-leak? -- a candidate fn FREELY reads/writes a `:`-sibling MAP (a memo tablet/hash bound in the
   ; same form, e.g. fact/bell's global `facts`/`bells`): the native group lane can only touch a map that
   ; is one of the fn's own PARAMS (mpeep/mpin), so a non-param map is mishandled -> a SILENT miscompile
   ; (fact jitted to a constant 1). DECLINE -> interp, which is correct. The map-PARAM lane (a map threaded
   ; as a call arg, read via the param) is unaffected: the binding name never occurs inside the callee.
   (memo-leak? form)
    (: ps <(anat-defs >form)
       mn (filter (\ p (atom? <p && two? >p && member? <>p '(tablet hash))) ps)   ; names bound to a tablet/hash
       !(atom? (filter (\ p ((two? <p)                                                    ; a fn binding whose body ...
       && !(atom? (filter (\ m (occurs? <m >p && !(member? <m ><p))) mn)))) ps)))   ; ... references a mapname that is NOT its param
   ; --- GROUP SWALLOW (always-on) ---------------------------------------------------------------
   ;     A group member calling an EXTERNAL global int fn used to REJECT (the callee isn't a
   ;     `:`-local candidate -> the whole group falls to interp). srcrec records every top-level
   ;     GLOBAL fn define's source (a body-less `:` leaks its bindings globally -- that parity is the
   ;     "this is a real global" test, so srcreg only ever holds genuine globals, never a bench's
   ;     local fn). swgather then pulls a called global's recorded source INTO the candidate set as a
   ;     synthetic member, to a fixpoint (a swallowed fn may call further globals). jitgroupir emits
   ;     it as an in-blob `(label g)` sibling under the group's ONE shared OVF/deopt -- so NO
   ;     cross-blob call, no split deopt. SOUND: the recorded source == the live global (autogroup
   ;     sees every define), so the native agrees with the interp twin (which calls the global); a
   ;     swallowed body over any free var / unknown call is rejected by groupok?'s existing grammar
   ;     -> the swallow declines, never miscompiles. (Staleness: a global redefined AFTER a caller
   ;     was glazed+memoized keeps the caller's cached native, as any memoized glaze does. The
   ;     recorded source is lowered through autogroup's FULL chain, so church/HOF/&& AND STRUCTURAL
   ;     callees swallow: a named-let helper lifts to `helper` + its loop fn, a value-let inlines, a
   ;     poly-fold closes. lift names a lifted fn `F_g` (content-derived, auto.l ~line 280), NOT a
   ;     counter/gensym -- collision-free here because a name is EITHER a `:`-local candidate OR a
   ;     swallowed global (swgather's member? split), never both, so each `F_` namespace is owned once.
   ;     The chain appends a `()` body (body-less -> body-having); anat-defs drops it, the fn pairs
   ;     (helper + every F_g sub-fn) are recorded and swgather pulls them transitively.
   srcreg (tablet 0)
   (swfnpair? pr) (: nf <pr (two? nf && 1 < (tally nf) && (tally nf) < 6 && all name? >nf && distinctp >nf))
   (srcrec form0) (? ((two? form0) && (id? <form0 ':) && !(atom? >form0)
                    && (0 = ((tally >form0) % 2)))                                   ; body-less `:` only -> a GLOBAL-leaking define
      (: low (plift (lift (delet (defoliate (loopclose (autospec (debool (dehof (dechurch (fold-consts form0))))))))))   ; autogroup's FULL lowering: church/HOF/&& + structural (lift/loops/value-lets)
         (foldl (\ _ pr (? (swfnpair? pr) (pin srcreg <<pr (><pr >< >pr)) ())) () <(anat-defs >low))) ())
   (swuniq l) (? (atom? l) () (member? <l >l) (swuniq >l) (<l >< swuniq >l))
   (swbnds e) (map (\ p (? (two? <p) <<p <p)) <(anat-defs >e))                       ; a `:`'s binding names
   ; collect call heads (symbols); skip lambda subforms. the walk reads OPFIXED source, so an
   ; infix (a op b) arrives factored and an operand never masquerades as a head. bnd is the
   ; binder set that filters real-but-local heads: `:` names + params (a `\` subform is
   ; skipped whole, so its params never leak).
   (swwalk e bnd acc) (? (atom? e) acc  (two? e && id? <e '\) acc
      (two? e && id? <e ':) (foldl (\ a x (swwalk x (swbnds e + bnd) a)) acc >e)
      (: h <e acc2 (? (name? h && !(member? h bnd)) (h >< acc) acc)
         (foldl (\ a x (swwalk x bnd a)) acc2 >e)))
   (swheads e bnd acc) (swwalk (opfix e) bnd acc)
   (swgather fns) (: names (map caap fns)                                                 ; enlarge fns with recorded globals reachable from the bodies (to fixpoint)
      heads (foldl (\ a p (swheads >p ><p a)) () fns)                                ; the fn's own params bind too
      new (swuniq (filter (\ h (!(member? h names) && !(id? (peep srcreg h 0) 0))) heads))
      (? (atom? new) fns
         (swgather (foldr (\ h a (: d (peep srcreg h 0) (((h >< <d) >< >d) >< a))) fns new))))
   (autogroup form0)
    (: _ (srcrec form0)                                                                  ; record global fn sources (flag-gated inside srcrec) BEFORE any decline
     (? !(two? form0) form0
       !(id? <form0 ':) form0
       (atom? >form0) form0
       ; DECLINE a BODY-LESS `:` (even # of forms) -- the global-leaking top-level-define form (sat.l, the
       ; corpus, apps). The lift pipeline rebuilds the `:` and can't faithfully preserve the leak (it
       ; appends a () body -> the natives go globally UNBOUND -> (() x)=const-1=1, a SILENT miscompile),
       ; and the group codegen is unverified in that context. Interp is correct; the benches that matter
       ; (fib/tak/primes) are body-HAVING (they call their fns locally inside the bench `:`) so they still
       ; glaze. Sound > fast: never hand back a wrong answer. (The real cure is explicit global-binding
       ; syntax instead of the parity-of-`:` leak convention; until then, decline.)
       (0 = ((tally >form0) % 2)) form0
       (memo-leak? form0) form0                                                                ; a `:`-sibling map memo (fact/bell) -> interp, sound
       (: form (plift (lift (delet (defoliate (loopclose (autospec (debool (dehof (dechurch (fold-consts form0))))))))))   ; lower literal church (N f x)->f^N(x), inline lambda-const HOFs to first-order, lower &&/||/min/max/abs to ?, specialize interp-over-static-data, CLOSE polynomial folds to a closed form, deforest the rest -> loops, inline value-lets, lambda-lift named-let bodies, THEN plift read-only loops out of nested lets (hash's scan) -> flat group fns
          parsed (anat-defs >form) pairs <parsed
          fns0 (filter (\ p (: nf <p (two? nf && 1 < (tally nf) && (tally nf) < 6 && all name? >nf && distinctp >nf))) pairs)   ; (name p..), 1..4 distinct sym params
          fns (swgather fns0)                                                              ; swallow: pull called globals' recorded sources in as synthetic members
          (? !(two? fns) form0                                                            ; no candidate functions -> original
             (: armap (map (\ p [<<p ((tally <p) - 1)]) fns)                  ; (name arity) candidates
                grp (grpfix fns armap)                                                        ; the closed glazeable subset
                (? (atom? grp) form0                                                          ; nothing survives the fixpoint -> original (un-lifted)
                   ; a group of SWALLOWED members only rewrites nothing here, so committing the
                   ; lowered form buys nothing and stakes the whole file on it.
                   !(ho-any (\ p (member? (? (two? <p) <<p <p) (map cap grp))) pairs) form0
                   (: gnames (map cap grp)
                      gdefs (map mapfix-def (filter (\ p (member? <<p gnames)) fns))   ; the group's (nameform . body) defs, with map-valued peeps rewritten to mpeep
                      (': >< rewrite-bindings >form gnames gdefs))))))))             ; each binding of the LIFTED form -> its native  (last ) closes the srcrec `:` wrapper)
   ; --- TWIN LOWERING (twolow): a complex(TWIN)-recurrence grid -> the real-pair float grid the
   ;     float-grid lane (autonat/jitfgridn) ALREADY glazes -- so idiomatic complex code glazes with
   ;     NO new codegen (Route A). A `:` whose recurrence carries TWIN params (a var built/updated
   ;     with (re ~ im), `*`, `+`, `-`, read with `re`/`im`) is lowered: each twin param p splits into
   ;     (p_r p_i), twin ops become real-pair arithmetic ((a+bi)(c+di) = (ac-bd, ad+bc)), and every
   ;     call site's twin args split in two. A small twin-TYPE fixpoint (propagate twin-ness from
   ;     call-site args to callee params) picks which params are twin. BIT-EXACT: z*z's imag rides
   ;     re*im+im*re = 2*re*im, and scaling a rounded product by 2 is exact, so the lowered floats
   ;     match the interpreted twins to the bit. Returns the form UNCHANGED with no twin param (every
   ;     non-twin form passes through). Runs BEFORE autonat in the chain. ⚠ x86-64 (-> autonat).
   (twolow form)
    (: (nthl2 l i) (? (i = 0) <l (nthl2 >l (i - 1)))
       (op2 e i)  (nthl2 >e i)
       (memq x l) (? (atom? l) 0 (id? <l x) 1 (memq x >l))
       (negf x)   ['- 0.0 x]
       (zero0? x) (gem? x && x = 0.0)
       (mkadd a b) (? (zero0? a) b (zero0? b) a ['+ a b])
       (mksub a b) (? (zero0? b) a ['- a b])
       (mkmul a b) (? (zero0? a || zero0? b) 0.0 ['* a b])
       (adefs bs) (? (atom? >bs) (() >< <bs)                          ; binding list -> (pairs . body)
                     (: r (adefs >>bs) (((<bs >< <>bs) >< <r) >< >r)))
       (fnpair? pr) (two? <pr)                                               ; nameform is a (name params..) list
       (fnname pr) <<pr  (fnparams pr) ><pr  (fnbody pr) >pr
       (twname p suf) (intern ((string p) + suf))                                ; p_r / p_i
       (? !(two? form && id? <form ':) form
          (atom? >form) form
          (: parsed (adefs >form) pairs <parsed body >parsed
             (? !(all fnpair? pairs) form                                      ; only the all-function-def `:` shape
                (: fnames (map fnname pairs)
                   pt (tablet 0) chg (tablet 0)                                   ; `(fn i) -> 1 if param i of fn is twin
                   ; --- twin-type inference: is e twin under env (param-name -> twin bool)? ---
                   (tty e env) (? (two? e)
                                    (: op <e
                                       (? (id? op 'twin) 1
                                          (id? op 're || id? op 'im) 0
                                          (memq op '(+ - *)) (tty (op2 e 0) env || tty (op2 e 1) env)
                                          (id? op '?) (tty (op2 e 1) env || tty (op2 e 2) env)
                                          0))                                      ; a defined-fn call returns int here
                                  (peep env e 0))
                   (mkenv fn ps) (: e (tablet 0) _ (mke fn ps 0 e) e)
                   (mke fn ps i e) (? (atom? ps) e (: _ (pin e <ps (peep pt [fn i] 0)) (mke fn >ps (i + 1) e)))
                   (propargs g as i env) (? (atom? as) 0                          ; call-site arg twin-ness -> callee param
                      (: _ (? (memq g fnames && tty <as env && !(peep pt [g i] 0)) (: _ (pin pt [g i] 1) (pin chg 0 1)) 0)
                         (propargs g >as (i + 1) env)))
                   (walkprop env e) (? !(two? e) 0
                      (: op <e _ (? (memq op fnames) (propargs op >e 0 env) 0) (: _ (map (\ a (walkprop env a)) >e) 0)))
                   (infer _) (: _ (pin chg 0 0)                                   ; fixpoint (monotone: flags only flip 0->1)
                               _ (map (\ pr (walkprop (mkenv (fnname pr) (fnparams pr)) (fnbody pr))) pairs)
                               _ (walkprop (tablet 0) body)
                               (? (peep chg 0 0) (infer ()) 0))
                   _ (infer ())
                   (anytwin? prs) (? (atom? prs) 0 (twfn? (fnname <prs) 0 || anytwin? >prs))
                   (twfn? fn i) (? (peep pt [fn i] 0) 1 (i < 8) (twfn? fn (i + 1)) 0)
                   (? !(anytwin? pairs) form                                   ; no twin param -> pass through
                      ; --- lowering: loR scalar-valued ; loT twin-valued -> (re-expr . im-expr) ---
                      (: (mknmap fn ps) (: m (tablet 0) _ (mkn fn ps 0 m) m)      ; twin param -> (p_r . p_i) pair
                         (mkn fn ps i m) (? (atom? ps) m (: _ (? (peep pt [fn i] 0) (pin m <ps (twname <ps "_r" >< twname <ps "_i")) 0) (mkn fn >ps (i + 1) m)))
                         (flatps fn ps i) (? (atom? ps) ()                        ; flatten params, splitting twins
                            (: p <ps rest (flatps fn >ps (i + 1))
                               (? (peep pt [fn i] 0) (twname p "_r" >< (twname p "_i" >< rest)) (p >< rest))))
                         (loR e env nm) (? (atom? e) e
                            (: op <e
                               (? (id? op 're) (: x (op2 e 0) (? (tty x env) <(loT x env nm) (loR x env nm)))
                                  (id? op 'im) (: x (op2 e 0) (? (tty x env) >(loT x env nm) 0.0))
                                  (memq op fnames) (op >< splitargs op >e 0 env nm)
                                  (op >< map (\ a (loR a env nm)) >e))))
                         (loT2 e env nm) (? (tty e env) (loT e env nm) (loR e env nm >< 0.0))
                         (loT e env nm) (? (atom? e) (peep nm e (e >< e))
                            (: op <e
                               (? (id? op 'twin) (? (1 < (tally >e)) (loR (op2 e 0) env nm >< loR (op2 e 1) env nm)
                                                    (: x (op2 e 0) (? (tty x env) (: p (loT x env nm) (<p >< negf >p)) (loR x env nm >< 0.0))))
                                  (id? op '+) (: a (loT2 (op2 e 0) env nm) b (loT2 (op2 e 1) env nm) (mkadd <a <b >< mkadd >a >b))
                                  (id? op '-) (: a (loT2 (op2 e 0) env nm) b (loT2 (op2 e 1) env nm) (mksub <a <b >< mksub >a >b))
                                  (id? op '*) (: a (loT2 (op2 e 0) env nm)
                                                 (? ((op2 e 0) = (op2 e 1))                                       ; SQUARING (z*z): im = 2*ar*love, ONE product (not ar*love + love*ar); also better ILP than the M+M chain
                                                    (: ar <a love >a (mksub (mkmul ar ar) (mkmul love love) >< mkmul (mkmul 2.0 ar) love))
                                                    (: b (loT2 (op2 e 1) env nm) ar <a love >a br <b bi >b
                                                       (mksub (mkmul ar br) (mkmul love bi) >< mkadd (mkmul ar bi) (mkmul love br)))))
                                  (id? op '?) (: c (loR (op2 e 0) env nm) a (loT (op2 e 1) env nm) b (loT (op2 e 2) env nm)
                                                 (['? c <a <b] >< ['? c >a >b]))
                                  (e >< e))))
                         (splitargs g as i env nm) (? (atom? as) ()
                            (: a <as rest (splitargs g >as (i + 1) env nm)
                               (? (peep pt [g i] 0) (: p (loT2 a env nm) (<p >< (>p >< rest))) (loR a env nm >< rest))))
                         (lowerfn pr) (: fn (fnname pr) ps (fnparams pr) bd (fnbody pr) env (mkenv fn ps) nm (mknmap fn ps)
                                         ((fn >< flatps fn ps 0) >< loR bd env nm))
                         (rebuild lowfns) (? (atom? lowfns) (loR body (tablet 0) (tablet 0) >< ())
                            (: lf <lowfns (<lf >< (>lf >< rebuild >lowfns))))
                         (': >< rebuild (map lowerfn pairs)))))))))
   (autonat form)
    (? !(two? form) form
       !(id? <form ':) form
       (atom? >form) form
       (: parsed (anat-defs >form) pairs <parsed
          (? !(1 < (tally pairs)) form                        ; need a recurrence + >=1 loop
             (: recp (afind anat-rec? pairs)
                (? !(two? recp) form                          ; no float recurrence found
                   (: recnm <<recp recps ><recp recbody >recp
                      cond (opd recbody 0) then (opd recbody 1)
                      itcmp (opd cond 0) c2 (opd cond 1)
                      (? !(two? itcmp && id? <itcmp '< && two? c2) form     ; cond = (&& (it < LIM) (cmp T B))
                         (: lim (opd itcmp 1) cmpop <c2 testexpr (opd c2 0) bound (opd c2 1)
                            cl (classify recps >then) fvars <cl updates <>cl invs (nthl cl 2)
                            looppairs (filter (\ q !(id? q recp)) pairs)             ; the loop defs
                            callees (map anat-callee looppairs)
                            outerp (afind (\ q !(member? <<q callees)) looppairs)   ; the loop no loop calls
                            (? !(two? outerp) form
                               (: chain (loopchain <<outerp pairs recnm)       ; outermost -> recurrence
                                  D (tally chain) M (tally invs) N (tally fvars)
                                  (? !(two? chain && member? cmpop '(< <= > >=) && 0 < N && (M + N) < 7 && D < 7) form
                                     (: cnames (map cap chain) dims (map caup chain)
                                        margs (nthl (last chain) 2)
                                        invd (map (\ iv (nthl margs (posof iv recps 0))) invs)
                                        glam ['\ '_ form]        ; faithful interp nest (deopt fallback / =-show)
                                        ng (jitfgridn glam cnames dims invs invd fvars updates testexpr cmpop bound lim arch)
                                        (? (lit? ng) (': >< anat-rebind >form <<outerp ng) form)))))))))))))
   ; COMPILE CACHE: each source \-expr compiles ONCE (no re-mmap on repeated ev).
   ; Keyed by source -- structurally-equal closures share the same native (sound:
   ; they are =). The transparent form of a hot-trigger: an execution-count trigger
   ; would need a counting wrapper, but a wrapper is not =/show its source; so the
   ; trigger is "compile on first request, cached after". Loops stay eager (their
   ; one application is internally hot). ⚠ pins natives (W^X + arenas) for the run.
   cache (tablet 0)
   (memo e mk) (: g (peep cache e 0) (? g g (: h (mk e) _ (pin cache e h) h)))
   ; auto-ev: a WHOLE-FORM lambda -> native (cached), else the base interpreter. (The
   ; creation hook -- rewriting embedded lambdas via a jit-walk -- was tried and pulled:
   ; it's pre-macroexpansion so it can disturb a macro's data args, AND the
   ; compile cache makes structurally-equal closures id?-IDENTICAL, breaking love's
   ; equal-but-distinct closure semantics (!(id? (\ x E) (\ x E))). A safe creation hook
   ; needs the POST-macro opfix seam + a CODE cache (distinct cells per instance). Future.)
   ; the lanes route to `arch` (the assembler target). on x86 (arch='x64) the FULL glaze runs. on arm64 the
   ; INTEGER lanes (leaf/n-ary/closure, counted loops, the mutually-recursive arith GROUP) AND the scalar
   ; FLOAT-result leaf (jitfr) are all live -- arch-parameterized and validated under qemu (`make
   ; test_arm64`, which builds tco=1 to match the native ABI: Ip/Hp/Sp in x1/x2/x3, the threaded Continue).
   ; Only the float-GRID lane (autonat/jitfgridn, packed SSE2) and castbuild's cask-fill stay x86-only, so
   ; on arm64 they fall to the interpreter (autogroup carries every source rewrite that lowers to the
   ; integer group, so fib/tak/primes still glaze).
   (auto-ev e) (? !x86? (? (qual e)  (memo e (\ l (jitir l arch)))        ; arm64: integer + scalar-float lanes native
                               (qualn e) (memo e (\ l (jitnir l arch)))
                               (qualc e) (memo e (\ l (jitnir (flatc l) arch)))
                               (qualfr e) (memo e (\ l (jitfr l arch)))      ; float-RESULT leaf -> heap-boxed gem
                               (: li (loopinfo e)                            ; counted loops (arch-aware loopcode)
                                  (? li (memo e (\ l (njit-loop l <li <>li <>>li <>>>li (base-ev l))))
                                     (: ln (loopinfo-n e)
                                        (? ln (memo e (\ l (njit-loop-n l <ln <>ln <>>ln <>>>ln <>>>>ln (base-ev l))))
                                           (base-ev (autogroup (autonat (twolow e)))))))))   ; float-GRID nest (scalar gridn, arch-aware) then group; castbuild (cask-fill) is x86-only so skipped
                  (qual e)  (memo e (\ l (jitir l arch)))
                  (qualn e) (memo e (\ l (jitnir l arch)))
                  (qualc e) (memo e (\ l (jitnir (flatc l) arch)))
                  (qualfr e) (memo e (\ l (jitfr l arch)))         ; float-RESULT leaf -> heap-boxed gem (sound: param floated at first use)
                  (: li (loopinfo e)
                     (? li (memo e (\ l (njit-loop l <li <>li <>>li <>>>li (base-ev l))))
                        (: ln (loopinfo-n e)
                           (? ln (memo e (\ l (njit-loop-n l <ln <>ln <>>ln <>>>ln <>>>>ln (base-ev l))))
                              (base-ev (autogroup (autonat (twolow (castbuild e))))))))))    ; fall-through: cask-build a string-accumulator loop (the jug threads as a loop param -> survives autogroup's pure passes), LOWER a complex(twin) grid to real-pairs (twolow), then glaze a float-grid nest / mutually-recursive arith group if either fits, else interpret
   ev (? glaze-loaded? ev                                      ; the auto-native ev, under the module's own name: `ev` is ORTH, which a module layer cannot write, so the BOOT installs it -- (: ev (from 'glaze 'ev)) -- exactly as it does overlay's hook. On a RE-LOAD (glaze-loaded?) this reads the baked auto-ev and hands it straight back: it captured the correct baked helpers, so double-loading is a no-op, not a crash.
         (0 < love-tco) auto-ev
         ev)                                                   ; a TRAMPOLINE build (love-tco 0): glazed code Continues by tail-jump, which only the threaded VM honors -- stay the pure interpreter (the module still installs for probes)
)

; --- re-declare member? through the glazed compiler: the recognizer's POINT-= grammar
; ((P = <..) over an entry-guarded nom|mint key) closes the singleton group, so the
; prel walker (egg-baked pre-glaze, interp) is re-stated here and autogroup hands back
; the native -- mooncc walks 4.27M calls / 17.5M elements through it per love.c compile.
; Post-boot streams (the mooncc cat, the corpus) fold the native; egg-baked consumers keep
; the interp closure they already folded. Mirrors the ev rebind's gates: a re-load keeps
; the bake, a trampoline (tco=0) build stays interp. Charm keys deopt at the entry guard
; (only interp knows the numeric bridges, (2 = 2.0)); string keys likewise (structural =).
; Like `ev` above this is ORTH's name, so it lands here as a module member and the BOOT
; installs it: (: member? (from 'glaze 'member?)). Two forms: memb0 must read the OUTER
; member? (one scope would read the rebind before its pin -- the missing condition).
(: memb0 member?)
(: member? (? glaze-loaded? memb0 (0 < love-tco)
              (ev '(: (member? x l) (? (two? l) (? (= x (cap l)) 1 (member? x (cup l))) 0) member?))
              memb0))
