; ============================================================================
; overlay-matrix -- RUNNABLE dependent-types showcase. ONE matrix domain handles BOTH
; concrete dims (uu computes: 3+4=7) AND polymorphic dims (variables, symbolic + threaded).
; Run: cat test/uu.l doc/misc/proto/overlay-lib.l doc/misc/proto/overlay-matrix.l | out/host/love /dev/stdin
;
;   matmul  : (mat m n) (mat n p) -> (mat m p)          inner dims match (unify), thread
;   hconcat : (mat r a) (mat r b) -> (mat r (add a b))  rows match (unify), cols ADD
;
; The dims may be numbers (checked by uu computation) OR variables (checked structurally,
; polymorphic over ALL compatible shapes). The `okr` normalizes a dim with uu iff it is
; CLOSED (no variables); an open dim stays symbolic. Primitives declared once with `sig`,
; user code with `deft` (checked). See memory pluggable-types.
; ============================================================================

; --- FORM 1: real matrix ops on lists-of-rows ---
(: (rdot a b)     (? (atom? a) 0 (+ (* (cap a) (cap b)) (rdot (cup a) (cup b))))
   (transpose m)  (? (atom? (cap m)) () (link (map cap m) (transpose (map cup m))))
   (matmul a b)   (: bt (transpose b) (map (\ ra (map (\ bc (rdot ra bc)) bt)) a))
   (hconcat a b)  (? (atom? a) () (link (cat (cap a) (cap b)) (hconcat (cup a) (cup b)))))

; --- FORM 2: the unified matrix domain (closed dims -> uu; open dims -> symbolic) ---
(: (closed? d)  (? (nom? d) (member? d '(add succ zero))    ; a symbol: a ctor is closed, a VARIABLE is open
                    (atom? d) 1                              ; a number / () : closed
                    (&& (closed? (cap d)) (closed? (cup d))))
   (dim-norm d) (? (closed? d) (vof () d) d)                ; closed -> uu computes; open -> leave symbolic
   mat-dims (\ d (? (&& (two? d) (id? (cap d) 'mat) (= 3 (tally d)))
                    (L 'mat (dim-norm (cap (cup d))) (dim-norm (cap (cup (cup d))))) d))
   mat-dom  {'dice () 'ctors '(mat add succ zero)
             'join (\ a b (? (= (mat-dims a) (mat-dims b)) a 'top))
             'okr  (\ bd decl (= (mat-dims bd) (mat-dims decl)))
             'litdie (\ e 'top) 'tr (\ op a b 'top)}
   _ (overlay-set mat-dom ()))

; --- FORM 3: declare the primitives' types once ---
(sig matmul  ((mat m n) (mat n p)) (mat m p))
(sig hconcat ((mat r a) (mat r b)) (mat r (add a b)))

; --- FORM 4: user code -- CONCRETE and POLYMORPHIC, all checked at compile time ---
(: _ (deft (prod  (a (mat 2 3)) (b (mat 3 2)))              (mat 2 2)         (matmul a b))            ; concrete ok
   _ (deft (side  (a (mat 2 3)) (b (mat 2 1)))              (mat 2 4)         (hconcat a b))           ; concrete, uu: 3+1=4
   _ (deft (chain (a (mat m n)) (b (mat n p)) (c (mat p q))) (mat m q)        (matmul (matmul a b) c)) ; POLYMORPHIC matmul
   _ (deft (grow  (a (mat r x)) (b (mat r y)))              (mat r (add x y)) (hconcat a b))           ; POLYMORPHIC hconcat
   _ (deft (oops  (a (mat 2 3)) (b (mat 4 2)))              (mat 2 2)         (matmul a b))            ; ERR inner 3 vs 4
   _ (deft (bad   (a (mat m n)) (b (mat p q)))              (mat m q)         (matmul a b))            ; ERR n vs p (variables)
   0)

; --- FORM 5: run the checked functions on real matrices ---
(: A '((1 2 3) (4 5 6))  B '((1 0) (0 1) (1 1))  C '((1 2 0 1) (0 1 1 0))  D '((7) (8))
   _ (puts "prod  A B      = ") _ (puts (show (prod A B)))       _ (putc 10)
   _ (puts "side  A D      = ") _ (puts (show (side A D)))       _ (putc 10)
   _ (puts "chain A B C    = ") _ (puts (show (chain A B C)))    _ (putc 10)   ; polymorphic, on 2x3.3x2.2x4
   _ (puts "grow  A D      = ") _ (puts (show (grow A D)))       _ (putc 10)   ; polymorphic hconcat
   0)
