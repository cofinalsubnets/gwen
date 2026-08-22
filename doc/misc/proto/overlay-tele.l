; ============================================================================
; overlay x tele -- STATIC TENSOR-SHAPE CHECKING over the autograd library.
; the thing PyTorch can't give you: a shape mismatch caught at COMPILE time, per
; top-level def -- yet the checker is OBSERVE-ONLY and ERASABLE, so the network
; still runs (and differentiates) exactly as it did before the overlay.
;
;   Run:  cat crew/tele/tele.l doc/misc/proto/overlay-lib.l doc/misc/proto/overlay-tele.l | out/host/love
;
; tele's ops are already shape-relational -- mm contracts, transp2 flips, sum
; reduces to a scalar. we just NAME those relations as sigs and let the overlay's
; unifier check them. the die language: a rank-2 tensor is `(mat R C)`, a scalar is
; `scalar`; R/C are dims -- a NUMBER (concrete, checked by equality) or a VARIABLE
; (polymorphic, checked by unification, holds over every compatible shape).
;
;   mm      : (mat m n) (mat n p) -> (mat m p)     inner dims contract (unify n)
;   transp2 : (mat m n)           -> (mat n m)     flip
;   add,mul : (mat m n) (mat m n) -> (mat m n)     ELEMENTWISE, exact shape (round 1)
;   sum     : (mat m n)           -> scalar        reduce-all
;
; ROUND 1 is exact-shape (no broadcasting, no uu). the bias broadcast in `linear`
; ((mat m n) + (vec n)) and the dependent dim-arithmetic of a concat ((mat r a) ++
; (mat r b) -> (mat r (add a b))) are ROUND 2 -- the same `okr`/`norm` dial the uu
; overlay already turns (doc/misc/proto/overlay-uu.l). here we prove the seam: erasure,
; polymorphic contraction, and rejection. see memory pluggable-types.
; ============================================================================

; --- the tensor domain: mat/vec/scalar constructors, dims by structural equality ---
; no `okr` slot -> the engine accepts a body die iff it JOINS with the declared
; return (i.e. is equal). no `norm` slot -> dims are compared as-is (round 2 adds uu).
(: tdom (hash 'dice     ()
              'ctors    '(mat vec scalar)          ; the constructors -- NOT dim variables
              'join     (\ a b (? (= a b) a 'top))
              'litdie   (\ e 'top)                 ; a bare literal in a body: unknown scalar
              'tr       (\ op a b 'top))           ; bare +/* unused here (tele calls add/mul by name)
   _ (overlay-set tdom ()))

; --- declare the tele primitives' shapes ONCE (the trusted boundary) ---
(sig mm      ((mat m n) (mat n p)) (mat m p))
(sig transp2 ((mat m n))           (mat n m))
(sig add     ((mat m n) (mat m n)) (mat m n))
(sig mul     ((mat m n) (mat m n)) (mat m n))
(sig sum     ((mat m n))           scalar)

; --- user models, shape-checked at compile time (each `deft` prints a clash NOW, if any) ---

; GOOD, polymorphic: holds for every (n,i)x(i,o). checks SILENT.
(deft (fwd (x (mat n i)) (W (mat i o))) scalar
   (sum (mm x W)))

; GOOD, a gram matrix W^T W then reduce -- transp2 + mm compose, dims thread. SILENT.
(deft (gram (W (mat i o))) scalar
   (sum (mm (transp2 W) W)))

; ERR (contraction): mm W x contracts o against n -- distinct rigid dims -> CLASH.
(deft (bad (x (mat n i)) (W (mat i o))) scalar
   (sum (mm W x)))

; ERR (result): body reduces to `scalar`, but the signature claims a matrix.
(deft (wrongret (x (mat n i)) (W (mat i o))) (mat n o)
   (sum (mm x W)))

; --- ERASURE PROOF: run the checked `fwd` on real tensors and take its gradient.
; the overlay never touched the runtime -- fwd is a plain global, tele's tape and
; adjoints work unchanged; the grads come back at the input shapes.
(: _  (tl-reset ())
   X  (array '(2 3) 1 2 3 4 5 6)          ; a 2x3 input
   Wv (array '(3 4) 1 0 0 1 0 1 1 0 1 1 0 0)  ; a 3x4 weight
   gx (glass X)  gW (glass Wv)
   L  (fwd gx gW)                          ; forward: sum(X@W) -- builds the tape
   _  (back L)                             ; reverse-mode: seed 1, walk the tape
   _  (puts "fwd loss           = ") _ (puts (show (tl-val L)))        _ (putc 10)
   _  (puts "grad x  shape      = ") _ (puts (show (shape (grad gx)))) _ (putc 10)   ; (2 3), matches X
   _  (puts "grad W  shape      = ") _ (puts (show (shape (grad gW)))) _ (putc 10)   ; (3 4), matches W
   0)
