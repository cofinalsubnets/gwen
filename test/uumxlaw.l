; test/uumxlaw.l -- the LAWS of love's `+` and `*` dispatch, proved of the
; GENERATED table (test/uumx.l, lifted from src/mx.l by tools/mx2uu.l; this file
; loads after it by sort order, and the kernel + nat vocabulary live in
; test/uu.l). the theorems are HAND-WRITTEN over a table nobody typed, so a
; dispatch edit that breaks the band quotient, the mirror symmetry, or any cell
; the narrative names one at a time, fails a PROOF right here at `make test`.
;
; every proof is `idpath true`: the claim is a BOUNDED FORALL and the kernel RUNS
; it, so a square closes by conversion where test/proof/rocq/mx.v's closes by
; vm_compute. the two models read one datum and derive the band partition by one
; rule, and these terms export through uu2coq/uu2lean -- so mx.l's shape stands in
; three kernels, twice in Rocq by two roads.
;
; what this cannot see, and the fuzz must: that a lane nom names the C function it
; says it does. the table is the interface; love.c is the other side of it.
(assert (member? 'vof (names ())))

; --- the roster every forall below ranges over. a kind added or a lane merged
; --- moves these numbers first ---
(assert
 (15 = (vof 0 'mxnk))
 (12 = (vof 0 'mxnl))
 (5 = (vof 0 'mxnb)))

; --- THE BAND LATTICE: the 225-cell squares carry no information beyond the
; --- quotient. a 5x5 band matrix plus the band assignment IS the whole table ---
(assert
 (defq mx-add-factors-through-bands
   (paths bool
     (mxsq (lam a (lam b (nateqb (mxadd a b) (mxaddband (mxband a) (mxband b))))))
     true)
   (idpath true))
 (defq mx-mul-factors-through-bands
   (paths bool
     (mxsq (lam a (lam b (nateqb (mxmul a b) (mxmulband (mxband a) (mxband b))))))
     true)
   (idpath true)))

; --- DISPATCH COMMUTES up to mirror: a symmetric lane orients itself, the
; --- positional bin_a/bin_b pair carries orientation in the name, and the table
; --- adds none of its own. mirror being an involution is half the claim ---
(assert
 (defq mx-mirror-involutive
   (paths bool (mxall mxnl (lam l (nateqb (mxmirror (mxmirror l)) l))) true)
   (idpath true))
 (defq mx-add-dispatch-commutes
   (paths bool (mxsq (lam a (lam b (nateqb (mxadd a b) (mxmirror (mxadd b a)))))) true)
   (idpath true))
 (defq mx-mul-dispatch-commutes
   (paths bool (mxsq (lam a (lam b (nateqb (mxmul a b) (mxmirror (mxmul b a)))))) true)
   (idpath true)))

; --- the partition, named: the numeric band, the text band, and KMint alone ---
(assert
 ; the nine numeric kinds -- the scalar gem tower and the parallel tray tower --
 ; route identically. that IS "every numeric kind is one band"
 (defq mx-numerics-one-band
   (paths bool (mxall 9 (lam i (nateqb (mxband (add mxKCharm i)) (mxband mxKCharm)))) true)
   (idpath true))
 ; a symbol takes the string algebra under +, so KNom and KString are ONE band
 (defq mx-nom-is-text
   (paths nat (mxband mxKNom) (mxband mxKString)) (idpath (mxband mxKString)))
 ; and KMint is its own, the unit lane whole -- so nothing rests on the
 ; dispatchers' early-out, which is the fast path and not the semantics
 (defq mx-mint-alone
   (paths bool (mxall 14 (lam k (negb (nateqb (mxband (succ k)) (mxband mxKMint))))) true)
   (idpath true)))

; --- () is the unit under + and the zero under *, in every lane at once ---
(assert
 (defq mx-mint-is-the-unit
   (paths bool (mxall mxnk (lam b (nateqb (mxadd mxKMint b) mxLbin_unit))) true)
   (idpath true))
 (defq mx-mint-annihilates
   (paths bool (mxall mxnk (lam b (nateqb (mxmul mxKMint b) mxL0))) true)
   (idpath true)))

; --- the diagonal reads the lattice: one add/mul lane pair per band, the algebra
; --- ladder. these five rows ARE the five bands ---
(assert
 (defq mx-diag-mint-add (paths nat (mxadd mxKMint mxKMint) mxLbin_unit) (idpath mxLbin_unit))
 (defq mx-diag-mint-mul (paths nat (mxmul mxKMint mxKMint) mxL0) (idpath mxL0))
 (defq mx-diag-text-add (paths nat (mxadd mxKNom mxKNom) mxLadd_string) (idpath mxLadd_string))
 (defq mx-diag-text-mul (paths nat (mxmul mxKNom mxKNom) mxL0) (idpath mxL0))
 (defq mx-diag-num-add (paths nat (mxadd mxKCharm mxKCharm) mxLaddn) (idpath mxLaddn))
 (defq mx-diag-num-mul (paths nat (mxmul mxKCharm mxKCharm) mxLmuln) (idpath mxLmuln))
 (defq mx-diag-chain-add (paths nat (mxadd mxKChain mxKChain) mxLadd_seq) (idpath mxLadd_seq))
 ; chain*chain is the cartesian product -- the one cell that makes the list lane a
 ; semiring rather than a monoid
 (defq mx-diag-chain-mul (paths nat (mxmul mxKChain mxKChain) mxLmul_cart) (idpath mxLmul_cart))
 (defq mx-diag-hot-add (paths nat (mxadd mxKTablet mxKTablet) mxLaddh) (idpath mxLaddh))
 (defq mx-diag-hot-mul (paths nat (mxmul mxKTablet mxKTablet) mxLmulh) (idpath mxLmulh)))

; --- and the cells the narrative names one at a time ---
(assert
 ; mixed bands DEGENERATE: a number against a string arrives as that band's unit
 ; and the other operand answers whole, positionally
 (defq mx-num-str-degenerates (paths nat (mxadd mxKCharm mxKString) mxLbin_b) (idpath mxLbin_b))
 (defq mx-str-num-degenerates (paths nat (mxadd mxKString mxKCharm) mxLbin_a) (idpath mxLbin_a))
 ; nom+str keeps the spelling hom -- it demotes to a string, it does not degenerate
 (defq mx-nom-str-spells (paths nat (mxadd mxKNom mxKString) mxLadd_string) (idpath mxLadd_string))
 ; nom+chain still adjoins: foo stays foo
 (defq mx-nom-chain-adjoins (paths nat (mxadd mxKNom mxKChain) mxLadd_seq) (idpath mxLadd_seq))
 ; str*str is the absorbing zero; a number against either repeats it
 (defq mx-str-str-is-zero (paths nat (mxmul mxKString mxKString) mxL0) (idpath mxL0))
 (defq mx-num-str-repeats (paths nat (mxmul mxKCharm mxKString) mxLmul_rep) (idpath mxLmul_rep))
 ; a tablet or a lambda wins over everything but a mint -- row and column both the
 ; Church lane, a tablet being a lookup lambda here
 (defq mx-tablet-dominates
   (paths bool
     (mxall mxnk (lam b (orb (nateqb b mxKMint) (nateqb (mxadd mxKTablet b) mxLaddh))))
     true)
   (idpath true))
 (defq mx-hot-dominates
   (paths bool
     (mxall mxnk (lam b (orb (nateqb b mxKMint) (nateqb (mxmul mxKHot b) mxLmulh))))
     true)
   (idpath true)))

; --- THE COMPOSE CELL: which cell of this table test/uuhom.l is about. that file
; --- proves the monoid laws of homcomp -- love's `*` on the top band, prel's
; --- compose -- as a free-standing algebra; here the lane is NAMED, so a
; --- dispatch edit that moved compose off the hot diagonal, or made the mint a
; --- unit there instead of the zero, fails a proof rather than quietly leaving
; --- uuhom a monoid about nothing. the guard fails by name if uuhom's terms are
; --- not up: they load earlier by sort order and the citation must not dangle ---
(assert (two? (peep GLOB 'homcomp 0))
        (two? (peep GLOB 'hom-assoc 0))
        (two? (peep GLOB 'idfun 0)))
(assert
 (defn mxLcompose nat mxLmulh)                     ; the lane uuhom's homcomp spells
 (defq mx-compose-diag (paths nat (mxmul mxKHot mxKHot) mxLcompose) (idpath mxLcompose))
 ; ..and the mint at that cell is the ZERO, not the monoid's unit. uuhom's unit is
 ; idfun, itself a lambda and so a KHot -- it rides the compose lane like any other
 (defq mx-compose-zero (paths nat (mxmul mxKHot mxKMint) mxL0) (idpath mxL0))
 ; the Church SUM is the band's other lane: uuhom's monoid is the multiplicative
 ; half of a two-operation band, and the two lanes are distinct
 (defq mx-church-add-diag (paths nat (mxadd mxKHot mxKHot) mxLaddh) (idpath mxLaddh))
 (defq mx-church-lanes-apart (paths bool (nateqb mxLaddh mxLcompose) false) (idpath false)))

; --- the planted faults. a bounded forall that came back `true` for the wrong
; --- reason -- an empty range, a lookup that fell off its table -- would paint
; --- every theorem above green, so each load-bearing claim gets its false twin
; --- and the kernel must refuse that one. every negative takes its positive. ---
(hear (\ p e (: h (peep HELPC () 0) (? h (h p e) 0))))
(assert
 ; the cartesian cell is not the zero
 (id? 'accepted (rejects (\ z (defq mx-ok-cart
    (paths nat (mxmul mxKChain mxKChain) mxLmul_cart) (idpath mxLmul_cart)))))
 (id? 'rejected (rejects (\ z (defq mx-bad-cart
    (paths nat (mxmul mxKChain mxKChain) mxL0) (idpath mxL0)))))
 ; the band assignment has to be the DERIVED one: shift it by a kind and the
 ; factorization stops holding
 (id? 'rejected (rejects (\ z (defq mx-bad-band
    (paths bool
      (mxsq (lam a (lam b (nateqb (mxadd a b) (mxaddband (mxband a) (mxband (succ b)))))))
      true)
    (idpath true)))))
 ; and mirror is not the identity: the positional pair really does trade places
 (id? 'rejected (rejects (\ z (defq mx-bad-mirror
    (paths bool (mxsq (lam a (lam b (nateqb (mxadd a b) (mxadd b a))))) true)
    (idpath true)))))
 ; the compose cell is the MUL lane, not the Church sum -- a band with one
 ; operation would make uuhom's monoid the whole story, and it is half of it
 (id? 'accepted (rejects (\ z (defq mx-ok-compose
    (paths nat (mxmul mxKHot mxKHot) mxLmulh) (idpath mxLmulh)))))
 (id? 'rejected (rejects (\ z (defq mx-bad-compose
    (paths nat (mxmul mxKHot mxKHot) mxLaddh) (idpath mxLaddh)))))
 ; ..and the mint annihilates there rather than uniting
 (id? 'rejected (rejects (\ z (defq mx-bad-compose-unit
    (paths nat (mxmul mxKHot mxKMint) mxLmulh) (idpath mxLmulh))))))
(hear test-strict)
