; mutate.l -- inle's MUTATION fuzzer: a second proposer for the judge, denser than
; fuzz.l's from-scratch random. it starts from REAL, self-contained love expressions
; (the CLAUDE.md demos) and perturbs their SYNTAX TREE -- swap an operator, tweak an
; int, wrap a subterm in a church apply, splice in a fresh random subtree. mutation
; near working code lands in the arithmetic edge-cases where bugs actually live
; (bignum growth, church towers, division, the ontology flips), not in the desert of
; uniformly-random junk. same propose->judge->keep loop as fuzz.l, just a smarter gen.
;
;   out/host/love -l free/judge.l -l free/fuzz.l -l free/mutate.l -e "(mutfuzz 100 1)"
;
; reuses judge.l (judge/judge-bug/judge-report) + fuzz.l (rng-next/rng-init/pick/ops/
; gen/gen-num). deterministic seeds only -> a host!=love0 disagreement is a real bug.

(: (mem? x l) (? (atom? l) 0 (id? x (cap l)) 1 (mem? x (cup l)))

   ;; -- an expression tree is an ATOM (leaf) or a LIST (node = its elements),
   ;; indexed in PREORDER: node 0 is the whole term, then its children depth-first. --
   (tsize E)  (? (atom? E) 1 (foldl (\ a c (+ a (tsize c))) 1 E))
   (tnth-list l j) (: c (cap l) sz (tsize c) (? (< j sz) (tnth c j) (tnth-list (cup l) (- j sz))))
   (tnth E i) (? (= i 0) E (atom? E) E (tnth-list E (- i 1)))              ; the i-th subterm
   (tset-list l j new) (: c (cap l) sz (tsize c)
                          (? (< j sz) (link (tset c j new) (cup l))
                             (link c (tset-list (cup l) (- j sz) new))))
   (tset E i new) (? (= i 0) new (atom? E) E (tset-list E (- i 1) new))    ; replace the i-th subterm

   ;; -- transform ONE subterm S by its kind --
   (mut-sub S)
      (? (charm? S) (: k (rng-next 4)                    ; an int: nudge / randomize
                       (? (= k 0) (+ S 1) (= k 1) (- S 1) (= k 2) (rng-next 9) (* S 2)))
         (nom? S)   (? (mem? S ops) (pick ops) S)        ; an operator: swap for another
         (: k (rng-next 3)                               ; a list / other subterm
            (? (= k 0) (L (gen-num ()) S)                ; wrap: (n S)  -- a church apply
               (= k 1) (gen 2)                           ; splice: a fresh random subtree
               S)))                                      ; leave it
   (mut1 E)     (: n (tsize E) i (rng-next n) (tset E i (mut-sub (tnth E i))))
   (mutate E k) (? (= k 0) E (mutate (mut1 E) (- k 1)))  ; k successive point mutations

   ;; -- seeds: self-contained expressions from the CLAUDE.md demos (as DATA) --
   seeds (L '(+ 1 2) '(3 2) '(2 3 4) '(// 5 2) '(| -3 4) '(gcd 1071 462)
            '(map (+ 1) (L 1 2 3)) '(sort (L 3 1 2)) '(cap (L 1 2 3))
            '((/ 1 2) 9) '(+ "ab" "cd") '("abc" 0) '(foldl (*) 1 (L 1 2 3 4)))
   (mcand E) (show (L 'puts (L 'show E)))                ; wrap so the VALUE is the diffed observable

   ;; -- the loop: pick a seed, mutate it 1-3 times, judge, keep bug>0 --
   (mut-go i n finds)
      (? (= i n) finds
         (: seed (pick seeds) m (mutate seed (+ 1 (rng-next 3))) c (mcand m) v (judge c) bug (judge-bug v)
            (? (< 0 bug)
               (: _ (puts "  [bug ") _ (puts (show bug)) _ (puts "] ") _ (puts c) _ (putc 10)
                  _ (puts "         ") _ (judge-report v)
                  (mut-go (+ i 1) n (link (link c v) finds)))
               (mut-go (+ i 1) n finds))))
   (mutfuzz n s)
      (: _ (rng-init s)
         _ (puts "-- mutating ") _ (puts (show n)) _ (puts " candidates (seed ") _ (puts (show s)) _ (puts ") --") _ (putc 10)
         finds (mut-go 0 n ())
         _ (puts "-- ") _ (puts (show (tally finds))) _ (puts " interesting / ") _ (puts (show n)) _ (putc 10)
         finds))
