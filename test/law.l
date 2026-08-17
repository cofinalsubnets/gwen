; --- test/law.l: CLAUDE.md's laws, QUANTIFIED ---
; a law written with a free `x` means "forall x, this is 1". prose cannot mean that; a lambda
; can. so every law here is a closure, applied to a generated corpus, and the law holds when
; every application answers the charm 1 -- modulo `=`, which is what the law spellings use.
;
; the corpus is built by JOT, which is recursive and whose glued sigil STACKS: ^4 is the charms,
; ^^4 lists of charms, ^^^4 lists of lists -- the depth is the caret count, so the shape of the
; data is generated rather than written down. the flat rungs -- gems, twins, text, trays, a hot,
; the empties -- are named beside it, since jot cannot mint those.
;
; ⚠ NOT A PROOF, and it says so: this is a fuzz over a finite spread, so it can only ever
; refute. what it buys is that a law in CLAUDE.md cannot be WRONG in silence, which is exactly
; how one dead name and two false laws got in when the laws were prose.
; ⚠ each law NAMES ITSELF and the value that broke it before failing -- a law that fails on one
; kind out of thirty is a sentence about that kind, and `assert` alone would hide it.
(: law-flat (list 0 1 2 5 -1 -5 2.5 -2.5 0.0 () "" "a" "ab" 'sym '(1 2) '(-2 1)
                  (twin 3 4) (twin -1 2) (twin 0 3) @() @(1 2) @(1 2 3) (q 7 2) (q -7 2) cap 'x)
   law-jot (list ^0 ^1 ^2 ^4 ^5 ^^0 ^^1 ^^2 ^^4 ^^5 ^^^0 ^^^1 ^^^2 ^^^3 ^^^4 ^^^^3)
   law-corpus (law-flat + law-jot)
   ; a law is (nom . closure); it holds when every value answers the charm 1
   (law-bad f) (: (go l) (? (atom? l) ()
                            (= 1 (f <l)) (go >l)
                            [1 <l])                       ; (1 witness) -- presence, not the value
                  (go law-corpus))
   (law-run r) (: nom <r f <>r b (law-bad f)
                  (? (two? b)
                     (: _ (puts ("law FAILED: " + string nom + "   at " + show <>b + "\n")) 0)
                     1))
   law-rows
    (list
      ; --- the glued lane IS the law: each line states the sigil and its word at once, and the
      ; valence law names the spaced twin alongside (doc/precedence.md).
      ['cap      (\ x (<x = cap x))]
      ['cup      (\ x (>x = cup x))]
      ['net      (\ x (+x = net x))]
      ['saturate (\ x ($x = saturate x))]
      ['nil?     (\ x (!x = nil? x))]
      ['abs      (\ x (|x = abs x))]
      ['negate   (\ x (-x = negate x))]
      ['jot      (\ x (^x = jot x))]
      ; --- the little schemer's five, inverted: his fence off scheme's undefined cases, and
      ; love is total, so ours say what holds everywhere. an atom is its own cap; its cup is ().
      ['cap-total  (\ x (? (two? x) 1 (x = <x)))]
      ['cup-total  (\ x (? (two? x) 1 (() = >x)))]
      ['link-apart (\ x (: p (link x x) ((<p = x) && (>p = x))))]
      ['link-back  (\ x (? (two? x) (x = link <x >x) 1))]
      ['nil?-total (\ x (: b !x ((b = 0) || (b = 1))))]
      ['=-total    (\ x (: b (x = x) (b = 1)))]
      ['id?-finer  (\ x (? (id? x x) (x = x) 1))]           ; id? implies =, never the converse
      ; --- THE MEASURE and the tower it retracts down. every value has a complex measure and
      ; each rung is that measure saturated into a smaller set -- idempotent, identity on its
      ; own image, which is what makes the tower retractions rather than conversions.
      ; ⚠ a STARRED coin nets ITSELF (the die's star law, q.l) -- net's numeric
      ; retraction story does not apply to it, so the two laws that route
      ; through re(net ..) step aside for coins; ratio-rungs polices their
      ; integer rungs on the exact lane instead.
      ['net-idem  (\ x (: n (net x) (n = net n)))]
      ['re-idem   (\ x (? (coin? x) 1 (: r (re (net x)) (r = re (net r)))))]
      ['ceil-idem (\ x (: c (ceil x) (c = ceil c)))]
      ['sat-idem  (\ x (: s $x (s = $s)))]
      ['bit-idem  (\ x (: b ?x (b = ?b)))]
      ['ceil-net  (\ x (? (coin? x) 1 (ceil x = ceil (re (net x)))))]   ; ceil retracts onto the integers
      ['sat-ceil  (\ x ($x = (? (0 < ceil x) (ceil x) 0)))] ; saturate is ceil, floor raised to 0
      ; --- the EXACT LANE: int/ceil/saturate on a ratio coin long-divide the (n d)
      ; parts, so a part past 2^53 still lands the true integer -- the float net
      ; rounds there, which is why ceil-net's corpus keeps its ratios small. `//`
      ; (exact trunc) is the oracle; the last row divides big by big.
      ; ⚠ THE RUNGS ANSWER A CHARM, and a charm is 2^62 wide on a 64-bit word and 2^30
      ; on a 32-bit one -- so on the narrow word the oracle names a bignum the codomain
      ; cannot hold and every rung saturates instead, correctly. the DIVISOR carries the
      ; word's scale (`word` is 00-init's own flag, false on the 32-bit wasm shim): the
      ; numerator stays past 2^53 either way, which is the whole point, and the quotient
      ; lands inside the charm on both.
      ['ratio-rungs (\ _ (: b ((60 2) + 1)
                            s (word ? 1 (32 2))
                            (all (\ p (: n <p d <>p c (ceil (q n d))
                                         ((int (q n d) = n // d)
                                          && (c = (n // d) + (? (0 < n && 0 != n % d) 1 0))
                                          && (saturate (q n d) = (? (0 < c) c 0)))))
                                 [[7 2] [-7 2] [0 5] [b (3 * s)] [-b (3 * s)]
                                  [b s] [(b * b) (b * s)]])))]
      ; --- + ASSOCIATES, mixed bands included (the degenerate law): a number against
      ; text or a chain arrives as that band's UNIT, which is exactly what lets
      ; (x+y)+z = x+(y+z) close over the bands. three seams are carved out BY NAME,
      ; each its own arc rather than a hedge: text<->chain still ADJOINS (the charlist
      ; hom is the deferred rung), a coin READS a chain as (n d) and mixes garbage
      ; with a twin, and mismatched trays answer () -- undefined-answers-() breaks
      ; any law it touches.
      ['add-assoc
       (\ x (? (lit? x || coin? x || tray? x) 1
               (: ws (list 0 2 -5 2.5 (twin 3 4) () "" "ab" 'sym '(1 2) '(-2 1))
                  (txt? v) (string? v || (? (id? v ()) 0 (nom? v)))
                  (mix? a b c) ((txt? a || txt? b || txt? c) && (two? a || two? b || two? c))
                  (ok? y z) (? (mix? x y z) 1 (((x + y) + z) = (x + (y + z))))
                  (all (\ y (all (ok? y) ws)) ws))))]
      ; ⚠ `bit x`, not `?x`: glued ? is bit EVERYWHERE except opening a parenthesized form, where
      ; `(` + `?` makes the CLAUSE head instead -- (? x) is the one-armed cond, which answers x,
      ; so the law reads x = (0 < $x) and holds only at 0 and 1. this harness caught that.
      ['bit-sat   (\ x (bit x = (0 < $x)))]                 ; bit is saturate, ceiling lowered to 1
      ['nil?-bit  (\ x (!x = (0 = $x)))]                    ; nil? is bit's complement
      ['sat-green (\ x (: s $x ((0 <= s) && (charm? s))))]  ; ..and the codomain each names
      ['bit-bool  (\ x (: b ?x ((b = 0) || (b = 1))))]
      ; --- THE TWO NAMESPACES. a sigil is GLUED, an operator is SPACED, and they share a
      ; CHARACTER, not a meaning -- so `?x` being bit while `(? ..)` is the clause form is a
      ; category difference and not a trap. seven of these eight sigil NAMES are not their
      ; glued word at all; they are the operator, or nothing.
      ['sigil-split (\ _ (: b (list (< = cap) (> = cup) (^ = jot) (+ = net)
                                    (* = prod) (| = abs) (% = fraction))
                           (all (\ v (v = 0)) b)))]
      ; ⚠ `!` is the ONE FOSSIL -- the only sigil character whose NAME is bound to its own glued
      ; word. (! = nil?) reads true for that reason alone; it is not a canonical spelling, and a
      ; law leaning on it is testing the fossil rather than the sigil.
      ['nil?-fossil (\ _ (! = nil?))]
      ; ..and the name `$` is the IDENTITY NUMERAL, which is the whole of why ($ f x) applies:
      ; (1 f) is f. there is no separate apply operator to learn.
      ['apply-is-id (\ _ (($ = 1) && (($ cap '(1 2)) = 1)))]
      ; --- the traps CLAUDE.md states as laws, held to the same standard
      ['unit-f     (\ x (: (g _) x (g = (g))))]             ; (f) == f: a nullary call is the value
      ['curry      (\ x (? (charm? x) (((x +) 2) = (+ x 2)) 1))]
      ['seq        (\ x (: y 1 y (y + 1) (y = 2)))]         ; the sequence law: a rebind reads the previous
      ['empty-form (\ x ((:) = ()))])
   law-n (tally law-corpus)
   law-ok (foldl (\ a r (a + law-run r)) 0 law-rows)
   _ (puts ("law: " + show (tally law-rows) + " laws quantified over "
            + show law-n + " values (jot-generated to depth 3)\n"))
   (assert (law-ok = tally law-rows)))
