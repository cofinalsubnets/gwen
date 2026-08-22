; --- test/law.l: CLAUDE.md's laws, QUANTIFIED ---
; a law written with a free `x` means "forall x, this is 1".
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
      ; valence law names the spaced twin alongside (doc/misc/precedence.md).
      ['cap      (x \ <x = cap x)]
      ['cup      (x \ >x = cup x)]
      ['net      (x \ +x = net x)]
      ['saturate (x \ $x = saturate x)]
      ['nil?     (x \ !x = nil? x)]
      ['abs      (x \ |x = abs x)]
      ['negate   (x \ -x = negate x)]
      ['jot      (x \ ^x = jot x)]
      ; --- the little schemer's five, inverted: his fence off scheme's undefined cases, and
      ; love is total, so ours say what holds everywhere. an atom is its own cap; its cup is ().
      ['cap-total  (x \ two? x ? 1 (x = <x))]
      ['cup-total  (x \ two? x ? 1 (() = >x))]
      ['link-apart (x \ : p (x >< x) (<p = x && >p = x))]
      ['link-back  (x \ two? x ? (x = link <x >x) 1)]
      ['nil?-total (x \ : b !x (b = 0 || b = 1))]
      ['=-total    (x \ : b (x = x) (b = 1))]
      ['id?-finer  (x \ id? x x ? (x = x) 1)]           ; id? implies =, never the converse
      ; --- THE MEASURE and the tower it retracts down. every value has a complex measure and
      ; each rung is that measure saturated into a smaller set -- idempotent, identity on its
      ; own image, which is what makes the tower retractions rather than conversions.
      ['net-idem  (x \ : n +x (n = +n))]
      ['re-idem   (x \ coin? x ? 1 (: r (re +x) (r = re +r)))]
      ['ceil-idem (x \ : c (ceil x) (c = ceil c))]
      ['sat-idem  (x \ : s $x (s = $s))]
      ['bit-idem  (x \ : b ?x (b = ?b))]
      ['ceil-net  (x \ coin? x ? 1 (ceil x = ceil (re +x)))]   ; ceil retracts onto the integers
      ['sat-ceil  (x \ $x = (0 < ceil x ? (ceil x) 0))] ; saturate is ceil, floor raised to 0
      ; --- the EXACT LANE: int/ceil/saturate on a ratio coin long-divide the (n d)
      ; parts, so a part past 2^53 still lands the true integer -- the float net
      ; rounds there, which is why ceil-net's corpus keeps its ratios small.
      ['ratio-rungs (\ _ (: b ((60 2) + 1)
                            s (word ? 1 (32 2))
                            (all (\ p (: n <p d <>p c (ceil (q n d))
                                         ((int (q n d) = n // d)
                                          && (c = (n // d) + (? (0 < n && 0 != n % d) 1 0))
                                          && (saturate (q n d) = (? (0 < c) c 0)))))
                                 [[7 2] [-7 2] [0 5] [b (3 * s)] [-b (3 * s)]
                                  [b s] [(b * b) (b * s)]])))]
      ['add-assoc
       (\ x (? (lit? x || coin? x || tray? x) 1
               (: ws (list 0 2 -5 2.5 (twin 3 4) () "" "ab" 'sym '(1 2) '(-2 1))
                  (txt? v) (string? v || (? (id? v ()) 0 (nom? v)))
                  (mix? a b c) ((txt? a || txt? b || txt? c) && (two? a || two? b || two? c))
                  (ok? y z) (? (mix? x y z) 1 (((x + y) + z) = (x + (y + z))))
                  (all (\ y (all (ok? y) ws)) ws))))]
      ['mul-assoc
       (\ x (? (lit? x || coin? x) 1
               (: ws (list 0 2 -5 2.5 (twin 3 4) () "" "ab" 'sym '(1 2) @(1 2))
                  (ch v) (? (two? v) 1 0)
                  (ok? y z) (? (coin? y || coin? z) 1
                               (1 < (ch x + ch y + ch z)) 1
                               (((x * y) * z) = (x * (y * z))))
                  (all (\ y (all (ok? y) ws)) ws))))]
      ['mul-dist
       (\ x (? (lit? x || coin? x) 1
               (: ms (list 0 1 2 5)
                  nums (list 0 2 -5 2.5 (twin 3 4))
                  act (all (\ m (all (\ n ((x * (m + n)) = ((x * m) + (x * n)))) ms)) ms)
                  num (? (two? x || tray? x || string? x || (? (id? x ()) 0 (nom? x))) 1
                         (all (\ y (all (\ z ((x * (y + z)) = ((x * y) + (x * z)))) nums)) nums))
                  cart (? (two? x)
                          (: bs (list '(3) '(-2 1))
                             (all (\ a (all (\ b (((a + b) * x) = ((a * x) + (b * x)))) bs)) bs))
                          1)
                  (act && num && cart))))]
      ['bit-sat   (x \ ?x = (0 < $x))]                      ; bit is saturate, ceiling lowered to 1
      ['nil?-bit  (\ x (!x = (0 = $x)))]                    ; nil? is bit's complement
      ['sat-net   (x \ $x = $(net x))]                      ; ..so $ factors through the net: one clamp, at the end
      ['bit-nil   (x \ ?x = !!x)]                           ; ..and the bit is nil?'s own double negation
      ['sat-green (\ x (: s $x ((0 <= s) && (charm? s))))]  ; ..and the codomain each names
      ['bit-bool  (\ x (: b ?x ((b = 0) || (b = 1))))]
      ['apply-is-id (x \ atom? x ? 1 (<x = (cap $ x)))]
      ; --- the traps CLAUDE.md states as laws, held to the same standard
      ['unit-f     (x \ : (g _) x (g = (g)))]             ; (f) == f: a nullary call is the value
      ['curry      (x \ charm? x ? (((x +) 2) = (+ x 2)) 1)]
      ['seq        (x \ : y 1 y (y + 1) (y = 2))]         ; the sequence law: a rebind reads the previous
      ['empty-form (x \ ((:) = ()))])
   law-ok (foldl (\ a r (a + law-run r)) 0 law-rows)
   (assert (law-ok = tally law-rows)))
