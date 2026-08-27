; test/uuvaldiff.l -- the DIFFERENTIAL that binds test/uuval.l's model to the
; implementation. the model proves the tower's SHAPE (retraction, idempotence,
; factorisation) and carries no arithmetic theorem on purpose; this file runs the
; other half -- love's own saturate/bit/nil?/ceil beside the model's vsat/vbit/
; vnil/vceil over an encoding of test/law.l's corpus, so a wrong zadd, a wrong
; fold, or a wrong ceil reddens `make test` even though no proof could see it.
;
; the encoding is STRUCTURAL where love's values are: a pair becomes the model's
; link (ii2 over a tpair, both limbs at the height below), an atom becomes a
; leaf carrying its measure. the leaf measure is read off love's own net -- that
; primitive is the one thing this lane cannot second-guess, the same standing gap
; uuwm has with the C runtime under crew/lux/core.l. what IS under test is
; everything above it: the fold over links, re, ceil, the clamp and the bit.
;
; the model's reals are the half-integers, so a measure that is neither whole nor
; a half is off the corpus and says so (uuval-not-a-half) rather than going quiet.
(assert (member? 'vof (names ())))

; a body-less `:` -- the uv2-* vocabulary leaks; none shadows a nif.
(:
   (uv2-max a b) (a < b ? b a)
   (uv2-floor r) (0 - (ceil (0 - r)))
   ; twice the real part, as an exact integer -- love's ceil already drops the
   ; imaginary rung, and ceil = floor is the whole-number test. this is the door
   ; `re` cannot open: the ratio band answers () to it, and 7/2 is a half
   (uv2-twice m)
    (: x (2 * m) c (ceil x)
       (c = (0 - (ceil (0 - x))) ? c (scare 'uuval-not-a-half m)))
   (uv2-zed z) (z >= 0 ? ['ii1 z] ['ii2 ((0 - z) - 1)])
   ; a half-integer, given twice itself, as a uu rea term
   (uv2-rea d)
    (: r (d / 2) f (uv2-floor r) g (r - f)
       (? (g = 0)   ['tpair (uv2-zed f) 'false]
          (g = 0.5) ['tpair (uv2-zed f) 'true]
          (scare 'uuval-not-a-half d)))
   (uv2-msr n)
    (: iv (im n)                                          ; () off the complex band
       ['tpair (uv2-rea (uv2-twice n))
               (uv2-rea (id? iv () ? 0 (uv2-twice iv)))])
   (uv2-ht x) (? (two? x) (1 + (uv2-max (uv2-ht <x) (uv2-ht >x))) 0)
   ; the tree at a given height bound: a leaf spells the same at every height, so
   ; the shallow limb of a pair costs nothing to raise
   (uv2-tre x d)
    (? (two? x) ['ii2 ['tpair (uv2-tre <x (d - 1)) (uv2-tre >x (d - 1))]]
       (d = 0)  (uv2-msr (net x))
       ['ii1 (uv2-msr (net x))])
   (uv2-enc x) (: d (uv2-ht x) ['tpair d (uv2-tre x d)])
   (uv2-unz v) (v @ ['i1 n] n ['i2 n] ((0 - n) - 1) (scare 'uuval-not-an-int v))
   ; the four model lanes, each run in the kernel over the encoded value
   (uv2-sat x)  (vof 0 ['vsat (uv2-enc x)])
   (uv2-ceil x) (uv2-unz (vof 0 ['vceil (uv2-enc x)]))
   (uv2-bit x)  (vof 0 ['vbit (uv2-enc x)] = 'true ? 1 0)
   (uv2-nil x)  (vof 0 ['vnil (uv2-enc x)] = 'true ? 1 0)

   ; test/law.l's corpus, minus the values whose measure is not a half-integer
   uv2-corpus
    (list 0 1 2 5 -1 -5 2.5 -2.5 0.0 () "" "a" "ab" 'sym 'x cap
          '(1 2) '(-2 1) '(2.5 -1) '((1 2) (3)) '(1 (2 (3 (4))))
          (twin 3 4) (twin -1 2) (twin 0 3) @() @(1 2) @(1 2 3)
          (q 7 2) (q -7 2) {} {'a 1} ^3 ^^2)

   ; a row holds when all four lanes agree; a miss names the value and the lane
   (uv2-row x)
    (? (! (uv2-sat x = saturate x))  [1 x 'saturate]
       (! (uv2-bit x = bit x))       [1 x 'bit]
       (! (uv2-nil x = nil? x))      [1 x 'nil?]
       (! (uv2-ceil x = ceil x))     [1 x 'ceil]
       ())
   (uv2-run l)
    (? (atom? l) 1
       (: b (uv2-row <l)
          (? (two? b)
             (: _ (puts ("uuval differential FAILED: " + show <>b + " on " + show >>b + "\n")) 0)
             (uv2-run >l)))))

(assert
 ; the four lanes agree with love over the whole corpus
 (uv2-run uv2-corpus)
 ; ..and the comparison has teeth: the fold over a link is what makes (1 2)
 ; differ from its head, so an encoder that dropped a limb would not pass above
 (! ((vof 0 ['vsat (uv2-enc 1)]) = (saturate '(1 2))))
 (3 = (uv2-sat '(1 2)))
 (0 = (uv2-sat '(-2 1)))                                  ; the net goes negative, the clamp bites
 (3 = (uv2-ceil 2.5))
 (-2 = (uv2-ceil -2.5))
 (4 = (uv2-ceil (q 7 2)))
 (3 = (uv2-ceil (twin 3 4))))                             ; re drops the imaginary rung
