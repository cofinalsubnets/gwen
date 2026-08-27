#!/usr/bin/env -S love -l
; lax -- find the pattern positions that the LAX `()` rule was carrying.
;
; a paren pattern used to be lax on its tail: (a b c) fit a chain of three OR
; LONGER, and the empty () asked nothing at all and so fit anything. making ()
; strict, the way [] always was, turns each of those into an exact-length test.
; every site below is one that changes meaning, and the respelling is a tail:
; (a b c) that wants "or longer" becomes (a b c . _), and a bare () that meant
; "anything" becomes _.
;
; usage:  love crew/libra/lax.l FILE ..
;
; it reads the surface, before opfix, so a pattern here is spelled the way the
; file spells it. the walk knows three pattern positions -- an @ arm, a \ param,
; and a : binder's head -- and flags a form that names no tag: no infix operator
; in an odd slot (. >< + || && @), and a head that is not list/hash/tuple/?/\.
; a quoted head is exactly the untagged case ('lam x b), so it is flagged, and
; a glued ^pin is not a form at all.
;
; false positives are the point: a site whose data is always exactly as long as
; the pattern reads the same strict or lax, and only a human knows which. the
; list is for reading, not for a gate.
(:
   infix '(. >< + || && @)
   tagheads '(list hash tuple ? \ mono jot)      ; mono/jot: a glued ^pin, already exact

   ; --- lines ----------------------------------------------------------------
   (nlupto s n) (: (go i a) (? (n <= i) a (go (i + 1) (? (s i = 10) (a + 1) a)))
                   (go 0 0))

   ; --- is this pattern an untagged form? -------------------------------------
   (odd-infix? p) (: (go l i) (? (nil? (two? l)) 0
                                 (&& (= 1 (% i 2)) (nom? <l) (member? <l infix)) 1
                                 (go >l (i + 1)))
                     (go p 0))
   (untagged? p)
    (? (nil? (two? p)) 0                      ; an atom is a literal: already strict
       (odd-infix? p) 0                       ; . >< + || && @ -- a tag, or a guard
       (: h <p (? (nom? h) (nil? (member? h tagheads)) 1)))   ; a quoted head IS untagged

   ; --- the walk -------------------------------------------------------------
   ; arms: pat body pat body .. with an optional lone else, so a pattern sits at
   ; an even index and never in the final odd slot.
   (arms as acc) (: n (tally as)
                    (go l i a) (? (nil? (two? l)) a
                                  (go >l (i + 1)
                                      (? (&& (= 0 (% i 2)) (i < (n - 1)) (untagged? <l))
                                         (<l >< a) a)))
                    (go as 0 acc))
   (params ps acc) (? (nil? (two? ps)) acc                  ; every param but the body
                      (nil? (two? >ps)) acc
                      (params >ps (? (untagged? <ps) (<ps >< acc) acc)))
   ; a : binder's head, (f p1 p2 ..): the name leads, the rest are params
   (binders bs acc) (? (nil? (two? bs)) acc
                       (nil? (two? >bs)) acc                ; the trailing body is not a pair
                       (: nm <bs
                          a2 (? (two? nm) (params >nm acc) acc)
                          (binders >>bs a2)))
   (atpos x acc) (: (go l i) (? (nil? (two? l)) acc
                                (&& (nom? <l) (= <l '@))
                                 (arms >l acc)                    ; infix: arms follow the @
                                (go >l (i + 1)))
                    (? (&& (nom? <x) (= <x '@)) (arms >>x acc)    ; prefix: skip the scrutinee
                       (go x 0)))
   (hits x acc)
    (? (nil? (two? x)) acc
       (: h <x
          a1 (? (&& (nom? h) (= h '\)) (params >x acc)
                (&& (nom? h) (= h ':)) (binders >x acc)
                (atpos x acc))
          (foldl (\ a e (hits e a)) a1 x)))

   ; --- the report -----------------------------------------------------------
   ; a love file is usually ONE top-level form, so the datum's own offset says
   ; almost nothing. a flagged pattern prints the way the source spells it --
   ; that is what untagged means -- so the line is found by looking its text up
   ; in the source, and the count says how many sites share the spelling.
   (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) t) ()))
   (linesof s) (: n (tally s)
                  (g i st acc) (? (n <= i) (rev (snip s st n >< acc))
                                  (s i = 10) (g (i + 1) (i + 1) (snip s st i >< acc))
                                  (g (i + 1) st acc))
                  (g 0 0 ()))
   (has? hay ndl) (: n (tally hay) m (tally ndl)
                     (go i) (? ((n - m) < i) 0 (= ndl (snip hay i (i + m))) 1 (go (i + 1)))
                     (? (m = 0) 0 (go 0)))
   (bare s) (: n (tally s)                       ; the line's first non-blank
               (g i) (? (n <= i) 32 (: c (s i) (? (|| (c = 32) (c = 9)) (g (i + 1)) c)))
               (g 0))
   ; a comment can quote a pattern, so prefer a line that is code
   (lineof lns t) (: (go l i best) (? (nil? (two? l)) best
                                      (nil? (has? <l t)) (go >l (i + 1) best)
                                      (= 59 (bare <l)) (go >l (i + 1) (? best best i))
                                      i)
                     (go lns 1 0))
   (tally-of x l) (foldl (\ a e (? (= e x) (a + 1) a)) 0 l)
   (scan path)
    (: s (rdf path)
       (? (nil? s) (say err ("lax: cannot open " + path + "\n"))
        (: lns (linesof s)
           (go rest acc)
            (: r (sound rest)
               (? (id? r 'torn) acc
                  (nil? (two? r)) acc
                  (: d <r rs >r a2 (cat acc (rev (hits d ())))
                     (? (two? rs) (go rs a2) a2))))
           hs (go s ())
           ts (map show hs)
           (each (uniq ts)
            (\ t (: ln (lineof lns t) k (tally-of t ts)
                    (say out (path + ":" + show ln + ": " + t
                              + (? (1 < k) ("   (" + show k + " sites)") "")),
                     putc 10)))))))
   (main as) (? (two? as) (: _ (each as scan) 0)
                (: _ (say err "usage: love crew/libra/lax.l FILE ..\n") 2))
   (main >argv))
