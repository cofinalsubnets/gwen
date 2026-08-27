#!/usr/bin/env -S love -l
; lax -- find the pattern positions that the LAX `()` rule was carrying.
;
; a paren pattern used to be lax on its tail: (a b c) fit a chain of three OR
; LONGER, and the empty () asked nothing at all and so fit anything. making ()
; strict, the way [] always was, turns each of those into an exact-length test.
; every site below is one that changes meaning, and the respelling is a tail:
; (a b c) that wants "or longer" becomes (a . b . c . _), and a bare () that
; meant "anything" becomes _. `.` is right-associative and cons takes TWO, so
; the dots go all the way down -- (a b c . _) is (cons (a b c) _), a different
; shape entirely, and it matches nothing the original did.
;
; usage:  love crew/libra/lax.l FILE ..
;
; it reads the surface, before opfix, so a pattern here is spelled the way the
; file spells it. the walk knows three pattern positions -- an @ arm, a \ param,
; and a : binder's head -- and flags a form that names no tag: no infix operator
; anywhere at the top level (. >< + || && @), and a head that is not one of
; list/hash/tuple/?/\. a quoted head is exactly the untagged case ('lam x b), so
; it is flagged, and a glued ^pin is not a form at all.
;
; false positives are the point: a site whose data is always exactly as long as
; the pattern reads the same strict or lax, and only a reader knows which. the
; list is for reading, not for a gate. two things sort it cheaply -- a file's own
; arity table or node constructor answers most of its sites at once (clay-tags in
; crew/moon/clay.l is one), and a longest-first arm cascade over a ranged tag
; reads the same either way, since the longer arm already took the longer node.
(:
   infix '(. >< + || && @)
   tagheads '(list hash tuple ? \ mono jot)      ; mono/jot: a glued ^pin, already exact

   ; --- lines ----------------------------------------------------------------
   (nlupto s n) (: (go i a) (? (n <= i) a (go (i + 1) (? (s i = 10) (a + 1) a)))
                   (go 0 0))

   ; --- is this pattern an untagged form? -------------------------------------
   ; the operand slots sit at odd or even indices depending on whether the head is
   ; one word or a quoted two, so an infix anywhere at the top level names the form
   (has-infix? p op) (: (go l) (? (nil? (two? l)) 0
                                  (&& (nom? <l) (= <l op)) 1
                                  (go >l))
                        (go p))
   (any-infix? p) (: (go l) (? (nil? (two? l)) 0
                               (&& (nom? <l) (member? <l infix)) 1
                               (go >l))
                     (go p))
   ; the run before op. a singleton collapses, `(x)` being `x`, so one element is
   ; the pattern itself and not a one-long form
   (upto p op) (: (go l) (? (nil? (two? l)) ()
                            (&& (nom? <l) (= <l op)) ()
                            (<l >< go >l))
                  q (go p)
                  (? (two? >q) q <q))
   (untagged? p)
    (? (nil? (two? p)) 0                      ; an atom is a literal: already strict
       (any-infix? p) 0                       ; . >< + || && @ -- a tag, or a guard
       (: h <p (? (nom? h) (nil? (member? h tagheads)) 1)))   ; a quoted head IS untagged

   ; --- the walk -------------------------------------------------------------
   ; a tagged form's operands are patterns in their own right and the strict rule
   ; reaches all the way down, so a site is a whole subtree. but only the slots pw
   ; walks as patterns count: a `?`'s leading operands are GUARDS and a `&&`'s
   ; right side is one, a `^`pin's operand is read from outside, and a `\`'s is a
   ; literal -- recursing into those reports ordinary calls as patterns.
   (kids p) (: h <p as >p
               (? (has-infix? p '&&) [(upto p '&&)]         ; the guard's pattern prefix
                  (any-infix? p) p                          ; every element but the operator noms
                  (nil? (nom? h)) as
                  (= h '?) (? (nil? (two? >as)) () [(last as)])   ; a lone operand is the guard
                  (|| (= h 'jot) (= h 'mono) (= h '\)) ()
                  (= h 'hash) (evens as)                    ; {k v ..}: the values, keys are literals
                  as))
   (last l) (? (two? >l) (last >l) <l)
   (evens l) (? (nil? (two? l)) ()
                (nil? (two? >l)) ()
                (<>l >< evens >>l))
   (pat1 p acc) (? (nil? (two? p)) acc
                   (: a (? (untagged? p) (p >< acc) acc)
                      (foldl (\ z e (pat1 e z)) a (kids p))))
   ; arms: pat body pat body .. with an optional lone else, so a pattern sits at
   ; an even index and never in the final odd slot.
   (arms as acc) (: n (tally as)
                    (go l i a) (? (nil? (two? l)) a
                                  (go >l (i + 1)
                                      (? (&& (= 0 (% i 2)) (i < (n - 1))) (pat1 <l a) a)))
                    (go as 0 acc))
   (params ps acc) (? (nil? (two? ps)) acc                  ; every param but the body
                      (nil? (two? >ps)) acc
                      (params >ps (pat1 <ps acc)))
   (allpats ps acc) (? (nil? (two? ps)) acc
                       (allpats >ps (pat1 <ps acc)))
   ; a : binder's head, (f p1 p2 ..): the name leads, the rest are params, and
   ; the body sits OUTSIDE the head, so unlike a \ there is no last one to skip
   (binders bs acc) (? (nil? (two? bs)) acc
                       (nil? (two? >bs)) acc                ; the trailing body is not a pair
                       (: nm <bs
                          a2 (? (two? nm) (allpats >nm acc) acc)
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
