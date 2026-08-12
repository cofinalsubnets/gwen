; test/pat.l -- the LAWS of `@`, pattern matching. the macro itself is love/pat.l
; (a module: the boot splices it, so the corpus reads `@` bare); this file is what
; keeps it honest. read love/pat.l for the grammar -- every tag it documents has
; its clause below, and the sections here follow that order.

(assert
 ; --- a nom binds, `_` swallows, and position is the whole grammar
 (2 = (@ '(1 2) [_a b] b))
 (2 = (@ '(1 2) [_ b] b))
 (1 = (@ '(1 2) [cap cup] cap))            ; the args are read OUTSIDE: a nom may wear an accessor's name
 ('no = (@ '(1 2 3) [a b] a 'no))          ; [a b] is EXACTLY two
 ('no = (@ '(1) [a b] a 'no))
 (() = (@ '(1 2 3) [a b] a))               ; no clause and no else: the zero point
 (93 = (@ '(3 (30 3)) [a [b c]] (c + (b * a))))
 ; --- a literal matches itself; 'a is the SYMBOL, never a binding
 ('sym = (@ 'a 'a 'sym 'other)) ('other = (@ 'b 'a 'sym 'other))
 (7 = (@ 5 5 7 0)) (7 = (@ "hi" "hi" 7 0)) (0 = (@ "hi" "ho" 7 0))
 ('empty = (@ () () 'empty 'no)) ('no = (@ 0 () 'empty 'no))   ; () and 0 are distinct nothings
 ('lst = (@ '(1 2) '(1 2) 'lst 'no))       ; a quoted LIST is one literal, not a shape
 ; --- clauses, in source order, like ?
 (72 = (@ '(sum 32 40) ['sum a b] (a + b) ['product a b] (a * b) 0))
 (72 = (@ '(product 6 12) ['sum a b] (a + b) ['product a b] (a * b) 0))
 ; --- >< : the cons, and the ONE law -- a pattern is what the constructor builds
 (1 = (@ '(1 2 3) (a >< t) a))
 ('(2 3) = (@ '(1 2 3) (a >< t) t))
 ('(3) = (@ '(1 2 3) (a >< b >< t) t))     ; ..it chains, so the tail walks further in
 ('no = (@ 5 (a >< t) a 'no))
 ('no = (@ '(1) (a >< b >< t) t 'no))      ; ..and every step still owes a two?
 ('(1 2) = (@ '(1 2) (h >< t) (h >< t)))   ; THE LAW: one text, matching and building
 (id? link ><)                             ; ..and >< IS link -- the tag is one, the spelling two
 ; ..so a bracket stays closed: [a b >< t] is ([a b] >< t) is (>< (list a b) t)
 ('yes = (@ '((1 2) 3) [a b >< t] 'yes 'no))
 ('yes = (@ '((1 2) 3) ([a b] >< t) 'yes 'no))
 ('yes = (@ '((1 2) 3) (link [a b] t) 'yes 'no))
 ('no = (@ '(1 2 3) [a b >< t] 'yes 'no))  ; ..1 is not the chain (a b)
 ; ..and "this tag, then more" is the cons chain, which is what says it
 ('at3 = (@ '(lam x b) ('lam >< x >< b >< _) 'at3 'no))
 ('at5 = (@ '(lam x b j k) ('lam >< x >< b >< _) 'at5 'no))
 ('no = (@ '(lam x) ('lam >< x >< b >< _) 'yes 'no))
 ('exact = (@ '(lam x b) ['lam x b] 'exact 'no)) ('no = (@ '(lam x b j) ['lam x b] 'exact 'no))
 (2 = (@ '(1 2 3) (a >< 'b >< t) 0 (a >< b >< t) b))   ; a tail is a pattern like any other
 (6 = (: (sum l) (@ l (h >< t) (h + (sum t)) 0) (sum '(1 2 3))))
 ; --- a repeated nom matches NON-LINEARLY
 ('same = (@ '(1 1) [a a] 'same 'diff)) ('diff = (@ '(1 2) [a a] 'same 'diff))
 (7 = (@ '(7 7) [a a] a 0))
 ('pair = (@ '((1 2) (1 2)) [a a] 'pair 'no))
 ('no = (@ '((1 2) (1 3)) [a a] 'pair 'no))
 ('hit = (@ [3 #('f 3)] [n {'f n}] 'hit 'no))     ; ..across shapes, too
 ('no = (@ [3 #('f 4)] [n {'f n}] 'hit 'no))
 ('(1 1 1) = (@ '(1 1 1) [a a a] [a a a] 0))      ; ..and past two
 ('no = (@ '(1 1 2) [a a a] 'yes 'no))
 (2 = (@ '(1 1 2) [a a b] b 0))                    ; ..a fresh nom after a repeat still binds
 ; --- + : the split. the left run's LENGTH picks the split point, so nothing searches
 ('(3 4) = (@ '(1 2 3 4) ([a b] + t) t))
 (() = (@ '(1 2) ([a b] + t) t))              ; ..the rest may be empty and still fit
 ('no = (@ '(1) ([a b] + t) t 'no))           ; ..but the run may not come up short
 (10 = (@ '(1 2 3 4) ([a b] + [c d]) (a + b + c + d) 0))   ; ..either side is a pattern
 ('no = (@ '(1 2 3) ([a b] + [c d]) 'four 'no))
 (3 = (@ '(1 2 3 4) ([a b] + (c >< _)) c 0))  ; ..and it composes with the cons
 ('right = (@ 5 ([a] + t) 'no-chain 'right))
 ; ..which is how "this tag, then more" is said -- the loose arity, spelled
 ('fits = (@ '(lam x b) (['lam a b] + _) 'fits 'no))
 ('fits = (@ '(lam x b j k) (['lam a b] + _) 'fits 'no))
 ('no = (@ '(lam x) (['lam a b] + _) 'fits 'no))
 ('exact = (@ '(lam x b) ['lam a b] 'exact 'no)) ('no = (@ '(lam x b j) ['lam a b] 'exact 'no))
 ; --- a map: keys are EXPRESSIONS, values are patterns, the match is by subset
 (9 = (@ #('f 9) {'f x} x 0))
 (0 = (@ #('g 9) {'f x} x 0))
 (9 = (@ #('f 9 'g 1) {'f x} x 0))         ; subset: an extra key is fine
 (12 = (@ #('f 5 'g 7) {'f a 'g b} (a + b) 0))
 ('found = (@ #('f ()) {'f x} 'found 'no))  ; ⚠ PRESENCE, not truth: a pinned () is present
 ('no = (@ 5 {'f x} 'found 'no))
 (: k 'f (9 = (@ #('f 9) {k x} x 0)))      ; ...and a key is an expression, so it can be computed
 ; --- a tray, spelled with the sigil the space just gave back
 (5 = (@ @(2 3) @(a b) (a + b) 0))
 (0 = (@ @(2 3 4) @(a b) (a + b) 0))       ; exactly two
 (0 = (@ '(2 3) @(a b) (a + b) 0))
 (6 = (@ @(1 @(3 4)) @(a @(b c)) (b + c - a) 0))
 ; --- the shapes nest, and a nested literal still binds nothing
 ('hit = (@ ['k #('f 1)] ['k {'f 1}] 'hit 'no))    ; ⚠ the ` ctor: a quoted #(..) is a FORM, not a map
 ('no = (@ ['k #('f 2)] ['k {'f 1}] 'hit 'no)))

; ⚠ ONE LAW IS UNGATED: an unknown tag scares at EXPAND time, and the only way to
; defer an expansion into a test is (ev '(@ 1 (bogus a) a)) under a trap -- which
; costs love0 its second compiler pass. it is not this macro's doing: three lines,
;   (:: 'zz (\ a (scare 'zz (cap a))))
;   ('zz = (trap (\ _ (ev '(zz 1))) () (\ a b a)))
; print ONE "tests pass" where a scare from EVALUATED code under the same trap
; prints two. the law holds by hand; it waits on that lane.
