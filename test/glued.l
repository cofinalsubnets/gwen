; multi-char glued sigils. the arity-0 lane keys on a word, so a row may be spelled
; with as many chars as it likes -- and a digit-led one (0? 1? 2?) is reachable too,
; because a digit-led token carrying a sigil reads as a run and not as a name.
; the factor walk is right to left: the sigil the reader glued to the datum is
; matched first, longest first. the reader half is `sound`, the factor half opfix.
(: (rdr s) (: r (sound s) (? (two? r) (cap r) r)))

(assert
 ; --- the reader: the run is cut one past the token's last sigil, digits and all
 ('(mono (2?<> x)) = (rdr "2?<>x"))
 ('(mono (1? x)) = (rdr "1?x"))
 ('(mono (2?<> (list 1 2))) = (rdr "2?<>[1 2]"))     ; and it fuses with the next datum
 ('(mono (2? -5)) = (rdr "2?-5"))                    ; a `-` before a digit still sheds
 ('2?<> = (rdr "2?<>"))                              ; nothing glued after it: the plain symbol
 ; the numeral lane is asked first, so a number is never a run
 (1.5 = (rdr "1.5")) (0.5 = (rdr "0.5")) (0.0015 = (rdr "1.5e-3")) (31 = (rdr "0x1f"))
 ; a name-led token keeps its sigils, as it always did
 ('a<>b = (rdr "a<>b"))

 ; --- the factor pass, and the value
 ('(two? (cap (cup x))) = (unglue (opfix '2?<>x)))
 ('(one? (cup x)) = (unglue (opfix '1?>x)))
 ('(two? -5) = (unglue (opfix '2?-5)))
 (1 = 2?[7 8]) (0 = 2?5) (0 = 2?<>[[1 2] 9]) (1 = 1?<>[[1 2] 9]) (1 = 0?[]) (0 = 0?[7]))

; --- right to left, and the one shape that can tell the directions apart: two rows
; overlapping mid-run. left to right would take <> then |; the walk takes >| then <.
(assert
 (: w1 (grip 0 '<> 'tally) w2 (grip 0 '>| 'net)
    tree (unglue (opfix '<>|x))
    _ (grip 0 '<> w1) _ (grip 0 '>| w2)
    (tree = '(cap (net x))))
 ; and it backtracks: |< is the longest suffix but its leftover & has no row, so the walk
 ; falls back to < and takes the &| that was waiting behind it.
 (: w1 (grip 0 '|< 'tally) w2 (grip 0 '&| 'net)
    tree (unglue (opfix '&|<x))
    _ (grip 0 '|< w1) _ (grip 0 '&| w2)
    (tree = '(net (cap x))))
 ; which is what keeps `grip 0` composable: without the fallback, coining ?< would unfactor
 ; 2?<x -- a run that has nothing to do with it -- and unfactoring is silent, since a run the
 ; table cannot spell has to fall through to the plain application.
 ('(two? (cap x)) = (unglue (opfix '2?<x)))
 (: w (grip 0 '?< 'tally) tree (unglue (opfix '2?<x)) _ (grip 0 '?< w)
    (tree = '(two? (cap x)))))                      ; ?< is preferred and cannot complete, so it yields

; --- the shadow law: a glued row is the function, not the word --------------
; the run factors to a VALUE, so nothing in scope can reach it: `<x` means cap wherever it
; is written, and a binding of `cap` is just a binding. this is the whole reason the glued
; lane differs from the spaced ones, where the sigil is the name and a shadow is visible in
; the text that uses it. a row `grip 0` puts there is whatever the caller handed over --
; a nom row resolves at the use site, as it always did.
(assert
 (1 = (: cap 5 ((\ v <v) '(1 2 3))))                ; the catastrophic one: <v stayed cap
 (5 = (: cap 5 cap))                                ; ..and the binding still stands
 (6 = (: net 'not-a-fn ((\ l +l) '(1 2 3))))         ; a love-closure row is reached the same way
 (1 = (: two? 0 ((\ l 2?l) '(1))))                   ; ..and one that is another nif's alias
 (2 = ((\ l (: cap <l cup >l (cap + tally cup))) '(1 2)))) ; the two names bound and the sigils used, one form

; --- opnom / unglue: the words, when a reader wants them --------------------
; a value head is a form no text can spell, so the map back is the door -- and every reader
; of post-opfix source (pat's pin, the glaze, spec2coq, wm2uu) goes through it.
(assert
 ('cap = (opnom cap)) ('jot = (opnom jot)) ('two? = (opnom two?)) ('nil? = (opnom nil?))
 (0 = (opnom 5)) (0 = (opnom 'cap)) (0 = (opnom '(1 2)))   ; a miss is 0; a list never reaches the hash
 ('(cap (cup x)) = (unglue (opfix '<>x)))
 ('(+ (f (abs x)) (cap (cup y))) = (unglue (opfix '(f |x + <>y))))
 ((opfix '(cap x)) = (unglue (opfix '<x)))          ; the two spellings meet at one tree
 ('(a (cap b)) = (unglue '(a (cap b))))             ; a tree of words is already unglued: idempotent
 ; and the printer says the word, so a glued body reads as the prefix one does
 ("(cap x)" = (show (opfix '<x)))
 ("(jot x)" = (show (opfix '^x)))
 ("(net x)" = (show (opfix '+x))))
