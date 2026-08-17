; MULTI-CHAR GLUED SIGILS. the arity-0 lane keys on a WORD, so a row may be spelled
; with as many chars as it likes -- and a digit-led one (0? 1? 2?) is reachable too,
; because a digit-led token carrying a sigil reads as a RUN and not as a name.
; the factor walk is RIGHT TO LEFT: the sigil the reader glued to the datum is
; matched first, longest first. the reader half is `sound`, the factor half opfix.
(: (rdr s) (: r (sound s) (? (two? r) (cap r) r)))

(assert
 ; --- the reader: the run is cut one past the token's LAST sigil, digits and all
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
 ('(two? (cap (cup x))) = (opfix '2?<>x))
 ('(one? (cup x)) = (opfix '1?>x))
 ('(two? -5) = (opfix '2?-5))
 (1 = 2?[7 8]) (0 = 2?5) (0 = 2?<>[[1 2] 9]) (1 = 1?<>[[1 2] 9]) (1 = 0?[]) (0 = 0?[7]))

; --- RIGHT TO LEFT, and the one shape that can tell the directions apart: two rows
; overlapping mid-run. left to right would take <> then |; the walk takes >| then <.
(assert
 (: w1 (grip 0 '<> 'tally) w2 (grip 0 '>| 'net)
    tree (opfix '<>|x)
    _ (grip 0 '<> w1) _ (grip 0 '>| w2)
    (tree = '(cap (net x))))
 ; and it BACKTRACKS: |< is the longest suffix but its leftover & has no row, so the walk
 ; falls back to < and takes the &| that was waiting behind it.
 (: w1 (grip 0 '|< 'tally) w2 (grip 0 '&| 'net)
    tree (opfix '&|<x)
    _ (grip 0 '|< w1) _ (grip 0 '&| w2)
    (tree = '(net (cap x))))
 ; which is what keeps `grip 0` composable: without the fallback, coining ?< would unfactor
 ; 2?<x -- a run that has nothing to do with it -- and unfactoring is SILENT, since a run the
 ; table cannot spell has to fall through to the plain application.
 ('(two? (cap x)) = (opfix '2?<x))
 (: w (grip 0 '?< 'tally) tree (opfix '2?<x) _ (grip 0 '?< w)
    (tree = '(two? (cap x)))))                      ; ?< is preferred and cannot complete, so it yields
