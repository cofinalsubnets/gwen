; crew/kore/text.l -- the shared text layer: lines/unlines, the list<->text bridge every
; line tool rides (diff's udiff, the coming wc/head/tail). the convention: a line is a
; string WITHOUT its newline; `unlines` terminates every line, so the round trip is exact
; on newline-terminated texts (a final unterminated line gains its newline -- the tool
; layer's one normalization). a body-less top-level `:` (the wm's app shape); loads
; BEFORE diff.l in the gate (udiff captures `lines` at its define -- the capture law).
(:
   ; text -> the list of its lines, split at charm 10
   (lines s)
    (: n (tally s)
       (go from k acc) (? (n <= k) (? (from < k) (rev (snip s from k >< acc)) (rev acc))
                          (s k = 10) (go (k + 1) (k + 1) (snip s from k >< acc))
                          (go from (k + 1) acc))
       (go 0 0 ()))
   ; the list of lines -> text, every line newline-terminated; an empty line is nothing
   ; and rides through say as the no-op it is
   (unlines ls)
    (: o (jug 0)
       (go m) (? (two? m) (: _ (say o <m) _ (put o 10) (go >m)) (slurp o))
       (go ls)))
