; crew/kore/text.l -- the shared text layer: lines/unlines, the list<->text bridge every
; line tool rides (diff's udiff, the coming wc/head/tail). the convention: a line is a
; string WITHOUT its newline; `unlines` terminates every line, so the round trip is exact
; only on a newline-terminated text. ⚠ A MISSING FINAL NEWLINE IS DATA -- every GNU line
; tool preserves it, so a tool whose output can carry the source's LAST line asks
; `nl-end?` of its input and `chomp`s when the answer is no. a body-less top-level `:` (the wm's app shape); loads
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
       (go ls))
   ; the two that put back what `unlines` cannot know: did this text end in a newline,
   ; and drop one if it is there. ⚠ an empty text ends in nothing, so it answers 0 and
   ; chomps to itself -- neither is a special case anywhere above.
   (nl-end? s) (: n (tally s) ((0 < n) && (s (n - 1) = 10)))
   (chomp t) (: n (tally t) (? (nl-end? t) (snip t 0 (n - 1)) t)))
