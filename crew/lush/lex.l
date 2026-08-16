; --- lush 🐚 the lexer -- one line (or a joined continuation) to tokens ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-lex s) -> (1 . tokens) | ('more . why) -- 'more is the PS2 protocol: an
; unfinished shape (open quote, open $( ), trailing \, a heredoc still owed
; its body) asks the caller for another line; join with "\n" and re-lex the
; whole buffer. tokens:
;   ('w . segs)          a WORD as a segment list, quote structure kept:
;     ('lit . s)           unquoted run (glob chars live, $ already carved out)
;     ('sq . s)            '..' literal
;     ('dq . inner)        ".." -- inner is lit/var/sub/vop segs, splitting off
;     ('var . name)        $NAME / $? $# $@ $* $$ / $0-9 (one digit, POSIX law)
;     ('vop . (name op text))  ${name<op>text}: op ":-" ":=" ":?" ":+" (and the
;                          colonless four), "#" "##" "%" "%%", "len" (${#x});
;                          text raw, expanded at eval through sh-dollarize
;     ('sub . text)        $( text ) or `text`, run at expansion time
;     ('lov . text)        ${( form )}: a LOVE form, evaluated at expansion time
;                          in this image. `text` is the parenthesized datum, the
;                          reader's own extent (sh-lovep). ONE field always --
;                          never IFS-split, never globbed, quiet like "${x}"
;   ('op . o)            | || & && ; ;; ( ) and "nl" (a newline is a separator,
;                        NOT whitespace)
;   ('rd . (ion . o))    a redirect: < > >> >& <& glued or spaced; ion is the
;                        io-number OUT OF BAND -- () default, (n) attached.
;                        a HEREDOC lexes as op "<h" whose target word IS the
;                        body: << collects the following lines to the delimiter
;                        and rewrites -- 'EOF' body lands ('sq . body), a bare
;                        EOF body ('dq . (sh-dollarize body)) so $ expands and
;                        nothing splits; <<- strips leading tabs. eval feeds
;                        the target through a pipe (the "<h" lane in rdmap).
; a # at a word START kills the rest of the line; \X is X literal;
; \<newline> vanishes (the join law); "" keeps \ escapes for $ ` " \ only.

(: (lx-ws? b) (b = 32 || b = 9)
   (sh-digit? b) (48 <= b < 58)
   (sh-digits? s)
     (: n (tally s)
        (go i) (? (i = n) (0 < n) (sh-digit? (s i)) (go (i + 1)) 0)
        (go 0))
   (sh-name? b) (sh-digit? b || sh-name1? b)
   (sh-name1? b) (65 <= b < 91 || 97 <= b < 123 || b = 95)
   (sh-alook al k) (al @ (e >< r) (<e = k ? >e (sh-alook r k)) ())

   ; --- the $-machinery, top level (heredoc bodies + vop words re-enter it) ---
   ; ⚠ a (payload . j) answer is read by KIND (two?), NEVER by truth. The payload
   ; may be EMPTY -- "" , `` , a heredoc owed no lines -- and an empty one nets
   ; zero with the improper tail out of the sum, so the whole pair reads red: `!r`
   ; then calls a finished parse unfinished and asks PS2 for a line nobody owes.
   ; An index answer (sh-subp, sh-brace) is past its opener, so it cannot be 0.
   ; ⚠ THE TWO SCANNERS BELOW STEP OVER A ${( .. )}, they never scan through one.
   ; A love body runs on love's lexical law -- a ' is a sigil, not a quote -- so a
   ; shell-quoting walk would open a string at the sigil and take the $( ) or the
   ; ${ } to the wrong end, or to none. Handing the span to sh-lovep keeps the
   ; nesting counts honest ({ and } skipped together, ( and ) with them). Inside
   ; '..' nothing expands, so the arm sits after that guard and never fires there.
   (sh-love3? s n i) ((i + 2) < n && s i = 36 && s (i + 1) = 123 && s (i + 2) = 40)
   ; $( body ): balanced parens, blind to quoted ones -> j past ) | ()
   ; ⚠ a per-byte loop, so it repeats its own name rather than close over the
   ; state in a helper -- the cost note lives on `lx` at the foot of this file
   (sh-subp s n i depth q pe)
     (? (i = n) ()
        pe (sh-subp s n (i + 1) depth q 0)
        (: b (s i)
           (? (q = 39) (sh-subp s n (i + 1) depth (b = 39 ? 0 39) 0)
              (b = 92) (sh-subp s n (i + 1) depth q 1)
              (sh-love3? s n i)
                (: r (sh-lovep s n (i + 2))
                   (sh-subp s n (two? r ? >r (i + 1)) depth q 0))
              (q = 34) (sh-subp s n (i + 1) depth (b = 34 ? 0 34) 0)
              (b @ 39 (sh-subp s n (i + 1) depth 39 0)
                     34 (sh-subp s n (i + 1) depth 34 0)
                     40 (sh-subp s n (i + 1) (depth + 1) 0 0)
                     41 (depth = 1 ? (i + 1) (sh-subp s n (i + 1) (depth - 1) 0 0))
                     _  (sh-subp s n (i + 1) depth 0 0)))))
   ; `...` -> (text . j-past) | ()
   (sh-btesc? b) (b = 96 || b = 92 || b = 36)
   (sh-bt s n i acc)
     (? (i = n) ()
        (: b (s i)
           (? (b = 96) (acc >< (i + 1))
              (b = 92 && (i + 1) < n && sh-btesc? (s (i + 1)))
                (sh-bt s n (i + 2) (acc + (s (i + 1))))
              (sh-bt s n (i + 1) (acc + b)))))
   ; ${(FORM)}: the LOVE arm. i is at the ( -- and THE READER IS THE SCANNER.
   ; `sound` speaks love's lexical law, which no shell scanner does: a ' is a
   ; sigil and not a quote, a ; runs to the line's end, a ) inside a "" is text,
   ; and #( [ ] \ all read as themselves. What it leaves unread measures the
   ; extent, so the form's end is the reader's own answer rather than a second
   ; opinion about it -- and the one that evaluates the form later is this one.
   ; -> (text . j past the }) | 'torn unfinished (PS2's cue) | () no } in place
   (sh-lovews s n k) ((k < n && lx-ws? (s k)) ? (sh-lovews s n (k + 1)) k)
   (sh-lovep s n i)
     (sound (snip s i n) @ (_ >< rest)
        (: k (n - (tally rest))                  ; past the datum, in s's own index
           j (sh-lovews s n k)
           (? (n <= j) 'torn
              (s j = 125) ((snip s i k) >< (j + 1))
              ()))
        'torn)                                   ; 'torn, and a bodyless () with it
   ; ${ ... }: to the matching }, blind to quoted braces -> j past } | ()
   (sh-brace s n i depth q)
     (? (i = n) ()
        (: b (s i)
           (? (q = 39) (sh-brace s n (i + 1) depth (b = 39 ? 0 39))
              (sh-love3? s n i)
                (: r (sh-lovep s n (i + 2))
                   (sh-brace s n (two? r ? >r (i + 1)) depth q))
              (q = 34) (sh-brace s n (i + 1) depth (b = 34 ? 0 34))
              (b @ 39  (sh-brace s n (i + 1) depth 39)
                     34  (sh-brace s n (i + 1) depth 34)
                     123 (sh-brace s n (i + 1) (depth + 1) 0)
                     125 (depth = 1 ? (i + 1) (sh-brace s n (i + 1) (depth - 1) 0))
                     _   (sh-brace s n (i + 1) depth 0)))))
   ; the ${..} content -> a var/vop seg | () bad shape
   (sh-vspec1? b) (b = 64 || b = 42 || b = 35 || b = 63 || b = 36 || sh-digit? b)
   (sh-vopcol? b) (b = 45 || b = 61 || b = 63 || b = 43)      ; the four - = ? + ops
   (sh-vopp c)
     (: n (tally c)
        (dbl? i b) ((i + 1) < n && c (i + 1) = b)             ; ## or %% at i
        (? !n ()
           (c 0 = 35 && 1 < n) ('vop >< [(snip c 1 n) "len" ""])            ; ${#x}
           (: nm (? (sh-vspec1? (c 0)) (("" + (c 0)) >< 1)
                    (sh-name1? (c 0))
                      (: (go i) ((i < n && sh-name? (c i)) ? (go (i + 1)) i)
                         j (go 1) (snip c 0 j >< j))
                    ())
              (nm @ (name >< i)
                 (? (i = n) ('var >< name)
                    (: b (c i)
                       op (? (b = 58 && (i + 1) < n && sh-vopcol? (c (i + 1)))
                                (":" + ("" + (c (i + 1))) >< (i + 2))
                             (sh-vopcol? b)  (("" + b) >< (i + 1))
                             (b = 35) (dbl? i 35 ? ("##" >< (i + 2)) ("#" >< (i + 1)))
                             (b = 37) (dbl? i 37 ? ("%%" >< (i + 2)) ("%" >< (i + 1)))
                             ())
                       (op @ (o >< j) ('vop >< [name o (snip c j n)]) ())))
                 ()))))
   ; a $-form at i (past the $) -> (seg . j) | ('dollar . i) literal | 'unterm
   (sh-dol s n i)
     (? (i = n) ('dollar >< i)
        (: b (s i)
           (spec nm) (('var >< nm) >< (i + 1))    ; the one-byte specials, $? $@ ..
           (b @ 63 (spec "?") 64 (spec "@") 42 (spec "*")
                 35 (spec "#") 36 (spec "$") 33 (spec "!")
              40 (: e (sh-subp s n (i + 1) 1 0 0)
                    (? !e 'unterm (('sub >< snip s (i + 1) (e - 1)) >< e)))
              ; ${( -- the love arm takes the extent off the READER, never off
              ; sh-brace, which counts a } inside a love "" or after a ; and would
              ; cut the form in the wrong place
              123 (? ((i + 1) < n && s (i + 1) = 40)
                     (: r (sh-lovep s n (i + 1))
                        (? !(two? r) 'unterm (('lov >< <r) >< >r)))
                     (: e (sh-brace s n (i + 1) 1 0)
                        (? !e 'unterm
                           (: sg (sh-vopp (snip s (i + 1) (e - 1)))
                              (? !sg 'unterm (sg >< e))))))
              _ (? (sh-digit? b) (spec ("" + b))   ; $0-9, ONE digit (the POSIX law)
                   (sh-name1? b)
                     (: (go j) ((j < n && sh-name? (s j)) ? (go (j + 1)) j)
                        e (go i) (('var >< snip s i e) >< e))
                   ('dollar >< i)))))
   ; a bare text (heredoc body, vop word) -> segs, $ and ` live, \ escapes $ ` \
   (sh-dollarize s)
     (: n (tally s)
        (addl cur segs) (tally cur ? (('lit >< cur) >< segs) segs)
        (go i cur segs)
          (? (i = n) (sh-rev (addl cur segs))
             (: b (s i)
                (? (b = 92 && (i + 1) < n && sh-btesc? (s (i + 1)))
                     (go (i + 2) (cur + (s (i + 1))) segs)
                   ; ⚠ 'dollar FIRST: a bare $ answers ('dollar . i), which a plain
                   ; pair pattern would take for a segment and splice as one
                   (b = 36) (sh-dol s n (i + 1) @ ('dollar >< j) (go j (cur + "$") segs)
                               (sg >< j) (go j "" (sg >< addl cur segs))
                               _ (go (i + 1) (cur + "$") segs))
                   (b = 96) (sh-bt s n (i + 1) "" @ (t >< j)
                               (go j "" (('sub >< t) >< addl cur segs))
                               _ (go (i + 1) (cur + b) segs))
                   (go (i + 1) (cur + b) segs))))
        (go 0 "" ()))
   ; vop word text -> segs, QUOTE-AWARE: ${x:-"a b"} carries its quotes as
   ; quotes ('sq/'dq segments), so use-time removal is free and a quoted glob
   ; char masks inert in ${v#pat}. heredoc bodies stay on sh-dollarize (no
   ; quote removal there). an unterminated quote falls back to a literal char.
   (sh-wordize s)
     (: n (tally s)
        (addl cur segs) (tally cur ? (('lit >< cur) >< segs) segs)
        ; ".." inner: $ ` live, \ escapes $ ` \ ", " closes -> (segs . j) | ()
        (dq i cur segs)
          (? (i = n) ()
             (: b (s i)
                (? (b = 34) (sh-rev (addl cur segs) >< (i + 1))
                   (b = 92 && (i + 1) < n && (sh-btesc? (s (i + 1)) || s (i + 1) = 34))
                     (dq (i + 2) (cur + (s (i + 1))) segs)
                   (b = 36) (sh-dol s n (i + 1) @ ('dollar >< j) (dq j (cur + "$") segs)
                               (sg >< j) (dq j "" (sg >< addl cur segs))
                               _ (dq (i + 1) (cur + "$") segs))
                   (b = 96) (sh-bt s n (i + 1) "" @ (t >< j)
                               (dq j "" (('sub >< t) >< addl cur segs))
                               _ (dq (i + 1) (cur + b) segs))
                   (dq (i + 1) (cur + b) segs))))
        (go i cur segs)
          (? (i = n) (sh-rev (addl cur segs))
             (: b (s i)
                (? (b = 39) (: (cl j acc) (? (j = n) (go (i + 1) (cur + b) segs)
                                             (s j = 39) (go (j + 1) "" (('sq >< acc) >< addl cur segs))
                                             (cl (j + 1) (acc + (s j))))
                              (cl (i + 1) ""))
                   (b = 34) (dq (i + 1) "" () @ (inner >< j)
                               (go j "" (('dq >< inner) >< addl cur segs))
                               _ (go (i + 1) (cur + b) segs))
                   (b = 92 && (i + 1) < n)
                     (go (i + 2) (cur + (s (i + 1))) segs)
                   (b = 36) (sh-dol s n (i + 1) @ ('dollar >< j) (go j (cur + "$") segs)
                               (sg >< j) (go j "" (sg >< addl cur segs))
                               _ (go (i + 1) (cur + "$") segs))
                   (b = 96) (sh-bt s n (i + 1) "" @ (t >< j)
                               (go j "" (('sub >< t) >< addl cur segs))
                               _ (go (i + 1) (cur + b) segs))
                   (go (i + 1) (cur + b) segs))))
        (go 0 "" ()))

   ; --- heredoc collection ------------------------------------------------------
   ; the delimiter, right after << : 'EOF' / "EOF" (quoted: body literal) or a
   ; bare run -> (quoted delim . j) | ()
   (sh-hddelim s n i)
     (? (i = n) ()
        (: b (s i)
           (? (b = 39 || b = 34)
                (: (go j acc) (? (j = n) ()
                                 (s j = b) (1 >< (acc >< (j + 1)))
                                 (go (j + 1) (acc + (s j))))
                   (go (i + 1) ""))
              (: (bare? x) !(lx-ws? x || x = 10 || x = 124 || x = 38 || x = 59
                             || x = 60 || x = 62 || x = 40 || x = 41)
                 (go j acc) ((j < n && bare? (s j)) ? (go (j + 1) (acc + (s j)))
                                                      (acc >< j))
                 r (go i "")
                 (tally <r ? (0 >< r) ())))))
   (sh-tabstrip l) (: (go i) ((i < (tally l) && l i = 9) ? (go (i + 1)) i)
                      (snip l (go 0) (tally l)))
   (sh-lineend s n j) (: (go i) ((i = n || s i = 10) ? i (go (i + 1))) (go j))
   ; body lines from j until the delimiter line -> (body . resume-j) | ()
   (sh-hdlines s n j strip delim)
     (: (go j2 acc)
          (? (n <= j2) ()                              ; the delimiter never came
             (: e (sh-lineend s n j2)
                line (snip s j2 e)
                l2 (strip ? (sh-tabstrip line) line)
                nj (e < n ? (e + 1) e)
                (? (l2 = delim) (acc >< nj)
                   (e = n) ()                          ; last line, no newline, not it
                   (go nj (acc + (l2 + "\n"))))))
        (go j ""))

   (sh-lex s)
   (: n (tally s)
      (addl cur segs) (tally cur ? (('lit >< cur) >< segs) segs)
      (fw cur started segs toks)                    ; flush the word in progress
        (: sg (addl cur segs)
           ((started || two? sg) ? (('w >< sh-rev sg) >< toks) toks))
      ; substitute each heredoc placeholder word with its collected body
      (fixw t bods)
        ((id? <t 'w && two? >t && id? <<>t 'hd)
         ? (: e (sh-alook bods ><>t)
              ('w >< (<e ? [('sq >< >e)] [('dq >< sh-dollarize >e)])))
           t)
      (fin toks hq bods)
        (two? hq ? ('more >< 'heredoc)
                   (1 >< map (\ t (fixw t bods)) (sh-rev toks)))
      ; '...' -> index of the closing quote | ()
      (sq j) (? (j = n) () (s j = 39) j (sq (j + 1)))
      (esc? b) (b = 34 || b = 36 || b = 92 || b = 96)
      (peek? i b) ((i + 1) < n && s (i + 1) = b)
      ; ".." -> (segs . j-past) | () unfinished
      (dq i cur segs)
        (? (i = n) ()
           (: b (s i)
              (? (b = 34) (sh-rev (addl cur segs) >< (i + 1))
                 (b = 92 && peek? i 10) (dq (i + 2) cur segs)
                 (b = 92 && (i + 1) < n && esc? (s (i + 1)))
                   (dq (i + 2) (cur + (s (i + 1))) segs)
                 (b = 36) (sh-dol s n (i + 1) @ ('dollar >< j) (dq j (cur + "$") segs)
                             (sg >< j) (dq j "" (sg >< addl cur segs))
                             _ ())
                 (b = 96) (sh-bt s n (i + 1) "" @ (t >< j)
                             (dq j "" (('sub >< t) >< addl cur segs))
                             _ ())
                 (dq (i + 1) (cur + b) segs))))
      ; hq/bods/k are the HEREDOC state, and only the << arms move them.
      ; ⚠ NO PER-STEP HELPER CLOSURES IN THIS LOOP OR IN sh-subp / sh-brace,
      ; however much one would shorten these call tails. Each runs once per BYTE,
      ; and a `:`-bound lambda in the body is built and dropped on every pass:
      ; four of them here measured +57% instructions over a whole lex. The state
      ; rides in the parameters, which is why `op2` takes nine of them.
      (op2 i cur started segs toks hq bods k o adv)  ; flush + emit an operator
        (lx (i + adv) "" 0 () (('op >< o) >< fw cur started segs toks) hq bods k)
      (lx i cur started segs toks hq bods k)
        (? (i = n) (fin (fw cur started segs toks) hq bods)
           (: b (s i)
              (? (lx-ws? b) (lx (i + 1) "" 0 () (fw cur started segs toks) hq bods k)
                 (b = 10)
                   (: tks (('op >< "nl") >< fw cur started segs toks)
                      (? !(two? hq) (lx (i + 1) "" 0 () tks hq bods k)
                         (: (coll j q bs)                 ; owed bodies, in arrival order
                              (q @ (h >< rest)
                                 (sh-hdlines s n j (sh-nth h 1) (sh-nth h 3) @ (body >< j2)
                                    (coll j2 rest ((sh-nth h 0 >< (sh-nth h 2 >< body)) >< bs))
                                    _ ())
                                 (j >< bs))
                            (coll (i + 1) (sh-rev hq) bods @ (j >< bs)
                               (lx j "" 0 () tks () bs k)
                               _ ('more >< 'heredoc)))))
                 ; a comment runs to the END OF ITS LINE, never the end of the
                 ; input -- the buffer is multi-line whenever PS2/'more gathered
                 ; (the nl itself still lexes, so heredoc bodies keep collecting)
                 (b = 35 && !started)
                   (: (sk j) ((j < n && !(s j = 10)) ? (sk (j + 1)) j)
                      (lx (sk (i + 1)) "" 0 () toks hq bods k))
                 (b = 39) (: j (sq (i + 1))
                             (? !j ('more >< 'quote)
                                (lx (j + 1) "" 1 (('sq >< snip s (i + 1) j) >< addl cur segs) toks hq bods k)))
                 (b = 34) (dq (i + 1) "" () @ (inner >< j)
                             (lx j "" 1 (('dq >< inner) >< addl cur segs) toks hq bods k)
                             _ ('more >< 'quote))
                 (b = 36) (sh-dol s n (i + 1) @ ('dollar >< j) (lx j (cur + "$") 1 segs toks hq bods k)
                             (sg >< j) (lx j "" 1 (sg >< addl cur segs) toks hq bods k)
                             _ ('more >< 'sub))
                 (b = 96) (sh-bt s n (i + 1) "" @ (t >< j)
                             (lx j "" 1 (('sub >< t) >< addl cur segs) toks hq bods k)
                             _ ('more >< 'sub))
                 (b = 92) (? ((i + 1) = n) ('more >< 'cont)
                             (s (i + 1) = 10) (lx (i + 2) cur started segs toks hq bods k)
                             (lx (i + 2) (cur + (s (i + 1))) 1 segs toks hq bods k))
                 (b = 60 || b = 62)
                   (: ion (!(two? segs) && sh-digits? cur ? [(sh-int cur)] ())
                      tks (two? ion ? toks (fw cur started segs toks))
                      o (b = 62 ? (? (peek? i 62) ">>" (peek? i 38) ">&" ">")
                                  (? (peek? i 60) "<<" (peek? i 38) "<&" "<"))
                      (? !(o = "<<")
                         (lx (i + (tally o)) "" 0 () (('rd >< (ion >< o)) >< tks) hq bods k)
                         (: strip ((i + 2) < n && s (i + 2) = 45)
                            (skip j2) ((j2 < n && lx-ws? (s j2)) ? (skip (j2 + 1)) j2)
                            j2 (skip (i + (strip ? 3 2)))
                            dl (sh-hddelim s n j2)
                            (? !dl ('more >< 'heredoc)
                               (lx >>dl "" 0 ()
                                   (('w >< [('hd >< k)])
                                    >< (('rd >< (ion >< "<h")) >< tks))
                                   [k (strip ? 1 0) <dl <>dl >< hq]
                                   bods (k + 1))))))
                 ; || && ;; each double their own byte, so one peek serves all three
                 (b = 124 || b = 38 || b = 59)
                   (peek? i b ? (op2 i cur started segs toks hq bods k
                                     (("" + b) + ("" + b)) 2)
                                (op2 i cur started segs toks hq bods k ("" + b) 1))
                 (b = 40) (op2 i cur started segs toks hq bods k "(" 1)
                 (b = 41) (op2 i cur started segs toks hq bods k ")" 1)
                 (lx (i + 1) (cur + b) 1 segs toks hq bods k))))
      (lx 0 "" 0 () () () () 0)))
