; --- love/p1.l: THE READER IN LOVE (doc/reader.md rung 6) -------------------
;
; the grammar p0 does not take: the operator run and its valence law, the
; constructor wraps with their splice rewrites, the comma datum, the bracket
; synonyms, the number tower. p0 (love.c's sound0) reads the pure lisp subset
; and reads THIS FILE; p1 reads ev.l, and after the flip it reads everything.
;
; ⚠ P1.L IS HELD TO THE P0 SUBSET, exactly as prel.l is (rung 4) and for the
; same reason -- p0 is what reads it. so: no ` # @ ~ wraps, no comma, no
; brackets, and NEVER A GLUED OPERATOR RUN. `!x` and `<c` are one symbol to p0
; and (mono ! x) / (mono < c) to the reader this file is replacing, and the
; PRINTER SHOWS BOTH THE SAME -- only `=` on two trees can see it. space every
; operator; say (nil? x) and (cap x) for the words.
;
; ⚠ AND IT IS PRE-OPFIX, so NO INFIX either -- prel.l's rule, and the same one:
; the boot stitch (rung 6b) evals this file to make it callable BEFORE prel
; runs, so `opfix` does not exist yet and an infix form would compile as a plain
; application -- silently, church-exponentiating where a comparison was meant.
; prefix nifs only. it is also PRE-PREL: nifs and its own bindings, nothing
; else, no prel helpers and no macros.
;
; the protocol is the historical one (2efbaa51^:g/repl.g) and NOT a port: read1
; walks a charlist and answers the value CONSED ONTO WHAT IS LEFT, () for no
; datum (end of text, or a stray close), or `torn` for a shape that did not
; finish. the residue IS the answer, which is what lets a caller resume a torn
; read by handing back more text -- there is no port to stash it in.
;
; ⚠ ASK `two?` FIRST, never the net: a cons is the one structural answer, and
; testing (nil? d) first would call a perfectly good (datum . rest) a clean end
; whenever the pair happens to net 0.
; ⚠ AND `torn` IS A NAMED SYMBOL, not a (nom 0). a nameless mint sits on the BLUE
; FLOOR -- it nets nothing, so (nil? torn) is TRUE and the torn answer reads as a
; clean end, silently, on exactly the inputs that matter. named, it nets its
; spelling, stays un-two, prints as itself in a scare, and `id?` is exact.

(: torn 'torn

   ; --- char classes (love.c's op_break / ioread1sym terminators) -----------
   (p1-in? s c) (: n (tally s)
                   (go k) (? (<= n k) () (= (s k) c) 1 (go (+ k 1)))
                   (go 0))
   ; ⚠ 12 IS SPELLED OUT, here and in every class below: love's escapes are
   ; n t r e 0 x and NOTHING ELSE, so "\f" in a love string is the LETTER f --
   ; which would quietly make f whitespace and eat it out of `ieee-inf`.
   (p1-ws? c)   (? (= c 12) 1 (p1-in? " \t\n\r" c))
   (p1-dig? c)  (? (<= 48 c) (<= c 57) ())
   (p1-hex? c)  (? (p1-dig? c) 1 (? (<= 97 (p1-low c)) (<= (p1-low c) 102) ()))
   (p1-low c)   (? (? (<= 65 c) (<= c 90) ()) (+ c 32) c)
   ; op_break complemented: the chars that FUSE into one operator run.
   (p1-opc? c)  (p1-in? "!$%&*+-./:<=>?\\^|" c)
   ; a name-led token ends here (love.c's ioread1sym switch). ⚠ ' is absent --
   ; a name keeps a trailing/internal prime (x', n''); a LEADING ' is quote and
   ; never reaches the token lane.
   (p1-tend? c) (? (= c 12) 1 (p1-in? " \t\n\r;#()[]{}\"`," c))

   ; --- whitespace and comments --------------------------------------------
   ; `;` runs to end of line; `#!` does too; a bare `#` is the hash wrap.
   (p1-eol cl) (? (nil? (two? cl)) cl
                  (p1-in? "\n\r" (cap cl)) (cup cl)
                  (p1-eol (cup cl)))
   (p1-skip cl)
     (? (nil? (two? cl)) cl
        (: c (cap cl)
           (? (p1-ws? c) (p1-skip (cup cl))
              (= c 59) (p1-skip (p1-eol (cup cl)))
              (? (= c 35) (? (two? (cup cl)) (= (cap (cup cl)) 33) ()) ())
                (p1-skip (p1-eol (cup (cup cl))))
              cl)))

   ; --- integers, at full precision ----------------------------------------
   ; love's + and * promote charm -> sun -> big by VALUE, so the tower is the
   ; arithmetic's and there is no width to get wrong (rung 2's law).
   (p1-dval c) (? (p1-dig? c) (- c 48) (- (p1-low c) 87))
   (p1-digits cl radix n)
     (? (nil? (two? cl)) n
        (p1-digits (cup cl) radix (+ (* n radix) (p1-dval (cap cl)))))
   ; the three predicates, love.c's is_dec_int / is_hex_int / is_oct_int. they
   ; are EXHAUSTIVE over what a base-0 strtol took whole, which is why a token
   ; that is none of them (0x, 08, 1e5, abc) goes on to the float lane.
   (p1-all? p cl) (? (nil? (two? cl)) 1 (p (cap cl)) (p1-all? p (cup cl)) ())
   (p1-oct? c)  (? (<= 48 c) (<= c 55) ())
   (p1-dec-int? cl)
     (? (nil? (two? cl)) ()
        ; a leading zero belongs to octal, below -- but "0" alone is decimal
        (? (= (cap cl) 48) (two? (cup cl)) ()) ()
        (p1-all? p1-dig? cl))
   (p1-hex-int? cl)                            ; [+-]?0[xX]<hex>+, sign peeled
     (? (nil? (two? cl)) ()
        (nil? (= (cap cl) 48)) ()
        (nil? (two? (cup cl))) ()
        (nil? (= (p1-low (cap (cup cl))) 120)) ()
        (nil? (two? (cup (cup cl)))) ()         ; a bare 0x is an honest symbol
        (p1-all? p1-hex? (cup (cup cl))))
   (p1-oct-int? cl)                            ; [+-]?0<oct>+, the zero the prefix
     (? (nil? (two? cl)) ()
        (nil? (= (cap cl) 48)) ()
        (nil? (two? (cup cl))) ()
        (p1-all? p1-oct? (cup cl)))
   ; a token charlist -> (1 . its number), or () if it is not one. the sign is
   ; peeled first so every base agrees about it.
   ; ⚠ THE WRAPPER IS THE PRESENCE, NEVER THE NET. -5 and 0 both net to nothing,
   ; so a bare answer tested with nil? calls every negative and every zero "not
   ; a number" -- silently, and only for the literals most likely to matter.
   (p1-int cl)
     (: sgn (? (nil? (two? cl)) 0 (= (cap cl) 45) -1 (= (cap cl) 43) 1 0)
        body (? (= sgn 0) cl (cup cl))
        mag (? (p1-dec-int? body) (link 1 (p1-digits body 10 0))
               (p1-hex-int? body) (link 1 (p1-digits (cup (cup body)) 16 0))
               (p1-oct-int? body) (link 1 (p1-digits (cup body) 8 0))
               ())
        (? (nil? (two? mag)) ()
           (< sgn 0) (link 1 (- 0 (cup mag)))
           mag))

   ; --- the token lanes -----------------------------------------------------
   (p1-tok cl acc)                                   ; a name-led token
     (? (nil? (two? cl)) (link (p1-rev acc ()) cl)
        (p1-tend? (cap cl)) (link (p1-rev acc ()) cl)
        (p1-tok (cup cl) (link (cap cl) acc)))
   (p1-rev l acc) (? (two? l) (p1-rev (cup l) (link (cap l) acc)) acc)
   ; an OPERATOR RUN: a maximal run of operator chars, read as one plain symbol
   ; and factored later by opfix. ⚠ a trailing '-' right before a digit is the
   ; next number's sign, not part of the run -- the run sheds it and the '-' is
   ; put back, so `!-5` is ! then -5.
   (p1-run cl acc)
     (? (? (two? cl) (p1-opc? (cap cl)) ())
        (p1-run (cup cl) (link (cap cl) acc))
        ; the run ended: shed a trailing '-' if a digit follows and the run has
        ; more than that '-' to it, and hand the '-' back for the number lane.
        (: shed (? (nil? (two? acc)) ()
                   (nil? (= (cap acc) 45)) ()
                   (nil? (two? (cup acc))) ()
                   (nil? (two? cl)) ()
                   (p1-dig? (cap cl)))
           (? shed (link (p1-rev (cup acc) ()) (link 45 cl))
              (link (p1-rev acc ()) cl))))

   ; --- an atom: integer, then the named floats, then float, then symbol -----
   ; ⚠ and the float lane asks the TYPE, not the value: gem answers nil on a
   ; token it could not parse, and nil is the charm 0 -- indistinguishable by
   ; net from the float 0.0 it answers for "0.0". gem? separates them.
   (p1-atom tok)
     (: n (p1-int tok)
        s (string tok)
        (? (two? n) (cup n)
           (= s "ieee-inf") (/ 1.0 0.0)
           (= s "-ieee-inf") (/ -1.0 0.0)
           (: lead (? (nil? (two? tok)) 0
                      (p1-in? "+-" (cap tok)) (? (two? (cup tok)) (cap (cup tok)) 0)
                      (cap tok))
              (? (? (p1-dig? lead) 1 (= lead 46))
                 (: f (gem s) (? (gem? f) f (intern s)))
                 (intern s)))))

   ; --- strings -------------------------------------------------------------
   (p1-esc c) (? (= c 110) 10 (= c 116) 9 (= c 114) 13 (= c 101) 27 (= c 48) 0 c)
   (p1-str cl acc)
     (? (nil? (two? cl)) torn
        (: c (cap cl) r (cup cl)
           ; ⚠ "" is the EMPTY STRING, and (string ()) is not it -- string is
           ; identity on the zero point, so an empty accumulator has to name the
           ; literal to get the object the C reader hands back.
           (? (= c 34) (link (? (two? acc) (string (p1-rev acc ())) "") r)
              (nil? (= c 92)) (p1-str r (link c acc))
              (nil? (two? r)) torn
              (= (cap r) 120)                              ; \xHH
                (? (nil? (? (two? (cup r)) (two? (cup (cup r))) ())) torn
                   (p1-str (cup (cup (cup r)))
                           (link (+ (* (p1-dval (cap (cup r))) 16)
                                    (p1-dval (cap (cup (cup r))))) acc)))
              (p1-str (cup r) (link (p1-esc (cap r)) acc)))))

   ; --- the wraps -----------------------------------------------------------
   ; the value surface, the printer's read-back contract: ' ` # @ ~ each wrap
   ; the next datum. a SPLICING wrap (hash tuple twin list) over a list splices
   ; instead: #(k v) is (hash k v), not (hash (k v)).
   (p1-splice? w) (? (= w 'hash) 1 (? (= w 'tuple) 1 (? (= w 'twin) 1 (= w 'list))))
   (p1-wrap w d)
     (? (? (= d ()) (= w 'hash) ()) (link 'tablet (link 0 ()))     ; #() -> (tablet 0)
        (? (= d ()) (= w 'tuple) ()) (link 'iota (link 0 ()))      ; @() -> (iota 0)
        (? (p1-splice? w) (? (two? d) 1 (? (= d ()) (nil? (= w 'hash)) ())) ()) (link w d)
        (link w (link d ())))

   ; --- the parser ----------------------------------------------------------
   ; (p1-read1 cl head) -- head is true when the next datum would be a list's
   ; FIRST, which the valence law needs: the : and ? special forms consume the
   ; whole list, so a run glued at head does not fuse for those two.
   (p1-read1 cl0 head)
     (: cl (p1-skip cl0)
        (? (nil? (two? cl)) ()
           (: c (cap cl) r (cup cl)
              ; a numeral has to start somewhere (rung 1): + and - are operator
              ; runs like every other punctuation, EXCEPT that a digit -- or a
              ; `.`, for -.5 -- right after them makes the token a NUMBER.
              numlead (? (p1-in? "+-" c) (? (two? r) (? (p1-dig? (cap r)) 1 (= (cap r) 46)) ()) ())
              (? (p1-in? "([{" c) (p1-list r 1 ())
                 (p1-in? ")]}" c) ()
                 (= c 34) (p1-str r ())
                 (= c 39) (p1-lift (intern "\\") r head)
                 (= c 96) (p1-lift 'list r head)
                 (= c 35) (p1-lift 'hash r head)
                 (= c 64) (p1-lift 'tuple r head)
                 (= c 126) (p1-lift (? (? (two? r) (= (cap r) 40) ()) 'twin 'conj) r head)
                 (= c 44) (link (intern ",") r)
                 (? (p1-opc? c) (nil? numlead) ()) (p1-op cl head)
                 (: tr (p1-tok cl ())
                    (link (p1-atom (cap tr)) (cup tr)))))))

   ; a wrap takes the next datum; an unfinished one is MORE, never EOF.
   (p1-lift w cl head)
     (: d (p1-read1 cl head)
        (? (nil? (two? d)) torn
           (link (p1-wrap w (cap d)) (cup d))))

   ; THE VALENCE LAW, reader half: a run GLUED to the datum after it is MONADIC
   ; -- the run becomes a wrap UNDER a mono wrap, so the emission is the nested
   ; (mono (run d)), which opfix factors against book['monadics] -- and a spaced
   ; run is the plain symbol, dyadic by opfix's default. head
   ; fuses too, so (<x = y) is (= (cap x) y); the exceptions are \ (which never
   ; fuses, form space) and : / ? at head (they consume the whole list, where
   ; fusion would swallow only the next datum).
   (p1-op cl head)
     (: rr (p1-run cl ())
        run (string (cap rr))
        rest (cup rr)
        sym (intern run)
        shed (? (two? rest) (= (cap rest) 45) ())    ; p1-run put a sign back: glued
        (? (= run "\\") (link sym rest)
           (? head (? (= run ":") 1 (= run "?")) ()) (link sym rest)
           (nil? (? shed 1 (p1-glued? rest))) (link sym rest)
           (: d (p1-read1 rest ())
              (? (nil? (two? d)) torn
                 (link (link 'mono (link (link sym (link (cap d) ())) ())) (cup d))))))
   ; glued = the next char starts a datum rather than ending the form.
   (p1-glued? cl)
     (? (two? cl) (: c (cap cl) (nil? (? (= c 12) 1 (p1-in? " \t\n\r;)]}" c)))) ())

   ; a list: datums until the close. an empty one IS the zero point.
   (p1-list cl0 head acc)
     (: cl (p1-skip cl0)
        (? (nil? (two? cl)) torn
           (p1-in? ")]}" (cap cl)) (link (p1-rev acc ()) (cup cl))
           (: d (p1-read1 cl head)
              (? (nil? (two? d)) torn
                 (p1-list (cup d) () (link (cap d) acc))))))

   ; --- the doors -----------------------------------------------------------
   ; SOUND: one datum off a charlist, and the tree's whole reader. answers
   ; (datum . rest), () at a clean end, or `torn` mid-shape.
   (sound cl) (p1-read1 cl ())
   ; every datum in a text, in source order -- what the boot stitch wants.
   ; answers `torn` if any shape did not finish.
   (p1-reads cl)
     (: (go cl acc)
          (: d (p1-read1 cl ())
             (? (two? d) (go (cup d) (link (cap d) acc))
                (id? d torn) torn
                (p1-rev acc ())))
        (go cl ()))
   ; a text -> its charlist. built by index rather than by map/jot: those are
   ; prel's, and prel is not loaded when the boot calls this.
   (p1-chars s)
     (: n (tally s)
        (go k acc) (? (< k 0) acc (go (- k 1) (link (s k) acc)))
        (go (- n 1) ()))
   ; the BOOT'S door, whole text -> every form: what ai_egg_ calls to read ev.l.
   (p1-text s) (p1-reads (p1-chars s))

   ; --- and THIS `:` HAS A BODY, so the 33 above are LOCAL -----------------
   ; the three below are the reader; the rest is its scaffolding. a body-less `:`
   ; would leak all 33 into the book, and the egg cannot mop what it still reads.
   ; `pin book` is the only publisher here -- prel is not loaded, so no `use` yet.
   ; ⚠ `p1-text` is pinned under exactly that spelling: C resolves it BY NOM on
   ; every ai_evals_ (readtext), and a miss there is silent -- the boot falls back
   ; to p0 and reads the pure-lisp subset for the rest of the run.
   _ (pin book 'torn torn)
   _ (pin book 'sound sound)
   (pin book 'p1-text p1-text))
