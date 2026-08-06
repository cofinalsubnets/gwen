; p1: real love parser; is read by and succeeds the love.c bootstrap parser p0.
; p0 does not parse the full love language, only the pure lisp subset. therefore
; this file may not use prefix or infix operators.
(: sound (: ; the input is a colist. thunk tails must be memoized or idempotent.
   (p1-cup cl) (: t (cup cl) (? (lit? t) (t ()) t))

   ; --- char classes (love.c's op_break / ioread1sym terminators) -----------
   ; --- THE CLASS TABLE ------------------------------------------------------
   ; ONE 256-byte string, a bitmask per char, so a membership test is an index
   ; and an `&` where it used to be a scan over a class string -- with a fresh
   ; closure and a `tally` minted per call, which is what made it the reader's
   ; whole cost (`p1-tend?` walked 15 chars for every character of every token).
   ; ⚠ BIT 0 IS LEFT UNUSED ON PURPOSE: a string indexed PAST ITS END answers the
   ; charm 1, so a char outside 0..255 would match whichever class held bit 0.
   ; with bit 0 empty, (& 1 m) is 0 for every mask below and an out-of-range
   ; charm matches NOTHING -- which is exactly what the scan it replaces said.
   ;   2 ws   4 opc   8 tend   16 open   32 close   64 sign   128 gstop
   (p1-cin? s c) (: n (tally s)
                    (go k) (? (<= n k) () (= (s k) c) 1 (go (+ k 1)))
                    (go 0))
   (p1-bits c) (+ (? (? (= c 12) 1 (p1-cin? " \t\n\r" c)) 2 0)
                (+ (? (p1-cin? "!$%&*+-./:<=>?\\^|" c) 4 0)
                (+ (? (? (= c 12) 1 (p1-cin? " \t\n\r;#()[]{}\"`," c)) 8 0)
                (+ (? (p1-cin? "([{" c) 16 0)
                (+ (? (p1-cin? ")]}" c) 32 0)
                (+ (? (p1-cin? "+-" c) 64 0)
                   (? (? (= c 12) 1 (p1-cin? " \t\n\r;)]}" c)) 128 0)))))))
   p1-cls (: (go k acc) (? (< k 0) (string acc) (go (- k 1) (link (p1-bits k) acc)))
             (go 255 ()))
   ; ⚠ 12 IS SPELLED OUT, here and in every class below: love's escapes are
   ; n t r e 0 x and NOTHING ELSE, so "\f" in a love string is the LETTER f --
   ; which would quietly make f whitespace and eat it out of `ieee-inf`.
   (p1-ws? c)   (& (p1-cls c) 2)
   (p1-dig? c)  (? (<= 48 c) (<= c 57) ())
   (p1-hex? c)  (? (p1-dig? c) 1 (? (<= 97 (p1-low c)) (<= (p1-low c) 102) ()))
   (p1-low c)   (? (? (<= 65 c) (<= c 90) ()) (+ c 32) c)
   ; op_break complemented: the chars that FUSE into one operator run.
   (p1-opc? c)  (& (p1-cls c) 4)
   ; a name-led token ends here (love.c's ioread1sym switch). ⚠ ' is absent --
   ; a name keeps a trailing/internal prime (x', n''); a LEADING ' is quote and
   ; never reaches the token lane.
   (p1-tend? c) (& (p1-cls c) 8)

   ; --- whitespace and comments --------------------------------------------
   ; `;` runs to end of line; `#!` does too; a bare `#` is the hash wrap.
   (p1-eol cl) (? (nil? (link? cl)) cl
                  (? (= (cap cl) 10) 1 (= (cap cl) 13)) (p1-cup cl)
                  (p1-eol (p1-cup cl)))
   (p1-skip cl)
     (? (nil? (link? cl)) cl
        (: c (cap cl)
           (? (p1-ws? c) (p1-skip (p1-cup cl))
              (= c 59) (p1-skip (p1-eol (p1-cup cl)))
              (? (= c 35) (? (link? (p1-cup cl)) (= (cap (p1-cup cl)) 33) ()) ())
                (p1-skip (p1-eol (p1-cup (p1-cup cl))))
              cl)))

   ; --- integers, at full precision ----------------------------------------
   ; love's + and * promote charm -> sun -> big by VALUE, so the tower is the
   ; arithmetic's and there is no width to get wrong (rung 2's law).
   (p1-dval c) (? (p1-dig? c) (- c 48) (- (p1-low c) 87))
   (p1-digits cl radix n)
     (? (nil? (link? cl)) n
        (p1-digits (cup cl) radix (+ (* n radix) (p1-dval (cap cl)))))
   ; the three predicates, love.c's is_dec_int / is_hex_int / is_oct_int. they
   ; are EXHAUSTIVE over what a base-0 strtol took whole, which is why a token
   ; that is none of them (0x, 08, 1e5, abc) goes on to the float lane.
   (p1-all? p cl) (? (nil? (link? cl)) 1 (p (cap cl)) (p1-all? p (cup cl)) ())
   (p1-oct? c)  (? (<= 48 c) (<= c 55) ())
   (p1-dec-int? cl)
     (? (nil? (link? cl)) ()
        ; a leading zero belongs to octal, below -- but "0" alone is decimal
        (? (= (cap cl) 48) (link? (cup cl)) ()) ()
        (p1-all? p1-dig? cl))
   (p1-hex-int? cl)                            ; [+-]?0[xX]<hex>+, sign peeled
     (? (nil? (link? cl)) ()
        (nil? (= (cap cl) 48)) ()
        (nil? (link? (cup cl))) ()
        (nil? (= (p1-low (cap (cup cl))) 120)) ()
        (nil? (link? (cup (cup cl)))) ()         ; a bare 0x is an honest symbol
        (p1-all? p1-hex? (cup (cup cl))))
   (p1-oct-int? cl)                            ; [+-]?0<oct>+, the zero the prefix
     (? (nil? (link? cl)) ()
        (nil? (= (cap cl) 48)) ()
        (nil? (link? (cup cl))) ()
        (p1-all? p1-oct? (cup cl)))
   ; a token charlist -> (1 . its number), or () if it is not one. the sign is
   ; peeled first so every base agrees about it.
   ; ⚠ THE WRAPPER IS THE PRESENCE, NEVER THE NET. -5 and 0 both net to nothing,
   ; so a bare answer tested with nil? calls every negative and every zero "not
   ; a number" -- silently, and only for the literals most likely to matter.
   (p1-int cl)
     (: sgn (? (nil? (link? cl)) 0 (= (cap cl) 45) -1 (= (cap cl) 43) 1 0)
        body (? (= sgn 0) cl (cup cl))
        mag (? (p1-dec-int? body) (link 1 (p1-digits body 10 0))
               (p1-hex-int? body) (link 1 (p1-digits (cup (cup body)) 16 0))
               (p1-oct-int? body) (link 1 (p1-digits (cup body) 8 0))
               ())
        (? (nil? (link? mag)) ()
           (< sgn 0) (link 1 (- 0 (cup mag)))
           mag))

   ; --- the token lanes -----------------------------------------------------
   (p1-tok cl acc)                                   ; a name-led token
     (? (nil? (link? cl)) (link (p1-rev acc ()) cl)
        (p1-tend? (cap cl)) (link (p1-rev acc ()) cl)
        (p1-tok (p1-cup cl) (link (cap cl) acc)))
   (p1-rev l acc) (? (link? l) (p1-rev (cup l) (link (cap l) acc)) acc)
   ; an OPERATOR RUN: a maximal run of operator chars, read as one plain symbol
   ; and factored later by opfix. ⚠ a trailing '-' right before a digit is the
   ; next number's sign, not part of the run -- the run sheds it and the '-' is
   ; put back, so `!-5` is ! then -5.
   (p1-run cl acc)
     (? (? (link? cl) (p1-opc? (cap cl)) ())
        (p1-run (p1-cup cl) (link (cap cl) acc))
        ; the run ended: shed a trailing '-' if a digit follows and the run has
        ; more than that '-' to it, and hand the '-' back for the number lane.
        (: shed (? (nil? (link? acc)) ()
                   (nil? (= (cap acc) 45)) ()
                   (nil? (link? (cup acc))) ()
                   (nil? (link? cl)) ()
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
        (? (link? n) (cup n)
           (= s "ieee-inf") (/ 1.0 0.0)
           (= s "-ieee-inf") (/ -1.0 0.0)
           (: lead (? (nil? (link? tok)) 0
                      (& (p1-cls (cap tok)) 64) (? (link? (cup tok)) (cap (cup tok)) 0)
                      (cap tok))
              (? (? (p1-dig? lead) 1 (= lead 46))
                 (: f (gem s) (? (gem? f) f (name s)))
                 (name s)))))

   ; --- strings -------------------------------------------------------------
   (p1-esc c) (? (= c 110) 10 (= c 116) 9 (= c 114) 13 (= c 101) 27 (= c 48) 0 c)
   (p1-str cl acc)
     (? (nil? (link? cl)) 'torn
        (: c (cap cl) r (p1-cup cl)
           (? (= c 34) (link (string (p1-rev acc ())) r)
              (nil? (= c 92)) (p1-str r (link c acc))
              (nil? (link? r)) 'torn
              (= (cap r) 120)                              ; \xHH
                (? (nil? (? (link? (p1-cup r)) (link? (p1-cup (p1-cup r))) ())) 'torn
                   (p1-str (p1-cup (p1-cup (p1-cup r)))
                           (link (+ (* (p1-dval (cap (p1-cup r))) 16)
                                    (p1-dval (cap (p1-cup (p1-cup r))))) acc)))
              (p1-str (p1-cup r) (link (p1-esc (cap r)) acc)))))

   ; --- the wraps -----------------------------------------------------------
   ; the value surface, the printer's read-back contract: ' ` # @ ~ each wrap
   ; the next datum. a SPLICING wrap (hash tuple twin list) over a list splices
   ; instead: #(k v) is (hash k v), not (hash (k v)).
   (p1-splice? w) (? (= w 'hash) 1 (? (= w 'tuple) 1 (? (= w 'twin) 1 (= w 'list))))
   (p1-wrap w d)
     (? (? (= d ()) (= w 'hash) ()) (link 'tablet (link 0 ()))     ; #() -> (tablet 0)
        (? (= d ()) (= w 'tuple) ()) (link 'iota (link 0 ()))      ; @() -> (iota 0)
        (? (p1-splice? w) (? (link? d) 1 (? (= d ()) (nil? (= w 'hash)) ())) ()) (link w d)
        (link w (link d ())))

   ; --- the parser ----------------------------------------------------------
   ; (p1-read1 cl head) -- head is true when the next datum would be a list's
   ; FIRST, which the valence law needs: the : and ? special forms consume the
   ; whole list, so a run glued at head does not fuse for those two.
   (p1-read1 cl0 head)
     (: cl (p1-skip cl0)
        (? (nil? (link? cl)) ()
           (: c (cap cl) r (p1-cup cl)
              ; a numeral has to start somewhere (rung 1): + and - are operator
              ; runs like every other punctuation, EXCEPT that a digit -- or a
              ; `.`, for -.5 -- right after them makes the token a NUMBER.
              numlead (? (& (p1-cls c) 64) (? (link? r) (? (p1-dig? (cap r)) 1 (= (cap r) 46)) ()) ())
              (? (& (p1-cls c) 16) (p1-list r 1 ())
                 (& (p1-cls c) 32) ()
                 (= c 34) (p1-str r ())
                 (= c 39) (p1-lift (name "\\") r head)
                 (= c 96) (p1-lift 'list r head)
                 (= c 35) (p1-lift 'hash r head)
                 (= c 64) (p1-lift 'tuple r head)
                 (= c 126) (p1-lift (? (? (link? r) (= (cap r) 40) ()) 'twin 'conj) r head)
                 (= c 44) (link (name ",") r)
                 (? (p1-opc? c) (nil? numlead) ()) (p1-op cl head)
                 (: tr (p1-tok cl ())
                    (link (p1-atom (cap tr)) (cup tr)))))))

   ; a wrap takes the next datum; an unfinished one is MORE, never EOF.
   (p1-lift w cl head)
     (: d (p1-read1 cl head)
        (? (nil? (link? d)) 'torn
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
        sym (name run)
        shed (? (link? rest) (= (cap rest) 45) ())    ; p1-run put a sign back: glued
        (? (= run "\\") (link sym rest)
           (? head (? (= run ":") 1 (= run "?")) ()) (link sym rest)
           (nil? (? shed 1 (p1-glued? rest))) (link sym rest)
           (: d (p1-read1 rest ())
              (? (nil? (link? d)) 'torn
                 (link (link 'mono (link (link sym (link (cap d) ())) ())) (cup d))))))
   ; glued = the next char starts a datum rather than ending the form.
   (p1-glued? cl)
     (? (link? cl) (: c (cap cl) (nil? (& (p1-cls c) 128))) ())

   ; a list: datums until the close. an empty one IS the zero point.
   (p1-list cl0 head acc)
     (: cl (p1-skip cl0)
        (? (nil? (link? cl)) 'torn
           (& (p1-cls (cap cl)) 32) (link (p1-rev acc ()) (p1-cup cl))
           (: d (p1-read1 cl head)
              (? (nil? (link? d)) 'torn
                 (p1-list (cup d) () (link (cap d) acc))))))

   ; --- a port is a charlist ------------------------------------------------
   ; p1-flow lays a port's readable run down as ordinary conses and closes it on a
   ; thunk for the next, which p1-cup fires like any other tail. the thunk must
   ; memoize: p1 forces a position more than once on purpose (the `#!` lookahead,
   ; the \xNN lane), and a re-drinking tail would take the next gulp instead, losing
   ; the bytes between. it wraps its answer (1 v) and reads it back with link?, since
   ; a promise of nothing is the ordinary case at eof. these are prel's flow/once
   ; written again -- prel is not loaded when p1 runs.
   (p1-port? o) (? (hot? o) (id? (peek 0 o) (peek 0 in)))
   (p1-once f) (: t (tablet 0)
                  (\ u (: v (peep t 0 ())
                          (? (link? v) (cup v)
                             (: w (f ()) _ (pin t 0 (link 1 w)) w)))))
   ; the gulp: `see` draws the first byte and is the only step that touches the
   ; device -- it parks the task and refills -- `unsee` hands it back, and `chug`
   ; takes it plus the run the port has in hand. one chug per gulp: the run is the
   ; port's own, so a file arrives 4096 at a time and never all at once.
   ; ⚠ the steps are PINNED IN ORDER, never nested as arguments: an n-ary call evals
   ; right to left, so a nested (go .. (chug p)) would draw the byte before it
   ; chugged. `go` lays the text down backwards onto the tail handed in, so the
   ; thunk is what the list closes on and there is no second pass.
   (p1-gulp p tl)
     (: c (see p)
        (? (= c -1) tl
           (: _ (unsee p c)
              t (chug p)
              (go k l) (? (nil? (< 0 k)) l (: j (- k 1) (go j (link (t j) l))))
              (go (tally t) tl))))
   (p1-flow p)
     (: c (see p)
        (? (= c -1) ()
           (: _ (unsee p c) (p1-gulp p (p1-once (\ u (p1-flow p)))))))
   ; every datum in a text, in source order -- what the boot stitch wants.
   ; answers `torn` if any shape did not finish.
   (p1-reads cl)
     (: (go cl acc)
          (: d (p1-read1 cl ())
             (? (link? d) (go (cup d) (link (cap d) acc))
                (id? d 'torn) 'torn
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

   _ (seal-hook 0 p1-text)
   ; --- the doors -----------------------------------------------------------
   ; sound: one datum off text, and the tree's whole reader. answers
   ; (datum . rest), () at a clean end, or `torn` mid-shape. four doors: a port, a
   ; string, an unforced tail, or the charlist a previous read left. the residue is
   ; always a charlist, so a caller threads it and never hands a port back twice.
   (\ x (p1-read1 (? (string? x)  (p1-chars x)
                     (p1-port? x) (p1-flow x)
                     (lit? x)     (x ())
                     x) ()))))
