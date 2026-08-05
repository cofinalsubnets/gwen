; --- love/p1.l: THE READER IN LOVE (doc/io.md rung 6) -------------------
;
; the grammar p0 does not take: the operator run and its valence law, the
; constructor wraps with their splice rewrites, the comma datum, the bracket
; synonyms, the number tower. p0 (love.c's sound0) reads the pure lisp subset
; and reads THIS FILE; p1 reads the corpus -- prel and ev both.
;
; ⚠ P1.L IS THE ONLY .l HELD TO THE P0 SUBSET, and it is the whole of why the
; subset exists -- p0 is what reads it, and love/egg.l (applyq's driver text).
; so: no ` # @ ~ wraps, no comma, no brackets, and NEVER A GLUED OPERATOR RUN.
; `!x` and `<c` are one symbol to p0 and (mono ! x) / (mono < c) to the reader
; this file is replacing, and the PRINTER SHOWS BOTH THE SAME -- only `=` on two
; trees can see it. space every operator; say (nil? x) and (cap x) for the words.
;
; ⚠ AND IT IS PRE-OPFIX, so NO INFIX either: the boot stitch (rung 6b) evals this
; file to make it callable BEFORE prel runs, so `opfix` does not exist yet and an
; infix form would compile as a plain application -- silently, church-
; exponentiating where a comparison was meant. prefix nifs only. it is also
; PRE-PREL: nifs and its own bindings, nothing else, no prel helpers, no macros.
;
; the protocol is the historical one (2efbaa51^:g/repl.g) and NOT a port: read1
; answers the value CONSED ONTO WHAT IS LEFT, () for no datum (end of text, or a
; stray close), or `torn` for a shape that did not finish. the residue IS the
; answer, which is what lets a caller resume a torn read by handing back more
; text -- there is no port to stash it in.
;
; ⚠ ASK `two?` FIRST, never the net: a cons is the one structural answer, and
; testing (nil? d) first would call a perfectly good (datum . rest) a clean end
; whenever the pair happens to net 0.
; ⚠ AND `torn` IS A NAMED SYMBOL, not a (nom 0). a nameless mint sits on the BLUE
; FLOOR -- it nets nothing, so (nil? torn) is TRUE and the torn answer reads as a
; clean end, silently, on exactly the inputs that matter. named, it nets its
; spelling, stays un-two, prints as itself in a scare, and `id?` is exact.

(: torn 'torn

   ; --- THE CURSOR: THREE ARGS, NOT A VALUE (doc/io.md rung 9) --------------
   ; a position is (t k th): t the current chunk (a text), k an index into it,
   ; th the SOURCE for the next chunk -- () or a thunk answering (text . thunk),
   ; () at eof. the three ride as SEPARATE ARGS through every scan loop, so a
   ; step is (+ k 1) and allocates NOTHING; ev gives a saturated call to a letrec
   ; sibling the one-shot n-ary apply, so the extra arity is three pushes and no
   ; currying.
   ; ⚠ WHICH MAKES SATURATION LOAD-BEARING FOR SPEED: partially apply any scan
   ; function below and apxh falls back to the l2r chain at the higher arity,
   ; silently. call them with every argument, always. test/host/rdiff.l asserts it.
   ; ⚠ EVERY SCAN LOOP OPENS ON (< k (tally t)) AND PULLS ON THE ELSE. that test
   ; is the chunk edge; a loop that skips it reads (t k) past the end, where a
   ; string answers the charm 1 rather than scaring.
   ; ⚠ AND A JUMP OF MORE THAN ONE GOES THROUGH p1-norm / p1-peek, never a raw
   ; (+ k n): those two carry the overshoot ACROSS the join, where a pull that
   ; resets k to 0 would drop it.
   (p1-pull th) (? (lit? th) (th ()) ())
   ; ⚠ THE CURSOR IS TWO CONSES, NOT A TABLET. a tablet is a HASH MAP here, so a
   ; three-field one costs three inserts to build and three probes to read -- and
   ; a cursor is built and read once per datum, which put map_probe and lvm_key
   ; together at 6% of the reader when this was a tablet. plain cells, no hashing.
   (p1-cur t k th) (link t (link k th))
   ; and the accessors are TOTAL, because a spent cursor is (): a cons cursor
   ; cannot answer a default the way `peep` does, so each one names the empty
   ; reading itself rather than leaning on ()-as-0 arithmetic.
   (p1-ct c)  (? (two? c) (cap c) "")
   (p1-ck c)  (? (two? c) (cap (cup c)) 0)
   (p1-cth c) (? (two? c) (cup (cup c)) ())
   ; the cursor as a VALUE, normalized: k lands inside its chunk. only an ESCAPING
   ; position is built -- a residue, a lookahead -- so this is per datum, never
   ; per char.
   ; ⚠ A SPENT CURSOR IS THE ZERO POINT. that keeps the door's law -- a clean end
   ; answers () and (nil? residue) means spent -- and a LIVE cursor is a cons whose
   ; cap is a TEXT, which is what tells it from a charlist (whose cap is a charm).
   (p1-norm t k th)
     (? (< k (tally t)) (p1-cur t k th)
        (: g (p1-pull th)
           (? (two? g) (p1-norm (cap g) (- k (tally t)) (cup g)) ())))
   ; the char at a position, WRAPPED (1 c) -- the lookahead door, and the one
   ; place a pull happens outside a scan loop.
   ; ⚠ THE WRAPPER IS THE PRESENCE: past the end answers (), and a bare charm
   ; would make the NUL byte indistinguishable from no byte at all.
   (p1-peek t k th)
     (? (< k (tally t)) (link 1 (t k))
        (: g (p1-pull th)
           (? (two? g) (p1-peek (cap g) (- k (tally t)) (cup g)) ())))

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
   ; both answer a NORMALIZED cursor, so a caller can ask (< k (tally t)) and
   ; trust it. p1-eolc / p1-skipc re-enter the worker from one.
   (p1-eolc c)  (p1-eol  (p1-ct c) (p1-ck c) (p1-cth c))
   (p1-skipc c) (p1-skip (p1-ct c) (p1-ck c) (p1-cth c))
   (p1-eol t k th)
     (? (nil? (< k (tally t)))
          (: g (p1-pull th)
             (? (two? g) (p1-eol (cap g) 0 (cup g)) ()))
        (? (= (t k) 10) 1 (= (t k) 13)) (p1-norm t (+ k 1) th)
        (p1-eol t (+ k 1) th))
   (p1-skip t k th)
     (? (nil? (< k (tally t)))
          (: g (p1-pull th)
             (? (two? g) (p1-skip (cap g) 0 (cup g)) ()))
        (: c (t k)
           (? (p1-ws? c) (p1-skip t (+ k 1) th)
              (= c 59) (p1-skipc (p1-eolc (p1-norm t (+ k 1) th)))
              (? (= c 35) (: n (p1-peek t (+ k 1) th) (? (two? n) (= (cup n) 33) ())) ())
                (p1-skipc (p1-eolc (p1-norm t (+ k 2) th)))
              (p1-cur t k th))))

   ; --- integers, at full precision ----------------------------------------
   ; love's + and * promote charm -> sun -> big by VALUE, so the tower is the
   ; arithmetic's and there is no width to get wrong (rung 2's law).
   ; ⚠ THE LANES BELOW TAKE A TOKEN LIST, never the input: p1-tok and p1-run
   ; build those strictly, so a plain `cup` is right here and a pull is not.
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
   (p1-tok t k th acc)                               ; a name-led token
     (? (nil? (< k (tally t)))
          (: g (p1-pull th)
             (? (two? g) (p1-tok (cap g) 0 (cup g) acc)
                (link (p1-rev acc ()) ())))
        (p1-tend? (t k)) (link (p1-rev acc ()) (p1-cur t k th))
        (p1-tok t (+ k 1) th (link (t k) acc)))
   (p1-rev l acc) (? (two? l) (p1-rev (cup l) (link (cap l) acc)) acc)
   ; an OPERATOR RUN: a maximal run of operator chars, read as one plain symbol
   ; and factored later by opfix.
   ; ⚠ a trailing '-' right before a digit is the NEXT number's sign, so the run
   ; STOPS IN FRONT OF IT rather than taking it and handing it back -- `!-5` is !
   ; then -5. the residue is then a real position in the text, which is the whole
   ; reason an index can carry it: there is no synthetic char to cons on.
   (p1-run t k th acc)
     (? (nil? (< k (tally t)))
          (: g (p1-pull th)
             (? (two? g) (p1-run (cap g) 0 (cup g) acc)
                (link (p1-rev acc ()) ())))
        (nil? (p1-opc? (t k))) (link (p1-rev acc ()) (p1-cur t k th))
        (? (= (t k) 45)
           (? (two? acc) (: d (p1-peek t (+ k 1) th) (? (two? d) (p1-dig? (cup d)) ())) ())
           ())
          (link (p1-rev acc ()) (p1-cur t k th))
        (p1-run t (+ k 1) th (link (t k) acc)))

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
                      (& (p1-cls (cap tok)) 64) (? (two? (cup tok)) (cap (cup tok)) 0)
                      (cap tok))
              (? (? (p1-dig? lead) 1 (= lead 46))
                 (: f (gem s) (? (gem? f) f (intern s)))
                 (intern s)))))

   ; --- strings -------------------------------------------------------------
   (p1-esc c) (? (= c 110) 10 (= c 116) 9 (= c 114) 13 (= c 101) 27 (= c 48) 0 c)
   (p1-strc c acc) (p1-str (p1-ct c) (p1-ck c) (p1-cth c) acc)
   (p1-str t k th acc)
     (? (nil? (< k (tally t)))
          (: g (p1-pull th)
             (? (two? g) (p1-str (cap g) 0 (cup g) acc) torn))
        (: c (t k)
           ; ⚠ "" is the EMPTY STRING, and (string ()) is not it -- string is
           ; identity on the zero point, so an empty accumulator has to name the
           ; literal to get the object the C reader hands back.
           (? (= c 34) (link (? (two? acc) (string (p1-rev acc ())) "")
                             (p1-norm t (+ k 1) th))
              (nil? (= c 92)) (p1-str t (+ k 1) th (link c acc))
              (: e (p1-peek t (+ k 1) th)
                 (? (nil? (two? e)) torn
                    (= (cup e) 120)                            ; \xHH
                      (: h1 (p1-peek t (+ k 2) th)
                         h2 (p1-peek t (+ k 3) th)
                         (? (nil? (? (two? h1) (two? h2) ())) torn
                            (p1-strc (p1-norm t (+ k 4) th)
                                     (link (+ (* (p1-dval (cup h1)) 16)
                                              (p1-dval (cup h2))) acc))))
                    (p1-strc (p1-norm t (+ k 2) th) (link (p1-esc (cup e)) acc)))))))

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
   ; (p1-read1 t k th head) -- head is true when the next datum would be a list's
   ; FIRST, which the valence law needs: the : and ? special forms consume the
   ; whole list, so a run glued at head does not fuse for those two.
   (p1-read1 t0 k0 th0 head)
     (: cu (p1-skip t0 k0 th0)
        t (p1-ct cu) k (p1-ck cu) th (p1-cth cu)
        (? (nil? (< k (tally t))) ()
           (: c (t k)
              ; a numeral has to start somewhere (rung 1): + and - are operator
              ; runs like every other punctuation, EXCEPT that a digit -- or a
              ; `.`, for -.5 -- right after them makes the token a NUMBER.
              nx (? (& (p1-cls c) 64) (p1-peek t (+ k 1) th) ())
              numlead (? (two? nx) (? (p1-dig? (cup nx)) 1 (= (cup nx) 46)) ())
              (? (& (p1-cls c) 16) (p1-list t (+ k 1) th 1 ())
                 (& (p1-cls c) 32) ()
                 (= c 34) (p1-str t (+ k 1) th ())
                 (= c 39) (p1-lift (intern "\\") t (+ k 1) th head)
                 (= c 96) (p1-lift 'list t (+ k 1) th head)
                 (= c 35) (p1-lift 'hash t (+ k 1) th head)
                 (= c 64) (p1-lift 'tuple t (+ k 1) th head)
                 (= c 126) (p1-lift (: p (p1-peek t (+ k 1) th)
                                       (? (? (two? p) (= (cup p) 40) ()) 'twin 'conj))
                                    t (+ k 1) th head)
                 (= c 44) (link (intern ",") (p1-norm t (+ k 1) th))
                 (? (p1-opc? c) (nil? numlead) ()) (p1-op t k th head)
                 (: tr (p1-tok t k th ())
                    (link (p1-atom (cap tr)) (cup tr)))))))

   ; a wrap takes the next datum; an unfinished one is MORE, never EOF.
   (p1-lift w t k th head)
     (: d (p1-read1 t k th head)
        (? (nil? (two? d)) torn
           (link (p1-wrap w (cap d)) (cup d))))

   ; THE VALENCE LAW, reader half: a run GLUED to the datum after it is MONADIC
   ; -- the run becomes a wrap UNDER a mono wrap, so the emission is the nested
   ; (mono (run d)), which opfix factors against book['monadics] -- and a spaced
   ; run is the plain symbol, dyadic by opfix's default. head
   ; fuses too, so (<x = y) is (= (cap x) y); the exceptions are \ (which never
   ; fuses, form space) and : / ? at head (they consume the whole list, where
   ; fusion would swallow only the next datum).
   (p1-op t k th head)
     (: rr (p1-run t k th ())
        run (string (cap rr))
        rest (cup rr)
        rt (p1-ct rest) rk (p1-ck rest) rth (p1-cth rest)
        sym (intern run)
        nx (p1-peek rt rk rth)
        shed (? (two? nx) (= (cup nx) 45) ())    ; p1-run stopped before a sign: glued
        (? (= run "\\") (link sym rest)
           (? head (? (= run ":") 1 (= run "?")) ()) (link sym rest)
           (nil? (? shed 1 (p1-glued? nx))) (link sym rest)
           (: d (p1-read1 rt rk rth ())
              (? (nil? (two? d)) torn
                 (link (link 'mono (link (link sym (link (cap d) ())) ())) (cup d))))))
   ; glued = the next char starts a datum rather than ending the form. it reads
   ; the PEEK, so p1-op pays one lookahead for both questions.
   (p1-glued? nx)
     (? (two? nx) (nil? (& (p1-cls (cup nx)) 128)) ())

   ; a list: datums until the close. an empty one IS the zero point.
   (p1-list t0 k0 th0 head acc)
     (: cu (p1-skip t0 k0 th0)
        t (p1-ct cu) k (p1-ck cu) th (p1-cth cu)
        (? (nil? (< k (tally t))) torn
           (& (p1-cls (t k)) 32) (link (p1-rev acc ()) (p1-norm t (+ k 1) th))
           (: d (p1-read1 t k th head)
              (? (nil? (two? d)) torn
                 (: r (cup d)
                    (p1-list (p1-ct r) (p1-ck r) (p1-cth r)
                             () (link (cap d) acc)))))))

   ; --- the doors -----------------------------------------------------------
   ; SOUND: one datum, and the tree's whole reader. answers (datum . cursor),
   ; () at a clean end, or `torn` mid-shape.
   ; the input is TEXT -- the whole of it, as one chunk -- or one of the three
   ; things that stand for text: a THUNK (a chunk source, what prel's `spout`
   ; hands over), a CURSOR (a residue, handed back), or () for a spent one. the
   ; residue IS a cursor, so a caller that threads it -- prel's rdev, rdiff's
   ; walk -- converts nothing per form and copies nothing per datum.
   ; ⚠ TEXT IS A STRING HERE, not a charlist: the reader indexes, so a caller
   ; holding chars conses them into one with `string` and hands that over once.
   (sound x)
     (? (string? x) (p1-read1 x 0 () ())
        (lit? x) (p1-read1 "" 0 x ())
        (two? x) (p1-read1 (p1-ct x) (p1-ck x) (p1-cth x) ())
        (p1-read1 "" 0 () ()))
   ; every datum in a text, in source order -- what the boot stitch wants.
   ; answers `torn` if any shape did not finish.
   (p1-reads t0 k0 th0)
     (: (go t k th acc)
          (: d (p1-read1 t k th ())
             (? (two? d) (: r (cup d)
                            (go (p1-ct r) (p1-ck r) (p1-cth r)
                                (link (cap d) acc)))
                (id? d torn) torn
                (p1-rev acc ())))
        (go t0 k0 th0 ()))
   ; SPOT: a place in a text, as the reader's own residue -- (sound (spot t k))
   ; reads the datum starting at k without copying the tail out first. that is
   ; what a byte-walking caller wants (tools/lcat.l probes at every space), and
   ; snipping the remainder per probe would make the walk quadratic.
   ; ⚠ PAST THE END IS THE ZERO POINT, which reads as a clean end -- the same
   ; answer the text's own end gives, so a walk needs no bounds test of its own.
   (spot t k) (p1-norm t k ())
   ; the BOOT'S door, whole text -> every form: what ai_egg_ calls to read ev.l.
   ; the text IS the first chunk, so the boot builds no charlist at all.
   (p1-text s) (p1-reads s 0 ())

   ; --- and THIS `:` HAS A BODY, so the 40 above are LOCAL -----------------
   ; `sound`, `spot` and `torn` are the reader's vocabulary and go on the book.
   ; p1-text is NOT vocabulary -- it is the boot's door, so it goes where C wants it
   ; and nowhere else: hook 0, the FIRST thing sealed in a boot. that is why the
   ; whole-text reader needs no name at all, and why nothing can rebind it out from
   ; under ai_evals_. `pin book` is the only publisher here -- prel is not loaded,
   ; so no `use` yet.
   _ (pin book 'torn torn)
   _ (pin book 'spot spot)
   _ (pin book 'sound sound)
   (seal-hook 0 p1-text))
