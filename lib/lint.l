; lib/lint.l -- the .l balance scan, one scanner under every consumer: `libra
; check` gates `make lint`, `libra doc` rides lint-cmts, vi's reindent rides
; lint-lay. it is a balance scan, not a parse -- delimiters and strings and
; nothing else. `'` and backtick are reader operators, never delimiters, so they
; scan as ordinary code: the trap a C lexer falls into.
;
; (lint-scan src) answers warnings, each [line col text] 1-based, and an unclosed
; opener or string reports where it opened rather than end-of-file.
;
; (lint-lay src) answers the indent plan: one entry per line, [1 n] for "starts at
; 0-based column n", or () for "leave this line alone" -- a line inside a string,
; whose leading spaces are the string. column 0 is the commonest plan there is, so
; a bare number could not say "no plan": the wrapper is not decoration.
; the rule: a continuation line aligns under its form's first operand, or one past
; the open delimiter when the head stands alone. line-preserving by construction,
; since it only ever says where a line starts. see doc/misc/libra.md.
(module (lint lint-scan lint-of lint-cmts lint-lay)
(: (lclose c) (? (c = 40) 41 (c = 91) 93 125)          ; ( -> )   [ -> ]   { -> }
   (lopen c)  (? (c = 41) 40 (c = 93) 91 123)          ; ...and back, for the report

   ; --- character classes ----------------------------------------------------
   ; the three sets are strings so they can be checked against love.c at a glance
   (lin? s c) (: n (tally s) (go k) (? (n <= k) () (s k = c) 1 (go (k + 1))) (go 0))

   ; the operator run characters -- love.c's op_break, complemented
   (lopc? c) (lin? "!$%&*+-./:<=>?\\^|" c)
   ; the constructor sigils, exhaustive: a ( glued behind one of these is a datum,
   ; not a form -- '(a) quote, `(a) list, #(a) map, @(a) tray, ~(a) twin
   (lctc? c) (lin? "'`#@~" c)
   ; a token character: not whitespace, a delimiter, a quote mark or a `;`
   (ltkc? c) !(c < 33 || c = 34 || c = 59 || c = 40 || c = 41
               || c = 91 || c = 93 || c = 123 || c = 125)

   (lall? p s) (: n (tally s)
                  (go k) (? (n <= k) (0 < n) (p (s k)) (go (k + 1)) ())
                  (go 0))
   ; the token starting at index i (empty if a delimiter sits there)
   (ltok-at src n i) (: (go k) (? (k < n && ltkc? (src k)) (go (k + 1)) k)
                        (snip src i (go i)))

   ; --- the config the rules read --------------------------------------------
   ; a plain tablet read with peep, never lib/salt.l: vi cats this file directly
   ; (test/test.mk) and a module it had to carry along would break that.
   ; a switch whose default is yes -- absent means on, only an explicit 0 is off
   (lint-yes? conf k) (: v (peep conf k ()) (? (two? v) (! (! <v)) 1))
   ; the name a token mentions: a leading ' is quote, so 'foo names foo and a
   ; rename wants both. a trailing ' is the prime idiom, part of the name.
   (lname t) (: n (tally t) (go k) (? (k < n && t k = 39) (go (k + 1)) (snip t k n)) (go 0))
   ; the roster entry for the token text t: a bare name, or (name "hint")
   (lint-dep roster t)
    (? !(two? roster) ()
       (: e <roster
          nm (? (two? e) <e e)
          (? (string nm = t) [1 e] (lint-dep >roster t))))
   (lint-depmsg e)
    (: nm (? (two? e) <e e)
       h  (? (two? e && two? >e) <>e ())
       ("deprecated " + string nm + (? (string? h) (" -- " + h) "")))

   ; is frame f a binder slot -- a `\` past its head, or a `:` at an even element?
   (lbind? src n f)
    (f @ {'e1 e1 'ne ne}
     (: h (ltok-at src n e1)
        (? (h = ":")  (2 <= ne && 0 = ne % 2)
           (h = "\\") (2 <= ne)
           0)))

   ; --- the frame ------------------------------------------------------------
   ; cl the expected closer, ln/co where the opener sat (so an unclosed one reports
   ; its own position), ne how many elements taken, e1 the index the first started
   ; at, pl/qd the singleton rule's two bits, am the match bit, pd the deferred
   ; verdicts. e1 is an index read back out of src, never accumulated as text.
   (lelem stk i)
    (? !(two? stk) stk
       (: f <stk ne (f 'ne + 1)
          (pin f 'ne ne, (? (1 = ne) (pin f 'e1 i) 0), stk)))

   ; a bare `@` marks its frame a match: everything after is arms, where (x) is a
   ; list shape and the singleton rule stands down. a glued @( is the tray ctor and
   ; never reaches here. lpush inherits the bit, so arm bodies go quiet too -- that
   ; costs missed gripes, never noise.
   (lat src stk it ti i)
    (? !(it && two? stk) stk
       !(snip src ti i = "@") stk
       (pin <stk 'am 1, stk))

   ; a new opener. pre is what it is glued to, empty when it stands free:
   ;   all constructor sigils -> a datum, and a ' among them makes the contents
   ;                             data too, inherited by every frame inside
   ;   all operator chars     -> exempt: glued, two sigil runs lex as one, and
   ;                             telling <(<l) (same value) from <(= x) (not <=x)
   ;                             needs the operator table a balance scan lacks
   ;   anything else          -> foo(x) is two datums, so the ( is a plain form
   (lpush stk c i line col src it ti)
    (: pre (? it (snip src ti i) "")
       pq  (? (two? stk) (<stk 'qd) 0)
       am  (? (two? stk) (<stk 'am) 0)
       ct  (lall? lctc? pre)
       op  (lall? lopc? pre)
       qd  (? (pq || ct && lin? pre 39) 1 0)
       pl  (? (pq || ct || op) 0 (? (c = 40) 1 0))
       (link {'cl (lclose c) 'ln line 'co col 'ne 0 'e1 0 'pl pl 'qd qd 'am am 'pd ()}
             stk))

   ; the singleton warning for a form about to close, or (). (f) == f, so the
   ; parens do nothing -- except where the one element is all punctuation, which is
   ; (+) as a value and (:) (?) (\) at their zero point. the exemption is narrower
   ; than "starts with punctuation": (<>b) is droppable and flagging it is the point.
   ;
   ; a singleton in pattern position is a list shape, and which positions are
   ; patterns is known only when the binder closes, its last element being the body.
   ; so an element of a `\`, an even element of a `:`, or one level inside such an
   ; element defers its verdict to that close -- final index warns, any other is a
   ; pattern and stays silent. odd `:` elements are values and warn at once.
   (lsing src n f)
    (f @ {'pl 1 'ne 1 'e1 e1 'ln ln 'co co}
     (: t (ltok-at src n e1)
        (? (lall? lopc? t) ()
           [ln co ("singleton " + (? (0 < tally t) ("(" + t + ") ") "") + "-- (x) == x")
            'singleton]))
     ())
   ; the deferral home: () to warn now, else [depth idx] naming the binder frame
   ; (1 parent, 2 grandparent) and the element index the verdict keys on
   (ldefer src n rest)
    (? !(two? rest) ()
       (lbind? src n <rest) [1 (<rest 'ne)]
       !(two? >rest) ()
       (lbind? src n <>rest) [2 (<>rest 'ne)]
       ())
   ; hang (idx . warn) on the binder's pend slot
   (lpend rest d idx wr)
    (: f (? (1 = d) <rest <>rest)
       (pin f 'pd ((idx >< wr) >< f 'pd), rest))

   ; --- the scan -------------------------------------------------------------
   ; (lint-of src conf) is the whole weighing; (lint-scan src) is it with no config,
   ; which runs balance alone. the rule is the 4th field, appended, so a consumer
   ; reading <w / <>w / <>>w for line, column and text did not have to change.
   (lint-scan src) (lint-of src ())

   (lint-of src conf)
    (: n  (tally src)
       ; no config at all -- lint-scan's () -- is balance alone, the contract vi's
       ; cat and the fixtures ride. singleton is on by default under any other.
       sg (? (id? conf ()) 0 (lint-yes? conf 'singleton))
       dp (peep conf 'deprecated ())
       ; a token just ended at i, having started at ti/tc. a no-op until a config asks
       (lend line tc ti i warns)
        (: t (snip src ti i)
           (? !(two? dp) warns
              (: r (lint-dep dp (lname t))
                 (? (two? r) (link [line tc (lint-depmsg <>r) 'deprecated] warns) warns))))

       ; stk the frames above, a matched-pair stack so ([)] is caught; sl/sc a
       ; string's opening spot; it/ti/tc the token in hand; st is code | cmt | str.
       ; it must fire once per token, at its first character -- once per character
       ; and a three-letter name counts as three elements.
       (w i st stk line col sl sc it ti tc warns)
        (? (n <= i)
            (: w0 (? it (lend line tc ti i warns) warns)
               w1 (? (id? st 'str) (link [sl sc "unclosed string" 'balance] w0) w0)
               (rev (foldl (\ a o (o @ {'cl cl 'ln ln 'co co}
                            (link [ln co ("unclosed " + string (lopen cl)) 'balance] a)))
                     w1 stk)))
           (: c (src i)
              (? (id? st 'str)                                    ; inside "..."
                  (? (c = 92) (w (i + 2) 'str stk line (col + 2) sl sc 0 ti tc warns)   ; \ escape: skip next
                     (c = 34) (w (i + 1) 'code stk line (col + 1) sl sc 0 ti tc warns)
                     (c = 10) (w (i + 1) 'str stk (line + 1) 1 sl sc 0 ti tc warns)
                     (w (i + 1) 'str stk line (col + 1) sl sc 0 ti tc warns))
                 (id? st 'cmt)                                    ; ; comment to EOL
                  (? (c = 10) (w (i + 1) 'code stk (line + 1) 1 sl sc 0 ti tc warns)
                     (w (i + 1) 'cmt stk line (col + 1) sl sc 0 ti tc warns))
                 (c = 59)                                                      ; ; -- start comment
                  (w (i + 1) 'cmt (lat src stk it ti i) line (col + 1) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (c = 35 && i + 1 < n && src (i + 1) = 33)                     ; #! shebang, to EOL
                  (w (i + 2) 'cmt stk line (col + 2) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (c = 34)                                                      ; " -- start string
                  (w (i + 1) 'str (? it stk (lelem stk i)) line (col + 1) line col 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (c = 10)
                  (w (i + 1) 'code (lat src stk it ti i) (line + 1) 1 sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (c = 40 || c = 91 || c = 123)                                 ; ( [ {  -- push
                  ; an opener is an element of its parent, unless glued to the
                  ; token before it, where the two are one datum
                  (w (i + 1) 'code (lpush (? it stk (lelem stk i)) c i line col src it ti)
                     line (col + 1) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (c = 41 || c = 93 || c = 125)                                 ; ) ] }  -- match
                  (: w0 (? it (lend line tc ti i warns) warns)
                     (? !(two? stk)
                         (w (i + 1) 'code stk line (col + 1) sl sc 0 ti tc
                            (link [line col ("stray " + string c) 'balance] w0))
                        !(<stk 'cl = c)
                         (w (i + 1) 'code >stk line (col + 1) sl sc 0 ti tc
                            (link [line col ("mismatched " + string c) 'balance] w0))
                        (: f <stk rest >stk
                           ; the verdicts this binder owed: an entry at the final
                           ; index was the body, which is where a singleton warns
                           w1 (foldl (\ a p (: wr >p (? (<p = f 'ne) (link wr a) a)))
                                     w0 (f 'pd))
                           sw (? sg (lsing src n f) ())
                           (? (id? sw ()) (w (i + 1) 'code rest line (col + 1) sl sc 0 ti tc w1)
                              (f 'am)     (w (i + 1) 'code rest line (col + 1) sl sc 0 ti tc w1)   ; match arms: silent
                              (: dh (ldefer src n rest)
                                 (? (id? dh ())
                                     (w (i + 1) 'code rest line (col + 1) sl sc 0 ti tc (link sw w1))
                                    (w (i + 1) 'code (lpend rest <dh <>dh sw) line (col + 1) sl sc 0 ti tc w1)))))))
                 (c < 33)                                                      ; space, tab, the rest
                  (w (i + 1) 'code (lat src stk it ti i) line (col + 1) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 ; an ordinary character: a token START only if we were not in one
                 (w (i + 1) 'code (? it stk (lelem stk i)) line (col + 1) sl sc
                    1 (? it ti i) (? it tc col) warns))))
       (w 0 'code () 1 1 1 1 0 0 1 ()))

   ; --- the comments ---------------------------------------------------------
   ; (lint-cmts src) answers every comment in reading order, [line col text kind]
   ; 1-based at the introducer, kind 'semi or 'bang. it answers the text and not
   ; just the span, because no reader in the tree keeps comments -- libra's doc
   ; verb walks these rather than encoding .l lexing a fifth time.
   (lint-cmts src)
    (: n (tally src)
       (shut i cl cc ti ck acc) (link [cl cc (snip src ti i) ck] acc)
       (w i st line col cl cc ti ck acc)
        (? (n <= i) (rev (? (id? st 'cmt) (shut i cl cc ti ck acc) acc))
           (: c (src i)
              (? (id? st 'str)
                  (? (c = 92) (w (i + 2) 'str line (col + 2) cl cc ti ck acc)
                     (c = 34) (w (i + 1) 'code line (col + 1) cl cc ti ck acc)
                     (c = 10) (w (i + 1) 'str (line + 1) 1 cl cc ti ck acc)
                     (w (i + 1) 'str line (col + 1) cl cc ti ck acc))
                 (id? st 'cmt)
                  (? (c = 10) (w (i + 1) 'code (line + 1) 1 cl cc ti ck (shut i cl cc ti ck acc))
                     (w (i + 1) 'cmt line (col + 1) cl cc ti ck acc))
                 (c = 59)  (w (i + 1) 'cmt line (col + 1) line col (i + 1) 'semi acc)
                 (c = 35 && i + 1 < n && src (i + 1) = 33)
                  (w (i + 2) 'cmt line (col + 2) line col (i + 2) 'bang acc)
                 (c = 34) (w (i + 1) 'str line (col + 1) cl cc ti ck acc)
                 (c = 10) (w (i + 1) 'code (line + 1) 1 cl cc ti ck acc)
                 (w (i + 1) 'code line (col + 1) cl cc ti ck acc))))
       (w 0 'code 1 1 0 0 0 'semi ()))

   ; --- the indent plan ------------------------------------------------------
   ; base where this form's continuation lines align (0-based), ol the line it
   ; opened on, nt how many of its tokens we have passed (the head is 1), pr
   ; whether it is a pairing form.
   ;
   ; `:` and `?` take their operands two at a time -- name/value, test/result --
   ; and the house sets the second of each pair one past the first. every other
   ; form aligns flat. without this the plan is off by one on every second
   ; operand, which is most of a file. a width guard over this was measured and
   ; refused: doc/misc/libra.md.
   (ldelim? c) (c < 33 || c = 40 || c = 41 || c = 91 || c = 93 || c = 123 || c = 125)
   (lpair? c nx) ((c = 58 || c = 63) && ldelim? nx)     ; a lone : or ?, not a name starting with one

   ; the next line's indent: the frame's base, plus one when the token that will
   ; start it is the second of a pair
   (lcont stk)
    (? !(two? stk) 0
       (<stk @ {'base base 'nt nt 'pr pr}
        (? (pr && 0 = nt % 2) (base + 1) base)))

   ; the head token just ended at 1-based column x. a form whose head stands alone
   ; on its line indents its body where operand one would have gone: one space past
   ; the head, so a `(:` body lands at 3. only the pairing forms -- `(assert` alone
   ; indents one past the open paren, and the house style is per-head.
   (lhend stk line x)
    (? !(two? stk) stk
       (<stk @ ({'nt 1 'pr 1 'ol ol} && ol = line)
        (pin <stk 'base x, stk)
        stk))

   ; a token (or a nested opener -- it is a token to its parent) begins at 1-based
   ; column tc; c is its first character and nx the one after
   (ltok stk line tc c nx)
    (? !(two? stk) stk
       (: f <stk
          (f @ {'nt 0} (pin f 'nt 1, pin f 'pr (? (lpair? c nx) 1 0), stk)
             ; operand one: this is where the form aligns
             ({'nt 1 'ol ol} && ol = line) (pin f 'base (tc - 1), pin f 'nt 2, stk)
             (pin f 'nt (f 'nt + 1), stk))))

   ; it is 1 while we are inside a token, so ltok fires once per token
   (lint-lay src)
    (: n (tally src)
       (w i st stk line col it plan)
        (? (n <= i) (rev plan)
           (: c (src i)
              (? (id? st 'str)
                  (? (c = 92) (w (i + 2) 'str stk line (col + 2) 1 plan)
                     (c = 34) (w (i + 1) 'code stk line (col + 1) 0 plan)
                     ; a line BEGINNING inside a string is the string's own text
                     (c = 10) (w (i + 1) 'str stk (line + 1) 1 1 (link () plan))
                     (w (i + 1) 'str stk line (col + 1) 1 plan))
                 (id? st 'cmt)
                  (? (c = 10) (w (i + 1) 'code stk (line + 1) 1 0 (link [1 (lcont stk)] plan))
                     (w (i + 1) 'cmt stk line (col + 1) 0 plan))
                 (c = 59) (w (i + 1) 'cmt stk line (col + 1) 0 plan)
                 (c = 35 && i + 1 < n && src (i + 1) = 33)
                  (w (i + 2) 'cmt stk line (col + 2) 0 plan)
                 (c = 34) (w (i + 1) 'str (ltok stk line col c 32) line (col + 1) 1 plan)
                 ; the head's end lands before the plan is read, or the line the
                 ; body starts on still carries the old fallback
                 (c = 10) (: s2 (? it (lhend stk line col) stk)
                             (w (i + 1) 'code s2 (line + 1) 1 0 (link [1 (lcont s2)] plan)))
                 (c = 40 || c = 91 || c = 123)
                  ; an opener is a token to its parent, unless glued to the one
                  ; before it: `'(a b)` and `#(k v)` are one operand wearing a
                  ; reader sigil, and counting two flips the pairing parity for
                  ; every line left in the form -- the one miscount that cascades
                  (w (i + 1) 'code
                     (link {'base col 'ol line 'nt 0 'pr 0} (? it stk (ltok stk line col c 32)))
                     line (col + 1) 0 plan)
                 (c = 41 || c = 93 || c = 125)
                  (w (i + 1) 'code (? (two? stk) >stk stk) line (col + 1) 0 plan)
                 (c = 32 || c = 9)
                  (w (i + 1) 'code (? it (lhend stk line col) stk) line (col + 1) 0 plan)
                 ; an ordinary character: a token START only if we were not in one
                 (w (i + 1) 'code
                    (? it stk (ltok stk line col c (? (i + 1 < n) (src (i + 1)) 10)))
                    line (col + 1) 1 plan))))
       (w 0 'code () 1 1 0 [[1 0]]))
)
) ; module lint
