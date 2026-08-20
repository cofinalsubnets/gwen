; lib/lint.l -- the .l balance scan, structured so its consumers can share one
; scanner: `libra check` prints what it finds and gates `make lint`, `libra doc`
; rides lint-cmts to lift a header, and vi's own reindent rides lint-lay. it
; reads .l correctly -- `;` and `#!` start comments to end of line, `"..."` is
; the only string, and `'` / backtick are READER OPERATORS (quote, list-ctor),
; never delimiters, so they scan as ordinary code (the trap a C lexer falls
; into). it is a BALANCE scan, not a parse: it knows delimiters and strings and
; nothing else about the language.
;
; (lint-scan src) answers a list of warnings, each [line col text] with line
; and col 1-BASED -- a position, so a consumer can point AT the character rather
; than re-derive one from a sentence. no warnings is (), which here means what
; it says: nothing to report.
;
; an UNCLOSED opener reports where it OPENED, not end-of-file -- the stack
; carries each opener's position for exactly that -- and an unclosed string the
; same way. (a dropped paren is the classic .l slip; pointing at the far end of
; the file is the least useful place to point.)
; (lint-lay src) answers the INDENT PLAN: one entry per line, [1 n] for "this
; line's first character belongs at 0-based column n", or () for "leave this
; line exactly as it is" (a line inside a string, where the leading spaces ARE
; the string). ⚠ the wrapper is not decoration -- column 0 is the commonest
; plan there is, so a bare number could not say "no plan" (the presence law).
;
; the rule is ONE sentence: a continuation line aligns under its form's FIRST
; OPERAND, or one past the open delimiter when the head stands alone on its
; line. that is the classic lisp rule and nothing more -- it is LINE-PRESERVING
; by construction, since it only ever says where a line STARTS. it never moves a
; break, never re-flows, never touches what is on the line. everything the house
; style does that this cannot predict (aligned comment columns, hand-laid
; tables) survives untouched for exactly that reason. it is a plan; vi's `=`
; applies it to one buffer, and nothing else applies it at all.
(: (lclose c) (? (c = 40) 41 (c = 91) 93 125)          ; ( -> )   [ -> ]   { -> }
   (lopen c)  (? (c = 41) 40 (c = 93) 91 123)          ; ...and back, for the report

   ; --- character classes ----------------------------------------------------
   ; is the charm c anywhere in s? the three sets below are spelled as STRINGS
   ; rather than as || chains, because a reader can check them against love.c at
   ; a glance and a || chain of seventeen cannot be read at all.
   (lin? s c) (: n (tally s) (go k) (? (n <= k) () (s k = c) 1 (go (k + 1))) (go 0))

   ; the OPERATOR run characters (love.c's op_break, complemented) -- the chars
   ; that fuse into one sigil when they touch.
   (lopc? c) (lin? "!$%&*+-./:<=>?\\^|" c)
   ; the CONSTRUCTOR sigils: a ( glued behind one of these is a DATUM, not a
   ; form -- '(a) quote, `(a) list, #(a) map, @(a) tray, ~(a) twin (love.c's
   ; ioparse). that is the exhaustive list.
   (lctc? c) (lin? "'`#@~" c)
   ; a token character: anything that is not whitespace, a delimiter, a quote
   ; mark or a comment start.
   (ltkc? c) !(c < 33 || c = 34 || c = 59 || c = 40 || c = 41
               || c = 91 || c = 93 || c = 123 || c = 125)

   (lall? p s) (: n (tally s)
                  (go k) (? (n <= k) (0 < n) (p (s k)) (go (k + 1)) ())
                  (go 0))
   ; the token starting at index i (empty if a delimiter sits there)
   (ltok-at src n i) (: (go k) (? (k < n && ltkc? (src k)) (go (k + 1)) k)
                        (snip src i (go i)))

   ; --- the config the rules read --------------------------------------------
   ; ⚠ lint.l takes a PLAIN TABLET and reads it with peep -- it does NOT depend on
   ; lib/salt.l. vi cats this file directly (test/test.mk), and a module it had to
   ; carry along would break that. salt fills the tablet; lint only reads it.
   (lint-on? conf k) (: v (peep conf k ()) (two? v && <v))
   ; ...and a switch whose default is YES: absent means on, and only an explicit 0
   ; turns it off. the opinion rules are off until asked; the halves of a rule that
   ; has BEEN asked for are on until refused.
   (lint-yes? conf k) (: v (peep conf k ()) (? (two? v) (! (! <v)) 1))
   ; the name a token MENTIONS: a leading ' is quote, and 'foo names foo just as
   ; much as bare foo does -- a rename wants both. (a TRAILING ' is the prime
   ; idiom, part of the name, and stays.)
   (lname t) (: n (tally t) (go k) (? (k < n && t k = 39) (go (k + 1)) (snip t k n)) (go 0))
   ; the roster entry for the token text t: a bare name, or (name "hint").
   (lint-dep roster t)
    (? !(two? roster) ()
       (: e <roster
          nm (? (two? e) <e e)
          (? (string nm = t) [1 e] (lint-dep >roster t))))
   (lint-depmsg e)
    (: nm (? (two? e) <e e)
       h  (? (two? e && two? >e) <>e ())
       ("deprecated " + string nm + (? (string? h) (" -- " + h) "")))

   ; --- the shadow rule ------------------------------------------------------
   ; the words a glued row names (`operators` at arity 0). a glued sigil factors to ONE of these
   ; and then
   ; resolves like any other name, so a binding of one IN SCOPE quietly re-aims every
   ; `<x` / `!x` / `+x` under it -- rare, real and silent, which is what a rule is for.
   ; ⚠ this does NOT reserve the names: the rule speaks, the binding stands.
   lshadow-words '("cap" "cup" "net" "prod" "abs" "negate" "reciprocal" "fraction"
                   "bit" "saturate" "nil?" "dot")
   (lsmem? l t) (? !(two? l) 0 (t = <l) 1 (lsmem? >l t))
   ; is frame f a BINDER slot -- a `\` past its head, or a `:` at an even (value)
   ; element? shared by the shadow rule (is the token that just ended a binding
   ; name?) and the singleton deferral (does this child sit in pattern position?).
   (lbind? src n f)
    (f @ {'e1 e1 'ne ne}
     (: h (ltok-at src n e1)
        (? (h = ":")  (2 <= ne && 0 = ne % 2)
           (h = "\\") (2 <= ne)
           0)))
   ; is the token that just ended a BINDING NAME? this shape over-fires on a
   ; BARE-NAME BODY ((: a 1 cap), (\ x cap)) -- the last operand is the body, and
   ; which element is last is not known until the closer. that shape is rare
   ; enough to cost one line of noise.
   (lbind-pos? src n stk) (? (two? stk) (lbind? src n <stk) 0)
   ; the SUGAR HEADER (: (bit x) ..): at token time element 1 of a form at an ODD `:`
   ; index, which a body call (: a 1 (cap x)) also is. the two part at the `:` CLOSER,
   ; where the final count is known -- the body is the LAST element and a name slot is
   ; not -- so the verdict defers on the binder's pend slot, the singleton rule's own
   ; machinery run at the opposite polarity.
   (lsugar-pos? src n stk)
    (? (! (two? stk && two? >stk)) 0
       (: f <stk p <>stk
          (? (! (1 = f 'ne)) 0                         ; not this form's FIRST element
             (lbind? src n p))))                       ; ..sitting in the parent's binder slot

   ; --- the frame ------------------------------------------------------------
   ; a TABLET: {'cl closer 'ln line 'co col 'ne nelem 'e1 e1i 'pl plain 'qd quoted
   ; 'am arm 'pd pend} -- the expected closer and where the opener SAT (so an
   ; unclosed one reports its own position), how many elements this form has
   ; taken, the index the FIRST one started at, the two bits the singleton rule
   ; turns on, the match bit, and the deferred verdicts. a tablet PINS IN PLACE,
   ; so nothing rebuilds the stack, and a pattern {'ne ne 'e1 e1} reads slots by
   ; name. the walkers keep plain args: a frame moves once per token, the char
   ; loop every byte.
   ;
   ; ⚠ e1 is an INDEX, read back out of src on demand rather than accumulated as
   ; text -- there is then no token-end bookkeeping to get wrong, and the scan
   ; stays one pass with no per-character allocation.
   (lelem stk i)
    (? !(two? stk) stk
       (: f <stk ne (f 'ne + 1)
          (pin f 'ne ne, (? (1 = ne) (pin f 'e1 i) 0), stk)))

   ; a bare `@` element marks its frame a MATCH: everything after it is arms, and in
   ; a PATTERN (x) is a list shape, not x -- so the singleton rule stands down there.
   ; fires only where a token ends FREE (space, newline, comment); a glued @( is the
   ; tray ctor and never reaches this. lpush inherits the bit, so nested pattern
   ; forms are covered; an arm BODY inherits too, the price of not pairing arms,
   ; and it costs missed gripes only, never noise.
   (lat src stk it ti i)
    (? !(it && two? stk) stk
       !(snip src ti i = "@") stk
       (pin <stk 'am 1, stk))

   ; a new opener. `pre` is what it is GLUED to (empty when it stands free), and
   ; that text decides everything:
   ;   all constructor sigils -> a datum, and a ' among them makes the CONTENTS
   ;                             data too (inherited by every frame inside)
   ;   all operator chars     -> ⚠ THE PARENS MAY BE LOAD-BEARING, and we cannot
   ;                             tell which. glued, two sigil runs LEX AS ONE, and
   ;                             sometimes that is the same value and sometimes a
   ;                             DIFFERENT OPERATOR: <(<l) survives (<< composes to
   ;                             the same caap), but <(= x) is (< (= x)) while <=x
   ;                             is the single operator <=. telling those apart
   ;                             needs the operator table, which a balance scan has
   ;                             no business knowing -- so every glued run is exempt.
   ;   anything else          -> foo(x) is two datums to the reader, so the ( is a
   ;                             plain form after all
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

   ; the SINGLETON warning for a form about to close, or (). a form of one element
   ; is that element -- (f) == f at zero operands -- so the parens do nothing and
   ; a nullary call never fires. EXCEPT when the one element is ALL PUNCTUATION:
   ; (+) is + as a value, the documented escape idiom, and (:) (?) (\) read their
   ; own zero point. ⚠ that exemption is narrower than "starts with punctuation"
   ; on purpose -- (<>b) IS droppable (probed: (<>l) and <>l are one value), and
   ; flagging it is the point. only the GLUED case above is load-bearing.
   ; a singleton in PATTERN position is a list shape, not x, so the rule stands down
   ; there -- but which positions are patterns is only known when the BINDER closes
   ; (the last element is the body). so: under a match's @ the whole subtree is arms
   ; and stays silent; a singleton that is an element of a `\`, an even element of a
   ; `:`, or one level inside such an element DEFERS its verdict to that binder's
   ; close, where an entry at the FINAL index was the body (warn) and any other was
   ; a pattern (silent). odd `:` elements are values and warn at once. patterns
   ; nested deeper than one level are rare and respell (x >< _).
   (lsing src n f)
    (f @ {'pl 1 'ne 1 'e1 e1 'ln ln 'co co}
     (: t (ltok-at src n e1)
        (? (lall? lopc? t) ()
           [ln co ("singleton " + (? (0 < tally t) ("(" + t + ") ") "") + "-- (x) == x")
            'singleton]))
     ())
   ; the deferral home: () to warn NOW, else [depth idx] naming the binder frame
   ; (1 parent, 2 grandparent) and the element index the verdict keys on.
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
   ; (lint-of src conf) is the whole weighing; (lint-scan src) is it with no
   ; config, which runs BALANCE ALONE and is exactly what this file has always
   ; answered. the extra rules cost nothing until a config turns them on.
   ;
   ; ⚠ the 4th field, the RULE, is APPENDED -- every consumer reads <w / <>w /
   ; <>>w for line, column and text and none of them had to change.
   (lint-scan src) (lint-of src ())

   (lint-of src conf)
    (: n  (tally src)
       ; singleton is ON by default (2026-08-16; three fresh nullary-define bugs in one
       ; session of new code); (singleton 0) turns it off. NO config at all -- lint-scan's
       ; () -- stays balance alone, the contract vi's cat and the fixtures ride.
       sg (? (id? conf ()) 0 (lint-yes? conf 'singleton))
       sh (lint-on? conf 'shadow)
       dp (peep conf 'deprecated ())
       ; a token just ENDED at index i, having started at ti/tc: the two token rules
       ; run here, and each is a no-op until its config asks -- an empty roster for
       ; `deprecated`, an unset switch for `shadow`.
       (lend line tc ti i stk warns)
        (: t  (snip src ti i)
           w0 (? !(two? dp) warns
                 (: r (lint-dep dp (lname t))
                    (? (two? r) (link [line tc (lint-depmsg <>r) 'deprecated] warns) warns)))
           shw [line tc ("shadow " + t + " -- a glued sigil factors to this name") 'shadow]
           (? (! (sh && lsmem? lshadow-words t)) w0
              (lbind-pos? src n stk) (link shw w0)
              (lsugar-pos? src n stk) (: _ (lpend stk 2 (<>stk 'ne) shw) w0)
              w0))

       ; stk: the frames above, a matched-pair stack, so ([)] is caught and not
       ; just a net count. sl/sc hold a string's opening spot. it/ti/tc are the
       ; token in hand -- ⚠ `it` must fire ONCE PER TOKEN, at its first character,
       ; never once per character, or a three-letter name counts as three
       ; elements and its second letter is mistaken for the form's first operand.
       ; states: code | cmt (; to EOL) | str (".." with \ escapes).
       (w i st stk line col sl sc it ti tc warns)
        (? (n <= i)
            (: w0 (? it (lend line tc ti i stk warns) warns)
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
                     (? it (lend line tc ti i stk warns) warns))
                 (c = 35 && i + 1 < n && src (i + 1) = 33)                     ; #! shebang, to EOL
                  (w (i + 2) 'cmt stk line (col + 2) sl sc 0 ti tc
                     (? it (lend line tc ti i stk warns) warns))
                 (c = 34)                                                      ; " -- start string
                  (w (i + 1) 'str (? it stk (lelem stk i)) line (col + 1) line col 0 ti tc
                     (? it (lend line tc ti i stk warns) warns))
                 (c = 10)
                  (w (i + 1) 'code (lat src stk it ti i) (line + 1) 1 sl sc 0 ti tc
                     (? it (lend line tc ti i stk warns) warns))
                 (c = 40 || c = 91 || c = 123)                                 ; ( [ {  -- push
                  ; an opener is an element of its PARENT -- unless it is GLUED to
                  ; the token before it, in which case the two are one datum.
                  (w (i + 1) 'code (lpush (? it stk (lelem stk i)) c i line col src it ti)
                     line (col + 1) sl sc 0 ti tc
                     (? it (lend line tc ti i stk warns) warns))
                 (c = 41 || c = 93 || c = 125)                                 ; ) ] }  -- match
                  (: w0 (? it (lend line tc ti i stk warns) warns)
                     (? !(two? stk)
                         (w (i + 1) 'code stk line (col + 1) sl sc 0 ti tc
                            (link [line col ("stray " + string c) 'balance] w0))
                        !(<stk 'cl = c)
                         (w (i + 1) 'code >stk line (col + 1) sl sc 0 ti tc
                            (link [line col ("mismatched " + string c) 'balance] w0))
                        (: f <stk rest >stk
                           ; the verdicts this binder owed its deferred children. an entry
                           ; at the FINAL index was the BODY: a singleton there warns, and
                           ; a shadow there does not -- the two rules read the same fact
                           ; from opposite sides.
                           w1 (foldl (\ a p (: wr >p last? (<p = f 'ne)
                                              (? (id? (wr 3) 'shadow) (? last? a (link wr a))
                                                 last? (link wr a)
                                                 a)))
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
                     (? it (lend line tc ti i stk warns) warns))
                 ; an ordinary character: a token START only if we were not in one
                 (w (i + 1) 'code (? it stk (lelem stk i)) line (col + 1) sl sc
                    1 (? it ti i) (? it tc col) warns))))
       (w 0 'code () 1 1 1 1 0 0 1 ()))

   ; --- the comments ---------------------------------------------------------
   ; (lint-cmts src) answers every comment in reading order, [line col text kind]
   ; -- line and col 1-BASED at the INTRODUCER, text the bytes after it to the end
   ; of the line, kind 'semi or 'bang (`;` or `#!`). the fourth walk, and the
   ; smallest: it wants no stack, only the three states, which is exactly what
   ; keeps a `;` inside a string from being a comment and a `"` inside a comment
   ; from opening one.
   ;
   ; ⚠ THE TEXT, not just the span, and that is the whole reason this exists: no
   ; reader in the tree keeps comments (`sound` drops them, `p0` drops them), so
   ; anything that documents a file FROM the file has to walk the text. lib/ldoc.l
   ; is the consumer -- one more rider on this scanner rather than a fifth
   ; independent encoding of what .l lexing means.
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
   ; a frame is {'base b 'ol openline 'nt ntok 'pr pair}: where this form's
   ; continuation lines align (0-based), the line it opened on, how many of its
   ; tokens we have passed (the HEAD is token 1), and whether it is a PAIRING form.
   ;
   ; ⚠ THE PAIRING RULE is the whole difference between a formatter that is close
   ; and one that is right. `:` and `?` take their operands TWO AT A TIME -- : is
   ; name/value, ? is test/result -- and the house sets the second of each pair
   ; ONE PAST the first, so a value or a result is never mistaken for another name
   ; or another test at a glance. every other form aligns all its operands flat.
   ; without this the pass is off by one space on every second operand, which is
   ; most of a file.
   ; ⚠ A WIDTH GUARD WAS TRIED HERE AND PAYS NOWHERE -- do not re-add it without
   ; new evidence. the idea: cap alignment, so a first operand sitting far right
   ; falls back to one past its own open paren (black's bargain), on the theory
   ; that it would rescue the hand-laid HANGING files. measured over the tree at
   ; thresholds 24/32/40/56: the hanging files barely move (love/ev.l 494 -> 506
   ; changed lines, test/holo/golden.l flat), because their authors broke to a
   ; column LEFT of the enclosing paren -- which no cap-and-fallback reproduces --
   ; while every well-laid file gets worse (crew/libra/libra.l 7 -> 22, this file
   ; 1 -> 4) and tree-wide churn RISES from 12591 to 16448. alignment it is.
   (ldelim? c) (c < 33 || c = 40 || c = 41 || c = 91 || c = 93 || c = 123 || c = 125)
   (lpair? c nx) ((c = 58 || c = 63) && ldelim? nx)     ; a LONE : or ? , not a name starting with one

   ; the next line's indent: the frame's base, plus one when the token that will
   ; start it is the second of a pair
   (lcont stk)
    (? !(two? stk) 0
       (<stk @ {'base base 'nt nt 'pr pr}
        (? (pr && 0 = nt % 2) (base + 1) base)))

   ; the HEAD token just ENDED at 1-based column x (the delimiter after it). a form
   ; whose head stands ALONE on its line -- `(:` opening a module, `(assert` opening
   ; a block -- indents its body where operand ONE would have gone had it been
   ; written there: one space past the head. so a `(:` body lands at 3 and an
   ; `(assert` body at 8, which is what the house does and what "open paren + 1"
   ; never gets right.
   ;
   ; ⚠ ONLY the pairing forms. `(assert` alone on its line indents its body
   ; ONE past the open paren, while `(:` alone indents THREE -- the house
   ; style is per-head, the way every lisp's indent spec is, and one rule
   ; forced over both moves whichever file it was not written for.
   (lhend stk line x)
    (? !(two? stk) stk
       (<stk @ ({'nt 1 'pr 1 'ol ol} & ol = line)
        (pin <stk 'base x, stk)
        stk))

   ; a token (or a nested opener -- it is a token to its parent) begins at 1-based
   ; column tc; c is its first character and nx the one after
   (ltok stk line tc c nx)
    (? !(two? stk) stk
       (: f <stk
          (f @ {'nt 0} (pin f 'nt 1, pin f 'pr (? (lpair? c nx) 1 0), stk)
             ; operand one: this is where the form aligns
             ({'nt 1 'ol ol} & ol = line) (pin f 'base (tc - 1), pin f 'nt 2, stk)
             (pin f 'nt (f 'nt + 1), stk))))

   ; ⚠ `it` is 1 while we are INSIDE a token: ltok must fire once per token, at
   ; its first character, never once per character -- a three-letter name would
   ; otherwise count as three tokens and its second letter would be mistaken for
   ; the form's first operand.
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
                 ; ⚠ the head's end must land BEFORE the plan is read, or the very
                 ; line the body starts on still carries the old fallback
                 (c = 10) (: s2 (? it (lhend stk line col) stk)
                             (w (i + 1) 'code s2 (line + 1) 1 0 (link [1 (lcont s2)] plan)))
                 (c = 40 || c = 91 || c = 123)
                  ; an opener is a token to its PARENT -- UNLESS it is GLUED to the
                  ; token before it. ⚠ `~(0 1)`, `'(a b)`, `#(k v)`, `@(1 2)` are ONE
                  ; operand wearing a reader sigil, not two, and counting them as two
                  ; flips the pairing parity for every line left in the form. it is
                  ; the one miscount that CASCADES, so it is the one worth naming:
                  ; `it` is already "the previous character was part of a token".
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
