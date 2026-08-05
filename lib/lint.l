; lib/lint.l -- the .l balance scan, structured so its consumers can share one
; scanner: `libra check` prints what it finds and gates `make lint`,
; `libra` (crew/libra/libra.l) turns the same warnings into lsp diagnostics. it
; reads .l correctly -- `;` and `#!` start comments to end of line, `"..."` is
; the only string, and `'` / backtick are READER OPERATORS (quote, list-ctor),
; never delimiters, so they scan as ordinary code (the trap a C lexer falls
; into). it is a BALANCE scan, not a parse: it knows delimiters and strings and
; nothing else about the language.
;
; (lint-scan src) answers a list of warnings, each `(line col text) with line
; and col 1-BASED -- a position, so a consumer can point AT the character rather
; than re-derive one from a sentence. no warnings is (), which here means what
; it says: nothing to report.
;
; an UNCLOSED opener reports where it OPENED, not end-of-file -- the stack
; carries each opener's position for exactly that -- and an unclosed string the
; same way. (a dropped paren is the classic .l slip; pointing at the far end of
; the file is the least useful place to point.)
; (lint-lay src) answers the INDENT PLAN: one entry per line, `(1 n) for "this
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
; tables) survives untouched for exactly that reason.
(: (lclose c) (? (c = 40) 41 (c = 91) 93 125)          ; ( -> )   [ -> ]   { -> }
   (lopen c)  (? (c = 41) 40 (c = 93) 91 123)          ; ...and back, for the report

   ; --- character classes ----------------------------------------------------
   ; is the charm c anywhere in s? the three sets below are spelled as STRINGS
   ; rather than as || chains, because a reader can check them against love.c at
   ; a glance and a || chain of seventeen cannot be read at all.
   (lin? s c) (: n (tally s) (go k) (? (n <= k) () ((s k) = c) 1 (go (k + 1))) (go 0))

   ; the OPERATOR run characters (love.c's op_break, complemented) -- the chars
   ; that fuse into one sigil when they touch.
   (lopc? c) (lin? "!$%&*+-./:<=>?\\^|" c)
   ; the CONSTRUCTOR sigils: a ( glued behind one of these is a DATUM, not a
   ; form -- '(a) quote, `(a) list, #(a) map, @(a) tray, ~(a) twin (love.c's
   ; ioparse). that is the exhaustive list.
   (lctc? c) (lin? "'`#@~" c)
   ; a token character: anything that is not whitespace, a delimiter, a quote
   ; mark or a comment start.
   (ltkc? c) (! (|| (c < 33) (|| (c = 34) (|| (c = 59) (|| (c = 40) (|| (c = 41)
                   (|| (c = 91) (|| (c = 93) (|| (c = 123) (c = 125))))))))))

   (lall? p s) (: n (tally s)
                  (go k) (? (n <= k) (0 < n) (p (s k)) (go (k + 1)) ())
                  (go 0))
   ; the token starting at index i (empty if a delimiter sits there)
   (ltok-at src n i) (: (go k) (? (&& (k < n) (ltkc? (src k))) (go (k + 1)) k)
                        (snip src i (go i)))

   ; --- the config the rules read --------------------------------------------
   ; ⚠ lint.l takes a PLAIN TABLET and reads it with peep -- it does NOT depend on
   ; lib/salt.l. vi cats this file directly (test/test.mk), and a module it had to
   ; carry along would break that. salt fills the tablet; lint only reads it.
   (lint-on? conf k) (: v (peep conf k ()) (&& (two? v) <v))
   ; ...and a switch whose default is YES: absent means on, and only an explicit 0
   ; turns it off. the opinion rules are off until asked; the halves of a rule that
   ; has BEEN asked for are on until refused.
   (lint-yes? conf k) (: v (peep conf k ()) (? (two? v) (! (! <v)) 1))
   ; the name a token MENTIONS: a leading ' is quote, and 'foo names foo just as
   ; much as bare foo does -- a rename wants both. (a TRAILING ' is the prime
   ; idiom, part of the name, and stays.)
   (lname t) (: n (tally t) (go k) (? (&& (k < n) ((t k) = 39)) (go (k + 1)) (snip t k n)) (go 0))
   ; the roster entry for the token text t: a bare name, or `(name "hint").
   (lint-dep roster t)
    (? (! (two? roster)) ()
       (: e <roster
          nm (? (two? e) <e e)
          (? ((string nm) = t) `(1 e) (lint-dep >roster t))))
   (lint-depmsg e)
    (: nm (? (two? e) <e e)
       h  (? (&& (two? e) (two? >e)) <>e ())
       ("deprecated " + (string nm) + (? (string? h) (" -- " + h) "")))

   ; --- the frame ------------------------------------------------------------
   ; `(closer line col nelem e1i plain quoted): the expected closer and where the
   ; opener SAT (so an unclosed one reports its own position), how many elements
   ; this form has taken, the index the FIRST one started at, and the two bits the
   ; singleton rule turns on.
   ;
   ; ⚠ e1i is an INDEX, read back out of src on demand rather than accumulated as
   ; text -- there is then no token-end bookkeeping to get wrong, and the scan
   ; stays one pass with no per-character allocation.
   (lelem stk i)
    (? (! (two? stk)) stk
       (: f <stk
          ne (<>>>f + 1)
          (link `(<f <>f <>>f ne (? (1 = ne) i <>>>>f) <>>>>>f <>>>>>>f) >stk)))

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
       pq  (? (two? stk) (: pf <stk <>>>>>>pf) 0)
       ct  (lall? lctc? pre)
       op  (lall? lopc? pre)
       qd  (? (|| pq (&& ct (lin? pre 39))) 1 0)
       pl  (? (|| pq (|| ct op)) 0 (? (c = 40) 1 0))
       (link `((lclose c) line col 0 0 pl qd) stk))

   ; the SINGLETON warning for a form about to close, or (). a form of one element
   ; is that element -- (f) == f at zero operands -- so the parens do nothing and
   ; a nullary call never fires. EXCEPT when the one element is ALL PUNCTUATION:
   ; (+) is + as a value, the documented escape idiom, and (:) (?) (\) read their
   ; own zero point. ⚠ that exemption is narrower than "starts with punctuation"
   ; on purpose -- (<>b) IS droppable (probed: (<>l) and <>l are one value), and
   ; flagging it is the point. only the GLUED case above is load-bearing.
   (lsing src n f)
    (? (&& <>>>>>f (1 = <>>>f))
        (: t (ltok-at src n <>>>>f)
           (? (lall? lopc? t) ()
              `(<>f <>>f ("singleton " + (? (0 < (tally t)) ("(" + t + ") ") "")
                          + "-- (x) == x") 'singleton)))
       ())

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
       sg (lint-on? conf 'singleton)
       dp (peep conf 'deprecated ())
       ; a token just ENDED at index i, having started at ti/tc: the deprecated
       ; roster is the only rule that reads whole tokens, so it is the only work
       ; here, and an empty roster makes it a no-op.
       (lend line tc ti i warns)
        (? (! (two? dp)) warns
           (: r (lint-dep dp (lname (snip src ti i)))
              (? (two? r) (link `(line tc (lint-depmsg <>r) 'deprecated) warns) warns)))

       ; stk: the frames above, a matched-pair stack, so ([)] is caught and not
       ; just a net count. sl/sc hold a string's opening spot. it/ti/tc are the
       ; token in hand -- ⚠ `it` must fire ONCE PER TOKEN, at its first character,
       ; never once per character, or a three-letter name counts as three
       ; elements and its second letter is mistaken for the form's first operand.
       ; states: code | cmt (; to EOL) | str (".." with \ escapes).
       (w i st stk line col sl sc it ti tc warns)
        (? (n <= i)
            (: w0 (? it (lend line tc ti i warns) warns)
               w1 (? (id? st 'str) (link `(sl sc "unclosed string" 'balance) w0) w0)
               (rev (foldl (\ a o (link `(<>o <>>o ("unclosed " + string (lopen <o)) 'balance) a)) w1 stk)))
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
                  (w (i + 1) 'cmt stk line (col + 1) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (&& (c = 35) (&& ((i + 1) < n) ((src (i + 1)) = 33)))          ; #! shebang, to EOL
                  (w (i + 2) 'cmt stk line (col + 2) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (c = 34)                                                      ; " -- start string
                  (w (i + 1) 'str (? it stk (lelem stk i)) line (col + 1) line col 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (c = 10)
                  (w (i + 1) 'code stk (line + 1) 1 sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (|| (c = 40) (|| (c = 91) (c = 123)))                          ; ( [ {  -- push
                  ; an opener is an element of its PARENT -- unless it is GLUED to
                  ; the token before it, in which case the two are one datum.
                  (w (i + 1) 'code (lpush (? it stk (lelem stk i)) c i line col src it ti)
                     line (col + 1) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 (|| (c = 41) (|| (c = 93) (c = 125)))                          ; ) ] }  -- match
                  (: w0 (? it (lend line tc ti i warns) warns)
                     (? (! (two? stk))
                         (w (i + 1) 'code stk line (col + 1) sl sc 0 ti tc
                            (link `(line col ("stray " + string c) 'balance) w0))
                        (! (<<stk = c))
                         (w (i + 1) 'code >stk line (col + 1) sl sc 0 ti tc
                            (link `(line col ("mismatched " + string c) 'balance) w0))
                        (: sw (? sg (lsing src n <stk) ())
                           (w (i + 1) 'code >stk line (col + 1) sl sc 0 ti tc
                              (? (two? sw) (link sw w0) w0)))))
                 (c < 33)                                                      ; space, tab, the rest
                  (w (i + 1) 'code stk line (col + 1) sl sc 0 ti tc
                     (? it (lend line tc ti i warns) warns))
                 ; an ordinary character: a token START only if we were not in one
                 (w (i + 1) 'code (? it stk (lelem stk i)) line (col + 1) sl sc
                    1 (? it ti i) (? it tc col) warns))))
       (w 0 'code () 1 1 1 1 0 0 1 ()))

   ; --- the indent plan ------------------------------------------------------
   ; a frame is `(base openline ntok pair): where this form's continuation lines
   ; align (0-based), the line it opened on, how many of its tokens we have passed
   ; (the HEAD is token 1), and whether it is a PAIRING form.
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
   ; changed lines, crew/holo/holotest.l flat), because their authors broke to a
   ; column LEFT of the enclosing paren -- which no cap-and-fallback reproduces --
   ; while every well-laid file gets worse (crew/libra/libra.l 7 -> 22, this file
   ; 1 -> 4) and tree-wide churn RISES from 12591 to 16448. alignment it is.
   (ldelim? c) (|| (c < 33) (|| (c = 40) (|| (c = 41) (|| (c = 91)
                                (|| (c = 93) (|| (c = 123) (c = 125)))))))
   (lpair? c nx) (&& (|| (c = 58) (c = 63)) (ldelim? nx))     ; a LONE : or ? , not a name starting with one

   ; the next line's indent: the frame's base, plus one when the token that will
   ; start it is the second of a pair
   (lcont stk) (? (! (two? stk)) 0
                  (: f <stk
                     base <f
                     nt <>>f
                     pr <>>>f
                     (? (&& pr (0 = (nt % 2))) (base + 1) base)))

   ; the HEAD token just ENDED at 1-based column x (the delimiter after it). a form
   ; whose head stands ALONE on its line -- `(:` opening a module, `(assert` opening
   ; a block -- indents its body where operand ONE would have gone had it been
   ; written there: one space past the head. so a `(:` body lands at 3 and an
   ; `(assert` body at 8, which is what the house does and what "open paren + 1"
   ; never gets right.
   (lhend stk line x)
    (? (! (two? stk)) stk
       (: f <stk
          ol <>f
          nt <>>f
          pr <>>>f
          ; ⚠ ONLY the pairing forms. `(assert` alone on its line indents its body
          ; ONE past the open paren, while `(:` alone indents THREE -- the house
          ; style is per-head, the way every lisp's indent spec is, and one rule
          ; forced over both moves whichever file it was not written for.
          (? (&& pr (&& (nt = 1) (ol = line)))
              (link `(x ol 1 pr) >stk)
             stk)))

   ; a token (or a nested opener -- it is a token to its parent) begins at 1-based
   ; column tc; c is its first character and nx the one after
   (ltok stk line tc c nx)
    (? (! (two? stk)) stk
       (: f <stk
          base <f
          ol <>f
          nt <>>f
          pr <>>>f
          (? (nt = 0)                (link `(base ol 1 (? (lpair? c nx) 1 0)) >stk)
             ; operand one: this is where the form aligns
             (&& (nt = 1) (ol = line)) (link `((tc - 1) ol 2 pr) >stk)
             (link `(base ol (nt + 1) pr) >stk))))

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
                  (? (c = 10) (w (i + 1) 'code stk (line + 1) 1 0 (link `(1 (lcont stk)) plan))
                     (w (i + 1) 'cmt stk line (col + 1) 0 plan))
                 (c = 59) (w (i + 1) 'cmt stk line (col + 1) 0 plan)
                 (&& (c = 35) (&& ((i + 1) < n) ((src (i + 1)) = 33)))
                  (w (i + 2) 'cmt stk line (col + 2) 0 plan)
                 (c = 34) (w (i + 1) 'str (ltok stk line col c 32) line (col + 1) 1 plan)
                 ; ⚠ the head's end must land BEFORE the plan is read, or the very
                 ; line the body starts on still carries the old fallback
                 (c = 10) (: s2 (? it (lhend stk line col) stk)
                             (w (i + 1) 'code s2 (line + 1) 1 0 (link `(1 (lcont s2)) plan)))
                 (|| (c = 40) (|| (c = 91) (c = 123)))
                  ; an opener is a token to its PARENT -- UNLESS it is GLUED to the
                  ; token before it. ⚠ `~(0 1)`, `'(a b)`, `#(k v)`, `@(1 2)` are ONE
                  ; operand wearing a reader sigil, not two, and counting them as two
                  ; flips the pairing parity for every line left in the form. it is
                  ; the one miscount that CASCADES, so it is the one worth naming:
                  ; `it` is already "the previous character was part of a token".
                  (w (i + 1) 'code
                     (link `(col line 0 0) (? it stk (ltok stk line col c 32)))
                     line (col + 1) 0 plan)
                 (|| (c = 41) (|| (c = 93) (c = 125)))
                  (w (i + 1) 'code (? (two? stk) >stk stk) line (col + 1) 0 plan)
                 (|| (c = 32) (c = 9))
                  (w (i + 1) 'code (? it (lhend stk line col) stk) line (col + 1) 0 plan)
                 ; an ordinary character: a token START only if we were not in one
                 (w (i + 1) 'code
                    (? it stk (ltok stk line col c (? ((i + 1) < n) (src (i + 1)) 10)))
                    line (col + 1) 1 plan))))
       (w 0 'code () 1 1 0 (link `(1 0) ())))

   ; --- the drop plan --------------------------------------------------------
   ; (lint-drops src conf) answers the byte indexes to DELETE, ascending -- two per
   ; dropped pair, the `(` and its mate -- so `libra fmt -p` can minify parens the
   ; way libra-lay applies lint-lay's plan: lint says WHERE, the tool rewrites. a
   ; THIRD walk rather than a rider on the balance scan, because that scan is the
   ; gate and nothing that is not balance belongs in its frame.
   ;
   ; TWO predicates ride here, and they rest on DIFFERENT laws:
   ;   a SPAN paren -- ("exit " + (show ec) + ")") -- is decided by OPFIX. splice
   ;     the child's elements into its parent's operand run, fold both, compare:
   ;     equal cores mean the parens did nothing. op-core recurses structurally, so
   ;     a form's fold reads only its OWN operand list -- the test is LOCAL, the
   ;     file is never read twice, and the precedence law is BORROWED rather than
   ;     copied. libra cannot drift from the compiler because it asks the compiler.
   ;   a SINGLETON -- (<>b) -- is decided by (f) == f, which opfix DECLINES to
   ;     apply: it keeps ((mov r3 r12)) wrapped on purpose, since a list of one may
   ;     be a program (has-top-op?, love/prel.l). so that half stays `lsing`'s law.
   ;
   ; ⚠ AND BOTH ARE GATED LEXICALLY, because the datum cannot see what a DELETION
   ; does to the text. ((show x)+"\n") reads fine and folds equal, but dropping the
   ; bytes leaves `x+` as ONE token, a different program. so a pair drops only when
   ; it stands FREE -- a delimiter, a comment or a quote on both outer sides. that
   ; one rule also disposes of foo(x), '(x), `(a b) and every sigil-glued run, and
   ; it is nearly free: 17 of the tree's 193389 code parens are right-glued.

   ; the frame: `(closer oi ne e1 pl qd kx ks) -- the expected closer, the opener's
   ; BYTE INDEX, how many elements this form has taken, where the first one started,
   ; whether the parens are plain (not sigil- or operator-glued), whether we sit
   ; inside a quote (data: hands off), this form's own index among its PARENT's
   ; elements, and the kids that closed inside it (`(ki kj kx) each, newest first).
   ; named accessors, because eight fields of <>>>>>> cannot be read.
   (ldf-cl f) <f
   (ldf-oi f) <>f
   (ldf-ne f) <>>f
   (ldf-e1 f) <>>>f
   (ldf-pl f) <>>>>f
   (ldf-qd f) <>>>>>f
   (ldf-kx f) <>>>>>>f
   (ldf-ks f) <>>>>>>>f
   (ldf-mk cl oi ne e1 pl qd kx ks) `(cl oi ne e1 pl qd kx ks)

   (ldelem stk i)
    (? (! (two? stk)) stk
       (: f <stk ne ((ldf-ne f) + 1)
          (link (ldf-mk (ldf-cl f) (ldf-oi f) ne (? (1 = ne) i (ldf-e1 f))
                        (ldf-pl f) (ldf-qd f) (ldf-kx f) (ldf-ks f)) >stk)))

   ; a new opener, classified exactly as lpush classifies it. ⚠ ldelem must run
   ; BEFORE this, so `kx` reads the parent's own count for the slot this form fills.
   (ldpush stk c i src it ti)
    (: pre (? it (snip src ti i) "")
       pq  (? (two? stk) (ldf-qd <stk) 0)
       ct  (lall? lctc? pre)
       op  (lall? lopc? pre)
       qd  (? (|| pq (&& ct (lin? pre 39))) 1 0)
       pl  (? (|| pq (|| ct op)) 0 (? (c = 40) 1 0))
       kx  (? (two? stk) ((ldf-ne <stk) - 1) 0)
       (link (ldf-mk (lclose c) i 0 0 pl qd kx ()) stk))

   ; register a closed form with its parent, iff it is ELIGIBLE for a span drop.
   (ldkid stk ki kj kx ok)
    (? (! (two? stk)) stk
       (! ok) stk
       (: f <stk
          (link (ldf-mk (ldf-cl f) (ldf-oi f) (ldf-ne f) (ldf-e1 f) (ldf-pl f)
                        (ldf-qd f) (ldf-kx f) (link `(ki kj kx) (ldf-ks f))) >stk)))

   ; a byte that cannot FUSE with an adjacent token: a delimiter, a comment start,
   ; or a string quote.
   (ldsep? c) (|| (ldelim? c) (|| (c = 59) (c = 34)))
   (ldfree? src n i j)
    (&& (? (i < 1) 1 (ldsep? (src (i - 1))))
        (? ((j + 1) < n) (ldsep? (src (j + 1))) 1))

   ; ⚠ ELIGIBILITY, and it is a matter of TASTE rather than of law. a child whose
   ; HEAD is punctuation folds the same either way -- (? (! (two? l)) ..) and
   ; (? ! (two? l) ..) are one program -- and nobody would write the second. the
   ; formatter must emit what you would have written, so the span half takes
   ; APPLICATION and INFIX children only, and leaves ! ? : \ + alone. sigil-glued
   ; parens are already out (pl), and so is a form with nothing in it.
   (ldeligible? src n f)
    (&& (ldf-pl f)
        (&& (0 < (ldf-ne f))
            (! (lall? lopc? (ltok-at src n (ldf-e1 f))))))

   (lany? p s) (: n (tally s)
                  (go k) (? (n <= k) () (p (s k)) 1 (go (k + 1)))
                  (go 0))
   ; the singleton half: `lsing`'s law, plus ONE exemption the formatter needs. a
   ; sole element carrying no operator character is a BARE NAME -- (go), (loop),
   ; (step) -- the nullary trap, the closure handed back unrun. dropping those
   ; parens is semantics-preserving (it was already a no-op) and it DELETES THE
   ; EVIDENCE, so fmt leaves them for `check`'s singleton rule to speak about.
   ; (drop-nullary 1) takes them anyway.
   ;
   ; ⚠ AND THIS HALF CANNOT BE VERIFIED, which is why it has its own key. the span
   ; half is checkable against opfix -- fold both ways, compare -- and the tree
   ; answers 303 files clean. this half rests on (f) == f, a law opfix DECLINES to
   ; apply, so there is no oracle to check it with; and that law is false for a
   ; MACRO, which reads its operand's SHAPE rather than its value. libra cannot know
   ; which heads are macros. it is on by default and (drop-singles 0) turns it off.
   (ldsing? src n f sg nu)
    (&& sg
     (&& (ldf-pl f)
        (&& (1 = (ldf-ne f))
            (: t (ltok-at src n (ldf-e1 f))
               (? (lall? lopc? t) ()              ; (+) (:) (?) -- the escape idiom
                  ; ⚠ a sole element that is ITSELF A FORM stays wrapped, and this
                  ; is opfix's call, not ours: has-top-op? keeps ((mov r3 r12))
                  ; whole because a list of one may be DATA -- a program, a type
                  ; signature. overriding it rewrote ((mat m n)) to (mat m n) in
                  ; doc/proto/overlay-tele.l, which is a different value.
                  (0 = (tally t)) ()
                  (|| nu (lany? lopc? t)))))))    ; (<>b) yes; (go) only if asked

   ; the i'th element of l replaced by its OWN elements
   (ldsplice l i) (? (! (two? l)) l
                     (0 < i) (link <l (ldsplice >l (i - 1)))
                     (cat <l >l))
   ; f has closed: weigh every kid that closed inside it against f's own text.
   ; the read starts at f's opener and stops at f's own closer, so `spot` names
   ; the place and copies nothing. one read and one base fold per FORM, then one
   ; fold per kid.
   (ldspans src n f ks acc)
    (: r (sound (spot src (ldf-oi f)))
       (? (! (two? r)) acc
          (! (two? <r)) acc
          (: x <r
             base (opfix x)
             (go l a)
              (? (! (two? l)) a
                 (: k <l ki <k kj <>k kx <>>k
                    (go >l (? (&& (ldfree? src n ki kj)
                                  (base = (opfix (ldsplice x kx))))
                              (link kj (link ki a)) a))))
             (go ks acc))))

   ; the walk. states and glue are lint-of's exactly -- ⚠ `it` fires ONCE PER TOKEN,
   ; at its first character, or a three-letter name counts as three elements and
   ; every kid index below it is wrong.
   (ldscan src conf)
    (: n  (tally src)
       sg (lint-yes? conf 'drop-singles)
       nu (lint-on? conf 'drop-nullary)
       (w i st stk it ti acc)
        (? (n <= i) acc
           (: c (src i)
              (? (id? st 'str)
                  (? (c = 92) (w (i + 2) 'str stk 0 ti acc)
                     (c = 34) (w (i + 1) 'code stk 0 ti acc)
                     (w (i + 1) 'str stk 0 ti acc))
                 (id? st 'cmt)
                  (? (c = 10) (w (i + 1) 'code stk 0 ti acc)
                     (w (i + 1) 'cmt stk 0 ti acc))
                 (c = 59) (w (i + 1) 'cmt stk 0 ti acc)
                 (&& (c = 35) (&& ((i + 1) < n) ((src (i + 1)) = 33)))
                  (w (i + 2) 'cmt stk 0 ti acc)
                 (c = 34) (w (i + 1) 'str (? it stk (ldelem stk i)) 0 ti acc)
                 (c = 10) (w (i + 1) 'code stk 0 ti acc)
                 (|| (c = 40) (|| (c = 91) (c = 123)))
                  (w (i + 1) 'code (ldpush (? it stk (ldelem stk i)) c i src it ti) 0 ti acc)
                 (|| (c = 41) (|| (c = 93) (c = 125)))
                  ; ⚠ an imbalanced file drops NOTHING: a mismatched closer pops and
                  ; says nothing rather than guessing. balance is `check`'s errand.
                  (? (! (two? stk)) (w (i + 1) 'code stk 0 ti acc)
                     (! ((ldf-cl <stk) = c)) (w (i + 1) 'code >stk 0 ti acc)
                     (: f <stk
                        a1 (? (&& (c = 41) (two? (ldf-ks f)))
                              (ldspans src n f (rev (ldf-ks f)) acc) acc)
                        sd (&& (ldsing? src n f sg nu) (ldfree? src n (ldf-oi f) i))
                        a2 (? sd (link i (link (ldf-oi f) a1)) a1)
                        (w (i + 1) 'code
                           (? sd >stk (ldkid >stk (ldf-oi f) i (ldf-kx f)
                                              (ldeligible? src n f)))
                           0 ti a2)))
                 (c < 33) (w (i + 1) 'code stk 0 ti acc)
                 (w (i + 1) 'code (? it stk (ldelem stk i)) 1 (? it ti i) acc))))
       (w 0 'code () 0 0 ()))

   ; the indexes ASCENDING. they arrive in close order -- innermost first, a form's
   ; kids after the form itself -- so a tablet turns the pile into a sorted walk and
   ; no sort is needed.
   (lint-drops src conf)
    (: ds (ldscan src conf)
       (? (! (two? ds)) ()
          (: n (tally src)
             (fill m l) (? (! (two? l)) m (: _ (pin m <l 1) (fill m >l)))
             t (fill (tablet 0) ds)
             (go k acc) (? (k < 0) acc (go (k - 1) (? (peep t k ()) (link k acc) acc)))
             (go (n - 1) ()))))
)
