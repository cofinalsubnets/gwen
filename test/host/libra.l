; --- test/host/libra.l: libra ⚖, the .l balance tool (crew/libra/libra.l) ---
; four verbs over one scanner, and this gate is that claim: the formatter, the
; check and the doc lifter all read lib/lint.l, so no two of them can disagree
; about what a paren or a comment is. run standalone from the repo root:
;   cat test/00-init.l test/host/libra.l | out/host/love

(use 'lint)          ; libra splices this BELOW itself, so the session must ask for it too
(use 'salt)
(use 'libra)
(use 'lapiz)         ; ..and the doc verb's surfaces, which libra loads on that verb alone

; the settings a test means, stated rather than inherited: an empty tablet is
; "no config anywhere", which is what every balance test wants. ⚠ never (salt
; 'libra) in a test -- that would read the machine's own ~/.love/etc/libra.l and
; make the gate depend on who is running it.
(: lt-none (tablet 0)
   (lt-conf prs) (foldl (\ t p (: _ (pin t <p >p) t)) (tablet 0) prs))

; --- the formatter and the check ------------------------------------------
; the FORMATTER is a reindenter: it replaces leading whitespace and touches
; nothing else, so a line break, an inside-the-line alignment and a comment all
; survive it. laying out an already-laid-out file is the identity, which is what
; makes `fmt -n` a usable gate.
(assert
 ("(: a 1\n   b 2)\n" = (libra-fmt "(: a 1\n b 2)\n"))          ; a binding aligns under the first
 ("(: a 1\n   b 2)\n" = (libra-fmt (libra-fmt "(: a 1\n b 2)\n")))   ; ...and again is the identity
 ("(? 1 'a\n   2 'b)\n" = (libra-fmt "(? 1 'a\n      2 'b)\n"))      ; ? clauses align, tests flat
 ; a line INSIDE a string keeps its own leading spaces -- they are the string
 (: s "(: a \"one\n   two\")\n" (s = (libra-fmt s)))
 ; trailing whitespace goes, and a whitespace-only line empties
 ("(: a 1)\n" = (libra-fmt "(: a 1)   \n"))
 ("\n(: a 1)\n" = (libra-fmt "   \n(: a 1)\n")))

; the CHECK is the scan itself, laid out for a terminal: one warning here is one
; line there, and the config rides in as an argument rather than being read
(assert
 (1 = (tally (lint-scan "(: a 1\n")))
 (1 = (tally (lint-of "(: a 1\n" lt-none)))
 !(lint-scan "(: a 1)\n") !(lint-of "(: a 1)\n" lt-none))

; --- the config rules -----------------------------------------------------
; ⚠ THE DEFAULT IS BALANCE ALONE. (lint-scan src) is (lint-of src ()), so a tool
; that never heard of a config gets exactly what this file has always answered,
; and turning a rule on can never redden a tree that was green.
; ⚠ a setting's value is the form's TAIL, so (singleton 1) stores (1) and
; (deprecated a b) stores (a b) -- one list, not a list holding one.
(: lt-sing (lt-conf `(`('singleton 1)))
   lt-shad (lt-conf `(`('shadow 1)))
   lt-dep  (lt-conf `(`('deprecated 'old-thing '(worse-thing "use better-thing"))))
   ; the 4th field is the RULE, appended -- so <w / <>w / <>>w still read line,
   ; column and text, which is why no consumer had to change
   (lt-rule ws) (map (\ w <>>>w) ws)
   (lt-msg ws)  (map (\ w <>>w) ws))

(assert
 !(lint-scan "(go)")                        ; off by default: the rule is an opinion
 (1 = (tally (lint-of "(go)" lt-sing)))
 ('(singleton) = (lt-rule (lint-of "(go)" lt-sing)))
 ('(balance) = (lt-rule (lint-scan "(: a 1\n"))))

; a form of one element IS that element, so the parens do nothing -- and a
; nullary call never fires. the position points at the OPENER.
(assert
 (: w <(lint-of "(: a 1\n   (go))" lt-sing) (&& (2 = <w) (4 = <>w)))
 (1 = (tally (lint-of "(0)" lt-sing)))         ; a number is no different
 (1 = (tally (lint-of "((f x))" lt-sing)))     ; nor is an inner FORM
 (1 = (tally (lint-of "foo(x)" lt-sing))))     ; foo(x) is two datums, so (x) is a form

; ⚠ THE THREE EXEMPTIONS, each load-bearing.
(assert
 ; 1. a CONSTRUCTOR is a datum, not a form -- and a ' makes its contents data too
 !(lint-of "'(x)" lt-sing) !(lint-of "`(x)" lt-sing) !(lint-of "#(x)" lt-sing)
 !(lint-of "@(x)" lt-sing) !(lint-of "~(x)" lt-sing)
 !(lint-of "'(a (b))" lt-sing)
 ; ...but a BACKTICK list evaluates its elements, so a form inside one is a form
 (1 = (tally (lint-of "`(a (b))" lt-sing)))
 ; 2. GLUED to an operator run: merged, two runs lex as ONE, and that is sometimes
 ; the same value and sometimes a DIFFERENT operator -- <(<l) survives (<< is the
 ; same caap) but <(= x) is (< (= x)) while <=x is the single operator <=. the scan
 ; cannot tell which without the operator table, so every glued run is exempt.
 !(lint-of "(-(-5))" lt-sing) !(lint-of "<(<l)" lt-sing) !(lint-of ">(<l)" lt-sing)
 !(lint-of "<(= x)" lt-sing)
 ; 3. an ALL-PUNCTUATION token is the escape idiom: (+) is + as a value, and
 ; (:) (?) (\) read their own zero point
 !(lint-of "(+)" lt-sing) !(lint-of "(*)" lt-sing) !(lint-of "(<=)" lt-sing)
 !(lint-of "(:)" lt-sing) !(lint-of "(?)" lt-sing) !(lint-of "(\\)" lt-sing)
 ; ⚠ and that is NARROWER than "starts with punctuation" on purpose -- (<>b) and
 ; <>b are one value, so those parens really are droppable
 (1 = (tally (lint-of "(<>b)" lt-sing))))

; the DEPRECATED roster: a bare name, or `(name "hint"). comments and strings are
; not code, and a quoted 'foo names foo just as much as bare foo does.
(assert
 !(lint-scan "(f old-thing)")                                  ; empty roster: silent
 ("deprecated old-thing" = <(lt-msg (lint-of "(f old-thing)" lt-dep)))
 ("deprecated worse-thing -- use better-thing"
  = <(lt-msg (lint-of "(f worse-thing)" lt-dep)))
 ('(deprecated) = (lt-rule (lint-of "(f old-thing)" lt-dep)))
 (1 = (tally (lint-of "(f 'old-thing)" lt-dep)))                ; 'foo is a mention
 !(lint-of "; old-thing" lt-dep)                                ; a comment is not code
 !(lint-of "(f \"old-thing\")" lt-dep)                          ; nor is a string
 !(lint-of "(f old-thing-x)" lt-dep)                            ; a longer name is another name
 (2 = (tally (lint-of "(old-thing worse-thing)" lt-dep))))

; the SHADOW rule: a glued sigil factors against the GLUED LANE (`operators` at arity 0)
; to one of a dozen plain
; names and then resolves like any other name, so a binding of one re-aims every <x /
; !x / +x in its scope. the rule speaks at the BINDING, and reserves nothing.
(assert
 !(lint-scan "(: net 5 (f net))")                              ; off by default, like the rest
 ('(shadow) = (lt-rule (lint-of "(: net 5 (f net))" lt-shad)))
 ("shadow net -- a glued sigil factors to this name"
   = <(lt-msg (lint-of "(: net 5 (f net))" lt-shad)))
 (: w <(lint-of "(: a 1\n   cap 2 (f a))" lt-shad) (&& (2 = <w) (4 = <>w)))  ; it points AT the name
 (1 = (tally (lint-of "(\\ cap (cap 1))" lt-shad)))            ; a lambda param binds too
 ; a USE is not a binding: the value half of a pair, and any operand of a call
 !(lint-of "(: a 1 (net a))" lt-shad)  !(lint-of "(f cap cup)" lt-shad)
 !(lint-of "(: a net b 2 (f a))" lt-shad)                      ; net here is a VALUE, element 3
 !(lint-of "(: capx 1 (f capx))" lt-shad)                      ; a longer name is another name
 !(lint-of "; cap" lt-shad)  !(lint-of "(: a \"cap\" (f a))" lt-shad)  ; comments and strings are not code
 ; ⚠ a BARE-NAME BODY over-fires -- (: net 5 net) warns TWICE, once for the binding and
 ; once for the body, because which element is last is not known until the closer.
 (2 = (tally (lint-of "(: net 5 net)" lt-shad)))
 ; ⚠ the SUGAR HEADER is a known gap: (: (bit x) ..) is, at token time, exactly what
 ; the far commoner body call (: a 1 (bit x)) is. flagging one would flag both.
 !(lint-of "(: (bit x) x)" lt-shad))

; WHICH WARNINGS FAIL splits with the rule: an imbalance always does, because the
; file does not read and that is not a matter of opinion; an opinion never does,
; so turning a rule on cannot redden a tree that was green. `(strict R)` promotes
; one, for a project that has actually finished with it.
(assert
 (fatal? lt-none <(lint-scan "(: a 1\n"))
 !(fatal? lt-sing <(lint-of "(go)" lt-sing))
 (fatal? (lt-conf `(`('strict 'singleton))) <(lint-of "(go)" lt-sing)))

; --- the paren minifier (fmt -p) ------------------------------------------
; ⚠ THE SPAN HALF IS OPFIX'S OWN JUDGEMENT, not a copy of it: splice the child's
; elements into the parent's operand run, fold both ways, and drop only when the
; cores are IDENTICAL. so the precedence law lives in one place and libra cannot
; drift from the compiler. the tree-wide check that gates this: apply every drop
; to all 303 tracked .l files, re-read, and compare cores -- 303 clean, and a
; rebuilt self-hosted tree gates green with the counts unmoved.
(: lt-nos (lt-conf `(`('drop-singles 0)))          ; span drops alone
   lt-nul (lt-conf `(`('drop-nullary 1)))          ; ...and the nullary trap too
   (lt-drop src) (libra-drop src (tablet 0))
   (lt-span src) (libra-drop src lt-nos))

; the pattern this exists for: application binds tighter than every infix
; operator, so a call alone in an operand span never needed its parens.
(assert
 ("(\"exit \" + show ec + \")\")" = (lt-drop "(\"exit \" + (show ec) + \")\")"))
 ("(\"a\" + show x)"  = (lt-drop "(\"a\" + (show x))"))       ; trailing
 ("(show x + \"a\")"  = (lt-drop "((show x) + \"a\")"))       ; and leading
 ("(x + a * b)"       = (lt-drop "(x + (a * b))")))           ; band, the plain case

; ⚠ and the refusals, which are the whole safety of it
(assert
 (: s "(f (show x))"        (s = (lt-drop s)))    ; an application OPERAND: (f show x) is ((f show) x)
 (: s "(\"a\" + f (show x))" (s = (lt-drop s)))   ; the group shares its span with f
 (: s "(x * (a + b))"       (s = (lt-drop s)))    ; band the other way
 (: s "(x + (a + b))"       (s = (lt-drop s)))    ; arithmetic is LEFT-handed: + cannot yield to +
 (: s "(f + `(a b))"        (s = (lt-drop s)))    ; reader sugar owns those parens
 (: s "(f + @(1 2))"        (s = (lt-drop s)))
 ; a PUNCT HEAD folds the same either way and reads far worse, so it is left alone
 (: s "(? (! (two? l)) () 1)" (s = (lt-drop s))))

; ⚠ THE LEXICAL GATE. the datum cannot see what a DELETION does to the text:
; ((show x)+"a") folds equal, but dropping the bytes fuses x+ into ONE token. so a
; pair drops only when it stands FREE -- a delimiter, comment or quote each side.
(assert
 (: s "((show x)+\"a\")" (s = (lt-drop s)))
 (: s "(a + (mid r)., b)" (s = (lt-drop s))))     ; a ) followed by , would fuse too

; the two halves rest on DIFFERENT laws and so have their own keys. a singleton is
; (f) == f, which opfix declines to apply -- it keeps ((mov r3 r12)) whole, because
; a list of one may be DATA -- so that half is lexical and separately refusable.
(assert
 ("(a = <b)"    = (lt-drop "(a = (<b))"))            ; a punct-bearing token: dropped
 (: s "(a = (<b))" (s = (lt-span s)))                ; ...but not with (drop-singles 0)
 (: s "((mat m n))" (s = (lt-drop s)))               ; a list of ONE FORM stays: it may be data
 (: s "(f (go) x)"  (s = (lt-drop s)))               ; the nullary trap stays VISIBLE by default
 ("(f go x)" = (libra-drop "(f (go) x)" lt-nul))     ; ...unless a config asks for it
 (: s "(f (+) x)"   (s = (lt-drop s))))              ; (+) is + as a value, the escape idiom

; fmt composes them in one pass, and MINIFIES BEFORE IT LAYS -- deleting two bytes
; moves every column after them, so a lay computed on the old text would align to
; parens that are gone.
(assert
 ("(: a (f x + 1)\n   b 2)\n"
  = (libra-fmt (libra-drop "(: a ((f x) + 1)\n b 2)\n" (tablet 0))))
 ; and with no drops in it, -p is the reindenter exactly
 (: s "(: a 1\n   b 2)\n" (s = (libra-fmt (libra-drop s (tablet 0))))))

; --- doc: the header out of the file ---------------------------------------
; the fourth verb, and the only one that keeps comments. three layers of claim:
; the SCANNER (lint-cmts reports every comment with its TEXT, and nothing that is
; not one -- a `;` inside a string is not a comment), the HEADER (which lines it
; is, on a source written to have every edge in it at once), and THE LAW, which
; is the one that matters.

; ⚠ the string is the whole point of asking lint rather than grepping for `;`.
(: src "#!/usr/bin/env -S love -l
; two
;  three

; five
(: x \"a ; not a comment\"  ; six, to the right
   y 2)
; eight
"
   cs (lint-cmts src)
   (nthl l k) (? (k = 0) <l (nthl >l (k - 1)))
   _ (assert
      (6 = tally cs)                                    ; six, and the string holds none
      ('(1 1 "/usr/bin/env -S love -l" bang) = <cs)     ; the introducer's own spot
      ('(2 1 " two" semi) = (nthl cs 1))                ; the text is what follows `;`
      ('(5 1 " five" semi) = (nthl cs 3))               ; the blank line is counted
      ('(6 27 " six, to the right" semi) = (nthl cs 4)) ; past the string, at its own column
      ('(8 1 " eight" semi) = (nthl cs 5)))
   0)

; the HEADER: a seat line, an indented display, a blank inside, an indented
; sentence, a comment to the RIGHT of code (never the header), one under it.
(: src "#!/usr/bin/env -S love -l
; the tool -- what it is for
;
;   a display, two spaces in
  ; and an indented line is still a sentence
; and back
;
(: run 1)   ; not the header
; nor this
"
   _ (assert
      ('("the tool -- what it is for" "" "  a display, two spaces in"
         "  and an indented line is still a sentence" "and back")
       = (ldoc-head src))                                ; ..and it keeps its indent
      ((ldoc-md src) = "the tool -- what it is for\n\n  a display, two spaces in\n  and an indented line is still a sentence\nand back\n")
      ; ..and the degenerate shapes answer nothing rather than something
      (() = (ldoc-head "(: x 1)\n; a tail comment\n"))  ; code first: no header at all
      (() = (ldoc-head ""))
      ("" = (ldoc-md "(: x 1)\n"))
      ('("just this") = (ldoc-head "; just this\n")))    ; no newline trouble at the end
   0)

; the SHIELD. `; --- name ---` is how every crew file rules a section off, and
; markdown claims a `---` line as a rule and then refuses it as anything else --
; so without the shield mdread stops dead there and everything under it is lost.
; the assertion is the TAIL: it is still in the document.
(: d (mdread (ldoc-md "; --- a banner ---
; the body under it
"))
   (nthl l k) (? (k = 0) <l (nthl >l (k - 1)))
   _ (assert
      (2 = tally d)                                      ; the banner is its own block..
      ('(para ((text "--- a banner ---"))) = <d)         ; ..as the text it is
      ('(para ((text "the body under it"))) = (nthl d 1)))
   0)

; ⚠ THE LAW, on files nobody wrote for this test: the document keeps every LETTER
; of the header. alphanumerics on both sides, so markdown's own normalizations --
; a paragraph filled onto one line, `code` losing its ticks, an escape losing its
; backslash -- are not mistaken for loss. what is left is the claim worth making:
; nothing was DROPPED. an extraction that stalls drops its whole tail in silence,
; and neither a length nor a block count would notice; letters do. it holds on 368
; of the tree's 378 tracked .l files (the other ten have no header) and the three
; named here are the hardest: all begin with a banner. (hue.l and eval.l have no
; doc/*.md at all, which is the gap the doc verb exists to close.)
(: (nthl l k) (? (k = 0) <l (nthl >l (k - 1)))
   (alnum? c) (((48 <= c) && (c <= 57)) || ((65 <= c) && (c <= 90)) || ((97 <= c) && (c <= 122)))
   (bare s) ((: (g i acc) (? (i < tally s) (g (i + 1) (? (alnum? (s i)) (acc + string (s i)) acc)) acc))
             0 "")
   (itext l) (foldl (+) "" (map (\ x (: t <x
                                        (? (id? t 'text) <>x
                                           (id? t 'code) <>x
                                           (id? t 'link) (itext <>x + nthl x 2)
                                           ((id? t 'strong) || (id? t 'em)) (itext <>x)
                                           ""))) l))
   (btext b) (: t <b
                (? (id? t 'head)  (itext (nthl b 2))
                   (id? t 'para)  (itext <>b)
                   (id? t 'fence) (nthl b 2)
                   (id? t 'quote) (dtext <>b)
                   ((id? t 'bullets) || (id? t 'numbers)) (foldl (+) "" (map itext <>b))
                   (id? t 'deflist) (foldl (+) "" (map (\ it (itext <it + itext <>it)) <>b))
                   ""))
   (dtext d) (foldl (+) "" (map btext d))
   (slurpf f) (: q (open f "r") (? q (slurp q) ""))
   _ (map (\ f (: src (slurpf f)
                  head (foldl (+) "" (ldoc-head src))
                  d (mdread (ldoc-md src))
                  (assert
                   (1 < tally d)                         ; a document, not a line
                   (0 < tally (bare head))
                   ((bare head) = (bare (dtext d))))))   ; ..and it kept every letter
          (L "crew/vi/hue.l" "crew/lush/eval.l" "crew/libra/libra.l"))
   ; the surfaces come free, and that is the seam's whole promise: libra says
   ; markdown, lapiz says the rest.
   _ (: md (ldoc-md (slurpf "crew/vi/hue.l"))
        _ (assert (0 < tally (md->ht md))
                  (0 < tally (md->rf md)))
        0)
   0)

(test_fin ())
(puts "libra: ok")
(putc 10)
