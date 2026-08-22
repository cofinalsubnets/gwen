; --- test/host/libra.l: libra ⚖, the .l balance tool (crew/libra/libra.l) ---
; two verbs over one scanner, and this gate is that claim: the check and the doc
; lifter both read lib/lint.l, so the two cannot disagree about what a paren or a
; comment is. run standalone from the repo root:
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
 !(lint-of "'(x)" lt-sing) !(lint-of "`(x)" lt-sing) !(lint-of "@(x)" lt-sing)
 !(lint-of "{x}" lt-sing)
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

; WHICH WARNINGS FAIL splits with the rule: an imbalance always does, because the
; file does not read and that is not a matter of opinion; an opinion never does,
; so turning a rule on cannot redden a tree that was green. `(strict R)` promotes
; one, for a project that has actually finished with it.
(assert
 (fatal? lt-none <(lint-scan "(: a 1\n"))
 !(fatal? lt-sing <(lint-of "(go)" lt-sing))
 (fatal? (lt-conf `(`('strict 'singleton))) <(lint-of "(go)" lt-sing)))

; --- doc: the header out of the file ---------------------------------------
; the second verb, and the one that keeps comments. three layers of claim:
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
      ; ..and an indented run is a DISPLAY, dedented by its own least indent
      ((ldoc-md src) = "the tool -- what it is for\n\n```\na display, two spaces in\nand an indented line is still a sentence\n```\nand back\n")
      ; ..and the degenerate shapes answer nothing rather than something
      (() = (ldoc-head "(: x 1)\n; a tail comment\n"))  ; code first: no header at all
      (() = (ldoc-head ""))
      ("" = (ldoc-md "(: x 1)\n"))
      ('("just this") = (ldoc-head "; just this\n")))    ; no newline trouble at the end
   0)

; a BANNER is where the header used to be lost. `starter` claims a `---` line and
; no markdown block can parse it, so mdread stopped dead there and dropped
; everything under it in silence. two things fixed that, and neither is a patch:
; lapiz's reader is total, and a banner is read as the SECTION HEADING it is.
(: d (mdread (ldoc-md "; --- a banner ---
; the body under it
(: x 1)
"))
   (nth l k) (? (k = 0) <l (nth >l (k - 1)))
   _ (assert
      (2 = tally d)
      ('(head 2 ((text "a banner"))) = <d)
      ('(para ((text "the body under it"))) = (nth d 1)))
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


; the STRUCTURE the corpus already carries (rung 1): a banner is a section, a
; display is a display, and a ⚠ is a hazard. the prose is read, never rewritten.
(assert
 ; a `--- name ---` banner is a level-2 heading, and the padding dashes are padding
 ((ldoc-md "; --- the table ------------------------------------------------\n; a row is a thing.\n(: x 1)\n")
  = "## the table\na row is a thing.\n")
 ((ldoc-md "; --- the middle ---\n(: x 1)\n") = "## the middle\n")
 ; ..but three dashes with nothing in them are not a banner
 ((ldoc-md "; ---\n(: x 1)\n") = "---\n")
 ; a `usage:` line and the indented lines under it are ONE display: the alignment
 ; is the content, and filling it into a paragraph would destroy it
 ((ldoc-md "; usage:  tool [-x] FILE ..    do the thing\n;     or: love -l tool.l FILE\n; and prose after it.\n(: x 1)\n")
  = "```\nusage:  tool [-x] FILE ..    do the thing\n    or: love -l tool.l FILE\n```\nand prose after it.\n")
 ; ⚠ an indent under a LIST ITEM is a wrap, not a display -- it stays a line and
 ; lapiz joins it to the item above (its lazy continuation)
 ((ldoc-md "; - a bullet that wraps onto\n;   a second line\n; - and one that does not\n(: x 1)\n")
  = "- a bullet that wraps onto\n  a second line\n- and one that does not\n")
 ((mdread (ldoc-md "; - a bullet that wraps onto\n;   a second line\n(: x 1)\n"))
  = '((bullets (((text "a bullet that wraps onto a second line"))))))
 ; the hazard marker in bold, and no more than that
 ((ldoc-md "; ⚠ a hazard, and it is one.\n(: x 1)\n") = "**⚠** a hazard, and it is one.\n")
 ; ..and a hazard STARTS a block: 22 of the tree's 74 sit jammed against the prose
 ; above them, and filling them into it is how a page becomes a wall
 ((ldoc-md "; prose.\n; ⚠ a hazard.\n(: x 1)\n") = "prose.\n\n**⚠** a hazard.\n")
 ((ldoc-md "; prose.\n;\n; ⚠ a hazard.\n(: x 1)\n") = "prose.\n\n**⚠** a hazard.\n"))

; and the whole of it through the lens: a real header comes out as a document with
; a heading its sections hang off, which is what papel reads a page's title,
; anchors and contents nav from.
(: (rd f) (: q (open f "r") (? q (slurp q) ""))
   (nth l k) (? (k = 0) <l (nth >l (k - 1)))
   d (mdread ("# crew/lapiz/lapiz.l\n\n" + ldoc-md (rd "crew/lapiz/lapiz.l")))
   heads (filter (\ b (id? <b 'head)) d)
   _ (assert
      (3 < tally heads)
      ('(head 1 ((text "crew/lapiz/lapiz.l"))) = <heads)   ; the file names the page
      (2 = <>(nth heads 1))                               ; ..and its banners are sections
      (0 < tally (filter (\ b (id? <b 'fence)) d)))       ; the diagrams survived as displays
   0)

(test_fin ())
(puts "libra: ok")
(putc 10)
