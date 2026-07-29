; --- test/host/libra.l: libra ⚖, the .l language server (crew/libra/libra.l) ---
; the ports are ARGUMENTS, so a whole lsp conversation runs in-process here: the
; client's messages go in on a (tap ..) and the server's come back out of a
; (jug 0). no subprocess, no pipes, no timing, and a failure points at a line
; rather than at a hung editor. run standalone from the repo root:
;   cat test/00-init.l test/host/libra.l | out/host/love

(use 'json)
(use 'lint)          ; libra splices this BELOW itself, so the session must ask for it too
(use 'salt)
(use 'libra)

; a string as an input PORT (prel's own trick: index it over its jot)
(: (lt-port s) (tap (map s (jot (tally s)))))

; the settings a test means, stated rather than inherited: an empty tablet is
; "no config anywhere", which is what every balance test wants. ⚠ never (salt
; 'libra) in a test -- that would read the machine's own ~/.love/etc/libra.l and
; make the gate depend on who is running it.
(: lt-none (tablet 0)
   (lt-conf prs) (foldl (\ t p (: _ (pin t <p >p) t)) (tablet 0) prs))

; one client message, framed the way an editor frames it
(: (lt-wire v) (: t (json-show v) ("Content-Length: " + show (tally t) + "\r\n\r\n" + t)))

; every frame the server wrote, parsed back into values
(: (lt-said j)
   (: p (lt-port (slurp j))
      (go acc) (: m (libra-recv p)
                  (? (! (two? m)) (rev acc)
                     (: r (json-read <>m)
                        (go (? (two? r) (link <>r acc) acc)))))
      (go ())))

; run a conversation: a list of message values in, the server's replies out
(: (lt-talkc conf vs)
   (: j (jug 0)
      txt (foldl (\ a v (a + (lt-wire v))) "" vs)
      _ (libra-serve conf (lt-port txt) j)
      (lt-said j)))
(: (lt-talk vs) (lt-talkc lt-none vs))

; is ndl anywhere in hay? (for reading the raw wire, where key order is the
; tablet's business and only the spelling of a value is ours)
(: (lt-has? hay ndl)
   (: n (tally hay)
      m (tally ndl)
      (go i) (? ((n - m) < i) ()
                ((snip hay i (i + m)) = ndl) 1
                (go (i + 1)))
      (go 0)))

(: (lt-obj prs) (foldl (\ t p (pin t <p <>p)) (tablet 0) prs))
(: (lt-req id mth ps) (lt-obj `(`("jsonrpc" "2.0") `("id" id) `("method" mth) `("params" ps))))
(: (lt-not mth ps)    (lt-obj `(`("jsonrpc" "2.0") `("method" mth) `("params" ps))))
(: (lt-doc uri txt)   (lt-obj `(`("textDocument" (lt-obj `(`("uri" uri) `("text" txt)))))))

(: lt-bad "(: a 1\n   (b 2)\n")     ; one dropped paren -- the classic .l slip
(: lt-ok  "(: a 1 (a + 1))")

; --- the frame ------------------------------------------------------------
; a header block then exactly N bytes; the length is in BYTES, so utf-8 rides
; through without a character count anywhere
(assert
 (: p (lt-port (lt-wire "hi")) r (libra-recv p) (&& (two? r) ("\"hi\"" = <>r)))
 (: p (lt-port (lt-wire "é")) r (libra-recv p) ("\"é\"" = <>r))
 !(libra-recv (lt-port ""))                                  ; eof: nothing, out of band
 !(libra-recv (lt-port "Content-Length: 5\r\n\r\nab"))        ; a short body is not a message
 ; the header name is matched case-blind, and a stray header rides through ignored
 (: p (lt-port "content-length: 2\r\nX-Junk: 1\r\n\r\n42") r (libra-recv p) ("42" = <>r)))

; --- initialize -----------------------------------------------------------
; ⚠ id 0 is a REQUEST, not a notification: json-rpc numbers from 0, and a bare
; truth test on the id would answer nothing and hang the editor on message one
(assert
 (: rs (lt-talk `((lt-req 0 "initialize" (tablet 0))))
    r  <rs
    (&& (1 = (tally rs)) (0 = (r "id")) ("2.0" = (r "jsonrpc"))
        (1 = (((r "result") "capabilities") "textDocumentSync"))
        ("libra" = (((r "result") "serverInfo") "name")))))

; --- diagnostics ----------------------------------------------------------
; the positions are lint's, dropped by one: lint counts from 1, lsp from 0. an
; unclosed opener points at the OPENER (line 1 col 1 -> line 0 character 0).
(assert
 (: ds (libra-diags lt-none lt-bad)
    d  <ds
    (&& (1 = (tally ds))
        (0 = (((d "range") "start") "line")) (0 = (((d "range") "start") "character"))
        (0 = (((d "range") "end") "line"))   (1 = (((d "range") "end") "character"))
        (1 = (d "severity")) ("libra" = (d "source")) ("unclosed (" = (d "message"))))
 !(libra-diags lt-none lt-ok)                                        ; a clean document: no diagnostics
 !(libra-diags lt-none ""))                                          ; ...and an empty one likewise

; didOpen publishes them; didChange with a repaired document publishes the EMPTY
; array, which is how lsp says "forget what i told you". `[]` and not `null` --
; the whole reason json names null instead of the empty array.
(assert
 (: rs (lt-talk `((lt-not "textDocument/didOpen" (lt-doc "file:///x.l" lt-bad))))
    r  <rs
    (&& (1 = (tally rs))
        ("textDocument/publishDiagnostics" = (r "method"))
        ("file:///x.l" = ((r "params") "uri"))
        (1 = (tally ((r "params") "diagnostics")))))
 (: rs (lt-talk `((lt-not "textDocument/didOpen" (lt-doc "file:///x.l" lt-bad))
                  (lt-not "textDocument/didChange"
                          (lt-obj `(`("textDocument" (lt-obj `(`("uri" "file:///x.l"))))
                                    `("contentChanges" `((lt-obj `(`("text" lt-ok)))))))) ))
    (&& (2 = (tally rs))
        (1 = (tally ((<rs "params") "diagnostics")))         ; broken: one
        (() = ((<>rs "params") "diagnostics"))))             ; repaired: cleared, exactly ()
 ; and it goes out ON THE WIRE as [] -- never null, which an editor would ignore
 ; instead of clearing (the whole reason json names null and not the empty array)
 (: j (jug 0)
    _ (libra-serve lt-none (lt-port (lt-wire (lt-not "textDocument/didOpen" (lt-doc "file:///x.l" lt-ok)))) j)
    t (slurp j)
    (&& (lt-has? t "\"diagnostics\":[]") (! (lt-has? t "\"diagnostics\":null")))))

; didClose clears them too
(assert
 (: rs (lt-talk `((lt-not "textDocument/didClose" (lt-doc "file:///x.l" lt-bad))))
    (() = ((<rs "params") "diagnostics"))))

; --- the edges ------------------------------------------------------------
; an unknown REQUEST is refused by number (a request must always be answered, or
; the editor waits forever); an unknown NOTIFICATION is dropped in silence
(assert
 (: rs (lt-talk `((lt-req 7 "textDocument/hover" (tablet 0))))
    r  <rs
    (&& (1 = (tally rs)) (7 = (r "id")) (-32601 = ((r "error") "code"))))
 !(lt-talk `((lt-not "$/setTrace" (tablet 0))))
 ; shutdown answers null, then exit ends the session -- what follows is never read
 (: rs (lt-talk `((lt-req 1 "shutdown" (tablet 0))
                  (lt-not "exit" (tablet 0))
                  (lt-req 2 "initialize" (tablet 0))))
    (&& (1 = (tally rs)) (json-null? (<rs "result")))))

; ⚠ a MALFORMED document notification is dropped, never answered with an invented
; uri. unguarded this is not an error but a plausible lie: () applied to a string
; is 1 (const-1) and 1 applied to it is the string back (the identity), so no
; params at all reads its uri as "uri" and no textDocument reads it as 1 -- and
; publishing an empty list at a bogus uri tells an editor to CLEAR warnings it is
; rightly showing. nothing goes out, and the session stays up.
(assert
 !(lt-talk `((lt-not "textDocument/didOpen" (tablet 0))))                  ; no textDocument
 !(lt-talk `((lt-not "textDocument/didOpen" (lt-obj `(`("textDocument" (tablet 0)))))))  ; no uri
 !(lt-talk `((lt-not "textDocument/didChange" (tablet 0))))
 !(lt-talk `((lt-not "textDocument/didClose" (tablet 0))))
 ; a uri that is not a string is not a uri
 !(lt-talk `((lt-not "textDocument/didOpen" (lt-obj `(`("textDocument" (lt-obj `(`("uri" 7)))))))))
 ; ...and a malformed one does not stop the next message being served
 (: rs (lt-talk `((lt-not "textDocument/didOpen" (tablet 0))
                  (lt-req 5 "initialize" (tablet 0))))
    (&& (1 = (tally rs)) (5 = (<rs "id"))))
 ; a document with no text, but a real uri, is an EMPTY document -- clean, not junk
 (: rs (lt-talk `((lt-not "textDocument/didOpen"
                          (lt-obj `(`("textDocument" (lt-obj `(`("uri" "file:///e.l")))))))))
    (&& (1 = (tally rs)) ("file:///e.l" = ((<rs "params") "uri"))
        (() = ((<rs "params") "diagnostics"))))
 ; an empty contentChanges list leaves the document empty rather than inventing text
 (: rs (lt-talk `((lt-not "textDocument/didChange"
                          (lt-obj `(`("textDocument" (lt-obj `(`("uri" "file:///x.l"))))
                                    `("contentChanges" ()))))))
    (() = ((<rs "params") "diagnostics"))))

; a garbled frame does not take the session down: the body is dropped and the
; next message is served as if nothing happened
(assert
 (: j (jug 0)
    txt ("Content-Length: 7\r\n\r\n{nope!!" + (lt-wire (lt-req 3 "initialize" (tablet 0))))
    _ (libra-serve lt-none (lt-port txt) j)
    rs (lt-said j)
    (&& (1 = (tally rs)) (3 = (<rs "id")))))

; --- the other two verbs --------------------------------------------------
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

; the CHECK is the same scan the server publishes, so the two can never
; disagree about a file: one warning here is one diagnostic there
(assert
 (1 = (tally (lint-scan "(: a 1\n")))
 (1 = (tally (libra-diags lt-none "(: a 1\n")))
 !(lint-scan "(: a 1)\n") !(libra-diags lt-none "(: a 1)\n"))

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

; the SEVERITY splits with the rule: an imbalance is an error (1) because the
; file does not read; an opinion is a warning (2) because it does.
(assert
 (1 = ((<(libra-diags lt-none "(: a 1\n")) "severity"))
 (2 = ((<(libra-diags lt-sing "(go)")) "severity")))

(puts "libra: ok")
(putc 10)
