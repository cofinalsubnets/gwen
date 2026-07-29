; --- test/host/libra.l: libra ⚖, the .l language server (crew/libra/libra.l) ---
; the ports are ARGUMENTS, so a whole lsp conversation runs in-process here: the
; client's messages go in on a (tap ..) and the server's come back out of a
; (jug 0). no subprocess, no pipes, no timing, and a failure points at a line
; rather than at a hung editor. run standalone from the repo root:
;   cat test/00-init.l test/host/libra.l | out/host/love

(use 'json)
(use 'lint)          ; libra splices this BELOW itself, so the session must ask for it too
(use 'libra)

; a string as an input PORT (prel's own trick: index it over its jot)
(: (lt-port s) (tap (map s (jot (tally s)))))

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
(: (lt-talk vs)
   (: j (jug 0)
      txt (foldl (\ a v (a + (lt-wire v))) "" vs)
      _ (libra-serve (lt-port txt) j)
      (lt-said j)))

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
 (: ds (libra-diags lt-bad)
    d  <ds
    (&& (1 = (tally ds))
        (0 = (((d "range") "start") "line")) (0 = (((d "range") "start") "character"))
        (0 = (((d "range") "end") "line"))   (1 = (((d "range") "end") "character"))
        (1 = (d "severity")) ("libra" = (d "source")) ("unclosed (" = (d "message"))))
 !(libra-diags lt-ok)                                        ; a clean document: no diagnostics
 !(libra-diags ""))                                          ; ...and an empty one likewise

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
    _ (libra-serve (lt-port (lt-wire (lt-not "textDocument/didOpen" (lt-doc "file:///x.l" lt-ok)))) j)
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
    _ (libra-serve (lt-port txt) j)
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
 (1 = (tally (libra-diags "(: a 1\n")))
 !(lint-scan "(: a 1)\n") !(libra-diags "(: a 1)\n"))

(puts "libra: ok")
(putc 10)
