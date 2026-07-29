#!/usr/bin/env -S love -l
; lupa 🔍 -- the magnifying glass: a language server for .l, so an editor can say
; what `make lint` says, at the moment you type it rather than at the gate.
;
; it speaks LSP over stdin/stdout, which is a protocol in two layers and neither
; of them is lovely: a header block (`Content-Length: N`, CRLF-terminated) framing
; a json-rpc body. lib/json.l is the translation desk for the second layer; the
; first is here, and it is about thirty lines -- read a line at a time until one
; comes back empty, take the length, then read EXACTLY that many bytes. the rest
; of this file is the conversation.
;
; the DIAGNOSTICS are lib/lint.l's, the same scan tools/ltidy.l prints and
; `make lint` gates on. one scanner, three consumers: what the editor underlines
; is what the gate will refuse, always, with no second implementation to drift.
;
; ⚠ THE PORTS ARE ARGUMENTS, never the ambient in/out: (lupa-serve inp outp) is
; the whole server, so test/host/lupa.l drives entire conversations in-process
; over (tap ..) and (jug 0) -- no subprocess, no pipes, no timing. the seat hands
; it the real in/out and nothing else changes.
;
; ⚠ AN `id` IS PRESENCE, and it is the one field that must not be read with `!`:
; json-rpc numbers its requests FROM 0, so a bare truth test would mistake
; request 0 for a notification and answer nothing, which hangs the editor on its
; very first message. absence is tested EXACTLY, with (() = id) -- a stored 0 is
; not (), and `=` knows the difference.
;
; usage:  lupa            (an editor launches it; it serves stdio until eof)
;     or: love -l crew/lupa/lupa.l
(use 'json)
(use 'lint)

(:
   ; --- the frame: a header block, then exactly N bytes -----------------------
   ; a trailing CR off a header line -- the protocol says CRLF, so the LF is the
   ; line end and the CR is the last byte before it
   (chomp s) (: n (tally s) (? (&& (0 < n) ((s (n - 1)) = 13)) (snip s 0 (n - 1)) s))

   ; ⚠ `(1 text) or (), never a bare string: the BLANK line is what ends the
   ; header block, so "" is a legitimate read and eof must not wear its face.
   (rdline p)
    (: (go acc) (: c (see p)
                   (? (c < 0)  (? (0 < (tally acc)) `(1 (chomp acc)) ())
                      (c = 10) `(1 (chomp acc))
                      (go (acc + c))))
       (go ""))

   (lc s) (: n (tally s)
             (go i acc) (? (n <= i) acc
                           (: c (s i)
                              (go (i + 1) (acc + (? (&& (65 <= c) (c <= 90)) (c + 32) c)))))
             (go 0 ""))

   ; the digits in a header value; the leading space rides through as a non-digit
   (atoi s) (: n (tally s)
               (go i acc) (? (n <= i) acc
                             (: c (s i)
                                (? (&& (48 <= c) (c <= 57)) (go (i + 1) (acc * 10 + c - 48))
                                   (go (i + 1) acc))))
               (go 0 0))

   ; "Content-Length: 42" -> `(1 42). the name is matched case-BLIND: the spec
   ; fixes its spelling, but a client is not obliged to be careful and one
   ; mis-cased header would park the reader on a body that never comes.
   (clen h) (: k "content-length:"
               n (tally k)
               (? (&& (n <= (tally h)) ((lc (snip h 0 n)) = k))
                   `(1 (atoi (snip h n (tally h))))
                  ()))

   ; the header block -> `(1 length). every other header is ignored (Content-Type
   ; is the only other one lsp defines, and it has exactly one legal value).
   (rdhdrs p)
    (: (go n) (: r (rdline p)
                 (? (! (two? r)) ()                            ; eof mid-block
                    (: h <>r
                       (? (! (0 < (tally h))) (? (two? n) n ())  ; the blank line ends it
                          (: v (clen h) (go (? (two? v) v n)))))))
       (go ()))

   (rdn p n) (: (go k acc) (? (! (k < n)) `(1 acc)
                              (: c (see p) (? (c < 0) () (go (k + 1) (acc + c)))))
                (go 0 ""))

   ; one whole message off the port: `(1 body-text), or () at eof / broken framing
   (lupa-recv p) (: h (rdhdrs p) (? (two? h) (rdn p <>h) ()))

   ; tally is BYTES and json-show writes utf-8, so the length is right by
   ; construction -- there is no character count anywhere in this lane
   (lupa-send p v) (: t (json-show v)
                      _ (say p ("Content-Length: " + show (tally t) + "\r\n\r\n" + t))
                      _ (flush p)
                      1)

   ; --- the objects we answer with -------------------------------------------
   (otab prs) (foldl (\ t p (pin t <p <>p)) (tablet 0) prs)
   (pos l c)  (otab `(`("line" l) `("character" c)))

   (reply p id res) (lupa-send p (otab `(`("jsonrpc" "2.0") `("id" id) `("result" res))))
   (fault p id code m)
    (lupa-send p (otab `(`("jsonrpc" "2.0") `("id" id)
                         `("error" (otab `(`("code" code) `("message" m)))))))
   (note p mth ps)
    (lupa-send p (otab `(`("jsonrpc" "2.0") `("method" mth) `("params" ps))))

   ; textDocumentSync 1 is FULL: every change carries the whole document, which is
   ; what a scanner that reads the whole document wants anyway.
   (lupa-caps u)
    (otab `(`("capabilities" (otab `(`("textDocumentSync" 1))))
            `("serverInfo" (otab `(`("name" "lupa") `("version" "0"))))))

   ; --- the diagnostics ------------------------------------------------------
   ; ⚠ lint counts lines and columns from ONE and lsp from ZERO, so every position
   ; drops by one on the way out. the range covers the single character the
   ; warning points at -- for an unclosed opener that is the opener itself, which
   ; is where the eye needs to go.
   (diag w) (: l (<w - 1)
               c (<>w - 1)
               (otab `(`("range" (otab `(`("start" (pos l c)) `("end" (pos l (c + 1))))))
                       `("severity" 1)                  ; an imbalance is an error, never a note
                       `("source" "lupa")
                       `("message" <>>w))))

   (lupa-diags text) (map diag (lint-scan text))

   ; a clean document publishes an EMPTY array, which is how lsp says "forget what
   ; i told you" -- and since lint-scan answers () for clean and json writes () as
   ; [], clearing costs nothing and says itself.
   (publish p uri text)
    (note p "textDocument/publishDiagnostics"
            (otab `(`("uri" uri) `("diagnostics" (lupa-diags text)))))

   ; full sync: the last change carries the whole document
   (lastext cs) (? (two? cs) (: c (last cs) (c "text")) "")

   ; --- the conversation -----------------------------------------------------
   ; answers 'stop when the client says exit, () otherwise. an unknown REQUEST is
   ; refused by number (a request must always be answered, or the editor waits
   ; forever); an unknown notification is simply dropped, which the spec allows.
   (lupa-handle p msg)
    (: mth (msg "method")
       id  (msg "id")
       ps  (msg "params")
       ask (! (() = id))                     ; ⚠ EXACT absence -- request 0 is a request
       (? (mth = "initialize")  (: _ (reply p id (lupa-caps ())) ())
          (mth = "shutdown")    (: _ (reply p id json-null) ())
          (mth = "exit")        'stop
          (mth = "textDocument/didOpen")
           (: d (ps "textDocument") _ (publish p (d "uri") (d "text")) ())
          (mth = "textDocument/didChange")
           (: d (ps "textDocument") _ (publish p (d "uri") (lastext (ps "contentChanges"))) ())
          (mth = "textDocument/didClose")
           (: d (ps "textDocument") _ (publish p (d "uri") "") ())
          ask (: _ (fault p id -32601 ("no such method: " + show mth)) ())
          ()))

   ; the server: read, dispatch, repeat, until the client says exit or the pipe
   ; ends. a message that is not a json object is DROPPED rather than fatal --
   ; an editor that garbles one frame should not take the session down with it.
   (lupa-serve inp outp)
    (: (go u) (: m (lupa-recv inp)
                 (? (! (two? m)) 0
                    (: r (json-read <>m)
                       s (? (&& (two? r) (tablet? <>r)) (lupa-handle outp <>r) ())
                       (? (id? s 'stop) 0 (go ())))))
       (go ()))

   (lupa-main as)
    (? (|| (member? "-h" as) (member? "--help" as))
        (: _ (say err "lupa -- the .l language server. an editor launches it; it speaks lsp over stdio.\n") 0)
       (lupa-serve in out)))

; the SEAT (kiosko's shape): fire lupa-main iff this file -- or the installed
; `lupa` script -- sits in the program seat of the real command line. body-having,
; so nothing leaks.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "lupa") (b = "lupa.l")))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a) (? (me? <>a) (link 1 >>a) (seat >>a)) ())
               (me? <a) (link 1 >a)
               ())
   s (seat >cmdline)
   (? (two? s) (lupa-main >s) ()))
