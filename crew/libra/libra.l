#!/usr/bin/env -S love -l
; libra ⚖ -- the scales: the .l balance tool. it weighs a file three ways and they
; are all the same weighing -- `libra fmt` lays it out, `libra serve` says what is
; wrong while you type, and tools/ltidy.l gates the tree on it. one scanner
; (lib/lint.l) under all three, so the editor, the formatter and the gate can
; never disagree about what balanced means.
;
; the FORMATTER is a REINDENTER and nothing more: it replaces leading whitespace
; and leaves every line break, every alignment inside a line, and every comment
; exactly where its author put them (moonfmt's posture for C, and the reason a
; reindent is safe to run over a whole tree). it cannot re-flow and will not try.
;
; the SERVER speaks LSP over stdin/stdout, a protocol in two layers and neither
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
; ⚠ THE PORTS ARE ARGUMENTS, never the ambient in/out: (libra-serve inp outp) is
; the whole server, so test/host/libra.l drives entire conversations in-process
; over (tap ..) and (jug 0) -- no subprocess, no pipes, no timing. the seat hands
; it the real in/out and nothing else changes.
;
; ⚠ AN `id` IS PRESENCE, and it is the one field that must not be read with `!`:
; json-rpc numbers its requests FROM 0, so a bare truth test would mistake
; request 0 for a notification and answer nothing, which hangs the editor on its
; very first message. absence is tested EXACTLY, with (() = id) -- a stored 0 is
; not (), and `=` knows the difference.
;
; usage:  libra            (an editor launches it; it serves stdio until eof)
;     or: love -l crew/libra/libra.l
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
   (libra-recv p) (: h (rdhdrs p) (? (two? h) (rdn p <>h) ()))

   ; tally is BYTES and json-show writes utf-8, so the length is right by
   ; construction -- there is no character count anywhere in this lane
   (libra-send p v) (: t (json-show v)
                      _ (say p ("Content-Length: " + show (tally t) + "\r\n\r\n" + t))
                      _ (flush p)
                      1)

   ; --- the objects we answer with -------------------------------------------
   (otab prs) (foldl (\ t p (pin t <p <>p)) (tablet 0) prs)
   (pos l c)  (otab `(`("line" l) `("character" c)))

   (reply p id res) (libra-send p (otab `(`("jsonrpc" "2.0") `("id" id) `("result" res))))
   (fault p id code m)
    (libra-send p (otab `(`("jsonrpc" "2.0") `("id" id)
                         `("error" (otab `(`("code" code) `("message" m)))))))
   (note p mth ps)
    (libra-send p (otab `(`("jsonrpc" "2.0") `("method" mth) `("params" ps))))

   ; textDocumentSync 1 is FULL: every change carries the whole document, which is
   ; what a scanner that reads the whole document wants anyway.
   (libra-caps u)
    (otab `(`("capabilities" (otab `(`("textDocumentSync" 1))))
            `("serverInfo" (otab `(`("name" "libra") `("version" "0"))))))

   ; --- the diagnostics ------------------------------------------------------
   ; ⚠ lint counts lines and columns from ONE and lsp from ZERO, so every position
   ; drops by one on the way out. the range covers the single character the
   ; warning points at -- for an unclosed opener that is the opener itself, which
   ; is where the eye needs to go.
   (diag w) (: l (<w - 1)
               c (<>w - 1)
               (otab `(`("range" (otab `(`("start" (pos l c)) `("end" (pos l (c + 1))))))
                       `("severity" 1)                  ; an imbalance is an error, never a note
                       `("source" "libra")
                       `("message" <>>w))))

   (libra-diags text) (map diag (lint-scan text))

   ; a clean document publishes an EMPTY array, which is how lsp says "forget what
   ; i told you" -- and since lint-scan answers () for clean and json writes () as
   ; [], clearing costs nothing and says itself.
   (publish p uri text)
    (note p "textDocument/publishDiagnostics"
            (otab `(`("uri" uri) `("diagnostics" (libra-diags text)))))

   ; --- reading a document notification's params -----------------------------
   ; ⚠ EVERY SLOT IS CHECKED, because a missing one does not error here -- it
   ; MANUFACTURES a plausible answer. () applied to a string is 1 (const-1) and 1
   ; applied to a string is the string itself (the identity), so a message with no
   ; params reads its uri as "uri" and one with no textDocument reads it as 1.
   ; unguarded, libra publishes diagnostics against a document the client never
   ; named -- and publishing an empty list at a bogus uri is how you tell an editor
   ; to CLEAR the warnings it is showing. the laws that make (f)==f lovely make
   ; this lane a liar, so the shape is confirmed before it is believed.
   (docuri ps) (? (tablet? ps)
                   (: d (ps "textDocument")
                      (? (tablet? d) (: u (d "uri") (? (string? u) `(1 u) ())) ()))
                  ())

   (stext v) (? (string? v) v "")                  ; absence and a non-string are both empty
   (dopen ps) (: d (ps "textDocument") (? (tablet? d) (stext (d "text")) ""))

   ; full sync: the LAST change carries the whole document
   (dchange ps) (: cs (ps "contentChanges")
                   (? (two? cs) (: c (last cs) (? (tablet? c) (stext (c "text")) "")) ""))

   ; --- the conversation -----------------------------------------------------
   ; answers 'stop when the client says exit, () otherwise. an unknown REQUEST is
   ; refused by number (a request must always be answered, or the editor waits
   ; forever); an unknown notification is simply dropped, which the spec allows.
   (libra-handle p msg)
    (: mth (msg "method")
       id  (msg "id")
       ps  (msg "params")
       ask (! (() = id))                     ; ⚠ EXACT absence -- request 0 is a request
       (? (mth = "initialize")  (: _ (reply p id (libra-caps ())) ())
          (mth = "shutdown")    (: _ (reply p id json-null) ())
          (mth = "exit")        'stop
          (mth = "textDocument/didOpen")
           (: r (docuri ps) _ (? (two? r) (publish p <>r (dopen ps)) 0) ())
          (mth = "textDocument/didChange")
           (: r (docuri ps) _ (? (two? r) (publish p <>r (dchange ps)) 0) ())
          (mth = "textDocument/didClose")
           (: r (docuri ps) _ (? (two? r) (publish p <>r "") 0) ())
          ask (: _ (fault p id -32601 ("no such method: " + show mth)) ())
          ()))

   ; the server: read, dispatch, repeat, until the client says exit or the pipe
   ; ends. a message that is not a json object is DROPPED rather than fatal --
   ; an editor that garbles one frame should not take the session down with it.
   (libra-serve inp outp)
    (: (go u) (: m (libra-recv inp)
                 (? (! (two? m)) 0
                    (: r (json-read <>m)
                       s (? (&& (two? r) (tablet? <>r)) (libra-handle outp <>r) ())
                       (? (id? s 'stop) 0 (go ())))))
       (go ()))

   ; --- the formatter --------------------------------------------------------
   ; lint-lay says where each line STARTS; laying it out is then just "replace the
   ; leading whitespace, keep the rest". every line break, every alignment inside
   ; a line, every comment stays exactly where its author put it -- moonfmt's
   ; posture for C, and the reason a reindent is safe to run over a whole tree.
   (spaces k) (? (k < 1) "" ((string 32) + (spaces (k - 1))))
   (ltrim s) (: n (tally s)
                (g i) (? (&& (i < n) (|| ((s i) = 32) ((s i) = 9))) (g (i + 1)) (snip s i n))
                (g 0))
   (rtrim s) (: n (tally s)
                (g j) (? (&& (0 < j) (|| ((s (j - 1)) = 32) ((s (j - 1)) = 9))) (g (j - 1)) (snip s 0 j))
                (g n))
   (linesof s) (: n (tally s)
                  (g i st acc) (? (n <= i) (rev (link (snip s st n) acc))
                                  ((s i) = 10) (g (i + 1) (i + 1) (link (snip s st i) acc))
                                  (g (i + 1) st acc))
                  (g 0 0 ()))
   (unlines ls) (? (! (two? ls)) "" (foldl (\ a x (a + "\n" + x)) <ls >ls))

   ; one line against its plan: no plan (inside a string) leaves it ALONE, an
   ; all-blank line empties, anything else takes the planned indent.
   (libra-lay ln pl)
    (? (! (two? pl)) ln
       (: b (ltrim ln) (? (! (0 < (tally b))) "" ((spaces <>pl) + (rtrim b)))))

   (libra-fmt src)
    (: ls (linesof src)
       pp (lint-lay src)
       (g l p acc) (? (! (two? l)) (unlines (rev acc))
                      (g >l (? (two? p) >p ())
                         (link (libra-lay <l (? (two? p) <p ())) acc)))
       (g ls pp ()))

   ; --- the check ------------------------------------------------------------
   ; the same warnings the server publishes, laid out for a terminal instead of an
   ; editor: `path:line:col: text`, the shape a compiler prints and an editor's
   ; error list already parses. QUIET when clean; a tab warns but never fails.
   (tabcol s) (: n (tally s)
                 (g i) (? (n <= i) () ((s i) = 9) (i + 1) (g (i + 1)))
                 (g 0))
   (tabwarns ls) (: (g l k acc) (? (! (two? l)) (rev acc)
                                   (: c (tabcol <l)
                                      (g >l (k + 1) (? c (link `(k c "tab") acc) acc))))
                    (g ls 1 ()))
   (warnline path w) (path + ":" + show <w + ":" + show <>w + ": " + <>>w + "\n")

   ; --- the verbs ------------------------------------------------------------
   (rdf p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))

   ; answers the count of FATAL warnings -- an imbalance fails, a tab does not
   (libra-check-files fs bad)
    (? (! (two? fs)) bad
       (: r (rdf <fs)
          (? (! (two? r)) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                              (libra-check-files >fs (bad + 1)))
             (: src <>r
                bw (lint-scan src)
                _ (foldl (\ _ w (: _ (say err (warnline <fs w)) 0)) 0
                         (bw + (tabwarns (linesof src))))
                (libra-check-files >fs (bad + (tally bw)))))))

   ; fmt: lay each file out on stdout; -w rewrites in place; -n (check) says
   ; nothing and answers the count that WOULD change, which is the gate's shape.
   (libra-fmt-files fs mode bad)
    (? (! (two? fs)) bad
       (: r (rdf <fs)
          (? (! (two? r)) (: _ (say err ("libra: cannot open " + <fs + "\n"))
                              (libra-fmt-files >fs mode (bad + 1)))
             (: src <>r
                lay (libra-fmt src)
                same (lay = src)
                _ (? (id? mode 'write) (? same 0 (: q (open <fs "w") _ (say q lay) (close q)))
                     (id? mode 'check) (? same 0 (say err (<fs + ": would reindent\n")))
                     (say out lay))
                (libra-fmt-files >fs mode (? same bad (bad + 1)))))))

   ; ⚠ ABOVE its reader, and not for tidiness: a lambda that closes over a LATER
   ; binding of the same `:` reads the cell before pin when the value is not
   ; itself a lambda (only lambda-to-lambda recursion resolves lazily), and the
   ; read raises `missing usage` at LOAD.
   ; ⚠ an infix CHAIN, not (+ a b c) -- three operands is APPLICATION, ((+ a b) c),
   ; so the third line would be indexed into the first two rather than joined
   usage ("usage: libra check FILE ..   balance: path:line:col: on err, quiet when clean, exit 1\n"
          + "       libra fmt   FILE ..   lay it out on stdout; -w rewrites, -n only checks (exit 1)\n"
          + "       libra serve           speak lsp over stdio -- diagnostics as you type (the default)\n")

   (libra-main as)
    (? (|| (member? "-h" as) (member? "--help" as))
        (: _ (say err "libra ⚖ -- the .l balance tool.\n") _ (say err usage) 0)
       (: v (? (two? as) <as "")
          r >as
          fs (filter (\ x (! ((x 0) = 45))) r)               ; the files: everything not a flag
          (? (v = "check") (: n (libra-check-files fs 0) (? (0 < n) (quit 1) 0))
             (v = "fmt")   (: n (libra-fmt-files fs
                                                 (? (member? "-w" r) 'write
                                                    (member? "-n" r) 'check
                                                    'show)
                                                 0)
                              (? (&& (member? "-n" r) (0 < n)) (quit 1) 0))
             (v = "serve") (libra-serve in out)
             (v = "")      (libra-serve in out)              ; an editor launches it bare
             (: _ (say err ("libra: no verb " + v + "\n")) _ (say err usage) (quit 1)))))
)

; the SEAT (kiosko's shape): fire libra-main iff this file -- or the installed
; `libra` script -- sits in the program seat of the real command line. body-having,
; so nothing leaks.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "libra") (b = "libra.l")))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a) (? (me? <>a) (link 1 >>a) (seat >>a)) ())
               (me? <a) (link 1 >a)
               ())
   s (seat >cmdline)
   (? (two? s) (libra-main >s) ()))
