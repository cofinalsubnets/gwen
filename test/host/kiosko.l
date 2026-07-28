; --- test/host/kiosko.l: the newsstand, over a real socket ---
; NOT in the test/*.l corpus: the socket nifs live in host/sock.c, which love0 does not
; link, and this loads a crew file and splices its module.
;
; the whole thing runs in ONE process, the way test/host/net.l does: a server task
; twirled onto a listener, the client in the main task, the two interleaving through the
; cooperative scheduler. so these are real HTTP responses off a real socket, not a call
; to `serve1` with a pretend port.
;
; what is asserted is kiosko's whole contract, and the security half of it first:
;   GUARD    a `..` SEGMENT is 403 -- percent-encoded too, which is the form that
;            actually gets tried. a file honestly named `a..b` is NOT an escape and is
;            served, so the guard is a segment test and not a substring test.
;   METHOD   GET and HEAD; a HEAD carries the length of the body it does not send.
;   PATHS    a query string is cut, a percent-escape is decoded, a directory redirects
;            to its slashed form and then answers index.html, or a listing without one.
;   TYPES    the extension picks the Content-Type; an unknown one is octet-stream.
;   404      a missing file, and nothing else, is 404.
;
; run:  cat test/00-init.l test/host/kiosko.l | out/host/love

(use 'kiosko)                                 ; the loader: lib/kiosko.l, one layer, registered, spliced

(assert
 (member? 'kiosko (from ()))
 (member? 'serve1 (from 'kiosko 'keys))
 (lit? (from 'kiosko 'kiosko-main))
 (! (from 'kiosco)))

; --- the shelf ------------------------------------------------------------------------
(: dir "/tmp/kioskotest"
   (put-file f s) (: q (open (dir + "/" + f) "w") _ (say q s) (close q))
   _ (run `("rm" "-rf" dir))
   _ (mkdir dir 493)
   _ (mkdir (dir + "/sub") 493)
   _ (mkdir (dir + "/bare") 493)
   _ (put-file "index.html" "<h1>root</h1>")
   _ (put-file "a.css" "body{color:red}")
   _ (put-file "odd name.txt" "spaced")
   _ (put-file "a..b" "not an escape")
   _ (put-file "blob.zzz" "unknown")
   _ (put-file "sub/index.html" "<h1>sub</h1>")
   _ (put-file "bare/one.txt" "listed")
   ; a file BIGGER THAN ONE CHUNK. kiosko reads in 64K gulps off `swig`, and every way
   ; that can go wrong -- a dropped tail, a doubled chunk, a byte lost at the seam where
   ; swig answers 0 and a single `see` carries the next byte -- shows up only past the
   ; first buffer. the body is checked whole, not just its length, so a reordering counts.
   big (: (rep s n) (? (n < 1) "" (s + rep s (n - 1)))
          line "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde\n"
          (rep line 3000))                              ; 64 bytes x 3000 = 192000
   _ (put-file "big.txt" big)

   ; --- one request, one connection: the client half -------------------------------------
   ; kiosko answers with Connection: close, so slurping to EOF is the whole response and
   ; there is no framing to get wrong on this side.
   port 7431
   l (listen port)
   _ (assert (lit? l))
   ; the server task is kiosko's own accept loop in miniature -- `await` parks it on the
   ; listener (a bare `accept` would BLOCK the whole process, and the client below would
   ; never get to run), then serve1 answers one request. it stops on a flag rather than a
   ; count, so adding an assert below does not silently hang the run.
   stop (tablet 0)
   srv (twirl (\ _ ((: (g k) (: _ (await l)
                                c (accept l)
                                _ (? c (: _ (serve1 dir c) _ (seal c 1) (close c)) 0)
                                (? (peep stop 0 0) 1 (g (k + 1))))) 0)) 0)
   (ask req) (: c (connect "127.0.0.1" port)
                _ (say c req)
                _ (flush c)
                r (slurp c)                            ; Connection: close -- EOF frames it
                _ (close c)
                r)
   (get p)   (ask ("GET " + p + " HTTP/1.1\r\nHost: x\r\n\r\n"))
   (code r)  (: n (tally r) ((: (g i) (? (&& (i < n) (! ((r i) = 32))) (g (i + 1)) (snip r (i + 1) (i + 4)))) 0))
   (sub? s t) (: n (tally s) m (tally t)
                 ((: (g i) (? ((i + m) <= n) (? ((snip s i (i + m)) = t) 1 (g (i + 1))) 0)) 0))

   ; --- THE GUARD, first ------------------------------------------------------------------
   _ (assert
      ((code (get "/../../etc/passwd")) = "403")
      ((code (get "/sub/../../etc/passwd")) = "403")
      ((code (get "/%2e%2e/%2e%2e/etc/passwd")) = "403")   ; ..the form that is actually tried
      ((code (get "..")) = "403")                          ; unrooted
      ; and it is a SEGMENT test: a file whose name merely contains .. is ordinary
      ((code (get "/a..b")) = "200")
      (sub? (get "/a..b") "not an escape"))

   ; --- methods ---------------------------------------------------------------------------
   hd (ask "HEAD /index.html HTTP/1.1\r\nHost: x\r\n\r\n")
   _ (assert
      ((code (get "/index.html")) = "200")
      ((code hd) = "200")
      (sub? hd "Content-Length: 13")     ; HEAD reports the body it does not send
      (! (sub? hd "<h1>root</h1>"))
      ((code (ask "PUT / HTTP/1.1\r\nHost: x\r\n\r\n")) = "405")
      ((code (ask "DELETE / HTTP/1.1\r\nHost: x\r\n\r\n")) = "405"))

   ; --- how the request ARRIVES -------------------------------------------------------------
   ; the head is taken in one `swig` gulp now, not a character at a time, and that puts two
   ; new ways to hang in the read: a client whose blank line has no carriage returns, and a
   ; request that arrives in PIECES (swig answers 0 mid-request and only a `see` can park
   ; for the rest). both would wedge the connection rather than fail an assert, so they are
   ; here -- a hang is the failure mode, and the gate has to be the thing that finds it.
   _ (assert ((code (ask "GET /index.html HTTP/1.1\nHost: x\n\n")) = "200"))     ; LF only
   split (: c (connect "127.0.0.1" port)
            _ (say c "GET /index.htm")
            _ (flush c)
            _ (say c "l HTTP/1.1\r\nHost: x\r\n\r\n")
            _ (flush c)
            r (slurp c)
            _ (close c)
            r)
   _ (assert ((code split) = "200") (sub? split "<h1>root</h1>"))

   ; --- paths ------------------------------------------------------------------------------
   _ (assert
      ((code (get "/")) = "200")                       ; the root directory's index.html
      (sub? (get "/") "<h1>root</h1>")
      (sub? (get "/index.html?v=2") "<h1>root</h1>")   ; a query string is cut
      (sub? (get "/odd%20name.txt") "spaced")          ; ..and an escape is decoded
      ((code (get "/sub")) = "301")                    ; a directory redirects to its slash
      (sub? (get "/sub") "Location: /sub/")
      (sub? (get "/sub/") "<h1>sub</h1>")
      (sub? (get "/bare/") "one.txt")                  ; no index.html -> a listing
      ((code (get "/nope.html")) = "404"))

   ; --- a body past the chunk boundary ----------------------------------------------------
   bigr (get "/big.txt")
   _ (: i ((: (g k) (? (&& (k < tally bigr) (! ((snip bigr k (k + 4)) = "\r\n\r\n"))) (g (k + 1)) k)) 0)
        body (snip bigr (i + 4) (tally bigr))
        _ (assert ((tally big) = 192000)                       ; the fixture is really 3 chunks
                  (sub? bigr "Content-Length: 192000")
                  ((tally body) = (tally big))
                  (body = big)))                               ; ..and byte-for-byte, not just as long

   ; --- the LANE BOUNDARY in the read -------------------------------------------------
   ; `gulp` picks its lane by size: at or above 8K it hands back the CASK it read into
   ; (zero copies, end-of-file proven by a one-byte probe), below it copies out (two
   ; copies, end-of-file proven by a short read). so the answer is a cask on one side of
   ; 8192 and a string on the other -- fine downstream, since `say` writes either and
   ; `tally` measures either, but it is exactly the kind of seam that serves right on one
   ; side and wrong on the other. one file each side of the line, byte for byte.
   under (: (rep s n) (? (n < 1) "" (s + rep s (n - 1))) (rep "abcdefgh" 1023))   ; 8184
   over  (: (rep s n) (? (n < 1) "" (s + rep s (n - 1))) (rep "abcdefgh" 1025))   ; 8200
   _ (put-file "under.txt" under)
   _ (put-file "over.txt" over)
   _ (: (bodyof r) (: n (tally r)
                      i ((: (g k) (? (&& (k < n) (! ((snip r k (k + 4)) = "\r\n\r\n"))) (g (k + 1)) k)) 0)
                      (snip r (i + 4) n))
        u (get "/under.txt") o (get "/over.txt")
        (assert ((tally under) = 8184) ((tally over) = 8200)      ; really either side of 8192
                ((bodyof u) = under)                              ; the copy lane
                ((bodyof o) = over)                               ; the cask lane
                (sub? u "Content-Length: 8184")
                (sub? o "Content-Length: 8200")))

   ; --- the read's SLOW lane, which no served file reaches any more --------------------
   ; `gulp` proves it has the whole file from a SHORT READ: the buffer is one byte larger
   ; than stat said, and on a regular file read comes up short only at the end. that means
   ; no ordinary file goes round the loop now -- so the loop needs a test of its own, or it
   ; rots as the fallback nobody runs. and it is a real fallback: the proof says nothing
   ; when stat's size was 0, because a procfs file is generated on demand and answers short
   ; reads that are NOT the end. /proc/cpuinfo handed back 3288 of its 26322 bytes the one
   ; time the fast path was let near it. /proc/self/environ is stable, and on most machines
   ; larger than the 4096 fallback buffer, so it also crosses a chunk boundary.
   _ (: e (gulp "/proc/self/environ")
        r (: q (open "/proc/self/environ" "r") (slurp q))
        big (gulp "/proc/cpuinfo")
        bigr (: q (open "/proc/cpuinfo" "r") (slurp q))
        (assert (0 < tally e)
                (e = r)                                   ; byte for byte, through the loop
                ((tally big) = (tally bigr))))            ; ..and nothing truncated at 4096

   ; --- content types -------------------------------------------------------------------
   _ (assert
      (sub? (get "/index.html") "Content-Type: text/html")
      (sub? (get "/a.css") "Content-Type: text/css")
      (sub? (get "/blob.zzz") "Content-Type: application/octet-stream")
      ; every answer is framed the same way, which is what lets the client above just slurp
      (sub? (get "/a.css") "Connection: close")
      (sub? (get "/a.css") "Content-Length: 15"))

   ; raise the flag, then one last request to let the loop see it and return
   _ (pin stop 0 1)
   _ (assert ((code (get "/index.html")) = "200"))
   _ (catch srv)                                       ; no orphan task
   _ (close l)

   _ (puts "crew/kiosko: ok -- the static web server (guard, methods, paths, types)
")
   0)
