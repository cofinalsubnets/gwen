#!/usr/bin/env -S love -l
; kiosko -- the newsstand: a static web server over a directory.
;
; it knows nothing about markdown, or about papel, or about how the files under its root
; came to be there. it is a directory and a port, and that is the whole of its idea --
; which is why it is its own crew member rather than a verb of the generator: `papel`
; happens to be the thing that fills the shelf, but anything can.
;
; the CONCURRENCY is the host's, not ours (port/inle/serve.l learned this first): `await`
; parks the accept loop on the listener fd, so the following `accept` never blocks, and
; each client is spun as its own task with `twirl`. love's fgetc yields on a not-ready fd,
; so a slow client parks itself and the others keep going -- there is no thread, no poll
; loop, and no timeout machinery here, because the scheduler already owns all three.
;
; what kiosko does own is HTTP, and only as much of it as a static server needs:
;   GET and HEAD; anything else is 405.
;   the request line is percent-decoded and then GUARDED -- a `..` SEGMENT is 403, always,
;     before any path is built. (a segment, not a substring: a file honestly named `a..b`
;     is not an escape attempt and is served.)
;   a query string is cut and ignored; a static file has no use for one.
;   a directory answers its index.html, and a LISTING when it has none.
;   Content-Type comes off the extension, Content-Length off the body, and every response
;     carries Connection: close -- we close after one request, which is what makes this
;     server small enough to read in one sitting.
;   Cache-Control: no-cache, because the thing kiosko is for is watching a site change.
;
; usage:  kiosko [-p PORT] [-q] [DIR]        (with the `kiosko` script on PATH)
;     or: love -l crew/kiosko/kiosko.l [-p PORT] [DIR]
; DIR defaults to the working directory, PORT to 8080. it serves until interrupted.

(enter 'kiosko)

; --- little utils -------------------------------------------------------------------
(: (nth l k)    (? (k = 0) <l (nth >l (k - 1)))
   (strcat l)   (foldl (+) "" l)
   (joins l sp) (? (! (two? l)) "" (! (two? >l)) <l (<l + sp + joins >l sp))
   (ends? s t)  (: n (tally s) m (tally t) (&& (m <= n) ((snip s (n - m) n) = t)))
   (splits s c) ((: (g i st acc) (? (i < tally s) (? ((s i) = c) (g (i + 1) (i + 1) (link (snip s st i) acc))
                                                       (g (i + 1) st acc))
                                    (rev (link (snip s st (tally s)) acc)))) 0 0 ())
   (dir? p)     (two? (readdir p))
   (fsize p)    (: st (stat p) (? (two? st) <st ()))
   ; read a whole file, in CHUNKS. prel's `slurp` draws one character at a time off the
   ; port, ~50ns a byte, and for a static server that is the entire cost of a response:
   ; a 66K page took 9.7ms to serve, of which 3.3ms was the read and ~0 the write (`say`
   ; is already one write(2) stroke). `swig` is the chunk lane a per-byte `see` cannot be
   ; -- it drains the port's own pending run first, then reads the fd -- and it takes 66K
   ; from 3.3ms to 0.25ms.
   ;
   ; the protocol is swig's: 0 means NOTHING WAITING OR EOF, and only a `see` tells those
   ; apart (it parks on a slow fd, and answers -1 at the end). the byte it returns is a
   ; real byte and has to be kept. a jug collects the pieces, since appending to a string
   ; would be quadratic across chunks; `(string buf)` is how a cask becomes one (`snip`
   ; does not reach into a cask, and swig always fills from offset 0, so a partial read
   ; cannot be resumed into the same buffer).
   swigp        (member? 'swig (names ()))       ; ⚠ pinned BEFORE gulp reads it
   (gulp f)     (: q (open f "r")
                   (? (! q) ()
                      (! swigp) (: s (slurp q) _ (close q) s)          ; no cb.c: the slow lane still works
                      (: j (jug 0) buf (cask 65536)
                         (go _) (: k (swig q buf)
                                   (? (0 < k) (: _ (say j (snip (string buf) 0 k)) (go ()))
                                      (: c (see q) (? (c = -1) 0 (: _ (put j c) (go ()))))))
                         _ (go ()) _ (close q) (slurp j))))
   ; ⚠ `got?`, not `lit?`: lit? asks "is this a FUNCTION", and a string is not one, so
   ; (lit? line) reads every request as a dead peer. and it cannot be a net test either --
   ; "" and () are both nothing. `=` is the one thing that tells them apart.
   (got? x)     (! (x = ()))
   ; read a \n-terminated line off a port, dropping a trailing \r. () at EOF, so the
   ; caller can tell "a blank line" (the header terminator) from "the peer went away".
   (rdline p)   ((: (g acc) (: ch (see p)
                                (? (ch = -1) (? (0 < tally acc) acc ())
                                   (ch = 10) (? (&& (0 < tally acc) ((acc (tally acc - 1)) = 13))
                                                (snip acc 0 (tally acc - 1)) acc)
                                   (g (acc + ch))))) "")
   ; the first space-separated word of s, and everything after it
   (word1 s)    (: n (tally s)
                   (go i) (? (&& (i < n) (! ((s i) = 32))) (go (i + 1)) i)
                   (snip s 0 (go 0)))
   (after1 s)   (: n (tally s)
                   (skipw i) (? (&& (i < n) (! ((s i) = 32))) (skipw (i + 1)) i)
                   (skips i) (? (&& (i < n) ((s i) = 32)) (skips (i + 1)) i)
                   (snip s (skips (skipw 0)) n))
   (numof s)    ((: (g i acc) (? (&& (i < tally s) (&& (48 <= (s i)) ((s i) <= 57)))
                                 (g (i + 1) (acc * 10 + ((s i) - 48))) acc)) 0 0))

; --- the request line ----------------------------------------------------------------
; ⚠ -1 for "not a hex digit", never () -- a valid digit can be 0, and 0 is nothing (it
; nets blue), so a presence test on the VALUE cannot tell %00 from a bad escape.
(: (hexv c)   (? (&& (48 <= c) (c <= 57)) (c - 48)
                 (&& (97 <= c) (c <= 102)) (c - 87)
                 (&& (65 <= c) (c <= 70)) (c - 55)
                 -1)
   ; percent-decoding, and `+` left alone: it means a space in a query string, never in a
   ; path, and the query is cut before we ever get here.
   (pdecode s) (: n (tally s)
                  ((: (g i acc)
                      (? (! (i < n)) acc
                         (: c (s i)
                            (? (&& (c = 37) ((i + 2) < n))
                               (: h (hexv (s (i + 1))) l (hexv (s (i + 2)))
                                  (? (&& (-1 < h) (-1 < l)) (g (i + 3) (acc + (16 * h + l)))
                                     (g (i + 1) (acc + c))))
                               (g (i + 1) (acc + c)))))) 0 ""))
   (cutq s)    (: n (tally s)
                  ((: (g i) (? (! (i < n)) s
                               (|| ((s i) = 63) ((s i) = 35)) (snip s 0 i)
                               (g (i + 1)))) 0))
   ; the ONE guard: no `..` segment, and the path must be rooted. everything downstream
   ; is a plain string append, so this is where an escape has to be refused.
   (safe? p)   (&& (&& (0 < tally p) ((p 0) = 47))
                   (! (member? ".." (splits p 47)))))

; --- what a file is ------------------------------------------------------------------
(: types '(("html" "text/html; charset=utf-8") ("htm" "text/html; charset=utf-8")
           ("css" "text/css; charset=utf-8")   ("js" "text/javascript; charset=utf-8")
           ("json" "application/json")         ("txt" "text/plain; charset=utf-8")
           ("md" "text/plain; charset=utf-8")  ("xml" "application/xml")
           ("svg" "image/svg+xml")             ("png" "image/png")
           ("jpg" "image/jpeg")                ("jpeg" "image/jpeg")
           ("gif" "image/gif")                 ("webp" "image/webp")
           ("ico" "image/x-icon")              ("pdf" "application/pdf")
           ("woff2" "font/woff2")              ("woff" "font/woff")
           ("wasm" "application/wasm"))
   (extof p) (: ps (splits p 46) (? (! (two? >ps)) "" (: (last l) (? (two? >l) (last >l) <l) (last ps))))
   (mime p)  (: e (extof p)
                ((: (g l) (? (! (two? l)) "application/octet-stream"
                             ((<(<l)) = e) (<>(<l))
                             (g >l))) types)))

; --- the response --------------------------------------------------------------------
; one shape for every answer: a status line, four headers, the body. HEAD sends the
; headers a GET would have sent and stops -- the Content-Length still describes the body
; that was not sent, which is what makes HEAD useful.
(: (reply c code why ctype body head?)
   (: _ (say c ("HTTP/1.1 " + show code + " " + why + "\r\n"
                + "Content-Type: " + ctype + "\r\n"
                + "Content-Length: " + show (tally body) + "\r\n"
                + "Cache-Control: no-cache\r\n"
                + "Connection: close\r\n\r\n"))
      _ (? head? 0 (say c body))
      _ (flush c)
      code)
   ; an error is a page too -- small, and readable when a browser shows it
   (oops c code why head?)
   (reply c code why "text/html; charset=utf-8"
          ("<!doctype html><meta charset=\"utf-8\"><title>" + show code + " " + why
           + "</title><body style=\"font:16px ui-serif,Georgia,serif;padding:3rem\">"
           + "<h1>" + show code + " &middot; " + why + "</h1><p>kiosko</p>") head?)
   ; a directory with no index.html: list it, directories first, each entry a link. plain
   ; enough that no stylesheet is needed and no page of the site is shadowed by it.
   (listing c root p head?)
   (: full (root + p)
      names (sort (readdir full))
      (row f) (: sub (? (dir? (full + f)) (f + "/") f)
                 ("<li><a href=\"" + sub + "\">" + sub + "</a></li>"))
      body ("<!doctype html><meta charset=\"utf-8\"><title>" + p + "</title>"
            + "<body style=\"font:16px ui-serif,Georgia,serif;padding:2rem\">"
            + "<h1>" + p + "</h1><ul>"
            + (? (p = "/") "" "<li><a href=\"..\">../</a></li>")
            + strcat (map row names) + "</ul>")
      (reply c 200 "OK" "text/html; charset=utf-8" body head?)))

; --- serving one request --------------------------------------------------------------
; the whole method: read the line, guard it, find the file, answer. the header block is
; read and dropped -- we need nothing from it, but draining it lets the peer finish its
; write before we close, which is the difference between a clean close and a reset.
(: (drain c) ((: (g k) (: l (rdline c) (? (! (got? l)) k (0 = tally l) k (&& (k < 100) (g (k + 1)))))) 0)
   (serve1 root c)
   (: line (rdline c)
      _ (drain c)
      (? (! (got? line)) 0                                    ; the peer went away
         (: meth (word1 line)
            raw (word1 (after1 line))
            head? (meth = "HEAD")
            (? (! (|| (meth = "GET") head?)) (: _ (oops c 405 "Method Not Allowed" 0) `(405 meth raw))
               (: p (pdecode (cutq raw))
                  (? (! (safe? p)) (: _ (oops c 403 "Forbidden" head?) `(403 meth raw))
                     (: full (root + p)
                        (? (dir? full)
                           ; a directory: redirect to the slashed form first, so the
                           ; relative links inside its index resolve against the right
                           ; base; then index.html; then a listing.
                           (? (! (ends? p "/"))
                              (: _ (say c ("HTTP/1.1 301 Moved Permanently\r\nLocation: " + p
                                           + "/\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))
                                 _ (flush c) `(301 meth raw))
                              (fsize (full + "index.html"))
                              (: b (gulp (full + "index.html"))
                                 _ (reply c 200 "OK" "text/html; charset=utf-8" b head?)
                                 `(200 meth raw))
                              (: _ (listing c root p head?) `(200 meth raw)))
                           (! (fsize full)) (: _ (oops c 404 "Not Found" head?) `(404 meth raw))
                           (: b (gulp full)
                              (? (got? b) (: _ (reply c 200 "OK" (mime full) b head?) `(200 meth raw))
                                 (: _ (oops c 403 "Forbidden" head?) `(403 meth raw)))))))))))))

; --- the loop ---------------------------------------------------------------------------
(: (note quiet l) (? quiet 0
                     (: _ (say err ("kiosko: " + show <l + " " + (<>l) + " " + (nth l 2)))
                        (put err 10)))
   ; `await` parks THIS task on the listener until a client is pending; `accept` then
   ; returns at once, the session is spun as its own task, and we re-park. the session
   ; tasks are deliberate orphans -- a server runs forever, so there is no one to catch them.
   ;
   ; `before` is a thunk run once per accepted client, before the request is served, or ()
   ; for none. it is how a generator turns this into a DEV SERVER without kiosko learning
   ; anything: the hook regenerates, kiosko still only knows a directory. it runs in the
   ; accept task, not the session task, so two clients cannot regenerate at once.
   (accept-loop root l quiet before)
   (: _ (await l)
      c (accept l)
      _ (? c (: _ (? before (before ()) 0)
                (twirl (\ _ (: r (serve1 root c) _ (? r (note quiet r) 0) _ (seal c 1) (close c))) ()))
           0)
      (accept-loop root l quiet before))
   ; the entry. `root` is a directory (no trailing slash), `port` a fixnum, `before` a thunk
   ; or (). answers () when the port cannot be taken, so a caller can say something better
   ; than we can. ⚠ all four operands, always -- three would hand back a CLOSURE, which is
   ; truthy, and the caller's "did it listen?" test would read a failure as a success.
   (serve root port quiet before)
   (: r (? (ends? root "/") (snip root 0 (tally root - 1)) root)
      l (listen port)
      (? l (: _ (? quiet 0 (: _ (say err ("kiosko: serving " + r + " on http://127.0.0.1:" + show port + "/"))
                              (put err 10)))
              _ (accept-loop r l quiet before)
              _ (close l) 1)
         ())))

; --- the command line ---------------------------------------------------------------------
(: _ (:
   (usage _) (: _ (puts "kiosko -- the newsstand: a static web server over a directory
usage: kiosko [-p PORT] [-q] [DIR]
  -p, --port N   the port to serve on (default 8080)
  -q, --quiet    do not log requests
  DIR            the document root (default .)
")
                (quit 0))
   (need h r) (? (two? r) 0 (: _ (say err ("kiosko: " + h + " needs an argument")) _ (put err 10) (quit 2)))
   (kiosko-main as)
   (: _ (use 'kiosko)
      box (tablet 0)
      _ (pin box 'port 8080)
      _ (pin box 'root ".")
      (loop ws) (? (! (two? ws)) 0
                   (: w <ws r >ws
                      (? (|| (w = "-p") (w = "--port"))  (: _ (need w r) _ (pin box 'port (numof <r)) (loop >r))
                         (|| (w = "-q") (w = "--quiet")) (: _ (pin box 'quiet 1) (loop r))
                         (|| (w = "-h") (w = "--help"))  (usage 0)
                         (: _ (pin box 'root w) (loop r)))))
      _ (loop as)
      port (peep box 'port 8080)
      _ (? (dir? (peep box 'root ".")) 0
           (: _ (say err ("kiosko: not a directory: " + (peep box 'root "."))) _ (put err 10) (quit 1)))
      _ (? (serve (peep box 'root ".") port (peep box 'quiet 0) ()) 0
           (: _ (say err ("kiosko: cannot listen on :" + show port)) _ (put err 10) (quit 1)))
      (quit 0))
   kiosko-main)

   kiosko-main _)

(leave ())

; the SEAT (cook's shape): fire kiosko-main iff this file -- or the installed `kiosko`
; script -- sits in the program seat of the real command line. body-having, so nothing leaks.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "kiosko") (b = "kiosko.l")))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a) (? (me? <>a) (link 1 >>a) (seat >>a)) ())
               (me? <a) (link 1 >a)
               ())
   s (seat >cmdline)
   (? (two? s) ((from 'kiosko 'kiosko-main) >s) ()))
