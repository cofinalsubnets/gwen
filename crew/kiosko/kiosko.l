#!/usr/bin/env -S love -l
; kiosko -- the newsstand: a static web server over a directory.
;
; it knows nothing about markdown, or about papel, or about how the files under its root
; came to be there. it is a directory and a port, and that is the whole of its idea --
; which is why it is its own crew member rather than a verb of the generator: `papel`
; happens to be the thing that fills the shelf, but anything can.
;
; the CONCURRENCY is the host's, not ours (free/serve.l learned this first): `await`
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
;
; NO BRACKETS HERE: the runtime supplies the layer. a module consumer says
; (use 'kiosko) (lib/kiosko.l) or probes (from 'kiosko 'serve) -- papel's -s
; does; a -l load rides the session layer and the seat reads bare.

; --- little utils -------------------------------------------------------------------
(: (nth l k)    (? (k = 0) <l (nth >l (k - 1)))
   (strcat l)   (foldl (+) "" l)
   (joins l sp) (? !(two? l) "" !(two? >l) <l (<l + sp + joins >l sp))
   (ends? s t)  (: n (tally s) m (tally t) ((m <= n) && (snip s (n - m) n = t)))
   (splits s c) ((: (g i st acc) (? (i < tally s) (? (s i = c) (g (i + 1) (i + 1) (snip s st i >< acc))
                                                       (g (i + 1) st acc))
                                    (rev (snip s st (tally s) >< acc)))) 0 0 ())
   (dir? p)     (two? (readdir p))
   (fsize p)    (: st (stat p) (? (two? st) <st ()))
   ; ⚠ fsize answers () for MISSING and 0 for EMPTY, and both are nil -- so it
   ; cannot double as an existence test. a 0-byte file is a perfectly good file
   ; and served 404 for it until `sb` tried to pull an empty blob over http.
   ; presence is the wrapper, never the net.
   (fhere? p)   (two? (stat p))
   ; read a whole file. prel's `slurp` draws one character at a time off the port, ~50ns a
   ; byte, and for a static server that is the entire cost of a response: a 66K page took
   ; 9.7ms to serve, of which 3.3ms was the read and ~0 the write. `swig` is the chunk lane
   ; a per-byte `see` cannot be -- it drains the port's own pending run first, then reads
   ; the fd. THREE lanes, and which one runs is decided by what `stat` can promise.
   ;
   ; the swig protocol, common to all three: 0 means NOTHING WAITING OR EOF, and only a
   ; `see` tells those apart (it parks on a slow fd, and answers -1 at the end). the byte
   ; `see` returns is a real byte and has to be kept.
   swigp        (member? 'swig (names ()))       ; ⚠ pinned BEFORE the lanes read it
   ; the CHUNK floor: at or above it, handing back the cask beats copying out of it. the
   ; two lanes below cost two copies of the body against two syscalls, and 8K is where
   ; those cross on this machine -- measured, 2000 reads: at 4K copying wins 23ms to 26ms,
   ; at 8K the cask wins 29ms to 32ms, and it only widens (128K: 114ms to 51ms).
   caskfloor    8192
   ; --- lane 3, the general one: chunk into a jug until the reads stop. for anything
   ; `stat` cannot size -- a procfs file, a fifo -- and the fallback whenever a promise
   ; below turns out to be false. it reopens, because the lanes above have consumed theirs.
   (gulp-loop f) (: q (open f "r")
                    (? !q ()
                       (: j (jug 0) buf (cask 65536)
                          (go _) (: k (swig q buf)
                                    (? (0 < k) (: _ (say j (snip (string buf) 0 k)) (go ()))
                                       (: c (see q) (? (c = -1) 0 (: _ (put j c) (go ()))))))
                          _ (go ()) _ (close q) (slurp j))))
   (gulp f)
   (: sz (fsize f)
      n (? sz sz 0)
      ; ⚠ everything below rests on stat's `n`, so a size of 0 buys nothing and goes round
      ; the loop. this is not a formality: a procfs file is generated on demand -- it
      ; reports size 0 and answers SHORT READS THAT ARE NOT THE END. /proc/cpuinfo handed
      ; back 3288 of its 26322 bytes the one time a short read was trusted here.
      (? !(0 < n) (gulp-loop f)
         !swigp (gulp-loop f)                          ; no cb.c: the slow lane still works
         (: q (open f "r")
            (? !q ()
               ; --- lane 1, ZERO copies: ask for exactly n, and if we got n, hand back the
               ; CASK. `say` writes a cask straight out and `tally` gives its length, so
               ; nothing downstream needs a string at all. the catch is that a full buffer
               ; is NOT evidence of the end, so the end is proven separately -- a one-byte
               ; probe, which costs two syscalls and copies nothing. that is the whole
               ; trade against lane 2, and why it only pays for a body worth copying.
               (caskfloor <= n)
               (: buf (cask n)
                  k (swig q buf)
                  probe (cask 1)
                  (? ((k = n) && (0 = swig q probe) && (-1 = see q))
                     (: _ (close q) buf)
                     (: _ (close q) (gulp-loop f))))    ; it grew, or it shrank
               ; --- lane 2, TWO copies: ask for ONE BYTE MORE than stat said. on a regular
               ; file `read` comes up short only at the end, so a read short of the buffer
               ; proves we have all of it in one syscall, with no probe and no jug. for a
               ; small body that beats lane 1, because two copies of it are cheaper than
               ; the two syscalls a proof costs.
               (: buf (cask (n + 1))
                  k (swig q buf)
                  (? ((0 < k) && (k < tally buf))
                     (: s (snip (string buf) 0 k) _ (close q) s)
                     (: _ (close q) (gulp-loop f))))))))    ; it grew past its own size
   ; ⚠ `got?`, not `lit?`: lit? asks "is this a FUNCTION", and a string is not one, so
   ; (lit? line) reads every request as a dead peer. and it cannot be a net test either --
   ; "" and () are both nothing. `=` is the one thing that tells them apart.
   (got? x)     !(x = ())
   (sub? s t)   (: n (tally s) m (tally t)
                   ((: (g i) (? ((i + m) <= n) (? (snip s i (i + m) = t) 1 (g (i + 1))) 0)) 0))
   ; the whole request head in ONE gulp, the same way a body is read. reading it a
   ; character at a time was the single biggest thing left between kiosko and darkhttpd:
   ; ~51us a request, because `(acc + ch)` reallocates the string on EVERY byte, so a
   ; line is quadratic in its own length and a request is ~140 allocations. darkhttpd
   ; does one recv() into a stack buffer and scans it (darkhttpd.c poll_recv_request).
   ;
   ; the terminator is a blank line -- \r\n\r\n, and \n\n too, because a client that
   ; omits the carriage returns is still asking a question and would otherwise hang here
   ; until it gave up. the CAP is deliberate: without it a peer that never sends a blank
   ; line makes us read until it feels like stopping.
   reqcap       32768
   (reqblk c)   (: buf (cask 2048)
                   (go acc)
                   (? (sub? acc "\r\n\r\n" || sub? acc "\n\n") acc
                      (reqcap < tally acc) acc
                      (: k (swig c buf)
                         (? (0 < k) (go (acc + snip (string buf) 0 k))
                            (: ch (see c) (? (ch = -1) acc (go (acc + ch)))))))
                   (go ""))
   ; the request line: up to the first newline, without its carriage return.
   (line1 s)    (: n (tally s)
                   (go i) (? ((i < n) && !(s i = 10)) (go (i + 1)) i)
                   e (go 0)
                   (snip s 0 (? ((0 < e) && (s (e - 1) = 13)) (e - 1) e)))
   ; the first space-separated word of s, and everything after it
   (word1 s)    (: n (tally s)
                   (go i) (? ((i < n) && !(s i = 32)) (go (i + 1)) i)
                   (snip s 0 (go 0)))
   (after1 s)   (: n (tally s)
                   (skipw i) (? ((i < n) && !(s i = 32)) (skipw (i + 1)) i)
                   (skips i) (? ((i < n) && (s i = 32)) (skips (i + 1)) i)
                   (snip s (skips (skipw 0)) n))
   (numof s)    ((: (g i acc) (? ((i < tally s) && (48 <= s i) && (s i <= 57))
                                 (g (i + 1) (acc * 10 + (s i - 48))) acc)) 0 0))

; --- the request line ----------------------------------------------------------------
; ⚠ -1 for "not a hex digit", never () -- a valid digit can be 0, and 0 is nothing (it
; nets blue), so a presence test on the VALUE cannot tell %00 from a bad escape.
(: (hexv c)   (? ((48 <= c) && (c <= 57)) (c - 48)
                 ((97 <= c) && (c <= 102)) (c - 87)
                 ((65 <= c) && (c <= 70)) (c - 55)
                 -1)
   ; percent-decoding, and `+` left alone: it means a space in a query string, never in a
   ; path, and the query is cut before we ever get here.
   (pdecode s) (: n (tally s)
                  ((: (g i acc)
                      (? !(i < n) acc
                         (: c (s i)
                            (? ((c = 37) && ((i + 2) < n))
                               (: h (hexv (s (i + 1))) l (hexv (s (i + 2)))
                                  (? ((-1 < h) && (-1 < l)) (g (i + 3) (acc + (16 * h + l)))
                                     (g (i + 1) (acc + c))))
                               (g (i + 1) (acc + c)))))) 0 ""))
   (cutq s)    (: n (tally s)
                  ((: (g i) (? !(i < n) s
                               ((s i = 63) || (s i = 35)) (snip s 0 i)
                               (g (i + 1)))) 0))
   ; the ONE guard: no `..` segment, and the path must be rooted. everything downstream
   ; is a plain string append, so this is where an escape has to be refused.
   (safe? p)   (((0 < tally p) && (p 0 = 47)) &&
                !(member? ".." (splits p 47))))

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
   (extof p) (: ps (splits p 46) (? !(two? >ps) "" (: (last l) (? (two? >l) (last >l) <l) (last ps))))
   (mime p)  (: e (extof p)
                ((: (g l) (? !(two? l) "application/octet-stream"
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
; the whole method: take the request head in one gulp, read its first line, guard it, find
; the file, answer. the headers are read and dropped -- we need nothing from any of them,
; but consuming them lets the peer finish its write before we close, which is the
; difference between a clean close and a reset.
(: (serve1 root c)
   (: blk (reqblk c)
      line (line1 blk)
      (? (0 = tally blk) 0                                    ; the peer went away
         (: meth (word1 line)
            raw (word1 (after1 line))
            head? (meth = "HEAD")
            (? !((meth = "GET") || head?) (: _ (oops c 405 "Method Not Allowed" 0) `(405 meth raw))
               (: p (pdecode (cutq raw))
                  (? !(safe? p) (: _ (oops c 403 "Forbidden" head?) `(403 meth raw))
                     (: full (root + p)
                        (? (dir? full)
                           ; a directory: redirect to the slashed form first, so the
                           ; relative links inside its index resolve against the right
                           ; base; then index.html; then a listing.
                           (? !(ends? p "/")
                              (: _ (say c ("HTTP/1.1 301 Moved Permanently\r\nLocation: " + p
                                           + "/\r\nContent-Length: 0\r\nConnection: close\r\n\r\n"))
                                 _ (flush c) `(301 meth raw))
                              (fhere? (full + "index.html"))
                              (: b (gulp (full + "index.html"))
                                 _ (reply c 200 "OK" "text/html; charset=utf-8" b head?)
                                 `(200 meth raw))
                              (: _ (listing c root p head?) `(200 meth raw)))
                           !(fhere? full) (: _ (oops c 404 "Not Found" head?) `(404 meth raw))
                           (: b (gulp full)
                              (? (got? b) (: _ (reply c 200 "OK" (mime full) b head?) `(200 meth raw))
                                 (: _ (oops c 403 "Forbidden" head?) `(403 meth raw)))))))))))))

; --- the loop ---------------------------------------------------------------------------
(: (note quiet l) (? quiet 0
                     (: _ (say err ("kiosko: " + show <l + " " + (<>l) + " " + (nth l 2)))
                        (put err 10)))
   ; `await` parks THIS task on the listener until a client is pending; `accept` then
   ; returns at once, the session is spun as its own task, and we re-park.
   ;
   ; and the finished sessions are COLLECTED. a session task that nobody catches is not
   ; free: it stays on the scheduler's run ring, which the scheduler walks on every switch,
   ; so a server that never collects gets steadily slower at the one thing it does.
   ; `catch` alone cannot be the answer -- it BLOCKS, and blocking the accept loop on one
   ; client would serialise the server. `scoop` hands back ONE finished task, (pid . retval),
   ; or () when none have finished, so the drain costs what actually ended and kiosko keeps
   ; no roster of live sessions at all. that matters at scale: a roster has to be asked
   ; about one pid at a time, and each ask is itself a ring search, so the two multiply --
   ; 400 idle clients held open cost 0.61 ms/req that way against 0.20 this way.
   ; ⚠ `two?` IS THE TEST, never the answer's value: a session task returns () every time,
   ; so a drain that read the net would stop on the first one with the ring still full.
   (drain _)  (? (two? (scoop 0)) (drain ()) 0)
   ; `before` is a thunk run once per accepted client, before the request is served, or ()
   ; for none. it is how a generator turns this into a DEV SERVER without kiosko learning
   ; anything: the hook regenerates, kiosko still only knows a directory. it runs in the
   ; accept task, not the session task, so two clients cannot regenerate at once.
   (accept-loop root l quiet before)
   (: _ (await l)
      c (accept l)
      _ (drain ())
      _ (? c (: _ (? before (before ()) 0)
                ; ⚠ THE SESSION'S RAISE IS THE SESSION'S: uncaught, a scare anywhere under
                ; serve1 -- a file that vanished between the stat and the gulp, a peer that
                ; hung up mid-write -- takes the whole SERVER down with it, every other
                ; session included. the trap lands here, where the close already is (nothing
                ; unwinds, so the cleanup has to be at the landing site), and the client
                ; gets a 500 instead of a reset. the help rides the task, so five sessions
                ; hold five of these at once without seeing each other's.
                (twirl (\ _ (: r (trap (serve1 root) c
                                       (\ a b (: _ (oops c 500 "Internal Server Error" 0)
                                                 `(500 "-" (show a)))))
                               _ (? r (note quiet r) 0) _ (seal c 1) (close c))) ()))
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
   (: _ (? (lit? (from 'kiosko)) (use 'kiosko) 0)   ; splice my REGISTERED self if I am one (cook-main's
                                                    ; guard); on a -l load my names already walk
      box (tablet 0)
      _ (pin box 'port 8080)
      _ (pin box 'root ".")
      (loop ws) (? !(two? ws) 0
                   (: w <ws r >ws
                      (? ((w = "-p") || (w = "--port"))  (: _ (need w r) _ (pin box 'port (numof <r)) (loop >r))
                         ((w = "-q") || (w = "--quiet")) (: _ (pin box 'quiet 1) (loop r))
                         ((w = "-h") || (w = "--help"))  (usage 0)
                         (: _ (pin box 'root w) (loop r)))))
      _ (loop as)
      port (peep box 'port 8080)
      _ (? (dir? (peep box 'root ".")) 0
           (: _ (say err ("kiosko: not a directory: " + peep box 'root ".")) _ (put err 10) (quit 1)))
      _ (? (serve (peep box 'root ".") port (peep box 'quiet 0) ()) 0
           (: _ (say err ("kiosko: cannot listen on :" + show port)) _ (put err 10) (quit 1)))
      (quit 0))
   kiosko-main)

   kiosko-main _)

; the SEAT (cook's shape): fire kiosko-main iff this file -- or the installed `kiosko`
; script -- sits in the program seat of the real command line. kiosko-main reads BARE:
; whichever layer holds this file's binds is still the head here. body-having, so
; nothing leaks.
(: _ (pins (from 'verbs 'tab) 'kiosko kiosko-main)
   s ((from 'verbs 'fire) ["kiosko" "kiosko.l"])
   (? (two? s) (kiosko-main >s) ()))
