; crew/sb/http.l -- the smallest HTTP/1.1 GET that can walk a store, over the
; `connect` socket floor (src/sock.c, the same one tools/ain.l dials with).
;
; it is a CLIENT only, and only GET: an sb remote needs nothing else. the store
; is content-addressed and immutable, so every fetch is a plain cacheable GET on
; a name the client already knows, verified by sha256 on arrival -- there is no
; protocol to speak, no index to publish, and a hostile or broken mirror is
; DETECTABLE rather than trusted. any static file server is a complete remote,
; kiosko included (`sb -s PORT` would serve .sb/ the way papel serves a site).
;
; we send `Connection: close` and read to EOF, which is what makes `slurp` safe
; on a socket -- prel's slurp draws to end-of-stream, so a keep-alive response
; would park the task forever. kiosko closes after one request for the same
; reason from the other side.
;
; rides module 'sb, so these names never reach the book; `dial` (resolve +
; connect) comes off module 'dns, scoped here to the wrap's own compile.
(module 'sb
(use 'dns)
(:
   ; "80" -> 80. `int` is a truncation, not a parser, so digits are ours.
   (digits s)
    (: n #s
       (go i a) (? (n <= i) (? (0 < n) a ())
                   (: c (s i)
                      (? ((47 < c) && (c < 58)) (go (i + 1) ((a * 10) + (c - 48))) ())))
       (go 0 0))

   ; the index of `sub` in `s` at or after `from`, searching at most `cap`
   ; positions -- headers sit in the first few KB and a whole-body scan would be
   ; O(n*m) for nothing. -1 when absent.
   (findsub s sub from cap)
    (: n #s m #sub lim (? ((n - m) < cap) (n - m) cap)
       (go i) (? (lim < i) -1
                 (snip s i (i + m) = sub) i
                 (go (i + 1)))
       (go from))

   (url? u) ((7 <= tally u) && (snip u 0 7 = "http://"))
   ; "http://host[:port]/path" -> (host port path) | ()
   (url-split u)
    (? !(url? u) ()
       (: r (snip u 7 #u) m #r
          (sl i) (? (m <= i) m (r i = 47) i (sl (i + 1)))
          k (sl 0)
          auth (snip r 0 k)
          path (? (k < m) (snip r k m) "/")
          a #auth
          (cn i) (? (a <= i) -1 (auth i = 58) i (cn (i + 1)))
          c (cn 0)
          host (? (c < 0) auth (snip auth 0 c))
          port (? (c < 0) 80 (digits (snip auth (c + 1) a)))
          (? ((0 < tally host) && port) [host port path] ())))

   ; one GET -> (status body) | (). () is a DEAD SOCKET or an unreadable
   ; response; a 404 is a perfectly good answer and comes back as (404 ..), so
   ; the caller can tell "no such object" from "no such host".
   (http-get host port path)
    (: s (dial host port)
       (? !s ()
          (: _ (say s ("GET " + path + " HTTP/1.1\r\nHost: " + host
                       + "\r\nConnection: close\r\nUser-Agent: sb\r\n\r\n"))
             _ (flush s)
             r (slurp s)
             _ (close s)
             (? !(0 < tally r) ()
                (: k (findsub r "\r\n\r\n" 0 8192)
                   (? (k < 0) ()
                      (: head (snip r 0 k)
                         body (snip r (k + 4) #r)
                         sp (findsub head " " 0 64)
                         code (? (sp < 0) () (digits (snip head (sp + 1) (sp + 4))))
                         (? code [code body] ()))))))))

   ; GET a store object under a base url -> (1 body) | (). the PRESENCE WRAPPER,
   ; not the bare body: a 0-byte blob is a legitimate object and `!""` is true,
   ; so a bare body would read as absence -- the same law the empty-file fix set
   ; down (presence is the wrapper, never the net). anything but 200 is absence,
   ; which is all a walk needs to know.
   (http-fetch base rel)
    (: u (url-split (base + rel))
       (? !(two? u) ()
          (: r (http-get <u <>u <>>u)
             (? (two? r && (200 = <r)) [1 <>r] ()))))

   ; the first form of a string, if it is a list; () for anything else.
   (readform s)
    (: r (sound s)
       d (? (two? r) <r ())
       (? (two? d) d ())))

) ; module 'sb
