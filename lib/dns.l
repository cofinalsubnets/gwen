; lib/dns.l -- THE RESOLVER, in love. `connect` takes a dotted quad and nothing
; else (host/sock.c), because getaddrinfo is not a syscall with an O_NONBLOCK to
; set but a config read that may speak DNS, and there is no nonblocking form of
; it. So the name half lives up here instead, where a lookup can PARK like
; anything else:
;
;   (resolve "example.org")      ; -> a list of ipv4 charms, in the server's order
;   (dial "example.org" 80)      ; -> a port, or ()  -- resolve, then connect
;   (resolve-in conf hosts name) ; the same, against named config paths
;
; ⚠ NOTHING HERE POLLS, and the shape that manages it is worth reading. a lookup
; needs "an answer, or a deadline", and the scheduler has no OR of the two -- a
; task parked on both a timer and an fd wakes when BOTH are ready. so it is three
; tasks: the receiver parks on the socket, a watchdog parks on the clock, and the
; caller parks in `catch`. whichever finishes first wins, because a FREEZE OUT
; FROM UNDER A CATCHER wakes it with the zero point (doc/io.md, the catch rung's
; law 15). no `cue?` loop, no 1 ms tick.
;
; ⚠ THIS IS A PORT OF crew/moon/lib/nolibc.c's RESOLVER, which is what a
; mooncc-built love used to reach through getaddrinfo -- and which BLOCKED, for up
; to 2 tries x 2.5 s per nameserver across three of them. that C is the reference
; for every fiddly part here (the wire format, the compression pointer, the two
; config parsers) and it stays where it is: nolibc is a libc, and a C program
; mooncc builds is entitled to getaddrinfo.
;
; what it speaks, and what it does not: A records over UDP, /etc/hosts first,
; /etc/resolv.conf's nameservers in order with the search domains appended on a
; miss. NOT AAAA and NOT the TCP fallback -- a truncated answer is a SCARE, never
; a silent partial, so the gap says so when something reaches it.
;
; ⚠ PRESENCE IS THE WRAPPER here as everywhere: `0.0.0.0` is a perfectly good
; address and nets nothing, so every "did it parse" answer is a ONE-LIST tested
; with `two?`, never the address itself.
(:
   ; --- files, lines, words ----------------------------------------------------
   ; the two config files are line-oriented text, not love data, so `sound` is no
   ; use and this is a hand walk. absent is the normal case and reads as "".
   (dns-look path) (: q (open path "r") (? q (: s (slurp q) _ (close q) s) ""))

   (dns-eol t i n) (? (|| (n <= i) ((t i) = 10)) i (dns-eol t (i + 1) n))
   (dns-lines t)
    (: n (tally t)
       (go i) (? (n <= i) () (: j (dns-eol t i n) (link (snip t i j) (go (j + 1)))))
       (go 0))

   ; a line's whitespace-separated words, stopping at a '#'.
   (dns-blank? c) (|| (c = 32) (c = 9))
   (dns-words l)
    (: n (tally l)
       (skip i) (? (&& (i < n) (dns-blank? (l i))) (skip (i + 1)) i)
       (end i)  (? (|| (n <= i) (|| (dns-blank? (l i)) ((l i) = 35))) i (end (i + 1)))
       (go i)   (: s (skip i)
                   (? (|| (n <= s) ((l s) = 35)) ()
                      (: e (end s) (link (snip l s e) (go e)))))
       (go 0))

   ; --- a dotted quad ----------------------------------------------------------
   ; -> a ONE-LIST holding the address in host order, or (). the same four-octet
   ; walk host/sock.c's `quad` does, because both have to agree on what a numeric
   ; host is: whatever this refuses is what gets a lookup.
   (dns-digit? c) (&& (47 < c) (c < 58))
   (dns-quad s)
    (: n (tally s)
       (oct i a k)
        (: (dig j v any)
            (? (&& (j < n) (dns-digit? (s j)))
               (: w ((v * 10) + ((s j) - 48)) (? (255 < w) () (dig (j + 1) w 1)))
               (? any `(j v) ()))
           r (dig i 0 0)
           (? (! (two? r)) ()
              (: j <r  b ((a * 256) + (caup r))
                 (? (k = 1) (? (n <= j) `(b) ())
                    (&& (j < n) ((s j) = 46)) (oct (j + 1) b (k - 1))
                    ()))))
       (oct 0 0 4))

   ; --- the wire ---------------------------------------------------------------
   (dns-be16 v) ((string ((v >> 8) & 255)) + (string (v & 255)))
   (dns-hi r p)  (((r p) * 256) + (r (p + 1)))

   ; a name as length-prefixed labels, NUL-terminated. () if any label is empty
   ; or over 63 bytes -- a malformed name must not become a malformed packet.
   (dns-labels h)
    (: n (tally h)
       (dot i) (? (|| (n <= i) ((h i) = 46)) i (dot (i + 1)))
       (go i) (? (n <= i) (string 0)
                 (: j (dot i)  l (j - i)
                    (? (|| (l = 0) (63 < l)) ()
                       (: r (go (j + 1))
                          (? (string? r) (((string l) + (snip h i j)) + r) ())))))
       (? (0 < n) (go 0) ()))

   ; the query packet: header (RD set, one question), the name, QTYPE A QCLASS IN.
   (dns-query id h)
    (: q (dns-labels h)
       (? (! (string? q)) ()
          ((dns-be16 id) + (dns-be16 256) + (dns-be16 1) + (dns-be16 0)
           + (dns-be16 0) + (dns-be16 0) + q + (dns-be16 1) + (dns-be16 1))))

   ; skip a name at `pos` -> the position after it, or -1. ⚠ A COMPRESSION POINTER
   ; ENDS THE NAME: its top two bits are set and it is two bytes wide, so a walk
   ; that treated it as a length would run off into the record data.
   (dns-skip r pos n)
    (? (n <= pos) -1
       (: l (r pos)
          (? (l = 0) (pos + 1)
             ((l & 192) = 192) (? (n < (pos + 2)) -1 (pos + 2))
             (dns-skip r (pos + l + 1) n))))

   ; a response -> the A addresses in the server's order, () if it says no, or
   ; 'torn when the TC bit says the answer did not fit. CNAMEs simply ride past:
   ; they are records of another type and the walk skips them.
   (dns-answers r id)
    (: n (tally r)
       (go k pos)
        (? (|| (k < 1) (n <= pos)) ()
           (: p (dns-skip r pos n)
              (? (|| (p < 0) (n < (p + 10))) ()
                 (: ty (dns-hi r p)  cl (dns-hi r (p + 2))  rl (dns-hi r (p + 8))
                    q (p + 10)
                    (? (n < (q + rl)) ()
                       (&& (ty = 1) (&& (cl = 1) (rl = 4)))
                         (link (((dns-hi r q) * 65536) + (dns-hi r (q + 2)))
                               (go (k - 1) (q + rl)))
                       (go (k - 1) (q + rl)))))))
       (? (n < 12) ()
          (! ((dns-hi r 0) = id)) ()                 ; not the question we asked
          (0 < ((r 2) & 2)) 'torn                    ; TC: it did not fit
          (0 < ((r 3) & 15)) ()                      ; NXDOMAIN, SERVFAIL, ..
          (: p0 (dns-skip r 12 n)
             (? (p0 < 0) () (go (dns-hi r 6) (p0 + 4))))))

   ; --- one question to one nameserver ------------------------------------------
   ; -> the addresses, () on no answer within `ms`. the three-task park is the
   ; header note: receiver on the socket, watchdog on the clock, caller in catch.
   ; the server is named by ADDRESS AND PORT: resolv.conf has no port syntax and
   ; every line it yields means 53, but a stub on another port is a real thing to
   ; want and the walk carries the pair either way.
   (dns-ask ns port h ms)
    (udp-bind 0 @ (p & p)
     (: id ((clock 0) & 65535)
        q (dns-query id h)
      (? !(string? q) (close p, ())
       (udp-send p (ns * 65536 + port) q,
        (: r (twirl (\ _ (udp-recv p)) ())
           w (twirl (\ _ (rest ms, freeze r)) ())
           d (catch r)
         (freeze w,
          close p,
          two? d ? (dns-answers >d id)))))))

   ; --- the two config files -----------------------------------------------------
   ; every word after `k` on every line that starts with it, in file order. this
   ; reads both `nameserver` (one address a line) and `search` (many a line).
   (dns-conf path k)
    (: (go (a >< u)) (dns-words a @ (^k >< w) (w + go u) (go u))
     (go (dns-lines (dns-look path))))

   ; /etc/hosts: the first line whose address parses and one of whose names is an
   ; exact match -> a ONE-LIST of the address. no packet is sent at all, which is
   ; what makes localhost and every LAN name free.
   (dns-hosts path h)
    (: (go (l >< u))
     (: w (dns-words l)
      (? (atom? w) (go u)
       (: a (dns-quad <w)
        (two? a && member? h >w ? a (go u)))))
     (go (dns-lines (dns-look path))))

   ; the nameservers, in order. ⚠ 127.0.0.1 when the file names NONE -- musl's
   ; fallback and nolibc's, and the reason a machine with an empty resolv.conf
   ; still reaches a local stub rather than answering "no such host".
   (dns-servers conf)
    (: ns (: (go l) (? (two? l)
                     (: a (dns-quad <l)
                      (two? a ? ((<a >< 53) >< go >l) (go >l))))
             (go (dns-conf conf "nameserver")))
       (two? ns ? ns [(2130706433 >< 53)]))

   ; --- the door -----------------------------------------------------------------
   dns-ms 2500                            ; nolibc's per-server deadline, kept

   ; ask every server in turn for one name. a server that ANSWERS ends the walk,
   ; even to say no -- only silence moves on to the next.
   (dns-walk ((a >< u) >< su) h)
    (dns-ask a u h dns-ms @
     'torn (scare 'torn ['dns h])
     (x & two? x) x
     (dns-walk su h))

   ; hosts first, then the name as given, then each search domain appended.
   (resolve-in conf hosts h)
    (: f (dns-hosts hosts h)
       (? (two? f) f
          (: servers (dns-servers conf)
             a (dns-walk servers h)
             (? (two? a) a
                (: (go ds) (? (atom? ds) ()
                              (: b (dns-walk servers ((h + ".") + <ds))
                                 (? (two? b) b (go >ds))))
                   (go (dns-conf conf "search")))))))

   (resolve h) (: a (dns-quad h) (? (two? a) a (resolve-in "/etc/resolv.conf" "/etc/hosts" h)))

   ; resolve, then connect to each address in turn until one takes. a dotted quad
   ; costs no lookup at all -- `resolve` hands it straight back.
   (dial h port)
    (: (go (a >< u)) (connect (dns-show a) port || go u)
     (go (resolve h)))

   ; an address back to its dotted quad, which is the only spelling `connect` takes.
   (dns-show a)
    (: (o k) (show $ (a >> k) & 255)
     (o 24 + "." + o 16 + "." + o 8 + "." + o 0)))
