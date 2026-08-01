; --- test/host/dns.l: the resolver's laws (lib/dns.l) ------------------------
; HERMETIC AND OFFLINE, which is the whole difficulty: a resolver's subject is a
; nameserver, and a gate that asks the real one is a gate that fails on a train.
; So the nameserver is a love TASK -- it binds a UDP port, waits for a query, and
; answers a canned response built with the module's own byte helpers. Nothing
; leaves the loopback and nothing here reads the machine's own /etc.
;
;   out/host/love test/host/dns.l      ; green dots = pass, exits 1 on fail
;
; ⚠ DRIVEN FROM test/host/run.l UNDER A TIMEOUT, like the other socket laws: a
; resolver that stops parking stops the whole vm, and a wedged gate is worse than
; a red one.
(: _ (use 'dns))

; --- the fake nameserver -------------------------------------------------------
; answer `q` (a query) with one A record for `addr`, or with the TC bit set and
; no records at all. ⚠ THE NAME IN THE ANSWER IS A COMPRESSION POINTER (192, 12)
; -- the two-byte form that ENDS a name -- which is what a real server sends and
; what the parser's skip has to get right.
(: (dns-reply q addr tc)
    ((snip q 0 2)                                     ; the id, echoed
     + (dns-be16 (? tc 33664 33152))                  ; QR|RD|RA, and TC if asked
     + (dns-be16 1) + (dns-be16 (? tc 0 1)) + (dns-be16 0) + (dns-be16 0)
     + (snip q 12 (tally q))                          ; the question, verbatim
     + (? tc ""
          ((dns-byte 192) + (dns-byte 12)             ; name: a pointer to offset 12
           + (dns-be16 1) + (dns-be16 1)              ; type A, class IN
           + (dns-be16 0) + (dns-be16 60)             ; ttl
           + (dns-be16 4)
           + (dns-byte ((addr >> 24) & 255)) + (dns-byte ((addr >> 16) & 255))
           + (dns-byte ((addr >> 8) & 255)) + (dns-byte (addr & 255)))))

   ; serve exactly one query on `port`, then exit. the recv PARKS -- this task is
   ; the peer whose running proves the resolver gave up its turn.
   (nameserver port addr tc)
    (twirl (\ _ (: p (udp-bind port)
                   d (udp-recv p)
                   _ (udp-send p (cap d) (dns-reply (cup d) addr tc))
                   _ (rest 20)                        ; let the datagram leave
                   _ (close p) 1)) ()))

; --- law 1: a query goes out, an answer comes back, the address is exact -------
(: port 7401
   s (nameserver port 16909060 ())                    ; 1.2.3.4
   _ (rest 20)                                        ; s reaches its park
   a (dns-ask 2130706433 port "ab.cd" 2000)
   _ (catch s)
   _ (assert (two? a)
             (<a = 16909060)
             (! (two? >a))))                          ; exactly one A record

; --- law 2: a nameserver that never answers gives up, and does not hang --------
; ⚠ THE DEADLINE IS A PARK, NOT A POLL -- the receiver waits on the socket, a
; watchdog waits on the clock, and the freeze out from under the catcher is what
; wakes the caller. this law pins that it wakes AT ALL; the elapsed time pins
; that it waited rather than answering at once.
(: t0 (clock ())
   a (dns-ask 2130706433 7402 "ab.cd" 200)            ; nothing is bound there
   ms ((clock ()) - t0)
   _ (assert (! (two? a))
             (200 <= ms)
             (ms < 2000)))

; --- law 3: a truncated answer is TORN, never a silent partial -----------------
; this resolver does not speak TCP, so the honest answer to "it did not fit" is
; to say so. the packet is built with the TC bit and no records: a parser that
; read the count instead of the bit would answer () -- "no such host" -- which is
; the lie.
(: q (dns-query 4660 "ab.cd")
   r (dns-reply q 0 1)
   _ (assert (id? (dns-answers r 4660) 'torn)))

; --- law 4: /etc/hosts answers with no packet at all ---------------------------
; and a fixture is the only way to say that: pointing at the machine's own file
; would make the gate depend on the machine.
(: dir "out/host/.dns-t"
   _ (cap (hark `("mkdir" "-p" dir)))
   hp (dir + "/hosts")
   cp (dir + "/conf")
   h (open hp "w")
   _ (say h "# a comment\n1.2.3.4  ab.cd  alias\n::1 v6.only\n5.6.7.8 other\n")
   _ (close h)
   c (open cp "w")
   _ (say c "search one.test two.test\nnameserver 9.9.9.9\n")
   _ (close c)
   ; no nameserver is reachable from here, so an answer proves hosts was enough
   a (resolve-in cp hp "ab.cd")
   _ (assert (two? a) (<a = 16909060))
   ; the conf parse, read back through the door that uses it
   ns (dns-servers cp)
   _ (assert (two? ns)
             (<(cap ns) = 151587081)                  ; 9.9.9.9
             (>(cap ns) = 53))                        ; ... and resolv.conf means 53
   _ (assert (= (dns-conf cp "search") '("one.test" "two.test"))))

; --- law 5: an empty resolv.conf still names a server --------------------------
; musl's fallback and nolibc's: 127.0.0.1, so a machine with no config reaches a
; local stub instead of answering "no such host".
(: ns (dns-servers "out/host/.dns-t/nope")
   _ (assert (two? ns) (<(cap ns) = 2130706433)))

(: _ (say out "dns: ok") (putc 10))
