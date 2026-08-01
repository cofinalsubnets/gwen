; test/host/nifpark.l -- the NIF FLOOR's laws. A nif that cannot answer YET parks the
; task (love.h: leave Ip unadvanced, `return Ap(lvm_yield_sw, g)`, the op re-runs)
; instead of stopping the whole vm inside a syscall. The device floor made every
; DEVICE entry point nonblocking; this is the layer where the caller is a nif.
;
;   out/host/love test/host/nifpark.l      ; green dots = pass, exits 1 on fail
;
; ⚠ DRIVEN FROM test/host/run.l UNDER A TIMEOUT, because two of the three
; regressions are HANGS rather than red asserts: a blocking `accept` with an empty
; backlog waits for a connection that only the wedged vm could still make. A wedged
; gate is worse than a red one, so the timeout is the instrument. (Law 3 does redden
; on its own -- a blocking `wait` merely finishes late. It rides here anyway, because
; the three laws are one invariant.)
;
; the shape of every law: a ticker task that ONLY advances if the task under test
; gave up its turn. Its count is the gauge, exactly as (naps ()) is in test/front.

(: (ticker n) (twirl (\ _ ((: (go u) (: _ (pin n 0 (1 + (peep n 0 0)))
                                         _ (rest 0)
                                         (go ())))
                           ())) ()))

; --- law 1: accept PARKS on an empty backlog ----------------------------------
; the server task reaches `accept` before anyone has connected. Under the blocking
; accept it sat in the syscall and NOTHING else in the process ran -- not the ticker,
; and not the main task that was about to connect, which is why the control hangs.
(: port 7394
   l (listen port)
   n (tablet 0)
   srv (twirl (\ _ (: c (accept l) _ (close c) 1)) ())
   t (ticker n)
   _ (rest 20)                          ; both reach their parks; the ticker spins
   k (peep n 0 0)
   c (connect "127.0.0.1" port)
   _ (catch srv)
   _ (freeze t)
   _ (close c)
   _ (close l)
   _ (assert (0 < k)))                  ; a blocking accept never gets here at all

; --- law 2: udp-recv PARKS on a quiet socket ----------------------------------
; the same law on the datagram side. The peer address is (ipv4 << 16) | port --
; 127.0.0.1 is 2130706433 -- which is how host/sock.c marshals a sockaddr into a
; charm.
(: p (udp-bind 7395)
   n (tablet 0)
   rx (twirl (\ _ (: d (udp-recv p) _ (close p) (cup d))) ())
   t (ticker n)
   _ (rest 20)
   k (peep n 0 0)
   s (udp-bind 7396)
   _ (udp-send s ((2130706433 * 65536) + 7395) "ping")
   got (catch rx)
   _ (freeze t)
   _ (close s)
   _ (assert (0 < k)
             (got = "ping")))           ; and the datagram still arrives whole

; --- law 3: wait PARKS while a child is up ------------------------------------
; ⚠ AND IT IS A POLL, not an fd wait: SIGCHLD is not in the scheduler's wait set and
; a pid is not an fd, so `wait` re-arms for the next tick. That is rung 5's shape for
; the write residue and the same trade. What the law pins is only that a peer RUNS --
; how often the waiter wakes to ask is the poll's business, not the law's.
(: n (tablet 0)
   t (ticker n)
   r (mind '("sleep" "0.3"))
   st (wait (cap r))
   _ (freeze t)
   _ (close (cup r))
   _ (assert (0 < (peep n 0 0))         ; 0 with the blocking waitpid back in
             (st = 0)))                 ; ... and the child's status is still exact

(: _ (say out "nifpark: ok") (putc 10))
