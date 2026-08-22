; test/host/nifpark.l -- the NIF FLOOR's laws. A nif that cannot answer YET parks the
; task (love.h: leave Ip unadvanced, `return Ap(lvm_yield_sw, g)`, the op re-runs)
; instead of stopping the whole vm inside a syscall. The device floor made every
; DEVICE entry point nonblocking; this is the layer where the caller is a nif.
;
;   out/host/love test/host/nifpark.l      ; green dots = pass, exits 1 on fail
;
; ⚠ DRIVEN FROM test/host/run.l UNDER A TIMEOUT, because most of these regressions
; are HANGS rather than red asserts: a blocking `accept` with an empty backlog waits
; for a connection that only the wedged vm could still make, and a blocking
; `connect` on a dropped SYN waits about two minutes. A wedged gate is worse than a
; red one, so the timeout is the instrument. (Law 3 does redden on its own -- a
; blocking `wait` merely finishes late. It rides here anyway, because the laws are
; one invariant.)
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
   r (tether '("sleep" "0.3"))
   st (wait (cap r))
   _ (freeze t)
   _ (close (cup r))
   _ (assert (0 < (peep n 0 0))         ; 0 with the blocking waitpid back in
             (st = 0)))                 ; ... and the child's status is still exact

; --- law 4: hark PARKS for the child's whole life ------------------------------
; not the reap -- the DRAIN. A blocking read(2) over the child's stdout held the
; vm for as long as the child had anything left to say. `hark` is a TWO-AP nif
; body ({start},{drain},{ret0}) because the op is not re-runnable where it parks:
; a re-entry from the top would fork a SECOND child. So the fork is one op, the
; capture is the other, and the five stack slots between them are the park state.
; ⚠ the child must be slow in the MIDDLE, not just at the end -- a child that
; writes everything at once and exits would let a blocking drain finish in one
; read and the law would pass over the bug.
(: n (tablet 0)
   t (ticker n)
   r (hark '("sh" "-c" "echo one; sleep 0.2; echo two"))
   _ (freeze t)
   _ (assert (0 < (peep n 0 0))         ; 0 with the blocking drain back in
             ((cap r) = 0)
             ((cup r) = "one\ntwo\n")))  ; ... and the capture is still whole

; --- law 5: connect PARKS on the handshake ------------------------------------
; the WRITE-direction park, and the only one there is -- doc/misc/io.md held it back as
; rung 7 "until something asks", and connect is what asked. a socket that has sent
; its SYN is not readable and will not be; POLLOUT is the question, which is why
; the scheduler carries a direction per parked task at all.
;
; ⚠ THE INSTRUMENT IS A FULL ACCEPT QUEUE, and nothing else offline works: an
; unroutable address is no good, because a machine with no route to it fails
; INSTANTLY with ENETUNREACH and the law would pass straight over the bug. a SYN
; the kernel drops is retried for about two minutes, which is the stall this needs.
;
; ⚠ SO THIS LAW KNOWS host/sock.c's `ai_listen_backlog` BY VALUE, and has to: it
; fills the queue that constant sizes. linux queues one MORE than asked (the test
; is `> max`), so backlog+1 arrivals fill it and the next is dropped. if the
; constant moves and this does not, the fill stops one short, the stalled connect
; lands instead, and `(nil? r)` reddens -- noisily, which is the point.
;
; ⚠ AND THE FILL CLOSES AS IT GOES, which costs one fd instead of 513: a client
; hangup does NOT free the slot. An unaccepted connection holds its place in the
; queue until something accepts it, and nothing here ever does.
;
; closing the LISTENER is what ends it: the queued connections reset, the stalled
; client's handshake fails, and `catch` reaps a plain nil. no freeze, so no socket
; is left behind by a task unspliced mid-park.
(: port 7397
   l (listen port)
   n (tablet 0)
   a (connect "127.0.0.1" port)         ; the witness: one that lands, kept open
   (fillq k) (? (k = 0) 0 (: c (connect "127.0.0.1" port) _ (close c) (fillq (k - 1))))
   _ (fillq 512)                        ; a + 512 = 513 = backlog + 1: the queue is full
   t (ticker n)
   s (twirl (\ _ (connect "127.0.0.1" port)) ())   ; the next: SYN dropped, stalls
   _ (rest 50)
   k (peep n 0 0)
   _ (close l)                          ; ... which resets it, so the task ends
   r (catch s)
   _ (freeze t)
   _ (close a)
   _ (assert (0 < k)                    ; 0 with a blocking connect back in
             (nil? r)                   ; the reset handshake is a plain nil
             (lit? a)))                 ; ... and a connect that DID land is a port

(: _ (say out "nifpark: ok") (putc 10))
