; --- test/host/net.l: a runnable smoke test for ain's socket nifs (src/sock.c) ---
; not in the test/*.l corpus on purpose: the socket nifs (connect/listen/accept/
; seal) live in src/sock.c, which the final `love` links but love0 does not -- and
; love0 bakes the whole corpus, so a corpus test would read these names as missing
; and fail love0's self-test. So this is verified the host-nif way: standalone,
; against the built binary. Run it (the assert macro comes from test/00-init.l):
;
;   cat test/00-init.l test/host/net.l | out/host/love      ; green dots = pass, exits 1 on fail
;
; It runs a whole loopback in one process: a server task accepts and echoes a
; line, the main task connects as a client and checks the round trip. The two
; tasks interleave through the cooperative scheduler -- get on a not-ready fd
; yields, the scheduler poll()s the parked fd and wakes the ready task. The
; two-process integration gate is `make nettest` (test/net/loopback.sh).

; read a \n-terminated line off port p; (string ch) spells the byte, + concats.
; EOF (get -> -1) also ends the line.
(: (rdline p) (: (go acc) (: ch (see p) (? (= ch 10) acc (= ch -1) acc (go (acc + string ch))))
                 (go "")))

; --- listen / accept / connect / get / say / seal over 127.0.0.1 ---
(: port 7391
 l (listen port)
 _ (assert
    (lit? l)                              ; the listener is a heap port
    (hot? l))                             ; ... an opaque hot, like any fd port
 ; server task: accept one client, echo its line back, half-close, close.
 srv (twirl (\ _ (: c (accept l)
                    ln (rdline c)
                    _ (say c (ln + "\n"))
                    _ (seal c 1)
                    _ (close c) 1)) 0)
 c (connect "127.0.0.1" port)
 _ (assert
    (lit? c)                              ; the client side is a heap port too
    (hot? c))
 _ (say c "neko\n")
 echo (rdline c)
 _ (assert
    (= echo "neko"))                      ; the round trip: what went out came back
 ; (seal s how) returns the port (chainable); a no-op on a non-port arg.
 _ (assert
    (lit? (seal c 1))                     ; returns the port
    (= 5 (seal 5 1)))                     ; misuse: returns the arg unchanged
 _ (catch srv)                             ; catch the server task -- no orphan
 _ (close c)
 _ (close l))

; --- every failure lane answers the unit (never a stray fixnum / hang) ---
; `nil?` cannot see the difference and that is why it is not the whole law: it
; is the net test, so the charm zero and () both pass it, and these nifs used to
; answer the charm while their posix.c siblings answered (). `id?` is what tells
; a port-returning nif's "nothing" from a number that happens to be zero.
(: _ (assert
    (nil? (listen -1))                     ; bad port
    (nil? (connect "127.0.0.1" 7392))      ; nothing listening on 7392 -> refused
    (nil? (connect 12345 80))              ; non-string host
    (nil? (connect "example.org" 80))      ; a NAME is a misuse: connect takes a quad
    (id? () (listen -1))
    (id? () (connect "127.0.0.1" 7392))
    (id? () (connect 12345 80))
    (id? () (udp-bind -1))))

; --- rung 5: seal lands the write run ------------------------------------------
; the bug: `seal` shuts the write half and never flushed. love.h has always
; said "close/seal call it first" and only close did -- harmless while a write
; delivered by blocking (the run was empty by then), a silently truncated stream
; the moment the door could answer short (rung 4). kiosko's own shape is exactly
; `(say c body) (seal c 1) (close c)`.
; `put` is what makes this cheap: it parks bytes in the port's write run without
; a drain, so two of them are enough -- no need to out-run a socket buffer the
; kernel auto-tunes into the megabytes. under the bug they are shut in behind the
; half-close, close's write answers epipe, and the server counts none.
(: port 7393
 l (listen port)
 got (tablet 0)
 srv (twirl (\ _ (: c (accept l)
                    n ((: (go k) (: b (see c) (? (b = -1) k (go (k + 1))))) 0)
                    _ (pin got 0 n)
                    _ (close c) 1)) 0)
 c (connect "127.0.0.1" port)
 _ (each [104 105] (\ b (put c b)))       ; held in the write run, not sent
 _ (seal c 1)                             ; ... and the fin it sends is the server's EOF
 _ (catch srv)
 _ (assert
    ((peep got 0 0) = 2))                 ; both bytes crossed the half-close
 _ (close c)
 _ (close l))

; --- slurp over a port that parks is linear ------------------------------------
; prel's slurp used to build the charlist on the way down -- (link c (rl i)) -- so
; the stack depth was the byte count. on a file that is merely deep; on a socket
; every would-block parks, and a park copies the task's stack, so the cost went
; quadratic in the stream and fell over somewhere past 200K. measured on the
; recursive form: 40K 1.75s, 80K 7.1s, 120K 15.0s, 160K 25.6s -- textbook n².
; with the accumulator it is 17/34/58/53 ms, and 4 MB lands in 1.9s.
(: port 7394
 l (listen port)
 got (tablet 0)
 srv (twirl (\ _ (: c (accept l) s (slurp c) _ (pin got 0 (tally s)) _ (close c) 1)) 0)
 c (connect "127.0.0.1" port)
 _ (say c (* "x" 160000))
 _ (close c)
 t0 (clock 0)
 _ (catch srv)
 ms (clock t0)
 _ (close l)
 _ (assert
    ((peep got 0 0) = 160000)             ; every byte
    (ms < 3000)))                         ; the recursive form sat here for 25 seconds

; --- a peer that hangs up mid-response does not kill the runtime ---------------
; the ordinary thing a browser does to kiosko. love ignores sigpipe now, so the
; write answers "the device is gone" (writen -1) and io_wdrain lets the run go;
; before, the process died on the first byte after the hangup and took every other
; task with it. the console keeps the old behaviour -- see test/host/run.l.
(: port 7395
 l (listen port)
 srv (twirl (\ _ (: c (accept l)
                    _ ((: (go k) (? (k = 0) 0
                                    (: _ (say c (* "x" 200000)) _ (rest 5) (go (k - 1))))) 30)
                    _ (close c) 1)) 0)
 c (connect "127.0.0.1" port)
 _ (rest 30)
 _ (close c)                              ; hang up mid-response
 _ (rest 400)
 _ (catch srv)
 _ (close l))                             ; reaching here at all is the law

(test_fin ())
(puts "net: ok") (putc 10)
