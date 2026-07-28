; --- test/host/net.l: a runnable smoke test for ain's socket nifs (host/sock.c) ---
; NOT in the test/*.l corpus on purpose: the socket nifs (connect/listen/accept/
; seal) live in host/sock.c, which the final `love` links but love0 does NOT -- and
; love0 bakes the whole corpus, so a corpus test would read these names as missing
; and fail love0's self-test. So this is verified the host-nif way: standalone,
; against the built binary. Run it (the assert macro comes from test/00-init.l):
;
;   cat test/00-init.l test/host/net.l | out/host/love      ; green dots = pass, exits 1 on fail
;
; It runs a whole loopback in ONE process: a server task accepts and echoes a
; line, the main task connects as a client and checks the round trip. The two
; tasks interleave through the cooperative scheduler -- get on a not-ready fd
; yields, the scheduler poll()s the parked fd and wakes the ready task. The
; two-process integration gate is `make nettest` (test/net/loopback.sh).

; read a \n-terminated line off port p; the byte law ("" + byte appends one char)
; rebuilds the text. EOF (get -> -1) also ends the line.
(: (rdline p) (: (go acc) (: ch (see p) (? (= ch 10) acc (= ch -1) acc (go (acc + ch))))
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
    (= echo "neko"))                      ; THE ROUND TRIP: what went out came back
 ; (seal s how) returns the port (chainable); a no-op on a non-port arg.
 _ (assert
    (lit? (seal c 1))                     ; returns the port
    (= 5 (seal 5 1)))                     ; misuse: returns the arg unchanged
 _ (catch srv)                             ; catch the server task -- no orphan
 _ (close c)
 _ (close l))

; --- the failure lanes all return nil (never a stray fixnum / hang) ---
(: _ (assert
    (nil? (listen -1))                     ; bad port -> nil
    (nil? (connect "127.0.0.1" 7392))      ; nothing listening on 7392 -> refused -> nil
    (nil? (connect 12345 80))))            ; non-string host -> nil

(puts "net: ok") (putc 10)
