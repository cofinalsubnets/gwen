#!/usr/bin/env -S love -l
; ain -- a netcat clone in ~50 lines of love (love's cat).
;
;   ain HOST PORT   TCP client: connect to HOST:PORT
;   ain -l PORT     TCP server: listen on PORT, accept one client
;
; Either way it becomes a BIDIRECTIONAL PUMP -- stdin -> socket and socket ->
; stdout, both live at once. Two tasks, each parked in a port read on a different
; fd, interleave with no select loop: the cooperative scheduler poll()s the fd a
; parked task blocks on (ai_wait_fds) and wakes the ready one (get yields when
; its fd is not ready -- see host/sock.c / love.c). So the whole program is just two
; pump loops + a teardown discipline; the real work is the socket nifs in C.
;
; Run it straight (prel is ALREADY in the egg -- do NOT `-l love/prel.l`; the egg mops
; book/tray/etc after it hatches, so re-feeding prel.l raises `missing book`):
;   out/host/love tools/ain.l HOST PORT
;   out/host/love tools/ain.l -l PORT
; or as the `nc` applet of the multi-call `kore` (crew/kore/kore.l -- the header's old
; "until the egg bakes ain into a multi-call binary" note, landed). The socket nifs
; (connect/listen/accept/seal) live in host/sock.c; prel stays socket-free so the
; kernel/wasm frontends are unaffected.

; The whole tool is ONE define: a body-less top-level `:` leaks `ain-main` (args ->
; conversation) and nothing else; the SEAT form at the bottom fires it only when the
; file itself was invoked (positional or -l), so a multi-call cat stays quiet.
; dns is a MODULE in the tree and a CATTED SIBLING in the dist artifact, so ask
; for it only when `dial` is not already here -- the artifact has no lib/ to load
; from, and a `use` that misses scares rather than shrugging.
(: _ (? (member? 'dial (names ())) 0 (use 'dns)))

(: (ain-main as)
 (:
    ; parse a decimal port string -> a fixnum (a string indexes its bytes; '0' = 48).
    (atoi s) (: (go i acc) (? (< i (tally s)) (go (+ 1 i) (+ (* 10 acc) (- (s i) 48))) acc)
                (go 0 0))
    (die m) (: _ (say err (+ "ain: " (+ m "\n"))) (quit 1))

    ; pump bytes from port `src` to port `dst`, one byte at a time, until src
    ; hits EOF (get -> -1). flush after each byte makes stdout prompt (it is a
    ; no-op on a socket port -- fd_flush only flushes the real stdout fd), so one
    ; pump serves both directions. Returns nil at EOF; the caller owns teardown.
    (pump src dst) (: ch (see src)
                      (? (= ch -1) 0
                         (: _ (put dst ch) _ (flush dst) (pump src dst))))

    ; the conversation over a connected socket `s`. The two directions are
    ; INDEPENDENT half-duplex channels: a read-side EOF does NOT mean the write
    ; side is dead, so we must drain our outbound stdin fully before closing.
    ;   - stdin -> socket runs in THIS task: it always sends every stdin byte
    ;     before teardown, then half-closes the write side (seal s 1) so the
    ;     peer sees EOF rather than a hung half-open socket.
    ;   - socket -> stdout runs in the spawned task `down`; it ends when the peer
    ;     closes (socket EOF). The two pumps interleave through the cooperative
    ;     scheduler (each parked in a port read on its own fd).
    ; Teardown joins through one path: send all of stdin, half-close, then WAIT
    ; for the inbound pump to drain to the peer's EOF before the final close.
    (talk s) (: down (twirl (\ _ (pump s out)) 0)   ; socket -> stdout, in a task
                _ (pump in s)                        ; stdin -> socket (drains fully here)
                _ (seal s 1)                     ; half-close write: peer sees stdin EOF
                _ (catch down)                        ; let the inbound pump drain to socket EOF
                _ (flush out)
                _ (close s)
                1)

    ; server: listen + accept one client, then talk. close the listener after.
    (serve port) (: l (listen port)
                    (? l (: c (accept l)
                            (? c (: _ (talk c) _ (close l) 1)
                               (: _ (close l) (die "accept failed"))))
                       (die (+ "cannot listen on port " (string port)))))

    ; client: resolve, connect, then talk.
    (client host port) (: s (dial host port)
                          (? s (talk s) (die (+ "cannot connect to " (+ host (+ ":" (string port)))))))

    ; dispatch on the args: -l PORT = server, HOST PORT = client, else usage.
    (? (= (cap as) "-l") (? (two? (cup as)) (serve (atoi (caup as)))
                            (die "usage: ain -l PORT"))
       (two? (cup as)) (client (cap as) (atoi (caup as)))
       (die "usage: ain HOST PORT  |  ain -l PORT"))))

; the SEAT: fire ain-main iff this very file sits in the program seat of the real
; command line -- the positional program, or a `-l` preload (the installed shebang) --
; taking the args AFTER the seat. `cmdline` is the full C argv, never rebound, so the
; test reads the same in both load orders; a multi-call cat (kore) never matches and the
; dispatcher there calls ain-main itself. body-having: nothing leaks.
(: (bn p) (: n (tally p)
             (go k r) (? (< k n) (go (+ k 1) (? (= (p k) 47) (+ k 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (= b "ain") (= b "ain.l")))
   (seat a) (? (! (two? a)) ()
               (= (cap a) "-l") (? (two? (cup a))
                                   (? (me? (caup a)) (link 1 (cuup a)) (seat (cuup a)))
                                   ())
               (me? (cap a)) (link 1 (cup a))
               ())
   s (seat (cup cmdline))
   (? (two? s) (ain-main (cup s)) ()))
