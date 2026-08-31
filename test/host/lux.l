; --- test/host/lux.l: a runnable smoke test for lux's socket nif (src/sock.c) ---
; Standalone against the built binary, like test/host/net.l and for the same reason
; (host/*.c links the final `love` only; love0 bakes the corpus without it):
;
;   cat test/00-init.l test/host/lux.l | out/host/love
;
; (connectu path) opens a unix-domain stream socket. a real round trip needs a
; unix listener on the other end -- that is an X display socket in practice, so
; the integration proof lives in the lux spikes (doc/misc/proto/lux.l against Xephyr),
; not the gate. Here: the nif is present and every refusal lane answers nil,
; not a scare -- absence is nothing.
(assert (lit? connectu))
(assert !(connectu "/nonexistent/never/sock"))     ; no such path -> nil
(assert !(connectu ""))                            ; empty path -> nil
(assert !(connectu "/tmp"))                        ; a directory, not a socket -> nil
(assert !(connectu 7))                             ; not a string -> nil
(test_fin ())
(say err "test/host/lux: ok\n")
