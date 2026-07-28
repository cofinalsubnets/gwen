; --- test/host/pty.l: a runnable smoke test for bao's pty nifs (host/posix.c) ---
; NOT in the test/*.l corpus on purpose: the pty nifs (mind/gather/still/winsize)
; live in host/posix.c, which the final `love` links but love0 does NOT -- and love0
; bakes the whole corpus, so a corpus test would read these names as missing and
; fail love0's self-test. So this is verified the host-nif way: standalone, against
; the built binary. Run it (the assert macro comes from test/00-init.l):
;
;   cat test/00-init.l test/host/pty.l | out/host/love      ; green dots = pass, exits 1 on fail
;
; Mechanics are scriptable (here); full interactive wrapping is manual (bao.l).

; gather is non-blocking (WNOHANG): a child can EOF its master a hair before it
; becomes a zombie, so a poll right then reads 0 "still running". That gap is
; exactly why gather returns a PAIR -- poll, don't twirl. waitfor blocks for it.
; The exited pair is told from still-running STRUCTURALLY (two?), never by net:
; the status pair (0) nets blue (a chain of nothings is nothing), so it can't be
; distinguished from 0 by ! / nil? -- only by being a chain.
(: (waitfor pid)
 (: s (gather pid) (? (two? s) s (= 0 s) (do (rest 2) (waitfor pid)) s)))

; --- (mind argv): spawn `echo hello` on a pty, read its output off the master ---
(: r (mind `("echo" "hello"))
 _ (assert
    (two? r)            ; (pid . master-port)
    (charm? (cap r))        ; pid is a fixnum
    (< 0 (cap r))         ; ... a real one
    (lit? (cup r)))       ; the master is a heap port
 out (slurp (cup r))      ; drain the master to EOF (echo exits -> slave closes)
 _ (assert
    (< 0 (tally out)))    ; got bytes ("hello" + the pty's \n -> \r\n)
 st (waitfor (cap r))     ; gather the child
 _ (assert
    (two? st)           ; reaped: a PAIR, not () "still running"
    (= 0 (cap st))      ; echo exited 0 -- the pair keeps 0 truthy/present
    (= st (L 0))))      ; ... a REAL ()-tailed list (not the charm-0 dotted fossil)

; --- (still pid sig) + signal-death gather: cat waits forever; SIGKILL it ---
(: r (mind `("cat"))
 _ (assert (two? r))
 _ (still (cap r) 9)       ; SIGKILL -- a positive pid, the process (not the group)
 _ (slurp (cup r))        ; drain to EOF (the killed cat's slave closes)
 st (waitfor (cap r))
 _ (assert
    (two? st)           ; reaped
    (= 137 (cap st))))    ; 128 + 9: the signal-death encoding

; --- (setwinsize port r c): ok on a live master, errno on a non-port ---
(: r (mind `("cat"))
 _ (assert
    (nil? (setwinsize (cup r) 24 80))   ; () = ok
    (charm? (setwinsize 5 24 80)))       ; a bare fixnum (not a port) -> EBADF
 _ (still (cap r) 9)
 _ (slurp (cup r))
 _ (waitfor (cap r)))

; --- (still pid sig) on a dead pid -> an errno fixnum (ESRCH), never () ---
(: dead (cap (mind `("true")))      ; exits immediately
 _ (waitfor dead)                      ; gather it so the pid is truly gone (not a zombie)
 _ (assert (charm? (still dead 0))))     ; sig 0 probes existence -> ESRCH

; --- bidirectional pty I/O: write to the master, read the child's echo back ---
; the foundation of bao's wrapper -- a host process and bao talk over the master
; port. cat echoes its input, and the pty's own line discipline echoes the typed
; bytes too, so the literal input comes straight back; read it byte by byte (a
; bounded read, NOT slurp -- cat never EOFs). proves the write AND read lanes of
; the master port in one round trip; the full stdin<->stdout pump (bao's `wrap`)
; rides real tty fds, so it is verified manually, like ain's `make nettest`.
(: r (mind `("cat"))
 m (cup r)
 _ (put m 110) _ (put m 101) _ (put m 107) _ (put m 111) _ (put m 10)  ; "neko\n"
 _ (flush m)
 a (see m) b (see m) c (see m) d (see m)    ; the first 4 bytes back off the master
 _ (assert (= a 110) (= b 101) (= c 107) (= d 111)) ; n e k o -- the round trip over the pty
 _ (still (cap r) 9)
 _ (waitfor (cap r)))

; --- (ptyecho port on): silence the pty's input echo so bao's edraw owns it ---
; with echo OFF, a child whose OUTPUT differs from its INPUT shows only the output:
; `tr a-z A-Z` turns "hi" into "HI", so the lowercase line-discipline echo is gone.
; (this is what stops wrap doubling each edited line.) tr's stdout is the slave tty,
; so it line-buffers and flushes "HI\n" on the newline.
(: r (mind `("tr" "a-z" "A-Z"))
 m (cup r)
 _ (assert (nil? (ptyecho m 0)))                  ; echo OFF -> () ok
 _ (put m 104) _ (put m 105) _ (put m 10)  ; "hi\n"
 _ (flush m)
 a (see m) b (see m)                          ; first bytes back are tr's OUTPUT
 _ (assert (= a 72) (= b 73))                     ; "HI" -- the "hi" echo is suppressed
 _ (still (cap r) 9)
 _ (waitfor (cap r)))
(: _ (assert (charm? (ptyecho 5 0))))               ; non-port -> errno (EBADF)

(puts "pty: ok") (putc 10)
