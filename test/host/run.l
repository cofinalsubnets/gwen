; --- test/host/run.l: smoke test for the hark / herald nifs (src/main.c) ---
; (hark argv) forks, captures the child's stdout into a string, and answers
; (status . output). (herald argv) is the SAME answer, but relays each chunk to our
; stdout as it is read instead of holding it to the exit -- what make does, and
; what lets a long cook step show progress. The tee must not disturb the capture.
; host-only (the nifs are host/*.c), so it runs standalone against the built
; binary like test/host/bake.l:
;   cat test/00-init.l test/host/run.l | out/host/love
;
; The payloads are deliberately TINY: herald echoes what it captures, so anything
; big would flood the gate log. The >64KB lane -- where the capture string
; outgrows its buffer and MOVES mid-drain -- is checked by hand instead:
;   (: c `("sh" "-c" "i=0; while [ $i -lt 20000 ]; do echo abcdefgh; i=$((i+1)); done")
;      (assert ((>(hark c)) = (>(herald c)))))
(: cmd ["sh" "-c" "printf 'tee-me\n'"]

   ; the two agree on BOTH halves of the answer: teeing changes when the bytes
   ; reach stdout, never what the caller is handed.
   a (hark cmd) b (herald cmd)
   _ (assert (two? a) (two? b))
   _ (assert (<a = <b) (<a = 0))
   _ (assert (>a = >b) (>a = "tee-me\n"))

   ; a nonzero exit still rides back through the tee path
   bad (herald ["sh" "-c" "printf 'nope\n'; exit 3"])
   _ (assert (two? bad) (<bad = 3) (>bad = "nope\n"))

   ; no output -> the EmptyString singleton, tee or not
   quiet (herald ["sh" "-c" "exit 0"])
   _ (assert (two? quiet) (<quiet = 0) (! >quiet))

   (puts "run: ok\n"))

; --- rung 4: a write to a device that will not take it does not stop the vm ----
; `sleep 1; cat` reads NOTHING for a second, so the pipe's 64K of kernel buffer
; fills and stays full. before the write door went nonblocking that was a write(2)
; that WAITED -- the whole vm, not the one task writing -- until the child got
; round to reading. the residue rides the port's write run now and the op comes
; straight back, so the wall clock is the law.
; ⚠ the peer must be another PROCESS. an in-process reader task cannot drain the
; pipe while the writer sits inside a blocking write(2) -- the scheduler is
; cooperative and never gets the turn -- so the control would DEADLOCK instead of
; reddening, and a gate that hangs is worse than one that fails.
(: pr (pipe 0)
   rfd (cap pr) wfd (cup pr)
   pid (spawnio ["sh" "-c" "sleep 1; cat >/dev/null"] rfd 1 2 [wfd] -1 0)
   _ (fdclose rfd)
   w (fdopen wfd)
   t0 (clock 0)
   _ (say w (* "x" 200000))          ; 200K against a 64K pipe: the tail cannot land
   ms (clock t0)
   _ (assert (charm? pid) (0 < pid)
             (ms < 500))             ; a blocking door sits here for the child's whole nap
   _ (close w)                       ; ai_io_wflush: the residue LANDS before the fd dies
   _ (wait pid))

(puts "run rung4: ok\n")

; --- the SIGPIPE pair: the console still dies, a child never inherits the ignore -
; love ignores SIGPIPE so a write to a hung-up peer ANSWERS instead of killing the
; runtime (test/host/net.l has that half). two things must survive that:
; ⚠ 1. A SHELL TOOL MUST STILL DIE ON A CLOSED PIPE, or `love ... | head` runs to
;   completion writing into nothing. the console re-raises by hand, so the wait
;   status stays a signal death and every downstream $? is unchanged. the spewer
;   below is BOUNDED (300000 lines) on purpose: under a regression it finishes
;   rather than hanging, so this reddens as a slow run, not a wedged gate.
; ⚠ 2. AN IGNORED DISPOSITION SURVIVES exec, so a child that inherited it is a
;   `yes | head` that never stops. every fork site resets it: main.c's two exec
;   sites by hand, and src/posix.c's sig_dfl_job for spawn/spawnio/spawnmap/tether.
;   the law below reaches `hark`'s door only -- the posix.c half is lush's, and
;   shares the one line.
; ⚠ the status has to come out of the PIPELINE, and $? there is head's, so love's
; own goes to a file from inside the subshell -- POSIX, no PIPESTATUS.
(: spew "( echo '(: (go n) (? (n = 0) 0 (: _ (putc 120) _ (putc 10) (go (n - 1)))) _ (go 300000) 0)' | out/host/love; echo $? > out/host/.sigst ) | head -1 > /dev/null; cat out/host/.sigst; rm -f out/host/.sigst"
   st (cup (hark ["sh" "-c" spew]))
   sig (cup (hark ["sh" "-c" "grep SigIgn /proc/self/status"]))
   _ (assert
      (st = "141\n")                       ; 128 + SIGPIPE: the death the shell has always seen
      (sig = "SigIgn:\t0000000000000000\n")))  ; the child inherits NO ignore at all

(puts "run sigpipe: ok\n")

; --- rung 6: seventeen parked tasks, and the wake reaches the last of them ------
; the law and the shape live in test/host/parked.l -- driven from HERE, under a
; timeout, because its regression is a HANG (the scheduler sleeping on a fd set
; the ninth parked task never got into) and a wedged gate is worse than a red one.
; ⚠ it must be a subprocess for that reason alone; nothing about it needs one.
(: rc (cap (hark ["sh" "-c" "timeout 20 out/host/love test/host/parked.l"]))
   _ (assert (rc = 0)))                    ; 124 is the timeout: the old cap's hang

(puts "run parked: ok\n")

; --- the nif floor: accept / udp-recv / wait all PARK --------------------------
; the laws and the shapes live in test/host/nifpark.l -- driven from HERE for the
; same reason parked.l is: two of its three regressions are HANGS (a blocking accept
; waits for a connection only the wedged vm could still make), and a wedged gate is
; worse than a red one. 124 IS the control's answer, and each law was verified by
; putting its own blocking call back.
(: rc (cap (hark ["sh" "-c" "timeout 20 out/host/love test/host/nifpark.l"]))
   _ (assert (rc = 0)))

(puts "run nifpark: ok\n")

; --- the resolver: a lookup parks, and a deadline is a park too -----------------
; test/host/dns.l stands up a fake nameserver as a love task, so nothing leaves
; the loopback. driven from HERE for the usual reason: a resolver whose deadline
; stops working does not redden, it HANGS -- there is no answer coming and nothing
; to wake the caller.
(: rc (cap (hark ["sh" "-c" "timeout 20 out/host/love test/host/dns.l"]))
   _ (assert (rc = 0)))

(test_fin ())
(puts "run dns: ok\n")
