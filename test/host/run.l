; --- test/host/run.l: smoke test for the run / runt nifs (host/main.c host_run) ---
; (run argv) forks, captures the child's stdout into a string, and answers
; (status . output). (runt argv) is the SAME answer, but relays each chunk to our
; stdout as it is read instead of holding it to the exit -- what make does, and
; what lets a long cook step show progress. The tee must not disturb the capture.
; host-only (the nifs are host/*.c), so it runs standalone against the built
; binary like test/host/bake.l:
;   cat test/00-init.l test/host/run.l | out/host/love
;
; The payloads are deliberately TINY: runt echoes what it captures, so anything
; big would flood the gate log. The >64KB lane -- where the capture string
; outgrows its buffer and MOVES mid-drain -- is checked by hand instead:
;   (: c `("sh" "-c" "i=0; while [ $i -lt 20000 ]; do echo abcdefgh; i=$((i+1)); done")
;      (assert ((>(run c)) = (>(runt c)))))
(: cmd `("sh" "-c" "printf 'tee-me\n'")

   ; the two agree on BOTH halves of the answer: teeing changes when the bytes
   ; reach stdout, never what the caller is handed.
   a (run cmd) b (runt cmd)
   _ (assert (two? a) (two? b))
   _ (assert ((<a) = (<b)) ((<a) = 0))
   _ (assert ((>a) = (>b)) ((>a) = "tee-me\n"))

   ; a nonzero exit still rides back through the tee path
   bad (runt `("sh" "-c" "printf 'nope\n'; exit 3"))
   _ (assert (two? bad) ((<bad) = 3) ((>bad) = "nope\n"))

   ; no output -> the EmptyString singleton, tee or not
   quiet (runt `("sh" "-c" "exit 0"))
   _ (assert (two? quiet) ((<quiet) = 0) (! (>quiet)))

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
   pid (spawnio `("sh" "-c" "sleep 1; cat >/dev/null") rfd 1 2 `(wfd) -1 0)
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
