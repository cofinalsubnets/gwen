; --- test/host/parked.l: MANY tasks parked on MANY fds, and the wake finds them --
; NOT in hostnif_tests: this is the program test/host/run.l runs as a bounded
; SUBPROCESS, because its regression is a HANG and a gate that wedges is worse
; than one that fails. The law it carries:
;
;   the scheduler used to fold every parked task's fd into an `int fds[8]` on its
;   own C frame and SILENTLY DROP the rest, so a ninth parked task with no timer
;   pending could not wake the wait at all. kiosko twirls a task per client, so
;   that was a live hang in shipped code. love.c holds no fd array now (love.h's
;   arm/go: the fds go to the frontend one at a time), so the only bound is the
;   frontend's own storage -- the host's pollfd vector, which grows.
;
; the shape that reaches the wait: EVERY task parked and NO timer running, so
; nothing is runnable and the scheduler must actually sleep on the fd set. the
; wake therefore has to come from OUTSIDE the process -- a task that wakes a peer
; is itself runnable, and find_runnable answers before the wait is ever entered.
; ⚠ and no `rest` anywhere: a timer bounds the wait, which would turn the dropped
; fd from a hang into a latency and the law would pass over the bug.
;
; the target is the FIRST-twirled task -- tasks are prepended to the ring, so it
; sits furthest from the walking head and is the one the old cap dropped first.
(: (mk k acc) (? (k = 0) acc (mk (k - 1) (link (pipe 0) acc)))
   ps (mk 16 ())
   q (pipe 0)
   qw (fdopen (cup q))
   qr (fdopen (cap q))

   ; ⚠ the write ends stay OPEN here. close one and its reader gets the end of the
   ; stream instead of parking, and the whole shape evaporates.
   _ (each ps (\ p (twirl (\ _ (: r (fdopen (cap p))
                                  _ (see r)
                                  _ (say qw "x")
                                  (flush qw))) ())))

   ; the one byte from outside, a second from now: by then all 17 are parked.
   pid (spawnio `("sh" "-c" "sleep 1; printf x") 0 (cup (cap ps)) 2 `() -1 0)

   b (see qr)                      ; parks; only the woken peer's byte releases it
   _ (wait pid)
   _ (assert (b = 120)))
(puts "parked: ok\n")
