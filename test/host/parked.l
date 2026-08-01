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

; --- defect 6: readiness is asked of an FD, but the bytes live in the PORT -----
; a task parked on a quiet fd over a buffer ANOTHER task's gulp filled used to
; sleep on top of exactly what it was waiting for. reaching it takes two tasks on
; ONE heap port where one uses the bulk lane, which is why it sat written down and
; unfixed: `in` cannot reach it (trickle never runs ahead) and `wrap` cannot (two
; tasks, two fds).
;
; ⚠ WHO WINS THE WAKE IS NOT A RACE, and the law depends on that. both tasks park
; on the same fd; when the byte lands, yield_sw_wait asks about the RUNNING task's
; own fd before it re-scans the ring, so main always comes back first and its `see`
; gulps BOTH bytes into the port. the peer is then parked on a fd with nothing left
; to say, over a buffer holding its byte -- and `catch` hangs instead of reaping.
(: p (pipe 0)
   r (fdopen (cap p))               ; ONE heap port; the write end stays a bare fd
   t (twirl (\ _ (see r)) ())
   _ (rest 20)                      ; t reaches its park BEFORE anything is written
   pid (spawnio `("sh" "-c" "sleep 0.3; printf AB") 0 (cup p) 2 `() -1 0)
   a (see r)
   b (catch t)                      ; hangs with the port half of the test taken out
   _ (wait pid)
   _ (assert (a = 65) (b = 66)))    ; one byte each, in order

; --- the same defect at the ENTRY: await asked the fd alone --------------------
; a port holding bytes is readable however quiet its fd is (love.c, bio_of's park
; law), so awaiting one whose buffer is already full parked on a device that has
; nothing left to say. one task is enough here, and the hang is the monotask wait.
(: p2 (pipe 0)
   r2 (fdopen (cap p2))
   pid2 (spawnio `("sh" "-c" "printf AB") 0 (cup p2) 2 `() -1 0)
   _ (wait pid2)
   a2 (see r2)                      ; gulps both; the second is in the buffer now
   _ (await r2)                     ; hangs with the buffer term taken out
   b2 (see r2)
   _ (assert (a2 = 65) (b2 = 66)))

(puts "parked: ok\n")
