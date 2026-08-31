; test/front/io.l -- the laws of the would-block park, gated at last.
;
; these run under test/front/main.c, the test-only frontend. the park itself is
; ordinary now -- every quiet device on every frontend comes through it, so the
; whole corpus walks it -- but the sharp case still cannot be reached from inside
; a normal love: `ai_ready` says go and the device then says no. the scheduler's
; readiness question and the device's answer are separated by no yield point, so
; only a second process sharing the open file description can lose that race --
; or a frontend that lies, which is what this one is for.
;
; the fault lives in test code. love carries no switch, no env var and no nif for
; it, which is the standing rule.
;
; `(stall p k)` arms k would-block answers on the device's readn while `ai_ready`
; keeps saying yes. what must then hold: the reading task parks and re-runs the op
; rather than spinning the vm or losing the byte it was about to get.

(: (take p n)                       ; n bytes off p as text, stopping at the end
   ((: (go k acc)
       (? (k = 0) (string (rev acc))
          (: c (see p)
             (? (c = -1) (string (rev acc))
                (go (k - 1) (link c acc))))))
    n ()))

; --- law 1: a stalled read loses nothing --------------------------------------
; three would-block answers stand between the reader and three bytes that are
; already queued. every one of them must park and re-run, and the bytes must come
; back in order. against a refill that waited instead of parking, the frontend's
; deadline-free wait exits 97 and the gate says so.
(: p (dev ())
   _ (feed p "abc")
   _ (stall p 3)
   _ (assert ((take p 3) = "abc")))

; --- law 2: the park yields, it does not stop the vm --------------------------
; the shape defect 5 was diagnosed on: one task reading a device that keeps
; answering would-block, one task doing something else. the reader must give the
; scheduler its turn. under the blocking refill the whole process froze -- ticker,
; reader and all -- which is what made it worth a rung.
(: p (dev ())
   _ (feed p "xy")
   _ (stall p 6)
   n (tablet 0)
   t (twirl (\ _ ((: (go u) (: _ (pin n 0 (1 + (peep n 0 0)))
                                _ (rest 0)
                                (go ())))
                  ())) ())
   got (take p 2)
   _ (freeze t)
   _ (assert (got = "xy")
             (0 < (peep n 0 0))))       ; the peer ran while the reader waited

; --- law 3: would-block is not the end ----------------------------------------
; the two answers that used to share the -1 sentinel. an empty device that has been
; shut answers end; an empty device that has not is merely quiet, and a stall in
; front of the end must not be mistaken for it.
(: p (dev ())
   _ (stall p 2)
   _ (shut p)
   _ (assert ((see p) = -1)))

; and the end survives a re-read -- a device law, not a port one: nothing above
; readn remembers the end, so a shut device owes -1 to every ask (love.h's readn
; contract). this reddens if a device ever answers the end once and then goes quiet.
(: p (dev ())
   _ (shut p)
   _ (assert ((see p) = -1) ((see p) = -1)))

; --- law 4: a stall mid-run keeps the run -------------------------------------
; the refill hands back byte 0 and stashes the rest, so a stall that lands after a
; successful gulp must not disturb what is already buffered.
(: p (dev ())
   _ (feed p "hello")
   c (see p)                          ; one refill: 'h' out, "ello" stashed
   _ (stall p 4)
   _ (assert (c = 104)
             ((take p 4) = "ello")))

; --- law 5: cue? agrees with the park -----------------------------------------
; `cue?` is the dual of the park guard and must answer the same three terms. a shut
; device is ready -- the end is an answer -- and a stall does not change that,
; because the stall is a lie the device tells, not a fact about the stream.
(: p (dev ())
   _ (shut p)
   _ (assert (cue? p)))
(: p (dev ())
   _ (feed p "z")
   _ (stall p 1)
   _ (assert (cue? p)))

; --- law 6: the colist rides the park -----------------------------------------
; `flow` gulps through `see`, so a stall lands inside the promise's own force. the
; lazy tail must survive it -- and memoize, so a re-force does not re-drink.
(: p (dev ())
   _ (feed p "one")
   _ (shut p)
   _ (stall p 2)
   cl (flow p)
   _ (assert (string cl = "one")
             (string cl = "one")))      ; forced twice, same answer: `once` holds

; --- law 7: a short write loses nothing ---------------------------------------
; the one this frontend was built for that no gate could reach before. io_wdrain
; used to zero the pending length before the stroke and throw writen's count away,
; so a device that took part of a run reported a clean write of bytes it never got
; -- silent truncation, on every port, on the ordinary epipe/enospc path. `wcap`
; makes the device take one byte per ask; every byte must still arrive, in order.
; the bytes go in through `put`, which parks them in the port's write run --
; that run is what io_wdrain strokes and what used to be zeroed before the stroke.
; `say` would take the direct lane and never touch it.
(: p (dev ())
   _ (wcap p 1)
   _ (each '(97 98 99 100 101 102 103 104) (\ c (put p c)))
   _ (flush p)
   _ (assert ((sent p) = "abcdefgh")))

; --- law 8: a stalled write keeps its residue and its order --------------------
; a device that lands nothing for a while must not lose the run, and the bytes that
; wait must not be overtaken by the bytes behind them. that second half is the trap:
; a refusal parks the byte in the port's buffer while the next stroke would go
; straight to the device, so a two-lane writer shuffles its own stream.
; two refusals: one to the bulk stroke, one to the flush that follows it. the run
; waits in the port and the third ask -- this flush -- carries the whole of it.
(: p (dev ())
   _ (wstall p 2)
   _ (say p "hello world")
   _ (flush p)
   _ (assert ((sent p) = "hello world")))

; and the same over many small writes, where the residue has to survive the gaps
(: p (dev ())
   _ (wcap p 2)
   _ (wstall p 1)
   _ (each '("one" "two" "six") (\ s (say p s)))
   _ (flush p)
   _ (assert ((sent p) = "onetwosix")))

; --- law 9: flush delivers -- the residue finishes on its own -----------------
; rung 4 made the write door answer short, so a flush that took its answer left
; bytes behind and "flush" quietly meant "try". it means deliver again: the run
; that will not go parks the task and the op re-runs until the port is empty.
; five refusals stand between these bytes and the device; every one must be
; outlived, and none of them may stop the vm.
(: p (dev ())
   _ (wstall p 5)
   _ (say p "hello")
   _ (flush p)
   _ (assert ((sent p) = "hello")))

; --- law 10: the writer parks, it does not stop the vm -----------------------
; law 2's shape on the write side, and the whole point of the rung: the task
; that cannot deliver gives up its turn, so a peer runs while it waits. under a
; blocking write door the ticker below never moved at all.
(: p (dev ())
   _ (wstall p 30)
   n (tablet 0)
   t (twirl (\ _ ((: (go u) (: _ (pin n 0 (1 + (peep n 0 0)))
                                _ (rest 0)
                                (go ())))
                  ())) ())
   _ (say p "xyz")
   _ (flush p)
   _ (freeze t)
   _ (assert ((sent p) = "xyz")
             (0 < (peep n 0 0))))       ; the peer ran while the writer waited

; --- law 11: the write run is a buffer, not a queue --------------------------
; rung 4 made the door answer short and hand the remainder to the port, which is
; what stopped bytes being lost -- and it also meant `say` never refuses. a peer
; that stops reading used to hold the writer at its own rate through a blocking
; write(2); nothing replaced that, so the run grew by a whole string per say and
; kept growing. now a say that would push the run past its own size waits for the
; device instead. the device below refuses long enough that four says would pile
; up all four; the run must never hold more than one buffer's worth.
(: p (dev ())
   s (: (dup t n) (? (n = 0) t (dup (t + t) (n - 1))) (dup "0123456789abcdef" 8))
   _ (wstall p 40)
   hi (foldl (\ m _ (: _ (say p s) k (wpending p) (? (m < k) k m))) 0 '(1 2 3 4))
   _ (flush p)
   _ (assert ((tally s) = 4096)
             (hi <= 4096)                  ; 16384 with the park taken back out
             ((tally (sent p)) = 16384)))  ; and not one byte lost to the waiting

; and the same rule on the byte lane. `put` fills the run one byte at a time,
; and one byte at a time is still without a bound -- the run just doubled its
; backing instead of waiting. (`putx` carries the identical guard; it is the same
; six lines and reaching it wants a value big enough to be its own story.)
(: p (dev ())
   _ (wstall p 40)
   hi (: (go k m) (? (k = 0) m
                     (: _ (put p 120) j (wpending p) (go (k - 1) (? (m < j) j m))))
         (go 9000 0))
   _ (flush p)
   _ (assert (hi <= 4096)                 ; 8192 with the park taken back out
             ((tally (sent p)) = 9000)))

; --- law 13: a catcher parks, so the scheduler gets to sleep -----------------
; not a device law, and it is here because this is the only frontend that can
; ask the question. `catch` used to clear its own park and come back always
; runnable, so find_runnable answered it on every pass and the scheduler never
; reached its wait -- one core burnt, and every peer parked on a quiet fd polled
; at full speed instead of slept on. That is the arc's invariant inverted: not
; something else waiting, but the scheduler prevented from waiting.
; the park has two halves and one shape cannot reach both, which is why 13 and
; 14 look alike and are not. This one is the catcher's own yield: it must not take
; yield_sw's "still runnable, keep going" arm, because Ip is still pointing at the
; catch and keeping going means running the catch again, and again.
; the `rest 0` is load-bearing -- it lets the peer reach its own park first, so
; that when the catcher yields there is nothing runnable left in the tree. Drop it
; and the peer is fresh and runnable, the catcher's yield never reaches the arm at
; all, and the law proves nothing.
(: n0 (naps ())
   b (twirl (\ _ (rest 60)) ())
   _ (rest 0)
   _ (catch b)
   _ (assert (0 < ((naps ()) - n0))))    ; 0 with that arm's guard taken back out

; --- law 14: and find_runnable must not answer a catcher either ---------------
; the other half, on some other task's yield. a chain is what reaches it: the
; sleeper is the last to yield here and it yields with a deadline, so law 13's arm
; is never even offered and only the find_runnable clause can keep this green.
(: n0 (naps ())
   b (twirl (\ _ (rest 60)) ())
   a (twirl (\ _ (catch b)) ())
   _ (catch a)
   _ (assert (0 < ((naps ()) - n0))))    ; 0 with the find_runnable clause out

; --- law 15: the park still answers, on an exit and on a freeze ---------------
; a catcher wakes on its peer's exit and reaps the value, which is the whole of
; what catch promised before it went and did it by spinning.
(: t (twirl (\ _ (: _ (rest 20) 41)) ())
   _ (assert ((catch t) = 41)))

; and a caught peer frozen out from under the catcher must wake it too. freeze
; unsplices the node, so the pid goes gone rather than dormant -- a park that
; waited for dormancy alone would wait out the 5 s that never comes, and this
; frontend would exit 97 on the deadlock rather than hang.
(: t (twirl (\ _ (rest 5000)) ())
   _ (twirl (\ _ (freeze t)) ())
   _ (assert (! (catch t))))

(: _ (say out "front: ok") (putc 10))
