; test/front/io.l -- the laws of the WOULD-BLOCK park, gated at last.
;
; these run under test/front/main.c, the test-only frontend. the PARK itself is
; ordinary now -- every quiet device on every frontend comes through it, so the
; whole corpus walks it -- but the SHARP case still cannot be reached from inside
; a normal love: `ai_ready` says go and the device then says no. the scheduler's
; readiness question and the device's answer are separated by no yield point, so
; only a second process sharing the open file description can lose that race --
; or a frontend that lies, which is what this one is for.
;
; the fault lives in TEST code. love carries no switch, no env var and no nif for
; it, which is the standing rule (doc/io.md).
;
; `(stall p k)` arms k would-block answers on the device's readn WHILE `ai_ready`
; keeps saying yes. what must then hold: the reading task PARKS and re-runs the op
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
; back in order. against a refill that WAITED instead of parking, the frontend's
; deadline-free wait exits 97 and the gate says so.
(: p (dev ())
   _ (feed p "abc")
   _ (stall p 3)
   _ (assert ((take p 3) = "abc")))

; --- law 2: the park yields, it does not stop the vm --------------------------
; the shape defect 5 was diagnosed on: one task reading a device that keeps
; answering would-block, one task doing something else. the reader must give the
; scheduler its turn. under the blocking refill the WHOLE process froze -- ticker,
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
; the two answers that used to share the -1 sentinel. an EMPTY device that has been
; shut answers END; an empty device that has not is merely quiet, and a stall in
; front of the end must not be mistaken for it.
(: p (dev ())
   _ (stall p 2)
   _ (shut p)
   _ (assert ((see p) = -1)))

; and the end survives a re-read -- ⚠ a DEVICE law, not a port one: nothing above
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
; device is READY -- the end is an answer -- and a stall does not change that,
; because the stall is a lie the DEVICE tells, not a fact about the stream.
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
; ⚠ THE ONE THIS FRONTEND WAS BUILT FOR that no gate could reach before. io_wdrain
; used to zero the pending length BEFORE the stroke and throw writen's count away,
; so a device that took part of a run reported a clean write of bytes it never got
; -- silent truncation, on every port, on the ordinary EPIPE/ENOSPC path. `wcap`
; makes the device take one byte per ask; every byte must still arrive, in order.
; ⚠ the bytes go in through `put`, which PARKS them in the port's write run --
; that run is what io_wdrain strokes and what used to be zeroed before the stroke.
; `say` would take the direct lane and never touch it.
(: p (dev ())
   _ (wcap p 1)
   _ (each '(97 98 99 100 101 102 103 104) (\ c (put p c)))
   _ (flush p)
   _ (assert ((sent p) = "abcdefgh")))

; --- law 8: a stalled write keeps its residue and its ORDER --------------------
; a device that lands nothing for a while must not lose the run, and the bytes that
; wait must not be overtaken by the bytes behind them. that second half is the trap:
; a refusal parks the byte in the PORT's buffer while the next stroke would go
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

(: _ (say out "front: ok") (putc 10))
