; test/front/io.l -- the laws of the WOULD-BLOCK park, gated at last.
;
; these run under test/front/main.c, the test-only frontend, because the branch
; they cover cannot be reached from inside a normal love: `lvm_fgetc`'s readiness
; guard and its refill are ONE op and the vm yields only at an `Ap`, so nothing can
; run between "poll said ready" and "read said no". only a second process sharing
; the open file description can lose that race -- or a frontend that lies, which is
; what this one is for.
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

; and the end survives a re-read: eof_seen latches, so the second ask agrees.
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

(: _ (say out "front: ok") (putc 10))
