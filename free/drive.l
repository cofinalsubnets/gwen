; drive.l -- the self-improvement loop: the crew driving the ship to write the
; ship. fuzz.l/mutate.l are the FIND half (propose a program, judge it); this is
; the WRITE half (propose a source CHANGE, gate it, keep it or revert). rides the
; dock's verbs (look/lay/rebuild/sh) -- load it beside serve.l + judge.l.
;
;   out/host/love -l free/judge.l -l free/serve.l -l free/drive.l -e "(dock 7620)"
;   > (drive 1)            ; one iteration with the current proposer/grader
;
; TWO SEAMS, both slots you can retune live -- this is what makes it agnostic:
;   proposer : () -> (path body) | ()    a source change, or () for "nothing to do"
;   grader   : ()  -> (status output)    green iff (cap ...) = 0
; the DEFAULT proposer is a tiny local demo (so the loop runs with no model and no
; risk); the DEFAULT grader is the dock's `rebuild` (real build + gate). Swap the
; proposer for patch.l's model-driven one and a model drives; swap the grader for a
; fast stub to exercise the loop logic without a full rebuild (the tests do this).
;
; GIT IS THE SAFETY NET. a proposal is applied with `lay`, graded, and on a RED
; grade REVERTED with `git checkout --`. so a bad proposal -- a model's included
; -- can never leave the tree broken: the worst case is a wasted build. commit is
; left to the human/caller (drive reports the verdict; it does not push).

(: proposer-slot (tablet 0)
   grader-slot   (tablet 0)

   ;; -- the seams: read a slot, fall back to the default. --
   (proposer _) (peep proposer-slot 0 demo-propose)
   (grader _)   (peep grader-slot   0 default-grade)
   (set-proposer f) (pin proposer-slot 0 f)     ; e.g. (set-proposer (\ _ (ask-for-a-patch ...)))
   (set-grader   f) (pin grader-slot   0 f)     ; e.g. (set-grader (\ _ (gate ())))
   (default-grade _) (rebuild ())               ; the real thing: build + gate in place

   ;; -- git safety net. `revert` restores one path from HEAD; `head` is the
   ;; current commit (a witness we can report). both shell out through the dock. --
   (revert path) (sh (L "git" "checkout" "--" path))
   (head _) (caup (sh (L "git" "rev-parse" "--short" "HEAD")))

   ;; -- one iteration: propose, apply, grade, keep-or-revert. returns a verdict
   ;; list (tag path ...) so a loop can decide what to do next. --
   ;;   ('idle)                proposer had nothing
   ;;   ('kept path status)    green -- the change stands in the tree (uncommitted)
   ;;   ('reverted path status) red -- restored from HEAD, tree clean again
   (drive-step _)
     (: p ((proposer ()) ())                            ; (proposer ()) gets the fn; (.. ()) calls it
        (? (! (two? p)) `('idle)                        ; () or a non-proposal -> nothing to do
           (: path (cap p) body (caup p)
              _ (note (+ "drive: proposing a change to " path))
              _ (lay path body)
              g ((grader ()) ())                         ; build + gate (blocks the dock; deliberate)
              st (cap g)
              (? (= st 0)
                 (: _ (note (+ "drive: GREEN -- keeping " path)) `('kept path st))
                 (: _ (note (+ "drive: RED (" (+ (show st) ") -- reverting " path)))
                    _ (revert path) `('reverted path st))))))

   ;; -- run up to n iterations, stopping early on idle. collects the verdicts. --
   (drive n)
     (: (go i acc)
          (? (< i n)
             (: v (drive-step ())
                (? (id? (cap v) 'idle) (rev (link v acc))    ; nothing left to do -- stop
                   (go (+ i 1) (link v acc))))
             (rev acc))
        (go 0 ()))

   ;; -- the default demo proposer: a SAFE, reversible, guaranteed-GREEN change.
   ;; it appends a dated marker line to a scratch doc that is NOT in the gate, so
   ;; the build+gate always pass -- the point is to exercise the mechanism, not to
   ;; improve anything. it proposes ONCE, then goes idle (so `(drive 5)` runs once
   ;; and stops). replace this with a mutation of real source, or a model patch. --
   demo-slot (tablet 0)
   demo-path "free/drive-scratch.txt"
   (demo-propose _)
     (? (peep demo-slot 0 0) ()                          ; already proposed -> idle
        (: _ (pin demo-slot 0 1)
           old (: s (look demo-path) (? (string? s) s ""))
           `(demo-path (+ old "drive touched this\n")))))
