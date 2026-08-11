; the lisp help calling convention. the heard help -- (hear f) installs it --
; receives (help a b): the condition data, and nothing else -- every raise that
; reaches a help is a scare, so a word saying so carried no news. a bare scare is
; observed, then the default escape proceeds (oom is the only C scare today and
; is terminal, so that lane is exercised manually, not here).
; ⚠ the arity is the protocol and a miss is SILENT: a 3-arg help under-applies to
; a closure, which is truthy, and the raise site takes that closure as its value.
; the ap is UNINSTALLED at the end of this file.
(: help-log (tablet 0))

; a counting ap, keyed by the tag it hears
(hear (\ p e (: _ (pin help-log p (+ 1 (peep help-log p 0))) 0)))

; (scare a b) is the deliberate raise: the ap hears (help a b). unlike a C scare
; (oom, terminal), the raise point is a clean boundary, so the ap's result comes
; back as (scare a b)'s value.
(assert (= 0 (scare 'deliberate 'data)))            ; the counting ap returns 0 on a scare
(assert (= 1 (peep help-log 'deliberate 0)))        ; ... and the observation logged, exactly once

; the help function owns the protocol: deliver a constant instead
(hear (\ p e 'gotcha))
(assert (= 'gotcha (scare 'any 'thing)))            ; the deliberate scare resumes through it

; uninstall; the raw default is back
(hear 0)                       ; uninstall: a non-lit? value defers to the raw default
(assert !(heard ()) !(lit? (heard ())))

; help is present by its net, like everything: "" is a value netting nothing,
; so it is no help -- a bare missing read would go helpless (and surface ";; missing",
; which we don't trigger here). the koan's help
; (\ a b 1) is =-equal to 0 yet a top netting 1: installed, every scare answers 1,
; and a missing read fires the help instead of going helpless.
(hear "")
(assert !(heard ()) !(lit? (heard ())))
(hear (\ a b 1))
(assert (1 = (scare 'k 'v)) (1 = one-nothing-unpinned-2))   ; net-1 help: the miss fires it -> 1, no helpless read
(hear 0)

; the help is PER TASK: a raise finds the one its OWN task installed. one shared
; slot would hand every task whichever install landed last -- and a help that
; escapes (call-cc) would then land in another task's stack. ⚠ every twirl here
; is caught: an orphan stalls the kernel runner.
(assert (: pa (twirl (\ _ (: _ (hear (\ a b 'A)) _ (rest 0) (scare 'x 0))) ())
           pb (twirl (\ _ (: _ (hear (\ a b 'B)) _ (rest 0) (scare 'x 0))) ())
           ra (catch pa) rb (catch pb)
           (&& (id? ra 'A) (id? rb 'B))))
; a child starts under its parent's help, and its own install stays its own
(hear (\ a b `('parent a)))
(assert (: pc (twirl (\ _ (: _ (rest 0) (scare 'from-c 0))) ())        ; installs none
           rc (catch pc)
           pd (twirl (\ _ (: _ (hear (\ a b 'child)) _ (rest 0) (scare 'x 0))) ())
           rd (catch pd)
           mine (scare 'mine 0)
           (&& (two? rc) (id? 'from-c (caup rc))        ; inherited the parent's
               (id? rd 'child)                          ; ... its own install took
               (two? mine) (id? 'mine (caup mine)))))   ; ... and the parent's survived it
(hear 0)

; trap -- the call-cc catcher: (trap f x n) answers (f x) when it ran clean and
; (n a b) when a scare landed here. there is no success continuation: a clean run
; just answers, and n's own answer is the form's, so the caller wrote both halves
; and needs no wrapper to tell them apart. currying supplies the x; n wears the
; help's own arity, so welp is one. ⚠ nothing unwinds: the escape jumps past every
; pending restore between the raise and here, so a cleanup belongs in n.
(assert (3 = (trap (+ 1) 2 (\ a b 'no)))
        (id? (:) (trap (\ _ ()) () (\ a b 'no)))              ; a clean nothing is clean
        ('(boom 7) = (trap (\ _ (scare 'boom 7)) () (\ a b `(a b))))
        (id? (:) (trap (\ _ (scare 'x 0)) () welp))           ; the floor handler IS an n
        (3 = (trap (foldl (\ a x (? (x = 3) (scare 'hit x) a)) 0)   ; caught from depth,
                   '(1 2 3 4) (\ a b b)))                          ; ..and x is curried in
        ('(outer in) = (trap (\ _ (trap (\ _ (scare 'in 0)) () (\ a b (scare 'outer a))))
                             () (\ a b `(a b))))              ; n may re-raise: the outer help hears it
        ; one trap per task, all open at once: each catches its own
        (: (sess i) (twirl (\ _ (: _ (rest 0) (trap (\ _ (scare 'boom i)) () (\ a b b)))) ())
           ('(1 2 3) = (map catch (map sess '(1 2 3))))))
; n fires AFTER the displaced help is back: a raise inside it belongs to the trap
; OUTSIDE, never to this one -- and the restore rides both paths
(hear (\ a b 'floor))
(assert ('(outer-saw from-n) = (trap (\ _ (trap (\ _ (scare 'x 0)) () (\ a b (scare 'from-n a))))
                                     () (\ a b `('outer-saw a))))
        (: _ (trap (\ _ (scare 'x 0)) () (\ a b 0)) (id? 'floor (scare 'y 0))))
(hear 0)

; mind -- trap's sibling, and the cleanup owned by the RESOURCE instead of by the
; catcher: (mind f x c) answers (f x) and fires c on the way out, clean or raised,
; once either way. the condition rides on outward, so the trap that was going to
; catch it still does -- which is what lets a helper hold its own port however deep
; under one it sits.
(: mlog (tablet 0)
   (note s) (pin mlog 0 (mlog 0 + `(s)))
   (fresh _) (pin mlog 0 ())
   (assert
    ; the clean path: c fires after f, and f's own value is the form's
    (: _ (fresh ())
       v (mind (\ _ (: _ (note 'body) 'value)) () (\ _ (note 'freed)))
       (&& (id? v 'value) ('(body freed) = (mlog 0))))
    ; the raised path: c fires, and the condition goes on to the catcher outside
    (: _ (fresh ())
       r (trap (\ _ (mind (\ _ (: _ (note 'body) (scare 'boom 7))) () (\ _ (note 'freed))))
               () (\ a b `(a b)))
       (&& ('(boom 7) = r) ('(body freed) = (mlog 0))))
    ; one raise fires EVERY c between it and the catcher, innermost first
    (: _ (fresh ())
       r (trap (\ _ (mind (\ _ (mind (\ _ (scare 'deep 0)) () (\ _ (note 'inner))))
                          () (\ _ (note 'outer))))
               () (\ a b a))
       (&& (id? r 'deep) ('(inner outer) = (mlog 0))))
    ; c runs with the displaced help BACK, so its own raise belongs to the trap OUTSIDE
    (id? 'from-c (trap (\ _ (mind (\ _ 'v) () (\ _ (scare 'from-c 0)))) () (\ a b a)))
    ; ⚠ RAISES ONLY: only a scare reaches a help, so a k captured outside jumps clean
    ; past every c between there and here -- the wall this rung stops at
    (: _ (fresh ())
       v (call-cc (\ k (mind (\ _ (k 'jumped)) () (\ _ (note 'never)))))
       (&& (id? v 'jumped) (nil? (mlog 0))))))

; welp -- the floor handler (repl.l), the fromempty the introductions never
; named: it welps a raise to the ZERO POINT, whatever the condition. it is a
; plain function, not the installed help -- a policy that runs out of cases
; delegates to it, and shell-help is welp WITH A FACE (it prints `;; a b` first,
; then welps).
(assert (id? (:) (welp 'x 'y))                 ; a scare -> the zero point
        !(welp 'x 'y))                         ; ... which nets nothing (false)
