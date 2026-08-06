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

; welp -- the floor handler (repl.l), the fromempty the introductions never
; named: it welps a raise to the ZERO POINT, whatever the condition. it is a
; plain function, not the installed help -- a policy that runs out of cases
; delegates to it, and shell-help is welp WITH A FACE (it prints `;; a b` first,
; then welps).
(assert (id? (:) (welp 'x 'y))                 ; a scare -> the zero point
        !(welp 'x 'y))                         ; ... which nets nothing (false)
