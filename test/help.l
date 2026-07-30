; the lisp help calling convention. a global `help` function receives
; (help s a b): s = the status word and a/b the condition data. a bare scare is
; observed, then the default escape proceeds (oom is the only C scare today and
; is terminal, so that lane is exercised manually, not here).
;
; ⚠ THE MORE BIT NO LONGER REACHES HERE. it used to be read control flow -- the
; reader raised an incomplete parse through help and the ap chose what the
; reader's caller saw -- but `sound` answers its own nothings as VALUES now
; (doc/reader.md rung 6c), so the scare bit is the only one a help ever sees.
; `scare?` is the whole status vocabulary now, exercised below directly.
; the ap is UNINSTALLED at the end of this file.
(: help-log (tablet 0))

; the status reader
(assert (scare? 1) !(scare? 3) !(scare? 2) !(scare? 0))

; a counting ap
(: (help s p e) (: _ (pin help-log s (+ 1 (peep help-log s 0))) 0))

; (scare a b) is the deliberate raise: the scare bit set unconditionally, the
; ap hears (help 1 a b). unlike a C scare (oom, terminal), the raise point is
; a clean boundary, so the ap's result comes back as (scare a b)'s value.
(assert (= 0 (scare 'deliberate 'data)))            ; the counting ap returns 0 on a scare
(assert (= 1 (peep help-log 1 0)))                  ; ... and the observation logged, exactly once

; the help function owns the protocol: deliver a constant instead
(: (help s p e) 'gotcha)
(assert (= 'gotcha (scare 'any 'thing)))            ; the deliberate scare resumes through it

; uninstall; the raw default is back
(: help 0)                       ; uninstall: a non-lit? value defers to the raw default
(assert !help !(lit? help))

; help is present by its net, like everything: "" is a value netting nothing,
; so it is no help -- a bare missing read would go helpless (and surface ";; missing",
; which we don't trigger here). the koan's help
; (\ s a b 1) is =-equal to 0 yet a top netting 1: installed, every scare answers 1,
; and a missing read fires the help instead of going helpless.
(: help "")
(assert !help !(lit? help))
(: help (\ s a b 1))
(assert (1 = (scare 'k 'v)) (1 = one-nothing-unpinned-2))   ; net-1 help: the miss fires it -> 1, no helpless read
(: help 0)

; welp -- the floor handler (repl.l), the fromempty the introductions never
; named: it welps a raise to the ZERO POINT, whatever the status. it is a plain
; function, not the installed help -- a policy that runs out of cases delegates
; to it, and shell-help is welp WITH A FACE (it prints `;; a b` first, then
; welps). it used to keep the read protocol too (more -> the port, eof -> the
; sentinel); the reader stopped raising those, so the floor is all there is.
(assert (id? (:) (welp 1 'x 'y))               ; a scare -> the zero point
        !(welp 1 'x 'y))                       ; ... which nets nothing (false)
