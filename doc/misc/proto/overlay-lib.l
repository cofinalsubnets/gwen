; ============================================================================
; overlay-lib -- the rung-1/2 type-overlay toolkit, as a LOADABLE library.
;
;   out/host/love -l doc/misc/proto/overlay-lib.l            # load it, then REPL on stdin
;
; then, one form per line:
;   (domain units meters seconds area)                # define a units type-system
;   (overlay-set units ())                            # turn it on
;   (deft (total (a meters) (b meters)) meters (+ a b))   # a typed function (checks + runs)
;   (total 3 4)                                       # => 7   (types erased; total is a global)
;   (deft (mix (a meters) (t seconds)) meters (+ a t))    # ;; overlay: type clash in (mix)
;   (overlay-off ())                                  # turn checking off (globals untouched)
;
; WHAT LIVES WHERE: the fn defs + the domain are ordinary globals (the book); the sigs live
; in the overlay's slot (overlay-off clears the checking, not the code). `deft` checks at
; COMPILE time, so a clash prints the instant you enter the def. The overlay engine itself
; is in love/overlay.l (baked in); this lib is pure user-level, riding the ov-slot/ov-eng globals it
; leaks. Nominal units domain: +/- must match units (else clash); * gives `top` (untracked).
; See doc/misc/proto/overlay-macros.l (worked demo) and memory pluggable-types.
; ============================================================================
(: overlay-sig (\ s    (: v (peep ov-slot 0 ()) (? (two? v) (pin ov-slot 0 [(cap v) (link s (caup v))]) 0)))     ; append a boundary sig
   overlay-chk (\ form (: v (peep ov-slot 0 ()) (? (two? v) (ov-eng (cap v) (caup v) form) 0)))                  ; check a def vs the installed overlay
   (units-tr op x y) (? (id? x 'top) 'top (id? y 'top) 'top
                        (member? op '(+ -)) (? (= x y) x (id? x 'bot) y (id? y 'bot) x 'clash)
                        'top)
   (units-join x y)  (? (= x y) x 'top)
   (units-lit e)     (? (charm? e) 'bot 'top)
   ; (domain NAME die..) -> binds NAME to the domain map (dice = the given units)
   _ (:: 'domain (\ a (: nm (cap a)
        dom (hash 'dice (cup a) 'ctors () 'tr units-tr 'join units-join 'litdie units-lit)
        [': nm ['\ dom]])))
   ; (deft (NAME (p TYPE)..) RET body) -> register the sig, check the body, expand to a plain def
   _ (:: 'deft (\ a (: header (cap a) ret (cap (cup a)) body (cap (cup (cup a)))
        name (cap header) tparams (cup header)
        sig (link name (link (map caup tparams) (link ret ())))     ; (NAME (ptypes..) RET)
        hdr (link name (map cap tparams))                            ; (NAME p1 p2..)
        defform (link ': (link hdr (link body ())))                  ; (: (NAME p..) body)
        _ (overlay-sig sig)                                          ; register the boundary type
        _ (overlay-chk defform)                                      ; check the body NOW (compile time)
        defform)))                                                   ; expand to the plain def (erasable)
   ; (sig NAME (PARAM-TYPE..) RET) -> declare a PRIMITIVE's type ONCE (the trusted boundary: matmul, etc.)
   _ (:: 'sig (\ a (link 'overlay-sig (link (link '\ (link a ())) ())))))   ; a IS the sig (NAME (ptypes) ret) -> (overlay-sig 'a)
