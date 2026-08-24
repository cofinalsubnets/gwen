; ============================================================================
; overlay-macros -- the ergonomic rung-1/2 surface: `domain` + `deft` macros.
; Run: out/host/love doc/misc/proto/overlay-macros.l   (the overlay lives in love/overlay.l, baked in.)
;
; The overlay (memory pluggable-types) checks a user-defined type domain against ai
; code, observe-only + erasable. Raw, that means a hand-built domain map and a
; disconnected sig-table. These two macros put a NICE face on the light end:
;
;   (domain units meters seconds area)   -- define a units type-system in one line
;   (overlay-set units ())               -- turn it on
;   (deft (total (a meters) (b meters)) meters (+ a b))   -- a typed function
;
; `deft` reads like a typed function (types at the boundary, in the header), REGISTERS
; the boundary sig + CHECKS the body at compile time (a clash reports as you compile,
; before it runs), and EXPANDS to a plain `:` def -- so the types are erasable and
; deft/plain code interoperate (gradual). All user-level: it rides `ov-slot`/`ov-eng`,
; the globals love/overlay.l already leaks -- no core change.
;
; NOTE (single-pass): the macros must be installed in an EARLIER top-level form than
; their uses, and `overlay-set` must run before the deft's compile (deft checks at
; EXPANSION time). Hence the form split below. Forward refs to a not-yet-typed callee
; read as `top` (gradual). `*`/`/` on units give `top` (an untracked compound) -- a
; richer domain would give rules; this nominal one just keeps `+`/`-` from mixing units.
; ============================================================================

; --- FORM 1: the toolkit (wiring + the `domain` and `deft` macros) ---
(: overlay-sig (\ s    (: v (peep ov-slot 0 ()) (? (two? v) (pin ov-slot 0 [(cap v) (link s (caup v))]) 0)))     ; append a boundary sig
   overlay-chk (\ form (: v (peep ov-slot 0 ()) (? (two? v) (ov-eng (cap v) (caup v) form) 0)))                  ; check a def against the installed overlay
   ; a nominal UNITS domain: +/- need matching units (bot = a unit-neutral scalar), else clash; * -> top
   (units-tr op x y) (? (id? x 'top) 'top (id? y 'top) 'top
                        (member? op '(+ -)) (? (= x y) x (id? x 'bot) y (id? y 'bot) x 'clash)
                        'top)
   (units-join x y)  (? (= x y) x 'top)
   (units-lit e)     (? (charm? e) 'bot 'top)
   ; (domain NAME die..) -> binds NAME to the domain map (dice = the given units)
   _ (:: 'domain (\ a (: nm (cap a)
        dom (hash 'dice (cup a) 'ctors () 'tr units-tr 'join units-join 'litdie units-lit)
        `(': nm `('\ dom)))))
   ; (deft (NAME (p TYPE)..) RET body) -> register the sig, check the body, expand to a plain def
   _ (:: 'deft (\ a (: header (cap a) ret (cap (cup a)) body (cap (cup (cup a)))
        name (cap header) tparams (cup header)
        sig (link name (link (map caup tparams) (link ret ())))     ; (NAME (ptypes..) RET)
        hdr (link name (map cap tparams))                            ; (NAME p1 p2..)
        defform (link ': (link hdr (link body ())))                  ; (: (NAME p..) body)
        _ (overlay-sig sig)                                          ; register the boundary type
        _ (overlay-chk defform)                                      ; check the body NOW (compile time)
        defform))))                                                  ; expand to the plain def (erasable)

; --- FORM 2: define a units type-system + turn it on ---
(domain units meters seconds area)
(overlay-set units ())

; --- FORM 3: typed defs -- read like typed functions, get checked, and RUN ---
(: _ (puts "-- deft total (meters,meters)->meters : ") _ (deft (total (a meters) (b meters)) meters (+ a b)) _ (puts "(silent = ok)\n")
   _ (puts "-- deft mix   (meters,seconds)->meters: ") _ (deft (mix (a meters) (t seconds)) meters (+ a t)) _ (putc 10)
   _ (puts "-- total 3 4 = ") _ (. (total 3 4)) _ (puts "   (runs -- types erased)\n")
   0)
