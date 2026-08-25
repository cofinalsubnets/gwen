; overlay -- an OPTIONAL, observe-only static-type overlay hooked at the ev seam (spike:
; doc/misc/proto/overlay.l; design: memory pluggable-types). Install a user DOMAIN + SIG-TABLE with
; (overlay-set DOM SIGS); `ev` then REPORTS a type clash for each top-level `:` def and runs the form
; UNCHANGED (observe-only -- never alters eval). (overlay-off ()) uninstalls; uninstalled, `ev` is
; transparent (one peep per top-level form). ERASABLE by construction. Engine internals stay private
; inside `ov-eng`; only ov-*/overlay-*/the ev rebind leak. See doc/misc/proto/overlay-lib.l for the domain
; shape (a map {'join J 'tr T 'litdie L 'dice D}) and sigs ((name (param-dies..) result-die)..).
;
; a MODULE (no brackets: the runtime supplies the layer -- the frontend registers this source and
; its boot says (use 'overlay), then binds the accessor under the module's OWN name and hooks ev
; through ov-hook below). the ambient splice serves the bare names
; (overlay-set / overlay-off / ov-slot / ov-eng).
; kanren must load FIRST -- the engine reads subst through the registry and unify/ufail?/var bare.

(module '(overlay ov-check ov-eng overlay-off overlay-set ov-hook ov-slot)
(: ov-both
    (: subst (from 'kanren 'subst)                    ; through the kanren module (unify/ufail?/var stay global)
       (ov-nthl l n)     (? (n = 0) <l (ov-nthl >l (n - 1)))   ; nth (nthl is glaze-layer, absent here)
       (ov-sigof sigs nm) (? (atom? sigs) () (id? nm <<sigs) <sigs (ov-sigof >sigs nm))
       (ov-look env x)    (? (atom? env) 'bot (: p <env (? (id? x <p) >p (ov-look >env x))))
       (ov2-var? dom d)   (nom? d && !(member? d (dom 'dice)) && !(member? d (dom 'ctors)))   ; a sig variable: not concrete, not a constructor
       (ov2-vars dom d acc) (? (atom? d) (? (ov2-var? dom d && !(member? d acc)) (d >< acc) acc) (foldl (\ a s (ov2-vars dom s a)) acc d))
       (ov2-lk vm d)      (? (atom? vm) d (id? d <<vm) ><vm (ov2-lk >vm d))   ; ⚠ ><vm is GLUED: cuap, not the cons
       (ov2-term vm d)    (? (atom? d) (ov2-lk vm d) (map (\ s (ov2-term vm s)) d))
       (ov2-uall ps as s) (? (atom? ps) s
                             (atom? as) s                              ; a PARTIAL application: the given prefix checked, the rest open
                             (member? <as '(top bot)) (ov2-uall >ps >as s)   ; an UNKNOWN arg is gradual: consistent with any param, binds nothing
                             (: s1 (unify s <ps <as) (? (ufail? s1) ufail (ov2-uall >ps >as s1))))
       (ov2-inst dom sg args)                                          ; sg = (name (param-dies) result): structured caller instantiation
        (: params <>sg result (ov-nthl sg 2)
           vm (map (\ nm (nm >< var nm)) (ov2-vars dom (params >< result) ()))            ; one fresh logic var per sig variable
           s  (ov2-uall (map (\ p (ov2-term vm p)) params) args '(()))   ; unify param patterns (fresh vars) vs arg dies AS-IS (a caller's rigid symbol is a constant here -- no capture)
           (? (ufail? s) 'clash                                        ; args contradict the params: the one hard verdict
              (tally args != tally params) 'top                        ; partial/over-application: a closure/re-application, die unknown
              (: r (subst s (ov2-term vm result)) nf (dom 'norm) (? (lit? nf) (nf r) r))))   ; reify + normalize (dependent index)
       (ov-kindof dom sigs env e)
          (? (nom? e) (ov-look env e)
             (atom? e) ((dom 'litdie) e)
             (: h <e as (map (\ s (ov-kindof dom sigs env s)) >e)
                (? (member? h '(+ *)) ((dom 'tr) h <as <>as)
                   (id? h '? && tally e = 4) ((dom 'join) <>as <>>as)
                   (: sg (ov-sigof sigs h) (? (two? sg) (ov2-inst dom sg as) 'top)))))
       (ov-zip ps ds)     (? (atom? ps) 0 ((<ps >< <ds) >< ov-zip >ps >ds))
       (ov-clash dom sigs env e) (? (atom? e) 0 (: d (ov-kindof dom sigs env e) sub (catmap (\ s (ov-clash dom sigs env s)) >e)
                                                    (? (id? d 'clash) ((e >< map (\ s (ov-kindof dom sigs env s)) >e) >< sub) sub)))   ; a clash site = (expr . operand-dies)
       (ov-pairs bs)      (? (atom? bs) 0 (atom? >bs) 0 ((<bs >< <>bs) >< ov-pairs >>bs))
       (ov2-okr dom bd decl) (: f (dom 'okr) (? (lit? f) (f bd decl) ((dom 'join) bd decl = decl)))   ; result acceptable? domain `okr` (dependent = up to conv/norm), else structural join
       (ov-check1 dom sigs prog)
          (catmap (\ p (: nm <<p ps ><p body >p sg (ov-sigof sigs nm)   ; ⚠ ><p GLUED: cuap
                         (? !(two? sg) 0
                            (: env (ov-zip ps <>sg) cl (ov-clash dom sigs env body)
                               bd (ov-kindof dom sigs env body) decl (ov-nthl sg 2)
                               (? (atom? cl && ov2-okr dom bd decl) 0 [(nm >< (? (atom? cl) ['result bd decl] cl))])))))
                  (filter (\ p !(atom? <p)) (ov-pairs >prog)))
       (ov-djoin dies) (? (atom? dies) "?" (atom? >dies) (show <dies) (show <dies + " vs " + ov-djoin >dies))
       (ov-report nm detail)                             ; detail = (result BD DECL) | ((expr . opdies) ..)
        (? (id? <detail 'result)
           (: _ (puts ";; overlay: ") _ (puts (string nm)) _ (puts " -- returns ") _ (puts (show <>detail)) _ (puts ", declared ") _ (puts (show (ov-nthl detail 2))) (putc 10))
           (: site <detail _ (puts ";; overlay: ") _ (puts (string nm)) _ (puts " -- clash in ") _ (puts (show <site)) _ (puts "  (") _ (puts (ov-djoin >site)) _ (puts ")") (putc 10)))
       (ov-reps r) (? (atom? r) 0 (: _ (ov-report <<r ><r) (ov-reps >r)))
       (ov-run dom sigs form) (? (two? form && id? <form ':)
                                 (: r (ov-check1 dom sigs form) (? (two? r) (ov-reps r) 0))
                                 0)
     [ov-run ov-check1])                           ; the run entry + the PURE checker (internals private)
   ov-eng      <ov-both
   ov-check    <>ov-both                           ; (ov-check dom sigs form) -> () clean | ((name . detail)..) -- the report as DATA (a gate's leg)
   ov-slot     (tablet 0)                          ; install slot: peep 0 -> (DOM SIGS) or ()
   (overlay-set dom sigs) (pin ov-slot 0 [dom sigs])
   (overlay-off _) (pin ov-slot 0 ())
   ; ov-hook: the ev WRAPPER MAKER. `ev` itself must rebind in ORTH (the whole
   ; point is that every later (ev 'ev ..) walks into it), and a module layer
   ; cannot write there -- so the BOOT does it, at exactly this load's old seat
   ; in the sequence: (: ev ((from 'overlay 'ov-hook) ev)). e0 = the ev the boot
   ; hands in, CLOSURE-PRIVATE. (it was a global `ov-ev0` once; sweeping that
   ; nom off the book broke the LOADED image -- parked in sigsuspend -- an
   ; undiagnosed snapshot-codec edge, sidestepped by not having the name.)
   ; ⚠ two params where the caller hands ONE: currying, so (ov-hook ev) is still the wrapper.
   (ov-hook e0 e)
     (: v (peep ov-slot 0 ()) (? (two? v) (: _ (ov-eng <v <>v e) (e0 e)) (e0 e))))

) ; module overlay
