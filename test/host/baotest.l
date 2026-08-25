; --- test/host/baotest.l: the editor + repl + parseall unit battery ---
; the multi-line line editor (ed*), the history zipper (uphist/downhist/mkframe/
; detach), joinln/flatten/splitat, and parseall used to be USER-VISIBLE book globals
; only because the corpus (the old test/repl.l + test/replio.l) tested them by name.
; They live INSIDE bao's closure (love/bao.l), off the global book -- and the CHECKS
; live here, where checks belong, reaching them through bao's test door:
;
;   (bao 'parts) -- the editor primitives as a tablet, the shape `from`
;   hands back for a module. (B 'edleft) reaches one, (keys B) lists them. Nothing
;   is defined globally to make this work, and the battery reads as ordinary asserts.
;
; bao is the BAKED shell core (in the host egg) and `bao` is public, so this file
; needs no -l:
;
;   cat test/00-init.l test/host/baotest.l | out/host/love
;
; (wired into `make test_hostnif`.) green dots = pass.
; k4/k7 pack the CPS continuation's (u l r d [hu hd cur]) arguments into a
; comparable list, exactly as the old test/repl.l did.

(: B ((from 'bao 'bao) 'parts)
   edleft (B 'edleft)     edright (B 'edright)   edbsp (B 'edbsp)       eddel (B 'eddel)
   edhome (B 'edhome)     edend (B 'edend)       edtop (B 'edtop)       edbot (B 'edbot)
   edup (B 'edup)         eddown (B 'eddown)     uphist (B 'uphist)     downhist (B 'downhist)
   detach (B 'detach)     emptybuf (B 'emptybuf) mkframe (B 'mkframe)   parses (B 'parses)
   joinln (B 'joinln)     flatten (B 'flatten)   splitat (B 'splitat)   parseall (B 'parseall)
   (k4 u l r d) (u >< l >< r >< d)
   (k7 u l r d hu hd cur) (u >< l >< r >< d >< hu >< hd >< cur)
   results [
   ; --- edleft / edright / edbsp / eddel / edhome / edend ---
  ((k4 () '(1) '(2 3) ()) = (edleft k4 () '(2 1) '(3) ()))
  ((k4 () '(2 1) () ['(3)]) = (edleft k4 ['(1 2)] () '(3) ()))
  ((k4 () () '(1 2) ()) = (edleft k4 () () '(1 2) ()))
  ((k4 () '(1) '(2 3) ()) = (edright k4 () () '(1 2 3) ()))
  ((k4 ['(1 2)] () '(3 4) ()) = (edright k4 () '(2 1) () ['(3 4)]))
  ((k4 () '(2 1) () ()) = (edright k4 () '(2 1) () ()))
  ((k4 () '(1) '(3) ()) = (edbsp k4 () '(2 1) '(3) ()))
  ((k4 () '(2 1) '(3) ()) = (edbsp k4 ['(1 2)] () '(3) ()))
  ((k4 () () '(1) ()) = (edbsp k4 () () '(1) ()))
  ((k4 () '(1) '(3) ()) = (eddel k4 () '(1) '(2 3) ()))
  ((k4 () '(1) '(2 3) ()) = (eddel k4 () '(1) () ['(2 3)]))
  ((k4 () '(1) () ()) = (eddel k4 () '(1) () ()))
  ((k4 () () '(1 2 3) ()) = (edhome k4 () '(2 1) '(3) ()))
  ((k4 () '(3 2 1) () ()) = (edend k4 () '(2 1) '(3) ()))
   ; --- edtop / edbot ---
  ((k4 () () '(1 2 3) ()) = (edtop k4 () '(2 1) '(3) ()))
  ((k4 () '(3 2 1) () ()) = (edbot k4 () '(2 1) '(3) ()))
  ((k4 () () '(1) ['(2) '(3)]) = (edtop k4 ['(2) '(1)] '(3) () ()))
  ((k4 ['(2) '(1)] '(3) () ()) = (edbot k4 () '(1) () ['(2) '(3)]))
  ((k4 () () '(1) ['(8 9) '(4)]) = (edtop k4 ['(1)] '(8) '(9) ['(4)]))
   ; --- edup / eddown ---
  ((k4 () '(1) '(2 3 4) ['(5 6)]) = (edup k4 ['(1 2 3 4)] '(5) '(6) ()))
  ((k4 () '(2 1) () ['(3 4 5 6)]) = (edup k4 ['(1 2)] '(4 3) '(5 6) ()))
  ((k4 () '(1) '(2) ()) = (edup k4 () '(1) '(2) ()))
  ((k4 ['(5 6)] '(1) '(2 3 4) ()) = (eddown k4 () '(5) '(6) ['(1 2 3 4)]))
  ((k4 () '(1) '(2) ()) = (eddown k4 () '(1) '(2) ()))
   ; --- joinln / flatten / splitat ---
  (() = (joinln ()))
  ('(1 2 3) = (joinln ['(1 2 3)]))
  ('(1 10 2) = (joinln ['(1) '(2)]))
  ('(1 2 10 3 10 4 5) = (joinln ['(1 2) '(3) '(4 5)]))
  ('(1 2 3) = (flatten () '(2 1) '(3) ()))
  ('(1 10 2) = (flatten ['(1)] '(2) () ()))
  ('(1 10 2 10 3) = (flatten ['(1)] '(2) () ['(3)]))
  (('(2 1) >< '(3 4)) = (splitat link () 2 '(1 2 3 4)))
  (('(2 1) >< ()) = (splitat link () 5 '(1 2)))
  ((() >< '(1 2 3)) = (splitat link () 0 '(1 2 3)))
   ; --- mkframe / detach / emptybuf / parses ---
  ([() '(1) ()] = (mkframe () '(1) () ()))
  ('(((1)) (2 3) (4) (5)) = (mkframe ['(1)] '(2 3) '(4) ['(5)]))
  (() = (detach () ()))
  (: cur (mkframe () '(1) () ()) ([cur] = (detach () cur)))
  (: cur (mkframe () '(1) () ()) old (mkframe () '(9) () ())
     ([cur old] = (detach [old] cur)))
  (emptybuf () () () ())
  !(emptybuf () '(1) () ())
  !(emptybuf ['(1)] () () ())
  !(emptybuf () () '(1) ())
  !(emptybuf () () () ['(1)])
  (parses () [49] () ())
  !(parses () () () ())
  !(parses () [40] () ())
  (parses [[40]] [41] () ())
   ; --- uphist / downhist ---
  (: cur (mkframe () '(1) () ()) old (mkframe () '(9) () ())
     ((k7 () '(9) () () () [cur] old) = (uphist k7 () '(1) () () [old] () cur)))
  (: old (mkframe () '(9) () ())
     ((k7 () '(9) () () () () old) = (uphist k7 () () () () [old] () ())))
  (: cur (mkframe () [49] () ())
     ((k7 () () () () () [cur] ()) = (uphist k7 () [49] () () () () cur)))
  (: cur (mkframe () [40] () ())
     ((k7 () [40] () () () () cur) = (uphist k7 () [40] () () () () cur)))
  ((k7 () [49] () () () () ()) = (uphist k7 () [49] () () () () ()))
  (: cur (mkframe () '(1) () ()) new (mkframe () '(9) () ())
     ((k7 () '(9) () () [cur] () new) = (downhist k7 () '(1) () () () [new] cur)))
  (: new (mkframe () '(9) () ())
     ((k7 () '(9) () () () () new) = (downhist k7 () () () () () [new] ())))
  (: cur (mkframe () [49] () ())
     ((k7 () () () () [cur] () ()) = (downhist k7 () [49] () () () () cur)))
   ; --- parseall: a line is ONE expression; floats; the more-sentinel ---
   ; a SINGLE datum is returned as-is in a one-list (no extra wrap), so a leading
   ; monadic stays fused: "+(-3)" -> (mono (+ (-3))) -> -3, not the head-position (+ -3).
  ('((1 2 3)) = (parseall '(49 32 50 32 51)))            ; "1 2 3" -> the application
  ('((1 2)) = (parseall '(40 49 32 50 41)))              ; "(1 2)" -> the one form, as-is
  (-3 = (ev <(parseall '(43 40 45 51 41))))              ; "+(-3)" -> monadic net = -3 (the bug fix)
  (() = (parseall ()))
  !(two? (parseall '(40 49)))
  (1.5 = <(parseall '(49 46 53)))
  (1000.0 = <(parseall '(49 101 51)))
  (two? <(parseall '(49 46 50 46 51)))      ; "1.2.3": a digit-led RUN factors now (test/glued.l), no longer a nom
   ; the more-sentinel: an unclosed shape is m (not two?); a complete datum is
  !(two? (parseall '(40 43 32 49)))                   ; "(+ 1" unclosed
  (two? (parseall '(49 32 43)))                       ; "1 +" complete (the curry partial)
  (two? (parseall '(49 32 63 32 39 97)))              ; "1 ? 'a" complete (curried cond)
  (two? (parseall '(40 43 32 49 32 50 41)))           ; "(+ 1 2)" closed
  (two? (parseall '(49 32 61 32 49)))]                ; "1 = 1"

   ; every check in the battery must be true.
   _ (each results (\ b (assert b)))
   ; and the battery must be non-trivial (a typo'd list returning () would
   ; vacuously pass the each above).
   _ (assert (< 50 (tally results))))

(test_fin ())
(puts "baotest: ok") (putc 10)
