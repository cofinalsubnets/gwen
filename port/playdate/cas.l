; port/playdate/cas.l -- the rune workbench: exact algebra riding the crank.
; the C frame (main.c) clears the console and fires (cas ()) each update;
; everything else -- the card deck, the state, the rendering -- lives here.
; nifs from main.c: (crank ()) the angle 0..359 or () docked, (pushed ())
; this frame's fresh button bits (left 1 right 2 up 4 down 8 B 16 A 32),
; (cur_set r c) seats the write cursor.
;
; the deck: crank (or left/right) picks a card, up/down turns the dial n,
; A differentiates the view (again and again -- the pipeline simps between),
; B flips a polynomial card into its factors. every answer on screen is
; exact -- Q coefficients, Zassenhaus under the B button, 30 fps.
(use 'rune)
(: NC 7
   px1 (poly (pvar 1 0)) px2 (poly (pvar 2 0)) py2 (poly (pvar 2 1))
   ; a card at dial n: 'tm the term, 'vs its variables, 'xp expand the rest
   ; view through the poly bridge, 'fac the B button works, 'as the
   ; assumptions contrast card (renders its own body)
   (card i n) (? (i = 0) {'vs '(x y) 'xp 1 'fac 1 'tm ['** '(+ x y) n]}
                 (i = 1) {'vs '(x) 'xp 1 'fac 1 'tm ['- ['** 'x n] 1]}
                 (i = 2) {'vs '(x y) 'xp 1 'fac 1 'tm ['- ['** 'x n] ['** 'y n]]}
                 (i = 3) {'vs '(x) 'tm '(* (sin x) (cos x))}
                 (i = 4) {'vs '(x) 'as 1 'tm '(sqrt (** x 2))}
                 (i = 5) {'vs '(x) 'tm '(+ (atan 1) (acos 0))}
                 {'vs '(x) 'tm '(** e x)})
   st {'card 0 'n 4 'dc 0 'fv 0 'key () 'txt ""}
   (clamp lo x hi) (? (x < lo) lo (hi < x) hi x)
   ; a term crosses to the canonical ring through the eval bridge: close it
   ; over its variables, apply the closure to the poly coins, and the coin
   ; arithmetic (die + - * and DieStar power) lands the monomial list.
   (tpoly t vs) (? (1 = tally vs) ((ev ['\ 'x t]) px1)
                   ((ev ['\ 'x 'y t]) px2 py2))
   (fshow p vs) (show (unfix (pemit p vs)))
   (joins l) (? (0 = tally l) "" (<l + (joins >l)))
   (count x l) (? (0 = tally l) 0
                  (<l = x) (1 + (count x >l))
                  (count x >l))
   (uniq l) (? (0 = tally l) ()
               (: f <l (link f (uniq (filter (\ y !(y = f)) >l)))))
   (factxt t vs) (: fs (map load (pfac (tpoly t vs)))
                    (joins (map (\ f (: k (count f fs) s ("  " + (fshow f vs))
                                        (? (k = 1) (s + "\n")
                                           (s + " ** " + (show k) + "\n"))))
                                (uniq fs))))
   ; the view, always a TERM (the display unfixes at the end): dc derivatives
   ; when A's been pressed (numeral application composes d/dx dc times), else
   ; the card's rest face -- the canonical monomial form back through the
   ; bridge for 'xp cards, collected simp otherwise
   (view c n dc) (: t0 (c 'tm)
                    (? (0 < dc) ((dc (\ u (deriv u 'x))) t0)
                       (c 'xp) (pemit (tpoly t0 (c 'vs)) (c 'vs))
                       (collect (simp t0))))
   (astxt _) ("f = sqrt (x ** 2)\n\n" +
              "  simp f          = " + (show (unfix (simp '(sqrt (** x 2))))) + "\n" +
              "  with ((pos x))  = " + (show (unfix (assume '((pos x)) '(sqrt (** x 2))))) + "\n\n" +
              "g = v * x / x\n\n" +
              "  d/dv g          = " + (show (unfix (deriv '(/ (* v x) x) 'v))) + "\n" +
              "  with ((nz x))   = " + (show (unfix (derivc '((nz x)) '(/ (* v x) x) 'v))) + "\n")
   (page ci n dc fv) (: c (card ci n)
      hd ("rune                                         [" + (show (ci + 1)) + "/" + (show NC) + "]\n" +
          "==================================================\n" +
          "n = " + (show n) + "\n\n")
      bd (? (c 'as) (astxt ())
            (: fl ("f = " + (show (unfix (c 'tm))) + "\n\n")
               dl (? (0 < dc) ("d/dx, " + (show dc) + " time" + (? (dc = 1) "" "s") + ":\n") "")
               rl (? (&& fv (c 'fac))
                     ("factors:\n" + (factxt (view c n dc) (c 'vs)))
                     ("  " + (show (unfix (view c n dc))) + "\n"))
               (fl + dl + rl)))
      (hd + bd))
   (idx a) (/ ((a * NC) - ((a * NC) % 360)) 360)
   (cas _) (: p (pushed ()) a (crank ())
              c0 (st 'card)
              c1 (? !a c0 (idx a))
              c2 (? (& p 1) (% (c1 + (NC - 1)) NC)
                    (& p 2) (% (c1 + 1) NC)
                    c1)
              n0 (st 'n)
              n1 (? (& p 4) (clamp 1 (n0 + 1) 8)
                    (& p 8) (clamp 1 (n0 - 1) 8)
                    n0)
              fresh (? (c2 = c0) (? (n1 = n0) 0 1) 1)
              dc (? fresh 0 (& p 32) (clamp 0 ((st 'dc) + 1) 4) (st 'dc))
              fv (? fresh 0 (& p 16) (1 - (st 'fv)) (st 'fv))
              key [c2 n1 dc fv]
              _ (? (key = (st 'key)) 0
                   (: _ (pin st 'card c2) _ (pin st 'n n1)
                      _ (pin st 'dc dc) _ (pin st 'fv fv)
                      _ (pin st 'key key)
                      (pin st 'txt (page c2 n1 dc fv))))
              _ (puts (st 'txt))
              _ (cur_set 28 0)
              (puts "[crank]card [^v]n [A]d/dx [B]factor")))
