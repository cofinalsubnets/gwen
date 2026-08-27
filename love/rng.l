; rng -- the random layer, two faces over the pure C steps; C holds no state.
; (wheel n) -> a fresh 256-bit state, turn/turnf step it. love/prel.l first.
;
; functional, reproducible: (random st n) draws once -> (value . st'), the value's type
; by n -- float -> float in [0,n), positive int -> int in [0,n), n <= 0 -> full-width int.
; global, one hidden stream lazily clock-seeded: (rand n) -> int in [0,n), (randf n) ->
; float in [0,n); n <= 0 widens both. rng-get reads the stream, (rng-set (wheel n)) pins
; it so rand/randf reproduce -- a bad state answers 0 and leaves the stream alone.
;
; a module because the stream is private mutable state: rng-box is bound, not inlined --
; separate copies would be separate streams.
(module '(rng rand randf random rng-get rng-set)
(: rng-box {}
   (random st n) (? (gem? n) (turnf st @ (a . u) (n * a . u))
                    (0 < n)  (turn  st @ (a . u) (a % n . u))
                    (turn st))
   (rng-get _) (: st (rng-box ()) (? (tray? st) st
                (: f (wheel (clock 0)) (pin rng-box () f, f))))
   ; a valid state is rank-1, length 4, and the platform's own limb type -- compare
   ; against a fresh seed's tier, never a baked-in `z` (a 32-bit word rides a C array)
   (rng-set st) (tray? st && rank st = 1 && tally st = 4 && tier (wheel 0) = tier st ?
                 (pin rng-box () st, st) 0)
   (rand n) (: r (turn (rng-get 0))
             (pin rng-box () >r,
              charm? n && 0 < n ? (<r % n) <r))
   (randf n) (: r (turnf (rng-get 0))
              (pin rng-box () >r,
               constellation? n && 0 < n ? (n * <r) <r))))
