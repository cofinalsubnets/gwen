; rng -- the random layer, two faces over the pure C steps (seed -> a fresh 256-bit state; C
; holds no state). a POST-EGG LAYER, not part of the language: prel is the language and stays
; minimal, so this rides its own out/lib/rng.h and each frontend includes the layers it wants
; (the host, love0, wasm and the K_TEST kernel do -- the corpus asserts on all five names;
; a shipped kernel and the playdate workbench carry no stream at all). love/prel.l first.
; the two raw steps STAY ON THE BOOK now (egg.l used to mop rand-next/randf-next as prel's
; internals): a wrapper that ships separately cannot capture what birth deletes, and the pair
; reads as public API anyway -- `seed` was never mopped, so minting a state without being able
; to step it was the odd shape.
; FUNCTIONAL (reproducible): (random st n) draws once -> (value . st'), value's TYPE by n --
; float -> float in [0,n), positive int -> int in [0,n), n <= 0 -> a full-width int.
; GLOBAL (hidden stream on book ('rng-state), lazily clock-seeded): (rand n) -> int in [0,n)
; (n<=0 full-width); (randf n) -> float in [0,n) (n<=0 -> [0,1)). rng-get/rng-set are the
; stream's PUBLIC plumbing -- (rng-set (seed n)) pins it, making rand/randf reproducible
; (a bad state answers 0, the stream unchanged); rng-get reads it.
(: (random st n) (? (gem? n) (: r (randf-next st) (link (* n (cap r)) (cup r)))
                    (< 0 n)  (: r (rand-next st)  (link (% (cap r) n) (cup r)))
                    (rand-next st))
   (rng-get _) (: st (peep book 'rng-state 0)
                  (? (tray? st) st (: f (seed (clock 0)) _ (pin book 'rng-state f) f)))
   ; valid state = rank-1, length 4, and the platform's own limb type -- 64-bit
   ; limbs ride a Z array on a 64-bit word but a C (8-byte) array on a 32-bit
   ; one, so compare against a fresh seed's type, never a baked-in `z`.
   (rng-set st) (? (&& (tray? st) (= 1 (rank st)) (= 4 (tally st)) (= (tier (seed 0)) (tier st)))
                   (: _ (pin book 'rng-state st) st)
                   0)
   (rand n) (: r (rand-next (rng-get 0)) _ (pin book 'rng-state (cup r))
               (? (&& (charm? n) (< 0 n)) (% (cap r) n) (cap r)))
   (randf n) (: r (randf-next (rng-get 0)) _ (pin book 'rng-state (cup r))
               (? (&& (constellation? n) (< 0 n)) (* n (cap r)) (cap r))))
