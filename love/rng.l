; rng -- the random layer, two faces over the pure C steps (wheel -> a fresh 256-bit state; C
; holds no state). a POST-EGG LAYER, not part of the language: prel is the language and stays
; minimal, so this rides its own out/lib/rng.h and each frontend includes the layers it wants
; (the host, love0, wasm and the K_TEST kernel do -- the corpus asserts on all five names;
; a shipped kernel and the playdate workbench carry no stream at all). love/prel.l first.
; the two raw steps STAY ON THE BOOK now (egg.l used to mop turn/turnf as prel's
; internals): a wrapper that ships separately cannot capture what birth deletes, and the pair
; reads as public API anyway -- `seed` was never mopped, so minting a state without being able
; to step it was the odd shape.
; FUNCTIONAL (reproducible): (random st n) draws once -> (value . st'), value's TYPE by n --
; float -> float in [0,n), positive int -> int in [0,n), n <= 0 -> a full-width int.
; GLOBAL (one hidden stream in a box, lazily clock-seeded): (rand n) -> int in [0,n)
; (n<=0 full-width); (randf n) -> float in [0,n) (n<=0 -> [0,1)). rng-get/rng-set are the
; stream's PUBLIC plumbing -- (rng-set (wheel n)) pins it, making rand/randf reproducible
; (a bad state answers 0, the stream unchanged); rng-get reads it.
; rng is a MODULE (kanren's shape) for one reason: it has private mutable state.
; the stream used to live at (pin book 'rng-state ..), which writes the BASE on
; every single draw -- the key appears the first time anyone calls rand and never
; leaves, and it lands there even in a baked image, because `book` folded into
; these closures at egg time and outlives being pulled off the namespace.
;
; the two halves are orthogonal and rng needs both. the LAYER gives privacy, so
; the cell never reaches the base. the BOX gives mutability: a binding in the
; layer is a compile-time definition, and nothing here could rewrite it at
; runtime -- reaching the layer's own map wants `books`, mopped long before this
; file loads, and `book` means the base forever (love.c pins it once). so the
; state is a box, love's mutable cell, the same thing boxfix allocates for the
; compiler's own letrec cells. it must be BOUND, not inlined: four `#0`s would be
; four separate streams.
(enter 'rng)
(: rng-box #0
   (random st n) (? (gem? n) (: r (turnf st) (link (* n (cap r)) (cup r)))
                    (< 0 n)  (: r (turn st)  (link (% (cap r) n) (cup r)))
                    (turn st))
   (rng-get _) (: st (peep rng-box () 0)
                  (? (tray? st) st (: f (wheel (clock 0)) _ (pin rng-box () f) f)))
   ; valid state = rank-1, length 4, and the platform's own limb type -- 64-bit
   ; limbs ride a Z array on a 64-bit word but a C (8-byte) array on a 32-bit
   ; one, so compare against a fresh seed's type, never a baked-in `z`.
   (rng-set st) (? (&& (tray? st) (= 1 (rank st)) (= 4 (tally st)) (= (tier (wheel 0)) (tier st)))
                   (: _ (pin rng-box () st) st)
                   0)
   (rand n) (: r (turn (rng-get 0)) _ (pin rng-box () (cup r))
               (? (&& (charm? n) (< 0 n)) (% (cap r) n) (cap r)))
   (randf n) (: r (turnf (rng-get 0)) _ (pin rng-box () (cup r))
               (? (&& (constellation? n) (< 0 n)) (* n (cap r)) (cap r))))
(leave ())
; the five PUBLIC names re-pin (kanren's emit-law shape): every consumer reads
; them bare -- the corpus, test/fuzz.l's generators, lux's seeded fuzz, sat's
; restarts -- and a sealed module cannot serve a bare name. rng-box is not among
; them, which is the whole point: it stays inside the layer, unreachable.
(: random (from 'rng 'random)
   rng-get (from 'rng 'rng-get)
   rng-set (from 'rng 'rng-set)
   rand (from 'rng 'rand)
   randf (from 'rng 'randf))
