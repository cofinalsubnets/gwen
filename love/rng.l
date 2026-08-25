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
;
; NO BRACKETS HERE: the runtime supplies the layer. a frontend registers this
; source (a row in its ai_libs table) and its boot says (use 'rng) -- the loader opens the layer,
; leave registers it, the splice stays, and every bare consumer (the corpus,
; test/fuzz.l's generators, lux's seeded fuzz, sat's restarts) resolves
; random/rand/randf/rng-get/rng-set down the chain at its own compile. the box
; rides the layer like everything else here -- what the module buys is not
; secrecy (from reaches any member) but a QUIET BASE: the state key never
; touches orth, in a session or a baked image.
(module '(rng rand randf random rng-get rng-set)
(: rng-box {0}
   (random st n) (? (gem? n) (: r (turnf st) ((n * <r) >< >r))
                    (0 < n)  (: r (turn st)  ((<r % n) >< >r))
                    (turn st))
   (rng-get _) (: st (peep rng-box () 0)
                  (? (tray? st) st (: f (wheel (clock 0)) _ (pin rng-box () f) f)))
   ; valid state = rank-1, length 4, and the platform's own limb type -- 64-bit
   ; limbs ride a Z array on a 64-bit word but a C (8-byte) array on a 32-bit
   ; one, so compare against a fresh seed's type, never a baked-in `z`.
   (rng-set st) (? (tray? st && rank st = 1 && tally st = 4 && tier (wheel 0) = tier st)
                   (: _ (pin rng-box () st) st)
                   0)
   (rand n) (: r (turn (rng-get 0)) _ (pin rng-box () >r)
               (? (charm? n && 0 < n) (<r % n) <r))
   (randf n) (: r (turnf (rng-get 0)) _ (pin rng-box () >r)
               (? (constellation? n && 0 < n) (n * <r) <r)))

) ; module rng
