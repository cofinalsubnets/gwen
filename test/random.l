; random (xoshiro256++ / SplitMix64), two layers. FUNCTIONAL: (wheel n) -> a fresh
; 256-bit state (rank-1 len-4 tuple); (random st n) -> (value . st'), value's TYPE
; by n -- positive int -> int in [0,n), float -> float in [0,n), n <= 0 -> a
; full-width int. The state is copied not mutated, so a seed repeats on every target
; (the tuple is a Z array on a 64-bit word, a C array on wasm -- the draw canonicalizes
; to 62 bits). GLOBAL (hidden clock-seeded stream; (rng-set (wheel n)) pins it): (rand n)
; -> int in [0,n) (n<=0 full-width); (randf n) -> float in [0,n) (n<=0 -> [0,1)).

(: (rdraw s) (cap (random (wheel s) 0))                  ; 1st full-width functional draw
   (rseq3 s) (: st (wheel s) a (random st 0) b (random (cup a) 0) c (random (cup b) 0)
                [(cap a) (cap b) (cap c)]))             ; first 3 full-width functional draws
(assert
 ; --- state-tuple structure: rank-1, length 4, 64-bit limbs (Z array, or C on
 ; a 32-bit word where C is the 8-byte-wide kind -- the 256-bit payload either way) ---
 (= 4 (tally (wheel 1)))
 (= 1 (rank (wheel 1)))
 (= (tier (wheel 0)) (tier (wheel 1)))            ; a seed's limb type is stable...
 (? word (= star-tray (tier (wheel 1))) 1)       ; ...a Z (word-int) array on a 64-bit word (a C array on wasm)

 ; --- determinism: same seed -> same functional sequence ---
 (= (rseq3 42) (rseq3 42))
 (= (rdraw 7) (rdraw 7))
 ; --- different seeds give different streams ---
 !(= (rdraw 1) (rdraw 2))

 ; --- functional purity: random does not mutate its input state ---
 (: st (wheel 7) a (cap (random st 0)) b (cap (random st 0)) (= a b))
 ; ...and chaining the returned state advances the stream
 (: st (wheel 7) p (random st 0) q (random (cup p) 0) !(= (cap p) (cap q)))
 (: st (wheel 7) p (random st 6) (= 4 (tally (cup p))))   ; st' is a state tuple

 ; --- (random st n), positive int n: bounded int draws stay in [0,n) ---
 (: st (wheel 9) (< (cap (random st 10)) 10))
 (: st (wheel 9) (<= 0 (cap (random st 10))))
 (: st (wheel 9) (< (cap (random st 2)) 2))
 (: r (random (wheel 9) 6) (<= 0 (cap r)))
 (: r (random (wheel 9) 6) (< (cap r) 6))
 (: st (wheel 9) (= (cap (random st 6)) (cap (random st 6))))    ; purity again

 ; --- (random st n), float n: float draw in [0,n) ---
 (: r (random (wheel 3) 1.0) (< (cap r) 1.0))
 (: r (random (wheel 3) 1.0) (<= 0 (cap r)))
 (: r (random (wheel 3) 1.0) (gem? (cap r)))
 (: r (random (wheel 3) 2.0) (< (cap r) 2.0))
 (: r (random (wheel 3) 1.0) (= 4 (tally (cup r))))      ; st' still a state tuple

 ; --- global rand: int in [0,n); full-width non-negative for n <= 0 ---
 (< (rand 10) 10)  (<= 0 (rand 10))
 (< (rand 2) 2)    (<= 0 (rand 0))
 (<= 0 (rand -5))

 ; --- global randf: float in [0,n); [0,1) for n <= 0 ---
 (< (randf 0) 1)   (<= 0 (randf 0))   (gem? (randf 0))  ; (randf 0) -> [0,1)
 (< (randf 1) 1)   (<= 0 (randf 1))                     ; (randf 1) -> [0,1)
 (: u (randf 4) (&& (<= 0 u) (< u 4)))                  ; (randf n) -> [0,n)
 (gem? (randf 100))

 ; --- rng-set pins the global stream: (rng-set (wheel n)) makes rand/randf
 ; reproducible -- the public writer beside the public readers; rng-get reads it ---
 (: _ (rng-set (wheel 5)) a (rand 0) _ (rng-set (wheel 5)) b (rand 0) (= a b))
 (: _ (rng-set (wheel 5)) a (randf 0) _ (rng-set (wheel 5)) b (randf 0) (= a b))
 (: _ (rng-set (wheel 5)) a (rand 0) _ (rng-set (wheel 6)) b (rand 0) !(= a b))
 (= 0 (rng-set 'junk))                          ; a bad state answers 0, the stream unchanged
 (= 4 (tally (rng-get 0)))

 ; --- cross-target reproducibility pins (the same on host, kernel, and wasm) ---
 (= (rseq3 42) '(1186220554705070239 1269524112903976849 4314585860703317388))
 (= 978559313737459048 (rdraw 12345)))
