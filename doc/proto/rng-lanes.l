; doc/proto/rng-lanes.l -- xoshiro128++ x W: W INDEPENDENT streams, one per lane
; of a z-tray, in pure love. the worked example of the bitwise broadcast set
; (love.c, test/spec.l's width law) on a real algorithm, and the measurement that
; answers "could the RNG leave C so it can be a module?".
;
; WHY LANE-PARALLEL AND NOT SCALAR. xoshiro's update PERMUTES lanes (s2 ^= s0),
; and elementwise ops cannot permute -- so a single stream does not map onto tray
; ops at all. W independent streams do: every step is elementwise, nothing
; crosses lanes, and one step answers W draws. that is the shape an array
; language wants, and it needs no permute primitive.
;
; WHY 128++ AND NOT 256++. a z-tray cell is a machine word and love's charm is
; ~62 bits, so emulating 64-bit lanes costs a hi/lo split on every op. xoshiro128++
; is the same author and structure with FOUR 32-BIT words -- each state word is
; one cell outright, and the only masking is `& M` after a shift or an add.
;
; THE MEASUREMENT (x86-64, 4M draws, `make host` build):
;   W=4    1350 ms      W=64    396 ms      W=1024   316 ms
;   W=16    586 ms      W=256   321 ms      C nif     98 ms
; wider lanes amortize the per-op tray ALLOCATION (each elementwise op conses a
; fresh tray, so W=4 pays ~3.5 allocations per draw), and it PLATEAUS at ~3.2x
; the C nif. the floor is the elementwise kernel's per-cell load/dispatch/store,
; not allocation and not dispatch -- the glaze measured ~0% on this shape too.
; a flat SCALAR version threading 8 charms through a tail call lands at ~3.7x, so
; lane-parallelism buys the allocation back and little more.
;
; SO: love can host its own RNG at ~3x, and bulk generation is the honest shape
; for it -- but it does not beat the nif, and swapping the generator would change
; the documented stream (spec.l names xoshiro256++). recorded so the experiment
; is not re-run from scratch.
(:
   M 4294967295                                   ; the 32-bit lane mask
   (rotl x k) (((x << k) | (x >> (32 - k))) & M)  ; rotate each lane, elementwise

   ; ONE STEP over four W-lane trays -> (draws a' b' c' d'). every line is a
   ; broadcast op; nothing here knows how many lanes there are.
   (step a b c d)
    (: r (((rotl ((a + d) & M) 7) + a) & M)       ; the ++ scrambler
       t ((b << 9) & M)
       c2 (c ^ a) d2 (d ^ b)                      ; s2 ^= s0 ; s3 ^= s1
       `(r (a ^ d2) (b ^ c2) (c2 ^ t) (rotl d2 11)))   ; s0 ^= s3' ; s1 ^= s2' ; s2' ^= t ; rotl s3'

   ; a W-lane state word: lane i takes (i*4 + o), so the four words start every
   ; lane on a distinct nonzero quadruple (xoshiro refuses an all-zero state).
   ; ⚠ DEMO SEEDING ONLY -- a linear ramp leaves the lanes CORRELATED for the
   ; first draw or two (641 1669 2697 .. is the ramp showing through; by draw 2
   ; they have diverged). a real generator decorrelates each lane first, either
   ; through a splitmix-style scrambler or xoshiro's own jump().
   (lanes W o) (: (go i acc) (? (i < W) (go (i + 1) (link ((i * 4) + o) acc)) (rev acc))
                  (star-tray (list W) (go 0 ())))
   (state W) `((lanes W 1) (lanes W 2) (lanes W 3) (lanes W 4))

   ; --- the scalar reference: one stream, plain charms, no trays ---
   (scalar n a b c d acc)
    (? (n = 0) (rev acc)
       (: u ((a + d) & M)
          r (((((u << 7) | (u >> 25)) & M) + a) & M)
          t ((b << 9) & M) c2 (c ^ a) d2 (d ^ b)
          (scalar (n - 1) (a ^ d2) (b ^ c2) (c2 ^ t) (((d2 << 11) | (d2 >> 21)) & M) (link r acc))))

   ; lane 0 of the W-lane generator, as a list -- what the check compares
   (lane0 n a b c d acc)
    (? (n = 0) (rev acc)
       (: s (step a b c d)
          (lane0 (n - 1) (<>s) (<>>s) (<>>>s) (<>>>>s) (link (peep <s 0 -1) acc)))))

; --- the check: lane 0 IS the scalar stream, so the vector form is the same
;     generator and not merely a plausible-looking one ---
(: sc (scalar 8 1 2 3 4 ())
   st (state 4)
   v4 (lane0 8 <st <>st <>>st <>>>st ())
   _ (puts ("scalar   : " + (show sc) + "\n"))
   _ (puts ("lane 0   : " + (show v4) + "\n"))
   _ (puts ("rng-lanes: " + (? ((show sc) = (show v4)) "ok" "MISMATCH") + "\n")) ())
