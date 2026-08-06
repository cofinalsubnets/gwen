; apcap: the numeral ceiling -- a public tunable box, (pin apcap 0 n) retunes.
; a compose count above (apcap 0) raises (scare 'apcap k) instead of allocating
; O(k). the numeric power lane guards the RESULT instead: past the count gate an
; exact base beyond +-1 scares only when ~ k*log2|b| bits top the box read as a
; WORD budget (64 bits a word) -- a gem or |b| <= 1 base runs free at any
; exponent, a deliberate big power fits, the slipped tower still scares.
(assert (tablet? apcap) (= 1048576 (apcap 0)) (= 1048576 (peep apcap 0 0)))

; install a help that answers 'apcap scares with (capped k); anything else
; falls through to whatever was installed before (nil pre-help.l: const-1).
(: apcap-t0 (heard ()))
(hear (\ a b (? (= a 'apcap) `('capped b) (apcap-t0 a b))))
(assert
 ; a bignum compose count saturates to maxcharm, far above the cap -> scares
 (: r ((100 2) (+ 1)) (&& (= 'capped (cap r)) (< 1048576 (caup r))))
 ; 2 ** 2^100 would be the boom: ~2^100 result bits top the word budget
 (: r ((100 2) 2) (= 'capped (cap r)))
 ; ... and via a huge negative exponent (the reciprocal still runs ipow)
 (: r ((- 0 (100 2)) 2) (= 'capped (cap r)))
 ; growthless bases run free at the same exponent: +-1, 0, and any gem
 (= 1 ((100 2) 1)) (= 0 ((100 2) 0)) (= 1 ((100 2) -1)) (= 1.0 ((100 2) 1.0))
 (< 1048576 ((25 2) 2.0))      ; a gem base never scares -- it just rides to inf
 ; a deliberate big power over the old count cap fits the word budget: 2^(2^21)
 (< 0 ((21 2) 2))
 ; at or below the cap nothing changes
 (= 1024 (10 2)) (= 13 ((3 (+ 1)) 10)) (= 0.5 (-1 2)))

; runtime tunable: drop the cap to 4 -- compose 5 scares; power scares only past
; the shrunken word budget (4 words = 256 bits): 2^5 fits, 2^300 does not.
(pin apcap 0 4)
(assert
 (= 14 ((4 (+ 1)) 10))
 (= 'capped (cap (5 (+ 1))))   ; the scare fires at compose time, mid-curry
 (= 16 (4 2))
 (= 32 (5 2))                  ; over the count, under the budget: runs
 (= 'capped (cap (300 2)))     ; ~300 result bits top 256
 (= 1 (300 1)))                ; growthless still free under a tiny box
(pin apcap 0 1048576)
; restore: a missing prior help reads 0 -- exactly the uninstall. (one nothing
; closed the old hole where a ()-saved help reinstalled as the unit.)
(hear apcap-t0)
(assert (= 1048576 (apcap 0)) (= 64 (6 2)) (= 12 ((2 (+ 1)) 10)))
