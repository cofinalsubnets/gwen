; --- test/host/gzc.l: the C twins of gz.l, held to the love they shadow ---
; NOT in the test/*.l corpus: `inflate` (host/inflate.c) and `crc32` (host/hash.c) are
; host nifs and lib/gz.l is a lib module, so neither is visible to the love0 bake.
;
; test/host/gz.l is where lib/gz.l's own laws live; this file asks the other question --
; does the C answer what love answers, on the same stream? gz-puff is the oracle
; (host/inflate.c says so in its head), so a disagreement is the C being wrong until
; proven otherwise.
;
;   SHAPES    the round trip over the five that break coders, plus a long dist-1 run and
;             a periodic one -- the overlapping copy, and the word move that is only
;             right from eight back.
;   HINTS     the size believed, absent, negative, too small and too large. the last
;             three are the retry, which counts the stream and allocates a second time.
;   TORN      every prefix of a good stream, both answering ().
;   FUZZ      a flipped bit anywhere in a good stream, and this is the one that matters:
;             gz-huff does not check that the lengths describe a code, so a corrupt block
;             decodes to NONSENSE in love rather than failing, and the C has to produce
;             the SAME nonsense. It is why inf_build fills first-writer-wins and zeroes
;             its symbol array -- drop either and the fuzz reddens within a few bits.
;   CRC       every length 0..129, which is the eight-at-a-time tail at every offset.
;   MISUSE    a wrong shape answers (), it does not decode.
;
; ⚠ `string?`, NEVER `?` OR `!`, to ask whether an answer is there: a decoded run of NULs
; sums to zero and reads as false, so a presence test spelled `(! r)` calls a correct
; answer a failure -- and only on inputs nobody hand-writes.
;
; run:  cat test/00-init.l test/host/gzc.l | out/host/love
(use 'gz)

(assert
 (lit? inflate)
 (lit? crc32))

(: (gzc-mk n f) (: b (cask n) (go i) (? (n <= i) (string b) (: _ (pin b i (f i)) (go (i + 1)))) (go 0))
   (gzc-l d h) (: r (gz-puff d h) (? (two? r) <>r 'nil))          ; the oracle
   (gzc-c d h) (: r (inflate d h) (? (string? r) r 'nil))         ; the C twin
   (gzc-same d h) (? (= (gzc-l d h) (gzc-c d h)) 1 0)
   (gzc-all f xs) (? (! (two? xs)) 1 (? (f <xs) (gzc-all f >xs) 0))

   gzc-empty ""
   gzc-one   "x"
   gzc-run   (gzc-mk 300 (\ i 65))                       ; dist 1, the overlapping copy
   gzc-long  (gzc-mk 100000 (\ i 65))                    ; ..and matches at the 258 ceiling
   gzc-per   (gzc-mk 40000 (\ i (65 + (i % 11))))        ; dist 11, three words at a time
   ; ⚠ EVERY SHORT PERIOD, 2 THROUGH 9, and the ones under eight are the whole reason:
   ; they are the distances at which a word move reads bytes it has not written yet, and
   ; a corpus of one period cannot say where that line falls. The C copies byte by byte
   ; today and this row still earns its place -- with only period 11 in the gate, a copy
   ; taking whole words from four back passed everything.
   gzc-near  (: (go j a) (? (9 < j) a (go (j + 1) (a + (gzc-mk 4000 (\ i (65 + (i % j)))))))
                (go 2 ""))
   gzc-text  ("the quick brown fox jumps over the lazy dog. "
              + "pack my box with five dozen liquor jugs. ")
   gzc-noise (gzc-mk 2000 (\ i (((i * 2654435761) >> 7) & 255)))   ; no matches: stored
   gzc-mixed (: (go k a) (? (k < 1) a (go (k - 1) (a + gzc-text + gzc-run + gzc-noise)))
                (go 8 ""))
   gzc-shapes `(gzc-empty gzc-one gzc-run gzc-long gzc-per gzc-near gzc-text gzc-noise
                gzc-mixed)

   ; a round trip, and the same stream read both ways with the size and without it
   (gzc-rt s) (: d (gz-deflate s)   n (tally s)
                 (? (! (gzc-same d n)) 0
                    (! (gzc-same d 0)) 0
                    (! (= (gzc-c d n) s)) 0
                    1))
   ; ..and with a size that lies, which is the retry
   (gzc-hint s) (: d (gz-deflate s)   n (tally s)
                   (? (! (gzc-same d (n + 100))) 0
                      (! (gzc-same d -3)) 0
                      (! (gzc-same d (? (n < 2) 1 (n - 1)))) 0
                      1))
   ; the door itself: `gz-inflate` reaches for the nif and must answer what puff answers
   (gzc-door s) (: d (gz-deflate s)   n (tally s)
                   r (gz-inflate d n)
                   (? (! (two? r)) 0 (= <>r s) 1 0)))

; --- the shapes ---
(assert
 (gzc-all gzc-rt gzc-shapes)
 (gzc-all gzc-hint gzc-shapes)
 (gzc-all gzc-door gzc-shapes))

; --- torn: every prefix of a good stream ---
(: gzc-td (gz-deflate gzc-mixed)
   (gzc-cut d i) (: c (cask i) _ (? i (pour c 0 d 0 i) 0) (string c))
   (gzc-torn d k step)
    (: (go i) (? (k <= i) 1 (! (gzc-same (gzc-cut d i) 400)) 0 (go (i + step)))
       (go 0)))
(assert (gzc-torn gzc-td (tally gzc-td) 7))

; --- fuzz: one flipped bit, over two streams and every bit of the smaller ---
(: (gzc-flip d k)
    (: n (tally d)   c (cask n)   _ (pour c 0 d 0 n)   o (// k 8)
       _ (pin c o ((peep c o 0) ^ (1 << (k % 8))))
       (string c))
   (gzc-fuzz s step)
    (: d (gz-deflate s)   nb (8 * (tally d))   n (tally s)
       (go k) (? (nb <= k) 1 (! (gzc-same (gzc-flip d k) n)) 0 (go (k + step)))
       (go 0))
   ; ⚠ a corrupt stream inflates to NONSENSE, and gz-puff walks that nonsense at love
   ; speed -- so the streams here are small on purpose. the shapes, not the sizes, are
   ; what the fuzz is for: a dynamic block (text), a stored one (noise), a fixed one (one).
   gzc-fz `(gzc-text gzc-run gzc-one))
(assert
 (gzc-all (\ s (gzc-fuzz s 1)) gzc-fz)
 (gzc-fuzz gzc-noise 29))

; --- crc32, the other twin in host/hash.c, against gz-crcwalk ---
; ⚠ EVERY LENGTH 0 THROUGH 129, because the C reads EIGHT BYTES AT A TIME where the walk
; reads one: the tail is the only place the two can part and it is a different tail at
; every length mod eight. The published check value is asserted off the C side, so a twin
; that agrees with a walk which has drifted still fails.
(: (gzc-crc s) (? (= (crc32 s) (gz-crcwalk s)) 1 0)
   (gzc-crclen k)
    (: (go i) (? (k < i) 1
                 (! (gzc-crc (gzc-mk i (\ j (((j * 2654435761) >> 11) & 255))))) 0
                 (go (i + 1)))
       (go 0)))
(assert
 (3421780262 = (crc32 "123456789"))              ; 0xcbf43926, what every crc32 publishes
 (gzc-crclen 129)
 (gzc-all gzc-crc gzc-shapes)
 ; ⚠ all-NUL and empty are the two answers a truth test would throw away: the crc of ""
 ; IS 0, and the crc of 300 NULs is not, and both have to come back as charms.
 (gzc-crc (gzc-mk 300 (\ i 0)))
 (0 = (crc32 ""))
 (charm? (crc32 (gzc-mk 300 (\ i 0)))))

; --- MISUSE: a value op, so a wrong shape answers () and decodes nothing ---
(assert
 (! (charm? (crc32 7)))
 (! (charm? (crc32 ())))
 (! (charm? (crc32 (cask 8))))
 (! (string? (inflate 7 10)))
 (! (string? (inflate () 10)))
 (! (string? (inflate (cask 8) 10)))
 (! (string? (inflate (iota 8) 10)))
 (! (string? (inflate gzc-td 'x)))
 (! (string? (inflate gzc-td 1.5)))
 ; ..and the empty stream is not a stream at all, at either lane
 (gzc-same "" 0))

(: _ (test_fin ())
   _ (puts "gzc: ok -- the C twins agree with love (shapes, hints, torn, fuzz, crc, misuse)
")
   0)
