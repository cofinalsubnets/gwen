; cask (Phase 3): a mutable flat byte string. One constructor (cask); pin /
; get / put fold into the generic ops (get/put dispatch on the value's kind);
; pour blits a byte range from a string or cask into a cask, and snip reads one
; back out as a string -- both take a cask where they take a string. Bytes are data, so
; a stored byte reads back as its unsigned value 0..255. Equality is by
; identity -- a cask is mutable, so two distinct bufs never compare =.

(assert
 ; --- construction + length, and what a cask is NOT ---
 (= 4 (tally (cask 4)))   ; the COUNT is tally now
 (= 0 (sat (cask 4)))     ; a zeroed cask NETS nothing: hot chars measure by content
 (= 0 (tally (cask 0)))
 !(string? (cask 4))                          ; a cask is not a string
 !(charm? (cask 4))                          ; ...nor a number

 ; --- a fresh cask is zeroed ---
 (: b (cask 4) (= 0 (+ (peep b 0 0) (peep b 1 0) (peep b 2 0) (peep b 3 0))))

 ; --- put stores a byte, get reads it back; put returns the cask ---
 (: b (cask 3) _ (pin b 0 65) _ (pin b 1 66) _ (pin b 2 67)
    (&& (= 65 (peep b 0 0)) (= 66 (peep b 1 0)) (= 67 (peep b 2 0))))
 (: b (cask 1) (= b (pin b 0 9)))             ; put returns the cask

 ; --- bytes are unsigned 0..255; a store keeps only the low byte ---
 (: b (cask 1) _ (pin b 0 255) (= 255 (peep b 0 0)))   ; 0xff reads unsigned
 (: b (cask 1) _ (pin b 0 257) (= 1 (peep b 0 0)))     ; 257 & 255 = 1

 ; --- out-of-range / misuse is a silent no-op (no crash) ---
 (: b (cask 2) _ (pin b 5 1) _ (pin b -1 1) (= 0 (+ (peep b 0 0) (peep b 1 0))))
 (= 0 (peep (cask 2) 9 0))                     ; out-of-range get -> default

 ; --- pour from a string source ---
 (: b (cask 4) _ (pour b 0 "ABCD" 0 4)
    (&& (= 65 (peep b 0 0)) (= 68 (peep b 3 0))))
 (: b (cask 4) _ (pour b 1 "ABCD" 2 2)       ; copy "CD" to offset 1
    (&& (= 0 (peep b 0 0)) (= 67 (peep b 1 0)) (= 68 (peep b 2 0))))
 (: b (cask 2) (= b (pour b 0 "XY" 0 2)))    ; pour returns dst

 ; --- pour cask -> cask ---
 (: s (cask 3) d (cask 3)
    _ (pin s 0 7) _ (pin s 1 8) _ (pin s 2 9)
    _ (pour d 0 s 0 3)
    (&& (= 7 (peep d 0 0)) (= 9 (peep d 2 0))))

 ; --- pour clamps an oversized count rather than trampling the heap ---
 (: b (cask 2) _ (pour b 0 "ABCD" 0 100) (= 2 (tally b)))

 ; --- snip reads a cask, the way pour does: the prefix a read filled, copied out ---
 (: b (cask 4) _ (pour b 0 "ABCD" 0 4) (= "AB" (snip b 0 2)))
 (: b (cask 4) _ (pour b 0 "ABCD" 0 4) (string? (snip b 0 2)))   ; a STRING, not a cask
 (: b (cask 4) _ (pour b 0 "ABCD" 0 4) (= "ABCD" (snip b -9 99)))  ; clamps like a string
 (: b (cask 4) (id? "" (snip b 2 2)))         ; an empty range is THE empty string
 ; the snapshot is taken, not aliased: a later pin does not reach it
 (: b (cask 2) _ (pour b 0 "AB" 0 2) s (snip b 0 2) _ (pin b 0 90) (= "AB" s))

 ; --- equality is by identity (a cask is mutable) ---
 (: b (cask 2) (= b b))                       ; a cask equals itself
 !(= (cask 2) (cask 2)))                 ; distinct bufs never compare =
