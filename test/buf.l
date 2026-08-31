; a cask is a mutable flat byte array -- the one collection whose pin lands in place, so a
; second holder sees the write. its surface laws (tally, $, the byte pin, the charlist ctor,
; identity equality) are spec.l's; what lives here are the doors that take a range.
; `pour` blits a byte run between casks and strings -- a dst offset, a src offset, a count --
; and answers dst; an oversized count clamps to the room there is rather than trampling the
; heap. `snip` reads a range back out as a string, clamping its bounds the way a string snip
; does, and it snapshots: the bytes are copied, so a later pin cannot reach what was read.
; out of range, either door is a silent no-op.
(assert
 ; --- a cask is its own kind ---
 !(string? (cask 4)) !(charm? (cask 4))

 ; --- an out-of-range pin or peep is a silent no-op, never a crash ---
 (: b (cask 2) _ (pin b 5 1) _ (pin b -1 1) (= 0 (+ (peep b 0 0) (peep b 1 0))))
 (= 0 (peep (cask 2) 9 0))

 ; --- pour: dst offset, src offset, count -- and it answers dst ---
 (: b (cask 4) _ (pour b 1 "ABCD" 2 2)       ; copy "cd" to offset 1
    (&& (= 0 (peep b 0 0)) (= 67 (peep b 1 0)) (= 68 (peep b 2 0))))
 (: b (cask 2) (= b (pour b 0 "XY" 0 2)))
 ; ... a cask is a source as readily as a sink
 (: s (cask 3) d (cask 3)
    _ (pin s 0 7) _ (pin s 1 8) _ (pin s 2 9)
    _ (pour d 0 s 0 3)
    (&& (= 7 (peep d 0 0)) (= 9 (peep d 2 0))))
 ; ... and an oversized count clamps
 (: b (cask 2) _ (pour b 0 "ABCD" 0 100) (= 2 (tally b)))

 ; --- snip reads a range back out as a string ---
 (: b (cask 4) _ (pour b 0 "ABCD" 0 4) (= "AB" (snip b 0 2)))
 (: b (cask 4) _ (pour b 0 "ABCD" 0 4) (string? (snip b 0 2)))
 (: b (cask 4) _ (pour b 0 "ABCD" 0 4) (= "ABCD" (snip b -9 99)))   ; clamps like a string snip
 (: b (cask 4) (id? "" (snip b 2 2)))                               ; an empty range is the empty string
 ; ... and the snapshot is taken, not aliased
 (: b (cask 2) _ (pour b 0 "AB" 0 2) s (snip b 0 2) _ (pin b 0 90) (= "AB" s))

 ; --- a cask equals itself; law.l's =-total corpus carries no cask, so it is said here ---
 (: b (cask 2) (= b b)))
