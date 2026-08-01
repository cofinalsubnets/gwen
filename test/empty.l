; "" is a distinct empty-string object -- the identity element for +
; over strings. It is falsy (an empty container), but NOT the number 0: 0 (nil) is
; still tolerated as an "empty" argument by the lenient string ops, and "" prints
; and compares as a string, yet the two are distinguishable objects.
(assert
  ; the literal "" reads as a real string object, distinct from 0 ...
  (string? "")                                  ; "" is a string
  (nil? "")                                  ; ... but falsy now -- an empty container
  !(id? "" 0)                               ; truthiness aside, "" is its own object, not 0/nil
  (nil? (= "" 0))                            ; "" is not equal to 0
  (nil? (id? "" 0))                         ; ... nor identical to it
  (= "" "")                                  ; but all empty strings are equal

  ; pin: both the empty string and 0 have empty length
  (= 0 (sat ""))
  (= 0 (sat 0))

  ; +: "" is a two-sided identity, and joining empties yields ""
  (= "ab" (+ "" "ab"))   (= "ab" (+ "ab" ""))
  (= "ab" (+ "ab" ""))   (= "ab" (+ "" "ab"))
  (= "" (+ "" ""))                        ; empty join -> the empty string
  (string? (+ "" ""))                        ; ... still a string, not 0
  ; scat is gone; + takes the byte law instead: 0 is byte 0, not an empty operand
  (= 3 (tally (+ "ab" 0)))  (= 0 ((+ "ab" 0) 2))
  (= 0 (+ 0 0))

  ; snip: an empty string snip returns "" -- a string snip yields a STRING, the
  ; closest form of nothing for this kind, not the bare floor (0). only a non-string
  ; input (no string witness to preserve) gives 0.
  (= "" (snip "abc" 1 1))   (string? (snip "abc" 1 1))
  (= "" (snip "" 0 5))
  (= 0 (snip 0 0 5))

  ; get: indexing the empty string returns the default
  (= 99 (peep "" 0 99))
  (= 99 (peep 0 0 99))

  ; applying "" IS byte-indexing now (it is a real text object): every index is
  ; out of range, so ("" k) == 1 -- the same value the number 0 yields, but via
  ; the string ap ap, not Church-numeral application.
  (= 1 ("" 5))
  (= 1 (0 5))

  ; string: coercing "" stays the empty string, and coercing NOTHING -- the unit
  ; -- stays nothing. the charm zero is a BYTE, so it coerces like every other
  ; charm: a one-byte NUL string, which nets nothing without being nothing.
  (= "" (string ""))   (string? (string ""))
  (id? () (string ()))
  (string? (string 0))   (1 = (tally (string 0)))   !(string 0)

  ; puts writes nothing for an empty operand; "" returns "", 0 returns 0
  (= "" (puts ""))
  (= 0 (puts 0)))
