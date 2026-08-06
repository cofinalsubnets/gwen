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

  ; and it is THE empty string: no 0-length string is ever allocated, so however one
  ; is arrived at -- snipped, concatenated, repeated zero times, coerced, drained --
  ; it is the same object, and id? holds where only = would be expected to.
  (id? "" (snip "abc" 1 1))   (id? "" (snip "" 0 5))
  (id? "" (+ "" ""))          (id? "" (* "ab" 0))
  (id? "" (string ()))        (id? "" (string ""))
  (id? "" (slurp (jug 0)))    (id? "" (slurp (tap ())))   (id? "" (slurp 42))

  ; get: indexing the empty string returns the default
  (= 99 (peep "" 0 99))
  (= 99 (peep 0 0 99))

  ; applying "" IS byte-indexing now (it is a real text object): every index is
  ; out of range, so ("" k) == 1 -- the same value the number 0 yields, but via
  ; the string ap ap, not Church-numeral application.
  (= 1 ("" 5))
  (= 1 (0 5))

  ; string: coercing "" stays the empty string, and so does coercing NOTHING --
  ; a point with no spelling answers the empty spelling, the unit and a bare mint
  ; alike, so `string` always lands a string. the charm zero is a BYTE, so it
  ; coerces like every other charm: a one-byte NUL string, which nets nothing
  ; without being nothing.
  (= "" (string ""))   (string? (string ""))
  (= "" (string ()))   (string? (string ()))
  (= "" (string (mint 0)))   (string? (string (mint 0)))
  (string? (string 0))   (1 = (tally (string 0)))   !(string 0)

  ; puts writes nothing for an empty operand; "" returns "", 0 returns 0
  (= "" (puts ""))
  (= 0 (puts 0)))
