; newtypes -- a DIE strikes COINS: user-defined kinds that ride the generic +/*.
; a coin is a typed hot (die + payload) reading KHot, so every +/* combination
; routes to the KHot lane where the coin is intercepted and the die's plus/times
; run over the loaded payloads. () stays the +/* identity for free (the mint hoist
; sits above the matrix); net/=/</show default over the payload in the C core.
; `coin`/`load`/`coin?`/`die-of` are the raw primitives; `ring`/`monoid` the builders.

(: ; Z/5Z -- a ring: add and multiply the residues mod 5
   z5 (ring 'z5 (\ x y ((x + y) % 5)) (\ x y ((x * y) % 5)))
   a  (z5 3)
   b  (z5 4)
   ; a SUM monoid -- a plus but no times (a monoid has no product)
   sm (monoid 'sm (\ x y (x + y)))
   smp (sm 2)
   smq (sm 3)
   ; a COUNTED kind -- net-mode 1 (slot 6): nets by tally, not by content
   cntd #(0 'cnt 6 1)
   (cnt p) (coin cntd p)
   (assert
    ; --- what a coin IS ---
    (coin? a)                      ; a is a coin
    !(coin? 5)               ; a plain value is not
    !(coin? "s")
    (3 = load a)                 ; load reads the payload
    (7 = load 7)                 ; load of a non-coin is itself
    (id? (die-of a) (die-of (z5 3)))   ; every z5 coin shares one die
    (nil? (die-of 5))              ; a non-coin has no die

    ; --- value equality: same die + eqv payloads ---
    (a = z5 3)                   ; by value, not identity
    (nil? (a = z5 4))            ; different payload
    (nil? (a = sm 3))            ; different die (even with equal payload)
    (id? a a)                      ; id? stays identity-sharp
    (nil? (id? a (z5 3)))          ; ... distinct objects

    ; --- + and * run the die's methods over the payloads ---
    (z5 2 = a + b)             ; (3+4) mod 5 = 2
    (z5 2 = a * b)             ; (3*4) mod 5 = 2
    (z5 0 = z5 2 + z5 3)   ; wraps to 0

    ; --- the () laws, inherited FREE (one per face) ---
    (a = () + a)                 ; () is +'s identity
    (a = a + ())                 ; ... either side
    (() = () * a)                ; ... and *'s ZERO: it annihilates
    (() = a * ())

    ; --- a monoid has a +, but no * ---
    (sm 5 = smp + smq)         ; 2 + 3 = 5
    !(smp * smq)             ; no times slot -> nil, like an undefined matrix cell

    ; --- net defaults over the payload: $, truth, sort ---
    !$(z5 0)            ; payload 0 nets nothing -> false
    (3 = $a)                  ; payload 3
    (5 = $(sm 5))
    !(z5 0)                  ; a zero-payload coin is false
    !!a                ; a positive-payload coin is true
    (`((z5 1) (z5 2) (z5 3)) = (sort `((z5 3) (z5 1) (z5 2))))  ; ordered by payload

    ; --- show: (name payload), reparsable when the name is a bound constructor ---
    ("(z5 3)" = show a)
    ("(sm 5)" = show (sm 5))

    ; --- the measure law: net is additive through a net-preserving monoid (sm: plus = +) ---
    ($(sm 3 + sm 4) = $($(sm 3) + $(sm 4)))   ; $(a+b) = $($a + $b) = 7
    (6 = +`((sm 1) (sm 2) (sm 3)))   ; net flows through a list of coins: 1+2+3
    (6 = $`((sm 1) (sm 2) (sm 3)))   ; ... and $ saturates the same sum

    ; --- two DISTINCT newtypes have no canonical +/* -> nil (like string*string) ---
    !(a + sm 3)            ; z5 coin + sm coin
    !(a * sm 3)
    (sm 5 = sm 2 + sm 3)   ; ... but same-die still combines

    ; --- tally delegates to the payload ---
    (3 = (tally (sm `(7 8 9))))   ; a coin over a 3-list tallies its payload

    ; --- net-mode 1 (slot 6): net by TALLY -- truth reads "has any", never "sums green" ---
    (2 = $(cnt `(1 -5)))     ; content sums -4; the COUNT is 2
    !!(cnt `(-3))     ; red content, still true: one element is SOMETHING
    !(cnt ())                ; an empty payload counts 0 -> nothing
    (0 = $(cnt ()))
    (3 = +`((cnt `(1)) (cnt `(1 1))))   ; counts flow through the list sum: 1+2
    (-4 = +(sm `(1 -5)))     ; mode absent: the payload hom, signed, as ever

    ; --- rationals (q): a multi-field coin; +/*/=/show all work. (a coin nets its
    ;     payload, so q's truth/sort go by representation, not value -- not asserted.) ---
    (q 5 6 = q 1 2 + q 1 3)    ; 1/2 + 1/3 = 5/6
    (q 1 2 = q 2 3 * q 3 4)    ; 2/3 * 3/4 = 1/2
    (q 1 2 = q 2 4)                ; reduced on construction
    (q 1 2 = q `(1 2))            ; the 2-list form (what show reads back)
    ; show is reduced, and REPARSABLE -- which wants the payload quoted: a bare (1 2)
    ; in a form is an application, so the old (q (1 2)) read back as q applied to 2.
    ("(q '(1 2))" = show (q 2 4))
    (q 1 2 = ev (cap (sound (show (q 2 4)))))   ; ..and that is the whole claim, checked

    ; --- `-` rides slot 5: q pins it (cross-multiply, reduce); the unit still rides ---
    (q 1 6 = q 1 2 - q 1 3)    ; 1/2 - 1/3 = 1/6
    (q -1 1 = q 1 2 - q 3 2)   ; goes red exactly
    (q 1 2 = q 3 2 - 1)          ; an integer rides cap/mint, either side
    (q 1 2 = 1 - q 1 2)
    (nil? (q 1 2 - ()))          ; () is absent to -, so there is nothing to compute
    (nil? (() - q 1 2))          ; ... either side
    !(a - z5 1)                ; a die without slot 5 -> nil, like a monoid's *
    !(a - sm 3)                ; distinct dies never combine

    ; --- `/` rides slot 8: q pins it (invert and multiply); zero has no reciprocal ---
    (q 3 2 = q 1 2 / q 1 3)    ; (1/2) / (1/3) = 3/2
    (q 1 4 = q 1 2 / 2)          ; an integer rides cap/mint, either side
    (q 2 1 = 1 / q 1 2)          ; ... so 1 / (1/2) is the reciprocal, exact
    (q -3 2 = q 1 2 / q -1 3)  ; the sign lands on the numerator, reduced
    !(q 1 2 / q 0 1)         ; by zero -> (): Q holds no inf
    !(q 1 2 / 0)               ; ... the bare zero too
    (nil? (q 1 2 / ()))          ; ... and absent to / as well
    (nil? (() / q 1 2))          ; ... either side
    !(a / z5 2)                ; a die without slot 8 -> nil
    !(a / sm 3)                ; distinct dies never combine

    ; --- a method OWNS its domain: off it (no star / (n d)-of-stars face), () ---
    !(q 1 2 + `((q 1 1) (q 0 1)))   ; a chain of coins is not an (n d)
    !(q 1 2 * `('x))                ; nor a chain of symbols
    !(q 1 2 - "s")                   ; nor text
    (q 1 1 = q 1 2 + `(1 2))          ; ... but a bare (n d) of stars reads as n/d

    ; --- an OPEN star (slot 7): q declares its coins numeric, so a numeral
    ;     POWERS them through their own * (exact squaring) instead of composing ---
    (q 1 4 = 2 (q 1 2))            ; (n x) = x**n, through the die
    (q 8 27 = 3 (q 2 3))
    (q 1 4 = q 1 2 ** 2)         ; the apply operator rides the same law
    (q 1 2 = 1 (q 1 2))            ; the identity numeral
    (1 = 0 (q 5 7))                  ; the zero numeral: x**0 = 1
    (q 1 2 = -1 (q 2 1))           ; a reciprocal power rides coin / (slot 8)
    (q 9 4 = -2 (q 2 3))           ; (2/3)**-2 = 9/4: ipow, then invert
    (q 9 4 = q 2 3 ** -2)        ; the apply operator rides the same law
    !(-1 (q 0 1))                ; zero has no reciprocal power
    !((1 / 2) (q 4 1))           ; a non-whole exponent off the tower: ()
    !(coin? (2 (sm 3)))          ; an UNflagged coin still composes (a closure)

    ; --- net-mode 2 (RATIO): a rational's TRUTH is its VALUE's sign, exactly ---
    !(q -1 2)                    ; a negative rational IS nothing: red by value
    !!(q 1 2)              ; a positive one is something
    !(q 0 5)                     ; zero is the blue floor
    (1 = $(q 1 2))                ; $ ceils the magnitude: 1/2 -> 1
    (2 = $(q 3 2))                ; ceil 3/2
    (0 = $(q -7 2))               ; ... and clamps red to the floor
    !!(q 1 2 - q 1 3)     ; the subtract-and-test sign idiom HOLDS: 1/2 - 1/3 is green
    !(q 1 3 - q 1 2)            ; ... and 1/3 - 1/2 is red: nothing
    !!(q 1 (1000 10))      ; a 10^1000 denominator underflows the double --
    !(q -1 (1000 10))            ; ... the sign survives it, both ways

    ; --- the order half: a ratio coin seats IN the number band, by VALUE ---
    (q 1 3 < q 1 2)                ; value order, not payload order
    !(q 1 2 < q 1 3)
    !(q 1 2 < q 2 4)         ; equal values tie (either side)
    !(q 2 4 < q 1 2)
    (q 1 2 < 1)                      ; ... interleaved with the closed tower
    (0 < q 1 2)
    (q -1 2 < 0)
    (1 < q 3 2)
    (q 3 2 >= 1)                     ; the reversed faces ride the same lane
    (q 1 2 <= q 2 4)
    (1 < q 9007199254740993 9007199254740992)   ; 1 + 2^-53: the double quotient TIES at 1.0;
                                       ; the exact cross-multiply tier does not
    (`((q -1 2) (q 1 3) (q 1 2) 1 (q 3 2) 2) =
     (sort `(2 (q 1 2) 1 (q 3 2) (q 1 3) (q -1 2))))   ; sort interleaves by value
    (q 9 1 < cnt `(1))            ; a NON-ratio coin keeps the coin band, above the numbers

    ; --- rung 2: a declared star SELF-NETS, so the vocabulary follows the flag ---
    (q 1 2 = +(q 1 2))          ; net answers the coin ITSELF (the fixpoint, by declaration)
    (star? (q 1 2))                    ; ... so star? holds with no coin branch of its own
    !(star? a)                   ; an undeclared coin still nets its payload: no star
    !(star? (sm 3))
    (q 5 6 = +`((q 1 2) (q 1 3)))   ; additivity lands IN the ring: an exact rational sum

    ; --- a coin's lit?-ness is its die's call: fresh DATA by default ---
    !(lit? (q 1 2))          ; a rational is DATA -- a value, not a reference (like a number)
    !(lit? a)                ; a mod-n residue too
    !(lit? (sm 5))
    !(lit? (coin #(0 'd) 5)) ; a plain die -> data
    (lit? (coin #(0 'h 4 1) 5))    ; ... but a die with DieHot (slot 4) strikes lit coins
    ))
