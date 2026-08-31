; (show x) -> string. Heap-allocates a data-sink port, prints x through
; gfputx, returns the harvested bytes. Should match what (dot x) writes to stdout.

(assert
  ; numbers
  (= "42"   (show 42))
  (= "-7"   (show -7))
  (= "0"    (show 0))

  ; "" is a distinct empty string, printed as a chain of quotes
  (= "\"\""           (show ""))
  ; strings: printed with surrounding quotes; specials re-escaped
  (= "\"hi\""         (show "hi"))
  (= "\"a\\nb\""      (show "a\nb"))
  (= "\"a\\tb\""      (show "a\tb"))
  (= "\"a\\eb\""      (show "a\eb"))   ; \e is ESC, the terminal's own letter
  (= "\e" "\x1b")                      ; ... and just byte 27
  (= "\"\\\\\""       (show "\\"))
  (= "\"\\\"\""       (show "\""))

  ; interned symbols print as their name
  (= "foo" (show 'foo))
  ; a mint prints (mint <serial>) -- the serial in decimal, the mint's identity. not an
  ; exact round-trip: (mint <serial>) re-reads to a fresh mint (mint ignores its arg),
  ; since identity is the mint's whole product and no spelling carries it -- the face only
  ; has to distinguish, which the serial does. the serial is non-deterministic, so we
  ; assert the shape: a leading "(mint ", stable, distinct.
  (= "(mint " (snip (show (mint 0)) 0 6))
  (: m (mint 0) ((show m) = (show m)))             ; stable: same mint, same face
  (: a (mint 0) b (mint 0) !((show a) = (show b))) ; distinct mints, distinct faces
  (nom? (mint 0))
  (= "(mint " (snip (show (mint 'bar)) 0 6))       ; (mint 'bar) ignores its arg -- still a bare mint

  ; quote sugar: a chain (` x) prints as 'x
  (= "'foo"      (show ''foo))
  (= "'(a b)"    (show ''(a b)))

  ; proper lists
  (= "(a b c)"       (show '(a b c)))
  (= "(1 2 3)"       (show (link 1 (link 2 (link 3 0)))))
  (= "(\"a\" \"b\")" (show (link "a" (link "b" 0))))

  ; nesting
  (= "(a (b) c)"     (show (link 'a (link (link 'b 0) (link 'c 0)))))
  (= "((1 2) (3 4))" (show '((1 2) (3 4))))

  ; result is always a string
  (string? (show 0))
  (string? (show "hi"))
  (string? (show '(1 2 3)))
  (string? (show (tablet 0)))

  ; buffer growth: a 200-element list far exceeds the 32-byte initial cask,
  ; forcing several doublings inside to_putc.
  (: (rng n) (? (= n 0) 0 (link n (rng (- n 1))))
     s (show (rng 200))
     ; 200 numbers + 199 separators + 2 parens = at least 400 chars
   (do (string? s) (< 400 (tally s))))

  ; GC stress: 500 inspects in a loop. Each call alloc'd a heap port + cask,
  ; provoking many GCs. Final call must still produce the expected output.
  (: (loop n) (? (= n 0) (show (link 1 (link 2 (link 3 0))))
                 (do (show "stress") (loop (- n 1))))
   (= "(1 2 3)" (loop 500))))
