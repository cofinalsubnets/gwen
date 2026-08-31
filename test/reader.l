; test/reader.l -- the reader's laws, in one file: the lexical half, what the reader
; emits for a given text, so there is one place to state it and one place to run a
; reimplementation against. the factorization half -- what opfix later does with a
; sigil -- stays in test/operator.l and test/infixop.l, being a compile-time law.
;
; every assert reads text and compares the emitted form structurally via `sound`, never
; by printing: the printer renders a mono wrap back in its surface spelling ((mono (< x))
; shows as `<x`) and would hide the distinction being asserted.
;
; this is the acceptance test for a reader rewritten in love (p0/p1): every law must
; survive that flip byte for byte, so a case deliberately allowed to change is edited
; here first, or the differential cannot tell the two apart.
(: (rdr s) (: r (sound s) (? (two? r) (cap r) r)))

; --- the read protocol ---------------------------------------------------------
; `sound` takes TEXT -- a string, or the charlist a previous read left -- and
; answers three ways: the datum consed onto what is left, () at a clean end, or
; `torn` where the text ran out inside a shape. the residue is the answer, so the
; caller owns the rest and a torn read resumes by handing back more text: no port
; to spend, nothing stashed anywhere.
; presence is the cons -- `two?` asks "did I get one", never the net, because a
; datum can perfectly well be a nothing (reading "()" answers (() . rest)).
; and the residue is a charlist whichever went in: the string is spread once at
; the door, so the two doors meet after the first datum and never diverge.
(assert
 (: r (sound '(49)) (&& (two? r) (1 = (cap r)) (nil? (cup r))))  ; a datum, nothing after
 (nil? (sound ()))                                    ; a clean end -- the blue floor
 (id? 'torn (sound '(40)))                            ; the text ran out inside a shape
 (: r (sound '(49 32 50))                             ; ...and the residue reads the next
    (&& (1 = (cap r)) (2 = (cap (sound (cup r))))))
 (: r (sound '(40 41)) (&& (two? r) (nil? (cap r)))))  ; the datum () is still a datum

; --- the string door -----------------------------------------------------------
; text is a string or a charlist and the reader spreads the first itself -- every
; caller had been writing that spread by hand. an empty string is a clean end, and
; a value with no text in it (a number, a name) has no datum in it either.
(assert
 (: r (sound "(1 2) 3")
    (&& ('(1 2) = (cap r)) (3 = (cap (sound (cup r))))))  ; ...and the residue resumes
 (: r (sound "1") (&& (two? r) (1 = (cap r))))
 (nil? (sound ""))                                    ; empty text: a clean end, not torn
 (id? 'torn (sound "(1"))
 (nil? (sound 42)) (nil? (sound 'notaport))           ; nothing to read is a clean end
 ((sound "(1 2) 3") = (sound (map "(1 2) 3" (jot 7)))))  ; the two doors agree, datum and rest

; --- the port door ---------------------------------------------------------------
; a port is the third door: sound lifts it to the flowing charlist it already is, so
; nobody slurps a file to read one form out of it. the datums are the law -- the residue
; cannot be, a port's rest ending in the thunk that drinks the next gulp where a string's
; ends in (). a tap is the strongest witness: the charlist row is unbuffered, so every
; byte arrives as its own gulp behind its own thunk. the `#!` lookahead forces one
; position three times and the \xnn lane twice, so a tail that drank instead of
; remembering would lose the byte between -- a misread, never a crash. the sweep below
; runs every reader lane through that sieve and demands the string's answer back.
(: (rdr-all x)                                      ; every datum of a text, in order
   (: (go cl acc) (: r (sound cl)
                     (? (two? r)      (go (cup r) (link (cap r) acc))
                        (id? r 'torn) 'torn
                        (rev acc)))
      (go x ()))
   (rdr-doors s) (: cl (map s (jot (tally s)))      ; string, charlist and port agree
                    d (rdr-all s)
                    (&& (d = (rdr-all cl)) (d = (rdr-all (tap cl))))))
(assert
 (: p (tap (map "(1 2) 3" (jot 7))) r (sound p)
    (&& ('(1 2) = (cap r)) (3 = (cap (sound (cup r))))))   ; a port, and its residue resumes
 (nil? (sound (tap ())))                            ; a spent port is a clean end
 (nil? (sound (jug 0)))                             ; ...and so is one with no read door
 (id? 'torn (sound (tap (map "(1" (jot 2)))))       ; torn reads torn off a port too
 ; the sieve: one byte per gulp, so every lane crosses a thunk at every position.
 (rdr-doors "(1 2) 3 \"hi\" 'q")
 (rdr-doors "#!/usr/bin/env love\n(a b)")           ; the lookahead: one position, 3 forces
 (rdr-doors "\"\\x41\\x42\" 7")                     ; the \xnn lane: 2 forces
 (rdr-doors "; comment\n(a . b) {1 2} @(3) (0 ~ 1)")
 (rdr-doors "map (+ 1) '(1 2 3)")                   ; an operator run, glued and spaced
 (rdr-doors "1.5e3 -2 0xff 1/2 [a b] {c d}"))

; --- the lexer law -------------------------------------------------------------
; the leading character splits the token. a name token (alnum/_) keeps - ? ! and
; the prime inside it (the kebab law); a punctuation-led token is a sigil -- a
; maximal run of operator chars. the kebab law is about the inside of a
; name-led token: it never governed a token that starts with punctuation.
(assert
 ('old-thing = (rdr "old-thing"))                     ; - inside a name
 ('nl->sp = (rdr "nl->sp"))                           ; ...and -> inside one too
 ('x? = (rdr "x?")) ('x! = (rdr "x!"))
 ("x'" = (string (rdr "x'")))                         ; the prime: x' is a name
 ('<= = (rdr "<= ")))                                 ; a spaced run is a plain symbol

; --- comments ------------------------------------------------------------------
; `;` runs to end of line; `#!` is a shebang line. there are no block comments.
(assert
 (7 = (rdr "; nope
7"))
 (7 = (rdr "#! nope ( ( \"
7")))

; --- strings -------------------------------------------------------------------
; "..." is the only string, with \ escapes. an unterminated one is `more`.
(assert
 ("hi" = (rdr "\"hi\""))
 ("a\"b" = (rdr "\"a\\\"b\""))
 ("" = (rdr "\"\""))
 (id? 'torn (sound "\"oops")))

; --- numbers -------------------------------------------------------------------
; an integer literal reads its value -- in all three bases, at full precision, and the
; same in every build (host, love0, wasm, the kernel), whatever that build's word width
; or libc. all three bases share one reader (is_dec_int/is_hex_int/is_oct_int ->
; ai_big_read_dec/_hex/_oct), and the predicates are exhaustive over what a base-0
; strtol accepted whole, so the reader never calls it. a float goes by am_strtod, which
; is ours and correctly rounded on every target.
(assert
 (3 = (rdr "3")) (-3 = (rdr "-3")) (3 = (rdr "+3"))
 (16 = (rdr "0x10")) (255 = (rdr "0xff")) (255 = (rdr "0XFF"))  ; hex, either case
 (-255 = (rdr "-0xff")) (255 = (rdr "+0xff"))
 (493 = (rdr "0755")) (7 = (rdr "007")) (0 = (rdr "00"))        ; octal, the leading zero its whole prefix
 ((0 - (1 / 2)) = (rdr "-.5")) ((1 / 2) = (rdr "+.5"))
 (18446744073709551615 = (rdr "18446744073709551615"))          ; decimal above the word: a big
 ; ..and the same value in the other two bases, in every build
 (18446744073709551615 = (rdr "0xffffffffffffffff"))
 (18446744073709551615 = (rdr "01777777777777777777777"))
 (18446744071564165120 = (rdr "0xffffffff80200000"))            ; a high-half address
 (9223372036854775807 = (rdr "0x7fffffffffffffff"))             ; the old strtol ceiling
 (340282366920938463463374607431768211455 = (rdr "0xffffffffffffffffffffffffffffffff"))
 (73786976294838206463 = (rdr "007777777777777777777777"))      ; 2^66-1, octal
 ; a prefix with no digits is no number: an honest symbol, free for binding.
 ; and 08 is neither octal (8 is not a digit there) nor decimal (the leading
 ; zero is a prefix) -- it falls through to the float lane, as it always has.
 ('0x = (rdr "0x")) ('0xg = (rdr "0xg")) ('0b101 = (rdr "0b101"))
 (8.0 = (rdr "08")))

; --- quote and the constructor wraps -------------------------------------------
; a sigil glued to its datum builds it: ' quote, @ tuple. those two are the
; exhaustive set -- the list wears the bracket […] and the map the brace {…},
; and the twin is the infix `~`. a list operand splices into the call rather than
; being wrapped, and the empty collections go to a direct one-arg nif.
(assert
 ('(\ x) = (rdr "'x"))
 ('(hash a b) = (rdr "{a b}"))
 ('(tuple 1 2) = (rdr "@(1 2)"))
 ('(0 ~ 1) = (rdr "(0 ~ 1)"))                         ; ~ spaced is a plain operator symbol:
 ('(mono (~ x)) = (rdr "~x"))                         ; the twin is opfix's, not the reader's.
                                                      ; ~ glued is conj (the valence law)
 ; the empty collections go direct to a one-arg nif (a zero-arg macro is not
 ; expanded by the self-hosted feel on love0). every reader emission is ()-tailed,
 ; these two included, so they compare whole against a source-quoted form.
 ('(tablet 0) = (rdr "{}"))
 ('(iota 0) = (rdr "@()"))
 (() = (cup (cup (rdr "{}"))))
 (() = (cup (cup (rdr "<x")))))

; --- the valence law, reader half ----------------------------------------------
; glued is monadic, spaced is dyadic -- everywhere, head included. a run glued to
; the next datum emits (mono (run datum)); spaced it emits the plain symbol and
; opfix decides later. only : and ? are suppressed at head (they take the whole
; list); \ never fuses.
(assert
 ('(mono (< x)) = (rdr "<x"))
 ('(f (mono (< x))) = (rdr "(f <x)"))
 ('(mono (<> b)) = (rdr "<>b"))
 ('(< x) = (rdr "(< x)"))                             ; spaced: no wrap
 ('(: a 1) = (rdr "(: a 1)"))                         ; : at head is not fused
 ('(mono (! -5)) = (rdr "!-5")))                      ; the shed: ! then -5

; --- + and - are operator runs too -------------------------------------------
; `+` and `-` are runs like every other punctuation; the one exception is that a numeral
; has to start somewhere, so a digit (or a `.`, for -.5) right after them makes a number.
; so --5 is -(-5) and ++5 is +(+5), symmetric with !!5, and -x is negate x, symmetric
; with !x. the two doubled forms arrive by different paths -- --5 sheds its trailing -
; before the digit and reads (mono (- -5)), while ++5 has no shed and reads (mono (++ 5))
; for opfix to split -- so both are asserted, or a later change breaks one lane silently.
(assert
 ('(mono (- -5)) = (rdr "--5"))       (5 = --5)
 ('(mono (++ 5)) = (rdr "++5"))       (5 = ++5)
 ('(mono (-- -5)) = (rdr "---5"))     (-5 = ---5)
 ('(mono (-+ 5)) = (rdr "-+5"))       (-5 = -+5)
 ('(mono (- x)) = (rdr "-x"))
 ('(mono (+ x)) = (rdr "+x"))
 ; ...and the numeral exception, which is the whole reason for the carve-out
 (-3 = (rdr "-3")) (3 = (rdr "+3"))
 ('- = (rdr "- x"))                                   ; a lone - is the symbol
 ('(mono (- (f x))) = (rdr "-(f x)"))                 ; glued to a form: negate
 ('(mono (+ (tuple 3 4))) = (rdr "+@(3 4)")))         ; ...or to a constructor

; --- delimiters and the zero point ---------------------------------------------
; the opener carries the wrap: [..] is the list and {..} the hash -- each is its
; wrap's only spelling, so the empties follow the splice law ([] -> (list),
; {} -> (tablet 0)).
; the three closers are one: ) ] } each only end the open form, so a crossed pair
; reads the opener's wrap in silence -- matching them is libra's, not the reader's.
; the empty form is the zero point, not the number 0.
(assert
 ('(list a b) = (rdr "[a b]"))          ('(hash a b) = (rdr "{a b}"))
 ('(list) = (rdr "[]"))                 ('(tablet 0) = (rdr "{}"))
 ('(list (list a b) (list c d)) = (rdr "[[a b] [c d]]"))   ; the nest is the whole point
 ((rdr "-(list 1 2)") = (rdr "-[1 2]"))               ; and it glues like a form does
 ('(list a b) = (rdr "[a b)")) ('(a b) = (rdr "(a b]"))    ; a crossed pair, silent
 (() = (rdr "()")) !(0 = (rdr "()"))
 (', = (rdr ",")))                                    ; the comma is its own datum

; --- a constructor sigil over an open form renames the wrap ---------------------
; @ -- the one splicing wrap that still wears a sigil -- names the constructor the
; opener would otherwise have picked, so @[1 2] is @(1 2) and any delimiter serves
; any wrap. stacking is what it cannot mean: @[a b] would splice the bracket's own
; list in and read (tuple list a b), a wrong VALUE with nothing to raise. ' and \
; are not constructors -- they take the next datum whatever it is, so '[a b] is the
; form (list a b) quoted, and a sigil over a sigil still stacks ('@(1 2) is unchanged).
; @ reads glue, and it alone: spaced it is the plain symbol -- the head of the
; match form, which is why a tray never wears a space.
(assert
 ((rdr "@(1 2 3)") = (rdr "@[1 2 3]")) ((rdr "@()") = (rdr "@[]"))
 ((rdr "@(a b)") = (rdr "@{a b}"))
 ((rdr "@(1 2)") = (rdr "@[1 2]"))
 ('@ = (rdr "@ [1 2]")) ('(@ x (list _a b) b) = (rdr "(@ x [_a b] b)"))  ; @ reads glue
 ('(\ (list a b)) = (rdr "'[a b]"))                   ; ' is not a constructor: it quotes the form
 ('(\ (tuple 1 2)) = (rdr "'@(1 2)"))                 ; sigil over sigil still stacks
 ('torn = (rdr "@[")))
