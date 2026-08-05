; test/reader.l -- THE READER'S LAWS, in one file.
;
; the reader is where source becomes data, and its laws were scattered: prose in
; test/spec.l's reader-operators section, a line in README.md, worked cases in
; test/{operator,infixop,valence,escape,comment,data,bignum,roundtrip}.l. this
; file gathers the LEXICAL half -- what the reader emits for a given text -- so
; there is one place to state it and one place to run a reimplementation against.
; (the FACTORIZATION half, what opfix later does with a sigil, stays in
; test/operator.l and test/infixop.l: that is a compile-time law, not a read one.)
;
; every assert here reads TEXT and compares the emitted FORM structurally, via
; `sound` -- never by printing, because the printer renders a mono wrap back in
; its surface spelling ((mono (< x)) shows as `<x`) and would hide the very
; distinction being asserted.
;
; ⚠ this file is the ACCEPTANCE TEST for a reader rewritten in love (doc/io.md
; -- p0/p1). every law it states must survive that flip byte for byte; a case that
; is deliberately allowed to change is a case that must be edited HERE first,
; before the reimplementation, or the differential cannot tell the two apart.
(: (rdr-cl s) (map s (jot (tally s)))
   (rdr s) (: r (sound (rdr-cl s)) (? (two? r) (cap r) r)))

; --- the read protocol ---------------------------------------------------------
; `sound` takes TEXT -- a charlist -- and answers THREE ways: the datum CONSED
; ONTO WHAT IS LEFT, () at a clean end, or `torn` where the text ran out inside a
; shape. the residue IS the answer, so the caller owns the rest and a torn read
; RESUMES by handing back more text: no port to spend, nothing stashed anywhere.
; presence is the cons -- `two?` asks "did I get one", never the net, because a
; datum can perfectly well be a nothing (reading "()" answers (() . rest)).
(assert
 (: r (sound '(49)) (&& (two? r) (1 = (cap r)) (nil? (cup r))))  ; a datum, nothing after
 (nil? (sound ()))                                    ; a clean end -- the blue floor
 (id? torn (sound '(40)))                             ; the text ran out inside a shape
 (: r (sound '(49 32 50))                             ; ...and the residue reads the next
    (&& (1 = (cap r)) (2 = (cap (sound (cup r))))))
 (: r (sound '(40 41)) (&& (two? r) (nil? (cap r)))))  ; the datum () is still a datum

; --- the lexer law -------------------------------------------------------------
; the leading character splits the token. a name token (alnum/_) keeps - ? ! and
; the prime INSIDE it (the kebab law); a punctuation-led token is a SIGIL -- a
; maximal run of operator chars. ⚠ the kebab law is about the INSIDE of a
; name-led token: it never governed a token that STARTS with punctuation.
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
 (id? torn (sound (rdr-cl "\"oops"))))

; --- numbers -------------------------------------------------------------------
; ⚠ AN INTEGER LITERAL READS ITS VALUE -- in all three bases, at full precision,
; and the SAME in every build (host, love0, wasm, the kernel), whatever that
; build's word width or libc. that is a law, not an accident, and it was won.
;
; hex and octal used to go through strtol, so a literal at or above 2^63 came
; back as whatever THAT libc did with an overflow: one source text, three
; different numbers (nolibc wrapped to the negative twin, glibc saturated to
; LONG_MAX, wasm's 32-bit `long` did neither) -- while decimal, which had its own
; reader, promoted to a big. port/inle/klink.l spells kernel addresses exactly
; this way and rode the wrap. now all three bases share one reader
; (is_dec_int/is_hex_int/is_oct_int -> ai_big_read_dec/_hex/_oct), and the
; predicates are EXHAUSTIVE over what a base-0 strtol accepted whole, so the
; reader does not call it at all. a float still goes by am_strtod, which is ours
; and correctly rounded on every target.
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
 (18446744071564165120 = (rdr "0xffffffff80200000"))            ; klink.l's kernel vbase
 (9223372036854775807 = (rdr "0x7fffffffffffffff"))             ; the old strtol ceiling
 (340282366920938463463374607431768211455 = (rdr "0xffffffffffffffffffffffffffffffff"))
 (73786976294838206463 = (rdr "007777777777777777777777"))      ; 2^66-1, octal
 ; a prefix with no digits is no number: an honest symbol, free for binding.
 ; ⚠ and 08 is NEITHER octal (8 is not a digit there) nor decimal (the leading
 ; zero is a prefix) -- it falls through to the float lane, as it always has.
 ('0x = (rdr "0x")) ('0xg = (rdr "0xg")) ('0b101 = (rdr "0b101"))
 (8.0 = (rdr "08")))

; --- quote and the constructor wraps -------------------------------------------
; a sigil GLUED to its datum builds it: ' quote, ` list, # hash, @ tuple, ~ twin.
; those five are the exhaustive set. a list operand SPLICES into the call rather
; than being wrapped, and the empty collections go to a direct one-arg nif.
(assert
 ('(\ x) = (rdr "'x"))
 ('(list a b) = (rdr "`(a b)"))
 ('(hash a b) = (rdr "#(a b)"))
 ('(tuple 1 2) = (rdr "@(1 2)"))
 ('(twin 0 1) = (rdr "~(0 1)"))
 ('(conj x) = (rdr "~x"))                             ; ~ NOT before ( is monadic
 ; the empty collections go DIRECT to a one-arg nif (a zero-arg macro is not
 ; expanded by the self-hosted feel on love0). every reader emission is ()-tailed,
 ; these two included, so they compare WHOLE against a source-quoted form.
 ('(tablet 0) = (rdr "#()"))
 ('(iota 0) = (rdr "@()"))
 (() = (cup (cup (rdr "#()"))))
 (() = (cup (cup (rdr "<x")))))

; --- the valence law, reader half ----------------------------------------------
; GLUED is monadic, SPACED is dyadic -- everywhere, HEAD INCLUDED. a run glued to
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

; --- ⚠ + AND - ARE OPERATOR RUNS TOO -------------------------------------------
; they were the one special case, held out because they also lead numerals -- and
; that carve-out was wider than its reason. now: `+` and `-` are runs like every
; other punctuation, and the ONE exception is that a numeral has to start
; somewhere, so a digit (or a `.`, for -.5) right after them makes a NUMBER.
;
; so --5 is -(-5) and ++5 is +(+5), symmetric with !!5; and -x is negate x,
; symmetric with !x. ⚠ the two doubled forms arrive by DIFFERENT paths -- --5
; sheds its trailing - before the digit and reads (mono (- -5)), while ++5 has no
; shed and reads (mono (++ 5)) for opfix to split -- so both are asserted, or a
; later change breaks one lane in silence.
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
; [ ] { } are ( ) synonyms; the empty form is the ZERO POINT, not the number 0.
(assert
 ('(a b) = (rdr "[a b]")) ('(a b) = (rdr "{a b}"))
 (() = (rdr "()")) !(0 = (rdr "()"))
 (', = (rdr ",")))                                    ; the comma is its own datum
