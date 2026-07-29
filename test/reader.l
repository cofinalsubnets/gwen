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
; ⚠ this file is the ACCEPTANCE TEST for a reader rewritten in love (doc/reader.md
; -- p0/p1). every law it states must survive that flip byte for byte; a case that
; is deliberately allowed to change is a case that must be edited HERE first,
; before the reimplementation, or the differential cannot tell the two apart.
(: (rdr s) (: p (tap (map s (jot (tally s)))) e (nom 0) (sound p e))
   rdr-eof (nom 0))

; --- the read protocol ---------------------------------------------------------
; `sound` answers THREE ways and the caller must tell them apart: the datum, the
; SENTINEL at end of input, or THE PORT ITSELF on an unfinished shape. nothing is
; stashed -- a torn read discards the partial parse and leaves the port exhausted,
; so `more` means "you own the text, re-read the concatenation", which is exactly
; what bao's editor and the socket repls do.
(assert
 (1 = (sound (tap '(49)) rdr-eof))                    ; a datum
 (id? rdr-eof (sound (tap 0) rdr-eof))                ; eof -> the sentinel
 (: p (tap '(40)) (id? p (sound p rdr-eof)))          ; unfinished -> the port back
 (: p (tap '(40)) _ (sound p rdr-eof)                 ; ...and the port is spent
    (id? rdr-eof (sound p rdr-eof))))

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
 (: p (tap (map "\"oops" (jot 5))) (id? p (sound p rdr-eof))))

; --- numbers -------------------------------------------------------------------
; signed decimal, floats, and 0x hex.
;
; ⚠ HEX AND DECIMAL DISAGREE ABOVE 2^63. decimal promotes to a big; hex goes
; through strtol and does not. the bits are what every emit path wants, but
; anything that DIVIDES overshoots -- crew/holo/link.l:542 routes an address
; split through an unsigned door for exactly this reason.
;
; ⚠⚠ AND THE TWO BUILDS DISAGREE WITH EACH OTHER, which is worse and is why the
; over-2^63 case is NOT asserted here. the same source text reads as two
; different numbers depending on which binary reads it:
;     out/host/love  (mooncc + nolibc)  0xffffffff80200000 -> -2145386496
;     out/host/love0 (system cc + glibc) 0xffffffff80200000 -> 9223372036854775807
; nolibc's strtol (crew/moon/lib/nolibc.c:1325) has no overflow handling at all
; and wraps; glibc's saturates to LONG_MAX with ERANGE. port/inle/klink.l spells
; kernel addresses exactly this way. rung 2 of doc/reader.md closes BOTH holes at
; once -- bit ops on bignums, then hex promotes like decimal and the strtol
; overflow path stops mattering. when it lands, assert the value here.
(assert
 (3 = (rdr "3")) (-3 = (rdr "-3")) (3 = (rdr "+3"))
 (16 = (rdr "0x10")) (255 = (rdr "0xff"))
 ((0 - (1 / 2)) = (rdr "-.5")) ((1 / 2) = (rdr "+.5"))
 (18446744073709551615 = (rdr "18446744073709551615"))          ; decimal above the lane: a big
 ; ⚠ and the hex lane is WIDTH-dependent on top of all that: strtol's `long` is
 ; 32 bits on the wasm shim, so a 64-bit hex literal is not even the same number
 ; there. a third disagreement, and the third reason rung 2 should retire this
 ; path rather than patch it.
 (? word (9223372036854775807 = (rdr "0x7fffffffffffffff")) 1))

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
 ; expanded by the self-hosted feel on love0). ⚠ compared head and operand, NOT
 ; whole: this lane emits a 0-TAILED list where the mono lane emits an ()-tailed
 ; one, so `=` against a source-quoted form fails on the tail alone. that
 ; inconsistency is real and is a thing a reimplementation must reproduce or
 ; deliberately normalize -- it is asserted here so the choice cannot be silent.
 ('tablet = (cap (rdr "#()"))) (0 = (cap (cup (rdr "#()"))))
 ('iota = (cap (rdr "@()"))) (0 = (cap (cup (rdr "@()"))))
 !(() = (cup (cup (rdr "#()"))))                      ; 0-tailed, not ()-tailed
 (() = (cup (cup (rdr "<x")))))                       ; ...while this one is ()

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
