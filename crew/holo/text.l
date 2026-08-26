; text -- a human-writable TEXT front-end for the neutral assembler, parsed with the PARSER
; COMBINATORS (love/peg.l, the same set cook and sat use). it turns lines like
;     li   r0, 60        ; a comment
;     add  r0, r0, r1
;     label loop
;     br   eq, loop
;     ret
; into the IR list ((li r0 60) (add r0 r0 r1) (label loop) (br eq loop) (ret)) that crew/holo/holo.l
; assembles. one line = one instruction; operands are separated by spaces and/or commas; a register
; or label is a symbol, an immediate a (optionally signed) number; `;` starts a comment. there is no
; arch here at all -- it lowers to whatever backend `holo` is handed. mooncc's inline asm
; (crew/moon/gen.l) parses its templates with asm-text; its cats carry this file.
;
; needs only (use 'peg) -- no assembler names, so it loads in the raw holo.l stream (test_holo)
; and on the baked host alike:
;   out/host/love -l crew/holo/text.l ...

; the combinators (love/peg.l). the use is its OWN top-level form -- a `:` body compiles
; as one unit, so a use inside the form below would run too late to serve it.
(module 'holo
(use 'peg)

(: ;; -- byte-class predicates (the combinator leaves feed a parser one byte at a time). --
   (txws?  c) ((c = 32) || (c = 9))                             ; space / tab
   (txsep? c) (txws? c || (c = 44))                             ; ... or a comma, between operands
   (txdig? c) ((c >= 48) && (c <= 57))
   (txidc? c) (((c >= 97) && (c <= 122)) ||                     ; a-z
               ((c >= 65) && (c <= 90)) ||                      ; A-Z
               txdig? c || (c = 95) || (c = 45))                ; 0-9 _ -  (kebab labels welcome)

   ;; -- small string helpers (a payload piece is a one-char string from `one` or a substring
   ;; from `span`, so `+` joins either by plain concat). --
   (txjoin ss) (foldl (\ a x (a + x)) "" ss)
   (txint s)   (foldl (\ a i ((a * 10) + (s i - 48))) 0 (jot (tally s)))
   (txnum p)   (? (<p = "-") (0 - txint (txjoin >p)) (txint (txjoin p)))

   ;; -- the line grammar. a token is at least one char (so `many` can't twirl on the empty match). --
   txtsym (pmap (\ x [(intern (txjoin x))]) (seq (one txidc?) (span txidc?)))
   txtnum (pmap (\ x [(txnum x)]) (seq (opt (lit "-")) (seq (one txdig?) (span txdig?))))
   txop   (alt txtnum txtsym)                                   ; an operand: number first, else symbol
   txgap  (skip (span txsep?))                                  ; a run of spaces/commas, dropped
   txline (seq txtsym (many (seq txgap txop)))                  ; head symbol then operands -> one form

   ;; -- line preprocessing: drop the `;` comment, trim the ends, split on newlines. --
   (txdecomment s) (: n (tally s)
      ((: (g i) (? (i >= n) s (s i = 59) (snip s 0 i) (g (i + 1)))) 0))
   (txtrim s) (: n (tally s)
      a ((: (gl i) (? ((i < n) && txws? (s i)) (gl (i + 1)) i)) 0)
      b ((: (gr j) (? (j > a && txws? (s (j - 1))) (gr (j - 1)) j)) n)
      (snip s a b))
   (txsplit s) (: n (tally s)
      ((: (g i start acc)
          (? (i >= n)     (rev (snip s start n >< acc))
             (s i = 10) (g (i + 1) (i + 1) (snip s start i >< acc))
             (g (i + 1) start acc)))
       0 0 ()))

   ;; -- the front door: TEXT -> IR. blank/comment-only lines vanish; a line that won't parse is a
   ;; deliberate raise (a typo shouldn't silently drop an instruction). the assemble step stays
   ;; with the caller ((holo-hex t (asm-text s)) and kin): this file binds NO assembler names, so
   ;; it loads clean in both worlds -- the raw holo.l stream AND the baked host (book-only). --
   (asm-text src)
      (foldr (\ ln acc
                (: clean (txtrim (txdecomment ln))
                   (? (0 = tally clean) acc
                      (: form (parse txline clean)
                         (? (two? form) (form >< acc) (scare 'asm-text clean))))))
             () (txsplit src)))
) ; module 'holo
