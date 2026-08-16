; lib/json.l -- json, so love can speak to things that are not love. it is the wire
; format the outside world settled on (language servers, http apis, sway's ipc), and
; nothing about it is lovely; this file is the translation desk, nothing more.
; `(use 'json)`, then json-read and json-show.
;
; the MAPPING, a BIJECTION in both directions:
;   null    <-> json-null       a coin, like the booleans -- see the notes below
;   true    <-> json-true
;   false   <-> json-false
;   number  <-> charm / sun / big / gem     (the celestial ladder; exact where json is)
;   string  <-> string
;   array   <-> list, and `[]` is plain ()
;   object  <-> tablet
;
; ⚠ NULL IS NAMED, because it cannot be (): `[]` and `null` would be the SAME VALUE
; (every nothing here is nil, and an empty list IS the zero point), and the writer would
; have to pick one -- a document round-tripped through one would come back as the other.
; the empty ARRAY is the common value: a mapped or filtered list should write `[]` with
; no renaming step, so plain () is the empty array and null -- the weird one -- carries
; the name. json-null still nets 0, so `(? v ..)` and `!` read null as false, while
; json-null? keeps it exact for the writer.
;
; ⚠ THE NAMED VALUES ARE COINS. 1 and 0 cannot round-trip (the writer would emit numbers,
; and lsp wants `true`). the symbol pair 'true/'false is WORSE than useless: 'false nets
; its spelling, which is POSITIVE, so `(? v ..)` would take the true branch on false --
; silent, plausible, and exactly the shape of bug this tree keeps paying for; 'null would
; be the same bug. a coin nets its PAYLOAD, so json-false and json-null are blue and
; `(? v ..)` is simply right, while coin? keeps them exact for the writer. that is what
; coins are for. json-null rides its OWN die: two coins with payload 0 on the SAME die
; would be `=`, and null is not false.
;
; ⚠ READING CARRIES PRESENCE OUT OF BAND: json-read answers (1 value), or () when the
; text is not json. test with two?, take the value with <>. it CANNOT be otherwise --
; null, 0, "", [] and {} are all legitimate documents and every one of them is nothing,
; so a bare value could never say whether it parsed. (the wrapper law, and the reason it
; exists.)
;
; the reader is plain recursive descent over the string, an index threaded through. each
; jXXX answers `(value next-index)` or () for "not that" -- pairs, so two? reads presence
; and a parsed () is still a parse. -1 is the "no digit" sentinel for the same reason a
; hex digit can legitimately be 0.
(use 'coin)

(: json-die   {0 'json-bool}
   json-true  (coin json-die 1)
   json-false (coin json-die 0)
   ; ⚠ `&&` is value-preserving, so the bare conjunction answers () and not the bit a
   ; predicate owes; `!!` is the clamp. same for json-null? below.
   (json-bool? v) !!(coin? v && id? (die-of v) json-die)
   (json-bool v)  (v ? json-true json-false)

   ; null, on its OWN die -- payload 0 so it nets blue like the nothing it is, but
   ; never `=` json-false.
   json-null-die  {0 'json-null}
   json-null      (coin json-null-die 0)
   (json-null? v) !!(coin? v && id? (die-of v) json-null-die)

   ; --- reading --------------------------------------------------------------------
   (jws s i n)  (i < n && member? (s i) '(32 9 10 13) ? (jws s (i + 1) n) i)

   (jdigv c)    (? (48 <= c <= 57) (c - 48) -1)
   (jhexv c)    (? (48 <= c <= 57)  (c - 48)
                   (97 <= c <= 102) (c - 87)
                   (65 <= c <= 70)  (c - 55)
                   -1)

   ; a run of decimals -> `(value end). end = start means there were none, which the
   ; callers check: json wants at least one digit everywhere it wants a digit.
   (jdigits s i n) ((: (go k acc)
                       (? (k < n && -1 < jdigv (s k))
                          (go (k + 1) (acc * 10 + jdigv (s k)))
                          [acc k]))
                    i 0)

   (jhex4 s i n)   ((: (go k acc d)
                       (? (d = 0)      [acc k]
                          (k >= n)  ()
                          (: v (jhexv (s k))
                             (v >= 0 ? (go (k + 1) (acc * 16 + v) (d - 1))))))
                    i 0 4)

   ; a code point out as utf-8 bytes, laid down by the byte law (string + charm appends).
   (jutf8 u) (? (u < 128)   ("" + u)
                (u < 2048)  ("" + (192 + (u // 64)) + (128 + (u % 64)))
                (u < 65536) ("" + (224 + (u // 4096)) + (128 + ((u // 64) % 64))
                                + (128 + (u % 64)))
                ("" + (240 + (u // 262144)) + (128 + ((u // 4096) % 64))
                    + (128 + ((u // 64) % 64)) + (128 + (u % 64))))

   ; \uXXXX, with the surrogate pair rejoined. a lone or unpaired surrogate is passed
   ; through as itself rather than refused -- the text is still json, and refusing a whole
   ; document over one bad code unit helps nobody.
   (juni s i n) (: h (jhex4 s i n)
                   (? (! (two? h)) ()
                      (: u <h
                         k <>h
                         (? !(55296 <= u <= 56319)            [(jutf8 u) k]
                            !((k + 1) < n &&
                              s k = 92 &&
                              s (k + 1) = 117) [(jutf8 u) k]
                            (: l (jhex4 s (k + 2) n)
                               (? (atom? l) [(jutf8 u) k]
                                  (: v <l
                                     (? (&& (56320 <= v) (v <= 57343))
                                        [(jutf8 (65536 + (((u - 55296) * 1024)
                                                           + (v - 56320)))) <>l]
                                        [(jutf8 u) k]))))))))

   ; the byte AFTER the backslash. -1 for "no such escape", never () -- 0 is a fine byte.
   (jesc s i n) (? (! (i < n)) ()
                   (: c (s i)
                      (? (c = 117) (juni s (i + 1) n)
                         (: b (? (c = 34)  34   (c = 92)  92   (c = 47)  47
                                 (c = 98)  8    (c = 102) 12   (c = 110) 10
                                 (c = 114) 13   (c = 116) 9
                                 -1)
                            (? (b < 0) () [("" + b) (i + 1)])))))

   (jstr s i n) ((: (go k acc)
                     (? (! (k < n)) ()
                        (: c (s k)
                           (? (c = 34) [acc (k + 1)]
                              (c = 92) (: e (jesc s (k + 1) n)
                                          (? (two? e) (go <>e (acc + <e)) ()))
                              (c < 32) ()            ; a raw control byte is not json
                              (go (k + 1) (acc + c))))))
                 (i + 1) "")

   ; an exponent: optional sign, then at least one digit.
   (jexp s i n) (: g  (? (&& (i < n) ((s i) = 45)) -1 1)
                   a  (? (&& (i < n) (member? (s i) '(43 45))) (i + 1) i)
                   dg (jdigits s a n)
                   (? (<>dg = a) () [(g * <dg) <>dg]))

   (jpow10 e) (? (e < 0) (1.0 / ((0 - e) 10)) (1.0 * (e 10)))

   ; a whole int stays EXACT (charm -> sun -> big, whatever it needs); anything with a
   ; fraction or an exponent becomes a gem, which is what json means by those.
   (jnum s i n) (: neg (&& (i < n) ((s i) = 45))
                   a   (? neg (i + 1) i)
                   ip  (jdigits s a n)
                   iv  <ip
                   b   <>ip
                   (? (b = a) ()                       ; a sign with no digits is nothing
                      (: fd (? (&& (b < n) ((s b) = 46)) (jdigits s (b + 1) n) ())
                         c  (? (two? fd) <>fd b)
                         fv (? (two? fd) <fd 0)
                         fl (? (two? fd) (c - (b + 1)) 0)
                         ee (&& (c < n) (member? (s c) '(101 69)))
                         ex (? ee (jexp s (c + 1) n) ())
                         d  (? (two? ex) <>ex c)
                         (? (&& (two? fd) (fl = 0)) ()   ; "1." is not json
                            (&& ee (! (two? ex)))     ()   ; "1e" is not json either
                            (: sgn (? neg -1 1)
                               (? (&& (fl = 0) (! ee))
                                  `((sgn * iv) d)
                                  (: mant ((iv * (fl 10)) + fv)
                                     base (((1.0 * sgn) * mant) / (fl 10))
                                     (? (two? ex) `((base * (jpow10 <ex)) d)
                                        `(base d)))))))))

   (jlit? s i n w) (: e (i + (tally w)) (&& (e <= n) ((snip s i e) = w)))

   (jlit s i n) (? (jlit? s i n "true")  `(json-true (i + 4))
                   (jlit? s i n "false") `(json-false (i + 5))
                   (jlit? s i n "null")  `(json-null (i + 4))
                   (jnum s i n))

   (jarr s i n) (: a (jws s (i + 1) n)
                   (? (&& (a < n) ((s a) = 93)) `(() (a + 1))
                      ((: (go k acc)
                          (: r (jval s k n)
                             (? (! (two? r)) ()
                                (: e (jws s <>r n)
                                   (? (! (e < n))    ()
                                      ((s e) = 44)   (go (e + 1) (link <r acc))
                                      ((s e) = 93)   `((rev (link <r acc)) (e + 1))
                                      ())))))
                       a ())))

   ; a later duplicate key wins, which is what pin does and what every other reader does.
   (jobj s i n) (: a (jws s (i + 1) n)
                   (? (a < n && s a = 125) [{} (a + 1)]
                      ((: (go k acc)
                          (: ks (jws s k n)
                             (? !(ks < n && s ks = 34) ()
                                (: kr (jstr s ks n)
                                   (? (atom? kr) ()
                                      (: cn (jws s <>kr n)
                                         (? !(cn < n && s cn = 58) ()
                                            (: vr (jval s (cn + 1) n)
                                               (? (atom? vr) ()
                                                  (: _ (pin acc <kr <vr)
                                                     e (jws s <>vr n)
                                                     (? (e >= n)    ()
                                                        (s e = 44)   (go (e + 1) acc)
                                                        (s e = 125)  [acc (e + 1)])))))))))))
                       a {})))

   (jval s i n) (: a (jws s i n)
                  (a < n ? (s a @ 34 (jstr s a n) 91 (jarr s a n) 123 (jobj s a n) (jlit s a n))))

   ; --- the door in ----------------------------------------------------------------
   ; (1 value), or () -- trailing junk after the document is a refusal, not a shrug.
   (json-read s) (: n (tally s)
                    r (jval s 0 n)
                    (? (two? r) (: e (jws s <>r n) (? (e < n) () [1 <r]))))

   ; --- writing --------------------------------------------------------------------
   (jhexd v) ("" + (? (v < 10) (48 + v) (87 + v)))

   ; bytes >= 128 ride out untouched: love strings are byte strings and json is utf-8, so
   ; already-utf-8 text is already right. only the seven named escapes and the control
   ; bytes need saying.
   ; ⚠ the escape is SPELLED, never built: `+` appends a charm as a byte, so the old
   ; ("" + 92 + 34) leaned on the band's left hand to mean \" -- one re-association and
   ; it is the byte 126. only the two computed nibbles ride the byte law now.
   (jqc c) (c @ 34 "\\\""  92 "\\\\"
                10 "\\n"   13 "\\r"   9 "\\t"
                8  "\\b"   12 "\\f"
                (c < 32 ? ("\\u00" + (jhexd (c // 16)) + (jhexd (c % 16)))
                          ("" + c)))

   (jqt s) (: n (tally s)
              ((: (go k acc) (? (k < n) (go (k + 1) (acc + (jqc (s k)))) (acc + 34)))
               0 ("" + 34)))

   (jjoin xs) (? (atom? xs) ""
                 ((: (go l acc) (? (two? l) (go >l ((acc + 44) + <l)) acc)) >xs <xs))

   ; show is the whole number lane -- it already lays a charm, a sun, a big and a gem down
   ; the way json spells them. the GUARD is for what json has no word for: ieee-inf and
   ; friends show as names, so anything not starting with a digit or a minus becomes null
   ; rather than riding out as text that no parser would take back.
   (jnumshow v) (: t (show v)
                   c (? (0 < (tally t)) (t 0) 0)
                   (? (|| (c = 45) (&& (48 <= c) (c <= 57))) t "null"))

   ; the () = v test is EXACT -- true only for () itself, never 0 or "" (different bands)
   ; -- so a bare () after a map or a filter writes `[]` with no renaming step. anything
   ; json has no spelling for scares rather than riding out as a plausible null; trays
   ; stay out of the contract (arrays are lists here).
   (json-show v)
      (? (string? v)    (jqt v)
         (json-bool? v) (? v "true" "false")
         (json-null? v) "null"
         (() = v)       ("" + 91 + 93)
         (tablet? v)    ((("" + 123)
                          + (jjoin (map (\ k ((jqt k) + 58 + (json-show (v k)))) (keys v))))
                         + ("" + 125))
         (tray? v)      (scare 'json "arrays are lists" v)
         (two? v)       ((("" + 91) + (jjoin (map json-show v))) + ("" + 93))
         (constellation? v) (jnumshow v)
         (scare 'json "no json spelling for this" v)))