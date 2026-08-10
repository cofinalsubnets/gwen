; crew/tls/bytes.l -- the byte floor every layer above stands on.
;
; tls is a protocol of byte runs, and love's string is a byte run, so these are
; the four conversions that keep turning up: a little-endian word out of a string,
; a little-endian word into a cask, a big-endian pair for the record header, and
; hex for the tests to speak in.
;
; ⚠ INFIX, not (+ a b c d), in every one of these: `+` is DYADIC, so a prefix `+`
; at four operands is APPLICATION -- it church-exponentiates and answers a bignum
; the size of the sky. the infix fold is left-handed and gives the sum.
(:
   ; a little-endian 32-bit word off a string at byte offset i. an absent byte
   ; reads the zero point, which is 0, so a SHORT INPUT IS A QUIET INPUT --
   ; callers check the length, never this.
   (le32 s i) ((s i) + (<< (s (i + 1)) 8) + (<< (s (i + 2)) 16) + (<< (s (i + 3)) 24))

   ; ..and the same word back out, into a cask at offset o.
   (le32! b o v)
    (: _a (pin b o (& v 255))
       _b (pin b (o + 1) (& (>> v 8) 255))
       _c (pin b (o + 2) (& (>> v 16) 255))
       _d (pin b (o + 3) (& (>> v 24) 255))
       0)

   ; the record layer counts in BIG-endian 16, which is the one place tls
   ; disagrees with the ciphers about which end a number starts at.
   (be16 s i) ((<< (s i) 8) + (s (i + 1)))
   (be16! b o v) (: _a (pin b o (& (>> v 8) 255)) _b (pin b (o + 1) (& v 255)) 0)

   ; an n-byte string, byte i being (f i). the one constructor everything uses.
   (bytes n f)
    (: b (cask n)
       (go i) (? (n <= i) (string b) (: _ (pin b i (f i)) (go (i + 1))))
       (go 0))

   ; lowercase hex, and back. the tests speak hex because the RFCs do.
   (hex s)
    (: d "0123456789abcdef" n (tally s)
       (go i a) (? (n <= i) a (go (i + 1) (a + (d (>> (s i) 4)) + (d (& (s i) 15)))))
       (go 0 ""))
   (unhex h)
    (: (nib c) (? (c < 58) (c - 48) (c - 87))
       n (// (tally h) 2)
       (bytes n (\ i (| (<< (nib (h (i * 2))) 4) (nib (h ((i * 2) + 1))))))))
