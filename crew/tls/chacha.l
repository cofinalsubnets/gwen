; crew/tls/chacha.l -- chacha20 (rfc 8439), in the WORD LANE.
;
; the cipher is add / xor / rotate on 32-bit words, which is exactly what a z-tray
; is: contiguous limbs, fixed-width two's complement, `+ - *` wrapping, the bitwise
; five broadcasting elementwise (test/spec.l's WIDTH LAW). so there is no C here,
; and none is wanted -- the arithmetic love already has IS the cipher's arithmetic.
;
; the shape that makes it fast enough to be worth doing: VECTORISE ACROSS BLOCKS,
; not across the state. each of the 16 state words becomes a GALAXY of n cells --
; one cell per block in flight -- so a quarter-round is pure elementwise work and
; NO PERMUTATION IS EVER NEEDED. the diagonal round differs from the column round
; only in WHICH galaxies it names, which is a list index, not a data movement.
; the counter word is the one that varies down a galaxy: ctr + (iota n).
;
; ~2 MB/s at any batch width from 256 blocks up (the plateau is the lane's
; per-cell cost, ~14ns, not per-op overhead). that is ~330x under the same
; cipher in C. it is ample for a handshake and for seed's pulls; if the serving
; path ever wants more, the C twin goes BEHIND this interface and this stays the
; differential oracle -- never the other way round.
;
; ⚠ THE ONE-SCOPE LAW EATS THIS CIPHER IF YOU LET IT. a body-having `:` is one
; scope, so a quarter-round written the obvious way --
;   (: a (& (a + b) m32) d (rotl (d ^ a) 16) ..)
; -- has `(a + b)` read the INNER `a`, before its pin: the missing condition, which
; is the zero point, which is 0. every round then computes nothing, `block` hands
; back its own input, and NOTHING SCARES. the first draft did exactly this and ran
; 27x "faster". fresh names per step, always.
(:
   m32 4294967295

   ; rotate left within 32 bits. x is always already masked, so it is non-negative
   ; and `>>` (arithmetic, sign-propagating) coincides with the logical shift.
   (rotl x n) (& (| (<< x n) (>> x (32 - n))) m32)

   ; the quarter-round, on four galaxies -> a 4-list of galaxies.
   (qr a b c d)
    (: a1 (& (a + b) m32)   d1 (rotl (d ^ a1) 16)
       c1 (& (c + d1) m32)  b1 (rotl (b ^ c1) 12)
       a2 (& (a1 + b1) m32) d2 (rotl (d1 ^ a2) 8)
       c2 (& (c1 + d2) m32) b2 (rotl (b1 ^ c2) 7)
       (list a2 b2 c2 d2))

   ; the state is a 16-list of galaxies; these are its two accessors.
   (lset l i v) (: (go j r) (? (j < 0) r (go (j - 1) (link (? (j = i) v (peep l j 0)) r)))
                   (go 15 ()))
   (qra s i j k l)
    (: r (qr (peep s i 0) (peep s j 0) (peep s k 0) (peep s l 0))
       s1 (lset s i (peep r 0 0)) s2 (lset s1 j (peep r 1 0))
       s3 (lset s2 k (peep r 2 0)) s4 (lset s3 l (peep r 3 0))
       s4)

   ; one double round: four columns, then four diagonals.
   (dround s)
    (: u1 (qra s 0 4 8 12)   u2 (qra u1 1 5 9 13)
       u3 (qra u2 2 6 10 14) u4 (qra u3 3 7 11 15)
       u5 (qra u4 0 5 10 15) u6 (qra u5 1 6 11 12)
       u7 (qra u6 2 7 8 13)  u8 (qra u7 3 4 9 14)
       u8)
   (rounds s k) (? (k < 1) s (rounds (dround s) (k - 1)))

   ; the block function: 20 rounds, then add the original state back in.
   (cc-block s0)
    (: s (rounds s0 10)
       (go j r) (? (j < 0) r
                   (go (j - 1) (link (& ((peep s j 0) + (peep s0 j 0)) m32) r)))
       (go 15 ()))

   ; --- getting bytes in and out ---------------------------------------------------
   ; a little-endian 32-bit word off a string at byte offset i. absence reads 0, so a
   ; short key is a quiet key -- callers check the length, not this.
   ; ⚠ INFIX, not (+ a b c d): `+` is DYADIC, so a four-operand prefix `+` is
   ; APPLICATION -- it church-exponentiates and answers a bignum the size of the sky.
   (le32 s i) ((s i) + (<< (s (i + 1)) 8) + (<< (s (i + 2)) 16) + (<< (s (i + 3)) 24))

   (ones n) (1 + (0 * (iota n)))            ; the n-cell galaxy of 1s -- the broadcaster
   (w32 v n) (v * (ones n))                 ; a scalar, spread down n cells

   ; the initial state, n blocks wide, counters ctr .. ctr+n-1.
   ; key is 32 bytes, nonce 12.
   (cc-init key nonce ctr n)
    (list (w32 1634760805 n) (w32 857760878 n) (w32 2036477234 n) (w32 1797285236 n)
          (w32 (le32 key 0) n)  (w32 (le32 key 4) n)
          (w32 (le32 key 8) n)  (w32 (le32 key 12) n)
          (w32 (le32 key 16) n) (w32 (le32 key 20) n)
          (w32 (le32 key 24) n) (w32 (le32 key 28) n)
          (& (ctr + (iota n)) m32)
          (w32 (le32 nonce 0) n) (w32 (le32 nonce 4) n) (w32 (le32 nonce 8) n))

   ; the keystream for n blocks, as a 64n-byte string. the galaxies hold word w for
   ; EVERY block, so the emit order is block-major: for each block, its 16 words,
   ; each little-endian.
   (cc-stream key nonce ctr n)
    (: out (cc-block (cc-init key nonce ctr n))
       b (cask (n * 64))
       (wd j i) (peep (peep out j 0) i 0)
       (go i)                                        ; i counts blocks
        (? (n <= i) b
           (: (gw j)                                 ; j counts words within block i
               (? (15 < j) 0
                  (: v (wd j i) o ((i * 64) + (j * 4))
                     _ (pin b o (& v 255))
                     _ (pin b (o + 1) (& (>> v 8) 255))
                     _ (pin b (o + 2) (& (>> v 16) 255))
                     _ (pin b (o + 3) (& (>> v 24) 255))
                     (gw (j + 1))))
              _ (gw 0)
              (go (i + 1))))
       _ (go 0)
       (string b))

   ; encrypt or decrypt (they are the same act): xor `txt` with the keystream from
   ; block `ctr` on. answers a string of the same length.
   (cc-xor key nonce ctr txt)
    (: n (tally txt)
       nb (// (n + 63) 64)
       ks (cc-stream key nonce ctr nb)
       b (cask n)
       (go i) (? (n <= i) 0 (: _ (pin b i (^ (txt i) (ks i))) (go (i + 1))))
       _ (go 0)
       (string b)))
