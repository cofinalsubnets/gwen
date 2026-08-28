; crew/tls/poly1305.l -- poly1305 (rfc 8439 §2.5), the one-time authenticator.
;
; the arithmetic is a polynomial evaluated mod 2^130 - 5: for each 16-byte block,
; h = (h + block) * r, and the tag is h + s. love has bignums and `%`, so the
; whole thing would fit in ten lines -- and would leak `r` through the timing of
; every multiply and every reduction. `r` is SECRET and a forged tag is the whole
; game, so this takes the long road instead: FIVE 26-BIT LIMBS, the shape
; poly1305-donna wears, where the modulus is folded into the multiply as *5 and
; the reduction is a fixed carry chain with no branch and no division.
;
; ⚠ THE MAGNITUDE BOUND IS THE INVARIANT, and it is what makes this both correct
; and constant-time. every limb stays under 2^26, so a product is under 2^52 and
; the five-term sum under 2^57 -- inside a CHARM, where love's `*` and `+` are
; exact (nothing to wrap) and single-tier (nothing promotes to a sun or a big).
; the bound depends only on the SHAPE of the sum, never on the values, so the
; tier is data-independent and so is the cost. widen a limb and you lose both at
; once, silently: the arithmetic would still be right, and the timing would start
; talking. re-derive the bound before changing any mask here.
;
; ⚠ and the final conditional subtract is a MASK, never a `?`. `(>> j4 63)` is
; -1 when h < p and 0 otherwise (`>>` is arithmetic, so it propagates the sign),
; so both branches are always computed and one is selected by AND/OR. a `?` here
; would be a branch on the accumulator, which is a branch on the message and the
; key together.
;
; src/tls.c's `poly1305` nif is the C twin, the same five limbs and the same
; mask; test/host/tlsc.l holds it to this file, crew/tls/bench.l times both.
(:
   m26 67108863                              ; 2^26 - 1, one limb
   hib 16777216                              ; 2^24, the high bit a full block carries

   ; --- the key: r clamped into five limbs, s left as four words ------------------
   ; the masks ARE rfc 8439 §2.5.1's clamp (r &= 0x0ffffffc0ffffffc0ffffffc0fffffff),
   ; folded into the split so no separate clamping step can be forgotten.
   (po-r key)
    (: t0 (le32 key 0) t1 (le32 key 4) t2 (le32 key 8) t3 (le32 key 12)
       [(t0 & m26)
        (((t0 >> 26) | (t1 << 6)) & 67108611)
        (((t1 >> 20) | (t2 << 12)) & 67092735)
        (((t2 >> 14) | (t3 << 18)) & 66076671)
        ((t3 >> 8) & 1048575)])

   ; --- absorb: h += the block at offset o, as five limbs -------------------------
   ; `hi` is the 2^128 term: 2^24 in the top limb for a full block, and 0 for the
   ; final short one (whose 0x01 byte already sits inside the sixteen).
   (po-absorb h msg o hi)
    (: t0 (le32 msg o) t1 (le32 msg (o + 4))
       t2 (le32 msg (o + 8)) t3 (le32 msg (o + 12))
       [(peep h 0 0 + (t0 & m26))
        (peep h 1 0 + (((t0 >> 26) | (t1 << 6)) & m26))
        (peep h 2 0 + (((t1 >> 20) | (t2 << 12)) & m26))
        (peep h 3 0 + (((t2 >> 14) | (t3 << 18)) & m26))
        (peep h 4 0 + ((t3 >> 8) | hi))])

   ; --- h = h * r mod 2^130 - 5 ---------------------------------------------------
   ; the modulus never appears: a limb that overflows past 2^130 comes back at the
   ; bottom multiplied by 5, which is exactly what the s1..s4 terms are.
   (po-mul h r)
    (: h0 (peep h 0 0) h1 (peep h 1 0) h2 (peep h 2 0)
       h3 (peep h 3 0) h4 (peep h 4 0)
       r0 (peep r 0 0) r1 (peep r 1 0) r2 (peep r 2 0)
       r3 (peep r 3 0) r4 (peep r 4 0)
       s1 (r1 * 5) s2 (r2 * 5) s3 (r3 * 5) s4 (r4 * 5)
       d0 (h0 * r0 + h1 * s4 + h2 * s3 + h3 * s2 + h4 * s1)
       d1 (h0 * r1 + h1 * r0 + h2 * s4 + h3 * s3 + h4 * s2)
       d2 (h0 * r2 + h1 * r1 + h2 * r0 + h3 * s4 + h4 * s3)
       d3 (h0 * r3 + h1 * r2 + h2 * r1 + h3 * r0 + h4 * s4)
       d4 (h0 * r4 + h1 * r3 + h2 * r2 + h3 * r1 + h4 * r0)
       ; one carry pass up, then the 2^130 -> *5 wrap back down
       c0 (d0 >> 26) e0 (d0 & m26)
       a1 (d1 + c0)  c1 (a1 >> 26) e1 (a1 & m26)
       a2 (d2 + c1)  c2 (a2 >> 26) e2 (a2 & m26)
       a3 (d3 + c2)  c3 (a3 >> 26) e3 (a3 & m26)
       a4 (d4 + c3)  c4 (a4 >> 26) e4 (a4 & m26)
       f0 (e0 + c4 * 5) c5 (f0 >> 26) g0 (f0 & m26)
       [g0 (e1 + c5) e2 e3 e4])

   ; --- the tag: carry out, subtract p if it fits, add s --------------------------
   (po-fin h key)
    (: h0 (peep h 0 0) h1 (peep h 1 0) h2 (peep h 2 0)
       h3 (peep h 3 0) h4 (peep h 4 0)
       ; settle every limb under 2^26
       c1 (h1 >> 26) i1 (h1 & m26)
       a2 (h2 + c1) c2 (a2 >> 26) i2 (a2 & m26)
       a3 (h3 + c2) c3 (a3 >> 26) i3 (a3 & m26)
       a4 (h4 + c3) c4 (a4 >> 26) i4 (a4 & m26)
       a0 (h0 + c4 * 5) c0 (a0 >> 26) i0 (a0 & m26)
       j1 (i1 + c0)
       ; g = h - p, computed as h + 5 - 2^130. j4 goes NEGATIVE exactly when h < p.
       b0 (i0 + 5)  k0 (b0 >> 26) n0 (b0 & m26)
       b1 (j1 + k0) k1 (b1 >> 26) n1 (b1 & m26)
       b2 (i2 + k1) k2 (b2 >> 26) n2 (b2 & m26)
       b3 (i3 + k2) k3 (b3 >> 26) n3 (b3 & m26)
       n4 (i4 + k3 - 67108864)
       keep (n4 >> 63)                       ; -1 when h < p (keep h), else 0
       drop (keep ^ -1)
       p0 ((i0 & keep) | (n0 & drop)) p1 ((j1 & keep) | (n1 & drop))
       p2 ((i2 & keep) | (n2 & drop)) p3 ((i3 & keep) | (n3 & drop))
       p4 ((i4 & keep) | (n4 & drop))
       ; five 26-bit limbs back into four 32-bit words
       w0 ((p0 | (p1 << 26)) & 4294967295)
       w1 (((p1 >> 6) | (p2 << 20)) & 4294967295)
       w2 (((p2 >> 12) | (p3 << 14)) & 4294967295)
       w3 (((p3 >> 18) | (p4 << 8)) & 4294967295)
       ; + s, with the carry walking up
       f0 (w0 + le32 key 16) o0 (f0 & 4294967295)
       f1 (w1 + le32 key 20 + (f0 >> 32)) o1 (f1 & 4294967295)
       f2 (w2 + le32 key 24 + (f1 >> 32)) o2 (f2 & 4294967295)
       f3 (w3 + le32 key 28 + (f2 >> 32)) o3 (f3 & 4294967295)
       b (cask 16)
       _a (le32! b 0 o0)  _b (le32! b 4 o1)
       _c (le32! b 8 o2)  _d (le32! b 12 o3)
       (string b))

   ; --- the whole authenticator: (po-mac key msg) -> a 16-byte tag ----------------
   ; key is 32 bytes: r in the first sixteen, s in the second.
   (po-mac key msg)
    (: r (po-r key)
       n #msg
       nfull (n // 16)
       rem (n - nfull * 16)
       (go i h) (nfull <= i ? h (go (i + 1) (po-mul (po-absorb h msg (i * 16) hib) r)))
       hf (go 0 [0 0 0 0 0])
       ; the short final block is padded into sixteen with its 0x01 in place, so
       ; the same absorb serves -- with no high bit, since the 1 is already inside.
       ht (rem < 1 ? hf
             (: o (nfull * 16)
                pad (bytes 16 (j \ j < rem && msg (o + j) || j = rem))
                (po-mul (po-absorb hf pad 0 0) r)))
       (po-fin ht key)))
