; doc/proto/zstd.l -- zstd in love (RFC 8878): the frame, its blocks, and the two
; entropy coders inside them -- FSE for the sequence codes, Huffman for the literals.
; A spike, weighed against lib/gz.l and doc/proto/lzo.l for the source blob the seed
; carries.
;
;   (zs-decompress s)      -> (1 bytes) | ()    reads what the reference `zstd` writes
;   (zs-compress s)        -> a zstd frame, a string
;
; ⚠ presence rides the (1 _) wrapper on the DECODE, gz.l's law.
;
; the shape of the format, since none of it is guessable from the code: a FRAME is a
; magic, a header and a run of BLOCKS. a compressed block is a LITERALS section --
; every byte that no match covered, packed together and Huffman-coded -- followed by a
; SEQUENCES section, which is a list of (how many literals, how far back, how long)
; triples entropy-coded with FSE. the decode interleaves them: take n literals, then
; copy a match, again, and the literals left over at the end close the block.
;
; ⚠ THE ENTROPY BITSTREAMS ARE READ BACKWARD. the Huffman streams and the sequence
; stream both start at their LAST byte and walk down, MSB first, and the highest set
; bit of that last byte is a padding marker rather than data. the FSE table
; descriptions in front of them are read FORWARD, LSB first. two readers, and mixing
; them up is the one mistake that still decodes a few symbols before it dies.
;
; ⚠ FSE IS NOT HUFFMAN. a symbol does not own a bit pattern; the decoder owns a STATE,
; the state names a symbol, and the bits read say which state comes next. so the
; decode runs in the opposite order from the encode -- which is why the stream is
; written backward -- and a table is a spread of symbols over 2^accuracy slots rather
; than a tree.
;
; ⚠ NO XXH64 HERE. the frame's optional checksum is stepped over, not verified, and
; the coder writes none -- so this is the one thing gz.l does that this does not, and
; a fair reading of the line counts has to add it back.
(:
   ; --- the growing byte buffer ---------------------------------------------
   ; the frame's whole output: a match may reach back into an earlier block, so the
   ; window is everything produced so far and the sink has to be indexable.
   (zs-buf n) (: b (tablet 0) _ (pin b 0 (cask (? (n < 64) 64 n))) _ (pin b 1 0) b)
   (zs-room b k)
    (: c (peep b 0 0) cap (tally c) len (peep b 1 0)
       (? ((len + k) <= cap) 0
          (: big (: (go m) (? ((len + k) <= m) m (go (m * 2))) (go cap))
             c2 (cask big)
             _ (? len (pour c2 0 (string c) 0 len) 0)
             _ (pin b 0 c2) 0)))
   ; ⚠ `pour`'s last argument is a COUNT, not an end offset, and the nif clamps it to
   ; the shorter of the two arrays -- so a wrong one can answer right and move the
   ; whole tail to do it.
   (zs-blit b s i k)
    (: _ (zs-room b k)
       c (peep b 0 0) o (peep b 1 0)
       _ (pour c o s i k)
       _ (pin b 1 (o + k)) 0)
   (zs-take b)
    (: c (peep b 0 0) len (peep b 1 0)
       (? len (: c2 (cask len) _ (pour c2 0 (string c) 0 len) (string c2)) ""))
   ; a match may overlap its own source, so this copies byte by byte
   (zs-copy b dist len)
    (: _ (zs-room b len)
       c (peep b 0 0)
       (go k p) (? (len <= k) (: _ (pin b 1 p) 1)
                   (p < dist) (: _ (pin b 1 p) ())
                   (: _ (pin c p (peep c (p - dist) 0)) (go (k + 1) (p + 1))))
       (go 0 (peep b 1 0)))
   (zs-cut s i k) (? (0 < k) (: c (cask k) _ (pour c 0 s i k) (string c)) "")
   (zs-le s i k) (: (go j a) (? (k <= j) a (go (j + 1) (a | ((s (i + j)) << (j * 8))))) (go 0 0))
   (zs-hb v) (: (go x k) (? (x <= 1) k (go (x >> 1) (k + 1))) (go v 0))

   ; --- the two bit readers --------------------------------------------------
   ; BACKWARD, MSB first, over a section held as its own string: the cursor counts bits
   ; from the section's END, and starts at the padding the last byte declares.
   (zs-bstart s)                           ; -> a cursor, or () if the pad byte is 0
    (: n (tally s)
       (? (! n) ()
          (: last (s (n - 1))
             (? (! last) ()
                (: t (tablet 0) _ (pin t 0 (8 - (zs-hb last))) _ (pin t 1 0) t)))))
   (zs-rb s n t k)                         ; k bits; past the end pads with zeros
    (: (go j a p)
        (? (k <= j) (: _ (pin t 0 p) a)
           ((n * 8) <= p) (: _ (pin t 1 1) _ (pin t 0 p) (a << (k - j)))
           (go (j + 1) ((a << 1) | (((s (n - 1 - (p >> 3))) >> (7 - (p & 7))) & 1)) (p + 1)))
       (go 0 0 (peep t 0 0)))
   ; FORWARD, LSB first, from a byte offset -- the FSE table descriptions and nothing
   ; else. the cursor counts bits from `base`.
   (zs-fb s n base t k)
    (: (go j a p)
        (? (k <= j) (: _ (pin t 0 p) a)
           (n <= (base + (p >> 3))) (: _ (pin t 1 1) _ (pin t 0 p) a)
           (go (j + 1) (a | ((((s (base + (p >> 3))) >> (p & 7)) & 1) << j)) (p + 1)))
       (go 0 0 (peep t 0 0)))

   ; --- FSE ------------------------------------------------------------------
   ; a table description is a run of normalized counts, each read with a number of bits
   ; that SHRINKS as the budget is spent -- so the reader carries the budget, not a
   ; width. a count of zero is followed by a run length of further zeroes, in 2-bit
   ; chunks that continue while they read 3.
   ; ⚠ every helper here is a TOP-LEVEL binding of this letrec and carries its cursor in
   ; arguments, rather than a nested one closing over it. a nested letrec inside a branch
   ; grows its import row after the analyser has frozen it, and the compiler says
   ; `imports-grew` rather than miscompile -- so the shape is forced, not chosen.
   (zs-narrow rem thr nb)                  ; spend the budget: the width shrinks with it
    (? (rem < thr) (zs-narrow rem (thr >> 1) (nb - 1)) `(thr nb))
   (zs-zeros s n base t a)                 ; a run of absent symbols, 2 bits at a time
    (: v (zs-fb s n base t 2) (? (v = 3) (zs-zeros s n base t (a + 3)) (a + v)))
   (zs-one s n base t thr nb rem)          ; one count, at the current budget
    (: mx ((2 * thr) - 1 - rem)
       lo (zs-fb s n base t (nb - 1))
       (? (lo < mx) lo
          (: v (lo + ((zs-fb s n base t 1) << (nb - 1)))
             (? (thr <= v) (v - mx) v))))
   (zs-nc2 s n base t norms sym thr nb prev0 rem)
    (? (rem <= 1) `(norms (sym - 1) (// ((peep t 0 0) + 7) 8))
       (255 < sym) ()
       (peep t 1 0) ()
       (: s2 (? prev0 (sym + (zs-zeros s n base t 0)) sym)
          (? (255 < s2) ()
             ; ⚠ THE ZERO TEST IS SPELLED OUT, and it has to be: a count here may be -1
             ; (the format's "less than one"), love's truth is the SIGN, so `(! c)` calls
             ; -1 absent and starts a run of zeroes that is not there. every count after
             ; it then lands on the wrong symbol, and the table still builds.
             (: c ((zs-one s n base t thr nb rem) - 1)
                _ (pin norms s2 c)
                rem2 (rem - (? (c < 0) (0 - c) c))
                r (zs-narrow rem2 thr nb)
                (zs-nc2 s n base t norms (s2 + 1) <r <>r (? (c = 0) 1 0) rem2)))))
   (zs-ncount s n base)                    ; -> (norms maxsym accuracy bytes) | ()
    (: t (tablet 0)
       norms (tablet 0)
       al ((zs-fb s n base t 4) + 5)
       r (? (peep t 1 0) () (zs-nc2 s n base t norms 0 (1 << al) (al + 1) 0 ((1 << al) + 1)))
       (? (! r) () `(<r <>r al <>>r)))

   ; the table: low-probability symbols are laid at the top walking down, the rest are
   ; SPREAD over the remaining slots by a stride that visits each exactly once.
   (zs-spread p step mask hi) (: q ((p + step) & mask) (? (hi < q) (zs-spread q step mask hi) q))
   (zs-fill sym s c i p step mask hi)      ; c slots for symbol s, at the stride
    (? (c <= i) p
       (: _ (pin sym p s) (zs-fill sym s c (i + 1) (zs-spread p step mask hi) step mask hi)))
   (zs-fse norms maxs al)                  ; -> (sym nbb nst al)
    (: sz (1 << al)  mask (sz - 1)
       sym (cask sz) nbb (cask sz) nst (cask (sz * 2))
       nxt (tablet 0)
       hi (: (go s h)
              (? (maxs < s) h
                 (: c (peep norms s 0)
                    (? (c < 0) (: _ (pin sym h s) _ (pin nxt s 1) (go (s + 1) (h - 1)))
                       (: _ (pin nxt s c) (go (s + 1) h)))))
             (go 0 (sz - 1)))
       step ((sz >> 1) + (sz >> 3) + 3)
       _ (: (go s p)
             (? (maxs < s) 0
                (: c (peep norms s 0)
                   (? (c <= 0) (go (s + 1) p)
                      (go (s + 1) (zs-fill sym s c 0 p step mask hi)))))
            (go 0 0))
       _ (: (go u)
             (? (sz <= u) 0
                (: s (peep sym u 0)
                   nx (peep nxt s 0)
                   _ (pin nxt s (nx + 1))
                   nb (al - (zs-hb nx))
                   ns ((nx << nb) - sz)
                   _ (pin nbb u nb)
                   _ (pin nst (u * 2) (ns & 255))
                   _ (pin nst ((u * 2) + 1) ((ns >> 8) & 255))
                   (go (u + 1))))
            (go 0))
       `(sym nbb nst al))
   (zs-tsym t st) (peep <t st 0)
   (zs-tnst t st) (: c <>>t o (st * 2) ((peep c o 0) + ((peep c (o + 1) 0) << 8)))
   (zs-tnb t st) (peep <>t st 0)
   (zs-tal t) <>>>t
   (zs-norms l)                            ; a list of counts -> the tablet zs-fse takes
    (: t (tablet 0)
       (go i r) (? (two? r) (: _ (pin t i <r) (go (i + 1) >r)) t)
       (go 0 l))

   ; --- Huffman --------------------------------------------------------------
   ; the tree arrives as WEIGHTS, one per symbol: weight w means a code of
   ; (maxbits + 1 - w) bits, and 0 means the symbol is absent. the last weight is not
   ; written -- it is whatever makes the total a power of two.
   ; ⚠ the decode table is indexed by the top `maxbits` bits of the stream, so a short
   ; code owns many slots. the slots for a weight are contiguous and the weights are
   ; laid out in INCREASING order, which puts the longest codes at the bottom.
   (zs-hfill sym nbb i nb o len k)         ; a short code owns many slots; they are contiguous
    (? (len <= k) 0
       (: _ (pin sym (o + k) i) _ (pin nbb (o + k) nb) (zs-hfill sym nbb i nb o len (k + 1))))
   (zs-huftab ws nsym)                     ; ws a tablet of weights -> (sym nbb log)
    (: tot (: (go i a) (? (nsym <= i) a
                          (: w (peep ws i 0) (go (i + 1) (? w (a + (1 << (w - 1))) a))))
              (go 0 0))
       (? (! tot) ()
          ; ⚠ the deduced last weight is ALREADY in `tot` here, so the total is exactly
          ; 2^log and the log is its high bit. adding one -- which is right where the
          ; last weight is still missing -- doubles the table and decodes noise.
          (: log (zs-hb tot)
             sz (1 << log)
             rank (tablet 0)
             _ (: (go i) (? (nsym <= i) 0
                            (: w (peep ws i 0)
                               _ (? w (pin rank w ((peep rank w 0) + 1)) 0)
                               (go (i + 1))))
                  (go 0))
             _ (: (go w a) (? (log < w) 0
                              (: c (peep rank w 0) _ (pin rank w a) (go (w + 1) (a + (c << (w - 1))))))
                  (go 1 0))
             sym (cask sz) nbb (cask sz)
             _ (: (go i)
                   (? (nsym <= i) 0
                      (: w (peep ws i 0)
                         _ (? w (: len (1 << (w - 1))
                                   o (peep rank w 0)
                                   _ (zs-hfill sym nbb i (log + 1 - w) o len 0)
                                   _ (pin rank w (o + len)) 0)
                              0)
                         (go (i + 1))))
                  (go 0))
             `(sym nbb log))))
   (zs-hufend ws cnt bytes)                ; deduce the unwritten last weight
    (: tot (: (go i a) (? (cnt <= i) a
                          (: w (peep ws i 0) (go (i + 1) (? w (a + (1 << (w - 1))) a))))
              (go 0 0))
       (? (! tot) ()
          (: log ((zs-hb tot) + 1)
             rest ((1 << log) - tot)
             (? (! rest) ()
                (: _ (pin ws cnt ((zs-hb rest) + 1))
                   `(ws (cnt + 1) bytes))))))
   ; the weights themselves: either four bits apiece, or an FSE stream of their own
   ; read with TWO interleaved states.
   (zs-hufws s n base)                     ; -> (ws nsym bytes) | ()
    (: h (s base)
       (? (127 < h)
          (: cnt (h - 127)
             ws (tablet 0)
             _ (: (go i) (? (cnt <= i) 0
                            (: byte (s (base + 1 + (i >> 1)))
                               _ (pin ws i (? (i & 1) (byte & 15) (byte >> 4)))
                               (go (i + 1))))
                  (go 0))
             (zs-hufend ws cnt (1 + ((cnt + 1) >> 1))))
          (: nc (zs-ncount s n (base + 1))
             (? (! nc) ()
                (: t (zs-fse <nc <>nc <>>nc)
                   body (zs-cut s (base + 1 + <>>>nc) (h - <>>>nc))
                   bn (tally body)
                   br (zs-bstart body)
                   (? (! br) ()
                      (: al (zs-tal t)
                         ws (tablet 0)
                         s1 (zs-rb body bn br al)
                         s2 (zs-rb body bn br al)
                         ; ⚠ the two states take turns and the LAST symbol each names is
                         ; still data -- so the walk stops when the bits run out, and the
                         ; state standing at that moment has already spoken.
                         (go i a bnext)
                          (: _ (pin ws i (zs-tsym t a))
                             (? (peep br 1 0) (i + 1)
                                (: nb (zs-tnb t a)
                                   a2 ((zs-tnst t a) + (zs-rb body bn br nb))
                                   (go (i + 1) bnext a2))))
                         cnt (go 0 s1 s2)
                         (zs-hufend ws cnt (1 + h)))))))))
   ; a PEEK, not a read: the slot the bits index says how far to move, so the cursor
   ; advances after rather than before.
   (zs-peekn s n p k)
    (: (go j a q)
        (? (k <= j) a
           ((n * 8) <= q) (go (j + 1) (a << 1) (q + 1))
           (go (j + 1) ((a << 1) | (((s (n - 1 - (q >> 3))) >> (7 - (q & 7))) & 1)) (q + 1)))
       (go 0 0 p))
   (zs-push b v)
    (: c (peep b 0 0) len (peep b 1 0)
       (? (len < (tally c)) (: _ (pin c len v) _ (pin b 1 (len + 1)) 0)
          (: _ (zs-room b 1) c2 (peep b 0 0)
             _ (pin c2 len v) _ (pin b 1 (len + 1)) 0)))
   ; one Huffman stream into the buffer: peek `log` bits, the slot names the symbol and
   ; how many of those bits it actually used.
   (zs-hufrun tab body bn br b cnt)
    (: sym <tab  nbb <>tab  log <>>tab
       (go k) (? (cnt <= k) 1
                 (peep br 1 0) ()
                 (: v (zs-rb body bn br 0)                ; keep the cursor honest
                    p (peep br 0 0)
                    idx (zs-peekn body bn p log)
                    _ (pin br 0 (p + (peep nbb idx 0)))
                    _ (zs-push b (peep sym idx 0))
                    (go (k + 1))))
       (go 0))
   (zs-hufseg tab body b o k want)         ; one of the four streams, into the same sink
    (: str (zs-cut body o k) sn (tally str) br (zs-bstart str)
       (? (! br) () (zs-hufrun tab str sn br b want)))
   (zs-hufdec tab body regen streams)      ; -> the literals, a string
    (: b (zs-buf regen)
       (? (streams = 1)
          (: bn (tally body) br (zs-bstart body)
             (? (! br) () (: r (zs-hufrun tab body bn br b regen) (? r (zs-take b) ()))))
          (: seg (// (regen + 3) 4)
             s1 (zs-le body 0 2) s2 (zs-le body 2 2) s3 (zs-le body 4 2)
             tot (tally body)
             s4 (tot - 6 - s1 - s2 - s3)
             (? (s4 < 0) ()
                (: r1 (zs-hufseg tab body b 6 s1 seg)
                   r2 (? r1 (zs-hufseg tab body b (6 + s1) s2 seg) ())
                   r3 (? r2 (zs-hufseg tab body b (6 + s1 + s2) s3 seg) ())
                   r4 (? r3 (zs-hufseg tab body b (6 + s1 + s2 + s3) s4 (regen - (3 * seg))) ())
                   (? r4 (zs-take b) ()))))))

   ; --- the literals section -------------------------------------------------
   ; four kinds: the bytes as they are, one byte repeated, Huffman with its tree, and
   ; Huffman with the PREVIOUS block's tree. the last is why a tree outlives its block.
   (zs-lits s n base cx)                   ; -> (literals bytes) | ()
    (: h0 (s base)
       kind (h0 & 3)
       sf ((h0 >> 2) & 3)
       (? (kind < 2)
          (: hb (? (sf = 1) 2 (sf = 3) 3 1)
             regen (? (hb = 1) (h0 >> 3) ((zs-le s base hb) >> 4))
             (? (kind = 0) (? (n < (base + hb + regen)) () `((zs-cut s (base + hb) regen) (hb + regen)))
                (? (n <= (base + hb)) ()
                   (: c (cask regen) ch (s (base + hb))
                      (go i) (? (regen <= i) 0 (: _ (pin c i ch) (go (i + 1))))
                      _ (go 0)
                      `((string c) (hb + 1))))))
          (: hb (? (sf < 2) 3 (sf = 2) 4 5)
             w (zs-le s base hb)
             bits (? (sf < 2) 10 (sf = 2) 14 18)
             regen ((w >> 4) & ((1 << bits) - 1))
             csize ((w >> (4 + bits)) & ((1 << bits) - 1))
             streams (? (sf = 0) 1 4)
             (? (n < (base + hb + csize)) ()
                ; ⚠ a tree read here must SURVIVE the block: a treeless section is the
                ; next block saying "the one you already have".
                (: hw (? (kind = 2) (zs-hufws s n (base + hb)) ())
                   tab (? (kind = 2) (? hw (zs-huftab <hw <>hw) ()) (peep cx 'huf ()))
                   tb (? hw <>>hw 0)
                   (? (! tab) ()
                      (: _ (pin cx 'huf tab)
                         body (zs-cut s (base + hb + tb) (csize - tb))
                         lit (zs-hufdec tab body regen streams)
                         (? (! lit) () `(lit (hb + csize))))))))))

   ; --- the sequence ladders -------------------------------------------------
   zs-lbase (star-tray '(36) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
                               16 18 20 22 24 28 32 40 48 64 128 256 512 1024 2048 4096
                               8192 16384 32768 65536))
   zs-lbits (star-tray '(36) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                               1 1 1 1 2 2 3 3 4 6 7 8 9 10 11 12 13 14 15 16))
   zs-mbase (star-tray '(53) '(3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18
                               19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
                               35 37 39 41 43 47 51 59 67 83 99 131 259 515 1027 2051
                               4099 8195 16387 32771 65539))
   zs-mbits (star-tray '(53) '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                               0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                               1 1 1 1 2 2 3 3 4 4 5 7 8 9 10 11
                               12 13 14 15 16))
   ; the three distributions a stream may NAME instead of writing its own. each one is
   ; the format's, not a choice, and each sums to 2^accuracy exactly with a -1 counting
   ; as one slot -- which is the check worth making on any change here.
   zs-llnorm '(4 3 2 2 2 2 2 2 2 2 2 2 2 1 1 1 2 2 2 2 2 2 2 2 2 3 2 1 1 1 1 1 -1 -1 -1 -1)
   zs-ofnorm '(1 1 1 1 1 1 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 -1 -1 -1 -1 -1)
   zs-mlnorm '(1 4 3 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 -1 -1 -1 -1 -1 -1 -1)
   zs-llpre (zs-fse (zs-norms zs-llnorm) 35 6)
   zs-ofpre (zs-fse (zs-norms zs-ofnorm) 28 5)
   zs-mlpre (zs-fse (zs-norms zs-mlnorm) 52 6)

   ; --- the sequences section ------------------------------------------------
   (zs-nseq s n base)                      ; -> (count bytes) | ()
    (: b0 (s base)
       (? (! b0) `(0 1)
          (b0 < 128) `(b0 1)
          (b0 < 255) (? (n <= (base + 1)) () `((((b0 - 128) << 8) + (s (base + 1))) 2))
          (? (n <= (base + 2)) () `(((s (base + 1)) + ((s (base + 2)) << 8) + 32512) 3))))
   ; a mode picks where a table comes from: the predefined one, a single repeated
   ; symbol, one written here, or the one the last block left behind.
   (zs-mode s n base mode pre old)         ; -> (table bytes) | ()
    (? (mode = 0) `(pre 0)
       (mode = 3) (? old `(old 0) ())
       (mode = 1) (? (n <= base) () (: t (zs-fse (zs-norms `((s base))) (s base) 0) `(t 1)))
       (: r (zs-ncount s n base)
          (? (! r) () (: t (zs-fse <r <>r <>>r) `(t <>>>r)))))

   ; ⚠ THE THREE STATES ARE STARTED IN THE ORDER LL, OF, ML and updated in the order
   ; LL, ML, OF -- they are not the same order, and nothing downstream complains if you
   ; pick one of them for both.
   ; ⚠ and the EXTRA BITS come off in the order offset, match, literal, which is a
   ; third order again.
   (zs-run lt ot mt body cnt cx b lit)
    (: bn (tally body)
       br (zs-bstart body)
       (? (! br) ()
          (: l0 (zs-rb body bn br (zs-tal lt))
             o0 (zs-rb body bn br (zs-tal ot))
             m0 (zs-rb body bn br (zs-tal mt))
             (go k ls os ms lp r1 r2 r3)
              (? (cnt <= k)
                 (: _ (pin cx 'r1 r1) _ (pin cx 'r2 r2) _ (pin cx 'r3 r3)
                    _ (zs-blit b lit lp ((tally lit) - lp))
                    1)
                 (: lc (zs-tsym lt ls) oc (zs-tsym ot os) mc (zs-tsym mt ms)
                    ov ((1 << oc) + (zs-rb body bn br oc))
                    ml ((peep zs-mbase mc 0) + (zs-rb body bn br (peep zs-mbits mc 0)))
                    ll ((peep zs-lbase lc 0) + (zs-rb body bn br (peep zs-lbits lc 0)))
                    ; the repeat offsets: a small Offset_Value names one of the last
                    ; three rather than a distance, and an empty literal run shifts
                    ; which one it means.
                    idx (? (3 < ov) 0 (? (! ll) (ov + 1) ov))
                    dist (? (! idx) (ov - 3)
                            (idx = 1) r1
                            (idx = 2) r2
                            (idx = 3) r3
                            (r1 - 1))
                    n1 dist
                    n2 (? (idx = 1) r2 (idx = 2) r1 (idx = 3) r1 (idx = 4) r1 r1)
                    n3 (? (idx = 1) r3 (idx = 2) r3 (idx = 3) r2 (idx = 4) r2 r2)
                    _ (? ll (zs-blit b lit lp ll) 0)
                    ok (? (0 < dist) (zs-copy b dist ml) ())
                    (? (! ok) ()
                       ((tally lit) < (lp + ll)) ()
                       (: last ((k + 1) = cnt)
                          (? last (go (k + 1) ls os ms (lp + ll) n1 n2 n3)
                             (: ls2 ((zs-tnst lt ls) + (zs-rb body bn br (zs-tnb lt ls)))
                                ms2 ((zs-tnst mt ms) + (zs-rb body bn br (zs-tnb mt ms)))
                                os2 ((zs-tnst ot os) + (zs-rb body bn br (zs-tnb ot os)))
                                (go (k + 1) ls2 os2 ms2 (lp + ll) n1 n2 n3)))))))
             (go 0 l0 o0 m0 0 (peep cx 'r1 1) (peep cx 'r2 4) (peep cx 'r3 8)))))

   (zs-seqs s n base cx b lit)             ; the whole section, executed
    (: r (zs-nseq s n base)
       (? (! r) ()
          (: cnt <r
             p (base + <>r)
             (? (! cnt) (: _ (zs-blit b lit 0 (tally lit)) 1)
                (n <= p) ()
                (: modes (s p)
                   lm (modes >> 6) om ((modes >> 4) & 3) mm ((modes >> 2) & 3)
                   a (zs-mode s n (p + 1) lm zs-llpre (peep cx 'll ()))
                   (? (! a) ()
                      (: b2 (zs-mode s n (p + 1 + <>a) om zs-ofpre (peep cx 'of ()))
                         (? (! b2) ()
                            (: c (zs-mode s n (p + 1 + <>a + <>b2) mm zs-mlpre (peep cx 'ml ()))
                               (? (! c) ()
                                  (: _ (pin cx 'll <a) _ (pin cx 'of <b2) _ (pin cx 'ml <c)
                                     off (p + 1 + <>a + <>b2 + <>c)
                                     body (zs-cut s off (n - off))
                                     (zs-run <a <b2 <c body cnt cx b lit))))))))))))

   ; --- blocks and the frame -------------------------------------------------
   (zs-copy2 b ch k)                       ; an RLE block: one byte, k times
    (: _ (zs-room b k)
       c (peep b 0 0)
       (go i p) (? (k <= i) (: _ (pin b 1 p) 1) (: _ (pin c p ch) (go (i + 1) (p + 1))))
       (go 0 (peep b 1 0)))
   (zs-block s n p cx b)                   ; -> (last? next) | ()
    (? (n < (p + 3)) ()
       (: h (zs-le s p 3)
          last (h & 1)
          kind ((h >> 1) & 3)
          sz (h >> 3)
          q (p + 3)
          (? (n < (q + (? (kind = 1) 1 sz))) ()
             (kind = 0) (: _ (zs-blit b s q sz) `(last (q + sz)))
             (kind = 1) (: _ (zs-copy2 b (s q) sz) `(last (q + 1)))
             (kind = 2)
             (: body (zs-cut s q sz)
                bn (tally body)
                r (zs-lits body bn 0 cx)
                (? (! r) ()
                   (: lit <r
                      off <>r
                      ok (zs-seqs body bn off cx b lit)
                      (? ok `(last (q + sz)) ()))))
             ())))

   (zs-decompress s)
    (: n (tally s)
       (? (n < 6) ()
          (! ((zs-le s 0 4) = 4247762216)) ()               ; 0xfd2fb528
          (: d (s 4)
             fcs (d >> 6)  single ((d >> 5) & 1)  ck ((d >> 2) & 1)  did (d & 3)
             fw (? (fcs = 0) (? single 1 0) (fcs = 1) 2 (fcs = 2) 4 8)
             dw (? (did = 3) 4 did)
             hdr (5 + (? single 0 1) + dw + fw)
             (? (n < hdr) ()
                (: hint (? fw (: v (zs-le s (hdr - fw) fw) (? (fw = 2) (v + 256) v)) 0)
                   b (zs-buf (? (hint < 64) 65536 hint))
                   cx (tablet 0)
                   _ (pin cx 'r1 1) _ (pin cx 'r2 4) _ (pin cx 'r3 8)
                   (go p) (: r (zs-block s n p cx b)
                             (? (! r) ()
                                <r (? (n < (<>r + (? ck 4 0))) () 1)
                                (go <>r)))
                   ok (go hdr)
                   (? ok `(1 (zs-take b)) ()))))))

   ; --- the coder ------------------------------------------------------------
   ; ⚠ THE SEQUENCE STREAM IS WRITTEN IN THE REVERSE OF THE ORDER IT IS READ. bits go
   ; out low-first into a forward run of bytes; the decoder starts at the last byte and
   ; walks down. So the coder runs from the LAST sequence to the first, the state
   ; transitions are written before the fields they belong to, and the three states are
   ; flushed at the very end, where the reader will meet them first.
   ; ⚠ AND THE LITERALS ARE RAW HERE. writing them Huffman-coded means building a
   ; depth-limited tree, writing its weights and splitting the run into four streams --
   ; another codec's worth of work, and the honest reading of the ratio below is that
   ; it is the price of not having done it.
   (zs-wb b t v k)                         ; k low bits of v
    (: (go a n) (? (n < 8) (: _ (pin t 0 a) _ (pin t 1 n) 0)
                   (: _ (zs-push b (a & 255)) (go (a >> 8) (n - 8))))
       (go ((peep t 0 0) | ((v & ((1 << k) - 1)) << (peep t 1 0))) ((peep t 1 0) + k)))
   (zs-wclose b t)                         ; the end marker, then the tail byte
    (: _ (zs-wb b t 1 1)
       (? (peep t 1 0) (: _ (zs-push b ((peep t 0 0) & 255)) _ (pin t 0 0) _ (pin t 1 0) 0) 0))

   ; the ENCODING table: the same spread as the decoder's, read the other way round --
   ; a symbol and a state give the bits to write and the state to leave behind.
   (zs-ctt norms maxs al dnb dfs s tot)
    (? (maxs < s) 0
       (: c (peep norms s 0)
          (? (c = 0) (: _ (pin dnb s (((al + 1) << 16) - (1 << al)))
                        (zs-ctt norms maxs al dnb dfs (s + 1) tot))
             ((c < 0) | (c = 1))
              (: _ (pin dnb s ((al << 16) - (1 << al)))
                 _ (pin dfs s (tot - 1))
                 (zs-ctt norms maxs al dnb dfs (s + 1) (tot + 1)))
             (: mb (al - (zs-hb (c - 1)))
                _ (pin dnb s ((mb << 16) - (c << mb)))
                _ (pin dfs s (tot - c))
                (zs-ctt norms maxs al dnb dfs (s + 1) (tot + c))))))
   (zs-cfse norms maxs al)                 ; -> (nxt dnb dfs al)
    (: sz (1 << al)  mask (sz - 1)
       sym (cask sz)
       cum (tablet 0)
       hi (: (go s h a)
              (? (maxs < s) h
                 (: c (peep norms s 0)
                    _ (pin cum s a)
                    (? (c < 0) (: _ (pin sym h s) (go (s + 1) (h - 1) (a + 1)))
                       (go (s + 1) h (a + c)))))
             (go 0 (sz - 1) 0))
       step ((sz >> 1) + (sz >> 3) + 3)
       _ (: (go s p)
             (? (maxs < s) 0
                (: c (peep norms s 0)
                   (? (c <= 0) (go (s + 1) p) (go (s + 1) (zs-fill sym s c 0 p step mask hi)))))
            (go 0 0))
       nxt (tablet 0)
       _ (: (go u) (? (sz <= u) 0
                      (: s (peep sym u 0) o (peep cum s 0)
                         _ (pin nxt o (sz + u)) _ (pin cum s (o + 1)) (go (u + 1))))
            (go 0))
       dnb (tablet 0) dfs (tablet 0)
       _ (zs-ctt norms maxs al dnb dfs 0 0)
       `(nxt dnb dfs al))
   (zs-cinit ct sy)                        ; the state a first symbol leaves
    (: d (peep <>ct sy 0)
       nb ((d + 32768) >> 16)
       v ((nb << 16) - d)
       (peep <ct ((v >> nb) + (peep <>>ct sy 0)) 0))
   (zs-cput b t ct st sy)                  ; write the transition, answer the new state
    (: nb ((st + (peep <>ct sy 0)) >> 16)
       _ (zs-wb b t st nb)
       (peep <ct ((st >> nb) + (peep <>>ct sy 0)) 0))

   zs-llct (zs-cfse (zs-norms zs-llnorm) 35 6)
   zs-ofct (zs-cfse (zs-norms zs-ofnorm) 28 5)
   zs-mlct (zs-cfse (zs-norms zs-mlnorm) 52 6)

   ; a value's code is the last rung at or below it. the ladders are short and the
   ; common lengths sit on the flat part, so the walk starts there and only the long
   ; ones pay for the scan.
   ; ⚠ BOTH LADDERS ARE READ IN THE SAME UNITS THEY ARE WRITTEN IN. a match length's
   ; rungs start at 3, not at 0 -- the format stores `length - 3` in its own tables and
   ; the two conventions differ by exactly the minimum match, so mixing them writes a
   ; stream that is three bytes long in every match and still decodes.
   (zs-llcode v) (? (v < 16) v (: (go i) (? (i < 16) 16 ((peep zs-lbase i 0) <= v) i (go (i - 1))) (go 35)))
   (zs-mlcode v) (? (v < 35) (v - 3) (: (go i) (? (i < 32) 32 ((peep zs-mbase i 0) <= v) i (go (i - 1))) (go 52)))

   ; --- the match finder -----------------------------------------------------
   ; one slot per hash, four-byte minimum, and the same skip on a miss that
   ; doc/proto/lzo.l uses -- this is not where a zstd's ratio comes from.
   zs-dsize 65536
   (zs-c32 c i) (: o (i * 4)
                   ((peep c o 0) + ((peep c (o + 1) 0) << 8)
                    + ((peep c (o + 2) 0) << 16) + ((peep c (o + 3) 0) << 24)))
   (zs-c32! c i v) (: o (i * 4)
                      _ (pin c o (v & 255))               _ (pin c (o + 1) ((v >> 8) & 255))
                      _ (pin c (o + 2) ((v >> 16) & 255)) _ (pin c (o + 3) ((v >> 24) & 255)) 0)
   (zs-le32 s i) ((s i) + ((s (i + 1)) << 8) + ((s (i + 2)) << 16) + ((s (i + 3)) << 24))
   (zs-hash dv) (((404168861 * dv) & 4294967295) >> 16)
   (zs-mlen s n i p lim)                   ; the run shared from i and from p, from 4
    (: (go l) (? (lim <= l) l (n <= (i + l)) l ((s (i + l)) = (s (p + l))) (go (l + 1)) l)
       (go 4))

   ; the scan over one block. `lit` gathers every byte no match covered; `sq` collects
   ; the sequences in REVERSE, which is the order the coder wants them in anyway.
   (zs-scan s d lit end ip ii sq)          ; -> (sq ii)
    (? (end <= ip) `(sq ii)
       ((end - 4) < ip) `(sq ii)
       (: dv (zs-le32 s ip)
          h (zs-hash dv)
          m (zs-c32 d h)
          _ (zs-c32! d h (ip + 1))
          skip (ip + 1 + ((ip - ii) >> 6))
          (? (! m) (zs-scan s d lit end skip ii sq)
             (: p (m - 1) off (ip - p)
                (? (! (dv = (zs-le32 s p))) (zs-scan s d lit end skip ii sq)
                   (: len (zs-mlen s end ip p (end - ip))
                      run (ip - ii)
                      _ (? run (zs-blit lit s ii run) 0)
                      (zs-scan s d lit end (ip + len) (ip + len)
                               (`(run len (off + 3)) >< sq))))))))

   ; the bitstream: the last sequence starts the three states, every earlier one writes
   ; its transition and then its own extra bits, and the states go out at the end.
   (zs-wseq b t r ls os ms)
    (? (! (two? r)) `(ls os ms)
       (: e <r
          ll <e  mb <>e  ob <>>e
          lc (zs-llcode ll)  mc (zs-mlcode mb)  oc (zs-hb ob)
          os2 (zs-cput b t zs-ofct os oc)
          ms2 (zs-cput b t zs-mlct ms mc)
          ls2 (zs-cput b t zs-llct ls lc)
          _ (zs-wb b t (ll - (peep zs-lbase lc 0)) (peep zs-lbits lc 0))
          _ (zs-wb b t (mb - (peep zs-mbase mc 0)) (peep zs-mbits mc 0))
          _ (zs-wb b t (ob - (1 << oc)) oc)
          (zs-wseq b t >r ls2 os2 ms2)))
   (zs-wbits sq)                           ; sq in reverse order -> the stream, a string
    (: b (zs-buf 256)  t (tablet 0)
       e <sq
       ll <e  mb <>e  ob <>>e
       lc (zs-llcode ll)  mc (zs-mlcode mb)  oc (zs-hb ob)
       ls (zs-cinit zs-llct lc)
       os (zs-cinit zs-ofct oc)
       ms (zs-cinit zs-mlct mc)
       _ (zs-wb b t (ll - (peep zs-lbase lc 0)) (peep zs-lbits lc 0))
       _ (zs-wb b t (mb - (peep zs-mbase mc 0)) (peep zs-mbits mc 0))
       _ (zs-wb b t (ob - (1 << oc)) oc)
       f (zs-wseq b t >sq ls os ms)
       _ (zs-wb b t <>>f (zs-tal zs-mlpre))
       _ (zs-wb b t <>f (zs-tal zs-ofpre))
       _ (zs-wb b t <f (zs-tal zs-llpre))
       _ (zs-wclose b t)
       (zs-take b))

   (zs-wnseq b k)                          ; the sequence count, in its three spellings
    (? (k < 128) (zs-push b k)
       (k < 32640) (: _ (zs-push b (128 + (k >> 8))) (zs-push b (k & 255)))
       (: _ (zs-push b 255) _ (zs-push b ((k - 32512) & 255)) (zs-push b ((k - 32512) >> 8))))
   ; --- Huffman for the literals ---------------------------------------------
   ; the literals are about half of what a block spends, and raw ones spend all of it.
   ; so: count the bytes, build a tree, cap its depth at eleven, hand the shape over as
   ; WEIGHTS, and write the run backward the way the reader will walk it.
   (zs-freq lit k f i) (? (k <= i) 0
                          (: c (lit i) _ (zs-c32! f c ((zs-c32 f c) + 1)) (zs-freq lit k f (i + 1))))
   ; the two lightest live nodes. a linear sweep, because the alphabet is 256 wide and a
   ; heap would cost more to write than it saves.
   (zs-h2 wt ac top i a b)
    (? (top <= i) `(a b)
       (! (peep ac i 0)) (zs-h2 wt ac top (i + 1) a b)
       (: v (zs-c32 wt i)
          (? (a < 0) (zs-h2 wt ac top (i + 1) i b)
             (v < (zs-c32 wt a)) (zs-h2 wt ac top (i + 1) i a)
             (b < 0) (zs-h2 wt ac top (i + 1) a i)
             (v < (zs-c32 wt b)) (zs-h2 wt ac top (i + 1) a i)
             (zs-h2 wt ac top (i + 1) a b))))
   (zs-hmerge wt pa ac top)                ; merge until one node is left
    (: r (zs-h2 wt ac top 0 -1 -1)
       a <r  b <>r
       (? (b < 0) 0
          (: _ (zs-c32! wt top ((zs-c32 wt a) + (zs-c32 wt b)))
             _ (pin ac a 0) _ (pin ac b 0) _ (pin ac top 1)
             _ (zs-c32! pa a (top + 1)) _ (zs-c32! pa b (top + 1))
             (zs-hmerge wt pa ac (top + 1)))))
   (zs-hdep pa i d) (: p (zs-c32 pa i) (? p (zs-hdep pa (p - 1) (d + 1)) d))
   (zs-hhalve f i) (? (255 < i) 0
                      (: c (zs-c32 f i)
                         _ (? c (zs-c32! f i (: h (c >> 1) (? h h 1))) 0)
                         (zs-hhalve f (i + 1))))
   ; ⚠ ELEVEN IS THE CEILING the format allows a literals tree, and an optimal Huffman
   ; code over 256 symbols can want more. halving the counts and rebuilding walks the
   ; code back under it -- crudely, and it always terminates, since all-ones is a
   ; balanced tree eight deep.
   (zs-hlens f pass)                       ; -> (ws maxsym log) | ()
    (: wt (cask (511 * 4))  pa (cask (511 * 4))  ac (cask 511)
       nl (: (go i n) (? (255 < i) n
                         (: c (zs-c32 f i)
                            _ (zs-c32! wt i c) _ (pin ac i (? c 1 0))
                            (go (i + 1) (? c (n + 1) n))))
             (go 0 0))
       (? (nl < 2) ()
          (: _ (zs-hmerge wt pa ac 256)
             ws (tablet 0)
             r (: (go i mx ms)
                   (? (255 < i) `(mx ms)
                      (! (zs-c32 wt i)) (go (i + 1) mx ms)
                      (: d (zs-hdep pa i 0)
                         _ (pin ws i d)
                         (go (i + 1) (? (mx < d) d mx) i)))
                  (go 0 0 0))
             mx <r  ms <>r
             (? ((11 < mx) & (pass < 12)) (: _ (zs-hhalve f 0) (zs-hlens f (pass + 1)))
                (11 < mx) ()
                ; a depth becomes a WEIGHT: the deepest code weighs one, the shallowest
                ; weighs most, and 2^(weight-1) summed over the alphabet is 2^log.
                (: _ (: (go i) (? (255 < i) 0
                                  (: d (peep ws i 0)
                                     _ (pin ws i (? d (mx + 1 - d) 0))
                                     (go (i + 1))))
                       (go 0))
                   `(ws ms mx))))))
   (zs-hcode ws ms log)                    ; -> (code bits), the layout zs-huftab reads
    (: rank (tablet 0)
       code (cask 256)  bits (cask 256)
       _ (: (go i) (? (ms < i) 0
                      (: w (peep ws i 0)
                         _ (? w (pin rank w ((peep rank w 0) + 1)) 0)
                         (go (i + 1))))
            (go 0))
       _ (: (go w a) (? (log < w) 0
                        (: c (peep rank w 0) _ (pin rank w a) (go (w + 1) (a + (c << (w - 1))))))
            (go 1 0))
       _ (: (go i) (? (ms < i) 0
                      (: w (peep ws i 0)
                         _ (? w (: o (peep rank w 0)
                                   _ (pin code i (o >> (w - 1)))
                                   _ (pin bits i (log + 1 - w))
                                   _ (pin rank w (o + (1 << (w - 1)))) 0)
                              0)
                         (go (i + 1))))
            (go 0))
       `(code bits))

   ; --- the tree description --------------------------------------------------
   ; ⚠ THE LAST WEIGHT IS NEVER WRITTEN. it is whatever makes the total a power of two,
   ; so the description covers symbols 0..maxsym-1 and the reader deduces maxsym.
   ; ⚠ AND THE DIRECT FORM ONLY REACHES SYMBOL 128 (its count rides one byte over 127),
   ; which on binary input is almost never enough -- so the FSE form is not the exotic
   ; branch here, it is the usual one.
   (zs-wdirect b ws n)
    (: _ (zs-push b (127 + n))
       (go i) (? (n <= i) 0
                 (: hi (peep ws i 0)  lo (? ((i + 1) < n) (peep ws (i + 1) 0) 0)
                    _ (zs-push b ((hi << 4) | lo))
                    (go (i + 2))))
       (go 0))
   (zs-wflush b t)                         ; the tail byte, with no end marker
    (? (peep t 1 0) (: _ (zs-push b ((peep t 0 0) & 255)) _ (pin t 0 0) _ (pin t 1 0) 0) 0)
   ; a normalised distribution over the weight values. it is small (a dozen symbols over
   ; sixty-four slots), so proportional shares and one fix-up pass are enough; the space
   ; a better fit would win is a handful of bytes a block.
   (zs-wnorm cnt maxw tot norms)           ; -> the sum it reached
    (: (go w a) (? (maxw < w) a
                   (: c (peep cnt w 0)
                      v (? c (: q (// (c * 64) tot) (? q q 1)) 0)
                      _ (pin norms w v)
                      (go (w + 1) (a + v))))
       (go 0 0))
   (zs-wbig norms maxw i best)             ; the widest share, the one a fix-up moves
    (? (maxw < i) best
       (: v (peep norms i 0)
          (zs-wbig norms maxw (i + 1) (? ((peep norms best 0) < v) i best))))
   (zs-wfix norms maxw sum)
    (? (sum = 64) 0
       (64 < sum) (: k (zs-wbig norms maxw 0 0)
                     _ (pin norms k ((peep norms k 0) - 1))
                     (zs-wfix norms maxw (sum - 1)))
       (: k (zs-wbig norms maxw 0 0)
          _ (pin norms k ((peep norms k 0) + 1))
          (zs-wfix norms maxw (sum + 1))))
   ; the description, mirroring zs-ncount exactly: a shrinking width, and a run of
   ; absent symbols written in twos.
   (zs-wnczero b t sym start)
    (? ((start + 3) <= sym) (: _ (zs-wb b t 3 2) (zs-wnczero b t sym (start + 3)))
       (zs-wb b t (sym - start) 2))
   (zs-wskip norms maxs sym) (? (maxs < sym) sym (peep norms sym 0) sym (zs-wskip norms maxs (sym + 1)))
   (zs-wnc1 b t norms maxs sym thr nb rem prev0)
    (? (rem <= 1) 0
       (maxs < sym) 0
       (: s2 (? prev0 (zs-wskip norms maxs sym) sym)
          _ (? prev0 (zs-wnczero b t s2 sym) 0)
          (? (maxs < s2) 0
             (: c (peep norms s2 0)
                mx ((2 * thr) - 1 - rem)
                rem2 (rem - (? (c < 0) (0 - c) c))
                v0 (c + 1)
                v (? (thr <= v0) (v0 + mx) v0)
                _ (zs-wb b t v (? (v < mx) (nb - 1) nb))
                r (zs-narrow rem2 thr nb)
                (zs-wnc1 b t norms maxs (s2 + 1) <r <>r rem2 (? (v0 = 1) 1 0))))))
   ; ⚠ TWO STATES TAKE TURNS, the even weights on one and the odd on the other, and each
   ; chain's LAST weight is started rather than written -- its transition is the one the
   ; reader is meant to run out of bits on. so the walk writes transitions for 0..n-3,
   ; downward, and flushes the odd chain before the even one.
   (zs-wfsew b t ct ws n al)
    (: alast (? ((n - 1) & 1) (n - 2) (n - 1))
       blast (? ((n - 1) & 1) (n - 1) (n - 2))
       sa (zs-cinit ct (peep ws alast 0))
       sb (zs-cinit ct (peep ws blast 0))
       (go i x y) (? (i < 0) `(x y)
                     ((i & 1) = 0) (: x2 (zs-cput b t ct x (peep ws i 0)) (go (i - 1) x2 y))
                     (: y2 (zs-cput b t ct y (peep ws i 0)) (go (i - 1) x y2)))
       r (go (n - 3) sa sb)
       _ (zs-wb b t <>r al)
       _ (zs-wb b t <r al)
       (zs-wclose b t))
   (zs-wtree ws ms)                        ; the whole description, a string, or ()
    (? (ms <= 128) (: b (zs-buf 160) _ (zs-wdirect b ws ms) (zs-take b))
       (: cnt (tablet 0)
          maxw (: (go i mw) (? (ms <= i) mw
                               (: w (peep ws i 0)
                                  _ (pin cnt w ((peep cnt w 0) + 1))
                                  (go (i + 1) (? (mw < w) w mw))))
                  (go 0 0))
          used (: (go w u) (? (maxw < w) u (go (w + 1) (? (peep cnt w 0) (u + 1) u))))
          nu (used 0 0)
          (? (nu < 2) ()                   ; one weight value only: FSE has nothing to say
             (: norms (tablet 0)
                sum (zs-wnorm cnt maxw ms norms)
                _ (zs-wfix norms maxw sum)
                ct (zs-cfse norms maxw 6)
                d (zs-buf 64)  dt (tablet 0)
                _ (zs-wb d dt 1 4)         ; accuracy 6, written as 6 - 5
                _ (zs-wnc1 d dt norms maxw 0 64 7 65 0)
                _ (zs-wflush d dt)
                hd (zs-take d)
                w (zs-buf 256)  wt (tablet 0)
                _ (zs-wfsew w wt ct ws ms 6)
                bs (zs-take w)
                tot ((tally hd) + (tally bs))
                (? (127 < tot) ()          ; the header byte only reaches 127
                   (: o (zs-buf 256)
                      _ (zs-push o tot)
                      _ (zs-blit o hd 0 (tally hd))
                      _ (zs-blit o bs 0 (tally bs))
                      (zs-take o)))))))

   ; one Huffman stream: the bytes go out LAST FIRST, because the reader walks the
   ; bitstream from its far end.
   (zs-hrun b t code bits lit lo i)
    (? (i < lo) 0
       (: c (lit i) _ (zs-wb b t (peep code c 0) (peep bits c 0)) (zs-hrun b t code bits lit lo (i - 1))))
   (zs-hstream lit lo hi code bits)
    (: b (zs-buf 256)  t (tablet 0)
       _ (zs-hrun b t code bits lit lo (hi - 1))
       _ (zs-wclose b t)
       (zs-take b))
   ; four streams and the six-byte table of where they start
   (zs-h4 lit k code bits)
    (: seg (// (k + 3) 4)
       s1 (zs-hstream lit 0 seg code bits)
       s2 (zs-hstream lit seg (2 * seg) code bits)
       s3 (zs-hstream lit (2 * seg) (3 * seg) code bits)
       s4 (zs-hstream lit (3 * seg) k code bits)
       (? ((65535 < (tally s1)) | (65535 < (tally s2)) | (65535 < (tally s3))) ()
          (: b (zs-buf 256)
             _ (zs-push b ((tally s1) & 255)) _ (zs-push b ((tally s1) >> 8))
             _ (zs-push b ((tally s2) & 255)) _ (zs-push b ((tally s2) >> 8))
             _ (zs-push b ((tally s3) & 255)) _ (zs-push b ((tally s3) >> 8))
             _ (zs-blit b s1 0 (tally s1)) _ (zs-blit b s2 0 (tally s2))
             _ (zs-blit b s3 0 (tally s3)) _ (zs-blit b s4 0 (tally s4))
             (zs-take b))))
   (zs-wle b v k) (: (go i) (? (k <= i) 0 (: _ (zs-push b ((v >> (i * 8)) & 255)) (go (i + 1)))) (go 0))
   (zs-hlits lit)                          ; -> a Compressed_Literals_Block, or ()
    (: k (tally lit)
       (? (k < 64) ()                      ; below this a tree costs more than it saves
          (: f (cask (256 * 4))
             _ (zs-freq lit k f 0)
             r (zs-hlens f 0)
             (? (! r) ()
                (: ws <r  ms <>r  log <>>r
                   tree (zs-wtree ws ms)
                   (? (! tree) ()
                      (: cb (zs-hcode ws ms log)
                         one (k < 1024)
                         body (? one (zs-hstream lit 0 k <cb <>cb) (zs-h4 lit k <cb <>cb))
                         (? (! body) ()
                            (: csize ((tally tree) + (tally body))
                               (? (one & (1023 < csize)) ()
                                  (: hb (? one 3 5)
                                     sf (? one 0 3)
                                     nb (? one 10 18)
                                     (? ((! one) & ((262143 < k) | (262143 < csize))) ()
                                        (: v (2 | (sf << 2) | (k << 4) | (csize << (4 + nb)))
                                           b (zs-buf (csize + 8))
                                           _ (zs-wle b v hb)
                                           _ (zs-blit b tree 0 (tally tree))
                                           _ (zs-blit b body 0 (tally body))
                                           (zs-take b))))))))))))))

   (zs-wlits b lit)                        ; a Raw_Literals_Block and its bytes
    (: k (tally lit)
       _ (? (k < 32) (zs-push b (k << 3))
            (k < 4096) (: _ (zs-push b (4 | ((k & 15) << 4))) (zs-push b (k >> 4)))
            (: _ (zs-push b (12 | ((k & 15) << 4))) _ (zs-push b ((k >> 4) & 255))
               (zs-push b ((k >> 12) & 255))))
       (? k (zs-blit b lit 0 k) 0))

   (zs-wblock s d out i end last)          ; one block, raw when the coding did not pay
    (: lit (zs-buf (end - i))
       r (zs-scan s d lit end i i ())
       sq <r
       ii <>r
       _ (? (ii < end) (zs-blit lit s ii (end - ii)) 0)
       lits (zs-take lit)
       huf (zs-hlits lits)
       body (: c (zs-buf 256)
               _ (? (? (tally huf) ((tally huf) < (tally lits)) 0)
                    (zs-blit c huf 0 (tally huf))
                    (zs-wlits c lits))
               _ (zs-wnseq c (tally sq))
               _ (? (two? sq) (: _ (zs-push c 0) w (zs-wbits sq) (zs-blit c w 0 (tally w))) 0)
               (zs-take c))
       raw (end - i)
       k (tally body)
       _ (? (k < raw)
            (: h (last | (2 << 1) | (k << 3))
               _ (zs-push out (h & 255)) _ (zs-push out ((h >> 8) & 255))
               _ (zs-push out ((h >> 16) & 255))
               (zs-blit out body 0 k))
            (: h (last | (raw << 3))
               _ (zs-push out (h & 255)) _ (zs-push out ((h >> 8) & 255))
               _ (zs-push out ((h >> 16) & 255))
               (zs-blit out s i raw)))
       0)
   zs-bsize 65536
   (zs-blocks s n d out i)
    (: end (? (n < (i + zs-bsize)) n (i + zs-bsize))
       _ (zs-wblock s d out i end (? (end = n) 1 0))
       (? (end < n) (zs-blocks s n d out end) 0))

   (zs-compress s)
    (: n (tally s)
       out (zs-buf (? (n < 1024) 1024 (// n 2)))
       d (cask (zs-dsize * 4))
       _ (zs-push out 40) _ (zs-push out 181) _ (zs-push out 47) _ (zs-push out 253)
       _ (zs-push out 160)                 ; single segment, a 4-byte content size, no check
       _ (zs-push out (n & 255)) _ (zs-push out ((n >> 8) & 255))
       _ (zs-push out ((n >> 16) & 255)) _ (zs-push out ((n >> 24) & 255))
       _ (? n (zs-blocks s n d out 0)
            (: h 1 _ (zs-push out 1) _ (zs-push out 0) (zs-push out 0)))
       (zs-take out)))
