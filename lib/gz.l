; lib/gz.l -- gzip in love, both ways: the RFC 1952 container, the RFC 1951 DEFLATE
; stream inside it, and the crc32 underneath. `(use 'gz)`, then:
;
;   (gz-crc32 s)           -> the IEEE crc32 of a string, a charm
;   (gz-deflate s)         -> raw DEFLATE bytes, one fixed-Huffman block
;   (gz-inflate s n)       -> (1 bytes) | ()    n a size hint, 0 when unknown
;   (gz-zip s nom stamp)   -> a .gz member; nom "" for no name, stamp 0 for no time
;   (gz-unzip s)           -> (1 bytes) | ()
;
; ⚠ presence rides the (1 _) wrapper on every DECODE, and it has to: the empty string
; is a legitimate member and a torn one answers nothing too, so a bare value could
; never say which happened. test with two?, take with <(>r).
;
; the CODER is greedy LZ77 into the fixed Huffman code -- one block, no dynamic code
; of our own. it costs no tree to build and no table in the stream, and it PAYS for
; that: on this tree's own sources it lands a shade behind `gzip -1` and about 24%
; behind `gzip -9` (137965 -> 49241, where -9 gets 39713 and -1 gets 47876). the gap is
; almost entirely the code the block declares, so a dynamic coder is where the next
; real win is, not a longer match chain.
; the DECODER is complete -- stored, fixed AND dynamic -- because the far side is not
; ours to choose: we must read whatever `gzip -9` wrote, and it writes dynamic.
;
; ⚠ THE THREE CONTAINERS ARE NOT INTERCHANGEABLE, and each is here for a reason a
; substitute could not serve: a `cask` is the mutable byte array (pin writes through),
; a `tray`'s pin answers a COPY so it holds only tables that never change, and a
; `tablet` is the mutable map the bit cursor and the Huffman decode tables ride. Reading
; a tray with peep is the fast path; reading a tablet spends a hash.
;
; ⚠ `+` IS DYADIC: every sum here is infix. A prefix (+ a b c) is APPLICATION, which
; church-exponentiates and answers a bignum the size of the sky (crew/tls/bytes.l pays
; the same warning).
(:
   ; --- crc32 (IEEE 802.3: reflected, polynomial 0xedb88320) -----------------
   ; built once at load into a tray, read with peep. the final xor is the same
   ; 0xffffffff the register starts at.
   (gz-crcrow n)
    (: (go k c) (? (8 <= k) c (go (k + 1) (? (c & 1) ((c >> 1) ^ 3988292384) (c >> 1))))
       (go 0 n))
   gz-crctab (star-tray '(256) (map gz-crcrow (jot 256)))
   (gz-crc32 s)
    (: n (tally s)
       (go i c) (? (n <= i) (c ^ 4294967295)
                   (go (i + 1) ((peep gz-crctab ((c ^ (s i)) & 255) 0) ^ (c >> 8))))
       (go 0 4294967295))

   ; --- the bit sink ---------------------------------------------------------
   ; DEFLATE packs bits into bytes LSB-first, but a Huffman CODE is stored
   ; MSB-first, so a code is reversed on the way in and its extra bits are not.
   ; the cursor is a tablet: 0 the accumulator, 1 how many bits it holds.
   (gz-put j t v k)
    (: (go a n) (? (n < 8) (: _ (pin t 0 a) _ (pin t 1 n) 0)
                    (: _ (say j (string (a & 255))) (go (a >> 8) (n - 8))))
       (go ((peep t 0 0) | ((v & ((1 << k) - 1)) << (peep t 1 0))) ((peep t 1 0) + k)))
   (gz-align j t)                          ; flush the tail byte, zero-padded
    (? (peep t 1 0)
       (: _ (say j (string ((peep t 0 0) & 255))) _ (pin t 0 0) _ (pin t 1 0) 0)
       0)
   (gz-rev v k) (: (go i a w) (? (k <= i) a (go (i + 1) ((a << 1) | (w & 1)) (w >> 1))) (go 0 0 v))

   ; the fixed literal/length code (RFC 1951 3.2.6) is four runs, so it is a
   ; function rather than a table -- there is nothing to look up.
   (gz-fixlit j t s)
    (? (s < 144) (gz-put j t (gz-rev (48 + s) 8) 8)
       (s < 256) (gz-put j t (gz-rev (400 + (s - 144)) 9) 9)
       (s < 280) (gz-put j t (gz-rev (s - 256) 7) 7)
                 (gz-put j t (gz-rev (192 + (s - 280)) 8) 8))

   ; --- the length and distance ladders (RFC 1951 3.2.5) ---------------------
   gz-lbase (star-tray '(29) '(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59
                               67 83 99 115 131 163 195 227 258))
   gz-lext  (star-tray '(29) '(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
   gz-dbase (star-tray '(30) '(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769
                               1025 1537 2049 3073 4097 6145 8193 12289 16385 24577))
   gz-dext  (star-tray '(30) '(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
   ; the ladders are short and walked from the top, so the first base at or below
   ; the value IS its code -- no search structure earns its keep at 30 rows.
   (gz-lcode v) (: (go i) (? (i < 1) 0 ((peep gz-lbase i 0) <= v) i (go (i - 1))) (go 28))
   (gz-dcode v) (: (go i) (? (i < 1) 0 ((peep gz-dbase i 0) <= v) i (go (i - 1))) (go 29))

   ; --- 32-bit cells in a cask ----------------------------------------------
   ; the match finder's two arrays hold POSITIONS, which outgrow a byte and outgrow
   ; 16 bits on any file over 64k. a cask is the only mutable array here, so a cell
   ; is four of its bytes.
   (gz-c32 c i) (: o (i * 4)
                   ((peep c o 0) + ((peep c (o + 1) 0) << 8)
                    + ((peep c (o + 2) 0) << 16) + ((peep c (o + 3) 0) << 24)))
   (gz-c32! c i v) (: o (i * 4)
                      _ (pin c o (v & 255))          _ (pin c (o + 1) ((v >> 8) & 255))
                      _ (pin c (o + 2) ((v >> 16) & 255)) _ (pin c (o + 3) ((v >> 24) & 255)) 0)

   ; --- the match finder -----------------------------------------------------
   ; deflate's window is 32k and its match 258 bytes, both the format's numbers.
   ; `head` maps a 3-byte hash to the most recent position + 1 (0 being "none", so
   ; the empty cell needs no sentinel of its own); `prev` chains older positions,
   ; indexed by position & the window mask, which is why a chain walk must stop at
   ; a distance past the window -- past it the cell belongs to a newer position.
   gz-wsize 32768   gz-wmask 32767   gz-hmask 32767
   gz-maxmatch 258  gz-minmatch 3
   gz-chain 32      ; how far down a chain to look before taking what we have
   (gz-hash s i) (((((s i) << 10) ^ ((s (i + 1)) << 5)) ^ (s (i + 2))) & gz-hmask)
   (gz-mlen s n i k lim)                   ; the run shared from i and from k
    (: (go l) (? (lim <= l) l
                 (n <= (i + l)) l
                 ((s (i + l)) = (s (k + l))) (go (l + 1))
                 l)
       (go 0))
   (gz-find s n i head prev)                ; -> (len dist), len 0 for no match
    (: lim (? ((n - i) < gz-maxmatch) (n - i) gz-maxmatch)
       (go k d bl bd)
        (? (! k) `(bl bd)
           (! d) `(bl bd)
           (: p (k - 1) dist (i - p)
              (? (gz-wsize < dist) `(bl bd)
                 (: l (gz-mlen s n i p lim)
                    nx (gz-c32 prev (p & gz-wmask))
                    (? (bl < l) (? (lim <= l) `(l dist) (go nx (d - 1) l dist))
                                (go nx (d - 1) bl bd))))))
       (go (gz-c32 head (gz-hash s i)) gz-chain 0 0))
   (gz-ins s n i head prev)                 ; index position i, if 3 bytes remain
    (? ((i + 2) < n)
       (: h (gz-hash s i)
          _ (gz-c32! prev (i & gz-wmask) (gz-c32 head h))
          _ (gz-c32! head h (i + 1)) 0)
       0)
   (gz-fill s n head prev i l)              ; index every position a match covered
    (: (go k) (? (l <= k) 0 (: _ (gz-ins s n (i + k) head prev) (go (k + 1)))) (go 0))

   ; the coder proper: greedy, one symbol at a time, tail-recursive over i.
   (gz-emit s n j t head prev i)
    (? (n <= i) (gz-fixlit j t 256)         ; 256 is end-of-block
       (: m (? ((i + gz-minmatch) <= n) (gz-find s n i head prev) '(0 0))
          l <m
          (? (l < gz-minmatch)
             (: _ (gz-fixlit j t (s i))
                _ (gz-ins s n i head prev)
                (gz-emit s n j t head prev (i + 1)))
             (: d (<(>m))
                lc (gz-lcode l)   dc (gz-dcode d)
                _ (gz-fixlit j t (257 + lc))
                _ (: e (peep gz-lext lc 0) (? e (gz-put j t (l - (peep gz-lbase lc 0)) e) 0))
                _ (gz-put j t (gz-rev dc 5) 5)
                _ (: e (peep gz-dext dc 0) (? e (gz-put j t (d - (peep gz-dbase dc 0)) e) 0))
                _ (gz-fill s n head prev i l)
                (gz-emit s n j t head prev (i + l))))))

   (gz-deflate s)
    (: n (tally s)
       j (jug 0)   t (tablet 0)
       head (cask (32768 * 4))   prev (cask (32768 * 4))
       _ (gz-put j t 1 1)                   ; BFINAL -- one block for the whole input
       _ (gz-put j t 1 2)                   ; BTYPE 01, the fixed code
       _ (gz-emit s n j t head prev 0)
       _ (gz-align j t)
       ((slurp j) + ""))

   ; --- the growing byte buffer ---------------------------------------------
   ; inflate writes bytes AND reads them back (a match copies out of what it has
   ; already produced), so its sink cannot be a jug -- a jug pours out, it does not
   ; index. a cask that doubles, then one exact copy at the end.
   (gz-buf n) (: b (tablet 0) _ (pin b 0 (cask (? (n < 64) 64 n))) _ (pin b 1 0) b)
   (gz-room b k)
    (: c (peep b 0 0) cap (tally c) len (peep b 1 0)
       (? ((len + k) <= cap) 0
          (: big (: (go m) (? ((len + k) <= m) m (go (m * 2))) (go cap))
             c2 (cask big)
             _ (? len (pour c2 0 (string c) 0 len) 0)
             _ (pin b 0 c2) 0)))
   ; ⚠ THE ROOM CHECK IS INLINE, and that is not tidiness: `gz-room` reads the cask
   ; and its length again for a test the pushing path has already made, and this
   ; runs once per OUTPUT BYTE -- nine and a half million of them for the source
   ; the artifact carries.
   (gz-push b v)
    (: c (peep b 0 0) len (peep b 1 0)
       (? (len < (tally c)) (: _ (pin c len v) _ (pin b 1 (len + 1)) 0)
          (: _ (gz-room b 1) c2 (peep b 0 0)
             _ (pin c2 len v) _ (pin b 1 (len + 1)) 0)))
   (gz-take b)
    (: c (peep b 0 0) len (peep b 1 0)
       (? len (: c2 (cask len) _ (pour c2 0 (string c) 0 len) (string c2)) ""))
   ; a match copies BYTE BY BYTE on purpose: deflate lets a run overlap its own
   ; source (dist 1, len 100 is a hundred of one byte), so a bulk move would be wrong.
   ; ⚠ 1 for the clean end, never 0 -- the caller asks `(! r)`, and 0 is nothing.
   ; ⚠ the ROOM IS TAKEN ONCE, before the loop, so the cask cannot be replaced
   ; under it and the walk holds `c` and the write cursor in hand -- two tablet
   ; reads for the whole match instead of nine per byte. the copy stays byte by
   ; byte: deflate lets a run overlap its own source and a bulk move would be wrong.
   (gz-copy b dist len)
    (: _ (gz-room b len)
       c (peep b 0 0)
       (go k p) (? (len <= k) (: _ (pin b 1 p) 1)
                   (p < dist) (: _ (pin b 1 p) ())         ; a reach before the start
                   (: _ (pin c p (peep c (p - dist) 0)) (go (k + 1) (p + 1))))
       (go 0 (peep b 1 0)))

   ; --- the bit source -------------------------------------------------------
   ; the cursor is a tablet again: 0 the bit position, 1 the torn flag. reading past
   ; the end sets torn and answers 0 rather than scaring, so a torn stream unwinds
   ; through the ordinary return path and the caller reads presence at the end.
   (gz-bit s n t)
    (: p (peep t 0 0)
       (? (n <= (p >> 3)) (: _ (pin t 1 1) 0)
          (: _ (pin t 0 (p + 1)) (((s (p >> 3)) >> (p & 7)) & 1))))
   ; ⚠ THE POSITION RIDES A LOCAL, read once and written back once. `gz-bit` pays a
   ; peep and a pin PER BIT because the cursor is a tablet, and a deflate stream is
   ; twenty-seven million bits -- so the cursor is taken in hand for the length of a
   ; run and put back at the end. the semantics are gz-bit's exactly, torn flag and
   ; all: past the end sets torn, answers a 0 bit, and does NOT advance.
   (gz-bits s n t k)
    (: (go i a p)
        (? (k <= i) (: _ (pin t 0 p) a)
           (: over (n <= (p >> 3))
              _ (? over (pin t 1 1) 0)
              (go (i + 1) (? over a (a | ((((s (p >> 3)) >> (p & 7)) & 1) << i)))
                  (? over p (p + 1)))))
       (go 0 0 (peep t 0 0)))

   ; --- canonical Huffman, built from code lengths --------------------------
   ; the shape is puff's: a count of codes per length and the symbols in canonical
   ; order. decoding walks one bit at a time -- no lookup table, which is slower per
   ; symbol and is the whole of the algorithm rather than an index over it.
   (gz-huff lens nsym)
    (: cnt (tablet 0)
       _ (: (go i) (? (nsym <= i) 0
                       (: l (peep lens i 0) _ (pin cnt l ((peep cnt l 0) + 1)) (go (i + 1))))
            (go 0))
       _ (pin cnt 0 0)                       ; a zero length is "no code", not a code
       ofs (tablet 0)
       _ (: (go l a) (? (15 < l) 0 (: _ (pin ofs l a) (go (l + 1) (a + (peep cnt l 0)))))
            (go 1 0))
       sym (tablet 0)
       _ (: (go i) (? (nsym <= i) 0
                       (: l (peep lens i 0)
                          _ (? l (: o (peep ofs l 0) _ (pin sym o i) _ (pin ofs l (o + 1)) 0) 0)
                          (go (i + 1))))
            (go 0))
       `(cnt sym))
   ; ..and the decode walks up to fifteen bits for ONE symbol, which is where the
   ; per-bit cursor cost lands hardest -- so it takes the position in hand too, and
   ; writes it back on every way out, the failing one included.
   (gz-dec s n t h)                          ; -> (1 symbol) | ()
    (: cnt <h sym (<(>h))
       (go l code first index p)
        (? (15 < l) (: _ (pin t 0 p) ())
           (: over (n <= (p >> 3))
              _ (? over (pin t 1 1) 0)
              c (? over code (code | (((s (p >> 3)) >> (p & 7)) & 1)))
              p2 (? over p (p + 1))
              k (peep cnt l 0)
              (? ((c - k) < first) (: _ (pin t 0 p2) `(1 (peep sym (index + (c - first)) 0)))
                 (go (l + 1) (c << 1) ((first + k) << 1) (index + k) p2))))
       (go 1 0 0 0 (peep t 0 0)))

   gz-fixlithuff (gz-huff (star-tray '(288) (map (\ i (? (i < 144) 8 (i < 256) 9 (i < 280) 7 8))
                                                 (jot 288))) 288)
   gz-fixdisthuff (gz-huff (star-tray '(30) (map (\ i 5) (jot 30))) 30)

   ; --- the three block kinds ------------------------------------------------
   (gz-stored s n t b)
    (: p (// ((peep t 0 0) + 7) 8)            ; a stored block starts at a byte
       (? (n < (p + 4)) ()
          (: len ((s p) + ((s (p + 1)) << 8))
             nl ((s (p + 2)) + ((s (p + 3)) << 8))
             (? ((((len ^ 65535) & 65535) = nl) & 1)
                (? (n < (p + 4 + len)) ()
                   (: (go k) (? (len <= k) 1 (: _ (gz-push b (s (p + 4 + k))) (go (k + 1))))
                      _ (go 0)
                      _ (pin t 0 ((p + 4 + len) * 8))
                      1))
                ()))))

   (gz-codes s n t b lit dst)                ; the symbol loop, fixed or dynamic
    (: (go u)
        (: d (gz-dec s n t lit)
           (? (! d) ()
              (peep t 1 0) ()                                  ; ran off the end
              (: sy (<(>d))
                 (? (sy < 256) (: _ (gz-push b sy) (go ()))
                    (sy = 256) 1
                    (285 < sy) ()
                    (: lc (sy - 257)
                       l ((peep gz-lbase lc 0) + (gz-bits s n t (peep gz-lext lc 0)))
                       dd (gz-dec s n t dst)
                       (? (! dd) ()
                          (: dc (<(>dd))
                             (? (29 < dc) ()
                                (: dist ((peep gz-dbase dc 0) + (gz-bits s n t (peep gz-dext dc 0)))
                                   r (gz-copy b dist l)
                                   (? (! r) () (go ())))))))))))
       (go ()))

   ; the dynamic block's own code, read as code LENGTHS through a third Huffman code
   ; -- the one place the format describes itself. 16/17/18 are runs, and 16 repeats
   ; the previous length, so the walk carries it.
   gz-clord '(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
   (gz-dynamic s n t b)
    (: hlit ((gz-bits s n t 5) + 257)
       hdist ((gz-bits s n t 5) + 1)
       hclen ((gz-bits s n t 4) + 4)
       cl (tablet 0)
       _ (: (go i o) (? (hclen <= i) 0
                         (: _ (pin cl <o (gz-bits s n t 3)) (go (i + 1) >o)))
            (go 0 gz-clord))
       ch (gz-huff cl 19)
       tot (hlit + hdist)
       lens (tablet 0)
       (rep i r v) (: (go k) (? (r <= k) 0 (: _ (pin lens (i + k) v) (go (k + 1)))) (go 0))
       (rd i prev)
        (? (tot <= i) 1
           (peep t 1 0) ()
           (: d (gz-dec s n t ch)
              (? (! d) ()
                 (: sy (<(>d))
                    (? (sy < 16) (: _ (pin lens i sy) (rd (i + 1) sy))
                       (sy = 16) (: r (3 + (gz-bits s n t 2)) _ (rep i r prev) (rd (i + r) prev))
                       (sy = 17) (: r (3 + (gz-bits s n t 3)) _ (rep i r 0) (rd (i + r) 0))
                       (sy = 18) (: r (11 + (gz-bits s n t 7)) _ (rep i r 0) (rd (i + r) 0))
                       ())))))
       ok (rd 0 0)
       (? (! ok) ()
          ; the distance lengths sit AFTER the literal ones in the same run, and
          ; gz-huff counts from 0, so they are carried down into a table of their own.
          (: dl (tablet 0)
             _ (: (go k) (? (hdist <= k) 0
                            (: _ (pin dl k (peep lens (hlit + k) 0)) (go (k + 1))))
                  (go 0))
             lit (gz-huff lens hlit)
             dst (gz-huff dl hdist)
             (gz-codes s n t b lit dst))))

   (gz-block s n t b)                         ; -> (1 last?) | ()
    (: fin (gz-bit s n t)
       typ (gz-bits s n t 2)
       r (? (typ = 0) (gz-stored s n t b)
            (typ = 1) (gz-codes s n t b gz-fixlithuff gz-fixdisthuff)
            (typ = 2) (gz-dynamic s n t b)
            ())
       (? r `(1 fin) ()))

   (gz-inflate s hint)
    (: n (tally s)
       t (tablet 0)   b (gz-buf hint)
       (go u) (: r (gz-block s n t b)
                 (? (! r) ()
                    (peep t 1 0) ()
                    (<(>r)) `(1 (gz-take b))
                    (go ())))
       (? n (go ()) ()))

   ; --- the container (RFC 1952) --------------------------------------------
   (gz-le32s v)
    (: b (cask 4)
       _ (pin b 0 (v & 255))          _ (pin b 1 ((v >> 8) & 255))
       _ (pin b 2 ((v >> 16) & 255))  _ (pin b 3 ((v >> 24) & 255))
       (string b))
   (gz-zip s nom stamp)
    (: txt (s + "")
       named (? (tally nom) 1 0)
       h (cask 10)
       _ (pin h 0 31) _ (pin h 1 139) _ (pin h 2 8) _ (pin h 3 (? named 8 0))
       _ (pin h 4 (stamp & 255))        _ (pin h 5 ((stamp >> 8) & 255))
       _ (pin h 6 ((stamp >> 16) & 255)) _ (pin h 7 ((stamp >> 24) & 255))
       _ (pin h 8 0) _ (pin h 9 3)               ; XFL none, OS 3 = unix
       ((string h) + (? named (nom + (string 0)) "") + (gz-deflate txt)
        + (gz-le32s (gz-crc32 txt)) + (gz-le32s ((tally txt) & 4294967295))))

   ; the header's optional fields are four flags, and every one of them must be
   ; STEPPED OVER even though we write none of them -- the far side is gzip, and
   ; `gzip -N` writes a name. FTEXT (bit 0) carries no bytes.
   (gz-skip s n i)                            ; step a NUL-ended field
    (? (n <= i) () ((s i) = 0) (i + 1) (gz-skip s n (i + 1)))
   (gz-unzip s)
    (: n (tally s)
       (? (n < 18) ()
          (! (((s 0) = 31) & ((s 1) = 139) & ((s 2) = 8)))  ()
          (: flg (s 3)
             i0 10
             i1 (? (flg & 4) (: x ((s i0) + ((s (i0 + 1)) << 8)) (i0 + 2 + x)) i0)   ; FEXTRA
             i2 (? (flg & 8) (gz-skip s n i1) i1)                                  ; FNAME
             i3 (? (! i2) () (flg & 16) (gz-skip s n i2) i2)                        ; FCOMMENT
             i4 (? (! i3) () (flg & 2) (i3 + 2) i3)                                   ; FHCRC
             (? (! i4) ()
                (n < (i4 + 8)) ()
                (: isize ((s (n - 4)) + ((s (n - 3)) << 8)
                          + ((s (n - 2)) << 16) + ((s (n - 1)) << 24))
                   crc ((s (n - 8)) + ((s (n - 7)) << 8)
                        + ((s (n - 6)) << 16) + ((s (n - 5)) << 24))
                   body (: c (cask (n - i4 - 8)) _ (pour c 0 s i4 (n - 8)) (string c))
                   r (gz-inflate body isize)
                   (? (! r) ()
                      (: out (<(>r))
                         ; the trailer is the whole point of the container: a stream
                         ; that inflated cleanly and hashes wrong is a CORRUPT one,
                         ; and saying so is the only thing the crc is for.
                         (? (! ((gz-crc32 out) = crc)) ()
                            (! ((tally out) = (isize & 4294967295))) ()
                            `(1 out))))))))))