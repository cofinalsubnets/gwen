; doc/misc/proto/lzo.l -- LZO1X in love, both ways: the byte-token stream `lzo1x_1_compress`
; writes and `lzo1x_decompress` reads. A spike, weighed against lib/gz.l for the source
; blob the seed carries.
;
;   (lzo-compress s)       -> an LZO1X-1 stream, a string
;   (lzo-decompress s n)   -> (1 bytes) | ()    n a size hint, 0 when unknown
;
; ⚠ presence rides the (1 _) wrapper on the DECODE, gz.l's law: the empty input and a
; torn stream would otherwise read alike. test with two?, take with <(>r).
;
; the format is BYTES, not bits -- that is the whole of why it is fast. a token names a
; match length and part of a distance; the rest of the distance is one or two bytes
; behind it; a literal run is its length and then the bytes. there is no Huffman code,
; no tree to build and none to read, so a decode is a memcpy loop with a switch on top.
; it pays the obvious price: no entropy coding at all, so the ratio is roughly half of
; what a real deflate reaches.
;
; ⚠ THE LOW TWO BITS OF THE LAST DISTANCE BYTE ARE A LITERAL COUNT (0-3), which is the
; one thing in the format that reaches BACKWARD: a short run after a match is written
; into a byte already emitted. So the sink cannot be a jug -- it has to be indexable.
;
; ⚠ FOUR BYTES IS THE MINIMUM MATCH here, as in the reference coder: the finder tests a
; whole 32-bit word at once, and the format's shortest match (2, via M1) is left to the
; decoder to honour and never emitted.
(:
   ; --- the growing byte buffer ---------------------------------------------
   ; a cask that doubles, then one exact copy at the end -- lib/gz.l's, and for the
   ; same reason on the decode side: a match copies out of what has already been
   ; produced, so the sink must be readable too.
   (lzo-buf n) (: b (tablet 0) _ (pin b 0 (cask (? (n < 64) 64 n))) _ (pin b 1 0) b)
   (lzo-room b k)
    (: c (peep b 0 0) cap (tally c) len (peep b 1 0)
       (? ((len + k) <= cap) 0
          (: big (: (go m) (? ((len + k) <= m) m (go (m * 2))) (go cap))
             c2 (cask big)
             _ (? len (pour c2 0 (string c) 0 len) 0)
             _ (pin b 0 c2) 0)))
   (lzo-push b v)
    (: c (peep b 0 0) len (peep b 1 0)
       (? (len < (tally c)) (: _ (pin c len v) _ (pin b 1 (len + 1)) 0)
          (: _ (lzo-room b 1) c2 (peep b 0 0)
             _ (pin c2 len v) _ (pin b 1 (len + 1)) 0)))
   ; k bytes of s from i, in one move -- a literal run is the bulk of the output and
   ; pushing it a byte at a time costs three tablet reads per byte.
   ; ⚠ `pour`'s last argument is a COUNT, not an end offset, and the nif clamps it to
   ; the shorter of the two arrays. here the sink has SLACK, so a wrong count is not
   ; clamped to anything useful: it moves the whole tail, the excess is overwritten
   ; before it is read, and the walk goes quadratic while still answering right.
   (lzo-blit b s i k)
    (: _ (lzo-room b k)
       c (peep b 0 0) o (peep b 1 0)
       _ (pour c o s i k)
       _ (pin b 1 (o + k)) 0)
   (lzo-or b k v)                          ; b[len - k] |= v
    (: c (peep b 0 0) i ((peep b 1 0) - k) _ (pin c i ((peep c i 0) | v)) 0)
   (lzo-take b)
    (: c (peep b 0 0) len (peep b 1 0)
       (? len (: c2 (cask len) _ (pour c2 0 (string c) 0 len) (string c2)) ""))
   ; a match may overlap its own source (dist 1, len 100 is a hundred of one byte), so
   ; this copies byte by byte and a bulk move would be wrong. the room is taken once,
   ; before the walk, so the cask cannot be replaced under it.
   (lzo-copy b dist len)
    (: _ (lzo-room b len)
       c (peep b 0 0)
       (go k p) (? (len <= k) (: _ (pin b 1 p) 1)
                   (p < dist) (: _ (pin b 1 p) ())      ; a reach before the start
                   (: _ (pin c p (peep c (p - dist) 0)) (go (k + 1) (p + 1))))
       (go 0 (peep b 1 0)))

   ; --- 32-bit cells in a cask ----------------------------------------------
   ; the dictionary holds POSITIONS, which outgrow 16 bits on any file over 64k.
   (lzo-c32 c i) (: o (i * 4)
                    ((peep c o 0) + ((peep c (o + 1) 0) << 8)
                     + ((peep c (o + 2) 0) << 16) + ((peep c (o + 3) 0) << 24)))
   (lzo-c32! c i v) (: o (i * 4)
                       _ (pin c o (v & 255))               _ (pin c (o + 1) ((v >> 8) & 255))
                       _ (pin c (o + 2) ((v >> 16) & 255)) _ (pin c (o + 3) ((v >> 24) & 255)) 0)

   ; --- the coder ------------------------------------------------------------
   ; the format's three match shapes and their reach. M1 (length 2) is decoded and
   ; never written; M4's distances run to 0xbfff and no further.
   lzo-m2off 2048   lzo-m3off 16384   lzo-m4off 49151
   lzo-m2len 8      lzo-m3len 33      lzo-m4len 9
   ; ⚠ the dictionary is ONE SLOT PER HASH -- no chain, no second candidate. that is
   ; the coder's whole speed story and the reference's own shape: a miss is a literal,
   ; not a longer search.
   lzo-dshift 18                           ; 32 - 14, the reference's D_BITS
   lzo-dsize 16384

   (lzo-le32 s i) ((s i) + ((s (i + 1)) << 8) + ((s (i + 2)) << 16) + ((s (i + 3)) << 24))
   ; the reference's multiplicative hash. the product is taken to 32 bits FIRST: love's
   ; integers do not wrap, so the fold the hash depends on has to be written down.
   (lzo-hash dv) (((404168861 * dv) & 4294967295) >> lzo-dshift)

   ; a literal run's length, written BEFORE its bytes. three spellings, and which one
   ; is legal depends on what came before: at the head of the stream a run rides a
   ; single byte over 17, after a match a run of 1-3 rides the low bits of the last
   ; distance byte, and everything else is a length byte or a zero-marked long form.
   (lzo-runlen b k)
    (? (! (peep b 1 0)) (? (k <= 238) (lzo-push b (17 + k)) (lzo-long b k))
       (k <= 3) (lzo-or b 2 k)
       (k <= 18) (lzo-push b (k - 3))
       (lzo-long b k))
   (lzo-long b k)                          ; 0, then a 255 per zero byte, then the rest
    (: (go t) (? (255 < t) (: _ (lzo-push b 0) (go (t - 255))) (lzo-push b t))
       _ (lzo-push b 0)
       (go (k - 18)))
   (lzo-ext b t)                           ; the same run, for a match length
    (? (255 < t) (: _ (lzo-push b 0) (lzo-ext b (t - 255))) (lzo-push b t))

   (lzo-match b len off)
    (? ((len <= lzo-m2len) & (off <= lzo-m2off))
       (: o (off - 1)
          _ (lzo-push b (((len - 1) << 5) | ((o & 7) << 2)))
          (lzo-push b (o >> 3)))
       (off <= lzo-m3off)
       (: o (off - 1)
          _ (? (len <= lzo-m3len) (lzo-push b (32 | (len - 2)))
               (: _ (lzo-push b 32) (lzo-ext b (len - lzo-m3len))))
          _ (lzo-push b ((o << 2) & 255))
          (lzo-push b (o >> 6)))
       ; ⚠ M4 carries the distance's BIT 14 in the token (as bit 3), because the two
       ; distance bytes hold only fourteen of its bits. drop it and every long-range
       ; match lands 16k early.
       (: o (off - lzo-m3off)
          _ (? (len <= lzo-m4len) (lzo-push b (16 | ((o >> 11) & 8) | (len - 2)))
               (: _ (lzo-push b (16 | ((o >> 11) & 8))) (lzo-ext b (len - lzo-m4len))))
          _ (lzo-push b ((o << 2) & 255))
          (lzo-push b ((o >> 6) & 255))))

   (lzo-mlen s n i p)                      ; the run shared from i and from p, from 4
    (: (go l) (? (n <= (i + l)) l ((s (i + l)) = (s (p + l))) (go (l + 1)) l)
       (go 4))

   ; the scan proper, tail-recursive over ip. ii is where the pending literals start.
   ; ⚠ A MISS ADVANCES BY MORE THAN ONE once the literal run is long: `1 + (run >> 5)`
   ; is the reference's own skip, and it is why incompressible input costs little.
   ; ⚠ the scan stops 20 bytes short of the end, so the four-byte read at ip and the
   ; match that follows it stay inside the string.
   (lzo-scan s n b d ip ii)                ; -> ii, the tail literals' start
    (? ((n - 20) <= ip) ii
       (: dv (lzo-le32 s ip)
          h (lzo-hash dv)
          m (lzo-c32 d h)
          _ (lzo-c32! d h (ip + 1))
          skip (ip + 1 + ((ip - ii) >> 5))
          (? (! m) (lzo-scan s n b d skip ii)
             (: p (m - 1) off (ip - p)
                (? (lzo-m4off < off) (lzo-scan s n b d skip ii)
                   (! (dv = (lzo-le32 s p))) (lzo-scan s n b d skip ii)
                   (: len (lzo-mlen s n ip p)
                      run (ip - ii)
                      _ (? run (: _ (lzo-runlen b run) (lzo-blit b s ii run)) 0)
                      _ (lzo-match b len off)
                      (lzo-scan s n b d (ip + len) (ip + len))))))))

   (lzo-compress s)
    (: n (tally s)
       b (lzo-buf (? (n < 256) 256 (// n 2)))
       d (cask (lzo-dsize * 4))
       ii (lzo-scan s n b d 4 0)
       t (n - ii)
       _ (? (0 < t) (: _ (lzo-runlen b t) (lzo-blit b s ii t)) 0)
       _ (lzo-push b 17) _ (lzo-push b 0) _ (lzo-push b 0)   ; the M4 end marker
       (lzo-take b))

   ; --- the decoder ----------------------------------------------------------
   ; four arms and the jumps between them, written as tail calls: `top` reads a token
   ; at the head of the loop, `flr` is the token right after a leading literal run,
   ; `mnext` is the 1-3 literals a match's own token asked for, and `mat` is the match
   ; itself. each answers 1 at the end marker, () on a torn stream.
   ;
   ; ⚠ EVERY READ IS BOUNDS-CHECKED against n and every match against what has been
   ; produced, so a torn or hostile stream unwinds through the ordinary path. the
   ; reference has a `_safe` variant for exactly this and an unsafe one that is faster;
   ; there is no unsafe one here.
   ;
   ; ⚠ AND NOTHING IN HERE ALLOCATES. no continuation closure, no (len ip) pair handed
   ; back from a length reader -- each arm carries its own length and its own cursor in
   ; arguments and tail-calls the next. a decode holds a nine-megabyte cask that every
   ; collection copies, so a cons per token is the one cost here worth refusing.
   (lzo-lits b s n ip k)                   ; k literals, then the token behind them
    (? (n < (ip + k)) () (: _ (lzo-blit b s ip k) (lzo-flr b s n (ip + k))))
   ; the long forms of a length: a run of zero bytes, 255 apiece, then the remainder.
   ; each has its own reader because each resumes somewhere else, and a shared one
   ; would have to answer a pair.
   (lzo-litx b s n i a)
    (? (n <= i) ()
       ((s i) = 0) (lzo-litx b s n (i + 1) (a + 255))
       (lzo-lits b s n (i + 1) (a + 15 + (s i) + 3)))

   (lzo-top b s n ip)
    (? (n <= ip) ()
       (: tk (s ip)
          (? (16 <= tk) (lzo-mat b s n (ip + 1) tk)
             (! tk) (lzo-litx b s n (ip + 1) 0)
             (lzo-lits b s n (ip + 1) (tk + 3)))))
   ; the token after a leading literal run may be a match of the ordinary kinds, or the
   ; format's one odd shape: a three-byte match whose distance starts at 2049.
   (lzo-flr b s n ip)
    (? (n <= ip) ()
       (: tk (s ip)
          (? (16 <= tk) (lzo-mat b s n (ip + 1) tk)
             (n <= (ip + 1)) ()
             (: dist (2049 + (tk >> 2) + ((s (ip + 1)) << 2))
                r (lzo-copy b dist 3)
                (? (! r) () (lzo-done b s n (ip + 2) (tk & 3)))))))
   (lzo-mnext b s n ip k)                  ; the trailing literals, then a match token
    (? (n < (ip + k)) ()
       (: _ (lzo-blit b s ip k)
          i (ip + k)
          (? (n <= i) () (lzo-mat b s n (i + 1) (s i)))))
   ; ⚠ the trailing count comes from the LAST DISTANCE BYTE, not from the token -- for
   ; M2 and M1 that byte IS the token, for M3 and M4 it is the low half of the distance.
   (lzo-done b s n ip k) (? k (lzo-mnext b s n ip k) (lzo-top b s n ip))

   ; the two-byte distance M3 and M4 share, once their length is known
   (lzo-m3 b s n i len)
    (? (n < (i + 2)) ()
       (: b0 (s i)
          r (lzo-copy b (1 + (b0 >> 2) + ((s (i + 1)) << 6)) len)
          (? (! r) () (lzo-done b s n (i + 2) (b0 & 3)))))
   (lzo-m3x b s n i a) (? (n <= i) ()
                          ((s i) = 0) (lzo-m3x b s n (i + 1) (a + 255))
                          (lzo-m3 b s n (i + 1) (a + 33 + (s i))))
   ; ⚠ M4's `hi` is the distance's bit 14, carried in the token because the two
   ; distance bytes hold only fourteen of the fifteen it can need.
   (lzo-m4 b s n i len hi)
    (? (n < (i + 2)) ()
       (: b0 (s i)
          pre (hi + (b0 >> 2) + ((s (i + 1)) << 6))
          (? (! pre) 1                                   ; the end marker, and only it
             (: r (lzo-copy b (pre + lzo-m3off) len)
                (? (! r) () (lzo-done b s n (i + 2) (b0 & 3)))))))
   (lzo-m4x b s n i a hi) (? (n <= i) ()
                             ((s i) = 0) (lzo-m4x b s n (i + 1) (a + 255) hi)
                             (lzo-m4 b s n (i + 1) (a + 9 + (s i)) hi))

   (lzo-mat b s n ip tk)
    (? (64 <= tk)                          ; M2: length 3-8, distance 1-2048
       (? (n <= ip) ()
          (: r (lzo-copy b (1 + ((tk >> 2) & 7) + ((s ip) << 3)) ((tk >> 5) + 1))
             (? (! r) () (lzo-done b s n (ip + 1) (tk & 3)))))
       (32 <= tk)                          ; M3: any length, distance 1-16384
       (: t (tk & 31) (? t (lzo-m3 b s n ip (t + 2)) (lzo-m3x b s n ip 0)))
       (16 <= tk)                          ; M4: distance 16385-49151, or the end
       (: t (tk & 7) hi ((tk & 8) << 11)
          (? t (lzo-m4 b s n ip (t + 2) hi) (lzo-m4x b s n ip 0 hi)))
       ; M1: length 2, distance 1-1024. never written here, always read.
       (? (n <= ip) ()
          (: r (lzo-copy b (1 + (tk >> 2) + ((s ip) << 2)) 2)
             (? (! r) () (lzo-done b s n (ip + 1) (tk & 3))))))

   ; ⚠ the FIRST byte is its own rule: over 17 it is a leading literal run of (it - 17),
   ; and a run under four goes straight to a match token. 17 itself is not that rule --
   ; it is the end marker's own token, so the walk must not read it as an empty run.
   (lzo-decompress s hint)
    (: n (tally s)
       b (lzo-buf (? (hint < 64) 64 hint))
       r (? (! n) ()
            (17 < (s 0))
            (: k ((s 0) - 17)
               (? (k < 4) (lzo-mnext b s n 1 k) (lzo-lits b s n 1 k)))
            (lzo-top b s n 0))
       (? r [1 (lzo-take b)] ())))
