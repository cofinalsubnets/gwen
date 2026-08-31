; --- test/host/hash.l: the digests in src/hash.c, held to what publishes them ---
; not in the test/*.l corpus: sha256, md5 and cksum are host nifs, so none is visible
; to the love0 bake. crew/kore's cksum, md5sum and sha256sum are these three plus a
; line of output, and test/gate/kore.sh holds those to GNU coreutils -- so this file is
; the half of the rope that does not need another implementation on the box.
;
;   vectors   RFC 1321's md5 suite and fips 180-4's sha-256 examples, verbatim. a digest
;             with no love twin has nothing else to be right against.
;   cksum     ..except this one, which gets a twin: the POSIX crc is short enough to
;             state in love, so `hash-ckwalk` below is the statement of the polynomial
;             and the C is held to it at every length 0..129. the walk is written from
;             the standard and not from the C: same poly 0x04c11db7, register the other
;             way round from crc32's, seed 0, and the byte count walked through after
;             the message, low byte first, which is why an empty file is 4294967295.
;   lengths   the block is 64 bytes and the length rides the last 8 of it, so 55/56 and
;             119/120 are where a padding off by one shows, at both digests.
;   misuse    value ops: a wrong shape answers (), it does not digest.
;   stream    <X>-init/feed/done is the same digest resumable -- all three of them --
;             and its oracle is the one-shot beside it: fed in chunks of every awkward
;             size, at every length where the padding could slip, it must answer what
;             the one-shot answers. a second spelling of the compression function is
;             exactly what this must not be, so the test is agreement and not another
;             vector. the state's width is the only thing telling the three apart,
;             so the misuse cases feed each nif the other two's cask.
;
; run:  cat test/00-init.l test/host/hash.l | out/host/love
(assert
 (lit? sha256)
 (lit? md5)
 (lit? cksum))

(: (hash-mk n f) (: b (cask n) (go i) (? (n <= i) (string b) (: _ (pin b i (f i)) (go (i + 1)))) (go 0))
   hash-alpha "abcdefghijklmnopqrstuvwxyz"
   hash-mixed ("ABCDEFGHIJKLMNOPQRSTUVWXYZ" + hash-alpha + "0123456789")
   hash-nums  (: (go k a) (? (k < 1) a (go (k - 1) (a + "1234567890"))) (go 8 ""))
   hash-56 "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
   hash-112 ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmn"
             + "hijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu")
   ; the bytes, not the text: NULs and the high half included
   (hash-bytes n) (hash-mk n (\ i (((i * 2654435761) >> 11) & 255))))

; --- md5, RFC 1321 appendix A.5 ---
(assert
 ((md5 "") = "d41d8cd98f00b204e9800998ecf8427e")
 ((md5 "a") = "0cc175b9c0f1b6a831c399e269772661")
 ((md5 "abc") = "900150983cd24fb0d6963f7d28e17f72")
 ((md5 "message digest") = "f96b697d7cb7938d525a2f31aaf161d0")
 ((md5 hash-alpha) = "c3fcd3d76192e4007dfb496cca67e13b")
 ((md5 hash-mixed) = "d174ab98d277d9f5a5611c2c9f419d9f")
 ((md5 hash-nums) = "57edf4a22be3c955ac49da2e2107b67a"))

; --- sha-256, fips 180-4 ---
(assert
 ((sha256 "") = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
 ((sha256 "abc") = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
 ((sha256 hash-56) = "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1")
 ((sha256 hash-112) = "cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1"))

; --- cksum: the love statement of the polynomial, and the C against it ---
(: hash-m32 4294967295
   (hash-ckstep c) (: c2 ((c << 1) & hash-m32)
                      (? (0 < (c & 2147483648)) (c2 ^ 79764919) c2))
   (hash-ckbyte c b) (: (go c i) (? (i < 1) c (go (hash-ckstep c) (i - 1)))
                        (go (c ^ ((b & 255) << 24)) 8))
   ; the message, then its own length low byte first, then the register complemented
   (hash-ckwalk s)
    (: n (tally s)
       (fold c m) (? (m < 1) (c ^ hash-m32) (fold (hash-ckbyte c (m & 255)) (m >> 8)))
       (go c i) (? (n <= i) (fold c n) (go (hash-ckbyte c (s i)) (i + 1)))
       (go 0 0))
   (hash-cksame s) (? ((cksum s) = (hash-ckwalk s)) 1 0)
   (hash-cklen k) (: (go i) (? (k < i) 1 !(hash-cksame (hash-bytes i)) 0 (go (i + 1)))
                     (go 0)))
(assert
 (930766865 = (cksum "123456789"))          ; what POSIX publishes for cksum
 (4294967295 = (cksum ""))                  ; the length fold: an empty message still turns it
 (hash-cklen 129)
 (hash-cksame hash-mixed)
 (hash-cksame (hash-mk 300 (\ i 0)))        ; 300 NULs: the register turns on a zero byte
 (charm? (cksum (hash-mk 300 (\ i 0)))))

; --- the lengths either side of a block and its length field ---
(: (hash-hexlen s n) (: d (? (n = 32) (md5 s) (sha256 s))
                        (go k) (? (n <= k) 1
                                  (: c (d k)
                                     (? (((47 < c) && (c < 58)) || ((96 < c) && (c < 103)))
                                        (go (k + 1)) 0)))
                        (? ((tally d) = n) (go 0) 0))
   (hash-shape ls)
    (? !(two? ls) 1
       (: s (hash-bytes <ls)
          (? (! (hash-hexlen s 32)) 0
             (! (hash-hexlen s 64)) 0
             (! (hash-cksame s)) 0
             (hash-shape >ls)))))
(assert (hash-shape '(0 1 54 55 56 57 63 64 65 118 119 120 127 128 129 200)))

; --- misuse: value ops, so a wrong shape answers () ---
(assert
 !(string? (md5 7))
 !(string? (md5 ()))
 !(string? (md5 (cask 8)))
 !(string? (sha256 7))
 !(string? (sha256 (iota 8)))
 !(charm? (cksum 7))
 !(charm? (cksum ()))
 !(charm? (cksum (cask 8))))

; --- the three resumable digests: each stream against its own one-shot ---------------
; the state is a cask the caller owns, and the width is the type -- 105 for sha-256,
; 89 for md5, 12 for cksum. a cask of any other size, or a feed of anything that is not
; a string, is a misuse and answers (); which is what stops one digest's state being
; fed to another's nif and answering a number that looks like an answer.
(: sha-st 105
   md5-st  89
   ck-st   12
   ; a text fed k bytes at a time through (width init feed done) -> the digest
   (dig-stream d s k)
    (: b (cask <d)
       _ (<>d b)
       n (tally s)
       (go i) (? (n <= i) (<>>>d b)
                 (: j (? (n < (i + k)) n (i + k))
                    _ (<>>d b (snip s i j))
                    (go j)))
       (go 0))
   dig-sha (\ _ [sha-st sha256-init sha256-feed sha256-done])
   dig-md5 (\ _ [md5-st  md5-init    md5-feed    md5-done])
   dig-ck  (\ _ [ck-st   cksum-init  cksum-feed  cksum-done])
   (sha-stream s k) (dig-stream (dig-sha 0) s k)
   (md5-stream s k) (dig-stream (dig-md5 0) s k)
   (ck-stream s k)  (dig-stream (dig-ck 0) s k)
   ; every length across both padding boundaries (55/56 and 119/120), each fed whole
   ; and in chunks that divide the block, straddle it, and outrun it
   (dig-agree? n)
    (: s (hash-bytes n)
       (go ks) (? (! (two? ks)) 1
                  (? (&& (= (sha256 s) (sha-stream s <ks))
                         (= (md5 s) (md5-stream s <ks))
                         (= (cksum s) (ck-stream s <ks)))
                     (go >ks) 0)))
   (sha-sweep lo hi)
    (: (go n) (? (hi < n) 1
                 (? (dig-agree? n '(1 3 7 63 64 65 127 128)) (go (n + 1)) 0))
       (go lo)))

(assert
 (lit? sha256-init) (lit? sha256-feed) (lit? sha256-done)
 ; the published vectors, arrived at a byte at a time
 ((sha-stream "" 1) = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
 ((sha-stream "abc" 1) = "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
 ((sha-stream hash-56 1) = (sha256 hash-56))
 ((sha-stream hash-112 7) = (sha256 hash-112))
 ; 0..130 by every chunking: the block, the two pad boundaries, and past them
 (sha-sweep 0 130)
 ; a body that outruns any one feed, at a chunk size that is not a block multiple
 ((sha-stream (hash-bytes 100000) 65536) = (sha256 (hash-bytes 100000)))
 ((sha-stream (hash-bytes 100000) 1000) = (sha256 (hash-bytes 100000)))
 ; misuse: the state is a 105-byte cask or it is nothing
 (nil? (sha256-init (cask 8)))
 (nil? (sha256-init "not a cask"))
 (nil? (sha256-feed (cask 8) "x"))
 (nil? (sha256-feed (cask sha-st) 7))
 (nil? (sha256-done (cask 8)))
 (nil? (sha256-done "not a cask"))
 ; --- md5 and cksum stream the same way ---
 (lit? md5-init) (lit? md5-feed) (lit? md5-done)
 (lit? cksum-init) (lit? cksum-feed) (lit? cksum-done)
 ((md5-stream "" 1) = "d41d8cd98f00b204e9800998ecf8427e")
 ((md5-stream "abc" 1) = "900150983cd24fb0d6963f7d28e17f72")
 ((md5-stream hash-112 7) = (md5 hash-112))
 ((md5-stream (hash-bytes 100000) 1000) = (md5 (hash-bytes 100000)))
 ; cksum folds the length in at the end, so the empty file is 4294967295 and not 0 --
 ; the one place a streamed digest could plausibly answer "nothing fed" and be wrong
 ((ck-stream "" 4) = 4294967295) ((ck-stream "" 4) = (cksum ""))
 ((ck-stream (hash-bytes 100000) 1000) = (cksum (hash-bytes 100000)))
 ; misuse, and crosswise: a state of the wrong width is refused, which is the only
 ; thing between three digests that all take a cask and a string
 (nil? (md5-init (cask sha-st))) (nil? (sha256-init (cask md5-st)))
 (nil? (cksum-init (cask sha-st))) (nil? (md5-init (cask ck-st)))
 (nil? (md5-feed (cask md5-st) 7)) (nil? (cksum-feed (cask ck-st) 7))
 (nil? (md5-done (cask 8))) (nil? (cksum-done (cask 8))))

(: _ (test_fin ())
   _ (puts "hash: ok -- md5 and sha-256 on their published vectors, the resumable sha-256
      against the one-shot, cksum against a love walk, and md5/cksum streamed too
")
   0)
