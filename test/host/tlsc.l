; --- test/host/tlsc.l: the C twins, held to the love ciphers ---
; not in the test/*.l corpus: chacha20/poly1305 are host nifs (src/tls.c) and
; the love side is a crew module, so neither is visible to the love0 bake.
;
; test/host/tls.l is where the RFC's own numbers live; this file asks the other
; question -- do the two implementations of each cipher answer the same bytes?
; the love files are the oracle (crew/tls/chacha.l says so in its head), so a
; disagreement is the C being wrong until proven otherwise.
;
;   vectors   the RFC's §2.4.2 ciphertext and §2.5.2 tag, off the C side, so a
;             twin that agrees with a love file that has drifted still fails.
;   lengths   every message length 0..129 -- both block edges of both ciphers
;             (chacha's 64, poly's 16) crossed twice, and the short final block
;             padded at every offset inside it.
;   counters  the block counter carried into the middle of a batch: this is what
;             love vectorises down a galaxy and C walks in a loop, the one place
;             the two shapes differ.
;   misuse    a wrong key or nonce length answers (), it does not encrypt.
;
; poly1305's conditional subtract is unreachable from a message (5 values out
; of 2^130), so it is untested here by construction: test/host/tls.l reaches it
; by handing po-fin an accumulator directly, and the C twin has no such door.
; if that ever matters, the door to open is C's, not this file's.
;
; run:  cat test/00-init.l test/host/tlsc.l | out/host/love

(use "lib/tls.l")

(assert
 (lit? chacha20)
 (lit? poly1305))

; the fixtures, leaked on purpose (a body-less `:`) so every assert below shares
; them -- the shape test/host/tls.l wears.
(: ckey (bytes 32 (\ i i))
   cnon (bytes 12 (\ i (? (i = 7) 74 0)))            ; §2.4.2's nonce: ..4a
   (cmsg n) (bytes n (\ i (& ((i * 7) + 11) 255)))   ; a fixed pseudo-random run
   ; the first length at which the twins disagree, or -1 if none does. a bare
   ; false would say "somewhere"; this names the block edge it broke on.
   (cbad i n) (? (n < i) -1
                 (: m (cmsg i)
                    (? (! ((chacha20 ckey cnon 1 m) = (cc-xor ckey cnon 1 m))) i
                       (! ((poly1305 ckey m) = (po-mac ckey m))) i
                       (cbad (i + 1) n))))
   (cctrbad c n) (: big (cmsg 512)                   ; eight blocks, so it carries
                    (? (n < c) -1
                       (! ((chacha20 ckey cnon c big) = (cc-xor ckey cnon c big))) c
                       (cctrbad (c + 1) n))))

; --- vectors: the C side against the RFC, not merely against love ------------------
(: pt242 ("Ladies and Gentlemen of the class of '99: If I could offer you only "
          + "one tip for the future, sunscreen would be it.")
   k252 (unhex "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b")
   (assert
    ((hex (chacha20 ckey cnon 1 pt242))
     = ("6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0b"
        + "f91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d8"
        + "07ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab7793736"
        + "5af90bbf74a35be6b40b8eedf2785e42874d"))
    ((hex (poly1305 k252 "Cryptographic Forum Research Group"))
     = "a8061dc1305136c6c22b8baf0c0127a9")
    ; xor is its own inverse in C too, so the twin decrypts what it encrypted
    (pt242 = (chacha20 ckey cnon 1 (chacha20 ckey cnon 1 pt242)))))

; --- lengths: the two twins agree at every length across both block edges ----------
(assert
 (-1 = (cbad 0 129))
 ; ..and the run is not two piles of nothing: lengths differ, counters differ
 (! ((poly1305 ckey (cmsg 32)) = (poly1305 ckey (cmsg 33))))
 (! ((chacha20 ckey cnon 1 (cmsg 64)) = (chacha20 ckey cnon 2 (cmsg 64)))))

; --- counters: love varies the counter down a galaxy, C walks it ------------------
(assert
 (-1 = (cctrbad 0 5))
 ; block 1 of a batch is what counter 2 gives on its own -- love's vectorised
 ; galaxy and C's loop, meeting on the same 64 bytes
 ((snip (chacha20 ckey cnon 1 (cmsg 512)) 64 128)
  = (cc-xor ckey cnon 2 (snip (cmsg 512) 64 128)))
 ; a counter step really moves the keystream, so the above compares something
 (! ((chacha20 ckey cnon 1 (cmsg 512)) = (chacha20 ckey cnon 3 (cmsg 512)))))

; --- misuse: a value op, so a wrong shape answers () and encrypts nothing ----------
(: shortk (bytes 31 (\ i i))
   longn (bytes 16 (\ i i))
   (assert
    (! (chacha20 shortk cnon 1 "hello"))
    (! (chacha20 ckey longn 1 "hello"))
    (! (chacha20 ckey cnon 'x "hello"))
    (! (chacha20 ckey cnon -1 "hello"))
    (! (poly1305 shortk "hello"))
    (! (poly1305 ckey 'x))
    ; ..and the empty message is a real answer, not a misuse: a 16-byte tag
    (16 = (tally (poly1305 ckey "")))
    ((poly1305 ckey "") = (po-mac ckey ""))))

(: _ (test_fin ())
   _ (puts "crew/tls: ok -- the C twins agree with love (vectors, lengths, counters, misuse)
")
   0)
