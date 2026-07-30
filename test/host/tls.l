; --- test/host/tls.l: the cipher floor, against the RFC's own numbers ---
; NOT in the test/*.l corpus: this loads a crew file and splices its module.
;
; chacha20 is arithmetic, not i/o, so what is asserted here is arithmetic --
; the published vectors of RFC 8439, which is the only kind of check a cipher
; accepts. a cipher that is merely SELF-consistent is a cipher that agrees with
; its own bug: every number below came from the RFC, and the ciphertext was
; independently re-derived before it was written down.
;
;   BLOCK    §2.3.2 -- the 20-round block function on the RFC's key/nonce/counter.
;   STREAM   the keystream is 64 bytes per block, and block n is what counter n
;            produces on its own -- so a batch is its blocks, not a fourth thing.
;   CIPHER   §2.4.2 -- the sunscreen paragraph, whole, and its round trip.
;   COUNTER  a batch of 4 agrees with 4 batches of 1, which is what proves the
;            vectorisation: the counter varies DOWN a galaxy, and if it did not,
;            every block in a batch would be identical and this assert would say so.
;
; run:  cat test/00-init.l test/host/tls.l | out/host/love

(use 'tls)

(assert
 (member? 'tls (from ()))
 (member? 'cc-block (from 'tls 'keys))
 (lit? (from 'tls 'cc-xor)))

; --- the RFC's key and nonces -----------------------------------------------------
(: (bytes n f)                                  ; an n-byte string, byte i = (f i)
    (: b (cask n)
       (go i) (? (n <= i) (string b) (: _ (pin b i (f i)) (go (i + 1))))
       (go 0))
   rfckey (bytes 32 (\ i i))                    ; 00 01 02 .. 1f
   n232 (bytes 12 (\ i (? (i = 3) 9 (i = 7) 74 0)))   ; §2.3.2: ..09 ..4a
   n242 (bytes 12 (\ i (? (i = 7) 74 0)))             ; §2.4.2: ..4a
   (hex s) (: d "0123456789abcdef" n (tally s)
              (go i a) (? (n <= i) a
                          (go (i + 1) (a + (d (>> (s i) 4)) + (d (& (s i) 15)))))
              (go 0 ""))
   (wd out j i) (peep (peep out j 0) i 0))

; --- BLOCK: §2.3.2, the first four words of the 20-round block ---------------------
(: b232 (cc-block (cc-init rfckey n232 1 1))
   (assert
    (3840405776 = (wd b232 0 0)) (358169553 = (wd b232 1 0))
    (534581072 = (wd b232 2 0))  (3295748259 = (wd b232 3 0))))

; --- STREAM: one block is 64 bytes, and a batch is its blocks ---------------------
(assert
 (64 = (tally (cc-stream rfckey n232 1 1)))
 (256 = (tally (cc-stream rfckey n232 1 4)))
 ; the counter varies DOWN the galaxy: block 2 of a 4-batch IS the 1-batch at
 ; counter 2. this is the assert that would fail if the vectorisation collapsed.
 ((snip (cc-stream rfckey n232 1 4) 64 128) = (cc-stream rfckey n232 2 1))
 ((snip (cc-stream rfckey n232 1 4) 192 256) = (cc-stream rfckey n232 4 1))
 ; ..and distinct blocks are distinct, so the above is not two copies of nothing
 (! ((cc-stream rfckey n232 1 1) = (cc-stream rfckey n232 2 1))))

; --- CIPHER: §2.4.2, the sunscreen paragraph ---------------------------------------
(: pt242 ("Ladies and Gentlemen of the class of '99: If I could offer you only "
          + "one tip for the future, sunscreen would be it.")
   ct242 (cc-xor rfckey n242 1 pt242)
   (assert
    (114 = (tally ct242))
    ((hex ct242) = ("6e2e359a2568f98041ba0728dd0d6981e97e7aec1d4360c20a27afccfd9fae0b"
                    + "f91b65c5524733ab8f593dabcd62b3571639d624e65152ab8f530c359f0861d8"
                    + "07ca0dbf500d6a6156a38e088a22b65e52bc514d16ccf806818ce91ab7793736"
                    + "5af90bbf74a35be6b40b8eedf2785e42874d"))
    ; xor is its own inverse, so decryption is the same act
    (pt242 = (cc-xor rfckey n242 1 ct242))
    ; ..and the keystream is what did it
    (! (ct242 = pt242))))

; the harness greps for this line, and that is the point: a reader stop exits 0
; with the asserts above never read, so only reaching HERE proves the file ran.
(: _ (puts "crew/tls: ok -- chacha20 (rfc 8439 block, stream, counter, cipher)
")
   0)
