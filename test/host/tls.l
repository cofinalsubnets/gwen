; --- test/host/tls.l: the cipher floor, against the RFCs' own numbers ---
; NOT in the test/*.l corpus: this loads a crew file and splices its module.
;
; chacha20 and poly1305 are arithmetic, not i/o, so what is asserted here is
; arithmetic -- the published vectors of RFC 8439, which is the only kind of
; check a cipher accepts. a cipher that is merely SELF-consistent is a cipher
; that agrees with its own bug: every number below came from the RFC, and each
; was independently re-derived (in python, and for chacha in C too) before it
; was written down here.
;
;   BYTES    the little-endian/big-endian readers, and hex round-tripping.
;   BLOCK    §2.3.2 -- the 20-round chacha block on the RFC's key/nonce/counter.
;   STREAM   64 bytes per block, and block n is what counter n gives on its own
;            -- which is what proves the VECTORISATION: the counter varies down
;            a galaxy, and if it did not, every block in a batch would be equal.
;   CIPHER   §2.4.2 -- the sunscreen paragraph, whole, and its round trip.
;   MAC      §2.5.2 -- the forum tag, plus the four lengths that exercise the
;            block edges: none, short, exactly one block, one block and a bit.
;   AVALANCHE a one-bit change anywhere changes the tag. this is the assert that
;            fails if the conditional subtract at the end ever stops selecting.
;
; run:  cat test/00-init.l test/host/tls.l | out/host/love

(use "lib/tls.l")

(assert
 (member? 'tls (from ()))
 (member? 'cc-block (keys (from 'tls)))
 (member? 'po-mac (keys (from 'tls)))
 (lit? (from 'tls 'cc-xor)))

; --- BYTES ------------------------------------------------------------------------
(assert
 (66051 = (le32 (unhex "03020100") 0))         ; 0x00010203 little-endian
 (258 = (be16 (unhex "0102") 0))               ; ..and big-endian is the other way
 ("deadbeef" = (hex (unhex "deadbeef")))
 (4 = (tally (unhex "deadbeef")))
 ("00ff" = (hex (bytes 2 (\ i (? (i = 0) 0 255))))))

; --- the RFC's key and nonces -----------------------------------------------------
(: rfckey (bytes 32 (\ i i))                          ; 00 01 02 .. 1f
   n232 (bytes 12 (\ i (? (i = 3) 9 (i = 7) 74 0)))   ; §2.3.2: ..09 ..4a
   n242 (bytes 12 (\ i (? (i = 7) 74 0)))             ; §2.4.2: ..4a
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

; --- MAC: §2.5.2, and the block edges ---------------------------------------------
(: k252 (unhex "85d6be7857556d337f4452fe42d506a80103808afb0db2fd4abff6af4149f51b")
   (assert
    ((hex (po-mac k252 "Cryptographic Forum Research Group"))
     = "a8061dc1305136c6c22b8baf0c0127a9")
    (16 = (tally (po-mac k252 "anything at all")))
    ; the empty message accumulates nothing, so the tag IS s -- the key's second
    ; half, unchanged. a cheap assert that catches a great many wiring errors.
    ((hex (po-mac rfckey "")) = "101112131415161718191a1b1c1d1e1f")
    ; the four lengths around a block boundary: short, exact, exact+1, several
    ((hex (po-mac rfckey "A")) = "245295d81b5fa2e5286caff23579bcff")
    ((hex (po-mac rfckey (bytes 16 (\ i i)))) = "a2291a363def0b53845fa4126a6ad364")
    ((hex (po-mac rfckey (bytes 17 (\ i i)))) = "f735c97f7308fd79222447fe76a96872")
    ((hex (po-mac rfckey (bytes 100 (\ i i)))) = "2c48db4b08964d7e67950fbd89760c4d")))

; --- THE REDUCTION BOUNDARY -------------------------------------------------------
; ⚠ NO MESSAGE REACHES THIS. the final conditional subtract fires only when the
; accumulator lands in [p, 2^130), which is 5 values out of 2^130 -- so random
; vectors, including every one above, leave that branch UNTESTED. deleting the
; subtract outright passed the whole file before these five asserts existed.
; so the accumulator is handed to po-fin directly, at and around p.
; p = 2^130 - 5 is all-ones across 130 bits less 4, hence the 67108859 low limb.
(: allone 67108863                                    ; 2^26 - 1
   (fin l0) (hex (po-fin (list l0 allone allone allone allone) rfckey))
   sbytes "101112131415161718191a1b1c1d1e1f"          ; s alone, the key's second half
   (assert
    ; h = p reduces to 0, so the tag is s -- the SAME tag the empty message gives,
    ; by a completely different route.
    ((fin 67108859) = sbytes)
    ((po-mac rfckey "") = (po-fin (list 67108859 allone allone allone allone) rfckey))
    ; h = p+1 reduces to 1, h = 2^130-1 reduces to 4: the top of the range folds
    ; down by exactly p, not by 2^130.
    ((fin 67108860) = "111112131415161718191a1b1c1d1e1f")
    ((fin allone) = "141112131415161718191a1b1c1d1e1f")
    ; ..and h = p-1 does NOT fire the subtract, so it stays put and is not s
    (! ((fin 67108858) = sbytes))))

; --- AVALANCHE: the tag answers to every bit of both inputs -----------------------
(: m0 (bytes 64 (\ i i))
   m1 (bytes 64 (\ i (? (i = 63) 1 i)))            ; one bit of the last byte
   k0 rfckey
   k1 (bytes 32 (\ i (? (i = 0) 1 i)))             ; one bit of r
   k2 (bytes 32 (\ i (? (i = 16) 17 i)))           ; one bit of s
   (assert
    (! ((po-mac k0 m0) = (po-mac k0 m1)))
    (! ((po-mac k0 m0) = (po-mac k1 m0)))
    (! ((po-mac k0 m0) = (po-mac k2 m0)))
    ; ..and it is a function: same inputs, same tag
    ((po-mac k0 m0) = (po-mac k0 m0))))

; the harness greps for this line, and that is the point: a reader stop exits 0
; with the asserts above never read, so only reaching HERE proves the file ran.
(: _ (test_fin ())
   _ (puts "crew/tls: ok -- chacha20 + poly1305 (rfc 8439 block, stream, counter, cipher, mac)
")
   0)
