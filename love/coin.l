; coin -- ring and monoid, the two ready-made dice. the mechanism is C
; (coin/coin?/die-of/load, the KHot lane in +/*); this file is only the constructor.
; love/prel.l first -- a die is a {..} tablet.
; (ring 'z5 plus times) / (monoid 'z5 plus) -> a constructor: payload -> coin.
; plus/times take payloads; net/=/</show default over the payload in the C core.
;
; the die's slots, read by the C side:
;   0 show-name  1 plus  2 times  5 `-`  6 net mode  7 stars  8 `/`
; an unpinned slot answers (), the undefined-op law -- so a monoid, pinning no 2, has no *.
(module 'coin
(: (ring name + *) (:
    d {0 name}
    t *                                                    ; bound: a bare * would read as prod
    _ (pin d 1 (\ a b (coin d (load a + load b))))
    _ (? t (pin d 2 (\ a b (coin d (load a * load b)))) 0) ; a monoid has no times
    (coin d))                                              ; curried: payload -> coin
   (monoid name plus) (ring name plus ())))
