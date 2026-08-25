; coin -- the two ready-made dice, ring and monoid. a POST-EGG LAYER, not part of the
; language: the COIN MECHANISM is C (coin/coin?/die-of/load, the KHot lane in +/*), and this
; file is only the ergonomic constructor over it, so prel stays minimal and each frontend
; includes the layers it wants (the host, love0, wasm and the K_TEST kernel do -- the corpus
; asserts on both; a shipped kernel and the playdate workbench carry neither). love/prel.l
; first -- the die is a {..} tablet.
;
; a COIN (coin?) is struck from a die over a payload; the die is the type descriptor (a
; monoid/ring -- a map of slot->method); die-of reads it back. + and * dispatch through the
; KHot lane to the die's plus/times, run over the LOADED payloads and re-struck; () stays their
; identity for free (the mint-identity hoist sits above the +/* matrix). net/=/</show default
; over the payload in the C core, so $/truth, value-equality, sort and the (name payload) show
; form all work with no extra ceremony. plus/times take payloads.
; (ring 'z5 plus times) / (monoid 'z5 plus) -> a constructor: payload -> coin.
; `-` rides slot 5 and `/` slot 8 for a die that pins them (ring/monoid pin none; love/q.l pins
; both): unpinned, they answer (), the undefined-op law. slot 6 is the NET MODE, a fixnum:
; absent/0 nets the payload (the hom); 1 nets by TALLY -- the count, never negative -- so a die
; over signed content (a ledger, a polynomial) can make its coins' truth read "has any", not
; "sums green". slot 7 truthy declares the die's coins STARS: numeral application powers them
; through their own * (num-ap's open fork) instead of composing them as functions. slot 0 is
; the show-name.
(module 'coin
(: (ring name plus times) (:
      d {0 name}                                                 ; the die; slot 0 = show-name
      _ (pin d 1 (\ a b (coin d (plus (load a) (load b)))))      ; slot 1 = +
      _ (? times (pin d 2 (\ a b (coin d (times (load a) (load b))))) 0)  ; slot 2 = * (a monoid has none)
      (\ p (coin d p)))                                          ; the ctor: payload -> coin
   (monoid name plus) (ring name plus 0))                        ; a monoid is a ring with no product

) ; module coin
