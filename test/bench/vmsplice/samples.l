; samples.l -- the closures the composer runs on, shared by compose.l (dis + emit) and
; check.l (the same source compiled by ev IS the differential twin). ⚠ run both under
; LOVE_NO_GLAZE=1: a glazed sample is a native cell, and dis would read its husk.
(: (sm-cup4 x) (cup (cup (cup (cup x))))
   (sm-pred x) (two? (cup (cup x)))
   (sm-caps x) (cap (cup (cap (cup x))))
   (sm-deep x) (cup (cup (cup (cup (cup (cup (cup (cup x))))))))
   samples (L ('sm-cup4 >< sm-cup4) ('sm-pred >< sm-pred)
              ('sm-caps >< sm-caps) ('sm-deep >< sm-deep)))
