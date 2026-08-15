; a map/filter/fold LIST pipeline: sum the squares-mod-p of the odd numbers in [0, N). written in the
; naive functional style -- (foldl + 0 (map f (filter odd (jot N)))) -- which under a plain interpreter
; materializes THREE intermediate lists (the jot range, the filtered odds, the mapped values). the
; glaze's DEFORESTATION pass (love/glaze/auto.l defoliate) fuses the whole pipeline into ONE native
; counted loop with NO intermediate allocation -- map substitutes the element, filter guards the step,
; foldl-over-jot lowers to the loop the group compiler natives -- so the cozy functional source runs as
; a hand-written loop would. the array-idiom counterpart is `mapfilter` (baked kernels); this is the
; LIST-pipeline idiom made fast by FUSION, not by rewriting to arrays.
;
; the `% p` in the body is deliberate: it makes the element function NON-polynomial, so the loop-closer
; (loopclose) DECLINES and this stays an honest O(n) fused loop -- the FUSION bench. the sibling
; `polysum` is the same shape with a pure-polynomial body (just the square), which the loop-closer
; collapses to O(1) -- the CLOSING bench. so the pair separates the two passes: deforest measures
; fusion (everyone O(n)), polysum measures closing (love O(1), others O(n)).
; checksum = sum_{odd k < 20000} (k*k mod 1000003) = 4891344686 (< 2^53, exact in every language).
;
; WHAT THE CROSS-LANGUAGE ROW MEASURES (read honestly): the abstraction cost of the functional-pipeline
; IDIOM, not a fixed algorithm. each language's number reflects how it handles the intermediates -- love
; FUSES them (deforestation); lua has no map/filter so a hand loop fuses them by hand; python
; is LAZY (no intermediate lists, per-element overhead); node/schemes allocate eagerly. the clean
; reading is the anchor: love's deforested pipeline lands at luajit's HAND-WRITTEN loop -- i.e. the
; high-level functional source costs what the loop costs.
(: (work n) (foldl (+) 0 (map (\ x (% (* x x) 1000003)) (filter (\ x (= 1 (% x 2))) (jot n))))
 (bench "deforest" (\ _ (work 20000))))
