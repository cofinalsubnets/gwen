; bench/ccgen.l -- lays the inflate row's input for ccbench.sh.
;
;   love ccgen.l SRC OUT    writes SRC deflated into OUT, prints SRC's byte count
;
; ⚠ IT RUNS THROUGH THE ALREADY-BUILT out/host/love, never through a lane binary. The
; three binaries under test live in out/bench/cc with no module path, so lib/gz.l is
; out of their reach and a DEFLATE stream cannot be made where it is used -- it has to
; arrive as a file, and the size has to arrive with it (the nif is handed the exact
; inflated length, and a blind call would time a counting pass as well).
(: _ (use 'gz)
   av >argv
   _ (? (2 <= tally av) 0 (: _ (say err "ccgen: usage: ccgen.l SRC OUT\n") (quit 1)))
   q (open <av "r")
   _ (? q 0 (: _ (say err ("ccgen: cannot read " + <av + "\n")) (quit 1)))
   s ((slurp q) + "")
   _ (close q)
   w (open <>av "w")
   _ (? w 0 (: _ (say err ("ccgen: cannot write " + <>av + "\n")) (quit 1)))
   _ (say w (gz-deflate s))
   _ (close w)
   _ (puts (show (tally s)))
   0)
