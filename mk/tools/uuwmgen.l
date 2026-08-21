; mk/tools/uuwmgen.l -- EMIT test/uuwm.l: the window-manager moves, compiled from
; crew/lux/core.l by mk/tools/wm2uu.l (kind-directed by crew/lux/sigs.l) and printed
; as corpus source under the model's own names. Run on the host binary:
;
;   love mk/tools/uuwmgen.l > test/uuwm.l        (or: make uuwm)
;
; test/uuwm.l is a COMMITTED artifact -- `make test_uuwm` regenerates and
; diffs, failing loudly if core.l moved without a refresh; test/uuwmlaw.l
; then proves the theorems OF THESE TERMS at corpus time on every target.
(: _ (reads (open "crew/lux/sigs.l" "r"))
   _ (reads (open "mk/tools/wm2uu.l" "r"))
   ; ⚠ OPFIX FIRST. `forms` reads the surface, where `(i - 1)` is three data
   ; elements; wm2uu matches the CORE shapes. opfix is the one door between them,
   ; so the translator reads what the compiler reads, whatever the surface wears.
   ; ⚠ NO rev: uu's defn wants a name before its uses, which is the order wm2uu-as answers
   gens (wm2uu-as (map (\ f (unglue (opfix f))) (forms (open "crew/lux/core.l" "r"))) lux-sigs ())
   _ (puts "; test/uuwm.l -- GENERATED from crew/lux/core.l by mk/tools/uuwmgen.l. Do not")
   _ (puts (+ (string 10) "; edit; refresh with `make uuwm`. The window-manager moves, compiled from"))
   _ (puts (+ (string 10) "; the SOURCE the lux runs (kind-directed by crew/lux/sigs.l) -- so the laws"))
   _ (puts (+ (string 10) "; in test/uuwmlaw.l are proved of the implementation itself; an invariant-"))
   _ (puts (+ (string 10) "; breaking core.l edit fails a PROOF at `make test`. Substrate: test/uu.l."))
   _ (puts (+ (string 10) "(assert"))
   _ (map (\ g (: _ (puts (+ (string 10) " (defn '"))
                  _ (puts (string (cap g)))
                  _ (puts (+ (string 10) "   '"))
                  _ (puts (show (cap (cup g))))
                  _ (puts (+ (string 10) "   '"))
                  _ (puts (show (cap (cup (cup g)))))
                  (puts ")")))
          gens)
   _ (puts (+ ")" (string 10)))
   0)
