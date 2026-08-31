; --- test/host/modnif.l: ai_defn's module lane (AiModNif, src/mem.c) ---
; peepw/pinw register under module 'mem at the boot drain -- nothing lands on the
; bare book -- and (module 'mem ..) text reopens the same tablet, so an app's C
; half and .l half share one module. Runs warm on purpose: a woken image carries
; the registry, and the drain re-pins the current addresses into the found tablet.
(: m (from 'mem)
   c (cask 16)
   _ (assert
      (member? 'mem (from ()))                          ; the drain registered the module
      (tablet? m)
      (member? 'peepw (keys m)) (member? 'pinw (keys m))
      (! (member? 'peepw (names ()))) (! (member? 'pinw (names ())))) ; the bare book is clean
   _ ((m 'pinw) c 1 305419896)
   t (module 'mem (: (getw2 k i) ((from 'mem 'peepw) k i)))
   _ (assert
      (t = m)                                           ; the reopen found the C-made tablet
      (305419896 = (m 'peepw) c 1)                      ; the word lane round-trips
      (305419896 = (from 'mem 'getw2) c 1))             ; the .l member reaches the C nif
   _ (test_fin ())
   _ (say out "modnif: ok") (put out 10))
