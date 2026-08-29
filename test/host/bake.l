; --- test/host/bake.l: smoke test for the bake nif (src/image.c + ai_image_save_) ---
; (bake path) snapshots the LIVE session to an image file MID-EVAL -- the running
; stack rides along as wake-unreachable ballast, live finalizer nodes forge into
; dead chains, and a NULL bio-buffer word encodes as an immortal -- and `love wake
; PATH prog.l` boots a session carrying every global pinned before the bake.
; host-only (the nif + hark + wake), so it runs standalone against the built
; binary like test/host/fs.l:
;   cat test/00-init.l test/host/bake.l | out/host/love
; the marker the woken child reads back. its own TOP-LEVEL body-less `:`, so it leaks to the
; session scope and the snapshot carries it -- `book` is off the book on every boot now, and
; a binding inside the body-having `:` below would be local to that form.
(: bake-smoke-marker 4242)

; the dump hash-conses equal chains and strings (doc/misc/snapshot.md). these ride the
; snapshot as top-level globals like the marker above, and the woken child asks what
; merged: the two lists are one value, and the two casks -- equal bytes, but written
; THROUGH their holder -- are not, or a write to either would land on both.
(: bake-dedup-l1 [1 2 3 4 5]
   bake-dedup-l2 [1 2 3 4 5]
   bake-dedup-c1 (cask 4)
   bake-dedup-c2 (cask 4))

; a native rides the bake: its code is bytes in the image's segment, woken as arena text,
; so the child runs it without a compile
(: bake-native (ev '(\ n ((: (go i a) (? (< i n) (go (+ i 1) (+ a i)) a)) 0 0))))

(: img "out/.bake-nif-smoke.image"
   drv "out/.bake-nif-drive.l"
   _ (unlink img) _ (unlink drv)                    ; a clean slate, tolerated if absent

   ; the bake answers 1 and the session RIDES ON (this very assert runs post-dump);
   ; note the file port feeding THIS script is live at the dump -- the fz leg.
   _ (assert ((bake img) = 1))

   ; the woken child reads the marker this session pinned before its bake
   _ (: p (open drv "w") _ (say p "(: _ (putn bake-smoke-marker) (quit 0))") (close p))
   r (hark ["out/host/love" "wake" img drv])
   _ (assert (two? r) (<r = 0) (>r = "4242"))

   ; ..and what the hash-cons did to the pair of each kind
   _ (: p (open drv "w")
        _ (say p "(: _ (pin bake-dedup-c1 0 65) ok (? (id? bake-dedup-l1 bake-dedup-l2) ")
        _ (say p "(? (id? bake-dedup-c1 bake-dedup-c2) 0 ")
        _ (say p "((peep bake-dedup-c1 0 -1) = 65 && (peep bake-dedup-c2 0 -1) = 0) 1 0) 0) ")
        _ (say p "_ (putn ok) (quit 0))")
        (close p))
   r2 (hark ["out/host/love" "wake" img drv])
   _ (assert (two? r2) (<r2 = 0) (>r2 = "1"))

   ; ..and the native: still native in the child, and the same answer
   _ (assert (nat? bake-native) ((bake-native 100000) = 4999950000))
   _ (: p (open drv "w")
        _ (say p "(: _ (putn (? (nat? bake-native) (bake-native 100000) -1)) (quit 0))")
        (close p))
   r3 (hark ["out/host/love" "wake" img drv])
   _ (assert (two? r3) (<r3 = 0) (>r3 = "4999950000"))

   _ (unlink img) _ (unlink drv)
   _ (test_fin ())
   (puts "bake: ok\n"))
