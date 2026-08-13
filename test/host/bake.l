; --- test/host/bake.l: smoke test for the bake nif (host/image.c + ai_image_save_) ---
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

(: img "out/.bake-nif-smoke.image"
   drv "out/.bake-nif-drive.l"
   _ (unlink img) _ (unlink drv)                    ; a clean slate, tolerated if absent

   ; the bake answers 1 and the session RIDES ON (this very assert runs post-dump);
   ; note the file port feeding THIS script is live at the dump -- the fz leg.
   _ (assert ((bake img) = 1))

   ; the woken child reads the marker this session pinned before its bake
   _ (: p (open drv "w") _ (say p "(: _ (putn bake-smoke-marker) (quit 0))") (close p))
   r (hark `("out/host/love" "wake" img drv))
   _ (assert (two? r) (<r = 0) (>r = "4242"))

   _ (unlink img) _ (unlink drv)
   (puts "bake: ok\n"))
