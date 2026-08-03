; post -- what is LEFT of the post-egg layer: one redefinition, waiting on its real home.
;
; `bake` is a NIF (host/image.c). This wraps it so the glaze's memo cache -- full of native
; closures whose ap is a W^X mmap address, which cannot serialize -- empties before the snapshot.
; ⚠ THE WRAPPER IS POSITIONAL: it reads `book`, which the --bake lane seals away (host/main.c,
; `(pull book 'book 0)`) on the way into an image. It works only because the boot loads this file
; BEFORE that seal, so the reference folds to a value. Loaded any later -- a consumer's own
; (use 'post) in a woken image -- the read misses, the (lit? gl) guard swallows the scare, and the
; flush becomes a SILENT no-op. That is why the boot still splices this unconditionally, and why
; this file cannot be devolved to its callers as it stands.
;
; It belongs to the GLAZE: the glaze owns the cache, is the only thing that creates the hazard,
; and closes over that cache directly (no `book`, nothing to seal). Moving it there is the last
; step of abolishing this file, and it waits on the glaze becoming a real module (today it is a
; hand-rolled book pin, love/glaze/export.l). The other three tenants have left: the parser
; combinators are love/peg.l, the type overlay love/overlay.l, revcat is in prel.
;
; callers, all bare with no (use ..): the image recipes in crew/build.mk, test/gate/fixpoint.sh,
; test/host/bake.l. wake an image with `love --wake x.image prog.l args..` -- the woken session
; carries every global pinned before the bake (an app loaded warm, its per-run load gone).
(: bake (: b0 bake
           (\ p (: gl (peep book 'glaze 0)
                   _ (? (lit? gl) (: c (gl 'cache) (map (\ k (pull c k 0)) (keys c))) 0)
                   (b0 p)))))
