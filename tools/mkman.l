; tools/mkman.l -- markdown -> roff(man), through the lapiz lens (crew/lapiz/lapiz.l).
; the man pages are WRITTEN in doc/*.md and this generates doc's roff at build time, so
; there is only one source to keep true. run from the repo root: love tools/mkman.l FILE
; (the (use 'lapiz) below loads lib/lapiz.l through the runtime loader)
; the @VERSION@ placeholder rides through as ordinary text; host/build.mk seds it after.
(use 'lapiz)
(: f (cap (cup argv))
   src (: q (open f "r")
           (? q (slurp q) (: _ (say err ("mkman: cannot read " + f)) _ (put err 10) (quit 1))))
   _ (puts (md->rf src))
   0)
