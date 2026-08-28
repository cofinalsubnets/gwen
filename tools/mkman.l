; tools/mkman.l -- markdown -> roff(man), through the lapiz lens (crew/lapiz/lapiz.l).
; the man pages are WRITTEN in doc/*.md and this generates doc's roff at build time, so
; there is only one source to keep true. run from the repo root:
;   love tools/mkman.l FILE.md [love_version.h]
; (the (use 'lapiz) below splices the baked module)
; @VERSION@ is the one placeholder the prose carries and the second argument is where the
; build keeps the version, so the fill lands here and the chain needs no sed. with no
; second argument the placeholder rides through, which is what a hand run wants.
(use 'lapiz)
(: (rd p) (: q (open p "r")
             (? q (: t (slurp q) _ (close q) t)
                (: _ (say err ("mkman: cannot read " + p)) _ (put err 10) (quit 1))))
   ; the version is whatever sits between love_version.h's two quotes
   (quoted t)
    (: n (tally t)
       (go i a) (? (n <= i) ""
                   !(t i = 34) (go (i + 1) a)
                   (two? a) (snip t <a i)
                   (go (i + 1) [(i + 1)]))
       (go 0 ()))
   ; segments onto a list and joined once -- a per-hit concat would re-copy the whole
   ; page each time, which on a man page is quadratic for no reason
   (fill t k v)
    (: n (tally t) m (tally k)
       (go i st acc)
        (? (n <= i) (foldl (+) "" (rev ((snip t st n) >< acc)))
           (((i + m) <= n) && (snip t i (i + m) = k))
            (go (i + m) (i + m) (v >< ((snip t st i) >< acc)))
           (go (i + 1) st acc))
       (go 0 0 ()))
   vh >>argv
   rf (md->rf (rd <>argv))
   _ (puts (? (two? vh) (fill rf "@VERSION@" (quoted (rd <vh))) rf))
   0)
