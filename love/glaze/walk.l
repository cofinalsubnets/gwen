; walk -- re-declare the prel list walkers through the glazed compiler. the prel
; originals are egg-baked pre-glaze; these compile under the live natjit, so amble owns
; their self-recursion, an applied param takes the drive, and the generic = keeps symbol
; and string keys native. bodies verbatim from love/prel.l, caap spelled through cap (a
; free accessor head declines). loads after hook.l; egg-baked consumers keep the closures
; they folded, every post-boot stream folds these.
(? (0 < love-tco)
   (ev '(: (foldl f z l) (? (two? l) (foldl f (f z (cap l)) (cup l)) z)
           (foldr f z l) (? (two? l) (f (cap l) (foldr f z (cup l))) z)
           (filter p l) (? (two? l) (: m (filter p (cup l)) (? (p (cap l)) (link (cap l) m) m)))
           (revcat l acc) (? (two? l) (revcat (cup l) (link (cap l) acc)) acc)
           (assoc x l) (? (two? l) (? (= x (cap (cap l))) (cap l) (assoc x (cup l))))
           (all f l) (? (two? l) (? (f (cap l)) (all f (cup l)) 0) 1)
           (any f l) (? (two? l) (? (f (cap l)) 1 (any f (cup l))) 0)
           (zip a b) (? (two? a) (? (two? b) (link (link (cap a) (cap b)) (zip (cup a) (cup b)))))
           (drop n l) (? n (drop (- n 1) (cup l)) l)
           (take n l) (? n (? (two? l) (link (cap l) (take (- n 1) (cup l)))))
           (init l) (? (two? (cup l)) (link (cap l) (init (cup l))))
           (last l) (? (two? (cup l)) (last (cup l)) (cap l))
           (ldel x l) (? (two? l) (? (= (cap l) x) (cup l) (link (cap l) (ldel x (cup l)))))))
   0)
