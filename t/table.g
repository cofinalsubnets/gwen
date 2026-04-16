(:
 t (new .)
 (Put k v) (put k v t)
 Del (tdel 0 t)
 (Get k) (get 0 k t)
 _ (Put 1 2)
 _ (Put 2 3)
 _ (Put 3 4)
 _ (Put 't 'f)
 _ (assert
    (= 4 (len t))
    (= 4 (len (tkeys t))))
 _ (Del 2)
 _ (Del 't)
 _ (assert
    (= 2 (len t))
    (= 2 (len (tkeys t)))
    (: (lll t) (foldl 0 (\ l k (X k (X (Get k) l))) (tkeys t))
     (= (* 2 (len t)) (len (lll t))))))
