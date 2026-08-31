(: ; self evaluator test
 evr '(: ; expression for the evaluator -- quoted source: operators stay plain
         ; symbols in a quote (data), and meta_eval has no opfix, so this
         ; source is written in core form (nil?, not !)
  (meta_eval x) (?
   (nom? x) (\ l (l x))
   (nil? (two? x)) (const x)
   (: x0 (cap x) a (cup x)
    (? (= x0 'do) (foldl (\ a b l (do (a l) (b l))) 0 (map meta_eval a))
       (= x0 '\) (? (atom? (cup a)) (const (cap a)) ; one operand: quote
                  (foldr (\ a f l x (f (\ y (? (= y a) x (l y)))))
                         (meta_eval (last a))
                         (init a)))
       (= x0 '?) (cond_loop a id)
       (= x0 ':) (let_loop a id)
       (: y (map meta_eval x)
        (\ l (foldl id id (map (\ x (x l)) y)))))))

  (cond_loop a f) (?
   (nil? a) (cond_loop (link 0 0) f)
   (nil? (cup a)) (f (meta_eval (cap a)))
   (: ant (meta_eval (cap a)) con (meta_eval (caup a))
    (cond_loop (cuup a) (\ alt (f (\ l (? (ant l) (con l) (alt l))))))))

  (let_loop a b m) (?
   (nil? a) (let_loop (link 0 0) b m)
   (nil? (cup a)) (meta_eval (cap a) (b m))
   (desugar (cap a) (caup a) (\ k v
    (: t (tablet 0) (Get k) (peep t k 0) (Put v) (pin t 0 v)
     (let_loop (cuup a) (\ l (do (Put (meta_eval v (b l))) l))
                        (\ x (? (= x k) (Get 0) (m x))))))))

  (desugar k v c)
   (? (two? k) (desugar (cap k) (link '\ (cat (cup k) (link v 0))) c)
               (c k v))
  ;return
  meta_eval)
 meta_eval (ev evr)
 expr '((\ a b (: c (+ a 9) d (+ c b) (* c d))) 4 5)
 G ev
 (assert
 (= 234 (ev expr))
    (= 234 (meta_eval expr G))
    (= 234 (meta_eval evr G expr G))
    (= 234 (meta_eval evr G evr G expr G))))
