(: ; self evaluator test
 evr '(: ; expression for the evaluator
  (meta-eval x) (?
   (symp x) (\ l (l x))
   (not (twop x)) (const x)
   (: x0 (A x) a (B x)
    (? (= x0 '`) (const (A a))
       (= x0 ',) (foldl 0 (\ a b l (, (a l) (b l)))
                          (map meta-eval a))
       (= x0 '\) (foldr (meta-eval (last a))
                        (\ a f l x (f (\ y (? (= y a) x (l y)))))
                        (init a))
       (= x0 '?) (cond-loop a id)
       (= x0 ':) (let-loop a id)
       (: y (map meta-eval x)
        (\ l (foldl id id (map (\ x (x l)) y)))))))

  (cond-loop a f) (?
   (nilp a) (cond-loop (X 0 0) f)
   (nilp (B a)) (f (meta-eval (A a)))
   (: ant (meta-eval (A a)) con (meta-eval (AB a))
    (cond-loop (BB a) (\ alt (f (\ l (? (ant l) (con l) (alt l))))))))

  (let-loop a b m) (?
   (nilp a) (let-loop (X 0 0) b m)
   (nilp (B a)) (meta-eval (A a) (b m))
   (desugar (A a) (AB a) (\ k v
    (: t (tnew 0) get (tget 0 t) set (tset t 0)
     (let-loop (BB a) (\ l (, (set (meta-eval v (b l))) l))
                        (\ x (? (= x k) (get 0) (m x))))))))

  (desugar k v c)
   (? (twop k) (desugar (A k) (X '\ (cat (B k) (X v 0))) c)
               (c k v))
  ;return
  meta-eval)
 meta-eval (ev evr)
 expr '((\ a b (: c (+ a 9) d (+ c b) (* c d))) 4 5)
 G ev
 (, (assert (= 234 (ev expr)))
    (assert (= 234 (meta-eval expr G)))
    (assert (= 234 (meta-eval evr G expr G)))
    (assert (= 234 (meta-eval evr G evr G expr G)))))
