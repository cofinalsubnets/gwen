(: ; self evaluator test
 evr '(: ; expression for the evaluator
  (meta-eval x) (?
   (symp x) (\ l (l x))
   (not (twop x)) (const x)
   (: x0 (car x) a (cdr x)
    (? (= x0 '`) (const (car a))
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
   (nilp a) (cond-loop (cons 0 0) f)
   (nilp (cdr a)) (f (meta-eval (car a)))
   (: ant (meta-eval (car a)) con (meta-eval (cadr a))
    (cond-loop (cddr a) (\ alt (f (\ l (? (ant l) (con l) (alt l))))))))

  (let-loop a b m) (?
   (nilp a) (let-loop (cons 0 0) b m)
   (nilp (cdr a)) (meta-eval (car a) (b m))
   (desugar (car a) (cadr a) (\ k v
    (: t (tnew 0) get (tget 0 t) set (tset t 0)
     (let-loop (cddr a) (\ l (, (set (meta-eval v (b l))) l))
                        (\ x (? (= x k) (get 0) (m x))))))))

  (desugar k v c)
   (? (twop k) (desugar (car k) (cons '\ (cat (cdr k) (cons v 0))) c)
               (c k v))
  ;return
  meta-eval)
 meta-eval (ev evr)
 expr '((\ a b (: c (+ a 9) d (+ c b) (* c d))) 4 5)
 G (tget 0 globals)
 (, (assert (= 234 (ev expr)))
    (assert (= 234 (meta-eval expr G)))
    (assert (= 234 (meta-eval evr G expr G)))
    (assert (= 234 (meta-eval evr G evr G expr G)))))
