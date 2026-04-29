(: (pat2pred p)
 (? (symp p) (const true)
    (atomp p) (= p)
    (= '` (car p)) (= (cadr p))
  (: p1 (pat2pred (car p))
     p2 (pat2pred (cdr p))
   (\ x (? (twop x) (? (p1 (car x)) (p2 (cdr x)))))))
 (pat2x p b) (:
  t (new 0)

  (ff q p) (?
   (symp p) (put p q t)
   (twop p) (: _ (ff (co car q) (car p))
                (ff (co cdr q) (cdr p))))
  _ (ff id p)
  ks (tkeys t)

  lam (cons '\ (cat ks (list b)))
  y (sym 0)
  (ev (list '\ y (cons lam (map (\ k (list (get 0 k t) y)) ks)))))

 (pmatch x1 x2 f) (:
 p (pat2pred x1)
 x (pat2x x1 x2)
 (\ y ((? (p y) x f) y)))

 (assert
 (pat2pred 1 1)
 (pat2pred '(1) '(1))
 (not (pat2pred 1 2))
 (not (pat2pred '(1 2) '(1 0)))
 (pat2pred '(this) '(1))
 (not (pat2pred '(this has five symbols though) '(1 2 3 4 5 6)))
 (pat2pred ''hi 'hi)
 (not (pat2pred ''hi 'hey))
 (= 7 (pmatch '(a b) '(+ a b) id '(3 4)))
 (= 8 (pmatch '(a b) '(+ a b) (const 8) 3))
 (= 93 (pmatch '(a (b c)) '(+ c (* b a)) id '(3 (30 3))))
 (= 72 (pmatch '('sum a b) '(+ a b) (pmatch '('product a b) '(* a b) id) '(sum 32 40)))
 (= 72 (pmatch '('sum a b) '(+ a b) (pmatch '('product a b) '(* a b) id) '(product 6 12)))
 ))
