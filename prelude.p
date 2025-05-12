(: prelude-defns
  '(; list of prelude expressions to be evaluated sequentially in order
    ; aliases
    (: true -1 nilp (= 0) not nilp
       cons X car A cdr B null nilp
       (caar x) (A (A x)) (cadr x) (A (B x))
       (cdar x) (B (A x)) (cddr x) (B (B x)))
    ; function functions
    (: (id x) x
       (const x y) x
       (flip f x y) (f y x)
       (diag f x) (f x x))
    ; number functions
    (: inc (+ 1)
       dec (+ -1))
    ; list functions
    (: (map f l) (? l (X (f (A l)) (map f (B l))))
       (foldl z f l) (? l (foldl (f z (A l)) f (B l)) z)
       (foldr z f l) (? l (f (A l) (foldr z f (B l))) z)
       (filter f l) (? l (: m (filter f (B l)) (? (f (A l)) (X (A l) m) m)))
       (init l) (? (B l) (X (A l) (init (B l))))
       (last l) (? (B l) (last (B l)) (A l))
       (each f l) (? l (, (f (A l)) (each f (B l))))
       (all f l) (? l (? (f (A l)) (all f (B l))) true)
       (any f l) (? l (? (f (A l)) true (any f (B l))))
       (append a b) (? a (X (A a) (append (B a) b)) b)
       reverse (foldl 0 (flip X))
       (partition p) (foldr '(0) (\ a m
        (? (p a) (X (X a (A m)) (B m))
                 (X (A m) (X a (B m))))))
       (llen l) (? (twop l) (+ 1 (llen (B l)))))
    ; data constructors
    (: (iota n) (: (k m) (? (< m n) (X m (k (inc m)))) (k 0))
       (puts s) (: (f n l) (? (= n l) s (, (putc (sget s n)) (f (+ n 1) l))) (f 0 (slen s))))

    (:: 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
    (:: '&& (: (f l) (? l (X '? (X (A l) (X (A (B l)) (X (f (B (B l))) 0))))) f))
    (: (vprintf s l) (: len (slen s)
                        (p n l) (? (< n len)
                         (: c (sget s n)
                          (? (= 37 c)
                           (? (= 37 (sget s (inc n)))
                             (, (putc 37) (p (+ 2 n) l))
                             (, (. (A l)) (p (inc n) (B l))))
                           (, (putc c)
                              (p (inc n) l)))))
                        (p 0 l)))
    ; end of prelude definitions
    ))

;; can't use fold yet heheh
(: (f v x) (? x (f (ev (A x)) (B x)) v) (f 0 prelude-defns))
; now we can use prelude functions
(: thread-compiler '(: E 1 Get (tget 0)
  (enscope par arg imp) (: s (tnew 0) set (tset s) (,
   (set 'arg arg) (set 'imp imp) (set 'par par)
   s))


E))

(tset global-namespace 'E (ev thread-compiler))
(each (tdel global-namespace) '(prelude-defns thread-compiler))
;  end of prelude
