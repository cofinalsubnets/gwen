(: prelude
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
       reverse ((: (r a b) (? b (r (X (A b) a) (B b)) a)) 0)
       (partition p) (foldr '(0) (\ a m
        (? (p a) (X (X a (A m)) (B m))
                 (X (A m) (X a (B m))))))
       (llen l) (? (twop l) (+ 1 (llen (B l)))))
    ; data constructors
    (: (iota n) (: (k m) (? (< m n) (X m (k (inc m)))) (k 0))
       (puts s) (: (f n l) (? (= n l) s (, (putc (sget s n)) (f (+ n 1) l))) (f 0 (slen s))))
    (: :: (tset macros))

    (tset macros 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
    (tset macros '&& (: (f l) (? l (X '? (X (A l) (X (A (B l)) (X (f (B (B l))) 0))))) f))
    (: (vprintf s l) (: len (slen s)
                        (p n l) (? (< n len)
                         (: c (sget s n)
                          (? (= 37 c)
                           (? (= 37 (sget s (inc n)))
                             (, (putc 37) (p (+ 2 n) l))
                             (, (. (A l)) (p (inc n) (B l))))
                           (, (putc c)
                              (p (inc n) l)))))
                        (p 0 l))))
  (, ((: (f v x) (? x (f (ev (A x)) (B x)) v)) 0
      (tset global-namespace 'prelude prelude))
     (ev '(: (evals x) (? x (, (ev (A x)) (evals (B x))))
             (reads l) (: r (read()) (? r (reads (X (A r) l)) l))
             (evalf f) (evals (readf f))
             (evalfs fs) (? fs (, (evalf (A fs)) (evalfs (B fs))))
           (\ fs (: prog (A fs) args (B fs)
                  (?  args      (evalfs args)
                     (isatty 0) (repl())
                                (evals (reads()))))))))
) ; : prelude
