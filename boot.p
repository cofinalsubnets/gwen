# top expression
((: (go a b) (? b (go (ev (A b)) (B b)) a)) 0 '(
# prelude is a list of expressions to be evaluated sequentially
  (: true -1 false 0 nilp (= 0) not nilp
     (atomp x) (nilp (twop x))
     cons X car A cdr B null nilp
     (caar x) (A (A x)) (cadr x) (A (B x))
     (cdar x) (B (A x)) (cddr x) (B (B x))
     AA caar AB cadr BA cdar BB cddr
     inc (+ 1) dec (+ -1) :: (tset macros)
     (id x) x (const x y) x (co f g x) (f (g x))
     (flip f x y) (f y x) (diag f x) (f x x)
     (map f l) (? l (X (f (A l)) (map f (B l))))
     (foldl z f l) (? l (foldl (f z (A l)) f (B l)) z)
     (foldr z f l) (? l (f (A l) (foldr z f (B l))) z)
     (foldl1 f l) (foldl (A l) f (B l))
     (filter p l) (? l (: m (filter p (B l))
                        (? (p (A l)) (X (A l) m) m)))
     (init l) (? (B l) (X (A l) (init (B l))))
     (last l) (? (B l) (last (B l)) (A l))
     (each f l) (? l (, (f (A l)) (each f (B l))))
     (all f l) (? l (? (f (A l)) (all f (B l))) true)
     (any f l) (? l (? (f (A l)) true (any f (B l))))
     (cat a b) (foldr b X a)
     (assq x l) (? l (? (= x (AA l)) (A l) (assq x (B l))))
     (memq x) (any (= x))
     (zip a b) (? (twop a) (? (twop b) (X (X (A a) (A b)) (zip (B a) (B b)))))
     rev (foldl 0 (flip X))
     (part p) (foldr '(0) (\ a m
      (? (p a) (X (X a (A m)) (B m))
               (X (A m) (X a (B m))))))
     (.. x) (, (. x) (putc 10) x)
     (llen l) (? (twop l) (+ 1 (llen (B l))))
     (iota n) (: (k m) (? (< m n) (X m (k (inc m)))) (k 0))
     (puts s) ((: (f n l) (? (= n l) s (, (putc (sget s n))
                                          (f (+ n 1) l)))
     )
               0 (slen s)))
  (, (:: 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
     (:: '&& (: (f l) (? l (X '? (X (A l) (X (AB l) (X (f (BB l)) 0))))) f))
     (:: ':- (\ a (X ': (cat (B a) (X (A a) 0)))))
     (:: '>>= (\ l (X (last l) (init l))))
     (:: '|> (foldl1 (\ m f (X f (X m 0))))))

# thread compiler
  (: (eval x)
   (:- (ana (scop 0 0 0) x (thd0 1) 0 0)
    (evens n) (? (atomp n) 0 (odds (B n)))
    (odds n) (? (atomp n) 0 (X (A n) (evens (B n))))
    (thd0 r) (em2 i_ret r (\ n (seek n (thd n))))
    (scop par arg imp) (: t (tnew()) (, (tset t 'par par) (tset t 'arg arg) (tset t 'imp imp) t))
    (cpush c k v) (, (tset c k (X v (tget 0 c k))) v)
    (cpop c k) (: s (tget 0 c k) (, (tset c k (B s)) (A s)))
    (cpeek c k) (A (tget 0 c k))
    (em1 i k n) (poke i (seek -1 (k (+ 1 n))))
    (em2 i x k) (em1 i (em1 x k))
    imm (em2 i_imm)
    (cata_var c x ins j m) (:
     k (j (+ 2 m))
     idx (stkidx c x)
     (poke i_ref (seek -1 (poke (+ idx ins) (seek -1 k)))))
    (stkidx d x)
      (: imp  (tget 0 d 'imp)
         arg  (tget 0 d 'arg)
         ii (idx imp x)
         ai (idx arg x)
       (? (>= ii 0) ii
          (>= ai 0) (+ (llen imp) ai)
          -1))
    (ana c x) (:- (? (symp x)  (ana_sym c x)            ; ok?
                     (atomp x) (imm x)                  ; ok
                               (ana_two c (A x) (B x))) ; to do

     (ana_apl c b k) (? (atomp b) k (ana c (A b) (em1 i_ap (ana_apl c (B b) k))))
     (ana_ap c f b j)  (,
        (cpush c 'stack ())
        (: k (ana c f (ana_apl c b j))
         (, (cpop c 'stack) k)))
     (ana_sym c x) (>>= c (:- ana_sym_r
      (idx l i) (>>= l 0 (: (ii l n) (? l (? (= i (A l)) n (ii (B l) (inc n))) -1)))
      (ana_sym_local_fn asq d k)
       (em2 i_lazy_bind (X (A asq) d) (ana_apl c (B asq) k))
      (ana_sym_stack_ref x d) (,
       (? (nilp (= c d)) (cpush c 'imp x))
       (cata_var d x (llen (tget 0 c 'stack))))
      (ana_sym_r d) (:
       (? (nilp d) (imm (tget x globals x))
        (: y (assq x (tget 0 d 'lam))
         (? y (ana_sym_local_fn y d)
          (: y (tget 0 d 'loc)
           (? (memq x y)          (ana_sym_local_def y)
              (>= (stkidx d x) 0) (ana_sym_stack_ref x d)
                                  (ana_sym_r (tget 0 d 'par))))))))))

     (ana_two c a b) (:- (? (= a '`)  (imm (A b))     ; ok
                            (= a '?)  (ana_if c b)    ; ok
                            (= a '\)  (ana_lam c b)   ; ok
                            (= a ':)  (ana_let c b)   ; to do
                            (= a ',)  (ana_seq c b)   ; ok
                            (atomp b) (ana c a)       ; ok
                            (: m (tget 0 macros a)
                             (? m (ana c (m b))       ; ok
                                  (ana_ap c a b))))   ; ok

      (ana_lam c b) (? (atomp b)     (imm 0)
                       (atomp (B b)) (ana c (A b))
                                     (ana c (ana_ll c 0 b)))
      (ana_ll c imp exp) (:
       arg (init exp)
       x (last exp)
       d (scop c arg imp)
       arity (+ (llen arg) (llen imp))
       k ((? (> arity 1) (em2 i_curry arity) id)
        (, (ana d x (thd0 arity))) 0)
       (X k (tget 0 d 'imp)))


      (desug n d) (? (atomp n) (X n d)
                     (desug (A n) (X '\ (cat (B n) (L d)))))
      (lambp x) (&& (twop x) (= '\ (A x)))

      (ana_let c b) (:-
       (? (atomp b)     (imm 0)
          (atomp (B b)) (ana c (A b))
          (l1 0 0 (A b) (AB b) (BB b)))
       pushs (cpush c 'stack)
       (pops _) (cpop c 'stack)
       q (scop c (tget 0 c 'arg) (tget 0 c 'imp))
       ; l1 collects bindings and passes them with the body expression to l2
       (l1 noms defs nom def rest) (:-
       (, ;(.. 'l1)
        (? (atomp rest) (l2 noms1 defs1 nom1)
           (atomp (B rest)) (l2 noms1 defs1 (A rest))
           (l1 noms1 defs1 (A rest) (AB rest) (BB rest))))
        nd1 (desug nom def)
        nom1 (A nd1) def1 (B nd1)
        noms1 (X nom1 noms) defs1 (X def1 defs))

       ; l2 finds closures for all local functions and passes a lambda for the body to l3
       (l2 noms defs exp) (:-
        (, ;(.. 'l2 )
         (l3 noms defs clams llam))

        lams (>>= 0 noms defs
              (: (b l n d) (? (atomp n) l
               (: ll (? (not (lambp (A d))) l (X (ana_ll q 0 (BA d)) l))
                (b ll (B n) (B d))))))
        (close lams) lams
        clams (close lams) ;; find transitive closures of closures
                           ;; exclude local functions from closures
        llam (X '\ (cat noms (L exp)))) ;; construct reversed lambda expression
        ;; l3 collects def values on stack and applies lambda
       (l3 noms defs clams llam k) (:
;         _ (.. 'l3)
         k1 (em1 i_ap  k)
;         _ (pushs 0)
         k2 (foldl k1 (\ k nd (ana c (B nd) k)) (zip noms defs))
;         _ (pops 0)
         (ana c llam k2)
         )
          ;; evaluate lambda and push on stack
          ;; evaluate arguments in original order
          ;;; push each name onto the stack afterwards
          ;;; if the argument is a lambda then remake with explicit closure
          ;;; and put in arg and lam list
          ;; output apply instruction
          ;; pop args off stack
          ;; done :>

       ) ; end ana_let

      (ana_seq c x k) (? (atomp x) (imm 0 k)
       (ana c (A x) (? (atomp (B x)) k (em1 i_drop1 (ana_seq c (B x) k)))))
      (ana_if c b k) (:- (pop 'end (ana_if_r b (push 'end  k)))
       (pop y k n) (: j (k n) (, (cpop c y) j))
       (push y k n) (cpush c y (k n))
       (peek_end c k n) (: j (k (+ 2 n))
        ; FIXME tail calls
        (poke i_jump (seek -1 (poke (cpeek c 'end) (seek -1 j)))))
       (pop_alt c k n) (: j (k (+ 2 n))
        (poke i_cond (seek -1 (poke (cpop c 'alt) (seek -1 j)))))
       (ana_if_r b k) (?
        (atomp b) (imm 0 k)
        (atomp (B b)) (ana c (A b) (peek_end c k))
        (ana c (A b) (pop_alt c (ana c (AB b) (peek_end c (push 'alt (ana_if_r (BB b) k))))))))))))
# end thread compiler
# the last item in the prelude is the boot script
# it evaluates to a function of a list of strings (arguments)
(, (each (tdel 0 globals)
    (cat '(peek poke seek macros thd globals)
     (filter (\ y (= "i_" (ssub (nom y) 0 2)))
      (tkeys globals))))
   (\ fs (:- (? args       (procs prog (A args) (B args))
                (isatty 0) (repl ())
                           (reads ()))

          prog (A fs) args (B fs)
          (reads l) (: r (read ()) (? r (, (ev (A r)) (reads l)) l))
          (repl _)  (: r (, (puts "    ") (read 0))
                     (? r (, (. (ev (A r))) (putc 10) (repl 0))))
          (procs prog a as) (, (proc1 prog a)
                               (? as (procs prog (A as) (B as))))
          (proc1 prog arg) (:-
           (? (= arg "-h") (, (puts "usage: ") (puts prog) (puts help))
                (= arg "-v") (, (puts prog) (puts " ") (puts version) (putc 10))
                (= arg "-r") (repl ())
                ((: (evals x) (? x (, (ev (A x)) (evals (B x)))))
                 (readf arg)))
           help " [args]
        args:
          -h    show this message
          -v    show version
          -r    start repl
          file  evaluate file
"))))))
