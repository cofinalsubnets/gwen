(: # top expression
# prelude is a list of expressions to be evaluated sequentially
pre '(
  ; define functions
  (: true -1 false 0 nilp (= 0) not nilp
     (atomp x) (nilp (twop x))
     cons X car A cdr B null nilp
     (caar x) (A (A x)) (cadr x) (A (B x))
     (cdar x) (B (A x)) (cddr x) (B (B x))
     inc (+ 1) dec (+ -1) :: (tset macros)
     (id x) x (const x y) x (co f g x) (f (g x))
     (flip f x y) (f y x) (diag f x) (f x x)
     (map f l) (? l (X (f (A l)) (map f (B l))))
     (foldl z f l) (? l (foldl (f z (A l)) f (B l)) z)
     (foldr z f l) (? l (f (A l) (foldr z f (B l))) z)
     (filter p l) (? l (: m (filter p (B l))
                        (? (p (A l)) (X (A l) m) m)))
     (init l) (? (B l) (X (A l) (init (B l))))
     (last l) (? (B l) (last (B l)) (A l))
     (each f l) (? l (, (f (A l)) (each f (B l))))
     (all f l) (? l (? (f (A l)) (all f (B l))) true)
     (any f l) (? l (? (f (A l)) true (any f (B l))))
     (cat a b) (foldr b X a)
     (assq x l) (? l (? (= x (A (A l))) (A l) (assq x (B l))))
     (memq x) (any (= x))
     rev (foldl 0 (flip X))
     (part p) (foldr '(0) (\ a m
      (? (p a) (X (X a (A m)) (B m))
               (X (A m) (X a (B m))))))
     (llen l) (? (twop l) (+ 1 (llen (B l))))
     (iota n) (: (k m) (? (< m n) (X m (k (inc m)))) (k 0))
     (puts s) ((: (f n l) (? (= n l) s (, (putc (sget s n))
                                          (f (+ n 1) l))))
               0 (slen s)))
  ; define macros
  (, (:: 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
     (:: '&& (: (f l) (? l (X '? (X (A l) (X (A (B l)) (X (f (B (B l))) 0))))) f))
     (:: ':- (\ a (X ': (cat (B a) (X (A a) 0)))))
     (:: '>>= (\ l (X (last l) (init l)))))

# thread compiler
  (: (eval x)
   (:- (ana (scop 0 0 0) x (em2 i_ret 1 (\ n (seek n (thd n)))) 0 0)
    (scop par arg imp) (: t (tnew()) (, (tset t 'par par) (tset t 'arg arg) (tset t 'imp imp) t))
    (cpush c k v) (, (tset c k (X v (tget 0 c k))) v)
    (cpop c k) (: s (tget 0 c k) (, (tset c k (B s)) (A s)))
    (cpeek c k) (A (tget 0 c k))
    (em1 i k n) (poke i (seek -1 (k (+ 1 n))))
    (em2 i x k) (em1 i (em1 x k))
    imm (em2 i_imm)
    (ana c x) (:- (? (symp x)  (ana_sym c x)            ; to do
                     (atomp x) (imm x)                  ; ok
                               (ana_two c (A x) (B x))) ; to do

     (ana_apl c b k) (? (atomp b) k (ana c (A b) (em1 i_ap (ana_apl c (B b) k))))
     (ana_ap c f b j) (ana c f (ana_apl c b j))
     (ana_sym c x k) (>>= c (:
      (idx l i) (>>= l 0 (: (ii l n) (? l (? (= i (A l)) n (ii (B l) (inc n))) -1)))
      (ana_sym_local_fn asq d)
       (em2 i_lazy_bind (X (A asq) d) (ana_apl c (B asq) k))
      (stkidx x d)
        (: imp  (tget 0 d 'imp)
           limp (llen imp)
           arg  (tget 0 d 'arg)
           ii (idx imp x)
           ai (idx arg x)
         (? (>= ii 0) ii
            (>= ai 0) (+ limp ai)
            -1))
      (ana_sym_r d)
       (? (nilp d) (imm (tget x globals x) k)
        (: y (assq x (tget 0 d 'lam))
         (? y (ana_sym_local_fn y d)
          (: y (tget 0 d 'loc)
           (? (memq x y)          (ana_sym_local_def y)
              (>= (stkidx x d) 0) (ana_sym_stack_ref d)
                                  (ana_sym_r (tget 0 d 'par)))))))))

     (ana_two c a b) (:- (? (= a '`)  (imm (A b))     ; ok
                            (= a '?)  (ana_if c b)    ; ok
                            (= a '\)  (ana_lam c b)   ; to do
                            (= a ':)  (ana_let c b)   ; to do
                            (= a ',)  (ana_seq c b)   ; ok
                            (atomp b) (ana c a)       ; ok
                            (: m (tget 0 macros a)
                             (? m (ana c (m b))       ; ok
                                  (ana_ap c a b))))   ; ok
      (ana_lam c b) (? (atomp b) (imm 0) (atomp (B b)) (ana c (A b)))
      (ana_let c b) (? (atomp b) (imm 0) (atomp (B b)) (ana c (A b)))


      (ana_seq c x k) (? (atomp x)     (imm 0 k)
                         (atomp (B x)) (ana c (A x) k)
                                       (ana c (A x) (em1 i_drop1 (ana_seq c (B x) k))))
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
        (ana c (A b) (pop_alt c (ana c (A (B b)) (peek_end c (push 'alt (ana_if_r (B (B b)) k))))))))))))
# end thread compiler

# the last item in the prelude is the boot script
# it evaluates to a function of a list of strings (arguments)
(: (reads l) (: r (read ()) (? r (, (ev (A r)) (reads l)) l))
   (repl _)  (: r (, (puts "    ") (read 0))
              (? r (, (. (ev (A r)))
                      (putc 10)
                      (repl 0))))
   (show_help prog) (,
    (puts "usage: ") (puts prog) (puts " [args]
  args:
    -h    show this message
    -v    show version
    -r    start repl
    file  evaluate file
"))
   (show_version prog) (, (puts prog) (puts " ") (puts version) (putc 10))
   (process prog arg args) (,
    (? (= arg "-h") (show_help prog)
       (= arg "-v") (show_version prog)
       (= arg "-r") (repl ())
       ((: (evals x) (? x (, (ev (A x)) (evals (B x)))))
        (readf arg)))
    (? args (process prog (A args) (B args))))

   ii (cat '(peek poke seek macros thd)
                     (filter (\ y (= "i_" (ssub 0 2 (nom y)))) (tkeys globals)))
   (, (each (tdel 0 globals) ii)
    (\ fs (: prog (A fs) args (B fs)
           (? args       (process prog (A args) (B args))
              (isatty 0) (repl ())
                         (reads ())))))))
# end of prelude
# main expression
 ((: (boot a b) (? b (, (ev a) (boot (A b) (B b))) (ev a))) (A pre) (B pre))

  ) # end of top expression
