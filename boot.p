(: prelude '(; list of prelude expressions to be evaluated sequentially in order
    ; aliases
    (: true -1 nilp (= 0) not nilp
       cons X car A cdr B null nilp
       (caar x) (A (A x)) (cadr x) (A (B x))
       (cdar x) (B (A x)) (cddr x) (B (B x))
    ; function functions
       (id x) x
       (const x y) x
       (co f g x) (f (g x))
       (flip f x y) (f y x)
       (diag f x) (f x x)
    ; number functions
       inc (+ 1)
       dec (+ -1)
    ; list functions
       (map f l) (? l (X (f (A l)) (map f (B l))))
       (foldl z f l) (? l (foldl (f z (A l)) f (B l)) z)
       (foldr z f l) (? l (f (A l) (foldr z f (B l))) z)
       (filter f l) (? l (: m (filter f (B l)) (? (f (A l)) (X (A l) m) m)))
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
    ; data constructors
       (iota n) (: (k m) (? (< m n) (X m (k (inc m)))) (k 0))
       (puts s) (: (f n l) (? (= n l) s (, (putc (sget s n)) (f (+ n 1) l))) (f 0 (slen s)))
       :: (tset macros))
    (, (:: 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
       (:: '&& (: (f l) (? l (X '? (X (A l) (X (A (B l)) (X (f (B (B l))) 0))))) f))
       (:: '>>= (\ l (X (last l) (init l)))))
    (: eval (:
     (scop par arg imp)
      (: t (tnew()) (, (tset t 'par par)
                       (tset t 'arg arg)
                       (tset t 'imp imp)
                       t))
        (cpush c k v)
         (, (tset c k (X v (tget 0 c k))) v)
        (cpop c k) (: s (tget 0 c k)
         (, (tset c k (B s))
            (A s)))
        (cpeek c k) (A (tget 0 c k))
        (em1 i k n) (poke i (seek -1 (k (+ 1 n))))
        (em2 i x k) (em1 i (em1 x k))
        ana_imm (em2 i_imm)
        ana_cons (em2 i_ret 1 (\ n (seek n (thd n))))
        (atomp x) (nilp (twop x))
        (ana_seq c x k) (?
         (atomp x)     (ana_imm 0 k)
         (atomp (B x)) (ana c (A x) k)
         (ana c (A x)  (em1 i_drop1 (ana_seq c (B x) k))))
        (ana c x) (?
         (symp x)  (ana_sym c x)          ; to do
         (atomp x) (ana_imm x)            ; ok
         (: a (A x) b (B x) (?
          (= a '`)        (ana_imm (A b)) ; ok
          (= a '?)        (ana_if c b)    ; to do
          (= a '\)        (ana_lam c b)   ; to do
          (= a ':)        (ana_let c b)   ; to do
          (= a ',)        (ana_seq c b)   ; ok
          (nilp (twop b)) (ana c a)       ; ok
          (: m (tget 0 macros a) (? m
           (ana c (m b))                  ; ok
           (ana_ap c a b))))))            ; to do
        (branch_push_end c k n)
         (cpush c 'end (k n))
        (branch_peek_end c k n)
         ; FIXME tail calls
         (poke i_jump (seek -1 (poke (cpeek c 'end) (seek -1 (k (+ 2 n))))))
        (branch_pop_alt c k n)
         (poke i_cond (seek -1 (poke (cpop c 'alt) (seek -1 (k (+ 2 n))))))
        (branch_peek_alt c k n)
         ()
        (ana_ap c a b)
         ()
        (ana_if c b k0 n) (:
         k ((ana_if_r c b (co (cpush c 'end) k0)) n)
         (, (cpop c 'end) k))
        (ana_if_r c b k) (?
         (atomp b) (ana_imm 0 k)
         (atomp (B b)) (ana c (A b) k))
        (ana_lam c b) (?
         (atomp b) (ana_imm 0)
         (atomp (B b)) (ana c (A b)) ; maybe this should make it a one argument function?
        )
        (ana_let c b) (?
         (atomp b) (ana_imm 0)
         (atomp (B b)) (ana c (A b)))
        (ana_sym c x k)
         (>>= c (: (ana_sym_r d)
          (? (nilp d) (ana_imm (tget x globals x) k)
           (ana_sym_r (tget 0 d 'par)))))
        (eval x) (ana (scop 0 0 0) x ana_cons 0 0)
        eval)))

  (((: (boot ev v x) (? x (boot ev (ev (A x)) (B x)) ev)) ev 0 prelude)
   '(: (evals x) (? x (, (ev (A x)) (evals (B x))))
       (reads l) (: r (read ()) (? r (reads (X (A r) l)) l))
       (evalf f) (evals (readf f))
       (evalfs fs) (? fs (, (evalf (A fs)) (evalfs (B fs))))
       (repl _) (: r (, (puts "    ") (read 0))
                 (? r (, (. (ev (A r)))
                         (putc 10) (repl 0))))
       (show_help prog) (,
        (puts "usage: ") (puts prog) (puts " [args]
  args:
    -h    show this message
    -v    show version
    -r    start repl
    file  evaluate file
"))
       (show_version prog) (,
        (puts prog) (puts " ") (puts version) (putc 10))
       (show_version prog) (, (puts prog) (puts " ") (puts version) (putc 10))
       (process prog arg args) (,
        (? (= arg "-h") (show_help prog)
           (= arg "-v") (show_version prog)
           (= arg "-r") (repl ())
           (evalf arg))
        (? args (process prog (A args) (B args))))
     (\ fs (: prog (A fs) args (B fs)
            (? args       (process prog (A args) (B args))
               (isatty 0) (repl ())
                          (evals (reads ()))))))))
