# top expression
(: prelude '(
  (: true -1 false 0 nilp (= 0) not nilp
     (atomp x) (nilp (twop x))
     (!= a b) (? (= a b) 0 -1)
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
     (ldel x l) (? (twop l) (? (= (A l) x) (B l) (X (A l) (ldel x (B l)))))
     (all f l) (? l (? (f (A l)) (all f (B l))) true)
     (any f l) (? l (? (f (A l)) true (any f (B l))))
     (cat a b) (foldr b X a)
     (assq x l) (? l (? (= x (AA l)) (A l) (assq x (B l))))
     (lidx x) ((: (f n l) (? (twop l) (? (= x (A l)) n (f (+ 1 n) (B l))) -1)) 0)
     (memq x) (any (= x))
     (zip a b) (? (twop a) (? (twop b) (X (X (A a) (A b)) (zip (B a) (B b)))))
     rev (foldl 0 (flip X))
     (part p) (foldr '(0) (\ a m
      (? (p a) (X (X a (A m)) (B m))
               (X (A m) (X a (B m))))))
     (.. x) (, (. x) (putc 10) x)
     (llen l) (? (twop l) (+ 1 (llen (B l))))
     (iota n) (: (k m) (? (< m n) (X m (k (inc m)))) (k 0))
     (puts s) ((: (f n l) (? (= n l) s (, (putc (sget s n)) (f (+ n 1) l)))) 0 (slen s)))
  (, (:: 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
     (:: '&& (: (f l) (? l (X '? (X (A l) (X (AB l) (X (f (BB l)) 0))))) f))
     (:: ':- (\ a (X ': (cat (B a) (X (A a) 0)))))
     (:: '>>= (\ l (X (last l) (init l)))))
     (:: '|> (foldl1 (\ m f (L f m ))))
     (:: '>=> (\ g (: y (sym 0) (L '\ y (foldl y (\ x f (L f x)) g)))))
     (:: '<=< (\ g (: y (sym 0) (L '\ y (foldr y (\ f x (L f x)) g)))))
  (:
    (scop par arg imp) (: t (tnew()) (, (tset t 'par par) (tset t 'arg arg) (tset t 'imp imp) t))
    (eval x)
   (:- (ana 0 top x (thd0 top) 0 0)
    anon (sym 0)
    top (scop 0 (L anon) 0)
    (evens n) (? (atomp n) 0 (odds (B n)))
    (odds n) (? (atomp n) 0 (X (A n) (evens (B n))))
    zget (tget 0)
    (arity c) (+ (llen (zget c 'arg)) (llen (zget c 'imp)))
    (thd0 r n) (poke i_ret (seek -1 (poke (arity r) (seek (+ 1 n) (thd (+ 2 n))))))
    (cpush c k v) (, (tset c k (X v (zget c k))) v)
    (import c v) (? (!= top c) (cpush c 'imp v))
    (cpop c k) (: s (zget c k) (, (tset c k (B s)) (A s)))
    (cpeek c k) (A (zget c k))
    (em1 i k n) (poke i (seek -1 (k (+ 1 n))))
    (em2 i x k) (em1 i (em1 x k))
    imm (em2 i_imm)
    (cata_var c x ins j m)
     (poke i_ref (seek -1 (poke (+ (stkidx c x) ins) (seek -1 (j (+ 2 m))))))
    (stkidx d x)
      (: imp  (zget d 'imp)
         arg  (zget d 'arg)
         ii (idx imp x)
         ai (idx arg x)
       (? (>= ii 0) ii
          (>= ai 0) (+ (llen imp) ai)
          -1))
    (ana s c x) (:- (? (symp x)  (ana_sym s c x)
                       (atomp x) (imm x)
                                 (ana_two s c (A x) (B x)))
     (ana_apl s c b)
      (? (atomp b) id
         (<=< (ana s c (A b))
              (em1 i_ap)
              (ana_apl s c (B b))))
     (ana_ap s c f b)  (<=< (ana s c f) (ana_apl (X 0 s) c b))
     (ana_sym s c x) (>>= c (:- ana_sym_r
      (idx l i) (>>= l 0 (: (ii l n) (? l (? (= i (A l)) n (ii (B l) (inc n))) -1)))
      (ana_sym_local_fn asq d)
       (<=< (em2 i_lazy_bind (X (A asq) (zget d 'lam)))
            (ana_apl (X 0 s) c (BB asq)))
      (ana_sym_stack_ref x d) (,
       (? (nilp (= c d)) (import c x))
       (cata_var d x (llen s)))
      (ana_sym_local_def stack)
       (em2 i_ref (lidx x stack))
      (ana_sym_free x)
       (: y (tget anon globals x)
          (? (!= y anon) (imm y)
          (, (import c x) (em2 i_free_variable x))))
      (ana_sym_r d)
       (? (nilp d) (ana_sym_free x)
        (: y (assq x (zget d 'lam))
         (? y (ana_sym_local_fn y d)
          (: y s
           (? (memq x y)          (ana_sym_local_def y)
              (>= (stkidx d x) 0) (ana_sym_stack_ref x d)
                                  (ana_sym_r (zget d 'par)))))))))

     (ana_two s c a b) (:- (? (= a '`)  (imm (A b))     ; ok
                              (= a '?)  (ana_if s c b)    ; ok
                              (= a '\)  (ana_lam s c b)   ; ok
                              (= a ':)  (ana_let c b)   ; to do
                              (= a ',)  (ana_seq s c b)   ; ok
                              (atomp b) (ana s c a)       ; ok
                              (: m (zget macros a)
                               (? m (ana s c (m b))       ; ok
                                    (ana_ap s c a b))))   ; ok

      (ana_lam s c b) (? (atomp b)     (imm 0)
                         (atomp (B b)) (ana s c (A b))
                                       (ana s c (ana_ll c 0 b)))
      (ana_ll c imp exp) (:
       arg (init exp)
       x (last exp)
       d (scop c arg imp)
       k0 (ana 0 d x (thd0 d))
       ar (arity d)
       k ((? (> ar 1) (em2 i_curry ar) id) k0 0)
       (X k (zget d 'imp)))

      (desug n d) (? (atomp n) (X n d)
                     (desug (A n) (X '\ (cat (B n) (L d)))))
      (lambp x) (&& (twop x) (= '\ (A x)))

      (ana_let c b) (:-
       (? (atomp b)     (imm 0)
          (atomp (B b)) (ana 0 c (A b))
          (l1 0 0 (A b) (AB b) (BB b)))
       q (scop c (zget c 'arg) (zget c 'imp))
       ; l1 collects bindings and passes them with the body expression to l2
       (l1 noms defs nom def rest) (:
        nd1 (desug nom def)
        nom1 (A nd1) def1 (B nd1)
        noms1 (X nom1 noms) defs1 (X def1 defs)
        (? (atomp rest) (l2 noms1 defs1 nom1)
           (atomp (B rest)) (l2 noms1 defs1 (A rest))
           (l1 noms1 defs1 (A rest) (AB rest) (BB rest))))

       ; l2 finds closures for all local functions and passes a lambda for the body to l3
       (l2 noms defs exp) (:- (l3 rnoms rdefs clams llam)
        rnoms (rev noms)
        rdefs (rev defs)
        lams (>>= 0 noms defs
              (: (b l n d) (? (atomp n) l
               (: ll (? (not (lambp (A d))) l (X
                                                 (X (A n)
                                                  (ana_ll q 0 (BA d))
                                                 )
                                               l))
                (b ll (B n) (B d))))))
        # find transitive closures of closures
        # exclude local functions from closures
        (set_cdr p x) (, (poke x (seek 3 p)) x) # don't do this :<
        (cl n l l1 l2) (?
         l2 (? (&& (!= l1 l2) (memq (AA l1) (BB (A l2))))
             (>>= n (BB (A l1)) (: (f n v)
                            (? v (: var ( (A v))
                                    vars ( (B (BA l2)))
                                    (? (memq var vars)
                                     (f n (B v))
                                     (, (set_cdr (BA l2) (X var vars))
                                        (f (+ 1 n) (B v)))))
                             (cl n l l1 (B l2)))))
             (cl n l l1 (B l2)))
         l1 (cl n l (B l1) l)
         n (close l)
         l)
        (close l) (cl 0 l l l)
        lnoms (map A lams)
        (lamdel cs) (flip map cs (\ ll (X (A ll) (X (AB ll) (foldl (BB ll) (flip ldel) lnoms)))))
        clams (lamdel (close lams))
        _ (? clams clams)
        llam (X '\ (cat noms (L exp)))) ;; construct reversed lambda expression
        ;; l3 collects def values on stack and applies lambda


       (l3 noms defs clams llam) (:
         (loop s nds) (? (nilp nds) id
          (: nd (A nds)
             n (A nd)
             d (B nd)
             d1 (? (not (lambp d)) d
                 (: qa (assq n clams)
                    x (ana_ll q (BB qa) (B d))
                  (set_cdr qa x)))
             (<=< (ana s c d1) (loop (X n s) (B nds)))))
         _ (tset q 'lam clams)
         a (llen noms)
         ap (? (> a 1) (em2 i_apn a) (em1 i_ap))
         (<=< (ana s c llam) (loop s (zip noms defs)) ap))) ; end ana_let

      (ana_seq s c x)
       (? (atomp x) (imm 0)
        (<=< (ana s c (A x))
             (? (atomp (B x)) id
                              (<=< (em1 i_drop1)
                                   (ana_seq s c (B x))))))
      (ana_if s c b) (:- (<=< (pop 'end) (ana_if_r b) (push 'end))
       (pop y k n) (: j (k n) (, (cpop c y) j))
       (push y k n) (cpush c y (k n))
       (peek_end c k n) (: j (k (+ 2 n))
        ; FIXME tail calls
        (poke i_jump (seek -1 (poke (cpeek c 'end) (seek -1 j)))))
       (pop_alt c k n) (: j (k (+ 2 n))
        (poke i_cond (seek -1 (poke (cpop c 'alt) (seek -1 j)))))
       (ana_if_r b) (?
        (atomp b) (imm 0)
        (atomp (B b)) (<=< (ana s c (A b)) (peek_end c))
        (<=< (ana s c (A b))
             (pop_alt c)
             (ana s c (AB b))
             (peek_end c)
             (push 'alt)
             (ana_if_r (BB b))))))))))
  boot_script '
   (\ fs (:- (? args       (procs prog (A args) (B args))
                (isatty 0) (repl prompt)
                           (reads ()))

          prog (A fs) args (B fs)
          prompt "    "
          (reads l) (: r (read ()) (? r (, (ev (A r)) (reads l)) l))
          (repl p)  (: r (, (puts p) (read 0))
                     (? r (, (. (ev (A r))) (putc 10) (repl p))))
          (procs prog a as) (, (proc1 prog a)
                               (? as (procs prog (A as) (B as))))
          (proc1 prog arg) (:-
           (? (= arg "-h") (, (puts "usage: ") (puts prog) (puts help))
                (= arg "-v") (, (puts prog) (puts " ") (puts version) (putc 10))
                (= arg "-r") (repl prompt)
                ((: (evals x) (? x (, (ev (A x)) (evals (B x)))))
                 (readf arg)))
           help " [args]
        args:
          -h    show this message
          -v    show version
          -r    start repl
          file  evaluate file
")))

(, ((: (go a b) (? b (go (ev (A b)) (B b)) a)) 0 prelude)
(tset globals 'prelude prelude)
; (each (tdel 0 globals) (cat '(peek poke seek macros thd globals) (filter (\ y (= "i_" (ssub (nom y) 0 2))) (tkeys globals))))
   (ev boot_script)
   ))
