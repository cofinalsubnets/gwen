(:
; there are three let bindings
 ; the prelude is a list of the standard library definitions.
 ; it is evaluated twice during different phases of initialization.
 egg '(
  ; these are all data and function definitions
  (: true -1 false 0 not nilp
     (atomp x) (nilp (twop x))
     (!= a b) (? (= a b) 0 -1)
     (AA x) (A (A x)) (AB x) (A (B x))
     (BA x) (B (A x)) (BB x) (B (B x))
     car A cdr B caar AA cadr AB cdar BA cddr BB
     inc (+ 1) dec (+ -1) (:: a b) (put a b macros)
     (id x) x
     (const x _) x
     (co f g x) (f (g x))
     (flip f x y) (f y x)
     (map f l) (? (twop l) (X (f (A l)) (map f (B l))))
     (foldl z f l) (? (twop l) (foldl (f z (A l)) f (B l)) z)
     (foldr z f l) (? (twop l) (f (A l) (foldr z f (B l))) z)
     (foldl1 f l) (foldl (A l) f (B l))
     (foldr1 f l) (foldr (last l) f (init l))
     (filter p l) (? (twop l) (: m (filter p (B l)) (? (p (A l)) (X (A l) m) m)))
     (init l) (? (B l) (X (A l) (init (B l))))
     (last l) (? (B l) (last (B l)) (A l))
     (each l f) (? (twop l) (: _ (f (A l)) (each (B l) f)))
     (ldel x l) (? (twop l) (? (= (A l) x) (B l) (X (A l) (ldel x (B l)))))
     (all f l) (? (twop l) (? (f (A l)) (all f (B l))) -1)
     (any f l) (? (twop l) (? (f (A l)) -1 (any f (B l))))
     catmap (co (foldr 0) (co cat))
     (cat a b) (foldr b X a)
     (assq x l) (? l (? (= x (A (A l))) (A l) (assq x (B l))))
     (lidx x) ((: (f n l) (? (twop l) (? (= x (A l)) n (f (+ 1 n) (B l))) -1)) 0)
     (memq x) (any (= x))
     (zip a b) (? (twop a) (? (twop b) (X (X (A a) (A b)) (zip (B a) (B b)))))
     rev (foldl 0 (flip X))
     (drop n l) (? n (drop (- n 1) (B l)) l)
     (take n l) (? n (X (A l) (take (- n 1) (B l))))
     (part p) (foldr '(0) (\ a m
      (? (p a) (X (X a (A m)) (B m))
               (X (A m) (X a (B m)))))))
  ; here are some macro definitions
  (:: 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
  (:: '&& (\ l (: (and l) (? (B l) (X '? (X (A l) (X (and (B l)) 0))) (A l)) (? l (and l) -1))))
  (:: '|| (\ l (: (or l) (? l (: y (sym 0) (L ': y (A l) (L '? y y (or (B l)))))) (or l))))
  (:: ':- (\ a (X ': (cat (B a) (X (A a) 0)))))
  (:: '>>= (\ l (X (last l) (init l))))
  (:: '|> (foldl1 (\ m f (L f m))))
  (:: ', (\ l (X ': (foldr (L (last l)) (\ l r (X '_ (X l r))) (init l)))))
  (:: '<=< (\ g (: y (sym 0) (L '\ y (foldr y (\ f x (L f x)) g)))))
  
 ; end of prelude

 ; next is an expression for the evaluator. this is evaluated three
 ; times by different init stages.
  (:- (\ x (: c (sco 0 (L 0) 0) (ana c x (k0 c) 0 0)))
    (sco p a i) (put 'val (new 0) (put 'par p (put 'imp i (put 'arg a (new 0)))))
    (p1 x k) (poke x (seek -1 k))
    (p2 i x k) (poke i (seek -1 (poke x (seek -1 k))))
    (em1 x k n) (p1 x (k (+ 1 n)))
    (em2 i x k n) (p2 i x (k (+ 2 n)))
    (k0 c n) (p1 g_vm_ret (poke (arity c) (seek (+ 1 n) (thd (+ 2 n)))))
;; functions for working with variable scope records
    (arity c) (+ (len (get 0 'arg c)) (len (get 0 'imp c)))

    (ana c x) (:- (?
     
     (symp x)  (foldl1 id (ava c x))
     (atomp x) (kim x)
     (: a (A x) b (B x) (?
      (atomp b) (ana c a)
      (= a '` ) (kim (A b))
      (= a '? ) (foldl1 id (aco c b))
      (= a '\ ) (? (atomp b)     (kim 0)
                   (atomp (B b)) (ana c (A b))
                                 (ana c (ala c 0 b)))
      (= a ': ) (foldl1 id (ale b))
      (: m (get 0 a macros)
       (? m (ana c (m b))
            (app a b))))))
    kim (em2 g_vm_quote)

    (app a b) (: f (ana c a) g (aplr 0 b) (\ x (f (g x))))
    (aplr n b) (:
     (l b) (? (atomp b) id
            (: f (ana c (A b))
               g (l (B b))
             (\ x (f (kapn 1 (g x))))))
     _ (put 'stk (X n (get 0 'stk c)) c)
     a (l b)
     _ (put 'stk (B (get 0 'stk c)) c)
     a)

   (kapn n k m) (: j (k (+ 2 m))
                 (? (= (peek j) g_vm_ret)
                  (? (> n 1) (p1 g_vm_tapn (poke n j)) (poke g_vm_tap j))
                  (? (> n 1) (p2 g_vm_apn n j)    (p1 g_vm_ap j))))

    ;aco is a bit complicated
    (aco c) (:-
     ;; enlist as a thunk
     (flip co (flip X 0) (X (\ b (: f (acr b) (\ k n (:
      k (f (\ n (: k (k n) _ (put 'end (X k (get 0 'end c)) c) k)) n)
      _ (put 'end (B (get 0 'end c)) c)
      k))))))
     (acr b) (:-
     (? (atomp b) (kim 0)
        (atomp (B b)) (co (ana c (A b)) peek_end)
        (: f (ana c (A b))
           g (ana c (AB b))
           h (acr (BB b))
         (\ x (f (\ n (:
          j (flip g (+ n 2) (peek_end (\ n
             (: k (h x n) _ (put 'alt (X k (get 0 'alt c)) c) k))))
          s (get 0 'alt c)
          _ (put 'alt (B s) c)
          (p2 g_vm_cond (A s) j)))))))
     (peek_end k n) (: j (k (+ 3 n)) a (A (get 0 'end c)) i (peek a)
      (? (|| (= i g_vm_ret) (= i g_vm_tap)) (p2 i (peek (seek 1 a)) j)
         (= i g_vm_tapn) (p2 i (peek (seek 1 a)) (p1 (peek (seek 2 a)) j))
         (p2 g_vm_jump a j)))))

    ; variable expression analyzer
    (ava d x) (:-
     (? (nilp d) ; outside all lexical scopes?
         (: z (sym 0)
            y (get z x globals) ; check global scope
          (? (!= y z) (L kim y) ; if it's there use that
           (: _ (? (get 0'par c) (put 'imp (X x (get 0 'imp c)) c))
            (L em2 g_vm_freev x))))

      (: lfd (assq x (get 0 'lam d))
       (? lfd (: p (em2 g_vm_lazyb lfd)
                 q (aplr 0 (BB lfd))
               (L co p q))
          (: stk (get 0 'stk d)
           (? (memq x stk)
               (? (= c d)
                (L em2 g_vm_arg (lidx x stk))
                (: _ (&& (get 0 'par c) (put 'imp (X x (get 0 'imp c)) c))
                 (L cata_var c)))
              (<= 0 (stki d)) ; is it bound as a closure or argument variable?
               (: _ (&& (!= c d) (get 0 'par c) (put 'imp (X x (get 0 'imp c)) c))
                (L cata_var c))
              (ava (get 0 'par d) x))))))

     (stki d) (lidx x (cat (get 0 'imp d) (get 0 'arg d)))

     (cata_var c) (:
      i (len (get 0 'stk c))
      (\ j m (: k (j (+ 2 m)) (p2 g_vm_arg (+ i (stki c)) k)))))


    ; lambda analyzer
    (ala c imp exp) (:
     d (sco c (init exp) imp)
     k (ana d (last exp) (k0 d))
     a (arity d)
     k ((? (= a 1) k (em2 g_vm_curry a k)) 0)
     (X (trim k) (get 0 'imp d)))


    ; let expression analyzer (the most complicated one)
    (ale b) (?
     (atomp b)     (L kim 0)
     (atomp (B b)) (L ana c (A b))
     (:- (L l1 0 0 (A b) (AB b) (BB b))
        q (sco c (get 0 'arg c) (get 0 'imp c))
        (set_cdr p x) (, (poke x (seek 3 p)) x) ; :[ weh
        (lambp x) (? (twop x) (= '\ (A x)))
        (desug n d) (? (atomp n) (X n d)
                       (desug (A n) (X '\ (cat (B n) (L d)))))
     ;; l1 pass nom def and value expressions to l2
     ; l1 collects bindings and passes them with the body expression to l2
     (l1 noms defs nom def rest) (:
      nd (desug nom def)
      noms (X (A nd) noms)
      defs (X (B nd) defs)
      (? (atomp rest)     (l2 noms defs (A nd)   1)
         (atomp (B rest)) (l2 noms defs (A rest) 0)
                          (l1 noms defs (A rest) (AB rest) (BB rest))))
     ;; l2 find closures and revise lambda defs then emit eval/apply
     ; l2 finds closures for all local functions and passes a lambda expression for the body to l3
     (l2 noms defs exp even) (:

      lams (:- (cl 0 l l l)

       l (>>= 0 noms defs (: (ll a n d) (?
          (atomp n) a
          (nilp (lambp (A d))) (ll a (B n) (B d))
          (: k (A n)
             v (ala q 0 (BA d)) ; first pass ..
             a (X (X k v) a)
           (ll a (B n) (B d))))))
       (cl n l l1 l2) (?
        (&& l1 l2 (!= l1 l2) (memq (AA l1) (BB (A l2))))
         (>>= n (BB (A l1)) (: (ll n v)
          (? (nilp v) (cl n l l1 (B l2))
           (: var (A v)
              vars (B (BA l2))
              n (? (memq var vars) n
                 (, (set_cdr (BA l2) (X var vars))
                    (+ 1 n)))
              (ll n (B v))))))
        l2 (cl n l l1 (B l2))
        l1 (cl n l (B l1) l)
        n (cl 0 l l l)
        l))

      (ldd ll) (X (A ll) (X (AB ll) (foldl (BB ll) (flip ldel) (map A lams))))
      clams (map ldd lams)
      _ (put 'lam clams q)
      (lls n nds) (: 0 (put 'stk (X n (get 0 'stk c)) c)
                       (ll nds))
      (ll nds) (? (nilp nds) id
       (: nd (A nds)
          n (A nd)
          d (B nd)
          d (? (lambp d) (: qa (assq n clams)
                            x (ala q (BB qa) (B d))
                            (set_cdr qa x))
                         d)
          f (ana c d)
          g (? (&& (nilp (get 0 'par c)) even) (em2 g_vm_defglob n) id)
          h (lls n (B nds))
          (\ x (f (g (h x))))))
      s (get 0 'stk c)
      f (ana c (X '\ (cat noms (L exp))))
      g (lls -1 (zip (rev noms) (rev defs)))
      h (kapn (len noms))
      _ (put 'stk s c)
      (\ x (f (g (h x))))))))))
 (go e z a) (? a (go e (e (A a)) (B a)) z)
 # init process
 t0 (clock 0)
 e (go (go ev 0 egg) 0 egg)
 (put 'boot_ms (- (clock 0) t0) (put 'ev e globals)))
