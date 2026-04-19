(:
; there are three let bindings
 ; the prelude is a list of the standard library definitions.
 ; it is evaluated twice during different phases of initialization.
 egg '(
  ; these are all data and function definitions
  (: (co f g x) (f (g x))
     (id x) x
     (const x _) x
     (flip f x y) (f y x))
  (: true -1 false 0 not nilp
     (atomp x) (nilp (twop x))
     (!= a b) (? (= a b) 0 -1)
     (AA x) (A (A x)) (AB x) (A (B x))
     (BA x) (B (A x)) (BB x) (B (B x)))
  (: caar AA cadr AB cdar BA cddr BB
     caaar (co car caar) caadr (co car cadr)
     cadar (co car cdar) caddr (co car cddr)
     cdaar (co cdr caar) cdadr (co cdr cadr)
     cddar (co cdr cdar) cdddr (co cdr cddr)
     inc (+ 1) dec (+ -1) (:: a b) (put a b macros))
  (: (map f l) (? (twop l) (cons (f (car l)) (map f (cdr l))))
     (foldl z f l) (? (twop l) (foldl (f z (car l)) f (cdr l)) z)
     (foldr z f l) (? (twop l) (f (car l) (foldr z f (cdr l))) z))
  (: (foldl1 f l) (foldl (car l) f (cdr l))
     (foldr1 f l) (foldr (last l) f (init l))
     (filter p l) (? (twop l) (: m (filter p (cdr l)) (? (p (car l)) (cons (car l) m) m)))
     (init l) (? (cdr l) (cons (car l) (init (cdr l))))
     (last l) (? (cdr l) (last (cdr l)) (car l))
     (each l f) (? (twop l) (: _ (f (car l)) (each (cdr l) f)))
     (ldel x l) (? (twop l) (? (= (car l) x) (cdr l) (cons (car l) (ldel x (cdr l)))))
     (all f l) (? (twop l) (? (f (car l)) (all f (cdr l))) -1)
     (any f l) (? (twop l) (? (f (car l)) -1 (any f (cdr l))))
     (cat a b) (foldr b cons a))
  (: catmap (co (foldr 0) (co cat))
     (assq x l) (? l (? (= x (caar l)) (car l) (assq x (cdr l))))
     (lidx x) ((: (f n l) (? (twop l) (? (= x (car l)) n (f (+ 1 n) (cdr l))) -1)) 0)
     memq (co any =)
     (zip a b) (? (twop a) (? (twop b) (cons (cons (car a) (car b)) (zip (cdr a) (cdr b)))))
     rev (foldl 0 (flip cons))
     (drop n l) (? n (drop (- n 1) (cdr l)) l)
     (take n l) (? n (cons (car l) (take (- n 1) (cdr l))))
     (part p) (foldr '(0) (\ a m (? (p a) (cons (cons a (car m)) (cdr m))
                                          (cons (car m) (cons a (cdr m)))))))
  ; here are some macro definitions
  (: l (foldr 0 (\ a l (cons cons (cons a (cons l 0))))) (:
   _ (:: 'L l)
   _ (:: 'list l)))
  (:: '&& (\ l (: (and l) (? (cdr l) (cons '? (cons (car l) (cons (and (cdr l)) 0))) (car l)) (? l (and l) -1))))
  (:: '|| (\ l (: (or l) (? l (: y (sym 0) (list ': y (car l) (list '? y y (or (cdr l)))))) (or l))))
  (:: ':- (\ a (cons ': (cat (cdr a) (cons (car a) 0)))))
  (:: '?- (\ a (cons '? (cat (cdr a) (cons (car a) 0)))))
  (:: '>>= (\ l (cons (last l) (init l))))
  (:: ', (\ l (cons ': (foldr (list (last l)) (\ l r (cons '_ (cons l r))) (init l)))))
  (:: '<=< (\ g (: y (sym 0) (list '\ y (foldr y (\ f x (list f x)) g)))))
  
 ; end of prelude

 ; next is an expression for the evaluator. this is evaluated three
 ; times by different init stages.
  (:- (\ x (: c (sco 0 (list 0) 0) (ana c x (k0 c) 0 0)))
    (sco p a i) (put 'val (new 0) (put 'par p (put 'imp i (put 'arg a (new 0)))))
    (p2 i x k) (poke -1 i (poke -1 x k))
    (em1 x k n) (poke -1 x (k (+ 1 n)))
    (em2 i x k n) (poke -1 i (poke -1 x (k (+ 2 n))))
    (k0 c n) (poke -1 g_vm_ret (poke (+ 1 n) (ary c) (thd (+ 2 n))))
    (ary c) (+ (len (get 0 'arg c)) (len (get 0 'imp c)))
    (pro f) (? (nump f) f (: a (peek 0 f) (?- f
     (= a g_vm_unc) (pro (seek -2 (peek 2 f)))
     (= a g_vm_cur) (?- f (= g_vm_unc (peek 2 f))
                            (pro (seek -2 (peek 4 f)))))))
    (kim x k n) (poke -1 g_vm_quote (poke -1 x (k (+ 2 n))))
    (ana c x) (:- (? (symp x)  (ava c x)
                     (atomp x) (kim x)
                     (: a (car x) b (cdr x) (?
                      (atomp b) (ana c a)
                      (= a '` ) (kim (car b))
                      (= a '? ) (aco b)
                      (= a '\ ) (ana c (? (atomp (cdr b)) (car b) (ali c 0 b)))
                      (= a ': ) (ale (car b) (cdr b))
                      (: m (get 0 a macros)
                       (? m (ana c (m b)) (app a b))))))
    (push k x) (: _ (put k (cons x (get 0 k c)) c) x)
    (pop k) (: x (get 0 k c) _ (put k (cdr x) c) (car x))
    (app a b) (: f (ana c a) ; analyze function expression
                 ca (len b)                                  ; call arity
                 i (? (= kim (pro f)) (peek 3 f))
                 i (? (nump i) 0 i)
                 fa (? (nump i) 1 (!= g_vm_cur (peek 0 i)) 1 (peek 1 i)) ; function arity
                 ub (&& i (= 1 ca) (= g_vm_ret0 (peek 1 i))) ; unary bif?
                 na (&& (< 1 ca) (= ca fa))                  ; n-ary ap?
                 nb (&& na (= g_vm_ret0 (peek 3 i)))         ; n-ary bif?
                 s (get 0 'stk c)                            ; get original stack
                 (? ub (co (ana c (A b)) (em1 (peek 0 i)))   ; unary bif
                    nb (: k (apr2l b) _ (put 'stk s c)
                        (co k (em1 (peek 2 i))))             ; n-ary bif
                  (: _ (push 'stk 0)                         ; stack rep of analyzed function f
                     g (? na (co (apr2l b) (kap ca)) (apl2r b)) ; r2l or l2r?
                     _ (put 'stk s c)                        ; put original stack
                   (co f g))))
   (apl2r b) (?- id (twop b) (: f (ana c (car b)) g (apl2r (cdr b)) (co f (co (kap 1) g))))
   (apr2l b) (?- id (twop b) (: g (apr2l (cdr b)) f (ana c (car b)) _ (push 'stk 0) (co g f)))
   (kap n k m)
    (: j (k (+ 2 m))
     (? (= (peek 0 j) g_vm_ret)
      (? (> n 1) (poke -1 g_vm_tapn (poke 0 n j)) (poke 0 g_vm_tap j))
      (? (> n 1) (p2 g_vm_apn n j) (poke -1 g_vm_ap j))))

    ;aco is a bit complicated
    (aco b) (:- (: f (acr b) (\ k n (: k (f (co (push 'end) k) n) _ (pop 'end) k)))
     (acx k n) (: ; jump out
      j (k (+ 3 n))
      a (car (get 0 'end c))
      i (peek 0 a)
      (? (| (= g_vm_ret i) (= g_vm_tap i)) (p2 i (peek 1 a) j)
         (= g_vm_tapn i)                   (p2 i (peek 1 a) (poke -1 (peek 2 a) j))
                                           (p2 g_vm_jump a j)))
     (acr b) (?
      (atomp b)       (kim 0)
      (atomp (cdr b)) (co (ana c (car b)) acx)
      (: f (ana c (car b))
         g (ana c (cadr b))
         h (acr (cddr b))
         (? (= kim (pro f)) (? (peek 3 f) g h)
          (\ x (f (\ n
           (: k (co (push 'alt) (h x))
              j (g (acx k) (+ 2 n))
              (p2 g_vm_cond (pop 'alt) j)))))))))

    ; variable expression analyzer
    (ava d x)
     (? (nilp d) ; outside all lexical scopes?
         (: z (sym 0)
            y (get z x globals) ; check global scope
          (? (!= y z) (kim y) ; if it's there use that
           (: _ (? (get 0 'par c) (push 'imp x))
            (em2 g_vm_freev x))))
      (: lfd (assq x (get 0 'lam d))
       (? lfd (: p (em2 g_vm_lazyb lfd)
                 _ (push 'stk 0)
                 q (apl2r (cddr lfd))
                 _ (pop 'stk)
               (co p q))
          (: stk (get 0 'stk d)
             (stki d) (lidx x (cat (get 0 'imp d) (get 0 'arg d)))
             q (\ i j m (: k (j (+ 2 m)) (p2 g_vm_arg (+ i (stki c)) k)))
           (?- (ava (get 0 'par d) x)
            (memq x stk) (? (= c d) (em2 g_vm_arg (lidx x stk))
                          (: _ (? (get 0 'par c) (push 'imp x))
                           (q (len (get 0 'stk c)))))
            (<= 0 (stki d)) (: _ (&& (!= c d) (get 0 'par c) (push 'imp x))
                             (q (len (get 0 'stk c)))))))))


    ; lambda analyzer
    (ali c imp exp) (:
     d (sco c (init exp) imp)
     k (ana d (last exp) (k0 d))
     a (ary d)
     k (trim ((? (= a 1) k (em2 g_vm_cur a k)) 0))
     (cons k (get 0 'imp d)))

    ; let expression analyzer (the most complicated one)
    (ale a b) (?
     (atomp b) (ana c a)
     (:- (l1 0 0 a (car b) (cdr b))
      q (sco c (get 0 'arg c) (get 0 'imp c))
      (set_cdr p x) (: _ (poke 3 x p) x) ; :[ weh
      (lambp x) (? (twop x) (= '\ (car x)))
      ;; l1 pass nom def and value expressions to l2
      ; l1 collects bindings and passes them with the body expression to l2
     (l1 ns ds n d rest) (:
      (dsug n d) (? (atomp n) (cons n d)
                     (dsug (car n) (cons '\ (cat (cdr n) (list d)))))
       nd (dsug n d) ns (cons (car nd) ns) ds (cons (cdr nd) ds)
      (? (atomp rest)       (l2 ns ds (car nd)   1)
         (atomp (cdr rest)) (l2 ns ds (car rest) 0)
                            (l1 ns ds (car rest) (cadr rest) (cddr rest))))

     (l2 ns ds exp even) (:- (cl 0 l l l)
      (jj a n d) (? (atomp n) a (nilp (lambp (car d))) (jj a (cdr n) (cdr d))
       (: k (car n) v (ali q 0 (cdar d)) a (cons (cons k v) a) (jj a (cdr n) (cdr d))))
      l (jj 0 ns ds)
      (cl n l k1 k2) (?
       (&& k1 k2 (!= k1 k2) (memq (caar k1) (cddar k2)))
        (>>= n (cddar k1) (: (kk n v)
         (? (nilp v) (cl n l k1 (cdr k2))
          (: var (car v)
             vars (cddar k2)
             n (? (memq var vars) n
                (: _ (set_cdr (cdar k2) (cons var vars))
                 (+ 1 n)))
             (kk n (cdr v))))))
       k2 (cl n l k1 (cdr k2))
       k1 (cl n l (cdr k1) l)
       n (cl 0 l l l)
       (l3 ns ds exp even
        (flip map l (\ x (cons (car x) (cons (cadr x) (foldl (cddr x) (flip ldel) (map car l)))))))))

     (l3 ns ds exp even lams) (:
      (ll nds) (? (nilp nds) id
       (: nd (car nds) n (car nd) d (cdr nd)
          d (?- d (lambp d) (: qa (assq (car nd) lams)
                               x (ali q (cddr qa) (cdr d))
                             (set_cdr qa x)))
          f (ana c d)
          g (?- id (&& even (nilp (get 0 'par c))) (em2 g_vm_defglob n))
          _ (push 'stk n)
          h (ll (cdr nds))
          (\ x (f (g (h x))))))
      _ (put 'lam lams q)
      s (get 0 'stk c)
      f (ana c (cons '\ (cat ns (list exp))))
      _ (push 'stk 0)
      g (ll (zip (rev ns) (rev ds)))
      h (kap (len ns))
      _ (put 'stk s c)
      (\ x (f (g (h x))))))))))
 (go e z a) (? a (go e (e (car a)) (cdr a)) z)
 t0 (clock 0)
 e (go (go ev 0 egg) 0 egg)
 (put 'boot_ms (clock t0) (put 'ev e globals)))
