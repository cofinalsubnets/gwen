(:
; there are three let bindings
 ; the prelude is a list of the standard library definitions.
 ; it is evaluated twice during different phases of initialization.
 prelude '(
  ; these are all data and function definitions
  (: true -1 false 0 not nilp
     (atomp x) (nilp (twop x))
     (!= a b) (? (= a b) 0 -1)
     (AA x) (A (A x)) (AB x) (A (B x))
     (BA x) (B (A x)) (BB x) (B (B x))
     car A cdr B caar AA cadr AB cdar BA cddr BB
     inc (+ 1) dec (+ -1) :: (tset macros)
     (id x) x (const x y) x (co f g x) (f (g x))
     (flip f x y) (f y x) (diag f x) (f x x)
     (map f l) (? (twop l) (X (f (A l)) (map f (B l))))
     (foldl z f l) (? (twop l) (foldl (f z (A l)) f (B l)) z)
     (foldr z f l) (? (twop l) (f (A l) (foldr z f (B l))) z)
     (foldl1 f l) (foldl (A l) f (B l))
     (foldr1 f l) (foldr (last l) f (init l))
     (filter p l) (? (twop l) (: m (filter p (B l)) (? (p (A l)) (X (A l) m) m)))
     (init l) (? (B l) (X (A l) (init (B l))))
     (last l) (? (B l) (last (B l)) (A l))
     (each l f) (? (twop l) (, (f (A l)) (each (B l) f)))
     (ldel x l) (? (twop l) (? (= (A l) x) (B l) (X (A l) (ldel x (B l)))))
     (all f l) (? (twop l) (? (f (A l)) (all f (B l))) -1)
     (any f l) (? (twop l) (? (f (A l)) -1 (any f (B l))))
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
               (X (A m) (X a (B m))))))
     (.. x) (, (. x) (putc 10) x)
     (llen l) (? (twop l) (+ 1 (llen (B l)))))
  ; here are some macro definitions
  (:: 'L (foldr 0 (\ a l (X X (X a (X l 0))))))
  (:: '&& (\ l (: (and l) (? (B l) (X '? (X (A l) (X (and (B l)) 0))) (A l)) (? l (and l) -1))))
  (:: '|| (\ l (: (or l) (? l (: y (sym 0) (L ': y (A l) (L '? y y (or (B l)))))) (or l))))
  (:: ':- (\ a (X ': (cat (B a) (X (A a) 0)))))
  (:: '>>= (\ l (X (last l) (init l))))
  (:: '|> (foldl1 (\ m f (L f m ))))
  (:: '>=> (\ g (: y (sym 0) (L '\ y (foldl y (\ x f (L f x)) g)))))
  (:: '<=< (\ g (: y (sym 0) (L '\ y (foldr y (\ f x (L f x)) g))))))
 ; end of prelude

 ; next is an expression for the evaluator. this is evaluated three
 ; times by different init stages.
  evaluator '(:- ; expression for the eval function
    (\ x (: c (scop 0 (L 0) 0) (ana c x (thd0 c) 0 0)))

    (scop par arg imp) (:
     t (tnew 0)
     _ (tset t 'par par)
     _ (tset t 'arg arg)
     _ (tset t 'imp imp)
     t)
    (p1 x k) (poke x (seek -1 k))
    (thd0 c n) (p1 g_vm_ret (poke (arity c) (seek (+ 1 n) (thd (+ 2 n)))))
;; functions for working with variable scope records
    (argof c) (tget 0 c 'arg)
    (impof c) (tget 0 c 'imp)
    (arity c) (+ (llen (argof c)) (llen (impof c)))
;; thread initializer

    (ana c x) (:- (?
     (symp x)  (ana_sym_r c x)
     (atomp x) (em2 g_vm_quote x)
     (: a (A x) b (B x) (?
      (= a '` ) (em2 g_vm_quote (A b))
      (= a '? ) (ana_if  c b)
      (= a '\ ) (? (atomp b)     (em2 g_vm_quote 0)
                   (atomp (B b)) (ana c (A b))
                   (ana c (ana_ll c 0 b)))
      (= a ': ) (ana_let c b)
      (= a ', ) (ana_seq c b)
      (atomp b) (ana c a)
      (: m (tget 0 macros a) (? m
       (ana c (m b))
       (ana_ap a b))))))

     (ana_ap a b)
     (: a0 (ana c a)
        a1 (ana_apl 0 c b)
      (co a0 a1))

     (toplp c) (nilp (parof c))
     (parof c) (tget 0 c 'par)
     (em1 i k n) (p1 i (k (+ 1 n)))
     (em2 i x k n) (p1 i (p1 x (k (+ 2 n))))
     (cpeek k) (A (tget 0 c k))
     (cpush k v) (, (tset c k (X v (tget 0 c k))) v)
     (cpop k) (: s (tget 0 c k) _ (tset c k (B s)) (A s))
     (em_ap k n) (:
      j (k (+ 1 n))
      (? (= (peek j) ret) (poke g_vm_tap j)
                          (p1 g_vm_ap j)))
     (em_apn n k m) (:
      j (k (+ 2 m))
      (? (= (peek j) g_vm_ret) (p1 g_vm_tapn (poke n j))
                            (p1 g_vm_apn (p1 n j))))
     ; ana_seq is recursive but pretty simple
     (ana_seq c x)
      (? (atomp x) (em2 g_vm_quote 0)
       (: a0 (ana c (A x))
          a1 (? (atomp (B x)) id
              (co (em1 g_vm_drop1) (ana_seq c (B x))))
        (co a0 a1)))
    ;ana_if is a bit complicated
    (ana_if c b) (:-
     (: a0 (pop 'end)
        a1 (ana_if_r b)
        a2 (push 'end)
      (>=> a2 a1 a0))
     ; where
     (pop y k n) (: j (k n) (, (cpop y) j))
     (push y k n) (cpush y (k n))
     (ana_if_r b) (:
      (pop_alt k n) (:
       j (k (+ 2 n))
       alt (cpop 'alt)
       (p1 g_vm_cond (p1 alt j)))
      (peek_end k n) (:
       j (k (+ 3 n))
       a (cpeek 'end)
       i (peek a)
       (?  (|| (= i g_vm_ret) (= i g_vm_tap))
            (p1 i (p1 (peek (seek 1 a)) j))
           (= i g_vm_tapn)
            (p1 i (p1 (peek (seek 1 a)) (p1 (peek (seek 2 a)) j)))
           (p1 g_vm_jump (p1 a j))))
     (?
      (atomp b) (em2 g_vm_quote 0)
      (atomp (B b))
       (: a0 (ana c (A b))
          a1 peek_end
        (>=> a1 a0))
      (: a0 (ana c (A b))
         a1 pop_alt
         a2 (ana c (AB b))
         a3 peek_end
         a4 (push 'alt)
         a5 (ana_if_r (BB b))
       (>=> a5 a4 a3 a2 a1 a0)))))

    (ana_apl n c b) (:
     (loop b)
      (? (atomp b) id
       (: a0 (ana c (A b))
          a1 em_ap
          a2 (loop (B b))
        (>=> a2 a1 a0)))
     _ (cpush 'stk n)
     a (loop b)
     _ (cpop 'stk)
     a)

    (ana_sym_r d x) (:
     (stki d)
       (: imp (impof d)
          i (lidx x imp)
        (? (>= i 0) i
         (: i (lidx x (argof d))
          (? (< i 0) i (+ (llen imp) i)))))

     (import v)
      (? (not (toplp c)) (cpush 'imp v))

     (cata_var c) (:
         i (llen (tget 0 c 'stk))
         (\ j m (:
          k (j (+ 2 m))
          i (+ i (stki c))
          (p1 g_vm_arg (p1 i k)))))

     (? (nilp d) ; outside all lexical scopes?
         (: a0 (sym 0)
            y (tget a0 globals x) ; check global scope
          (? (!= y a0) (em2 g_vm_quote y) ; if it's there use that
           (, (import x) ; if it's not there... do something
              (em2 g_vm_freev x))))

      (: ; else...
         stk (tget 0 d 'stk)
         lfd (assq x (tget 0 d 'lam))
       ; local function def?
       (?
        lfd
         (: a1 (em2 g_vm_lazyb lfd)
            a2 (ana_apl 0 c (BB lfd))
          (>=> a2 a1))
         ; is it bound on a let stack in this scope?
        (memq x stk)
         (? (= c d)
          (em2 g_vm_arg (lidx x stk))
          (, (import x)
             (cata_var c)))
        ; is it bound as a closure or argument variable?
        (<= 0 (stki d))
         (, (? (!= c d) (import x))
            (cata_var c))
        ; else recur
        (ana_sym_r (parof d) x)))))


    (ana_ll c imp exp) (:
     arg (init exp)
     x (last exp)
     d (scop c arg imp)
     k (ana d x (thd0 d))
     ar (arity d)
     k (trim ((? (> ar 1) (em2 g_vm_curry ar) id) k 0))
     imp (impof d)
     (X k imp))


    (ana_let c b) (?
     (atomp b)     (em2 g_vm_quote 0)
     (atomp (B b)) (ana c (A b))
     (:- (l1 0 0 (A b) (AB b) (BB b))
        q (scop c (argof c) (impof c))
        (set_cdr p x) (, (poke x (seek 3 p)) x) ; :[ weh
        (lambp x) (? (twop x) (= '\ (A x)))
        (desug n d) (? (atomp n) (X n d)
                       (desug (A n) (X '\ (cat (B n) (L d)))))
     ;; l1 pass nom def and value expressions to l2
     ; l1 collects bindings and passes them with the body expression to l2
     (l1 noms defs nom def rest) (:
      nd (desug nom def)
      nom (A nd)
      def (B nd)
      noms (X nom noms)
      defs (X def defs)
      (? (atomp rest)     (l2 noms defs nom      1)
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
             v (ana_ll q 0 (BA d)) ; first pass ..
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


      lnoms (map A lams)
      (ldd ll) (X (A ll) (X (AB ll) (foldl (BB ll) (flip ldel) lnoms)))
      clams (map ldd lams)
      llam (X '\ (cat noms (L exp)))
      _ (tset q 'lam clams)
      (lls n nds) (, (cpush 'stk n)
                     (ll nds))
      (ll nds) (? (nilp nds) id
       (: nd (A nds)
          n (A nd)
          d (B nd)
          d (? (lambp d) (: qa (assq n clams)
                            x (ana_ll q (BB qa) (B d))
                            (set_cdr qa x))
                         d)
          a1 (ana c d)
          a2 (? (&& (toplp c) even) (em2 g_vm_defglob n) id)
          a3 (lls n (B nds))
          (>=> a3 a2 a1)))
      s0 (tget 0 c 'stk)
      n  (llen noms)
      a1 (ana c llam)
      a2 (lls -1 (zip (rev noms) (rev defs)))
      a3 (? (> n 1) (em_apn n) em_ap)
      _ (tset c 'stk s0)
      (>=> a3 a2 a1))))))


 ; helper to evaluate each prelude expression in order
 (go e a) (? a (, (e (A a)) (go e (B a))))

 # init process
 t0 (clock 0)      ; start time
 e0 ev             ; stage 0 evaluator (C level)
 _ (go e0 prelude) ; stage 1 prelude (eval'd by C)
 e1 (e0 evaluator) ; stage 1 evaluator (eval'd by C);
 _ (go e1 prelude) ; stage 2 prelude (eval'd by G)
 e2 (e1 evaluator) ; stage 2 evaluator (eval'd by G)
 t1 (clock 0)      ; end time
 _ (tset globals 'boot_ms (- t1 t0))
 _ (tset globals 'ev e2) ; redefine eval
 _)
