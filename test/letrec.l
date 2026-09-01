; --- test/letrec.l: the letrec capture laws under a shadowing binder ----------------------
; both letrecs load a sibling lambda's captures by NAME at each reference site and
; propagate them by name across siblings, so a lambda binder that shadows a name free in
; a sibling def used to hand the site its own value. both compilers now mint such a
; binder before analysis -- ev.l's cplam and c0_lambda hold the same law -- and every
; shape here answered wrongly without it, under either.
(assert
    ; a sibling's param shadows the name a later sibling captures; reads and calls both ways
    (5 = ((\ a (: (h a) (go a) (go b) (* b a) (h 5))) 1))
    (5 = ((\ a (: h (\ a (go a)) go (\ b (* b a)) (h 5))) 1))
    (6 = ((\ a (: (h a) (+ 1 (go a)) (go b) (* b a) (h 5))) 1))
    ('(5 6) = ((\ a (: (h a) (go a) (go b) (* b a) (map h [5 6]))) 1))
    (5 = ((\ a (: (h a) (go a) (k b) (* b a) (go c) (k c) (h 5))) 1))       ; a chain h -> k -> go
    (5 = ((\ a (: (h a) (: (in a) (go a) (in a)) (go b) (* b a) (h 5))) 1))  ; through a nested let
    (5 = ((\ a (: (h a) ((\ a (go a)) a) (go b) (* b a) (h 5))) 1))          ; an inline shadow too
    (5 = ((\ a (: (h x) ((\ a (go a)) x) (go b) (* b a) (h 5))) 1))
    ('(5 7) = ((\ a (: (h x) (map (\ a (go a)) [x 7]) (go b) (* b a) (h 5))) 1))   ; the inline survives
    ('(5 7) = ((\ a (: (go b) (* b a) (h x) (map (\ a (go a)) [x 7]) (h 5))) 1))   ; go first: same
    (5 = ((\ a (: (h a) (go a) (go b) (: (k a) (* b a) (k a)) (h 5))) 1))
    (5 = ((\ a (: (h a) (go a) (go b) ((\ c (* c b)) a) (h 5))) 1))
    (6 = ((\ a (: (h a) (: c (go a) c) (go b) (+ b a) (h 5))) 1))
    (8 = ((\ a b (: (h a) (go a) (go x) (+ x b) (h 5))) 1 3))
    (6 = ((\ a b (: (h b) (go b) (go x) (+ x a) (h 5))) 1 3))
    (5 = ((\ a (: (h a) (go a) (go b) (* b a) (: (m a) (h a) (m 5)))) 1))
    (5 = ((\ a (: (h a) (go a) (go b) (* b a) (: z (h 5) z))) 1))
    (2 = ((\ a (: (h a) (? (< a 3) (go a) (h (- a 1))) (go b) (* b a) (h 5))) 1))  ; h recurs first
    (5 = ((\ a (: (h a) (go (go a)) (go b) (* b a) (h 5))) 1))
    (5 = ((\ a (: (f a) (g a) (g a) (h a) (h b) (* b a) (f 5))) 1))          ; f -> g -> h, one name
    ('(10 10) = ((\ a (: (h a) (go a) (go b) (* b a) [(h 5) (go 5)])) 2))
    ; a lambda in the let body shadowing the capture
    ('(5 6) = ((\ a (: (go b) (* b a) (map (\ a (go a)) [5 6]))) 1))
    ; the shapes that were already right, held right: an inner : VALUE shadow (cells are
    ; by location), a sibling reading the let's own value name, and a shadowed let name
    (2 = ((\ a (: go (\ b (+ b a)) (: a 9 (go 1)))) 1))
    (11 = ((\ a (: v (+ a 1) go (\ b (+ b v)) h (\ v (go v)) (h 9))) 1))
    (4 = ((\ a (: (f y) (* y 2) g (\ f (f 3)) (g (\ z (+ z a))))) 1))
    ; the let* law shy's cluster rename must keep: a pre-pin read sees the outer
    (43 = ((\ p (: p (+ p 1) p)) 42))
    ; a sugared head's param shadowing a value sibling of the SAME `:`, both orders,
    ; and the explicit-lambda spelling it must agree with
    (3 = (: (x f) f  f 2  (x 3)))
    (7 = (: (x f y) (+ f y)  f 2  (x 3 4)))
    (3 = (: f 2  (x f) f  (x 3)))
    (3 = (: x (\ f f)  f 2  (x 3)))
    (11 = (: f 2  (g f) (+ f 1)  (g 10))))
