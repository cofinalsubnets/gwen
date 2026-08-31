; auto -- rebind `ev` so a qualifying closure compiles to native, transparently: no verb
; and no annotation, and the native is =/show-identical to its source. Everything else
; falls through to the base interpreter unchanged.
;
; the lanes, over the emitter's grammar E := var | int | (OP E E) | (? E E E),
; OP in + - * // % & | ^ < <= > >= = -- every recognizer here is exact-arity, since
; (+ x 1 2) is the tower ((+ x 1) 2) and not the two-operand add it reads as:
;   leaf   (\ x E), (\ p0 .. pn E) any arity, and (\ p (\ q E)) flattened to the 2-param
;   fleaf  (\ x F), F a float body -> a heap-boxed gem; sound only where the param is
;          floated at first use (allsafe), else the interpreter keeps an int subexpr exact
;   loop   (\ n ((: (go i a) (? (i < n) (go (i + 1) (a + B)) a)) 0 0)), a counted sum --
;          owning the back-edge is where the win is, well past the leaf's few percent
;   then the source rewrites (castbuild/twolow/autonat/autogroup) carry everything else.
;
; ev stays 1-arg, so the loader's `(ev 'ev r)` = `((ev 'ev) r)` survives the rebind;
; njit-loop and njit-loop-n call the captured base-ev, so a jit never re-enters ev.
; loads after love/glaze/emit.l. emit.l + this file are the `glaze` module (src/main.c
; registers the pair), so every name here is reached by (from 'glaze 'k) and none of them
; touches the book -- `ev` and `member?` are the exceptions the tail explains.
(: sfuel {})   ; autospec's unfold-fuel, a global because a sibling-captured value-local misses in the hatched ev; smix re-seeds it per specialization
(: glaze-loaded? (lit? (from 'glaze))   ; a re-load detector, read before this load defglob's anything: `glaze` in the registry means the baked glaze is already installed, and the `ev` rebind below hands it straight back
   ; the registry, never `book` -- a woken image has `book` sealed away and the read would scare
   ; the pure interpreter, captured once: on a re-load `ev` is auto-ev already, so read the interpreter
   ; out of the module book -- else base-ev is the glaze, and autospec's static fold re-enters it
   base-ev (: G (from 'glaze) (? G (G 'base-ev) ev))
   arch love-arch                         ; the host CPU as the assembler target ('amd64 / 'arm64), already a nom; glaze loads only on these
   x86? (id? arch 'amd64)                 ; gates the still-x86-only lanes (float result, counted loops, float grids)
   ; the emitter's expression grammar over var v, exact arity: (+ x 1 2) is a tower, not x + 1
   (exprp e v) (? (e = v) 1  (whole? e) 1  (atom? e) 0
                  (e @ (op a b) (member? op '(+ - * // % & | ^ < <= > >= =) && exprp a v && exprp b v)
                       ('? c a b) (exprp c v && exprp a v && exprp b v)
                       0))
   ; --- leaf: (\ x E) ---
   (qual ['\ x E]) (name? x && exprp E x)
   ; --- arity-N leaf: (\ p0 .. p_{n-1} E), E over the params (any distinct symbols) ---
   (exprn ps e) (? (member? e ps) 1  (whole? e) 1  (atom? e) 0
                   (e @ (op a b) (member? op '(+ - * // % & | ^ < <= > >= =) && exprn ps a && exprn ps b)
                        ('? c a b) (exprn ps c && exprn ps a && exprn ps b)
                        0))
   (distinctp l) (l @ (a . u) (!(member? a u) && distinctp u) 1)                ; no repeated param
   (qualn ('\ . u)) (: ps (init u)  body (last u)
                       (all name? ps && 1 < #ps && distinctp ps && exprn ps body))
   ; --- loop: the counted-sum skeleton (\ N ((: (go i a) (? (cmp i N) (go (+ i 1) (op a B)) a)) 0 a0)) ---
   ;     the pattern is exact at every level: one extra operand anywhere is a different program
   (loopinfo lam)
    (lam @ ['\ N [[': [GO I A] ['? [cmp I N] [GO ['+ I 1] [op A B]] A]] 0 z2]]
          (name? N && name? GO && name? I && name? A && I !== A
           && member? cmp '(< <= > >= =)                             ; the loop-test comparison
           && member? op '(+ *) && z2 = (foldinit op)                ; the fold op, a0 its identity (0 / 1)
           && exprp B I                                              ; the summed term, over i
           ? [B I op cmp] 0)
          0)
   ; --- call-out loop: loopinfo's shape, with the per-step term a call (F ARG) rather than arithmetic.
   ; answers (F op i ARG); the caller resolves F and compiles arg for the callout arg (emit.l jitcalloutx).
   (calloutinfo lam)                                                    ; the one param means F is free, hence a global
    (lam @ ['\ N [[': [GO I A] ['? [cmp I N] [GO ['+ I 1] [op A [F ARG]]] A]] 0 z2]]
          (name? N && name? GO && name? I && name? A && I !== A
           && member? cmp '(< <= > >= =)
           && member? op '(+ *) && z2 = (foldinit op)
           && name? F && F !== I && F !== A && F !== N && exprp ARG I
           ? [F op I ARG] 0)
          0)
   ; --- the same sum-over-(F i), with F and the bound N read from the [imps args] frame instead of baked:
   ; a captured import (ala prepends the imp) or a genuine multi-arg. answers (Fidx Nidx op ARG).
   (calloutinfo-frame lam)
    (: ps (init >lam)
       ((last >lam) @ [[': [GO I A] ['? [cmp I N] [GO ['+ I 1] [op A [F ARG]]] A]] 0 z2]
          (two? lam && <lam = '\ && 1 < #ps && all name? ps && distinctp ps   ; >=2 frame params
           && name? GO && name? I && name? A && I !== A
           && member? cmp '(< <= > >= =)
           && member? op '(+ *) && z2 = (foldinit op)
           && member? N ps && N !== I && N !== A                            ; the bound is a frame param
           && name? F && member? F ps && F !== I && F !== A && F !== N      ; ... and so is the callout head
           && exprp ARG I
           ? [(posof F ps 0) (posof N ps 0) op ARG] 0)
          0))
   (njit-loop lam B cv op cmp interp) (nif (emit (loopcode B cv op cmp arch)) interp lam 1)
   ; --- general n-variable loop: (\ n ((: (go v0..vk) (? T (go U..) R)) I0..Ik)),
   ; up to 3 loop vars + arbitrary update exprs -- this is the shape iterative fib/tak
   ; take (i,a,b with updates b and a+b), not just the fold skeleton above.
   (loopinfo-n lam)
    (lam @ ['\ N ([': (GO . vars) ['? T (GO . updates) R]] . inits)]
          (: fr (N . vars)                                            ; cgl frame: {n, v0..vk}
             (name? N && name? GO && two? vars && all name? vars && distinctp vars
              && !(member? GO vars) && #vars < 4                      ; <= 3 loop vars (register limit)
              && #updates = #vars && #inits = #vars
              && member? <T '(< <= > >= =) && exprn fr T              ; test a comparison over {n,vars}
              && all (u \ exprn fr u) updates && exprn fr R           ; updates + result exprs over {n,vars}
              && all whole? inits                                     ; inits are fixnum consts
              ? [vars updates T R inits] 0))
          0)
   ; --- c-finite bignum deopt (doc/misc/proto/cfinite.l): a constant-coefficient linear recurrence with a
   ;     canonical counter (init 0, step (c + 1), test (c < n)) closes to m^n x0, O(log n) -- so only
   ;     njit-loop-n's post-overflow path changes. linearity is a structural certificate, read off the
   ;     syntax by cf-lin: anything non-linear or non-canonical declines to `interp`. the state carries a
   ;     trailing `1` coordinate so an affine update reads linear, and M's last row [0..0 1] keeps it.
   (cf-nul? l)     !(two? l)
   (cf-zeros m)    (? (m = 0) () (0 . cf-zeros (m - 1)))
   (cf-unit m i)   (: (go j) (? (j < m) ((? (j = i) 1 0) . go (j + 1)) ()) (go 0))      ; e_i in dim m
   (cf-const m k)  ((cf-zeros (m - 1)) + [k])                                             ; [0..0 k]
   (cf-idx l v)    (: (go xs i) (? (cf-nul? xs) -1 (id? <xs v) i (go >xs (i + 1))) (go l 0))
   (cf-nth l i)    (? (i = 0) <l (cf-nth >l (i - 1)))
   (cf-dot a b)    (? (cf-nul? a) 0 ((<a * <b) + (cf-dot >a >b)))
   (cf-rowf f a b) (? (cf-nul? a) () (f <a <b . cf-rowf f >a >b))  ; elementwise
   (cf-scale k r)  (map (x \ k * x) r)
   (cf-const? r)   (all (x \ x = 0) (init r))                                              ; pure constant row
   (cf-lin vs e)                                                                            ; e -> augmented coeff row, or () if not integer-affine
    (: m (#vs + 1)
       (? (whole? e) (cf-const m e)
          (name? e)  (: i (cf-idx vs e) (? (i < 0) () (cf-unit m i)))                        ; a state var -> e_i ; else (e.g. n) decline
          (two? e && member? <e '(+ -) && 3 = #e)
            (: a (cf-lin vs <>e) b (cf-lin vs <>>e)
               (? (cf-nul? a || cf-nul? b) () (cf-rowf (? (id? <e '+) (\ x y (x + y)) (\ x y (x - y))) a b)))
          (two? e && id? <e '* && 3 = #e)                                     ; a product is linear only if a side is constant
            (: a (cf-lin vs <>e) b (cf-lin vs <>>e)
               (? (cf-nul? a || cf-nul? b) ()
                  (cf-const? a) (cf-scale (last a) b)
                  (cf-const? b) (cf-scale (last b) a)
                  ()))                                                                       ; var*var -> nonlinear -> decline
          ()))
   (cf-mat vs ups) (: m (#vs + 1) rows (map (u \ cf-lin vs u) ups)
                      (? (any cf-nul? rows) () (rows + [(cf-unit m (m - 1))])))           ; () if any update is non-linear
   (cf-id m)       (: (go i) (? (i < m) (cf-unit m i . go (i + 1)) ()) (go 0))
   (cf-tpose M)    (? (cf-nul? <M) () (map cap M . cf-tpose (map cup M)))
   (cf-mv M v)     (map (r \ cf-dot r v) M)
   (cf-mul A B)    (: Bt (cf-tpose B) (map (r \ map (c \ cf-dot r c) Bt) A))
   (cf-mpow M n)   (: D #M (go p acc base)
                      (? (p = 0) acc (: b2 (cf-mul base base) (? (1 = (p % 2)) (go (p // 2) (cf-mul acc base) b2) (go (p // 2) acc b2))))
                      (go n (cf-id D) M))                                                    ; m^n by repeated squaring, O(log n) matmuls
   (cf-aug inits)  (inits + [1])                                                          ; x0 = [inits.. 1]
   (cf-canon? lam vs ups test inits)                                                        ; counter c -> exactly n trips (the lambda param)?
    ((two? test) && (id? <test '<) && (3 = #test) && (id? <>>test <>lam)
     && (: c <>test i (cf-idx vs c)
           ((i >= 0) && ((cf-nth inits i) = 0)
            && (: u (cf-nth ups i) (two? u && id? <u '+ && 3 = #u && id? <>u c && <>>u = 1)))))
   ; matrix power if linear+canonical, else the caller's own deopt target `interp` -- never
   ; (base-ev lam) here, which under the natjit hook would re-enter ala
   (cf-deopt lam vs ups test result inits interp)
    (: M (cf-mat vs ups) rrow (cf-lin vs result)
       (? (cf-canon? lam vs ups test inits && !(cf-nul? M) && !(cf-nul? rrow))
          (n \ cf-dot rrow (cf-mv (cf-mpow M n) (cf-aug inits)))            ; the matrix-power closure is itself bytecode under the hook -- its body isn't leaf/loop grammar, so no re-entry
          interp))
   (njit-loop-n lam vars updates test result inits interp)
    (nif (emit (loopcode-n <>lam vars updates test result inits arch)) (cf-deopt lam vars updates test result inits interp) lam 1)
   ; --- captured-closure constructor (\ p (\ q E)): compiled as the flat 2-param native, whose partial
   ; application at p is =/show-identical to the interpreter's own partial. a curry-equivalent form.
   (qualc ['\ p ['\ q E]]) (name? p && name? q && p !== q && exprn [p q] E)
   (flatc ['\ p ['\ q E]]) ['\ p q E]                          ; (\ p (\ q E)) -> (\ p q E)
   ; --- float-result leaf (\ x <fexpr>): a body that boxes a gem, over +-*/ on a fixnum param and
   ;     float/int consts. soundness turns on when the param floats -- native cvtsi2sd's it at every use,
   ;     where the interpreter keeps an integer subexpression exact until a float forces promotion, so
   ;     ((x * 3) + 1.5) diverges at x=2^53+1. fty types each node F / I / 0(reject) and allsafe demands
   ;     the param never sit inside an I-typed compound, which lands the two promotion points together.
   (fty e v) (? (e = v) 'I  (gem? e) 'F  (whole? e) 'I                     ; param/int = I, float const = F
                (e @ (('+ || '- || '*) a b)                                ; +-*: float iff an operand is float
                       (: ta (fty a v) tb (fty b v)
                          (? (id? ta 0 || id? tb 0) 0  (id? ta 'F || id? tb 'F) 'F 'I))
                     ['/ a b]                                              ; /: float-division ok; int/int is value-dependent -> reject
                       (: ta (fty a v) tb (fty b v)
                          (? (id? ta 0 || id? tb 0) 0  (id? ta 'F || id? tb 'F) 'F 0))
                     0))
   (allsafe e v) (? !(occurs? v e) 1  (atom? e) 1  (id? (fty e v) 'I) 0    ; an I-typed compound holding the param -> late promotion -> reject
                    (allsafe <>e v && allsafe <>>e v))
   (qualfr ['\ v body]) (name? v && id? (fty body v) 'F && allsafe body v)   ; a float result, param floated everywhere
   ; --- the grid nest, D-dimensional: a letrec of D counted-loop defs wrapping a float recurrence.
   ; the recurrence self-recurses under an (&& (it < LIM) (cmp TEST BOUND)) escape test; each loop def
   ; is (? (cnt < N) (self ..outer.. (cnt + 1) ..(acc + INNERCALL)..) acc), the outermost driven by a
   ; trailing (driver 0 ..). autonat walks the chain for D bounds + counters, splits the recurrence into
   ; M invariants / N float-vars, and builds the whole grid via jitfgridn. M+N<=6 is the xmm budget.
   (anat-defs bs) (? (atom? >bs) [() <bs]             ; a : binding list -> (pairs body); pairs = ((nameform . body)..)
                     (: r (anat-defs >>bs)
                        [((<bs . <>bs) . <r) <>r]))
   (afind pred l) (? (atom? l) () (pred <l) <l (afind pred >l))
   ; the recurrence pair: a (? (&& ..) (SELF ..) ..) whose then-branch self-calls (head = its own name)
   (anat-rec? p) (: nm <<p
                    (>p @ ['? cond then _]
                        (two? cond && id? <cond '&& && two? then && id? <then nm)
                        0))
   ; anat-callee: the next-inner function a loop def calls -- innercall's head in (? (cnt < N)
   ; (self .. (acc + INNERCALL)) acc) -- or () if the pair doesn't match the loop shape. The outermost
   ; loop is the one no other loop names (the driver reaches it -- maybe wrapped, e.g. (bench .. (g 0 0))).
   (anat-callee q)
    (>q @ ['? _ then _]
        (? !(two? then) ()
           ((last >then) @ ['+ _ ic] (? (two? ic) <ic ()) ()))
        ())
   ; loopchain: walk loopnm's pair -- (? (cnt < N) (self ..(acc + INNERCALL)..) acc) -- record
   ; (cnt N rec-args), recursing on innercall's head until it reaches the recurrence recnm. The list
   ; runs outer->inner; () on any structural mismatch (so autonat bails to the unchanged form).
   (loopchain loopnm pairs recnm)
    (: p (afind (q \ id? <<q loopnm) pairs)
       (? !(two? p) ()
          (>p @ ['? ['< cnt bnd] then _]
              (? !(two? then) ()
                 ((last >then) @ ['+ _ ic]
                     (? !(two? ic) ()
                        (id? <ic recnm) [[cnt bnd >ic]]                    ; innermost: the recurrence call's args
                        (: rest (loopchain <ic pairs recnm)                ; else recurse on the next loop
                           (? (two? rest) ([cnt bnd ()] . rest) ())))
                     ()))
              ())))
   ; rebind: rebuild a : binding list, replacing the outermost loop's body with (native 0)
   (anat-rebind bs outernm ng)
    (? (atom? >bs) bs
       (: nm <bs bd <>bs
          (nm . ((? (two? nm && id? <nm outernm) [ng 0] bd) . anat-rebind >>bs outernm ng))))
   ; --- a group of mutually-recursive arithmetic fns in a `:`, each body in the integer fragment
   ; `groupok?` accepts: param | int | (op E E) | (cmp E E) | (? E E E) | (g E..), g any member of
   ; matching arity. autogroup takes the maximal closed subset -- a fn calling out of the fragment
   ; drops, taking its callers with it -- and rewrites each binding to its own native (jitgroupir), so
   ; a reference from anywhere resolves to native while the calls among the group stay in the buffer.
   ; --- lambda-lifting: a fn body (NL inits..), nl = (: (g q..) GBODY), is a local loop; lift g to a
   ;     top-level (g' FV.. q..) with fv the enclosing params free in gbody, threaded through g's own
   ;     self-calls, and leave the body as (g' FV.. inits..) -- so the loop joins the group's call graph.
   ;     one level only; a non-named-let body passes through untouched.
   (occurs? s e) (? (two? e) (foldl (\ a x (a || occurs? s x)) 0 e) (id? e s))                  ; s occurs (syntactically) in e?
   (subst-call g gn fv e) (? (two? e) (? (id? <e g) (gn . fv + (map (x \ subst-call g gn fv x) >e))   ; (g A..) -> (gn fv.. A'..)
                                          (map (x \ subst-call g gn fv x) e)) e)
   (named-let? e) ((two? e) && (: nl <e                                                      ; e = ((: (g q..) GBODY) inits..) ?
                      ((two? nl) && (id? <nl ':) && (two? >nl)
                       && (: gnf <>nl
                             ((two? gnf) && (name? <gnf) && (1 < #gnf) && (all name? >gnf) && (distinctp >gnf)
                              && (two? >>nl) && !(two? >>>nl)               ; (: (g q..) GBODY), body-less (one binding)
                              && (#>e = #>gnf))))))                       ; inits arity == g's arity
   (subst-sym nm val e) (? (id? e nm) val (two? e) (map (x \ subst-sym nm val x) e) e)            ; symbol nm -> val in e
   ; norm-let: a fn body (: aux.. (g q..) GBODY (g inits..)) -- value aux bindings, one local fn, a call
   ; result -- normalizes to the named-let ((: (g q..) GBODY[aux inlined]) inits..) that lift handles.
   ; the aux bindings are dropped, each leaving with its value, so auxclose closes the values against
   ; each other left to right; every use is a copy, hence only a dcheap? value goes -- a call would
   ; recompute at each use and an allocating one would hand each use a buffer of its own.
   (auxclose aps) (foldl (\ acc ap (acc + [(<ap . (foldl (\ b q (subst-sym <q >q b)) >ap acc))])) () aps)
   (auxinl aps x) (foldl (\ b ap (subst-sym <ap >ap b)) x (auxclose aps))
   (norm-let body)
    (? !(two? body && id? <body ': && !(atom? >body)) body
       (: parsed (anat-defs >body) pairs <parsed result <>parsed
          fnps (filter (pr \ two? <pr) pairs) auxps (filter (pr \ !(two? <pr)) pairs)
          (? !(1 = #fnps && two? result) body                                         ; need exactly one local fn + a call result
             !(all (ap \ dcheap? >ap) auxps) body                                          ; a call-valued aux does not copy -> leave the let alone
             (: g <<<fnps q ><<fnps gbody ><fnps
                (? !(id? <result g && #>result = #q) body            ; result must be (g inits..) matching arity
                   (: inl (x \ auxinl auxps x)                                             ; inline every aux name -> its closed value
                      ((': . ((g . q) . (inl gbody . ()))) . map inl >result)))))))
   (lift-pair p)                                                                                  ; (nameform . body) -> (rewritten-pair newdefs)
    (: nf <p pbody (norm-let >p)
       (? !(two? nf && name? <nf && 1 < #nf && named-let? pbody) [p ()]
          (: F <nf P >nf  nl <pbody inits >pbody
             gnf <>nl g <gnf q >gnf GBODY <>>nl
             gn (name (string F + "_" + string g))                                        ; lifted name F_g (unique per enclosing fn)
             fv (filter (pp \ occurs? pp GBODY && !(member? pp q)) P)                        ; enclosing params free in gbody
             [(nf . (gn . fv + inits))                                                 ; F's body -> (gn fv.. inits..)
               [((gn . fv + q) . subst-call g gn fv GBODY)]])))                       ; new def: (gn fv.. q..) . gbody'
   (unpair ps body) (? (atom? ps) [body] (: pr <ps (<pr . (>pr . unpair >ps body))))    ; pairs + body -> a : binding list
   (lift form)                                                                                    ; lambda-lift every named-let fn body in a `:`
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          results (map lift-pair pairs)
          allpairs ((map cap results) + (foldl (\ a r (a + <>r)) () results))            ; rewritten originals ++ all the lifted defs
          hasbody (1 = (#>form % 2))
          (': . (? hasbody (unpair allpairs body) (cbflat allpairs)))))                       ; body-less in, body-less out
   ; --- partial lift (plift): hoist a read-only loop fn out of a nested `:` body to the top level,
   ;     threading its captures as leading params -- the sibling case lift can't reach, where a read loop
   ;     sits among allocating siblings that must stay interpreted. read-only means no pin/link/tablet/
   ;     cask/say, since a write cannot hoist past the interpreted fills it interleaves with; and the loop
   ;     must be self-recursive with no sibling call. the lifted fn gets the same captures by value.
   roalloc '(pin pull link tablet hash cask say pour jug tap put puts putc putn putx flush dot see sip slurp sound read reads rand randf twirl catch scare ev)   ; allocating/mutating/IO heads -- a `_`-bound one is an effect that must not be dropped or hoisted
   (uses-head? hs e) (? (two? e) (member? <e hs || foldl (\ a x (a || uses-head? hs x)) 0 >e) 0)
   (fnpair? pr) (: nf <pr (two? nf && name? <nf && 1 < #nf && all name? >nf && distinctp >nf))  ; (g q..) define-sugar
   (roloop? pr sibs) (: g <<pr body >pr                                                 ; a hoistable read-only self-loop
      ((fnpair? pr) && (occurs? g body) && !(uses-head? roalloc body)
       && !(any (s \ !(id? s g) && occurs? s body) sibs)))                              ; calls no sibling fn
   ; --- write-loop, the map update-in-place lane: (g i) (? COND (: k.. _ (pin H k V) (g STEP)) H), a
   ;     read-modify-write self-loop over a captured map whose only effect is that one in-place pin.
   ;     plift hoists it like a roloop, but the hoisted body threads the pin into the recursive call's H
   ;     slot as (mpin H k V) -- mpin writes and answers H -- and the base H becomes 0, since native
   ;     returns a fixnum, not the pointer the nif would tag. sound only where H's result is discarded at
   ;     every call site, which bumpok? enforces; a missing key deopts.
   (wmaph pr sibs) (: g <<pr body >pr                                                   ; the captured map H of an rmw write-loop pr, else ()
      (? !((fnpair? pr) && (occurs? g body) && !(any (s \ !(id? s g) && occurs? s body) sibs)
             && (two? body) && (id? <body '?) && (4 = #body)) ()
         (: THEN (opd body 1) BASE (opd body 2)
            (? !(name? BASE && two? THEN && id? <THEN ': && !(atom? >THEN)) ()
               (: pp (anat-defs >THEN) binds <pp call <>pp
                  pinpr (afind (b \ (: v >b (two? v && id? <v 'pin && 4 = #v && id? <>v BASE))) binds)
                  (? ((two? pinpr)
                      && (all (b \ id? b pinpr || !(uses-head? roalloc >b)) binds)   ; the only effect is that one pin
                      && (two? call) && (id? <call g)) BASE ()))))))                            ; tail self-call (H is captured pre-lift; wthread threads it via the recursive call after plift adds it)
   (wthread g gn fv H pr) (: body >pr COND (opd body 0) THEN (opd body 1)                    ; the threaded hoisted body for a write-loop (still peep/pin -> interpretable; mapfix rewrites to mpeep/mpin later)
      pp (anat-defs >THEN) binds <pp call <>pp
      pinpr (afind (b \ (: v >b (two? v && id? <v 'pin && id? <>v H))) binds)
      pv >pinpr K (opd pv 1) V (opd pv 2)
      others (filter (b \ !(id? b pinpr)) binds)
      inl (e \ auxinl others e)                                                     ; inline the pure value-binds (e.g. k), closed against each other
      nc (map (x \ id? x H ? ['pin H (inl K) (inl V)] x) (subst-call g gn fv call))          ; (gn fv.. STEP) with H's slot threaded as (pin H k v) -- pin writes and returns H, threading the map
      ['? (subst-call g gn fv COND) nc 0])                                                       ; base -> 0 (the map result is discarded -- bumpok?)
   (other-use? nm self ipairs ibody) (occurs? nm ibody || any (pr \ !(id? pr self) && occurs? nm >pr) ipairs)   ; nm used outside its own binding?
   (bumpok? g defpr ipairs ibody)                                                                ; every (g ..) call (besides g's def) is a discarded whole-binding value
    (!(occurs? g ibody)
     && (all (pr \ (id? pr defpr) || !(occurs? g >pr)
                    || (!(fnpair? pr) && (: v >pr (two? v && id? <v g)) && !(other-use? <pr pr ipairs ibody))) ipairs))
   (subst-pairs g gn fv pairs) (map (pr \ <pr . subst-call g gn fv >pr) pairs)    ; rewrite (g A..) in every pair's value
   (plift-fn Fpair)                                                                               ; ((F P..) . FBODY) -> `(Fpair' newdefs)
    (: nf <Fpair FBODY >Fpair
       (? !(two? nf && name? <nf && two? FBODY && id? <FBODY ': && !(atom? >FBODY)) [Fpair ()]
          (: F <nf P >nf
             parsed (anat-defs >FBODY) ipairs <parsed ibody <>parsed
             sibnames (map caap (filter fnpair? ipairs))                                           ; all inner fn names
             valnames (map cap (filter (pr \ !(fnpair? pr)) ipairs))                            ; inner value-binding names (bare symbols)
             wlifts (filter (pr \ name? (wmaph pr sibnames) && bumpok? <<pr pr ipairs ibody) ipairs)   ; the hoistable write-loops (map RMW, discarded result)
             mapsz (foldl (\ m w (: H (wmaph w sibnames) bd >w                                  ; presize: each write-loop's map H -> its counted bound N
                            cnd (? (two? bd && id? <bd '?) (opd bd 0) ())
                            N (? (two? cnd && id? <cnd '<) (opd cnd 1) ())                   ; cond (i < N) -> the bound
                            (? (name? H && !(id? N ())) ((H . N) . m) m))) () wlifts)       ; so `(tablet 0)` -> `(tablet N)`: the native inserts never grow
             lifts ((filter (pr \ roloop? pr sibnames) ipairs) + wlifts)
             (? (atom? lifts) [Fpair ()]
                (: remain (filter (pr \ !(any (lp \ id? pr lp) lifts)) ipairs)                ; the non-lifted inner bindings (by identity)
                   specs (map (pr \ <<pr                                               ; per lift a spec (g gn fv..): name, hoisted name F_g, captures
                               . ((name (string F + "_" + string <<pr))
                                     . (filter (s \ !(id? s '_) && !(member? s ><pr) && occurs? s >pr) (P + valnames)))) lifts)   ; `_` is the throwaway/effect binding -- the loop body rebinds it locally (the pin slot), never captures the enclosing one
                   (presz pr) (? (fnpair? pr) pr (: ms (afind (x \ id? <x <pr) mapsz) v >pr   ; a map binding `(H (tablet 0))` -> `(H (tablet N))`
                                  (? (two? ms && two? v && id? <v 'tablet) (<pr . ['tablet >ms]) pr)))    ; presize `(tablet N)` so the N native inserts never grow
                   rwp (map presz (foldl (\ ps sp (subst-pairs <sp <>sp >>sp ps)) remain specs))   ; rewrite (g A..) calls through every spec, then presize the maps
                   rwb (foldl (\ b sp (subst-call <sp <>sp >>sp b)) ibody specs)       ; ... and in the trailing body
                   newdefs (map (sp \ (: lp (afind (pr \ id? <<pr <sp) lifts)            ; the hoisted def: (gn fv.. q..) . body'[g->gn]
                                  wl (any (w \ id? w lp) wlifts)                                   ; a write-loop -> threaded body; else the plain read-only rename
                                  ((<>sp . >>sp + ><lp)
                                     . (? wl (wthread <sp <>sp >>sp (wmaph lp sibnames) lp)
                                           (subst-call <sp <>sp >>sp >lp))))) specs)
                   [(nf . (': . unpair rwp rwb)) newdefs])))))
   (plift form)                                                                                   ; hoist read-only loops out of every top-level fn's nested :
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          results (map plift-fn pairs)
          extradefs (foldl (\ a r (a + <>r)) () results)
          hasbody (1 = (#>form % 2))
          (? (atom? extradefs) form
             (: allp ((map cap results) + extradefs)
                (': . (? hasbody (unpair allp body) (cbflat allp))))))) ; body-less in -> body-less out
   ; --- map lane (mapfix): a group fn's param that is peep'd but never tally'd is a map, not a string,
   ;     so its (peep h k d) becomes (mpeep h k d) and the emitter lays an open-addressed probe. the
   ;     heuristic is backstopped by the entry's mapkind guard -- a non-map arg deopts to interp.
   (puse? e P) (? (two? e) (? (id? <e 'peep && id? <>e P) 1 (foldl (\ a x (a || puse? x P)) 0 >e)) 0)   ; P used as (peep P k d)?
   (tuse? e P) (? (two? e) (? (id? <e 'tally && id? <>e P) 1 (foldl (\ a x (a || tuse? x P)) 0 >e)) 0)   ; P used as (tally P)?
   (peep->mpeep P e) (? (two? e) (? (id? <e 'peep && id? <>e P) ('mpeep . map (x \ peep->mpeep P x) >e) (map (x \ peep->mpeep P x) e)) e)
   (pin->mpin P e)   (? (two? e) (? (id? <e 'pin && id? <>e P) ('mpin . map (x \ pin->mpin  P x) >e) (map (x \ pin->mpin  P x) e)) e)   ; a map param's (pin P k v) update -> (mpin P k v); a cask param is pin'd-not-peep'd so never in mps
   (mapfix-def def) (: nf <def body >def
      mps (filter (P \ puse? body P && !(tuse? body P)) >nf)                              ; this fn's map params (peep'd, not tally'd)
      (nf . foldl (\ b P (pin->mpin P (peep->mpeep P b))) body mps))
   ; --- string-builder fusion (castbuild): (: (g v.. s ..) (? COND (g u.. (s + X) ..) s) (g i.. "" ..))
   ;     builds a string by repeated append, and each `+` copies the whole prefix -- an O(n^2) build. the
   ;     clean single-byte counted case lowers to a native cask-fill (cbcask below); the general case here
   ;     threads a `jug` -- the memory output port -- in the accumulator's own loop slot, said into per step
   ;     and drained once at the base. threading it as a loop var rather than an aux let is what survives
   ;     autogroup's purity-assuming inliners, which would substitute an aux (jug 0) into both use sites and
   ;     reborn it empty each step. sound because (say s (string X)) reproduces +'s string coercion exactly.
   ;     the guard demands a pure append accumulator: init "", s only as the left operand of (s + X) in the
   ;     self-call's slot and as the base result -- not in the test, not in X, not in a sibling's arg.
   (cbquote? x) (two? x && id? <x '\ && 2 = #x)                       ; (\ datum) -- quoted data, don't descend
   (cbsay? x)   (string? x || (two? x && id? <x 'string && 2 = #x)) ; already string-valued -> say x as-is
   (cbcall? e g ar) (two? e && id? <e g && #>e = ar)             ; e a self-call (g ..) of arity ar
   (cbset l i v) (? (i = 0) (v . >l) (<l . cbset >l (i - 1) v))  ; l with index i replaced by v
   (cbnotelse? s as si) (foldl (\ a j (a && (? (j = si) 1 !(occurs? s (nthl as j))))) 1 (rangeL 0 #as))   ; s in no arg but si
   (cbacc nf)                                                                        ; -> (si X g q COND RECUR recur-is-then?) on a match, else ()
    (? !(named-let? nf) ()
       (: nl <nf inits >nf gnf <>nl g <gnf q >gnf B <>>nl    ; B = the loop body
          (? !(two? B && id? <B '? && 4 = #B) ()                   ; B = (? COND e1 e2)
             (: ar #q c (opd B 0) e1 (opd B 1) e2 (opd B 2)
                rth (cbcall? e1 g ar) r (? rth e1 (cbcall? e2 g ar) e2 ()) base (? rth e2 e1)   ; which branch is the tail self-call; the other is the base
                (? !(two? r && name? base && member? base q) ()                ; need a self-call branch + a param-returning base
                   (: si (posof base q 0) sa (>r si) IN (inits si)
                      (? !(two? sa && id? <sa '+ && 3 = #sa && id? (opd sa 0) base) ()   ; arg_si = (s + X)
                         (: X (opd sa 1)
                            (? (string? IN && 0 = #IN && !(occurs? base c) && !(occurs? base X) && cbnotelse? base >r si)
                               [si X g q c r rth] ())))))))))                        ; init "" + s only in (s + X)@si and the base
   (cbrw nf [si X g q c r rth])                                                  ; the threaded-jug rewrite: the accumulator param becomes a jug, init (jug 0)
    (: inits >nf  s (q si)                                                      ; s = the accumulator param (now the jug, threaded unchanged)
       app  (? (cbsay? X) ['say s X] ['say s ['string X]])                        ; (say s X) | (say s (string X)) -- mirrors (s + X)
       r2   (': . ['_ app (g . cbset >r si s)])                        ; (: _ (say s ..) (g ..s..)) -- pass the jug unchanged
       b2   (? rth ['? c r2 ['slurp s]] ['? c ['slurp s] r2])                    ; keep the original branch orientation; base -> (slurp s)
       (': . [(g . q) b2 (g . cbset inits si '(jug 0))]))                ; (: (g q..) b2 (g ..(jug 0)..)) -- the jug born once at the call
   (cbwalk e) (? (atom? e) e  (cbquote? e) e
                 (: nf (norm-let e) info (cbacc nf) (? (two? info) (cbrw nf info) (map cbwalk e))))
   ; --- the native cask variant: a clean single-byte counted builder, i:0-by-1 the cask index and the
   ;     appended E a charm expr, compiled to a counted byte-store. the cask cannot come back from native
   ;     -- the epilogue tags rax as a fixnum and would mangle the pointer -- so the loop lifts to a
   ;     sibling F-fill answering the count, with the cask threaded as a buf param, and the wrapper F
   ;     holds the cask and converts it. multi-char appends, other counters and the else-orientation fall
   ;     back to the threaded jug.
   (cbbyte? e i) (? (whole? e) 1  (id? e i) 1  (two? e && id? <e 'peep) 1                ; a charm expr: const | the counter | a peep byte | arith thereof
                    (two? e && member? <e '(+ - * // % & | ^ << >>) && 3 = #e) (cbbyte? (opd e 0) i && cbbyte? (opd e 1) i) 0)
   (cbcask nlval nf)                                                                              ; -> (wrapper-pair fill-pair) on a clean single-byte counted match, else ()
    (? !(two? nf && name? <nf && 1 < #nf) ()                                    ; nf must be (F p..) so we can sibling the fill
       ((cbacc nlval) @ [si X g q cond r rth]
          (: inits >nlval
             (? !(rth && (two? cond) && (id? <cond '<) && (3 = #cond) && (member? (opd cond 0) q)   ; recur-then; cond = (i < n)
                   && (two? X) && (id? <X 'string) && (2 = #X) && (cbbyte? (opd X 0) (opd cond 0))) ()   ; append = (string E), E a charm in the counter
                (: i (opd cond 0) n (opd cond 1) ci (posof i q 0) E (opd X 0) rargs >r ia (rargs ci)
                   (? !(0 = (inits ci) && two? ia && id? <ia '+ && 3 = #ia && id? (opd ia 0) i && 1 = (opd ia 1)) ()   ; counter init 0, step (i + 1)
                      (: F <nf Fp >nf s (q si) fillnm (name (string F + "-fill")) cbn (name (string F + "-cb"))
                         fv (filter (p \ occurs? p cond || occurs? p r) Fp)               ; enclosing params the loop reads
                         fbody ['? cond (fillnm . fv + (cbset rargs si ['pin s i E])) i]     ; (? COND (F-fill fv.. (i + 1) (pin s i E)) i)
                         wbody (': . [cbn ['cask n] '_ (fillnm . fv + (cbset inits si cbn)) ['string cbn]])
                         [(nf . wbody) ((fillnm . fv + q) . fbody)])))))
          ()))
   (cbcpair p) (: info (cbcask (norm-let >p) <p) (? (two? info) info [(<p . cbwalk >p)]))    ; cask variant, else keep (jug via cbwalk)
   (cbflat ps) (? (atom? ps) () (: pr <ps (<pr . (>pr . cbflat >ps))))   ; pairs -> a binding list, no trailing body (preserves a body-less `:`)
   (castbuild form)
    (? !(two? form && id? <form ': && !(atom? >form)) (cbwalk form)              ; a `:` group: try the cask variant per binding (adds F-fill siblings); else the in-place walk
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          newpairs (foldl (\ a r (a + r)) () (map cbcpair pairs))
          (? (1 = (#>form % 2)) (': . unpair newpairs (cbwalk body))             ; odd count -> has a body
             (': . cbflat newpairs))))                                                       ; even -> body-less, which leaks its bindings globally: keep it so
   ; fold-consts: inline a top-level `:`'s whole-constant value bindings (e.g. strscan's `hmod 1000000007`)
   ; into every body and drop them -- so a folded constant doesn't read as an un-glazeable free symbol.
   ; a pair's own params -- the sugared head's tail, or a lambda value's -- shadow a
   ; same-named sibling throughout its body, so no inliner may substitute one there
   (prps pr) (? (two? <pr) ><pr (two? >pr && id? <>pr '\) (ho-initl >>pr) ())
   (fold-consts form)
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          consts (filter (pr \ name? <pr && whole? >pr) pairs)
          (? (atom? consts) form
             (: sub (\ ps e (foldl (\ b pr (? (member? <pr ps) b (subst-sym <pr >pr b))) e consts))
                hasbody (1 = (#>form % 2))
                keep (? hasbody (filter (pr \ !(name? <pr && whole? >pr)) pairs) pairs)  ; body-less: the const is exported, keep it bound
                subbed (map (pr \ <pr . sub (prps pr) >pr) keep)
                (': . (? hasbody (unpair subbed (sub () body)) (cbflat subbed)))))))
   ; --- hof inline (dehof): a non-recursive lambda-constant binding inlines into every body, and dsimp
   ;     beta-collapses the now-literal curried application -- so ((twice (adder i)) i) reduces to the
   ;     first-order arithmetic that lift+autogroup glaze, and the inlined lambdas drop like fold-consts'.
   ; desugar the binding head so `(f a..) body` decides exactly what the explicit `f (\ a.. body)` does --
   ; glazeability must not turn on how a sibling binding was spelled.
   (dehead pr) (: h <pr (? (atom? h) pr (<h . ('\ . >h + [>pr]))))    ; (f a..) body -> (f . (\ a.. body)); a plain name binding passes through (dsug is mopped post-egg)
   ; --- higher-order inference: a group fn is higher-order iff it returns a function or takes a function
   ;     param, and dehof inlines exactly those -- the group lane is first-order, so a hof can only glaze
   ;     by being inlined and collapsed. a monotone fixpoint over the call graph: func-param(f,k) = param k
   ;     is applied in operator position, or passed to a callee's func-param; func-result(f) = the body
   ;     yields a function (a lambda, a partial call, a call to a ho-result fn, a func-typed param, or a
   ;     ?-branch that does). marks ho only when provable -- a missed hof falls to interp, never to a wrong
   ;     answer. the ho-* helpers are local because the boxfix globals they would use are mopped post-egg.
   (ho-initl l) (? (atom? >l) () (<l . ho-initl >l))                         ; all but the last
   (ho-lamb? v) (two? v && id? <v '\ && 2 < #v)
   (ho-any p l) (foldl (\ a x (a || p x)) 0 l)
   (ho-memb x l) (? (atom? l) 0 (id? <l x) 1 (ho-memb x >l))
   (ho-posn x l i) (? (atom? l) -1 (id? <l x) i (ho-posn x >l (i + 1)))
   (ho-dedup l) (? (atom? l) () (ho-memb <l >l) (ho-dedup >l) (<l . ho-dedup >l))
   (ho-assq k l) (? (atom? l) () (id? <<l k) <l (ho-assq k >l))                ; entry (k . rest) by car
   (ho-nth l i) (? (i = 0) <l (ho-nth >l (-1 + i)))
   (ho-fpm nm hm) (: e (ho-assq nm hm) (? (two? e) >>e ()))                                  ; nm's func-param mask
   (ho-rf nm hm) (: e (ho-assq nm hm) (? (two? e) <>e 0))                                    ; nm returns a function?
   (ho-ar nm info) (: e (ho-assq nm info) (? (two? e) #<>e -1))                       ; nm's arity, -1 if not a group fn
   (ho-walkfp fp body hm)                                                                         ; func-param mask of fp in body
    (ho-dedup ((: (walk e acc) (? !(two? e) acc
                  (: hd <e a0 (? (ho-memb hd fp) (ho-posn hd fp 0 . acc) acc)            ; (p ..) -> p applied as a fn
                     gm (ho-fpm hd hm)
                     a1 (? (atom? gm) a0 (foldl (\ ac j (: arg (ho-nth >e j) (? (ho-memb arg fp) (ho-posn arg fp 0 . ac) ac))) a0 gm))   ; our param at a callee's func-pos
                     (foldl (\ ac x (walk x ac)) a1 >e)))) body ()))
   (ho-ret? e fp fpm info hm)                                                                     ; does e yield a function?
    (? (atom? e) (ho-memb e fp && ho-memb (ho-posn e fp 0) fpm)                               ; a func-typed param
       (id? <e '\) (ho-lamb? e)                                                              ; a lambda literal
       (id? <e '?) (ho-any (b \ ho-ret? b fp fpm info hm) >e)                          ; a ?-branch that does
       (: hd <e ar (ho-ar hd info) (? (ar < 0) 0 (#>e < ar) 1 (ho-rf hd hm))))    ; non-group->0; partial->fn; full->g's result
   (ho-fn fp body info hm) (: fpm (ho-walkfp fp body hm) ((? (ho-ret? body fp fpm info hm) 1 0) . fpm))
   (ho-sz hm) (foldl (\ a e (a + <>e + #>>e)) 0 hm)                            ; monotone measure (rflags + |fpmask|)
   (ho-fix info hm) (monofix hm (st \ map (f \ <f . ho-fn (ho-nth f 1) (ho-nth f 2) info st) info) ho-sz)   ; higher-order lattice (ascending: rflags+|fpmask|) over the shared harness (prel)
   (dehof form)
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          fns (filter (p \ ho-lamb? >p) (map dehead pairs))                              ; lambda-valued (desugared) bindings = the group fns
          info (map (p \ [<p (ho-initl >>p) (last >p)]) fns)                 ; (name params body)
          hm (ho-fix info (map (f \ <f . (0 . ())) info))                            ; the higher-order fixpoint
          ho? (nm \ (: e (ho-assq nm hm) (two? e && (<>e || two? >>e))))             ; returns-fn or takes-fn -> inline it
          lamc? (pr \ (: nd (dehead pr) (ho-lamb? >nd && !(dcount <nd >nd) && ho? <nd)))   ; a non-recursive higher-order const
          lamcs0 (map dehead (filter lamc? pairs))
          ; a lamc named in another lamc's body stays bound: the fold substitutes each value as
          ; written, so a name already spent is never rewritten inside the text replacing a later
          ; one -- inlining both would leave the dropped binding dangling in that text.
          lamcs (filter (q \ !(ho-any (r \ !(id? <r <q) && occurs? <q >r) lamcs0)) lamcs0)
          (? (atom? lamcs) form                                                                  ; no hof const -> leave the form for the other lanes (dsimp mangles string/cask/loop forms)
             (: sub (\ ps e (dsimp (foldl (\ b pr (? (member? <pr ps) b (subst-sym <pr >pr b))) e lamcs)))   ; inline each hof lamc the pair does not shadow, then beta-collapse
                hasbody (1 = (#>form % 2))
                ; a body-less `:` exports every binding, so an inlined hof const stays bound:
                ; its callers are outside the form (kore's verb table names bracket-main, and
                ; core.l holds its only other mention -- its own define). Dropping what we
                ; inlined is dead-binding elimination, sound for a body-having `:` -- a local
                ; let, where an unreferenced binding is unobservable -- and a deletion of the
                ; module's surface here. The inlining still lands at every call site either way.
                keep (? hasbody (filter (pr \ !(ho-any (q \ id? <q <(dehead pr)) lamcs)) pairs)
                                pairs)                                                           ; non-inlined bindings keep their raw form (grpfix recognizes define-sugar function heads)
                subbed (map (pr \ <pr . sub (prps pr) >pr) keep)
                (': . (? hasbody (unpair subbed (sub () body)) (cbflat subbed)))))))
   ; --- church lowering (dechurch): (N f x) with N a small numeral and f a literal function is f applied
   ;     to x N times, so lower it to the nesting at compile time -- (2 (+ i) i) -> (i + i + i), pure
   ;     arithmetic the group lane glazes, where church otherwise dispatches through `num-ap` and stays
   ;     interpreted. exact: this is love's church semantics, (0 f x)=x and (1 f x)=(f x). what tells it
   ;     apart from the tower is that the operand is a function -- (2 i)/(2 5)/(2 3 4) carry a numeral, so
   ;     dch-fn? is false and they stay exponentiation. a variable count or operand rides num-ap.
   (dch-fn? f) (ho-lamb? f || (two? f && 2 = #f && member? <f '(+ - * // % & | ^)))   ; a lambda, or a dyadic-op partial like (+ i)
   (dch-ap f x) (? (ho-lamb? f) (dsimp [f x]) (f + [x]))                                            ; apply f to x: beta a lambda, extend an op-partial
   (dch-nest f n x) (? (n = 0) x (dch-ap f (dch-nest f (n - 1) x)))                                     ; f^n(x), built innermost-out
   (dch-occs v e) (? (id? e v) 1  (atom? e) 0  (dquote? e) 0                                            ; occurrences of the param in the body (shadow/quote overcount = safe)
                     (foldl (\ s t (s + (dch-occs v t))) 0 e))
   ; the fat gate: beta splices the accumulated x at every param occurrence, so a body naming its param
   ; k>=2 times residualizes ~k^N nodes. unroll only while k^N stays small; a fat one rides num-ap.
   ; op-partials and multi-param lambdas never duplicate (dsimp betas unary only), so only k for a
   ; unary lambda is measured.
   (dch-fat? f n) ((ho-lamb? f) && (3 = #f)
                   && (: k (dch-occs <>f <>>f) (1 < k && 256 < (n k))))
   (dechurch e)
    (? (atom? e) e  (dquote? e) e
       (: y (map dechurch e)                                                                           ; bottom-up: lower nested church first
          (? ((3 = #y) && (whole? <y) && (0 <= <y) && (<y < 64) && (dch-fn? <>y)
              && !(dch-fat? <>y <y))
             (dch-nest <>y <y <>>y)                                       ; (N f x) -> f^N(x), the flat surface form
             ((2 = #y) && (two? <y) && (2 = #<y) && (whole? <<y) && (0 <= <<y) && (<<y < 64) && (dch-fn? <><y)
              && !(dch-fat? <><y <<y))
             (dch-nest <><y <<y <>y)                                                ; ((N f) x) -> f^N(x), the curried surface form -- ((2 (+ i)) i) and (2 (+ i) i) are the same value, so compile the same
             y)))
   ; --- local let (delet): a pure value-let (: n1 v1 .. result), every binder a distinct plain symbol,
   ;     inlines by ordered substitution into one fragment expression, so a fn that names its intermediates
   ;     joins the group. substitution preserves value and order only for a dup-cheap value (arithmetic,
   ;     cmp, peep, tally -- no call): a binding holding a call bails to the unchanged `:`, since a dup
   ;     would recompute, a 0-use drop would lose an effect, and even a 1-use move breaks the sequence law
   ;     when the call effects. delet-body walks the whole body bottom-up, so a let nested in a
   ;     sub-expression surfaces too -- a raw `:` left in arg position deopts the whole group -- and a
   ;     shadowing inner let resolves before its outer namesake substitutes. a `\` is opaque, a local-fn
   ;     binding defers to norm-let/lift, and delet runs before lift, whose free vars it must fix up.
   (dcount s e) (? (id? e s) 1 (two? e) (foldl (\ a x (a + (dcount s x))) 0 e) 0)            ; syntactic occurrences of s in e
   (dcheap? e) (? (atom? e) 1  (dquote? e) 0                                                  ; dup-cheap: arith/cmp/bit/peep/tally, no call
                  (: h <e (? (id? h '? || member? h '(+ - * // % & | ^ < <= > >= = peep tally)) (all dcheap? >e) 0)))
   (delet-go pairs result orig)
    (? (atom? pairs) result
       (: pr <pairs nm <pr val >pr rest >pairs
          uses (foldl (\ a e (a + (dcount nm e))) 0 ((map cup rest) + [result]))          ; uses in subsequent values + body
          (? !(dcheap? val) orig                       ; a call never inlines -- bail to the unchanged `:`
             (delet-go (map (p \ <p . subst-sym nm val >p) rest) (subst-sym nm val result) orig))))
   (delet-body e)                                                                             ; inline value-lets anywhere in a fn body (root or nested), bottom-up
    (? (atom? e) e
       (id? <e '\) e                                                                     ; a quote / lambda is opaque -- never descend
       !(id? <e ':) (map delet-body e)                                                ; a (?/call/arith) form -> recurse into children to reach a nested let
       (atom? >e) e                                                                      ; a bare (:) -> leave
       (: parsed (anat-defs >e) pairs <parsed result <>parsed names (map cap pairs)
          hasbody (1 = (#>e % 2))                                                  ; odd count -> a trailing result; even -> body-less (a named-let returning its fn)
          pairs2  (map (pr \ <pr . delet-body >pr) pairs)                     ; bottom-up: inline within each value (incl. a local-fn's BODY) ...
          result2 (delet-body result)                                                         ; ... and within the result
          (? (!hasbody || !(all name? names && distinctp names) || uses-head? roalloc e) ; body-less, a local-fn `(g q..)`, or an effectful let (a `_`-bound `pin`/`say`/.. used 0x must not be dropped) -> keep the structure (parity-preserving),
               (': . (? hasbody (unpair pairs2 result2) (cbflat pairs2)))                   ; but surface a value-let nested in any body (else it survives lift -> deopt)
             (delet-go pairs2 result2 (': . unpair pairs2 result2)))))                    ; a pure value-let with a body -> inline it (a bail keeps the walked `:`, sound)
   (delet form)                                                                              ; inline value-lets in every fn body of a `:`
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed body <>parsed
          hasbody (1 = (#>form % 2))                                               ; as delet-body reads it: even is body-less
          pairs2 (map (pr \ <pr . (? (two? <pr) (delet-body >pr) >pr)) pairs)
          (': . (? hasbody (unpair pairs2 body) (cbflat pairs2)))))                      ; cbflat keeps a body-less `:` body-less
   ; --- booleans & clamps (debool): (&& a b)=(? a b 0), (|| a b)=(? a a (? b b 0)),
   ;     (abs x)=(? (x < 0) (0 - x) x) -- exact, because &&/|| carry the floor-clamp semantics that make
   ;     them these forms and not the looser (? a b a). ||/abs duplicate their operand, so they reuse
   ;     delet's dcheap? guard and leave a call-valued form un-lowered. bottom-up, so nested compounds
   ;     collapse; runs inside autogroup, so autonat still sees a raw (&& (it < L) ..) escape test.
   ;     `min`/`max` are array reducers, not binary scalar ops -- (min 7 3) is (7 3), the tower -- so a
   ;     binary (min a b) is no language feature and is not lowered here.
   (dband as) (? (atom? as) 1  (atom? >as) <as  ['? <as (dband >as) 0])    ; (&& a R..) = (? a (&& R..) 0)
   (dbor as)  (? (atom? as) 0  !(dcheap? <as) ('|| . as)                             ; (|| a R..) = (? a a (|| R..)); a dup'd, so guard
                 ['? <as <as (dbor >as)])
   (dbabs x) (? !(dcheap? x) ['abs x] ['? ['< x 0] ['- 0 x] x])                           ; (abs x) = (? (x < 0) (0 - x) x)
   ; an `@` clause's odd operands are patterns: `(b && lx-ws? b)` there is a guard, and a lowered `&&`
   ;   would ask for `b` before `b` binds, matching nothing and saying nothing. bodies lower, patterns
   ;   stay verbatim -- the guard expressions inside them included.
   (dbarms cs) (? (atom? cs) ()  (atom? >cs) [(debool <cs)]                     ; pat body .. [trailing default]
                  (<cs . ((debool <>cs) . dbarms >>cs)))
   (debool e)                                                                                   ; recursively lower &&/||/abs to ?
    (? (atom? e) e  (dquote? e) e
       (two? e && id? <e '@ && two? >e) ('@ . ((debool <>e) . dbarms >>e))
       (: y (map debool e) h (opword <y) as >y                    ; lower operands first (bottom-up); opword names a glued value head
          (? (id? h '&&) (dband as)  (id? h '||) (dbor as)
             (id? h 'abs && 1 = #as) (dbabs <as)  y)))
   ; --- specialization (autospec): mix (doc/misc/proto/mix.l) wired in as a recognizer -- the 1st
   ;     Futamura projection composed with the native lane. an interpreter applied to a static program (a
   ;     const-bound quoted AST) and a dynamic input specializes: the dispatch and AST navigation fold
   ;     away to the program's straight-line arithmetic, which the later passes then native. a
   ;     static-driven recursion unrolls, bounded by specall's descent guard and sfuel; a dynamic-driven
   ;     one residualizes clean. a no-op on any form with no defined-fn call over a static-data binding.
   (sstat v) ['s v]  (sdyn c) ['r c]  (sstat? p) (id? <p 's)  (spval p) <>p
   (sqv v) ['\ v]                                                                 ; quote a static datum -> seen as data
   (sreify p) (: v <>p (? (sstat? p && !(constellation? v)) (sqv v) v))  ; non-number static -> quoted
   (sstatval? p v) (sstat? p && v = (spval p))
   (szipenv ps apv) (? (atom? ps) () ([<ps <apv] . szipenv >ps >apv))
   (slook env k) (? (atom? env) (sdyn k) (id? k <<env) <><env (slook >env k))  ; unbound -> dynamic
   (sdeflook nm defs) (? (atom? defs) () (id? nm <<defs) <defs (sdeflook nm >defs))
   (sprim? h) (member? h '(* - + ! < <= > >= = cap cup caap caup cuap cuup id?))   ; arith + comparisons + static AST navigation
   (ssimp1 h a b)
    (? (id? h '* && sstatval? a 1) b   (id? h '* && sstatval? b 1) a       ; drop the * and + units on a residual
       (id? h '+ && sstatval? a 0) b   (id? h '+ && sstatval? b 0) a
       (sdyn [h (sreify a) (sreify b)]))
   (speprim h apv)
    (? (all sstat? apv) (sstat (base-ev (h . map sqv (map spval apv))))       ; all static -> fold over quoted data
       (2 = #apv) (ssimp1 h <apv <>apv)                           ; binary -> residualize, drop units
       (sdyn (h . map sreify apv)))
   (spcondm cs env seen defs)
    (? (atom? >cs) (spe <cs env seen defs)                              ; lone else
       (: cv (spe <cs env seen defs)
          (? !(sstat? cv) (sdyn ('? . sresidcl cv <>cs >>cs env seen defs))  ; dynamic test -> residual tail
             !(spval cv) (spcondm >>cs env seen defs)                     ; static false -> drop clause
             (spe <>cs env seen defs))))                                     ; static true  -> take result
   (sresidcl cv then rest env seen defs)
    (sreify cv . (sreify (spe then env seen defs) . sresidtl rest env seen defs))
   (sresidtl cs env seen defs)
    (? (atom? >cs) [(sreify (spe <cs env seen defs))]
       (: cv (spe <cs env seen defs)
          (? !(sstat? cv) (sresidcl cv <>cs >>cs env seen defs)
             !(spval cv) (sresidtl >>cs env seen defs)
             [(sreify (spe <>cs env seen defs))])))
   ; the descent guard: an unfold re-entering a fn along the chain must strictly shrink its static
   ; weight -- the structural-recursion certificate. an AST walk shrinks its datum every level and so
   ; unfolds to the bottom; a non-descending re-entry residualizes as a plain call, still correct.
   ; a same-signature seen-set cannot see a changing static ((n - 1) never repeats) and would unroll a
   ; non-folding test to the fuel floor. `seen` threads the in-progress chain as (nm . weight) pairs.
   (ssz v) (? (two? v) ((ssz <v) + (ssz >v)) 1)                          ; total leaf count of a static datum
   (sw apv) (foldl (\ a p (? (sstat? p) (a + (ssz (spval p))) a)) 0 apv)           ; the call's static weight
   (sanc nm seen) (? (atom? seen) -1 (id? nm <<seen) ><seen (sanc nm >seen))   ; nearest in-progress weight for nm (-1 = none)
   (specall def apv seen defs)                                                    ; unfold a defined-fn call, unless non-descending / out of fuel
    (: nm <def ps <>def bd <>>def sz (sw apv)
       w (sanc nm seen) n (peep sfuel 'n 0)
       (? (0 <= w && w <= sz) (sdyn (nm . map sreify apv))               ; no strict descent -> residual call
          !n (sdyn (nm . map sreify apv))                                 ; fuel out -> bail to a residual call (still correct)
          (: _ (pin sfuel 'n (n - 1)) (spe bd (szipenv ps apv) ((nm . sz) . seen) defs))))
   (spe x env seen defs)                                                          ; the partial evaluator
    (? (constellation? x) (sstat x)
       (nom? x) (slook env x)
       (atom? x) (sstat x)
       (: h <x a >x
          (? (id? h '\ && atom? >a) (sstat <a)                      ; (\ datum) quote -> a static datum
             (id? h '?) (spcondm a env seen defs)
             (sprim? h) (speprim h (map (e \ spe e env seen defs) a))
             (: d (sdeflook h defs)
                (? d (specall d (map (e \ spe e env seen defs) a) seen defs)
                     (sdyn (h . map sreify (map (e \ spe e env seen defs) a))))))))
   ; the residual-size gate: spe substitutes a param's residual at every occurrence, so nested unfolds
   ; share one small DAG whose tree reading is exponential -- and every downstream walker reads trees,
   ; so an over-budget residual is a hang, not a win. stsz walks with a step budget, 0 = blown.
   (stsz e n) (? (n < 1) 0 (atom? e) (n - 1) (: m (stsz <e (n - 1)) (? !m 0 (stsz >e m))))
   (smix call statenv defs) (: _ (pin sfuel 'n 100000) r (sreify (spe call statenv () defs))
                               (? (stsz r 20000) r call))   ; specialize one call -> residual source (small ones only)
   ; autospec proper: build defs (fn bindings) + statenv (names bound to a quoted datum), then replace
   ; every call to a defined fn that mentions a static name with its specialized residual (in place).
   (sconst? v) (two? v && id? <v '\ && 2 = #v)                      ; a quoted-constant binding value (\ datum)
   (sment? e statn) (? (name? e) (member? e statn)  (two? e) (srefany? e statn)  0)
   (srefany? args statn) (? (atom? args) 0 (sment? <args statn || srefany? >args statn))
   (specall? e defn statn) (two? e && member? <e defn && srefany? >e statn)   ; defined-fn call mentioning a static name
   (swalk e statenv defs defn statn)
    (? (atom? e) e
       (dquote? e) e                                                              ; never descend into a quoted datum
       (specall? e defn statn) (smix e statenv defs)                              ; specialize this call in place
       (map (x \ swalk x statenv defs defn statn) e))                           ; walk every element (a list head holds the loop body)
   (autospec form)
    (? !(two? form && id? <form ': && !(atom? >form)) form
       (: parsed (anat-defs >form) pairs <parsed
          fnps (filter (pr \ two? <pr) pairs)
          defs (map (pr \ <<pr . (><pr . (>pr . ()))) fnps)   ; (name (p..) body)
          statps (filter (pr \ name? <pr && sconst? >pr) pairs)
          statenv (map (pr \ [<pr (sstat <>>pr)]) statps)         ; name -> (stat datum)
          (? !(two? defs && two? statenv) form                            ; need a defined fn and a static-data binding
             (swalk form statenv defs (map cap defs) (map cap statenv)))))
   ; --- deforestation: fuse a map/filter pipeline under its consumer into one pass, then lower it over
   ;     (jot N) to the named-let loop lift already glazes (doc/misc/proto/forest.l has the spec).
   ;     consumers: foldl/foldr, a counted loop counting up for foldl and down for foldr, and all/any,
   ;     an early-exit loop answering truth bits -- all/any glaze only because their false is the fixnum
   ;     0 like =/</!, not the zero point (), which is what the integer compiler returns. `each` is not
   ;     here: its body is an effect, outside the arithmetic fragment. the transducer view of shortcut
   ;     fusion -- map f: elem|->(f elem); filter p: step|->(? (p elem) step skip) -- and dsimp then
   ;     beta-inlines the stage lambdas to a first-order step. step binders are fresh gensym noms.
   (dquote? x) (two? x && id? <x '\ && 2 = #x)                  ; (\ datum) -> a quote, never descend
   (drm ks env) (filter (p \ !(member? <p ks)) env)                    ; drop the shadowed substitutions
   (dcolnms bs) (? (atom? >bs) () (: n <bs (? (nom? n) (n . dcolnms >>bs) (dcolnms >>bs))))   ; a : binding list's binder noms
   (dsubx env x)                                                              ; capture-avoiding multi-substitution (twin of ev.l's): α-rename both \ and : binders
    (? (nom? x) (: p (assoc x env) (? (two? p) >p x))
       (atom? x) x
       (dquote? x) x
       (id? <x '\) (? (atom? >>x) x
          (: ps (init >x) ren (map (p \ p . nom 0) ps)
             ('\ . (map cup ren) + [(dsubx (ren + (drm ps env)) (last x))])))
       (id? <x ':) (: nms (dcolnms >x) ren (map (p \ p . nom 0) nms)
          (map (e \ dsubx (ren + (drm nms env)) e) x))
       (map (e \ dsubx env e) x))
   (dsub old new B) (dsubx [(old . new)] B)                               ; substitute old->new in B, capture-avoiding
   ; the define-sugar decline guard: a sugared : binder ((g z..) B) is a list --
   ; dsubx never renames it -- so a beta whose argument carries a colliding free name would capture.
   ; decline such a beta: the redex stays source -- a missed inline, never a miscompile.
   (dsyms e) (? (nom? e) [e] (atom? e) () (dquote? e) () (foldl (\ a x (a + (dsyms x))) () e))
   (dsugcol bs) (? (atom? >bs) () (: r (dsugcol >>bs) n <bs (? (two? n) (n + r) r)))
   (dsugs e) (? (atom? e) () (dquote? e) ()
                (: r (foldl (\ a x (a + (dsugs x))) () e)
                   (? (id? <e ':) ((dsugcol >e) + r) r)))
   (dbeta? old new B) (: ds (dsugs B)
      (? (atom? ds) 1
         (!(member? old ds) && !(foldl (\ a s (a || member? s ds)) 0 (dsyms new)))))
   (dmap? P)   (two? P && id? <P 'map && 3 = #P)                ; exact: (map f l x) indexes the mapped list
   (dfilt? P)  (two? P && id? <P 'filter && 3 = #P)
   (dprod? P)  (dmap? P || dfilt? P)
   (dsfn P)    <>P  (dssrc P) <>>P                              ; (map f P') -> f / P'
   (dpeel B e felse P)                                                        ; thread step body B over (elem e, filter-skip felse) through the chain
    (? (dmap? P)  (dpeel (dsub e [(dforest (dsfn P)) e] B) e felse (dssrc P))    ; e := (f e)
       (dfilt? P) (dpeel ['? [(dforest (dsfn P)) e] B felse] e felse (dssrc P))   ; (? (p e) B skip)
       [B (dforest P)])                                                      ; base list (forest nested pipelines)
   (dfold? x) (member? <x '(foldl foldr) && 4 = #x && (: P <>>>x (dprod? P || djot? P)))   ; (foldl/foldr c z P)
   (dpred? x) (member? <x '(all any) && 3 = #x && (: P <>>x (dprod? P || djot? P)))         ; (all/any q P)
   (dforest x)
    (? (atom? x)   x
       (dquote? x) x
       (dfold? x)  (: tag <x c <>x z <>>x P <>>>x e (nom 'e) a (nom 'a)
                      B0 (? (id? tag 'foldl) [c a e] [c e a])               ; foldl step (c a e); foldr step (c e a)
                      r (dpeel B0 e a P)                                      ; filter-skip = the acc (unchanged)
                      hd (? (id? tag 'foldl) ['\ a e <r] ['\ e a <r])
                      [tag hd (dforest z) <>r])
       (dpred? x)  (: tag <x q <>x P <>>x e (nom 'e) unit (? (id? tag 'all) 1 0)   ; filter-skip = the short-circuit unit
                      r (dpeel [q e] e unit P)
                      [tag ['\ e <r] <>r])
       (<x . map dforest >x))
   (dspecial? h) (id? h '? || (id? h '\ || id? h ':))
   (dsimp x)                                                                  ; reduce to first-order: beta unary lambdas, flatten curried heads
    (? (atom? x)   x
       (dquote? x) x
       (: y (dsimp <x . map dsimp >x) h <y as >y
          (? (atom? h) y
             (id? <h '\ && (3 = #h && (1 = #as && dbeta? <>h <as <>>h))) (dsimp (dsub <>h <as <>>h))
             (atom? <h && !(dspecial? <h))                 (dsimp (<h . >h + as))
             y)))
   ; (jot N) is the list source -- the chain (0 .. N-1) foldl/map iterate, which the loop never builds.
   ; jot only: iota is the array, which the chain ops do not iterate, so a foldl over one answers z.
   (djot? s) (two? s && id? <s 'jot && 2 = #s)                 ; (jot N)
   (dstep? f)  (two? f && id? <f '\ && 4 = #f)                 ; (\ A E BODY)  -- a 2-param fold step
   (dstep1? f) (two? f && id? <f '\ && 3 = #f)                 ; (\ E BODY)    -- a 1-param predicate
   (dlowerfold x)                                                            ; (foldl/foldr STEP Z (jot N)) -> counted loop (up / down)
    (: tag <x STEP <>x Z <>>x SRC <>>>x
       (? !(dstep? STEP && djot? SRC) x
          (: P1 <>STEP P2 <>>STEP BODY <>>>STEP N <>SRC go (nom 'go)
             (? (id? tag 'foldl)                                            ; step=(\ a e B): p1=a p2=e ; count e up 0..n-1
                [[': [go P2 P1] ['? ['< P2 N] [go ['+ P2 1] BODY] P1]] 0 Z]
                [[': [go P1 P2] ['? ['< P1 0] P2 [go ['- P1 1] BODY]]] ['- N 1] Z]))))           ; step=(\ e a B): count e down n-1..0
   (dlowerpred x)                                                            ; (all/any PRED (jot N)) -> short-circuit loop over one index
    (: tag <x PRED <>x SRC <>>x
       (? !(dstep1? PRED && djot? SRC) x
          (: E <>PRED BODY <>>PRED N <>SRC go (nom 'go)
             (? (id? tag 'all)                                              ; all: a false body returns 0 early; run-off returns 1
                [[': [go E] ['? ['< E N] ['? BODY [go ['+ E 1]] 0] 1]] 0]
                [[': [go E] ['? ['< E N] ['? BODY 1 [go ['+ E 1]]] 0]] 0]))))           ; any: a true body returns 1 early; run-off 0
   (dlower x)
    (? (atom? x) x (dquote? x) x
       (member? <x '(foldl foldr) && 4 = #x) (dlowerfold (<x . map dlower >x))
       (member? <x '(all any) && 3 = #x)     (dlowerpred (<x . map dlower >x))
       (<x . map dlower >x))
   (has-pipe? x) (? (atom? x) 0  (dquote? x) 0  (dprod? x) 1  (dfold? x) 1  (dpred? x) 1  (any has-pipe? >x))
   (defol-body b) (? (has-pipe? b) (dlower (dsimp (dforest b))) b)            ; fuse+lower a pipeline body, else leave it
   (defol-bs bl) (? (atom? bl) ()                                  ; exhausted, and a body-less `:` ends
                                                                   ; here: without this the next arm reads
                                                                   ; () as a trailing body and invents one,
                                                                   ; turning a defglob into a local let
                    (atom? >bl) [(defol-body <bl)]                 ; trailing body
                    (<bl . (defol-body <>bl . defol-bs >>bl)))   ; (name value ..) -> transform each value
   (defoliate form)
    (? !(has-pipe? form) form                                             ; no pipeline anywhere -> byte-identical (every legacy form)
       !(two? form && id? <form ': && !(atom? >form)) (dlower (dsimp (dforest form)))
       (': . defol-bs >form))
   ; --- loop-closer (loopclose): collapse a polynomial accumulation (foldl/foldr (+) z SRC) to its
   ;     closed form in the bound, work independent of the trip count (doc/misc/proto/close.l).
   ;     runs before defoliate -- it wants the raw fold, not the lowered loop. the engine is finite
   ;     differences in exact integers: the forward differences c_k = Δ^k g(0) plus the hockey-stick
   ;     identity Σ_{i<N} C(i,k) = C(N,k+1) give Σ_{i<N} g(i) = Σ_k c_k C(N,k+1), emitted as falling
   ;     factorials over k! with no runtime helper. the body is a black box we sample through base-ev.
   ;     sound only where map F / filter P `qual` (so nothing captures the bound) and the difference
   ;     table dies -- the top difference exactly 0, not `nil?`, which also fires on a negative leading
   ;     term. a period-2 filter reparametrizes to the dense kept index; other predicates decline.
   (lcfd ys)    (? (atom? >ys) () (<>ys - <ys . lcfd >ys))         ; one forward difference
   (lclead ys)  (? (atom? ys) () (<ys . lclead (lcfd ys)))                          ; leading diagonal c_0..c_d
   (lctopfd ys) (? (atom? >ys) <ys (lctopfd (lcfd ys)))                             ; last entry: Δ^(L-1) g(0)
   (lcsamp g d) (: (go i) (? (i <= d) (g i . go (i + 1)) ()) (go 0))                   ; (g 0)..(g d)
   (lcdeg g cp) (: (go d) (? (d >= cp) -1 (? (0 = (lctopfd (lcsamp g (d + 1)))) d (go (d + 1)))) (go 0))   ; degree, -1 if not polynomial within cp
   (lcfac r)    (: (go k a) (? (k < 2) a (go (k - 1) (a * k))) (go r 1))                      ; r! (a literal at glaze time)
   (lcsub N j)  (? (j = 0) N ['- N j])                                                       ; (N - j), or N when j=0
   (lcff N j r) (? (j = (r - 1)) (lcsub N j) ['* (lcsub N j) (lcff N (j + 1) r)])            ; falling factorial N(N-1)..(N-r+1) as source
   (lccbn N r)  (? (r = 1) N ['// (lcff N 0 r) (lcfac r)])                                   ; C(N,r) as source (r consecutive ints / r!, exact)
   (lcmul c t)  (? (c = 1) t ['* c t])
   (lcadd a b)  (? (b = 0) a ['+ a b])
   (lcsum cks N) (: (go ks k) (? (atom? ks) 0 (: rest (go >ks (k + 1)) (? (<ks = 0) rest (lcadd (lcmul <ks (lccbn N (k + 1))) rest)))) (go cks 0))   ; Σ_k c_k C(N,k+1), nonzero terms
   (lcfinal z sum) (? (z = 0) sum ['+ z sum])
   (lcemit z g M) (: d (lcdeg g 8) (? (d < 0) () [(lcfinal z (lcsum (lclead (lcsamp g d)) M))]))    ; sample -> degree -> closed source, wrapped; () declines
   (lcjot? s)   (two? s && id? <s 'jot && 2 = #s)
   (lcmap? s)   (two? s && id? <s 'map && 3 = #s)
   (lcfilt? s)  (two? s && id? <s 'filter && 3 = #s)
   (lcplus? f)  (id? f '+ || (two? f && id? <f '+ && 1 = #f))                ; + or (+)
   (lcfold? x)  (two? x && member? <x '(foldl foldr) && 4 = #x && lcplus? (opd x 0))
   (lcsrc s)                                                                                  ; src -> (Fsrc Psrc Nexpr) | () ; F/P = () when absent
    (? (lcjot? s)  [() () (opd s 0)]
       (lcfilt? s) (: in (opd s 1) (? (lcjot? in) [() (opd s 0) (opd in 0)] ()))
       (lcmap? s)  (: in (opd s 1) (? (lcjot? in) [(opd s 0) () (opd in 0)]
                                       (lcfilt? in && lcjot? (opd in 1)) [(opd s 0) (opd in 0) (opd (opd in 1) 0)]
                                       ()))
       ())
   ; soundness: only close a body that is a pure polynomial (+ - * over the param + int consts) -- a
   ; genuine polynomial, so the difference table dies exactly at its degree (a body with //, %, <, or ?
   ; can fit a low-degree polynomial on the first samples yet break later -- sampling can't prove it).
   (lc1lam? f)   (two? f && id? <f '\ && 3 = #f && name? (opd f 0))               ; (\ v BODY)
   (lcpoly? v e) (? (id? e v) 1 (whole? e) 1
                    (two? e && member? <e '(+ - *) && 3 = #e) (lcpoly? v (opd e 0) && lcpoly? v (opd e 1))
                    0)
   (lcpolyf? f)  (lc1lam? f && lcpoly? (opd f 0) (opd f 1))
   ; and recognize the filter predicate structurally (odd/even over (v % 2)) -- not by sampling, which
   ; a longer-period predicate could fool. canonical forms: (v % 2)/(1 = (v % 2)) odd, (0 = (v % 2)) even.
   (lcmod2? e v) (two? e && id? <e '% && 3 = #e && id? (opd e 0) v && (opd e 1) = 2)
   (lcoddp? f)   (? !(lc1lam? f) ()                                                          ; -> 'odd | 'even | ()
                    (: v (opd f 0) b (opd f 1)
                       (? (lcmod2? b v) 'odd
                          (two? b && id? <b '= && 3 = #b)
                             (: a (opd b 0) c (opd b 1)
                                (? (a = 1 && lcmod2? c v) 'odd  (c = 1 && lcmod2? a v) 'odd
                                   (a = 0 && lcmod2? c v) 'even (c = 0 && lcmod2? a v) 'even ()))
                          ())))
   (lcclose1 x)                                                                               ; (foldl/foldr (+) z SRC) -> (closed-source) | () decline
    (: z (opd x 1) p (lcsrc (opd x 2))
       (? (atom? p) ()
          (: Fsrc <p Psrc <>p N <>>p
             Fc (? (id? Fsrc ()) (i \ i) (? (lcpolyf? Fsrc) (base-ev Fsrc) ()))              ; identity if no map; else F must be a pure polynomial
             (? (id? Fc ()) ()
                (id? Psrc ()) (lcemit z Fc N)                                                 ; no filter: g = F over [0,N)
                (: pat (lcoddp? Psrc)                                                         ; else need a recognized odd/even filter
                   (? (id? pat 'odd)  (lcemit z (j \ Fc (1 + (2 * j))) ['// N 2])          ; k=2j+1, M=N//2
                      (id? pat 'even) (lcemit z (j \ Fc (2 * j)) ['// ['+ N 1] 2])        ; k=2j,   M=(N+1)//2
                      ()))))))
   (lcquote? x) (two? x && id? <x '\ && 2 = #x)
   (lc:form? x) (two? x && id? <x ': && !(atom? >x))                            ; a (: ..) with bindings
   (lcocc? v e) (? (id? e v) 1 (atom? e) 0 (lcquote? e) 0 (any (s \ lcocc? v s) >e))   ; v occurs in e
   ; --- named-let accumulator: (NM i a) (? (i < N) (NM (i + 1) (a + B)) a) with B a pure polynomial in i
   ;     and N loop-invariant closes to the sum in O(1) -- the same finite-difference engine as the fold
   ;     lane, reached through a `:` binding instead of a combinator. Σ_{i<=j<N} g(j) = G[N]-G[i], guarded
   ;     by the original test so the function stays faithful for i>=N, where the loop answers acc.
   ;     restricted to step (i + 1), test `<` and op `+`; lcpoly? is what rejects a reference to acc or an
   ;     outer var.
   (lcacc? name val)
    ((two? name) && (3 = #name) && (name? <name) && (name? <>name) && (name? <>>name)
     && !(id? <>name <>>name)                                               ; (NM i a), i /= a
     && (two? val) && (id? <val '?) && (4 = #val)                                        ; (? T THEN ELS)
     && (: NM <name I <>name A <>>name
           T (opd val 0) THEN (opd val 1) ELS (opd val 2)
           ((two? T) && (id? <T '<) && (3 = #T) && (id? (opd T 0) I)                     ; T = (i < N)
            && !(lcocc? I (opd T 1)) && !(lcocc? A (opd T 1))                               ; N loop-invariant in i,a
            && (id? ELS A)                                                                      ; base case -> acc
            && (two? THEN) && (id? <THEN NM) && (3 = #THEN)                               ; then = (NM STEP ACC)
            && (: STEP (opd THEN 0) ACC (opd THEN 1)
                  ((two? STEP) && (id? <STEP '+) && (3 = #STEP) && (id? (opd STEP 0) I) && ((opd STEP 1) = 1)  ; (i + 1)
                   && (two? ACC) && (id? <ACC '+) && (3 = #ACC) && (id? (opd ACC 0) A)       ; (a + B)
                   && (lcpoly? I (opd ACC 1)))))))                                              ; B a pure polynomial in i
   (lcaccsum gfn N i)                                                                          ; G[N]-G[i] source, wrapped ; () declines
    (: d (lcdeg gfn 8) (? (d < 0) () (: cks (lclead (lcsamp gfn d)) [['- (lcsum cks N) (lcsum cks i)]])))
   (lcaccrw name val)                                                                          ; the accumulator binding -> its closed value
    (: I <>name A <>>name T (opd val 0)
       N (opd T 1) B (opd (opd (opd val 1) 1) 1)                                               ; then=(NM (i + 1) (a + B)) -> acc=(a + B) -> B
       cs (lcaccsum (base-ev ['\ I B]) N I)
       (? (atom? cs) val ['+ A ['? T <cs 0]]))                                           ; (a + (? (i < N) (G[N]-G[i]) 0))
   (lc-bs bl) (? (atom? bl) bl  (atom? >bl) [(lcwalk <bl)]                           ; trailing body
                 (<bl . ((? (lcacc? <bl <>bl) (lcaccrw <bl <>bl) (lcwalk <>bl)) . lc-bs >>bl)))
   (lcwalk x) (? (atom? x) x (lcquote? x) x
                 (lc:form? x) (': . lc-bs >x)                                         ; walk `:` bindings (accumulators + recurse)
                 (: c (? (lcfold? x) (lcclose1 x) ())
                    (? (atom? c) (<x . map lcwalk >x) <c)))               ; () decline -> recurse; (src) -> the residual
   (lc-bs-has? bl) (? (atom? bl) 0  (atom? >bl) (lc-has-acc? <bl)
                      (lcacc? <bl <>bl || (lc-has-acc? <>bl || lc-bs-has? >>bl)))
   (lc-has-acc? x) (? (atom? x) 0 (lcquote? x) 0 (lc:form? x) (lc-bs-has? >x) (any lc-has-acc? >x))
   (lc-has-fold? x) (? (atom? x) 0 (lcquote? x) 0 (lcfold? x) 1 (any lc-has-fold? >x))
   (loopclose form) (? (lc-has-fold? form || lc-has-acc? form) (lcwalk form) form)          ; byte-identical when nothing closeable
   (clook2 nm al) (? (atom? al) -1 (id? nm <<al) <><al (clook2 nm >al))   ; (name arity) alist -> arity (-1 absent)
   (cmlook nm cm) (? (atom? cm) () (id? nm <<cm) ><cm (cmlook nm >cm))          ; (name . chain-param-INDEX-list) -> indices (() absent)
   ; cok? -- e is chain-valued: a raw chain word, not an untagged int. such a word must never reach an
   ; arith/cmp operand, where the body would read it as an int; it may only flow to two?/cap/cup or a
   ; callee's chain slot.
   (cok? cm cands cps ps e) (? (member? e cps) 1
                               (two? e && member? <e '(cap cup) && tally e = 2) (cok? cm cands cps ps <>e)
                               0)
   ; pwcar? -- w is a raw chain-car word: <ce/>ce over a chain-valued operand. The one
   ; place such a word may reach a compare: the point-= production below (raw word identity).
   (pwcar? cm cands cps ps w) (two? w && member? <w '(cap cup) && 2 = #w && cok? cm cands cps ps <>w)
   (gargok? cm pm cands cps pps ps as i callee) (? (atom? as) 1                                ; call args: slot i chain (per callee's cmap) -> cok?; point (per pm) -> a point param; else int
      ((? (member? i (cmlook callee cm)) (cok? cm cands cps ps <as)
             (member? i (cmlook callee pm)) (member? <as pps)                             ; a point slot takes a point param, nothing else (the guard is at the entry only)
             (groupok? cm pm cands cps pps ps <as))
       && (gargok? cm pm cands cps pps ps >as (i + 1) callee)))
   (groupok? cm pm cands cps pps ps e)                                                        ; int-valued e over cands; cps/pps = the current fn's chain-/point-param names
    (? (member? e cps) 0                                                                       ; a chain param is not int-valued (only valid via cok?)
       (member? e pps) 0                                                                       ; a point param neither (raw word: only the point-= and point slots)
       (member? e ps) 1  (whole? e) 1
       (two? e) (: op <e ar (clook2 op cands)
          (? (-1 < ar)                                  (#>e = ar && gargok? cm pm cands cps pps ps >e 0 op)   ; group call: arity ok, each arg ok for its slot's kind
             ((id? op '=) && ((3 = #e)                                              ; point-=: (P = <..)/(<.. = P) -- word identity, int 0/1.
              && (: x1 <>e y1 <>>e                                             ;   total because P is entry-guarded nom|mint and a point equals only
                    ((member? x1 pps && pwcar? cm cands cps ps y1)                     ;   itself, whatever kind the car is (charm keys deopt at the entry --
                     || (member? y1 pps && pwcar? cm cands cps ps x1))))) 1               ;   only interp knows the numeric bridges, (2 = 2.0))
             (member? op '(+ - * // % & | ^ < <= > >= =)) (3 = #e && groupok? cm pm cands cps pps ps <>e && groupok? cm pm cands cps pps ps <>>e)
             (id? op '?)                                  (4 = #e && groupok? cm pm cands cps pps ps <>e && groupok? cm pm cands cps pps ps <>>e && groupok? cm pm cands cps pps ps <>>>e)
             (id? op 'tally)                              (2 = #e && member? <>e ps)                                          ; (tally STR): str a param (string lane)
             (id? op 'peep)                               (4 = #e && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && whole? <>>>e)   ; (peep STR i d): str param, i ok, d const
             (id? op 'mpeep)                              (4 = #e && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && whole? <>>>e)   ; (mpeep MAP k d): map param, key ok, d const (the map lane -- mapfix rewrote a map-valued peep here)
             (id? op 'mpin)                               (4 = #e && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && groupok? cm pm cands cps pps ps <>>>e)   ; (mpin MAP k v): map param, key ok, v ok -- update-in-place; produces the map (threaded into a call's map slot, or a discarded result)
             (id? op 'pin)                                (4 = #e && member? <>e ps && groupok? cm pm cands cps pps ps <>>e && groupok? cm pm cands cps pps ps <>>>e)   ; (pin BUF i V): buf a raw-buf param, i ok, V ok
             (id? op 'two?)                               (2 = #e && cok? cm cands cps ps <>e)               ; (two? CE): ce chain-valued; result int 0/1 (total -- a non-chain reads false)
             0))                                                                               ; bare cap/cup are chain-valued -> not admitted in an int position (cok? and the point-= carry them)
       0)
   ; --- vok? -- the value grammar, a heap-allocating fn's body: an int const | a param | (link V V) |
   ;     (? INT V V), the test through groupok? and the branches value | a call to a value-typed
   ;     candidate. cross-call soundness comes from the candidate sets being separate -- vok?'s calls
   ;     resolve only in vcands, groupok?'s only in icands, so neither kind can call the other.
   (vok? cm pm icands vcands cps pps ps e)
    (? (whole? e) 1
       (member? e cps) 0  (member? e pps) 0                                                   ; raw-word params (chain/point) are not values: cggvir would re-tag them
       (member? e ps) 1
       (two? e) (: op <e ar (clook2 op vcands)
          (? (-1 < ar)         (#>e = ar && gargok? cm pm icands cps pps ps >e 0 op)   ; a value-group call (target in vcands)
             (id? op 'link)    (3 = #e && vok? cm pm icands vcands cps pps ps <>e && vok? cm pm icands vcands cps pps ps <>>e)
             (id? op '?)       (4 = #e && groupok? cm pm icands cps pps ps <>e && vok? cm pm icands vcands cps pps ps <>>e && vok? cm pm icands vcands cps pps ps <>>>e)
             0))
       0)
   (chps body ps) (filter (k \ cuse? body (nthl ps k)) (rangeL 0 #ps))                ; chain-param indices: params used in cap/cup/two? (cuse? from emit.l)
   (grpok-1 fns cm pm icands vcands vnames c) (: p (afind (q \ id? <<q <c) fns)
                                cps (map (k \ nthl ><p k) (cmlook <c cm))      ; this fn's chain-param names (its index list -> names)
                                pps (map (k \ nthl ><p k) (cmlook <c pm))      ; ... and its point-param names (same alist shape)
                                (? (member? <c vnames) (vok? cm pm icands vcands cps pps ><p >p)   ; a value fn -> value grammar
                                   (groupok? cm pm icands cps pps ><p >p)))      ; an int fn -> int grammar, over the int candidates only
   ; the group-membership fixpoint as a descending projection of the shared `monofix` (prel): each step
   ; drops candidates that fail the int/value grammar, recomputing the int/value split from the surviving
   ; set; tally (shrinking) is the measure. Converges when a pass drops nothing -- the closed glazeable subset.
   (gfix fns cm pm vnames cands)
    (monofix cands
       (st \ (: icands (filter (c \ !(member? <c vnames)) st)
                vcands (filter (c \ member? <c vnames) st)
                (filter (c \ grpok-1 fns cm pm icands vcands vnames c) st)))
       tally)
   (grpfix fns cands) (: cm (map (f \ <<f . chps >f ><f) fns)   ; (name . chain-param-index-list) for every candidate fn
                         pm (map (f \ (: fps ><f cix (chps >f fps)              ; (name . point-param-index-list): point-compared ((P = <..)), never chain-used
                                         (<<f . filter (k \ nuse? >f (nthl fps k) && !(member? k cix)) (rangeL 0 #fps)))) fns)
                         vnames (map (f \ <<f) (filter (f \ hasl >f) fns))    ; the value-typed candidate names (body conses)
                         (gfix fns cm pm vnames cands))
   (rewrite-bindings bl gnames gdefs) (? (atom? >bl) bl                                  ; the trailing body form -> keep
      (: nf <bl val <>bl  hit (two? nf && member? <nf gnames)
         ((? hit <nf nf)                                                            ; drop the (name p..) define-sugar -> a plain binding
            . ((? hit (? (jgir-ok? gdefs) (jitgroupir gdefs <nf arch) val) val)   ; jgir-ok? -> the neutral-IR group lane; else interp -- jgir spans the recognizer grammar, so that
         ;   fallback is a soundness backstop, not a live lane
                  . (rewrite-bindings >>bl gnames gdefs)))))
   ; memo-leak? -- a candidate fn reading a `:`-sibling map free: the native group lane can only touch a
   ; map that is one of the fn's own PARAMS, so a free one is a silent miscompile. decline to interp.
   ; the map-param lane is unaffected -- there the binding name never occurs inside the callee.
   (memo-leak? form)
    (: ps <(anat-defs >form)
       mn (filter (p \ atom? <p && two? >p && member? <>p '(tablet hash)) ps)   ; names bound to a tablet/hash
       !(atom? (filter (p \ (two? <p)                                                    ; a fn binding whose body ...
       && !(atom? (filter (m \ occurs? <m >p && !(member? <m ><p)) mn))) ps)))   ; ... references a mapname that is not its param
   ; --- group swallow: a member calling an external global int fn would drop the whole group to interp,
   ;     so srcrec records every top-level global fn define's source and swgather pulls a called global's
   ;     recording into the candidate set as a synthetic member, to a fixpoint. jitgroupir emits it as an
   ;     in-blob (label g) sibling under the group's one shared ovf/deopt: no cross-blob call, no split
   ;     deopt. the "real global" test is the body-less `:` that leaks its bindings, so srcreg never holds
   ;     a bench's local fn. a swallowed body over a free var or an unknown call is rejected by groupok?'s
   ;     own grammar, so the swallow declines rather than miscompiles. `F_g`, lift's name for a lifted fn,
   ;     is content-derived and collision-free because a name is either a `:`-local candidate or a
   ;     swallowed global, never both, so each F_ namespace is owned once.
   ; srcreg holds name -> (params . body) once lowered, or the form's shared cell {'form f} until a pull
   ; first asks for one of its names: a crew file's forms are recorded by the thousand while the swallow
   ; pulls a handful, so the lowering waits for the first pull and then registers the lowered pairs
   ; (a lifted F_g included) over entries that are free or this cell's own.
   srcreg {}
   (swfnpair? pr) (: nf <pr (two? nf && 1 < #nf < 6 && all name? >nf && distinctp >nf))
   (srcrec form0) (? ((two? form0) && (id? <form0 ':) && !(atom? >form0)
                    && (0 = (#>form0 % 2)))                                   ; body-less `:` only -> a global-leaking define
      (: cell (pin {} 'form form0)
         (foldl (\ _ n (pin srcreg n cell)) () (swbnds form0))) ())
   (swlow c) (? (peep c 'low 0) 0
      (: _ (pin c 'low 1)
         low (plift (lift (delet (defoliate (loopclose (autospec (debool (dehof (dechurch (fold-consts (opfix (c 'form))))))))))))   ; autogroup's full lowering: church/hof/&& + structural (lift/loops/value-lets)
         (foldl (\ _ pr (? (swfnpair? pr && (: cur (peep srcreg <<pr 0) (id? cur 0 || id? cur c)))
                            (pin srcreg <<pr (><pr . >pr)) ())) () <(anat-defs >low))))
   (swsrc h) (: d (peep srcreg h 0)                                                   ; a recorded global's lowered pair, or 0
      (? (id? d 0) 0
         (tablet? d) (: _ (swlow d) d2 (peep srcreg h 0)
                        (? (tablet? d2) (: _ (pin srcreg h 0) 0) d2))                   ; the name did not survive its form's lowering
         d))
   (swuniq l) (? (atom? l) () (member? <l >l) (swuniq >l) (<l . swuniq >l))
   (swbnds e) (map (p \ two? <p ? <<p <p) <(anat-defs >e))                       ; a `:`'s binding names
   ; collect call heads (symbols); skip lambda subforms. the walk reads opfixed source, so an
   ; infix (a op b) arrives factored and an operand never masquerades as a head. bnd is the
   ; binder set that filters real-but-local heads: `:` names + params (a `\` subform is
   ; skipped whole, so its params never leak).
   (swwalk e bnd acc) (? (atom? e) acc  (two? e && id? <e '\) acc
      (two? e && id? <e ':) (foldl (\ a x (swwalk x (swbnds e + bnd) a)) acc >e)
      (: h <e acc2 (? (name? h && !(member? h bnd)) (h . acc) acc)
         (foldl (\ a x (swwalk x bnd a)) acc2 >e)))
   (swheads e bnd acc) (swwalk (opfix e) bnd acc)
   (swgather fns) (: names (map caap fns)                                                 ; enlarge fns with recorded globals reachable from the bodies (to fixpoint)
      heads (foldl (\ a p (swheads >p ><p a)) () fns)                                ; the fn's own params bind too
      new (swuniq (filter (h \ !(member? h names) && !(id? (swsrc h) 0)) heads))
      (? (atom? new) fns
         (swgather (foldr (\ h a (: d (swsrc h) (((h . <d) . >d) . a))) fns new))))
   (autogroup form0)
    (: _ (srcrec form0)                                                                  ; record global fn sources (flag-gated inside srcrec) before any decline
     (? !(two? form0) form0
       !(id? <form0 ':) form0
       (atom? >form0) form0
       ; decline a body-less `:`, the global-leaking top-level define every crew file is made of.
       ; lowering it is sound, but autogroup is all-or-nothing per form and only a handful of one-line
       ; integer predicates survive grpfix there, so the whole file pays the lifting to carry them --
       ; measurably slower everywhere and faster nowhere. it comes back when grpfix's fragment reaches
       ; past integers into list and charlist code. srcrec above still walks the same chain over these
       ; forms to feed the swallow, so the lowering has to be right for them: a price, not a guard.
       (0 = (#>form0 % 2)) form0
       (memo-leak? form0) form0                                                                ; a `:`-sibling map memo (fact/bell) -> interp, sound
       ; opfixed at the head: every pass below reads a call as a list with a head, where raw source
       ; leaves a bare symbol standing in infix operand position
       (: form (plift (lift (delet (defoliate (loopclose (autospec (debool (dehof (dechurch (fold-consts (opfix form0)))))))))))
          parsed (anat-defs >form) pairs <parsed
          fns0 (filter (p \ (: nf <p (two? nf && 1 < #nf < 6 && all name? >nf && distinctp >nf))) pairs)   ; (name p..), 1..4 distinct sym params
          fns (swgather fns0)                                                              ; swallow: pull called globals' recorded sources in as synthetic members
          (? !(two? fns) form0                                                            ; no candidate functions -> original
             (: armap (map (p \ [<<p (#<p - 1)]) fns)                  ; (name arity) candidates
                grp (grpfix fns armap)                                                        ; the closed glazeable subset
                (? (atom? grp) form0                                                          ; nothing survives the fixpoint -> original (un-lifted)
                   ; a group of swallowed members only rewrites nothing here, so committing the
                   ; lowered form buys nothing and stakes the whole file on it.
                   !(ho-any (p \ member? (? (two? <p) <<p <p) (map cap grp)) pairs) form0
                   (: gnames (map cap grp)
                      gdefs (map mapfix-def (filter (p \ member? <<p gnames) fns))   ; the group's (nameform . body) defs, with map-valued peeps rewritten to mpeep
                      (': . rewrite-bindings >form gnames gdefs))))))))             ; each binding of the lifted form -> its native
   ; --- twin lowering (twolow): a complex-recurrence grid becomes the real-pair float grid autonat
   ;     already glazes, so idiomatic complex code needs no new codegen. each twin param p splits into
   ;     (p_r p_i), twin ops become real-pair arithmetic ((a+bi)(c+di) = (ac-bd, ad+bc)), and every call
   ;     site's twin args split in two; a small fixpoint propagating twin-ness from call-site args to
   ;     callee params picks which params are twin. bit-exact: z*z's imag rides re*im+im*re = 2*re*im,
   ;     and scaling a rounded product by 2 is exact. runs before autonat.
   (twolow form)
    (: (nthl2 l i) (? (i = 0) <l (nthl2 >l (i - 1)))
       (op2 e i)  (nthl2 >e i)
       (memq x l) (? (atom? l) 0 (id? <l x) 1 (memq x >l))
       (negf x)   ['- 0.0 x]
       (zero0? x) (gem? x && x = 0.0)
       (mkadd a b) (? (zero0? a) b (zero0? b) a ['+ a b])
       (mksub a b) (? (zero0? b) a ['- a b])
       (mkmul a b) (? (zero0? a || zero0? b) 0.0 ['* a b])
       (adefs bs) (? (atom? >bs) (() . <bs)                          ; binding list -> (pairs . body)
                     (: r (adefs >>bs) (((<bs . <>bs) . <r) . >r)))
       (fnpair? pr) (two? <pr)                                               ; nameform is a (name params..) list
       (fnname pr) <<pr  (fnparams pr) ><pr  (fnbody pr) >pr
       (twname p suf) (name (string p + suf))                                ; p_r / p_i
       (? !(two? form && id? <form ':) form
          (atom? >form) form
          (: parsed (adefs >form) pairs <parsed body >parsed
             (? !(all fnpair? pairs) form                                      ; only the all-function-def `:` shape
                (: fnames (map fnname pairs)
                   pt {} chg {}                                   ; `(fn i) -> 1 if param i of fn is twin
                   ; --- twin-type inference: is e twin under env (param-name -> twin bool)? ---
                   (tty e env) (? (atom? e) (peep env e 0)
                                  (e @ ('twin . _) 1
                                       (('re || 'im) . _) 0
                                       (('+ || '- || '*) a b) (tty a env || tty b env)
                                       ('? _ a b) (tty a env || tty b env)
                                       0))                                         ; a defined-fn call returns int here
                   (mkenv fn ps) (: e {} _ (mke fn ps 0 e) e)
                   (mke fn ps i e) (? (atom? ps) e (: _ (pin e <ps (peep pt [fn i] 0)) (mke fn >ps (i + 1) e)))
                   (propargs g as i env) (? (atom? as) 0                          ; call-site arg twin-ness -> callee param
                      (: _ (? (memq g fnames && tty <as env && !(peep pt [g i] 0)) (: _ (pin pt [g i] 1) (pin chg 0 1)) 0)
                         (propargs g >as (i + 1) env)))
                   (walkprop env e) (? !(two? e) 0
                      (: op <e _ (? (memq op fnames) (propargs op >e 0 env) 0) (: _ (map (a \ walkprop env a) >e) 0)))
                   (infer _) (: _ (pin chg 0 0)                                   ; fixpoint (monotone: flags only flip 0->1)
                               _ (map (pr \ walkprop (mkenv (fnname pr) (fnparams pr)) (fnbody pr)) pairs)
                               _ (walkprop {} body)
                               (? (peep chg 0 0) (infer ()) 0))
                   _ (infer ())
                   (anytwin? prs) (? (atom? prs) 0 (twfn? (fnname <prs) 0 || anytwin? >prs))
                   (twfn? fn i) (? (peep pt [fn i] 0) 1 (i < 8) (twfn? fn (i + 1)) 0)
                   (? !(anytwin? pairs) form                                   ; no twin param -> pass through
                      ; --- lowering: loR scalar-valued ; loT twin-valued -> (re-expr . im-expr) ---
                      (: (mknmap fn ps) (: m {} _ (mkn fn ps 0 m) m)      ; twin param -> (p_r . p_i) pair
                         (mkn fn ps i m) (? (atom? ps) m (: _ (? (peep pt [fn i] 0) (pin m <ps (twname <ps "_r" . twname <ps "_i")) 0) (mkn fn >ps (i + 1) m)))
                         (flatps fn ps i) (? (atom? ps) ()                        ; flatten params, splitting twins
                            (: p <ps rest (flatps fn >ps (i + 1))
                               (? (peep pt [fn i] 0) (twname p "_r" . (twname p "_i" . rest)) (p . rest))))
                         (loR e env nm) (? (atom? e) e
                            (e @ ['re x] (? (tty x env) <(loT x env nm) (loR x env nm))
                                 ['im x] (? (tty x env) >(loT x env nm) 0.0)
                                 (: op <e (? (memq op fnames) (op . splitargs op >e 0 env nm)
                                               (op . map (a \ loR a env nm) >e)))))
                         (loT2 e env nm) (? (tty e env) (loT e env nm) (loR e env nm . 0.0))
                         (loT e env nm) (? (atom? e) (peep nm e (e . e))
                            (e @ ['twin u v] (loR u env nm . loR v env nm)
                                 ['twin u] (? (tty u env) (: p (loT u env nm) (<p . negf >p)) (loR u env nm . 0.0))
                                 ['+ u v] (: a (loT2 u env nm) b (loT2 v env nm) (mkadd <a <b . mkadd >a >b))
                                 ['- u v] (: a (loT2 u env nm) b (loT2 v env nm) (mksub <a <b . mksub >a >b))
                                 ['* u v] (: a (loT2 u env nm)
                                             (? (u = v)                                       ; squaring (z*z): the imag is 2*ar*ai, one product, and better ilp than the M+M chain
                                                (: ar <a ai >a (mksub (mkmul ar ar) (mkmul ai ai) . mkmul (mkmul 2.0 ar) ai))
                                                (: b (loT2 v env nm) ar <a ai >a br <b bi >b
                                                   (mksub (mkmul ar br) (mkmul ai bi) . mkadd (mkmul ar bi) (mkmul ai br)))))
                                 ['? t u v] (: c (loR t env nm) a (loT u env nm) b (loT v env nm)
                                               (['? c <a <b] . ['? c >a >b]))
                                 (e . e)))
                         (splitargs g as i env nm) (? (atom? as) ()
                            (: a <as rest (splitargs g >as (i + 1) env nm)
                               (? (peep pt [g i] 0) (: p (loT2 a env nm) (<p . (>p . rest))) (loR a env nm . rest))))
                         (lowerfn pr) (: fn (fnname pr) ps (fnparams pr) bd (fnbody pr) env (mkenv fn ps) nm (mknmap fn ps)
                                         ((fn . flatps fn ps 0) . loR bd env nm))
                         (rebuild lowfns) (? (atom? lowfns) (loR body {} {} . ())
                            (: lf <lowfns (<lf . (>lf . rebuild >lowfns))))
                         (': . rebuild (map lowerfn pairs)))))))))
   (autonat form)
    (? !(two? form) form
       !(id? <form ':) form
       (atom? >form) form
       (: parsed (anat-defs >form) pairs <parsed
          (? !(1 < #pairs) form                        ; need a recurrence + >=1 loop
             (: recp (afind anat-rec? pairs)
                (? !(two? recp) form                          ; no float recurrence found
                   (: recnm <<recp recps ><recp
                      (>recp @ ['? ['&& ['< _ lim] [cmpop testexpr bound]] then _]
                         (: cl (classify recps >then) fvars <cl updates <>cl invs (nthl cl 2)
                            looppairs (filter (q \ !(id? q recp)) pairs)             ; the loop defs
                            callees (map anat-callee looppairs)
                            outerp (afind (q \ !(member? <<q callees)) looppairs)   ; the loop no loop calls
                            (? !(two? outerp) form
                               (: chain (loopchain <<outerp pairs recnm)       ; outermost -> recurrence
                                  D #chain M #invs N #fvars
                                  (? !(two? chain && member? cmpop '(< <= > >=) && 0 < N && (M + N) < 7 && D < 7) form
                                     (: cnames (map cap chain) dims (map caup chain)
                                        margs (nthl (last chain) 2)
                                        invd (map (iv \ nthl margs (posof iv recps 0)) invs)
                                        glam ['\ '_ form]        ; faithful interp nest (deopt fallback / =-show)
                                        ng (jitfgridn glam cnames dims invs invd fvars updates testexpr cmpop bound lim arch)
                                        (? (lit? ng) (': . anat-rebind >form <<outerp ng) form))))))
                         form)))))))
   ; compile cache: each source \-expr compiles once, keyed by source, so structurally-equal closures
   ; share the one native -- sound, they are =. this is the transparent form of a hot-trigger: an
   ; execution-count trigger wants a counting wrapper, and a wrapper is not =/show its source, so the
   ; trigger is "compile on first request, cached after". pins natives for the run.
   cache {}
   (memo e mk) (: g (peep cache e 0) (? g g (: h (mk e) _ (pin cache e h) h)))
   ; auto-ev: a whole-form lambda -> native (cached), else the base interpreter. a creation hook -- one
   ; that rewrote embedded lambdas on a jit-walk -- cannot sit here: it is pre-macroexpansion, so it can
   ; disturb a macro's data args, and the compile cache would make structurally-equal closures
   ; id?-identical, against love's !(id? (\ x E) (\ x E)).
   ; the lanes route to `arch`. on x86 the full glaze runs; on arm64 the integer lanes and the scalar
   ; float-result leaf are live, and only the float-grid lane and castbuild's cask-fill are x86-only --
   ; autogroup carries every source rewrite that lowers to the integer group, so fib/tak/primes glaze
   ; there too.
   (auto-ev e) (? !x86? (? (qual e)  (memo e (l \ jitir l arch))        ; arm64: integer + scalar-float lanes native
                               (qualn e) (memo e (l \ jitnir l arch))
                               (qualc e) (memo e (l \ jitnir (flatc l) arch))
                               (qualfr e) (memo e (l \ jitfr l arch))      ; float-result leaf -> heap-boxed gem
                               (: li (loopinfo e)                            ; counted loops (arch-aware loopcode)
                                  (? li (memo e (l \ njit-loop l <li <>li <>>li <>>>li (base-ev l)))
                                     (: ln (loopinfo-n e)
                                        (? ln (memo e (l \ njit-loop-n l <ln <>ln <>>ln <>>>ln <>>>>ln (base-ev l)))
                                           (base-ev (autogroup (autonat (twolow e)))))))))   ; float-grid nest (scalar gridn, arch-aware) then group; castbuild (cask-fill) is x86-only so skipped
                  (qual e)  (memo e (l \ jitir l arch))
                  (qualn e) (memo e (l \ jitnir l arch))
                  (qualc e) (memo e (l \ jitnir (flatc l) arch))
                  (qualfr e) (memo e (l \ jitfr l arch))         ; float-result leaf -> heap-boxed gem (sound: param floated at first use)
                  (: li (loopinfo e)
                     (? li (memo e (l \ njit-loop l <li <>li <>>li <>>>li (base-ev l)))
                        (: ln (loopinfo-n e)
                           (? ln (memo e (l \ njit-loop-n l <ln <>ln <>>ln <>>>ln <>>>>ln (base-ev l)))
                              ; fall-through: cask-build a string-accumulator loop, lower a complex
                              ; grid to real pairs, then a float-grid nest or an arith group, else interp
                              (base-ev (autogroup (autonat (twolow (castbuild e))))))))))
   ; the auto-native ev under the module's own name: `ev` is orth, which a module layer cannot write,
   ; so the boot installs it as (: ev (from 'glaze 'ev)). a re-load hands the baked auto-ev straight
   ; back, and a trampoline build (love-tco 0) stays the interpreter -- glazed code Continues by
   ; tail-jump, which only the threaded vm honors
   ev (? glaze-loaded? ev
         (0 < love-tco) auto-ev
         ev)
)

; --- member? re-declared through the glazed compiler: the point-= grammar closes the singleton group,
; so the prel walker (egg-baked, interp) is re-stated here and autogroup hands back the native. post-boot
; streams fold it; egg-baked consumers keep the interp closure they already folded. the gates mirror the
; ev rebind's, and charm and string keys deopt at the entry guard, only interp knowing (2 = 2.0).
; like `ev` this is orth's name, so the boot installs it: (: member? (from 'glaze 'member?)).
; two forms: memb0 must read the outer member?, where one scope would read the rebind before its pin.
(: memb0 member?)
(: member? (? glaze-loaded? memb0 (0 < love-tco)
              (ev '(: (member? x l) (? (two? l) (? (= x (cap l)) 1 (member? x (cup l))) 0) member?))
              memb0))
