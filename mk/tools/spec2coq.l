; spec2coq: generate Coq from test/spec.l, in love.
;
; The GENERATED half of the verification (see test/proof/rocq/spec.v for the hand-written
; LAWS -- the half a generator structurally can't do). Each top-level (assert ..)
; operand is `opfix`-factored (infix -> prefix) and translated to a Coq theorem
; closed by vm_compute, over models that together cover every EXACT, computable
; domain in the corpus:
;   * COMPUTATIONAL (Z / list Z / assoc lists) for value equalities: church
;     numerals (app), the integer algebra (+ - * // % & | ^ << >>, comparisons),
;     nilp / sat / abs / gcd, lists / strings / arrays (asum/amax/.., iota),
;     array-valued = (broadcast / append / repeat), hashes & peep; the list
;     functions (map/filter/foldl/foldr/sort/rev/cat/take/drop/..) with their
;     function arguments -- sections, escaped operators, one-param integer
;     lambdas -- as Coq funs (direct beta and n-fold compose ride the same lane);
;     predicates as integers (love's =/</! answer 1|0 charms; Z.b2z bridges).
;   * the UNIT lane: () (and a fresh mint) rides through every dyadic -- modeled
;     over option Z / option (list Z), None the unit, uop/ulop the lifted ops.
;   * TEXT as spelling (the symbol-as-spelling lane): symbols and strings are
;     their byte lists under + (concat) and * (repeat), equality band-guarded
;     (symbol never = string).
;   * a KIND lattice (value tags) for the type predicates (fixp/chainp/strp/..) and
;     the band order (symbol < string < number < chain < map); cross-band = is false.
;   * GAUSSIAN twins (exact complex over Z as (re,im) pairs): twin/conj/i literals,
;     + - * and negation, = numeric within the band, order and SIGN lex on (re,im),
;     $ the REAL PART of a twin the gate passes -- the part the gate itself reads.
;   * LAMBDAS as de Bruijn terms: function = is alpha + structural (never eta/beta),
;     so two lambda literals compare by lteqb on their ltm translations; < answers
;     nothing, a closure nets 1, and a closure never = a plain nom.
;   * id? on literals and the UNIT-reading forms ((:) (?) (\) '(), a fallen-through
;     cond, && / || landing on nothing): interned noms and charms are id?-equal by
;     value, fresh allocations (quoted lists, lambdas) never are, () is not 0.
; Left to the hand-written laws: the FLOAT tier and transcendental complex (not
; vm_compute-able, the honest misses) and the universally-quantified LAWS. Untranslatable asserts are
; SKIPPED and COUNTED -- never silently dropped (the trailing comment reports it).
; Z not nat: (3 3 3) = 3^27 would blow up unary-nat vm_compute.
; Reads test/spec.l, writes Coq to `out`.  Run: out/host/love mk/tools/spec2coq.l > test/proof/rocq/gen.v

(: (join l) (foldl (+) "" l)                           ; join a list of strings with + (was `cat`, which shadowed the global dyadic cat)
   ; the four interning tables, HOISTED to the head of the chain: bound down beside their
   ; readers, a capturing closure resolves them as a global import and the runtime says
   ; ";; missing vtab" &c. bound first, they resolve lexically and it is quiet.
   vtab (tablet 0) vc (tablet 0)      ; lambda params in flight: nom -> coq var; vc the freshness counter
   ltab (tablet 0) lc (tablet 0)      ; free noms/operator heads -> LO index; lc the counter

   ; read a file's top-level forms as DATA (the reader strips ; comments)

   (e2 l) (cap (cup l))  (e3 l) (cap (cup (cup l)))  (e4 l) (cap (cup (cup (cup l))))  ; 2nd 3rd 4th
   ; exactly three elements? (STRUCTURAL chainp, never `!`: '(0) nets zero)
   (len3? x) (&& (two? x) (two? (cup x)) (two? (cup (cup x))) (! (two? (cup (cup (cup x))))))
   ; opfix wraps an operator form one layer ((f) == f); unwrap a 1-element list
   (unwrap r) (? (&& (two? r) (! (two? (cup r)))) (cap r) r)
   (fix1 x) (unwrap (opfix x))

   ; an integer binary operator symbol -> its Coq function (prefix), else 0
   (binop h) (? (id? h '+) "Z.add"  (id? h '-) "Z.sub"  (id? h '*) "Z.mul"
                (id? h '//) "Z.quot" (id? h '%) "Z.rem"  (id? h 'gcd) "Z.gcd"
                (id? h '&) "Z.land"  (id? h '|) "Z.lor"  (id? h '^) "Z.lxor"  0)
   (shiftf h) (? (id? h '<<) "shlz" (id? h '>>) "shrz" 0)  ; word shifts: the COUNT rides mod 64
   ; a WORD-lane operand: a charm, a nested bitwise tree of them, or a numeral
   ; spine that lands back in a charm
   (wz? o) (? (charm? o) 1
              (&& (two? o) (len3? o) (member? (cap o) '(& | ^))) (&& (wz? (e2 o)) (wz? (e3 o)))
              (&& (two? o) (charm? (cap o)) (allv? o)) (charm? (ev o))
              0)
   (byte? c) (&& (charm? c) (<= 0 c) (< c 256))
   bs (intern "\\")                                     ; the quote symbol \ (for '(..) z-list data)
   (zel? e) (| (charm? e) (id? e ()))                   ; a charm element, or the unit (reads 0)
   (zlist? l) (? (two? l) (&& (zel? (cap l)) (zlist? (cup l))) (id? l ()))  ; PROPER list of them (ends in (), not any atom)
   ; a NETTABLE quoted list may also hold noms (each nets its spelling) -- good for the
   ; MEASURE lanes (sat/net/tally) ONLY: extraction or elementwise = would conflate a
   ; nom with its net (caup answers the SYMBOL, never a number)
   (znlist? l) (? (two? l) (&& (| (zel? (cap l)) (nom? (cap l))) (znlist? (cup l))) (id? l ()))
   (qnlist? x) (&& (two? x) (id? (cap x) bs) (un2? x) (znlist? (e2 x)))
   (qlist? x) (&& (two? x) (id? (cap x) bs) (un2? x) (zlist? (e2 x)))  ; '(1 2 3): \ + a z-list, EXACTLY 2 (a lambda is 3+)
   (arrv? x)  (&& (two? x) (id? (cap x) 'tuple) (zlist? (cup x)))      ; @(1 2 3) = (tuple 1 2 3): a z-array
   (iotav? x) (&& (two? x) (id? (cap x) 'iota) (un2? x) (intv? (e2 x)))  ; (iota n): the z-array 0..n-1
   (un2? x) (&& (two? (cup x)) (! (two? (cup (cup x))))) ; a 2-element (op arg) form

   ; -- lambdas as Coq funs (single param, exact body), sections, escaped operators --
   (lamform? x) (&& (two? x) (id? (cap x) bs) (two? (cup x)) (two? (cup (cup x))))  ; (\ p .. b): 3+ elements (2 is quote)
   (lam1? x) (&& (lamform? x) (! (two? (cup (cup (cup x))))) (nom? (e2 x)))         ; exactly (\ p b), p a nom
   (inlam f k) (: v ("v" + (show (peep vc 0 0))) _ (pin vc 0 (+ 1 (peep vc 0 0)))
                  _ (pin vtab (e2 f) v) r (k v) _ (pull vtab (e2 f) 0) r)
   ; a function TERM (map/fold/compose argument, or a beta head) -> its Coq fun, else 0
   (tfn f) (? (! (two? f)) 0
              (&& (! (two? (cup f))) (binop (cap f))) (binop (cap f))               ; (+) -- the escaped operator
              (&& (un2? f) (binop (cap f)) (intv? (e2 f)))
                ("(" + (binop (cap f)) + " " + (tv (e2 f)) + ")")                   ; (+ 1) -- the left section
              (lam1? f) (inlam f (\ v (? (intv? (e3 f))
                                         ("(fun " + v + " : Z => " + (tv (e3 f)) + ")") 0)))
              0)
   ; a PREDICATE term (filter/all/any): a bool body rides bare, an integer body is true iff POSITIVE
   (tfb f) (? (lam1? f) (inlam f (\ v (: p (tp (e3 f))
                                        (? p ("(fun " + v + " : Z => " + p + ")")
                                           (intv? (e3 f)) ("(fun " + v + " : Z => Z.ltb 0 " + (tv (e3 f)) + ")")
                                           0))))
              0)
   (tfn2 f) (? (&& (two? f) (! (two? (cup f)))) (binop (cap f)) 0)                  ; (+) (*) as a fold's dyadic
   (cmpb f) (? (&& (two? f) (! (two? (cup f))))                                     ; sortby's goes-before test
               (? (id? (cap f) '<) "Z.leb"  (id? (cap f) '>) "(fun a b : Z => Z.leb b a)"  0)  0)

   ; -- the unit lane: () (and a fresh mint) rides through every dyadic --
   (mintv? x) (&& (two? x) (id? (cap x) 'mint))
   (unit? x)  (| (id? x ()) (mintv? x))
   ; a unit among the ELEMENTS (never a proper list's own terminal ())
   (hasu? x)  (? (unit? x) 1 (two? x) (| (hasu? (cap x)) (? (two? (cup x)) (hasu? (cup x)) 0)) 0)
   (uzf h) (? (binop h) (binop h) (shiftf h) (shiftf h)
              (id? h '/) "Z.div" 0)                     ; / only ever meets its unit (the Some/Some lane never fires)
   ; scalar u-tree: + - * recurse; the narrower ops take their unit DIRECTLY
   (uz? x) (? (unit? x) 1  (intv? x) 1
              (! (&& (two? x) (len3? x))) 0
              (: h (cap x)
                 (? (member? h '(+ - *)) (&& (uz? (e2 x)) (uz? (e3 x)))
                    (uzf h)              (| (&& (unit? (e2 x)) (uz? (e3 x)))
                                            (&& (uz? (e2 x)) (unit? (e3 x))))
                    0)))
   (tuz x) (? (unit? x) "None"  (intv? x) ("(Some " + (tv x) + ")")
              ("(uop " + (uzf (cap x)) + " " + (tuz (e2 x)) + " " + (tuz (e3 x)) + ")"))
   ; list u-tree: a unit on either side of + or * answers the list side
   (ul? x) (? (lst? x) 1
              (! (&& (two? x) (len3? x) (| (id? (cap x) '+) (id? (cap x) '*)))) 0
              (| (&& (unit? (e2 x)) (ul? (e3 x))) (&& (ul? (e2 x)) (unit? (e3 x)))))
   (tul x) (? (unit? x) "None"  (lst? x) ("(Some " + (tl x) + ")")
              ("(ulop " + (tul (e2 x)) + " " + (tul (e3 x)) + ")"))

   ; -- text as SPELLING: symbols and strings are byte lists under + (concat) and * (repeat);
   ;    equality is band-guarded (a symbol never = a string, whatever the spelling)
   (qnom? x) (&& (two? x) (id? (cap x) bs) (un2? x) (nom? (e2 x)))     ; 'ef
   (txt? x) (? (string? x) 1  (qnom? x) 1
               (! (two? x)) 0
               (: h (cap x)
                  (? (&& (id? h '+) (len3? x)) (&& (txt? (e2 x)) (txt? (e3 x)))
                     (&& (id? h '*) (len3? x)) (&& (txt? (e2 x)) (charm? (e3 x)))
                     (&& (id? h 'intern) (un2? x)) (txt? (e2 x))
                     (&& (id? h 'string) (un2? x)) (txt? (e2 x))
                     0)))
   (tband x) (? (string? x) 1  (qnom? x) 0
                (: h (cap x)
                   (? (id? h '+) (? (= 0 (tband (e2 x))) (tband (e3 x)) 1)   ; symbol+symbol stays symbol, a string wins
                      (id? h '*) (tband (e2 x))
                      (id? h 'intern) 0  1)))                                ; intern -> symbol, string -> string
   (ttl x) (? (string? x) ("[" + (joinb x 0) + "]")
              (qnom? x)   ("[" + (joinb (string (e2 x)) 0) + "]")
              (: h (cap x)
                 (? (id? h '+) ("(" + (ttl (e2 x)) + " ++ " + (ttl (e3 x)) + ")")
                    (id? h '*) ("(srep (Z.to_nat " + (tv (e3 x)) + ") " + (ttl (e2 x)) + ")")
                    (ttl (e2 x)))))                     ; intern / string: the spelling itself

   ; -- gaussian twins: exact complex over Z as (re im) pairs -- conj negates im, the
   ;    unit rides every dyadic, and the SIGN (so the order too) is LEX on (re, im);
   ;    $ of a lex-positive twin is its magnitude (the corpus instances are pythagorean)
   (twinv? x) (&& (two? x) (id? (cap x) 'twin) (len3? x) (charm? (e2 x)) (charm? (e3 x)))
   ; a twin/conj/i anywhere in the term: the GATE into the gaussian lane (a bare charm
   ; or unit is gaussian-coercible, but only in gaussian company)
   (hasg? x) (? (nom? x) (member? x '(i twin conj)) (two? x) (| (hasg? (cap x)) (hasg? (cup x))) 0)
   (gneg? x) (&& (two? x) (un2? x) (two? (cap x))
                 (: s (cap x) (&& (un2? s) (id? (cap s) '-) (id? (e2 s) 0))))   ; ((- 0) g): negation
   (gz? x) (? (twinv? x) 1  (id? x 'i) 1  (charm? x) 1  (unit? x) 1
              (! (two? x)) 0
              (gneg? x) (gz? (e2 x))
              (: h (cap x)
                 (? (&& (id? h 'conj) (un2? x)) (gz? (e2 x))
                    (&& (member? h '(+ - *)) (len3? x)) (&& (gz? (e2 x)) (gz? (e3 x)))
                    0)))
   (tg x) (? (twinv? x) ("(" + (tv (e2 x)) + ", " + (tv (e3 x)) + ")")
             (id? x 'i) "(0, 1)"
             (charm? x) ("(" + (tv x) + ", 0)")
             (gneg? x) ("(gsub (0, 0) " + (tg (e2 x)) + ")")
             (: h (cap x)
                (? (id? h 'conj) ("(gconj " + (tg (e2 x)) + ")")
                   (unit? (e2 x)) (tg (e3 x))                     ; the unit RIDES: the twin side is the answer
                   (unit? (e3 x)) (tg (e2 x))
                   (id? h '+) ("(gadd " + (tg (e2 x)) + " " + (tg (e3 x)) + ")")
                   (id? h '-) ("(gsub " + (tg (e2 x)) + " " + (tg (e3 x)) + ")")
                   ("(gmul " + (tg (e2 x)) + " " + (tg (e3 x)) + ")"))))
   ; a (list ..) / (array n ..) of gaussian elements: its net is the gadd fold
   (gels x) (? (id? (cap x) 'array) (cup (cup x)) (cup x))
   (allg? l) (? (two? l) (&& (gz? (cap l)) (allg? (cup l))) 1)
   (glist? x) (&& (two? x) (| (id? (cap x) 'list) (&& (id? (cap x) 'array) (charm? (e2 x))))
                  (two? (gels x)) (allg? (gels x)))
   (tgsum l) (? (two? (cup l)) ("(gadd " + (tg (cap l)) + " " + (tgsum (cup l)) + ")") (tg (cap l)))

   ; -- lambdas as DATA: love's function = is alpha + structural (no eta/beta/numeral
   ;    bridge), so a lambda literal translates to a de Bruijn term (ltm) and alpha-
   ;    equality is lteqb, plain structural equality on the indices. a bound param is
   ;    its LV depth, a free nom or operator head interns to an LO index (ltab), a
   ;    literal is LN; quote data and special forms in a body stay out
   (lidx nm) (? (peep ltab nm 0) (peep ltab nm 0)
                (: n (+ 1 (peep lc 0 0)) _ (pin lc 0 n) _ (pin ltab nm n) n))
   (idxof nm l k) (? (! (two? l)) -1 (id? (cap l) nm) k (idxof nm (cup l) (+ 1 k)))
   colon (intern ":") qm (intern "?")
   (lt x env) (? (charm? x) ("(LN " + (tv x) + ")")
                 (nom? x) (: p (idxof x env 0)
                             (? (>= p 0) ("(LV " + (show p) + ")")
                                ("(LO " + (show (lidx x)) + ")")))
                 (lamform? x) (ltlam (cup x) () env)
                 (! (two? x)) 0
                 (member? (cap x) (list bs colon qm)) 0
                 (: r (lt (cap x) env) (? r (ltap r (cup x) env) 0)))
   (ltap acc args env) (? (! (two? args)) acc
                          (: r (lt (cap args) env)
                             (? r (ltap ("(LAp " + acc + " " + r + ")") (cup args) env) 0)))
   ; params then body: the LAST param is the innermost binder (LV 0 in the body)
   (ltlam rest ps env) (? (two? (cup rest))
                          (? (nom? (cap rest)) (ltlam (cup rest) (link (cap rest) ps) env) 0)
                          (: r (lt (cap rest) (cat ps env))
                             (? r (llwrap r (tally ps)) 0)))
   (llwrap s n) (? (< 0 n) (llwrap ("(LLam " + s + ")") (- n 1)) s)

   ; -- forms that READ the unit: (:) (?) (\) and '(), a cond whose literal tests all
   ;    fail with no else, a map over (), && / || landing on nothing (the and-or-unit law)
   (qempty? x) (&& (two? x) (id? (cap x) bs) (un2? x) (id? (e2 x) ()))   ; '()
   (nothing? x) (| (id? x 0) (id? x ()))
   (condu? l) (? (! (two? l)) 1                                  ; pairs exhausted, no else: the unit
                 (! (two? (cup l))) 0                            ; a lone else would answer
                 (&& (nothing? (cap l)) (condu? (cup (cup l)))))
   (andu? l) (? (! (two? l)) 0
                (nothing? (cap l)) 1
                (charm? (cap l)) (andu? (cup l))
                0)
   (allnothing? l) (? (two? l) (&& (nothing? (cap l)) (allnothing? (cup l))) 1)
   (unitv? x) (? (id? x ()) 1  (qempty? x) 1
                 (! (two? x)) 0
                 (: h (cap x)
                    (? (! (two? (cup x))) (member? h (list colon qm bs))   ; (:) (?) (\): an empty form reads its head
                       (id? h qm) (condu? (cup x))
                       (id? h '&&) (andu? (cup x))
                       (id? h '||) (allnothing? (cup x))
                       (&& (id? h 'map) (len3? x)) (id? (e3 x) ())
                       0)))
   ; id? on LITERALS: noms intern (spelling decides), a charm is its tagged word, every
   ; unit-reading form is THE one unit; a quoted nonempty list or a lambda is a FRESH
   ; allocation, never id?-equal to another; and the unit is id?-distinct from 0
   (qplist? x) (&& (two? x) (id? (cap x) bs) (un2? x) (two? (e2 x)))
   (idv a b) (? (&& (qnom? a) (qnom? b)) (? (id? (e2 a) (e2 b)) "true" "false")
                (&& (unitv? a) (unitv? b)) "true"
                (&& (charm? a) (charm? b)) (? (id? a b) "true" "false")
                (| (&& (unitv? a) (charm? b)) (&& (charm? a) (unitv? b))) "false"
                (| (&& (unitv? a) (qnom? b)) (&& (qnom? a) (unitv? b))) "false"
                (&& (qplist? a) (qplist? b)) "false"
                (&& (lamform? a) (lamform? b)) "false"
                (| (&& (lamform? a) (charm? b)) (&& (charm? a) (lamform? b))) "false"
                0)

   ; the KIND of a collection value -- arrays BROADCAST elementwise, lists APPEND
   (arr? x) (| (arrv? x) (| (iotav? x)
              (&& (two? x) (len3? x) (: h (cap x)
                 (? (id? h '+) (&& (arr? (e2 x)) (arr? (e3 x)))
                    (| (id? h '*) (id? h '//)) (&& (arr? (e2 x)) (intv? (e3 x))) 0)))))
   (lst? x) (? (qlist? x) 1  (string? x) 1
               (! (two? x)) 0
               (: h (cap x)
                  (? (&& (id? h '+) (len3? x)) (| (&& (intv? (e2 x)) (lst? (e3 x)))       ; scalar prepend
                                                  (&& (lst? (e2 x)) (| (lst? (e3 x))       ; append
                                                                       (byte? (e3 x)))))   ; byte-append (the byte LAW: out of 0..255 answers ())
                     (&& (id? h '*) (len3? x)) (&& (lst? (e2 x)) (intv? (e3 x)))           ; repeat
                     (&& (id? h 'map) (len3? x)) (&& (tfn (e2 x)) (lst? (e3 x)))
                     (&& (id? h 'filter) (len3? x)) (&& (tfb (e2 x)) (lst? (e3 x)))
                     (&& (id? h 'sortby) (len3? x)) (&& (cmpb (e2 x)) (lst? (e3 x)))
                     (&& (id? h 'cat) (len3? x)) (&& (lst? (e2 x)) (lst? (e3 x)))
                     (&& (id? h 'take) (len3? x)) (&& (intv? (e2 x)) (lst? (e3 x)))
                     (&& (id? h 'drop) (len3? x)) (&& (intv? (e2 x)) (lst? (e3 x)))
                     (&& (id? h 'link) (len3? x)) (&& (intv? (e2 x)) (| (lst? (e3 x)) (id? (e3 x) ())))
                     (&& (id? h 'rev) (un2? x)) (lst? (e2 x))
                     (&& (id? h 'sort) (un2? x)) (lst? (e2 x))
                     (&& (id? h 'init) (un2? x)) (lst? (e2 x))
                     (&& (id? h 'cup) (un2? x)) (lst? (e2 x))
                     (&& (id? h 'jot) (un2? x)) (intv? (e2 x))
                     0)))
   (listv? x) (| (arr? x) (lst? x))                     ; any collection value
   ; the list reducers (love surface -> coq): net -> asum, prod -> aprod, max/min, tally -> count
   (lop? h) (| (id? h 'net) (| (id? h 'prod) (| (id? h 'tally) (| (id? h 'max) (id? h 'min)))))
   ; a hash literal #(k v ..) = (hash k v ..): an assoc list of fixnum chains (EVEN count)
   (even? l) (? (two? l) (! (even? (cup l))) 1)
   (hashv? x) (&& (two? x) (id? (cap x) 'hash) (zlist? (cup x)) (even? (cup x)))
   (len4? x) (&& (two? x) (two? (cup x)) (two? (cup (cup x)))
                 (two? (cup (cup (cup x)))) (! (two? (cup (cup (cup (cup x)))))))
   (chains l) (? (&& (two? l) (two? (cup l)))
               ("(" + (tv (cap l)) + ", " + (tv (e2 l)) + ")" + (? (two? (cup (cup l))) ";" "") + (chains (cup (cup l)))) "")
   (th x) ("[" + (chains (cup x)) + "]")             ; #(1 10 2 20) -> [(1, 10);(2, 20)]
   ; translate a collection value to a Coq `list Z` -- a unit element reads 0, a nom its spelling
   (zv e) (? (charm? e) (tv e) (nom? e) (show (net e)) "0")
   (joinl l) (? (two? l) ((zv (cap l)) + (? (two? (cup l)) ";" "") + (joinl (cup l))) "")
   (joinb s i) (? (< i (tally s)) ((show (s i)) + (? (< (+ 1 i) (tally s)) ";" "") + (joinb s (+ 1 i))) "")
   (tl x) (? (qnlist? x) ("[" + (joinl (e2 x)) + "]")    ; covers qlist? too; noms reach here via the measure lanes only
             (arrv? x)   ("[" + (joinl (cup x)) + "]")    ; tuple: the elements are the cup
             (iotav? x)  ("(iotaZ " + (tv (e2 x)) + ")")   ; spec.v's iota, Z-arg face
             (string? x)    ("[" + (joinb x 0) + "]")
             (: h (cap x)                                     ; the broadcast / append / repeat ops
                (? (id? h '+)  (? (arr? x)        ("(vaddt " + (tl (e2 x)) + " " + (tl (e3 x)) + ")")
                                  (intv? (e2 x))  ("(" + (tv (e2 x)) + " :: " + (tl (e3 x)) + ")")  ; scalar prepend
                                  (intv? (e3 x))  ("(" + (tl (e2 x)) + " ++ [" + (tv (e3 x)) + "])") ; byte-append
                                  ("(" + (tl (e2 x)) + " ++ " + (tl (e3 x)) + ")"))                ; list append
                   (id? h '*)  (? (arr? x) ("(vscale " + (tv (e3 x)) + " " + (tl (e2 x)) + ")")     ; array scale
                                  ("(srep (Z.to_nat " + (tv (e3 x)) + ") " + (tl (e2 x)) + ")"))    ; list repeat
                   (id? h '//) ("(vquot " + (tv (e3 x)) + " " + (tl (e2 x)) + ")")
                   (id? h 'map)    ("(map " + (tfn (e2 x)) + " " + (tl (e3 x)) + ")")
                   (id? h 'filter) ("(filter " + (tfb (e2 x)) + " " + (tl (e3 x)) + ")")
                   (id? h 'sortby) ("(insortby " + (cmpb (e2 x)) + " " + (tl (e3 x)) + ")")
                   (id? h 'cat)    ("(" + (tl (e2 x)) + " ++ " + (tl (e3 x)) + ")")
                   (id? h 'take)   ("(firstn (Z.to_nat " + (tv (e2 x)) + ") " + (tl (e3 x)) + ")")
                   (id? h 'drop)   ("(skipn (Z.to_nat " + (tv (e2 x)) + ") " + (tl (e3 x)) + ")")
                   (id? h 'link)   ("(" + (tv (e2 x)) + " :: " + (? (id? (e3 x) ()) "[]" (tl (e3 x))) + ")")
                   (id? h 'rev)    ("(rev " + (tl (e2 x)) + ")")
                   (id? h 'sort)   ("(insort " + (tl (e2 x)) + ")")
                   (id? h 'init)   ("(removelast " + (tl (e2 x)) + ")")
                   (id? h 'cup)    ("(skipn (Z.to_nat 1) " + (tl (e2 x)) + ")")
                   (id? h 'jot)    ("(iotaZ " + (tv (e2 x)) + ")")
                   "[]")))

   ; integer-valued term? a fixnum, a bound lambda param, an operator tree, a list
   ; reducer/indexer, a fold, a beta/compose application, a numeral spine -- or a
   ; PREDICATE (love's =/</! answer the charms 1|0; Z.b2z carries them into Z)
   (intv? x) (? (charm? x) 1
                (nom? x) (? (peep vtab x 0) 1 0)
                (two? x) (: h (cap x)
                            (? (binop h)     (&& (len3? x)
                                                 (? (member? h '(& | ^))          ; bitwise is the WORD lane:
                                                    (&& (wz? (e2 x)) (wz? (e3 x))) ; a big operand answers (), off the Z model
                                                    (&& (intv? (e2 x)) (intv? (e3 x)))))
                               (shiftf h)    (&& (len3? x) (charm? (e2 x)) (charm? (e3 x)))
                               (id? h 'saturate) (&& (un2? x) (| (intv? (e2 x)) (| (listv? (e2 x)) (| (hashv? (e2 x)) (| (qnlist? (e2 x)) (| (lamform? (e2 x)) (&& (hasg? (e2 x)) (glist? (e2 x)))))))))
                               (id? h 'peep)  (&& (len4? x) (| (hashv? (e2 x)) (listv? (e2 x))) (intv? (e3 x)) (intv? (e4 x)))
                               (lop? h)       (&& (un2? x) (| (listv? (e2 x)) (qnlist? (e2 x))))
                               (id? h 'abs)   (&& (un2? x) (intv? (e2 x)))
                               (id? h 'cap)   (&& (un2? x) (lst? (e2 x)))
                               (id? h 'caup)  (&& (un2? x) (lst? (e2 x)))
                               (id? h 'last)  (&& (un2? x) (lst? (e2 x)))
                               (| (id? h 'foldl) (id? h 'foldr))
                                              (&& (len4? x) (tfn2 (e2 x)) (intv? (e3 x)) (lst? (e4 x)))
                               (hashv? h)     (&& (un2? x) (intv? (e2 x)))   ; (#(..) k) map lookup
                               (&& (two? h) (un2? x)) (&& (tfn h) (intv? (e2 x)))  ; direct beta / an applied section
                               (charm? h)     (| (allv? x)
                                                 (&& (len3? x) (tfn (e2 x)) (intv? (e3 x))))  ; n-fold compose
                               (: p (tp x) (? p 1 0))))                      ; a predicate, as its 1|0 charm
                0)
   (allv? l) (? (two? l) (&& (intv? (cap l)) (allv? (cup l))) (! (two? l)))
   ; a CLOSED +/-/* tree of numeric literals: evaluable with no free vars, so we can
   ; check its actual value. the Coq model has only Vz (fixnum) for integers -- no big
   ; kind -- so an expr that OVERFLOWS a charm (e.g. (* 2 2^61) = 2^62) is unmodellable
   ; for a charm? test and must skip, not masquerade as Vz. (used by tk below.)
   (concrete-int? x) (? (two? x) (&& (len3? x) (member? (cap x) '(+ - *))
                                     (concrete-int? (e2 x)) (concrete-int? (e3 x)))
                          (&& (! (nom? x)) (! (two? x))))

   ; translate an integer-valued term to a Coq Z expression
   (tv x) (? (charm? x) (? (< x 0) ("(" + (show x) + ")") (show x))  ; parens: `Z.eqb -2` parses as subtraction
             (nom? x) (peep vtab x "?")                              ; a lambda param in flight
             (: h (cap x) b (binop h)
                (? b ("(" + b + " " + (tv (e2 x)) + " " + (tv (e3 x)) + ")")   ; (Z.add a b) / (Z.gcd a b)
                   (shiftf h) ("(" + (shiftf h) + " " + (tv (e2 x)) + " " + (tv (e3 x)) + ")")
                   (id? h 'saturate) (? (lamform? (e2 x)) "1"                        ; a closure nets 1
                                        (&& (hasg? (e2 x)) (glist? (e2 x))) ("(gsat " + (tgsum (gels (e2 x))) + ")")
                                        (hashv? (e2 x)) ("(Z.of_nat (length " + (th (e2 x)) + "))")  ; $hash = key count
                                        (| (listv? (e2 x)) (qnlist? (e2 x))) ("(Z.max 0 (asum " + (tl (e2 x)) + "))")
                                        ("(Z.max 0 " + (tv (e2 x)) + ")"))  ; $list / $int
                   (id? h 'net)   ("(asum " + (tl (e2 x)) + ")")   ; net/+list -- the sum
                   (id? h 'tally) ("(Z.of_nat (length " + (tl (e2 x)) + "))")  ; count
                   (id? h 'max)  ("(amax " + (tl (e2 x)) + ")")
                   (id? h 'min)  ("(amin " + (tl (e2 x)) + ")")
                   (id? h 'prod) ("(aprod " + (tl (e2 x)) + ")")
                   (id? h 'abs)   ("(Z.abs " + (tv (e2 x)) + ")")
                   (id? h 'cap)   ("(hd 0 " + (tl (e2 x)) + ")")
                   (id? h 'caup)  ("(nth (Z.to_nat 1) " + (tl (e2 x)) + " 0)")
                   (id? h 'last)  ("(last " + (tl (e2 x)) + " 0)")
                   (id? h 'foldl) ("(fold_left " + (tfn2 (e2 x)) + " " + (tl (e4 x)) + " " + (tv (e3 x)) + ")")
                   (id? h 'foldr) ("(fold_right " + (tfn2 (e2 x)) + " " + (tv (e3 x)) + " " + (tl (e4 x)) + ")")
                   (id? h 'peep)  (? (hashv? (e2 x)) ("(hget " + (th (e2 x)) + " " + (tv (e3 x)) + " " + (tv (e4 x)) + ")")
                                     ("(nth (Z.to_nat " + (tv (e3 x)) + ") " + (tl (e2 x)) + " " + (tv (e4 x)) + ")"))  ; hash lookup / array index
                   (hashv? h)     ("(hget " + (th h) + " " + (tv (e2 x)) + " 0)")  ; (#(..) k) -- lookup, default 0
                   (&& (two? h) (tfn h)) ("(" + (tfn h) + " " + (tv (e2 x)) + ")") ; beta / applied section
                   (&& (charm? h) (allv? x)) (spine (tv h) (cup x))                ; (app .. ..) power spine
                   (charm? h) ("(Nat.iter (Z.to_nat " + (tv h) + ") " + (tfn (e2 x)) + " " + (tv (e3 x)) + ")")  ; n-fold compose
                   (: p (tp x) ("(Z.b2z " + p + ")")))))                           ; the predicate's charm
   (spine acc rest) (? (two? rest)
                       (spine ("(appZ " + acc + " " + (tv (cap rest)) + ")") (cup rest)) acc)

   ; the KIND tag of a literal (syntactic) for the type predicates; "Vbot" if untaggable
   (tk x) (? (unitv? x) "Vunit"                                        ; () and every form that reads it
             (intv? x) (? (&& (concrete-int? x) (! (charm? (ev x)))) "Vbot" "Vz")  ; a concrete overflow (big) is unmodellable -> skip
             (gem? x) "Vflo"  (string? x) "Vstr"  (id? x 'i) "Vcx"
             (two? x) (: h (cap x)
                        (? (id? h bs) (tkq (e2 x))                     ; '(..) or 'x
                           (id? h 'twin) "Vcx"                         ; ~(re im)
                           (id? h 'conj) "Vcx"                         ; conj always answers a twin
                           (id? h 'hash) "Vmap"                        ; #(..)
                           (| (id? h 'tuple) (id? h 'iota)) "Varr"     ; @(..) / (iota n)
                           "Vbot"))
             "Vbot")
   (tkq q) (? (nom? q) "Vsym" (two? q) "Vpair" (charm? q) "Vz" "Vbot")   ; the quoted datum's kind
   (kpred h) (? (id? h 'charm?) "fixp" (id? h 'gem?) "flop" (id? h 'twin?) "comp"
                (id? h 'string?) "strp" (id? h 'nom?) "nomp" (id? h 'name?) "namep" (id? h 'two?) "chainp"
                (id? h 'tablet?) "tabp" (id? h 'set?) "setp" (id? h 'constellation?) "nump"
                (id? h 'whole?) "whole" (id? h 'atom?) "atomp" (id? h 'packp) "packp"
                (id? h 'lit?) "lamp" 0)
   ; cross-kind ORDER: symbol<string<number<chain<map by band; same band stays value/lex
   (ordop? h) (| (id? h '<) (| (id? h '<=) (| (id? h '>) (id? h '>=))))
   (litband x) (: t (tk x)
                 (? (= t "Vbot") -1  (= t "Vunit") -1                 ; the unit stays out of the band order
                    (= t "Vsym") 0 (= t "Vstr") 1
                    (| (= t "Vz") (| (= t "Vflo") (| (= t "Vcx") (= t "Varr")))) 2
                    (= t "Vpair") 3 (= t "Vmap") 4  9))
   (xband? f) (&& (len3? f) (ordop? (cap f)) (>= (litband (e2 f)) 0) (>= (litband (e3 f)) 0)
                  (! (= (litband (e2 f)) (litband (e3 f)))))           ; both tagged, DIFFERENT bands
   (xband f) (: h (cap f) a ("(vband " + (tk (e2 f)) + ")") b ("(vband " + (tk (e3 f)) + ")")
               (? (id? h '<)  ("(Z.ltb " + a + " " + b + ")")  (id? h '<=) ("(Z.leb " + a + " " + b + ")")
                  (id? h '>)  ("(Z.ltb " + b + " " + a + ")")  ("(Z.leb " + b + " " + a + ")")))

   ; translate a top-level predicate to a Coq bool, or 0 if not translatable.
   ; `!` factors to `nilp`: (nil? x) is the bit "x is nothing" (x <= 0 for integers);
   ; (nil? (nil? x)) is `!!x`, x truthy -- the negation of the inner bit.
   (tp f) (? (! (two? f)) 0
             (: h (cap f)
                (? (id? h 'nil?) (: a (e2 f)
                                   (? (intv? a)  ("(Z.leb " + (tv a) + " 0)")
                                      (listv? a) ("(Z.leb (asum " + (tl a) + ") 0)")  ; !list: net <= 0
                                      (unitv? a) "true"                               ; the unit is nothing
                                      (lamform? a) "false"                            ; a closure nets 1: something
                                      (&& (hasg? a) (gz? a)) ("(negb (gpos " + (tg a) + "))")       ; a twin is true iff LEX-positive
                                      (&& (hasg? a) (glist? a)) ("(negb (gpos " + (tgsum (gels a)) + "))")
                                      (: p (tp a) (? p ("(negb " + p + ")") 0))))
                   (kpred h)  (: t (tk (e2 f)) (? (= t "Vbot") 0 ("(" + (kpred h) + " " + t + ")")))  ; (charm? 5), (two? '(..))..
                   (id? h 'member?) (? (&& (len3? f) (intv? (e2 f)) (lst? (e3 f)))
                                       ("(existsb (Z.eqb " + (tv (e2 f)) + ") " + (tl (e3 f)) + ")") 0)
                   (id? h 'all) (? (&& (len3? f) (lst? (e3 f)) (tfb (e2 f)))
                                   ("(forallb " + (tfb (e2 f)) + " " + (tl (e3 f)) + ")") 0)
                   (id? h 'any) (? (&& (len3? f) (lst? (e3 f)) (tfb (e2 f)))
                                   ("(existsb " + (tfb (e2 f)) + " " + (tl (e3 f)) + ")") 0)
                   (! (len3? f)) 0
                   (id? h 'id?) (idv (e2 f) (e3 f))                   ; id? on literals: the verdict, or 0 (skip)
                   (id? h '=) (? (&& (intv? (e2 f)) (intv? (e3 f)))   ("(Z.eqb " + (tv (e2 f)) + " " + (tv (e3 f)) + ")")
                                 (&& (listv? (e2 f)) (listv? (e3 f))) ("(leqb " + (tl (e2 f)) + " " + (tl (e3 f)) + ")")  ; list/array =
                                 ; gaussian =: numeric within the band, so a zero-im twin equals its charm
                                 (&& (| (hasg? (e2 f)) (hasg? (e3 f))) (gz? (e2 f)) (gz? (e3 f)))
                                   ("(geqb " + (tg (e2 f)) + " " + (tg (e3 f)) + ")")
                                 ; lambda =: alpha + structural on the de Bruijn terms
                                 (&& (lamform? (e2 f)) (lamform? (e3 f)))
                                   (: a (lt (e2 f) ()) b (lt (e3 f) ())
                                      (? (&& a b) ("(lteqb " + a + " " + b + ")") 0))
                                 ; no eta: a literal closure never = a plain nom (a nif/ap value)
                                 (| (&& (lamform? (e2 f)) (nom? (e3 f))) (&& (nom? (e2 f)) (lamform? (e3 f)))) "false"
                                 ; the unit lanes: a mint rides an OP like a unit but never STANDS as an
                                 ; = side itself (two mints are distinct; only a literal () compares)
                                 (&& (! (mintv? (e2 f))) (! (mintv? (e3 f)))
                                     (| (hasu? (e2 f)) (hasu? (e3 f))) (uz? (e2 f)) (uz? (e3 f)))
                                   ("(ueq " + (tuz (e2 f)) + " " + (tuz (e3 f)) + ")")          ; the unit rides, scalars
                                 (&& (! (mintv? (e2 f))) (! (mintv? (e3 f)))
                                     (| (hasu? (e2 f)) (hasu? (e3 f))) (ul? (e2 f)) (ul? (e3 f)))
                                   ("(uleq " + (tul (e2 f)) + " " + (tul (e3 f)) + ")")         ; the unit rides, lists
                                 (&& (txt? (e2 f)) (txt? (e3 f)))
                                   (? (= (tband (e2 f)) (tband (e3 f)))
                                      ("(leqb " + (ttl (e2 f)) + " " + (ttl (e3 f)) + ")") 0)   ; text: same band, same spelling
                                 (&& (>= (litband (e2 f)) 0) (>= (litband (e3 f)) 0)
                                     (! (= (litband (e2 f)) (litband (e3 f))))) "false"         ; cross-band = is false
                                 0)
                   ; gaussian ordering is LEX on (re, im)
                   (&& (ordop? h) (| (hasg? (e2 f)) (hasg? (e3 f))) (gz? (e2 f)) (gz? (e3 f)))
                     (: a (tg (e2 f)) b (tg (e3 f))
                        (? (id? h '<)  ("(gltb " + a + " " + b + ")")
                           (id? h '>)  ("(gltb " + b + " " + a + ")")
                           (id? h '<=) ("(negb (gltb " + b + " " + a + "))")
                                       ("(negb (gltb " + a + " " + b + "))")))
                   ; functions carry no strict order: < and > answer nothing
                   (&& (| (id? h '<) (id? h '>)) (lamform? (e2 f)) (lamform? (e3 f))) "false"
                   (xband? f) (xband f)                               ; cross-kind ordering: symbol<string<number<chain<map
                   (&& (ordop? h) (lst? (e2 f)) (lst? (e3 f)))        ; same-band chain/string order is LEX
                     (: a (tl (e2 f)) b (tl (e3 f))
                        (? (id? h '<)  ("(lexltb " + a + " " + b + ")")
                           (id? h '>)  ("(lexltb " + b + " " + a + ")")
                           (id? h '<=) ("(negb (lexltb " + b + " " + a + "))")
                                       ("(negb (lexltb " + a + " " + b + "))")))
                   (&& (ordop? h) (| (id? (e2 f) ()) (id? (e3 f) ())) (uz? (e2 f)) (uz? (e3 f)))
                     (: a ("(unet " + (tuz (e2 f)) + ")") b ("(unet " + (tuz (e3 f)) + ")")
                        (? (id? h '<)  ("(Z.ltb " + a + " " + b + ")")
                           (id? h '>)  ("(Z.ltb " + b + " " + a + ")")
                           (id? h '<=) ("(Z.leb " + a + " " + b + ")")
                                       ("(Z.leb " + b + " " + a + ")")))
                   (! (&& (intv? (e2 f)) (intv? (e3 f)))) 0           ; the within-band ORDERING ops below stay integer-only
                   (id? h '<)  ("(Z.ltb " + (tv (e2 f)) + " " + (tv (e3 f)) + ")")
                   (id? h '<=) ("(Z.leb " + (tv (e2 f)) + " " + (tv (e3 f)) + ")")
                   (id? h '>)  ("(Z.ltb " + (tv (e3 f)) + " " + (tv (e2 f)) + ")")
                   (id? h '>=) ("(Z.leb " + (tv (e3 f)) + " " + (tv (e2 f)) + ")")
                   0)))

   ; the original assert (infix, as in spec.l) shown above each theorem. Coq
   ; comments NEST: a shown `(*)` (the escaped operator) opens one and swallows
   ; the file, so pad the two magic pairs apart.
   (esc s) (: n (tally s)
              ((: (go i acc)
                 (? (< i n)
                    (: c (s i) d (? (< (+ 1 i) n) (s (+ 1 i)) -1)
                       (? (| (&& (= c 40) (= d 42)) (&& (= c 42) (= d 41)))
                          (go (+ 1 i) (acc + c + 32))
                          (go (+ 1 i) (acc + c))))
                    acc)) 0 ""))
   (thm n f s) ("(* " + (esc (show f)) + " *)" + (string 10) + "Theorem gen_" + (show n) + " : " + s + " = true.  Proof. vm_compute. reflexivity. Qed." + (string 10))

   nt (tablet 0) ns (tablet 0)                          ; translated / seen counters
   (bump b) (pin b 0 (+ 1 (peep b 0 0)))
   (walkops ops) (? (! ops) 0
                 (: f (cap ops) _ (bump ns) s (tp (fix1 f))
                    _ (? s (: _ (bump nt) (. (thm (peep nt 0 0) f s)))
                           (say err ("  UNMODELED: " + (show f) + (string 10))))   ; LOUD: an assert the generator can't translate -> never proved
                    (walkops (cup ops))))
   (walk fs) (? (! fs) 0
                (: f (cap fs)
                   _ (? (&& (two? f) (id? (cap f) 'assert)) (walkops (cup f)) 0)
                   (walk (cup fs))))

   _ (. (join `(
     "(* test/proof/rocq/gen.v -- GENERATED from test/spec.l by mk/tools/spec2coq.l. Do not edit;" (string 10)
     "   regenerate with `make test_gen`. Each integer-comparison corpus assert is" (string 10)
     "   translated to Coq, with its source assert shown in the comment above it." (string 10)
     "   The proofs are `vm_compute. reflexivity.`: every goal is a CLOSED computation," (string 10)
     "   so vm_compute RUNS both sides (Coq's bytecode VM -- fast even on bignums like" (string 10)
     "   2^64) to a normal form, and reflexivity checks the two results are identical." (string 10)
     "   A church numeral n applied to x is x ** n (`appZ`); a list/string nets the" (string 10)
     "   sum of its elements/bytes (`asum`). The MODEL IS SHARED: this file imports" (string 10)
     "   test/proof/rocq/spec.v and checks against ITS definitions -- appZ/asum/aprod/amax/vscale/srep" (string 10)
     "   verbatim, iotaZ the Z-arg face, app_appZ proving appZ IS the nat `app` -- so the" (string 10)
     "   generated instances and the hand-written LAWS speak one model. The defs below are" (string 10)
     "   gen-only (no spec.v twin): amin, leqb, the TOTAL vaddt (spec.v's vadd is option-" (string 10)
     "   valued for its shape-mismatch law), vquot, hget, the syntactic kind lattice," (string 10)
     "   the word shifts (count mod 64), the option-lifted UNIT lane (None rides through" (string 10)
     "   uop/ulop -- () the identity of every dyadic), insertion sort, lex order, the" (string 10)
     "   gaussian twins (exact complex over Z; sign and order LEX on (re, im)), and the" (string 10)
     "   de Bruijn lambda terms (function = is alpha + structural: lteqb). *)" (string 10)
     "From Stdlib Require Import ZArith List." (string 10)
     "Require Import spec." (string 10)
     "Import ListNotations." (string 10)
     "Open Scope Z_scope." (string 10)
     "Definition amin (l:list Z):Z := match l with nil=>0 | x::xs=>fold_right Z.min x xs end." (string 10)
     "Fixpoint leqb (a b:list Z):bool := match a,b with nil,nil=>true | x::a',y::b'=>andb (Z.eqb x y) (leqb a' b') | _,_=>false end." (string 10)
     "Fixpoint vaddz (a b:list Z):list Z := match a,b with x::a',y::b'=>(x+y)::vaddz a' b' | _,_=>nil end." (string 10)
     ; love adds galaxies ELEMENTWISE only at equal length; a length mismatch answers () (net 0). the
     ; length guard makes the model faithful (a truncating vaddt would net the common prefix -- wrong).
     "Definition vaddt (a b:list Z):list Z := if Nat.eqb (length a) (length b) then vaddz a b else nil." (string 10)
     "Definition vquot (c:Z):list Z->list Z := map (fun e=>Z.quot e c)." (string 10)
     "Fixpoint hget (m:list (Z*Z)) (k d:Z):Z := match m with nil=>d | p::m' => if Z.eqb k (fst p) then snd p else hget m' k d end." (string 10)
     ; the word shifts: the count rides mod 64, >> is arithmetic (Z.shiftr on negatives)
     "Definition shlz (a b:Z):Z := Z.shiftl a (Z.modulo b 64).  Definition shrz (a b:Z):Z := Z.shiftr a (Z.modulo b 64)." (string 10)
     ; the UNIT lane: None is () and rides through every lifted dyadic untouched
     "Definition uop (f:Z->Z->Z) (a b:option Z) : option Z := match a,b with None,x=>x | x,None=>x | Some x, Some y => Some (f x y) end." (string 10)
     "Definition ueq (a b:option Z) : bool := match a,b with None,None=>true | Some x, Some y => Z.eqb x y | _,_=>false end." (string 10)
     "Definition unet (a:option Z) : Z := match a with None => 0 | Some z => z end." (string 10)
     "Definition ulop (a b:option (list Z)) : option (list Z) := match a,b with None,x=>x | x,None=>x | Some x, Some y => Some (x++y) end." (string 10)
     "Definition uleq (a b:option (list Z)) : bool := match a,b with None,None=>true | Some x, Some y => leqb x y | _,_=>false end." (string 10)
     ; insertion sort, parameterized by the goes-before test (sortby's comparator)
     "Fixpoint insZ (le:Z->Z->bool) (x:Z) (l:list Z) : list Z := match l with nil => x::nil | y::t => if le x y then x::y::t else y::insZ le x t end." (string 10)
     "Fixpoint insortby (le:Z->Z->bool) (l:list Z) : list Z := match l with nil => nil | x::t => insZ le x (insortby le t) end." (string 10)
     "Definition insort := insortby Z.leb." (string 10)
     ; same-band chain/string order: lexicographic, a proper prefix sorts first
     "Fixpoint lexltb (a b:list Z) : bool := match a,b with nil,nil=>false | nil,_=>true | _,nil=>false | x::a',y::b' => orb (Z.ltb x y) (andb (Z.eqb x y) (lexltb a' b')) end." (string 10)
     ; gaussian twins: exact complex over Z as (re, im) pairs; the sign (and the order) is LEX
     "Definition gadd (a b:Z*Z):Z*Z := (fst a + fst b, snd a + snd b)." (string 10)
     "Definition gsub (a b:Z*Z):Z*Z := (fst a - fst b, snd a - snd b)." (string 10)
     "Definition gmul (a b:Z*Z):Z*Z := (fst a * fst b - snd a * snd b, fst a * snd b + snd a * fst b)." (string 10)
     "Definition gconj (a:Z*Z):Z*Z := (fst a, - snd a)." (string 10)
     "Definition geqb (a b:Z*Z):bool := andb (Z.eqb (fst a) (fst b)) (Z.eqb (snd a) (snd b))." (string 10)
     "Definition gltb (a b:Z*Z):bool := orb (Z.ltb (fst a) (fst b)) (andb (Z.eqb (fst a) (fst b)) (Z.ltb (snd a) (snd b)))." (string 10)
     ; the TRUTH GATE is the REAL PART, not the total order -- `gltb` stays
     ; lexicographic because sorting needs totality, but truth cannot depend on
     ; which root of x^2+1 we named `i` (conjugation is a field automorphism, and
     ; C admits no order compatible with its arithmetic). so a pure phase is BLUE.
     "Definition gpos (a:Z*Z):bool := Z.ltb 0 (fst a)." (string 10)
     ; $ of a twin: its REAL PART once the gate passes, else the 0 floor -- the same
     ; part the gate reads, so clamp and gate agree; the magnitude is abs's
     "Definition gsat (a:Z*Z):Z := if gpos a then fst a else 0." (string 10)
     ; de Bruijn lambda terms: love compares functions by ALPHA + structure (no eta/beta/numeral bridge),
     ; which is plain structural equality on the de Bruijn form
     "Inductive ltm := LV (n:nat) | LO (o:Z) | LN (z:Z) | LAp (f a:ltm) | LLam (b:ltm)." (string 10)
     "Fixpoint lteqb (a b:ltm):bool := match a,b with LV n,LV m=>Nat.eqb n m | LO n,LO m=>Z.eqb n m | LN n,LN m=>Z.eqb n m | LAp f x,LAp g y=>andb (lteqb f g) (lteqb x y) | LLam s,LLam t=>lteqb s t | _,_=>false end." (string 10)
     ; the KIND lattice: a value's tag, for the type predicates (a literal's kind is syntactic; vkind/vband avoid spec.v's value/band)
     "Inductive vkind := Vz | Vflo | Vcx | Vstr | Vsym | Vpair | Vmap | Varr | Vunit | Vbot." (string 10)
     "Definition fixp  v := match v with Vz   => true | _ => false end." (string 10)
     "Definition flop  v := match v with Vflo => true | _ => false end." (string 10)
     "Definition comp  v := match v with Vcx  => true | _ => false end." (string 10)
     "Definition strp  v := match v with Vstr => true | _ => false end." (string 10)
     "Definition nomp  v := match v with Vsym => true | _ => false end." (string 10)
     "Definition namep v := match v with Vsym => true | _ => false end." (string 10)
     "Definition chainp  v := match v with Vpair=> true | _ => false end." (string 10)
     "Definition tabp  v := match v with Vmap => true | _ => false end." (string 10)
     "Definition setp  v := match v with Varr => true | _ => false end." (string 10)
     "Definition whole   v := match v with Vz   => true | _ => false end." (string 10)
     "Definition nump  v := match v with Vz|Vflo|Vcx|Varr => true | _ => false end." (string 10)
     "Definition packp v := match v with Vflo|Vcx|Varr => true | _ => false end." (string 10)
     "Definition atomp v := match v with Vpair => false | _ => true end." (string 10)
     "Definition lamp  v := match v with Vmap => true | _ => false end." (string 10)   ; lit? = the top region (map + the unmodeled fns/hots); among modeled kinds only Vmap qualifies
     "Definition vband v : Z := match v with Vsym => 0 | Vstr => 1 | Vz|Vflo|Vcx|Varr => 2 | Vpair => 3 | Vmap => 4 | Vunit|Vbot => 9 end." (string 10) (string 10))))
   _ (say err ("spec2coq: UNMODELED asserts (seen but never proved) --" + (string 10)))
   _ (walk (forms (open "test/spec.l" "r")))
   _ (. ((string 10) + "(* " + (show (peep nt 0 0)) + " theorems generated from " + (show (peep ns 0 0)) + " asserts seen *)" + (string 10)))
   _ (say err ("spec2coq: " + (show (peep nt 0 0)) + " translated, " + (show (- (peep ns 0 0) (peep nt 0 0))) + " UNMODELED, of " + (show (peep ns 0 0)) + " asserts seen" + (string 10)))
   (quit 0))
