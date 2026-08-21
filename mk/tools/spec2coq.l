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
;   * the UNIT lane: () (and a fresh mint) rides through every dyadic EXCEPT *,
;     where it ANNIHILATES (the zero; the associativity law) -- modeled over
;     option Z / option (list Z), None the unit, uop/ulop lifted, umul/ulmul absorbing.
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

(: join (foldl (+) "")                           ; join a list of strings with + (was `cat`, which shadowed the global dyadic cat)
   ; the four interning tables, HOISTED to the head of the chain: bound down beside their
   ; readers, a capturing closure resolves them as a global import and the runtime says
   ; ";; missing vtab" &c. bound first, they resolve lexically and it is quiet.
   vtab {} vc {}      ; lambda params in flight: nom -> coq var; vc the freshness counter
   ltab {} lc {}      ; free noms/operator heads -> LO index; lc the counter

   ; read a file's top-level forms as DATA (the reader strips ; comments)

   ; exactly three elements? (STRUCTURAL chainp, never `!`: '(0) nets zero)
   (len3? [_ _ _]) 1
   ; opfix wraps an operator form one layer ((f) == f); unwrap a 1-element list
   (unwrap r) (two? r && atom? >r ? <r r)
   (fix1 x) (unwrap (unglue (opfix x)))          ; unglue: the glued lane emits VALUES, and this file matches WORDS

   ; an integer binary operator symbol -> its Coq function (prefix), else 0
   (binop h) (h @ '+ "Z.add"   '- "Z.sub"  '* "Z.mul"
                  '// "Z.quot" '% "Z.rem"  'gcd "Z.gcd"
                  '& "Z.land"  '| "Z.lor"  '^ "Z.lxor"  0)
   (shiftf h) (h @ '<< "shlz" '>> "shrz" 0)  ; word shifts: the COUNT rides mod 64
   ; a WORD-lane operand: a charm, a nested bitwise tree of them, or a numeral
   ; spine that lands back in a charm
   (wz? o) (? (charm? o) 1
              (len3? o && member? <o '(& | ^)) (wz? <>o && wz? <>>o)
              (two? o && charm? <o && allv? o) (charm? (ev o))
              0)
   bs '\                                                ; the quote symbol \ (for '(..) z-list data)
   (zel? e) (charm? e || id? e ())                      ; a charm element, or the unit (reads 0)
   (zlist? l) (two? l ? (zel? <l && zlist? >l) (id? l ()))  ; PROPER list of them (ends in (), not any atom)
   ; a NETTABLE quoted list may also hold noms (each nets its spelling) -- good for the
   ; MEASURE lanes (sat/net/tally) ONLY: extraction or elementwise = would conflate a
   ; nom with its net (caup answers the SYMBOL, never a number)
   (znlist? l) (two? l ? ((zel? <l || nom? <l) && znlist? >l) (id? l ()))
   (qnlist? x) (two? x && id? <x bs && un2? x && znlist? <>x)
   (qlist? x) (two? x && id? <x bs && un2? x && zlist? <>x)  ; '(1 2 3): \ + a z-list, EXACTLY 2 (a lambda is 3+)
   (arrv? x)  (two? x && id? <x 'tuple && zlist? >x)      ; @(1 2 3) = (tuple 1 2 3): a z-array
   (iotav? x) (two? x && id? <x 'iota && un2? x && intv? <>x)  ; (iota n): the z-array 0..n-1
   (un2? x) (two? >x && atom? >>x)                      ; a 2-element (op arg) form

   ; -- lambdas as Coq funs (single param, exact body), sections, escaped operators --
   (lamform? x) (two? x && id? <x bs && two? >x && two? >>x)  ; (\ p .. b): 3+ elements (2 is quote)
   (lam1? x) (lamform? x && atom? >>>x && nom? <>x)           ; exactly (\ p b), p a nom
   (inlam f k) (: v ("v" + show (peep vc 0 0)) _ (pin vc 0 (1 + peep vc 0 0))
                  _ (pin vtab <>f v) r (k v) _ (pull vtab <>f 0) r)
   ; a function TERM (map/fold/compose argument, or a beta head) -> its Coq fun, else 0
   (tfn f) (? (atom? f) 0
              (atom? >f && binop <f) (binop <f)                   ; (+) -- the escaped operator
              (un2? f && binop <f && intv? <>f)
                ("(" + binop <f + " " + tv <>f + ")")             ; (+ 1) -- the left section
              (lam1? f) (inlam f (v \ intv? <>>f ? ("(fun " + v + " : Z => " + tv <>>f + ")") 0))
              0)
   ; a PREDICATE term (filter/all/any): a bool body rides bare, an integer body is true iff POSITIVE
   (tfb f) (? (lam1? f) (inlam f (v \ (: p (tp <>>f)
                                        (? p ("(fun " + v + " : Z => " + p + ")")
                                           (intv? <>>f) ("(fun " + v + " : Z => Z.ltb 0 " + tv <>>f + ")")
                                           0))))
              0)
   (tfn2 f) (two? f && atom? >f ? (binop <f) 0)                   ; (+) (*) as a fold's dyadic
   (cmpb f) (two? f && atom? >f ?                                 ; sortby's goes-before test
             (<f @ '< "Z.leb" '> "(fun a b : Z => Z.leb b a)" 0) 0)

   ; -- the unit lane: () (and a fresh mint) rides through every dyadic --
   (mintv? ('mint >< _)) 1
   (unit? x)  (id? x () || mintv? x)
   ; a unit among the ELEMENTS (never a proper list's own terminal ())
   (hasu? x)  (? (unit? x) 1 (two? x) (hasu? <x || (two? >x ? (hasu? >x) 0)) 0)
   (uzf h) (binop h || shiftf h || (id? h '/ ? "Z.div" 0))  ; / only ever meets its unit (the Some/Some lane never fires)
   ; scalar u-tree: + - * recurse; the narrower ops take their unit DIRECTLY
   (uz? x) (? (unit? x) 1  (intv? x) 1
              (x @ [h i j] (? (member? h '(+ - *)) (uz? i && uz? j)
                              (uzf h)              ((unit? i && uz? j) || (uz? i && unit? j))
                              0)
                   0))
   (tuz x) (? (unit? x) "None"  (intv? x) ("(Some " + tv x + ")")
              (id? <x '*) ("(umul " + tuz <>x + " " + tuz <>>x + ")")   ; * : the unit ANNIHILATES
              ("(uop " + uzf <x + " " + tuz <>x + " " + tuz <>>x + ")"))
   ; list u-tree: a unit on either side of + answers the list side; under * it
   ; ANNIHILATES (() is the zero -- the associativity law)
   (ul? x) (? (lst? x) 1
              !(len3? x && (id? <x '+ || id? <x '*)) 0
              ((unit? <>x && ul? <>>x) || (ul? <>x && unit? <>>x)))
   (tul x) (? (unit? x) "None"  (lst? x) ("(Some " + tl x + ")")
              (id? <x '*) ("(ulmul " + tul <>x + " " + tul <>>x + ")")
              ("(ulop " + tul <>x + " " + tul <>>x + ")"))

   ; -- text as SPELLING: symbols and strings are byte lists under + (concat) and * (repeat);
   ;    equality is band-guarded (a symbol never = a string, whatever the spelling)
   (qnom? x) (two? x && id? <x bs && un2? x && nom? <>x)     ; 'ef
   (txt? x) (? (string? x) 1  (qnom? x) 1
               (atom? x) 0
               (<x @ ('+ && len3? x) (txt? <>x && txt? <>>x)
                     ('* && len3? x) (txt? <>x && charm? <>>x)
                     (('intern || 'string) && un2? x) (txt? <>x)
                     0))
   (tband x) (? (string? x) 1  (qnom? x) 0
                (<x @ '+ (tband <>x = 0 ? (tband <>>x) 1)   ; symbol+symbol stays symbol, a string wins
                      '* (tband <>x)
                      'intern 0  1))                        ; intern -> symbol, string -> string
   (ttl x) (? (string? x) ("[" + joinb x 0 + "]")
              (qnom? x)   ("[" + joinb (string <>x) 0 + "]")
              (<x @ '+ ("(" + ttl <>x + " ++ " + ttl <>>x + ")")
                    '* ("(srep (Z.abs_nat " + tv <>>x + ") " + ttl <>x + ")")
                    (ttl <>x)))                             ; intern / string: the spelling itself

   ; -- gaussian twins: exact complex over Z as (re im) pairs -- conj negates im, the
   ;    unit rides + and - (under * it annihilates, out of this band's reach), and
   ;    the SIGN (so the order too) is LEX on (re, im);
   ;    $ of a lex-positive twin is its magnitude (the corpus instances are pythagorean)
   (twinv? x) (len3? x && id? <x 'twin && charm? <>x && charm? <>>x)
   ; a twin/conj/i anywhere in the term: the GATE into the gaussian lane (a bare charm
   ; or unit is gaussian-coercible, but only in gaussian company)
   (hasg? x) (? (nom? x) (member? x '(i twin conj)) (two? x) (hasg? <x || hasg? >x) 0)
   (gneg? x) (two? x && un2? x && two? <x &&
              (: s <x (un2? s && id? <s '- && id? <>s 0)))        ; ((- 0) g): negation
   (gz? x) (? (twinv? x) 1  (id? x 'i) 1  (charm? x) 1  (unit? x) 1
              (atom? x) 0
              (gneg? x) (gz? <>x)
              (<x @ ('conj && un2? x) (gz? <>x)
                    ; ⚠ a unit under * leaves the band (() annihilates now), so only
                    ; + and - let the unit ride here; * demands both sides gaussian-proper
                    ('* && len3? x) (gz? <>x && gz? <>>x && !(hasu? <>x) && !(hasu? <>>x))
                    (('+ || '-) && len3? x) (gz? <>x && gz? <>>x)
                    0))
   (tg x) (? (twinv? x) ("(" + tv <>x + ", " + tv <>>x + ")")
             (id? x 'i) "(0, 1)"
             (charm? x) ("(" + tv x + ", 0)")
             (gneg? x) ("(gsub (0, 0) " + tg <>x + ")")
             (: h <x
                (? (id? h 'conj) ("(gconj " + tg <>x + ")")
                   (unit? <>x) (tg <>>x)                     ; the unit RIDES: the twin side is the answer
                   (unit? <>>x) (tg <>x)
                   (id? h '+) ("(gadd " + tg <>x + " " + tg <>>x + ")")
                   (id? h '-) ("(gsub " + tg <>x + " " + tg <>>x + ")")
                   ("(gmul " + tg <>x + " " + tg <>>x + ")"))))
   ; a (list ..) / (array n ..) of gaussian elements: its net is the gadd fold
   (gels x) (id? <x 'array ? >>x >x)
   (allg? l) (two? l ? (gz? <l && allg? >l) 1)
   (glist? x) (two? x && (id? <x 'list || (id? <x 'array && charm? <>x))
               && two? (gels x) && allg? (gels x))
   (tgsum l) (two? >l ? ("(gadd " + tg <l + " " + tgsum >l + ")") (tg <l))

   ; -- lambdas as DATA: love's function = is alpha + structural (no eta/beta/numeral
   ;    bridge), so a lambda literal translates to a de Bruijn term (ltm) and alpha-
   ;    equality is lteqb, plain structural equality on the indices. a bound param is
   ;    its LV depth, a free nom or operator head interns to an LO index (ltab), a
   ;    literal is LN; quote data and special forms in a body stay out
   (lidx nm) (peep ltab nm 0 ||
              (: n (1 + peep lc 0 0) _ (pin lc 0 n) _ (pin ltab nm n) n))
   (idxof nm l k) (? (atom? l) -1 (id? <l nm) k (idxof nm >l (k + 1)))
   colon ': qm '?
   (lt x env) (? (charm? x) ("(LN " + tv x + ")")
                 (nom? x) (: p (idxof x env 0)
                             (p >= 0 ? ("(LV " + show p + ")") ("(LO " + show (lidx x) + ")")))
                 (lamform? x) (ltlam >x () env)
                 (atom? x) 0
                 (member? <x [bs colon qm]) 0
                 (: r (lt <x env) (? r (ltap r >x env) 0)))
   (ltap acc args env) (? (atom? args) acc
                          (: r (lt <args env)
                             (? r (ltap ("(LAp " + acc + " " + r + ")") >args env) 0)))
   ; params then body: the LAST param is the innermost binder (LV 0 in the body)
   (ltlam rest ps env) (? (two? >rest)
                          (? (nom? <rest) (ltlam >rest (<rest >< ps) env) 0)
                          (: r (lt <rest (ps + env))
                             (? r (llwrap r (tally ps)) 0)))
   (llwrap s n) (0 < n ? (llwrap ("(LLam " + s + ")") (n - 1)) s)

   ; -- forms that READ the unit: (:) (?) (\) and '(), a cond whose literal tests all
   ;    fail with no else, a map over (), && / || landing on nothing (the and-or-unit law)
   (qempty? x) (two? x && id? <x bs && un2? x && id? <>x ())   ; '()
   (nothing? x) (id? x 0 || id? x ())
   (condu? l) (? (atom? l) 1                                    ; pairs exhausted, no else: the unit
                 (atom? >l) 0                                   ; a lone else would answer
                 (nothing? <l && condu? >>l))
   (andu? l) (? (atom? l) 0
                (nothing? <l) 1
                (charm? <l) (andu? >l)
                0)
   (allnothing? l) (two? l ? (nothing? <l && allnothing? >l) 1)
   (unitv? x) (? (id? x ()) 1  (qempty? x) 1
                 (atom? x) 0
                 (atom? >x) (member? <x [colon qm bs])   ; (:) (?) (\): an empty form reads its head
                 (<x @ '? (condu? >x)  '&& (andu? >x)  '|| (allnothing? >x)
                       ('map && len3? x) (id? <>>x ())
                       0))
   ; id? on LITERALS: noms intern (spelling decides), a charm is its tagged word, every
   ; unit-reading form is THE one unit; a quoted nonempty list or a lambda is a FRESH
   ; allocation, never id?-equal to another; and the unit is id?-distinct from 0
   (qplist? x) (two? x && id? <x bs && un2? x && two? <>x)
   (idv a b) (? (qnom? a && qnom? b) (id? <>a <>b ? "true" "false")
                (unitv? a && unitv? b) "true"
                (charm? a && charm? b) (id? a b ? "true" "false")
                ((unitv? a && charm? b)  || (charm? a && unitv? b) ||
                 (unitv? a && qnom? b)   || (qnom? a && unitv? b) ||
                 (qplist? a && qplist? b) || (lamform? a && lamform? b) ||
                 (lamform? a && charm? b) || (charm? a && lamform? b)) "false"
                0)

   ; the KIND of a collection value -- arrays BROADCAST elementwise, lists APPEND
   (arr? x) (arrv? x || iotav? x ||
             (len3? x && (<x @ '+ (arr? <>x && arr? <>>x)
                               ('* || '//) (arr? <>x && intv? <>>x)
                               0)))
   (lst? x) (? (qlist? x) 1  (string? x) 1
               (atom? x) 0
               ; + is three lanes: append, and either side DEGENERATING when a number
               ; meets a list (the associativity law)
               (x @ ['+ a b] ((intv? a && lst? b) || (lst? a && (lst? b || intv? b)))
                    ['* a b] (lst? a && intv? b)                          ; repeat
                    ['map a b]    (tfn a && lst? b)
                    ['filter a b] (tfb a && lst? b)
                    ['sortby a b] (cmpb a && lst? b)
                    ['cat a b]    (lst? a && lst? b)
                    [('take || 'drop) a b] (intv? a && lst? b)
                    ['link a b]   (intv? a && (lst? b || id? b ()))
                    [('rev || 'sort || 'init || 'cup) a] (lst? a)
                    ['jot a]      (intv? a)
                    0))
   (listv? x) (arr? x || lst? x)                     ; any collection value
   ; the list reducers (love surface -> coq): net -> asum, prod -> aprod, max/min, tally -> count
   (lop? h) (member? h '(net prod tally max min))
   ; a hash literal #(k v ..) = (hash k v ..): an assoc list of fixnum chains (EVEN count)
   (even? l) (two? l ? !(even? >l) 1)
   (hashv? x) (two? x && id? <x 'hash && zlist? >x && even? >x)
   (len4? [_ _ _ _]) 1
   (chains l) (atom? l || atom? >l ? ""
               ("(" + tv <l + ", " + tv <>l + ")" + (two? >>l ? ";" "") + chains >>l))
   (th x) ("[" + chains >x + "]")             ; #(1 10 2 20) -> [(1, 10);(2, 20)]
   ; translate a collection value to a Coq `list Z` -- a unit element reads 0, a nom its spelling
   (zv e) (? (charm? e) (tv e) (nom? e) (show (net e)) "0")
   (joinl l) (two? l ? (zv <l + (two? >l ? ";" "") + joinl >l) "")
   (joinb s i) (i < tally s ? (show (s i) + (i + 1 < tally s ? ";" "") + joinb s (i + 1)) "")
   (tl x) (? (qnlist? x) ("[" + joinl <>x + "]")     ; covers qlist? too; noms reach here via the measure lanes only
             (arrv? x)   ("[" + joinl >x + "]")      ; tuple: the elements are the cup
             (iotav? x)  ("(iotaZ " + tv <>x + ")")  ; spec.v's iota, Z-arg face
             (string? x) ("[" + joinb x 0 + "]")
             (: h <x                                          ; the broadcast / append / repeat ops
                (? (id? h '+)  (? (arr? x)    ("(vaddt " + tl <>x + " " + tl <>>x + ")")
                                  (intv? <>x)  (tl <>>x)   ; DEGENERATE: a number against a list arrives as the unit
                                  (intv? <>>x) (tl <>x)    ; ... either side (the associativity law)
                                  ("(" + tl <>x + " ++ " + tl <>>x + ")"))              ; list append
                   (id? h '*)  (? (arr? x) ("(vscale " + tv <>>x + " " + tl <>x + ")")   ; array scale
                                  ("(srep (Z.abs_nat " + tv <>>x + ") " + tl <>x + ")")) ; list repeat
                   (id? h '//) ("(vquot " + tv <>>x + " " + tl <>x + ")")
                   (h @ 'map    ("(map " + tfn <>x + " " + tl <>>x + ")")
                        'filter ("(filter " + tfb <>x + " " + tl <>>x + ")")
                        'sortby ("(insortby " + cmpb <>x + " " + tl <>>x + ")")
                        'cat    ("(" + tl <>x + " ++ " + tl <>>x + ")")
                        'take   ("(firstn (Z.to_nat " + tv <>x + ") " + tl <>>x + ")")
                        'drop   ("(skipn (Z.to_nat " + tv <>x + ") " + tl <>>x + ")")
                        'link   ("(" + tv <>x + " :: " + (id? <>>x () ? "[]" (tl <>>x)) + ")")
                        'rev    ("(rev " + tl <>x + ")")
                        'sort   ("(insort " + tl <>x + ")")
                        'init   ("(removelast " + tl <>x + ")")
                        'cup    ("(skipn (Z.to_nat 1) " + tl <>x + ")")
                        'jot    ("(iotaZ " + tv <>x + ")")
                        "[]"))))

   ; integer-valued term? a fixnum, a bound lambda param, an operator tree, a list
   ; reducer/indexer, a fold, a beta/compose application, a numeral spine -- or a
   ; PREDICATE (love's =/</! answer the charms 1|0; Z.b2z carries them into Z)
   (intv? x) (? (charm? x) 1
                (nom? x) (peep vtab x 0 ? 1 0)
                (two? x) (: h <x
                            (? (binop h)     (len3? x &&
                                              (member? h '(& | ^)             ; bitwise is the WORD lane:
                                               ? (wz? <>x && wz? <>>x)        ; a big operand answers (), off the Z model
                                                 (intv? <>x && intv? <>>x)))
                               (shiftf h)    (len3? x && charm? <>x && charm? <>>x)
                               (id? h 'saturate) (un2? x && (intv? <>x || listv? <>x || hashv? <>x ||
                                                             qnlist? <>x || lamform? <>x ||
                                                             (hasg? <>x && glist? <>x)))
                               (id? h 'peep)  (len4? x && (hashv? <>x || listv? <>x) && intv? <>>x && intv? <>>>x)
                               (lop? h)       (un2? x && (listv? <>x || qnlist? <>x))
                               (id? h 'abs)   (un2? x && intv? <>x)
                               (member? h '(cap caup last)) (un2? x && lst? <>x)
                               (member? h '(foldl foldr))
                                              (len4? x && tfn2 <>x && intv? <>>x && lst? <>>>x)
                               (hashv? h)     (un2? x && intv? <>x)          ; (#(..) k) map lookup
                               (two? h && un2? x) (tfn h && intv? <>x)       ; direct beta / an applied section
                               (charm? h)     (allv? x ||
                                               (len3? x && tfn <>x && intv? <>>x))  ; n-fold compose
                               (: p (tp x) (? p 1 0))))                      ; a predicate, as its 1|0 charm
                0)
   (allv? l) (two? l ? (intv? <l && allv? >l) (atom? l))
   ; a CLOSED tree of numeric literals under any modelled binary op: evaluable with no free
   ; vars, so we can check its actual value. the Coq model has only Vz (fixnum) for integers
   ; -- no big kind -- so an expr that OVERFLOWS a charm (e.g. (* 2 2^61) = 2^62) is
   ; unmodellable for a charm? test and must skip, not masquerade as Vz. ⚠ THE WHOLE binop
   ; ROW, not just +/-/*: a narrow op WIDENS on a zero divisor -- (% 7 0) is 0.0 and
   ; (// 7 0) is ieee-inf -- and the kind lattice here is syntactic, so it cannot see that.
   ; the value can, which is what this asks. (used by tk below.)
   (concrete-int? x) (? (two? x) (len3? x && binop <x &&
                                  concrete-int? <>x && concrete-int? <>>x)
                          (!(nom? x) && atom? x))

   ; translate an integer-valued term to a Coq Z expression
   (tv x) (? (charm? x) (x < 0 ? ("(" + show x + ")") (show x))  ; parens: `Z.eqb -2` parses as subtraction
             (nom? x) (peep vtab x "?")                          ; a lambda param in flight
             (: h <x b (binop h)
                (? b ("(" + b + " " + tv <>x + " " + tv <>>x + ")")   ; (Z.add a b) / (Z.gcd a b)
                   (shiftf h) ("(" + shiftf h + " " + tv <>x + " " + tv <>>x + ")")
                   (id? h 'saturate) (? (lamform? <>x) "1"                        ; a closure nets 1
                                        (hasg? <>x && glist? <>x) ("(gsat " + tgsum (gels <>x) + ")")
                                        (hashv? <>x) ("(Z.of_nat (length " + th <>x + "))")  ; $hash = key count
                                        (listv? <>x || qnlist? <>x) ("(satZ (asum " + tl <>x + "))")
                                        ("(satZ " + tv <>x + ")"))  ; $list / $int -- spec.v's clamp, floor AND ceiling
                   (id? h 'net)   ("(asum " + tl <>x + ")")   ; net/+list -- the sum
                   (id? h 'tally) ("(Z.of_nat (length " + tl <>x + "))")  ; count
                   (id? h 'max)  ("(amax " + tl <>x + ")")
                   (id? h 'min)  ("(amin " + tl <>x + ")")
                   (id? h 'prod) ("(aprod " + tl <>x + ")")
                   (id? h 'abs)   ("(Z.abs " + tv <>x + ")")
                   (id? h 'cap)   ("(hd 0 " + tl <>x + ")")
                   (id? h 'caup)  ("(nth (Z.to_nat 1) " + tl <>x + " 0)")
                   (id? h 'last)  ("(last " + tl <>x + " 0)")
                   (id? h 'foldl) ("(fold_left " + tfn2 <>x + " " + tl <>>>x + " " + tv <>>x + ")")
                   (id? h 'foldr) ("(fold_right " + tfn2 <>x + " " + tv <>>x + " " + tl <>>>x + ")")
                   (id? h 'peep)  (hashv? <>x ? ("(hget " + th <>x + " " + tv <>>x + " " + tv <>>>x + ")")
                                     ("(nth (Z.to_nat " + tv <>>x + ") " + tl <>x + " " + tv <>>>x + ")"))  ; hash lookup / array index
                   (hashv? h)     ("(hget " + th h + " " + tv <>x + " 0)")  ; (#(..) k) -- lookup, default 0
                   (two? h && tfn h) ("(" + tfn h + " " + tv <>x + ")")  ; beta / applied section
                   (charm? h && allv? x) (spine (tv h) >x)               ; (app .. ..) power spine
                   (charm? h) ("(Nat.iter (Z.to_nat " + tv h + ") " + tfn <>x + " " + tv <>>x + ")")  ; n-fold compose
                   (: p (tp x) ("(Z.b2z " + p + ")")))))                 ; the predicate's charm
   (spine acc rest) (two? rest ? (spine ("(appZ " + acc + " " + tv <rest + ")") >rest) acc)

   ; the KIND tag of a literal (syntactic) for the type predicates; "Vbot" if untaggable.
   ; ⚠ an int-SHAPED expression that is closed answers by its VALUE's kind, not its shape:
   ; a narrow op widens on a zero divisor ((% 7 0) is 0.0), and a big overflows the lattice,
   ; which has no big kind -- so that one skips rather than masquerading as Vz.
   (tk x) (? (unitv? x) "Vunit"                                        ; () and every form that reads it
             (intv? x) (? !(concrete-int? x) "Vz"
                          (: v (ev x) (? (charm? v) "Vz" (gem? v) "Vflo" "Vbot")))
             (gem? x) "Vflo"  (string? x) "Vstr"  (id? x 'i) "Vcx"
             (two? x) (: h <x
                        (? (id? h bs) (tkq <>x)                        ; '(..) or 'x
                           (member? h '(twin conj)) "Vcx"              ; ~(re im); conj always answers a twin
                           (id? h 'hash) "Vmap"                        ; #(..)
                           (member? h '(tuple iota)) "Varr"            ; @(..) / (iota n)
                           "Vbot"))
             "Vbot")
   (tkq q) (? (nom? q) "Vsym" (two? q) "Vpair" (charm? q) "Vz" "Vbot")   ; the quoted datum's kind
   (kpred h) (h @ 'charm? "fixp"   'gem? "flop"   'twin? "comp"
                  'string? "strp"  'nom? "nomp"   'name? "namep"  'two? "chainp"
                  'tablet? "tabp"  'set? "setp"   'constellation? "nump"
                  'whole? "whole"  'atom? "atomp" 'packp "packp"
                  'lit? "lamp" 0)
   ; cross-kind ORDER: symbol<string<number<chain<map by band; same band stays value/lex
   (ordop? h) (member? h '(< <= > >=))
   (litband x) (: t (tk x)
                 (? (t = "Vbot") -1  (t = "Vunit") -1                 ; the unit stays out of the band order
                    (t = "Vsym") 0 (t = "Vstr") 1
                    (t = "Vz" || t = "Vflo" || t = "Vcx" || t = "Varr") 2
                    (t = "Vpair") 3 (t = "Vmap") 4  9))
   (xband? f) (len3? f && ordop? <f && litband <>f >= 0 && litband <>>f >= 0 &&
               litband <>f != litband <>>f)                  ; both tagged, DIFFERENT bands
   (xband f) (: h <f a ("(vband " + tk <>f + ")") b ("(vband " + tk <>>f + ")")
               (h @ '<  ("(Z.ltb " + a + " " + b + ")")  '<= ("(Z.leb " + a + " " + b + ")")
                    '>  ("(Z.ltb " + b + " " + a + ")")  ("(Z.leb " + b + " " + a + ")")))

   ; translate a top-level predicate to a Coq bool, or 0 if not translatable.
   ; `!` factors to `nilp`: (nil? x) is the bit "x is nothing" (x <= 0 for integers);
   ; (nil? (nil? x)) is `!!x`, x truthy -- the negation of the inner bit.
   (tp f) (? (atom? f) 0
             (: h <f
                (? (id? h 'nil?) (: a <>f
                                   (? (intv? a)  ("(Z.leb " + tv a + " 0)")
                                      (listv? a) ("(Z.leb (asum " + tl a + ") 0)")   ; !list: net <= 0
                                      (unitv? a) "true"                              ; the unit is nothing
                                      (lamform? a) "false"                           ; a closure nets 1: something
                                      (hasg? a && gz? a) ("(negb (gpos " + tg a + "))")  ; a twin is true iff LEX-positive
                                      (hasg? a && glist? a) ("(negb (gpos " + tgsum (gels a) + "))")
                                      (: p (tp a) (? p ("(negb " + p + ")") 0))))
                   (kpred h)  (: t (tk <>f) (t = "Vbot" ? 0 ("(" + kpred h + " " + t + ")")))  ; (charm? 5), (two? '(..))..
                   (id? h 'member?) (len3? f && intv? <>f && lst? <>>f ?
                                     ("(existsb (Z.eqb " + tv <>f + ") " + tl <>>f + ")") 0)
                   (id? h 'all) (len3? f && lst? <>>f && tfb <>f ?
                                 ("(forallb " + tfb <>f + " " + tl <>>f + ")") 0)
                   (id? h 'any) (len3? f && lst? <>>f && tfb <>f ?
                                 ("(existsb " + tfb <>f + " " + tl <>>f + ")") 0)
                   !(len3? f) 0
                   (id? h 'id?) (idv <>f <>>f)                        ; id? on literals: the verdict, or 0 (skip)
                   (id? h '=) (? (intv? <>f && intv? <>>f)   ("(Z.eqb " + tv <>f + " " + tv <>>f + ")")
                                 (listv? <>f && listv? <>>f) ("(leqb " + tl <>f + " " + tl <>>f + ")")  ; list/array =
                                 ; gaussian =: numeric within the band, so a zero-im twin equals its charm
                                 ((hasg? <>f || hasg? <>>f) && gz? <>f && gz? <>>f)
                                   ("(geqb " + tg <>f + " " + tg <>>f + ")")
                                 ; lambda =: alpha + structural on the de Bruijn terms
                                 (lamform? <>f && lamform? <>>f)
                                   (: a (lt <>f ()) b (lt <>>f ())
                                      (a && b ? ("(lteqb " + a + " " + b + ")") 0))
                                 ; no eta: a literal closure never = a plain nom (a nif/ap value)
                                 ((lamform? <>f && nom? <>>f) || (nom? <>f && lamform? <>>f)) "false"
                                 ; the unit lanes: a mint rides an OP like a unit but never STANDS as an
                                 ; = side itself (two mints are distinct; only a literal () compares)
                                 (!(mintv? <>f) && !(mintv? <>>f) &&
                                  (hasu? <>f || hasu? <>>f) && uz? <>f && uz? <>>f)
                                   ("(ueq " + tuz <>f + " " + tuz <>>f + ")")          ; the unit rides, scalars
                                 (!(mintv? <>f) && !(mintv? <>>f) &&
                                  (hasu? <>f || hasu? <>>f) && ul? <>f && ul? <>>f)
                                   ("(uleq " + tul <>f + " " + tul <>>f + ")")         ; the unit rides, lists
                                 (txt? <>f && txt? <>>f)
                                   (tband <>f = tband <>>f ?
                                    ("(leqb " + ttl <>f + " " + ttl <>>f + ")") 0)     ; text: same band, same spelling
                                 (litband <>f >= 0 && litband <>>f >= 0 &&
                                  litband <>f != litband <>>f) "false"                 ; cross-band = is false
                                 0)
                   ; gaussian ordering is LEX on (re, im)
                   (ordop? h && (hasg? <>f || hasg? <>>f) && gz? <>f && gz? <>>f)
                     (: a (tg <>f) b (tg <>>f)
                        (h @ '<  ("(gltb " + a + " " + b + ")")
                             '>  ("(gltb " + b + " " + a + ")")
                             '<= ("(negb (gltb " + b + " " + a + "))")
                                 ("(negb (gltb " + a + " " + b + "))")))
                   ; functions carry no strict order: < and > answer nothing
                   ((id? h '< || id? h '>) && lamform? <>f && lamform? <>>f) "false"
                   (xband? f) (xband f)                               ; cross-kind ordering: symbol<string<number<chain<map
                   (ordop? h && lst? <>f && lst? <>>f)                ; same-band chain/string order is LEX
                     (: a (tl <>f) b (tl <>>f)
                        (h @ '<  ("(lexltb " + a + " " + b + ")")
                             '>  ("(lexltb " + b + " " + a + ")")
                             '<= ("(negb (lexltb " + b + " " + a + "))")
                                 ("(negb (lexltb " + a + " " + b + "))")))
                   (ordop? h && (id? <>f () || id? <>>f ()) && uz? <>f && uz? <>>f)
                     (: a ("(unet " + tuz <>f + ")") b ("(unet " + tuz <>>f + ")")
                        (h @ '<  ("(Z.ltb " + a + " " + b + ")")
                             '>  ("(Z.ltb " + b + " " + a + ")")
                             '<= ("(Z.leb " + a + " " + b + ")")
                                 ("(Z.leb " + b + " " + a + ")")))
                   !(intv? <>f && intv? <>>f) 0                       ; the within-band ORDERING ops below stay integer-only
                   (h @ '<  ("(Z.ltb " + tv <>f + " " + tv <>>f + ")")
                        '<= ("(Z.leb " + tv <>f + " " + tv <>>f + ")")
                        '>  ("(Z.ltb " + tv <>>f + " " + tv <>f + ")")
                        '>= ("(Z.leb " + tv <>>f + " " + tv <>f + ")")
                        0))))

   ; the original assert (infix, as in spec.l) shown above each theorem. Coq
   ; comments NEST: a shown `(*)` (the escaped operator) opens one and swallows
   ; the file, so pad the two magic pairs apart.
   (esc s) (: n (tally s)
              ((: (go i acc)
                 (? (i < n)
                    (: c (s i) d (i + 1 < n ? (s (i + 1)) -1)
                       (? ((c = 40 && d = 42) || (c = 42 && d = 41))
                          (go (i + 1) (acc + c + 32))
                          (go (i + 1) (acc + c))))
                    acc)) 0 ""))
   (thm n f s) ("(* " + esc (show f) + " *)\nTheorem gen_" + show n + " : " + s
                + " = true.  Proof. vm_compute. reflexivity. Qed.\n")

   nt {} ns {}                                          ; translated / seen counters
   (bump b) (pin b 0 (1 + peep b 0 0))
   (walkops ops) (? !ops 0
                 (: f <ops _ (bump ns) s (tp (fix1 f))
                    _ (? s (: _ (bump nt) (. (thm (peep nt 0 0) f s)))
                           (say err ("  UNMODELED: " + show f + "\n")))   ; LOUD: an assert the generator can't translate -> never proved
                    (walkops >ops)))
   (walk fs) (? !fs 0
                (: f <fs
                   _ (? (two? f && id? <f 'assert) (walkops >f) 0)
                   (walk >fs)))

   _ (. (join [
     "(* test/proof/rocq/gen.v -- GENERATED from test/spec.l by mk/tools/spec2coq.l. Do not edit;\n"
     "   regenerate with `make test_gen`. Each integer-comparison corpus assert is\n"
     "   translated to Coq, with its source assert shown in the comment above it.\n"
     "   The proofs are `vm_compute. reflexivity.`: every goal is a CLOSED computation,\n"
     "   so vm_compute RUNS both sides (Coq's bytecode VM -- fast even on bignums like\n"
     "   2^64) to a normal form, and reflexivity checks the two results are identical.\n"
     "   A church numeral n applied to x is x ** n (`appZ`); a list/string nets the\n"
     "   sum of its elements/bytes (`asum`). The MODEL IS SHARED: this file imports\n"
     "   test/proof/rocq/spec.v and checks against ITS definitions -- appZ/asum/aprod/amax/vscale/srep\n"
     "   verbatim, iotaZ the Z-arg face, app_appZ proving appZ IS the nat `app` -- so the\n"
     "   generated instances and the hand-written LAWS speak one model. The defs below are\n"
     "   gen-only (no spec.v twin): amin, leqb, the TOTAL vaddt (spec.v's vadd is option-\n"
     "   valued for its shape-mismatch law), vquot, hget, the syntactic kind lattice,\n"
     "   the word shifts (count mod 64), the option-lifted UNIT lane (None rides through\n"
     "   uop/ulop -- () the identity of every dyadic), insertion sort, lex order, the\n"
     "   gaussian twins (exact complex over Z; sign and order LEX on (re, im)), and the\n"
     "   de Bruijn lambda terms (function = is alpha + structural: lteqb). *)\n"
     "From Stdlib Require Import ZArith List.\n"
     "Require Import spec.\n"
     "Import ListNotations.\n"
     "Open Scope Z_scope.\n"
     "Definition amin (l:list Z):Z := match l with nil=>0 | x::xs=>fold_right Z.min x xs end.\n"
     "Fixpoint leqb (a b:list Z):bool := match a,b with nil,nil=>true | x::a',y::b'=>andb (Z.eqb x y) (leqb a' b') | _,_=>false end.\n"
     "Fixpoint vaddz (a b:list Z):list Z := match a,b with x::a',y::b'=>(x+y)::vaddz a' b' | _,_=>nil end.\n"
     ; love adds galaxies ELEMENTWISE only at equal length; a length mismatch answers () (net 0). the
     ; length guard makes the model faithful (a truncating vaddt would net the common prefix -- wrong).
     "Definition vaddt (a b:list Z):list Z := if Nat.eqb (length a) (length b) then vaddz a b else nil.\n"
     "Definition vquot (c:Z):list Z->list Z := map (fun e=>Z.quot e c).\n"
     "Fixpoint hget (m:list (Z*Z)) (k d:Z):Z := match m with nil=>d | p::m' => if Z.eqb k (fst p) then snd p else hget m' k d end.\n"
     ; the word shifts: the count rides mod 64, >> is arithmetic (Z.shiftr on negatives)
     "Definition shlz (a b:Z):Z := Z.shiftl a (Z.modulo b 64).  Definition shrz (a b:Z):Z := Z.shiftr a (Z.modulo b 64).\n"
     ; the UNIT lane: None is (). it rides through every lifted dyadic untouched --
     ; EXCEPT *, where () is the ZERO and the zero annihilates (the associativity law)
     "Definition uop (f:Z->Z->Z) (a b:option Z) : option Z := match a,b with None,x=>x | x,None=>x | Some x, Some y => Some (f x y) end.\n"
     "Definition umul (a b:option Z) : option Z := match a,b with Some x, Some y => Some (x*y) | _,_ => None end.\n"
     "Definition ulmul (a b:option (list Z)) : option (list Z) := None.\n"
     "Definition ueq (a b:option Z) : bool := match a,b with None,None=>true | Some x, Some y => Z.eqb x y | _,_=>false end.\n"
     "Definition unet (a:option Z) : Z := match a with None => 0 | Some z => z end.\n"
     "Definition ulop (a b:option (list Z)) : option (list Z) := match a,b with None,x=>x | x,None=>x | Some x, Some y => Some (x++y) end.\n"
     "Definition uleq (a b:option (list Z)) : bool := match a,b with None,None=>true | Some x, Some y => leqb x y | _,_=>false end.\n"
     ; insertion sort, parameterized by the goes-before test (sortby's comparator)
     "Fixpoint insZ (le:Z->Z->bool) (x:Z) (l:list Z) : list Z := match l with nil => x::nil | y::t => if le x y then x::y::t else y::insZ le x t end.\n"
     "Fixpoint insortby (le:Z->Z->bool) (l:list Z) : list Z := match l with nil => nil | x::t => insZ le x (insortby le t) end.\n"
     "Definition insort := insortby Z.leb.\n"
     ; same-band chain/string order: lexicographic, a proper prefix sorts first
     "Fixpoint lexltb (a b:list Z) : bool := match a,b with nil,nil=>false | nil,_=>true | _,nil=>false | x::a',y::b' => orb (Z.ltb x y) (andb (Z.eqb x y) (lexltb a' b')) end.\n"
     ; gaussian twins: exact complex over Z as (re, im) pairs; the sign (and the order) is LEX
     "Definition gadd (a b:Z*Z):Z*Z := (fst a + fst b, snd a + snd b).\n"
     "Definition gsub (a b:Z*Z):Z*Z := (fst a - fst b, snd a - snd b).\n"
     "Definition gmul (a b:Z*Z):Z*Z := (fst a * fst b - snd a * snd b, fst a * snd b + snd a * fst b).\n"
     "Definition gconj (a:Z*Z):Z*Z := (fst a, - snd a).\n"
     "Definition geqb (a b:Z*Z):bool := andb (Z.eqb (fst a) (fst b)) (Z.eqb (snd a) (snd b)).\n"
     "Definition gltb (a b:Z*Z):bool := orb (Z.ltb (fst a) (fst b)) (andb (Z.eqb (fst a) (fst b)) (Z.ltb (snd a) (snd b))).\n"
     ; the TRUTH GATE is the REAL PART, not the total order -- `gltb` stays
     ; lexicographic because sorting needs totality, but truth cannot depend on
     ; which root of x^2+1 we named `i` (conjugation is a field automorphism, and
     ; C admits no order compatible with its arithmetic). so a pure phase is BLUE.
     "Definition gpos (a:Z*Z):bool := Z.ltb 0 (fst a).\n"
     ; $ of a twin: its REAL PART once the gate passes, else the 0 floor -- the same
     ; part the gate reads, so clamp and gate agree; the magnitude is abs's
     "Definition gsat (a:Z*Z):Z := if gpos a then satZ (fst a) else 0.\n"
     ; de Bruijn lambda terms: love compares functions by ALPHA + structure (no eta/beta/numeral bridge),
     ; which is plain structural equality on the de Bruijn form
     "Inductive ltm := LV (n:nat) | LO (o:Z) | LN (z:Z) | LAp (f a:ltm) | LLam (b:ltm).\n"
     "Fixpoint lteqb (a b:ltm):bool := match a,b with LV n,LV m=>Nat.eqb n m | LO n,LO m=>Z.eqb n m | LN n,LN m=>Z.eqb n m | LAp f x,LAp g y=>andb (lteqb f g) (lteqb x y) | LLam s,LLam t=>lteqb s t | _,_=>false end.\n"
     ; the KIND lattice: a value's tag, for the type predicates (a literal's kind is syntactic; vkind/vband avoid spec.v's value/band)
     "Inductive vkind := Vz | Vflo | Vcx | Vstr | Vsym | Vpair | Vmap | Varr | Vunit | Vbot.\n"
     "Definition fixp  v := match v with Vz   => true | _ => false end.\n"
     "Definition flop  v := match v with Vflo => true | _ => false end.\n"
     "Definition comp  v := match v with Vcx  => true | _ => false end.\n"
     "Definition strp  v := match v with Vstr => true | _ => false end.\n"
     "Definition nomp  v := match v with Vsym => true | _ => false end.\n"
     "Definition namep v := match v with Vsym => true | _ => false end.\n"
     "Definition chainp  v := match v with Vpair=> true | _ => false end.\n"
     "Definition tabp  v := match v with Vmap => true | _ => false end.\n"
     "Definition setp  v := match v with Varr => true | _ => false end.\n"
     "Definition whole   v := match v with Vz   => true | _ => false end.\n"
     "Definition nump  v := match v with Vz|Vflo|Vcx|Varr => true | _ => false end.\n"
     "Definition packp v := match v with Vflo|Vcx|Varr => true | _ => false end.\n"
     "Definition atomp v := match v with Vpair => false | _ => true end.\n"
     "Definition lamp  v := match v with Vmap => true | _ => false end.\n"   ; lit? = the top region (map + the unmodeled fns/hots); among modeled kinds only Vmap qualifies
     "Definition vband v : Z := match v with Vsym => 0 | Vstr => 1 | Vz|Vflo|Vcx|Varr => 2 | Vpair => 3 | Vmap => 4 | Vunit|Vbot => 9 end.\n\n"]))
   _ (say err "spec2coq: UNMODELED asserts (seen but never proved) --\n")
   _ (walk (forms (open "test/spec.l" "r")))
   _ (. ("\n(* " + show (peep nt 0 0) + " theorems generated from " + show (peep ns 0 0) + " asserts seen *)\n"))
   _ (say err $ "spec2coq: " + show (peep nt 0 0) + " translated, "
                + show (peep ns 0 0 - peep nt 0 0) + " UNMODELED, of "
                + show (peep ns 0 0) + " asserts seen\n")
   (quit 0))
