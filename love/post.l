; post -- ONE FILE IN TWO HALVES: `@` (pattern matching) first, then the printer. post
; is written in @, so the macro must be in the book before post's own text compiles --
; being the half above is what puts it there, and a frontend embeds the one header.
; nothing registers a 'pat module: @ is on the book, and no `use` reaches for it.

; pat -- @ : pattern matching, ONE MACRO and no special form. a POST-EGG LAYER,
; not part of the language: prel stays minimal and each frontend includes the
; layers it wants (the host, love0, wasm and the K_TEST kernel do, since love/uu.l
; is written in it; a shipped kernel takes it for the same reason). ⚠ its `use`
; must PRECEDE every reader's compile -- a macro is only in the chain once its
; layer is spliced, and a consumer folds its refs at its own compile.
; the module exports no names: the macro IS the surface, and it rides the splice
; in this layer's macro slot. laws in test/pat.l.
;
; (@ x pat body .. else) reads like `?` with a SHAPE where the test goes: the
; first pattern that fits x runs its body with the pattern's noms bound, an odd
; trailing operand is the else, and no match with no else is the zero point.
; the reader hands `@` over SPACED (test/reader.l); glued it is still the tray
; sigil, so @(1 2) never changed hands and `(@ @(a b) ..)` says both in a breath.
; and it is INFIX at `?`'s own band, n-ary like it: (x @ p b .. else) is the same form,
; scrutinee on the left, where a compound one spends no parens -- (vof en <t @ ..)
; not (@ (vof en <t) ..). love/prel.l's `operators` row; this file sees neither.
;
; A PATTERN ALSO GOES IN A \ PARAM, and that half is opfix's, not this file's: a param
; that is not a nom becomes a fresh one and the body takes it apart, so (\ [a b] x) IS
; (\ v (@ v [a b] x)) and (map (\ [a b] (a * b)) ..) reads the way it should. A
; define-sugar binder is a \ too, at every level. love/prel.l's op-depat / op-dpb.
;
; A PATTERN IS ITS CONSTRUCTOR, READ BACKWARDS: the same text that BUILDS a value
; MATCHES it, so nothing has to be learned twice and a bracket is as closed in a
; pattern as it is in a value. it is also ordinary surface, ALREADY FACTORED --
; opfix runs before macros, so the walk below sees core forms and never re-parses.
; a nom binds, `_` swallows, any other atom matches itself, and a form's HEAD is
; its destructuring tag:
;   (a b c)                a FORM: the shape of a chain, lax on the tail -- so it fits
;                          at three OR LONGER, and ([a b c] + _) is what it says. this
;                          is the UNTAGGED case, the one a head that names no tag falls
;                          to, and it is the common one: ('lam x b) fits (lam x b ..).
;   [a b]  (list a b)      a chain of exactly two        <- a bracket IS its wrap
;   (a >< t)  (link a t)   the cons: head a, tail t. it CHAINS, which is how a
;                          pattern says "and more" -- ('lam >< x >< b >< _) fits
;                          (lam x b ..) at three or longer, ['lam x b] at three
;                          exactly. ⚠ [a b >< t] is (>< (list a b) t) -- the same
;                          datum as ([a b] >< t), so it reads the ONE way both
;                          spellings can: a head that IS the two-chain (a b).
;   ([a b] + t)            a SPLIT: the leading run, then t over what is left.
;                          a pattern is its constructor, so + means here what it
;                          means there -- A + B is the value that A and B make.
;                          ⚠ the LEFT side must be a bracket: its length is what
;                          picks the split point, so nothing is searched for. and
;                          ⚠ it needs its own parens inside @'s operands, like any
;                          infix -- a bare + would capture the whole span each side.
;                          this is how a pattern says "at least": ([a b] + _).
;   'a  (\ a)              the literal a, never a binding
;   {'f x}  (hash 'f x)    a map carrying key 'f -- the KEY is an expression, the
;                          value a pattern, and the match is by SUBSET (the one
;                          place a pattern is looser than its constructor)
;   @(a b)  (tuple a b)    a tray of exactly two
;   (pat & g)              a GUARD, left to right: pat must fit AND g must hold.
;                          THE ONE OPERAND A PATTERN READS AS AN EXPRESSION, and it
;                          chains: (c & 0 < c & c < 10). `&&` is the same tag.
;   (? g pat)              the guard the other way round -- (& ..) lowers to exactly
;                          this, so they are one thing with two spellings
;   (? g)                  the guard alone -- fits anything g holds for
;   (p1 | p2)              ALTERNATIVES: either shape fits. `||` is the same tag
;   (v @ pat)              the AS-pattern, haskell's v@pat, and the ONLY and-pattern:
;                          name the whole AND take it apart, both sides patterns,
;                          neither privileged. ([a _] @ [_ a]) is a pair of equals.
;                          ⚠ SPACED. glued v@pat is one nom -- opfix splits only a
;                          LEADING sigil -- and glued @( is already the tray below.
;   ^x  (jot x)            the PIN, elixir's ^x: x is read from OUTSIDE and the value
;                          must EQUAL it. the one way a pattern spends a nom without
;                          binding it, so (a ^a) fits a pair of equals against an
;                          outer a, where (a a) would fit any pair.
;   ()                     the form at ZERO length: asks nothing, so it fits anything
;   []                     the LIST at zero length: the empty chain, exactly
; ⚠ a map key is read for PRESENCE, not truth: a pinned () matches, an absent
; key does not.
;
; PARENS ARE THE FORM, BRACKETS ARE THE LIST, and the pair is the whole of it:
; [a b c] fits a chain of EXACTLY three, (a b c) one of three or longer. Both read
; as their constructor -- for the paren it is the constructor of the domain pat is
; mostly used on, SYNTAX: uu.l matches terms, a term IS a form, and ('lam x b)
; against (lam x b) is that form written down. lax on the tail because a term wears
; annotations there, which is why every one of those arms used to end `+ _`.
; `+` stays for the tail you want to NAME ((['a b] + t)); parens only drop the `_`.
; this RETIRES the unknown-tag scare, and that is the better half of the change: a
; head naming no tag used to be a rejected program, and we do not reject programs --
; the language is defined so every form can be READ ((f) is f, an empty form is its
; head, an undefined op answers the unit). A tag was the one place a pattern refused
; to mean anything, and now it means the plain thing it looks like.
;
; THE GUARD IS THE ONE PATTERN THAT IS NOT A CONSTRUCTOR -- the one operand read for
; TRUTH where every other is read for SHAPE -- and that is why it needs a tag of its
; own at all. a pattern is its constructor, so pattern text IS expression text: (two? x)
; is a fine untagged pattern (a form of two, binding both) as well as a fine call, and
; nothing but the tag can pick. `&` is that tag, because (c & 0 < c) is the shape a hand
; reaches for; `?` is the same thing written the other way, for a guard that reads better
; in front. THE AND-PATTERN IS `@` AND ONLY `@` -- it had three spellings and needed one,
; so the other two went to the guard, where a wrong reading used to fit nothing in silence.
; g is an ORDINARY EXPRESSION, not a pattern: it is read for truth, it binds
; nothing, and it sees every nom THE WHOLE ARM binds -- `(? (a < b) [a b])` reads
; the pair it is testing. That is because guards do not run where they are written:
; like the non-linear `=` tests, they ride at the END of the conjunction, in source
; order, once the structural tests have earned every accessor. So a guard costs
; nothing on an arm whose shape already failed, and a positional one
; (`[a (? (a > 0) b)]`) is a placement for the READER -- it does not short-circuit
; any earlier. It is the body's own binder wrapped around g, so a nom the arm does
; not bind still resolves outward, in the scope the `@` was written in.
;
; A REPEATED NOM MATCHES NON-LINEARLY: the second sighting is an `=` against the
; first's access form, not a second binding, so [a a] fits a pair of equals and
; nothing else. the equality tests ride at the END of the conjunction, where the
; structural ones have already earned every accessor they read.
;
; the expansion is plain core: ((\ v (? test bound-body .. else)) x), each bound
; body a lambda over the pattern's noms applied to the accessor forms. the
; ARGUMENTS are read in the OUTER scope, so a pattern nom may wear the name of an
; accessor its own test needs -- [cap cup] binds both and still matches. cprop
; beta-reduces the wrapper away when the scrutinee is already a value, and keeps
; it when it is a call -- so the binding is free exactly when it is not needed.
(: gone (mint 0)                 ; absence out of band: no stored value poses as a fresh mint
   (lit d) ['\ d]               ; d, as the code that answers d
   ; a literal is compared with `=` -- EXCEPT a NOM, which is interned, so id? is
   ; the same answer off one pointer where = walks the generic table. ⚠ noms and
   ; nothing else: = on a charm is numeric ACROSS the band (5 matches the gem 5.0),
   ; and id? would quietly narrow that to one representation.
   (weq d v) (? (nom? d) [['id? (lit d) v]] [['= (lit d) v]])
   wnil [() () ()]
   (wts ts) [ts () ()]
   (wgd g) [() () [g]]
   (wcat x y) [(cat <x <y) (cat <>x <>y) (cat <>>x <>>y)]

   ; `&&` IS THE GUARD, LEFT TO RIGHT: (p && g ..) IS (? g .. p) and nothing else, so the
   ; one operand a pattern reads as an EXPRESSION is the one after the `&&`.
   ; ⚠ `&&` is RIGHT-HANDED, so a chain arrives NESTED -- (p && a && b) is (&& p (&& a b))
   ; -- and that inner `&&` is the chain, never a guard on its own. flatten it.
   (gand? x) (? (link? x) (id? <x '&&) ())
   (gchain as) (? (! (two? >as)) as
                  (gand? <>as) (link <as (gchain (cup <>as)))
                  as)
   (gwrap p gs) (? (two? gs) ['? <gs (gwrap p >gs)] p)   ; nested, so `arms` still distributes
   (gnorm p) (: fl (gchain (cup p)) (gwrap <fl >fl))

   ; (pw p v) -- one walk, three answers: the TESTS over the access form v, the
   ; (nom access) pairs its body binds, and the GUARDS, which are neither.
   (pw p v)
     (? (nom? p) (? (= p '_) wnil [() [[p v]] ()])
        (id? p ()) wnil                                      ; () is the form at ZERO: it asks nothing
        (! (link? p)) (wts [['= p v]])
        (: h (opword <p) as >p                     ; opfix's glued heads are VALUES; opword names them
           (? (= h '?) (? (! (two? as)) (wrest p '_ v)        ; a GUARD: (? g1 .. gn pat), or (? g)
                          (! (two? >as)) (wgd <as)           ; ..one operand IS the lone guard
                          (wq as v))                         ; ..else every one but the last guards
              ; THE GUARD, in the spelling a hand reaches for: (c && 0 < c) is (? (0 < c) c).
              (h = '&&) (pw (gnorm p) v)
              ; THE AND-PATTERN, haskell's v@pat and the ONLY spelling of it: both shapes must
              ; fit, and the walks simply CONCATENATE -- one conjunction, no winner to pick,
              ; because both sides read the SAME value and their access forms are already
              ; rooted there. neither side is privileged, so both stay patterns.
              ; ⚠ so a nom in both is the ORDINARY non-linear nom, and dedup makes the second
              ; sighting an equality: ([a _] @ [_ a]) fits a pair of equals, for free.
              ; the everyday one is naming the whole while taking it apart: (v @ ['lam x b]).
              (h = '@) (foldl wcat wnil (map (\ q (pw q v)) as))
              ; the PIN (elixir's ^x, glued to jot by the operators table): NOT a binding but
              ; an equality against x read from OUTSIDE -- the same shape a bare literal takes
              ; above, which is what a pin is. one operand only; anything else is a plain form.
              ((h = 'jot) && (= 1 (tally as))) (wts [['= <as v]])
              ; ALTERNATIVES, anywhere: ONE test, the branches ||ed. each branch owns its own
              ; equalities and its own guards, so a disjunct is that branch entire.
              ; ⚠ a nom lives at a DIFFERENT PLACE in each branch, so it cannot be one access
              ; form -- it is read from whichever branch matched, and () where none binds it.
              ; ⚠ that conditional re-asks the branch tests. A branch that binds nothing adds
              ; no nom and so asks nothing again, which is why the literal alternative -- the
              ; common one -- stays exactly one test with no second look.
              (h = '||)
              (: ws (map (\ q (pw q v)) as)
                 ds (map (\ w (dedup <>w ())) ws)
                 ts (map (\ e (: w <e d <>e
                                  (pand (cat (cat <w <d) (map (\ g (bind <>d g)) <>>w)))))
                         (zip ws ds))
                 bss (map (\ d <>d) ds)
                 uni (foldl (\ u bs (adds u (map cap bs))) () bss)
                 [[('|| >< ts)] (map (\ n [n (pick n bss ts)]) uni) ()])
              (= h '\) (? (two? >as) (wrest p '_ v) (wts (weq <as v)))   ; (\ x b) is a LAMBDA, a form
              (= h 'list) (wlist as v)
              (h = 'link || h = '><) (? (= 2 (tally as))    ; ONE tag: >< IS link, prefix or infix
                                        (wcat (wts [['two? v]])
                                              (wcat (pw <as ['cap v]) (pw <>as ['cup v])))
                                        (wrest p '_ v))
              (= h '+) (? (! (= 2 (tally as))) (wrest p '_ v)   ; a SPLIT: the left run, then the rest
                          (: l <as t <>as
                             (? (? (link? l) (= <l 'list) ()) (wrest >l t v)
                                ; ⚠ `+` is LEFT-handed, so ([a b] + [c] + t) arrives as a + whose
                                ; LEFT is a +. cat is associative and so is the split: re-associate
                                ; right and the ordinary rule reads it. this is the multi-split.
                                (? (link? l) (= <l '+) ()) (pw ['+ <>l ['+ <>>l t]] v)
                                (wrest p '_ v))))
              (= h 'hash) (wcat (wts [['tablet? v]]) (wkv as v))
              (= h 'tuple) (wcat (wts [['tray? v] ['= (tally as) ['tally v]]])
                                 (widx as v 0))
              (wrest p '_ v))))                            ; UNTAGGED: the form's own shape, lax
   (wq as v) (? (two? >as) (wcat (wgd <as) (wq >as v)) (pw <as v))   ; guards, then the shape
   ; a nom's access across an alternative. ⚠ THE FIRST BRANCH THAT MATCHES WINS, and it
   ; supplies EVERY nom -- one it does not bind reads (), it does not fall through to a
   ; later branch. Otherwise one body could wear bindings from two different shapes at
   ; once, and the arm would answer for a value no single branch described.
   (pick n bss ts) (? (! (two? bss)) (lit ())
                      (: e (assoc n <bss)
                         ['? <ts (? (atom? e) (lit ()) <>e) (pick n >bss >ts)]))
   ; ⚠ a pattern longer than its value must never reach a `cap`, so the WHOLE run is
   ; claimed before any of it is read: `two?` on the head, then ONE `long?` for the rest
   ; of the depth. asking `two?` per step instead spends a cup, a load and a dispatch at
   ; each -- a quarter of what matching costs over hand-written accessors, measured.
   ; ⚠ the head's test stays FIRST and separate, and is NOT folded into the long?: it is
   ; what the decision tree hoists, and the tag compare has to be reachable before the
   ; arity walk, or 20 arms of a 21-arm dispatch walk to learn they are the wrong tag.
   ; one walk, TWO ENDS: a bracket ends on (id? () rest) -- exact length -- a split on
   ; the right pattern's own walk, wherever the run stopped.
   (warity k v) (? (k = 1) [['two? v]] [['long? k v]])   ; at one, two? is the same answer and fuses
   (wrun ps v end)
     (? (! (two? ps)) (end v)
        (: n (tally ps)
           (wcat (wcat (wts [['two? v]]) (pw <ps ['cap v]))
                 (? (n = 1) (end ['cup v])
                    (wcat (wts (warity (n - 1) ['cup v])) (wskip >ps ['cup v] end))))))
   (wskip ps v end) (? (two? ps) (wcat (pw <ps ['cap v]) (wskip >ps ['cup v] end))
                       (end v))
   (wlist as v) (wrun as v (\ r (wts [['id? () r]])))
   (wrest ps t v) (wrun ps v (\ r (pw t r)))
   (wkv as v) (? (two? >as)
                 (: g ['peep v <as (lit gone)]
                    (wcat (wcat (wts [['! ['id? (lit gone) g]]]) (pw <>as g))
                          (wkv (cuup as) v)))
                 wnil)
   (widx as v i) (? (two? as) (wcat (pw <as ['peep v i ()]) (widx >as v (i + 1))) wnil)

   ; the non-linear pass: a nom already sighted becomes an `=` against the access
   ; form that first bound it, and drops out of the binder list.
   (dedup bs seen)
     (? (! (two? bs)) wnil
        (: b <bs e (assoc <b seen)
           (wcat (? (atom? e) [() [b] ()] (wts [['= <>e <>b]]))
                 (dedup >bs (? (atom? e) (link b seen) seen)))))

   (pand ts) (? (! (two? ts)) 1  (! (two? >ts)) <ts  ('&& >< ts))

   ; ⚠ THE WRAPPER IS ev'S TO REMOVE, not ours. A nom read once could have its accessor
   ; inlined here, and that is what love/ev.l's read-once refinement already does to the
   ; whole tree: a lone read of an effectful argument substitutes and the call is gone. A
   ; second copy in this macro measured 3.5 M on a 32.5 G corpus -- noise -- so the one
   ; mechanism lives in the compiler, where every caller gets it and not just `@`.
   (bind bs body) (? (two? bs) (link (link '\ (cat (map cap bs) [body])) (map caup bs)) body)

   ; --- THE DECISION TREE -------------------------------------------------------
   ; a clause is [tests body]. compiled FLAT, every clause re-runs the tests it
   ; shares with its neighbours: a 21-arm tag dispatch asks `two?` 21 times where
   ; the hand-written `?` asks once, and that repetition -- not the tag compare, not
   ; the accessor -- is the whole gap (measured 1.91x flat, 1.01x as a tree).
   ; so: FACTOR THE COMMON PREFIX. every clause of a dispatch opens with the same
   ; (two? v), so it is asked ONCE outside the chain and dropped from each clause.
   ; ⚠ the else is then reached two ways and appears TWICE in the source, which is
   ; why this only fires for a SMALL else -- a big one would double the code.
   ; ⚠ and it is one level, never recursive: the duplication stays at exactly 2.
   (same? a b) (? (link? a) (? (link? b) (? (same? <a <b) (same? >a >b) ()) ())
                  (link? b) ()
                  (id? a b))
   ; --- factoring, and its ONE safe shape --------------------------------------
   ; a full decision tree groups each level's clauses by their next test and recurses
   ; both ways -- into the group, and PAST it into the fallthrough. correct, and it
   ; writes the fallthrough TWICE PER LEVEL: with 21 distinct tags that is 2^21 nodes
   ; and the compile never finishes (it did not loop -- it exploded). a real compiler
   ; escapes by making the fallthrough a JOIN POINT; the nearest thing here is a thunk
   ; bound per level, which costs a closure per level per call -- more than the test
   ; it saves. so factoring pays only where the fallthrough does NOT branch:
   ; THE PREFIX EVERY CLAUSE SHARES. failing it fails the whole match, so its
   ; fallthrough IS the else -- one level, one extra copy, no tree to explode.
   ; that is the whole win here anyway: uu's clauses share (two? v) and then differ
   ; by tag, so the tree has exactly one interior node to find.
   (cpx cs)                                         ; the test-prefix common to EVERY clause
     (? (! (two? cs)) ()
        (! (two? <(<cs))) ()                        ; a clause with no tests left: stop
        (: t <<(<cs)
           (? (all (\ c (? (two? <c) (same? <<c t) ())) cs)
              (link t (cpx (map (\ c [>(<c) <>c]) cs)))
              ())))
   (nodes x) (? (link? x) (foldl (\ n e (n + (nodes e))) 1 x) 1)
   (flat cs els) (link '? ((catmap (\ c [(pand <c) <>c]) cs) + [els]))
   ; ⚠ TWO CLAUSES OR MORE, or there is nothing to discriminate: at one clause cpx eats
   ; ALL of its tests, so factoring would read (? p (? 1 body els) els) -- a vacuous test
   ; and the else written twice, for nothing. uu's hot sites are exactly that shape.
   (tree cs els)
     (: p (cpx cs)
        (? (&& (two? >cs) (two? p) (< (nodes els) 40))   ; >=2 clauses, a prefix, a small else
           ['? (pand p) (flat (map (\ c [(drop (tally p) <c) <>c]) cs) els) els]
           (flat cs els)))
   ; --- ALTERNATIVES, and why they split two ways ------------------------------
   ; (p1 || p2) fits when either does -- `|` too, the way `><` and `link` are one tag.
   ; ⚠ A NOM'S ACCESS FORM IS WHERE IT SAT, and it sits somewhere else in every branch,
   ; so one access cannot serve two shapes. Two lanes answer that, and which one runs is
   ; decided by WHERE the alternative is, both obeying first-branch-wins:
   ;   at the top of an arm -> ONE CLAUSE PER BRANCH, sharing the body. A clause is a
   ;     thing there can be more than one of, so each branch keeps its own plain accesses
   ;     and nothing is asked twice. The body is then written once per branch -- the same
   ;     trade `tree` makes for the else, and the reason an alternative is not the cheaper
   ;     spelling of two arms: it is the spelling that says the bodies are THE SAME one.
   ;   nested          -> ONE clause, the branches `||`ed, each nom read from the winner
   ;     (pw above). Costs a re-ask per BOUND nom, and a literal alternative binds none --
   ;     so the common one is exactly one test, at any depth.
   ; ⚠ THE BODY BINDS THE UNION, and a nom this branch did not bind is THE UNIT. That is
   ; the whole of the different-noms question: binding it to () is what stops the body
   ; reaching whatever wears that name OUTSIDE, which is the one way this could answer
   ; something wrong. absence reads as nothing here exactly as it does everywhere else.
   ; the arm, normalised: its BRANCHES, each with the guards standing over it. a guard
   ; wrapping an alternative belongs to every branch, so (? g (p1 || p2)) is two branches
   ; guarded alike -- the guard is written once and asked once per branch.
   (arms p) (? (! (link? p)) [[p ()]]
               (: h <p as >p
                  (? (h = '||) (catmap arms as)
                     (h = '&&) (arms (gnorm p))                 ; the guard, normalised first
                     ((h = '?) && (= 2 (tally as)))
                       (map (\ b [<b (link <as <>b)]) (arms <>as))
                     [[p ()]])))
   (wgds w gs) (foldl (\ x g (wcat x (wgd g))) w gs)
   (adds u ns) (cat u (filter (\ n (! (member? n u))) ns))    ; u, then what it did not have
   _ (:: '@ (\ a
       (: v (nom 0)
          ; the clause a walk answers: its tests, then its non-linear equalities, then its
          ; guards -- and a body bound over `uni`, whatever this branch did not bind reading ().
          (clofu w uni b)
            (: d (dedup <>w ())
               bs (cat <>d (map (\ n [n (lit ())]) (filter (\ n (! (assoc n <>d))) uni)))
               gs (map (\ g (bind bs g)) <>>w)
               [(cat (cat <w <d) gs) (bind bs b)])
          (clof w b) (clofu w () b)
          (arm p b)                                    ; the clauses ONE arm answers for
            (: qs (arms p)
               (? (! (two? >qs)) [(clof (pw p v) b)]   ; one branch: pw takes any guard itself
                  (: ws (map (\ q (wgds (pw <q v) <>q)) qs)
                     (? (all (\ w (! (two? <>w))) ws) [(clof (pw p v) b)]   ; none binds: pw's one test
                        (: uni (foldl (\ u w (adds u (map cap <>(dedup <>w ())))) () ws)
                           (map (\ w (clofu w uni b)) ws))))))
          (cls r) (? (! (two? r)) [() ()]              ; no clause and no else: the zero point
                     (! (two? >r)) [() <r]             ; the odd tail IS the else
                     (: rest (cls (cuup r))
                        [(cat (arm <r <>r) <rest) <>rest]))
          cl (cls >a)
          [['\ v (tree <cl <>cl)] <a])))
   ())

; post -- the third egg stage. prel and ev are sat TWICE (ev compiles itself);
; post is sat ONCE, on the hatchling, after that and before the mop. it is the
; one seat that sees both the finished compiler and the runtime-internal noms
; (peek seek atype, and lamsrc/nifnom below) the mop is about to take off the book -- a
; reference compiled here folds to a value and survives, the way ev's own
; `feels` does. so: love that needs to read the heap raw belongs here.
;
; what lives here is THE PRINTER. it is the reader's mirror image: p0 is a floor
; in C and p1 the real thing in love, and the printer splits the same way -- the
; floor is love.c's facex (the scare face, which runs when the VM has stopped and
; there is nobody left to call), the grammar is all below. `print` `dot` `show`
; `putx` and the `.` sigil are pinned at the foot; prel leaves those four holes
; for us, because a nif folded into prel's own compile could never be replaced.

(: px (:
  ; ⚠ EVERY EAGER BINDING SITS IN THIS OUTER `:`. where boxfix engages (any forward
  ; or self reference -- the printer below is one), a sibling bound to an UNFOLDABLE
  ; CALL is boxed too and its cell never filled, so every read of it raises `missing`:
  ;   (: f (: (a o) (f o) c (tablet 0) (\ o c)))   -> ;; missing c
  cur (peek 0 (+)) ret0 (peek 1 cap) unc (peek 0 (+ 1))
  ; ⚠ THE FLOAT WIDTH IS THE SEAT'S, never this file's: love.c gives a 32-bit seat
  ; (wasm, mps2, teensy41, playdate, rp2040) binary32 for ai_flo_t. so ASK the
  ; arithmetic instead of spelling binary64's numbers -- a hard-coded 2^53 sends
  ; norm's doubling loop past every float32 there is and it never returns.
  p53 (: (go m) (? (m = (1.0 + m)) m (go (m * 2.0))) (go 1.0))   ; where +1 is lost = 2^mantissa
  p52 (p53 / 2.0)
  emin (: (go v e) (: h (v / 2.0) (? (h = 0) e (go h (e - 1)))) (go 1.0 0))   ; the denormal floor
  inf (1.0 / 0.0)
 (:

  ; --- integers: the digits of a non-negative charm/sun/big, most significant first
  (dch n a) (: q (n // 10) d (48 + (n - (q * 10))) (? (0 < q) (dch q (d >< a)) (d >< a)))
  (dstr n) (string (dch n ()))
  (ndig n) (: q (n // 10) (? (0 < q) (1 + (ndig q)) 1))          ; the count, without building the list
  (pnum o n) (? (n < 0) (: _ (put o 45) (say o (dstr (0 - n)))) (say o (dstr n)))

  ; --- text ---
  (hexd d) (? (d < 10) (48 + d) (87 + d))
  ; ⚠ post compiles before any layer is spliced, so `@` has to be in the book already --
  ; it is, from the half of this file above. an unaccompanied `@` folds to a plain
  ; application and a run of charms church-towers into an apcap naming no seat.
  (esc o c)
   (c @ (92 || 34) (put o 92, put o c)
        10 (say o "\\n")   9  (say o "\\t")
        13 (say o "\\r")   27 (say o "\\e")
        0  (say o "\\0")
        (c >= 32 ? (put o c)
                   (say o "\\x", put o (hexd (c // 16)),
                    put o (hexd (c - (16 * (c // 16)))))))
  (pstr o s) (: n (tally s)
                (go i) (? (i < n) (: _ (esc o (peep s i 0)) (go (1 + i))) 0)
                _ (put o 34) _ (go 0) (put o 34))

  ; --- gems: the shortest roundtrip, exact steele & white. C had to carry digit
  ; arrays and its own x2/x5 carry walks; here 4m and the midpoints 4m±2 are
  ; bignums at one shared scale and `//` does the rest.
  ; v = m*2^e exactly, m an integer, normalized into [p52,p53) -- but NEVER past the
  ; denormal floor, where the true spacing stops halving and one more step would hand
  ; the search an interval far too narrow.
  (norm m e) (? (p53 <= m) (norm (m / 2.0) (1 + e))
                (m < p52 && emin < e) (norm (m * 2.0) (e - 1))
                (int m >< e))
  ; inside the rounding interval? a midpoint counts only for an even mantissa --
  ; round-to-nearest-even is what reads it back.
  (fits c lo hi evn) ((lo < c ? 1 (evn && c = lo)) &&
                      (c < hi ? 1 (evn && c = hi)))
  ; the fewest digits whose rounded form sits inside: pk is the place being cut,
  ; walking DOWN by ten a step, and at each one try the truncation and the bump,
  ; the nearer first (a display nicety). at pk = 1 it terminates, dv itself being
  ; inside by construction.
  (pick dv lo hi evn pk) (? (pk < 1) dv
    (: t ((dv // pk) * pk)
       up (1 < pk && 5 <= (dv // (pk // 10) - (10 * (dv // pk))))
       c1 (? up (t + pk) t) c2 (? up t (t + pk))
       (? (fits c1 lo hi evn) c1 (fits c2 lo hi evn) c2 (pick dv lo hi evn (pk // 10)))))
  ; strip trailing zeros, keeping one digit
  (strip n) (: q (n // 10) (0 < q && (n - (q * 10)) = 0 ? (strip q) n))
  (zeros o n) (0 < n ? (put o 48, zeros o (n - 1)) 0)
  (pgem2 o a) (: me (norm a 0) m <me e2 >me sh (e2 - 2)
    up (sh < 0 ? (5 ** -sh) (2 ** sh))   ; one shared scale: 2^sh, or 5^-sh with the point moved
    flo (m = p52 && emin < e2)                ; at a binade floor the gap below is already half
    dv (4 * m * up)
    dlo ((4 * m - (flo ? 1 2)) * up)
    dhi ((4 * m + 2) * up)
    evn (m = 2 * (m // 2))
    dc (pick dv dlo dhi evn (** 10 ((ndig dv) - 1)))
    lead (ndig dc)
    ss (dstr (strip dc)) sig (tally ss)
    dxp (lead + (sh < 0 ? sh 0))
    (? (1e16 <= a || a < 1e-4)
       (: e (dxp - 1)
          _ (put o (peep ss 0 0))
          _ (1 < sig ? (put o 46, say o (snip ss 1 sig)))
          _ (put o 101)
          _ (e < 0 ? (put o 45) 0)
          (say o (dstr (e < 0 ? -e e))))
       (0 < dxp)                                  ; 1 <= v < 1e16: the point sits inside or after
       (: (go i) (? (i < dxp) (: _ (put o (? (i < sig) (peep ss i 0) 48)) (go (1 + i))) 0)
          _ (go 0) _ (put o 46)
          (? (dxp < sig) (say o (snip ss dxp sig)) (put o 48)))   ; integral: force "X.0", never a fixnum face
       (: _ (say o "0.") _ (zeros o (0 - dxp)) (say o ss))))
  (pgem o v) (? !(v <= v) (say o "ieee-nan")
     ; the sign is the BIT, not v < 0: -0.0 < 0 is FALSE, and a comparison there
     ; printed "0.0" -- the one value the roundtrip law lost.
     (: _ (? (v < 0 || (v = 0 && (1.0 / v) < 0)) (put o 45) 0) a (abs v)
        (? (a = inf) (say o "ieee-inf") (a = 0) (say o "0.0") (pgem2 o a))))
  ; a twin prints INFIX, parenthesized: (re ~ im) is self-delimiting, so it reads
  ; straight back wherever the printer put it.
  (ptwin o x) (: _ (put o 40) _ (pgem o (re x)) _ (say o " ~ ") _ (pgem o (im x)) (put o 41))

  ; --- chains. a one-operand `\` is quote, and a `(mono (run datum))` is a GLUED
  ; monadic -- print run+datum back the way opfix fused them.
  (plist o x sn) (: (go l) (: _ (px o <l sn)
                              (? (two? >l) (: _ (put o 32) (go >l)) 0))
                    _ (put o 40) _ (go x) (put o 41))
  (pchain o x sn) (: h <x t >x one (two? t && !(two? >t))
    (? (id? h '\ && one) (: _ (put o 39) (px o <t sn))
       (id? h 'mono && one && (: m <t (two? m && two? >m && !(two? >>m))))
         (: m <t _ (px o <m sn) (px o <>m sn))
       (plist o x sn)))

  ; --- trays. rank 1 is @(…); past that (array '(shape) elem…), since @ has no
  ; shape spelling. an object tray quotes what would otherwise re-evaluate.
  ; a tray is read by COORDINATE, so walk the flat index back into one, row-major
  (coord sh i) (? !(two? >sh) (i >< ())
                  (: r (foldl (*) 1 >sh) q (i // r) (q >< coord >sh (i - (q * r)))))
  (ptray o v sn) (: ty (atype v) sh (shape v) n (foldl (*) 1 sh)
    (pel i) (: e (peep v (coord sh i) 0)
               _ (? (ty = 3 && (nom? e || two? e)) (put o 39) 0) (px o e sn))
    (go i f) (? (i < n) (: _ (? f 0 (put o 32)) _ (pel i) (go (1 + i) 0)) 0)
    (gs l f) (? (two? l) (: _ (? f 0 (put o 32)) _ (pnum o <l) (gs >l 0)) 0)
    (? ((rank v) = 1) (: _ (say o "@(") _ (go 0 1) (put o 41))
       (: _ (say o "(array '(") _ (gs sh 1) _ (put o 41)
          (ge i) (? (i < n) (: _ (put o 32) _ (pel i) (ge (1 + i))) 0)
          _ (ge 0) (put o 41))))

  ; --- maps print {k v …}, the empty one {}; both read straight back. a map is
  ; mutable and can hold itself, so the path is carried and matched by IDENTITY.
  (onpath? x l) (? !(two? l) 0 (id? x <l) 1 (onpath? x >l))
  (pmap o m sn) (? (onpath? m sn) (say o "<cycle>")
    (: ks (keys m) s2 (m >< sn)
       (go l f) (? (two? l) (: _ (? f 0 (put o 32))
                                _ (px o <l s2) _ (put o 32) _ (px o (peep m <l ()) s2)
                                (go >l 0)) 0)
       (? !(two? ks) (say o "{}") (: _ (put o 123) _ (go ks 1) (put o 125)))))

  ; --- coins show as (name 'payload): the arity-1 ctor love/coin.l hands out, applied
  ; to what it was given, so the form EVALS BACK to an equal coin when the name is a
  ; bound ctor. ⚠ the payload is QUOTED where it would otherwise re-evaluate -- a chain
  ; reads as an application and a nom as a variable. without that (q (1 2)) read back as
  ; q applied to 2, which is not the number and was never a coin.
  (pcoin o x sn) (: nm (peep (die-of x) 0 ()) p (load x)
    (put o 40, nil? nm ? (say o "coin") (px o nm sn),
     put o 32, two? p || nom? p ? (put o 39) 0,
     px o p sn, put o 41))

  ; --- closures. a partial's every unc cell holds its arg at [1] and the link at
  ; [2], so the base value sits at link-2.
  (partp v) (id? unc (peek 0 v) || (id? cur (peek 0 v) && id? unc (peek 2 v)))
  (unc0 v) (? (id? cur (peek 0 v)) (seek 2 v) v)
  (nargs v) (: (go u n) (: l (peek 2 u) (? (id? unc (peek 0 l)) (go l (1 + n)) n)) (go (unc0 v) 1))
  (fbase v) (: (go u) (: l (peek 2 u) (? (id? unc (peek 0 l)) (go l) (seek -2 l))) (go (unc0 v)))
  (farg v i n) (: (go u w) (? (0 < w) (go (peek 2 u) (w - 1)) (peek 1 u)) (go (unc0 v) ((n - 1) - i)))
  ; --- a lambda's source, de Bruijn renamed: a \-bound variable prints as its level's
  ; letter, so a-equivalent lambdas print alike and inspect agrees with `=`. free, global
  ; and :-bound names keep their spelling; quoted data is shared verbatim.
  (inq x l) (? !(two? l) 0 (id? x <l) 1 (inq x >l))
  ; the shortlex letters: a b .. z aa ab .. , one-based so the digits carry no zero
  (sxch n a) (: q ((n - 1) // 26) c (97 + ((n - 1) % 26))
              (? (0 < q) (sxch q (c >< a)) (c >< a)))
  (lnom i) (name (string (sxch (1 + i) ())))
  ; a binder takes the next letter not spelled free anywhere in the source -- a name kept
  ; as written is a name a rename would capture, and skipping it is the whole guard.
  ; (\ a b (+ a b)) canonicalizes to itself; a body naming e takes f in e's place. the
  ; candidate index is what threads, so the letters are walked once across the whole term.
  (nxt i fv) (? (peep fv (lnom i) 0) (nxt (1 + i) fv) i)
  (lam? x) (two? x && id? <x '\ && two? >x && two? >>x)
  (qot? x) (two? x && id? <x '\ && two? >x && !(two? >>x))
  ; the noms canon leaves spelled as written: free, global and :-bound. quoted data is
  ; data, never a reference, so a binder cannot capture into it. one walk, into a tablet
  ; the proposals then read by key; bd is the binders in scope, a stack, so it stays a list.
  (fvs x bd t) (? !(two? x) (? (nom? x) (? (inq x bd) t (pin t x 1)) t)
                  (qot? x) t
                  (lam? x) (fvl >x bd t)
                  (fvs >x bd (fvs <x bd t)))
  (fvl l bd t) (? !(two? >l) (fvs <l bd t)
                  (fvl >l (<l >< bd) t))
  (canon x sc i fv) (? !(two? x) (? (nom? x) (: r (assoc x sc) (? r >r x)) x)
                       (qot? x) x
                       (lam? x) ('\ >< cops >x sc i fv)
                       (canon <x sc i fv >< canon >x sc i fv))
  (cops l sc i fv) (? !(two? >l) ((canon <l sc i fv) >< ())          ; the last operand is the body
                      (: j (nxt i fv) nm (lnom j)
                       (nm >< cops >l ((<l >< nm) >< sc) (1 + j) fv)))

  (pfn o v sn) (? (partp v)
    (: n (nargs v)
       (go i) (? (i < n) (: _ (put o 32) _ (px o (farg v i n) sn) (go (1 + i))) 0)
       _ (put o 40) _ (pfn o (fbase v) sn) _ (go 0) (put o 41))
    (: nm (nifnom v)
       (? (string? nm) (say o nm)
          ; a glued sigil's backing value has no nif name but it has exactly ONE word;
          ; say that instead of inlining its whole body wherever `<x` was written.
          (: gn (opnom v)
             (? gn (say o (string gn))
                (: s (lamsrc v)
                   (? (two? s) (px o (canon s 0 0 (fvs s () {})) sn)
                      (: _ (put o 92) (pnum o (abs (dig v)))))))))))   ; opaque: a distinguisher, nothing more -- C spelled the ADDRESS here, this is the digest, which at least survives a collection

  (px o x sn) (? (charm? x) (pnum o x)
                (coin? x) (pcoin o x sn)
                (tablet? x) (pmap o x sn)
                (lit? x) (pfn o x sn)
                (id? x ()) (say o "()")
                (two? x) (pchain o x sn)
                (string? x) (pstr o x)
                (nom? x) (? (mint? x) (: _ (say o "(mint ") _ (pnum o (dig x)) (put o 41))
                              (say o (string x)))
                (gem? x) (pgem o x)
                (tray? x) (ptray o x sn)
                (twin? x) (ptwin o x)
                (sun? x || big? x) (pnum o x)
                o)
  px)))

; the surface. `dot` takes a string or cask VERBATIM -- the `.` sigil's raw face --
; and everything else in external form; it answers its operand, so it drops into
; any sequence. the glued lane -- `operators` at arity 0 -- gets its `.` row here,
; where dot exists; the row is the NOM, so a glued `.x` resolves through whatever
; `.` names at the use site.
(: (print o x) (: _ (px o x ()) x)
   (show x) (: o (jug 0) _ (px o x ()) (slurp o))
   (dot x) (: _ (? (string? x || (hot? x && !(port? x))) (say out x) (px out x ())) _ (flush out) x))
(: putx (print out)
   . dot
   _ (seal-hook 7 show)                            ; `string` coerces through it from here on
   _ (pin (operators 0) '. '.))
