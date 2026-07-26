;;; book is present in prelude
;;; definitions in this file must not use infix notation
;;; FIXME move ev support functions to ev where possible
; prel.l -- love prel: data + function + macro definitions.
; ┌─── PRE-OPERATORS ZONE: here down to the `operators` table (~the `(: operators ...)`
; │    form below) is compiled by the bootstrap c0 BEFORE opfix/the operators table exist,
; │    AND before the church `add`/`mul` hooks are installed (num-ap block). So this zone is
; │    OPERATOR-FREE: no infix, no unquoted sigils, and no `*`-as-compose / `+`-as-cat (the
; │    generic hooks aren't live yet). PREFIX nif calls only -- `(compose f g)`, `(cap (cap
; │    x))`. Using `*` for caap..cuuup here cores the egg (host loops on every eval). ┐
(: id 1
   (const x _) x
   <- pin -> peep
   (co f x y) (f y x)                ; co: the argument-transpose (so `co compose` reads as reverse-compose)
   flip co                           ; flip stays an alias for co (minimize the blast)
   sat saturate)                     ; `saturate` is the canonical name now (the `$` sigil factors to it); `sat` is the short alias
(: macros (peep book 0 0))
(: : () ? () \ ())
(: pi 3.141592653589793
   tau (* 2 pi) ;; no infix here
   e 2.718281828459045                          ; Euler's number; exp is (\ x (x e))
   true 1 false 0
   i ~(0 1) ;; FIXME up top
   (tangent x) (/ (sine x) (cosine x))          ; derived: tan = sin/cos
   star-tray (tray 0) gem-tray (tray 0.0) twin-tray (tray ~(0 0)) top-tray (tray ())  ; the four typed-tray ctors (witness baked in)
   (tier v) (? (tray? v) (: c (atype v) (? (= c 0) star-tray (= c 1) gem-tray (= c 2) twin-tray top-tray)) ())  ; a tray's tier IS its ctor; nil off a tray
   zero? nil?                                  ; zero? aliases nil? (the net-to-zero test)
   (one? x) (= x (cap x))
   (atom? x) (nil? (two? x))
   (names _ ) (keys book)
   ; the scope-layer door (the abyss) + the MODULE REGISTRY (`mods`: g->mods, a lazy singleton
   ; tablet, name -> module-book). TOP is always the defglob target, under it a use-stack, orth
   ; last; every verb takes an operand (love has no nullary fns -- fire with the unit).
   ; enter: push a fresh layer as the head, stashing the pre-enter chain AND the name under the
   ; reserved CHARM key 31337 (a charm can never collide with a name); (enter ()) is anonymous.
   (enter nm) (: F (tablet 0) _ (pin F 31337 (link (books ()) nm)) _ (setbook (link F (books ()))) ())
   ; use: splice a registered module just BELOW the head -- its names resolve on the walk but never
   ; shadow yours. a book value splices directly; a missing name scares 'no-module, since a silent
   ; ()-splice would be a silent no-op.
   (use m)    (: b (? (book? m) m
                      (: r (peep (mods ()) m ()) (? (nil? r) (scare (\ no-module) m) r)))
                 (setbook (link (cap (books ())) (link b (cup (books ()))))))
   ; leave: unwind to the pre-enter chain, REGISTER the layer under its name, answer the book. a
   ; BARE leave (top never entered, so no charm) truncates to the head -- the use-stack clears.
   (leave _)  (: F (cap (books ())) s (peep F 31337 ())
                 (? (two? s)
                    (: _ (pull F 31337 0) _ (setbook (cap s))
                       _ (? (nil? (cup s)) 0 (pin (mods ()) (cup s) F)) F)
                    (setbook (link F ()))))
   ; from: an OPAQUE accessor over the module -- currying reaches a member, 'keys introspects,
   ; (from ()) lists the registry, a missing module answers () (the presence guard). the raw
   ; tablet stays closure-private.
   (from k)   (? (nil? k) (keys (mods ()))
                 (: m (peep (mods ()) k ())
                    (? (nil? m) ()
                       (\ q (? (id? q (\ keys)) (keys m) (peep m q 0))))))
   (caap x) (cap (cap x)) (caup x) (cap (cup x))
   (cuap x) (cup (cap x)) (cuup x) (cup (cup x))
   (caaap x) (cap (cap (cap x))) (caaup x) (cap (cap (cup x)))
   (cauap x) (cap (cup (cap x))) (cauup x) (cap (cup (cup x)))
   (cuaap x) (cup (cap (cap x))) (cuaup x) (cup (cap (cup x)))
   (cuuap x) (cup (cup (cap x))) (cuuup x) (cup (cup (cup x)))
   >< link)                                         ; the infix cons: (a >< b) IS (link a b); glued ><x stays the cup-cap accessor (valence)
; the four element tiers (enum g_vec_type, one word per element): z integers, r reals,
; c packed complex, o objects -- surfaced by the star-/gem-/twin-/top-tray ctors + `tier`.
; the imaginary unit: complex values are written e.g. (+ 2 (* 3 i))
; the bare ~ digraph (~x, ~ not before `(`) reads as (conj x): conj LIFTS now, so
; it IS the monadic twin -- a real r -> ~(r 0), a complex z -> ~(re -im), an
; involution. the 2-operand ~(a b) splices into (twin a b), the C constructor.
; a NOM is a NAMED point -- its own kind (KNom), built by the C `nom` nif: a fresh
; uninterned named atom (a string names it, a symbol lends its spelling, else a bare
; mint). McCarthy's symbol, restored as its own atom -- not the (name . mint) chain.
; integer power (b ** k) is just numeral application: (k b) == b**k. The squaring
; algorithm lives in `ipow`, local to the num-ap block below, so there is no `**`.
(: (gcd a b) (? (= b 0) (? (< a 0) (- 0 a) a) (gcd b (% a b)))
   (modpow b k m) (? (< k 1) 1 (: h (modpow b (// k 2) m) h2 (% (* h h) m)
                                 (? (= 0 (% k 2)) h2 (% (* h2 b) m)))))
(:
   )
; X link; cap/cup -- the chain's two projections, the matched pair the string diagrams bend
; (lineage: cap/cup <- cap/cbp <- car/cdr) -- and the caap..cuuup compounds above are the
; primitives; the older names are gone.
(: (:: a b) (pin macros a b)
   putc (put out)
   puts (say out)
   (putn n) (putbn out n 10)
   putx (print out)
   (getc _) (see in))
; the accessor aliases are plain globals too, so escaping works like (+)/'+ --
; ((->) t k d) peeps.
; fixnum-as-function application, pinned on book for the VM's apply path: applying a
; fixnum n to x is Church-numeral application -- numeric x exponentiates (x ** n), a
; function x composes ($ n) times. n<1 -> 1 (itself the identity numeral: (1 x) == x
; for every x, see below); n=1 -> x directly. numfn assembles the n-fold text
; directly via spin/poke (no `ev`, so applying a fixnum never re-enters the compiler):
; n quote-f's then n ap's -- apl2r's [head..][arg][ap..] shape -- via the interned
; opcode globals (kernel/g.c `insts`). Cell 0 is the source \-expr at value[-1] for
; the printer's fn_src (cf. ev.c c1); entry is cell 1, handed out via (seek 1 th), and
; the src rides the text span so GC traces it. Helpers pass th/f/n explicitly -- a
; closure over the forward-bound th miscompiles under the bootstrap c0.
(: (nfnest f i acc) (? (< i 1) acc (nfnest f (+ -1 i) (link f (link acc ()))))
   (nfq th f i n) (? (< i n) (: _ (poke (+ 1 (* 2 i)) lvm_quote th) _ (poke (+ 2 (* 2 i)) f th) (nfq th f (+ 1 i) n)) th)
   (nfa th i n) (? (< i n) (: _ (poke (+ (+ 3 (* 2 n)) i) lvm_ap th) (nfa th (+ 1 i) n)) th)
   (numfn n f) (:                                  ; build (\ x (f (f ... (f x)))), n f's
     src (link '\ (link 'x (link (nfnest f n 'x) ())))  ; alloc before spin; printer source
     th (spin (+ 5 (* 3 n)))                         ; [src] n*[quote f] [arg n] n*[ap] [ret 1]
     _ (poke 0 src th) _ (nfq th f 0 n)             ; no allocation past spin: th can't move
     _ (poke (+ 1 (* 2 n)) lvm_arg th) _ (poke (+ 2 (* 2 n)) n th)   ; arg n: x, now under n f's
     _ (nfa th 0 n)
     _ (poke (+ 3 (* 3 n)) lvm_ret th) _ (poke (+ 4 (* 3 n)) 1 th)
   (seek 1 th))
   ;; FIXME rename | -> bit-or to free | for pipe operator
   (constellation? x) (| (charm? x) (| (packp x) (big? x)))   ; whole tower: fixnum / float / wide int / complex / array / bignum
   (whole? n) (| (charm? n) (| (sun? n) (big? n)))       ; integer-valued numeral (exact power exponent / composition count)
   ; b ** k by exact squaring (O(log k)); k>=0. The lone home of integer power --
   ; (k b) numeral application routes here, so `**` is unneeded.
   ; the ceiling: a PUBLIC tunable box. a numeral compose count above it raises
   ; (scare 'apcap k) instead of allocating O(k) -- so (bignum f) asks for help
   ; where it would oom. retune at runtime: (pin apcap 0 n).
   apcap (pin (tablet 0) 0 1048576)              ; (a raw box: the # sigil's hash isn't built yet)
   (ipow b k) (? (< k 1) 1 (: h (ipow b (// k 2)) h2 (* h h) (? (= 0 (% k 2)) h2 (* h2 b))))
   ; does the count/exponent k top the apcap box? the cheap gate. each site
   ; keeps its own (scare 'apcap k) as the cond RESULT, so a recovering help's value
   ; (e.g. (capped k)) becomes the result directly -- no thunk, no allocation on the
   ; hot num-ap path; the only duplication is the trivial, identical scare.
   (apover k) (< (peep apcap 0 0) k)
   ; the numeric lane guards the RESULT, not the count: past apover only an exact
   ; base beyond +-1 can grow -- a gem/twin/tray result is size-fixed and |b| <= 1
   ; spins in place, so those run free at any exponent. an exact |b| >= 2 conjures
   ; ~ m*log2|b| bits, and powover reads the box as a WORD budget (64 bits a word:
   ; the stock 2^20 box allows an 8MB number) -- so (2000000 2) runs deliberate
   ; while the slipped tower ((10 9 8) = 8^(9^10)) still scares. all cold: apover
   ; said no on every power under the count cap before log is ever asked.
   (powover b m) (? (apover m)
                    (? (whole? b)
                       (: a (abs b)
                          (? (< a 2) 0
                             (< (* 64 (peep apcap 0 0)) (* m (/ (log a) (log 2))))))
                       0)
                    0)
   ; x ** n across the whole tower. array exponent -> elementwise power (broadcasts
   ; via the vmap2 engine, float result); integer exponent -> exact squaring, with
   ; a NEGATIVE integer giving the reciprocal power (so (-1 x) == (/ 1 x) and
   ; (sqrt x) == ((/ 1 2) x)); otherwise power, which carries a complex lane
   ; (w^z = exp(z Log w)) and a real lane.
   (powg x n) (? (tray? n) (power x n)
                 (whole? n) (: m (abs n)
                             (? (powover x m) (scare 'apcap m)
                                (< n 0) (/ 1 (ipow x m)) (ipow x m)))
                 (power x n))
   ; Church-numeral application generalized across the whole tower. n is the
   ; operator (the numeral), x the operand. numeric operand -> x ** n. non-numeric
   ; operand (a function) -> compose ($ n) times: saturation reduces ANY numeral to
   ; a fixnum nat, true to the !!$ truth bits -- a non-positive numeral acts as the
   ; zero numeral ((-3 f) = (0 f) = 1, the literal lambda construction), a float
   ; ceils, a complex its modulus when it sorts positive, an array its element
   ; sum -- the saturated net. k=0 -> 1 (const 1); k=1 -> x directly -- the snappy basics.
   ; cached compose-combinators: cmpk = (\ f (\ x f^k(x))) -- applying a numeral k to a FUNCTION f is just
   ; (cmpk f), a normal closure CAPTURE of f, no per-call thread assembly (numfn's spin/poke). Built ONCE,
   ; compiled like any lambda; k past the cache falls back to numfn. The hot case (2/3) hits hand-written
   ; `twice`/`thrice` speed. They + num-ap sit in this nested (body-HAVING) `:`, so they are LOCAL -- num-ap
   ; captures them as siblings, they never reach the book, no mop entry. pre-operators: prefix only.
   ; dechurch takes the numeral applications it can SEE -- (3 (+ 1) x) unrolls to nested (+ 1 ..) at
   ; compile time -- so what reaches here is exactly what it can't: a numeral over a runtime function.
   ; that lane wants the cache; a counting iterative applier measured 22% slower at k=2/3 and 45%
   ; slower across k=2..8, since the cached body is k straight-line applications with no loop.
   _ (: cmp2 (\ f (\ x (f (f x))))
        cmp3 (\ f (\ x (f (f (f x)))))
        cmp4 (\ f (\ x (f (f (f (f x))))))
        cmp5 (\ f (\ x (f (f (f (f (f x)))))))
        cmp6 (\ f (\ x (f (f (f (f (f (f x))))))))
        cmp7 (\ f (\ x (f (f (f (f (f (f (f x)))))))))
        cmp8 (\ f (\ x (f (f (f (f (f (f (f (f x))))))))))
        ; FREQUENCY-ORDERED, and that is why it stays a chain: k=2/3 (the hot case) answers in one
        ; or two ~9ns compares where a tablet peep is ~47ns flat in size -- a table pays past k=6.
        (cmp k) (? (= k 2) cmp2 (= k 3) cmp3 (= k 4) cmp4 (= k 5) cmp5 (= k 6) cmp6 (= k 7) cmp7 (= k 8) cmp8 0)
        ; an OPEN star: a coin whose die pins slot 7 is numeric BY DECLARATION, so a
        ; numeral powers it through its own * -- ipow is generic, the exact squaring
        ; rides the die's mul. only whole exponents mean anything off the closed
        ; tower (a negative rides coin division, slot 8 -- unpinned, (); a non-whole
        ; answers (), the undefined-op law). an open star has no size to read, so
        ; this lane keeps the plain COUNT cap where powg refines by result size.
        ; unflagged coins keep composing below.
        (starred? x) (? (coin? x) (= 1 (-> (die-of x) 7 0)) 0)
        (num-ap n x) (? (constellation? x) (powg x n)
                        (starred? x)
                        (? (whole? n) (: m (abs n)
                                         (? (apover m) (scare 'apcap m)
                                            (< n 0) (/ 1 (ipow x m)) (ipow x m)))
                           ())
                        (: k (sat n)
                           (? (< k 1) 1 (= k 1) x
                              (apover k) (scare 'apcap k)
                              (: c (cmp k) (? c (c x) (numfn k x))))))
        (pin book 'num-ap num-ap)))
; the function combinators for + and * (README's Church numeral arithmetic, so +/* on
; numerals agree with the numbers) are `stack` and `compose` -- immortal constant threads
; in C (a text operand takes precedence over every other type, so +/* of a function build a
; function, and lvm_addh/lvm_mulh build the 2-arg partial (stack f g) / (compose f g)):
;   (+ f g) = Church add   ((+ f g) a x) = (f a (g a x))  -- stack; its extra a-slot threaded
;             into both operands is what makes + agree with addition, not (f x (g x)).
;   (* f g) = composition  ((* f g) x)   = (f (g x))      -- compose.
; cache num-ap into the VM (g->hot_numap) now that it is pinned, so the hot apply paths
; (lvm_numap/numtap, data_num_apply) read it directly instead of interning + book-probing per
; call. MUST follow num-ap's pin above; the prel runs in every warm, so every runtime gets sealed.
(seal-hooks ())
(: (map f l) (? (two? l) (link (f (cap l)) (map f (cup l))))
   (foldl f z l) (? (two? l) (foldl f (f z (cap l)) (cup l)) z)
   (foldr f z l) (? (two? l) (f (cap l) (foldr f z (cup l))) z))
; the monadic words behind the glued sigils (the valence law; see opfix and
; book['monadics] below). net is the CONTENT MEASURE as a value -- + turned
; inward: a number is its own net (phase intact, exact), an array sums its
; cells, a list the nets of its spine. a coin: a DECLARED star (slot 7) nets
; ITSELF -- the self-netting fixpoint by declaration, so star? opens with no
; edit and additivity lands exactly in the die's own ring (net (a + b) IS
; (net a) + (net b), the measure-hom) -- else its tally under net-mode 1
; (slot 6, lockstep with the C truth path), else its payload's net SIGNED.
; everything else is non-negative so its net IS its sat. prod is the product
; twin; negate/reciprocal/fraction the dyadics turned inward; bit the truth
; bit (the Iverson bracket, !!).
(: (net x) (? (tray? x) (asum x)
              (constellation? x) x
              (two? x) (foldl (\ a e (+ a (net e))) 0 x)
              (coin? x) (? (= 1 (-> (die-of x) 7 0)) x
                           (= 1 (-> (die-of x) 6 0)) (tally x)
                           (net (load x)))
              (sat x))
   (prod x) (? (tray? x) (aprod x)
               (two? x) (foldl (\ a e (* a (prod e))) 1 x)
               x)
   negate (- 0)
   reciprocal (/ 1)
   (fraction x) (% x 1)
   (bit x) (nil? (nil? x))
   ; a STAR is a self-netting SCALAR (charm/wide/float/complex/big) -- the fixpoint of
   ; net (x IS its own net; id?, not `=`, since net returns a number operand unchanged
   ; while a set's net is a fresh sum). a SET (set?) is an array of any element kind;
   ; a GALAXY is a set of STARS -- a numeric set. star? and galaxy? are DISJOINT: a set
   ; never self-nets (its net is a fresh scalar), so a galaxy is never a star -- it is
   ; COMPOSED of stars, not a kind of star. galaxy? keys on the CELLS, not the O code, so a
   ; Z/R/C set qualifies outright and an object-typed set of star cells (e.g. bignums)
   ; still counts -- a set of strings does not (a string is no star).
   ; (the fixpoint is OPEN: a coin whose die pins slot 7 nets itself, so it
   ; passes with no coin branch here -- declaration arrives through net.)
   (star? x) (id? x (net x))
   ; (charm? -- a fixnum, the word-sized star -- is the C nif now, oddp C-side.)
   (galaxy? x) (? (tray? x) (? (nil? (= top-tray (tier x))) 1 (star? (peep x 0 0))) 0))
(: (foldl1 f l) (foldl f (cap l) (cup l))
   (foldr1 f l) (foldr f (last l) (init l))
   ap (foldl id)
   (filter p l) (? (two? l) (: m (filter p (cup l)) (? (p (cap l)) (link (cap l) m) m)))
   (init l) (? (two? (cup l)) (link (cap l) (init (cup l))))
   (last l) (? (two? (cup l)) (last (cup l)) (cap l))
   (each l f) (? (two? l) (: _ (f (cap l)) (each (cup l) f)))
   (ldel x l) (? (two? l) (? (= (cap l) x) (cup l) (link (cap l) (ldel x (cup l)))))
   (all f l) (? (two? l) (? (f (cap l)) (all f (cup l)) 0) 1)   ; truth BITS (0/1, like =/</!), not ()
   (any f l) (? (two? l) (? (f (cap l)) 1 (any f (cup l))) 0)   ; -- a predicate's false is 0, the green floor
   ; monofix: the shared monotone-fixpoint driver. Iterate `step` on the state until the `size` measure
   ; stops changing -- one engine for every ascending/descending chain (the compiler's interprocedural
   ; analyses: the glaze's smfix/ho-fix/gfix, the interpreter's closure-capture propagate). `size` must be
   ; strictly monotone under `step` until the fixpoint, so equality == convergence (it ascends for a
   ; growing fact-map, descends for a shrinking candidate set). The measure is snapshot BEFORE the step,
   ; so a step that mutates the state IN PLACE (and returns it) is detected by the pre/post size delta.
   (monofix st step size) (: b (size st) nx (step st) (? (= (size nx) b) nx (monofix nx step size)))

   ; kconst: the one constant-form recognizer -- a present (1 . value) iff e is a constant form (a
   ; non-symbol atom or a (\ v) quote of one), else () (no fact, NOT a number -- absence is the unit).
   ; The compiler's constant queries are projections of it across scopes that can't otherwise share
   ; (feels' cval, cprop's propk?, wxc's cst) -- one fact, many readers. (No `&&`: it is a macro defined
   ; later in prel, and a single-pass bootstrap can't expand a forward macro.) It could live in ev.l
   ; instead -- all three readers are there and nothing else in the corpus touches it -- unlike
   ; monofix above, which the glaze reads bare from its own post-egg compile.
   (kconst e) (? (nom? e) ()                                                    ; a variable -> no fact
                 (atom? e) (link 1 e)                                           ; a literal -> (1 . itself)
                 (id? (cap e) '\) (? (atom? (cuup e)) (link 1 (caup e)) ())     ; (\ v) quote of an atom -> (1 . v); a real lambda -> no fact
                 ())
   (cat a b) (foldr link b a))
; sort: (sort l) orders l ascending by the total order -- the C lane (a stable
; bottom-up merge in the heap gap, one cmp3 per compare, no lisp re-entry);
; descending is (rev (sort l)). an arbitrary strict less-than predicate takes
; the lisp merge sort: (sortby le l), top-down alternating split + merge.
(: (merge le a b) (? (atom? a) b (atom? b) a
                     (le (cap a) (cap b)) (link (cap a) (merge le (cup a) b))
                     (link (cap b) (merge le a (cup b))))
   (sortsplit l) (? (atom? l) (link l ()) (atom? (cup l)) (link l ())
                    (: r (sortsplit (cuup l)) (link (link (cap l) (cap r)) (link (caup l) (cup r)))))
   (sortby le l) (? (atom? l) l (atom? (cup l)) l
                  (: s (sortsplit l) (merge le (sortby le (cap s)) (sortby le (cup s))))))
(: catmap (compose (flip foldr ()) (compose cat))
   (assoc x l) (? (two? l) (? (= x (caap l)) (cap l) (assoc x (cup l))))
   (lidx x) ((: (f n l) (? (two? l) (? (= x (cap l)) n (f (+ 1 n) (cup l))) -1)) 0)
   (member? x l) (? (two? l) (? (= x (cap l)) 1 (member? x (cup l))) 0)   ; any of (= x); first-order so the glaze's point-= grammar closes it (auto.l re-declares this exact body)
   (zip a b) (? (two? a) (? (two? b) (link (link (cap a) (cap b)) (zip (cup a) (cup b)))))
   rev (foldl (flip link) ())
   (jot x) ((: (go i) (? (< i x) (link i (go (+ i 1))))) 0)   ; the range 0..x-1 (iota its array twin); a section of tally, off the `*` glyph -- monadic `*` is prod now
   (drop n l) (? n (drop (- n 1) (cup l)) l)
   (take n l) (? n (? (two? l) (link (cap l) (take (- n 1) (cup l)))))   ; stop at end-of-list (else it pads past it)
   (part p) (foldr (\ a m (? (p a) (link (link a (cap m)) (cup m))
                                        (link (cap m) (link a (cup m))))) '(())))
(: (tap cl)
 (poke -1 (peek 0 in) (poke -1 -4 (poke -1 -1 (poke -1 0 (poke 4 cl (spin 5))))))
 (jug _)                                          ; the output dual of `tap`: a memory-backed output PORT -- say INTO it, slurp it back OUT (you pour both ways, like a jug). NOT a cask (a cask is a byte ARRAY)
  (poke -1 (peek 0 in) (poke -1 -2 (poke -1 -1 (poke -1 0 (poke -1 "  " (poke 5 0 (spin 6)))))))
 (sip o) (see o)                                  ; draw ONE unit (pairs with slurp = all)
 ; slurp drains a vessel to a value: an output jug reads its backing buffer; a tap drains via see.
 (slurp o) (? (= -2 (peek 1 o)) (snip (peek 4 o) 0 (peek 5 o))   ; a jug BY ITS FD (-2): a buffered fd port's word 4 is a string too (its read backing), so the old string? probe lies
              (: (rl i) (: c (see i) (? (= c -1) () (link c (rl i)))) (string (rl o))))  ; (pre-opfix: no ! here)
 (show x) (: o (jug 0) _ (print o x) (slurp o)))
; help status readers. a global `help` function is called as (help s a b):
; s = the status word, two bits -- scare (1: something went wrong) and more
; (2: read control flow, more input wanted). more alone = incomplete; eof =
; more|scare (the end is the scary case of wanting more). a/b = the condition
; data (the more bit: the port and the read sentinel). the result is delivered
; per the bits: more -> to the reader's resume; a bare scare -> observed.
(: (scare? s) (= s 1)
   (more? s) (= s 2)
   (eof? s) (= s 3))
; here are some macro definitions
(: l (foldr (\ a l (link link (link a (link l ())))) ()) (: _ (:: 'L l) _ (:: 'list l)))
; the false answer is the UNIT, never a 0-fossil: (&& a ..) expands (? a .. ())
; and ||'s exhausted tail reads () -- both blue, but the zero point is nothing,
; not a number (a && result must ride ? and id? against (), not arithmetic)
(:: '&& (\ l (: (and l) (? (two? (cup l)) (list '? (cap l) (and (cup l)) ()) (cap l)) (? (two? l) (and l) 1))))
(:: '|| (\ l (: (or l) (? (two? l) (: y (nom 0) (list ': y (cap l) (list '? y y (or (cup l))))) ()) (or l))))
(:: ':- (\ a (link ': (cat (cup a) (link (cap a) ())))))
(:: '?- (\ a (link '? (cat (cup a) (link (cap a) ())))))
(:: '>>= (\ l (link (last l) (init l))))
(:: '<=< (\ g (: y (nom 0) (list '\ y (foldr (\ f x (list f x)) y g)))))

; --- reader operators: the table + the opfix factor pass --------------------
; a SIGIL is a maximal all-punct token (the reader's lexer law); the reader
; leaves it a plain symbol. code factors a sigil into a composition of
; operators against THIS table at compile time -- opfix below, the
; source->source pass hooked by both compilers (c0 probes book['opfix] like
; boxfix; ev.l's feel runs it first) -- while data reading stays structural.
; ONE table: symbol -> arity (the symbol names itself) | (name . arity) (an
; alias). arity ONE applies to the next datum, never capturing a left
; operand. arities two..seven are infix: with a left operand available it
; captures it and collects arity-1 more, nesting RIGHT-associatively; with
; none it reads as the plain symbol ((+) is + as a value, '+ quotes it).
; the DEFAULT is infix at TWO: ANY all-punct symbol absent from the table
; still reads dyadic, so the table holds the EXCEPTIONS -- the arity-one
; sigils, the cond ?, the aliases -- PLUS the dyadic words a COMPOSITE run
; factors against (!= -> ! =, op-long reads them here). a binding form is
; exempt: opfix keeps a `:`/`\` binder name verbatim, so a fresh punct
; operator is define-then-use with no pin -- the default needs no table entry.
; both tables go BOOK-PRIVATE at birth (the egg mops them: a stray pin into a
; compile table would poison every later compile); a new arity or alias waits
; for the scope-layer door. everything above this point is pre-opfix and
; must stay operator-free (the bootstrap restriction: no unquoted sigil
; before its definitions); everything after -- and every later file, the
; corpus, the repl -- reads hot.
; └─── END PRE-OPERATORS ZONE: infix + operators are live from the `operators` table on. ┘
; FOLD `pin` over the (key value) pairs (pin gives back its map). Open-coded as a fold, NOT the
; #(..)/map macro: this is the PRE-OPFIX zone, compiled by the C bootstrap c0, which doesn't run the
; self-hosted maps pass -- but foldl/L are already defined above and are operator-free.
; a value carries arity AND a GRIP (its precedence -- higher binds tighter, op-ent normalizes to the
; triple (name arity . grip)): a bare charm is arity at the house grip; (arity . grip) self-names with
; an explicit grip; (name . arity) aliases at house; (name arity . grip) aliases with an explicit grip.
; the bands, coarse with gaps to slot more (doc/precedence.md): apply 70 (** flip-apply, tightest),
; multiplicative 60, additive 50, comparison 40, logical 30, house 27 (the coined-operator default,
; set at op-w's fresh-punct fallback), cons builder 25, assignment 20, cond 10, weak-apply 5 ($ the
; haskell-like weak application, loosest). an incoming operator STEALS a pending filled
; operand iff its grip >= the frame's grip (op-steal), so equal grips stay right-associative (the
; conservative-extension case, today's flat read) and a tighter operator climbs in.
(: operators
   (foldl (\ t p (pin t (cap p) (cup p))) (tablet 0)
      (L (link '! (link 'nil? 1)) (link '. (link 'dot 1))                         ; arity-1 sigils: grip inert (prefix folds on fill)
         (link '** (link 2 70))                                                   ; apply -- flip-apply, tightest infix (a ** b = (b a)); glued a**b unaffected (not in monadics)
         (link '* (link 2 60)) (link '/ (link 2 60)) (link '% (link 2 60))        ; multiplicative
         (link '+ (link 2 50)) (link '- (link 2 50))                              ; additive
         (link '= (link 2 40)) (link '< (link 2 40)) (link '<= (link 2 40)) (link '> (link 2 40)) (link '>= (link 2 40)) ; comparison
         (link '| (link 2 30)) (link '& (link 2 30)) (link '&& (link 2 30)) (link '|| (link 2 30)) ; logical (&& || also macros; the row pins the grip below comparison)
         (link '>< (link 2 25))                                                   ; cons -- the loosest builder
         (link '<- (link 'pin (link 3 20))) (link '-> (link 'peep (link 3 20)))   ; assignment aliases
         (link '? (link 3 10))                                                    ; cond
         (link '$ (link 2 5)))))                                                  ; weak apply -- loosest (a $ b = (a b), haskell $); glued $x stays saturate (monadics)
; the VALENCE LAW's table: a glued operator run is MONADIC (the reader emits
; (mono (run datum))); spaced, the run keeps its dyadic/infix life. each entry
; maps a run-piece to its monadic word; a run factors greedily (longest
; prefix), all factors monadic, so <>x is (cap (cup x)) and !!x double-
; negates. << and >> ride the shift tokens' free monadic slots. an
; unfactorable run falls back to the plain application.
(: monadics
   (foldl (\ t p (pin t (cap p) (caup p))) (tablet 0)
   ; << and >> earn their rows by SPEED, not by reach: without them the greedy factor splits >>x
   ; into > then >, which composes to the same cuup -- the entry makes it one call instead of two.
      `('(< cap) '(> cup) '(<< caap) '(>> cuup) '(+ net) '(* prod)
        '(| abs) '(- negate) '(/ reciprocal) '(% fraction) '(? bit)
        '($ saturate)
        '(! nil?) '(. dot))))
; the FACTORIZATION LAW: greedy longest-prefix match against the table, no
; backtracking; every factor but the last must be arity one; the last factor
; takes the positional valence. a token that doesn't factor is a plain
; symbol. so != factors to ! and = ((a != b) = (nil? (= a b))) while &&,
; >>=, :-, ?- (two-led or unmatched) stay atomic names.
; everything from here down to the SECOND (seal-hooks) is the opfix machinery itself, so it stays
; PREFIX: the first bootstrap pass compiles prel with no opfix on the book (it is pinned and sealed
; at the foot of this block), so an infix form in here reads as a raw application and love0's lcat
; dies. past that seal the new style is free.
; the four ranges are DISJOINT -- 97..122 lower, 48..57 digits, 65..90 upper, 95 underscore -- so no
; test implies another, and they run COMMONEST-FIRST: op-puncp bails on a symbol's first byte, and
; that byte is usually a lowercase letter.
(: (op-punc-byte c) (? (&& (<= 97 c) (<= c 122)) 0
                       (&& (<= 48 c) (<= c 57)) 0
                       (&& (<= 65 c) (<= c 90)) 0
                       (= c 95) 0
                       1)
   ; cheap prefilter: only an all-punct symbol can factor
   (op-puncp x)
    (? (nom? x)
       (: s (string x)
          (? (string? s)
             (: n (tally s)
                (? (< 0 n)
                   ((: (go i) (? (< i n) (? (op-punc-byte (s i)) (go (+ 1 i)) 0) 1)) 0)
                   0))
             0))
       0)
   ; normalize a table entry for key k -> the triple (name arity . grip), or 0. four value shapes:
   ; a bare charm (arity, self-named, house grip 27); (arity . grip) (self-named, explicit grip);
   ; (name . arity) (alias, house grip); (name arity . grip) (alias, explicit grip). the (arity . grip)
   ; vs (name . arity) fork is by (cap v)'s kind (a charm arity vs a nom name).
   (op-ent k) (: v (operators k)
                 (? (charm? v) (? (&& (< 0 v) (< v 8)) (link k (link v 27)) 0)
                    (two? v)
                     (? (charm? (cap v))                                     ; (arity . grip): self-named
                          (? (&& (< 0 (cap v)) (< (cap v) 8) (charm? (cup v))) (link k v) 0)
                        (charm? (cup v))                                     ; (name . arity): alias, house grip
                          (? (&& (nom? (cap v)) (< 0 (cup v)) (< (cup v) 8)) (link (cap v) (link (cup v) 27)) 0)
                        (? (&& (nom? (cap v)) (charm? (caup v)) (< 0 (caup v)) (< (caup v) 8) (charm? (cuup v))) v 0)) ; (name arity . grip)
                    0))
   ; longest prefix of nm[i..n) that is a table key -> (len . entry), or 0
   (op-long nm i n)
    ((: (go k) (? (< k 1) 0
                  (: e (op-ent (intern (snip nm i (+ i k))))
                     (? (two? e) (link k e) (go (- k 1))))))
     (- n i))
   ; factor symbol s -> a list of (name . arity) entries, else nothing. every no-match falls off the
   ; end of its `?` to the UNIT: the one consumer (op-w, below) reads the answer with two?, so an
   ; omitted else says "does not factor" without leaving a 0-fossil in a do-nothing slot.
   (opfactor s)
    (: nm (string s) n (tally nm)
       ((: (go i)
           (? (< i n)
              (: m (op-long nm i n)
                 (? (two? m) (: k (cap m) e (cup m)
                         (? (= (+ i k) n) (link e ())        ; the last factor: any arity
                            (= (caup e) 1) (: r (go (+ i k)) ; non-last factors must be arity one
                                             (? (two? r) (link e r)))))))))
        0))
   ; longest prefix of nm[i..n) that has a monadic meaning -> (len . word),
   ; or 0. monadics first, then an operators entry at arity ONE (the legacy
   ; prefix lane -- $ ! . ride here).
   (mono-long nm i n)
    ((: (go k) (? (< k 1) 0
                  (: s (intern (snip nm i (+ i k)))
                     e (monadics s)
                     (? (nom? e) (link k e)
                        (: o (op-ent s)
                           (? (? (two? o) (= 1 (caup o)) 0) (link k (cap o))
                              (go (- k 1))))))))
     (- n i))
   ; factor a glued run against the monadic table -> the word list
   ; (outermost first), or 0; every factor is monadic by construction
   (monofactor s)
    (: nm (string s) n (tally nm)
       ((: (go i)
           (? (< i n)
              (: m (mono-long nm i n)
                 (? (two? m)
                    (? (= (+ i (cap m)) n) (link (caup m) ())
                       (: r (go (+ i (cap m))) (? (two? r) (link (caup m) r) 0)))
                    0))
              0))
        0)))
; the walk. a pending-operator frame is (orig chain name need grip . got): orig =
; the source symbol (delivered back verbatim when no operand ever arrives --
; a bare sigil is a plain symbol), chain = the arity-one wrapper names
; (outermost first), need = operands still wanted (0 = FILLED, fold deferred
; so a following infix operator can steal the last operand -- that deferral
; IS right-associativity), grip = the operator's precedence (captured from the
; factor entry, so an alias `<-` or composite `!=` carries the operative level
; neither of its two symbols alone recovers), got = operands so far, reversed.
; out/pend thread as one (out . pend) chain through the helpers.
(: (op-fr o ch nm nd g gt) (link o (link ch (link nm (link nd (link g gt)))))
   (op-fro f) (cap f) (op-frc f) (caup f) (op-frn f) (cauup f)
   (op-frd f) (cap (cuuup f)) (op-frgrip f) (cap (cup (cuuup f))) (op-frg f) (cup (cup (cuuup f)))
   ; a frame is only ever rebuilt with a new need + got -- orig, chain, name and grip are fixed
   ; the moment the operator is read -- so the walk says which two slots move and nothing else.
   (op-frw f nd g) (op-fr (op-fro f) (op-frc f) (op-frn f) nd (op-frgrip f) g)
   ; wrap the chain of arity-one names around a folded form
   (op-fin ch x) (? (two? ch) (link (cap ch) (link (op-fin (cup ch) x) ())) x)
   ; fold a collected span (out, reversed) into ONE operand: a lone datum stays
   ; itself, two-or-more become the application (juxtaposition binds tighter than
   ; any infix operator, so a whole run to an operator's side is one operand).
   (op-span out) (? (two? (cup out)) (rev out) (cap out))
   ; fold a frame with whatever it has: nothing collected -> the plain
   ; symbol; a shortfall folds to the partial application (curry law). a
   ; frame at arity ONE (nil name: the whole op lives in the chain) wraps its
   ; lone operand.
   ; KIND tests throughout the walk, never truth: a collected operand list
   ; like (0) or ("") NETS zero, so nil? would read it as empty and drop
   ; the operand (the round-16 discipline: kinds for structure)
   (op-fold f) (? (atom? (op-frg f)) (op-fro f)
                  (nom? (op-frn f)) (op-fin (op-frc f) (link (op-frn f) (rev (op-frg f))))
                  (op-fin (op-frc f) (cap (op-frg f))))
   ; deliver datum d -> (out' . pend'). a collecting frame takes it; a FILLED
   ; infix frame EXTENDS its right operand into an application span, because
   ; juxtaposition binds tighter than any infix operator: (a = b c) is
   ; (= a (b c)), not ((= a b) c). the span state rides `need` -- 0 = the last
   ; operand is still a lone datum (the first extra datum wraps it into a
   ; 2-application), -1 = already a growing span (append). a frame at one that
   ; fills folds AT ONCE -- ones bind tightest ($"ab" + 2 is (+ ($ "ab") 2));
   ; only infix frames sit FILLED, deferring their fold so a following operator
   ; can steal the last operand (that deferral IS right-associativity).
   (op-del d out pend)
    (? (atom? pend) (link (link d out) pend)
       (: f (cap pend)
          (? (< 0 (op-frd f))
             (: f2 (op-frw f (- (op-frd f) 1) (link d (op-frg f)))
                ; (nil? ..), not !: this block is pre-opfix (the bootstrap restriction)
                (? (&& (= 0 (op-frd f2)) (nil? (nom? (op-frn f2))))
                   (op-del (op-fold f2) out (cup pend))
                   (link out (link f2 (cup pend)))))
             (: op (cap (op-frg f))                          ; the frame's last (right) operand
                nx (? (= 0 (op-frd f)) (link op (link d ())) ; lone -> wrap into (op d)
                      (cat op (link d ())))                  ; span -> append d
                f2 (op-frw f -1 (link nx (cup (op-frg f))))
                (link out (link f2 (cup pend)))))))
   ; take a left operand for an incoming operator of grip g -> `(. (out' . pend')), or 0.
   ; a FILLED top frame: g >= its grip re-arms (need 1) and yields its last operand (the C reader's
   ; A(F)=1 move -- today's steal; equal grip steals, so same-level stays right-associative); g < its
   ; grip means the frame binds tighter, so FOLD it and retry beneath (the precedence climb, reusing
   ; op-del/op-fold, no new folding machinery). at top level the WHOLE leading span is taken (op-span):
   ; application binds tighter than infix, so (a b = c) captures (a b), not just b. a filled frame is
   ; need <= 0 (0 = lone operand, -1 = a span the right operand grew into) -- either can be stolen.
   (op-steal g out pend)
    (? (two? pend)
       (: f (cap pend)
          (? (< (op-frd f) 1)
             (? (< g (op-frgrip f))
                (: r (op-del (op-fold f) out (cup pend)) (op-steal g (cap r) (cup r)))
                (link (cap (op-frg f))
                      (link out (link (op-frw f 1 (cup (op-frg f))) (cup pend)))))
             0))
       (two? out) (link (op-span out) (link () pend))
       0)
   ; drain at end of list: fold every pending frame innermost-out
   (op-drain out pend)
    (? (atom? pend) (rev out)
       (: r (op-del (op-fold (cap pend)) out (cup pend))
          (op-drain (cap r) (cup r))))
   (op-w l out pend)
    (? (atom? l) (op-drain out pend)
       (: e (cap l)
          (? (&& (nom? e) (op-puncp e))
             ; an all-punct symbol factors against the table; whatever does NOT
             ; factor DEFAULTS to infix at two -- a fresh punct symbol acts dyadic
             ; with no table pin, the table now holding only the EXCEPTIONS (arity
             ; =/= 2, the aliases, the arity-one sigils). with no left operand the
             ; infix frame still folds to the plain symbol (op-fold's bare case).
             (: fs0 (opfactor e)
                fs (? (two? fs0) fs0 (link (link e (link 2 27)) ()))   ; fresh punct: the (e 2 . house) triple
                ch (map cap (init fs)) en (last fs) nm (cap en) n (caup en)
                (? (= n 1)                       ; arity one (or a one-chain): prefix --
                   ; nil name, the op rides the chain: marks the frame to fold on fill (grip inert)
                   (op-w (cup l) out (link (op-fr e (cat ch (link nm ())) 0 1 27 ()) pend))
                   (: st (op-steal (cuup en) out pend)  ; infix: capture if a left operand exists (of grip (cuup en))
                      (? (two? st) (op-w (cup l) (cap (cup st))
                                  (link (op-fr e ch nm (- n 1) (cuup en) (link (cap st) ()))
                                        (cup (cup st))))
                         (two? ch)               ; composite with no left: prefix
                         (op-w (cup l) out (link (op-fr e ch nm 1 (cuup en) ()) pend))
                         (: r (op-del e out pend) ; bare operator: the plain symbol
                            (op-w (cup l) (cap r) (cup r)))))))
             (: r (op-del (op-core e) out pend)
                (op-w (cup l) (cap r) (cup r))))))
   ; a `:` is a BINDING form, not an application -- opfix must NOT rearrange it.
   ; binder positions are NAMES (a symbol, or a define-sugar header list) -- left
   ; verbatim, never infix-factored -- and only each pair's VALUE and the trailing
   ; body are expressions to factor. (: a 1 b 2 body) pairs (a 1)(b 2); a lone
   ; trailing form is the body, an even count leaks the bindings (no body): the
   ; eval pairing, exactly, so a punct binder like (: >< (\ ..) ..) keeps its name.
   (op-colon fs)
    (? (atom? fs) fs
       (atom? (cup fs)) (link (op-core (cap fs)) ())        ; a lone form is the body
       (link (cap fs)                                       ; the name: verbatim
             (link (op-core (caup fs))                      ; the value: factored
                   (op-colon (cuup fs)))))                  ; the rest: pairs on
   ; a \ lambda is a binder too: the args stay verbatim, only the body factors.
   ; (\ x) -- one operand -- is quote, kept whole by op-core's first clause.
   (op-lam fs)
    (? (atom? (cup fs)) (link (op-core (cap fs)) ())        ; the last operand is the body
       (link (cap fs) (op-lam (cup fs))))                    ; an arg: verbatim, recurse
   ; op-flat-ol: flatten a directly-nested lambda body into a multi-param lambda at the SOURCE level (shared
   ; by BOTH compilers through opfix, so neither c0 nor feel builds the extra inner closure a curried HOF
   ; would): (\ a (\ b B)) -> (\ a b B). SHADOWING stays nested (op-disjoint?): (\ x (\ x x)) must NOT become
   ; the ambiguous (\ x x x) (its body would bind two same-named params -- show and = disagree). Walks the
   ; operand list (params.. body), accumulating params for the shadow check; factors the final body via op-core.
   (op-mem? y l)      (? (atom? l) 0 (id? y (cap l)) 1 (op-mem? y (cup l)))
   (op-disjoint? a b) (? (atom? b) 1 (op-mem? (cap b) a) 0 (op-disjoint? a (cup b)))
   (op-params ol)     (? (atom? (cup ol)) () (link (cap ol) (op-params (cup ol))))
   (op-flat-ol ol seen)                                      ; -> the flattened operand list, body left RAW (op-lam factors it after; no op-core ref here, so c0 compiles it clean)
    (? (atom? (cup ol))
       (: b (cap ol)
          (? (&& (two? b) (id? (cap b) '\) (two? (cuup b)) (op-disjoint? seen (op-params (cup b))))   ; (two? (cuup b)) = a real lambda, not quote (avoids the `!` sigil, unbound this early in prel)
             (op-flat-ol (cup b) seen)                       ; disjoint inner lambda -> merge its operands up
             ol))                                             ; else the body stays raw
       (link (cap ol) (op-flat-ol (cup ol) (link (cap ol) seen))))   ; a param: keep + accumulate for the shadow check
   ; the comma layer -- the loosest grouping, ABOVE infix: a list's datums split
   ; into clauses at top-level commas, each clause a list of its own ((f) == f
   ; makes a one-datum clause free), and the whole becomes the `,` sequencing
   ; macro call: (a, b c) -> (, (a) (b c)) -> (: _ a (b c)). empty clauses drop,
   ; so a doubled interior comma composes to one, a leading or trailing comma to
   ; nothing; one clause left means the commas composed away entirely. quote
   ; interiors stay data (op-core checks quote first); a binder spine splits
   ; like any list (the comma outranks even `:` -- parenthesize the form).
   (op-cm? l)   (? (atom? l) 0 (id? (cap l) ',) 1 (op-cm? (cup l)))
   (op-take l)  (? (atom? l) () (id? (cap l) ',) () (link (cap l) (op-take (cup l))))
   (op-drop l)  (? (atom? l) () (id? (cap l) ',) (cup l) (op-drop (cup l)))
   (op-split l) (: c (op-take l) r (? (op-cm? l) (op-split (op-drop l)) ())
                   (? (two? c) (link c r) r))
   ; op-core: the factor pass. quote interiors are DATA and stay verbatim (so a
   ; quoted list keeps its operators as plain symbols); the binding forms keep
   ; their binders (op-colon/op-lam); everything else walks. idempotent: factored
   ; output carries operators only in head position, where they read plain.
   ; did the ORIGINAL form carry a TOP-LEVEL operator (an infix/prefix punct symbol)?
   ; only then may a lone folded operand splice up -- a bare list-of-one like
   ; ((mov r3 r12)) has no top-level operator and STAYS wrapped (it is data: a program).
   (has-top-op? x) (? (atom? x) 0 (&& (nom? (cap x)) (op-puncp (cap x))) 1 (has-top-op? (cup x)))
   (op-core x)
    (? (atom? x) x
       (: h (cap x)
          (? (&& (id? h '\) (atom? (cuup x))) x              ; (\) / (\ x): quote, verbatim
             (op-cm? (cup x))                                ; the comma layer (helpers above)
              (: cs (op-split x)
                 (? (atom? cs) ()                            ; commas alone: nothing
                    (atom? (cup cs)) (op-core (cap cs))      ; one clause: the commas composed away
                    (link ', (map op-core cs))))
             (id? h '\) (link '\ (op-lam (op-flat-ol (cup x) ())))   ; (\ a.. body): flatten nested lambdas, then factor the body
             (id? h ':) (link ': (op-colon (cup x)))         ; (: n v ..): binders verbatim
             ; the reader's glued-run emission (mono (run d)): factor the run
             ; monadically around the (recursively opfixed) datum; an
             ; unfactorable run is the plain application (run d)
             (&& (id? h 'mono) (two? (cup x)) (two? (caup x)))
              (: p (caup x) r (cap p) d (op-core (caup p))
                 fs (? (nom? r) (monofactor r) 0)
                 (? (two? fs) (op-fin fs d) (link r (link d ()))))
             (: res (op-w x 0 0)                        ; splice a lone FOLDED operand up (an infix/prefix-op form)
                (? (&& (has-top-op? x) (two? res) (atom? (cup res)) (two? (cap res))) (cap res) res)))))) ; (a = b)->(= a b) not ((= a b)); ((mov r3 r12)) (list-of-one, no top-level op) is KEPT; (f)/(f x) untouched
; opfix = op-core, the operator factor pass. (Historically opfix also ran an
; optional native-codegen hook, book['opjit]; that JIT experiment was retracted --
; its one real win, reduction reassociation, lives in the C builtins now, and the
; trampoline finding in love/glaze/README.md.)
(: (opfix x) (op-core x))
; the SECOND seal: opfix exists now, so it joins num-ap/add/mul in the g->hot_*
; fields (idempotent for the trio) -- the C eval lane reads the field henceforth,
; so no later rebind of the nom can reach it.
(seal-hooks ())
; the two words behind the self-named apply operators (opfix is live now, so their punct binder
; names ride verbatim): ** flips (a ** b = (b a), tightest grip), $ is the identity so a $ b = (a b)
; -- haskell-like weak-binding application (loosest grip). glued $x stays saturate, a different valence.
(: (** a b) (b a))
(: $ 1)
; the RNG, two layers over the pure C steps (seed -> a fresh 256-bit state; C holds
; no state). FUNCTIONAL (reproducible): (random st n) draws once -> (value . st'),
; value's TYPE by n -- float -> float in [0,n), positive int -> int in [0,n), n <= 0
; -> a full-width int. GLOBAL (hidden stream on book ('rng-state), lazily clock-seeded):
; (rand n) -> int in [0,n) (n<=0 full-width); (randf n) -> float in [0,n) (n<=0 -> [0,1)).
; rng-get/rng-set are the stream's PUBLIC plumbing -- (rng-set (seed n)) pins it, making
; rand/randf reproducible (a bad state answers 0, the stream unchanged); rng-get reads it.
(: (random st n) (? (gem? n) (: r (randf-next st) (link (* n (cap r)) (cup r)))
                    (< 0 n)  (: r (rand-next st)  (link (% (cap r) n) (cup r)))
                    (rand-next st))
   (rng-get _) (: st (peep book 'rng-state 0)
                  (? (tray? st) st (: f (seed (clock 0)) _ (pin book 'rng-state f) f)))
   ; valid state = rank-1, length 4, and the platform's own limb type -- 64-bit
   ; limbs ride a Z array on a 64-bit word but a C (8-byte) array on a 32-bit
   ; one, so compare against a fresh seed's type, never a baked-in `z`.
   (rng-set st) (? (&& (tray? st) (= 1 (rank st)) (= 4 (tally st)) (= (tier (seed 0)) (tier st)))
                   (: _ (pin book 'rng-state st) st)
                   0)
   (rand n) (: r (rand-next (rng-get 0)) _ (pin book 'rng-state (cup r))
               (? (&& (charm? n) (< 0 n)) (% (cap r) n) (cap r)))
   (randf n) (: r (randf-next (rng-get 0)) _ (pin book 'rng-state (cup r))
               (? (&& (constellation? n) (< 0 n)) (* n (cap r)) (cap r))))
; readability / lisp-compat aliases by head-symbol substitution.
; do/begin/progn sequence side effects and return the last (identical to the
; current `,` macro -> `(: _ a _ b ... last)`); let -> the `:` let form;
; if/cond -> the `?` conditional.
(: seqx (\ l (link ': (foldr (\ l r (link '_ (link l r))) (list (last l)) (init l)))))
(:: ', seqx) (:: 'do seqx) (:: 'begin seqx) (:: 'progn seqx)
(:: 'let (\ a (link ': a)))
(:: 'if (\ a (link '? a)))
(:: 'cond (\ a (link '? a)))
(:: 'quote (\ a (link '\ a)))

; --- assert / suite: the strict checkers, baked so they work with NO harness ---
; assert was never a global -- a bare (assert ...) read the missing nom and ABSORBED
; its args (a silent always-pass, the leniency that hid a stale claim). now it is real:
; each form must net true or assert SCARES ('assert . the-source-form), terminal without
; a help, so a false claim stops the run loud and CROSS-TARGET -- no `quit` (host-only),
; so this fires on the kernel too. each form is checked once; the failing SOURCE prints,
; not its (false) value. the big test corpus still pins its own dot-counting `assert`
; over this in test/00-init.l (a richer report); everywhere else -- the glaze self-tests,
; an app, the freestanding kernel -- gets this one. `suite` wraps the recurring shape:
; a battery then a "<name>: ok" sentinel, so a file gates ITSELF (the asserts scare on
; failure, no sentinel printed; else the name lands and the run is provably whole).
; NOT named `test` -- that is a common local (ev.l binds it), and a macro ignores
; lexical scope, so the name must be globally clear; `suite` is unused as a call head.
; the message shows the SOURCE form: (assert (< x 3)) -> ";; assert (< x 3)" on failure.
; a purely-constant assertion ((= 5 (+ 1 2))) is folded by cprop -- the partial-eval
; pre-pass, BEFORE this macro expands -- so the macro is handed the value (0) and the
; message reads ";; assert 0". harmless: the assertion still fails correctly, and a real
; assert tests a RUNTIME value (never a closed constant), so its form survives intact.
(:: 'assert (\ l (link 'do (map (\ e `('? e 0 `('scare `('\ 'assert) `('\ e)))) l))))
(:: 'suite (\ l (link 'do (link (link 'assert (cup l)) (list (list 'puts (cap l)) (list 'puts ": ok\n"))))))


; --- array element-type inference + shape coercion (shared by `tuple` and `array`).
; a-type: a WITNESS STAR of the element tier -- the highest of {Z<R<C<O} present in a
; list of values, returned as an inhabitant (0/0.0/~(0 0)/()) for `tray` to read the
; tier off (the generic ctor takes a witness, not a code). a box still fits z; a complex value lifts
; to a packed (re,im) `c` array; only a value past the real numeric tower (bignum,
; symbol, string, nested array, …) lifts the whole array to an object (o) array.
; a-dim/a-shape: a shape dimension may be any numeric -- a float, complex, or even
; a vector -- and is coerced to a nat by saturation (($ d) -- the count law:
; non-positive -> 0, a float ceils, a complex its modulus, a vector its net sum). a list
; shape is coerced per-dim; a lone numeric is a rank-1 length (so (array 2.5 …)
; means rank-1 length 3, (array ~(3 4) …) length 5).
(: (a-rank x) (? (gem? x) 1 (| (charm? x) (sun? x)) 0 (twin? x) 2 3)
   (a-emax m x) (: k (a-rank x) (? (< m k) k m))
   (a-type d) (: k (foldl a-emax 0 d) (? (= k 0) 0 (= k 1) 0.0 (= k 2) ~(0 0) ()))   ; a witness star of the tier
   (a-dim d) (? (two? d) (tally d) (sat d))
   (a-shape s) (? (two? s) (map a-dim s) `((a-dim s))))

; tuple/hash value constructors (also the read-back targets of the `@(…)` / `#(…)`
; printer-sugar forms). `tuple` builds a rank-1 array from its element args,
; inferring the element type via a-type (so it carries bignums/symbols in an o
; array, not just z/r); `map` builds a map from alternating key/value args.
(: (spread l) (tray (a-type l) `((foldl (\ n _ (+ 1 n)) 0 l)) l))
(:: 'tuple (\ a (list 'spread (link 'list a))))
; (hash x) boxes any non-list -- a fresh mutable hash pinning x
; at 0, so #5 = #(0 5) and #() IS #0 (() is 0); chains pin as usual, and an odd
; trailing operand lands in the 0 box slot. the empty hash is (tablet 0).
(: (hashtx a) (? (atom? a) '(tablet 0)
                 (atom? (cup a)) (list 'pin (hashtx 0) () (cap a))
                 (list 'pin (hashtx (cuup a)) (cap a) (caup a))))
(:: 'hash (\ a (hashtx a)))
; (pins t k v ..): pin each key/val pair onto an EXISTING tablet t, nested left
; to right -- (pins t 'a 1 'b 2) = (pin (pin t 'a 1) 'b 2). the mutable-
; accumulator twin of #(..) (which mints a FRESH tablet); pin hands its map
; back, so the pins chain. an odd trailing operand is dropped (no lone key).
(:: 'pins (\ l (: (go t r) (? (two? (cup r)) (go (list 'pin t (cap r) (caup r)) (cuup r)) t)
                  (go (cap l) (cup l)))))


; (array shape elem…): the ergonomic N-D array constructor. `shape` is either a
; dimension *list* (rank-N) or a single number n (a rank-1 vector of length |n|),
; coerced via a-shape (the saturated net). the element type is INFERRED via a-type, so a
; bignum / complex / string / nested array forces an object array automatically --
; no explicit type code, and exactness is never silently lost. the elements are
; ordinary trailing arguments (not a list): since the shape fixes the count,
; `array` curries them one at a time and builds once it has them all -- so
; (array 2 'a 'b), (array '(2 2) 1 2 3 4) and a partial like (array 3) (a collector
; awaiting 3 elems) all fall out of l's auto-currying, like numap's n-ary text.
(: (a-coll sh n acc) (? (< n 1) (: d (rev acc) (tray (a-type d) sh d))
                        (\ x (a-coll sh (- n 1) (link x acc))))
   (array s) (: sh (a-shape s) (a-coll sh (foldl (*) 1 sh) 0)))

; --- newtypes: a DIE strikes COINS -- user kinds that ride +/* with ()-identity ---
; a COIN (coin?) is struck from a die over a payload; the die is the type descriptor (a
; monoid/ring -- a map of slot->method); die-of reads it back. + and * dispatch through
; the KHot lane to the die's plus/times, run over the LOADED payloads and re-struck; ()
; stays their identity for free (the mint-identity hoist sits above the +/* matrix).
; net/=/</show default over the payload in the C core, so $/truth, value-equality, sort
; and the (name payload) show form all work with no extra ceremony. plus/times take
; payloads. (ring 'z5 plus times) / (monoid 'z5 plus) -> a constructor: payload -> coin.
; `-` rides slot 5 and `/` slot 8 for a die that pins them (ring/monoid pin none;
; q pins both): unpinned, they answer (), the undefined-op law. slot 6 is the NET
; MODE, a fixnum: absent/0 nets the payload (the hom); 1 nets by TALLY -- the
; count, never negative -- so a die over signed content (a ledger, a polynomial)
; can make its coins' truth read "has any", not "sums green". slot 7 truthy
; declares the die's coins STARS: numeral application powers them through their
; own * (num-ap's open fork) instead of composing them as functions.
; LIVES HERE, below the #(..) `hash` macro: a macro must precede its use in the prel,
; since the bootstrap c0 is a single forward pass (no macro hoist). slot 0 = show-name.
(: (ring name plus times) (:
      d #(0 name)                                                ; the die; slot 0 = show-name
      _ (pin d 1 (\ a b (coin d (plus (load a) (load b)))))      ; slot 1 = +
      _ (? times (pin d 2 (\ a b (coin d (times (load a) (load b))))) 0)  ; slot 2 = * (a monoid has none)
      (\ p (coin d p)))                                          ; the ctor: payload -> coin
   (monoid name plus) (ring name plus 0))                        ; a monoid is a ring with no product

; --- rationals: an exact field shipped as a coin over a reduced (num den) payload ---
; +/*/-// cross-multiply and reduce (/ inverts and multiplies; by zero -> (), Q holds
; no inf, so reciprocal powers ride: (-1 (q 2 1)) is (q 1 2)); = is payload equality after reduction (so (q 2 4) =
; (q 1 2)); show is (q (num den)), which reads back since q also accepts the 2-list.
; the die declares its coins STARS (slot 7: numerals power through *, the surface net
; answers the coin ITSELF) and pins net-mode 2 (slot 6, RATIO): the C truth path
; reads n/d with the sign EXACT, so a rational's color is its VALUE's -- (q -1 2) is
; nothing, $ ceils the magnitude, the subtract-and-test sign idiom holds. ORDER too:
; a ratio coin seats IN the number band, interleaved with the closed tower by value
; (word-sized components compare by exact cross-multiply, so near-equal rationals
; never tie by double rounding; wider fall to the sign-exact quotients). a method
; gets RAW operands, so it OWNS its domain: ok? admits a star or an (n d) of stars
; (an integer rides cap/mint: (cap 7) is 7, (caup 7) the unit), and anything else
; answers () -- the undefined-op law, and the fence that keeps a foreign operand (a
; chain of coins, say) from dragging gcd into a loop. `reduce`/`ok?` are local
; plumbing; q is the only name this leaks.
; it sits in prel because post.l is HOST-ONLY (main.c is the one frontend that evals it) and rune
; rides q everywhere -- port/playdate bakes rune.h with no post, so a move would take the CAS
; workbench with it. the same tie holds the kanren block below.
(: q (: reduce (\ n d (? (= 0 n) (L 0 1)                        ; gcd out, sign on num, den > 0
                        (: g (gcd (abs n) (abs d)) s (? (< d 0) -1 1)
                           (L (// (* s n) g) (// (* s d) g)))))
        ; the domain test is the CLOSED star (the fixpoint minus coins): star?
        ; is open now, and an open star as an (n d) component would drag gcd
        ; off the tower it terminates on -- a coin is never a coefficient.
        (nstar? v) (? (coin? v) 0 (id? v (net v)))
        (ok? v) (? (nstar? v) 1
                   (two? v) (&& (nstar? (cap v)) (nstar? (caup v)))
                   0)
        st #(0 'q 7 1 6 2)                                      ; slot 7: a rational IS a star;
                                                                ; slot 6 mode 2: C truth by VALUE
        ; `#(..)` IS the pins macro -- it expands to nested pins -- but the methods cannot ride the
        ; literal: each names `st` back, and a self-referential bind compiles here yet fails in the
        ; love0 lane ((q 1 2) + (q 1 3) came out unequal to (q 5 6)). bind the die, THEN pin onto it.
        _ (pin st 1 (\ a b (: x (load a) y (load b)             ; (n1 d1) + (n2 d2)
              (? (&& (ok? x) (ok? y))
                 (coin st (reduce (+ (* (cap x) (caup y)) (* (cap y) (caup x))) (* (caup x) (caup y))))
                 ()))))
        _ (pin st 2 (\ a b (: x (load a) y (load b)             ; (n1 d1) * (n2 d2)
              (? (&& (ok? x) (ok? y))
                 (coin st (reduce (* (cap x) (cap y)) (* (caup x) (caup y))))
                 ()))))
        _ (pin st 5 (\ a b (: x (load a) y (load b)             ; (n1 d1) - (n2 d2)
              (? (&& (ok? x) (ok? y))
                 (coin st (reduce (- (* (cap x) (caup y)) (* (cap y) (caup x))) (* (caup x) (caup y))))
                 ()))))
        _ (pin st 8 (\ a b (: x (load a) y (load b)             ; (n1 d1) / (n2 d2): invert and
              (? (&& (ok? x) (ok? y) (nil? (= 0 (cap y))))      ; multiply; by zero -> () (no inf in Q)
                 (coin st (reduce (* (cap x) (caup y)) (* (caup x) (cap y))))
                 ()))))
        (\ n (? (two? n) (coin st (reduce (cap n) (caup n)))    ; (q (n d)) -- the show form
                (\ d (coin st (reduce n d)))))))               ; (q n d) -- the ergonomic curried form

; --- kanren: first-order unification + a church-stream search monad ----------
; the UNIFIER is the compiler-grade core (a future relational rewrite layer sits
; on `unify`/`walk`); the SEARCH half (streams, `query`, `===`) is its userland
; face. `unify s u v` is PURE -- a substitution or `ufail` -- with NO occurs
; check: love's own values are non-well-founded (a chain is a mu-type, the book
; holds the book), so a self-referential binding is welcome -- it's a rational
; tree, and `walk_star` mu-marks a back-edge to reify it without looping. a goal
; is (\ s stream); est/=== unify, + interleaves (disjunction), * binds
; (conjunction) -- so a relation is a polynomial in unifications (see the ring).
; the module span opens here: the trailer below (names ()) sweeps everything this
; section leaks into the `kanren` book, keeping the public few on the global book.
; post would be the natural home, but it is host-only and rune's matcher unifies on this -- same
; tie as q above, so kanren stays where every frontend can reach it.
(: kanren-mark (names ()))
(: (dict_has d k) (: (loop k ks vs) (? ks (? (= k (cap ks)) vs (loop k (cup ks) (cup vs)))) (loop k (cap d) (cup d)))
   (dict_set d k v) (link (link k (cap d)) (link v (cup d)))
   empty_dict '(())
   has_s dict_has
   ext_s dict_set
   (mintp x) (&& (nom? x) (nom? (string x)))       ; string of a named sym is its name; of a mint, itself
   (var? x) (&& (two? x) (mintp (caup x)))
   (var x) (link x (nom 0))                         ; (label . fresh-nom) -- identity by the nom
   (walk s u) (? (nil? (var? u)) u (: t (has_s s u) (? (two? t) (walk s (cap t)) u)))
   ufail (nom 0)
   (ufail? x) (id? x ufail)
   (unify s u v) (: x (walk s u) y (walk s v)       ; PURE: a substitution, or ufail
     (? (= x y) s
        (var? x) (ext_s s x y)
        (var? y) (ext_s s y x)
        (&& (two? x) (two? y)) (: s1 (unify s (cap x) (cap y)) (? (ufail? s1) ufail (unify s1 (cup x) (cup y))))
        ufail))
   (subst s t) (? (var? t) (: w (walk s t) (? (var? w) w (subst s w)))   ; instantiate a template under s
                  (two? t) (link (subst s (cap t)) (subst s (cup t)))
                  t)
   (rewrite pat tpl x) (: r (unify '(()) pat x) (? (ufail? r) x (subst r tpl)))   ; pattern -> template, or x unchanged
   (sX x s n m y) (y x s)                            ; stream: cons
   (sno n m y) (n sno)                               ; stream: empty
   (sun x) (sX x sno)
   (s_plus s t) (s (\ _ t) (\ s _ m _ (m (s_plus t s))) (\ x s (sX x (s_plus s t))))   ; interleave (disj)
   (s_star s g) (s (\ _ s) (\ s _ m _ (m (s_star s g))) (\ x s (s_plus (g x) (s_star s g))))  ; bind (conj)
   (stake i s) (? (nil? i) () (s (\ _ ()) (stake i) (\ x s (link x (stake (+ -1 i) s)))))
   (smap f s) (s (\ _ s) (\ s _ m _ (m (smap f s))) (\ x s (sX (f x) (smap f s))))
   (sfold f z s) (s (\ _ z) (sfold f z) (\ x s (f x (sfold f z s))))
   slist (sfold link ())
   (est u v s) (: r (unify s u v) (? (ufail? r) sno (sun r)))        ; unify-as-goal
   (query n ks g) (:
    (walk_star s u path) (?                          ; cycle-safe: (mu label) on a back-edge
     (var? u) (: t (has_s s u)
                 (? (nil? (two? t)) u
                    (member? u path) (L 'mu (cap u))
                    (walk_star s (cap t) (link u path))))
     (two? u) (link (walk_star s (cap u) path) (walk_star s (cup u) path))
     u)
    (keyvar keys k) (? (atom? keys) 0 (: kv (cap keys) (? (= (cap kv) k) kv (keyvar (cup keys) k))))
    (reify1 st) (flip catmap ks (\ k (: kv (keyvar (cap st) k) (? (two? kv) `(k (walk_star st kv ()))))))
    (map reify1 ((? n (stake n) slist) (g empty_dict))))
   g-die (: d #(0 'goal)                             ; the goal-coin: + = disj, * = conj
            _ (pin d 1 (\ A B (coin d (\ s (s_plus ((load A) s) ((load B) s))))))
            _ (pin d 2 (\ A B (coin d (\ s (s_star ((load A) s) (load B))))))
            d)
   (rel goal) (coin g-die goal))                     ; wrap a goal as a ring element
(:: 'zz (\ x (: s (nom 0) n (nom 0) m (nom 0) y (nom 0) x2 (cap x) `('\ s n m y `(m `(x2 s))))))  ; delay a goal (inverse-eta)
(:: 'et (\ xs (: x (cap xs) r (cup xs) (foldl (\ a b `((\ a b s (s_star (a s) b)) a b)) x r))))   ; conj of goals
(:: 'vel (\ xs (: x (cap xs) r (cup xs) (foldl (\ a b `((\ a b s (s_plus (a s) (b s))) a b)) x r)))) ; disj of goals
(:: '\\ (\ a (link (link '\ a) (map (\ a `(var `('\ a))) (init a)))))                             ; lambda w/ fresh logic vars
(: === (\ u v (rel (est u v))))                       ; the relation's infix unify
(pin operators '=== 2)
; the kanren MODULE BOUNDARY (the holo/glaze pattern, in-prel so it holds on every
; target): the span since kanren-mark sweeps into the `kanren` book -- (kanren 'query),
; (kanren 'walk) .. -- except the PUBLIC keepers: `unify` (the compiler-grade core)
; with its ufail/ufail? contract, and the names the goal macros + the === operator
; EMIT into user code (var, s_plus, s_star, ===) -- the emit law, the same reason the
; egg keeps spread/link/mono. re-evals (the egg warm) rebuild the book: idempotent.
(: seen (: t (tablet 0) _ (each kanren-mark (\ k (pin t k 1))) t)
   m (tablet 0)
   keep '(unify ufail ufail? === var s_plus s_star)
   _ (each (names ())
      (\ k (? (peep seen k 0) 0
              (member? k keep) 0
              (: _ (pin m k (peep book k 0)) _ (pull book k 0) 0))))
   _ (pull book 'kanren-mark 0)
   _ (pull m 'kanren-mark 0)
   _ (pin book 'kanren (\ k (? (id? k 'keys) (keys m) (peep m k 0))))  ; lookup-only closure: m private,
   0)                                                                  ;   unpoisonable; (kanren 'keys) introspects

; recursive-value boxing for `:` (the letrec* "capture by location" fix).
; ONE SCOPE: every name bound in a `:` resolves locally over the whole form.
; `boxfix-core` takes a source-order (name . def) chain-list `prs` and the body
; expression. a reference to a name at-or-before its own (first) binding is a
; FORWARD reference; the name is indirected through a CELL -- a box (a fresh
; map, the # adjoint of :) keyed by the binding site's nom: prepend
; `cell (tablet 0)`, store via pin, read via (missing cell 'nom) -- the nif
; VALUES, not their names, so the rewrite survives the egg pulling names and
; user bindings shadowing them. a pre-fill read is a map miss, i.e. THE MISSING
; CONDITION with the binding-site nom as payload: a call for help, the zero point helpless --
; read-before-pin is one law at every depth, local or book. a value binding
; boxes on ANY forward reference; a lambda binding only on a DIRECT (not
; under-lambda) one -- ordinary recursion keeps the lazy lane. values defined
; before their users are untouched. It returns 0 when nothing needs boxing,
; else the rewritten (prs' . body'). This is the single source of truth,
; shared verbatim by both compilers: ev.l's feel and c0 (ev.c) both call the
; flat `boxfix` wrapper below, bpr+flatten around the same core. `lambp`/`dsug`
; are globals so `ale` shares them (lambda detection, binding desugaring).
(: (lambp x) (? (two? x) (&& (id? '\ (cap x)) (two? (cuup x)))) ; \ with >=1 param (one-operand \ is quote)
   (dsug n d) (? (atom? n) (link n d) (dsug (cap n) (link '\ (cat (cup n) (list d)))))
   (lp x) (: a (cup x) (? (atom? (cup a)) (link 0 (cap a)) (link (init a) (last a))))
   (ln bs) (? (atom? bs) 0 (atom? (cup bs)) 0 (link (cap (dsug (cap bs) 0)) (ln (cuup bs))))
   ; w/wl: does x / binding-list bs reference symbol v? multi-operand \ and
   ; : shadow; one-operand \ is quote (data, nothing free); macro expansion (so :- ?-
   ; &&/|| are seen as the compiler sees them). u tracks lambda depth; dm (direct
   ; mode) counts only references NOT under a lambda.
   (w v x bnd u dm) (?
    (nom? x) (&& (? dm (nil? u) 1) (id? x v) (nil? (member? v bnd)))
    (atom? x) 0
    (: h (cap x) (?
      (id? h '\) (? (atom? (cuup x)) 0 (: r (lp x) (w v (cup r) (cat (cap r) bnd) 1 dm)))
      (id? h ':) (wl v (cup x) (cat (ln (cup x)) bnd) u dm)
      (: m (? (nom? h) (peep macros h 0) 0) (? (&& m (nil? (member? h bnd))) (w v (m (cup x)) bnd u dm) (any (\ e (w v e bnd u dm)) x))))))
   (wl v bs bnd u dm) (?
    (atom? bs) 0
    (atom? (cup bs)) (w v (cap bs) bnd u dm)
    (: nd (dsug (cap bs) (caup bs)) (? (w v (cup nd) bnd u dm) 1 (wl v (cuup bs) bnd u dm))))
   ; boxset: ONE SCOPE -- a reference at-or-before the name's own (first) binding
   ; is a forward reference and earns a cell. a value binding boxes on ANY such
   ; reference; a lambda binding only on a DIRECT one (not under a lambda), so
   ; ordinary recursion keeps the lazy lane.
   (boxset prs) (: nm (map cap prs)
     (filter (\ v (any (\ p (w v (cup p) 0 0 (lambp (cup (assoc v prs)))))
                       (take (+ 1 (lidx v nm)) prs))) nm))
   (mkcs bx) (map (\ v (link v (link (nom 0) (nom v)))) bx)
   (cellbinds cs) (map (\ c (link (caup c) (list tablet 0))) cs))
; boxfix-core (c0): same prep + rewrites refs via sub/subl (c0 has no analysis-time
; redirect). Returns (prepped-prs . body') or 0.
(: (boxfix-core prs body) (:
   (rmc cs ns) (filter (\ p (nil? (member? (cap p) ns))) cs)
   ; bnd: the lexical binder set, seeded with the scope's own names -- a locally-bound name is an
   ; ordinary variable, never a macro head (the shadow law; wx/cprop carry the same guard).
   (sub cs x bnd) (?
    (nom? x) (: p (assoc x cs) (? (two? p) (list missing (caup p) (list '\ (cuup p))) x))
    (atom? x) x
    (: h (cap x) (?
      (id? h '\) (? (atom? (cuup x)) x (: r (lp x) (link '\ (cat (cap r) (list (sub (rmc cs (cap r)) (cup r) (cat (cap r) bnd)))))))
      (id? h ':) (: ns (ln (cup x)) (link ': (subl (rmc cs ns) (cup x) (cat ns bnd))))
      (: m (? (nom? h) (peep macros h 0) 0) (? (&& m (nil? (member? h bnd))) (sub cs (m (cup x)) bnd) (map (\ e (sub cs e bnd)) x))))))
   (subl cs bs bnd) (?
    (atom? bs) bs
    (atom? (cup bs)) (list (sub cs (cap bs) bnd))
    (: nd (dsug (cap bs) (caup bs)) (link (cap nd) (link (sub cs (cup nd) bnd) (subl cs (cuup bs) bnd)))))
   (: bx (boxset prs)
      (? (nil? bx) 0
       (: cs (mkcs bx) ns0 (map cap prs)
          (one p) (: d (sub cs (cup p) ns0)
                     (? (member? (cap p) bx)
                        (: c (assoc (cap p) cs) (link '_ (list pin (caup c) (list '\ (cuup c)) d)))
                        (link (cap p) d)))
          (link (cat (cellbinds cs) (map one prs)) (sub cs body ns0)))))))
; flat-list adapter for c0 (ev.c): a `:` binding list (n1 d1 .. [body]) -> the
; rewritten list, or the input unchanged. even (body-less) lets and lets that
; need no boxing pass through verbatim.
; it sits last because it needs boxfix-core and dsug above, not because it is optional: love.c
; interns "boxfix" and calls it off the book, so c0 compiles prel itself through this -- it must
; exist before ev.l does. ev.l's feel calls the same one, which is the point (one pass, both lanes).
(: (boxfix fs) (:
   (ev l) (? (atom? l) 1 (atom? (cup l)) 0 (ev (cuup l)))
   (bpr fs) (? (atom? (cup fs)) (link 0 (cap fs))
               (: r (bpr (cuup fs)) (link (link (dsug (cap fs) (caup fs)) (cap r)) (cup r))))
   (? (ev fs) fs
    (: pb (bpr fs) r (boxfix-core (cap pb) (cup pb))
       (? r (cat (catmap (\ p (list (cap p) (cup p))) (cap r)) (list (cup r))) fs)))))
