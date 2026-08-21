; prel.l -- love prel: data + function + macro definitions.
; prefix only down to the `operators` table: c0 compiles this before opfix and before the church
; add/mul hooks exist. an operator up here cores the egg.
; FIXME move ev support functions to ev where possible
(: id 1
   (const x _) x
   (co f x y) (f y x)                ; the argument-transpose
   flip co
   two? link?
   intern name
   sat saturate)
(: : () ? () \ ())
(: pi 3.141592653589793
   tau (* 2 pi)
   e 2.718281828459045                          ; exp is (\ x (x e))
   true 1 false 0
   i (twin 0 1) ;; FIXME up top
   (tangent x) (/ (sine x) (cosine x))
   star-tray (tray 0) gem-tray (tray 0.0) twin-tray (tray (twin 0 0)) top-tray (tray ())  ; the four typed-tray ctors (witness baked in)
   (tier v) (? (tray? v) (: c (atype v) (? (= c 0) star-tray (= c 1) gem-tray (= c 2) twin-tray top-tray)) ())  ; a tray's tier IS its ctor; nil off a tray
   zero? nil?
   (atom? x) (nil? (two? x))
   one? atom?
   ; a fresh layer: its own macro table at key 0, the pre-enter chain and name at charm key 31337.
   ; a charm key cannot collide with a name. (enter ()) is anonymous.
   (enter nm) (: F (tablet 0) _ (pin F 0 (tablet 0)) _ (pin F 31337 (link (books ()) nm))
                 _ (setbooks (link F (books ()))) ())
   ; use: splice a registered module just BELOW the head, so its names resolve on the walk but
   ; never shadow yours. a missing name scares -- a silent ()-splice would be a silent no-op.
   (use m)    (: b (? (tablet? m) m
                      (: r (peep (mods ()) m ()) (? (nil? r) (scare (\ no-module) m) r)))
                 (setbooks (link (cap (books ())) (link b (cup (books ()))))))
   ; unwind to the pre-enter chain, register the layer under its name, answer the book.
   ; a bare leave -- never entered, so no charm -- pops whatever sits below the head instead.
   (leave _)  (: F (cap (books ())) s (peep F 31337 ())
                 (? (two? s)
                    (: _ (pull F 31337 0) _ (setbooks (cap s))
                       _ (? (nil? (cup s)) 0 (pin (mods ()) (cup s) F)) F)
                    (setbooks (link F (cup (cup (books ())))))))
   ; from: a module IS its tablet. currying reaches a member because a tablet already indexes
   ; itself, so (from 'holo 'assemble) reads the same as ever; `keys` is the ordinary function,
   ; not a reserved key; (from ()) lists the registry; a missing module answers ().
   ; the tablet is the real one -- pin it and you are doing surgery on a live runtime, which is
   ; yours to do. it was never a safety wrapper: a module's members are folded into its consumers
   ; at THEIR compile, so gutting one changes nothing already compiled and everything compiled
   ; after -- the hazard is divergence, and no closure here could have caught it.
   (from k)   (? (nil? k) (keys (mods ())) (peep (mods ()) k ()))
   (caap x) (cap (cap x)) (caup x) (cap (cup x))
   (cuap x) (cup (cap x)) (cuup x) (cup (cup x))
   (caaap x) (cap (cap (cap x))) (caaup x) (cap (cap (cup x)))
   (cauap x) (cap (cup (cap x))) (cauup x) (cap (cup (cup x)))
   (cuaap x) (cup (cap (cap x))) (cuaup x) (cup (cap (cup x)))
   (cuuap x) (cup (cup (cap x))) (cuuup x) (cup (cup (cup x)))
   >< link)                                         ; the infix cons: (a >< b) IS (link a b); glued ><x stays the cup-cap accessor (valence)
(: (gcd a b) (? (= b 0) (? (< a 0) (- 0 a) a) (gcd b (% a b)))
   (modpow b k m) (? (< k 1) 1 (: h (modpow b (// k 2) m) h2 (% (* h h) m)
                                 (? (= 0 (% k 2)) h2 (% (* h2 b) m)))))
(:
   )
; `::` defines a macro in the head layer's table -- the same book a plain `:` defglobs to, so
; prel's macros land in orth during boot, a script's in the session layer, a module's in the module.
(: ; look a macro up across the layer chain -- the twin of love.c's macroget, kept in step with
   ; it. each book holds its table at the nil key; a layer without one is skipped. 0 = no macro.
   (macros k) (: (go c) (? (nil? (two? c)) 0
                           (: m (peep (cap c) 0 ())
                              (? (tablet? m) (: v (peep m k 0) (? v v (go (cup c))))
                                 (go (cup c)))))
                 (go (books ())))
   (:: a b) (: F (cap (books ())) m (peep F 0 ())
               (pin (? (tablet? m) m (: t (tablet 0) _ (pin F 0 t) t)) a b))
   putc (put out)
   puts (say out)
   (putn n) (putbn out n 10)
   (getc _) (see in))
; numfn lays the n-fold thread with spin/poke, so applying a fixnum never re-enters the compiler:
; n quote-f's then n ap's, cell 0 the printer's source, entry at cell 1.
; the helpers pass th/f/n explicitly: a closure over the forward-bound th miscompiles under c0.
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
   ; apcap: a PUBLIC tunable ceiling. a numeral compose count above it scares instead of
   ; allocating O(k), so (bignum f) asks for help where it would oom. retune: (pin apcap 0 n).
   apcap (pin (tablet 0) 0 1048576)              ; (a raw box: the # sigil's hash isn't built yet)
   (ipow b k) (? (< k 1) 1 (: h (ipow b (// k 2)) h2 (* h h) (? (= 0 (% k 2)) h2 (* h2 b))))
   ; each site keeps its OWN (scare 'apcap k) as the cond result, so a recovering help's value
   ; lands as the result directly -- no thunk on the hot path. don't factor it out.
   (apover k) (< (peep apcap 0 0) k)
   ; powover guards the RESULT size, not the count: only an exact base beyond +-1 can grow, so it
   ; reads the box as a WORD budget (the stock 2^20 allows an 8MB number).
   (powover b m) (? (apover m)
                    (? (whole? b)
                       (: a (abs b)
                          (? (< a 2) 0
                             (< (* 64 (peep apcap 0 0)) (* m (/ (log a) (log 2))))))
                       0)
                    0)
   ; x ** n across the whole tower: array exponent -> elementwise, integer -> exact squaring
   ; (a NEGATIVE one gives the reciprocal power, so (-1 x) == (/ 1 x)), else `power`.
   (powg x n) (? (tray? n) (power x n)
                 (whole? n) (: m (abs n)
                             (? (powover x m) (scare 'apcap m)
                                (< n 0) (/ 1 (ipow x m)) (ipow x m)))
                 (power x n))
   ; church application across the tower: a numeric operand gives x ** n, a function composes
   ; ($ n) times, saturation reducing any numeral to a fixnum nat. cmp2..cmp8 are cached
   ; combinators; past them numfn assembles the thread. keep the straight-line bodies -- a
   ; counting applier measures ~45% slower at k=2..8.
   _ (: cmp2 (\ f (\ x (f (f x))))
        cmp3 (\ f (\ x (f (f (f x)))))
        cmp4 (\ f (\ x (f (f (f (f x))))))
        cmp5 (\ f (\ x (f (f (f (f (f x)))))))
        cmp6 (\ f (\ x (f (f (f (f (f (f x))))))))
        cmp7 (\ f (\ x (f (f (f (f (f (f (f x)))))))))
        cmp8 (\ f (\ x (f (f (f (f (f (f (f (f x))))))))))
        ; frequency-ordered, so it stays a chain: k=2/3 costs one or two compares. a table would
        ; only pay past k=6.
        (cmp k) (? (= k 2) cmp2 (= k 3) cmp3 (= k 4) cmp4 (= k 5) cmp5 (= k 6) cmp6 (= k 7) cmp7 (= k 8) cmp8 0)
        ; past cmp8 the count still has to compose. numfn LAYS the thread -- straight-line, no
        ; counter, the fast shape -- but the 3n+5 cells ARE the cost, and they are the one thing a
        ; numeral can grow without bound: n=100000 is ~72MB, and apcap admits 2^20. so the thread
        ; is for counts small enough to be free, and past nfcap nfloop composes by COUNTING
        ; instead -- one closure, O(1) cells, the same function.
        (nfloop n f x) (? (< n 1) x (nfloop (+ -1 n) f (f x)))
        nfcap 1024
        ; an open star: a coin whose die pins slot 7 is numeric by declaration, so a numeral powers
        ; it through its own * (ipow is generic). no size to read, so this lane keeps the count cap.
        (starred? x) (? (coin? x) (= 1 (peep (die-of x) 7 0)) 0)
        (num-ap n x) (? (constellation? x) (powg x n)
                        (starred? x)
                        (? (whole? n) (: m (abs n)
                                         (? (apover m) (scare 'apcap m)
                                            (< n 0) (/ 1 (ipow x m)) (ipow x m)))
                           ())
                        (: k (saturate n)
                           (? (< k 1) 1 (= k 1) x
                              (apover k) (scare 'apcap k)
                              (: c (cmp k) (? c (c x) (< nfcap k) (nfloop k x) (numfn k x))))))
        (pin book 'num-ap num-ap)))
; seal-hook caches num-ap into the VM, so it must follow the pin above.
(seal-hook 1 num-ap)
; the + and * of two FUNCTIONS, which is all church arithmetic is: the extra a-slot threaded into
; both of stack's operands is what makes + agree with addition, not the S combinator's (f x (g x)).
; the C lanes (lvm_addh/lvm_mulh) read them by slot.
(: (stack f g a x) (f a (g a x))                  ; (+ f g)
   (compose f g x) (f (g x))                      ; (* f g)
   _ (seal-hook 2 stack)
   _ (seal-hook 3 compose))
; map runs LEFT TO RIGHT: a mapped effect fires in list order. ⚠ the value is computed as an
; ARGUMENT -- link's own two operands go right to left, so nothing inside link could order them.
(: (map f l) (? (two? l) ((\ f x l (link x (map f l))) f (f (cap l)) (cup l)))
   (foldl f z l) (? (two? l) (foldl f (f z (cap l)) (cup l)) z)
   (foldr f z l) (? (two? l) (f (cap l) (foldr f z (cup l))) z)
   ; (rev l) glued onto acc in one pass. a cat per step re-copies the whole accumulation;
   ; link plus one revcat keeps it linear.
   (revcat l acc) (? (two? l) (revcat (cup l) (link (cap l) acc)) acc))   ; no `!` here: not bound yet
; the monadic words behind the glued sigils. net is the content measure -- `+` turned inward --
; and a coin's lane is by DECLARATION: die slot 7 nets itself (so additivity lands in the die's
; own ring), else its tally under net-mode 1, else its payload signed.
(: (net x) (? (tray? x) (asum x)
              (constellation? x) x
              (two? x) (foldl (\ a e (+ a (net e))) 0 x)
              (coin? x) (? (= 1 (peep (die-of x) 7 0)) x
                           (= 1 (peep (die-of x) 6 0)) (tally x)
                           (net (load x)))
              (saturate x))
   (prod x) (? (tray? x) (aprod x)
               (two? x) (foldl (\ a e (* a (prod e))) 1 x)
               x)
   negate (- 0)
   reciprocal (/ 1)
   (fraction x) (% x 1)
   (bit x) (nil? (nil? x))
   ; a star is the fixpoint of net. id?, not `=`: a number comes back unchanged where a tray's
   ; net is a fresh sum, so star? and galaxy? stay disjoint.
   (star? x) (id? x (net x))
   ; galaxy? keys on the CELLS, not the tier code, so an object tray of bignums still counts
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
   ; iterate `step` until `size` stops changing. size must be strictly monotone under step until
   ; the fixpoint, and is read before the step, so a step that mutates in place is still seen.
   (monofix st step size) (: b (size st) nx (step st) (? (= (size nx) b) nx (monofix nx step size)))

   ; the one constant-form recognizer. the answer is wrapped -- (1 . value) or () -- because a
   ; constant's value can itself be nothing. no `&&`: c0 is single-pass and it is defined later.
   (kconst e) (? (nom? e) ()                                                    ; a variable -> no fact
                 (atom? e) (link 1 e)                                           ; a literal -> (1 . itself)
                 (id? (cap e) '\) (? (atom? (cuup e)) (link 1 (caup e)) ())     ; (\ v) quote of an atom -> (1 . v); a real lambda -> no fact
                 ())
   (cat a b) (foldr link b a))
; (sort l) is the C lane -- a stable bottom-up merge by the total order, no lisp re-entry;
; descending is (rev (sort l)). an arbitrary predicate takes this lisp merge sort instead.
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
   (member? x l) (? (two? l) (? (= x (cap l)) 1 (member? x (cup l))) 0)   ; any of (= x); first-order, so the glaze's point-= grammar closes it (auto.l re-declares this body)
   (zip a b) (? (two? a) (? (two? b) (link (link (cap a) (cap b)) (zip (cup a) (cup b)))))
   rev (foldl (flip link) ())
   ; every nom on the layer chain, deduped through a tablet. noms only -- a layer's keys also hold
   ; the macro table and the unwind chain. it sits down here so catmap/foldl are already bound.
   (names _) (filter nom? (keys (foldl (\ t k (pin t k 1)) (tablet 0) (catmap keys (books ())))))
   ; the macro twin: each layer keeps its table at its nil key, so this walks the chain `macros`
   ; looks up through. same placement rule as `names`, and its `books` folds at compile time.
   (macro-names _) (filter nom? (keys (foldl (\ t k (pin t k 1)) (tablet 0)
                                             (catmap (\ b (: m (peep b 0 ())
                                                             (? (tablet? m) (keys m) ())))
                                                     (books ())))))
   ; the range 0..x-1 (iota its array twin), and LIFTED over a list so the glued run
   ; composes: ^^3 is (jot (jot 3)), a list of lists, since a run factors greedily.
   ; ⚠ no glued sigil in here -- this sits ABOVE the operators table, where infix is not
   ; yet live, so a `$x` would stay a bare nom and holo's fregs would lose a register.
   (jot x) (? (two? x) (map jot x) (: n (saturate x) (go i) (? (< i n) (link i (go (+ i 1)))) (go 0)))
   (drop n l) (? n (drop (- n 1) (cup l)) l)
   (take n l) (? n (? (two? l) (link (cap l) (take (- n 1) (cup l)))))   ; stop at end-of-list (else it pads past it)
   (part p) (foldr (\ a m (? (p a) (link (link a (cap m)) (cup m))
                                        (link (cap m) (link a (cup m))))) '(())))
; the port head is three words -- ap, vt, ungetc -- and both languages know it: love.h declares
; it, these build it. each poke answers the cell it wrote, so `poke -1` walks backwards. the vt
; is the port's whole kind, so these stamp the one they mean; C hands the address over as ci-vt
; and to-vt, the way it hands over lvm_quote, and the egg mops both noms.
(: (tap cl)
 (poke -1 (peek 0 in) (poke -1 ci-vt (poke -1 -1 (poke 3 cl (spin 4)))))
 (jug _)                                          ; the output dual of `tap`: say INTO it, slurp it back OUT
  ; ⚠ the backing starts EMPTY, and that is the whole isolation: a literal here is
  ; ONE string every jug would share, and to_writen fills a backing IN PLACE -- two
  ; live jugs then write over each other until the first one grows.
  (poke -1 (peek 0 in) (poke -1 to-vt (poke -1 -1 (poke -1 "" (poke 4 0 (spin 5))))))
 ; hot? names the zoo (a buf or a port), the head's ap word the species -- the recognition C's iop
 ; does. hot? goes first so the peek only ever lands on a heap cell.
 (port? o) (? (hot? o) (id? (peek 0 o) (peek 0 in)) 0)   ; ⚠ the else arm is the BIT: a two-armed ? answers the zero point
 ; drain a vessel to a value: a jug reads its backing buffer, everything else A GULP AT A TIME.
 ; the drain accumulates, it does not recurse -- a recursion's stack depth is the gulp count, and
 ; on a parking port every task switch copies that stack.
 ; ⚠ the see/unsee pair is what makes the loop terminate: `see` is what touches the device (chug
 ; never does), and the byte it hands back is one chug is guaranteed to count -- so a gulp is
 ; never empty and the walk always advances.
 (slurp o) (? (? (port? o) (id? to-vt (peek 1 o))) (snip (peek 3 o) 0 (peek 4 o))   ; a jug BY ITS VT: a buffered fd port's word 3 is a string too
              (: j (jug 0)
                 (go u) (: c (see o) (? (= c -1) (slurp j)
                                        (: _ (unsee o c) _ (say j (chug o)) (go ()))))
                 (go ())))
 ; the promise, through a tablet -- the mutation door this tree sanctions. the answer is wrapped
 ; (1 v) and read with two?: a promise of nothing is ordinary (eof gives ()), and a bare pin would
 ; leave the slot reading like an unforced one, so f would re-run forever.
 (once f) (: t (tablet 0)
             (\ u (: v (peep t 0 ())
                     (? (two? v) (cup v) (: w (f ()) _ (pin t 0 (link 1 w)) w)))))
 ; a port is a charlist: the run just gulped is realized, the tail a promise for the next.
 ; the tail must be a `once` -- p1 forces a position twice, and a re-drinking tail would lose the
 ; bytes between. one chug per gulp, and the run is whatever the port has in hand; the `see` is
 ; what refills, since chug never touches the device.
 (flow p) (: (gulp tl) (: c (see p)
                          (? (= c -1) tl
                             (: _ (unsee p c)
                                t (chug p)
                                (go k l) (? (nil? (< 0 k)) l (: j (- k 1) (go j (link (t j) l))))
                                (go (tally t) tl))))
             c (see p)
             (? (= c -1) () (: _ (unsee p c) (gulp (once (\ u (flow p)))))))
 ; flow at one byte per force, for taking a port slowly where a whole run would read too far
 ; ahead. `in` already flows that way: it holds nothing in hand -- and still holds nothing
 ; where the DEVICE gulps behind it (chug asks the port, which is the static, not the run
 ; borrowed under it), so this stays the only door that reads stdin exactly.
 (trickle p) (: c (see p) (? (= c -1) () (link c (once (\ u (trickle p)))))))
(: l (foldr (\ a l (link link (link a (link l ())))) ()) (: _ (:: 'L l) _ (:: 'list l)))
; the false answer is the unit, not 0: (&& a ..) expands to (? a .. ()) and ||'s exhausted tail
; reads (). test a result with ? or id?, not arithmetic.
(:: '&& (\ l (: (and l) (? (two? (cup l)) (list '? (cap l) (and (cup l)) ()) (cap l)) (? (two? l) (and l) 1))))
(:: '|| (\ l (: (or l) (? (two? l) (: y (nom 0) (list ': y (cap l) (list '? y y (or (cup l))))) ()) (or l))))
(:: ':- (\ a (link ': (cat (cup a) (link (cap a) ())))))
(:: '?- (\ a (link '? (cat (cup a) (link (cap a) ())))))
(:: '>>= (\ l (link (last l) (init l))))
(:: '<=< (\ g (: y (nom 0) (list '\ y (foldr (\ f x (list f x)) y g)))))
; (pins t k v ..): pin each pair onto an EXISTING tablet -- the mutable-accumulator twin of #(..),
; which mints a fresh one. an odd trailing operand is dropped (no lone key).
(:: 'pins (\ l (: (go t r) (? (two? (cup r)) (go (list pin t (cap r) (caup r)) (cuup r)) t)
                  (go (cap l) (cup l)))))

(: (** a b) (b a) $ 1 ! nil?  (!= a b) (nil? (= a b)))   ; prefix: infix is not live until seal-hook 4

; lisp compatibility synonyms
(: seqx (\ l (>< ': (foldr (\ l r (>< '_ (>< l r))) (list (last l)) (init l))))
   ifx (>< '?) letx (>< ':) lamx (>< '\ )
   _ (:: ', seqx) _ (:: 'do seqx) _ (:: 'begin seqx) _ (:: 'progn seqx)
   _ (:: 'let letx)
   _ (:: 'cond ifx) _ (:: 'if ifx)
   _ (:: 'quote lamx) _ (:: 'lambda lamx))

; --- the loader: use-on-miss ------------------------------------------------
; the `use` up top splices a registered module; here it also loads one it lacks, from the baked
; lib book or the filesystem walk. a load is one fresh layer: enter, read-eval, leave registers.
; a slashed name includes into the current head instead; a string is an explicit path.
; `open` is captured by peep, so a fs-less frontend holds () there and falls back to the table.
(: use
   (: prior use
      ; read-eval every form of a module source; a torn form scares.
      ; `flow`, never `slurp`: slurp conses the bytes, reverses, re-strings, and sound would cons
      ; them again -- four copies of the module.
      (rdev q) ((: (go cl) (: r (sound cl)
                              (? (two? r) (: _ (ev 'ev (cap r)) (go (cup r)))
                                 (id? r 'torn) (scare 'unfinished-form q)
                                 0)))
                (flow q))
      (slashy? s) ((: (go i) (? (< i (tally s)) (? (= 47 (peep s i 0)) 1 (go (+ i 1))) 0)) 0)
      (mname s) (: n (tally s)
                   b ((: (go k r) (? (< k n) (go (+ k 1) (? (= 47 (peep s k 0)) (+ k 1) r)) r)) 0 0)
                   e (? (? (< 2 (- n b)) (= ".l" (snip s (- n 2) n)) 0) (- n 2) n)
                   (intern (snip s b e)))
      fsopen (peep book 'open ())      ; the host's open nif, by peep -- see above
      ; the walk: the baked table, then lib/ off the cwd -- the dev tree's lane, kept
      ; for a tree and its tools (papel's want, the gates); it too retires when the
      ; tree stops being special. the SEAT rungs (<seat>/../lib/, its love/ subfolder
      ; -- the nest and FHS shapes) retired with the modules arc: an artifact's
      ; modules are baked or image-carried, never found beside the binary.
      (libsrc nm) (: s (lib nm)                                ; the baked table: a read port over the C text
                     (? (port? s) s
                        (nil? fsopen) ()
                        (fsopen (+ "lib/" (+ (string nm) ".l")) "r")))
      (\ m (? (tablet? m) (prior m)
              (string? m)
              (: nm (mname m)
                 (? (tablet? (peep (mods ()) nm ())) (prior nm)
                    (: q (? (nil? fsopen) () (fsopen m "r"))
                       (? (nil? q) (prior m)
                          ; a file whose own (module nm ..) registered wins: keep ITS tablet
                          ; over the loader's wrapper layer, which leave would pin over it.
                          (: _ (enter nm) _ (rdev q) t (peep (mods ()) nm ()) _ (leave ())
                             _ (? (nil? t) 0 (pin (mods ()) nm t))
                             (prior nm))))))
              (tablet? (peep (mods ()) m ())) (prior m)
              (: q (libsrc m)
                 (? (nil? (port? q)) (prior m)                ; every source is a port now, baked or on disk
                    (slashy? (string m)) (: _ (rdev q) ())
                    (: _ (enter m) _ (rdev q) t (peep (mods ()) m ()) _ (leave ())
                       _ (? (nil? t) 0 (pin (mods ()) m t))
                       (prior m))))))))

; --- module: filesystem-free definition --------------------------------------
; (module 'nm form ..): open nm's layer -- the REGISTERED tablet on a reopen, a
; fresh one otherwise -- eval each form there as the loader would, leave
; registers; answers the tablet. text anywhere defines or reopens any module;
; (module () ..) is a scratch layer, registered nowhere. each form compiles at
; ITS eval, after the open, so bindings defglob into the module layer (a form
; compiled early would defglob past it) and a macro form serves the NEXT form,
; not its own. the run rides the expansion as a VALUE: enter/leave are mopped.
(: _
 (: (mopen nm) (: r (? (nil? nm) () (peep (mods ()) nm ()))
                  F (? (tablet? r) r (: t (tablet 0) _ (pin t 0 (tablet 0)) t))
                  _ (pin F 31337 (link (books ()) nm))
                  (setbooks (link F (books ()))))
    (mrun nm fs) (: _ (mopen nm) _ (each fs (\ f (ev 'ev f))) (leave ()))
    ; ⚠ the quote takes the forms LIST whole -- (\ (f ..)), one element PER FORM.
    ; an extra (link .. ()) here once made the body ONE application chain: every
    ; form's effects still ran (the gates stayed green), but each answer was
    ; APPLIED to the next -- caught only when a bodyless (: answered a function.
    (:: 'module (\ l (list mrun (cap l) (list '\ (cup l)))))))


; --- reader operators: the operators tablet + the opfix factor pass ---------
(: operators (pins (tablet 0)
 0 (pins (tablet 0)                                 ; --- ARITY 0: glued. takes nothing from the form
 '< 'cap '> 'cup '+ 'net '* 'prod '| 'abs '- 'negate '/ 'reciprocal
 '% 'fraction '? 'bit '$ 'saturate '! 'nil? '^ 'jot   ; `.` is pinned here by post.l, whose dot it is
 '0? 'nil? '1? 'one? '2? 'two?)                ; the arity predicates: a row is a WORD, so it may
                                               ; spell digits -- 2?<>x is (two? (cap (cup x)))
                                               ; ⚠ ^ ALSO rides arity 2 as xor: the lane is the
                                               ; key, so glued ^x counts up and spaced a ^ b does
                                               ; not -- $ is the same two-lane sigil (saturate/apply)
 1 (tablet 0)                                       ; --- ARITY 1: a right operand and no left. no walk yet
 2 (pins (tablet 0)                                 ; --- ARITY 2: spaced. a left operand and a right
 '** 70
 '* -60 '/ -60 '% -60 '// -60                       ; multiplicative, LEFT -- the MINUS is the hand
 '+ -50 '- -50                                      ; additive
 '<< -47 '>> -47                                    ; shifts
 '& -46 '^ -45 '| -44                               ; bitwise, C's order
 '&& 31 '|| 30                                      ; logical, below comparison; also pat.l's guard and alternative
 '>< 25                                             ; cons -- the loosest builder
 '$ 5 '\ 6)                                         ; weak apply (a $ b = (a b), haskell $; glued $x stays saturate), and the
                                                    ; lambda a hair ABOVE it, so ([a b] \ a * b $ [2 3]) closes the body at
                                                    ; the $ and then applies -- at one band the right hand ate the apply
 -1 (pins (tablet 0)                                ; --- ARITY -1: spaced, no bound. a left, then all of them
 '? 10 '@ 10                                        ; the CLAUSE forms: cond, and match -- every arm stays an operand
 ': 3 ':= ':)                                       ; the binder, loosest of all; := is a bare nom, so it NAMES
                                                    ; `:` and takes its band -- an alias cannot drift from it
 -2 (pins (tablet 0)                                ; --- ARITY -2: spaced, and it CHAINS. a left and a right, but
 '= 40 '!= 40 '< 40 '<= 40 '> 40 '>= 40)))          ; a same-band neighbour JOINS the run instead of nesting
 ; ⚠ the rows are NOMS, not values: a value would be unwritable, and an unwritable head is a
 ; form no text can spell -- which is what keeps opfix invertible.

;;; FIXME the code below is rather old and hoary and can probably be freshened up.
;;; remember we can't use operators or pattern matching yet here though.
;;; catalog all the op-* functions,
(: ; the lanes, by their arity key. LOCAL: the tablets are the ones in `operators`, not copies,
   ; so a `grip` write through the door is seen here at once and there is nothing beside it.
   monadics (operators 0) dyadics (operators 2) naries (operators -1) chains (operators -2)
   (op-punc-byte c) (? (&& (<= 97 c) (<= c 122)) 0
                       (&& (<= 48 c) (<= c 57)) 0
                       (&& (<= 65 c) (<= c 90)) 0
                       (= c 95) 0
                       1)
   ; cheap prefilter: only an all-punct symbol can factor. the four ranges above are disjoint and
   ; run commonest-first, so don't reorder them.
   (op-puncp x)
    (? (nom? x)
       (: s (string x)
          (? (string? s) ;;; FIXME string should *always* return a string, if !(string? (string x)) that's a bug
             (: n (tally s)
                (? (< 0 n)
                   ((: (go i) (? (< i n) (? (op-punc-byte (s i)) (go (+ 1 i)) 0) 1)) 0)
                   0))
             0)) ;;; FIXME dangling 0 clauses like these are bad end-weight coding and
       0) ;;; they may not even be semantically needed since ? fall-through returns ()

   ; a BAND is a SIGNED charm: the magnitude is the level (higher binds tighter) and the sign is
   ; the hand -- positive right, negative LEFT. the arithmetic, shift and bitwise bands are the
   ; left-handed ones, so the minus marks them and the hand stops being a slot anyone counts to.
   ; ⚠ zero can never be a band: a tablet MISS answers 0, and a 0 band would read that miss as a
   ; live row at the loosest level there is.
   (op-band? b) (&& (charm? b) (nil? (= 0 b)))   ; ⚠ prefix-only region: != is not a function yet
   (op-row nm b ar) (>< nm (>< (abs b) (>< (? (< b 0) 1 0) ar)))       ; -> (name band hand . arity)
   ; an alias tail. ⚠ SOURCE can only spell the list form -- love has no dotted-pair syntax, so a
   ; written (pin . 20) is the three-element list (pin . 20) -- but a pair BUILT with >< reads too.
   (op-tail v) (? (charm? (cup v)) (cup v) (&& (two? (cup v)) (charm? (caup v))) (caup v) 0)
   ; k's row in the SPACED lane t of arity ar -> (name band hand . ar), or 0. the row is read BY
   ; KIND, three shapes and no positions: a CHARM is the band and the sigil names itself; a NOM is
   ; an ALIAS -- the sigil means that word and takes its band, ONE HOP, so no alias can drift from
   ; its target and no cycle can be spelled; a (nom band) pair is an alias carrying a band of its
   ; own, for a target that has no row to lend (`pin` and its kind).
   (op-lane k t ar)
    (: v (t k)
       (? (op-band? v) (op-row k v ar)
          (nom? v) (: b (t v) (? (op-band? b) (op-row v b ar) 0))
          (? (two? v) (nom? (cap v)) 0) (: b (op-tail v) (? (op-band? b) (op-row (cap v) b ar) 0))
          0))
   ; the SPACED lanes, TIGHTEST FIRST -- so a nom sitting in two reads as the earliest. arity 1
   ; has no walk, so it is never asked and a row there stays inert.
   (op-ent k) (: d (op-lane k dyadics 2)
                 (? (two? d) d (: c (op-lane k chains -2) (? (two? c) c (op-lane k naries -1)))))
   (op-chain? k) (two? (op-lane k chains -2))
   ; factor nm[0..j) against the monadic table onto acc -> the word list (outermost first), or 0.
   ; ⚠ RIGHT TO LEFT: the sigil the reader glued to the datum matches first, longest first, so on
   ; "abc" rows `a` `bc` beat `ab` `c`. single-char rows agree either way, which is why the stock
   ; table cannot tell the directions apart. ⚠ AND IT BACKTRACKS: a split whose leftover
   ; dead-ends falls back to the next-shorter sigil, so a run factors iff SOME split exists.
   ; that completeness is what a dead end cannot buy -- an unfactorable run must fall through
   ; to the plain application (data and coined heads ride that), so there is no room to report
   ; one, and a fresh `grip 0` row would silently unfactor runs that used to work.
   ; the table holds word values, so a hit is asked out of band against the miss answer --
   ; a truth test would drop any word that nets nothing.
   (mono-go nm j acc)
    (? (< 0 j)
       ((: (try k)
           (? (< k 1) ()
              (: e (monadics (intern (snip nm (- j k) j)))
                 (? (id? () e) (try (- k 1))
                    (: r (mono-go nm (- j k) (>< e acc)) (? (two? r) r (try (- k 1))))))))
        j)
       acc)
   (monofactor s) (: nm (string s) (mono-go nm (tally nm) ()))
; the walk. a pending-operator frame is (orig name need band . got): the source symbol, the
; resolved name, the operands still wanted, the captured band, and the operands so far, reversed.
; need 0 means filled with the fold deferred, so a later operator can steal the last operand --
; that deferral is right-associativity. -1 is that steal already spent, the right operand growing
; as a span; -2 is N-ARY, which never fills and so never spans. out/pend thread as (out . pend).
   (op-fr o nm nd g gt) (>< o (>< nm (>< nd (>< g gt))))
   (op-fro f) (cap f) (op-frn f) (caup f)
   (op-frd f) (cap (cuup f)) (op-frband f) (cap (cuuup f)) (op-frg f) (cup (cuuup f))
   ; a frame is only ever rebuilt with a new need + got -- orig, name and band are fixed the
   ; moment the operator is read.
   (op-frw f nd g) (op-fr (op-fro f) (op-frn f) nd (op-frband f) g)
   ; wrap a chain of monadic words around a datum, outermost first
   (op-fin ch x) (? (two? ch) (>< (cap ch) (>< (op-fin (cup ch) x) ())) x)
   ; fold a collected span (out, reversed) into ONE operand: a lone datum stays itself, two-or-more
   ; become the application (juxtaposition binds tighter than any infix operator).
   (op-span out) (? (two? (cup out)) (rev out) (cap out))
   ; the two BINDERS fold back through op-core, so an infix spelling lowers exactly as the prefix
   ; one does -- patterns to noms, the nested lambda flattened, the body factored. nothing is
   ; added: (a : b) is the plain (: a b), body-less and leaking its binding, as it reads.
   ; ⚠ a `:` CHAIN IS ONE SCOPE: the nesting op-steal builds is spliced flat here, so
   ; (a : 1 b : 2 (a + b)) and (: a 1 b 2 (a + b)) are the same form, and a chain of bare
   ; definitions is one body-less `:` carrying every pair. it is the BODY position that splices,
   ; however that body was spelled -- and a wholly prefix `:` never reaches this fold, so no
   ; extant form moves.
   (op-cl-splice fs)
    (? (atom? (cup fs))
       (: b (cap fs)
          (? (&& (two? b) (id? (cap b) ':)) (cup b) fs))
       (>< (cap fs) (op-cl-splice (cup fs))))
   ; a CHAIN, arity -2: (a < b <= c) is (a < b) AND (b <= c), and the shared operand is evaluated
   ; ONCE. the binds NEST INSIDE the conjunction so all three of python's promises hold at once --
   ; left to right, once each, and && still short-circuits, so a false first link never reaches c.
   ; the operators may differ per link; each carries its own. ⚠ the emission is a `:` and an `&&`
   ; and stops -- && is a macro `feel` expands later, which is where the laziness comes from.
   ; os is (a b op c op d ..): the head operator is the frame's name, every later one is inline.
   ; ⚠ ONLY A COMPOUND OPERAND IS BOUND. an atom -- a nom, a numeral, a string -- has no effect
   ; and no cost to read twice, so binding it would buy nothing and spend an UNWRITABLE nom: a
   ; fresh nom has no spelling, and a factored form carrying one is a form no text can say
   ; (the same reason the table's rows are noms). so (a < b < c) stays plain.
   (op-chn-rest pv r)                                        ; r = (op z op z ..), or a dangling op
    (? (atom? r) 0
       (: op (cap r)
          (? (atom? (cup r)) (>< op (>< pv ()))              ; a trailing operator: the curry law, per link
             (: z (caup r) v (? (atom? z) z (nom 0))
                e (>< op (>< pv (>< v ()))) t (op-chn-rest v (cuup r))
                bd (? (two? t) (>< '&& (>< e (>< t ()))) e)
                (? (id? v z) bd (>< ': (>< v (>< z (>< bd ())))))))))
   ; ⚠ the whole run wears a `bit`, because A COMPARISON ANSWERS A BIT and a chain is still one.
   ; `&&` hands back () for a false LEFT and the failing operand otherwise, so a bare chain would
   ; answer () when the first link failed, 0 when a later one did, and 1 when none did -- three
   ; faces for one predicate. the clamp is outside the binds, so nothing about the short-circuit
   ; or the evaluation order moves; only the answer is normalized.
   (op-chn nm os)
    (: a (cap os) b (caup os) u (? (atom? a) a (nom 0)) v (? (atom? b) b (nom 0))
       e (>< nm (>< u (>< v ()))) t (op-chn-rest v (cuup os))
       bd (? (two? t) (>< '&& (>< e (>< t ()))) e)
       b2 (? (id? v b) bd (>< ': (>< v (>< b (>< bd ())))))
       b3 (? (id? u a) b2 (>< ': (>< u (>< a (>< b2 ())))))
       (>< 'bit (>< b3 ())))
   ; fold a frame with what it has: nothing collected gives the plain symbol, a shortfall the
   ; partial application -- the curry law, with no case of its own.
   ; kind tests throughout the walk, never truth -- an operand list like (0) or ("") nets zero.
   ; ⚠ a chainable frame with only its two operands is a PLAIN comparison and folds as one -- the
   ; chain is the join's doing, and it shows as the interleaved operators the join left in `got`.
   (op-fold f) (? (atom? (op-frg f)) (op-fro f)
                  (: nm (op-frn f) os (rev (op-frg f))
                     (? (id? nm '\) (op-core (>< '\ os))
                        (id? nm ':) (op-core (>< ': (op-cl-splice os)))
                        (&& (two? (cup os)) (&& (two? (cuup os)) (op-chain? nm))) (op-chn nm os)
                        (>< nm os))))
   ; deliver datum d -> (out' . pend'). a collecting frame takes it; a filled infix frame extends
   ; its right operand into a span, since juxtaposition binds tighter: (a = b c) is (= a (b c)).
   ; the span state rides `need`: 0 = a lone datum, -1 = growing.
   (op-del d out pend)
    (? (atom? pend) (>< (>< d out) pend)
       (: f (cap pend) nd (op-frd f)
          (? (= nd -2) (>< out (>< (op-frw f -2 (>< d (op-frg f))) (cup pend)))  ; n-ary: a new operand, always
             (< 0 nd)  (>< out (>< (op-frw f (- nd 1) (>< d (op-frg f))) (cup pend)))
             (: op (cap (op-frg f))                          ; the frame's last (right) operand
                nx (? (= 0 nd) (>< op (>< d ()))         ; lone -> wrap into (op d)
                      (cat op (>< d ())))                  ; span -> append d
                f2 (op-frw f -1 (>< nx (cup (op-frg f))))
                (>< out (>< f2 (cup pend)))))))
   ; take a left operand for an incoming operator of band g and hand h -> (. (out' . pend')), or 0.
   ; against a filled top frame the incomer either steals its last operand or folds it and retries
   ; beneath. at equal band the incomer's hand decides: right-handed steals, left-handed yields.
   ; at top level the whole leading span is taken: (a b = c) captures (a b), not just b.
   ; ⚠ an n-ary frame is never filled, so it yields a RIGHT operand or nothing: its left is the
   ; scrutinee, and a frame holding only that has none to give -- the same refusal an unfilled
   ; dyadic frame already makes, and what keeps (c ? a + b) reading (? c (+ a b)) instead of
   ; swallowing the test. a looser operator still folds it, from the branch above.
   (op-steal g h out pend)
    (? (two? pend)
       (: f (cap pend) nd (op-frd f)
          (? (< nd 1)
             (? (? h (<= g (op-frband f)) (< g (op-frband f)))
                (: r (op-del (op-fold f) out (cup pend)) (op-steal g h (cap r) (cup r)))
                (? (? (= nd -2) (atom? (cup (op-frg f))) 0) 0
                   (>< (cap (op-frg f))
                         (>< out (>< (op-frw f (? (= nd -2) -2 1) (cup (op-frg f))) (cup pend))))))
             0))
       (two? out) (>< (op-span out) (>< () pend))
       0)
   ; fold every pending frame TIGHTER than an incoming (g,h) and answer (out . pend) -- the climb
   ; op-steal makes before it can take anything, split out so the chain JOIN can see the frame it
   ; will actually meet. ⚠ an operator BETWEEN two links is such a frame: (a < b + c < d) reaches
   ; the second `<` with the additive frame on top, and without this the join never fires.
   ; op-steal repeats the climb and finds nothing left to do, so nothing else changes.
   (op-climb g h out pend)
    (? (&& (two? pend) (&& (< (op-frd (cap pend)) 1)
                           (? h (<= g (op-frband (cap pend))) (< g (op-frband (cap pend))))))
       (: r (op-del (op-fold (cap pend)) out (cup pend)) (op-climb g h (cap r) (cup r)))
       (>< out pend))
   ; drain at end of list: fold every pending frame innermost-out
   (op-drain out pend)
    (? (atom? pend) (rev out)
       (: r (op-del (op-fold (cap pend)) out (cup pend))
          (op-drain (cap r) (cup r))))
   (op-w l out pend)
    (? (atom? l) (op-drain out pend)
       (: e (cap l)
          (? (op-puncp e)
             ; a spaced sigil is ONE name -- an unrowed one takes the house band, and with no
             ; left operand the infix frame still folds to the plain symbol (op-fold's bare case)
             (: en0 (op-ent e)
                en (? (two? en0) en0 (>< e (>< 95 (>< 0 2))))   ; fresh punct: (e house right . arity 2)
                nm (cap en) gp (caup en) hd (cap (cuup en))
                ; ⚠ fresh noms: this `:` is ONE SCOPE, so rebinding out/pend here would make the
                ; op-climb call read its own later binding and raise `missing out`.
                cl (op-climb gp hd out pend) ou (cap cl) pd (cup cl)   ; fold what is tighter, THEN look
                st (op-steal gp hd ou pd)   ; capture a left operand if one exists (at this band + hand)
                ; ⚠ THE JOIN, and it is the only place a chain can be seen. a chainable sigil
                ; meeting a FILLED chainable frame at its own band does not steal the left operand
                ; and nest -- it pushes its own nom into that frame's got and asks for one more,
                ; so the run stays flat and op-fold reads the whole of it. a parenthesized
                ; (a < (b < c)) arrives as ONE folded datum and never enters this list, which is
                ; exactly why it stays the nested comparison -- the escape hatch is the parens,
                ; and the factored form alone could never have told the two apart.
                ; ⚠ FILLED is op-steal's test (need below 1), not need 0: a right operand grown
                ; into a span rides -1, and (97 <= s 0 <= 122) is that. a chainable frame never
                ; carries -2 -- op-ent reads chains before naries, so its need starts at 1.
                (? (&& (= -2 (cup (cuup en)))
                       (&& (two? pd)
                           (&& (< (op-frd (cap pd)) 1)
                               (&& (= gp (op-frband (cap pd))) (op-chain? (op-frn (cap pd)))))))
                    (op-w (cup l) ou (>< (op-frw (cap pd) 1 (>< nm (op-frg (cap pd)))) (cup pd)))
                   (two? st) (op-w (cup l) (cap (cup st))
                            (>< (op-fr e nm (? (= -1 (cup (cuup en))) -2 1) gp (>< (cap st) ()))   ; -2 = n-ary: the row's LANE, so an alias inherits it
                                  (cup (cup st))))
                   (: r (op-del e ou pd)     ; no left operand: the plain symbol
                      (op-w (cup l) (cap r) (cup r)))))
             (: r (op-del (op-core e) out pend)
                (op-w (cup l) (cap r) (cup r))))))
   ; a `:` binds, it does not apply, so opfix must not rearrange it. binders stay verbatim and only
   ; each pair's value and the trailing body factor. the pairing matches eval's exactly.
   (op-colon fs)
    (? (atom? fs) fs
       (atom? (cup fs)) (>< (op-core (cap fs)) ())        ; a lone form is the body
       (: r (op-dpb (cap fs))                             ; define-sugar binds a \ too: lower its patterns
          (>< (cap r)                                     ; the name: verbatim, its params lowered
                (>< (op-core (op-wrap (cup r) (caup fs))) ; the value: wrapped where they were, then factored
                      (op-colon (cuup fs))))))              ; the rest: pairs on
   ; a \ lambda is a binder too: the args stay verbatim, only the body factors.
   ; (\ x) -- one operand -- is quote, kept whole by op-core's first clause.
   (op-lam fs)
    (? (atom? (cup fs)) (>< (op-core (cap fs)) ())        ; the last operand is the body
       (>< (cap fs) (op-lam (cup fs))))                    ; an arg: verbatim, recurse
   ; ⚠ A PARAM THAT IS NOT A NOM IS A PATTERN, and the body takes it apart: (\ [a b] x) is
   ; (\ v (@ v [a b] x)), v fresh. Nothing is displaced -- such a param used to bind NOTHING
   ; (the body read `missing a`), so no code can have meant anything by one.
   ; ⚠ it emits the `@` SYMBOL and stops: pat is a module and expands it at the reader's own
   ; compile, so neither compiler learns what a pattern is. A seat with no pat spliced has an
   ; unbound `@` -- which is the seat's tiering answering, not this pass's business.
   ; ⚠ and it runs BEFORE the nested-lambda flatten, so that pass still only ever sees noms
   ; and its shadow check stays a comparison of names. (\ [a b] (\ a x)) must not merge.
   (op-wrap ws b) (? (atom? ws) b
                     (op-wrap (cup ws) (>< (intern "@") (>< (cap (cap ws)) (>< (caup (cap ws))
                                                       (>< b ()))))))
   (op-depat ol ws)
    (? (atom? (cup ol)) (>< (op-wrap ws (cap ol)) ())          ; the body: wrapped outermost-last
       (: p (cap ol)
          (? (nom? p) (>< p (op-depat (cup ol) ws))
             (: v (nom 0) (>< v (op-depat (cup ol) (>< (>< v (>< p ())) ws)))))))
   ; the same over a param list with NO body -- answers (params . wraps), for the binder below.
   (op-dp ol ws)
    (? (atom? ol) (>< () ws)
       (: p (cap ol)
          (? (nom? p) (: r (op-dp (cup ol) ws) (>< (>< p (cap r)) (cup r)))
             (: v (nom 0) r (op-dp (cup ol) (>< (>< v (>< p ())) ws))
                (>< (>< v (cap r)) (cup r))))))
   ; a define-sugar binder IS a lambda: (: (f [a b]) V ..) is (: (f v) (@ v [a b] V) ..).
   ; ⚠ POSITION 0 IS NEVER A PATTERN -- it is the name, or (curried) the binder that carries it --
   ; so every LATER position at every level is a param and lowers. That is also why the two
   ; features cannot collide: `:` does not match patterns in an assignment, only under a binder.
   ; ⚠ a curried binder's levels are separate argument positions, and the inner binder's params
   ; are the EARLIER arguments -- so its wraps go OUTSIDE this level's. ws is innermost-first.
   (op-dpb b)
    (? (atom? b) (>< b ())
       (: hr (op-dpb (cap b)) pr (op-dp (cup b) ())
          (>< (>< (cap hr) (cap pr)) (cat (cup pr) (cup hr)))))
   ; flatten a directly-nested lambda body -- (\ a (\ b B)) -> (\ a b B) -- so neither compiler
   ; builds the extra closure. shadowing stays nested: (\ x (\ x x)) must not merge.
   (op-mem? y l)      (? (atom? l) 0 (id? y (cap l)) 1 (op-mem? y (cup l)))
   (op-disjoint? a b) (? (atom? b) 1 (op-mem? (cap b) a) 0 (op-disjoint? a (cup b)))
   (op-params ol)     (? (atom? (cup ol)) () (>< (cap ol) (op-params (cup ol))))
   ; ⚠ the inner lambda LOWERS BEFORE the shadow check, never after the merge: a pattern carried up
   ; as a param binds nothing, and the check compares noms -- (\ [a b] (\ a x)) must not merge.
   (op-flat-ol ol seen)                                      ; -> the flattened operand list, body left RAW (op-lam factors it after)
    (? (atom? (cup ol))
       (: b (cap ol)
          (? (&& (two? b) (id? (cap b) '\) (two? (cuup b)))  ; (two? (cuup b)) = a real lambda, not quote
             (: ol2 (op-depat (cup b) ())
                (? (op-disjoint? seen (op-params ol2)) (op-flat-ol ol2 seen) ol))   ; disjoint inner lambda -> merge its operands up
             ol))                                             ; else the body stays raw
       (>< (cap ol) (op-flat-ol (cup ol) (>< (cap ol) seen))))   ; a param: keep + accumulate for the shadow check
   ; the comma layer, the loosest grouping, above infix. datums split at top-level commas and the
   ; whole becomes the `,` macro: (a, b c) -> (: _ a (b c)); empty clauses drop. the comma outranks
   ; `:`, so a binder spine splits like any list. the symbol is interned, never written literally.
   (op-cm? l)   (? (atom? l) 0 (id? (cap l) ',) 1 (op-cm? (cup l)))
   (op-take l)  (? (atom? l) () (id? (cap l) ',) () (>< (cap l) (op-take (cup l))))
   (op-drop l)  (? (atom? l) () (id? (cap l) ',) (cup l) (op-drop (cup l)))
   (op-split l) (: c (op-take l) r (? (op-cm? l) (op-split (op-drop l)) ())
                   (? (two? c) (>< c r) r))
   ; op-core: the factor pass. quote interiors stay verbatim, binding forms keep their binders,
   ; everything else walks. idempotent. has-top-op? asks whether the original form carried a
   ; top-level operator -- only then may a lone folded operand splice up, since a bare
   ; list-of-one like ((mov r3 r12)) is data and stays wrapped.
   (has-top-op? x) (? (atom? x) 0 (op-puncp (cap x)) 1 (has-top-op? (cup x)))
   (op-core x)
    (? (atom? x) x
       (: h (cap x)
          (? (&& (id? h '\) (atom? (cuup x))) x              ; (\) / (\ x): quote, verbatim
             (op-cm? (cup x))                                ; the comma layer (helpers above)
              (: cs (op-split x)
                 (? (atom? cs) ()                            ; commas alone: nothing
                    (atom? (cup cs)) (op-core (cap cs))      ; one clause: the commas composed away
                    (>< (intern ",") (map op-core cs))))
             (id? h '\) (>< '\ (op-lam (op-flat-ol (op-depat (cup x) ()) ())))   ; (\ a.. body): patterns to noms, flatten nested lambdas, factor the body
             (id? h ':) (>< ': (op-colon (cup x)))         ; (: n v ..): binders verbatim
             ; the reader's glued-run emission (mono (run d)): factor the run monadically around
             ; the (recursively opfixed) datum; an unfactorable run is the plain application (run d)
             (&& (id? h 'mono) (two? (cup x)) (two? (caup x)))
              (: p (caup x) r (cap p) d (op-core (caup p))
                 fs (? (nom? r) (monofactor r) 0)
                 (? (two? fs) (op-fin fs d) (>< r (>< d ()))))
             (: res (op-w x 0 0)                        ; splice a lone FOLDED operand up (an infix/prefix-op form)
                (? (&& (has-top-op? x) (two? res) (atom? (cup res)) (two? (cap res))) (cap res) res))))) ; (a = b) -> (= a b), not ((= a b))
   (opfix x) (op-core x)

   ; the grip door: the one sanctioned write into the compile table, and the only reach to
   ; `operators` once birth mops the nom. ONE signature at every lane, (grip ar nm v), because a
   ; curried door of two arities would hand back a closure and write nothing -- and it is curried,
   ; so (grip 2) is the dyadic lane's own door and (grip 0) the glued one, which the sigil map never
   ; had. it answers the row it replaced in the shape it takes, so (grip ar nm (grip ar nm new))
   ; restores exactly; () clears a row. a shape the lane refuses rolls back and scares, and a lane
   ; with no key is refused rather than minted -- a lane the walk never asks is a silent no-op
   ; wearing a live row's face. the table is read live.
   ; ⚠ the GLUED lane accepts anything: its row becomes the emitted head, so a closure there evals
   ; perfectly well and only costs the round trip -- opfix's output stops being spellable for that
   ; sigil, and libra's two directions with it. a nom is what the tree writes; the rest is yours.
   (op-lane-ok? t ar nm) (? (= ar 0) 1 (two? (op-lane nm t ar)))
   (grip ar nm v)
    (: t (operators ar)
       (? (nil? (nom? nm)) (scare 'bad-grip nm)
          (nil? (tablet? t)) (scare 'bad-grip ar)
          (: old (peep t nm ())
             ; ⚠ IDENTITY, never truth: a left-handed band is a NEGATIVE charm, which nets red --
             ; (nil? -60) is true, and this door would read every left-handed row as a clear.
             (? (id? () v) (: _ (pull t nm 0) old)                   ; () clears the row
                (: _ (pin t nm v)
                   (? (op-lane-ok? t ar nm) old                      ; the lane's own read is the acceptor
                      (: _ (? (id? () old) (pull t nm 0) (pin t nm old))
                         (scare 'bad-grip nm))))))))

   ; --- this `:` has a body, so the helpers above are local ------------------
   ; opfix and grip are the door; the rest is the factor pass's scaffolding.
   ; `operators` and `pureset` stay global-and-mopped instead: ev.l holds them, and a
   ; cross-file capture has only the book to go through.
   _ (pin book 'grip grip)
   (pin book 'opfix opfix))
; the LAST seal: opfix joins the other four in the g->hot_* fields, so no later rebind can reach it.
(seal-hook 4 opfix)

; --- assert / suite: the strict checkers, baked so they work with no harness ---
;;; FIXME how are these used at the moment?
(:: 'assert (\ l (>< 'do (map (\ e (list '? e 0 (list 'scare (list '\ 'assert) (list '\ e)))) l))))
(:: 'suite (\ l (>< 'do (>< (>< 'assert (cup l)) (list (list 'puts (cap l)) (list 'puts ": ok\n"))))))


; --- array element-type inference + shape coercion (shared by `tuple` and `array`).
; a-type answers a witness star of the highest tier present -- an inhabitant, not a code, since
; the generic `tray` ctor reads the tier off a witness. past the numeric tower the whole array
; lifts to an object array. a shape dimension may be any numeric and saturates to a nat.
(: (a-rank x) (? (gem? x) 1 (| (charm? x) (sun? x)) 0 (twin? x) 2 3)
   (a-emax m x) (: k (a-rank x) (? (m < k) k m))
   (a-type d) (: k (foldl a-emax 0 d) (? (k = 0) 0 (k = 1) 0.0 (k = 2) (twin 0 0) ()))   ; a witness star of the tier
   (a-dim d) (? (two? d) (tally d) (saturate d))
   (a-shape s) (? (two? s) (map a-dim s) (list (a-dim s))))

;;; FIXME we new names for these. tuple isn't too bad but "hash" for the tablet constructor is wrong
; tuple/hash value constructors -- also the read-back targets of the printed @(…) / #(…)
(: (spread l) (tray (a-type l) (list (foldl (\ n _ (1 + n)) 0 l)) l))
(:: 'tuple (\ a (list 'spread ('list >< a))))
; (hash x) boxes any non-list: a fresh tablet pinning x at 0, so #5 = #(0 5) and #() IS #0.
; an odd trailing operand lands in that 0 slot.
(: (hashtx a) (? (atom? a) '(tablet 0)
                 (atom? >a) (list 'pin (hashtx 0) () <a)
                 (list 'pin (hashtx >>a) <a <>a)))
(:: 'hash (\ a (hashtx a)))

; (array shape elem…): the N-D constructor. shape is a dimension list or a lone number; the
; element type is inferred, so exactness is never silently lost. the shape fixes the count, so
; the elements are ordinary trailing args and `array` curries them one at a time.
(: (a-coll sh n acc) (? (< n 1) (: d (rev acc) (tray (a-type d) sh d))
                        (\ x (a-coll sh (n - 1) (x >< acc))))
   (array s) (: sh (a-shape s) (a-coll sh (foldl (*) 1 sh) 0)))

; recursive-value boxing for `:` -- the letrec* "capture by location" fix. a reference at-or-before
; its own binding is forward, and indirects through a cell keyed by the binding site's nom, so a
; pre-fill read is a map miss = the missing condition. it emits nif values, not names, so the
; egg's mop cannot break it. 0 when nothing needs boxing, else (prs' . body').
(: (lambp x) (? (two? x) (&& (id? '\ (cap x)) (two? (cuup x)))) ; \ with >=1 param (one-operand \ is quote)
   (dsug n d) (? (atom? n) (link n d) (dsug (cap n) (link '\ (cup n + list d))))
   (lp x) (: a (cup x) (? (atom? (cup a)) (0 >< cap a) (init a >< last a)))
   (ln bs) (? (atom? bs) 0 (atom? (cup bs)) 0 (link (cap (dsug (cap bs) 0)) (ln (cuup bs))))
   ; w/wl: does x / binding-list bs reference symbol v? \ and : shadow, one-operand \ is quote,
   ; macros expand. u tracks lambda depth; dm counts only references NOT under a lambda.
   (w v x bnd u dm) (?
    (nom? x) (&& (? dm (nil? u) 1) (id? x v) (nil? (member? v bnd)))
    (atom? x) 0
    (: h (cap x) (?
      (id? h '\) (? (atom? (cuup x)) 0 (: r (lp x) (w v (cup r) (cat (cap r) bnd) 1 dm)))
      (id? h ':) (wl v (cup x) (cat (ln (cup x)) bnd) u dm)
      (: m (? (nom? h) (macros h) 0) (? (&& m (nil? (member? h bnd))) (w v (m (cup x)) bnd u dm) (any (\ e (w v e bnd u dm)) x))))))
   (wl v bs bnd u dm) (?
    (atom? bs) 0
    (atom? (cup bs)) (w v (cap bs) bnd u dm)
    (: nd (dsug (cap bs) (caup bs)) (? (w v (cup nd) bnd u dm) 1 (wl v (cuup bs) bnd u dm))))
   ; a name wants a cell when something that runs BEFORE its slot is filled can read it. two
   ; doors: a direct read from a binding at or before it (for a lambda name, only one not
   ; shielded by a \ -- a call comes after the pin), and, for a VALUE name, a read from
   ; anywhere the earlier bindings REACH through the let's own names. the second door is
   ; capture, not evaluation order: an earlier closure inherits a later sibling's captures, so
   ; it wants the value's slot at CONSTRUCTION, before that slot exists.
   (boxset prs) (: nm (map cap prs)
     ; bref: ONE walk of a def collecting every sibling name it reads, each tagged with whether
     ; the read sat under a \ (u) -- w's question asked for the whole row at once, so the pass
     ; costs a walk per binding where the name-at-a-time form cost a walk per pair. \ and : bind,
     ; a one-operand \ is quote, macros expand -- except a head THIS let binds, which is a
     ; variable and never a macro (boxfix-core's sub reads it the same way).
     (bref x bnd u acc) (?
      (nom? x) (? (&& (member? x nm) (nil? (member? x bnd))) (link (link x u) acc) acc)
      (atom? x) acc
      (: h (cap x) (?
        (id? h '\) (? (atom? (cuup x)) acc (: r (lp x) (bref (cup r) (cat (cap r) bnd) 1 acc)))
        (id? h ':) (brefl (cup x) (cat (ln (cup x)) bnd) u acc)
        (: m (? (nom? h) (macros h) 0)
           (? (&& m (nil? (member? h bnd)) (nil? (member? h nm))) (bref (m (cup x)) bnd u acc)
              (foldl (\ a e (bref e bnd u a)) acc x))))))
     (brefl bs bnd u acc) (?
      (atom? bs) acc
      (atom? (cup bs)) (bref (cap bs) bnd u acc)
      (: nd (dsug (cap bs) (caup bs)) (brefl (cuup bs) bnd u (bref (cup nd) bnd u acc))))
     es (map (\ p (link (cap p) (bref (cup p) 0 0 ()))) prs)   ; the read graph, source order
     (reads m) (: e (assoc m es) (? (two? e) (cup e) ()))
     (anyname m) (map cap (reads m))                           ; read anywhere
     (barename m) (map cap (filter (\ r (nil? (cup r))) (reads m)))   ; read NOT under a \
     (grow s) (: s2 (foldl (\ a m (cat a (filter (\ k (nil? (member? k a))) (anyname m)))) s s)
                  (? (= (tally s2) (tally s)) s (grow s2)))
     ; near: a read from a binding at or before v. a lambda name takes only the bare ones --
     ; a call comes after the pin -- a value name every one.
     (near v) (: d (lambp (cup (assoc v prs)))
                 (any (\ m (member? v (? d (barename m) (anyname m))))
                      (take (1 + lidx v nm) nm)))
     ; reach: v is read from anywhere the bindings BEFORE it get to through the let's own
     ; names -- capture, not evaluation order. an earlier closure inherits a later sibling's
     ; captures, so it wants v's slot at CONSTRUCTION, before that slot exists.
     (reach v) (member? v (grow (catmap anyname (take (lidx v nm) nm))))
     (filter (\ v (? (near v) 1 (lambp (cup (assoc v prs))) 0 (reach v))) nm))
   (mkcs bx) (map (\ v (link v (link (nom 0) (nom v)))) bx)
   (cellbinds cs) (map (\ c (caup c >< list tablet 0)) cs)
   ; the cell read boxfix emits. a filled cell answers its value; a pre-fill read is a map miss, so
   ; the missing condition carries the binding-site nom and the help's result is the value there.
   ; with no help heard it answers the zero point instead of raising -- a cell miss stays
   ; survivable. `sent` is minted once and closure-private, so nothing a cell holds poses as absence.
   cellread (: sent (mint 0)
               (\ c nm (: v (peep c nm sent)
                          (? !(id? v sent) v
                             !(heard ()) ()
                             (scare 'missing nm))))))

;;; FIXME help/io setters below here. redesign the API with more structure
; binary?: does f take exactly two operands? read two ways -- the arity off the n-ary closure
; marker, else the param count off the source a compiled lambda stashes. ⚠ ONE-SIDED on purpose:
; a shape neither reads (a nif, a partial) PASSES.
; ⚠ the marker is read off a known binary rather than named: `peek`'s nom dies at the hatch but
; the folded reference lives. ⚠ and it must be lvm_cur -- the ap TABLE has it. binding lvm_ret0
; the same way (to catch a one-armed nif) segfaults the image bake, so the nif case stays open.
(: hcur (peek 0 (\ a b a))
   (binary? f) (? (charm? f) 0                      ; a charm is no handler at all
                  (id? hcur (peek 0 f)) (= 2 (peek 1 f))
                  (: s (lamsrc f) (? (two? s) (= 4 (tally s)) 1)))
   (hear f) (? !f (seal-hook 5 f)                   ; every nothing uninstalls
               (binary? f) (seal-hook 5 f)
               (scare 'help-arity f)))
; the stdio door, the help's twin: (wear (i o e)) re-seats in/out/err FOR THIS TASK,
(: (wear l) (seal-hook 6 l))
; trap -- (f x) under a help that ESCAPES: the value on a clean run, (n a b) when a
; scare landed here.
; ⚠ ALL THREE OPERANDS, always: two hand back a closure, which is truthy, and f never runs at
; all. n's own arity is checked (binary?) BEFORE f runs -- so the refusal lands under the
; AMBIENT help, not inside the raise this form exists to catch.
; ⚠ nothing unwinds: the escape jumps past every pending restore between the raise and here, so
; a cleanup belongs in n, never inside f -- or in `mind`'s c, which is the same cleanup owned by
; the resource instead of by the catcher.
(: (trap f x n)
     (: _ (? (binary? n) 0 (scare 'help-arity n))   ; refuse BEFORE f runs, under the ambient help
        prev (heard ())
        home (myself ())
        r (call-cc (\ k (: _ (hear (\ a b (? (id? (myself ()) home) (k (\ _ (n a b)))
                                             (: _ (hear prev) (scare a b)))))
                           v (f x)
                           (\ _ v))))
        _ (hear prev)
        (r ())))
; mind -- (f x) with c minded on the way out: c fires EXACTLY ONCE, clean or raised, the
; condition rides on to the help outside, and the answer is f's. so a helper holds its own
; port or its own restore, however far under a catcher it sits.
; the n hands back a marked condition rather than re-raising in place, so the one (c ())
; below serves both paths -- re-raise inside the n and a help that ANSWERS (the shell's
; default, welp, any plain (hear f)) returns through it and the tail fires c a second time.
; `mark` is minted once and closure-private, so no value f answers can pose as the raise.
; ⚠ ALL THREE OPERANDS, like trap, and c is a thunk fired with the unit.
; ⚠ RAISES ONLY: a scare is what reaches a help, so a bare (k v) into a continuation
; captured outside jumps clean past every c between there and here.
; ⚠ c runs with the displaced help back, so its own raise belongs to the trap OUTSIDE --
; and REPLACES the condition c was firing for, which is the one way out of a lost raise.
(: mind (: mark (mint 0)
           (\ f x c
              (: r (trap f x (\ a b (mark >< `(a b))))
                 _ (c ())
                 (? (&& (two? r) (id? <r mark)) (scare <>r <>>r) r)))))
; boxfix-core: same prep, rewriting refs via sub/subl since c0 has no analysis-time redirect.
; the law: EAGER reads see let*, closures see letrec*. a bare read BEFORE its name's pin
; keeps the plain nom -- a boxed name's own binding is renamed _, so the nom resolves
; OUTWARD, and (: s (trim s)) under a param s reads the param. a read under a \ takes the
; cell, filled by the time the closure runs (or the cell-miss door when it runs early).
; pn is the pinned-so-far set; u is 1 under a \.
(: (boxfix-core prs body) (:
   (rmc cs ns) (filter (\ p !(member? <p ns)) cs)
   ; bnd: the lexical binder set, seeded with the scope's own names, so a locally-bound name is
   ; an ordinary variable and never a macro head.
   (sub cs x bnd pn u) (?
    (nom? x) (: p (assoc x cs)
                (? !(two? p) x
                   (|| u (member? x pn)) (list cellread <>p (list '\ >>p))
                   x))
    (atom? x) x
    (: h (cap x) (?
      (id? h '\) (? (atom? >>x) x (: r (lp x) (link '\ (cat <r (list (sub (rmc cs <r) >r (cat <r bnd) pn 1))))))
      (id? h ':) (: ns (ln >x) (': >< subl (rmc cs ns) >x (cat ns bnd) pn u))
      (: m (macros h) (? (&& m !(member? h bnd)) (sub cs (m >x) bnd pn u) (map (\ e (sub cs e bnd pn u)) x))))))
   (subl cs bs bnd pn u) (?
    (atom? bs) bs
    (atom? >bs) (list (sub cs <bs bnd pn u))
    (: nd (dsug <bs <>bs) (<nd >< sub cs >nd bnd pn u >< subl cs >>bs bnd pn u)))
   (: bx (boxset prs)
      (? (id? () bx) ()
       (: cs (mkcs bx) ns0 (map cap prs)
          (one p pn) (: d (sub cs >p ns0 pn 0)
                        (? (member? <p bx)
                           (: c (assoc <p cs) (link '_ (list pin <>c (list '\ >>c) d)))
                           (link (cap p) d)))
          (ones ps pn) (? (atom? ps) ()
                          ((one <ps pn) >< ones >ps (<<ps >< pn)))
          (cellbinds cs + ones prs () >< sub cs body ns0 ns0 0))))))
; the flat-list adapter: a `:` binding list -> the rewritten list, or the input unchanged.
; love.c interns "boxfix" and calls it off the book, so c0 compiles prel through this. it must
; exist before ev.l does, and both compilers share the one pass.
(: (boxfix fs) (:
   (ev l) (? (atom? l) 1 (atom? >l) 0 (ev >>l))
   (bpr fs) (? (atom? >fs) (0 >< <fs)
               (: r (bpr >>fs) ((dsug <fs <>fs >< <r) >< >r)))
   (? (ev fs) fs
    (: pb (bpr fs) r (boxfix-core <pb >pb)
       (? r (catmap (\ p (list <p >p)) <r + list >r) fs)))))
