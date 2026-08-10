; prel.l -- love prel: data + function + macro definitions.
; prefix only down to the `dyadics` table: c0 compiles this before opfix and before the church
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
   (one? x) (= x (cap x))
   (atom? x) (nil? (two? x))
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
   ; from: the opaque accessor. currying reaches a member, 'keys introspects, (from ()) lists
   ; the registry, a missing module answers (). the raw tablet stays closure-private.
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
        ; an open star: a coin whose die pins slot 7 is numeric by declaration, so a numeral powers
        ; it through its own * (ipow is generic). no size to read, so this lane keeps the count cap.
        (starred? x) (? (coin? x) (= 1 (peep (die-of x) 7 0)) 0)
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
; seal-hook caches num-ap into the VM, so it must follow the pin above.
(seal-hook 1 num-ap)
; the + and * of two FUNCTIONS, which is all church arithmetic is: the extra a-slot threaded into
; both of stack's operands is what makes + agree with addition, not the S combinator's (f x (g x)).
; the C lanes (lvm_addh/lvm_mulh) read them by slot.
(: (stack f g a x) (f a (g a x))                  ; (+ f g)
   (compose f g x) (f (g x))                      ; (* f g)
   _ (seal-hook 2 stack)
   _ (seal-hook 3 compose))
; ⚠ map is for VALUES: link takes its two operands right to left, so a mapped EFFECT
; (a print, a pin) fires from the tail back and the page comes out reversed -- silently,
; every element having run. ordered effects want foldl, which is a left fold by shape.
(: (map f l) (? (two? l) (link (f (cap l)) (map f (cup l))))
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
              (sat x))
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
   (jot x) ((: (go i) (? (< i x) (link i (go (+ i 1))))) 0)   ; the range 0..x-1 (iota its array twin)
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
  (poke -1 (peek 0 in) (poke -1 to-vt (poke -1 -1 (poke -1 "  " (poke 4 0 (spin 5))))))
 ; hot? names the zoo (a buf or a port), the head's ap word the species -- the recognition C's iop
 ; does. hot? goes first so the peek only ever lands on a heap cell.
 (port? o) (? (hot? o) (id? (peek 0 o) (peek 0 in)))
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
 ; ahead. `in` already flows that way: it holds nothing in hand.
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
      ; the seat root: where the running binary lives. <seat>/../lib/ is the nest shape, its
      ; love/ subfolder the FHS one. no readlink gives (), and the rungs miss.
      ; read at each miss, never captured -- a binding folds the build tree's seat into the image.
      rlink (peep book 'readlink ())
      (seatlib _) (: x (? (nil? rlink) () (rlink "/proc/self/exe"))
                     (? (string? x)
                        (: n (tally x)
                           b ((: (go k r) (? (< k n) (go (+ k 1) (? (= 47 (peep x k 0)) (+ k 1) r)) r)) 0 0)
                           (+ (snip x 0 b) "../lib/"))
                        ()))
      ; the walk: the baked table, then lib/ off the cwd (the dev tree), then the seat root's lib/,
      ; then its love/ subfolder
      (libsrc nm) (: s (lib nm)                                ; the baked table: a read port over the C text
                     (? (port? s) s
                        (nil? fsopen) ()
                        (: f (+ (string nm) ".l")
                           q (fsopen (+ "lib/" f) "r")
                           (? q q
                              (: sl (seatlib 0)
                                 (? (nil? sl) ()
                                    (: q2 (fsopen (+ sl f) "r")
                                       (? q2 q2 (fsopen (+ (+ sl "love/") f) "r")))))))))
      (\ m (? (tablet? m) (prior m)
              (string? m)
              (: nm (mname m)
                 (? (tablet? (peep (mods ()) nm ())) (prior nm)
                    (: q (? (nil? fsopen) () (fsopen m "r"))
                       (? (nil? q) (prior m)
                          (: _ (enter nm) _ (rdev q) _ (leave ()) (prior nm))))))
              (tablet? (peep (mods ()) m ())) (prior m)
              (: q (libsrc m)
                 (? (nil? (port? q)) (prior m)                ; every source is a port now, baked or on disk
                    (slashy? (string m)) (: _ (rdev q) ())
                    (: _ (enter m) _ (rdev q) _ (leave ()) (prior m))))))))

; --- reader operators: the two tables + the opfix factor pass ---------------
; the valence law's two faces, one form: spaced is dyadic, glued is monadic. the reader leaves a
; sigil a plain symbol; opfix factors it against these at compile time.
; EVERY operator is dyadic -- a row carries no arity, only a grip (higher binds tighter) and a
; hand (0 right, 1 left -- how a same-grip run folds); op-ent normalizes every written shape to
; (name grip . hand). undeclared is infix at grip 95 -- above every row here, so the table
; holds only exceptions. infix is live from this table on.
; a monadics entry maps a run-piece to its monadic word and a run factors greedily, so <>x is
; (cap (cup x)) -- glued runs only, and a monadic-only sigil has no dyadics row at all.
(: dyadics (pins (tablet 0)
 '** 70
 '* (>< 60 1) '/ (>< 60 1) '% (>< 60 1)             ; multiplicative, LEFT
 '+ (>< 50 1) '- (>< 50 1)                          ; additive, LEFT
 '= 40 '!= 40 '< 40 '<= 40 '> 40 '>= 40             ; comparison
 '| 30 '& 30 '&& 30 '|| 30      ; logical (&& || also macros; the row pins the grip below comparison)
 '>< 25                                                          ; cons -- the loosest builder
 '? 10                                                           ; cond -- infix it is the one-armed form; an else arm is the prefix (? c a b)
 '$ 5)                                                           ; weak apply -- loosest (a $ b = (a b), haskell $); glued $x stays saturate (monadics)
   monadics (pins (tablet 0)
 '< 'cap '> 'cup '+ 'net '* 'prod '| 'abs '- 'negate '/ 'reciprocal
 '% 'fraction '? 'bit '$ 'saturate '! 'nil?))   ; `.` is pinned here by post.l, whose dot it is
 ; ⚠ the rows are NOMS, not values: a value would be unwritable, and an unwritable head is a
 ; form no text can spell -- which is what keeps opfix invertible (lib/unfix.l). a row resolves
 ; at the use site like any name, so libra's `shadow` rule speaks where one is rebound.
; factorization is the GLUED lane's alone: a run splits greedily, longest-prefix, no backtracking,
; against monadics. a SPACED sigil is one whole name -- it either carries a dyadics row or takes
; the fresh-punct default -- so != && >>= :- ?- each stay one operator.
; everything to (seal-hook 4 opfix) stays prefix -- the first pass compiles prel with no opfix.
(: (op-punc-byte c) (? (&& (<= 97 c) (<= c 122)) 0
                       (&& (<= 48 c) (<= c 57)) 0
                       (&& (<= 65 c) (<= c 90)) 0
                       (= c 95) 0
                       1)
   ; cheap prefilter: only an all-punct symbol can factor. the four ranges above are disjoint and
   ; run commonest-first, so don't reorder them.
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
   ; normalize a table entry for key k -> the triple (name grip . hand), or 0. a nom-led row is an
   ; ALIAS (it names another word), anything else is self-named; a bad tail answers 0.
   ; ⚠ the grip must be POSITIVE: a tablet MISS answers 0, and a grip of 0 would read that miss
   ; as a live row at the loosest grip there is.
   (op-grip? g) (&& (charm? g) (< 0 g))
   ; the grip/hand tail, dotted OR as a plain list -- the table writes (60 . 1) but `fixity`'s
   ; caller writes '(60 1), and the door should not make anyone count dots.
   (op-gh v)  (? (op-grip? v) (>< v 0)                                     ; a grip alone: right-handed
                 (nil? (two? v)) 0
                 (nil? (op-grip? (cap v))) 0
                 (charm? (cup v)) v                                          ; (grip . hand), dotted
                 (&& (two? (cup v)) (charm? (caup v))) (>< (cap v) (caup v))  ; (grip hand), a list
                 (atom? (cup v)) (>< (cap v) 0))                           ; (grip), a list
   (op-ent k) (: v (dyadics k)
                 (? (? (two? v) (nom? (cap v)) 0)                            ; nom-led: (name grip[. hand])
                      (: t (op-gh (cup v)) (? (two? t) (>< (cap v) t) 0))
                    (: t (op-gh v) (? (two? t) (>< k t) 0))))                ; self-named: the tail itself
   ; longest prefix of nm[i..n) with a monadic meaning -> (len . word), or 0. the table holds word
   ; values, so a hit is asked out of band against the miss answer -- a truth test would drop any
   ; word that nets nothing.
   (mono-long nm i n)
    ((: (go k) (? (< k 1) 0
                  (: e (monadics (intern (snip nm i (+ i k))))
                     (? (id? () e) (go (- k 1)) (>< k e)))))
     (- n i))
   ; factor a glued run against the monadic table -> the word list (outermost first), or 0
   (monofactor s)
    (: nm (string s) n (tally nm)
       ((: (go i)
           (? (< i n)
              (: m (mono-long nm i n)
                 (? (two? m)
                    (? (= (+ i (cap m)) n) (>< (caup m) ())
                       (: r (go (+ i (cap m))) (? (two? r) (>< (caup m) r))))))))
        0))
; the walk. a pending-operator frame is (orig name need grip . got): the source symbol, the
; resolved name, the operands still wanted, the captured grip, and the operands so far, reversed.
; need 0 means filled with the fold deferred, so a later operator can steal the last operand --
; that deferral is right-associativity. out/pend thread as (out . pend).
   (op-fr o nm nd g gt) (>< o (>< nm (>< nd (>< g gt))))
   (op-fro f) (cap f) (op-frn f) (caup f)
   (op-frd f) (cap (cuup f)) (op-frgrip f) (cap (cuuup f)) (op-frg f) (cup (cuuup f))
   ; a frame is only ever rebuilt with a new need + got -- orig, name and grip are fixed the
   ; moment the operator is read.
   (op-frw f nd g) (op-fr (op-fro f) (op-frn f) nd (op-frgrip f) g)
   ; wrap a chain of monadic words around a datum, outermost first
   (op-fin ch x) (? (two? ch) (>< (cap ch) (>< (op-fin (cup ch) x) ())) x)
   ; fold a collected span (out, reversed) into ONE operand: a lone datum stays itself, two-or-more
   ; become the application (juxtaposition binds tighter than any infix operator).
   (op-span out) (? (two? (cup out)) (rev out) (cap out))
   ; fold a frame with what it has: nothing collected gives the plain symbol, a shortfall the
   ; partial application -- the curry law, with no case of its own.
   ; kind tests throughout the walk, never truth -- an operand list like (0) or ("") nets zero.
   (op-fold f) (? (atom? (op-frg f)) (op-fro f)
                  (>< (op-frn f) (rev (op-frg f))))
   ; deliver datum d -> (out' . pend'). a collecting frame takes it; a filled infix frame extends
   ; its right operand into a span, since juxtaposition binds tighter: (a = b c) is (= a (b c)).
   ; the span state rides `need`: 0 = a lone datum, -1 = growing.
   (op-del d out pend)
    (? (atom? pend) (>< (>< d out) pend)
       (: f (cap pend)
          (? (< 0 (op-frd f))
             (>< out (>< (op-frw f (- (op-frd f) 1) (>< d (op-frg f))) (cup pend)))
             (: op (cap (op-frg f))                          ; the frame's last (right) operand
                nx (? (= 0 (op-frd f)) (>< op (>< d ())) ; lone -> wrap into (op d)
                      (cat op (>< d ())))                  ; span -> append d
                f2 (op-frw f -1 (>< nx (cup (op-frg f))))
                (>< out (>< f2 (cup pend)))))))
   ; take a left operand for an incoming operator of grip g and hand h -> (. (out' . pend')), or 0.
   ; against a filled top frame the incomer either steals its last operand or folds it and retries
   ; beneath. at equal grip the incomer's hand decides: right-handed steals, left-handed yields.
   ; at top level the whole leading span is taken: (a b = c) captures (a b), not just b.
   (op-steal g h out pend)
    (? (two? pend)
       (: f (cap pend)
          (? (< (op-frd f) 1)
             (? (? h (<= g (op-frgrip f)) (< g (op-frgrip f)))
                (: r (op-del (op-fold f) out (cup pend)) (op-steal g h (cap r) (cup r)))
                (>< (cap (op-frg f))
                      (>< out (>< (op-frw f 1 (cup (op-frg f))) (cup pend)))))
             0))
       (two? out) (>< (op-span out) (>< () pend))
       0)
   ; drain at end of list: fold every pending frame innermost-out
   (op-drain out pend)
    (? (atom? pend) (rev out)
       (: r (op-del (op-fold (cap pend)) out (cup pend))
          (op-drain (cap r) (cup r))))
   (op-w l out pend)
    (? (atom? l) (op-drain out pend)
       (: e (cap l)
          (? (op-puncp e)
             ; a spaced sigil is ONE name -- an unrowed one takes the house grip, and with no
             ; left operand the infix frame still folds to the plain symbol (op-fold's bare case)
             (: en0 (op-ent e)
                en (? (two? en0) en0 (>< e (>< 95 0)))   ; fresh punct: the (e house . right) triple
                nm (cap en) gp (caup en)
                st (op-steal gp (cuup en) out pend)   ; capture a left operand if one exists (at this grip + hand)
                (? (two? st) (op-w (cup l) (cap (cup st))
                            (>< (op-fr e nm 1 gp (>< (cap st) ()))
                                  (cup (cup st))))
                   (: r (op-del e out pend)     ; no left operand: the plain symbol
                      (op-w (cup l) (cap r) (cup r)))))
             (: r (op-del (op-core e) out pend)
                (op-w (cup l) (cap r) (cup r))))))
   ; a `:` binds, it does not apply, so opfix must not rearrange it. binders stay verbatim and only
   ; each pair's value and the trailing body factor. the pairing matches eval's exactly.
   (op-colon fs)
    (? (atom? fs) fs
       (atom? (cup fs)) (>< (op-core (cap fs)) ())        ; a lone form is the body
       (>< (cap fs)                                       ; the name: verbatim
             (>< (op-core (caup fs))                      ; the value: factored
                   (op-colon (cuup fs)))))                  ; the rest: pairs on
   ; a \ lambda is a binder too: the args stay verbatim, only the body factors.
   ; (\ x) -- one operand -- is quote, kept whole by op-core's first clause.
   (op-lam fs)
    (? (atom? (cup fs)) (>< (op-core (cap fs)) ())        ; the last operand is the body
       (>< (cap fs) (op-lam (cup fs))))                    ; an arg: verbatim, recurse
   ; flatten a directly-nested lambda body -- (\ a (\ b B)) -> (\ a b B) -- so neither compiler
   ; builds the extra closure. shadowing stays nested: (\ x (\ x x)) must not merge.
   (op-mem? y l)      (? (atom? l) 0 (id? y (cap l)) 1 (op-mem? y (cup l)))
   (op-disjoint? a b) (? (atom? b) 1 (op-mem? (cap b) a) 0 (op-disjoint? a (cup b)))
   (op-params ol)     (? (atom? (cup ol)) () (>< (cap ol) (op-params (cup ol))))
   (op-flat-ol ol seen)                                      ; -> the flattened operand list, body left RAW (op-lam factors it after)
    (? (atom? (cup ol))
       (: b (cap ol)
          (? (&& (two? b) (id? (cap b) '\) (two? (cuup b)) (op-disjoint? seen (op-params (cup b))))   ; (two? (cuup b)) = a real lambda, not quote
             (op-flat-ol (cup b) seen)                       ; disjoint inner lambda -> merge its operands up
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
             (id? h '\) (>< '\ (op-lam (op-flat-ol (cup x) ())))   ; (\ a.. body): flatten nested lambdas, then factor the body
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

   ; the fixity door: the one sanctioned write into the compile table, and the only reach to
   ; `dyadics` once birth mops the nom. it answers the row it replaced in the shape it takes, so
   ; (fixity nm (fixity nm new)) restores exactly; () clears a row. v is any shape op-ent takes,
   ; a refused one rolls back and scares. the table is read live.
   (fixity nm v)
    (? (nil? (nom? nm)) (scare 'bad-fixity nm)
       (: old (peep dyadics nm ())
          (? (nil? v) (: _ (pull dyadics nm 0) old)                ; () clears the row
             (: _ (pin dyadics nm v)
                (? (two? (op-ent nm)) old                            ; op-ent itself is the acceptor
                   (: _ (? (nil? old) (pull dyadics nm 0) (pin dyadics nm old))
                      (scare 'bad-fixity nm)))))))

   ; --- this `:` has a body, so the helpers above are local ------------------
   ; opfix and fixity are the door; the rest is the factor pass's scaffolding.
   ; `dyadics`, `monadics` and `pureset` stay global-and-mopped instead: ev.l holds them, and a
   ; cross-file capture has only the book to go through.
   _ (pin book 'fixity fixity)
   (pin book 'opfix opfix))
; the LAST seal: opfix joins the other four in the g->hot_* fields, so no later rebind can reach it.
(seal-hook 4 opfix)
; the two self-named apply operators: ** flips (a ** b = (b a), tightest grip), $ is the identity
; so a $ b = (a b), weak-binding application at the loosest grip. glued $x is saturate.
(: (** a b) (b a))
(: $ 1)
; ! . != are ordinary functions under their punct names. ! and . are monadic and nothing else, so
; the walk has no row to fold and hands the plain symbol through. != carries the comparison row,
; the one thing a binding cannot say. escape mid-list as (!) (.), like any bare operator.
(: ! nil?)
(: (!= a b) !(a = b))
; lisp-compat aliases by head-symbol substitution
(: seqx (\ l (>< ': (foldr (\ l r (>< '_ (>< l r))) (list (last l)) (init l)))))
(:: (intern ",") seqx) (:: 'do seqx) (:: 'begin seqx) (:: 'progn seqx)
(:: 'let (\ a (>< ': a)))
(:: 'if (\ a (>< '? a)))
(:: 'cond (\ a (>< '? a)))
(:: 'quote (\ a (>< '\ a)))

; --- assert / suite: the strict checkers, baked so they work with no harness ---
; each form must net true or assert scares with the source form, terminal without a help -- no
; `quit`, which is host-only, so a false claim stops the kernel run too. `suite` adds a
; "<name>: ok" sentinel, so a file gates itself. not `test`: that is a common local, and a macro
; ignores lexical scope.
(:: 'assert (\ l (>< 'do (map (\ e (list '? e 0 (list 'scare (list '\ 'assert) (list '\ e)))) l))))
(:: 'suite (\ l (>< 'do (>< (>< 'assert (cup l)) (list (list 'puts (cap l)) (list 'puts ": ok\n"))))))


; --- array element-type inference + shape coercion (shared by `tuple` and `array`).
; a-type answers a witness star of the highest tier present -- an inhabitant, not a code, since
; the generic `tray` ctor reads the tier off a witness. past the numeric tower the whole array
; lifts to an object array. a shape dimension may be any numeric and saturates to a nat.
(: (a-rank x) (? (gem? x) 1 (| (charm? x) (sun? x)) 0 (twin? x) 2 3)
   (a-emax m x) (: k (a-rank x) (? (m < k) k m))
   (a-type d) (: k (foldl a-emax 0 d) (? (k = 0) 0 (k = 1) 0.0 (k = 2) (twin 0 0) ()))   ; a witness star of the tier
   (a-dim d) (? (two? d) (tally d) (sat d))
   (a-shape s) (? (two? s) (map a-dim s) (list (a-dim s))))

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
; the help door: (hear f) installs f as the help every raise reaches -- seal-hook's one dynamic
; slot, the same hot_help the C raise lanes read -- and (heard ()) answers the installed one.
; (hear ()) uninstalls: a raise then takes the default escape.
(: (hear f) (seal-hook 5 f))
; trap -- (f x) under a help that ESCAPES: the value on a clean run, (n a b) when a
; scare landed here. peg's (\ s y n) aimed at conditions, less the y a caller already
; has -- a clean run just answers, and n's own answer is the form's, so which one you
; got is a thing the caller wrote both halves of. currying supplies the operand ((trap
; (serve1 root) c ..)), and the unit is the x when there is genuinely nothing to hand.
; n wears the help's own arity, so `welp` is one.
; ⚠ ALL THREE OPERANDS, always: two hand back a closure, which is truthy, and f never
; runs at all.
; ⚠ n fires only once the displaced help is BACK, which is what the thunks are for --
; the presence a caller no longer carries is carried HERE, since a clean v may be a
; closure like the escape's. run n inside and its own raise would land in this trap.
; ⚠ nothing unwinds: the escape jumps past every pending restore between the raise and
; here, so a cleanup belongs in n, never inside f. to catch selectively, let n re-raise
; what is not yours -- by then the outer help is the one listening.
(: (trap f x n)
     (: prev (heard ())
        r (call-cc (\ k (: _ (hear (\ a b (k (\ _ (n a b)))))
                           v (f x)
                           (\ _ v))))
        _ (hear prev)
        (r ())))
; boxfix-core: same prep, rewriting refs via sub/subl since c0 has no analysis-time redirect.
(: (boxfix-core prs body) (:
   (rmc cs ns) (filter (\ p !(member? <p ns)) cs)
   ; bnd: the lexical binder set, seeded with the scope's own names, so a locally-bound name is
   ; an ordinary variable and never a macro head.
   (sub cs x bnd) (?
    (nom? x) (: p (assoc x cs) (? (two? p) (list cellread <>p (list '\ >>p)) x))
    (atom? x) x
    (: h (cap x) (?
      (id? h '\) (? (atom? >>x) x (: r (lp x) (link '\ (cat <r (list (sub (rmc cs <r) >r (cat <r bnd)))))))
      (id? h ':) (: ns (ln >x) (': >< subl (rmc cs ns) >x (cat ns bnd)))
      (: m (macros h) (? (&& m !(member? h bnd)) (sub cs (m >x) bnd) (map (\ e (sub cs e bnd)) x))))))
   (subl cs bs bnd) (?
    (atom? bs) bs
    (atom? >bs) (list (sub cs <bs bnd))
    (: nd (dsug <bs <>bs) (<nd >< sub cs >nd bnd >< subl cs >>bs bnd)))
   (: bx (boxset prs)
      (? (id? () bx) ()
       (: cs (mkcs bx) ns0 (map cap prs)
          (one p) (: d (sub cs >p ns0)
                     (? (member? <p bx)
                        (: c (assoc <p cs) (link '_ (list pin <>c (list '\ >>c) d)))
                        (link (cap p) d)))
          (cellbinds cs + map one prs >< sub cs body ns0))))))
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
