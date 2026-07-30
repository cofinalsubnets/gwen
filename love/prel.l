;;; book is present in prelude
;;; definitions in this file must not use infix notation
;;; this file stays inside the PURE LISP subset -- delimiters, `;` comments, "…" with escapes,
;;; atoms, integers, `'` quote, and four float literals. no `(, no ~(, no #(/@(, no glued
;;; operator run, no bare `,`, no [/{, no prime inside a name: those are p1's half of the
;;; grammar and p0 will not read them. doc/reader.md rung 4.
;;; FIXME move ev support functions to ev where possible
; prel.l -- love prel: data + function + macro definitions.
; ⚠ PRE-OPERATORS ZONE down to the `operators` table: c0 compiles this before opfix and
; before the church add/mul hooks exist, so it stays OPERATOR-FREE -- prefix nifs only, no
; infix, no unquoted sigils. Using `*` for caap..cuuup here cores the egg.
(: id 1
   (const x _) x
   <- pin -> peep
   (co f x y) (f y x)                ; the argument-transpose
   flip co
   sat saturate)
(: : () ? () \ ())
(: pi 3.141592653589793
   tau (* 2 pi) ;; no infix here
   e 2.718281828459045                          ; Euler's number; exp is (\ x (x e))
   true 1 false 0
   i (twin 0 1) ;; FIXME up top
   (tangent x) (/ (sine x) (cosine x))          ; derived: tan = sin/cos
   star-tray (tray 0) gem-tray (tray 0.0) twin-tray (tray (twin 0 0)) top-tray (tray ())  ; the four typed-tray ctors (witness baked in)
   (tier v) (? (tray? v) (: c (atype v) (? (= c 0) star-tray (= c 1) gem-tray (= c 2) twin-tray top-tray)) ())  ; a tray's tier IS its ctor; nil off a tray
   zero? nil?                                  ; zero? aliases nil? (the net-to-zero test)
   (one? x) (= x (cap x))
   (atom? x) (nil? (two? x))
   ; the scope-layer door. enter pushes a fresh layer with its OWN macro table (key 0,
   ; love.c's book[nil]), stashing the pre-enter chain + the name under the reserved charm
   ; key 31337 -- a charm can never collide with a name. (enter ()) is anonymous.
   (enter nm) (: F (tablet 0) _ (pin F 0 (tablet 0)) _ (pin F 31337 (link (books ()) nm))
                 _ (setbooks (link F (books ()))) ())
   ; use: splice a registered module just BELOW the head, so its names resolve on the walk but
   ; never shadow yours. a missing name scares -- a silent ()-splice would be a silent no-op.
   (use m)    (: b (? (tablet? m) m
                      (: r (peep (mods ()) m ()) (? (nil? r) (scare (\ no-module) m) r)))
                 (setbooks (link (cap (books ())) (link b (cup (books ()))))))
   ; leave: unwind to the pre-enter chain, register the layer under its name, answer the book.
   ; ⚠ a BARE leave (no charm, so never entered) pops whatever sits just below the head -- the
   ; inverse of `use`. UNPAIRED it pops your downstairs neighbor whatever that is.
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
; `::` defines a macro in THE HEAD LAYER's table -- the same book a plain `:` defglobs to,
; so prel's macros land in orth during boot, a script's in the session layer, a module's in
; the module. the (tablet 0) fallback is for a layer built without one; enter makes one now.
(: ; macros: look a macro up across the LAYER CHAIN -- the love twin of love.c's macroget,
   ; and it has to stay in step with it. each book carries its own table at the nil key; a
   ; layer without one is skipped; 0 is the no-macro answer.
   ; ⚠ prefix-only, nifs only: pre-operators zone, so nil?, not `!`.
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
   putx (print out)
   (getc _) (see in))
; numfn assembles the n-fold text directly via spin/poke, so applying a fixnum never
; re-enters the compiler: n quote-f's then n ap's, cell 0 the source \-expr for the
; printer, entry at cell 1. ⚠ the helpers pass th/f/n explicitly -- a closure over the
; forward-bound th miscompiles under the bootstrap c0.
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
   ; each site keeps its OWN (scare 'apcap k) as the cond result, so a recovering help's
   ; value lands as the result directly -- no thunk on the hot path. don't factor it out.
   (apover k) (< (peep apcap 0 0) k)
   ; powover guards the RESULT size, not the count: only an exact base beyond +-1 can grow,
   ; so it reads the box as a WORD budget (the stock 2^20 allows an 8MB number). (2000000 2)
   ; runs deliberate while the slipped tower ((10 9 8) = 8^(9^10)) still scares.
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
   ; Church-numeral application across the tower: numeric operand -> x ** n, a function ->
   ; compose ($ n) times, where saturation reduces ANY numeral to a fixnum nat.
   ; cmp2..cmp8 are cached compose-combinators, so a numeral over a runtime function is a
   ; closure capture rather than per-call thread assembly; past the cache numfn takes it.
   ; ⚠ a counting iterative applier measured 22% slower at k=2/3, 45% across k=2..8 -- the
   ; cached body is k straight-line applications with no loop. don't "simplify" it back.
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
        ; an OPEN star: a coin whose die pins slot 7 is numeric by declaration, so a numeral
        ; powers it through its own * (ipow is generic). only whole exponents mean anything
        ; off the closed tower. no size to read, so this lane keeps the plain COUNT cap.
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
; the + and * combinators are `stack` and `compose`, immortal constant threads in C:
;   (+ f g) = Church add   ((+ f g) a x) = (f a (g a x))  -- the extra a-slot threaded into
;             both operands is what makes + agree with addition, not (f x (g x)).
;   (* f g) = composition  ((* f g) x)   = (f (g x))
; ⚠ seal-hooks caches num-ap into the VM, so it MUST follow num-ap's pin above.
(seal-hooks ())
(: (map f l) (? (two? l) (link (f (cap l)) (map f (cup l))))
   (foldl f z l) (? (two? l) (foldl f (f z (cap l)) (cup l)) z)
   (foldr f z l) (? (two? l) (f (cap l) (foldr f z (cup l))) z))
; the monadic words behind the glued sigils. net is the content measure -- `+` turned
; inward -- and a coin's lane is by DECLARATION: die slot 7 nets itself (so additivity
; lands in the die's own ring), else its tally under net-mode 1, else its payload signed.
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
   ; a star is the fixpoint of net. id?, not `=`: net hands a number operand back unchanged
   ; where a tray's net is a fresh sum, so star? and galaxy? stay DISJOINT.
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
   ; the shared monotone-fixpoint driver: iterate `step` until `size` stops changing. size must
   ; be strictly monotone under step until the fixpoint, so equality == convergence. the measure
   ; is snapshot BEFORE the step, so a step that mutates in place is still detected.
   (monofix st step size) (: b (size st) nx (step st) (? (= (size nx) b) nx (monofix nx step size)))

   ; the one constant-form recognizer. the answer is PRESENCE-WRAPPED -- (1 . value) or () --
   ; because a constant's value can itself be nothing. (no `&&`: it is a macro defined later in
   ; prel, and a single-pass bootstrap can't expand a forward macro.)
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
   (member? x l) (? (two? l) (? (= x (cap l)) 1 (member? x (cup l))) 0)   ; any of (= x); first-order so the glaze's point-= grammar closes it (auto.l re-declares this exact body)
   (zip a b) (? (two? a) (? (two? b) (link (link (cap a) (cap b)) (zip (cup a) (cup b)))))
   rev (foldl (flip link) ())
   ; every nom visible from here -- the WHOLE layer chain, deduped through a tablet since a
   ; nom in two layers is one name. NOMS only: a layer's keys also carry the macro table (nil
   ; key) and the unwind chain (a charm), and `names` answers what you could WRITE.
   ; ⚠ lives down here so catmap/foldl are bound when it compiles; a forward reference would
   ; ride lvm_index and never fold.
   (names _) (filter nom? (keys (foldl (\ t k (pin t k 1)) (tablet 0) (catmap keys (books ())))))
   ; the MACRO twin. each layer keeps its macro table at its nil key (love.h: "the
   ; macro table is book[nil] -- no separate field"), so this walks the same chain
   ; `macros` looks up through and answers what you could WRITE AS A HEAD. a layer
   ; without a table is skipped, exactly as the lookup skips it. together with
   ; `names` there is nothing left to keep a list of: the vocabulary answers for
   ; itself. ⚠ same zone rules as `names` -- prefix only, and it folds its `books`
   ; reference at compile time, which is what lets birth mop the nom out from under it.
   (macro-names _) (filter nom? (keys (foldl (\ t k (pin t k 1)) (tablet 0)
                                             (catmap (\ b (: m (peep b 0 ())
                                                             (? (tablet? m) (keys m) ())))
                                                     (books ())))))
   (jot x) ((: (go i) (? (< i x) (link i (go (+ i 1))))) 0)   ; the range 0..x-1 (iota its array twin); a section of tally, off the `*` glyph -- monadic `*` is prod now
   (drop n l) (? n (drop (- n 1) (cup l)) l)
   (take n l) (? n (? (two? l) (link (cap l) (take (- n 1) (cup l)))))   ; stop at end-of-list (else it pads past it)
   (part p) (foldr (\ a m (? (p a) (link (link a (cap m)) (cup m))
                                        (link (cap m) (link a (cup m))))) '(())))
(: (tap cl)
 (poke -1 (peek 0 in) (poke -1 -4 (poke -1 -1 (poke -1 0 (poke 4 cl (spin 5))))))
 (jug _)                                          ; the output dual of `tap`: say INTO it, slurp it back OUT
  (poke -1 (peek 0 in) (poke -1 -2 (poke -1 -1 (poke -1 0 (poke -1 "  " (poke 5 0 (spin 6)))))))
 (sip o) (see o)                                  ; draw ONE unit (pairs with slurp = all)
 ; slurp drains a vessel to a value: a jug reads its backing buffer, a tap drains via see.
 (slurp o) (? (= -2 (peek 1 o)) (snip (peek 4 o) 0 (peek 5 o))   ; a jug BY ITS FD (-2): a buffered fd port's word 4 is a string too, so a string? probe would lie
              (: (rl i) (: c (see i) (? (= c -1) () (link c (rl i)))) (string (rl o))))  ; (pre-opfix: no ! here)
 (show x) (: o (jug 0) _ (print o x) (slurp o)))
; the help status reader. ONE bit reaches a help now -- scare (1). the more bit
; that named an unfinished read went out with ioparse: `sound` answers its own
; nothings as values (doc/reader.md rung 6c), so nothing raises it and nothing
; asks.
(: (scare? s) (= s 1))
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

; --- the loader: use-on-miss ------------------------------------------------
; the `use` up top splices a REGISTERED module; down here -- where tap/map/jot are bound --
; it learns to LOAD one it doesn't have, from the baked lib book or the filesystem walk. a
; load is ONE FRESH LAYER: enter, read-eval every form, leave registers, the prior use
; splices it. a SLASHED name is an INCLUDE (evals into the current head, no layer, no
; registration); a STRING is an explicit path, the module named by its basename.
; the open nif is captured by PEEP, not a nom read, so a fs-less frontend holds () there
; and the lanes just miss -- it degrades to the baked table instead of raising.
(: use
   (: prior use
      ; read-eval every form of a module source. the port is drained ONCE and
      ; walked as text -- a module is a file or a baked string, never a live
      ; wire, so there is nothing to stream. ⚠ a torn form used to fall through
      ; and get EVALED as the port object; it scares now.
      (rdev q) ((: (go cl) (: r (sound cl)
                              (? (two? r) (: _ (ev 'ev (cap r)) (go (cup r)))
                                 (id? r torn) (scare (\ unfinished-form) q)
                                 0)))
                (: s (slurp q) (map s (jot (tally s)))))
      (slashy? s) ((: (go i) (? (< i (tally s)) (? (= 47 (peep s i 0)) 1 (go (+ i 1))) 0)) 0)
      (mname s) (: n (tally s)
                   b ((: (go k r) (? (< k n) (go (+ k 1) (? (= 47 (peep s k 0)) (+ k 1) r)) r)) 0 0)
                   e (? (? (< 2 (- n b)) (= ".l" (snip s (- n 2) n)) 0) (- n 2) n)
                   (intern (snip s b e)))
      fsopen (peep book 'open ())      ; the host's open nif, by peep -- see above
      ; the SEAT ROOT: where the running binary lives. <seat>/../lib/ is the nest shape
      ; (~/.love/bin/love finds ~/.love/lib), its love/ subfolder the FHS one. PATH picks
      ; the love, the love carries its library. absent readlink, () and the rungs miss.
      ; READ AT EACH MISS, never captured: a binding would fold the BUILD tree's seat
      ; into the loader closure and ride the baked image forever (probed: the installed
      ; love walked out/lib instead of its own nest).
      rlink (peep book 'readlink ())
      (seatlib _) (: x (? (nil? rlink) () (rlink "/proc/self/exe"))
                     (? (string? x)
                        (: n (tally x)
                           b ((: (go k r) (? (< k n) (go (+ k 1) (? (= 47 (peep x k 0)) (+ k 1) r)) r)) 0 0)
                           (+ (snip x 0 b) "../lib/"))
                        ()))
      ; the walk: the baked table, then lib/ off the cwd (the dev tree), then the
      ; seat root's lib/, then its love/ subfolder
      (libsrc nm) (: s (peep (lib ()) nm ())
                     (? (string? s) (tap (map s (jot (tally s))))
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
                 (? (nil? q) (prior m)
                    (slashy? (string m)) (: _ (rdev q) ())
                    (: _ (enter m) _ (rdev q) _ (leave ()) (prior m))))))))

; --- reader operators: the table + the opfix factor pass --------------------
; the reader leaves a sigil (a maximal all-punct token) a plain symbol; opfix factors it
; against THIS table at compile time. arity ONE applies to the next datum and never captures
; a left operand; two..seven are infix. the DEFAULT is infix at two -- any all-punct symbol
; absent from the table still reads dyadic -- so the table holds only the EXCEPTIONS, plus
; the dyadic words a composite run factors against (!= -> ! =).
; a row carries arity, a GRIP (precedence, higher binds tighter) and a HAND (which way a
; same-grip run folds: 0 right, 1 left). op-ent below normalizes all five written shapes to
; the quad (name arity grip . hand). an incoming operator STEALS a pending filled operand
; when its grip beats the frame's; at EQUAL grip its hand decides, which is why arithmetic
; is left-handed ((1 - 2 - 3) is -4) and everything else folds right.
; ⚠ END PRE-OPERATORS ZONE: infix is live from this table on. the fold is open-coded rather
; than written with #(..) because c0 doesn't run the self-hosted maps pass.
(: operators
   (foldl (\ t p (pin t (cap p) (cup p))) (tablet 0)
      (L (link '! (link 'nil? 1)) (link '. (link 'dot 1))                         ; arity-1 sigils: grip inert (prefix folds on fill)
         (link '** (link 2 70))                                                   ; apply -- flip-apply, tightest infix (a ** b = (b a)); glued a**b unaffected (not in monadics)
         (link '* (link 2 (link 60 1))) (link '/ (link 2 (link 60 1))) (link '% (link 2 (link 60 1)))  ; multiplicative, LEFT
         (link '+ (link 2 (link 50 1))) (link '- (link 2 (link 50 1)))            ; additive, LEFT
         (link '= (link 2 40)) (link '< (link 2 40)) (link '<= (link 2 40)) (link '> (link 2 40)) (link '>= (link 2 40)) ; comparison
         (link '| (link 2 30)) (link '& (link 2 30)) (link '&& (link 2 30)) (link '|| (link 2 30)) ; logical (&& || also macros; the row pins the grip below comparison)
         (link '>< (link 2 25))                                                   ; cons -- the loosest builder
         (link '<- (link 'pin (link 3 20))) (link '-> (link 'peep (link 3 20)))   ; assignment aliases
         (link '? (link 3 10))                                                    ; cond
         (link '$ (link 2 5)))))                                                  ; weak apply -- loosest (a $ b = (a b), haskell $); glued $x stays saturate (monadics)
; the VALENCE LAW's table: a GLUED operator run is monadic (the reader emits (mono (run
; datum))), where spaced it keeps its infix life. each entry maps a run-piece to its monadic
; word and a run factors greedily, so <>x is (cap (cup x)) and !!x double-negates.
(: monadics
   (foldl (\ t p (pin t (cap p) (caup p))) (tablet 0)
   ; one row per RUN-PIECE, never per run: a multi-piece run is what the greedy factor is for,
   ; and caap/cuup are prel closures the compiler inlines, so a `<<`/`>>` row bought nothing a
   ; timing loop could see -- and left the tree's commonest run, `<>`, looking conspicuously absent.
      (L '(< cap) '(> cup) '(+ net) '(* prod)
         '(| abs) '(- negate) '(/ reciprocal) '(% fraction) '(? bit)
         '($ saturate)
         '(! nil?) '(. dot))))
; the FACTORIZATION LAW: greedy longest-prefix match, no backtracking; every factor but the
; last must be arity one, and the last takes the positional valence. a token that doesn't
; factor is a plain symbol -- so != factors to ! and =, while && >>= :- ?- stay atomic names.
; ⚠ everything from here to the SECOND (seal-hooks) is the opfix machinery itself and must
; stay PREFIX: the first bootstrap pass compiles prel with no opfix on the book, so an infix
; form in here reads as a raw application and love0's lcat dies.
; the four ranges are disjoint and run COMMONEST-FIRST -- op-puncp bails on a symbol's first
; byte, and that byte is usually a lowercase letter. don't reorder them.
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
   ; normalize a table entry for key k -> the quad (name arity grip . hand), or 0. both head and
   ; tail fork BY KIND (a charm arity vs a nom name; a bare charm grip vs a (grip . hand) pair),
   ; so all five written shapes land here and nothing downstream sees the difference. a bad
   ; arity or tail answers 0 -- not a table operator, which is what a fresh punct gets anyway.
   (op-ar? n) (&& (charm? n) (< 0 n) (< n 8))
   ; the grip/hand tail, dotted OR as a plain list -- the table writes (2 60 . 1) but `fixity`'s
   ; caller writes '(2 60 1), and the door should not make anyone count dots.
   (op-gh v)  (? (charm? v) (link v 0)                                       ; a grip alone: right-handed
                 (nil? (two? v)) 0
                 (nil? (charm? (cap v))) 0
                 (charm? (cup v)) v                                          ; (grip . hand), dotted
                 (&& (two? (cup v)) (charm? (caup v))) (link (cap v) (caup v))  ; (grip hand), a list
                 (atom? (cup v)) (link (cap v) 0)                            ; (grip), a list
                 0)
   (op-ent k) (: v (operators k)
                 (? (charm? v) (? (op-ar? v) (link k (link v (link 27 0))) 0)
                    (two? v)
                     (? (charm? (cap v))                                     ; self-named: (arity . grip[. hand])
                          (: t (op-gh (cup v))
                             (? (&& (op-ar? (cap v)) (two? t)) (link k (link (cap v) t)) 0))
                        (nom? (cap v))
                          (? (charm? (cup v))                                ; (name . arity): alias at house
                               (? (op-ar? (cup v)) (link (cap v) (link (cup v) (link 27 0))) 0)
                             (: t (op-gh (cuup v))                           ; (name arity . grip[. hand])
                                (? (&& (op-ar? (caup v)) (two? t)) (link (cap v) (link (caup v) t)) 0)))
                        0)
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
        0))
; the walk. a pending-operator frame is (orig chain name need grip . got):
;   orig  the source symbol, handed back verbatim when no operand ever arrives
;   chain the arity-one wrapper names, outermost first
;   need  operands still wanted; 0 = FILLED, the fold deferred so a following operator can
;         steal the last operand -- that deferral IS right-associativity
;   grip  the precedence, captured from the factor entry, so an alias `<-` or a composite
;         `!=` carries the level neither of its symbols alone recovers
;   got   operands so far, reversed
; out/pend thread as one (out . pend) chain through the helpers.
   (op-fr o ch nm nd g gt) (link o (link ch (link nm (link nd (link g gt)))))
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
   ; fold a frame with whatever it has: nothing collected -> the plain symbol, a shortfall ->
   ; the partial application (the curry law), arity one -> wrap its lone operand.
   ; ⚠ KIND tests throughout the walk, never truth: a collected operand list like (0) or ("")
   ; NETS zero, so nil? would read it as empty and silently drop the operand.
   (op-fold f) (? (atom? (op-frg f)) (op-fro f)
                  (nom? (op-frn f)) (op-fin (op-frc f) (link (op-frn f) (rev (op-frg f))))
                  (op-fin (op-frc f) (cap (op-frg f))))
   ; deliver datum d -> (out' . pend'). a collecting frame takes it; a FILLED infix frame
   ; EXTENDS its right operand into an application span, since juxtaposition binds tighter
   ; than any infix operator: (a = b c) is (= a (b c)), not ((= a b) c). the span state rides
   ; `need`: 0 = still a lone datum (the first extra wraps it), -1 = a growing span (append).
   ; a frame at arity one folds AT ONCE -- ones bind tightest ($"ab" + 2 is (+ ($ "ab") 2)).
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
   ; take a left operand for an incoming operator of grip g and hand h -> ((. (out' . pend')), or 0.
   ; against a FILLED top frame the incomer either STEALS its last operand or FOLDS it and retries
   ; beneath (the precedence climb). the whole associativity question is which way EQUAL grip goes,
   ; and that is the incomer's HAND -- right-handed steals, left-handed yields. a strictly tighter
   ; incomer steals and a looser one folds either way, so precedence itself is untouched.
   ; at top level the WHOLE leading span is taken: (a b = c) captures (a b), not just b.
   (op-steal g h out pend)
    (? (two? pend)
       (: f (cap pend)
          (? (< (op-frd f) 1)
             (? (? h (<= g (op-frgrip f)) (< g (op-frgrip f)))
                (: r (op-del (op-fold f) out (cup pend)) (op-steal g h (cap r) (cup r)))
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
             ; whatever does NOT factor defaults to infix at two; with no left operand the
             ; infix frame still folds to the plain symbol (op-fold's bare case)
             (: fs0 (opfactor e)
                fs (? (two? fs0) fs0 (link (link e (link 2 (link 27 0))) ()))   ; fresh punct: the (e 2 house . right) quad
                ch (map cap (init fs)) en (last fs) nm (cap en) n (caup en) gp (cauup en)
                (? (= n 1)                       ; arity one (or a one-chain): prefix --
                   ; nil name, the op rides the chain: marks the frame to fold on fill (grip inert)
                   (op-w (cup l) out (link (op-fr e (cat ch (link nm ())) 0 1 27 ()) pend))
                   (: st (op-steal gp (cuuup en) out pend)  ; infix: capture if a left operand exists (at this grip + hand)
                      (? (two? st) (op-w (cup l) (cap (cup st))
                                  (link (op-fr e ch nm (- n 1) gp (link (cap st) ()))
                                        (cup (cup st))))
                         (two? ch)               ; composite with no left: prefix
                         (op-w (cup l) out (link (op-fr e ch nm 1 gp ()) pend))
                         (: r (op-del e out pend) ; bare operator: the plain symbol
                            (op-w (cup l) (cap r) (cup r)))))))
             (: r (op-del (op-core e) out pend)
                (op-w (cup l) (cap r) (cup r))))))
   ; a `:` is a BINDING form, not an application -- opfix must not rearrange it. binder
   ; positions stay verbatim and only each pair's VALUE and the trailing body factor. the
   ; pairing matches eval's exactly, so a punct binder like (: >< (\ ..) ..) keeps its name.
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
   ; flatten a directly-nested lambda body at the SOURCE level -- (\ a (\ b B)) -> (\ a b B) --
   ; so neither compiler builds the extra inner closure a curried HOF would. ⚠ SHADOWING stays
   ; nested: (\ x (\ x x)) must NOT become (\ x x x), whose body binds two same-named params.
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
   ; the comma layer -- the loosest grouping, ABOVE infix. datums split into clauses at
   ; top-level commas and the whole becomes the `,` sequencing macro: (a, b c) -> (: _ a (b c)).
   ; empty clauses drop, so doubled/leading/trailing commas compose away. ⚠ the comma outranks
   ; even `:`, so a binder spine splits like any list -- parenthesize the form.
   ; ⚠ the comma symbol is INTERNED, never written as a literal: prel stays inside the pure-lisp
   ; subset p0 reads (doc/reader.md), and a source `,` is the reader datum these three describe.
   ; `intern` is in ev.l's pureset, so cprop folds each call back to the constant it used to be.
   (op-cm? l)   (? (atom? l) 0 (id? (cap l) (intern ",")) 1 (op-cm? (cup l)))
   (op-take l)  (? (atom? l) () (id? (cap l) (intern ",")) () (link (cap l) (op-take (cup l))))
   (op-drop l)  (? (atom? l) () (id? (cap l) (intern ",")) (cup l) (op-drop (cup l)))
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
                    (link (intern ",") (map op-core cs))))
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
                (? (&& (has-top-op? x) (two? res) (atom? (cup res)) (two? (cap res))) (cap res) res))))) ; (a = b)->(= a b) not ((= a b)); ((mov r3 r12)) (list-of-one, no top-level op) is KEPT; (f)/(f x) untouched
   (opfix x) (op-core x)

   ; the FIXITY DOOR -- the one sanctioned write into the compile table, and the only reach to
   ; `operators` at all once birth mops the nom (this closure captures it; nothing else can).
   ; (fixity nm v) pins v as nm's row and answers the row it REPLACED in the same shape it takes,
   ; so (fixity nm (fixity nm new)) restores exactly and a non-() answer says someone got there
   ; first. () clears the row, which is also what an absent row answers. v is any shape op-ent
   ; takes, so (fixity '<+> '(2 60 1)) is haskell's `infixl 7 <+>` and '(2 60) is `infixr 7`.
   ; a refused shape is ROLLED BACK and scared: a bad row would otherwise silently demote its
   ; operator to the fresh-punct default -- on a core operator, a poisoned compiler with no message.
   ; the table is read live, so a declaration governs every form compiled after it and none already
   ; folded. it is global, deliberately: an operator's grip is part of its meaning. it lives INSIDE
   ; this form because op-ent is its acceptor and op-ent is local -- and so, like everything else
   ; here, it is written PREFIX: opfix is not pinned until this form's own body, so `operators <- nm v`
   ; would compile as a plain application and the pin would go nowhere, silently.
   (fixity nm v)
    (? (nil? (nom? nm)) (scare 'bad-fixity nm)
       (: old (peep operators nm ())
          (? (nil? v) (: _ (pull operators nm 0) old)                ; () clears the row
             (: _ (pin operators nm v)
                (? (two? (op-ent nm)) old                            ; op-ent itself is the acceptor
                   (: _ (? (nil? old) (pull operators nm 0) (pin operators nm old))
                      (scare 'bad-fixity nm)))))))

   ; --- and THIS `:` HAS A BODY, so the 34 helpers above are LOCAL -----------
   ; opfix and fixity are the door; the rest is the factor pass's scaffolding. the
   ; frame walk and the factor helpers are ONE form for exactly that reason -- closing
   ; them separately would wall them apart. nothing outside this file has ever named one.
   ; ⚠ `operators`, `monadics` and `pureset` stay global-and-mopped instead: ev.l holds
   ; them, and a cross-FILE capture has only the book to go through. `opfix` is pinned
   ; by nom for that reason and one more -- C seals it into g->hot_* just below.
   _ (pin book 'fixity fixity)
   (pin book 'opfix opfix))
; the SECOND seal: opfix joins num-ap/add/mul in the g->hot_* fields, so the C eval lane reads
; the field from here on and no later rebind of the nom can reach it.
(seal-hooks ())
; the two self-named apply operators: ** flips (a ** b = (b a), tightest grip), $ is the
; identity so a $ b = (a b), weak-binding application at the loosest grip. glued $x is saturate.
(: (** a b) (b a))
(: $ 1)
; lisp-compat aliases by head-symbol substitution
(: seqx (\ l (link ': (foldr (\ l r (link '_ (link l r))) (list (last l)) (init l)))))
(:: (intern ",") seqx) (:: 'do seqx) (:: 'begin seqx) (:: 'progn seqx)
(:: 'let (\ a (link ': a)))
(:: 'if (\ a (link '? a)))
(:: 'cond (\ a (link '? a)))
(:: 'quote (\ a (link '\ a)))

; --- assert / suite: the strict checkers, baked so they work with NO harness ---
; each form must net true or assert SCARES with the SOURCE form, terminal without a help --
; no `quit`, which is host-only, so a false claim stops the run loud on the kernel too.
; `suite` wraps the recurring shape: a battery then a "<name>: ok" sentinel, so a file gates
; ITSELF. not named `test`: that is a common local, and a macro ignores lexical scope.
; a purely-constant assertion is folded by cprop BEFORE this expands, so the message reads
; ";; assert 0" rather than the form. harmless -- a real assert tests a runtime value.
(:: 'assert (\ l (link 'do (map (\ e (L '? e 0 (L 'scare (L '\ 'assert) (L '\ e)))) l))))
(:: 'suite (\ l (link 'do (link (link 'assert (cup l)) (list (list 'puts (cap l)) (list 'puts ": ok\n"))))))


; --- array element-type inference + shape coercion (shared by `tuple` and `array`).
; a-type answers a WITNESS star of the highest tier present -- an inhabitant, not a code,
; since the generic `tray` ctor reads the tier off a witness. anything past the real numeric
; tower (bignum, symbol, string, nested array) lifts the whole array to an object array.
; a shape dimension may be ANY numeric and coerces to a nat by saturation, so (array 2.5 …)
; is rank-1 length 3 and (array ~(3 4) …) length 5.
(: (a-rank x) (? (gem? x) 1 (| (charm? x) (sun? x)) 0 (twin? x) 2 3)
   (a-emax m x) (: k (a-rank x) (? (< m k) k m))
   (a-type d) (: k (foldl a-emax 0 d) (? (= k 0) 0 (= k 1) 0.0 (= k 2) (twin 0 0) ()))   ; a witness star of the tier
   (a-dim d) (? (two? d) (tally d) (sat d))
   (a-shape s) (? (two? s) (map a-dim s) (L (a-dim s))))

; tuple/hash value constructors -- also the read-back targets of the printed @(…) / #(…)
(: (spread l) (tray (a-type l) (L (foldl (\ n _ (+ 1 n)) 0 l)) l))
(:: 'tuple (\ a (list 'spread (link 'list a))))
; (hash x) boxes any non-list: a fresh tablet pinning x at 0, so #5 = #(0 5) and #() IS #0.
; an odd trailing operand lands in that 0 slot.
(: (hashtx a) (? (atom? a) '(tablet 0)
                 (atom? (cup a)) (list 'pin (hashtx 0) () (cap a))
                 (list 'pin (hashtx (cuup a)) (cap a) (caup a))))
(:: 'hash (\ a (hashtx a)))
; (pins t k v ..): pin each pair onto an EXISTING tablet -- the mutable-accumulator twin of
; #(..), which mints a fresh one. an odd trailing operand is dropped (no lone key).
(:: 'pins (\ l (: (go t r) (? (two? (cup r)) (go (list 'pin t (cap r) (caup r)) (cuup r)) t)
                  (go (cap l) (cup l)))))


; (array shape elem…): the N-D constructor. shape is a dimension list or a lone number (a
; rank-1 length); the element type is inferred, so exactness is never silently lost.
; the elements are ordinary trailing args -- the shape fixes the count, so `array` curries
; them one at a time and a partial like (array 3) is just a collector awaiting three.
(: (a-coll sh n acc) (? (< n 1) (: d (rev acc) (tray (a-type d) sh d))
                        (\ x (a-coll sh (- n 1) (link x acc))))
   (array s) (: sh (a-shape s) (a-coll sh (foldl (*) 1 sh) 0)))

; recursive-value boxing for `:` -- the letrec* "capture by location" fix. ONE SCOPE: every
; name bound in a `:` resolves locally over the whole form. a reference at-or-before its own
; binding is FORWARD, and the name indirects through a CELL keyed by the binding site's nom:
; store via pin, read via (missing cell 'nom). ⚠ it emits the nif VALUES, not their names, so
; the rewrite survives the egg pulling names and user bindings shadowing them.
; a pre-fill read is therefore a map miss = the MISSING condition carrying the binding-site
; nom, so read-before-pin is one law at every depth. a value binding boxes on ANY forward
; reference, a lambda binding only on a DIRECT one, so ordinary recursion keeps the lazy lane.
; answers 0 when nothing needs boxing, else the rewritten (prs' . body').
(: (lambp x) (? (two? x) (&& (id? '\ (cap x)) (two? (cuup x)))) ; \ with >=1 param (one-operand \ is quote)
   (dsug n d) (? (atom? n) (link n d) (dsug (cap n) (link '\ (cat (cup n) (list d)))))
   (lp x) (: a (cup x) (? (atom? (cup a)) (link 0 (cap a)) (link (init a) (last a))))
   (ln bs) (? (atom? bs) 0 (atom? (cup bs)) 0 (link (cap (dsug (cap bs) 0)) (ln (cuup bs))))
   ; w/wl: does x / binding-list bs reference symbol v? \ and : shadow, one-operand \ is
   ; quote, macros expand. u tracks lambda depth; dm counts only references NOT under a lambda.
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
   (boxset prs) (: nm (map cap prs)
     (filter (\ v (any (\ p (w v (cup p) 0 0 (lambp (cup (assoc v prs)))))
                       (take (+ 1 (lidx v nm)) prs))) nm))
   (mkcs bx) (map (\ v (link v (link (nom 0) (nom v)))) bx)
   (cellbinds cs) (map (\ c (link (caup c) (list tablet 0))) cs))
; boxfix-core: same prep, rewriting refs via sub/subl since c0 has no analysis-time redirect.
(: (boxfix-core prs body) (:
   (rmc cs ns) (filter (\ p (nil? (member? (cap p) ns))) cs)
   ; bnd: the lexical binder set, seeded with the scope's own names -- the SHADOW LAW, so a
   ; locally-bound name is an ordinary variable and never a macro head.
   (sub cs x bnd) (?
    (nom? x) (: p (assoc x cs) (? (two? p) (list missing (caup p) (list '\ (cuup p))) x))
    (atom? x) x
    (: h (cap x) (?
      (id? h '\) (? (atom? (cuup x)) x (: r (lp x) (link '\ (cat (cap r) (list (sub (rmc cs (cap r)) (cup r) (cat (cap r) bnd)))))))
      (id? h ':) (: ns (ln (cup x)) (link ': (subl (rmc cs ns) (cup x) (cat ns bnd))))
      (: m (? (nom? h) (macros h) 0) (? (&& m (nil? (member? h bnd))) (sub cs (m (cup x)) bnd) (map (\ e (sub cs e bnd)) x))))))
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
; the flat-list adapter: a `:` binding list -> the rewritten list, or the input unchanged.
; ⚠ love.c interns "boxfix" and calls it off the book, so c0 compiles prel itself through
; this -- it must exist before ev.l does, and both compilers share this one pass.
(: (boxfix fs) (:
   (ev l) (? (atom? l) 1 (atom? (cup l)) 0 (ev (cuup l)))
   (bpr fs) (? (atom? (cup fs)) (link 0 (cap fs))
               (: r (bpr (cuup fs)) (link (link (dsug (cap fs) (caup fs)) (cap r)) (cup r))))
   (? (ev fs) fs
    (: pb (bpr fs) r (boxfix-core (cap pb) (cup pb))
       (? r (cat (catmap (\ p (list (cap p) (cup p))) (cap r)) (list (cup r))) fs)))))
