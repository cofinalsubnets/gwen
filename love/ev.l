; iop/apx: unforgeable tag markers (uninterned) shared by feel -- which emits (iop op . args) and
; (apx nary head . args) nodes -- and ana, which dispatches on them. ⚠ TOP-LEVEL globals, not
; ev-block locals, so they stay out of ev's captured frame (an ap closing over a sibling that
; captured iop/apx would drag both in dead -- see .feels).
(: iop (nom 'iop)
   apx (nom 'apx))

; gone: avw's not-in-book sentinel (the `(peep book x gone)` default). ⚠ a TOP-LEVEL global for
; the SAME reason as iop/apx, and one more: it folds into avw as a kim constant at compile time,
; and only a global resolves identically in every bootstrap generation.
(: gone (nom 'gone))

; gv: the LAYERED global read -- (books ()) is the live chain of books, walked head-first with
; `gone` as the per-layer miss sentinel (a stored () SHADOWS, never falls through), answering d on
; total miss. every book read in this file routes here, so a later layer can shadow orth without
; touching the compiler. the C twin is love.c's bookget; keep in step.
; the sentinel test is `==` (id?), never the pin `^gone`: a pin compiles to `=`, which
; walks whatever the layer stored, and every global read comes through here.
(: (gv k d) ((: (go c) (c @ (a . u) (: v (peep a k gone) (v == gone ? (go u) v)) d))
             (books ())))

; feels: value->ap memo for global fns, built once by scanning every global. a ap folds an
; application to a value when its args are constant (pure fns only), else emits an (iop op . args)
; node iap inlines (any nif of matching arity), else leaves it. a nif is found by text shape
; (lvm_ret0 terminator); fold eligibility from the curated `names`. it lives as a global -- feel's
; fold reaches it by name -- so the table is NOT baked into ev's value; `.ev` then prints as just
; the ana+feel logic, and the table prints under `.feels`.
; `names`/`pureset` are LIFTED to the top level so feel's INLINER (cprop, below) shares the same
; effect-free set its purity gates read. pureset is keyed by VALUE.
(: pureset (: names ('(+ - * / % & | < <= = >= >) +
          '(// << >> ^
            id? gcd modpow abs
            link cap cup caap caup cuap cuup
            dig lidx assoc member? last rev cat
            charm? nom? two? tablet? lit? string? nil? gem? twin? atom?
            snip string intern
            re im conj arg gem twin
            sine cosine log fpow))
   (foldl (\ t s (: v (gv s ()) (? v (pin t v 1) t))) {} names)))
; the CONTAINER DOORS -- pure or not depending on WHAT they are handed, which is why they
; cannot sit in pureset. on a tablet peep reads mutable state and pin writes it; on text or a
; chain neither can -- pin has no cell there, so it answers a FRESH container and the call is
; a value like any other. keyed by value, like pureset. ⚠ a tray is immutable too, but @(..)
; reads as a `tuple` CALL, so no tray ever reaches a fold site known -- the gate below would
; never fire on one and does not name it.
(: readset (: names '(peep pin)
   (foldl (\ t s (: v (gv s ()) (? v (pin t v 1) t))) {} names)))
(: feels (:
   lvm_cur  (peek 0 (+))
   lvm_ret0 (peek 1 cap)
   (bake v) (charm? v ? v ['\ v])           ; value -> source: fixnum bare else quote
   ; (op . arity) iff v is a nif. `id?` (identity) not `=`: a non-nif global may peep to a word
   ; that looks like a heap pointer, which `=`/eql would deref and crash on.
   (opof v) (? (lvm_ret0 == peek 1 v) (peek 0 v . 1)
               (lvm_cur == peek 0 v && lvm_ret0 == peek 3 v) (peek 2 v . peek 1 v))
   (hgen v op ar pure x) (: as >x n (tally as)
    (go hv a) (atom? a ? (bake hv)             ; all args constant -> fold to a value
     (: cv (kconst <a)
      (? (pure && <cv) (go (hv >cv) >a)         ; static arg: fold it into the partial
         (op && n = ar) (iop . op . as)              ; matching-arity nif -> inline (the hot path, keep)
         (id? hv v)                                            ; no static prefix folded -> the original apply
          (1 < ar && ar <= n ? (apx . ar . x)   ;   multi-arg fn the args saturate -> n-ary apply
                               (apx . 0 . x))   ;   generic l2r apply
         (apx . 0 . bake hv . a))))             ; static prefix folded -> apply the partial to the rest
    (go v as))
   ; ⚠ the operator symbols are quoted one by one -- bare mid-list they would capture their left
   ; neighbor when this file compiles (the opfix pass).
   (add t s) (: v (gv s ())
    (? (charm? v) t
     (: o (opof v) p (pureset v)
      (? (o || p)
       (: op (? o <o 0)
          ar (? o >o (id? lvm_cur (peek 0 v)) (peek 1 v) 0)   ; nif/multi-arg-fn arity
          (pin t v (hgen v op ar p)))
       t))))
   (foldl add {} (catmap keys (books ())))))

(:- (\ x (:
 c (sco 0 [()] ())
 y (feel c (opfix x))    ; opfix (factor sigils), feel (desugar) -- ev = the deserialized-data lane
 (ana c y (k0 c) 0 0)))
  lvm_cur (peek 0 (+))
  (sco p a i) {'arg a 'imp i 'par p}
  (rootc c) (: p (c 'par) (nil? p ? c (rootc p)))   ; top scope: where feel stashes 'src
  ; --- THE EMISSION INTERFACE (the codegen seam; the design in one page:) ---
  ; ana walks the analyzed form and emits code through these primitives. an EMITTER is a curried
  ; (\ k n ..): k = the continuation emitter, n = the current cell index (the thread builds
  ; BACKWARD, poke -1 prepending). `compose` sequences emitters -- (compose A B) emits A, then B,
  ; then k. the leaf emitters:
  ;   kim x    -- a quote / constant            karg i  -- an arg ref at frame index i
  ;   em1 op   -- a 1-word inline vm op          em2 op x -- a 2-word op (op + operand)
  ;   kap n    -- an n-ary apply (peeps the continuation for lvm_ret -> a tail call)
  ;   aco      -- a cond (below);  k0/k0s -- thread terminators (start a thread)
  ; fuse/spa/spq are THIS backend's peepholes; ava (avw, the scope walker) resolves a variable to
  ; karg (a local, at its [imps args] frame slot) / kim (a global constant) / em2 lvm_index.
  ; this is the THREADED (bytecode) backend, the portable default every target runs. a NATIVE
  ; backend (love/glaze/emit.l, x86-64, opt-in) implements the SAME leaf interface over a byte
  ; buffer -- kim -> movabs, karg -> mov [rcx+8i] + a fixnum guard, aco -> cmp+jcc -- today via the
  ; ala creation hook (book['natjit]) for leaf bodies, with kap (a call) DEOPTing into this thread.
  (p2 i x) (poke -1 i * poke -1 x)
  (em1 x k) (poke -1 x * k * (+ 1))
  (em2 i x k) (poke -1 i * poke -1 x * k * (+ 2))
  ; karg/kim: emit an arg ref / a quote, fusing with an immediately-following 1-ary apply (lvm_ap)
  ; or tail-call (lvm_tap) that kap already wrote into the continuation head -- overwrite that op
  ; cell with the operand (seek 1 keeps what follows) and prepend the fused op. kap over-reserves a
  ; word, so the reclaimed cell becomes -1 slack (spin memsets it; GC-safe).
  ; operand-value specialization: a zero-operand opcode for the hottest arg indices / quote
  ; constants, emitted in the plain path. a compare chain beats a `peep` here -- `peep` is a
  ; heavyweight polymorphic nif (type-dispatch + hash + eql) where a few fixnum compares
  ; short-circuit on the common small values (a hash made boot +13% vs +5% for this chain).
  ; ⚠ the charm? guard keeps spq from ever hashing a heap value.
  (spa i) (i @ 0 lvm_arg0 1 lvm_arg1 2 lvm_arg2 3 lvm_arg3 0)
  (spq v) (charm? v ? (v @ 0 lvm_quo0 1 lvm_quo1 2 lvm_quo2
                           3 lvm_quo3 -1 lvm_quom1 -2 lvm_quom2 0) 0)
  ; --- run fusion: the peephole's SECOND axis --------------------------------------
  ; spa/spq specialize on an operand's VALUE; these specialize on the SHAPE of a RUN --
  ; one op for two consecutive loads (lvm_aa/aq/qa/qq, and aap/aqp/qap/qqp when the
  ; second load carries the apply). Measured on the corpus + the benches: half of all
  ; runs are a lone load, but the pairs behind them are worth ~25% of every load
  ; dispatch -- (- n 1) and friends emit `quote;arg`, an l2r call emits `quote;argap`.
  ; NO LOOKAHEAD IS NEEDED: code builds backward, so a load's successor is already in
  ; place at `tail` when it emits. GROWING is the same move the ap-fusion above makes --
  ; overwrite the successor's OP cell with our operand and prepend the fused op, which
  ; leaves the successor's own operands exactly where they sat, backpatch sites (qsite)
  ; included. a successor that was operand-SPECIALIZED carries no operand cell, so its
  ; implicit value is written out first; that is the only case spending the second
  ; reserved word.
  ;
  ; how to grow the op at the head is READ OFF THE OP WORD, which `h` already holds.
  ; nothing is remembered between emits: the shape of an instruction is a function of
  ; its op word alone, so the three questions -- what letter is it, does it carry an
  ; explicit operand cell, and what value does a specialized one imply -- are each a
  ; plain lookup on `h`. an op that is not a growable load is absent from `rsh`, which
  ; is every ap-fused, run-fused and consumer-fused op: a pair never grows into a
  ; triple and nothing grows onto a tap, with no bookkeeping to say so.
  rsh {lvm_arg 1 lvm_arg0 1 lvm_arg1 1 lvm_arg2 1 lvm_arg3 1 lvm_quote 2 lvm_quo0 2 lvm_quo1 2 lvm_quo2 2 lvm_quo3 2 lvm_quom1 2 lvm_quom2 2 lvm_argap 3 lvm_quoteap 4}
  ; the specialized loads and the operand each implies. absent = wide: the op carries an
  ; explicit operand cell instead, so arg0/quo0 sit here at 0 rather than being left out.
  rimm {lvm_arg0 0 lvm_arg1 1 lvm_arg2 2 lvm_arg3 3 lvm_quo0 0 lvm_quo1 1 lvm_quo2 2 lvm_quo3 3 lvm_quom1 -1 lvm_quom2 -2}
  ; the SHAPE CODE names a 1-load run: 1 a, 2 q, 3 A(argap), 4 Q(quoteap). gop grows one
  ; by a leading load (gs: 1 arg, 2 quote), and a PAIR is where it stops -- length 3 was
  ; built, measured and removed. Sixteen more opcodes bought 0.71% of corpus cycles where
  ; one predicate/cond op bought 4.3%; a longer run just chunks into pairs.
  (gop c gs) (gs @ 1 (c @ 1 lvm_aa 2 lvm_aq 3 lvm_aap lvm_aqp)
                     (c @ 1 lvm_qa 2 lvm_qq 3 lvm_qap lvm_qqp))
  ;                                       a=ap-op o=tap-op(0=none) t=plain-op sp=specializer
  ; pc/tc are the shape codes to memoize for the ap-fused and the plain op;
  ; gs is OUR letter (1=arg, 2=quote), read against a memoized head.
  ; --- the OTHER axis: fuse a load with the op that CONSUMES it -----------------
  ; the runs above are only half the picture -- something eats them, and on the corpus
  ; that consumer is an accessor, a predicate or a branch, not arithmetic: cup 133.7M,
  ; `?` 128.8M, cap 80.7M, two? 77.5M, against `+` at 5.5M. 64% of every load dispatch
  ; goes into the first four. `cfuse` answers the fused op for a consumer we know, 0
  ; otherwise; the emit is the SAME move as everywhere else here (overwrite the
  ; consumer's op cell with our index, prepend the fused op), because all four consumers
  ; keep whatever followed them exactly where it was -- `?`'s else-address included.
  ; arg only: a quote consumed by one of these folds at compile time anyway.
  ; the consumer ops are NIF bodies, not `insts` entries, so they are not names in the
  ; book -- read them off the nif VALUE the way opof/feels does ((peek 0 f) is a 1-arg
  ; nif's code slot). lvm_cond is an inst and resolves by name.
  lvm_cup_ (peek 0 cup) lvm_cap_ (peek 0 cap) lvm_two_ (peek 0 two?)
  ; `==` (id?), not the pin `^`: h is a raw op ADDRESS and a pin compiles to `=`, which
  ; would deref it. () when the head is not a known consumer.
  (cfuse h) (? (h == lvm_cup_) lvm_argcup (h == lvm_cap_) lvm_argcap
               (h == lvm_two_) lvm_argtwo (h == lvm_cond) lvm_argcond)
  ; the rung above: load + PREDICATE + cond in one op. Only 7.1M conds test a bare local;
  ; 86.1M test a fused load+accessor, and `(? (two? b) ..)` alone is 64.4M. h2 is the cell
  ; AFTER the predicate's op -- always readable, since a thread always has its terminator
  ; past the last instruction. The triple consumes the predicate's op cell AND the cond's,
  ; so it costs NO new word: op at tail, index over the cond, else-address left where it
  ; already sat at tail+2. Overwriting the cond cell is the same move the argcond arm
  ; makes and is safe for the same reason -- nothing JUMPS to a cond, only falls into it
  ; (aco's targets are the alt = rest's head, and the join).
  ; ONLY two?. cap/cup were built and measured too and are NOT here: at RUNTIME an
  ; argcap/argcup is followed by a cond 21.7M times, but at EMIT time cap/cup sit before
  ; a cond in 8 sites out of 2497 -- those runtime adjacencies come from branch joins and
  ; let bodies, where the cell after the accessor was not yet the cond. Two opcodes that
  ; fire ~never are worse than none.
  (cfuse2 h h2) (h2 == lvm_cond && h == lvm_two_ && lvm_argtwocond)
  (fuse a o t sp pc tc gs x k n)
   (: tail (k (2 + n)) h (peek 0 tail)
      cf (gs = 1 ? (cfuse h))              ; arg only; () when the head is not a known consumer
      c2 (gs = 1 ? (cfuse2 h (peek 1 tail)))   ; ... and the load+predicate+cond triple
    ; the guard is `!== ()`, not a bare `cf`: cf is a raw op ADDRESS, and asking one for its
    ; truth sends `net` into it as a heap object -- the answer is then the machine code at
    ; that address, not whether we found a consumer. `==` is id?, and dereferences nothing.
    ; (the `(? s ..)` spa/spq test below is the same shape, working by luck of address.)
    (? (h == lvm_ap)       (p2 a x (seek 1 tail))
       (o && h == lvm_tap) (p2 o x (seek 1 tail))
       (c2 !== ())         (poke 1 x tail, poke 0 c2 tail)
       (cf !== ())         (p2 cf x (seek 1 tail))
       (: c (rsh h)
        (? c (p2 (gop c gs) x (rimm h @ () (seek 1 tail) i (poke -1 i (seek 1 tail))))
           (: s (sp x) (? s (poke -1 s tail)
                            (p2 t x tail)))))))
  ; arg fuses both ap and tap; quote only ap. ⚠ quote;tap (a constant arg to a tail call) is left
  ; unfused: overwriting the shared tap cell in some emit contexts corrupts a branch's jump target.
  (karg x k n) (fuse lvm_argap   lvm_argtap lvm_arg   spa 3 1 1 x k n)
  (kim  x k n) (fuse lvm_quoteap 0           lvm_quote spq 4 2 2 x k n)
  (k0 c n) (poke -1 lvm_ret (poke (1 + n) (ary c) (spin (2 + n))))
  ; k0s: like k0 but reserves one extra LEADING cell for ala to stash the lambda's source \-expr at
  ; value[-1], where the printer (gzput_fn) finds it. entry = cell 1, src = cell 0.
  (k0s c n) (poke -1 lvm_ret (poke (2 + n) (ary c) (spin (3 + n))))
  (ary c) (#(c 'arg) + #(c 'imp))
  (pro f) (charm? f ? f (: a (peek 0 f) (?- f
   (a == lvm_unc) (pro (seek -2 (peek 2 f)))
   (a == lvm_cur) (?- f (lvm_unc == peek 2 f)
                          (pro (seek -2 (peek 4 f)))))))
  ; feel: source->source pre-pass before `ana` -- expands macros, constant-folds, and marks applies
  ; with their n-ary-vs-l2r strategy. tail position is NOT tracked here -- kap detects tail by
  ; peeking the continuation for lvm_ret at analysis time.
  (feel c x) (:
   (wx x bnd)
    (: h <x (h @
     ^x x
     '\ (atom? >>x ? x (wxl1 c x (lp x) bnd))
     '? (atom? >x ? '? (keep x (wxc >x bnd) '?))
     ': (? (atom? >x) ':
           (atom? >>x) (keep x (keep1 >x (wx <>x bnd)) ':)
           (keep x (wxl (boxfix (lflat >x)) bnd) ':))   ; boxfix = the capture-by-location source rewrite, post-macroexpand
     ; no macro lane: cprop's output is MACRO-FREE -- it expands every nom-headed call (under the
     ; same shadow guard) and emits COMPOUND heads walked (h2, below).
     (fold (mapi (e \ wx e bnd) x) bnd)))
   ; lflat: (: a b (: c d e)) -> (: a b c d e). a body-position `:` is the same letrec* scope one
   ; frame and one apply deeper, so splice its list up. three gates: BOTH lets carry a body (a
   ; body-less inner would hand the outer's names its defglob lane), the names are DISJOINT, and no
   ; inner name is read by an outer def -- merging widens every outer def's scope over the inner
   ; names, and either would rebind or capture where the nesting shadowed. it re-runs on its own
   ; answer, so a chain of any depth lands in one frame.
   (lend bs) (? (atom? bs) ()
                (atom? >bs) bs
                (lend >>bs))             ; the body's one-cell tail, 0 = body-less
   (lsub? (': . u)) (two? (lend u))                 ; a spliceable inner let?
   (lup bs) (atom? >bs ? ><bs (<bs . lup >bs))               ; the body's list, in its place
   ; lrd/lrdl: does x read any name in ms? the whole ROW in one walk, where wl asks per name --
   ; and no macro lane, cprop's output having none (the same reading wx's own fold takes).
   (lrd ms x bnd) (?
    (nom? x) (member? x ms && nil? (member? x bnd))
    (atom? x) 0
    (<x == '\) (atom? >>x ? 0 (: r (lp x) (lrd ms >r (<r + bnd))))
    (<x == ':) (lrdl ms >x (ln >x + bnd))
    (any (\ e (lrd ms e bnd)) x))
   (lrdl ms bs bnd) (? (atom? bs) 0
    (atom? >bs) 0                                                    ; the trailing body is not a def
    (: nd (dsug <bs <>bs) (lrd ms >nd bnd || lrdl ms >>bs bnd)))
   (lflat bs) (: t (lend bs)
    (? !(two? t && lsub? <t) bs
     (: b <t ns (ln bs) ms (ln >b)
      (? (any (\ m (member? m ns)) ms) bs                            ; a name collision
         (lrdl ms bs ns) bs                                          ; an outer def reads an inner name
         (lflat (lup bs))))))
   ; --- the identity kit: unchanged -> the ORIGINAL, no copy --------------------------------
   ; cprop/wx hand back the input tree wherever nothing rewrote, so an untouched subtree costs zero
   ; allocation (and fold/apx/'src share its spine). ⚠ the shape law is love-alloc's: a `:`-bound
   ; non-tail call pays ~112b where an arg/tail-position call is FREE -- so every walk result rides
   ; to its comparison as a sibling-call ARGUMENT, never a fresh binding.
   (keep x r h) (r == >x ? x (h . r))                        ; the walked tail vs the original
   (keep2 x h r) (h == <x && r == >x ? x (h . r))           ; walked head AND tail vs the original
   (keep1 b e) (e == <b ? b [e])                             ; the one-element list
   (keep3 b p q r) (p == <b && q == <>b && r == >>b ? b (p . q . r))   ; the three-slot spine
   (mapi f x) (atom? x ? x (mapi2 x (f <x) (mapi f >x)))      ; the identity-preserving map
   (mapi2 x e r) (e == <x && r == >x ? x (e . r))
   (relam x r b) (b == >r ? x ('\ . <r + [b]))              ; \ rebuild; r = (lp x), b the walked body (lp's body IS (last x))
   (wxl1 c x r bnd) (wxl2 c x (relam x r (wx >r (<r + bnd))))   ; wx's \ lane, framed for arg-position walks
   (wxl2 c x lam) (: _ (push c 'src (>lam . x)) lam)            ; tagged-body -> surface src for ala (stash even on identity: keeps the assoc order)
   (wxl bs bnd) (wxb bs (ln bs + bnd))        ; a let's names all shadow throughout
   (wxb bs bnd) (?
    (atom? bs) bs
    (atom? >bs) (keep1 bs (wx <bs bnd))
    (: nd (dsug <bs <>bs)               ; desugar (f a..) so wx's \ adds params to bnd
     (keep3 bs <nd (wx >nd bnd) (wxb >>bs bnd))))
   ; cflip: the nil?-flip peephole on a cond clause list. since the cond tests g_false == nil?,
   ; (? (nil? e) c [el]) and (? e [el] c) are equal -- flipping the branches drops the nil? wrapper
   ; (and lets cst then fold a now-bare constant test). fires ONLY on an EXACT (nil? e) test in a
   ; 2- or 3-form clause; a longer clause list keeps its tail unflipped.
   (cflip bs) (two? >bs && two? <bs && id? <<bs 'nil? && atom? >><bs ?
    (: e <><bs c <>bs rest >>bs
     (? (atom? rest)       [e () c]                ; (nil? e) c    -> (e () c)
        (atom? >rest) [e <rest c]        ; (nil? e) c el -> (e el c)
        bs))
    bs)
   ; cst: re-read the shared `kconst` (const? . value, prel) as (const? . falsy?) -- the constant's
   ; truth is exactly nil? of its value, so a folded test prunes its dead branch. one recognizer
   ; across scopes that can't otherwise share.
   (cst t) (: c (kconst t)
              (? <c (1 . !>c)       ; kconst knows it (atom / quote literal)
                 (atom? >t && atom? <t && !(nom? <t)) (1 . !<t)  ; PLUS (c) == c: a folded monadic (opfix wraps (! e) -> ((nil? e)), so a folded test lands wrapped)
                 0))
   (wxc bs bnd) (: fb (? (member? 'nil? bnd) bs (cflip bs))
    (? (atom? fb) [()]                           ; clauses ran out: implicit () else (nil-ontology)
       (atom? >fb) (keep1 fb (wx <fb bnd))      ; the lone else
       (: t (wx <fb bnd) k (cst t)               ; walk the test; is it a static constant?
        (? (atom? k) (keep3 fb t (wx <>fb bnd) (wxc >>fb bnd))   ; unknown -> keep the clause
           >k   (wxc >>fb bnd)               ; statically FALSE -> drop this clause, try the rest
                     [(wx <>fb bnd)]))))     ; statically TRUE  -> this result IS the cond, drop rest
   ; napof: v is a non-nif multi-arg fn the n>1 call args SATURATE -> its arity, the width of the
   ; leading n-ary apply. a SURPLUS arg curries onto that answer instead of onto the head, one
   ; frame in place of one closure per arg. 0 = no n-ary lane.
   ; `id?`-safe peeps (a non-fn value never matches lvm_cur).
   (napof v args) (charm? v || !(id? lvm_cur (peek 0 v)) ? 0
    (: ar (peek 1 v) (1 < ar && ar <= #args ? ar 0)))
   ; fold: un-shadowed global head -> ap (fold/iop/apx); else mark the call as an apx node
   ; (apx n head . args) carrying the apply strategy: n > 1 = the leading n-ary apply's width,
   ; 0 = the l2r chain.
   (fold x bnd) (: h <x
    (? (atom? >x) x                          ; (f) single element -> ana's (atom? b) handles
       ; a VALUE head -- opfix's glued lane emits one, and a macro splice can too. no binding
       ; reaches a value, so it is a resolved global by construction and takes feels' lane
       ; straight; without this every `<x` in the tree would pay the generic apply.
       (nil? (nom? h)) (: hd (charm? h || two? h ? 0 (feels h))
                          (? hd (hd x) (apx . 0 . x)))
       (member? h bnd) (apx . 0 . x)                    ; a shadowed name: local head
       (: v (gv h ()) hd (feels v)
        (? hd (hd x) (apx . napof v >x . x)))))
   ; cprop: the partial-evaluation pre-pass. it threads a static VALUE env `se` (name . const-form)
   ; and (1) propagates a constant-bound var to its value, (2) FOLDS a pure-fn call on constant args
   ; -- gv resolves the nif and pureset, the same curated effect-free set feels' purity gates read,
   ; is the fold license; a bnd-shadowed head never folds (the alpha law). it does NOT prune conds
   ; (wxc does, downstream) and it does NOT unfold recursion (near-inert on this VM; true
   ; interpreter specialization lives in the offline partial evaluator, doc/misc/proto/mix.l). it runs
   ; before wx (pre-boxfix, pre-expansion); only an already-constant RHS propagates (a forward ref /
   ; cell read never is -- letrec-safe), and a non-constant rebind or a lambda param KILLS the name.
   (propk? v) <(kconst v)   ; a constant FORM? -- the const? bit of the shared kconst recognizer (prel; cf. cst, the falsy projection)
   (rmq k (a . u)) (<a @ ^k (rmq k u) (a . rmq k u))
   (cbake v) (v == () || !(nom? v || two? v) ? v ['\ v])                       ; folded value -> source
   (kval v) (atom? v ? v <>v)                                                  ; a const FORM -> its value
   allk (all propk?)                                                       ; all args constant?
   ; apply the fn to the consts. pure-head?'s licence covers the WHOLE saturated call -- a partial
   ; of a pure fn is pure at every arity -- so this keeps applying without re-asking.
   ; ⚠ it still owes cpgrew?. OVER-application turns the answer into a CHURCH numeral, the one
   ; unbounded lane, and it reaches that lane through a head pure-head? already cleared:
   ; (+ 1 200000 2) is ((200001) 2), a 60207-digit bignum baked into the image. a step that grows
   ; residualizes the rest, exactly as cpap does -- the bound belongs to both lanes, not one.
   (foldc o as) (atom? as ? (cbake o)
                 (: r (o (kval <as))
                  (cpgrew? o r ? (cbake o . as)
                                 (foldc r >as))))
   ; cpap: fold a head's KNOWN PREFIX, one operand at a time, for as long as the value in hand
   ; still applies purely -- so an unknown tail costs only itself: ("ab" "cd" x) leaves ("abcd" x)
   ; and (3 2 x) leaves (8 x). the predicate is re-read every step, so a value that stops being
   ; foldable simply ends the walk and gets baked as the residual head.
   ; ⚠ a NUMERIC head is held to a CHARM-SIZED answer. church application is the one lane where a
   ; tiny source form answers an unboundedly large value -- (200000 2) is a 25KB bignum, (3 f) a
   ; composition -- and folding BAKES that into the image. a star bounded by max-charm costs about
   ; what the form it replaced did, so (3 2), (2 3 4), (2 1.5) and a small twin all still fold.
   ; `star?` is doing size work, not order work: it drops the CONTAINERS, whose magnitude says
   ; nothing about how many cells they are.
   (cpgrew? v r) (constellation? v && nil? (star? r && abs r <= max-charm))
   (cpap v as) (: k (two? as ? (kconst <as))
    (? (atom? as) (cbake v)                                              ; every operand consumed -> the value
       (nil? (<k && pure-ap? v >k)) (cbake v . as)                      ; unknown operand or an impure apply -> stop
       (: r (v >k) (? (cpgrew? v r) (cbake v . as)                      ; ... or a numeral that left the charm band
                       (cpap r >as)))))                                  ; else take the step and walk on
   ; --- the known-lambda INLINER (cprop's beta-reduction lane) ----------------------------
   ; beta-reduce an application whose (resolved) head is a statically-known lambda. SOUND by three
   ; gates: the lambda is INLINABLE (non-recursive, body bsub-safe `simple?`, and CLOSED -- every
   ; free var a global, so it means the same at the call site), the args are PURE (`allp` -- so
   ; duplicating/dropping them is unobservable), and a FUEL bound caps the reduction cascade. the
   ; cascade runs through cprop's normal recursion, so an UNDER-SATURATED call substitutes the given
   ; args and answers a \ of the REST -- currying, statically -- collapsing curried HOFs to
   ; first-order arithmetic the back end (and the glaze) can take native. see doc/misc/proto/inl.l.
   ; ⚠ SOUNDNESS: beta IS NOT bridged by `=` on closures (the spec), so a beta-PRODUCED closure must
   ; never ESCAPE to where =/id?/show could observe it. the operator-position flag `op` is the
   ; guard: a beta result that is itself a lambda is committed only when CONSUMED (op=1, an
   ; enclosing application's head); at a VALUE position (op=0) we REVERT to the un-inlined source.
   ; the purity oracle, in ONE place: applying this FUNCTION VALUE to this KNOWN first operand
   ; is effect-free. the curated set is pure at every operand, so it ignores k; readset is pure
   ; only on an IMMUTABLE container, which is the whole reason k is a parameter -- every site
   ; that asks has the operand in hand by construction, so consulting it costs nothing.
   (pure-fn? v k) (pureset v || (readset v && (string? k || two? k)))
   ; ⚠ TWO operands, and THE ARITY IS THE PROTOCOL: under-applied this answers a closure, which
   ; is truthy, and every purity gate below silently opens. `as` is the ARG FORMS, not values --
   ; kconst declines a non-constant, so a site that cannot know its operand denies readset.
   (pure-head? h as) (pure-fn? (gv h ()) (: k (two? as ? (kconst <as)) (? <k >k)))
   ; pure-ap?: the same question for cpap's walk, widened by the DATA lanes -- which never RUN
   ; what they are handed: a sequence indexes or joins (text by byte, a chain by element), a point
   ; answers const-1, a numeric or array COMPOSES (it builds the composition, it does not call it).
   ; ⚠ a PARTIAL of a pure fn is not itself in pureset, so the walk stops at one: ((+) 1 2 x)
   ; folds ((+) 1) and no further. that is why readset pays off in the nom lane above -- peep
   ; wants three operands -- and not here. a sticky licence would close it; not today.
   ; ⚠ the zero point is named SEPARATELY: it applies through the very same sentinel a mint
   ; does, but `nom?` is false of it by design -- name? and mint? partition nom?, and () is in
   ; neither, so that absence never wears a real point's face. here it is just a point.
   (pure-ap? v k) (string? v || two? v || nom? v || v == () || constellation? v || pure-fn? v k)
   (allp as se) (all (a \ purep a se) as)                                  ; all args effect-free?
   (purep a se) (: h <a (?                                                                     ; effect-free -> duplicable/droppable
    (h = '\ || h = a) 1                                                                         ; nom / number / string
    (h = '?) (allp >a se)                                                  ; cond: pure iff every limb is
    (nom? h) (: e (assq h se)                                               ; a known pure lambda OR a pure fn, applied to pure args
     (? (two? e && lambp <>e) (allp >a se) (pure-head? h >a) (allp >a se)))))
   (smpl? x) (?                                                                        ; bsub-safe body: no : / macro head
    (atom? x) 1
    (<x == ':) 0
    (<x == '\) (atom? >>x || smpl? <>x)
    (nom? <x) (!(macros <x) && all smpl? x)
    (all smpl? x))
   (fvs x bs) (?                                                                       ; free vars of a simple term
    (nom? x) (x == () || member? x bs ? () [x])
    (atom? x) ()
    (id? <x '\) (two? >>x ? (: r (lp x) (fvs (last x) (<r + bs))))
    (foldl (a \ e \ a + fvs e bs) () x))
   (closedl? lam) (all (v \ gv v ()) (fvs lam ()))                            ; every free var a global (means the same elsewhere)
   (pbody? x bnd) (?                                                                   ; body is PURE: every call head a pure fn or a param
    (atom? x) 1
    (<x == '?) (all (e \ pbody? e bnd) >x)
    (<x == '\) (? (atom? >>x) 1 (: r (lp x) (pbody? (last x) (<r + bnd))))
    (nom? <x) ((pure-head? <x >x || member? <x bnd) && all (e \ pbody? e bnd) >x)
    (all (e \ pbody? e bnd) x))
   (inlinable? nm lam) (lambp lam && smpl? lam && closedl? lam && pbody? lam () && !(w nm lam 0 0 0))
   (rmkeys ps env) (filter (\ p !(member? <p ps)) env)                         ; drop shadowed substitutions
   (colnms bs) (atom? >bs ? ()                                                  ; the : letrec's binder NOMS (sugar already lowered to \ by cpbinds)
                (: n <bs (nom? n ? (n . colnms >>bs) (colnms >>bs))))
   (bsub env x) (?                                                                     ; capture-avoiding substitution
    (nom? x) (assq x env @ (_ . x) x x)
    (atom? x) x
    (id? <x '\) (atom? >>x ? x
       (: r (lp x) ps <r ren (map (p \ p . nom 0) ps)                      ; rename binders to fresh -> no capture
          ('\ . (map cup ren + [(bsub (ren + rmkeys ps env) (last x))]))))
    (id? <x ':) (: nms (colnms >x) ren (map (p \ p . nom 0) nms)       ; rename the :'s binders fresh so a subst can't be captured -- whole-form, which is
                                                                             ;   why an ENTANGLED : (a bare pre-pin sibling read) never reaches bsub (entg?)
       (map (\ e (bsub (ren + rmkeys nms env) e)) x))
    (map (\ e (bsub env e)) x))
   (beta lam as) (: r (lp lam) ps <r np #ps                               ; apply a lambda to args (curried)
    (? (#as < np)                                                             ; under-saturated -> substitute the given,
       (bsub (zip ps as) ('\ . drop #as ps + [(last lam)]))             ;   answer a \ of the REST (bsub's \ case
                                                                                   ;   renames the rest fresh -> capture-free)
       (: bd2 (bsub (zip ps (take np as)) (last lam)) rest (drop np as)
          (two? rest ? (bd2 . rest) bd2))))
   ; --- the BINDING SPLICE (the license's other half; laws: test/uuspllaw.l) --------------
   ; a saturated known-lambda call with an IMPURE arg no longer declines whole. pure args
   ; substitute as before (sl-sub). the LAST effectful arg substitutes too when its param is
   ; read EXACTLY ONCE, unconditionally, and the body is otherwise pure (sl-sub-1) -- it
   ; becomes the only body-resident effect, after every binding, so the call can vanish with
   ; no let at all. every other effectful arg binds ONCE to a fresh nom, in call order,
   ; ahead of the body -- `:` binds in source order, the call's own effect order (sl-ord) --
   ; and the body reads the nom at every use site (sl-lic: bind == call, value and trace,
   ; forall states). a read-many effectful form is never substituted (sl-flip: value equal,
   ; trace doubled -- the by-name breach R cannot convict).
   ; rdn: reads of param v in x. an occurrence under \ or ? counts DOUBLE (a site that can
   ; run zero or many times is never "exactly once"), a shadowing binder zeroes its subtree,
   ; a quote is data. cprop already desugared any `:` in x, so colnms reads its binders.
   (rdn v x) (?
    (nom? x) (x == v)
    (atom? x) 0
    (<x == '\) (atom? >>x ? 0 (: r (lp x) (member? v <r ? 0 (2 * (rdn v (last x))))))
    (<x == '?) (2 * foldl (a \ e \ a + rdn v e) 0 >x)
    (<x == ':) (member? v (colnms >x) ? 0 (foldl (a \ e \ a + rdn v e) 0 >x))
    (foldl (a \ e \ a + rdn v e) 0 x))
   (lasti ps as se) (atom? ps ? -1                                             ; index of the LAST effectful arg, -1 = none
    (: k (lasti >ps >as se)
     (0 <= k ? (1 + k) (purep <as se) -1 0)))
   (spl ps as se i si bd) (atom? ps ? '(())                               ; (binds . env), call order; si = the substitutable index
    (: r (spl >ps >as se (i + 1) si bd) p <ps a <as
     (? (purep a se) (<r . ((p . a) . >r))
        (i = si && 1 = rdn p bd) (<r . ((p . a) . >r))                  ; the licensed effectful substitution
        (: n (nom 0) ((n . a . <r) . ((p . n) . >r))))))
   (bspl x h h2 hr as se fl op bnd) (: r (lp hr) np #<r
    (#as < np ? (keep2 x h2 as)                                           ; under-saturated -> decline (a partial's effects fire at build time; the pure lane's curry bet is not licensed here)
     (: bd0 (last hr)
        ; a nom head resolved through se already passed pbody? at admission -- only a
        ; literal lambda head pays the walk here.
        bok (nom? h || pbody? hr ())
        sp (spl <r (take np as) se 0 (? bok (lasti <r (take np as) se) -1) bd0)
        bd (bsub >sp bd0)
        rest (drop np as)
      (op || two? rest || !(lambp bd) ?
         (: f2 (atom? <sp ? bd (': . (<sp + [bd])))                          ; no binding left -> no let: the call is gone
            (cprop (two? rest ? (f2 . rest) f2) se (fl - 1) op bnd))
         (keep2 x h2 as)))))                                                     ; a lambda body escaping at a value position -> decline (the escape law)
   (cplam x se fl op bnd) (atom? >>x ? x                                       ; (\ v) quote -> leave
    (: r (lp x) (relam x r (cprop >r (rmkeys <r se) fl 0 (<r + bnd)))))          ; the body is a value -> op 0
   ; the new-law guard (eager reads see let*, closures see letrec*): bsub renames a :'s
   ; binders over the WHOLE form -- letrec-wise -- but a bare read BEFORE its name's pin
   ; reads the OUTER binding, and that rename would capture it (the fresh nom then reads
   ; as a wild cell miss). a body carrying such an ENTANGLED : declines beta and the
   ; splice, and the call runs un-inlined. over-approximate on purpose: any occurrence of
   ; a not-yet-pinned sibling name in a def flags, even under a \ or a quote.
   (occn v e) (? (id? e v) 1 (atom? e) 0 (|| (occn v <e) (occn v >e)))
   (entg? x) (?
    (atom? x) 0
    (id? <x '\) (? (atom? >>x) 0 (entg? (last x)))
    (id? <x ':) (: nms (colnms >x)
       (walk bs pn) (?
        (atom? bs) 0
        (atom? >bs) (entg? <bs)
        (? (any (\ v (nil? (member? v pn) && occn v <>bs)) nms) 1
           (entg? <>bs) 1
           (walk >>bs (<bs . pn))))
       (walk >x ()))
    (any entg? x))
   (cpbinds bs se fl op bnd) (?
    (atom? bs) bs
    (atom? >bs) (keep1 bs (cprop <bs se fl op bnd))                              ; the let's BODY inherits the let's op
    (: nd (dsug <bs <>bs) nm <nd v1 (cprop >nd se fl 0 bnd)       ; a binding value is a value -> op 0
       se2 (propk? v1 || inlinable? nm v1 ?                                      ; a const OR a known pure lambda enters se
              ([nm v1] . rmq nm se) (rmq nm se))
       (keep3 bs nm v1 (cpbinds >>bs se2 fl op bnd))))
   ; bnd: the lexical binder set (\ params + : names), one-scope. its ONE reader is the macro lane:
   ; a locally-bound name is an ordinary variable, never a macro head (the shadow law; se can't
   ; stand in -- absence from se doesn't distinguish local from global).
   ; ⚠ the atom test stays OUT of the @ chain: as a pattern it would be the pin ^x,
   ; and a pin compiles to `=` -- a deep structural walk where this is one cheap
   ; test per node. in cprop, which runs on every node, that cost the bake 7 minutes.
   (cprop x se fl op bnd) (<x = x ?
    (nom? x ? (assq x se @ (_ . e . _) e x) x)               ; const/known-lambda var -> value
    (<x @
    '\ (cplam x se fl op bnd)
    ': (ln >x @ ns (keep x (cpbinds >x (rmkeys ns se) fl op (ns + bnd)) ':))
    '? (keep x (mapi (e \ cprop e se fl op bnd) >x) '?)              ; no prune here -- wxc does it
    h (: m (? (nom? h) (macros h) 0)                                          ; macro head, not shadowed? expand on RAW args -- a
       (? (m && !(member? h bnd)) (cprop (m >x) se fl op bnd)                  ; structural macro (a two?/cap probe) must see them
          (: as (mapi (e \ cprop e se fl 0 bnd) >x)
             hr (nom? h ? (: e (assq h se) (atom? e ? h <>e)) (cprop h se fl 1 bnd)) ; HEAD is consumed -> op 1
             h2 (nom? h ? h hr)                                                 ; the emitted head: a nom stays a nom (ana resolves it);
             hk (kconst hr)                                                       ; the head's own value, when it has one
             (? (0 < fl && lambp hr && two? as)
                 (? (entg? (last hr))                   ; an entangled : in the body -> no bsub lane is sound;
                     (keep2 x h2 as)                                                     ;   keep the call (boxfix's runtime law answers it)
                    (allp as se)                        ; two? as: (f) is f -- nothing to substitute
                     (: red (cprop (beta hr as) se (fl - 1) op bnd)                       ; KNOWN LAMBDA -> beta, re-cprop (fuel-bounded)
                      (op || !(lambp red) ? red (keep2 x h2 as)))                    ; escaping lambda result -> REVERT (keep the closure)
                    (bspl x h h2 hr as se fl op bnd))                                     ;    foldc/cpap are dead for a lambda head, so its decline keeps the call)
                (nom? h && allk as && !(member? h bnd) && pure-head? h as)            ; pure-fn on consts -> fold. bnd-guarded: a binder
                  (foldc (gv h ()) as)                                                ;   spelled like an op still SHADOWS (the alpha law)
                (two? as && propk? <as && <hk && pure-ap? >hk (kval <as))             ; a PURE-APPLYING head with a KNOWN operand: fold the whole
                  (cpap >hk as)                                                       ;   known prefix, an unknown tail keeps the value as its head
                (keep2 x h2 as)))))))                                                ; nothing rewrote -> the ORIGINAL call
   (wx (cprop x () 64 0 ()) ()))   ; bnd seeds (): a 0 seed rode `list + 0` into every binder set
  ; a bare symbol is a variable reference -- EXCEPT the zero point, which reads as a nom? literal.
  ; ⚠ routing it to ava captures it as a SPURIOUS import: it inflates a closure's arity by one with
  ; no matching value applied at the ref site -> an under-saturated partial (silently wrong). the
  ; test is id?-the-zero-point, NOT nil?: boxfix's cell names are fresh nameless mints (also nil?)
  ; bound and referenced as real variables, so they must still reach ava.
  ; ⚠ iop/apx stay `=` tests in the cond, NOT ^-pins in an @ chain: a pin reads the
  ; name to match against, and these two are letrec siblings this form is compiled
  ; BEFORE -- the pattern's read finds the cell empty and the whole lane goes missing.
  (ana c x) (? (nom? x && x != ())  (ava c x)
               (atom? x) (kim x)
               (: a <x b >x (?
                (atom? b) (ana c a)
                (a = '? ) (aco c b)
                (a = '\ ) (? (atom? >b) (kim <b) (ana c (ala c () b 0 1)))
                (a = ': ) (ale c <b >b)
                (a = iop) (iap c b)
                (a = apx) (apxh c b)
                (app c a b))))     ; a VALUE head: ala's (entry . imports) -- apply the built closure
                                  ; to its captured imports. never a macro (feel expands every
                                  ; nom-headed call), so no (macros a) consult here.
  (push c k x) (pin c k (x . c k), x)
  (pop c k) (c k @ (a . u) (pin c k u, a))
  (iap c b) (: op <b as >b                       ; (iop op . args) -> inline the vm op
   (atom? >as ? (ana c <as * em1 op)      ; one arg
      (: s (c 'stk) k (apr2l c as) (pin c 'stk s, k * em1 op)))) ; n-ary, r2l args
  ; falook: walk the scope chain for head's fixed arity, recorded by ale in each enclosing let's
  ; 'far map. > 0 = a multi-arg lambda sibling of that arity; 0 = a binding of that name that is NOT
  ; an n-ary-callable fixed-arity lambda -- it SHADOWS outer fars, so stop; -1 = not bound in this
  ; scope, keep walking. so a saturated call to a letrec lambda sibling can take the one-shot n-ary
  ; apply (apn/tapn) instead of currying, the same as a book global.
  (falook c h) (?- 0 c
   (: far (c 'far) a (tablet? far ? (peep far h -1) -1)
    (? (0 < a) a (-1 = a) (falook (c 'par) h) 0)))
  ; apxh: (apx n head . args) -> apply. n > 1 -> r2l eval of the leading n args and one n-ary
  ; apply, with any surplus arg curried onto the ANSWER l2r; n = 0 -> the plain 1-ary chain (kap
  ; detects tail vs non-tail by peeking the continuation). the width comes from a book-global
  ; multi-arg fn (fold/napof) OR a letrec lambda sibling the args saturate (falook -- locals fold
  ; can't see).
  (apxh c b) (: nary <b head <>b args >>b
              ; ⚠ THE SURPLUS LANE IS napof's ALONE. lvm_apn enters the callee at cell+2, PAST the
              ; lvm_cur prologue, so `apn n` is only sound where n IS the callee's arity -- napof
              ; reads that off the value itself, but falook reads a name out of a compile-time
              ; far map and can answer an ENCLOSING binding's arity where an inner scope rebinds
              ; the name (its walk passes frames carrying no far at all). Guessing n < the real
              ; arity then skips the saturation check and enters a body short of its arguments.
              ; Exact saturation is the most a guess can carry.
              n2 (? nary nary (nom? head && (: a (falook c head) (1 < a && a = #args && a))))
              f (ana c head) s (c 'stk) _ (push c 'stk 0) s2 (c 'stk)
              g (? n2 (napn c args n2 s2) (apl2r c args))
              (pin c 'stk s, f * g))
  ; napn: the n-ary apply, then the surplus. the pin puts the compile-time stack back at the head
  ; slot -- apn consumes its arg slots and leaves the answer there, which is where l2r addresses from.
  (napn c args n s) (: r (drop n args)
                       g (apr2l c (take n args) * kap n)
                     (pin c 'stk s,
                      atom? r ? g (g * apl2r c r)))
  (app c a b) (: f (ana c a)
                 s (c 'stk)
                 g (push c 'stk 0,
                    apl2r c b)
               (pin c 'stk s, f * g))
 (apl2r c b) (?- id (two? b) (: f (ana c <b) g (apl2r c >b) (f * kap 1 * g)))
 (apr2l c b) (?- id (two? b) (: g (apr2l c >b) f (ana c <b) (push c 'stk 0, g * f)))
 (kap n k m)
  (:- (p j)
    j (k (2 + m))
    p (peek 0 j == lvm_ret ?                              ; peep the continuation for ret -> tail
       (n > 1 ? (poke -1 lvm_tapn * poke 0 n) (poke 0 lvm_tap))
       (n > 1 ? (p2 lvm_apn n) (poke -1 lvm_ap))))

  ; aco -- compile a cond (? test result .. else). jump-threaded: each test, on false, branches
  ; (lvm_cond) to the next clause; on true, runs its result and JUMPS to the join (the cond's
  ; continuation k). the wrapper records k on the 'join stack for the duration. a constant test
  ; (pro folds it to a quote) picks its branch outright, no code.
  (aco c b) (:- (: f (clauses b) (\ k n (: k2 (f (push c 'join * k) n) (pop c 'join, k2))))
   ; tojoin: after a result, jump to the join (top of 'join) -- but fuse a ret/tap/tapn join INLINE
   ; rather than emit a jump straight to a return.
   (tojoin k n) (:
    j (k $ 3 + n)
    a <(c 'join)
    i (peek 0 a)
    (? (lvm_ret == i || lvm_tap == i) (p2 i (peek 1 a) j)
       (lvm_tapn == i)                (p2 i (peek 1 a) (poke -1 (peek 2 a) j))
                                      (p2 lvm_jump a j)))
   ; clauses: test/result pairs then a trailing else. a constant test folds to one branch; else emit
   ; lvm_cond -- true runs `then` + jumps to join, false falls to `rest`.
   (clauses b) (?
    (atom? b) (kim ())                                ; no else (feel usually supplies one)
    (atom? >b) (ana c <b * tojoin)            ; trailing else: result, then join
    (: test (ana c <b)
       then (ana c <>b)
       rest (clauses >>b)
       (? (kim = pro test) (? (peek 3 test) then rest)   ; constant test -> pick a branch
        (\ x (test (\ n
         (: k (push c 'alt * rest x)                  ; k = the false branch (the rest)
            j (then (tojoin k) $ 2 + n)                    ; true: then, jump to join, into k
            (p2 lvm_cond (pop c 'alt) j)))))))))

  ; recursive-fn ref: bake `quote code` if the closure is built, else `quote 0` + a backpatch site
  ; (lfd . box-of-cell) on the scope for wire to resolve. ⚠ the cell rides a box (pin, barriered) --
  ; a raw poke into tenured data is the one write the precise barrier can't cover.
  (qsite site k n) (: cell (poke -1 0 * k $ 2 + n)
                      _ (pin >site 0 cell)
                    (poke -1 lvm_quote cell))
  ; a closure CELL is a box (tablet 0) holding the closure chain (entry . imports): mkc makes one,
  ; cof reads it, cput stores into it. ⚠ the letrec* capture fixpoint and the recursive-ref
  ; backpatch mutate THROUGH the box, so the shared mutable cell is a proper heap object.
  (mkc v) {0 v}
  (cof e) (peep >e 0 0)
  (cput e v) (pin >e 0 v, v)
  (lz c lfd scope)(: code <(cof lfd)
                   p (? code (em2 lvm_quote code)
                        (: site (lfd . {})
                         (pin scope 'sites (site . scope 'sites),
                          qsite site)))
                   q (push c 'stk 0,
                      apl2r c >(cof lfd))
                 (pop c 'stk,
                  p * q))
  (ava c x) (avw c c x)
  (shadowed? c d x) (: far (d 'far)
   (tablet? far && 0 <= peep far x -1 && !(member? x (d 'par 'stk))))
  (avw c d x)
   (? !d ; outside all lexical scopes?
    (: y (gv x gone) (y == gone ? (em2 lvm_index x) (kim y)))   ; ==, not the pin: see gv
    (: lfd (assq x (d 'lam)) home (d == c)
     (? (two? lfd) (lz c lfd d)
        (!home && shadowed? c d x)
         (c 'par ? (push c 'imp x), em2 lvm_index x)
        (: s (d 'stk)
           (stki d2) (lidx x (d2 'imp + d2 'arg))
           (q i j m) (karg (i + stki c) j m)
         (?- (avw c (d 'par) x)
          (member? x s) (? home (karg (lidx x s))
                         (c 'par ? (push c 'imp x), q #(c 'stk)))
          (stki d >= 0) (? home (q #(c 'stk))
                         (c 'par ? (push c 'imp x), q #(c 'stk))))))))
  ; lambda analyzer
  (ala c imp exp nm hq) (:
   d (sco c (init exp) imp)
   k (ana d (last exp) (k0s d))
   a (ary d)
   e ((a = 1 ? k (em2 lvm_cur a k)) 0)           ; entry = cell 1 (cell 0 is the spare src slot)
   os (assq exp (rootc c 'src))
   s ('\ . (d 'imp + (? (two? os) >>os exp)))
   hk (poke -1 s e,                                  ; value[-1] = source \-expr
       trim (seek -1 e),                             ; tag head spans [src .. body]; value stays e
       gv 'natjit 0)
   ((!hk ? e (hq ? (hk e s a gv (farq c) (c 'lam) nm) e)) . d 'imp))
  (farq k x) (? !k 0
               (: f (k 'far)
                  (? (tablet? f && 0 <= peep f x -1) 1 (farq (k 'par) x))))

  (ale c a b) (?
   (atom? b) (ana c a)
   (:- (wire <cb <>cb >>cb (closures <cb))
    q (sco c (c 'arg) (c 'imp))
    ; collect: gather (name . def) chains in source order -> (prs body . even). the body is the
    ; trailing odd form; even = a body-LESS let (last value is result, names defglob).
    (collect prs n d rest) (: nd (dsug n d)
     (? (atom? rest)       (rev (nd . prs) . <nd . 1)   ; body-less: last NAME is the body
        (atom? >rest)      (rev (nd . prs) . <rest . 0)   ; a trailing body form
                           (collect (nd . prs) <rest <>rest >>rest)))
    cb (collect 0 a <b >b)
    ; far: {name -> arity} for the lambda siblings, SYNTACTIC (binder count off the \-expr), stable
    ; through the backpatch rebuild that zeroes closure cells. only fixed-arity multi-arg lambdas
    ; carry one; a value binding, single-arg lambda, or a rebound name pins 0 -> the call stays l2r.
    ; ⚠ pinned BEFORE the closures analyze their bodies (and weave's re-analysis), so falook sees it.
    (farof ps t) (?- t (two? ps)
     (: pr <ps k <pr d >pr
        a (? (-1 = peep t k -1 && lambp d) (#>d - 1) 0)
      (farof >ps (pin t k (1 < a ? a 0)))))
    _ (pin q 'far (farof <cb {}))
    ; closures: each lambda def -> a closure via ala (a non-lambda -> just a stack slot), building
    ; [names..] on the stack; then PROPAGATE captures to a fixpoint, and return the lambdas with the
    ; let's OWN names dropped from each capture set (those are locals, resolved by wire).
    (closures prs) (:- (fixcaps l)
     s (c 'stk)
     _ (push c 'stk 0)
     (split a ps) (?
      (atom? ps) a
      !(lambp ><ps)
       (push c 'stk <<ps,
        split a >ps)
      (: k <<ps
         v (ala q () >><ps 0 0)                  ; hq 0: the first-build closure is cleared and rebuilt by weave, so no creation hook
         a2 ((k . mkc v) . a)
       (push c 'stk k,
        split a2 >ps)))
     l (split 0 prs)
     _ (pin c 'stk s)
     ; capture propagation: if k1 references sibling k2, k2 must ALSO capture k1's imports (mutual
     ; recursion). `pass1` sweeps all ordered pairs ONCE, pouring k1's vars into k2 IN PLACE (cput);
     ; `monofix` repeats the pass until `capsize` stops growing -- the pass mutates the cells and
     ; returns the SAME list, so monofix's snapshot-BEFORE size catches each pass's growth.
     (pour e1 e2) (each >(cof e1)                         ; pour e1's imports into e2 (skip dups; prepend)
        (var \ member? var >(cof e2) ? 0 (cput e2 (<(cof e2) . var . >(cof e2)))))
     (pass1 lst k1 k2) (? (atom? k1) lst                       ; one full sweep over ordered pairs k1 x k2
        (atom? k2) (pass1 lst >k1 lst)                    ; k2 done -> next k1, reset k2
        (k1 != k2 && member? <<k1 >(cof <k2))   ; k2 references k1
         (pour <k1 <k2, pass1 lst k1 >k2)
        (pass1 lst k1 >k2))
     (capsize lst) (foldl (a \ e \ a + #>(cof e)) 0 lst)   ; total imports across closures (the measure)
     (fixcaps lst) (: done (monofix lst (st \ pass1 st st st) capsize)
                      j (map cap done)
                      (dropj x) (<x . mkc (<(cof x) . foldl (flip ldel) >(cof x) j))
                      (map dropj done)))

   (wire prs body even lams) (:
    ; weave: emit the binding defs left-to-right. a lambda def is RE-analyzed with its final import
    ; set (and backpatched in); a top-level body-less let's name is defglob'd.
    (weave nds) (? (atom? nds) id
     (: nd <nds n <nd d >nd
        d (?- d (lambp d) (: cls (assoc n lams)
                             x (ala q >(cof cls) >d n 1)
                             ; ⚠ the import row is FROZEN here: every ref site that already
                             ; compiled applied (cup (cof cls)), so growth means those sites
                             ; under-apply -- scare, never miscompile.
                           (#>x != #>(cof cls) ? (scare 'imports-grew n),
                            cput cls x)))
        f (ana c d)
        g (?- id (even && !(c 'par) && !(id? n '_)) (em2 lvm_defglob n))  ; `_` binds locally, never to the book
        h (push c 'stk n,
           weave >nds)
        (f * g * h)))
    s (pin q 'lam lams,
       c 'stk)
    g (push c 'stk 0,
    ; clear stale first-build closures so a ref hit during the rebuild defers to a backpatch site;
    ; the import sets are kept.
       each lams (e \ cput e (0 . >(cof e))),
       weave prs)
    ; closures final -> backpatch each recorded recursive-fn ref with its text.
    f (each (q 'sites) (s \ poke 0 <(cof <s) (peep >s 0 0)),
       pin q 'sites 0,
       pin c 'stk s,
       ; body-lambda analyzed AFTER inits (it takes the bindings as args; never makes sites)
       ana c ('\ . revcat (map cap prs) [body]))
    h (kap (tally prs))
    (pin c 'stk s,
     f * g * h)))))
