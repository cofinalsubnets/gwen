; glaze-data.l -- a PLAN: extend the glaze past the arithmetic kernel to the data workloads
; (the `tree` and `hash` benches), and the recognizer gate that says which shapes can come along.
; STATUS: stages A/B are built -- the `tree` traverse `ck` (the chain fold) compiles native
; (6.7x); `hash` (stages C/D) is still a study. The model at the bottom also reproduces the
; chain-vs-int SOUNDNESS gate that came with the build (cap-into-arith).
;
; the standing fact (measured on this host, bench/): the benches where love trails the
; compiled/JIT'd field are exactly the ones it runs INTERPRETED. the glaze (love/glaze/) is prepended
; only for float/fib/tak/primes/strscan/deforest (bench/run.sh); everything else -- hash, tree,
; closure, sum, mapfilter, sort, strcat, bell -- rides the tree-threaded interpreter while its columns
; (chez/sbcl native, node/V8, luajit, pypy, clojure/JVM) are compiled. so those rows read "ai's
; interpreter vs everyone's compiler", and a good threaded interpreter is the usual 2-10x off native:
;   hash ms/it: ai 3.13 | chez 1.60 | sbcl 1.18 | node 1.29 | luajit 1.84 | pypy 0.94 | python 7.68
;   tree ms/it: ai 3.67 | chez 1.59 | racket 0.61 | sbcl 1.77 | node 3.35 | pypy 3.72 | luajit 7.35
; the A/B nails it: prepending the glaze to tree/hash changes NOTHING (245->265ms, 378->353ms -- the
; recognizers don't match), while the same glaze is ~12x on primes (interp 15.25 -> glazed 1.28 ms/it).
; the glaze is worth an order of magnitude on the code it owns; tree and hash just aren't in its grammar.
;
; --- WHY they aren't in the grammar --- the emitter (love/glaze/auto.l) compiles E := var | int |
; (OP E E) | (? E E E), OP arithmetic/comparison, with a first-order recursive-group extension (fib/
; tak/primes call each other; primes' named lets lambda-lift in). NOTHING in that grammar touches the
; heap as data. but tree is BUILT from `link` and walked by `cap`/`cup`/`two?`; hash is `pin`/`peep`
; over a map backing. those operators have no native lowering, so the bodies fall through to ev. the
; runnable model below PROVES the exclusion structurally: it walks each bench body and reports the
; exact heap operator that keeps it interpreted (mk needs `link`, ck needs `two? cap cup`, ins `pin`,
; scan `peep`) -- and confirms fib/primes need none, so the recognizer's yes-set is exactly the glazed
; benches.
;
; --- WHERE the interpreted time goes (decomposition, us/it) --- it is per-op DISPATCH, not GC and not
; table quality:
;   tree depth-16 (65535 nodes): mk(build+alloc+gc) 2550 | ck(traverse, 0 alloc) 1650 | mk+ck 4050.
;     pure traversal -- zero allocation -- is already 41% of the bench. the gen collector is healthy
;     (93% infant mortality, mostly minor colls), so copying is the smaller half; the cost is the
;     interpreted recursion over generic `cap`/`cup`/`two?`/`+`, ~25 ns/node.
;   hash N=10000: insert(grows cap 4->16384, 12 doublings) 575 | scan(steady) 325 | bump(peep+pin) 850.
;     the table GROWTH is the cheap part. the bulk is per-key interpreted key-math `(+ 1 (* 97 i))` +
;     the nif call + generic `peep`/`pin`, x40000 across four passes. the hash fn `rot(x*mix)` and the
;     2-word open-addressed slots (load<3/4) are already good -- this is dispatch, not hashing.
; so the orthogonal alloc-attacks already in tree DON'T apply: gengc REAPS the churn cheaply (done --
; and tree's 93% mortality shows it working), and deforestation (doc/misc/proto/forest.l) AVOIDS intermediate
; lists -- but tree/hash have no throwaway intermediates to fuse; their allocation IS the result. only
; owning the loop, or cheapening dispatch, can move these.
;
; --- THE LAW that shapes the plan --- the glaze README's hard-won result: "a glaze wins only when it
; OWNS THE LOOP." the scalar hook (a native arith body called per element from an interpreted loop)
; measured ~25% SLOWER -- the call-boundary tax (marshal in, eat nif, decode out) beat interpreting a
; small body. only the loop-owning reduction won (~45x). so a per-NODE native function called from an
; interpreted tree recursion would LOSE by the same tax. the win requires owning the whole iteration.
;
; --- THE PLAN (staged; each stage MEASURES against ev before baking, per the scalar-hook lesson) ---
; the tractable wins are the ALLOCATION-FREE loops -- they own the iteration AND sidestep the one hard
; problem (allocating from native code under a moving collector):
;   stage A -- recognizer. extend auto.l's grammar to admit allocation-free chain/map READS as glaze
;     shapes: a `cap`/`cup`/`two?` fold and a `peep` probe loop. the `glaze-read?` predicate below IS
;     that gate -- it admits the readers, still rejects the allocators.
;   stage B -- emit the `ck`-class kernel: a native chain fold (pointer-chase cap/cup, accumulate a
;     fixnum, deopt on a non-chain). the direct generalization of the baked array reductions (asum/
;     aprod) from a packed array to a cons spine. allocation-free -> no GC interaction. removes the
;     1650us traverse half's dispatch. gated by memory latency (a 2MB depth-16 tree blows L2), so
;     expect less than the pure-arith loops but a real cut.
;   stage C -- emit the map READ probe loop (inline hash() + linear probe over the backing, accumulate).
;     allocation-free; smaller absolute prize (scan ~325us) but proves "the glaze owns a map read".
;   stage D -- FRONTIER, defer: allocation under the collector (tree `mk`, hash `ins`). native code that
;     bumps Hp must keep the two-space/gen invariants -- a GC mid-build forwards the half-built
;     structure, and the write barrier fires on old->young stores. two routes: (1) PRE-RESERVE one
;     `Have` for a statically-bounded build (tree depth is a literal -> 65535 cells up front, no GC in
;     the kernel) -- works for `mk`, not for data-dependent growth+rehash like `ins`; (2) GC-cooperative
;     native (the kernel publishes roots / is interruptible at safe points) -- the real method-JIT, a
;     much larger effort. recommend deferring D until B/C have paid.
; honest ceiling: even with B+C, the allocation halves (mk/ins) stay interpreted, so this alone does
; NOT reach chez/racket. the DEEPER, orthogonal lever is the interpreter dispatch itself -- interp-spec.l
; measured ~7x headroom there, and cheaper dispatch helps EVERY non-glazed bench (closure/sum/sort/...),
; not just these two. that is the bigger program; this file scopes the glaze half.
;
; the model: two recognizers over the bench bodies. glaze-now? = today's grammar (no heap touch at all).
; glaze-read? = stage A/B/C (allocation-free reads allowed, allocators still out). the asserts pin the
; staging: ck/scan cross from now-rejected to read-admitted; mk/ins stay out (they allocate -> stage D).
(:
  ; heap operators split by whether they ALLOCATE (producers) or only READ (no alloc).
  alloc-ops '(link pin pull tablet hash array iota jot map filter foldl foldr sort cask mint nom)
  read-ops  '(cap cup caap caup cuap cuup caaap caaup two? peep keys tally string)
  ; collect every operator from `ops` that appears anywhere in the source expr e (over-approximates:
  ; a head symbol in `ops` counts wherever it textually occurs -- exactly "does this body REQUIRE it").
  (collect e ops) (? (two? e)
                      (+ (? (member? (cap e) ops) [(cap e)] ())
                         (foldl (\ a x (+ a (collect x ops))) () e))
                      ())
  (allocs e) (collect e alloc-ops)                    ; heap producers this body needs
  (reads  e) (collect e read-ops)                     ; heap readers this body needs
  ; the two grammar gates. `!` of a list is true iff the list is empty (nothing needed).
  (glaze-now?  e) (&& (! (allocs e)) (! (reads e)))   ; today: no heap touch at all
  (glaze-read? e) (! (allocs e))                      ; stage A/B/C: reads ok, allocators still out

  ; the bench bodies (the lambda bodies, as source data) -- bench/benches/{tree,hash}.l, fib for contrast.
  fib  '(? (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))
  prim '(? (< n (* d d)) 1 (? (= 0 (% n d)) 0 (go (+ d 1))))                  ; primes' inner test loop
  mk   '(? (< d 1) 0 (link (mk (- d 1)) (mk (- d 1))))                        ; tree build  -- allocates
  ck   '(? (two? t) (+ 1 (+ (ck (cap t)) (ck (cup t)))) 0)                    ; tree walk   -- reads only
  ins  '(? (< i n) (: _ (pin h (+ 1 (* 97 i)) i) (ins h n (+ i 1))) h)        ; hash insert -- allocates (pin/grow)
  scan '(? (< i n) (scan h n (+ i 1) (+ acc (peep h (+ 1 (* 97 i)) 0))) acc)  ; hash read   -- reads only

  ; --- the recognizer reproduces reality: the glaze's yes-set is exactly the benches it speeds up ---
  _ (assert (glaze-now? fib)  "fib is in today's grammar (and it IS glazed, ~12x)")
  _ (assert (glaze-now? prim) "primes' loop is in today's grammar (glazed)")
  _ (assert !(glaze-now? mk)  "tree build is OUT today")
  _ (assert !(glaze-now? ck)  "tree walk is OUT today")
  _ (assert !(glaze-now? ins) "hash insert is OUT today")
  _ (assert !(glaze-now? scan)"hash read is OUT today")
  ; the offending operator is named precisely -- the structural reason each stays interpreted:
  _ (assert (member? 'link (allocs mk))  "mk needs link (heap pair)")
  _ (assert (member? 'two? (reads ck))   "ck needs two?/cap/cup (chain reads)")
  _ (assert (member? 'cap  (reads ck))   "ck chases cap")
  _ (assert (member? 'pin  (allocs ins)) "ins needs pin (map mutate+grow)")
  _ (assert (member? 'peep (reads scan)) "scan needs peep (map read)")
  _ (assert (! (allocs ck))   "ck allocates NOTHING -- the stage-B win")
  _ (assert (! (allocs scan)) "scan allocates NOTHING -- the stage-C win")

  ; --- stage A/B/C: admitting allocation-free reads pulls ck and scan in; mk/ins stay (stage D) ---
  _ (assert (glaze-read? ck)   "stage B: ck (read-only chain fold) becomes glazeable")
  _ (assert (glaze-read? scan) "stage C: scan (read-only probe loop) becomes glazeable")
  _ (assert !(glaze-read? mk)  "mk still out: it ALLOCATES -> stage D (pre-reserved bounded build)")
  _ (assert !(glaze-read? ins) "ins still out: it allocates+rehashes -> stage D (GC-cooperative)")
  ; fib/primes are read-loop-glazeable too (a fortiori -- they touch no heap):
  _ (assert (&& (glaze-read? fib) (glaze-read? prim)) "the arith benches stay in under the wider gate")

  ; --- STAGE A/B, AS BUILT: the build carries a SOUNDNESS gate ---
  ; glaze-read? (no allocators) is NECESSARY BUT NOT SUFFICIENT. (cap t) yields the TAGGED stored word
  ; putcharm(v), while the group compiler's body math runs on UNTAGGED ints -- so a chain word must
  ; never reach an arith/cmp operand ((+ (cap t) 0) would read 15 where 7 is meant). The real groupok?
  ; (love/glaze/auto.l) carries a chain-vs-int typing discipline; the model below reproduces its verdict.
  (cval? cps e) (? (member? e cps) 1  (&& (two? e) (member? (cap e) '(cap cup))) (cval? cps (caup e))  0)   ; e is a CHAIN word: a chain param, or cap/cup thereof
  (arith-on-chain? cps e) (? (atom? e) 0                                                                    ; a chain word feeding +/-/*/cmp -- the unsound shape glaze-read? misses
     (&& (member? (cap e) '(+ - * // % < <= > >= =)) (foldl (\ a x (? a 1 (cval? cps x))) 0 (cup e))) 1
     (foldl (\ a x (? a 1 (arith-on-chain? cps x))) 0 (cup e)))
  ck-cps '(t)                                                                                              ; ck's chain param (used in cap/cup/two?)
  bad    '(+ (cap t) 0)                                                                                    ; a cap feeding arithmetic
  _ (assert (! (arith-on-chain? ck-cps ck)) "ck: no chain word reaches arithmetic -> SOUND, glazes (Stage B: 6.7x)")
  _ (assert (arith-on-chain? ck-cps bad)    "(+ (cap t) 0): chain word in an int slot -> the real groupok? REJECTS it")
  _ (assert (glaze-read? bad)               "... yet glaze-read? (coverage only) ADMITS it -- exactly why the typing gate was added")

  _ (. ";; doc/misc/proto/glaze-data.l: glaze-coverage self-checks pass\n")
  _ (. ";;   STAGE A/B LANDED: ck glazes (native chain fold, 6.7x); the typing gate rejects (+ (cap t) 0)\n")
  _ (. ";;   today's grammar admits ") _ (. (show ['fib 'primes])) _ (. ", rejects tree/hash (heap ops)\n")
  _ (. ";;   tree walk  needs ") _ (. (show (reads ck)))   _ (. " (reads only)   -> stage B\n")
  _ (. ";;   tree build needs ") _ (. (show (allocs mk)))  _ (. " (allocator)    -> stage D\n")
  _ (. ";;   hash read  needs ") _ (. (show (reads scan))) _ (. " (read only)    -> stage C\n")
  _ (. ";;   hash write needs ") _ (. (show (allocs ins))) _ (. " (allocator)    -> stage D\n")
  (. ";;   the wide gate (reads allowed) pulls in ck+scan; mk+ins wait on native allocation\n"))
