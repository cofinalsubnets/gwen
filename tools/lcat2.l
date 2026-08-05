; lcat2: lcat + variable-name minification. Like lcat, it serializes a l
; source file to a C string literal (re-print each top-level form via show,
; space-join, drop spaces the reader doesn't need, C-escape). lcat2 ALSO renames
; local variables to short fresh names before re-printing, shrinking the literal.
;
; --- scope-local renaming with name reuse --- the renamer is a scope-threaded
; walk carrying two things down the scope tree:
;   env  = assoc (orig-sym . new-sym), innermost binding first -- a reference is
;          renamed by the nearest enclosing binding (assoc), so kept bindings map to
;          themselves and correctly SHADOW an outer renamed binding of the same name.
;   used = the new names live on the CURRENT path (root..here). A scope may not reuse
;          a name in `used` (would capture an outer reference), but SIBLING scopes
;          reset `used`, so two disjoint functions both get `a`. That reuse is the
;          whole point: the scarce 1-char names recycle across the ~dozens of sibling
;          helper scopes instead of being spent globally.
; New names come from a namespace FRESH to the file: shortlex over a-zA-Z (a..z A..Z
; aa ab ..) skipping any name that already occurs as a source symbol (`cnt`) -- so a
; fresh name can only collide with another fresh name, which `used` already forbids.
; Correctness then needs nothing more: a free ref resolves unchanged (fresh names
; dodge every global), a leaking top-level `:` keeps its names (they're the cross-file
; API the assembled headers share), and quote subtrees are data so they're left whole.
;
; --- count-priority --- within each scope the bindings are assigned in occurrence-
; count order (desc): the most-used local claims the shortest available name, since a
; scope's byte cost is sum(count * newlen). A binding is renamed only when its fresh
; name is strictly shorter than the original; otherwise it is kept (mapped to itself
; so shadowing still works) and costs no name.
;
; --- factoring: a macroexpanded core + a thin surface adapter --- the renaming proper
; lives in `mxrn` (below), which is CORRECT FOR MACROEXPANDED INPUT: it knows only the
; core special forms `\` (lambda, and the one-operand quote) and `:` (letrec*), plus
; application -- on expanded input nothing else can bind. That is the piece meant to
; move into the compiler, which already macroexpands. lcat2 works on UN-expanded source,
; so a surface wrapper `rn` adapts the few binding macros the compiler would have
; expanded away: `:-` (body-first let) is reordered to a `:` and back, and `let` maps
; to `:`. mxrn takes the recursion as a
; link `rec`: the compiler calls (mxrn x env used top mxrn); lcat2 passes `rn`, so nested
; macros get adapted too. Macro names are folded into `cnt` so no fresh name can shadow a
; macro head (which the compiler expands by name even past a local binding).

; cnt = every source symbol -> its occurrence count (also the fresh-name avoid-set);
; taint = the frozen "do not rename" set (empty for the current corpus). both mutated in place.
(: cnt (tablet 0) taint (tablet 0))

; === CORE -- naming-with-reuse for MACROEXPANDED forms (the part destined for the
; compiler). Knows only `\` / `:` / application; recurses through the link `rec`.
; Inputs are the two globals above: `cnt` (counts = avoid-set + count order) and
; `taint` (frozen symbols -- empty for genuinely macroexpanded input).
;
; TO LIFT INTO THE COMPILER: take this whole CORE block plus the two globals, and
;   * drop the `rec` parameter in favour of self-recursion -- expanded input has no
;     surface macros to intercept, so mxrn just calls mxrn (and rn-let/rn-binds call
;     mxrn). lcat2 only needs `rec` because it feeds in UN-expanded source.
;   * feed `cnt` from the compiler's symbol info (occurrence counts), and leave `taint`
;     empty -- or repurpose it as a general frozen "do not rename" set.
;   * keep relying on the prel binding-shape helpers lp/lambp/ln (already present).
; The SURFACE block below (collect + rn with its :-/let handling) stays in
; lcat2: it is exactly the un-expanded-source adaptation the compiler no longer needs. ===
(: (sl-char d) (? (< d 26) (+ 97 d) (+ 65 (- d 26)))   ; shortlex over a-zA-Z (bijective base-52):
   (sl-digits n acc) (? (< n 1) acc (: m (+ -1 n) (sl-digits (/ m 52) (link (sl-char (% m 52)) acc))))
   (shortlex k) (string (sl-digits (+ 1 k) 0))         ; 0->a .. 25->z 26->A .. 51->Z 52->aa
   (freshname? nm) (= 0 (peep cnt nm 0))                ; nm occurs as no source symbol
   (avail? nm u) (&& (freshname? nm) !(has? nm u))     ; ..and not live on this path
   (pickname u) ((: (g k) (: nm (intern (shortlex k)) (? (avail? nm u) nm (g (+ 1 k))))) 0)
   (cntle a b) (: ca (peep cnt a 0) cb (peep cnt b 0)    ; order a scope's bindings: count desc,
       (? (> ca cb) 1 (< ca cb) 0                      ; then length desc, then name -- deterministic
          (: la (tally (string a)) lb (tally (string b)) (? (> la lb) 1 (< la lb) 0 (< a b)))))
   ; assign fresh names to a scope's compose-live bindings (count order). a frozen (tainted)
   ; or can't-shrink binding is kept (mapped to itself, no name spent). -> (env-frame . used').
   (assign syms used) ((: (g ss m u) (? (two? ss)
         (: s (cap ss) nm (pickname u)
            (? (&& !(peep taint s 0) (< (tally nm) (tally (string s))))
               (: i (intern nm) (g (cup ss) (link (link s i) m) (link i u)))
               (g (cup ss) (link (link s s) m) u)))
         (link m u))) (sortby cntle syms) 0 used)
   (mapped s m) (: p (assoc s m) (? p (cup p) s))
   (flat lhs) (? (nom? lhs) (link lhs 0) (: r (flat (cap lhs)) (link (cap r) (cat (cup r) (cup lhs)))))  ; unwind : head-sugar
   (lhs-rename lhs bmap am) (? (nom? lhs) (mapped lhs (cat am bmap))  ; name via bmap, params via am
       (two? lhs) (link (lhs-rename (cap lhs) bmap am) (lhs-rename (cup lhs) bmap am)) lhs)
   (bodyless? bs) (? (atom? bs) 1 (atom? (cup bs)) 0 (bodyless? (cuup bs)))
   ; rebuild a : binding list; each binding's sugar params open a fresh sub-scope of its
   ; value (siblings all assign from u1, so their names recycle). recurse via `rec`.
   (rn-binds bs bmap env1 u1 rec) (?
     (&& (two? bs) (two? (cup bs)))
       (: lhs (cap bs) val (caup bs) f (flat lhs) r (assign (cup f) u1) am (cap r) u2 (cup r)
          penv (cat am env1) lhs2 (lhs-rename lhs bmap am) val2 (rec val penv u2 0 rec)
          (link lhs2 (link val2 (rn-binds (cuup bs) bmap env1 u1 rec))))
     (two? bs) (link (rec (cap bs) env1 u1 0 rec) 0)         ; trailing body
     0)
   (rn-let bs env used top rec) (: names (ln bs) leak (? top (bodyless? bs) 0)
       r (? leak (link 0 used) (assign names used)) bmap (cap r) u1 (cup r) env1 (cat bmap env)
       (link ': (rn-binds bs bmap env1 u1 rec)))
   ; the macroexpanded renamer. top=1 only at a file form; a body-less top-level : leaks
   ; (its names are the global API -> kept). env/used thread scope; a one-operand `\`
   ; (quote) is data and is passed whole.
   (mxrn x env used top rec) (?
     (nom? x) (mapped x env)
     (atom? x) x
     (= '\ (cap x)) (? (lambp x)
        (: lb (lp x) ps (cap lb) bd (cup lb) r (assign ps used) am (cap r) u2 (cup r)
           env2 (cat am env) ps2 (map (\ p (mapped p am)) ps)
           (link '\ (cat ps2 (list (rec bd env2 u2 0 rec)))))
        x)
     (= ': (cap x)) (rn-let (cup x) env used top rec)
     (map (\ e (rec e env used 0 rec)) x)))

; === lcat2 SURFACE -- builds the core's inputs (cnt/taint) from un-expanded source, and
; adapts the binding macros the compiler would have already expanded away. `rn` is
; the recursion the core threads, so nested `:-`/`let` are adapted at every level. ===
(: (collect x) (? (nom? x) (pin cnt x (+ 1 (peep cnt x 0)))
     (two? x) (: _ (collect (cap x)) (collect (cup x))) 0)
   (rn x env used top rec) (? (two? x)
       (? (= ':- (cap x)) (: rl (rn-let (cat (cuup x) (list (caup x))) env used top rec)  ; :- = body-first :
             rb (cup rl) (link ':- (link (last rb) (init rb))))                             ; reorder body back
          (= 'let (cap x)) (rn-let (cup x) env used top rec)                            ; let = :
          (mxrn x env used top rec))
       (mxrn x env used top rec)))

; --- minify (identical to lcat): drop a space only where the two flanking bytes
; re-parse to the same datum count without it; then C-escape the survivors. ---
(: (nforms s) ((: (g x n) (: r (sound x) (? (two? r) (g (cup r) (+ 1 n)) n)))
                  s 0)
   ; the valence-law guard (cf. lcat.l): a space between an operator byte and
   ; a datum start is semantic mid-list (glued = monadic); the probe below
   ; tests at head position where fusion is suppressed, so keep it.
   (opb? c) (? (= c 32) 0 (= c 10) 0 (= c 9) 0 (= c 92) 0
               (= c 40) 0 (= c 41) 0 (= c 91) 0 (= c 93) 0 (= c 123) 0 (= c 125) 0
               (= c 34) 0 (= c 59) 0 (= c 39) 0 (= c 96) 0 (= c 44) 0
               (= c 64) 0 (= c 126) 0 (= c 35) 0
               (&& (<= 48 c) (<= c 57)) 0
               (&& (<= 65 c) (<= c 90)) 0
               (&& (<= 97 c) (<= c 122)) 0
               (= c 95) 0
               1)
   (dstart? b) (? (= b 41) 0 (= b 93) 0 (= b 125) 0 (= b 59) 0
                  (= b 32) 0 (= b 10) 0 (= b 9) 0
                  1)
   (redundant? a b) (? (&& (opb? a) (dstart? b)) 0
                       (= (nforms (string `(a b))) (nforms (string `(a 32 b)))))
   (eb c) (? (= c 10) (say out "\\n")
              (| (= c 92) (= c 34)) (: _ (put out 92) (put out c))
              (put out c)))

(: (readforms x) (: r (sound x) (? (two? r) (link (cap r) (readforms (cup r))) 0))
   (joinforms fs) (? (atom? fs) "" (atom? (cup fs)) (show (rn (cap fs) 0 0 1 rn))
       (+ (+ (show (rn (cap fs) 0 0 1 rn)) " ") (joinforms (cup fs))))
   (emit acc) (: _ (put out 34)
       _ ((: (g i pc instr) (? (< i (tally acc))
            (: c (peep acc i 0) (? instr
               (? (= c 92) (: _ (eb c) _ (eb (peep acc (+ i 1) 0)) (g (+ i 2) 0 1))
                  (: _ (eb c) (g (+ i 1) c (? (= c 34) 0 1))))
               (? (= c 32)
                  (? (redundant? pc (peep acc (+ i 1) 0)) (g (+ i 1) pc 0)
                     (: _ (eb c) (g (+ i 1) c 0)))
                  (: _ (eb c) (g (+ i 1) c (= c 34)))))) 0)) 0 0 0)
       (put out 34))
   p (open (cap (cup argv)) "r")
   _ (? p 0 (: _ (say err "lcat2: cannot open input") (quit 1)))
   forms (readforms (slurp p))
   _ (map (\ m (pin cnt m (+ 1 (peep cnt m 0)))) (keys (peep book 0 0)))  ; macro heads bypass scoping
   _ (map collect forms)                                        ; count symbols
   acc (joinforms forms)
   _ (emit acc)
   (put out 10))
