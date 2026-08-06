; crew/moon/cpp.l -- the C preprocessor (rung 3 stage 5), TOKEN-BASED: it sits
; between clex and cparse, so identifiers inside string/char literals are never
; mistaken for macros (they lexed as one opaque token). a token is (kind val
; line); expansion tacks on a fourth element, a HIDESET (the macros a token may
; no longer be expanded by -- Prosser's blue paint, so FOO expanding to FOO
; stops). the pipeline: text -> clex -> cpp -> cparse. line splicing (\<nl>) and
; # / ## already fell out of the lexer.
;
; (cpp toks incf) -> (1 out-tokens) | (). incf resolves a #include: (incf name
; sys?) -> (1 text) | (), text then lexed and preprocessed sharing the macro
; table (so include guards work); () means no includes (the law harness).
; directives: #define (object + function, # stringize, ## paste, ... variadic
; __VA_ARGS__), #undef, #if/#ifdef/#ifndef/#elif/#else/#endif (with defined and
; an integer constant-expression evaluator, undefined names -> 0), #include,
; #error, #pragma/#line (ignored). predefined __LINE__ __FILE__ __STDC__.
; adjacent string literals concatenate (phase 6). fenced: _Pragma, __COUNTER__,
; the GNU ,##__VA_ARGS__ comma elision, #if with character constants.
(:
   ; -- token parts (a clex token is (kind val line); expansion adds a hideset) --
   (kof t) <t
   (vof t) <>t
   (lof t) <>>t
   (hof t) (? (3 < (tally t)) <>>>t ())
   ; a numeric token's SOURCE SPELLING, which its value cannot answer: 0777 and 511
   ; are one value, 1u and 1 are one value, and a float's value is not text at all.
   ; # owes the spelling (C11 6.10.3.2), so the lexer carries it and every rebuild
   ; below hands it on. () on a token the cpp minted itself (__LINE__, defined ..),
   ; where the value IS the spelling. ⚠ parse reuses this slot once cpp is done.
   (sof t) (? (4 < (tally t)) <>>>>t ())
   (tok k v l) `(k v l)
   ; -- include markers (doc/moon-diag.md rung 2) -- an #include splices the
   ; header's OWN line numbering into the one flat output stream, so a line
   ; number alone can't say which file it is (and isn't monotone across the
   ; splice). doinc brackets each included run with these; parse's ctag pass
   ; reads them into its position table and strips them, so no later stage
   ; ever sees one. 'in carries the header's name, 'out pops back.
   (fmin name) `('fmark 'in name)
   fmout `('fmark 'out ())
   (idt? t) ((kof t) = 'id)
   (idv? t v) (&& ((kof t) = 'id) ((vof t) = v))
   (pv? t v) (&& ((kof t) = 'p) ((vof t) = v))
   (kwv? t v) (&& ((kof t) = 'kw) ((vof t) = v))
   (addhide t hs)                                   ; union hs into t's hideset
    `((kof t) (vof t) (lof t) (dedup ((hof t) + hs)) (sof t))
   (dedup l) (: (go l seen) (? (! (two? l)) ()
                              (member? <l seen) (go >l seen)
                              (link <l (go >l (link <l seen))))
                (go l ()))

   ; -- the spelling of a token, for ## paste and # stringize --
   numks '(num unum ulnum llnum ullnum flo iflo)
   (spell t) (: k (kof t) s (sof t)
                (? (0 < (tally s)) s                          ; the spelling the lexer kept
                   (member? k numks) (show (vof t))           ; ..or a cpp-minted decimal
                   (k = 'kw) (show (vof t))
                   (k = 'str) ("\"" + (vof t) + "\"")
                   (vof t)))                          ; id / p are their text
   ; C11 6.10.3.2's stringize: the argument's own spelling, quotes and escapes and
   ; all. the standard's extra \ before each " and \ buys a text that RE-LEXES to
   ; this; we hand the value straight to codegen and never re-lex it, so the
   ; spelling IS the answer. ⚠ whitespace: one space between every pair of tokens,
   ; where C keeps `a+1` tight -- the lexer would have to record what it dropped.
   (spellcat ts) (: (go l acc) (? (! (two? l)) acc
                                  (go >l (acc + (? (acc = "") "" " ") + (spell <l))))
                    (go ts ""))

   ; -- macro table entries: (obj body) | (fun params body variadic?) --
   (mdef macs name) (peep macs name ())
   (mobj? m) (&& (two? m) (<m = 'obj))
   (mfun? m) (&& (two? m) (<m = 'fun))

   ; -- lex a spelling back into one token (## paste result) --
   (relex s line) (: r (clex s)
                     (? (! (two? r)) ()
                        (: ts <>r
                           (? (two? ts) (tok (kof <ts) (vof <ts) line) ()))))

   ; -- split off the first LOGICAL line (tokens sharing the head's line no.) --
   (linesplit toks) (: ln (lof <toks)
                       (go ts acc) (? (! (two? ts)) `((rev acc) ())
                                      ((lof <ts) = ln) (go >ts (link <ts acc))
                                      `((rev acc) ts))
                       (go toks ()))

   ; -- collect function-macro arguments: TS is positioned just after '('.
   ; returns (args rest) with args a list of token-lists (commas at depth 1
   ; split; nested parens preserved), rest past the closing ')'. () on error. --
   (collargs ts)
    (: (go ts depth cur args)
        (? (! (two? ts)) ()
           (: t <ts
              (? (&& (depth = 1) (pv? t ")")) `((rev (link (rev cur) args)) >ts)
                 (&& (depth = 1) (pv? t ",")) (go >ts depth () (link (rev cur) args))
                 (pv? t "(") (go >ts (depth + 1) (link t cur) args)
                 (pv? t ")") (go >ts (depth - 1) (link t cur) args)
                 (go >ts depth (link t cur) args))))
       (go ts 1 () ()))
   (argn args i) (: (go l k) (? (! (two? l)) () (k < 1) <l (go >l (k - 1))) (go args i))
   (paramidx params name) (: (go l i) (? (! (two? l)) -1 (<l = name) i (go >l (i + 1)))
                             (go params 0))

   ; -- ## paste: fold a token list, pasting across every (p "##") by joining
   ; the previous result token's spelling with the next token's. --
   (paste toks line)
    (: (go l acc) (? (! (two? l)) (rev acc)
                     (&& (pv? <l "##") (&& (two? acc) (two? >l)))
                      (: a <acc b <>l
                         r (relex ((spell a) + (spell b)) line)
                         (go >>l (link (? r r a) >acc)))
                     (go >l (link <l acc)))
       (go toks ()))

   ; -- substitute args into a function-macro body. a param preceded by # is
   ; stringized; a param that is an operand of ## substitutes UNEXPANDED; else
   ; it substitutes the fully-expanded arg. then ## pastes are folded. --
   ; (adjh is a closure-local, not a sibling -- a lambda PARAMETER named after a
   ; sibling :-local gets shadowed by boxfix's shared cell, so the macro-body
   ; array rides as `mb`, a nom bound nowhere else, and its ## check is inlined.)
   (subst mb params args var? line macs)
    (: (adjh i) (|| (&& (0 < i) (pv? (argn mb (i - 1)) "##"))
                    (&& ((i + 1) < (tally mb)) (pv? (argn mb (i + 1)) "##")))
       (go i acc)
        (? ((tally mb) <= i) (rev acc)
           (: t (argn mb i)
              (? (&& (pv? t "#") ((i + 1) < (tally mb)))    ; # param -> stringize
                  (: p (argn mb (i + 1))
                     pi (? (idt? p) (paramidx params (vof p)) -1)
                     va (&& var? (idv? p "__VA_ARGS__"))
                     (? (|| (-1 < pi) va)
                        (go (i + 2) (link (tok 'str (spellcat (? va (vaargs args params) (argn args pi))) line) acc))
                        (go (i + 1) (link t acc))))
                 (&& (idt? t) (-1 < (paramidx params (vof t))))   ; a named parameter
                  (: pi (paramidx params (vof t))
                     raw (argn args pi)
                     sub (? (adjh i) raw (expand raw macs))
                     (go (i + 1) (revcat sub acc)))
                 (&& var? (idv? t "__VA_ARGS__"))               ; the variadic tail
                  (: va (vaargs args params)
                     prevhh (&& (0 < i) (pv? (argn mb (i - 1)) "##"))
                     commahh (&& (&& prevhh (1 < i)) (pv? (argn mb (i - 2)) ","))
                     (? commahh                                 ; GNU `, ## __VA_ARGS__` elision: the ## is a marker, not a paste
                         (: acc2 >acc                       ; drop the ## already emitted (no join)
                            (? (two? va) (go (i + 1) (revcat (expand va macs) acc2))         ; args present: keep the comma
                               (go (i + 1) (? (&& (two? acc2) (pv? <acc2 ",")) >acc2 acc2))))  ; empty: drop the comma too
                        prevhh (go (i + 1) (revcat va acc))     ; a real `## __VA_ARGS__` paste: unexpanded, paste folds it
                        (go (i + 1) (revcat (expand va macs) acc))))
                 (go (i + 1) (link t acc)))))
       (paste (go 0 ()) line))
   (vaargs args params)                             ; the arg token-lists past the named params, comma-joined
    (: np (tally params)
       (skip l k) (? (|| (k < 1) (! (two? l))) l (skip >l (k - 1)))
       (join l) (? (! (two? l)) ()
                   (! (two? >l)) <l
                   (<l + (link (tok 'p "," 0) (join >l))))
       (join (skip args np)))

   ; -- the expander: rescan TOKS, replacing macro invocations (honoring
   ; hidesets), fully. mutually recursive with subst via expand-of-arg. --
   (expand toks macs)
    (? (! (two? toks)) ()
       (: t <toks rest >toks
          (? (! (idt? t)) (link t (expand rest macs))
             (idv? t "__LINE__") (link (tok 'num (lof t) (lof t)) (expand rest macs))
             (member? (vof t) (hof t)) (link t (expand rest macs))   ; blue-painted: leave it
             (: m (mdef macs (vof t))
                (? (mobj? m)
                    (: hs (link (vof t) (hof t))
                       body (mapadd <>m hs (lof t))
                       (expand (body + rest) macs))
                   (&& (mfun? m) (&& (two? rest) (pv? <rest "(")))
                    (: ca (collargs >rest)
                       (? (! (two? ca)) (link t (expand rest macs))
                          (: args <ca after <>ca
                             params <>m mbody <>>m mvar <>>>m
                             hs (link (vof t) (hof t))
                             body (subst mbody params args mvar (lof t) macs)
                             (expand ((mapadd body hs (lof t)) + after) macs))))
                   (link t (expand rest macs)))))))
   (mapadd toks hs line)                            ; give body tokens the hideset + the invocation line
    (? (! (two? toks)) ()
       (: t <toks (link `((kof t) (vof t) line (dedup ((hof t) + hs)) (sof t)) (mapadd >toks hs line))))

   ; -- #if / #elif integer constant-expression evaluation (undefined ids -> 0,
   ; defined X / defined(X) resolved first, then macro-expanded). --
   (resolve-defined ts macs)                        ; replace `defined X` / `defined ( X )` with 0/1
    (? (! (two? ts)) ()
       (idv? <ts "defined")
        (: r >ts
           (? (&& (two? r) (pv? <r "("))
               (: nm <>r
                  (link (tok 'num (? (two? (mdef macs (vof nm))) 1 0) 0)
                        (resolve-defined >>>r macs)))    ; skip ( name )
              (&& (two? r) (idt? <r))
               (link (tok 'num (? (two? (mdef macs (vof <r))) 1 0) 0) (resolve-defined >r macs))
              (resolve-defined r macs)))
       (link <ts (resolve-defined >ts macs)))
   (zero-ids ts) (? (! (two? ts)) ()               ; any surviving identifier is 0
                    (: t <ts (link (? (|| (idt? t) ((kof t) = 'kw)) (tok 'num 0 0) t) (zero-ids >ts))))
   (ifval ts macs) (: r0 (resolve-defined ts macs)
                      r1 (expand r0 macs)
                      r2 (zero-ids r1)
                      (ceval r2))

   ; a tiny precedence-climbing evaluator over cpp number tokens -> integer.
   (cprim ts)
    (? (! (two? ts)) `(0 ())
       (: t <ts
          (? (member? (kof t) '(num unum ulnum llnum ullnum)) `((vof t) >ts)
             (pv? t "(") (: r (ccond >ts) (? (&& (two? <>r) (pv? (cap (caup r)) ")")) `(<r (cup (caup r))) r))
             (pv? t "!") (: r (cprim >ts) `((? (nil? <r) 1 0) <>r))
             (pv? t "~") (: r (cprim >ts) `(((0 - <r) - 1) <>r))
             (pv? t "-") (: r (cprim >ts) `((0 - <r) <>r))
             (pv? t "+") (cprim >ts)
             `(0 >ts))))
   (clevel sub ops ts)
    (: r (sub ts)
       (go v rest) (: t (? (two? rest) <rest ())
                      hit (? (two? t) (cassoc (kof t) (vof t) ops) ())
                      (? (! hit) `(v rest)
                         (: r2 (sub >rest) (go (hit v <r2) <>r2))))
       (go <r <>r))
   (cassoc k v ops) (: (go l) (? (! (two? l)) () (&& ((cap (cap l)) = k) ((caup (cap l)) = v)) (caup (cup (cap l))) (go >l))
                       (go ops))
   (cmul ts) (clevel cprim `(`('p "*" (\ a b (* a b))) `('p "/" (\ a b (? (b = 0) 0 (// a b)))) `('p "%" (\ a b (? (b = 0) 0 (a - (* (// a b) b)))))) ts)
   (cadd ts) (clevel cmul `(`('p "+" (\ a b (a + b))) `('p "-" (\ a b (a - b)))) ts)
   (cshl ts) (clevel cadd `(`('p "<<" (\ a b (<< a b))) `('p ">>" (\ a b (>> a b)))) ts)
   (crel ts) (clevel cshl `(`('p "<" (\ a b (? (a < b) 1 0))) `('p "<=" (\ a b (? (a <= b) 1 0)))
                            `('p ">" (\ a b (? (b < a) 1 0))) `('p ">=" (\ a b (? (b <= a) 1 0)))) ts)
   (ceq ts) (clevel crel `(`('p "==" (\ a b (? (a = b) 1 0))) `('p "!=" (\ a b (? (a = b) 0 1)))) ts)
   (cband ts) (clevel ceq `(`('p "&" (\ a b (& a b)))) ts)
   (cbxor ts) (clevel cband `(`('p "^" (\ a b (^ a b)))) ts)
   (cbor ts) (clevel cbxor `(`('p "|" (\ a b (| a b)))) ts)
   (cland ts) (: r (cbor ts)
                 (go v rest) (? (&& (two? rest) (pv? <rest "&&"))
                                (: r2 (cbor >rest) (go (? (&& (0 < v) (0 < <r2)) 1 0) <>r2))
                                `(v rest))
                 (go <r <>r))
   (clor ts) (: r (cland ts)
                (go v rest) (? (&& (two? rest) (pv? <rest "||"))
                               (: r2 (cland >rest) (go (? (|| (0 < v) (0 < <r2)) 1 0) <>r2))
                               `(v rest))
                (go <r <>r))
   (ccond ts) (: r (clor ts)
                 (? (&& (two? <>r) (pv? (cap (caup r)) "?"))
                    (: rt (ccond (cup (caup r)))
                       (? (&& (two? <>rt) (pv? (cap (caup rt)) ":"))
                          (: re (ccond (cup (caup rt))) `((? (0 < <r) <rt <re) <>re))
                          r))
                    r))
   (ceval ts) (cap (ccond ts))

   ; -- parse a #define body into a macro entry --
   (defmacro macs line-toks)                        ; line-toks: after the leading (# define)
    (? (! (two? line-toks)) ()
       (: name (vof <line-toks)
          r >line-toks
          ; function-like ONLY if '(' immediately follows the name (no space) -- the
          ; lexer marks a glued '(' with a 4th token field (`#define F(x)` vs the
          ; object macro `#define EOF (-1)`, whose '(' is spaced and begins the body).
          (? (&& (two? r) (&& (pv? <r "(") (cauup (cup (cap r)))))
              (: pr (defparams >r () ())
                 (? (! (two? pr)) ()
                    (: params <pr var? <>pr body <>>pr
                       _ (pin macs name `('fun params body var?)) 1)))
             (: _ (pin macs name `('obj r)) 1))))
   (defparams ts params var?)                        ; -> (params variadic? body) | () ; ts after '('
    (? (! (two? ts)) ()
       (pv? <ts ")") `((rev params) var? >ts)    ; F() -- empty params; body follows the )
       (pv? <ts "...") (: r >ts (? (&& (two? r) (pv? <r ")")) `((rev params) 1 >r) ()))
       (idt? <ts)
        (: nm (vof <ts) r >ts
           (? (&& (two? r) (pv? <r ",")) (defparams >r (link nm params) var?)
              (&& (two? r) (pv? <r ")")) `((rev (link nm params)) var? >r)
              ()))
       ())

   ; -- adjacent string literal concatenation (phase 6) --
   ; it also RETIRES the file marks here, in the one pass that already rebuilds
   ; the whole spine -- so parse receives a clean stream and pays nothing for a
   ; separate strip. the SPANS it collects on the way -- ((index file)..),
   ; which file each stretch of the flat output came from -- land in a stash
   ; rather than riding back up, so the walk keeps its original shape: adding a
   ; return value would have meant either a rev of the whole stream or a pair
   ; allocated per token, and this pass runs over every token of every compile.
   ; ⚠ the join must see THROUGH a mark: host/main.c splices the egg by
   ; #include-ing bare string literals between two of its own ("(" then egg.h
   ; then "'(" ..), so the strings a mark sits between are exactly the ones
   ; that have to join. a mark consumed mid-join takes effect AFTER the merged
   ; token (index n + 1) -- the joined literal belongs to the file its FIRST
   ; piece came from.
   (fmark? t) ((kof t) = 'fmark)
   (fmpush t files) (? ((vof t) = 'in) (link (lof t) files) >files)
   (fmtop files) (? (two? files) <files ())
   spanacc (tablet 0)                               ; the spans, REVERSED, of the strcat-pass now running
   (spanat n files) (pin spanacc 'l (link `(n (fmtop files)) (peep spanacc 'l ())))
   ; -> the rest after a run of marks, the file stack they leave, and whether
   ; the next real token is a string (so the caller knows to join)
   (skipfm ts files) (? (&& (two? ts) (fmark? <ts)) (skipfm >ts (fmpush <ts files)) `(ts files))
   (strcat-pass ts n files)
    (? (! (two? ts)) ()
       (: t <ts
          (? (fmark? t)
              (: fs (fmpush t files) _ (spanat n fs) (strcat-pass >ts n fs))
             (! ((kof t) = 'str)) (link t (strcat-pass >ts (n + 1) files))
             (: sr (skipfm >ts files) nxt (cap sr) fs (caup sr)
                (? (! (&& (two? nxt) ((kof <nxt) = 'str))) (link t (strcat-pass >ts (n + 1) files))
                   ; the marks we just consumed take effect after the merged token
                   (: _ (? ((fmtop fs) = (fmtop files)) () (spanat (n + 1) fs))
                      (strcat-pass (link (tok 'str ((vof t) + (vof <nxt)) (lof t)) >nxt) n fs)))))))

   ; -- the driver: walk logical lines, run directives, expand active code.
   ; macs is a MUTABLE tablet (so #define/#undef persist and includes share it);
   ; cond is a stack of (active taken) frames; buf accumulates active code
   ; tokens, flushed (expanded) whenever a directive or EOF interrupts. --
   (dirname t) (? ((kof t) = 'kw) (show (vof t)) (vof t))   ; #if/#else lex as keywords -> their text
   (condactive cond) (? (! (two? cond)) 1 (cap (cap cond)))
   (parentactive cond) (? (! (two? cond)) 1 (cap (cap cond)))
   ; buf accumulates REVERSED (a cat per line re-copies the whole run -- love.c's
   ; longest directive-free run is ~50k tokens); flush restores source order.
   (flush buf macs out) (revcat (expand (rev buf) macs) out)
   (cppgo toks macs cond buf out incf)
    (? (! (two? toks)) (? (two? cond) 'cppbad (rev (flush buf macs out)))
       (: ls (linesplit toks) line <ls rest <>ls
          (? (pv? <line "#")
              (: out1 (flush buf macs out)
                 dir (? (two? >line) (dirname <>line) ())   ; if/else are keywords; the rest ids
                 body >>line
                 (? (member? dir '("if" "ifdef" "ifndef" "elif" "else" "endif"))
                     (: cond2 (docond dir body line macs cond)
                        (? (id? cond2 'err) 'cppbad             ; unbalanced #else/#elif/#endif -- FATAL, not empty
                           (cppgo rest macs cond2 () out1 incf)))
                    (! (condactive cond)) (cppgo rest macs cond () out1 incf)   ; skipped: ignore non-cond directives
                    (dir = "define") (: _ (defmacro macs body) (cppgo rest macs cond () out1 incf))
                    (dir = "undef") (: _ (pin macs (vof <body) ()) (cppgo rest macs cond () out1 incf))
                    (dir = "include") (doinc body macs cond out1 rest incf)
                    (dir = "error") (: _ (say err "cc: #error directive") _ (put err 10) 'cppbad)
                    (member? dir '("pragma" "line")) (cppgo rest macs cond () out1 incf)   ; ignored
                    (cppgo rest macs cond () out1 incf)))                              ; unknown directive: ignore
             (! (condactive cond)) (cppgo rest macs cond buf out incf)                 ; skipped code: drop
             (cppgo rest macs cond (revcat line buf) out incf))))                       ; active code: accumulate (reversed)

   ; a directive that manages the conditional stack. dir keyword symbols.
   (docond dir body line macs cond)
    (? (member? dir '("if" "ifdef" "ifndef"))
        (: pa (condactive cond)                        ; parent active?
           v (? (! pa) 0
                (dir = "ifdef") (? (two? (mdef macs (vof <body))) 1 0)
                (dir = "ifndef") (? (two? (mdef macs (vof <body))) 0 1)
                (? (0 < (ifval body macs)) 1 0))
           (link `((&& pa (0 < v)) (&& pa (0 < v))) cond))
       (dir = "elif")
        (? (! (two? cond)) 'err
           (: fr <cond pa (parentactive >cond) taken <>fr
              v (? (|| taken (! pa)) 0 (? (0 < (ifval body macs)) 1 0))
              (link `((&& (! taken) (&& pa (0 < v))) (|| taken (0 < v))) >cond)))
       (dir = "else")
        (? (! (two? cond)) 'err
           (: fr <cond pa (parentactive >cond) taken <>fr
              (link `((&& (! taken) pa) 1) >cond)))
       (dir = "endif") (? (two? cond) >cond 'err)
       'err)

   ; #include: resolve via incf, lex, and preprocess sharing macs. incf may
   ; answer (1 text child-incf): the child resolves the INCLUDED file's own
   ; quote includes relative to ITS dir (gcc's rule -- host/cb.c's unity
   ; include of ../crew/quay/quay.c needs quay.c's "quay.h" found beside it);
   ; a plain (1 text) keeps this incf (the laws feed includes as data).
   (doinc body macs cond out rest incf)
    (? (! (condactive cond)) (cppgo rest macs cond () out incf)
       (: sys? (&& (two? body) (pv? <body "<"))
          name (incname body)
          txt (? name (incf name sys?) ())
          spelt (? sys? ("<" + name + ">") ("\"" + name + "\""))
          (? (! (two? txt)) (: _ (say err ("cc: cannot resolve #include " + spelt)) _ (put err 10) 'cppbad)  ; unresolved -> FATAL
             (: lr (clex <>txt)                         ; sentinel, NOT () -- () masks as an empty include and the ancestor
                (? (! (two? lr)) (: _ (say err ("cc: lex error in " + spelt)) _ (put err 10) 'cppbad)         ; doinc drops its accumulated `out`, reordering the stream
                   (: itoks <>lr
                      chf (: c <>>txt (? c c incf))
                      ; the marks bracket the child's run. `out` is the REVERSED
                      ; accumulator, so a link onto it appends in source order:
                      ; the 'in mark lands before the header's tokens, the 'out
                      ; mark onto (rev done) lands after them.
                      done (cppgo itoks macs () () (link (fmin name) out) chf)   ; recurse with FRESH cond; shares macs; accumulates
                      (? (id? done 'cppbad) 'cppbad           ; a nested failure PROPAGATES up (never wipe `out` for it)
                         (cppgo rest macs cond () (link fmout (rev done)) incf))))))))
   (incname body)                                     ; the header name from #include tokens
    (? (! (two? body)) ()
       ((kof <body) = 'str) (vof <body)       ; "file"
       (pv? <body "<") (: (go l acc) (? (! (two? l)) () (pv? <l ">") acc (go >l (acc + (spell <l)))) (go >body ""))
       ())

   (cpp toks incf fname)                              ; -> (1 out) | ()
    (: macs (: t (tablet 0)
               _ (pin t "__STDC__" `('obj `((tok 'num 1 0))))
               ; WHO IS COMPILING. every other predefine here says something about
               ; the language or the machine; this one says the compiler, and it is
               ; the seam a source uses to pick a spelling only we accept -- the
               ; kernel's asmops.h reaches for it to write holo's NEUTRAL inline-asm
               ; template for us and GNU's AT&T/ARM string for clang, one header,
               ; both builds. cpp's, not the driver's: it is true of every mooncc
               ; compile, target and flags alike.
               _ (pin t "__mooncc__" `('obj `((tok 'num 1 0))))
               ; __FILE__ -> the TU path as a string literal. pinned as an object
               ; macro (not per-token) so it costs nothing until used; includes
               ; share this table, so it reads the top file's name throughout --
               ; fine for assert/error diagnostics, which live in the .c anyway.
               _ (pin t "__FILE__" `('obj `((tok 'str fname 0))))
               ; C11: cc has _Static_assert (+ _Alignas/varargs). advertising 201112
               ; lets gnulib take its _Static_assert lane (verify.h) not a fallback trick.
               _ (pin t "__STDC_VERSION__" `('obj `((tok 'num 201112 0))))
               ; HOSTED: a cc-built object links against libc into a hosted program,
               ; so love.c must take its hosted lanes -- the W^X mmap+mprotect code
               ; arena (toast/nif), NOT the freestanding heap copy (which is NX, so
               ; a glazed native body jumped into non-executable heap and faulted).
               ; the whole tco=0 corpus missed it: the glaze is the only arena user
               ; and it's gated off on trampoline builds.
               _ (pin t "__STDC_HOSTED__" `('obj `((tok 'num 1 0))))
               ; the ARCH predefine (__x86_64__ / __aarch64__) is the DRIVER's:
               ; cc.l prepends it as a -D-style define by target (love.c's AI_ARCH/
               ; IMAGE_ARCH read it -- a build without one believed its arch was
               ; "other" and the glaze handed holo an unknown target).
               ; __SIZEOF_INT128__ rides the DRIVER's -D channel too, x64 only
               ; (gen's d128 lane): love.c takes 64-bit limbs there, and the
               ; `divq` __asm__ lane with them; every other target keeps the
               ; portable 32-bit branch.
               ; and Linux is the OS under every cc-built object (raw syscalls,
               ; ELF, the host seam) -- without it host/posix.c's signalfd source
               ; silently compiled OUT of the selfhost binary (a rung-2 wart)
               _ (pin t "__linux__" `('obj `((tok 'num 1 0))))
               ; gcc's limit predefines, the ones the host seam reads
               _ (pin t "__INT_MAX__" `('obj `((tok 'num 2147483647 0))))
               _ (pin t "__LONG_MAX__" `('obj `((tok 'num 9223372036854775807 0))))
               ; FLT_MAX exactly: (2 - 2^-23) * 2^127 = 2^128 - 2^104, double-exact
               ; (love.c's 32-bit dtoa branch reads it; gcc predefines it everywhere)
               _ (pin t "__FLT_MAX__" `('obj `((tok 'flo ((2.0 - (/ 1.0 8388608.0)) * (127 2.0)) 0))))
               ; ..and the DOUBLE twin, (2 - 2^-52) * 2^1023, built the same exact way
               ; (love.c's dtoa reads it as its finite ceiling)
               _ (pin t "__DBL_MAX__" `('obj `((tok 'flo ((2.0 - (/ 1.0 4503599627370496.0)) * (1023 2.0)) 0))))
               ; bool/true/false BUILTIN (C23-style, no <stdbool.h> needed): modern C
               ; (gnulib, gzip) uses them bare. bool stays cc's int (the old stdbool.h
               ; choice) -- an int keyword token; true/false the 1/0 constants.
               _ (pin t "bool"  `('obj `((tok 'kw 'int 0))))
               _ (pin t "true"  `('obj `((tok 'num 1 0))))
               _ (pin t "false" `('obj `((tok 'num 0 0))))
               t)
       r (cppgo toks macs () () () incf)
       ; the spans ride the stream's HEAD as one marker token, so cpp keeps its
       ; single (1 out) channel and parse peels them in O(1). a caller that
       ; feeds parse raw lexer output simply has none, and parse reads ().
       ; the spans ride the stream's HEAD as one marker token, so cpp keeps its
       ; single (1 out) channel and parse peels them in O(1). a caller that
       ; feeds parse raw lexer output simply has none, and parse reads ().
       (? (id? r 'cppbad) ()
          (: _ (pin spanacc 'l ())
             out (strcat-pass r 0 ())
             `(1 (link (tok 'fspans (rev (peep spanacc 'l ())) 0) out))))))   ; an EMPTY token list is a valid empty TU, not a failure -- only 'cppbad fails
