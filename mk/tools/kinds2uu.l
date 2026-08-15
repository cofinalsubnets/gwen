; mk/tools/kinds2uu.l -- the kinds-lattice analog of mk/tools/wm2uu.l: read the ABSTRACT
; DOMAIN of doc/proto/kinds.l AS DATA and emit test/uukind.l -- the join (and its
; num? guard) compiled into uu terms over a nat-coded carrier, so test/uukindlaw.l
; proves the semilattice laws OF THE ACTUAL ANALYSIS, not a hand transcription. Run:
;
;   love mk/tools/kinds2uu.l > test/uukind.l        (or: make uukind)
;
; the ENCODING is derived from kinds.l's own `rank`: each kind symbol takes its
; rank as its nat code (bot 0 .. top 5), and `str` -- the incomparable branch, off
; rank's table -- takes the next code up. so `rank` folds to identity here, and the
; translation is otherwise structural: ? -> nested bool_rect, id? -> nateqb,
; < -> natgtb (swapped), && -> andb, num? -> the emitted numq, a `:` -> a let.
; kind symbols appear only QUOTED in kinds.l ('bot ..), so a bare nom is a variable.
;
; a body-LESS `:` -- the k2-* vocabulary leaks; none shadows a nif.
(:
   (k2-pairs bs) (? (! (two? bs)) () (! (two? (cup bs))) ()
                    (link (L (cap bs) (cap (cup bs))) (k2-pairs (cup (cup bs)))))
   (k2-find nm ps) (cap (filter (\ p (&& (two? (cap p)) (id? (cap (cap p)) nm))) ps))
   k2-a1 (\ e (cap (cup e)))                 ; first operand
   k2-a2 (\ e (cap (cup (cup e))))           ; second operand
   k2-qmark (cap '(\ x))                     ; the \ head a quote wears as data
   (k2-quoted? e) (&& (two? e) (id? (cap e) k2-qmark))
   (k2-qdat q) (cap (cup q))                 ; the symbol under a quote

   ; --- the code map, read off kinds.l's `rank` (its ? pairs), then str on top ---
   (k2-rankmap body) (: (go cs) (? (! (two? (cup cs))) ()   ; the trailing -1 else: stop
                          (: c (cap cs) r (cap (cup cs))
                             sym (k2-qdat (k2-a2 c))         ; c = (id? k 'SYM)
                             (link (L sym r) (go (cup (cup cs))))))
                        (go (cup body)))
   (k2-max l) (foldl (\ a x (? (< a x) x a)) 0 l)
   (k2-codes km) (map (\ p (cap (cup p))) km)
   (k2-upto n) (: (go i) (? (< i n) (link i (go (i + 1))) ()) (go 0))   ; 0..n-1
   (k2-kc km s) (: p (assoc s km) (? (two? p) (cap (cup p)) -1))

   ; --- the translation. ty = the uu result type at this position ---
   (k2-tx km e ty)
    (? (charm? e) e
       (k2-quoted? e) (k2-kc km (k2-qdat e))
       (nom? e) e
       (! (two? e)) e
       (: h (cap e)
          (? (id? h '?)    (k2-cond km (cup e) ty)
             (id? h ':)    (k2-lets km (cup e) ty)
             (id? h 'id?)  (L 'nateqb (k2-tx km (k2-a1 e) 'nat) (k2-tx km (k2-a2 e) 'nat))
             (id? h '<)    (L 'natgtb (k2-tx km (k2-a2 e) 'nat) (k2-tx km (k2-a1 e) 'nat))
             (id? h '&&)   (L 'andb (k2-tx km (k2-a1 e) 'bool) (k2-tx km (k2-a2 e) 'bool))
             (id? h 'rank) (k2-tx km (k2-a1 e) ty)     ; code IS rank -- fold
             (id? h 'num?) (L 'numq (k2-tx km (k2-a1 e) 'nat))
             'k2-untranslatable)))
   (k2-cond km cs ty)
    (? (! (two? (cup cs))) (k2-tx km (cap cs) ty)      ; the bare else
       (: c (cap cs) r (cap (cup cs))
          (L 'bool_rect (L 'lam 'gx ty)
             (k2-tx km r ty) (k2-cond km (cup (cup cs)) ty) (k2-tx km c 'bool))))
   (k2-lets km bs ty)
    (? (! (two? (cup bs))) (k2-tx km (cap bs) ty)      ; the trailing body
       (L 'let (cap bs) (k2-tx km (cap (cup bs)) 'nat)
          (k2-lets km (cup (cup bs)) ty)))

   ; --- assemble one op: (name (pi params) (lam params body)) ---
   (k2-pi ps res) (? (two? ps) (L 'pi (cap ps) 'nat (k2-pi (cup ps) res)) res)
   (k2-lams ps b) (? (two? ps) (L 'lam (cap ps) (k2-lams (cup ps) b)) b)
   (k2-op km ps pr resty ty) (L (k2-pi ps resty) (k2-lams ps (k2-tx km (cap (cup pr)) ty)))

   ; --- drive: read kinds.l, lift num? and join ---
   fs (forms (open "doc/proto/kinds.l" "r"))
   big (cap (filter (\ f (&& (two? f) (id? (cap f) ':))) fs))
   ps (k2-pairs (cup big))
   km0 (k2-rankmap (cap (cup (k2-find 'rank ps))))
   km  (link (L 'str ((k2-max (k2-codes km0)) + 1)) km0)   ; str: the branch off rank
   nump (k2-find 'num? ps) joinp (k2-find 'join ps)
   numq  (k2-op km (cup (cap nump))  nump  'bool 'bool)
   kjoin (k2-op km (cup (cap joinp)) joinp 'nat  'nat)

   nl (string 10)
   (pl s) (puts (nl + s))                              ; a line, newline-led
   (pdefn nm op close) (: _ (pl (" (defn '" + nm))     ; op = (type term); close = ) or ))
                          _ (pl ("   '" + (show (cap op))))
                          (pl ("   '" + (show (cap (cup op))) + close)))

   _ (puts "; test/uukind.l -- GENERATED from doc/proto/kinds.l by mk/tools/kinds2uu.l. Do")
   _ (pl "; not edit; refresh with `make uukind`. The abstract kinds-lattice JOIN (and")
   _ (pl "; its num? guard) compiled into uu over a nat-coded carrier, kind = its rank")
   _ (pl "; (str the branch off the spine). test/uukindlaw.l proves the semilattice laws")
   _ (pl "; of THESE terms at corpus time -- a law-breaking kinds.l edit fails a check.")
   _ (pl "; Substrate: test/uu.l (natgtb / nateqb / andb / bool_rect).")
   _ (pl "(assert")
   _ (pdefn "numq" numq ")")
   _ (pdefn "kjoin" kjoin "))")
   ; the carrier codes 0..str, for the law file (derived, so drift follows kinds.l)
   _ (pl ("(: uk-codes '" + (show (k2-upto ((k2-max (k2-codes km)) + 1)))))
   _ (puts (")" + nl))
   0)
