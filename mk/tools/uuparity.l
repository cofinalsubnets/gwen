; mk/tools/uuparity.l -- audit test/uu.l against the published UniMath sources.
; every name defined in test/uu.l (defn/defq/axm) must be one of:
;   (a) a name published in the installed UniMath package
;       (/usr/lib/ocaml/coq/user-contrib/UniMath, e.g. arch coq-unimath), or
;   (b) prefixed uu- : local glue that claims no UniMath ancestry.
; anything else is an impostor and fails the audit. also reports coverage per
; published file -- the road to parity. a hand tool:
;
;   out/host/love mk/tools/uuparity.l          (exit 1 on impostors)
;
; skips with a note when UniMath is absent. the scan is TEXTUAL on both sides
; (a commented-out defn still counts), the shape of the python it replaced.
(: unimath "/usr/lib/ocaml/coq/user-contrib/UniMath"
   uul "test/uu.l"

   (rdf p) (: q (open p "r") t (slurp q) _ (close q) t)
   (dir? p) (: st (stat p) (? !(two? st) 0 ((& 61440 <>>st) = 16384)))
   (sfx-v? s) (: n (tally s) ((2 < n) && (s (n - 2) = 46) && (s (n - 1) = 118)))
   (alnum? c) (((48 <= c) && (c <= 57)) || ((65 <= c) && (c <= 90)) || ((97 <= c) && (c <= 122)))
   (vnamec? c) ((alnum? c) || (c = 95) || (c = 39))       ; [A-Za-z0-9_']
   (unamec? c) ((alnum? c) || (c = 95) || (c = 45))       ; [A-Za-z0-9_-]
   (blank? c) ((c = 32) || (c = 9) || (c = 13))

   ; every *.v under p, then one flat sort so the visit order is the path order
   (vfiles p acc)
    (: (go m acc) (? !(two? m) acc
                     (: e (p + "/" + <m)
                        (go >m (? (dir? e) (vfiles e acc) (sfx-v? e) (e >< acc) acc))))
       (go (readdir p) acc))

   kws '("Definition" "Theorem" "Lemma" "Corollary" "Proposition" "Axiom" "Fixpoint" "Record" "Inductive")
   (pfx? s n i kw) (: k (tally kw)
                      (go j) (? (k <= j) (i + k)
                                ((i + j) < n) (? ((s (i + j)) = (kw j)) (go (j + 1)) ())
                                ())
                      (go 0))
   (kw-end s n i) (: (try m) (? !(two? m) ()
                                (: e (pfx? s n i <m) (? (charm? e) e (try >m))))
                     (try kws))

   ; one .v: harvest names, first-file wins in pub, answer this file's distinct count
   (scan-v s pubT file)
    (: n (tally s) seen (tablet 0)
       (name-end i) (? ((i < n) && (vnamec? (s i))) (name-end (i + 1)) i)
       (eol i) (? ((i < n) && !((s i) = 10)) (eol (i + 1)) (i + 1))
       (ws i) (? ((i < n) && (blank? (s i))) (ws (i + 1)) i)
       ; after the keyword the reference \s+ crosses newlines too
       (wsn i) (? ((i < n) && ((blank? (s i)) || ((s i) = 10))) (wsn (i + 1)) i)
       (line i cnt)                                     ; i at a line start
        (? (n <= i) cnt
           (: j (ws i) e (kw-end s n j)
              (? (!e || (n <= e) || !((blank? (s e)) || ((s e) = 10))) (line (eol j) cnt)
                 (: a (wsn e) b (name-end a)
                    (? (b = a) (line (eol j) cnt)
                       (: nm (intern (snip s a b))
                          _ (? (peep pubT nm 0) 0 (pin pubT nm file))
                          (? (peep seen nm 0) (line (eol j) cnt)
                             (: _ (pin seen nm 1) (line (eol j) (cnt + 1))))))))))
       (line 0 0))

   ; test/uu.l: every (defn '/(defq '/(axm ' occurrence, in order, dups kept
   (uu-names s)
    (: n (tally s)
       (lit? i t) (: k (tally t)
                     (go j) (? (k <= j) (i + k)
                               ((i + j) < n) (? ((s (i + j)) = (t j)) (go (j + 1)) ())
                               ())
                     (go 0))
       (name-end i) (? ((i < n) && (unamec? (s i))) (name-end (i + 1)) i)
       (go i acc)
        (? (n <= i) (rev acc)
           (? !((s i) = 40) (go (i + 1) acc)
              (: e (: a (lit? (i + 1) "defn '") (? (charm? a) a
                       (: b (lit? (i + 1) "defq '") (? (charm? b) b
                          (lit? (i + 1) "axm '")))))
                 (? !(charm? e) (go (i + 1) acc)
                    (: b (name-end e)
                       (? (b = e) (go (i + 1) acc)
                          (go b ((snip s e b) >< acc))))))))
       (go 0 ()))

   (pad3 v) (: t (show v) (? ((tally t) = 1) ("  " + t) ((tally t) = 2) (" " + t) t))

   ; stable insertion by count, descending -- ties keep first-seen order
   (ins p l) (? !(two? l) [p] (<><l < <>p) (p >< l) (<l >< (ins p >l)))

   _ (? (dir? unimath) 0
        (: _ (puts "uuparity: UniMath not installed, audit skipped\n") (quit 0)))

   pubT (tablet 0)
   ; (file . distinct) per file, walked in flat path order; file = the short name
   tot (: (go m acc)
           (? !(two? m) (rev acc)
              (: full <m
                 short (snip full ((tally unimath) + 1) (tally full))
                 (go >m ([short (scan-v (rdf full) pubT short)] >< acc))))
          (go (sort (vfiles unimath ())) ()))
   ; the preamble ctors and notations that are names in uu.l's object language
   _ (map (\ x (: nm (intern x) (? (peep pubT nm 0) 0 (pin pubT nm "Foundations/Preamble.v"))))
          '("tpair" "pr1" "pr2" "idpath" "ii1" "ii2" "tt" "succ"
            "total2" "paths" "coprod" "nat" "bool" "unit" "empty" "UU"))

   names (uu-names (rdf uul))
   (uu-loc? s) ((2 < tally s) && (s 0 = 117) && (s 1 = 117) && (s 2 = 45))
   local (filter uu-loc? names)
   matched (filter (\ s (!(uu-loc? s) && (peep pubT (intern s) 0))) names)
   impostors (filter (\ s (!(uu-loc? s) && !(peep pubT (intern s) 0))) names)

   _ (puts (uul + ": " + show (tally names) + " names = " + show (tally matched)
            + " published + " + show (tally local) + " uu-local + "
            + show (tally impostors) + " impostors\n"))
   ; coverage per file: counts over matched (dups counted), first-seen order, then sorted
   cov (: (go m acc)
           (? !(two? m) acc
              (: f (peep pubT (intern <m) 0)
                 hit (assoc f acc)
                 (go >m (? (two? hit) (map (\ p (? (<p = f) [f (<>p + 1)] p)) acc)
                           (acc + [[f 1]])))))
          (go matched ()))
   _ (map (\ p (: f <p c <>p t (assoc f tot)
                  (puts ("  " + pad3 c + "/" + pad3 (? (two? t) <>t "?") + " " + f + "\n"))))
          (foldl (\ a p (ins p a)) () cov))
   _ (? (two? impostors)
        (: _ (puts "IMPOSTORS (unpublished names without uu- prefix):\n")
           _ (map (\ x (puts ("  " + x + "\n"))) impostors)
           (quit 1))
        (: _ (puts "parity audit clean: every unprefixed name is published UniMath\n") 0))
   0)
