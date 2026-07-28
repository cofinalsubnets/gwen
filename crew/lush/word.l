; --- lush 🐚 the word -- expansion: segments to fields ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-fields segs) -> the FIELDS a word becomes, POSIX order: tilde, then
; parameter/command substitution, then IFS field splitting -- ONLY over
; expansion output, never literal text -- then glob, then quote removal (free:
; quotes never entered the text). a field grows as a run list ((text . live?)..)
; so quoting survives to the glob (glob.l's mask law); `seen` carries the
; empty-word law out of band. parameters resolve shell vars (sh-vars) OVER the
; environment; the positionals and specials ($@ $* $# $? $$ $0-9) live here,
; and "$@" in quotes fields one positional each (the passthrough law).
; ${x:-y} and kin ride ('vop . (name op text)) -- text re-enters through
; sh-dollarize at use. ('sub . text) runs through the box sh-subeval -- eval.l
; pins the evaluator there at load (word compiles before eval exists).

(: sh-subeval #0
   (sh-ifs? b) (|| (= b 32) (= b 9) (= b 10))
   (sh-chomp t)
     (: (go k) (? (&& (< 0 k) (= (t (- k 1)) 10)) (go (- k 1)) (snip t 0 k))
        (go (tally t)))
   (sh-runsub x) (sh-chomp (+ "" ((sh-subeval ()) x)))

   ; --- parameters: shell vars over env; specials; presence OUT OF BAND --------
   ; (sh-vget nm) -> () unset | (1 . value) -- a tablet hit is a STRING (maybe
   ; ""), the miss the (); string? tells them apart where net cannot
   (sh-vget nm)
     (? (= nm "?") (link 1 (show (sh-status ())))
        (= nm "#") (link 1 (show (tally (sh-pos ()))))
        (= nm "$") (link 1 (show (getpid 0)))
        (= nm "!") (link 1 (show (sh-lastbg ())))
        (= nm "0") (link 1 (+ "" (sh-arg0 ())))
        (|| (= nm "@") (= nm "*")) (link 1 (sh-unwords (sh-pos ())))
        (sh-digits? nm)
          (: (go l k)
               (? (! (two? l)) ()
                  (= k 1) (link 1 (cap l))
                  (go (cup l) (- k 1)))
             (go (sh-pos ()) (sh-int nm)))
        (: v (peep sh-vars nm ())
           (? (string? v) (link 1 v)
              (: e (getenv nm) (? (string? e) (link 1 e) ())))))
   (sh-vval nm)
     (: g (sh-vget nm)
        (? (two? g) (cup g)
           (: _ (? (sh-uflag ())
                   (: _ (say err (+ "lush: " (+ nm ": unbound\n")))
                      (? (sh-tty ()) 0 (quit 2)))
                   0)
              "")))
   (sh-setvar nm v)                             ; an exported name stays exported
     (? (string? (getenv nm)) (setenv nm v) (pin sh-vars nm v))

   ; --- ${name<op>text}: the word text re-enters through sh-dollarize ----------
   (sh-textof segs)                             ; segs -> ONE string (no split/glob)
     (foldl (\ a sg (: k (cap sg)
                       (+ a (? (id? k 'lit) (cup sg)
                               (id? k 'sq) (cup sg)
                               (id? k 'dq) (sh-textof (cup sg))
                               (id? k 'var) (sh-vval (cup sg))
                               (id? k 'vop) (sh-vopseg sg)
                               (id? k 'sub) (sh-runsub (cup sg))
                               ""))))
            "" segs)
   (sh-voptext text) (sh-textof (sh-dollarize text))
   (sh-vopseg sg) (: p (cup sg) (sh-vop (sh-nth p 0) (sh-nth p 1) (sh-nth p 2)))
   (sh-vop nm op text)
     (? (= op "len") (show (tally (sh-vval nm)))
        (: g (sh-vget nm)
           set? (two? g)
           v (? set? (cup g) "")
           colon (&& (tally op) (= (op 0) 58))
           use? (? colon (&& set? (tally v)) set?)
           o (? colon (snip op 1 (tally op)) op)
           (? (= o "-") (? use? v (sh-voptext text))
              (= o "=") (? use? v (: t (sh-voptext text) _ (sh-setvar nm t) t))
              (= o "?") (? use? v
                           (: _ (say err (+ "lush: " (+ nm (+ ": " (? (tally text) text "parameter not set")))))
                              _ (put err 10)
                              (? (sh-tty ()) "" (quit 2))))
              (= o "+") (? use? (sh-voptext text) "")
              (sh-vstrip v o text))))
   ; ${v#pat} ${v##pat} ${v%pat} ${v%%pat}: shortest/longest prefix/suffix strip
   (sh-patmask segs)                            ; segs -> (text . mask), quotes inert
     (foldl (\ a sg (: k (cap sg)
                       t (? (id? k 'lit) (cup sg)
                            (id? k 'sq) (cup sg)
                            (id? k 'dq) (sh-textof (cup sg))
                            (id? k 'var) (sh-vval (cup sg))
                            (id? k 'vop) (sh-vopseg sg)
                            (id? k 'sub) (sh-runsub (cup sg))
                            "")
                       live (? (|| (id? k 'sq) (id? k 'dq)) "0" "1")
                       (link (+ (cap a) t) (+ (cup a) (sh-mrep (tally t) live)))))
            (link "" "") segs)
   (sh-vstrip v o text)
     (: pw (sh-patmask (sh-dollarize text))
        pt (cap pw) pm (cup pw) n (tally v)
        (? (= o "#")  (: (go i) (? (< n i) v
                                   (? (sh-fmatch pt pm (snip v 0 i)) (snip v i n) (go (+ i 1))))
                         (go 0))
           (= o "##") (: (go i) (? (< i 0) v
                                   (? (sh-fmatch pt pm (snip v 0 i)) (snip v i n) (go (- i 1))))
                         (go n))
           (= o "%")  (: (go k) (? (< n k) v
                                   (? (sh-fmatch pt pm (snip v (- n k) n)) (snip v 0 (- n k)) (go (+ k 1))))
                         (go 0))
           (= o "%%") (: (go k) (? (< k 0) v
                                   (? (sh-fmatch pt pm (snip v (- n k) n)) (snip v 0 (- n k)) (go (- k 1))))
                         (go n))
           v))

   ; --- the field machinery -----------------------------------------------------
   ; field state: (done runs seen) -- done finished fields (rev), runs the
   ; field in progress (rev run list), seen "this field exists"
   (addrun runs t a) (? (tally t) (link (link t a) runs) runs)
   (closef done runs seen) (? seen (link runs done) done)
   (skifs t i n) (? (&& (< i n) (sh-ifs? (t i))) (skifs t (+ i 1) n) i)
   (chunk t i n) (? (&& (< i n) (! (sh-ifs? (t i)))) (chunk t (+ i 1) n) i)
   ; split expansion output on IFS: chunks join the field, runs of IFS close it
   (spl t i n done runs seen)
     (? (= i n) (L done runs seen)
        (? (sh-ifs? (t i))
           (spl t (skifs t i n) n (closef done runs seen) () 0)
           (: j (chunk t i n)
              (spl t j n done (addrun runs (snip t i j) 1) 1))))

   ; ".." with the $@ law: each positional its own field, edges glue
   (dqat inner st)
     (? (! (two? inner)) st
        (: sg (cap inner) k (cap sg)
           d (sh-nth st 0) r (sh-nth st 1)
           (? (&& (id? k 'var) (= (cup sg) "@"))
              (: ps (sh-pos ())
                 st2 (? (! (two? ps)) st
                        (foldl (\ a p (: dd (sh-nth a 0) rr (sh-nth a 1) sn2 (sh-nth a 2)
                                         (? sn2 (L (link rr dd) (addrun () p 0) 1)
                                            (L dd (addrun () p 0) 1))))
                               (L d (addrun r (? (two? ps) (cap ps) "") 0) 1)
                               (cup ps)))
                 (dqat (cup inner) st2))
              (dqat (cup inner)
                    (L d (addrun r (? (id? k 'lit) (cup sg)
                                      (id? k 'var) (sh-vval (cup sg))
                                      (id? k 'vop) (sh-vopseg sg)
                                      (id? k 'sub) (sh-runsub (cup sg))
                                      "")
                                 0)
                       1)))))
   (wseg sg st)
     (: d (sh-nth st 0) r (sh-nth st 1) sn (sh-nth st 2)
        k (cap sg) v (cup sg)
        (? (id? k 'lit) (L d (addrun r v 1) 1)
           (id? k 'sq)  (L d (addrun r v 0) 1)
           ; "": a real (seen) field even empty -- EXCEPT a lone "$@" over zero
           ; positionals, which contributes nothing (dqat's glue lane sets seen)
           (id? k 'dq)  (dqat v (L d r (? (two? v) sn 1)))
           (id? k 'var) (: t (sh-vval v) (spl t 0 (tally t) d r sn))
           (id? k 'vop) (: t (sh-vopseg sg) (spl t 0 (tally t) d r sn))
           (id? k 'sub) (: t (sh-runsub v) (spl t 0 (tally t) d r sn))
           st))
   (wsegs segs st) (? !segs st (wsegs (cup segs) (wseg (cap segs) st)))

   ; ~ / ~/rest at the word's head -> $HOME, shielded (never split, never glob);
   ; the rest of the run stays live so ~/*.l still globs
   (sh-tilde segs)
     (? (&& (two? segs) (id? (cap (cap segs)) 'lit)
            (tally (cup (cap segs))) (= ((cup (cap segs)) 0) 126))
        (: t (cup (cap segs)) h (+ "" (getenv "HOME"))
           (? (= t "~") (link (link 'sq h) (cup segs))
              (= (t 1) 47) (link (link 'sq h) (link (link 'lit (snip t 1 (tally t))) (cup segs)))
              segs))
        segs)

   ; a finished field: join the runs; a live glob char sends it to sh-glob,
   ; no match keeps the word (the POSIX default)
   (sh-mrep k c) (? (< k 1) "" (+ (sh-mrep (- k 1) c) c))
   (sh-field1 runs)
     (: rs (sh-rev runs)
        t (foldl (\ a r (+ a (cap r))) "" rs)
        gb (foldl (\ a r (? a a (&& (cup r) (sh-anyglob (cap r))))) 0 rs)
        (? gb
           (: m (foldl (\ a r (+ a (sh-mrep (tally (cap r)) (? (cup r) "1" "0")))) "" rs)
              g (sh-glob t m)
              (? g g (L t)))
           (L t)))
   (sh-fields segs)
     (: st (wsegs (sh-tilde segs) (L () () 0))
        fs (sh-rev (closef (sh-nth st 0) (sh-nth st 1) (sh-nth st 2)))
        (catmap sh-field1 fs)))
