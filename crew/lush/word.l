; --- lush 🐚 the word -- expansion: segments to fields ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; (sh-fields segs) -> the FIELDS a word becomes, POSIX order: tilde, then
; parameter/$?/command substitution, then IFS field splitting -- ONLY over
; expansion output, never literal text -- then glob, then quote removal (free:
; quotes never entered the text). a field grows as a run list ((text . live?)..)
; so quoting survives to the glob (glob.l's mask law); `seen` carries the
; empty-word law out of band: a quoted empty ("" or '') is a real field, an
; unset $x alone is NO field (bash's empty removal, the POSIX one).
; ('sub . text) runs through the box sh-subeval -- eval.l pins the evaluator
; there at load (word compiles before eval exists; the cell rides the gap).

(: sh-subeval #0
   (sh-ifs? b) (|| (= b 32) (= b 9) (= b 10))
   (sh-chomp t)
     (: (go k) (? (&& (< 0 k) (= (t (- k 1)) 10)) (go (- k 1)) (snip t 0 k))
        (go (tally t)))
   (sh-vval nm) (? (= nm "?") (show (sh-status ())) (+ "" (getenv nm)))
   (sh-runsub x) (sh-chomp (+ "" ((sh-subeval ()) x)))

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

   ; ".." contents -> one shielded text (var/sub expand, nothing splits)
   (dqtext inner)
     (foldl (\ a sg (: k (cap sg)
                       (+ a (? (id? k 'lit) (cup sg)
                               (id? k 'var) (sh-vval (cup sg))
                               (id? k 'sub) (sh-runsub (cup sg))
                               ""))))
            "" inner)
   (wseg sg st)
     (: d (sh-nth st 0) r (sh-nth st 1) sn (sh-nth st 2)
        k (cap sg) v (cup sg)
        (? (id? k 'lit) (L d (addrun r v 1) 1)
           (id? k 'sq)  (L d (addrun r v 0) 1)
           (id? k 'dq)  (L d (addrun r (dqtext v) 0) 1)
           (id? k 'var) (: t (sh-vval v) (spl t 0 (tally t) d r sn))
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
