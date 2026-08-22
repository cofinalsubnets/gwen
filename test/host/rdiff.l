; --- test/host/rdiff.l: the READER DIFFERENTIAL (doc/misc/io.md rung 3) ---
; two readers, one text, one comparison -- the oracle rungs 5-6 were built against.
; run standalone from the repo root:
;   cat test/00-init.l test/host/rdiff.l | out/host/love
;
; the input set is THE TREE: every .l under the root, walked off readdir (not off
; git -- an untracked stray in the tree is still text the reader must read), read form by
; form. that is ~1300 real forms carrying every construct the grammar has in
; anger -- the operator runs, the glued monos, the constructor wraps, three
; integer bases, the comma -- which is a better corpus than any generator, and
; it is already here. two laws ride it:
;
;   A/B          two readers agree on one text, form for form
;   print->read  (rd (show f)) is (f) -- one form back, equal to the one that went in
;
; ⚠ THE A/B LEG IS P0 AGAINST P1 NOW, and it is the only pair left: rung 6c
; retired the C reader, so `sound` and `rd-test` are one reader and comparing
; them proves nothing. p0 (love.c's sound0, the pure lisp subset) is the real
; second implementation, and rd-p0 below is where the law does its work. the
; tree-wide sweep keeps print->read, which one reader can still fail.
; what keeps all of it honest is the INJECTORS -- rd-hurt and rd-p0hurt, readers
; deliberately broken part-way -- because a green diff over a reader that never
; ran proves nothing, and neither does a comparison that cannot fail. both are
; asserted at the foot.

(: (rd-cl s) (map s (jot (tally s)))
   (rd-slurp path) (: q (open path "r") (? q (: t (slurp q) _ (close q) t) ""))
   (rd-cut s n) (? ((tally s) <= n) s (snip s 0 n))
   (rd-rep c n) (? (n <= 0) "" (c + (rd-rep c (n - 1))))
   ; every form a reader gets off a text. the read protocol IS sound's, and the
   ; residue is the answer, so the walk threads it: a clean end and a torn shape
   ; both stop, which is what makes the injected-fault runs below comparable.
   (rd-forms r txt)
     (: (go cl acc) (: x (r cl)
                       (? (two? x) (go (cup x) (<x >< acc)) (rev acc)))
        (go (rd-cl txt) ())))

; the smallest subform where two trees part -- descend while the differing child
; is itself a pair, then report that pair. a leaf alone says "0 vs ()" and names
; nothing; its parent says which form it sat in.
(: (rd-dn a b) (&& (! (= a b)) (two? a) (two? b))
   (rd-small a b) (? (rd-dn <a <b) (rd-small <a <b)
                     (rd-dn >a >b) (rd-small >a >b)
                     (L a b)))

; --- the input set: every .l under the root, off readdir --------------------
; out/ is generated, dot-directories are not ours. stat's mode word decides
; directory-or-not: (two? (readdir p)) would call an EMPTY directory a file.
(: rd-skip '("out")
   (rd-dir? p) (: st (stat p) (&& (two? st) (= 16384 (& 61440 (cauup st)))))
   (rd-dot? n) (= 46 (peep n 0 0))
   (rd-dotl? n) (: t (tally n) (&& (2 < t) (= ".l" (snip n (t - 2) t))))
   (rd-walk d acc)
     (foldl (\ a n (: p (? (= "." d) n (d + "/" + n))
                      (? (|| (rd-dot? n) (member? n rd-skip)) a
                         (rd-dir? p) (rd-walk p a)
                         (rd-dotl? n) (p >< a)
                         a)))
            acc (sort (readdir d))))

; --- the sweep -------------------------------------------------------------
; answers a tally tablet: files, forms, bad. `loud` prints the first few
; divergences with the subform that parted -- off for the injected run, which is
; SUPPOSED to diverge.
(: (rd-run r paths loud)
   (: n (tablet 0)
      (bump k v) (pin n k (v + (peep n k 0)))
      (say leg path a b)
        (: _ (bump 'bad 1)
           (? (&& loud ((peep n 'bad 0) <= 8))
              (: d (rd-small a b)
                 _ (puts ("  " + leg + " " + path + "\n    A " + (rd-cut (show <d) 120)
                                                 + "\n    B " + (rd-cut (show <>d) 120) + "\n")) 0)
              0))
      (one path)
        (: txt (rd-slurp path)
           fa (rd-forms sound txt)
           fb (rd-forms r txt)
           _ (bump 'files 1)
           _ (bump 'forms (tally fa))
           _ (? (= fa fb) 0 (say "A/B" path fa fb))
           ; print->read rides reader A's forms so it still measures something
           ; when A and B are the same reader
           (each fa (\ f (: g (rd-forms r (show f))
                            (? (= (L f) g) 0 (say "print->read" path (L f) g))))))
      _ (each paths one)
      n))

; --- the fault injector ----------------------------------------------------
; the rig's own gate. rd-hurt is the C reader with one form spoiled part-way
; through a file -- the shape a real second reader's bug takes (not a crash, a
; plausible wrong tree) -- and the sweep must SEE it. gate the rig on this, never
; on the diff being green: green is what a rig that never ran also prints.
(: rd-hurt-n (tablet 0)
   (rd-hurt cl)
     (: x (sound cl)
        _ (pin rd-hurt-n 0 (1 + (peep rd-hurt-n 0 0)))
        (? (= 3 (peep rd-hurt-n 0 0)) 'rd-spoiled x)))

(: rd-files (rd-walk "." ())
   _ (puts ("rdiff: " + (show (tally rd-files)) + " .l files\n"))
   rd-clean (rd-run rd-test rd-files 1)
   rd-hurtn (rd-run rd-hurt (L "test/spec.l") 0)
   _ (puts ("rdiff: files=" + (show (peep rd-clean 'files 0))
            + " forms=" + (show (peep rd-clean 'forms 0))
            + " bad=" + (show (peep rd-clean 'bad 0))
            + " injected=" + (show (peep rd-hurtn 'bad 0)) + "\n")))

; --- p0, over the subset it owns (doc/misc/io.md rung 5) ---------------------
; `sound0` is the bootstrap reader in love.c: the pure lisp subset, no sigil
; surface. this is the A/B law with a REAL second reader on one leg -- and since
; 6c retired the C reader it is the ONLY pair left in the tree.
;
; the input is the SUBSET'S OWN files, deliberately. p0 diverges on every sigil BY
; DESIGN (#(a b) reads as # then (a b)), so a tree sweep would measure the
; divergence instead of the agreement. p1.l and egg.l are what p0 reads at boot;
; p0fix.l spans the grammar they are too thin to cover. a sigil creeping into any
; of the three fails HERE -- and prel.l is NOT among them: it rides p1 now.
(: rd-p0 (rd-run sound0 (L "test/host/p0fix.l" "love/p1.l" "love/egg.l") 1)
   ; and the same rig pointed at a file OUTSIDE the subset, which must go red:
   ; the proof this leg can fail, the rd-hurt discipline applied to a real reader
   ; rather than a spoiled one.
   rd-p0x (rd-run sound0 (L "love/ev.l") 0)
   ; p0 puts NESTING ON THE C STACK -- the trade that buys its size, and the one
   ; thing the stackless reader has over it. so pin the depth it reaches, far
   ; past the deepest form in the tree (38, test/uupatch.l).
   rd-deep (: s ((rd-rep "(" 20000) + (rd-rep ")" 20000))
              (= (sound0 (rd-cl s)) (sound (rd-cl s))))
   ; and p0's own injector: the A/B leg above is the load-bearing one now, so it
   ; owes the same proof that its comparison CAN fail.
   rd-p0hn (tablet 0)
   (rd-p0hurt cl) (: x (sound0 cl)
                     _ (pin rd-p0hn 0 (1 + (peep rd-p0hn 0 0)))
                     (? (= 3 (peep rd-p0hn 0 0)) 'rd-spoiled x))
   rd-p0hurtn (rd-run rd-p0hurt (L "test/host/p0fix.l") 0)
   _ (puts ("rdiff: p0 forms=" + (show (peep rd-p0 'forms 0))
            + " bad=" + (show (peep rd-p0 'bad 0))
            + " off-subset=" + (show (peep rd-p0x 'bad 0))
            + " injected=" + (show (peep rd-p0hurtn 'bad 0))
            + " deep=" + (show rd-deep) + "\n")))

(assert
 ; the sweep reached the tree and read it -- the PROOF OF WORK, checked and not
 ; merely printed. floors, not exact counts: the tree grows.
 (250 < (peep rd-clean 'files 0))
 (1200 < (peep rd-clean 'forms 0))
 ; the two laws
 (0 = (peep rd-clean 'bad 0))
 ; and the rig can go RED: one spoiled form in one file is enough to see
 (0 < (peep rd-hurtn 'bad 0))
 ; rung 5: p0 agrees with p1 on every form of the subset it owns ...
 (50 <= (peep rd-p0 'forms 0))
 (0 = (peep rd-p0 'bad 0))
 ; ... and parts from it off the subset, so the agreement above is a measurement
 (0 < (peep rd-p0x 'bad 0))
 ; ... and its C-stack nesting reaches far past anything the tree contains
 rd-deep
 ; ... and THAT leg can go red too, so its zero is a measurement as well
 (0 < (peep rd-p0hurtn 'bad 0)))

(test_fin ())
(puts "rdiff: ok")
(putc 10)
