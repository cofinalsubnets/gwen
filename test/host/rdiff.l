; --- test/host/rdiff.l: the READER DIFFERENTIAL (doc/reader.md rung 3) ---
; two readers, one text, one comparison -- the oracle rungs 5-6 are built against.
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
;   A/B          the reader under test agrees with the C reader, form for form
;   print->read  (rd (show f)) is (f) -- one form back, equal to the one that went in
;
; ⚠ TODAY BOTH LEGS OF A/B ARE THE SAME READER, so that law is vacuous until p1
; exists. rung 6 points `rd-test` (test/00-init.l) at p1 and it goes live; the
; whole socket is that one binding. what keeps the rig honest MEANWHILE is
; rd-hurt below -- a deliberately broken reader the sweep is REQUIRED to catch.
; a green diff over a reader that never ran proves nothing, and neither does a
; comparison that cannot fail; both are asserted at the foot.

(: rd-eof (nom 0)
   (rd-port s) (tap (map s (jot (tally s))))
   (rd-slurp path) (: q (open path "r") (? q (: t (slurp q) _ (close q) t) ""))
   (rd-cut s n) (? ((tally s) <= n) s (snip s 0 n))
   (rd-rep c n) (? (n <= 0) "" (c + (rd-rep c (n - 1))))
   ; every form a reader gets off a text. the read protocol IS sound's: the
   ; sentinel at eof, the PORT ITSELF where the shape did not finish -- p1 owes
   ; the same two answers, which is why the socket is a plain two-operand call.
   (rd-forms r txt)
     (: p (rd-port txt)
        (go acc) (: x (r p rd-eof)
                    (? (id? x rd-eof) (rev acc) (id? x p) (rev acc) (go (x >< acc))))
        (go ())))

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
   (rd-hurt p e)
     (: x (sound p e)
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

; --- p0, over the subset it owns (doc/reader.md rung 5) ---------------------
; `sound0` is the bootstrap reader in love.c: the pure lisp subset, no sigil
; surface. this is the A/B law with a REAL second reader on one leg -- the first
; non-vacuous run of it, and what rung 6 repeats with p1 on the whole tree.
;
; the input is prel.l ALONE, deliberately. p0 diverges on every sigil BY DESIGN
; (#(a b) reads as # then (a b)), so a tree sweep would measure the divergence
; instead of the agreement; prel.l is held to the subset by rung 4 and is the
; whole of what p0 must read. the rung-4 rule and this gate are the same fact,
; so a sigil creeping back into prel.l fails HERE.
(: rd-p0 (rd-run sound0 (L "love/prel.l") 1)
   ; and the same rig pointed at a file OUTSIDE the subset, which must go red:
   ; the proof this leg can fail, the rd-hurt discipline applied to a real reader
   ; rather than a spoiled one.
   rd-p0x (rd-run sound0 (L "love/ev.l") 0)
   ; p0 puts NESTING ON THE C STACK -- the trade that buys its size, and the one
   ; thing the stackless reader has over it. so pin the depth it reaches, far
   ; past the deepest form in the tree (38, test/uupatch.l).
   rd-deep (: s ((rd-rep "(" 20000) + (rd-rep ")" 20000))
              (= (sound0 (rd-port s) rd-eof) (sound (rd-port s) rd-eof)))
   _ (puts ("rdiff: p0 forms=" + (show (peep rd-p0 'forms 0))
            + " bad=" + (show (peep rd-p0 'bad 0))
            + " off-subset=" + (show (peep rd-p0x 'bad 0))
            + " deep=" + (show rd-deep) + "\n")))

; --- p1, over the whole tree (doc/reader.md rung 6) ------------------------
; the READER IN LOVE, love/p1.l. this is the A/B law as rung 3 meant it: a real
; second reader, over the whole input set, form for form.
;
; p1 walks a CHARLIST and the socket is a (reader port sentinel) call, so the
; shim slurps each port once and keeps the residue against the port's identity.
; that is test scaffolding -- p1's own door is the charlist, and the boot hands
; it whole text.
(: _ (use "love/p1.l")
   rd-p1res (tablet ())
   (rd-p1 p e)
     (: seen (peep rd-p1res p ())
        cl (? (two? seen) (cup seen)
              (: txt (slurp p) (map txt (jot (tally txt)))))
        d (p1-read1 cl ())
        (? (id? d p1-eof) (: _ (pin rd-p1res p (link 1 ())) e)
           (id? d p1-more) p
           (: _ (pin rd-p1res p (link 1 (cup d))) (cap d))))
   ; and the same fault injector the sweep itself is gated on, pointed at THIS
   ; leg: a green diff over a reader that never ran proves nothing, and neither
   ; does one whose comparison cannot fail.
   rd-p1hn (tablet 0)
   (rd-p1hurt p e)
     (: x (rd-p1 p e)
        _ (pin rd-p1hn 0 (1 + (peep rd-p1hn 0 0)))
        (? (= 3 (peep rd-p1hn 0 0)) 'rd-spoiled x)))

(: rd-p1sweep (rd-run rd-p1 rd-files 1)
   rd-p1hurtn (rd-run rd-p1hurt (L "test/spec.l") 0)
   _ (puts ("rdiff: p1 files=" + (show (peep rd-p1sweep 'files 0))
            + " forms=" + (show (peep rd-p1sweep 'forms 0))
            + " bad=" + (show (peep rd-p1sweep 'bad 0))
            + " injected=" + (show (peep rd-p1hurtn 'bad 0)) + "\n")))

(assert
 ; the sweep reached the tree and read it -- the PROOF OF WORK, checked and not
 ; merely printed. floors, not exact counts: the tree grows.
 (250 < (peep rd-clean 'files 0))
 (1200 < (peep rd-clean 'forms 0))
 ; the two laws
 (0 = (peep rd-clean 'bad 0))
 ; and the rig can go RED: one spoiled form in one file is enough to see
 (0 < (peep rd-hurtn 'bad 0))
 ; rung 5: p0 agrees with the C reader on every form of prel.l ...
 (50 <= (peep rd-p0 'forms 0))
 (0 = (peep rd-p0 'bad 0))
 ; ... and parts from it off the subset, so the agreement above is a measurement
 (0 < (peep rd-p0x 'bad 0))
 ; ... and its C-stack nesting reaches far past anything the tree contains
 rd-deep
 ; rung 6: p1, the reader in love, over the WHOLE tree -- the A/B law with a
 ; real second reader on one leg, which is what rung 3 built this rig for.
 (250 < (peep rd-p1sweep 'files 0))
 (1200 < (peep rd-p1sweep 'forms 0))
 (0 = (peep rd-p1sweep 'bad 0))
 ; and this leg can go red, so the zero above is a measurement
 (0 < (peep rd-p1hurtn 'bad 0)))

(puts "rdiff: ok")
(putc 10)
