; crew/kore/patch.l -- patch(1), the other half of diff. crew/kore/diff.l WRITES unified
; hunks; this reads them back and lays them on a tree, which is what every third-party
; source in doc/plan/lfs.md's ladder is delivered as. a toolbox of one: patch-main, no
; seat, kore.l is its door.
;
; UNIFIED DIFFS ONLY, deliberately. The context (`***`/`---`) and normal (`3c3`) formats
; are two more parsers for a shape nothing in this decade emits, and half a parser is
; worse than a refusal that names itself.
;
; the shape of the work:
;   * a FILE is a list of (text nl) pairs -- the line and whether a newline follows it.
;     ⚠ A MISSING FINAL NEWLINE IS DATA here as everywhere in kore, and a patch can
;     both carry one in and take one away ("\ No newline at end of file"), so the flag
;     rides per line rather than per file.
;   * a HUNK is (ostart nstart lines), a line (op text nl), op 32 keep / 45 del / 43 add.
;   * applying walks the hunks in order over the EVOLVING file, carrying a delta: the
;     running difference between a position in the original and the same content's
;     position now. a hunk that does not sit where it says searches outward from there,
;     which is what `offset` in patch's own report means.
;
; the exit code is patch's: 0 everything applied, 1 some hunk was rejected (and a .rej
; beside the file says which, with the original .orig kept), 2 the patch itself is
; unreadable.
(:
   (pt-pfx s p) ((tally p <= tally s) && (snip s 0 (tally p) = p))
   ; ⚠ the say FLUSHES: the hunk reports go to err and the file report to out, so an
   ; unflushed out lands its line in the middle of a later one -- patch's own output
   ; reads as one conversation and has to stay one
   (pt-say s) (: _ (say out s) _ (put out 10) (flush out))
   (pt-cry s) (: _ (flush out) _ (say err s) _ (put err 10) (flush err))
   ; a header line -> its path: everything past "--- " up to the first tab, or the whole
   ; rest when there is none (git's headers carry no timestamp)
   (pt-hname ln)
    (: n (tally ln)
       (go k) (? (n <= k) n (ln k = 9) k (go (k + 1)))
       (? (n < 5) "" (snip ln 4 (go 4))))
   ; -p N: drop N leading components. UNSAID drops them all, which is patch's own
   ; default and the reason a bare `patch < p` run from inside the tree works at all
   (pt-strip nm p)
    (? !(charm? p) (ubase nm "")
       (: (go s k)
           (? (k < 1) s
              (: n (tally s)
                 (at i) (? (n <= i) -1 (s i = 47) i (at (i + 1)))
                 i (at 0)
                 (? (i < 0) s (go (snip s (i + 1) n) (k - 1)))))
          (go nm p)))
   ; "@@ -1,8 +3 @@ .." -> (ostart ocount nstart ncount) | ()
   (pt-side w)
    (: b (snip w 1 (tally w))
       ps (usplit 44 b)
       `((uatoi <ps) (? (two? ps && two? >ps) (uatoi <>ps) 1)))
   (pt-hdr ln)
    (: ws (filter (\ w (0 < tally w)) (usplit 32 ln))
       (? (tally ws < 3) ()
          (: ow <>ws nw <>>ws
             (? (((0 < tally ow) && (0 < tally nw)) && ((ow 0 = 45) && (nw 0 = 43)))
                (: o (pt-side ow) n (pt-side nw) `(<o <>o <n <>n))
                ()))))

   ; --- the reader: a patch text -> ((oldname newname hunks)..) -------------------
   ; a section opens on a --- / +++ pair and runs to the next one. anything between
   ; sections is prose (a mail body, a commit message) and is skipped, which is how a
   ; patch pulled off a mailing list applies at all.
   ; ⚠ THE `\` LINE IS TESTED BEFORE THE COUNTS RUN OUT. `\ No newline at end of file`
   ; carries no count of its own, so the one that follows the hunk's LAST line arrives
   ; after both counters have hit zero -- and a body that stopped on the counts alone
   ; would leave every "the patch takes the newline away" case unmarked.
   (pt-body m no nn acc)
    (? !(two? m) `((rev acc) m)
       (: ln <m
          c (? (0 < tally ln) (ln 0) 32)
          (? (c = 92)
             (pt-body >m no nn (? (two? acc)
                                  ((: h <acc `(<h <>h 0)) >< >acc)
                                  acc))
             ((no < 1) && (nn < 1)) `((rev acc) m)
             (: txt (? (0 < tally ln) (snip ln 1 (tally ln)) "")
                (? (c = 45) (pt-body >m (no - 1) nn (`(45 txt 1) >< acc))
                   (c = 43) (pt-body >m no (nn - 1) (`(43 txt 1) >< acc))
                   ((c = 32) || (c = 0)) (pt-body >m (no - 1) (nn - 1) (`(32 txt 1) >< acc))
                   `((rev acc) m))))))
   (pt-hunks m acc)
    (? (!(two? m) || !(pt-pfx <m "@@")) `((rev acc) m)
       (: hd (pt-hdr <m)
          (? !(two? hd) `((rev acc) m)
             (: b (pt-body >m <>hd (at hd 3) ())
                (pt-hunks <>b (`(<hd <>>hd <b) >< acc))))))
   (pt-parse ls)
    (: (go m acc)
        (? !(two? m) (rev acc)
           (? ((pt-pfx <m "--- ") && ((two? >m) && (pt-pfx <>m "+++ ")))
              (: h (pt-hunks >>m ())
                 (go <>h (? (two? <h) (`((pt-hname <m) (pt-hname <>m) <h) >< acc) acc)))
              (go >m acc)))
       (go ls ()))

   ; --- the file, as (text nl) pairs ---------------------------------------------
   (pt-pairs t)
    (: ls (lines t) n (tally ls) e (nl-end? t)
       (go m i acc) (? !(two? m) (rev acc)
                       (go >m (i + 1) (`(<m (? ((i + 1) = n) e 1)) >< acc)))
       (go ls 0 ()))
   (pt-render ps)
    (: o (jug 0)
       (go m) (? !(two? m) (slurp o)
                 (: _ (say o <<m) _ (? <><m (put o 10) 0) (go >m)))
       (go ps))
   ; the two sides of a hunk: the old TEXTS to look for, the new PAIRS to lay down
   (pt-old h) (map (\ l <>l) (filter (\ l !(<l = 43)) (at h 2)))
   (pt-new h) (map (\ l `(<>l (at l 2))) (filter (\ l !(<l = 45)) (at h 2)))

   ; --- the search: where does this hunk's old side actually sit ------------------
   (pt-match ps k old)
    (: (go m o) (? !(two? o) 1 !(two? m) 0 (<<m = <o) (go >m >o) 0)
       (go (drop k ps) old))
   (pt-fits ps k old) (((0 <= k) && ((k + tally old) <= tally ps)) && (pt-match ps k old))
   ; ⚠ the reach is (n + 1 + |want|), not n: a create hunk's `-0,0` wants seat -1 in a
   ; file of no lines, so a search bounded by the file's own length never takes a step.
   (pt-seek ps old want)
    (: far ((tally ps + 1) + (abs want))
       (try d) (? (far < d) ()
                  (pt-fits ps (want - d) old) `(1 (want - d))
                  (pt-fits ps (want + d) old) `(1 (want + d))
                  (try (d + 1)))
       (? (pt-fits ps want old) `(1 want) (try 1)))

   ; --- the reject, written back as the patch it failed to be ---------------------
   (pt-rej nm hs)
    (: o (jug 0)
       (rng l c) (? (c = 1) (show l) ((show l) + "," + (show c)))
       (one h) (: ls (at h 2)
                  no (tally (filter (\ l !(<l = 43)) ls))
                  nn (tally (filter (\ l !(<l = 45)) ls))
                  _ (say o ((("@@ -" + (rng <h no)) + (" +" + (rng <>h nn))) + " @@\n"))
                  (foldl (\ _ l (: _ (put o <l) _ (say o <>l)
                                   (? (at l 2) (put o 10) (say o "\n\\ No newline at end of file\n"))))
                         0 ls))
       _ (say o (("--- " + nm + "\n") + ("+++ " + nm + "\n")))
       _ (foldl (\ _ h (one h)) 0 hs)
       (slurp o))

   ; --- one file's worth ----------------------------------------------------------
   ; ⚠ the hunks are reversed BEFORE anything else under -R: swapping each line's op and
   ; the two starts turns the patch around, and everything downstream then reads one
   ; shape. a -R that reversed at apply time would owe every later step a second lane.
   (pt-flip h)
    `(<>h <h (map (\ l `((? (<l = 45) 43 (<l = 43) 45 32) <>l (at l 2))) (at h 2)))
   (pt-file nm hs dry? silent?)
    (: r (uread nm)
       fresh !(two? r)
       ps0 (? fresh () (pt-pairs <>r))
       _ (? silent? () (pt-say ((? dry? "checking file " "patching file ") + nm)))
       (go m ps delta i bad off? rej)
        (? !(two? m) `(ps bad (rev rej) off?)
           (: h <m
              old (pt-old h) new (pt-new h)
              ; the -0,0 convention: a side with no lines names the seat BEFORE it, so
              ; start 0 is position 0 and not -1 (diff.l writes it, this reads it)
              want ((? (0 < <h) (<h - 1) 0) + delta)
              s (pt-seek ps old want)
              (? !(two? s)
                 (: _ (pt-cry (("Hunk #" + (show i)) + (" FAILED at " + (show (want + 1)) + ".")))
                    (go >m ps delta (i + 1) (bad + 1) off? (h >< rej)))
                 (: k <>s
                    ps2 ((take k ps) + (new + (drop (k + tally old) ps)))
                    off (k - want)
                    _ (? ((off = 0) || silent?) ()
                         (pt-say ((("Hunk #" + (show i)) + (" succeeded at " + (show (k + 1))))
                                  + ((" (offset " + (show off)) + (? ((off = 1) || (off = -1))
                                                                    " line)." " lines).")))))
                    (go >m ps2 ((delta + off) + ((tally new) - tally old)) (i + 1) bad
                        (? off 1 off?) rej)))))
       res (go hs ps0 0 1 0 0 ())
       ps' <res bad <>res rej <>>res off? (at res 3)
       nh (tally hs)
       _ (? bad
            (pt-cry ((((show bad) + " out of " + (show nh)) + (? (nh = 1) " hunk" " hunks"))
                     + (" FAILED -- saving rejects to file " + nm + ".rej")))
            ())
       _ (? dry? ()
            ; ⚠ the .orig lands on a MISMATCH and not only on a failure -- a hunk
            ; that moved applied to a file the patch did not quite describe, and GNU's
            ; --backup-if-mismatch keeps the original for exactly that case
            (: _ (? ((bad || off?) && !fresh) (uwrite (nm + ".orig") <>r) ())
               _ (? bad (uwrite (nm + ".rej") (pt-rej nm rej)) ())
               (uwrite nm (pt-render ps'))))
       bad)

   (patch-main as)
    (: (fl a p R inp dry silent outp rest)
        (? !(two? a) `(p R inp dry silent outp (rev rest))
           ((<a = "-R") || (<a = "--reverse")) (fl >a p 1 inp dry silent outp rest)
           (<a = "--dry-run") (fl >a p R inp 1 silent outp rest)
           ((<a = "-s") || (<a = "--silent")) (fl >a p R inp dry 1 outp rest)
           (<a = "-p") (? (two? >a) (fl >>a (uatoi <>a) R inp dry silent outp rest)
                          (udie 2 "patch: -p wants a count"))
           (<a = "-i") (? (two? >a) (fl >>a p R <>a dry silent outp rest)
                          (udie 2 "patch: -i wants a file"))
           (<a = "-o") (? (two? >a) (fl >>a p R inp dry silent <>a rest)
                          (udie 2 "patch: -o wants a file"))
           ((2 < tally <a) && (snip <a 0 2 = "-p"))
            (fl >a (uatoi (snip <a 2 (tally <a))) R inp dry silent outp rest)
           ((8 < tally <a) && (snip <a 0 8 = "--strip="))
            (fl >a (uatoi (snip <a 8 (tally <a))) R inp dry silent outp rest)
           ((8 < tally <a) && (snip <a 0 8 = "--input="))
            (fl >a p R (snip <a 8 (tally <a)) dry silent outp rest)
           (fl >a p R inp dry silent outp (<a >< rest)))
       q (fl as () 0 () 0 0 () ())
       p <q R? <>q inp <>>q dry? (at q 3) silent? (at q 4) outp (at q 5) rest (at q 6)
       txt (? (string? inp)
              (: r (uread inp) (? (two? r) <>r (udie 2 ("patch: cannot open " + inp))))
              (slurp in))
       secs (pt-parse (lines txt))
       _ (? (two? secs) () (udie 2 "patch: no unified diff in the input"))
       ; the name: the new side unless the patch CREATES the file, and an operand wins
       ; over both -- which is how `patch f < p` retargets a diff cut somewhere else
       (name sec) (? (two? rest) <rest
                     (: nn <>sec
                        (? (nn = "/dev/null") (pt-strip <sec p) (pt-strip nn p))))
       (one acc sec)
        (: hs (? R? (map pt-flip <>>sec) <>>sec)
           nm (? (string? outp) outp (name sec))
           (+ acc (pt-file nm hs dry? silent?)))
       bad (foldl one 0 secs)
       _ (flush out) _ (flush err)
       (? bad 1 0)))
