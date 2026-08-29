; crew/kore/re.l -- grep, over the regex engine in love/re.l. the engine moved out
; (it is pure and had no kore couplings); this half is the one that reads files,
; so it keeps the u-floor (uread, udie) and text.l's lines. smoked byte-identical
; against GNU in `make test_kore`. a toolbox file, no seat -- kore.l is its door.
(use 're)

(module 'kore
(: rebre (from 're 'rebre) reere (from 're 'reere)      ; the four engine names grep reads
   refold (from 're 'refold) refindp (from 're 'refindp)
   recomp (from 're 'recomp) refindpm (from 're 'refindpm)

   ; -- grep --------------------------------------------------------------------
   ; flags ride a tablet keyed by the option charm (u.l's uopts fills it), so a
   ; new one is a row here and not another leg on a tuple every caller re-spells
   (gflag t c) (peep t c 0)
   ; a pattern text -> its node list, by the dialect and the -F/-i flags. a
   ; NEWLINE SPLITS: GNU reads one pattern per line, which is also how -e stacks
   (gnodes t txt)
    ; ⚠ THE PARSER'S SHAPE IS (1 nodes ngroups), so -F answers the same three.
    ; ⚠ AND -F WALKS BY INDEX: `two?` is false for a string, so (map f s) is () --
    ; which reads as the empty pattern and matches every line
    (: (one s) (? (gflag t 70) [1 (map (\ i ['chr (s i)]) (jot (tally s))) 0]
                  ((? (gflag t 69) reere rebre) s))
       (wrap nds) (: a (? (gflag t 120) (cat ('(bol) >< nds) '((eol))) nds)
                     (? (gflag t 105) (refold a) a))
       (go ls acc)
        (ls @ (l >< more)
           (one l @ [_ nds _] (go more ((wrap nds) >< acc)) ())
           [1 (rev acc)])
       ; ⚠ the empty pattern is ONE empty pattern, not none: `lines ""` is (), and
       ; a walk over () would answer no patterns at all -- so `grep ''`, which
       ; matches every line in GNU, would match nothing here
       ls (lines txt)
       (go (? (two? ls) ls [""]) ()))
   ; -w: the span must not sit inside a word. checked at the SPAN, not the
   ; pattern, because a word boundary is not a node this engine can hold
   (gword? c) ((48 <= c <= 57) || (65 <= c <= 90) || (97 <= c <= 122) || (c = 95))
   ; the leftmost match of any of ms (compiled) in ln, word-bounded if w? -> (1 st en) | ()
   (gfind ms ln w? i)
    (: n (tally ln)
       (bnd st en) (? !w? 1
                      ((st = 0 || !(gword? (ln (st - 1)))) &&
                       (en = n || !(gword? (ln en)))))
       (best acc m)
        (refindpm m ln i bnd @ [_ st en _]
          (? !(two? acc) [1 st en] (st < <>acc) [1 st en] acc)
          acc)
       (foldl best () ms))
   (grep-main as)
    (: t (tablet 0)
       o (uopts as t '(101 109) ())              ; -e PATTERN (stacks), -m COUNT
       _ (? (two? o) () (udie 2 "grep: bad option"))
       eps <>o rest <>>o
       ; with -e the whole tail is files; without it the first word is the pattern
       _ (? (two? eps || two? rest) ()
            (udie 2 "usage: grep [-nvclqhaixwFEo] [-e PAT] [-m N] PATTERN [FILE..]"))
       pt (? (two? eps) (ujoinc 10 eps) <rest)
       fs (? (two? eps) rest >rest)
       nss (gnodes t pt)
       _ (? (two? nss) () (udie 2 ("grep: invalid pattern: " + pt)))
       ast (map recomp <>nss)                     ; compiled once, not per line
       n? (gflag t 110) v? (gflag t 118) c? (gflag t 99) l? (gflag t 108)
       q? (gflag t 113) h? (gflag t 104) o? (gflag t 111) w? (gflag t 119)
       mx (: v (uoptv t 109) (? v (uatoi v) ()))
       many (!h? && (two? fs && two? >fs))
       (sel ln) (: m (? (two? (gfind ast ln w? 0)) 1 0) (? v? (? m 0 1) m))
       (pfx label) (? many (: _ (say out label) (put out 58)) ())
       ; -o: every match on the line, each on its own row. -v has nothing to show
       (emit1 label no ln)
        (: (go i)
            (gfind ast ln w? i @ [_ st en]
              ; ⚠ a ZERO-LENGTH match prints NOTHING (GNU's rule, and `-o '[a-z]*'`
              ; is how you meet it) -- but it still advances, or the walk sits still
              (: _ (? (st < en)
                      (: _ (pfx label)
                         _ (? n? (: _ (say out (show no)) (put out 58)) ())
                         _ (say out (snip ln st en)) (put out 10))
                      ())
                 (? (en < tally ln) (go (? (en = st) (en + 1) en)) ()))
              ())
           (go 0))
       ; one input through the chosen face; answers the hit flag onward. ⚠ it READS a
       ; line at a time -- grep of 50 MB used to hold 8.2 GB, because `lines` made a
       ; string and a cell for every line before the first one was matched. -q and -l
       ; stop at the first hit, which is now a stop and not just a shortcut.
       (dof label rd hit)
        (: (anym _) (: l (urd-line rd) (? !(two? l) 0 (? (sel <l) 1 (anym 0))))
           (? q? (? (anym 0) 1 hit)
              l? (? (anym 0) (: _ (say out label) _ (put out 10) 1) hit)
              c? (: (cnt k) (? (mx && (mx <= k)) k
                               (: l (urd-line rd)
                                  (? !(two? l) k (cnt (? (sel <l) (k + 1) k)))))
                    k (cnt 0)
                    _ (pfx label)
                    _ (say out (show k)) _ (put out 10)
                    (? (0 < k) 1 hit))
              (: (lp no h seen)
                  (? (mx && (mx <= seen)) h
                     (: l (urd-line rd)
                        (? !(two? l) h
                           (: x <l
                              m (sel x)
                              _ (? (m && o? && !v?) (emit1 label no x)
                                   m (: _ (pfx label)
                                         _ (? n? (: _ (say out (show no)) (put out 58)) ())
                                         _ (say out x) (put out 10))
                                   ())
                              (lp (no + 1) (? m 1 h) (? m (seen + 1) seen))))))
                 (lp 1 hit 0))))
       ; `bad`, not `err`: err is the PORT, and a shadow here would say into a charm
       st (tablet 0) _ (pin st 'hit 0) _ (pin st 'bad 0)
       _ (ureachm (\ f (: _ (say err ("grep: " + f + ": No such file or directory"))
                          _ (put err 10) _ (pin st 'bad 1) 0))
                  (\ f first rd
                     (: _ (pin st 'hit (dof (? (f = "-") "(standard input)" f) rd
                                            (peep st 'hit 0)))
                        1))
                  fs)
       _ (flush out) _ (flush err)
       (? (peep st 'bad 0) 2 (peep st 'hit 0) 0 1)))
) ; module 'kore
