; --- lush 🐚 the glob -- fnmatch + pathname expansion, pure love ---
; NO BRACKETS HERE: the runtime supplies the layer (see job.l for the cat law).
;
; no libc glob anywhere (L1/L2 won't have one): readdir + stat are the whole
; substrate. patterns travel as (text . mask) -- the mask a parallel string of
; '1'/'0' bytes, '1' where the char came in UNQUOTED (glob-live), '0' where a
; quote shielded it ('*' matches a literal star) -- absence-of-specialness
; carried out of band, the quoting law made byte-addressable.
;   (sh-fmatch pat mask s) -> 1 | 0     * ? [a-z] [!..], quoted chars literal
;   (sh-match pat s)       -> the all-live face (tests, case patterns later)
;   (sh-anyglob t)         -> does t hold any * ? [ at all (the cheap pre-scan)
;   (sh-globby? t m)       -> any LIVE * ? [ under the mask
;   (sh-glob t m)          -> sorted matching paths | () (caller keeps the word)
; the dotfile law lives at the directory scan: a leading . matches only a
; pattern that spells the . itself.

(: (sh-anyglob t)
     (: n (tally t)
        (go i) (? (= i n) 0
                  (: b (t i) (? (|| (= b 42) (= b 63) (= b 91)) 1 (go (+ i 1)))))
        (go 0))
   (sh-globby? t m)
     (: n (tally t)
        (go i) (? (= i n) 0
                  (: b (t i)
                     (? ((= (m i) 49) && (|| (= b 42) (= b 63) (= b 91))) 1 (go (+ i 1)))))
        (go 0))
   (sh-ones k) (? (< k 1) "" (+ (sh-ones (- k 1)) "1"))
   (sh-match p s) (sh-fmatch p (sh-ones (tally p)) s)

   (sh-fmatch p m s)
   (: np (tally p) ns (tally s)
      (act? i) ((< i (tally m)) && (= (m i) 49))
      ; [..] at i against (s j) -> (hit . j-past-class) | () unterminated
      (cls i j)
        (: neg ((< (+ i 1) np) && ((= (p (+ i 1)) 33) || (= (p (+ i 1)) 94)))
           st (? neg (+ i 2) (+ i 1))
           (fnd k first hit)
             (? (= k np) ()
                (: b (p k)
                   (? ((= b 93) && !first) (hit >< (+ k 1))
                      (&& (< (+ k 2) np) (= (p (+ k 1)) 45) !(= (p (+ k 2)) 93))
                        (fnd (+ k 3) 0 (hit || ((<= b (s j)) && (<= (s j) (p (+ k 2))))))
                      (fnd (+ k 1) 0 (hit || (= b (s j)))))))
           r (fnd st 1 0)
           (? (nil? r) () ((? neg !<r <r) >< >r)))
      (fm i j)
        (? (= i np) (= j ns)
           (: b (p i)
              (? (act? i && (= b 42))
                   (fm (+ i 1) j || ((< j ns) && fm i (+ j 1)))
                 (act? i && (= b 63)) ((< j ns) && fm (+ i 1) (+ j 1))
                 (&& (act? i) (= b 91) (< j ns))
                   (: c (cls i j)
                      (? (nil? c) ((= (s j) 91) && fm (+ i 1) (+ j 1))     ; lone [ is itself
                         (<c && fm >c (+ j 1))))
                 ((< j ns) && (= (s j) b)) (fm (+ i 1) (+ j 1))
                 0)))
      (fm 0 0))

   ; pattern -> components split on / (every /, quoted or not, is a path break)
   ; (core `rev`, not job.l's sh-rev: this part stands ALONE -- cook includes
   ; just the glob via (use 'lush/glob) for $(wildcard))
   (sh-psplit t m)
     (: n (tally t)
        (go i st acc)
          (? (= i n) (rev ((snip t st i >< snip m st i) >< acc))
             (? (= (t i) 47)
                (go (+ i 1) (+ i 1) ((snip t st i >< snip m st i) >< acc))
                (go (+ i 1) st acc)))
        (go 0 0 ()))
   (sh-glob t m)
     (: comps (sh-psplit t m)
        abs (two? comps && !(tally <<comps))
        cs (? abs >comps comps)
        (? !cs ()                                   ; the pattern was just "/"
           (sort (gwalk (? abs (L "/") (L "")) cs))))
   ; walk: prefixes are "" (cwd) / "x/" (always /-tailed); a live component
   ; scans its directory, a dead one appends + stats (the path must EXIST)
   ; ⚠ presence by two?, never !: ("") is a real prefix list netting 0
   (gwalk pfxs cs)
     (? !(two? pfxs) ()
        (: c <cs ct <c cm >c rest >cs
           nxt (? (sh-globby? ct cm)
                  (catmap (\ d (gmatches d ct cm)) pfxs)
                  (catmap (\ d (: p (+ d ct) (? (two? (stat p)) (L p) ()))) pfxs))
           (? !(two? rest) nxt (gwalk (map (\ p (+ p "/")) nxt) rest))))
   (gmatches d ct cm)
     (: names (readdir (? (tally d) d "."))
        dot (tally ct && (= (ct 0) 46))
        (map (\ nm (+ d nm))
             (filter (\ nm ((dot || !(= (nm 0) 46)) && sh-fmatch ct cm nm))
                     names))))
