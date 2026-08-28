; --- lush 🐚 the glob -- fnmatch + pathname expansion, pure love ---
; no brackets here: the runtime supplies the layer (see job.l for the cat law).
; no libc glob anywhere (L1/L2 won't have one): readdir + stat are the whole
; substrate. patterns travel as (text . mask) -- the mask a parallel string of
; '1'/'0' bytes, '1' where the char came in unquoted (glob-live), '0' where a
; quote shielded it ('*' matches a literal star) -- absence-of-specialness
; carried out of band, the quoting law made byte-addressable.
;   the matcher itself is love/glob.l -- fmatch / match / anyglob / ones,
;   bound back to their sh- names below so every reader here is unchanged.
;   (sh-globby? t m)       -> any live * ? [ under the mask
;   (sh-glob t m)          -> sorted matching paths | () (caller keeps the word)
; the dotfile law lives at the directory scan: a leading . matches only a
; pattern that spells the . itself.

(module 'lush
(: sh-anyglob (from 'glob 'anyglob)   ; the matcher moved to love/glob.l; every love
   sh-fmatch (from 'glob 'fmatch)     ; carries it now, this one included. the sh- names
   sh-match (from 'glob 'match)       ; stay -- word.l, eval.l, line.l, cook and kore's
   sh-ones (from 'glob 'ones)         ; find all read them, some through (from 'lush ..)
   (sh-globby? t m)
     (: n #t
        (go i) (? (i = n) 0
                  (: b (t i)
                     (? (((m i) = 49) && (|| (b = 42) (b = 63) (b = 91))) 1 (go (i + 1)))))
        (go 0))

   ; pattern -> components split on / (every /, quoted or not, is a path break)
   ; (core `rev`, not job.l's sh-rev: this part stands ALONE -- cook loads
   ; just the glob, by path, for $(wildcard))
   (sh-psplit t m)
     (: n #t
        (go i st acc)
          (? (i = n) (rev ((snip t st i . snip m st i) . acc))
             (? ((t i) = 47)
                (go (i + 1) (i + 1) ((snip t st i . snip m st i) . acc))
                (go (i + 1) st acc)))
        (go 0 0 ()))
   (sh-glob t m)
     (: comps (sh-psplit t m)
        abs (two? comps && !#<<comps)
        cs (? abs >comps comps)
        (? !cs ()                                   ; the pattern was just "/"
           (sort (gwalk (? abs ["/"] [""]) cs))))
   ; walk: prefixes are "" (cwd) / "x/" (always /-tailed); a live component
   ; scans its directory, a dead one appends + stats (the path must EXIST)
   ; presence by two?, never !: ("") is a real prefix list netting 0
   (gwalk pfxs cs)
     (? !(two? pfxs) ()
        (: c <cs ct <c cm >c rest >cs
           nxt (? (sh-globby? ct cm)
                  (catmap (\ d (gmatches d ct cm)) pfxs)
                  (catmap (\ d (: p (d + ct) (? (two? (stat p)) [p] ()))) pfxs))
           (? !(two? rest) nxt (gwalk (map (\ p (p + "/")) nxt) rest))))
   ; sorted, the posix law for expansion -- and readdir's raw order is the
   ; filesystem's (ext4 hashes, ufs appends), so an unsorted glob leaks the
   ; disk into anything built over it.
   (gmatches d ct cm)
     (: names (readdir (? #d d "."))
        dot (tally ct && ((ct 0) = 46))
        (map (\ nm (d + nm))
             (sort (filter (\ nm ((dot || !((nm 0) = 46)) && sh-fmatch ct cm nm))
                           names)))))
) ; module 'lush
