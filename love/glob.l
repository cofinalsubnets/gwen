; glob -- fnmatch, pure love and no filesystem. the pattern law only: pathname
; expansion (readdir, the dotfile rule at the directory scan) stays in crew/lush/glob.l,
; where the substrate belongs. this half is a predicate over two strings, and more than
; the shell asks it -- lush's case patterns, kore's find -name, the sb ignore law, the
; selfpack walk. selfpack runs on love0, which carries no lush: that is why this is a
; love/ module and not a crew one.
;
; patterns travel as (text . mask) -- the mask a parallel string of '1'/'0' bytes, '1'
; where the char came in unquoted (glob-live), '0' where a quote shielded it ('*' matches
; a literal star) -- absence-of-specialness carried out of band, the quoting law made
; byte-addressable. a caller with no quoting to express asks `match`, which is all-live.
;   (fmatch pat mask s) -> 1 | 0     * ? [a-z] [!..], quoted chars literal
;   (match pat s)       -> the all-live face
;   (anyglob t)         -> does t hold any * ? [ at all (the cheap pre-scan)
;   (ones k)            -> an all-live mask of width k

(module '(glob match fmatch anyglob ones)
(: (anyglob t)
     (: n #t
        (go i) (i != n && (t i @ (42 || 63 || 91) 1 (go (i + 1))))
        (go 0))
   (ones k) (k < 1 ? "" (ones (k - 1) + "1"))
   (match p s) (fmatch p (ones #p) s)
   (fmatch p m s)
   (: np #p ns #s
      (act? i) (i < #m && m i = 49)
      ; [..] at i against (s j) -> (hit . j-past-class) | () unterminated
      (cls i j)
        (: neg (i + 1 < np && (p (i + 1) = 33 || p (i + 1) = 94))
           st (? neg (i + 2) (i + 1))
           (fnd k first hit) (k != np ?  (: b (p k)
            (? (b = 93 && !first) (hit . (k + 1))
               (k + 2 < np && p (k + 1) = 45 && p (k + 2) != 93)
                (fnd (k + 3) 0 (hit || b <= s j && s j <= p (k + 2)))
               (fnd (k + 1) 0 (hit || b = s j)))))
           r (fnd st 1 0)
           ; presence is `two?`, never truth. a class that misses answers
           ; (0 . past), and net drops an improper tail, so a falsy r would read
           ; as "no class here" -- which negation then never gets to invert.
           (two? r ? ((neg ? !<r <r) . >r)))
      (fm i j)
        (? (i = np) (j = ns)
           (: b (p i)
              (? (act? i && (b = 42))
                   (fm (i + 1) j || ((j < ns) && fm i (j + 1)))
                 (act? i && (b = 63)) ((j < ns) && fm (i + 1) (j + 1))
                 (&& (act? i) (b = 91) (j < ns))
                   (: c (cls i j)
                      (? !(two? c) (((s j) = 91) && fm (i + 1) (j + 1))  ; lone [ is itself
                         (<c && fm >c (j + 1))))
                 (j < ns && s j = b) (fm (i + 1) (j + 1)))))
      (bit (fm 0 0))))

) ; module glob
