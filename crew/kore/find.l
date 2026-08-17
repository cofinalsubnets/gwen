; crew/kore/find.l -- find: walk the trees under the named paths, test each name
; against an expression, act on what matches. a toolbox file, no seat -- kore
; (crew/kore/kore.l) is its door. it rides the u-floor (udie, ubase, uatoi) and
; lush's fnmatch (`sh-match`, crew/lush/glob.l) for -name and -path, which is why
; it loads LATE in the cat: a body captures its free names at its define, so the
; matcher has to be there first.
;
; the surface:
;   find [PATH..] [EXPR]        PATH defaults to .
;   primaries  -name PAT  -path PAT  -type f|d|l  -print  -prune  -exec CMD.. ;
;              -true  -false  -maxdepth N  -mindepth N
;   operators  ( )  ! / -not  -a / -and (implicit between two primaries)  -o / -or
; an expression naming no action gets -print, exactly as GNU does; -a binds tighter
; than -o and both short-circuit.
;
; ⚠ THE WALK SORTS EACH DIRECTORY. GNU hands out readdir order, which is the file
; system's business and not a promise anyone can repeat -- so `find | sort` is the
; only honest way to smoke us against it, and that is what test/gate/kore.sh does.
; sorted is also what a build wants: the same tree cuts the same image twice.
;
; ⚠ SYMLINKS ARE NOT FOLLOWED (GNU's -P, the default). the `stat` nif follows, so
; the type read asks `readlink` FIRST -- a link answers l, whatever it points at,
; and the walk does not descend through it. a dangling link is still visited.
(module 'kore
(use 'lush) ; -name/-path ride lush's fnmatch (sh-match), captured at the define below
(:
   ; a path and one entry -> the child's path (a trailing / on the parent stays one /)
   (fi-join p b)
    (: n (tally p) (? ((0 < n) && (p (n - 1) = 47)) (p + b) (p + "/" + b)))
   ; a path -> its type charm: f, d, l, or 0 for a thing that is none of the three
   (fi-type p)
    (? (string? (readlink p)) 108
       (: st (stat p)
          (? !(two? st) 0
             (: m (& 61440 <>>st) (? (m = 16384) 100 (m = 32768) 102 0)))))

   ; -- the expression: words in, a tree out --
   ; every parse door answers (node rest) or () -- a node is never (), so presence
   ; carries in the shape and nothing needs a wrapper
   (fi-exec m acc)                              ; -exec's words, up to the closing ;
    (? !(two? m) ()
       (<m = ";") (? (two? acc) [['exec (rev acc)] >m] ())
       (fi-exec >m (<m >< acc)))
   (fi-prim ws)
    (? !(two? ws) ()
       (: a <ws m >ws
          (? (a = "-print") [['print] m]
             (a = "-prune") [['prune] m]
             (a = "-true")  [['true] m]
             (a = "-false") [['false] m]
             (a = "-exec")  (fi-exec m ())
             !(two? m) ()
             (a = "-name") [['name <m] >m]
             (a = "-path") [['path <m] >m]
             (a = "-type") [['type <m] >m]
             ())))
   (fi-not ws)
    (? !(two? ws) ()
       ((<ws = "!") || (<ws = "-not"))
        (: r (fi-not >ws) (? (two? r) [['not <r] <>r] ()))
       (<ws = "(")
        (: r (fi-or >ws)
           (? !(two? r) ()
              (: m <>r (? ((two? m) && (<m = ")")) [<r >m] ()))))
       (fi-prim ws))
   ; -a is implicit: any word that is not a closing sigil opens another and-arm
   (fi-and ws)
    (: r (fi-not ws)
       (? !(two? r) ()
          (: (go n m)
              (? !(two? m) [n m]
                 (: m2 (? ((<m = "-a") || (<m = "-and")) >m m)
                    (? (!(two? m2) || (<m2 = "-o") || (<m2 = "-or") || (<m2 = ")")) [n m]
                       (: r2 (fi-not m2)
                          (? (two? r2) (go ['and n <r2] <>r2) ())))))
             (go <r <>r))))
   (fi-or ws)
    (: r (fi-and ws)
       (? !(two? r) ()
          (: (go n m)
              (? ((two? m) && ((<m = "-o") || (<m = "-or")))
                 (: r2 (fi-and >m)
                    (? (two? r2) (go ['or n <r2] <>r2) ()))
                 [n m])
             (go <r <>r))))
   ; -maxdepth/-mindepth are GLOBAL wherever they sit (GNU's reading too), so they
   ; come out of the word list before the grammar ever sees them
   (fi-depths ws maxd mind acc)
    (? !(two? ws) [(rev acc) maxd mind]
       (: a <ws
          (? ((a = "-maxdepth") && (two? >ws)) (fi-depths >>ws (uatoi <>ws) mind acc)
             ((a = "-mindepth") && (two? >ws)) (fi-depths >>ws maxd (uatoi <>ws) acc)
             (fi-depths >ws maxd mind (a >< acc)))))
   ; the leading words that are neither a flag nor an open bracket are the paths
   (fi-paths as acc)
    (? !(two? as) [(rev acc) ()]
       (: a <as
          (? (((0 < tally a) && (a 0 = 45)) || (a = "(") || (a = "!"))
             [(rev acc) as]
             (fi-paths >as (a >< acc)))))
   ; does the tree act, or only test -- the question that decides the added -print
   (fi-acts? e)
    (: k <e
       (? ((k = 'print) || (k = 'exec)) 1
          (k = 'not) (fi-acts? <>e)
          ((k = 'and) || (k = 'or)) (? (fi-acts? <>e) 1 (fi-acts? <>>e))
          0))

   ; -- the walk --
   ; st carries the two things a visit leaves behind: 112 the prune mark (cleared at
   ; every name, set by -prune), 101 the trouble mark (an -exec that would not run)
   (fi-eval e p base ty st)
    (: k <e
       (? (k = 'print) (: _ (say out p) _ (put out 10) 1)
          (k = 'name)  (? (sh-match <>e base) 1 0)
          (k = 'path)  (? (sh-match <>e p) 1 0)
          (k = 'type)  (: c <>e (? ((0 < tally c) && (ty = (c 0))) 1 0))
          (k = 'prune) (: _ (pin st 112 1) 1)
          (k = 'true)  1
          (k = 'false) 0
          (k = 'not)   (? (fi-eval <>e p base ty st) 0 1)
          (k = 'and)   (? (fi-eval <>e p base ty st) (fi-eval <>>e p base ty st) 0)
          (k = 'or)    (? (fi-eval <>e p base ty st) 1 (fi-eval <>>e p base ty st))
          (k = 'exec)
           (: ws (map (\ w (? (w = "{}") p w)) <>e)
              _ (flush out)                     ; the child shares the port: say what we said
              pid (spawn ws)
              (? (pid < 0) (: _ (say err ("find: cannot run " + <ws)) _ (put err 10)
                              _ (pin st 101 1) 0)
                 (? (wait pid) 0 1)))
          0))
   (fi-walk p depth e st maxd mind)
    (: ty (fi-type p)
       _ (pin st 112 0)
       _ (? (mind <= depth) (fi-eval e p (ubase p "") ty st) 0)
       (? ((ty = 100) && (depth < maxd) && !(peep st 112 0))
          (: (go m) (? (two? m)
                       (: _ (fi-walk (fi-join p <m) (depth + 1) e st maxd mind) (go >m))
                       0)
             (go (sort (readdir p))))
          0))

   (find-main as)
    (: sp (fi-paths as ())
       d (fi-depths <>sp 536870911 0 ())
       ws <d maxd <>d mind <>>d
       pr (? (two? ws) (fi-or ws) [['true] ()])
       _ (? ((two? pr) && !(two? <>pr)) ()
            (udie 1 "usage: find [PATH..] [EXPR]  -- see doc/kore.md for the grammar"))
       e (? (fi-acts? <pr) <pr ['and <pr ['print]])
       st (tablet 0)
       ; a named path that is not there is find's one loud failure: it complains,
       ; the walk skips it, and the exit code remembers -- the rest still walk
       (there? p) ((string? (readlink p)) || (two? (stat p)))
       (go m) (? !(two? m) (: _ (flush out) (? (peep st 101 0) 1 0))
                 (: _ (? (there? <m) (fi-walk <m 0 e st maxd mind)
                         (: _ (say err ("find: '" + <m + "': No such file or directory"))
                            _ (put err 10) (pin st 101 1)))
                    (go >m)))
       (go (? (two? <sp) <sp '(".")))))
) ; module 'kore
