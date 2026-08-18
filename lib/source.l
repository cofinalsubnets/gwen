(: (src-die m) (say err (";; " + m + "\n"), quit 1)

   (src-head nom) (: n (tally nom)
                     (go i) (? (n <= i) nom (nom i = 47) (snip nom 0 i) (go (i + 1)))
                     (go 0))

   (src-top es) (? (atom? es) ""
    (src-head (tar-get <es 'nom "") @ ("." | "") (src-top >es) c c))

   (src-lay-tgz root top gz)
    (: _ (mkdir (root + "/out") 493)
       _ (mkdir (root + "/out/dist") 493)
       p (root + "/out/dist/" + top + ".tar.gz")
       q (open p "w")
       _ (!q ? (src-die ("can't write " + p)))
       _ (say q gz)
       _ (close q)
       p)

   (src-lay-love root)
    (: exe (selfpath 0)
       _ (string? exe ? 0 (src-die "can't read seed path"))
       _ (mkdir (root + "/bin") 493)
       p (root + "/bin/love")
       ; ⚠ the SIGN is the test, not truth: copyfile answers the byte count, and an empty
       ; source would answer 0 -- which reads false exactly as a -errno does.
       e (copyfile exe p)
       _ (e < 0 ? (src-die ("can't write " + p)))
       (chmod p 493, p))

   (src-lay dir)
    (: gz (source-gz "x")
       _ (!(string? gz) ? (src-die "this love carries no source"))
       u (gz-unzip gz)
       _ (atom? u ? (src-die "the embedded archive did not inflate"))
       t (tar-unpack <>u)
       _ (atom? t ? (src-die "the embedded archive is torn"))
       es <>t
       top (src-top es)
       _ (0 >= tally top ? (src-die "the embedded archive names no top directory"))
       w (tar-scatter dir es)
       _ (atom? w ? (src-die ("can't write " + dir)))
       root (dir = "." ? top (dir + "/" + top))
       (src-lay-love root,
        src-lay-tgz root top gz,
        [1 root <>w]))

   (source-main as)
    (: r (src-lay (? (two? as) <as "."))
     (puts (";; wrote " + <>r + "\n"), 0))

   (src-dirs _)
    (: p ("" + getenv "PATH") n (tally p)
       (go k st acc) (? (n <= k) (rev (snip p st k >< acc))
                        (p k = 58) (go (k + 1) (k + 1) (snip p st k >< acc))
                        (go (k + 1) st acc))
       (go 0 0 ()))
   (src-foreign? p) (: a (stat p) b (stat (selfpath 0)) (two? a ? !(a = b) 0))
   (src-ccworks? cc dir)
    (: c (dir + "/.ccprobe.c") o (dir + "/.ccprobe.o")
       q (open c "w")
       (? !q 0
          (: _ (say q "int main(void){return 0;}\n")
             _ (close q)
             s (herald [cc "-c" "-o" o c])
             st (? (two? s) <s s)
             _ (unlink c) _ (unlink o)
             (0 = st))))
   (src-cc dir)
    (: (try (w >< x))
     (: (go ds) (atom? ds ? (try x)
              (: d (tally <ds ? <ds ".") p (d + "/" + w)
               (src-foreign? p && src-ccworks? p dir ? [1 p] (go >ds))))
      (go (src-dirs ())))
     (try '("cc" "gcc" "clang")))
   (src-farm root exe)
    (d : (root + "/out/.seedbin")
       (mkdir d 493,
        each (keys (from 'verbs 'tab))
             (k \ (d + "/" + string k @ p (unlink p, symlink exe p))),
        d))
   src-needs '("cc" "make" "sh" "sed" "grep" "tr" "cat" "mkdir" "mv" "rm" "ln"
               "test" "uname" "printf" "install" "cmp" "chmod" "touch" "echo")
   ; kore's ujoin is not in this layer -- src-dirs rolls its own split for the
   ; same reason, so the join is local too.
   (src-words l) (? (!(two? l)) "" (!(two? >l)) <l (<l + " " + (src-words >l)))
   (src-ours ns)
    (: (go l acc) (? !(two? l) (rev acc)
                     (: w <l
                        (any ds) (? !(two? ds) 0
                                    (: d (tally <ds ? <ds ".") p (d + "/" + w)
                                       (src-foreign? p ? 1 (any >ds))))
                        (go >l (any (src-dirs ()) ? acc (w >< acc)))))
       (go ns ()))
   ; ⚠ NOT (sha256 (slurp a) = sha256 (slurp b)): slurp draws a byte at a time
   ; and the answer is copied, so two 13 MB binaries cost the waiter ~140 MB it
   ; never gets back. The question is only whether the bytes agree, and a walk
   ; answers it in two buffers. A short read rides as a LEFTOVER, so the streams
   ; line up however the reads happen to fall.
   (src-chunk q b) (: k (swig q b) (? (0 < k) (snip (string b) 0 k) ""))
   (src-same? p1 p2)
    (: q1 (open p1 "r")
       q2 (open p2 "r")
       (? (!(q1 && q2)) (: _ (q1 ? (close q1) 0) _ (q2 ? (close q2) 0) 0)
          (: b1 (cask 65536)
             b2 (cask 65536)
             (go r1 r2)
              (: r1 (tally r1 ? r1 (src-chunk q1 b1))
                 r2 (tally r2 ? r2 (src-chunk q2 b2))
                 n1 (tally r1)
                 n2 (tally r2)
                 m (? (n1 < n2) n1 n2)
                 (? (0 = m) (? (0 = (n1 + n2)) 1 0)      ; both ended, same place
                    (n1 = n2) (r1 = r2 ? (go "" "") 0)   ; the even case: no snip at all
                    (!((snip r1 0 m) = (snip r2 0 m))) 0
                    (go (snip r1 m n1) (snip r2 m n2))))
             v (go "" "")
             _ (close q1)
             _ (close q2)
             v)))

   ; ⚠ THE WAIT RUNS IN A FRESH PROCESS. src-lay's inflate leaves ~200 MB of
   ; heap that the parent would otherwise hold through the whole build, for
   ; nothing -- and no collection gives it back, because free coalesces into a
   ; list and never unmaps, so tightening the budget only ever RAISES rss. exec
   ; is the one thing that returns the pages; the waiter boots at ~57 MB, which
   ; is what a small box under build pressure has to spare. PATH, CC and the cwd
   ; all ride the environment across it, so only the root has to be said.
   (seed-wait root)
    (: exe (selfpath 0)
       _ (string? exe ? 0 (src-die "can't find seed path"))
       here (cwd 0)
       _ (? (charm? (chdir root)) (src-die ("can't enter " + root)) 0)
       s (herald [exe "make" "dist"])
       st (two? s ? <s s)
       _ (chdir here)
       _ (0 != st ? (src-die ("the build failed (exit " + show st + ")")))
       ; the ONE artifact (seed-universal U2): the seed is the laid tree's own
       ; binary, and the rebuild must BE this binary, byte for byte.
       out (root + "/out/host/love")
       (? (src-same? exe out)
         (puts ";; fixpoint ok\n", 0)
        (say err ";; FIXPOINT NOT OK\n", 1)))

   (seed-lay as0)
    (: first? (two? as0 ? (<as0 = "-first") 0)
       as1 (first? ? >as0 as0)
       self? (two? as1 ? (<as1 = "-self") 0)
       as (? self? >as1 as1)
       r (src-lay (two? as ? <as "."))
       root <>r
       exe (selfpath 0)
       _ (string? exe ? 0 (src-die "can't find seed path"))
       farm (src-farm root exe)
       path ("" + getenv "PATH")
       _ (setenv "PATH" (first? ? (farm + ":" + path) (path + ":" + farm)))
       env (getenv "CC")
       amb (!self? && !first? && !(string? env) ? (src-cc $ root + "/out"))
       _ (two? amb ? (setenv "CC" <>amb) 0)
       ours (src-ours src-needs)
       _ (? (two? ours)
          (puts $ ";; using own tools: " + (src-words ours) + "\n"))
       cc (? (string? env) env (two? amb) <>amb "this binary")
       _ (puts (";; seeding with " + cc + "\n"))
       e (exec [exe "seed" "-wait" root])
       (src-die ("can't wait for the build (exec: " + show e + ")")))

   (seed-main as0)
    (? ((two? as0) && (<as0 = "-wait")) (seed-wait (two? >as0 ? <>as0 "."))
       (seed-lay as0))

   (pins (from 'verbs 'tab)
    'source source-main
    'src    source-main
    'seed   seed-main))
