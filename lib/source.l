; the wrap: see lib/tarcmd.l -- `use` splices into the current layer, so the two asks
; need a layer of this file's own rather than the dist cat's. the seed's lay reads the
; carried archive through both, and rode that cat's leak to reach them before.
(module 'source
(use 'gz)
(use 'tar)

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
   ; is there an executable by this name on PATH -- no foreign test, unlike
   ; src-cc: the question is only whether the emulator is installed.
   (src-onpath? w)
    (: (go ds) (? (!(two? ds)) ()
                  (: d (tally <ds ? <ds ".") p (d + "/" + w)
                     (two? (stat p) ? [1 p] (go >ds))))
       (go (src-dirs ())))
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
   ; ⚠ `snip` reads the CASK: (snip (string b) 0 k) would copy all 64K to take the k
   ; bytes the read filled, and a short read at the tail of every file pays it twice.
   (src-chunk q b) (: k (swig q b) (? (0 < k) (snip b 0 k) ""))
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

   ; ⚠ THE FIXPOINT IS SCOPED, NOT DROPPED. It can hold only where the output
   ; ought to BE this binary, and a cross lay is the one case the invocation
   ; itself says it cannot -- so it is skipped for a reason the caller NAMED,
   ; never for one that was discovered. A dirty tree still fails.
   ; ⚠ AND THE LAY IS PRE-CREW: the link answers three verbs (bake wake verbs),
   ; because the crew arrives at BAKE and a bake means RUNNING the artifact --
   ; which this machine cannot do for another ISA. qemu-user can where it is
   ; installed; where it is not, the target machine runs the line we print.
   ; ⚠ THE BAKE RUNS IN THE LAID TREE. `use` reaches a module through the baked
   ; table or lib/ off the CWD and nowhere else, so a bake standing anywhere but
   ; the root scares no-module on the first `use` the cats reach. The binary is
   ; named absolutely across the chdir, since dst is the caller's cwd's.
   (seed-cross root xa dst)
    (: made (root + "/out/x-" + xa + "/love")
       e (copyfile made dst)
       _ (e < 0 ? (src-die ("can't write " + dst)))
       _ (chmod dst 493)
       here (cwd 0)
       abs (? (dst 0 = 47) dst (here + "/" + dst))
       d0 "out/host/.docs-cat.l:libra,help"
       d1 "out/host/.rest-cat.l"
       q (src-onpath? ("qemu-" + xa))
       _ (two? q ? (puts (";; baking through qemu-" + xa + " -- minutes, and the crew\n"
                          + ";; is what makes it a seed and not a three-verb egg\n")))
       _ (two? q ? (? (charm? (chdir root)) (src-die ("can't enter " + root)) 0))
       s (two? q ? (herald [<>q abs "bake" "-L" d0 "-L" d1]))
       _ (two? q ? (chdir here))
       st (two? s ? <s s)                       ; herald answers a pair or a status
       ok (two? q ? (0 = st) 0)
       _ (puts (";; laid " + dst + (ok ? " -- baked\n" " -- an EGG\n")))
       ; three ends, three stories: baked, no emulator to bake with, and a bake
       ; that ran and failed -- which is never the emulator's absence.
       _ (? ok (puts (";; no fixpoint: a cross lay cannot BE this binary, so the claim\n"
                      + ";; is not made. `love seed` on the " + xa + " machine proves it.\n"))
              (two? q)
              (puts (";; the bake FAILED under qemu-" + xa + " (exit " + show st + ") and the\n"
                     + ";; lay is three verbs (bake wake verbs). To see it again:\n"
                     + ";;   cd " + root + " && " + abs + " bake -L " + d0 + " -L " + d1 + "\n"))
              (puts (";; three verbs (bake wake verbs), and no qemu-" + xa + " here to\n"
                     + ";; grow them. On the " + xa + " machine, or with the emulator:\n"
                     + ";;   cd " + root + " && " + abs + " bake -L " + d0 + " -L " + d1 + "\n")))
       ; a bake we ASKED FOR and did not get is a failure; no emulator is not.
       (ok ? 0 (two? q ? 1 0)))

   ; ⚠ THE WAIT RUNS IN A FRESH PROCESS. src-lay's inflate leaves ~200 MB of
   ; heap that the parent would otherwise hold through the whole build, for
   ; nothing -- and no collection gives it back, because free coalesces into a
   ; list and never unmaps, so tightening the budget only ever RAISES rss. exec
   ; is the one thing that returns the pages; the waiter boots at ~57 MB, which
   ; is what a small box under build pressure has to spare. PATH, CC and the cwd
   ; all ride the environment across it, so only the root has to be said.
   (seed-wait root xa dst)
    (: exe (selfpath 0)
       _ (string? exe ? 0 (src-die "can't find seed path"))
       here (cwd 0)
       _ (? (charm? (chdir root)) (src-die ("can't enter " + root)) 0)
       ; the cats ride along: they are the crew a bake needs, and arch-blind
       s (herald (string? xa ? [exe "make" ("xa=" + xa) ("out/x-" + xa + "/love")
                                "out/host/.docs-cat.l" "out/host/.rest-cat.l"]
                              [exe "make" "dist"]))
       st (two? s ? <s s)
       _ (chdir here)
       _ (0 != st ? (src-die ("the build failed (exit " + show st + ")")))
       (? (string? xa) (seed-cross root xa dst)
          ; the ONE artifact (seed-universal U2): the seed is the laid tree's own
          ; binary, and the rebuild must BE this binary, byte for byte.
          (: out (root + "/out/host/love")
             (? (src-same? exe out)
               (puts ";; fixpoint ok\n", 0)
              (say err ";; FIXPOINT NOT OK\n", 1)))))

   ; the cross door: we are a cross compiler carrying our own source, so a seed
   ; lays a seed for a machine it is not -- which is the portability story now
   ; that one binary per ISA is the shape. Every love carries every ISA's
   ; runtime (mkrt joins all three), so any version builds any other.
   seed-arches '("x86_64" "aarch64" "riscv64")
   ; the arch word is positional and A PATH STILL WINS: `./aarch64` is the
   ; directory, the verb rail's own rule worn one level down.
   (seed-arch? w) (: n (tally w)
                     (go i) (? (n <= i) (member? w seed-arches)
                               (w i = 47) 0
                               (go (i + 1)))
                     (go 0))
   ; this machine's arch in seed-arches' spelling -- holo names its arches and
   ; uname names them differently, and love-arch speaks holo's.
   (seed-native _) (? (love-arch = "x64") "x86_64" (love-arch = "arm64") "aarch64" love-arch)

   (seed-lay as0)
    (: first? (two? as0 ? (<as0 = "-first") 0)
       as1 (first? ? >as0 as0)
       self? (two? as1 ? (<as1 = "-self") 0)
       as2 (? self? >as1 as1)
       ; naming THIS machine is a NATIVE seed, fixpoint and all: the cross lane
       ; exists for a machine we are not, and the fixpoint is scoped to where the
       ; output ought to BE this binary -- which `love seed x86_64` on an x86_64
       ; box asks for exactly as much as a bare `love seed`. ⚠ the WORD is eaten
       ; either way, or the arch would go on to be read as the directory.
       xa0 (? ((two? as2) && (seed-arch? <as2)) <as2 ())
       xa (? ((string? xa0) && (xa0 = (seed-native 0))) () xa0)
       as (string? xa0 ? >as2 as2)
       dir (two? as ? <as ".")
       r (src-lay dir)
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
       _ (puts (";; seeding " + (string? xa ? (xa + " ") "") + "with " + cc + "\n"))
       dst (string? xa ? (dir + "/love-" + xa) ())
       e (string? xa ? (exec [exe "seed" "-wait" root xa dst])
                       (exec [exe "seed" "-wait" root]))
       (src-die ("can't wait for the build (exec: " + show e + ")")))

   (seed-main as0)
    (? (!((two? as0) && (<as0 = "-wait"))) (seed-lay as0)
       (: a1 >as0
          root (two? a1 ? <a1 ".")
          a2 (two? a1 ? >a1 ())
          xa (two? a2 ? <a2 ())
          a3 (two? a2 ? >a2 ())
          dst (two? a3 ? <a3 ())
          (seed-wait root xa dst)))

   (pins (from 'verbs 'tab)
    'source source-main
    'src    source-main
    'seed   seed-main))

) ; module 'source
