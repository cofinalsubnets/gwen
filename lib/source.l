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
    (src-head (tar-get <es 'nom "") @ ("." || "") (src-top >es) c c))

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
   ; ⚠ AND THE LAY IS AN EGG: the crew arrives at BAKE and a bake means RUNNING
   ; the artifact, which this machine cannot do for another ISA. it does not have
   ; to: the egg carries its source and the dist roster, and its FIRST BOOT on
   ; the target machine bakes and patches it in place (host/main.c) -- the same
   ; bytes a native `make dist` lays, with nothing foreign anywhere in the chain.
   (seed-cross root xa dst)
    (: made (root + "/out/x-" + xa + "/love")
       e (copyfile made dst)
       _ (e < 0 ? (src-die ("can't write " + dst)))
       _ (chmod dst 493)
       _ (puts (";; laid " + dst + " -- an egg for " + xa + ": its first run on that\n"
                + ";; machine bakes the crew from the carried source and patches itself.\n"
                + ";; no fixpoint claimed here: a cross lay cannot BE this binary --\n"
                + ";; `love seed` on the " + xa + " machine proves it.\n"))
       0)

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
       s (herald (string? xa ? [exe "make" ("xa=" + xa) ("out/x-" + xa + "/love")]
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
   seed-arches '(x86_64 aarch64 riscv64)
   ; the other spellings of the same three machines -- uname's, Debian's, the ISA
   ; shorthands and holo's own -- each normalized to the make spelling above.
   ; the word arrives as an argv string and leaves as one (exec, the artifact
   ; name); between the doors it is a symbol.
   seed-spell '((arm64 aarch64) (amd64 x86_64) (x64 x86_64)
                (x86-64 x86_64) (rv64 riscv64) (riscv riscv64))
   (seed-canon w) (: s (intern w) a (assoc s seed-spell) (? (two? a) <>a s))
   ; the arch word is positional and A PATH STILL WINS: `./aarch64` is the
   ; directory, the verb rail's own rule worn one level down.
   (seed-arch? w) (: n (tally w)
                     (go i) (? (n <= i) (member? (seed-canon w) seed-arches)
                               (w i = 47) 0
                               (go (i + 1)))
                     (go 0))
   ; this machine's arch in seed-arches' spelling -- holo names its arches and
   ; uname names them differently, and love-arch speaks holo's.
   (seed-native _) (? (love-arch = "x64") 'x86_64 (love-arch = "arm64") 'aarch64 (intern love-arch))

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
       xa0 (? ((two? as2) && (seed-arch? <as2)) (seed-canon <as2) ())
       xa (? ((nom? xa0) && (id? xa0 (seed-native 0))) () xa0)
       as (nom? xa0 ? >as2 as2)
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
       _ (puts (";; seeding " + (nom? xa ? (show xa + " ") "") + "with " + cc + "\n"))
       dst (nom? xa ? (dir + "/love-" + show xa) ())
       e (nom? xa ? (exec [exe "seed" "-wait" root (show xa) dst])
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

   ; --- `love kernel OUT.elf` -- the metal projection of THIS binary ----------
   ; the artifact is the kernel (doc/misc/plan/inle-fusion.md): this verb emits
   ; the flat image a boot door eats, from nothing but what it already carries.
   ; the boot stub is LAID here, not stored: mkboot.l rides the source blob and
   ; holo is baked, so the lay's cat (kore text/u/asbook, then mkboot, then
   ; kproject) evals off the in-memory tar, exactly the shape kernel.mk cats
   ; from the tree. the arch is the binary's own -- e_machine says it, and a
   ; projection of self can be nothing else.
   (kern-pick es top nm)
    (: want (top + "/" + nm)
       (go l) (? (atom? l) (src-die ("the blob carries no " + nm))
                 ((tar-get <l 'nom "") = want) (tar-get <l 'body "")
                 (go >l))
       (go es))
   (kern-b16 s i) ((s i) + (256 * (s (+ i 1))))
   kern-box (tablet 0)                  ; the verb's inputs, read back by the stream
   (kernel-main as)
    (: _ (? (two? as) 0 (src-die "usage: love kernel OUT.elf"))
       out <as
       exe (selfpath 0)
       _ (string? exe ? 0 (src-die "can't read the seed path"))
       pie (: h (open exe "r")
             (? h (: s (slurp h) _ (close h) s)
                (src-die "can't read this binary")))
       m (kern-b16 pie 18)
       arch (? (m = 62) "x86_64" (m = 183) "aarch64"
               (src-die "no boot door for this machine"))
       gz (source-gz "x")
       _ (string? gz ? 0 (src-die "this love carries no source"))
       u (gz-unzip gz)
       _ (two? u ? 0 (src-die "the embedded archive did not inflate"))
       t (tar-unpack <>u)
       _ (two? t ? 0 (src-die "the embedded archive is torn"))
       es <>t
       top (src-top es)
       ; the INPUTS cross on the verb table -- the one tablet every layer can
       ; reach ((from 'verbs 'tab) hands the real thing) -- under a key pulled
       ; back off below, so `love verbs` never lists it. the stream evals in
       ; its own layer, where a plain name cannot cross either way; its LAST
       ; FORM does the whole projection where everything is visible, so
       ; nothing has to cross back out either.
       _ (pin kern-box 'pie pie)
       _ (pin kern-box 'out out)
       _ (pin kern-box 'arch arch)
       vt (from 'verbs 'tab)
       _ (pin vt 'kern-in kern-box)
       cat ("(use 'holo)"
            + (kern-pick es top "crew/kore/text.l")
            + (kern-pick es top "crew/kore/u.l")
            + "(use 'kore)"
            + (kern-pick es top "crew/kore/asbook.l")
            + (kern-pick es top "free/mkboot.l")
            + (kern-pick es top "tools/kproject.l")
            + "(: kb (peep (from 'verbs 'tab) 'kern-in 0)"
            + "   bb (string (cask (lay-boot-bytes (peep kb 'arch 0))))"
            + "   (kproject (peep kb 'pie 0) bb (peep kb 'out 0) (peep kb 'arch 0)))")
       _ (reads (tap ((: (g i) (? (< i (tally cat)) (link (peep cat i 0) (g (+ 1 i))))) 0)))
       _ (pull vt 'kern-in 0)
       _ (puts (";; wrote " + out + " (" + arch + ")\n"))
       0)

   (pins (from 'verbs 'tab)
    'source source-main
    'src    source-main
    'seed   seed-main
    'kernel kernel-main))

) ; module 'source
