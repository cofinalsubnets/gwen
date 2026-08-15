; lib/source.l -- `love source [DIR]`, the artifact handing out its own source.
;
; the seed carries the source tarball in .rodata (tools/mksrc.l lays it,
; host/src.c answers it), so there is no second download and no `tar xf`: the binary
; you ran IS the source distribution. doc/dist.md.
;
;   love source [DIR]         lay ./love-<ver>/ and build in it
;   love seed   [DIR]         lay it, BUILD it, and refuse unless the seed it produces is
;                             this binary byte for byte -- the whole fixpoint, one word.
;                             prefers an ambient cc (diverse double compiling); -self forbids it
;   love seed ARCH [DIR]      lay a seed for ANOTHER machine (x86_64 aarch64 riscv64) --
;                             we are a cross compiler carrying our own source, so one
;                             laptop can hand out the pi's download. NO fixpoint there:
;                             see the ⚠ over the check. `./aarch64` is still a directory.
;
; ⚠ IT LAYS bin/love TOO, and that is not a convenience -- it is the point. ./Makefile
; takes a bin/love beside the tree as the C compiler ($(origin CC), bundled_love), so
; the unpacked tree builds with the binary that carried it and touches no ambient cc.
; Source alone would unpack fine and then fall back to whatever compiler the box had,
; which is exactly the claim the seed exists to make good on.
;
; ⚠ the top directory comes from THE ARCHIVE, never from love-version: the tarball's
; own name is what the paths inside it say, and deriving it twice is how the two come
; to disagree on a version whose id ends up dirty.
(: (src-die m) (: _ (say err ("source: " + m + "\n")) (quit 1))
   (src-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) `(1 (s + ""))) ()))

   ; the leading path component of an entry -- everything before the first "/"
   (src-head nom) (: n (tally nom)
                     (go i) (? (n <= i) nom (nom i = 47) (snip nom 0 i) (go (i + 1)))
                     (go 0))
   ; ⚠ the archive's OWN ROOT rides first, as "./" -- tar-gather walks the directory it
   ; was handed and that directory is an entry too. So the top is the first component
   ; that names something, not the first entry; taking entry one lands every path at "."
   ; and lays bin/love beside the tree instead of inside it.
   (src-top es) (? (atom? es) ""
                   (: c (src-head (tar-get <es 'nom ""))
                      (? ((c = ".") || (c = "")) (src-top >es) c)))

   ; ⚠ THE ARCHIVE ITSELF RIDES ALONG, at the path $(dist_source) names. An extracted
   ; tree has no .git, and the stage is `git checkout-index` -- so it cannot RE-CUT the
   ; tarball, and `make dist` there dies "not a git repository". Laying the very bytes we
   ; carried means the rebuilt seed binary embeds the SAME archive, so it comes out
   ; byte-identical by construction rather than by a re-pack that has to coincide.
   (src-lay-tgz root top gz)
    (: _ (mkdir (root + "/out") 493)
       _ (mkdir (root + "/out/dist") 493)
       p (root + "/out/dist/" + top + ".tar.gz")
       q (open p "w")
       _ (? q 0 (src-die ("cannot write " + p)))
       _ (say q gz)
       _ (close q)
       p)

   ; the running binary, laid where the unpacked tree's `make` looks for it
   (src-lay-love root)
    (: exe (readlink "/proc/self/exe")
       _ (? (string? exe) 0 (src-die "cannot read my own path"))
       b (src-slurp exe)
       _ (? (two? b) 0 (src-die ("cannot read " + exe)))
       _ (mkdir (root + "/bin") 493)
       p (root + "/bin/love")
       q (open p "w")
       _ (? q 0 (src-die ("cannot write " + p)))
       _ (say q <>b)
       _ (close q)
       _ (chmod p 493)
       p)

   ; the lay itself, answering (1 root files) -- `seed` builds on exactly this, so the
   ; two verbs cannot drift on where the tree lands or what it contains.
   (src-lay dir)
    (: gz (source-gz "x")
       ; ⚠ `string?`, not `!`: an artifact with no source and one carrying an empty
       ; archive are both nil by net, and only the KIND tells them apart (host/src.c).
       _ (? (string? gz) 0 (src-die "this love carries no source"))
       u (gz-unzip gz)
       _ (? (two? u) 0 (src-die "the embedded archive did not inflate"))
       t (tar-unpack <>u)
       _ (? (two? t) 0 (src-die "the embedded archive is torn"))
       es <>t
       top (src-top es)
       _ (? (0 < tally top) 0 (src-die "the embedded archive names no top directory"))
       w (tar-scatter dir es)
       _ (? (two? w) 0 (src-die ("cannot write under " + dir)))
       root (? (dir = ".") top (dir + "/" + top))
       _ (src-lay-love root)
       _ (src-lay-tgz root top gz)
       [1 root <>w])

   (source-main as)
    (: r (src-lay (? (two? as) <as "."))
       _ (puts ("source: " + show <>>r + " files in " + <>r
                + ", with bin/love and the archive -- `cd " + <>r + " && make`\n"))
       0)

   ; --- seed: the whole fixpoint, run by the artifact ON ITSELF -------------------
   ; `love seed [DIR]` lays the source, builds a new seed with the binary that carried
   ; it, and REFUSES unless what comes out is this binary byte for byte. That is the
   ; release claim -- doc/dist.md's circle, test/gate/distboot.sh's fourth leg -- said as
   ; one word, so somebody holding one file can check it on their own machine.
   ; ⚠ SETTING CC IS HOW IT DEFERS, and the mechanism is the same one that made an earlier
   ; well-meant override wrong: CC in the ENVIRONMENT turns OFF ./Makefile's bundled_love
   ; branch ($(origin CC) is the guard). Wrong when the point was "the toolchain is us",
   ; exactly right when the point is "let a foreign compiler hold the scaffold".
   ; ⚠ THE FIXPOINT ONLY HOLDS ON A COMMITTED TREE, and that is a property of the artifact
   ; rather than a limit of this verb: a release is cut from the git INDEX, so an artifact
   ; built from a dirty tree carries source that is not what built it, and rebuilding from
   ; that source cannot land on the same bytes. It comes out megabytes apart, not one byte
   ; -- which is the shape that tells this apart from a reproducibility bug.
   ; ⚠ and it is a REFUSAL, not a report: a fixpoint that failed and exited 0 would be a
   ; worse artifact than one that never claimed it.
   (seed-arch _) (? (love-arch = "arm64") "aarch64" (love-arch = "x64") "x86_64" love-arch)
   ; the arches a seed can be laid FOR, each with the ELF e_machine its artifact must
   ; answer. ⚠ crew/build.mk's twin roster is the AUTHORITY -- an arch missing there is
   ; refused by make, loudly and before anything is built; this list is what the usage
   ; line reads and what the cross lane's shape check compares against.
   seed-arches '(("x86_64" 62) ("aarch64" 183) ("riscv64" 243))
   (seed-em a) (: (go l) (? !(two? l) ()
                            (: r <l (? (<r = a) <>r (go >l))))
                  (go seed-arches))
   (seed-names _) (ujoin (map (\ r <r) seed-arches))
   ; the e_machine off an ELF header (bytes 18-19, little-endian) | ()
   (seed-elfmach p)
    (: b (src-slurp p)
       (? !(two? b) ()
          (: t <>b (? ((tally t) < 20) () ((t 18) + ((t 19) * 256))))))
   ; PATH, split -- "" is the cwd, the way execvp reads an empty field
   (src-dirs _)
    (: p ("" + (getenv "PATH")) n (tally p)
       (go k st acc) (? (n <= k) (rev (snip p st k >< acc))
                        (p k = 58) (go (k + 1) (k + 1) (snip p st k >< acc))
                        (go (k + 1) st acc))
       (go 0 0 ()))
   ; ⚠ AND IT MUST NOT BE US. An installed love wearing the name `cc` is our own mooncc,
   ; so building through it proves nothing a self build does not -- the whole value of
   ; the ambient lane is that a DIFFERENT compiler held the scaffold. stat says which.
   (src-foreign? p) (: a (stat p) b (stat "/proc/self/exe") (two? a ? !(a = b) 0))
   ; ⚠ AND IT IS PROBED, not merely found. A `cc` on PATH that cannot compile hello-world
   ; -- a stub, a wrong-arch install, a shim whose backend is gone -- would otherwise take
   ; the lane and fail the whole verb, when this binary could have done the job. One tiny
   ; compile is what configure has always spent to learn the same thing.
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
   (src-cc dir)                                ; the first ambient cc that is not us AND works
    (: (try ws) (? !(two? ws) ()
                   (: w <ws
                      (go ds) (? !(two? ds) (try >ws)
                                 (: d (? (tally <ds) <ds ".") p (d + "/" + w)
                                    (? (src-foreign? p && src-ccworks? p dir) [1 p] (go >ds))))
                      (go (src-dirs ()))))
       (try '("cc" "gcc" "clang")))
   ; OUR OWN TOOLS, as a directory of argv[0] symlinks -- every verb this binary answers,
   ; wearing its own name, so a build that reaches for `sed` or `make` or `cc` can land on
   ; us through PATH like any other program.
   (src-farm root exe)
    ; ⚠ the `:` is PARENTHESISED inside the lambda. It is the loosest band there is, so a
    ; bare `k \ p : ..` folds to (: (\ k p) ..) -- the binder takes the LAMBDA as its name
    ; and the body never binds p at all. `;; missing p`, at the first symlink.
    (d : (root + "/out/.seedbin")
       (mkdir d 493,
        each (keys (from 'verbs 'tab))
             (k \ (p : (d + "/" + string k) (unlink p, symlink exe p))),
        d))
   ; ..and which of the tools a build spends have NO ambient provider, so we supplied them
   src-needs '("cc" "make" "sh" "sed" "grep" "tr" "cat" "mkdir" "mv" "rm" "ln"
               "test" "uname" "printf" "install" "cmp" "chmod" "touch" "echo")
   (src-ours ns)                               ; -> the names PATH answers only from us
    (: (go l acc) (? !(two? l) (rev acc)
                     (: w <l
                        (any ds) (? !(two? ds) 0
                                    (: d (tally <ds ? <ds ".") p (d + "/" + w)
                                       (src-foreign? p ? 1 (any >ds))))
                        (go >l (any (src-dirs ()) ? acc (w >< acc)))))
       (go ns ()))
   (seed-main as0)
    (: first? (two? as0 ? (<as0 = "-first") 0)        ; -first: OUR tools win (the gate's own lane)
       as1 (first? ? >as0 as0)
       self? (two? as1 ? (<as1 = "-self") 0)          ; -self: never defer to an ambient cc
       as2 (? self? >as1 as1)
       ; THE ARCH WORD, when the first operand names one. ⚠ a path always wins: `./aarch64`
       ; is the directory, which is the verb rail's own rule (a `/` is always the file) worn
       ; one level down -- so a dir sharing an arch's name is still reachable, spelled.
       arch? (two? as2 ? (seed-em <as2) 0)
       want (? arch? <as2 (seed-arch ()))
       as (? arch? >as2 as2)
       cross? !(want = seed-arch ())
       r (src-lay (two? as ? <as "."))
       root <>r
       exe (readlink "/proc/self/exe")
       _ (? (string? exe) 0 (src-die "cannot find my own path"))
       ; WHOSE COMPILER: an explicit CC wins, then an ambient one unless -self, then us.
       ; ⚠ deferring is the STRONGER claim, not a concession. A foreign cc holding the
       ; scaffold makes this diverse double compiling -- the one thing a self build cannot
       ; say, since a compiler carrying a Thompson attack reproduces itself byte for byte
       ; too. It costs nothing to prefer: the ambient cc builds love0 and NOTHING else,
       ; and both lanes were measured to the same artifact hash.
       ; ⚠ OUR TOOLS GO LAST ON PATH BY DEFAULT. The ambient system wins wherever it can,
       ; and we fill only what it does not have -- so `love seed` on an ordinary box is a
       ; check against the machine's own toolchain, not against ourselves. `-first` puts us
       ; ahead of it, which is the lane this project's own gates want.
       farm (src-farm root exe)
       path ("" + getenv "PATH")
       _ (setenv "PATH" (? first? (farm + ":" + path) (path + ":" + farm)))
       env (getenv "CC")
       amb (!self? && !first? && !(string? env) ? (src-cc $ root + "/out"))
       _ (? (two? amb) (setenv "CC" <>amb) 0)
       ours (src-ours src-needs)                ; what the box could not supply
       lane (? (string? env) ("CC=" + env + ", from the environment")
               (two? amb) (<>amb + " -- a FOREIGN compiler, so this is diverse double compiling")
               first? "this binary (-first: our own tools ahead of the system's)"
               self? "this binary (-self)"
               "this binary -- no working ambient cc")
       ; ⚠ SAY SO WHEN WE STOOD IN. Falling back is the point of carrying a toolchain, but a
       ; fixpoint checked entirely against ourselves is a weaker claim than one a foreign
       ; compiler helped make -- and a gate that quietly swapped the thing under test would
       ; be spending the user's trust without telling them. It still exits 0: the artifact
       ; is sound, only the claim is narrower.
       _ (? (two? ours)
          (say err $ ";; seed: default " + ujoin ours + "\n"))
       here (cwd 0)
       ; ⚠ charm?, never a truth test: chdir answers () on success and the ERRNO on
       ; failure, and an errno is negative -- so it is RED and a truth test reads the
       ; failure as the success. The kind is the only thing that tells them apart.
       _ (? (charm? (chdir root)) (src-die ("cannot enter " + root)) 0)
       _ (puts ("seed: " + show <>>r + " files in " + root
                + (? cross? ("\nseed: laying for " + want + " (this machine is "
                             + (seed-arch ()) + ")") "")
                + "\nseed: toolchain -- " + lane + "\n"))
       s (herald (? cross? [exe "make" ("xa=" + want) "dist_cross"] [exe "make" "dist"]))
       st (? (two? s) <s s)
       _ (chdir here)
       _ (? (0 = st) 0 (src-die ("the build failed (exit " + show st + ")")))
       out (root + "/out/dist/love-" + want)
       ; ⚠ THE FIXPOINT IS SCOPED, NOT DROPPED. It can hold only where the output OUGHT to be
       ; this binary, and a cross lay is the one case the INVOCATION itself says it cannot --
       ; so running it there would be a claim nothing could ever satisfy, which is a bug
       ; wearing a safeguard's face. (a dirty tree is the other case it cannot hold, and that
       ; one still FAILS: nobody asked for it, and the megabyte-scale mismatch is the report.)
       ; what stays checkable is that the artifact is the SHAPE asked for -- the ELF says
       ; which machine it is for, and a cross build quietly laying the host's is the failure
       ; this catches. no trust claim is made about it, and none is owed: the binary doing the
       ; cross build is the one `love seed` proves natively.
       (? cross?
        (: m (seed-elfmach out) em (seed-em want)
           (? (m = em)
              (: _ (puts ("seed: laid " + out + " for " + want + ", built by " + lane + "\n"
                          + "seed: no fixpoint -- the output is " + want
                          + ", this binary is " + (seed-arch ())
                          + ". run `love seed` on " + want + " to check it there.\n")) 0)
              (: _ (say err ("seed: WRONG ARCH -- " + out + " answers e_machine " + (show m)
                             + ", not " + want + "'s " + (show em) + "\n")) 1)))
        (: a (src-slurp exe)
           _ (atom? a ? (src-die $ "cannot read " + exe))
           ha (sha256 <>a)
           b (src-slurp out)
           _ (? (two? b) 0 (src-die ("cannot read " + out)))
           hb (sha256 <>b)
           (? (ha = hb)
          (: _ (puts ("seed: FIXPOINT -- " + out + "\nseed: is this binary, byte for byte ("
                      + (snip ha 0 16) + "..), built by " + lane + "\n")) 0)
          (: _ (say err ("seed: FIXPOINT FAILED -- what it built is NOT this binary\n"
                         + "seed:   running " + ha + "\n"
                         + "seed:   built   " + hb + "\n"))
             ; ⚠ A CHARM, NOT A QUIT. The verb table's contract is that a main ANSWERS its
             ; status (moon.l:752), and this one is a gate -- so a script wants to read the
             ; verdict, and the in-image lane would have a quit end the shell that asked.
             ; src-die still quits: those are setup failures with nothing to report on.
             1)))))

   ; ⚠ one function, two words: `src` is the short spelling and nothing more, so a
   ; reader meeting either finds the same row rather than a wrapper to chase.
   (pins (from 'verbs 'tab)
      'source source-main
      'src    source-main
      'seed   seed-main))
