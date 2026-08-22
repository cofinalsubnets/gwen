; tools/ccdb.l -- emit compile_commands.json, the compilation database clangd wants.
;
;   love tools/ccdb.l              (from the tree root)
;
; clangd runs its fallback command from THE FILE'S OWN DIRECTORY, so a relative -I in
; .clangd resolves against host/ or free/ and love.h is not found. that miss is FATAL --
; the parse stops and every name after it reports undeclared, a flood that says nothing
; about the code. a database carries an absolute directory per entry, so the paths land
; where they mean.
;
; the flag groups mirror the real builds: host/build.mk (hcc) and free/kernel.mk
; (kcflags + kcppflags). they are few and they are stable; when one moves, move it here.
;
; THE TREE IS THE SOURCE: the same pruned walk selfpack cuts the archive with finds the
; .c files, so nothing here asks a version control system what the tree holds. what it
; writes carries absolute paths, so it is this machine's and rides in no archive.
(use 'tar)
(use 'json)

(: root (cwd 0)
   (inc p) ("-I" + root + p)
   (pre? s p) ((snip s 0 (tally p)) = p)
   (dot-c? nom) (: n (tally nom) (2 < n && (snip nom (n - 2) n) = ".c"))

   ; the host lane: love.c, host/, and the crew's C.
   ; ⚠ /core carries love.h and every lane names it bare, so a group that drops it
   ; parses nothing: the miss is fatal and the flood after it says nothing.
   host ["-std=gnu23" "-Dai_tco=1"
         (inc "") (inc "/core") (inc "/out/lib") (inc "/out/host") (inc "/crew/quay")]
   ; the freestanding kernel: its own libc, its own headers, nothing hosted.
   inle (["-std=gnu23" "-Dai_tco=1" "-ffreestanding" "-nostdinc"
          "-DK_TEST"
          (inc "") (inc "/core") (inc "/out/lib") (inc "/out/host") (inc "/crew/quay")]
         + [(inc "/free") (inc "/crew/moon/include")])
   ; moon's replacement libc -- compiled against its OWN headers, so glibc's
   ; declarations would conflict with the real ones.
   moonlibc ["-std=gnu23" "-ffreestanding" "-nostdinc" (inc "/crew/moon/include") (inc "")]
   ; the playdate workbench takes its lcat'd cas.h out of out/playdate.
   playdate (host + ["-DTARGET_PLAYDATE=1" "-DTARGET_EXTENSION=1"
                     (inc "/port/playdate") (inc "/out/playdate")])

   ; two files reach for a VENDOR SDK outside the tree. point the database at it when the
   ; environment names one; otherwise the file stays out entirely and .clangd quiets it,
   ; since no flag set here could resolve the include.
   pd-sdk (getenv "PLAYDATE_SDK_PATH")
   emsdk (getenv "EMSDK")

   (flags-for rel)
    (? (pre? rel "crew/moon/lib/") moonlibc
       ; asmops.h lives once per arch under the same name, so the arch dir is named
       ; here and never both at once.
       (pre? rel "free/aarch64/") (inle + [(inc "/free/aarch64") "--target=aarch64-unknown-none-elf"])
       (pre? rel "free/") (inle + [(inc "/free/x86_64")])
       ; the asm seam probe reads free/<a>/asmops.h from OUTSIDE free/, deliberately:
       ; kernel.mk globs that directory and a file there would join the kernel build.
       (rel = "test/gate/asmops.c") (host + [(inc "/free/x86_64")])
       (pre? rel "port/playdate/") (? (tally pd-sdk) (playdate + [("-I" + pd-sdk + "/C_API")]) playdate)
       (pre? rel "wasm/") (host + [("-I" + emsdk + "/upstream/emscripten/system/include")])
       host)

   ; rp2040 is a thumb1 cross target and this database models the host and freestanding
   ; lanes only, so its flags would be the wrong ones; .clangd quiets the path to match.
   (skip? rel)
    (pre? rel "port/rp2040/"
     || (rel = "port/playdate/pdglue.c" && !(tally pd-sdk))
     || ((pre? rel "wasm/") && !(tally emsdk)))

   ; the walk prunes what is not source before it descends -- out/ and dl/ are gigabytes
   ; on a build box, and everything hidden at the root is the machine's.
   skip-top '("out" "bin" "dl")
   (top nom) (: n (tally nom)
                (go i) (? (n <= i) nom (nom i = 47) (snip nom 0 i) (go (i + 1)))
                (go 0))
   (keep? nom) (: seg (top nom)
                  (! (member? seg skip-top || (tally seg && (peep seg 0 0 = 46)))))

   (entry rel)
    #("directory" root
      "file" (root + "/" + rel)
      "arguments" (["/usr/bin/clang" "-fsyntax-only"] + (flags-for rel) + [rel]))

   (: t (tar-thin? keep? "." "")
      _ (? t 0 (: _ (say err "ccdb: cannot read the tree\n") (quit 1)))
      noms (map (\ e (tar-get e 'nom "")) <>t)
      rels (filter (\ n (dot-c? n && !(skip? n))) noms)
      db (map entry rels)
      out-nom (root + "/compile_commands.json")
      q (open out-nom "w")
      _ (? q 0 (: _ (say err ("ccdb: cannot write " + out-nom + "\n")) (quit 1)))
      _ (say q (json-show db + "\n"))
      _ (close q)
      _ (say out (out-nom + ": " + show (tally db) + " entries\n"))
      0))
