; crew/kore/kore.l -- the multi-call dispatcher (busybox's trick): ONE binary, `kore`,
; shadowing nothing on the host, picking the util off the command line -- or off
; argv[0] through a tool-named symlink, which is how the distro will shadow at will.
;
;   kore diff OLD NEW    unified diff (patience hunks); exit 0 same / 1 differ / 2 trouble
;   kore nc HOST PORT    the netcat (tools/ain.l); nc -l PORT listens
;   kore make [F] [T..]  the build tool (crew/cook/cook.l)
;   kore as TGT IN OUT   assemble IR forms from IN (x64|arm64) into an ELF at OUT
;   kore cat|head|tail|wc|sort|uniq|tee|echo|seq|yes|true|false|basename|dirname
;   kore cut|tr|nl|rev   the line + field tools + trivia (crew/kore/core.l)
;   kore ls|cp|mv|rm|mkdir|rmdir|ln|touch|pwd|chmod   the fs tools (crew/kore/fs.l)
;   kore grep [-nvcl] PATTERN [F..]   the BRE matcher (crew/kore/re.l)
;   kore sed [-n] SCRIPT [F..]        the stream editor (crew/kore/sed.l)
;   kore env|sleep|kill|xargs        the process tools (crew/kore/proc.l)
;   kore vi [FILE]                   the editor (crew/vi/)
; (the C compiler is NO LONGER here -- it split into its own app `mooncc`, doc/moon.md.)
;
; loads LAST in the kore cat, when every tool main is already defined; the tool files'
; own SEATs stay quiet here (no file of theirs sits in the program seat), so this
; dispatcher is the one thing that fires. ONE body-having `:` -- the registry, the
; thin mains that live here (diff over the engines, as over the holo book), and the
; dispatch as the body; nothing leaks, and every path ends in a quit (the exit door).
(: kore-main (: (read-text p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))   ; (1 text) | ()
   (die m) (: _ (say err m) _ (put err 10) (quit 2))

   ; diff: two files -> unified hunks on out; the classic exit triple
   (diff-main as)
    (? (! (two? >as)) (die "usage: kore diff OLD NEW")
       (: fa <as fb <>as
          ta (read-text fa) tb (read-text fb)
          _ (? (two? ta) () (die ("diff: cannot open " + fa)))
          _ (? (two? tb) () (die ("diff: cannot open " + fb)))
          d (udiff fa fb <>ta <>tb)
          (? (nil? d) (quit 0)
             (: _ (puts d) _ (flush out) (quit 1)))))

   ; as: IR forms -> a runnable ELF (crew/holo/elf.l over the baked holo book)
   (as-main as)
    (? (! (two? >>as)) (die "usage: kore as TARGET IN OUT   (TARGET: x64 | arm64)")
       (: tgt (intern <as) src <>as dst <>>as
          _ (? (|| (tgt = 'x64) (|| (tgt = 'amd64) (|| (tgt = 'arm64) (tgt = 'aarch64)))) ()
               (die ("as: unknown target " + <as)))
          q (open src "r")
          _ (? q () (die ("as: cannot open " + src)))
          e (nom 0)
          (rd acc) (: r (sound q e)
                      (? (id? r e) (rev acc)
                         (id? r q) (die ("as: unfinished form in " + src))
                         (rd (link r acc))))
          ir (rd ())
          _ (? (two? ir) () (die ("as: no IR forms in " + src)))
          res (elf64 tgt (\ _ ir) ())
          _ (write-bytes dst (peep res 'bytes 0))
          _ (say err ("as: wrote " + dst)) _ (put err 10)
          (quit 0)))

   ; the registry: name -> main (aliases share)
   applets #('diff diff-main
             'nc ain-main 'ain ain-main
             'make cook-main 'cook cook-main   ; bare: cook.l precedes this file in the cat, bracket-free, so it folds here
             'as as-main
             'cat cat-main 'echo echo-main
             'head head-main 'tail tail-main
             'wc wc-main 'sort sort-main
             'uniq uniq-main 'tee tee-main
             'seq seq-main 'yes yes-main
             'true true-main 'false false-main
             'basename basename-main 'dirname dirname-main
             'cut cut-main 'tr tr-main
             'nl nl-main 'rev rev-main
             'ls ls-main 'cp cp-main 'mv mv-main
             'rm rm-main 'mkdir mkdir-main 'rmdir rmdir-main
             'ln ln-main 'touch touch-main
             'pwd pwd-main 'chmod chmod-main
             'grep grep-main 'sed sed-main
             'vi vi-main
             'env env-main 'sleep sleep-main
             'kill kill-main 'xargs xargs-main)
   (usage _) (: _ (say err "kore -- the love multi-call toolbox
usage: kore TOOL ARGS..     (or symlink a tool's name to kore)
  diff OLD NEW       unified diff, patience hunks (0 same / 1 differ / 2 trouble)
  nc HOST PORT       netcat; nc -l PORT listens
  make [FILE] [T..]  the build tool (Cards.l / Cookfile / Makefile)
  as TGT IN OUT      assemble IR forms from IN (x64|arm64) into ELF OUT
  cat head tail wc sort uniq tee          the line tools (crew/kore/core.l)
  cut tr nl rev                           the field tools
  grep [-nvcl] PAT [F..]                  the BRE matcher (crew/kore/re.l)
  sed [-n] SCRIPT [F..]                   the stream editor (s///gp, d, p, q, addresses)
  env sleep kill xargs                    the process tools (crew/kore/proc.l)
  vi [FILE]                               the editor (crew/vi/)
  ls cp mv rm mkdir rmdir ln touch pwd chmod   the fs tools (crew/kore/fs.l)
  echo seq yes true false basename dirname     ... and the trivia
")
                (quit 2))
   (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))

   ; -- the dispatch (this form's body) --
   ; the program seat off the true command line (cmdline is never rebound, so the
   ; read is the same whether kore loaded positionally or as a -l preload): drop the
   ; -l pairs, and the first element left is the seat. its BASENAME picks the face
   ; -- a tool name (the symlink lane) runs that tool on the rest; anything else
   ; (kore itself, a dev path) reads the next word as the tool.
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a) (seat >>a) ())
               a)
   ; me?: is this seat word kore itself, or a tool name (the symlink lane)? the
   ; dispatch fires only when it is -- so a `-l` preload that seats a flag (the bake
   ; loads the cat as `love -l .kore-cat.l -e ..`) stays quiet, like moon.l's me?.
   (me? w) (: b (bn w) (|| (b = "kore") (|| (b = "kore.l") (applets (intern b)))))
   ; kore-main is the ONE leaked global: the baked kore.image wakes to it
   ; (--wake kore.image -e '(kore-main (link "kore" (cuup (cup cmdline))))' -- the
   ; synthetic "kore" argv0 makes an unknown tool usage+quit like the real cli). it
   ; dispatches a program-relative arg list -- argv0 a tool symlink, or `kore`/a dev
   ; path then the tool word -- and stays QUIET when kore is not seated (the bake).
   (\ a (: s (seat a)
           (? (! (two? s)) ()
              (! (me? <s)) ()
              (: f0 (applets (intern (bn <s)))
                 _ (? f0 (: _ (f0 >s) (quit 0)) ())
                 t >s
                 f (? (two? t) (applets (intern <t)) ())
                 _ (? f (: _ (f >t) (quit 0)) ())
                 (usage ())))))))
; the SEAT: file mode (cat | love) dispatches on the real command line; the -l bake
; preload seats a flag, not kore, so me? is false and kore-main returns quietly.
(kore-main >cmdline)
