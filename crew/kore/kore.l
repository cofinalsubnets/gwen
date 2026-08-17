; crew/kore/kore.l -- the multi-call dispatcher (busybox's trick): ONE binary, `kore`,
; shadowing nothing on the host, picking the util off the command line -- or off
; argv[0] through a tool-named symlink, which is how the distro will shadow at will.
;
;   kore diff OLD NEW    unified diff (patience hunks); exit 0 same / 1 differ / 2 trouble
;   kore nc HOST PORT    the netcat (mk/tools/ain.l); nc -l PORT listens
;   kore make [F] [T..]  the build tool (crew/cook/cook.l)
;   kore as TGT IN OUT   assemble IR forms from IN (x64|arm64) into an ELF at OUT
;   kore objcopy -O binary|ihex IN OUT   a linked ELF flattened (crew/holo/copy.l)
;   kore nm [-gup] FILE..   an ELF's symbols, nm's three columns (crew/holo/link.l)
;   kore cat|head|tail|wc|sort|uniq|tee|echo|seq|yes|true|false|basename|dirname
;   kore cut|tr|nl|rev|paste|comm|join|split|od   the line, field and record tools
;                                                 (crew/kore/core.l)
;   kore cksum|md5sum|sha256sum [-c] [F..]   the checksums (crew/kore/sum.l)
;   kore ls|cp|mv|rm|mkdir|rmdir|ln|touch|pwd|chmod   the fs tools (crew/kore/fs.l)
;   kore stat|du|chown|mktemp                        ... and what they report
;   kore expr EXPRESSION             the little language (crew/kore/expr.l)
;   kore patch [-pN] [-R] [-i F]     unified diffs laid on a tree (crew/kore/patch.l)
;   kore grep [-nvclqhaixwFEo] [-e P] [-m N] PATTERN [F..]   BRE/ERE (crew/kore/re.l)
;   kore sed [-nEi] [-e SCRIPT].. SCRIPT [F..]   the stream editor (crew/kore/sed.l)
;   kore awk [-F FS] [-v v=x].. 'PROG' [F..]     the language (crew/kore/awk.l)
;   kore find [PATH..] [EXPR]                    the tree walk (crew/kore/find.l)
;   kore env|sleep|kill|xargs|date|id   the process and world tools (crew/kore/proc.l)
;   kore vi [FILE]                   the editor (crew/vi/)
;   kore sh [-c CMD | FILE]          the shell, lush (crew/lush/; also `lush`, and an sh symlink)
; (the C compiler is NO LONGER here -- it split into its own app `mooncc`, doc/moon.md.)
;
; loads LAST in the kore cat, when every tool main is already defined; the tool files'
; own SEATs stay quiet here (no file of theirs sits in the program seat), so this
; dispatcher is the one thing that fires. ONE body-having `:` -- the registry, the
; thin mains that live here (diff over the engines, as over the holo book), and the
; dispatch as the body; nothing leaks, and every path ANSWERS a charm through core.l's
; urun, so an in-image caller lives through a tool that would have quit (the SEAT below
; is the one site that quits). ⚠ a main here leaves deep through `die`, never `quit`.
(module 'kore
(use 'holo) ; the assembler module: elf64, arbytes, ld-syms, at -- as/ar/ld/nm read it
(: kore-main (: (read-text p) (: q (open p "r") (? q (: t (slurp q) _ (close q) `(1 t)) ()))   ; (1 text) | ()
   (die m) (: _ (say err m) _ (put err 10) (uleave 2))

   ; diff: two files -> unified hunks on out; the classic exit triple
   (diff-main as)
    (? !(two? >as) (die "usage: kore diff OLD NEW")
       (: fa <as fb <>as
          ta (read-text fa) tb (read-text fb)
          _ (? (two? ta) () (die ("diff: cannot open " + fa)))
          _ (? (two? tb) () (die ("diff: cannot open " + fb)))
          d (udiff fa fb <>ta <>tb)
          (? (nil? d) 0
             (: _ (puts d) _ (flush out) 1))))

   ; as: IR forms -> a runnable ELF (crew/holo/elf.l over the baked holo book)
   (as-main as)
    (? !(two? >>as) (die "usage: kore as TARGET IN OUT   (TARGET: x64 | arm64)")
       (: tgt (intern <as) src <>as dst <>>as
          _ (? ((tgt = 'x64) || (tgt = 'amd64) || (tgt = 'arm64) || (tgt = 'aarch64)) ()
               (die ("as: unknown target " + <as)))
          q (open src "r")
          _ (? q () (die ("as: cannot open " + src)))
          (rd x acc) (: r (sound x)
                      (? (two? r) (rd >r (<r >< acc))
                         (id? r 'torn) (die ("as: unfinished form in " + src))
                         (rev acc)))
          ir (rd (slurp q) ())
          _ (? (two? ir) () (die ("as: no IR forms in " + src)))
          res (elf64 tgt (\ _ ir) ())
          _ (write-bytes dst (peep res 'bytes 0))
          _ (say err ("as: wrote " + dst)) _ (put err 10)
          0))

   ; ar: the classic "!<arch>" archive, GNU shape to the byte -- the bytes come
   ; from holo's `arbytes` (crew/holo/link.l), beside the reader that pulls them
   ; back, so mooncc's runtime archive and this one are the same writer.
   ; `ar rcs OUT M..` writes OUT whole (fresh, like liblove.a's rm-first recipe),
   ; member names stored as BASENAMES (GNU's rule). `ar t OUT` lists.
   ; (x/d verbs: add when a real script wants them.)
   (ar-write dst ms)                                          ; ms: ((basename bytes)..)
    (: q (open dst "w")
       _ (? q () (die ("ar: cannot write " + dst)))
       _ (say q (arbytes ms))
       (close q))
   (ar-t p)                                                   ; list member names
    (: r (uread p) _ (? (two? r) () (die ("ar: cannot open " + p)))
       b <>r n (tally b)
       _ (? ((<= 8 n) && (snip b 0 8 = "!<arch>\n")) () (die ("ar: not an archive: " + p)))
       (field s) (: (go k) (? ((0 < k) && (s (k - 1) = 32)) (go (k - 1)) (snip s 0 k)) (go (tally s)))
       (lname lt off) (: (go k) (? ((< k (tally lt)) && !(lt k = 10)) (go (k + 1)) k)
                         e (go off)
                         (: nm (snip lt off e) z (tally nm)
                            (? ((0 < z) && (nm (z - 1) = 47)) (snip nm 0 (z - 1)) nm)))
       (go pos lt) (? (< (n - pos) 60) 0
                      (: nm (field (snip b pos (pos + 16)))
                         sz (uatoi (field (snip b (pos + 48) (pos + 58))))
                         d0 (pos + 60)
                         nx ((d0 + sz) + (& sz 1))
                         (? (nm = "/") (go nx lt)
                            (nm = "//") (go nx (snip b d0 (d0 + sz)))
                            (: shown (? (nm 0 = 47) (lname lt (uatoi (snip nm 1 (tally nm))))
                                        (: z (tally nm) (? (nm (z - 1) = 47) (snip nm 0 (z - 1)) nm)))
                               _ (puts (shown + "\n"))
                               (go nx lt)))))
       _ (go 8 "")
       0)
   (ar-main as)
    (? !(two? as) (die "usage: kore ar rcs OUT MEMBER..   |   kore ar t OUT")
       (: fl <as
          (? (fl = "t") (? (two? >as) (ar-t <>as) (die "ar: t wants an archive"))
             (: hasr ((: (go i) (? (< i (tally fl)) (? (fl i = 114) 1 (go (i + 1))) 0)) 0)
                _ (? hasr () (die ("ar: unsupported command " + fl + " (rcs / t)")))
                _ (? (two? >as) () (die "ar: no archive named"))
                dst <>as
                fs >>as
                _ (? (two? fs) () (die "ar: no members"))
                ms (map (\ f (: r (uread f)
                                 (? (two? r) `((ubase f "") <>r) (die ("ar: cannot open " + f)))))
                        fs)
                _ (ar-write dst ms)
                0))))

   ; ld: holo's static linker as an applet -- mooncc .o (and any ELF relocatable
   ; crew/holo/link.l reads) into a static exe, no binutils anywhere; -pie the
   ; self-relocating ET_DYN, -Ttext the bare-metal org (qemu -kernel's lane).
   (ld-main as)
    (: (num s) (: n (tally s)
                  hex ((2 < n) && (s 0 = 48) && (s 1 = 120))
                  (go i a) (? (< i n)
                              (go (i + 1) ((a * (? hex 16 10))
                                           + (: b (s i)
                                                (? ((<= 48 b) && (< b 58)) (b - 48)
                                                   ((<= 97 b) && (< b 103)) (b - 87)
                                                   ((<= 65 b) && (< b 71)) (b - 55)
                                                   (die ("ld: bad number " + s))))))
                              a)
                  (go (? hex 2 0) 0))
       (go a fs o pie org tgt rd)
        (? !(two? a) `((rev fs) o pie org tgt rd)
           (<a = "-o")     (? (two? >a) (go >>a fs <>a pie org tgt rd) (die "ld: -o wants a path"))
           (<a = "-pie")   (go >a fs o 1 org tgt rd)
           (<a = "-t")     (? (two? >a) (go >>a fs o pie org (intern <>a) rd) (die "ld: -t wants a target"))
           (<a = "-Ttext") (? (two? >a) (go >>a fs o pie (num <>a) tgt rd) (die "ld: -Ttext wants an address"))
           ; --readme FILE: plain text into `.README`, for whoever opens the output
           (<a = "--readme") (? (two? >a)
                                (: t (uread <>a) (? (two? t) (go >>a fs o pie org tgt <>t)
                                                    (die ("ld: cannot open " + <>a))))
                                (die "ld: --readme wants a path"))
           (go >a (<a >< fs) o pie org tgt rd))
       r (go as () () 0 () 'x64 ())
       fs <r o <>r pie <>>r org (at r 3) tgt (at r 4) rdme (at r 5)
       _ (? (two? fs) () (die "usage: kore ld [-pie] [-t x64|arm64|riscv64] [-Ttext ADDR] [--readme F] A.o.. -o OUT"))
       _ (? (tally o) () (die "ld: no -o OUT"))
       srcs (map (\ f (: r2 (uread f) (? (two? r2) `(f <>r2) (die ("ld: cannot open " + f))))) fs)
       ps (ldlink tgt pie org srcs ("love " + <(usplit 43 love-version)) rdme)
       _ (ld-write o ps)
       _ (chmod o 493)
       0)

   ; nm: the symbols a file names, in nm's three columns -- VALUE TYPE NAME, over
   ; crew/holo/link.l's ld-syms. the letter is ELF's own facts and never a guess: U
   ; undefined, A absolute, C common, else the defining section's flags pick T
   ; executable / B nobits / D writable / R the rest, and a LOCAL bind lowercases it
   ; -- that case IS nm's whole convention. -g keeps the externals, the only view a
   ; link has an opinion about; -u the undefined ones, which is what says a lane was
   ; BORROWED from libgcc rather than ours (mk/tools/moon-parity.sh reads exactly that).
   ; ⚠ an ARCHIVE is not walked here: `ar t` lists members, and nm's per-member
   ; header shape wants the member walk ar-t owns. an ELF of any e_type is fine.
   ; the sort is by BYTE, which is `LC_ALL=C nm`'s order and not the bare `nm` a
   ; desk with a locale set answers -- a build tool reads no locale, so the order
   ; is the same on every machine.
   (nm-sp w) (? (w < 1) "" (nm-sp (w - 1) + " "))
   (nm-hex v w) (: dg "0123456789abcdef"
                   (go k a) (? (k < 0) a (go (k - 1) (a + string (dg ((v // (k 16)) % 16)))))
                   (go (w - 1) ""))
   (nm-low t) (: c (t 0) (? ((65 <= c) && (c <= 90)) (string (c + 32)) t))
   (nm-let sy)
    (: x (at sy 3) sec (at sy 6)
       u (? (x = 0) "U" (x = 65521) "A" (x = 65522) "C"
            !(two? sec) "?"
            (: ty (at sec 1) fl (at sec 2)
               (? (ty = 8) "B" (0 < (fl & 4)) "T" (0 < (fl & 1)) "D" "R")))
       (? (at sy 1 = 2) (? (x = 0) "w" "W")
          (at sy 1 = 0) (nm-low u)
          u))
   (nm-file p wide g u srt)
    (: r (uread p) _ (? (two? r) () (die ("nm: cannot open " + p)))
       b <>r
       w (? (b 4 = 2) 16 8)
       ; a SECTION or FILE symbol names no value of the program's -- nm shows
       ; neither, and an empty name is the null-ish filler a section symbol leaves
       all (filter (\ sy (? (nil? (tally <sy)) () (member? (at sy 2) '(3 4)) () 1))
                   (ld-syms p b))
       ; ⚠ "no symbols" is the FILE's fact, asked before the flags narrow: a -u that
       ; comes back empty says the lanes are all ours, which is an answer, not a lack
       _ (? (two? all) () (: _ (say err ("nm: " + p + ": no symbols")) _ (put err 10) 1))
       keep (filter (\ sy (? (g && (at sy 1 = 0)) () (u && !(at sy 3 = 0)) () 1)) all)
       ord (? srt keep (sortby (\ a b (<a <= <b)) keep))
       _ (? wide (: _ (puts ("\n" + p + ":\n")) 0) 0)
       ; foldl is the printing walk: it answers the accumulator, so nothing is built
       _ (foldl (\ _ sy (puts ((? (at sy 3 = 0) (nm-sp w) (nm-hex (at sy 4) w))
                               + " " + nm-let sy + " " + <sy + "\n")))
                0 ord)
       (two? all))
   (nm-main as)
    ; flags CLUSTER: -gu is -g -u, which is what a hand types and what nm takes
    (: (nm-flags t k g u srt)
        (? (tally t <= k) `(g u srt)
           (: c (t k)
              (? (c = 103) (nm-flags t (k + 1) 1 u srt)
                 (c = 117) (nm-flags t (k + 1) g 1 srt)
                 (c = 112) (nm-flags t (k + 1) g u 1)
                 (die ("nm: unknown flag -" + (string c))))))
       (go a g u srt fs)
        (? !(two? a) `(g u srt (rev fs))
           ((1 < tally <a) && (<a 0 = 45))
            (: f (nm-flags <a 1 g u srt) (go >a <f <>f <>>f fs))
           (go >a g u srt (<a >< fs)))
       r (go as () () () ())
       fs (at r 3)
       _ (? (two? fs) () (die "usage: kore nm [-g] [-u] [-p] FILE.."))
       wide (two? >fs)
       any (foldl (\ a p (? (nm-file p wide <r <>r <>>r) 1 a)) () fs)
       _ (flush out)
       (? any 0 1))

   ; the registry: name -> main (aliases share)
   applets #('diff diff-main
             'nm nm-main
             'nc ain-main 'ain ain-main
             'make (from 'cook 'cook-main) 'cook (from 'cook 'cook-main)   ; cook.l's wrap registered ahead in the cat; the map ctor evaluates values
             'as as-main
             'ar ar-main 'ld ld-main
             'objcopy (from 'holo 'objcopy)   ; the applet IS the function: crew/holo/copy.l, module-side
             'cat cat-main 'echo echo-main
             'head head-main 'tail tail-main
             'wc wc-main 'sort sort-main
             'uniq uniq-main 'tee tee-main
             'seq seq-main 'yes yes-main
             'true true-main 'false false-main
             ; ⚠ test/[ are PROGRAMS here, not just shell builtins: cook's in-image lane
             ; execs a recipe's words directly, so a `test -s $t && mv ..` looks them up
             ; on PATH. uname and printf are the other two a bootstrap cannot start without.
             ; ⚠ `[` cannot be QUOTED -- it opens a bracket in the reader -- so the nom
             ; is interned by name; the map ctor evaluates its keys, which is the door.
             'test test-main (name "[") bracket-main
             'uname uname-main 'printf printf-main
             'basename basename-main 'dirname dirname-main
             'cut cut-main 'tr tr-main
             'nl nl-main 'rev rev-main
             'paste paste-main 'comm comm-main 'split split-main
             'join join-main 'od od-main
             'cksum cksum-main 'md5sum md5sum-main 'sha256sum sha256sum-main
             'ls ls-main 'cp cp-main 'mv mv-main
             'rm rm-main 'mkdir mkdir-main 'rmdir rmdir-main
             'ln ln-main 'touch touch-main
             'pwd pwd-main 'chmod chmod-main
             'install install-main 'readlink readlink-main
             'cmp cmp-main
             'stat stat-main 'du du-main 'chown chown-main 'mktemp mktemp-main
             'expr expr-main 'date date-main 'id id-main
             'patch patch-main
             'grep grep-main 'sed sed-main
             'awk awk-main 'find find-main
             'vi (from 'vi 'vi-main)   ; the editor's wrap registered ahead in the cat
             'env env-main 'sleep sleep-main
             'kill kill-main 'xargs xargs-main
             ; the shell: lush's wrap registered ahead of this file in the cat, so sh-main
             ; folds here. `kore sh ..` and a /bin/sh symlink both land it (the
             ; symlink usually through lush's OWN seat, which fires first at
             ; cat load on an sh/lush basename -- this row serves the image
             ; wake, where only kore-main dispatches)
             'sh (from 'lush 'sh-main) 'lush (from 'lush 'sh-main))
   (usage _) (: _ (say err "kore -- the love multi-call toolbox
usage: kore TOOL ARGS..     (or symlink a tool's name to kore)
  diff OLD NEW       unified diff, patience hunks (0 same / 1 differ / 2 trouble)
  nc HOST PORT       netcat; nc -l PORT listens
  make [FILE] [T..]  the build tool (Cards.l / Cookfile / Makefile)
  as TGT IN OUT      assemble IR forms from IN (x64|arm64) into ELF OUT
  ar rcs OUT M..     the archive, GNU shape + ranlib index (ar t OUT lists)
  ld A.o.. -o OUT    holo's static linker (-pie, -t x64|arm64|riscv64, -Ttext)
  objcopy -O F IN OUT   flatten a linked ELF: F is binary or ihex (Intel HEX)
  nm [-gup] FILE..   an ELF's symbols; -g externals, -u undefined, -p unsorted
  cat head tail wc sort uniq tee          the line tools (crew/kore/core.l)
  cut tr nl rev                           the field tools
  paste comm join split od                the record tools
  cksum | md5sum [-c] | sha256sum [-c]    the checksums (crew/kore/sum.l)
  expr EXPRESSION                         arithmetic, comparison and BRE matching
  patch [-pN] [-R] [-i F] [--dry-run]     lay a unified diff on a tree
  grep [-nvclqhaixwFEo] [-e P] [-m N] PAT [F..]   BRE, or ERE under -E (crew/kore/re.l)
  sed [-nEi] [-e SCRIPT].. SCRIPT [F..]   the stream editor (s///gp, d, p, q, addresses)
  awk [-F FS] [-v var=val].. 'PROG' [F..]  patterns and actions over records
  awk [-F FS] -f PROGFILE.. [F..]          ... the program off a file instead
  find [PATH..] [EXPR]   -name -path -type f|d|l -print -prune -exec CMD.. \\;
                         ( ) ! -a -o -maxdepth N -mindepth N   (PATH defaults to .)
  env sleep kill xargs                    the process tools (crew/kore/proc.l)
  date [-u] [-d @SEC] [-r F] [+FMT]       the clock, UTC (no tz database here)
  id [-u|-g|-G] [-n]                      who this process is
  vi [FILE]                               the editor (crew/vi/)
  sh [-c CMD | FILE]                      the shell, lush (crew/lush/)
  ls cp mv rm mkdir rmdir ln touch pwd chmod   the fs tools (crew/kore/fs.l)
  install readlink cmp                         ... the build's fs verbs
  stat -c FMT | du [-sabhc] | chown | mktemp   ... and what they report
  echo seq yes true false basename dirname     ... and the trivia
")
                2)
   (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
             (go 0 0))

   ; -- the dispatch (this form's body) --
   ; the SPELLINGS kore answers to: itself, its file, and every applet -- the symlink
   ; lane is just an applet name in the program seat, so the table IS the list. the
   ; scan is love/verbs.l's `seatof` (over the list we are handed, never a command
   ; line: the in-image door dispatches (link "kore" "ls" "-l")), which drops the -l
   ; pairs and answers (SEAT-WORD . rest), () when the seat is someone else's -- so a
   ; bake loading the cat as `love -l .kore-cat.l -e ..` stays quiet.
   ; the seat word's BASENAME picks the face: a tool name runs that tool on the rest;
   ; anything else (kore itself, a dev path) reads the next word as the tool.
   mynames ("kore" >< "kore.l" >< map string (keys applets))
   ; the VERBS: kore's applet table IS a verb collection, and the registry is where one
   ; goes. every row lands twice over -- `love ls -l` (the word door) and an `ls` symlink
   ; onto the artifact (the seat door) -- while `love kore ls` still nests through the
   ; dispatch below. pinned HERE rather than by whoever cats the file, so every image
   ; carrying this cat carries the names: the dist artifact, and the kernel, whose
   ; userland this IS.
   ; ⚠ each row closes over ITS OWN f -- the inner `:` is a fresh scope per step, so the
   ; closures do not all end up holding the last applet.
   _ (: T (from 'verbs 'tab)
        ; ⚠ kore-main is UNBOUND here -- the outer form binds it only after this
        ; whole form answers -- so the closure reads it through the registry at
        ; the call, when module 'kore holds it. a bare read compiled here would
        ; ride a missing cell against the CALLER's walk, where kore never walks.
        _ (pin T 'kore (\ as ((from 'kore 'kore-main) ("kore" >< as))))
        (go ks) (? (two? ks) (: f (applets <ks) _ (pin T <ks (\ as (urun f as))) (go >ks)) 0)
        (go (keys applets)))
   ; kore-main is the ONE leaked global, and the in-image door too: it dispatches a
   ; program-relative arg list -- argv0 a tool symlink, or `kore`/a dev path then the
   ; tool word -- and ANSWERS the status charm, () when kore is not seated (the bake
   ; stays quiet; an unknown tool is usage's 2, a status like any other). the baked
   ; kore.image wakes to it with a synthetic "kore" argv0, so a caller holding the
   ; image runs a tool by handing it (link "kore" "ls" "-l") and reading the charm.
   ; ⚠ two doors: a list handed in is SCANNED, () means "the line I was started with".
   (\ a (: s (? (two? a) ((from 'verbs 'scan) mynames a) ((from 'verbs 'fire) mynames))
           (? !(two? s) ()
              (: f0 (applets (intern (bn <s)))
                 (? f0 (urun f0 >s)
                    (: t >s
                       f (? (two? t) (applets (intern <t)) ())
                       (? f (urun f >t) (usage ()))))))))))
; the SEAT: file mode (cat | love) dispatches on the real command line and quits with
; the status. the -l bake preload seats a flag, not kore, so the scan answers () and
; kore-main answers () -- the presence guard, and the one answer that is not a status.
(: r (kore-main ()) (? (charm? r) (quit r) ()))
) ; module 'kore
