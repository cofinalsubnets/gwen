; crew/kore/kore.l -- the multi-call dispatcher (busybox's trick): ONE binary, `kore`,
; shadowing nothing on the host, picking the util off the command line -- or off
; argv[0] through a tool-named symlink, which is how the distro will shadow at will.
;
;   kore diff OLD NEW    unified diff (patience hunks); exit 0 same / 1 differ / 2 trouble
;   kore nc HOST PORT    the netcat (tools/ain.l); nc -l PORT listens
;   kore make [F] [T..]  the build tool (crew/cook/cook.l)
;   kore as TGT IN OUT   assemble IR forms from IN (x64|arm64) into an ELF at OUT
;   kore objcopy -O binary|ihex IN OUT   a linked ELF flattened (crew/holo/copy.l)
;   kore nm [-gup] FILE..   an ELF's symbols, nm's three columns (crew/holo/link.l)
;   kore cat|head|tail|wc|sort|uniq|tee|echo|seq|yes|true|false|basename|dirname
;   kore cut|tr|nl|rev   the line + field tools + trivia (crew/kore/core.l)
;   kore ls|cp|mv|rm|mkdir|rmdir|ln|touch|pwd|chmod   the fs tools (crew/kore/fs.l)
;   kore grep [-nvcl] PATTERN [F..]   the BRE matcher (crew/kore/re.l)
;   kore sed [-n] SCRIPT [F..]        the stream editor (crew/kore/sed.l)
;   kore env|sleep|kill|xargs        the process tools (crew/kore/proc.l)
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

   ; ar: the classic "!<arch>" archive, GNU shape to the byte. `ar rcs OUT M..`
   ; writes OUT whole (fresh, like liblove.a's rm-first recipe; deterministic:
   ; date/uid/gid 0, mode 644 -- binutils' own -D default), WITH the ranlib
   ; index: the "/" member's big-endian offsets point at member headers, and
   ; each ELF member's DEFINED globals come through ld-read (crew/holo/link.l)
   ; -- no ranlib anywhere. member names store as BASENAMES (GNU's rule); a
   ; >15-char name rides the "//" long-name table (blank numeric fields, GNU's
   ; spelling). `ar t OUT` lists. (x/d verbs: add when a real script wants them.)
   (arp w s) (? (tally s < w) (arp w (s + " ")) s)            ; ar fields are LEFT-justified
   (arb4 v) (((("" + ((v >> 24) & 255)) + ((v >> 16) & 255)) + ((v >> 8) & 255)) + (v & 255))
   (ar-hdr nm date uid gid mode sz)
    ((((((arp 16 nm + arp 12 date) + arp 6 uid) + arp 6 gid) + arp 8 mode) + arp 10 (show sz)) + "`\n")
   (ar-pad s) (? (& (tally s) 1) (s + "\n") s)                ; members lie on even offsets
   (ar-elf? b) ((<= 20 (tally b)) &&
                (b 0 = 127) && (b 1 = 69) && (b 2 = 76) && (b 3 = 70))
   (ar-syms f b)                                              ; an ELF member's defined globals
    (? !(ar-elf? b) ()
       (: mach (b 18 + (256 * b 19))
          ob (ld-read f b mach)
          syms (peep ob 'syms 0) ns (peep ob 'nsym 0)
          (go i acc) (? (<= ns i) (rev acc)
                        (: sy (peep syms i ())
                           keep (two? sy && member? (at sy 1) '(1 2) && (0 < at sy 3))
                           (go (i + 1) (? keep (<sy >< acc) acc))))
          (go 1 ())))
   (ar-write dst ms)                                          ; ms: ((basename bytes)..)
    (: names (map (\ m <m) ms)
       datas (map (\ m <>m) ms)
       ; the long-name table: parallel to names, each entry (offset) | ()
       lofs (: (go l off acc) (? !(two? l) (rev acc)
                                 (: ln (tally <l)
                                    (? (< ln 16) (go >l off (() >< acc))
                                       (go >l ((off + ln) + 2) (`(off) >< acc)))))
               (go names 0 ()))
       ltab (: (go ns ls) (? !(two? ns) ""
                             (? (two? <ls) ((<ns + "/\n") + go >ns >ls) (go >ns >ls)))
               (go names lofs))
       syml (map (\ m (ar-syms <m <>m)) ms)                   ; per-member defined-global names
       nsym (foldl (+) 0 (map tally syml))
       strt (foldl (+) "" (map (\ ss (foldl (\ a s ((a + s) + 0)) "" ss)) syml))
       ; the index pads to even with a NUL that COUNTS in its size field (GNU's
       ; spelling -- members pad with "\n" excluded from theirs)
       isz (: z ((4 + (4 * nsym)) + tally strt) (z + (& z 1)))
       ; member header offsets: magic, index hdr + data, "//" (when any), then each
       m0 ((8 + 60) + isz)
       m1 (? (tally ltab) ((m0 + 60) + (tally ltab + (& (tally ltab) 1))) m0)
       offs (: (go ds off acc) (? !(two? ds) (rev acc)
                                  (go >ds ((off + 60) + (: z (tally <ds) (z + (& z 1))))
                                      (off >< acc)))
               (go datas m1 ()))
       idx (: raw ((arb4 nsym
                    + (: (go os ss) (? !(two? os) ""
                                       (foldl (\ a _ (a + arb4 <os)) "" <ss + go >os >ss))
                         (go offs syml)))
                   + strt)
              (? (& (tally raw) 1) (raw + 0) raw))
       body (: (go ns ls ds) (? !(two? ns) ""
                                (: nf (? (two? <ls) ("/" + show <<ls) (<ns + "/"))
                                   ((ar-hdr nf "0" "0" "0" "644" (tally <ds) + ar-pad <ds)
                                    + go >ns >ls >ds)))
               (go names lofs datas))
       out ((("!<arch>\n" + (ar-hdr "/" "0" "0" "0" "0" isz + idx))
             + (? (tally ltab) (ar-hdr "//" "" "" "" "" (tally ltab) + ar-pad ltab) ""))
            + body)
       q (open dst "w")
       _ (? q () (die ("ar: cannot write " + dst)))
       _ (say q out)
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
       (go a fs o pie org tgt)
        (? !(two? a) `((rev fs) o pie org tgt)
           (<a = "-o")     (? (two? >a) (go >>a fs <>a pie org tgt) (die "ld: -o wants a path"))
           (<a = "-pie")   (go >a fs o 1 org tgt)
           (<a = "-t")     (? (two? >a) (go >>a fs o pie org (intern <>a)) (die "ld: -t wants a target"))
           (<a = "-Ttext") (? (two? >a) (go >>a fs o pie (num <>a) tgt) (die "ld: -Ttext wants an address"))
           (go >a (<a >< fs) o pie org tgt))
       r (go as () () 0 () 'x64)
       fs <r o <>r pie <>>r org (at r 3) tgt (at r 4)
       _ (? (two? fs) () (die "usage: kore ld [-pie] [-t x64|arm64|riscv64] [-Ttext ADDR] A.o.. -o OUT"))
       _ (? (tally o) () (die "ld: no -o OUT"))
       srcs (map (\ f (: r2 (uread f) (? (two? r2) `(f <>r2) (die ("ld: cannot open " + f))))) fs)
       ps (ldlink tgt pie org srcs)
       _ (ld-write o ps)
       _ (chmod o 493)
       0)

   ; nm: the symbols a file names, in nm's three columns -- VALUE TYPE NAME, over
   ; crew/holo/link.l's ld-syms. the letter is ELF's own facts and never a guess: U
   ; undefined, A absolute, C common, else the defining section's flags pick T
   ; executable / B nobits / D writable / R the rest, and a LOCAL bind lowercases it
   ; -- that case IS nm's whole convention. -g keeps the externals, the only view a
   ; link has an opinion about; -u the undefined ones, which is what says a lane was
   ; BORROWED from libgcc rather than ours (tools/moon-parity.sh reads exactly that).
   ; ⚠ an ARCHIVE is not walked here: `ar t` lists members, and nm's per-member
   ; header shape wants the member walk ar-t owns. an ELF of any e_type is fine.
   ; the sort is by BYTE, which is `LC_ALL=C nm`'s order and not the bare `nm` a
   ; desk with a locale set answers -- a build tool reads no locale, so the order
   ; is the same on every machine.
   (nm-sp w) (? (w < 1) "" (nm-sp (w - 1) + " "))
   (nm-hex v w) (: dg "0123456789abcdef"
                   (go k a) (? (k < 0) a (go (k - 1) (a + dg ((v // (k 16)) % 16))))
                   (go (w - 1) ""))
   (nm-low t) (: c (t 0) (? ((65 <= c) && (c <= 90)) ("" + (c + 32)) t))
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
       ; ⚠ foldl, never map: map's side effects fire RIGHT TO LEFT (it conses on the
       ; way back up), so a map that prints hands the list back reversed
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
                 (die ("nm: unknown flag -" + ("" + c))))))
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
             'make cook-main 'cook cook-main   ; bare: cook.l precedes this file in the cat, bracket-free, so it folds here
             'as as-main
             'ar ar-main 'ld ld-main
             'objcopy objcopy         ; the applet IS the function: crew/holo/copy.l
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
             'install install-main 'readlink readlink-main
             'cmp cmp-main
             'grep grep-main 'sed sed-main
             'vi vi-main
             'env env-main 'sleep sleep-main
             'kill kill-main 'xargs xargs-main
             ; the shell: lush's parts precede this file in the cat, so sh-main
             ; folds here. `kore sh ..` and a /bin/sh symlink both land it (the
             ; symlink usually through lush's OWN seat, which fires first at
             ; cat load on an sh/lush basename -- this row serves the image
             ; wake, where only kore-main dispatches)
             'sh sh-main 'lush sh-main)
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
  grep [-nvcl] PAT [F..]                  the BRE matcher (crew/kore/re.l)
  sed [-n] SCRIPT [F..]                   the stream editor (s///gp, d, p, q, addresses)
  env sleep kill xargs                    the process tools (crew/kore/proc.l)
  vi [FILE]                               the editor (crew/vi/)
  sh [-c CMD | FILE]                      the shell, lush (crew/lush/)
  ls cp mv rm mkdir rmdir ln touch pwd chmod   the fs tools (crew/kore/fs.l)
  install readlink cmp                         ... the build's fs verbs
  echo seq yes true false basename dirname     ... and the trivia
")
                2)
   (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r n))
             (go 0 0))

   ; -- the dispatch (this form's body) --
   ; the program seat off the true command line (cmdline is never rebound, so the
   ; read is the same whether kore loaded positionally or as a -l preload): drop the
   ; -l pairs, and the first element left is the seat. its BASENAME picks the face
   ; -- a tool name (the symlink lane) runs that tool on the rest; anything else
   ; (kore itself, a dev path) reads the next word as the tool.
   (seat a) (? !(two? a) ()
               (<a = "-l") (? (two? >a) (seat >>a) ())
               a)
   ; me?: is this seat word kore itself, or a tool name (the symlink lane)? the
   ; dispatch fires only when it is -- so a `-l` preload that seats a flag (the bake
   ; loads the cat as `love -l .kore-cat.l -e ..`) stays quiet, like moon.l's me?.
   (me? w) (: b (bn w) ((b = "kore") || (b = "kore.l") || applets (intern b)))
   ; kore-main is the ONE leaked global, and the in-image door too: it dispatches a
   ; program-relative arg list -- argv0 a tool symlink, or `kore`/a dev path then the
   ; tool word -- and ANSWERS the status charm, () when kore is not seated (the bake
   ; stays quiet; an unknown tool is usage's 2, a status like any other). the baked
   ; kore.image wakes to it with a synthetic "kore" argv0, so a caller holding the
   ; image runs a tool by handing it (link "kore" "ls" "-l") and reading the charm.
   (\ a (: s (seat a)
           (? !(two? s) ()
              !(me? <s) ()
              (: f0 (applets (intern (bn <s)))
                 (? f0 (urun f0 >s)
                    (: t >s
                       f (? (two? t) (applets (intern <t)) ())
                       (? f (urun f >t) (usage ()))))))))))
; the SEAT: file mode (cat | love) dispatches on the real command line and quits with
; the status; the -l bake preload seats a flag, not kore, so me? is false and kore-main
; answers () -- the presence guard, and the one answer that is not a status.
(: r (kore-main >cmdline) (? (charm? r) (quit r) ()))
