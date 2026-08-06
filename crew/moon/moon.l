; crew/moon/moon.l -- the C compiler's driver:
;   mooncc [-c] [-I dir] [-D name[=val]] [-o out] in.c|in.o|lib.a ..
; reads C, rides lex -> cpp -> parse -> gen -> the holo book. TWO outcomes, the
; cc shape: WITH -c it lays relocatable ELF objects (crew/holo/obj.l --
; separate sections, a symbol table, and relocations), several .c inputs each
; landing in the cwd as x.o (gcc-shaped); WITHOUT it every input (a .c compiled
; on the way, a .o read off disk) goes to our own static linker
; (crew/holo/link.l, rung 3), which binds them with crt0 and the implicit
; runtime into -o's exe, `a.out` bare. the old positional
; pair `mooncc [-c] IN OUT` still reads (two bare args, no .c and -- linking --
; no .o). -I dirs search before the system pair on both include forms
; (-nostdinc drops /usr/include off the tail, so only our own headers answer); a -D
; rides in as a synthesized #define line, so token bodies and function-like
; -DF(x)=.. take the one macro path. the driver wears the cc conventions: the
; advisory flag families (-W.. -O.. -g.. -std= -f.. -pipe -static) ride through
; ignored so `CC=mooncc` drives a gcc-shaped recipe unchanged, and a link owing
; libc symbols pulls the runtime (nolibc + am math + sys) by need from the
; sources beside it -- -nostdlib/-ffreestanding turn that off. errors speak on
; err and exit 1; usage exits 2. LAST in the mooncc cat (its own app, doc/moon.md), after
; crew/holo/{elf,obj,link}.l and crew/moon/{lex,cpp,parse,gen}.l; rides the
; u-floor (uread udie). its tail SEAT fires moon-main when mooncc is the program.
(:
   ; the directory portion of a path (up to and including the last '/'), or ""
   (cdir path) (: n (tally path)
                  (go i last) (? (n <= i) last ((path i) = 47) (go (i + 1) (i + 1)) (go (i + 1) last))
                  d (go 0 0)
                  (? (0 < d) (snip path 0 d) ""))
   ; THE TOOLCHAIN ROOT -- where our own headers and the runtime sources live.
   ; two rungs, tried in order: the dev tree off the cwd, then the INSTALLED
   ; nest through the seat (love/prel.l's own walk -- readlink /proc/self/exe,
   ; then <seat>/../lib/love/moon/, so ~/.love/bin/love finds ~/.love/lib/love/
   ; moon/ and a distro's /usr/bin/love finds /usr/lib/love/moon/). without the
   ; second rung mooncc only works with the repo root as the cwd: every path
   ; here was cwd-relative, so an installed mooncc fell through to /usr/include
   ; and could not even preprocess a hello-world (glibc's stdio.h wants the
   ; compiler's own stddef.h).
   ; ⚠ READ AT EACH CALL, never bound: mooncc rides a BAKED image, and a
   ; captured seat would fold the BUILD tree's path into that image and ride it
   ; forever -- prel.l carries the same warning because it learned this one the
   ; hard way (the installed love walked out/lib instead of its own nest).
   (mseat _) (: x (readlink "/proc/self/exe")
                (? (string? x) ((cdir x) + "../lib/love/moon/") ()))
   (mroots _) (: s (mseat ()) (? (string? s) `("crew/moon/" s) `("crew/moon/")))
   ; the runtime home: the FIRST root that actually carries the sources. by
   ; READ, not by a name test -- presence is the wrapper (uread answers
   ; (1 text), an absent file the zero point), and an empty nolibc.c would be
   ; no runtime either.
   (mhome _) (: (go rs) (? (! (two? rs)) ()
                           (two? (uread (<rs + "lib/nolibc.c"))) <rs
                           (go >rs))
                (go (mroots ())))
   ; the first three of a name list, comma-joined, with a count of the rest --
   ; a diagnostic wants a taste, not the whole bag
   (mfew ns)
    (: n (tally ns)
       (go l k acc) (? (|| (! (two? l)) (3 <= k)) acc
                       (go >l (k + 1) (? (0 < k) (acc + ", " + <l) <l)))
       s (go ns 0 "")
       (? (3 < n) (s + " (+" + (show (n - 3)) + " more)") s))
   ; #include resolver bound to a source dir + the -I list: "" searches relative
   ; first (the src dir, then cwd) then the -I dirs; <> the -I dirs straight off;
   ; both fall to the system pair, our own headers before /usr. answers
   ; (1 text child) -- the child hook resolves the hit file's OWN quote
   ; includes beside it (gcc's rule; cpp.l threads it). -> that | ().
   (incload nsi is srcdir name sys?)
    (: ours ((map (\ r (r + "include/")) (mroots ()))                        ; our headers (stdarg.h ..) win for <>
             + (? nsi () `("/usr/include/")))                                ; -nostdinc: no glibc tail at all
       dirs (? sys? (is + ours)
                (`(srcdir "") + is + ours))
       (go ds) (? (! (two? ds)) ()
                  (: r (uread (<ds + name))
                     (? (two? r)
                        (: hd (cdir (<ds + name))
                           `(1 <>r (\ n2 s2 (incload nsi is hd n2 s2))))
                        (go >ds))))
       (go dirs))
   (el l i) (? (i < 1) <l (el >l (i - 1)))
   ; a -D definition as #define text: the first = becomes the space, so
   ; -DX -> "X" (defined 1 below), -DX=v -> "X v", -DF(x)=x -> "F(x) x"
   ; (the glued ( survives the lex, so it stays function-like).
   (eqsp d) (: n (tally d)
               (go i) (? (n <= i) (d + " 1")
                         ((d i) = 61) ((snip d 0 i) + " " + (snip d (i + 1) n))
                         (go (i + 1)))
               (go 0))
   (deftext ds) (foldl (\ a d (a + "#define " + (eqsp d) + "\n")) "" ds)
   ; the target: 'x64 (the default), 'arm64, or 'thumb2 (ARMv7E-M / Cortex-M7),
   ; picked by -t. the arch predefine (__x86_64__ / __aarch64__ / __arm__) rides
   ; the -D channel so cpp stays target-blind.
   (totgt s) (? (|| (s = "x64") (s = "amd64")) 'x64
                (|| (s = "arm64") (s = "aarch64")) 'arm64
                (|| (s = "thumb2") (s = "cortex-m7")) 'thumb2
                (|| (s = "thumb1") (s = "cortex-m0")) 'thumb1   ; ARMv6-M (RP2040 / Cortex-M0+)
                (|| (s = "thumb2sp") (s = "playdate")) 'thumb2sp   ; ARMv7E-M with an SP-only FPU (STM32F746): f64 softens to __aeabi_*
                (|| (s = "riscv64") (|| (s = "riscv") (s = "rv64"))) 'riscv64   ; RV64IMFD LP64 (the pine64 ox64 / qemu-virt)
                (udie 2 ("cc: unknown target " + s + " (x64 | arm64 | thumb2 | thumb2sp | thumb1 | riscv64)")))
   (archdef tgt) (? (id? tgt 'arm64) "__aarch64__" (member? tgt '(thumb2 thumb2sp thumb1)) "__arm__"
                    (id? tgt 'riscv64) "__riscv" "__x86_64__")
   ; the diagnostic driver (doc/moon-diag.md): a stage answered a gripe (a cause
   ; it named itself) or a bare bad. a gripe prints "cc: <file>:<line>:<col>: msg"
   ; -- an unknown file takes the source path, an unknown line/col elides; anything
   ; else falls back to <plain>, so a not-yet-converted stage keeps today's line.
   ; either way a fatal exit 1.
   (ccdie val src plain)
    (? (gripe? val)
       (: file (: f (el val 1) (? f f src))
          ln (el val 2) col (el val 3) msg (el val 4)
          loc (file + (? ln (":" + (show ln)) "") + (? col (":" + (show col)) ""))
          (udie 1 ("cc: " + loc + ": " + msg)))
       (udie 1 plain))
   ; the -D lines are prepended TEXT (below), so every line the lexer stamped on
   ; a TU token sits that many lines high. take them back off before reporting.
   ; a gripe naming a FILE came from an #include, which lexed fresh from its own
   ; line 1 and never saw the defines -- leave it alone. ⚠ any stage that learns
   ; to mint a gripe has to come through here, or it reports skewed lines.
   (deskew val n)
    (? (! (gripe? val)) val
       (: f (el val 1) ln (el val 2)
          (? f val
             (! (charm? ln)) val
             (gripe f (ln - n) (el val 3) (el val 4)))))
   ; the shared front end: source path (+ -I dirs, -D defines) -> (1 ast stag
   ; sigs ..) | a fatal exit. the -D lines PREPEND to the source text before the
   ; one lex, so the line numbering stays whole (tokens carry lines and a define
   ; body runs to its line's end -- a separately-lexed stream would restart at 1
   ; and the last -D would swallow the file's first line); deskew pays the
   ; numbering back at the report.
   (cc-parse src is ds tgt nsi)
    (: ds9 (link (archdef tgt)             ; ds9, not ds: a :-local shadows the PARAM over the whole scope (the boxfix cell trap)
                 (? (id? tgt 'x64) (link "__SIZEOF_INT128__=16" ds) ds))   ; x64 alone carries the wide pair (gen's d128 lane); love.c's limb seam reads it
       r (uread src)
       _ (? (two? r) () (udie 1 ("cc: cannot open " + src)))
       lx (clex ((deftext ds9) + <>r))
       _ (? (okv? lx) () (ccdie (deskew lx (tally ds9)) src ("cc: lex error in " + src)))
       pp (cpp <>lx (\ name sys? (incload nsi is (cdir src) name sys?)) src)
       _ (? (okv? pp) () (ccdie (deskew pp (tally ds9)) src ("cc: preprocessor error in " + src)))
       ps (cparse-t <>pp tgt)                 ; thread the target so parse sizes pointers/longs at the ABI width
       _ (? (okv? ps) () (ccdie (deskew ps (tally ds9)) src ("cc: parse error in " + src)))
       ps)
   ; the parser's static/section tablets, threaded to the gen as one list
   (pxtra ps) `((el ps 5) (el ps 6) (el ps 7) (el ps 8) (el ps 9) (el ps 10) (el ps 11) (el ps 12))   ; sfns svars sects aliases aligns AINLS NOINLS CONSTS
   ; a relocatable object: text/data/.bss/ai_nifs/.image sections + symbols + relocations
   ; -- the bytes (`-c` writes them; the link path feeds them to ldlink).
   ; both targets: obj.l picks the reloc vocabulary by tgt.
   ; ..plus one section per OTHER __attribute__((section(NAME))) name the TU
   ; used, appended in declaration order (cgen-obj's tail). the kernel's
   ; .limine_requests is why: a bootloader SCANS for that section by name, and
   ; three of them bracket the scan, so both their EXISTENCE and their order are
   ; the protocol's. objsecs is the door that takes them.
   (cc-objbytes src is ds tgt nsi)
    (: ps (cc-parse src is ds tgt nsi)
       ob (cgen-obj <>ps <>>ps <>>>ps (el ps 4) (pxtra ps) tgt)
       _ (? (okv? ob) () (ccdie ob src ("cc: codegen error in " + src)))
       xs ((map (\ x (objsec-named <x <>x) ) (el ob 8))
           + (map (\ x (objsec-code <x <>x <>>x)) (el ob 11)))   ; ..and the fns that named a section
       ; the two writers, one shape: the standard set for the width, plus whatever
       ; the TU named. ⚠ arm32 has no .image lane and no LOCAL FUNC symbols for
       ; statics -- objsecs3 refuses the first, and (el ob 7) simply doesn't ride.
       (? (arm32? tgt)
          (objsecs32 tgt ((objsecs3 tgt (el ob 1) (el ob 2) (el ob 5) (el ob 6) (el ob 9) (el ob 10)) + xs)
                     (el ob 3) (el ob 4) (el ps 4))
          (objsecs tgt ((objsecs4 (el ob 1) (el ob 2) (el ob 5) (el ob 6) (el ob 9) (el ob 10)) + xs)
                   (el ob 3) (el ob 4) (el ps 4) (el ob 7))))
   (cc-obj src dst is ds tgt nsi) (write-bytes dst (cc-objbytes src is ds tgt nsi))
   ; the LINK path (rung 3, crew/holo/link.l): every input lands as object
   ; bytes (a .c compiled here, a .o read off disk), crt0 rides FIRST (its
   ; text is the entry), and ldlink binds the static exe -- no system linker.
   ; the entry hands the OS stack pointer (argc sits at [rsp]) to __ai_start
   ; and exits with its answer if it ever returns. __ai_start is defined here
   ; WEAK -- the bare call-main tail every small link gets (the push 16-aligns
   ; the call) -- so a libc object (crew/moon/lib/nolibc.c, rung 4) can override
   ; it strong and unpack argv/envp/auxv before main. no conditional anywhere:
   ; the weak machinery is the whole switch.
   crt0 `(`('mov 'r6 'sp)
           `('call '__ai_start)
           `('mov 'r6 'r0) `('li 'r0 60) `('sys)
           `('label '__ai_start)
           `('push 'r6)
           `('call 'main)
           `('mov 'r6 'r0) `('li 'r0 60) `('sys))
   ; the aarch64 twin: x0 = the OS stack pointer (mov from sp must be lea --
   ; ORR reads XZR at 31), exit_group = 93 in x8; the default __ai_start pushes
   ; lr (its own 16-byte cell, keeping sp 16-aligned for main) and exits
   ; straight off main's answer, already in x0.
   crt0-a64 `(`('lea 'r0 'sp 0)
               `('call '__ai_start)
               `('li 'r8 93) `('sys)
               `('label '__ai_start)
               `('push 'lr)
               `('call 'main)
               `('li 'r8 93) `('sys))
   ; the riscv twin: a0 = the OS stack pointer, exit_group = 94 in a7 (the
   ; answer already rides a0); the default __ai_start mirrors arm64's.
   crt0-rv `(`('lea 'r0 'sp 0)
              `('call '__ai_start)
              `('li 'r7 94) `('sys)
              `('label '__ai_start)
              `('push 'lr)
              `('call 'main)
              `('li 'r7 94) `('sys))
   ; the driver's LIBC (tolerance's other half): a link still owing strong
   ; symbols after the given objects is the cue every cc answers by supplying
   ; the runtime -- ours is nolibc + the am math + the sys leaf, compiled from
   ; the sources beside us and pulled MEMBER BY NEED, archive-fashion: a member
   ; whose every def the link already carries stays out, so a set bringing its
   ; own am.o (love0's does) never meets a twin. the lane engages exactly where a
   ; nom is owed; the -nostdlib family leaves it off whole. the
   ; sources come from the toolchain root (mhome above), so an INSTALLED mooncc
   ; pulls from its nest -- and owing symbols with no root in reach is its own
   ; diagnostic below, never a silent miss.
   ; the noms the LINKER synthesizes rather than any object defining them (holo's
   ; ldres). they are not owed by anybody, and counting them would report the nif
   ; bracket as a missing symbol.
   ldsynth '("__start_ai_nifs" "__stop_ai_nifs" "__start_ai_rela" "__stop_ai_rela"
             ; ..and the copied region's brackets, which only ldbare32's map knows
             "__data_start__" "__data_end__" "__data_load__"
             "__bss_start__" "__bss_end__")
   ; THE LINK CAME UP SHORT. noms that no input object defines and that no runtime
   ; member could add: a misspelled name, a function declared and never defined, or
   ; a `goto` to a label that is not there (gen spells the label main.<lbl> and it
   ; reaches the symbol table as an undefined GLOBAL, so an unresolvable label
   ; arrives here rather than at the refusal it should be -- doc/moon-diag.md).
   ; ⚠ this used to be holo's `(scare 'link-undef nm)` arriving raw: `;; link-undef
   ; "main.nowhere"`, which is a love DEBUG NOTE and not a diagnostic -- no cc:
   ; prefix, no sentence, and the reader is left to guess that a compiler-internal
   ; label spelling is their typo. every cc says this sentence; now so does ours,
   ; and it says it about ALL the owed noms at once rather than whichever the
   ; patcher happened to reach first.
   (qfew ns) (mfew (map (\ n ("'" + n + "'")) ns))     ; a taste of the bag, each nom quoted
   (undie us) (udie 1 ("cc: undefined reference to " + (qfew us)))
   (dusyms id bs mach)                     ; (id bytes) -> (defs strong-undefs), global noms
    (: ob (ld-read id bs mach) sy9 (peep ob 'syms 0)
       (go i ds us)
        (? ((peep ob 'nsym 0) <= i) `(ds us)
           (: sy (peep sy9 i ()) bind (el sy 1)
              (? (! (member? bind '(1 2))) (go (i + 1) ds us)
                 (0 < (el sy 3)) (go (i + 1) (link <sy ds) us)
                 (bind = 1) (go (i + 1) ds (link <sy us))
                 (go (i + 1) ds us))))    ; a weak undef resolves to 0 (ELF's law): no need
       (go 1 () ()))
   ; THE LEDGER a set of objects leaves: every nom it defines (plus the ones the
   ; linker synthesizes rather than any object defining them), and the strong
   ; undefineds still owed. both pulling lanes below start from it, and its `owed`
   ; at termination is the honest answer to "what does nothing here define?".
   ; -> (dt owed), dt a tablet the caller goes on pinning into.
   (ldledger srcs mach)
    (: duos (map (\ p (dusyms <p <>p mach)) srcs)
       dt (tablet 0)
       _ (map (\ nm (pin dt nm 1)) ldsynth)
       _ (map (\ du (map (\ nm (pin dt nm 1)) <du)) duos)
       `(dt (foldl (\ a du (foldl (\ a2 nm (? (|| (peep dt nm ()) (member? nm a2)) a2 (link nm a2)))
                                  a <>du))
                   () duos)))
   ; an ARCHIVE resolves BY NEED and BY INDEX: for each nom the link still owes,
   ; the ranlib table names the member that defines it, so a 1767-member libgcc
   ; costs a dozen ELF reads and not 1767. a pulled member's own undefineds join
   ; the owe and the round repeats, which is how one member drags in another.
   ; -> (picked-srcs owed); `dt` grows in place, as the ledger's owner expects.
   (arpull path s dt owed mach)
    (: idx (ld-arsyms path s) lt (ld-arlong s) seen (tablet 0)
       (go u picked)
        (: hit (foldl (\ a nm (? (two? a) a
                                 (: o (peep idx nm ())
                                    (? (! (o-has? idx nm)) a (o-has? seen o) a `(o)))))
                      () u)
           (? (! (two? hit)) `(picked u)
              (: off <hit
                 _ (pin seen off 1)
                 m (ld-armem s off lt)
                 id ((path + "(") + (<m + ")"))
                 du (dusyms id <>m mach)
                 _ (map (\ nm (pin dt nm 1)) <du)
                 u2 (foldl (\ a nm (? (|| (peep dt nm ()) (member? nm a)) a (link nm a)))
                           (filter (\ nm (! (peep dt nm ()))) u) <>du)
                 (go u2 (picked + `(`(id <>m)))))))
       (go owed ()))
   ; every .a on the line, in COMMAND ORDER and after every object -- a library is
   ; what a link falls back on, never what it leads with.
   (arload srcs paths tgt)
    (? (! (two? paths)) ()
       (: mach (peep elf-machines tgt 0)
          lg (ldledger srcs mach) dt <lg
          (go ps u acc)
           (? (! (two? ps)) acc
              (: r (uread <ps)
                 _ (? (two? r) 0 (udie 1 ("cc: cannot open " + <ps)))
                 _ (? (ld-ar? <>r) 0 (udie 1 ("cc: not an archive: " + <ps)))
                 pr (arpull <ps <>r dt u mach)
                 (go >ps <>pr (acc + <pr))))
          (go paths <>lg ())))
   (rtpull srcs tgt nostd)
    (: mach (peep elf-machines tgt 0)
       lg (ldledger srcs mach) dt <lg owed <>lg
       ; -nostdlib and friends: no runtime BY REQUEST, so there is nothing left to
       ; consult and an owed nom is undefined, full stop. this lane used to fall
       ; through to holo's raw scare, and it is the lane where a bad name is most
       ; likely -- freestanding code has no libc to have meant.
       _ (? (&& nostd (two? owed)) (undie owed) 0)
       home (? (&& (two? owed) (! nostd)) (mhome ()) ())
       ; owed symbols and NO runtime in reach: BOTH facts, in the order they can
       ; be acted on. the undefined reference is the one that is certainly true --
       ; the old message led with the absent toolchain and prescribed "install the
       ; nest", which is the wrong repair for a program whose label is misspelled,
       ; and the two conditions read identically from here.
       _ (? (&& (two? owed) (! (string? home)))
            (udie 1 ("cc: undefined reference to " + (qfew owed)
                     + "; no runtime in reach to supply it either (looked in "
                     + (mfew (mroots ())) + ") -- check the spelling, then the nest"))
            0)
       (? (|| nostd (! (string? home))) ()
          (: mdir (home + "lib/math/")
             inc (home + "include/")
             ; the IMPLICIT runtime compiles with the system tail LEFT ON (the () below):
             ; these are our own sources reaching only our own headers, and the user's
             ; -nostdinc is about the user's translation units, not this one.
             mems (map (\ p (p + (dusyms <p <>p mach)))
                       ((`("nolibc" (string (cc-objbytes (home + "lib/nolibc.c") `(inc) () tgt ())))
                          >< (map (\ f `(f (string (cc-objbytes (mdir + f) `(mdir inc) () tgt ()))))
                                  (sort (filter dotc? (readdir (home + "lib/math"))))))
                        + (? (member? tgt '(x64 arm64 riscv64))
                             `(`("sys" (string (mksys-bytes tgt)))) ())))
             (needs? m u) (foldl (\ a nm (|| a (member? nm u))) () (el m 2))
             ; ⚠ `u` AT TERMINATION IS THE ANSWER, and it used to be thrown away.
             ; no member satisfies anything left in u, so u is exactly the set
             ; nothing anywhere defines -- computed here, three lines before holo
             ; would have scared about one of them by surprise. it is already
             ; filtered against dt each round, so a nom that a pulled member
             ; defined is gone from it.
             (go ms u picked)
              (: hits (filter (\ m (needs? m u)) ms)
                 (? (! (two? hits))
                    (: _ (? (two? u) (undie u) 0) (map (\ m `(<m <>m)) picked))
                    (: rest (filter (\ m (! (needs? m u))) ms)
                       _ (map (\ m (map (\ nm (pin dt nm 1)) (el m 2))) hits)
                       u2 (foldl (\ a m (foldl (\ a2 nm (? (|| (peep dt nm ()) (member? nm a2)) a2 (link nm a2)))
                                               a (el m 3)))
                                 (filter (\ nm (! (peep dt nm ()))) u) hits)
                       (go rest u2 (picked + hits)))))
             (go mems owed ()))))
   (cc-link ins dst is ds tgt pie org dorg nostd nsi)
    (: ars (filter dota? ins)
       srcs (map (\ f (? (dotc? f) `(f (string (cc-objbytes f is ds tgt nsi)))
                         (doto? f) (: r (uread f)
                                      (? (two? r) `(f <>r)
                                         (udie 1 ("cc: cannot open " + f))))
                         (udie 2 ("cc: not a .c/.o/.a input: " + f))))
                 (filter (\ f (! (dota? f))) ins))
       ; an arm32 target has no hosted lane -- there is no OS on this side of the
       ; toolchain that runs a thumb exe, so every thumb link is a BOARD's: the
       ; image lays at the address the core boots from, and no crt0 rides first
       ; because the first text byte has to be the port's own vector table.
       arm? (arm32? tgt)
       _ (? (&& arm? pie) (udie 2 "cc: -pie needs an MMU; an arm32 link is bare metal") 0)
       _ (? (&& arm? (! (charm? org))) (udie 2 "cc: an arm32 link needs -Ttext <addr>") 0)
       ; the MAP the board's image lays under. -Ttext alone is the ONE-region
       ; part (mps2's SSRAM, where code and data share a memory): everything at
       ; org, and the file is the image. -Tdata splits it the way every XIP MCU
       ; wants -- code and literals run from flash where they are stored, .data
       ; runs from SRAM but is STORED after them, and the port's startup copies
       ; it across at boot off __data_load__. a board with a real map (a ROM
       ; header at a fixed offset, a lane order of its own) writes its own link
       ; driver over ldbare32 instead, the way port/inle/klink.l does over ldkern.
       runs (? (charm? dorg) `(`('at org 'text 'rodata 'nifs)
                               `('copy dorg 'data 'bss))
              `(`('at org 'text 'rodata 'data 'nifs 'bss)))
       c9 (? (id? tgt 'arm64) crt0-a64 (id? tgt 'riscv64) crt0-rv crt0)
       all (? arm? srcs
              (link `("crt0" (string (objelf tgt c9 () `("__ai_start") () `("__ai_start") () () () () ()))) srcs))
       ; the archives first, then whatever the runtime still owes: a library the
       ; line NAMED is what the link meant, and the implicit one is the fallback.
       lib (all + (arload all ars tgt))
       set (lib + (rtpull lib tgt nostd))
       ps (? arm? (ldbare32 tgt runs set) (ldlink tgt pie org set))
       _ (ld-write dst ps)
       (chmod dst 493))
   usage "usage: mooncc [-c] [-pie] [-nostdinc] [-t x64|arm64] [-Ttext addr] [-Tdata addr] [-I dir] [-D name[=val]] [-o out] in.c|in.o|lib.a .."
   ; a flag's numeric argument: 0x.. hex or decimal (-Ttext 0x80000000)
   (tonum s)
    (: n (tally s)
       hex (&& (2 < n) (&& ((s 0) = 48) (|| ((s 1) = 120) ((s 1) = 88))))
       (go i acc b) (? (n <= i) acc
                       (: c (s i)
                          d (? (&& (48 <= c) (c <= 57)) (c - 48)
                               (&& (97 <= c) (c <= 102)) (c - 87)
                               (&& (65 <= c) (c <= 70)) (c - 55)
                               (udie 2 usage))
                          _ (? (d < b) 0 (udie 2 usage))
                          (go (i + 1) ((acc * b) + d) b)))
       (? hex (go 2 0 16) (go 0 0 10)))
   ; the flag scan: -c, -pie, -t tgt / -ttgt, -Ttext addr, -I dir / -Idir,
   ; -D def / -Ddef, -o out; every other bare arg is an input. -pie links an
   ; ET_DYN static-PIE (the linker slides its abs64 pointers at startup); the
   ; default is the fixed-base ET_EXEC. -Ttext lays the image at addr instead
   ; of the classic base (the bare-metal lane; it forces the link path like
   ; -pie). the ADVISORY cc families ride through ignored (tol? below), the
   ; -nostdlib family turns the driver's libc off (n9 -- the runtime pull in
   ; cc-link), and any other dash arg refuses. -> (c idirs defs out ins tgt
   ; pie org nostd).
   (glu? a ch) (&& (2 < (tally a)) (&& ((a 0) = 45) ((a 1) = ch)))
   ; the cc-driver tolerance: every cc accepts the advisory families -- warnings,
   ; -O, -g, -pipe, -std= (we speak the one dialect we speak), -static (the
   ; output already is), the -f codegen-hygiene family -- so `CC=mooncc` drives
   ; a make recipe written for gcc/clang unchanged. what would CHANGE the
   ; artifact stays loud: -shared, -m.., and -Wl,/-Wa,/-Wp, (their payload is
   ; real arguments; dropping them is the silent no-op wearing a cc face).
   (pfx? a p) (: n (tally p) (&& (n <= (tally a)) ((snip a 0 n) = p)))
   (tol? a) (|| (a = "-pipe") (|| (a = "-static")
             (|| (pfx? a "-g") (|| (pfx? a "-O") (|| (pfx? a "-std=")
             (|| (pfx? a "-f")
                 (&& (pfx? a "-W")
                     (! (|| (pfx? a "-Wl,") (|| (pfx? a "-Wa,") (pfx? a "-Wp,")))))))))))
   ; the -nostdlib family: freestanding on any cc's surface -- the driver
   ; supplies no runtime, and an owed symbol is an undefined reference outright
   ; -- there is nothing left to consult, so rtpull says so rather than pulling.
   (nostd? a) (|| (a = "-nostdlib") (|| (a = "-nostartfiles")
               (|| (a = "-nodefaultlibs") (a = "-ffreestanding"))))
   (dirn d) (: n (tally d) (? (n = 0) d ((d (n - 1)) = 47) d (d + "/")))
   (dota? p) (: n (tally p) (&& (2 < n) (&& ((p (n - 2)) = 46) ((p (n - 1)) = 97))))
   (dotc? p) (: n (tally p) (&& (2 < n) (&& ((p (n - 2)) = 46) ((p (n - 1)) = 99))))
   (doto? p) (: n (tally p) (&& (2 < n) (&& ((p (n - 2)) = 46) ((p (n - 1)) = 111))))
   (obase p) (: n (tally p)                        ; dir/x.c -> x.o (in the cwd, gcc-shaped)
                (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r (n - 1)))
                ((go 0 0) + "o"))
   (flags as c is ds o ins t9 p9 g9 n9 x9 d9)
    (? (! (two? as)) `(c (rev is) (rev ds) o (rev ins) t9 p9 g9 n9 x9 d9)
       (: a <as r >as
          (? (a = "-c") (flags r 1 is ds o ins t9 p9 g9 n9 x9 d9)
             (a = "-pie") (flags r c is ds o ins t9 1 g9 n9 x9 d9)
             (a = "-I") (? (two? r) (flags >r c (link <r is) ds o ins t9 p9 g9 n9 x9 d9) (udie 2 usage))
             (a = "-D") (? (two? r) (flags >r c is (link <r ds) o ins t9 p9 g9 n9 x9 d9) (udie 2 usage))
             (a = "-o") (? (two? r) (flags >r c is ds <r ins t9 p9 g9 n9 x9 d9) (udie 2 usage))
             (a = "-t") (? (two? r) (flags >r c is ds o ins (totgt <r) p9 g9 n9 x9 d9) (udie 2 usage))
             (a = "-Ttext") (? (two? r) (flags >r c is ds o ins t9 p9 (tonum <r) n9 x9 d9) (udie 2 usage))
             ; -Tdata is the SPLIT: .data runs there and is stored after the code,
             ; which is what every XIP part needs and what -Ttext alone cannot say.
             (a = "-Tdata") (? (two? r) (flags >r c is ds o ins t9 p9 g9 n9 x9 (tonum <r)) (udie 2 usage))
             (glu? a 73) (flags r c (link (snip a 2 (tally a)) is) ds o ins t9 p9 g9 n9 x9 d9)
             (glu? a 68) (flags r c is (link (snip a 2 (tally a)) ds) o ins t9 p9 g9 n9 x9 d9)
             (glu? a 116) (flags r c is ds o ins (totgt (snip a 2 (tally a))) p9 g9 n9 x9 d9)
             ; -nostdinc is the INCLUDE half of -nostdlib's word, and it is LOUD
             ; rather than advisory: with the /usr/include tail still on, a header
             ; we simply do not carry resolves to GLIBC's instead, and a
             ; freestanding build quietly taking a hosted declaration is the wrong
             ; artifact wearing a green face. (x9 -- it reaches incload as nsi.)
             (a = "-nostdinc") (flags r c is ds o ins t9 p9 g9 n9 1 d9)
             ; -ffreestanding is the one member of the -nostd* family that says
             ; something about the LANGUAGE and not just the link: it makes the
             ; implementation freestanding, and __STDC_HOSTED__ is how the
             ; standard lets a source ask. love.c asks exactly that, to choose
             ; the W^X mmap arena over the freestanding heap copy -- so without
             ; this a kernel build took the hosted lane and reached for sysconf.
             ; it rides as a DEFINE (cpp applies -D after its predefines, so this
             ; overrides), which keeps it out of the flag accumulators. ⚠ NOT the
             ; rest of the family: -nostdlib alone is a hosted program that
             ; simply supplies its own runtime, and test_raw is exactly that.
             (a = "-ffreestanding")
              (flags r c is (link "__STDC_HOSTED__=0" ds) o ins t9 p9 g9 1 x9 d9)
             (nostd? a) (flags r c is ds o ins t9 p9 g9 1 x9 d9)
             (tol? a) (flags r c is ds o ins t9 p9 g9 n9 x9 d9)
             (&& (0 < (tally a)) ((a 0) = 45)) (udie 2 usage)
             (flags r c is ds o (link a ins) t9 p9 g9 n9 x9 d9))))
   (moon-main as)
    ; the default target is the ground we stand on: love-arch (the C boot's pin)
    ; names the running binary's arch, so a native mooncc emits native code and
    ; -t stays what it always was, the CROSS flag. unknown grounds keep x64.
    (: ntv (? (love-arch = "arm64") 'arm64 (love-arch = "riscv64") 'riscv64 'x64)
       f (flags as () () () () () ntv () () () () ())
       c (el f 0) is (map dirn (el f 1)) ds (el f 2) o (el f 3) ins (el f 4) tgt (el f 5) pie (el f 6) org (el f 7) nostd (el f 8) nsi (el f 9) dorg (el f 10)
       ; TWO outcomes now, the cc shape: -c lays objects, anything else LINKS.
       ; one .c used to take a third path (cc-exe: elf64 straight off the IR,
       ; no crt0 and no linker) which could not bind a libc -- so `mooncc
       ; hello.c -o hello`, the most canonical cc line there is and what every
       ; configure script writes, answered undef-label printf. rung 3 gave us a
       ; linker; one exe path is the whole point of having it.
       (? (! (two? ins)) (udie 2 usage)
          o (? c (? (two? >ins) (udie 2 "cc: -o takes one input")
                    (: _ (cc-obj <ins o is ds tgt nsi) (quit 0)))
               (: _ (cc-link ins o is ds tgt pie org dorg nostd nsi) (quit 0)))
          ; the legacy pair: exactly two bare args, the second no .c -- IN OUT
          ; (a .c/.o mix is a LINK, never a pair; -c keeps its .o OUT). ⚠ and a
          ; .a is never an OUT: `mooncc a.o libgcc.a` reading as a pair would
          ; write the library it was asked to read FROM.
          (&& (two? >ins)
              (&& (! (two? >>ins))
                  (&& (! (dotc? <>ins))
                      (&& (! (dota? <ins)) (&& (! (dota? <>ins))
                           (|| c (&& (! (doto? <ins)) (! (doto? <>ins)))))))))
           (: _ (? c (cc-obj <ins <>ins is ds tgt nsi)
                     (cc-link `(<ins) <>ins is ds tgt pie org dorg nostd nsi)) (quit 0))
          c (: (go l) (? (! (two? l)) (quit 0)
                         (! (dotc? <l)) (udie 2 ("cc: not a .c input: " + <l))
                         (: _ (cc-obj <l (obase <l) is ds tgt nsi) (go >l)))
               (go ins))
          (: _ (cc-link ins "a.out" is ds tgt pie org dorg nostd nsi) (quit 0)))))

; the SEAT: fire moon-main iff mooncc sits in the program seat of the real command line
; -- the positional program, or a `-l` preload (the installed shebang) -- taking the
; args AFTER the seat. `cmdline` is the full C argv, read the same in both load orders.
; mooncc is its OWN app (its own catted shebang script, doc/moon.md): it does NOT ride the
; au multi-call cat, so a cc edit rebuilds only mooncc, never au. matching mooncc/moon.l here
; keeps the seat quiet under any other program name.
(: (bn p) (: n (tally p)
             (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r n))
             (go 0 0))
   (me? w) (: b (bn w) (|| (b = "mooncc") (|| (b = "mooncc.l") (b = "moon.l"))))
   (seat a) (? (! (two? a)) ()
               (<a = "-l") (? (two? >a)
                                   (? (me? <>a) (link 1 >>a) (seat >>a))
                                   ())
               (me? <a) (link 1 >a)
               ())
   s (seat >cmdline)
   (? (two? s) (moon-main >s) ()))
