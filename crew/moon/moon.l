; crew/moon/moon.l -- the C compiler's driver:
;   mooncc [-c] [-I dir] [-D name[=val]] [-o out] in.c|in.o ..
; reads C, rides lex -> cpp -> parse -> gen -> the holo book. one .c WITHOUT -c
; lays a runnable static ELF directly (chmod 755, _start + text + data one
; segment); WITH -c it lays relocatable ELF objects (crew/holo/obj.l --
; separate sections, a symbol table, and relocations), several .c inputs each
; landing in the cwd as x.o (gcc-shaped); a .o input or SEVERAL inputs without
; -c is a LINK -- our own static linker (crew/holo/link.l, rung 3) binds them
; (compiling any .c on the way) into -o's exe, `a.out` bare. the old positional
; pair `mooncc [-c] IN OUT` still reads (two bare args, no .c and -- linking --
; no .o). -I dirs search before the system pair on both include forms; a -D
; rides in as a synthesized #define line, so token bodies and function-like
; -DF(x)=.. take the one macro path. errors speak on err and exit 1; usage
; exits 2. LAST in the mooncc cat (its own app, doc/moon.md), after
; crew/holo/{elf,obj,link}.l and crew/moon/{lex,cpp,parse,gen}.l; rides the
; u-floor (uread udie). its tail SEAT fires moon-main when mooncc is the program.
(:
   ; the directory portion of a path (up to and including the last '/'), or ""
   (cdir path) (: n (tally path)
                  (go i last) (? (n <= i) last ((path i) = 47) (go (i + 1) (i + 1)) (go (i + 1) last))
                  d (go 0 0)
                  (? (0 < d) (snip path 0 d) ""))
   ; #include resolver bound to a source dir + the -I list: "" searches relative
   ; first (the src dir, then cwd) then the -I dirs; <> the -I dirs straight off;
   ; both fall to the system pair, our own headers before /usr. answers
   ; (1 text child) -- the child hook resolves the hit file's OWN quote
   ; includes beside it (gcc's rule; cpp.l threads it). -> that | ().
   (incload is srcdir name sys?)
    (: dirs (? sys? (is + `("crew/moon/include/" "/usr/include/"))   ; our headers (stdarg.h ..) win for <>
                (`(srcdir "") + is + `("crew/moon/include/" "/usr/include/")))
       (go ds) (? (! (two? ds)) ()
                  (: r (uread (<ds + name))
                     (? (two? r)
                        (: hd (cdir (<ds + name))
                           `(1 <>r (\ n2 s2 (incload is hd n2 s2))))
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
   (cc-parse src is ds tgt)
    (: ds9 (link (archdef tgt) ds)         ; ds9, not ds: a :-local shadows the PARAM over the whole scope (the boxfix cell trap)
       r (uread src)
       _ (? (two? r) () (udie 1 ("cc: cannot open " + src)))
       lx (clex ((deftext ds9) + <>r))
       _ (? (okv? lx) () (ccdie (deskew lx (tally ds9)) src ("cc: lex error in " + src)))
       pp (cpp <>lx (\ name sys? (incload is (cdir src) name sys?)) src)
       _ (? (okv? pp) () (ccdie (deskew pp (tally ds9)) src ("cc: preprocessor error in " + src)))
       ps (cparse-t <>pp tgt)                 ; thread the target so parse sizes pointers/longs at the ABI width
       _ (? (okv? ps) () (ccdie (deskew ps (tally ds9)) src ("cc: parse error in " + src)))
       ps)
   ; the parser's static/section trio, threaded to the gen as one list
   (pxtra ps) `((el ps 5) (el ps 6) (el ps 7) (el ps 8))   ; sfns svars sects ALIASES
   ; a standalone static executable (the default): _start + text + data, chmod +x
   (cc-exe src dst is ds tgt)
    (: ps (cc-parse src is ds tgt)
       ir (cgen <>ps <>>ps <>>>ps (el ps 4) (pxtra ps) tgt)
       _ (? (okv? ir) () (ccdie ir src ("cc: codegen error in " + src)))
       res (elf64 tgt (\ _ <>ir) ())
       _ (write-bytes dst (peep res 'bytes 0))
       (chmod dst 493))
   ; a relocatable object: text/data/ai_nifs/.image sections + symbols + relocations
   ; -- the bytes (`-c` writes them; the link path feeds them to ldlink).
   ; both targets: obj.l picks the reloc vocabulary by tgt.
   (cc-objbytes src is ds tgt)
    (: ps (cc-parse src is ds tgt)
       ob (cgen-obj <>ps <>>ps <>>>ps (el ps 4) (pxtra ps) tgt)
       _ (? (okv? ob) () (ccdie ob src ("cc: codegen error in " + src)))
       (objelf tgt (el ob 1) (el ob 2) (el ob 3) (el ob 4) (el ps 4) (el ob 5) (el ob 6)))
   (cc-obj src dst is ds tgt) (write-bytes dst (cc-objbytes src is ds tgt))
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
   (cc-link ins dst is ds tgt pie org)
    (: srcs (map (\ f (? (dotc? f) `(f (string (cc-objbytes f is ds tgt)))
                         (doto? f) (: r (uread f)
                                      (? (two? r) `(f <>r)
                                         (udie 1 ("cc: cannot open " + f))))
                         (udie 2 ("cc: not a .c/.o input: " + f))))
                 ins)
       c9 (? (id? tgt 'arm64) crt0-a64 (id? tgt 'riscv64) crt0-rv crt0)
       ps (ldlink tgt pie org (link `("crt0" (string (objelf tgt c9 () `("__ai_start") () `("__ai_start") () ()))) srcs))
       _ (ld-write dst ps)
       (chmod dst 493))
   usage "usage: mooncc [-c] [-pie] [-t x64|arm64] [-Ttext addr] [-I dir] [-D name[=val]] [-o out] in.c|in.o .."
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
   ; -D def / -Ddef, -o out; every other bare arg is an input, any other dash
   ; arg refuses. -pie links an ET_DYN static-PIE (the linker slides its abs64
   ; pointers at startup); the default is the fixed-base ET_EXEC. -Ttext lays
   ; the image at addr instead of the classic base (the bare-metal lane; it
   ; forces the link path like -pie). -> (c idirs defs out ins tgt pie org).
   (glu? a ch) (&& (2 < (tally a)) (&& ((a 0) = 45) ((a 1) = ch)))
   (dirn d) (: n (tally d) (? (n = 0) d ((d (n - 1)) = 47) d (d + "/")))
   (dotc? p) (: n (tally p) (&& (2 < n) (&& ((p (n - 2)) = 46) ((p (n - 1)) = 99))))
   (doto? p) (: n (tally p) (&& (2 < n) (&& ((p (n - 2)) = 46) ((p (n - 1)) = 111))))
   ; a LINK-shaped input list: any .o in it, or several inputs at once
   (lnky? ins) (|| (two? >ins) (foldl (\ a x (|| a (doto? x))) () ins))
   (obase p) (: n (tally p)                        ; dir/x.c -> x.o (in the cwd, gcc-shaped)
                (go k r) (? (k < n) (go (k + 1) (? ((p k) = 47) (k + 1) r)) (snip p r (n - 1)))
                ((go 0 0) + "o"))
   (flags as c is ds o ins t9 p9 g9)
    (? (! (two? as)) `(c (rev is) (rev ds) o (rev ins) t9 p9 g9)
       (: a <as r >as
          (? (a = "-c") (flags r 1 is ds o ins t9 p9 g9)
             (a = "-pie") (flags r c is ds o ins t9 1 g9)
             (a = "-I") (? (two? r) (flags >r c (link <r is) ds o ins t9 p9 g9) (udie 2 usage))
             (a = "-D") (? (two? r) (flags >r c is (link <r ds) o ins t9 p9 g9) (udie 2 usage))
             (a = "-o") (? (two? r) (flags >r c is ds <r ins t9 p9 g9) (udie 2 usage))
             (a = "-t") (? (two? r) (flags >r c is ds o ins (totgt <r) p9 g9) (udie 2 usage))
             (a = "-Ttext") (? (two? r) (flags >r c is ds o ins t9 p9 (tonum <r)) (udie 2 usage))
             (glu? a 73) (flags r c (link (snip a 2 (tally a)) is) ds o ins t9 p9 g9)
             (glu? a 68) (flags r c is (link (snip a 2 (tally a)) ds) o ins t9 p9 g9)
             (glu? a 116) (flags r c is ds o ins (totgt (snip a 2 (tally a))) p9 g9)
             (&& (0 < (tally a)) ((a 0) = 45)) (udie 2 usage)
             (flags r c is ds o (link a ins) t9 p9 g9))))
   (moon-main as)
    (: f (flags as () () () () () 'x64 () ())
       c (el f 0) is (map dirn (el f 1)) ds (el f 2) o (el f 3) ins (el f 4) tgt (el f 5) pie (el f 6) org (el f 7)
       lnk? (|| (lnky? ins) (|| pie (charm? org)))   ; -pie/-Ttext force the link path (crt0 + layout)
       (? (! (two? ins)) (udie 2 usage)
          o (? c (? (two? >ins) (udie 2 "cc: -o takes one input")
                    (: _ (cc-obj <ins o is ds tgt) (quit 0)))
               lnk? (: _ (cc-link ins o is ds tgt pie org) (quit 0))
               (: _ (cc-exe <ins o is ds tgt) (quit 0)))
          ; the legacy pair: exactly two bare args, the second no .c -- IN OUT
          ; (a .c/.o mix is a LINK, never a pair; -c keeps its .o OUT)
          (&& (two? >ins)
              (&& (! (two? >>ins))
                  (&& (! (dotc? <>ins))
                      (|| c (&& (! (doto? <ins)) (! (doto? <>ins)))))))
           (: _ (? c (cc-obj <ins <>ins is ds tgt)
                     lnk? (cc-link `(<ins) <>ins is ds tgt pie org)   ; -pie/-Ttext IN OUT: link the one input
                     (cc-exe <ins <>ins is ds tgt)) (quit 0))
          c (: (go l) (? (! (two? l)) (quit 0)
                         (! (dotc? <l)) (udie 2 ("cc: not a .c input: " + <l))
                         (: _ (cc-obj <l (obase <l) is ds tgt) (go >l)))
               (go ins))
          lnk? (: _ (cc-link ins "a.out" is ds tgt pie org) (quit 0))
          (udie 2 usage))))

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
