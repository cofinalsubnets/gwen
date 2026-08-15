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
; ignored so `CC=mooncc` drives a gcc-shaped recipe unchanged -- less -fno-inline,
; which is real here and bars every splice. a link owing
; libc symbols pulls the runtime (nolibc + am math + sys) by need from the
; sources beside it -- -nostdlib/-ffreestanding turn that off. errors speak on
; err and exit 1; usage exits 2. LAST in the mooncc cat (its own app, doc/moon.md), after
; crew/holo/{elf,obj,link}.l and crew/moon/{lex,cpp,parse,gen}.l; rides the
; u-floor (uread udie urun). its tail SEAT fires moon-main when mooncc is the program;
; moon-run is the same compile ANSWERING its status, for a caller staying in the image.
;
; ⚠ THE `use` IS LOAD-BEARING, and love0 is why. This cat is read TWICE -- once by the
; full love to bake mooncc.image, and once by LOVE0 to bake mooncc0.image, the image that
; breaks the self-host circle. pat rides love0's library (pat0.h is in gl0_h) but the boot
; splices it for neither, so `@` is simply an unbound name there: without this line moon.l
; reads fine under `love` and answers `;; missing @` under love0 -- at BAKE time, in the
; one lane the compiler cannot be built without. asbook.l's (use 'holo) is the same shape.
(use 'pat)

(:
   ; the directory portion of a path (up to and including the last '/'), or ""
   (cdir path) (: n (tally path)
                  (go i last) (? (n <= i) last (path i = 47) (go (i + 1) (i + 1)) (go (i + 1) last))
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
                (? (string? x) (cdir x + "../lib/love/moon/") ()))
   (mroots _) (: s (mseat ()) (? (string? s) ["crew/moon/" s] '("crew/moon/")))
   ; the runtime home: the FIRST root that actually carries the sources. by
   ; READ, not by a name test -- presence is the wrapper (uread answers
   ; (1 text), an absent file the zero point), and an empty core.c would be
   ; no runtime either. core.c is the one member every link takes, so it is
   ; the one that says a root is real.
   (mhome _) (: (go rs) (? !(two? rs) ()
                           (two? (uread (<rs + "lib/nolibc/core.c"))) <rs
                           (go >rs))
                (go (mroots ())))
   ; the first three of a name list, comma-joined, with a count of the rest --
   ; a diagnostic wants a taste, not the whole bag
   (mfew ns)
    (: n (tally ns)
       (go l k acc) (? (!(two? l) || (3 <= k)) acc
                       (go >l (k + 1) (? (0 < k) (acc + ", " + <l) <l)))
       s (go ns 0 "")
       (? (3 < n) (s + " (+" + show (n - 3) + " more)") s))
   ; #include resolver bound to a source dir + the -I list: "" searches relative
   ; first (the src dir, then cwd) then the -I dirs; <> the -I dirs straight off;
   ; both fall to the system pair, our own headers before /usr. answers
   ; (1 text child) -- the child hook resolves the hit file's OWN quote
   ; includes beside it (gcc's rule; cpp.l threads it). -> that | ().
   (incload nsi is srcdir name sys?)
    (: ours (map (\ r (r + "include/")) (mroots ())                          ; our headers (stdarg.h ..) win for <>
             + (? nsi () '("/usr/include/")))                                ; -nostdinc: no glibc tail at all
       dirs (? sys? (is + ours)
                ([srcdir ""] + is + ours))
       (go ds) (? !(two? ds) ()
                  (: r (uread (<ds + name))
                     (? (two? r)
                        (: hd (cdir (<ds + name))
                           [1 <>r (\ n2 s2 (incload nsi is hd n2 s2))])
                        (go >ds))))
       (go dirs))
   (el l i) (? (i < 1) <l (el >l (i - 1)))
   ; a -D definition as #define text: the first = becomes the space, so
   ; -DX -> "X" (defined 1 below), -DX=v -> "X v", -DF(x)=x -> "F(x) x"
   ; (the glued ( survives the lex, so it stays function-like).
   (eqsp d) (: n (tally d)
               (go i) (? (n <= i) (d + " 1")
                         (d i = 61) (snip d 0 i + " " + snip d (i + 1) n)
                         (go (i + 1)))
               (go 0))
   (deftext ds) (foldl (\ a d (a + "#define " + eqsp d + "\n")) "" ds)
   ; the target: 'x64 (the default), 'arm64, or 'thumb2 (ARMv7E-M / Cortex-M7),
   ; picked by -t. the arch predefine (__x86_64__ / __aarch64__ / __arm__) rides
   ; the -D channel so cpp stays target-blind.
   (totgt s) (name s @
    ('x64 | 'amd64)             'x64
    ('arm64 | 'aarch64)         'arm64
    ('thumb2 | 'cortex-m7)      'thumb2
    ('thumb1 | 'cortex-m0)      'thumb1
    ('thumb2sp | 'playdate)     'thumb2sp
    ('riscv64 | 'riscv | 'rv64) 'riscv64
                                (udie 2 ("cc: unknown target " + s)))

   ; the arch predefines, BOTH spellings where gcc gives two -- pdclib's config asks
   ; `defined(__x86_64)` for its varargs lane, and one spelling missing reads as an
   ; unsupported machine rather than as a compiler that only knows the other word.
   (archdefs tgt)
   (tgt @ 'arm64                          '("__aarch64__")
          'riscv64                        '("__riscv")
          ('thumb2 | 'thumb2sp | 'thumb1) '("__arm__" "__arm")
                                          '("__x86_64__" "__x86_64" "__amd64__" "__amd64"))
   ; C11's CONDITIONAL FEATURES (6.10.8.3) -- saying an absence out loud is what makes
   ; it conforming rather than a hole, and lets a portable source pick its other lane.
   ; ⚠ every row is TARGET-true: complex and VLAs each ride a subset (the parity table),
   ; so claiming an absence we don't have would send a consumer down a fallback for
   ; nothing. the UTF pair is the positive claim: u"" IS UTF-16, U"" UTF-32.
   (featdefs tgt)
    ('("__STDC_NO_ATOMICS__=1" "__STDC_NO_THREADS__=1"
       "__STDC_UTF_16__=1" "__STDC_UTF_32__=1")
     + (? (id? tgt 'x64) () '("__STDC_NO_COMPLEX__=1"))
     + (? (member? tgt '(x64 arm64)) () '("__STDC_NO_VLA__=1")))
   ; the gcc-shaped predefine table (doc/moon-c-gaps: the predefine surface) --
   ; the <stdint>/<limits>/<float.h> family a library config reads instead of
   ; probing: pdclib, gnulib and musl typedef straight off these names. one
   ; table forked once on the word width (+ the wchar fork), every value gcc's
   ; own spelling on that target, so a mooncc object and a gcc object agree on
   ; every typedef. two deliberate deviations: __CHAR_UNSIGNED__ stays out
   ; (gcc's arm/riscv char is unsigned, ours is signed on every target, and a
   ; predefine describes THIS compiler), and the __LDBL_* rows answer double's
   ; values -- no long double here -- so a consumer takes its double lane, the
   ; one we can compile. a user -D lands after these and wins.
   stdcommon
    ; ⚠ __mooncc__ says WHICH COMPILER, __moon__ says WHICH DIALECT, and a source wanting an
    ; extension must ask the second: under -std=c this is still mooncc and the extension is
    ; still refused. cc-parse adds __moon__ because only it knows the dialect.
    '("__mooncc__=1"
      "__CHAR_BIT__=8" "__SCHAR_MAX__=0x7f" "__SHRT_MAX__=0x7fff"
      "__SCHAR_WIDTH__=8" "__SHRT_WIDTH__=16" "__INT_WIDTH__=32"
      "__LONG_LONG_MAX__=0x7fffffffffffffffLL" "__LONG_LONG_WIDTH__=64"
      "__ORDER_LITTLE_ENDIAN__=1234" "__ORDER_BIG_ENDIAN__=4321" "__ORDER_PDP_ENDIAN__=3412"
      "__BYTE_ORDER__=__ORDER_LITTLE_ENDIAN__" "__FLOAT_WORD_ORDER__=__ORDER_LITTLE_ENDIAN__"
      "__INT8_TYPE__=signed char" "__UINT8_TYPE__=unsigned char"
      "__INT8_MAX__=0x7f" "__UINT8_MAX__=0xff" "__INT8_C(c)=c" "__UINT8_C(c)=c"
      "__INT16_TYPE__=short int" "__UINT16_TYPE__=short unsigned int"
      "__INT16_MAX__=0x7fff" "__UINT16_MAX__=0xffff" "__INT16_C(c)=c" "__UINT16_C(c)=c"
      "__INT32_TYPE__=int" "__UINT32_TYPE__=unsigned int"
      "__INT32_MAX__=0x7fffffff" "__UINT32_MAX__=0xffffffffU"
      "__INT32_C(c)=c" "__UINT32_C(c)=c ## U"
      "__INT_LEAST8_TYPE__=signed char" "__UINT_LEAST8_TYPE__=unsigned char"
      "__INT_LEAST8_MAX__=0x7f" "__UINT_LEAST8_MAX__=0xff" "__INT_LEAST8_WIDTH__=8"
      "__INT_LEAST16_TYPE__=short int" "__UINT_LEAST16_TYPE__=short unsigned int"
      "__INT_LEAST16_MAX__=0x7fff" "__UINT_LEAST16_MAX__=0xffff" "__INT_LEAST16_WIDTH__=16"
      "__INT_LEAST32_TYPE__=int" "__UINT_LEAST32_TYPE__=unsigned int"
      "__INT_LEAST32_MAX__=0x7fffffff" "__UINT_LEAST32_MAX__=0xffffffffU" "__INT_LEAST32_WIDTH__=32"
      "__WINT_TYPE__=unsigned int" "__WINT_MAX__=0xffffffffU" "__WINT_MIN__=0U" "__WINT_WIDTH__=32"
      "__SIZEOF_SHORT__=2" "__SIZEOF_INT__=4" "__SIZEOF_LONG_LONG__=8"
      "__SIZEOF_FLOAT__=4" "__SIZEOF_DOUBLE__=8" "__SIZEOF_WCHAR_T__=4" "__SIZEOF_WINT_T__=4"
      "__FLT_RADIX__=2"
      "__FLT_MANT_DIG__=24" "__FLT_DIG__=6" "__FLT_DECIMAL_DIG__=9"
      "__FLT_MIN_EXP__=(-125)" "__FLT_MIN_10_EXP__=(-37)" "__FLT_MAX_EXP__=128" "__FLT_MAX_10_EXP__=38"
      "__FLT_MIN__=1.17549435082228750796873653722224568e-38F"
      "__FLT_EPSILON__=1.19209289550781250000000000000000000e-7F"
      "__FLT_DENORM_MIN__=1.40129846432481707092372958328991613e-45F"
      "__FLT_NORM_MAX__=3.40282346638528859811704183484516925e+38F"
      "__FLT_HAS_DENORM__=1" "__FLT_HAS_INFINITY__=1" "__FLT_HAS_QUIET_NAN__=1"
      "__DBL_MANT_DIG__=53" "__DBL_DIG__=15" "__DBL_DECIMAL_DIG__=17"
      "__DBL_MIN_EXP__=(-1021)" "__DBL_MIN_10_EXP__=(-307)" "__DBL_MAX_EXP__=1024" "__DBL_MAX_10_EXP__=308"
      "__DBL_MIN__=2.22507385850720138309023271733240406e-308"
      "__DBL_EPSILON__=2.22044604925031308084726333618164062e-16"
      "__DBL_DENORM_MIN__=4.94065645841246544176568792868221372e-324"
      "__DBL_NORM_MAX__=1.79769313486231570814527423731704357e+308"
      "__DBL_HAS_DENORM__=1" "__DBL_HAS_INFINITY__=1" "__DBL_HAS_QUIET_NAN__=1"
      "__LDBL_MANT_DIG__=53" "__LDBL_DIG__=15" "__LDBL_DECIMAL_DIG__=17"
      "__LDBL_MIN_EXP__=(-1021)" "__LDBL_MIN_10_EXP__=(-307)" "__LDBL_MAX_EXP__=1024" "__LDBL_MAX_10_EXP__=308"
      "__LDBL_MIN__=2.22507385850720138309023271733240406e-308"
      "__LDBL_MAX__=1.79769313486231570814527423731704357e+308"
      "__LDBL_EPSILON__=2.22044604925031308084726333618164062e-16"
      "__LDBL_DENORM_MIN__=4.94065645841246544176568792868221372e-324"
      "__LDBL_NORM_MAX__=1.79769313486231570814527423731704357e+308"
      "__LDBL_HAS_DENORM__=1" "__LDBL_HAS_INFINITY__=1" "__LDBL_HAS_QUIET_NAN__=1"
      "__DECIMAL_DIG__=17")
   stdlp64
    '("__LP64__=1" "_LP64=1"
      "__LONG_MAX__=0x7fffffffffffffffL" "__LONG_WIDTH__=64"
      "__SIZEOF_LONG__=8" "__SIZEOF_POINTER__=8" "__SIZEOF_SIZE_T__=8" "__SIZEOF_PTRDIFF_T__=8"
      "__INT64_TYPE__=long int" "__UINT64_TYPE__=long unsigned int"
      "__INT64_MAX__=0x7fffffffffffffffL" "__UINT64_MAX__=0xffffffffffffffffUL"
      "__INT64_C(c)=c ## L" "__UINT64_C(c)=c ## UL"
      "__INT_LEAST64_TYPE__=long int" "__UINT_LEAST64_TYPE__=long unsigned int"
      "__INT_LEAST64_MAX__=0x7fffffffffffffffL" "__UINT_LEAST64_MAX__=0xffffffffffffffffUL"
      "__INT_LEAST64_WIDTH__=64"
      "__INT_FAST8_TYPE__=signed char" "__UINT_FAST8_TYPE__=unsigned char"
      "__INT_FAST8_MAX__=0x7f" "__UINT_FAST8_MAX__=0xff" "__INT_FAST8_WIDTH__=8"
      "__INT_FAST16_TYPE__=long int" "__UINT_FAST16_TYPE__=long unsigned int"
      "__INT_FAST16_MAX__=0x7fffffffffffffffL" "__UINT_FAST16_MAX__=0xffffffffffffffffUL"
      "__INT_FAST16_WIDTH__=64"
      "__INT_FAST32_TYPE__=long int" "__UINT_FAST32_TYPE__=long unsigned int"
      "__INT_FAST32_MAX__=0x7fffffffffffffffL" "__UINT_FAST32_MAX__=0xffffffffffffffffUL"
      "__INT_FAST32_WIDTH__=64"
      "__INT_FAST64_TYPE__=long int" "__UINT_FAST64_TYPE__=long unsigned int"
      "__INT_FAST64_MAX__=0x7fffffffffffffffL" "__UINT_FAST64_MAX__=0xffffffffffffffffUL"
      "__INT_FAST64_WIDTH__=64"
      "__INTMAX_TYPE__=long int" "__UINTMAX_TYPE__=long unsigned int"
      "__INTMAX_MAX__=0x7fffffffffffffffL" "__UINTMAX_MAX__=0xffffffffffffffffUL"
      "__INTMAX_C(c)=c ## L" "__UINTMAX_C(c)=c ## UL" "__INTMAX_WIDTH__=64"
      "__INTPTR_TYPE__=long int" "__UINTPTR_TYPE__=long unsigned int"
      "__INTPTR_MAX__=0x7fffffffffffffffL" "__UINTPTR_MAX__=0xffffffffffffffffUL" "__INTPTR_WIDTH__=64"
      "__SIZE_TYPE__=long unsigned int" "__SIZE_MAX__=0xffffffffffffffffUL" "__SIZE_WIDTH__=64"
      "__PTRDIFF_TYPE__=long int" "__PTRDIFF_MAX__=0x7fffffffffffffffL" "__PTRDIFF_WIDTH__=64")
   stdt32
    '("__LONG_MAX__=0x7fffffffL" "__LONG_WIDTH__=32"
      "__SIZEOF_LONG__=4" "__SIZEOF_POINTER__=4" "__SIZEOF_SIZE_T__=4" "__SIZEOF_PTRDIFF_T__=4"
      "__INT64_TYPE__=long long int" "__UINT64_TYPE__=long long unsigned int"
      "__INT64_MAX__=0x7fffffffffffffffLL" "__UINT64_MAX__=0xffffffffffffffffULL"
      "__INT64_C(c)=c ## LL" "__UINT64_C(c)=c ## ULL"
      "__INT_LEAST64_TYPE__=long long int" "__UINT_LEAST64_TYPE__=long long unsigned int"
      "__INT_LEAST64_MAX__=0x7fffffffffffffffLL" "__UINT_LEAST64_MAX__=0xffffffffffffffffULL"
      "__INT_LEAST64_WIDTH__=64"
      "__INT_FAST8_TYPE__=int" "__UINT_FAST8_TYPE__=unsigned int"
      "__INT_FAST8_MAX__=0x7fffffff" "__UINT_FAST8_MAX__=0xffffffffU" "__INT_FAST8_WIDTH__=32"
      "__INT_FAST16_TYPE__=int" "__UINT_FAST16_TYPE__=unsigned int"
      "__INT_FAST16_MAX__=0x7fffffff" "__UINT_FAST16_MAX__=0xffffffffU" "__INT_FAST16_WIDTH__=32"
      "__INT_FAST32_TYPE__=int" "__UINT_FAST32_TYPE__=unsigned int"
      "__INT_FAST32_MAX__=0x7fffffff" "__UINT_FAST32_MAX__=0xffffffffU" "__INT_FAST32_WIDTH__=32"
      "__INT_FAST64_TYPE__=long long int" "__UINT_FAST64_TYPE__=long long unsigned int"
      "__INT_FAST64_MAX__=0x7fffffffffffffffLL" "__UINT_FAST64_MAX__=0xffffffffffffffffULL"
      "__INT_FAST64_WIDTH__=64"
      "__INTMAX_TYPE__=long long int" "__UINTMAX_TYPE__=long long unsigned int"
      "__INTMAX_MAX__=0x7fffffffffffffffLL" "__UINTMAX_MAX__=0xffffffffffffffffULL"
      "__INTMAX_C(c)=c ## LL" "__UINTMAX_C(c)=c ## ULL" "__INTMAX_WIDTH__=64"
      "__INTPTR_TYPE__=int" "__UINTPTR_TYPE__=unsigned int"
      "__INTPTR_MAX__=0x7fffffff" "__UINTPTR_MAX__=0xffffffffU" "__INTPTR_WIDTH__=32"
      "__SIZE_TYPE__=unsigned int" "__SIZE_MAX__=0xffffffffU" "__SIZE_WIDTH__=32"
      "__PTRDIFF_TYPE__=int" "__PTRDIFF_MAX__=0x7fffffff" "__PTRDIFF_WIDTH__=32")
   ; wchar_t forks by ABI, not by width: signed int on x64/riscv64 (SysV),
   ; unsigned int on arm64/arm32 (AAPCS) -- gcc's values, so a header's typedef
   ; lays the same type a gcc object carries.
   (wchdefs tgt)
    (? (id? tgt 'arm64 || arm32? tgt)
       '("__WCHAR_TYPE__=unsigned int" "__WCHAR_MAX__=0xffffffffU" "__WCHAR_MIN__=0U" "__WCHAR_WIDTH__=32")
       '("__WCHAR_TYPE__=int" "__WCHAR_MAX__=0x7fffffff" "__WCHAR_MIN__=(-__WCHAR_MAX__ - 1)" "__WCHAR_WIDTH__=32"))
   (stddefs tgt) ((? (arm32? tgt) stdt32 stdlp64) + wchdefs tgt + stdcommon)
   ; the diagnostic driver (doc/moon-diag.md): a stage answered a gripe (a cause
   ; it named itself) or a bare bad. a gripe prints "cc: <file>:<line>:<col>: msg"
   ; -- an unknown file takes the source path, an unknown line/col elides; anything
   ; else falls back to <plain>, so a not-yet-converted stage keeps today's line.
   ; either way a fatal exit 1.
   (ccdie val src plain)
    (? (gripe? val)
       (: file (: f (el val 1) (? f f src))
          ln (el val 2) col (el val 3) msg (el val 4)
          loc (file + (? ln (":" + show ln) "") + (? col (":" + show col) ""))
          (udie 1 ("cc: " + loc + ": " + msg)))
       (udie 1 plain))
   ; the shared front end: source path (+ -I dirs, -D defines) -> (1 ast stag
   ; sigs ..) | a fatal exit. the -D lines PREPEND to the source text before the
   ; one lex, so a define body runs to its line's end and the numbering stays
   ; whole -- clexat stamps them 1-k..0 and the source itself from 1, so every
   ; stamped line (a gripe, __LINE__) is TRUE with no pay-back pass. an
   ; #include lexes fresh at its own line 1 either way.
   (cc-parse src {'is is 'ds ds 'tgt tgt 'nsi nsi 'dia dia})
    (: ds9 (archdefs tgt + featdefs tgt + stddefs tgt
                 + (? (id? dia 'moon) '("__moon__=1") ())   ; the DIALECT's own macro: extensions on
                 +   ; ds9, not ds: a :-local shadows the PARAM over the whole scope (the boxfix cell trap)
                 (? (id? tgt 'x64) ("__SIZEOF_INT128__=16" >< ds) ds))     ; x64 alone carries the wide pair (gen's d128 lane); love.c's limb seam reads it
       r (uread src)
       _ (? (two? r) () (udie 1 ("cc: cannot open " + src)))
       lx (clexat (deftext ds9 + <>r) (1 - tally ds9))
       _ (? (okv? lx) () (ccdie lx src ("cc: lex error in " + src)))
       pp (cpp <>lx (\ name sys? (incload nsi is (cdir src) name sys?)) src)
       _ (? (okv? pp) () (ccdie pp src ("cc: preprocessor error in " + src)))
       ps (cparse-d <>pp tgt dia)             ; the target sizes pointers/longs at the ABI width; the dialect says what grammar
       _ (? (okv? ps) () (ccdie ps src ("cc: parse error in " + src)))
       ps)
   ; a relocatable object: text/data/.bss/ai_nifs/.image sections + symbols + relocations
   ; -- the bytes (`-c` writes them; the link path feeds them to ldlink).
   ; both targets: obj.l picks the reloc vocabulary by tgt.
   ; ..plus one section per OTHER __attribute__((section(NAME))) name the TU
   ; used, appended in declaration order (cgen-obj's tail). the kernel's
   ; .limine_requests is why: a bootloader SCANS for that section by name, and
   ; three of them bracket the scan, so both their EXISTENCE and their order are
   ; the protocol's. objsecs is the door that takes them.
   ; ⚠ THE TWO WIDE TUPLES ARE NAMED ONCE, HERE -- parse.l's `tops` answers fourteen fields
   ; and gen.l's cgen-obj twelve. `[]` is the strict arity, so a shape that gains a member is
   ; a refusal naming the file rather than every site quietly reading its NEIGHBOUR.
   ; the gen's option list is the parser's nine tablets plus the driver's own two words
   ; (-fno-inline, then -fir's prefix list).
   ; THE RECORD'S SYMBOL, one per TU: `ai_ir_` and the source's basename, every character
   ; outside [A-Za-z0-9_] becoming _ so a path or a dash cannot mint an unspellable nom.
   ; ⚠ one fixed name made -fir a one-TU flag (`link-dup "ai_lvm_ir"` the moment a second
   ; object carried it), and the basename is what a reader wants anyway: `nm | grep ai_ir_`
   ; names the translation units, in the order the link laid them.
   (irsym src)
    (: b (ubase src ".c") n (tally b)
       (go i acc) (? (n <= i) acc
                     (: c (b i)
                        ok ?(((97 <= c) && (c <= 122)) || ((65 <= c) && (c <= 90))
                             || ((48 <= c) && (c <= 57)) || (c = 95))
                        (go (i + 1) (acc + (? ok (snip b i (i + 1)) "_")))))
       (go 0 "ai_ir_"))
   (cc-objbytes src (t @ {'ni ni 'fir fir 'tgt tgt}))
    (@ (cc-parse src t)
       [_ ast stag sigs weaks sfns svars sects alias aligns ainl noinl consts rstrs]
       (: ob (cgen-obj ast stag sigs weaks
                       [sfns svars sects alias aligns ainl noinl consts rstrs ni fir (irsym src)] tgt)
          _ (? (okv? ob) () (ccdie ob src ("cc: codegen error in " + src)))
          (@ ob [_ tforms dforms fnames gnames nforms iforms lnames xsecs bforms rforms xfns]
             (: xs (map (\ [nm forms ro] (objsec-named nm forms ro)) xsecs
                        + map (\ [nm forms al] (objsec-code nm forms al)) xfns)   ; ..and the fns that named a section
                ; the two writers, one shape: the standard set for the width, plus whatever
                ; the TU named. ⚠ arm32 has no .image lane and no LOCAL FUNC symbols for
                ; statics -- objsecs3 refuses the first, and `lnames` simply doesn't ride.
                (? (arm32? tgt)
                   (objsecs32 tgt (objsecs3 tgt tforms dforms nforms iforms bforms rforms + xs)
                              fnames gnames weaks)
                   (objsecs tgt (objsecs4 tforms dforms nforms iforms bforms rforms + xs)
                            fnames gnames weaks lnames)))
             (udie 1 ("cc: the code generator answered an unexpected shape for " + src))))
       (udie 1 ("cc: the parser answered an unexpected shape for " + src)))
   (cc-obj src dst t) (write-bytes dst (cc-objbytes src t))
   ; the LINK path (rung 3, crew/holo/link.l): every input lands as object
   ; bytes (a .c compiled here, a .o read off disk), crt0 rides FIRST (its
   ; text is the entry), and ldlink binds the static exe -- no system linker.
   ; the entry hands the OS stack pointer (argc sits at [rsp]) to __ai_start
   ; and exits with its answer if it ever returns. __ai_start is defined here
   ; WEAK -- the call-main tail every small link gets (the push 16-aligns the
   ; call, and argc/argv unpack so an arg-taking main reads truth, not the raw
   ; sp) -- so a libc object (crew/moon/lib/nolibc.c, rung 4) can override it
   ; strong and unpack envp/auxv too. no conditional anywhere: the weak
   ; machinery is the whole switch.
   crt0 '((mov r6 sp)
           (call __ai_start)
           (mov r6 r0) (li r0 60) (sys)
           (label __ai_start)
           (push r6)
           (lea r5 r6 8)
           (ld r6 r6 0)
           (call main)
           (mov r6 r0) (li r0 60) (sys))
   ; the aarch64 twin: x0 = the OS stack pointer (mov from sp must be lea --
   ; ORR reads XZR at 31), exit_group = 93 in x8; the default __ai_start pushes
   ; lr (its own 16-byte cell, keeping sp 16-aligned for main) and exits
   ; straight off main's answer, already in x0.
   crt0-a64 '((lea r0 sp 0)
               (call __ai_start)
               (li r8 93) (sys)
               (label __ai_start)
               (push lr)
               (lea r1 r0 8)
               (ld r0 r0 0)
               (call main)
               (li r8 93) (sys))
   ; the riscv twin: a0 = the OS stack pointer, exit_group = 94 in a7 (the
   ; answer already rides a0); the default __ai_start mirrors arm64's.
   crt0-rv '((lea r0 sp 0)
              (call __ai_start)
              (li r7 94) (sys)
              (label __ai_start)
              (push lr)
              (lea r1 r0 8)
              (ld r0 r0 0)
              (call main)
              (li r7 94) (sys))
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
   (undie us) (udie 1 ("cc: undefined reference to " + qfew us))
   ; every .a on the line: holo's ldarch resolves them by need, this reads them and
   ; owns the sentence a reader gets when one will not open -- `cc: cannot open x`,
   ; not the scare ldarch would raise, which names no file the user typed.
   (arload srcs paths tgt)
    (? !(two? paths) ()
       (ldarch srcs
               (map (\ p (: r (uread p)
                            _ (? (two? r) 0 (udie 1 ("cc: cannot open " + p)))
                            _ (? (ld-ar? <>r) 0 (udie 1 ("cc: not an archive: " + p)))
                            [p <>r]))
                    paths)
               tgt))
   ; -- THE RUNTIME CACHE. a member must be COMPILED to know what it defines, so
   ; the pull below cannot select without paying for all of them -- ~23s of the
   ; ~23s a cold hello-world link costs. so the whole compiled runtime rides
   ; ~/.love/cache/moon/<sha256>.a, ONE archive per (compiler, target), keyed on
   ; everything that decides its bytes. an archive because the ranlib index IS
   ; what each member defines, written once and read back rather than recomputed
   ; over 190 objects on every warm link -- and because one file is one
   ; generation: whole the moment it lands, and countable when mcsweep asks
   ; which generations to keep.
   ; ⚠ the compiler is one of those inputs and the costly one to get wrong -- a
   ; stale entry is an old codegen worn silently, and the gates that compile
   ; nolibc by hand (test_raw, test_libc, test_fixpoint) would pass against it.
   ; its identity is the BAKED IMAGE, which the wake strips from cmdline: we
   ; cannot know which one we woke, so every *.image beside the love rides, by
   ; name and stat. hashing the compiler's .l sources instead looks tighter and
   ; is a hole -- edit gen.l, link once before the image catches up, and the
   ; entry stored under the new sources holds the old image's codegen.
   ; no identity, no cache: the digest is the only door, and a love with no image
   ; in reach does not open it.
   ; ⚠ read at each call, never bound -- mhome's law, for mhome's reason.
   (sdir? st) ((two? st) && (16384 = (& (el st 2) 61440)))
   (mcdir _)
    (: h (getenv "HOME")
       _ (? (string? h) (map (\ s (mkdir (h + s) 493)) '("/.love" "/.love/cache" "/.love/cache/moon")) 0)
       d (? (string? h) (h + "/.love/cache/moon/") ())
       (? ((string? d) && (sdir? (stat d))) d ()))
   ; the input-name predicates, ahead of every reader of them.
   (dota? p) (: n (tally p) (2 < n && p (n - 2) = 46 && p (n - 1) = 97))
   (dotc? p) (: n (tally p) (2 < n && p (n - 2) = 46 && p (n - 1) = 99))
   (doto? p) (: n (tally p) (2 < n && p (n - 2) = 46 && p (n - 1) = 111))
   (dotim? p) (: n (tally p) (6 < n && snip p (n - 6) n = ".image"))
   ; the build tree keeps the images in the love's own directory, an installed
   ; nest one rung over in lib/love/ -- the two seats mseat already knows.
   ; ⚠ NOT point-free: the mapped lambda reads `d` to build the path it stats, so dropping
   ; the parameter leaves that read free -- `;; missing d`, at the mooncc0 bake.
   (mcims d) (map (\ f [f (stat (d + f))]) (sort (filter dotim? (readdir d))))
   ; a directory's whole text, names and bytes, sorted and recursive. ⚠ the
   ; HEADERS are an input too: an edited stdio.h changes what nolibc.c means, and
   ; hashing the .c alone would not see it.
   (mctree d)
    (foldl (\ a f (: p (d + f)
                     st (stat p)
                     (? !(two? st) a
                        (sdir? st) ((a + f) + ("/\n" + (mctree (p + "/"))))
                        (: r (uread p) ((a + f) + ("\n" + (? (two? r) <>r "") + "\n"))))))
           "" (sort (readdir d)))
   ; ⚠ the exe's PATH rides beside its stat: two worktrees hold the same
   ; cwd-relative include paths, and only the seat tells their headers apart.
   ; the image is the ONE this session woke, which main.c pins as `love-image`.
   ; ⚠ THE NOM IS DATA HERE, NEVER A BARE REF: a baked consumer folds its bare
   ; globals at its own compile, and mooncc lives baked -- read `love-image`
   ; straight and the image gets the BAKE session's answer wired in forever (a 0,
   ; the bake egg-boots). (ev 'love-image) is the lookup that survives, and the
   ; presence ask beside it is the same law -- the nom is pinned only when a
   ; session woke something. unpinned is an egg boot or a love that does not say,
   ; and every *.image beside the binary is the honest reading for both.
   ; "<baked>" is the binary's own section: no file to stat, and the exe stat
   ; above already carries its identity.
   (mcid home)
    (: x (readlink "/proc/self/exe")
       d (? (string? x) (cdir x) ())
       w (? (member? 'love-image (names ())) [1 (ev 'love-image)] ())
       ims (? (two? w) (: im <>w
                          (? !(string? im) ()
                             (im = "<baked>") ["<baked>"]
                             [[im (stat im)]]))
              (? (string? d) ((mcims d) + (mcims (d + "../lib/love/"))) ()))
       src (? (two? ims) ((mctree (home + "include/")) + (mctree (home + "lib/"))) "")
       (? !(two? ims) ()
          (sha256 ((("moon-rt 1\n" + x) + ("\n" + show (stat x))) + (("\n" + show ims) + ("\n" + src))))))
   ; an entry, read back and checked for its magic: the rename below lands it
   ; whole, so a torn file cannot appear, and a mangled one is passed over rather
   ; than linked. the (1 bytes) wrapper -- an empty entry is absence.
   (mcget d f mag)
    (: r (uread (d + f))
       s (? (two? r) <>r "")
       (? ((tally mag <= tally s) && (snip s 0 (tally mag) = mag)) [1 s] ()))
   ; write beside the entry, then rename ONTO it: two links racing under make -j
   ; land the same bytes either way, and neither can read a half-written one.
   ; every failure here is silent by design -- a read-only home costs speed, and
   ; a cache is never owed a link's correctness.
   (mcput d f bs)
    (: p (d + f)
       t (p + "." + show (getpid ()))
       q (open t "w")
       _ (q ? (say q bs, close q))
       _ (q && !(rename t p) ? () (unlink t))
       ())
   ; the bound, and the cache keeps it ITSELF: swept on every link, hit or miss,
   ; so a folder grown fat under an older policy trims on the next one rather than
   ; waiting for a rebuild to notice it. nothing here knows which generations are
   ; dead, so COUNT decides -- count and not age, because the rate is the tree's
   ; own: a day of rebuilds mints more generations than a month of use, and a
   ; clock cannot tell those apart. the order is by USE, which is what the touch
   ; on a hit is for -- a write time would evict the identity you link with daily
   ; in favour of the ones you minted once.
   (mcsweep d)
    (: mckeep 32                                       ; ~400K each, so ~13M held.
       ; the live set is one generation per (worktree, lane, target) -- a tree
       ; carries the shim's love+mooncc.image and the build's love0+mooncc0.image
       ; as two distinct compilers -- so the room here is deliberate, not slack.
       ; ⚠ AND ANYTHING THAT IS NOT AN ARCHIVE GOES, which is what the paragraph above
       ; promised and did not do: the cache held one .o PER MEMBER before it held one .a
       ; per (compiler, target), and nothing has read a .o since. The count kept the
       ; archives at 32 while 24130 orphans of the old policy sat beside them -- 116 MB in
       ; someone's HOME, growing, with no reader. A pass that cannot say which generations
       ; are dead can still say which SHAPES are.
       _ (foldl (\ a f (? (dota? f) a (: _ (unlink (d + f)) a))) 0 (readdir d))
       fs (foldl (\ a f (: st (stat (d + f))
                          (? (dota? f && two? st) (a + [[(el st 1) f]]) a)))
                 () (readdir d))
       (go l k) (? !(two? l) 0
                   (: e <l
                      _ (? (mckeep <= k) (unlink (d + <>e)) 0)
                      (go >l (k + 1))))
       (go (sortby (\ a b (<b <= <a)) fs) 0))
   ; the compiled runtime as one archive: the cache when it can answer, the
   ; compiler otherwise -- and the compile is what fills the cache. a cache that
   ; cannot open is invisible, and the link is the one that happens today, at
   ; today's price.
   (rtarch cd id home tgt)
    (: k (? (string? cd && string? id) (sha256 $ id + "\n" + show tgt) ())
       hit (? (string? k) (mcget cd (k + ".a") "!<arch>\n") ())
       _ (? (two? hit) (utime (cd + k + ".a") (clock ())) 0)   ; the sweep orders by use
       bs (? (two? hit) <>hit
             (: b (arbytes (rtmems home tgt))
                _ (? (string? k) (mcput cd (k + ".a") b) 0)
                b))
       _ (? (string? cd) (mcsweep cd) 0)
       bs)
   ; every member compiled, ready to index. the IMPLICIT runtime compiles with the
   ; system tail LEFT ON (the () below): these are our own sources reaching only
   ; our own headers, and the user's -nostdinc is about the user's translation
   ; units, not this one. nolibc is a TREE: every .c directly under lib/nolibc/ and
   ; every .c one level down (string/, ..), one function to a file where the
   ; file-scope state allows. A link owing no calendar takes no calendar
   ; (doc/moon-diff.md's libc rows). ⚠ one level, not a walk -- the depth is the
   ; vocabulary, and a deeper tree would say nothing. ⚠ the member names flatten
   ; that tree with _ and the math ones wear m_: time.c and sys/time.c are two
   ; members of one basename, and an archive resolves by name alone.
   (rtmems home tgt)
    (: ndir (home + "lib/nolibc/") mdir (home + "lib/math/") inc (home + "include/")
       (dotobj n) (? (dotc? n) (snip n 0 (tally n - 2) + ".o") n)
       (nlmems d)
        (foldl (\ a e (: p (d + e)
                         (? (dotc? e) (a + [[(dotobj e) p]])
                            (sdir? (stat p))
                             (a + foldl (\ a2 f (a2 + [[(dotobj $ e + "_" + f) (p + "/" + f)]]))
                                        () (sort (filter dotc? (readdir p))))
                            a)))
               () (sort (readdir d)))
       ((map ((a b) \ [a (string (cc-objbytes b (pin (fseed tgt 'moon) 'is [inc])))])
             (nlmems ndir))
        + map (\ f [("m_" + dotobj f) (string (cc-objbytes (mdir + f) (pin (fseed tgt 'moon) 'is [mdir inc])))])
              (sort (filter dotc? (readdir (home + "lib/math"))))
        + (member? tgt '(x64 arm64 riscv64) ? [["mksys.o" (string (mksys-bytes tgt))]])))
   (rtpull srcs tgt nostd)
    (: mach (peep elf-machines tgt 0)
       lg (ldledger srcs mach) dt <lg owed <>lg
       ; -nostdlib and friends: no runtime BY REQUEST, so there is nothing left to
       ; consult and an owed nom is undefined, full stop. this lane used to fall
       ; through to holo's raw scare, and it is the lane where a bad name is most
       ; likely -- freestanding code has no libc to have meant.
       _ (? (nostd && two? owed) (undie owed) 0)
       home (? (two? owed && !nostd) (mhome ()) ())
       ; owed symbols and NO runtime in reach: BOTH facts, in the order they can
       ; be acted on. the undefined reference is the one that is certainly true --
       ; the old message led with the absent toolchain and prescribed "install the
       ; nest", which is the wrong repair for a program whose label is misspelled,
       ; and the two conditions read identically from here.
       _ (? (two? owed && !(string? home))
            (udie 1 ("cc: undefined reference to " + qfew owed
                     + "; no runtime in reach to supply it either (looked in "
                     + mfew (mroots ()) + ") -- check the spelling, then the nest"))
            0)
       (? (nostd || !(string? home)) ()
          (: id (mcid home) cd (? (string? id) (mcdir ()) ())   ; no identity, no cache, no directory made
             pr (arpull "moon-runtime" (rtarch cd id home tgt) dt owed mach)
             ; ⚠ the OWE AT TERMINATION IS THE ANSWER: no member defines anything
             ; left in it, so it is exactly the set nothing anywhere defines --
             ; said here, whole, rather than one nom at a time by whichever holo
             ; scares about first.
             _ (? (two? <>pr) (undie <>pr) 0)
             <pr)))
   (cc-link ins dst (t @ {'is is 'ds ds 'tgt tgt 'pie pie 'org org 'dorg dorg
                          'nostd nostd 'nsi nsi 'ni ni 'fir fir 'rdme rdme}))
    (: ars (filter dota? ins)
       srcs (map (\ f (? (dotc? f) [f (string (cc-objbytes f t))]
                         (doto? f) (: r (uread f)
                                      (? (two? r) [f <>r]
                                         (udie 1 ("cc: cannot open " + f))))
                         (udie 2 ("cc: not a .c/.o/.a input: " + f))))
                 (filter (\ f !(dota? f)) ins))
       ; an arm32 target has no hosted lane -- there is no OS on this side of the
       ; toolchain that runs a thumb exe, so every thumb link is a BOARD's: the
       ; image lays at the address the core boots from, and no crt0 rides first
       ; because the first text byte has to be the port's own vector table.
       arm? (arm32? tgt)
       _ (? (arm? && pie) (udie 2 "cc: -pie needs an MMU; an arm32 link is bare metal") 0)
       _ (? (arm? && !(charm? org)) (udie 2 "cc: an arm32 link needs -Ttext <addr>") 0)
       ; the MAP the board's image lays under. -Ttext alone is the ONE-region
       ; part (mps2's SSRAM, where code and data share a memory): everything at
       ; org, and the file is the image. -Tdata splits it the way every XIP MCU
       ; wants -- code and literals run from flash where they are stored, .data
       ; runs from SRAM but is STORED after them, and the port's startup copies
       ; it across at boot off __data_load__. a board with a real map (a ROM
       ; header at a fixed offset, a lane order of its own) writes its own link
       ; driver over ldbare32 instead, the way free/klink.l does over ldkern.
       runs (? (charm? dorg) [['at org 'text 'rodata 'nifs]
                              ['copy dorg 'data 'bss]]
              [['at org 'text 'rodata 'data 'nifs 'bss]])
       c9 (tgt @ 'arm64 crt0-a64 'riscv64 crt0-rv crt0)
       all (? arm? srcs
              (["crt0" (string (objelf tgt c9 () '("__ai_start") () '("__ai_start") () () () () ()))] >< srcs))
       ; the archives first, then whatever the runtime still owes: a library the
       ; line NAMED is what the link meant, and the implicit one is the fallback.
       lib (all + arload all ars tgt)
       set (lib + rtpull lib tgt nostd)
       ps (? arm? (ldbare32 tgt runs set ()) (ldlink tgt pie org set ("love " + <(usplit 43 love-version)) rdme))
       _ (ld-write dst ps)
       (chmod dst 493))
   usage "usage: mooncc [-c] [-pie] [-nostdinc] [-t x64|arm64] [-Ttext addr] [-Tdata addr] [-I dir] [-D name[=val]] [-freadme=F] [-o out] in.c|in.o|lib.a .."
   ; a flag's numeric argument: 0x.. hex or decimal (-Ttext 0x80000000)
   (tonum s)
    (: n (tally s)
       hex (2 < n && s 0 = 48 && (s 1 = 120 || s 1 = 88))
       (go i acc b) (? (n <= i) acc
                       (: c (s i)
                          d (? (48 <= c <= 57) (c - 48)
                               (97 <= c <= 102) (c - 87)
                               (65 <= c <= 70) (c - 55)
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
   (glu? a ch) (2 < tally a && a 0 = 45 && a 1 = ch)
   ; the cc-driver tolerance: every cc accepts the advisory families -- warnings,
   ; -O, -g, -pipe, -std= (we speak the one dialect we speak), -static (the
   ; output already is), the -f codegen-hygiene family -- so `CC=mooncc` drives
   ; a make recipe written for gcc/clang unchanged. what would CHANGE the
   ; artifact stays loud: -shared, -m.., and -Wl,/-Wa,/-Wp, (their payload is
   ; real arguments; dropping them is the silent no-op wearing a cc face).
   (pfx? a p) (: n (tally p) (n <= tally a && snip a 0 n = p))
   ; -fir= / -fno-ir= accumulate into ONE comma list, a leading `-` on a part being the
   ; subtraction gen.l reads. ⚠ presence is the wrapper's job here too: an unset f9 is (),
   ; and `-fir=` with an empty prefix is a legitimate value meaning EVERY function -- so the
   ; join tests the slot, never the string's length.
   (irjoin old new) (? (string? old) (old + "," + new) new)
   (irneg s) (: n (tally s)                                 ; "a,b" -> "-a,-b"
                (go i j acc) (? (n <= i) (rev (("-" + snip s j i) >< acc))
                                (= 44 (s i)) (go (i + 1) (i + 1) (("-" + snip s j i) >< acc))
                                (go (i + 1) j acc))
                (foldl (\ a p (? (string? a) (a + "," + p) p)) () (go 0 0 ())))
   ; -lm and its family ride through too, and the reason is particular to us: the
   ; driver pulls its runtime BY NEED, so the libc/libm/libdl a recipe asks for is
   ; already in the artifact before it asks. a name we cannot satisfy does not go
   ; quiet either -- it lands at the link as a named undefined reference, the same
   ; sentence a missing archive gets -- and a real third-party library still has its
   ; door: name the .a as an input. -L likewise: we search no library path, and
   ; saying so by refusing the whole compile helps nobody. ⚠ GLUED only (glu? wants
   ; a third charm), so a bare `-l` still refuses rather than eating the next word
   ; as a library name and the word after it as an input.
   ; -- THE DIALECTS ----------------------------------------------------------------
   ; ⚠ -std= is LOUD, and used to ride the tolerated family. That is the wrong answer for
   ; the one flag that says what LANGUAGE the input is: reading one dialect as another, or
   ; an unknown word as either, is worse than refusing.
   ; `moon` is OURS and it is the DEFAULT -- modern C plus the extensions we can add without
   ; contradicting it (struct labels today, borrowed from HolyC). Every one of them is
   ; syntax C rejects outright, so moon is a strict SUPERSET: no C program means anything
   ; different here, which is the whole licence for defaulting to it.
   ; `c` is the fence for when that matters: strict C, no extensions, at whatever iteration
   ; we hover around (C11, doc/moon-c-gaps.md). The ISO names are all one dialect -- we do
   ; not distinguish them from a codegen point of view -- so they differ only in what we can
   ; say we accept, and the bare `c` is the honest spelling of that.
   ; ⚠ NO `holyc` ROW. It named one borrowed extension and would have refused U0, I64 and
   ; class -- a parse error on line 1 of anything actually HolyC, which is the one input
   ; that would reach for the name. It comes back when it is true.
   stds {"moon" 'moon
         "c" 'c "c89" 'c "c90" 'c "c99" 'c "c11" 'c "c17" 'c "c18" 'c "c23" 'c
         "gnu89" 'c "gnu90" 'c "gnu99" 'c "gnu11" 'c "gnu17" 'c "gnu18" 'c "gnu23" 'c}
   (stdof w) (peep stds w ())                    ; a tablet indexes itself; () is the miss
   ; ⚠ SORTED, because `keys` answers HASH order -- a usage line that reshuffles itself
   ; between builds is a worse sentence than an alphabetical one.
   (stdnames _) (ujoin (sort (keys stds)))
   ; ⚠ A PRE-PASS, never a sixteenth accumulator. `flags` carries fifteen curried slots
   ; across twenty-four call sites, and (f) == f -- so one mistyped site would hand back a
   ; CLOSURE instead of a result, in silence. -std= interacts with nothing else on the line,
   ; so it comes OFF the line here and the walk below keeps the arity it has.
   (std-take as)                                   ; -> (dialect args-without-std)
    (: (go l dia acc)
        (? !(two? l) [dia (rev acc)]
           (: a <l
              (? (pfx? a "-std=")
                 (: w (snip a 5 (tally a))
                    d (stdof w)
                    _ (? d 0 (udie 2 ("cc: unknown -std=" + w + " -- mooncc reads: " + (stdnames ()))))
                    (go >l d acc))
                 (go >l dia (a >< acc)))))
       (go as 'moon ()))          ; unsaid means moon: our C, extensions and all
   (tol? a) (a = "-pipe" || a = "-static" ||
             pfx? a "-g" || pfx? a "-O" ||
             pfx? a "-f" || glu? a 108 || glu? a 76 ||
             (pfx? a "-W" &&
              !(pfx? a "-Wl," || pfx? a "-Wa," || pfx? a "-Wp,")))
   ; the -nostdlib family: freestanding on any cc's surface -- the driver
   ; supplies no runtime, and an owed symbol is an undefined reference outright
   ; -- there is nothing left to consult, so rtpull says so rather than pulling.
   (nostd? a) ((a = "-nostdlib") || (a = "-nostartfiles") ||
               (a = "-nodefaultlibs") || (a = "-ffreestanding"))
   (dirn d) (: n (tally d) (? (n = 0) d (d (n - 1) = 47) d (d + "/")))
   (obase p) (: n (tally p)                        ; dir/x.c -> x.o (in the cwd, gcc-shaped)
                (go k r) (? (k < n) (go (k + 1) (? (p k = 47) (k + 1) r)) (snip p r (n - 1)))
                (go 0 0 + "o"))
   ; ⚠ ONE TABLET, not fifteen curried slots. The walk used to carry every accumulator BY
   ; POSITION across twenty-four call sites: adding a flag meant editing all of them, and a
   ; slip put the right value in the wrong slot -- in silence, since (f) == f makes a short
   ; call a CLOSURE rather than an error. Now a flag is one `pin` at its own arm and nothing
   ; anywhere else, and the reader asks for it by name. -std= came off this walk for exactly
   ; the hazard the tablet retires; it stays a pre-pass because it names the LANGUAGE and
   ; belongs before the line is read at all.
   ; the three LISTS (is ds ins) collect reversed and are righted at the end; every other key
   ; is last-one-wins, and `fir` alone accumulates into one comma string.
   ; the one helper that is not already a primitive: prepend onto a keyed list. `pin`
   ; ANSWERS its tablet, so every arm below is a plain pin.
   (fpush t k v) (pin t k (v >< (peep t k ())))
   ; ⚠ EVERY KEY, SEEDED -- and THIS IS THE MANIFEST. The consumers destructure the set in
   ; their own parameter, and a map pattern matches on PRESENCE: a key pinned only when its
   ; flag appears would fail to match exactly when the flag was absent, and the whole call
   ; would answer the zero point. Seeding also puts the flag set in one readable place, which
   ; the positional walk had and the bare tablet lost. ⚠ it is NOT a wrapper on `{}`: the
   ; literal has to be COMPLETE and it is wanted at two sites, so it is spelled once.
   (fseed tgt dia) {'c () 'is () 'ds () 'o () 'ins () 'tgt tgt 'pie () 'org () 'dorg ()
                    'nostd () 'nsi () 'ni () 'fir () 'rdme () 'dia dia}
   (flags as t)
    (? !(two? as)
       (: _ (pin t 'is (rev (peep t 'is ())))
          _ (pin t 'ds (rev (peep t 'ds ())))
          _ (pin t 'ins (rev (peep t 'ins ())))
          t)
       (: a <as r >as
          (? (a = "-c") (flags r (pin t 'c 1))
             ; ⚠ the SAME string love answers, because they are one artifact: mooncc is a
             ; verb on the love binary, not a separate program, and a build that reported
             ; two versions would be reporting a thing that cannot happen. `love-version`
             ; is that one id (mk/lib.mk computes it: ./VERSION plus a VCS suffix).
             ((a = "--version") || (a = "-v"))
              (: _ (puts ("mooncc " + love-version)) _ (putc 10) (quit 0))
             (a = "-pie") (flags r (pin t 'pie 1))
             (a = "-I") (? (two? r) (flags >r (fpush t 'is <r)) (udie 2 usage))
             (a = "-D") (? (two? r) (flags >r (fpush t 'ds <r)) (udie 2 usage))
             (a = "-o") (? (two? r) (flags >r (pin t 'o <r)) (udie 2 usage))
             (a = "-t") (? (two? r) (flags >r (pin t 'tgt (totgt <r))) (udie 2 usage))
             (a = "-Ttext") (? (two? r) (flags >r (pin t 'org (tonum <r))) (udie 2 usage))
             ; -Tdata is the SPLIT: .data runs there and is stored after the code,
             ; which is what every XIP part needs and what -Ttext alone cannot say.
             (a = "-Tdata") (? (two? r) (flags >r (pin t 'dorg (tonum <r))) (udie 2 usage))
             ; -fno-inline bars the SPLICE, fn-wide and TU-wide -- the same door
             ; __attribute__((noinline)) opens one name at a time. it is an instrument
             ; before it is an optimization flag: a splice rewrites the code you are
             ; reading into its caller, so comparing one function against another cc's
             ; needs a way to say don't. gcc's -fno-inline-functions is the same word.
             ((a = "-fno-inline") || (a = "-fno-inline-functions"))
              (flags r (pin t 'ni 1))
             ; -fir=PREFIX[,PREFIX..] -- WRITE DOWN WHAT WE BUILT: the machine-form IR of every
             ; function whose name starts with one of them, laid in .rodata as one readable
             ; datum. -fno-ir=PREFIX[,..] carves back out, gcc's -fno- word and this file's own
             ; -fno-inline beside it. BOTH REPEAT, and both are just appended to one comma list
             ; -- gen.l's irblob splits it and reads a leading `-` as the subtraction, so the
             ; CLI has two flags and the compiler one string. Positives collect first, then the
             ; negatives cut; with no positive at all the set opens, so -fno-ir= alone means
             ; everything but. It is a debugging instrument before it is a JIT input: the
             ; compiler's IR beside the code it emitted, for whoever opens the binary.
             ; ⚠ both ahead of tol?, which swallows every other -f.
             ; -freadme=FILE -- plain text into the output's `.README` section, mapped by
             ; nothing, for whoever OPENS the binary. The source is already in our artifact,
             ; so what a reader lacks is not more data but a page telling them where to stand.
             ; ⚠ a LINK flag: with -c there is no output ELF to carry it and it is ignored.
             (pfx? a "-freadme=")
              (: f (snip a 9 (tally a)) rt (uread f)
                 (? (two? rt) (flags r (pin t 'rdme <>rt))
                    (udie 1 ("cc: cannot open " + f))))
             (pfx? a "-fir=") (flags r (pin t 'fir (irjoin (peep t 'fir ()) (snip a 5 (tally a)))))
             (pfx? a "-fno-ir=") (flags r (pin t 'fir (irjoin (peep t 'fir ()) (irneg (snip a 8 (tally a))))))
             ; ⚠ BARE, and the algebra gives them their meaning rather than a rule doing it:
             ; `-fir` is the EMPTY POSITIVE, which every name has as a prefix, so it opens the
             ; set wherever it sits -- `-fir=` in front. `-fno-ir` is the empty NEGATIVE, and
             ; since the negatives carve after the positives whatever their order, it empties
             ; the set from anywhere -- `-fno-ir=` at the end, and it beats a later `-fir=`
             ; without anyone writing down that it wins. ⚠ they must be named HERE: `tol?`
             ; swallows the whole advisory -f family, so before this both were accepted in
             ; silence and answered a binary with no record at all.
             (a = "-fir") (flags r (pin t 'fir (irjoin (peep t 'fir ()) "")))
             (a = "-fno-ir") (flags r (pin t 'fir (irjoin (peep t 'fir ()) "-")))
             (glu? a 73) (flags r (fpush t 'is (snip a 2 (tally a))))
             (glu? a 68) (flags r (fpush t 'ds (snip a 2 (tally a))))
             (glu? a 116) (flags r (pin t 'tgt (totgt (snip a 2 (tally a)))))
             ; -nostdinc is the INCLUDE half of -nostdlib's word, and it is LOUD
             ; rather than advisory: with the /usr/include tail still on, a header
             ; we simply do not carry resolves to GLIBC's instead, and a
             ; freestanding build quietly taking a hosted declaration is the wrong
             ; artifact wearing a green face. (it reaches incload as nsi.)
             (a = "-nostdinc") (flags r (pin t 'nsi 1))
             ; -ffreestanding is the one member of the -nostd* family that says
             ; something about the LANGUAGE and not just the link: it makes the
             ; implementation freestanding, and __STDC_HOSTED__ is how the
             ; standard lets a source ask. love.c asks exactly that, to choose
             ; the W^X mmap arena over the freestanding heap copy -- so without
             ; this a kernel build took the hosted lane and reached for sysconf.
             ; it rides as a DEFINE (cpp applies -D after its predefines, so this
             ; overrides). ⚠ NOT the rest of the family: -nostdlib alone is a
             ; hosted program that simply supplies its own runtime, and test_raw
             ; is exactly that.
             (a = "-ffreestanding")
              (flags r (pin (fpush t 'ds "__STDC_HOSTED__=0") 'nostd 1))
             (nostd? a) (flags r (pin t 'nostd 1))
             (tol? a) (flags r t)
             ((0 < tally a) && (a 0 = 45)) (udie 2 usage)
             (flags r (fpush t 'ins a)))))
   ; EVERY cc SAYS ITS SENTENCE, the internal ones too: gen and holo raise noms
   ; (musttail-not-a-tail, cs-unsaved, link-undef) that would otherwise reach the reader
   ; as a bare love debug note -- no cc: prefix, nothing naming the input. it goes on
   ; here rather than at the seat so every door below says the same thing.
   ; moon-run is the IN-IMAGE door: it ANSWERS the status charm, so a caller holding
   ; this image compiles again after a failed compile instead of dying with it -- the
   ; warm-compiler lane, where the cat is read once and every cc after it is free.
   ; ⚠ 'leave is the exit door, not a fault (crew/kore/u.l's floor, which a udie
   ; anywhere in here rides out on): taking it as an internal raise would print a
   ; nonsense sentence over every usage error and flatten its 2 to a 1.
   (moon-run as)
    (trap (\ x (: r (moon-cc x) (? (charm? r) r 0))) as
          (\ a b (? (id? a 'leave) b
                    (say err ("cc: internal error: " + show a + " " + show b + "\n"),
                     flush err,
                     1))))
   ; moon-main is the SHIMS' door and quits, exactly as it always has -- the six -e
   ; drives and svalbard's verb table call it straight, and a compile ends the same way
   ; through all of them.
   (moon-main as) (quit (moon-run as))
   (moon-cc as)
    ; the default target is the ground we stand on: love-arch (the C boot's pin)
    ; names the running binary's arch, so a native mooncc emits native code and
    ; -t stays what it always was, the CROSS flag. unknown grounds keep x64.
    (: sd (std-take as)
       dia <sd
       as9 <>sd                                    ; the line with -std= lifted off it
       ntv (? (love-arch = "arm64") 'arm64 (love-arch = "riscv64") 'riscv64 'x64)
       f (flags as9 (fseed ntv dia))       ; the flag set, seeded whole
       ; ⚠ NORMALISE IN THE TABLET, not at a reader. -I dirs want a trailing slash, and the
       ; consumers now take `f` itself -- so a `dirn` applied on the way out of one reader
       ; would leave every other consumer holding the raw list. one pin, before anything reads.
       _ (pin f 'is (map dirn (peep f 'is ())))
       c (peep f 'c ()) o (peep f 'o ()) ins (peep f 'ins ())
       ; TWO outcomes now, the cc shape: -c lays objects, anything else LINKS.
       ; one .c used to take a third path (cc-exe: elf64 straight off the IR,
       ; no crt0 and no linker) which could not bind a libc -- so `mooncc
       ; hello.c -o hello`, the most canonical cc line there is and what every
       ; configure script writes, answered undef-label printf. rung 3 gave us a
       ; linker; one exe path is the whole point of having it.
       ; ⚠ the arms are ORDERED and two of them are not about `ins` at all, so the scrutinee
       ; carries flag guards beside its shapes -- `(? g)` fits anything g holds for, and a
       ; guard that fails falls through, which is the whole reason this can be one form.
       ; What the match buys is the pair: `[a b]` IS "exactly two inputs", where the test
       ; spelled it (two? >ins && !(two? >>ins)) and then read both back out by hand.
       (ins @
        []      (udie 2 usage)
        (? o)   (? c (? (two? >ins) (udie 2 "cc: -o takes one input")
                        (: _ (cc-obj <ins o f) 0))
                   (: _ (cc-link ins o f) 0))
        ; the legacy pair: exactly two bare args, the second no .c -- IN OUT
        ; (a .c/.o mix is a LINK, never a pair; -c keeps its .o OUT). ⚠ and a
        ; .a is never an OUT: `mooncc a.o libgcc.a` reading as a pair would
        ; write the library it was asked to read FROM.
        (? (!(dotc? b) && !(dota? a) && !(dota? b)
            && (c || (!(doto? a) && !(doto? b)))) [a b])
                (: _ (? c (cc-obj a b f)
                          (cc-link [a] b f)) 0)
        (? c)   (: (go l) (? !(two? l) 0
                             !(dotc? <l) (udie 2 ("cc: not a .c input: " + <l))
                             (: _ (cc-obj <l (obase <l) f) (go >l)))
                   (go ins))
                (: _ (cc-link ins "a.out" f) 0))))

; the SEAT: fire moon-main iff mooncc sits in the program seat of the real command line
; -- the positional program, or a `-l` preload (the installed shebang) -- taking the
; args AFTER the seat. `cmdline` is the full C argv, read the same in both load orders.
; mooncc is its OWN app (its own catted shebang script, doc/moon.md): it does NOT ride the
; au multi-call cat, so a cc edit rebuilds only mooncc, never au. matching mooncc/moon.l here
; keeps the seat quiet under any other program name.
; and the VERB, pinned HERE rather than by whoever cats this file: the app owns its own
; name, so every image carrying moon.l answers `love mooncc ..` and the baked mooncc.image
; is a compiler without an `-e` shim to make it one. FN takes the args after the name and
; answers a status charm -- moon-main quits instead, which is the same contract kept early.
; ⚠ `cc` IS THE SAME ROW. A Makefile's default CC is `cc`, so without this word an
; artifact carrying a whole compiler still could not answer the name every recipe
; reaches for -- and `CC=cc` on a box with no toolchain found nothing at all.
(: _ (pins (from 'verbs 'tab) 'mooncc moon-main 'cc moon-main)
   s ((from 'verbs 'fire) ["mooncc" "mooncc.l" "moon.l"] >cmdline)
   (? (two? s) (moon-main >s) ()))
