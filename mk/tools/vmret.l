#!/usr/bin/env l
; vmret.l -- flag lvm_* VM-instruction aps that contain a `ret`.
;
; Port of mk/tools/vmret.py (see it for the full rationale). The l VM is
; threaded: every lvm_* opcode ap should tail-jump to the next, never
; emit an ordinary `ret`. This tool disassembles an ELF (shelling out to
; objdump / llvm-objdump via the `hark` nif) and reports every lvm_* function
; whose body still contains a `ret` -- a first-pass heuristic, expect false
; positives. It is cross-arch: it reads e_machine and picks a per-arch return
; mnemonic, preferring multi-target llvm-objdump for non-x86 images.
;
; Usage: l vmret.l ELF [--prefix PREFIX] [--objdump PATH]
; Exit:  0 = all clean, 1 = suspects found, 2 = usage/tool error.
;
; This needs the host-only `hark`/`getenv` nifs (subprocess + environment).

(: (die m)   (: _ (say err "vmret: ") _ (say err m) _ (say err "\n") (quit 2))
   (usage 0) (: _ (say out "usage: vmret.l ELF [--prefix PREFIX] [--objdump PATH]\n") (quit 0)))

; --- character classes + cursors over a line (string s) --------------
; ws = Python \s within a line (space tab cr ff vtab); hex = [0-9a-fA-F].
(: (ws?  c) (any (\ x (= x c)) `(32 9 13 12 11))
   (hex? c) (? (&& (>= c 48)  (<= c 57))  1
            (? (&& (>= c 97)  (<= c 102)) 1
               (&& (>= c 65)  (<= c 70))))
   (skip-ws    s i) (? (< i (tally s)) (? (ws?  (peep s i 0)) (skip-ws    s (+ i 1)) i) i)
   (skip-hex   s i) (? (< i (tally s)) (? (hex? (peep s i 0)) (skip-hex   s (+ i 1)) i) i)
   (skip-nonws s i) (? (< i (tally s)) (? (ws?  (peep s i 0)) i (skip-nonws s (+ i 1))) i)
   ; one past the last non-ws char (0 if the line is all whitespace).
   (trim-end s) ((: (f i e) (? (< i (tally s)) (f (+ i 1) (? (ws? (peep s i 0)) e (+ i 1))) e)) 0 0)
   (prefix? p s) (= p (snip s 0 (tally p)))
   (dash? a) (&& (< 0 (tally a)) (= 45 (peep a 0 0))))

; --- line classifiers (the three vmret.py regexes, by hand) -----------
; HEADER_RE  ^\s*[0-9a-fA-F]+\s+<(.+)>:\s*$  -> the captured name, else 0.
(: (parse-header line)
   (: i (skip-ws line 0)
      j (skip-hex line i)
      (? (= j i) 0                                       ; need >=1 hex digit
         (: k (skip-ws line j)
            (? (= k j) 0                                  ; need >=1 space
               (? (= 60 (peep line k 0))                   ; '<'
                  (: e (trim-end line)
                     (? (&& (>= e (+ k 4))                 ; room for "<x>:"
                            (&& (= 58 (peep line (- e 1) 0)) ; ':' at end
                                (= 62 (peep line (- e 2) 0)))) ; '>' before it
                        (snip line (+ k 1) (- e 2))         ; the (.+) name
                        0))
                  0))))))

; INSN_RE  ^\s*([0-9a-fA-F]+):\s+(.*)$  -> (addr . text), else 0.
(: (parse-insn line)
   (: i (skip-ws line 0)
      j (skip-hex line i)
      (? (&& (> j i) (= 58 (peep line j 0)))               ; hexrun then ':'
         (: k (skip-ws line (+ j 1))
            (? (> k (+ j 1))                                ; need >=1 ws after ':'
               (link (snip line i j) (snip line k (tally line)))
               0))
         0)))

; --- mnemonic = first whitespace token that isn't an x86 prefix -------
(: PREFIXES `("rep" "repz" "repe" "repnz" "repne" "lock" "bnd" "notrack" "data16")
   (mnemonic text)
   ((: (f i)
       (: ts (skip-ws text i)
          (? (< ts (tally text))
             (: te (skip-nonws text ts)
                w  (snip text ts te)
                (? (any (\ p (= p w)) PREFIXES) (f te) w))
             "")))
    0))

; --- per-arch return-mnemonic matchers --------------------------------
; x86: ^l?ret[a-z]*$ (ret/retq/lret/...; not iret). arm: ^ret(aa|ab)?$
; (iret/eret never match).
(: (lower? s i) (? (< i (tally s)) (: c (peep s i 0) (? (&& (>= c 97) (<= c 122)) (lower? s (+ i 1)) 0)) 1)
   (ret-tail? r) (&& (prefix? "ret" r) (lower? r 3))
   (retx?  m) (|| (ret-tail? m) (&& (= 108 (peep m 0 0)) (ret-tail? (snip m 1 (tally m)))))
   (reta?  m) (|| (= m "ret") (|| (= m "retaa") (= m "retab"))))

; --- split a string on '\n' into a list of lines ---------------------
(: (lines s)
   ((: (f start i)
       (? (< i (tally s))
          (? (= 10 (peep s i 0)) (link (snip s start i) (f (+ i 1) (+ i 1))) (f start (+ i 1)))
          (? (< start i) `((snip s start i)) 0)))
    0 0))

; --- ELF64 symbol sizes: bound each function to its real extent -------
; The text scan charges any `ret` in inter-function padding to the nearest
; preceding <name>: header (e.g. a stray aligned `ret` after a clean tail-jump).
; Reading each FUNC symbol's st_size from .symtab lets us drop instructions past
; a function's [start, start+size). ELF64 LE is asserted in the driver below.
; Addresses can be high-half (a kernel image links at 0xffffffff80000000), which
; overflows a fixnum -- so u64/BIG/hex->int use `+`/`*` (which promote to bignum),
; never bitwise `<<`/`|` (which would not), keeping the values exact.
(: (hex->int s) ((: (f i acc)
                    (? (< i (tally s))
                       (: c (peep s i 0)
                          v (? (&& (>= c 48) (<= c 57))  (- c 48)
                            (? (&& (>= c 97) (<= c 102)) (- c 87)
                            (? (&& (>= c 65) (<= c 70))  (- c 55) 0)))
                          (f (+ i 1) (+ (* acc 16) v)))
                       acc))
                 0 0)
   BIG (* 4294967296 4294967296)                          ; 2^64: above any real address
   (parse-header-addr line)                               ; the <addr> before <name>:, as an int
     (: i (skip-ws line 0) j (skip-hex line i)
        (? (= j i) 0 (hex->int (snip line i j))))
   (u8  d i) (peep d i 0)                                  ; little-endian fixed-width readers
   (u16 d i) (| (u8  d i) (<< (u8  d (+ i 1)) 8))
   (u32 d i) (| (u16 d i) (<< (u16 d (+ i 2)) 16))
   (u64 d i) (+ (u32 d i) (* (u32 d (+ i 4)) 4294967296))   ; `+`/`*` so a high-half addr promotes to bignum
   (find-symtab d base ent n k)                           ; SHT_SYMTAB (2) -> (off . size), else 0
     (? (< k n)
        (: sh (+ base (* k ent))
           (? (= 2 (u32 d (+ sh 4))) (link (u64 d (+ sh 24)) (u64 d (+ sh 32)))
              (find-symtab d base ent n (+ k 1))))
        0)
   (collect-syms d off n k m)                             ; Elf64_Sym (24 B): value -> size, sized FUNCs
     (? (< k n)
        (: p (+ off (* k 24)) info (u8 d (+ p 4)) val (u64 d (+ p 8)) sz (u64 d (+ p 16))
           (collect-syms d off n (+ k 1)
              (? (&& (= 2 (& info 15)) (< 0 sz)) (pin m val sz) m)))
        m)
   (symtab-sizes d)                                       ; -> map: fn start address -> byte size
     (: st (find-symtab d (u64 d 40) (u16 d 58) (u16 d 60) 0)  ; e_shoff / e_shentsize / e_shnum
        (? st (collect-syms d (cap st) (// (cup st) 24) 0 (tablet 0)) (tablet 0))))

; --- the scan (vmret.py scan(), grouping by function header) ----------
; close the current watched function: flag it iff it collected any rets.
(: (close name watching rets flagged)
   (? (&& watching (two? rets)) (link (link name (rev rets)) flagged) flagged))

; -> (flagged . total), where flagged = ((name . rets) ...), total = count of
; prefix* functions seen. prefix, the ret matcher, and a function-start -> size
; map are captured. lim = end of the current function's symtab extent; a `ret`
; at or past it is padding (an orphan in the gap before the next symbol), not
; the function's -- so it does not count. unsized symbols -> lim BIG (old behavior).
(: (scan-all ls prefix ret? sizes)
   ((: (loop ls name watching lim rets flagged total)
       (? (two? ls)
          (: line (cap ls)
             rest (cup ls)
             h    (parse-header line)
             (? h
                (? (prefix? "." h)                              ; local label: stay inside
                   (loop rest name watching lim rets flagged total)
                   (: fl2  (close name watching rets flagged)   ; real header: close, open new
                      w2   (prefix? prefix h)
                      ha   (parse-header-addr line)
                      sz   (peep sizes ha 0)                     ; symtab size, 0 if absent
                      lim2 (? (< 0 sz) (+ ha sz) BIG)           ; extent end, or unbounded
                      (loop rest h w2 lim2 0 fl2 (? w2 (+ total 1) total))))
                (: ins (? watching (parse-insn line) 0)         ; instruction: maybe a ret
                   (? (&& ins (< (hex->int (cap ins)) lim) (ret? (mnemonic (cup ins))))
                      (loop rest name watching lim (link (cap ins) rets) flagged total)
                      (loop rest name watching lim rets flagged total)))))
          (link (close name watching rets flagged) total)))
    ls 0 0 BIG 0 0 0))

; --- report helpers --------------------------------------------------
(: (basename p) ((: (f i last) (? (< i (tally p)) (f (+ i 1) (? (= 47 (peep p i 0)) (+ i 1) last)) (snip p last (tally p)))) 0 0)
   (spaces n)    (? (< 0 n) (+ " " (spaces (- n 1))) "")
   (str< a b)    ((: (f i) (? (>= i (tally a)) (< (tally a) (tally b))
                            (? (>= i (tally b)) 0
                               (: ca (peep a i 0) cb (peep b i 0)
                                  (? (< ca cb) 1 (? (> ca cb) 0 (f (+ i 1)))))))) 0)
   (maxlen fl)   (foldl (\ m e (: l (tally (cap e)) (? (< m l) l m))) 0 fl)
   (join-rets rs) (? (two? rs)
                     (? (two? (cup rs)) (+ (+ "0x" (cap rs)) (+ ", " (join-rets (cup rs))))
                                        (+ "0x" (cap rs)))
                     ""))

; sort the flagged entries by function name (ascending), matching .py.
; the merge split counts the SPINE (entries are chains; $ is the net sum), so fold.
(: (emerge a b) (? (two? a) (? (two? b)
                     (? (str< (cap (cap a)) (cap (cap b)))
                        (link (cap a) (emerge (cup a) b)) (link (cap b) (emerge a (cup b))))
                     a) b)
   (llen l) (foldl (\ n _ (+ 1 n)) 0 l)
   (esort l) (? (two? (cup l)) (: n (>> (llen l) 1) (emerge (esort (take n l)) (esort (drop n l)))) l))

; --- argument parsing ------------------------------------------------
(: (parse-args args path prefix od)
   (? (two? args)
      (: a (cap args)
         (? (= a "--prefix")
            (? (two? (cup args)) (parse-args (cup (cup args)) path (cap (cup args)) od) (die "missing argument for --prefix"))
         (? (= a "--objdump")
            (? (two? (cup args)) (parse-args (cup (cup args)) path prefix (cap (cup args))) (die "missing argument for --objdump"))
         (? (|| (= a "-h") (= a "--help")) (usage 0)
         (? (dash? a) (die (+ "unknown option: " a))
         (? path (die (+ "unexpected argument: " a))
            (parse-args (cup args) a prefix od)))))))
      `(path prefix od)))

; --- try candidate disassemblers in order ----------------------------
; (hark ...) returns (status . output) when the tool ran, else a fixnum errno
; (spawn failed -> try the next). -> (cand status . output) or 0 if exhausted.
(: (try-cands cs elf)
   (? (two? cs)
      (: r (hark `((cap cs) "-d" "--no-show-raw-insn" elf))
         (? (two? r) (link (cap cs) r) (try-cands (cup cs) elf)))
      0))

; --- driver ----------------------------------------------------------
(: parsed   (parse-args (cup argv) 0 "lvm_" 0)
   path     (cap parsed)
   prefix   (cap (cup parsed))
   od       (cap (cup (cup parsed)))
   _        (? path 0 (die "usage: vmret.l ELF [--prefix PREFIX] [--objdump PATH]"))
   base     (basename path)
   port     (open path "r")
   _        (? port 0 (die (+ "cannot open " path)))
   hdr      (slurp port)                                 ; whole image: e_ident/e_machine + .symtab
   _        (close port)
   _        (? (&& (= 127 (peep hdr 0 0)) (&& (= 69 (peep hdr 1 0)) (&& (= 76 (peep hdr 2 0)) (= 70 (peep hdr 3 0))))) 0
               (die (+ path ": not an ELF file")))
   _        (? (&& (= 2 (peep hdr 4 0)) (= 1 (peep hdr 5 0))) 0
               (die (+ path ": not ELF64 little-endian")))
   sizes    (symtab-sizes hdr)                           ; function start address -> byte size
   mach     (| (peep hdr 18 0) (<< (peep hdr 19 0) 8))
   x86?     (= mach 62)
   known?   (|| x86? (= mach 183))
   ret?     (? x86? retx? (? (= mach 183) reta? retx?))
   _        (? known? 0
               (: _ (say err "vmret: note: ") _ (say err base)
                  _ (say err " is an unrecognized arch (0x") _ (putbn err mach 16)
                  _ (say err "); falling back to the generic `ret` matcher\n") 0))
   ; objdump preference: GNU objdump for x86 (host-arch), llvm-objdump for the
   ; cross arches; an explicit --objdump or $OBJDUMP wins. execvp does the PATH
   ; search, so a candidate that isn't found just spawn-fails -> next one.
   cands    (filter id `(od (getenv "OBJDUMP")
                          (? x86? "objdump" "llvm-objdump") (? x86? "llvm-objdump" "objdump")))
   chosen   (try-cands cands path)
   _        (? chosen 0 (die "no disassembler found (tried llvm-objdump, objdump); pass --objdump"))
   status   (cap (cup chosen))
   output   (cup (cup chosen))
   _        (? (= 0 status) 0 (die (+ (+ (cap chosen) " failed: status ") (show status))))
   scanned  (scan-all (lines output) prefix ret? sizes)
   flagged  (cap scanned)
   total    (cup scanned)
   ; --- the report (byte-for-byte the same shape as vmret.py) ---
   (? (= 0 total)
      (: _ (say err "vmret: no ") _ (say err prefix) _ (say err "* functions found in ")
         _ (say err base) _ (say err " (stripped binary?)\n") (quit 2))
   (? (two? flagged)
      (: k      (llen flagged)
         width  (maxlen flagged)
         sorted (esort flagged)
         _ (say out base) _ (say out ": ") _ (putbn out k 10) _ (say out " of ")
         _ (putbn out total 10) _ (say out " ") _ (say out prefix)
         _ (say out "* functions contain a ret (TCO suspects):\n")
         _ (each sorted (\ e
              (: name (cap e) rets (cup e)
                 _ (say out "  ") _ (say out name) _ (say out (spaces (- width (tally name))))
                 _ (say out "  ret @ ") _ (say out (join-rets rets)) _ (put out 10) 0)))
         (quit 1))
      (: _ (say out base) _ (say out ": all ") _ (putbn out total 10) _ (say out " ")
         _ (say out prefix) _ (say out "* functions are ret-free\n") (quit 0)))))
