; lib/splice.l -- the template JIT over the tail-threaded VM, as a door rather than a
; pipeline. `dis` reads a compiled closure back to its op rows, each row's op contributes
; its OWN C body with the dispatch deleted, mooncc compiles the one function that makes,
; `jit-bind` links it against THIS process's live symbols and `nif` installs it. `(use 'splice)`:
;
;   (jit f)        -> a native closure agreeing with f, or () when a row is uncovered
;   (jit-c f)      -> the composed C text | ()        -- exactly what the compiler is handed
;   (jit-bind obj) -> a cask of process-bound bytes | ()   -- an .o aimed at live addresses
;   (jit-root p)   -> pin the tree the op bodies are read from ("" = the cwd)
;
; ⚠ IN-PROCESS BY CONSTRUCTION. The bind resolves against /proc/self/exe's symtab plus
; this process's own load bias, so the bytes run in the love that made them and nowhere
; else -- a separate process has a different ASLR base. That is what a JIT is, and it is
; why none of this can be written out for later (bench/vmsplice/lift.l is the address-free
; special case, and it refuses any body carrying a relocation).
; ⚠ TWO SOURCES FOR ONE FACT, and only one of them ships. `jit` today lowers each op from
; its C body, read out of LOVE.C'S TEXT (a macro argument) and handed to mooncc -- so it needs
; the compiler in the image AND the source tree beside the cwd. `jit-irtab` is the successor
; and is already live: `mooncc -fir=lvm_` writes every op handler's machine-form IR into this
; binary's own .rodata, so a plain love reads what it is made of and holo assembles it back --
; no compiler, no source, no disassembler. What stands between the two is the SPLICER (see
; `jit-ir`'s note on the guard hoist); until it lands, `jit` keeps the C route and `jit-irtab`
; is what the gate proves.
; ⚠ LOVE_NO_GLAZE=1 while jitting: a glazed closure is a native cell, and `dis` reads its
; husk. The glaze is the specializing tier this one sits UNDER, not its competition.
;
; Covered today: the load family (arg/argN/quote-of-charm/quoN/quomN, the aa..qq pairs),
; the fld consumers (argcap/argcup/argtwo) and every op11/op-declared nif body -- the
; call-free ones over love.h's public accessor macros. An op that jumps, applies or
; allocates ends the straight line and DECLINES the closure by name; that report is what
; prices the next rung (doc/moon-regalloc.md, the splice client).
(:
   jit-memo (tablet 0)                               ; the harvested tables + the work counter
   (jit-say m) (: _ (say err ("splice: " + m + "\n")) ())
   (jit-root p) (pin jit-memo 'root p)               ; the tree love.c is read from
   (jit-tree _) (peep jit-memo 'root "")
   (jit-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) s) ()))

   ; --- tiny text kit (the op bodies are C source, so this reads text) ---------------
   (idxof s sub from) (: n (tally s) m (tally sub)
    (: (go i) (? ((i + m) > n) -1
                 (: (eq j) (? (j = m) 1 (? ((peep s (i + j) 0) = (peep sub j 0)) (eq (j + 1)) 0))
                    (? (eq 0) i (go (i + 1)))))
       (go from)))
   (s2n s) (: (go i a) (? (i < (tally s)) (: c (peep s i 0)
                            (? (&& (47 < c) (c < 58)) (go (i + 1) ((a * 10) + (c - 48))) a)) a)
             (go 0 0))
   (ws? c) (|| (c = 32) (|| (c = 10) (c = 9)))
   (trimt s) (: a (: (go i) (? (&& (i < (tally s)) (ws? (peep s i 0))) (go (i + 1)) i) (go 0))
                b (: (go i) (? (&& (a < i) (ws? (peep s (i - 1) 0))) (go (i - 1)) i) (go (tally s)))
                (snip s a b))
   ; one macro argument: from i, the index of the top-level `,` or the macro's own `)`.
   ; char literals ('X') are skipped; these bodies carry no strings.
   (arg1 s i) (: (go j d) (: c (peep s j 0)
                  (? (c = 39) (go (j + 3) d)
                     (c = 40) (go (j + 1) (d + 1))
                     (c = 41) (? (d = 0) j (go (j + 1) (d - 1)))
                     (&& (c = 44) (d = 0)) j
                     (go (j + 1) d)))
                 (go i 0))

   ; --- the op table: love.c's op11(nom, x) / op(nom, n, x) / op1(nom, i, x) / fld(nom, val).
   ; read ONCE and memoized -- a jit that re-slurped 600 KB per closure would be measuring
   ; its own table build. ⚠ presence rides the (1 _) wrapper: an unreadable love.c and an
   ; empty one are the same value, and only one of them is a broken install.
   (jit-src nm key) (: h (peep jit-memo key ())
    (? (two? h) h
       (: s (jit-slurp (jit-tree () + nm))
          (? (string? s) (: v (L 1 s) _ (pin jit-memo key v) v) ()))))
   (jit-lovec _) (jit-src "love.c" 'lovec)
   (jit-nifh _) (jit-src "nifs.h" 'nifh)
   (harv lovec pat nargs tag) (: pl (tally pat)
    (: (go i acc) (: j (idxof lovec pat i)
        (? (j < 0) acc
           (: a0 (j + pl)
              e0 (arg1 lovec a0) nm (name (trimt (snip lovec a0 e0)))
              a1 (e0 + 1)
              e1 (arg1 lovec a1) x1 (trimt (snip lovec a1 e1))
              (? (nargs = 2) (go (j + pl) ((L nm tag x1) >< acc))
                 (: a2 (e1 + 1) e2 (arg1 lovec a2) x2 (trimt (snip lovec a2 e2))
                    (go (j + pl) ((L nm tag x1 x2) >< acc)))))))
       (go 0 ())))
   (jit-optab _) (: t (peep jit-memo 'optab ())
    (? (two? t) t
       (: lc (jit-lovec ())
          (? !(two? lc) ()
             (: lovec (caup lc)
                v (((harv lovec "\nop11(" 2 'e1) + (harv lovec " op11(" 2 'e1)) +
                   (((harv lovec "\nfld(" 2 'fld) + (harv lovec "\nop(" 3 'opn)) +
                    ((harv lovec " op(" 3 'opn) + (harv lovec "\nop1(" 3 'ipn))))
                _ (pin jit-memo 'optab v) v)))))
   (findop nm) (assoc nm (jit-optab ()))

   ; --- nifs.h: "name" -> lvm nom (a def1 row aims at a nifs[] run; word 0 = the code) --
   (jit-nrows _) (: r (peep jit-memo 'nrows ())
    (? (two? r) r
       (: nh (jit-nifh ())
          (? !(two? nh) ()
             (: nifh (caup nh)
                nifend (idxof nifh "};" 0)
                v (: (go i acc) (: j (idxof nifh "\n {" i)
                       (? (|| (j < 0) (j > nifend)) (rev acc)
                          (: k (j + 3)
                             e (idxof nifh "}" k)
                             (go (j + 3) ((trimt (snip nifh k e)) >< acc)))))
                     (go 0 ()))
                _ (pin jit-memo 'nrows v) v)))))
   (nrow i) (: (go l k) (? (atom? l) "" (k = i) (cap l) (go (cup l) (k + 1))) (go (jit-nrows ()) 0))
   (lvmof nm) (: nh (jit-nifh ())
    (? !(two? nh) ()
       (: nifh (caup nh)
          pat (+ "{ \"" (+ (string nm) "\","))
          j (idxof nifh pat 0)
          (? (j < 0) ()
             (: k (idxof nifh "nifs + " j)
                (? (k < 0) ()
                   (: r (nrow (s2n (snip nifh (k + 7) (k + 17))))
                      (? (= "" r) () (name r)))))))))

   ; --- the composer: one op row -> (stmt . push-delta), () = decline. a push is Sp -= 1.
   (num v) (show v)
   (ld i)  ("  Sp[-1] = Sp[" (num i) "]; Sp -= 1;\n")
   (ldq w) ("  Sp[-1] = (intptr_t) " (num w) "; Sp -= 1;\n")    ; a charm operand: its tagged word, 2v+1
   (cw v) ((2 * v) + 1)
   (ok2 os) (&& (two? os) (charm? (cap os)))
   (okk os) (&& ((tally os) = 2) (&& (charm? (cap os)) (charm? (caup os))))
   (say1 r) (: nm (cap r) os (cup r)
    (? (id? nm 'lvm_arg)  (? (ok2 os) ((ld (cap os)) >< 1) ())
       (id? nm 'lvm_arg0) ((ld 0) >< 1)  (id? nm 'lvm_arg1) ((ld 1) >< 1)
       (id? nm 'lvm_arg2) ((ld 2) >< 1)  (id? nm 'lvm_arg3) ((ld 3) >< 1)
       (id? nm 'lvm_quote) (? (ok2 os) ((ldq (cw (cap os))) >< 1) ())
       (id? nm 'lvm_quo0) ((ldq 1) >< 1)   (id? nm 'lvm_quo1) ((ldq 3) >< 1)
       (id? nm 'lvm_quo2) ((ldq 5) >< 1)   (id? nm 'lvm_quo3) ((ldq 7) >< 1)
       (id? nm 'lvm_quom1) ((ldq -1) >< 1) (id? nm 'lvm_quom2) ((ldq -3) >< 1)
       (id? nm 'lvm_aa) (? (okk os) (((ld (cap os)) (ld (caup os))) >< 2) ())
       (id? nm 'lvm_qq) (? (okk os) (((ldq (cw (cap os))) (ldq (cw (caup os)))) >< 2) ())
       (id? nm 'lvm_aq) (? (okk os) (((ld (cap os)) (ldq (cw (caup os)))) >< 2) ())
       (id? nm 'lvm_qa) (? (okk os) (((ldq (cw (cap os))) (ld (caup os))) >< 2) ())
       (: cnm (? (= "lvm_" (snip (string nm) 0 4)) nm (lvmof nm))
          (? (nil? cnm) ()
             (: e (findop cnm)
                (? (nil? e) ()
                   (: kind (caup e) x1 (caup (cup e))
                      (? (id? kind 'fld)
                          (? (ok2 os)
                             ((("  { intptr_t v = Sp[" (num (cap os)) "]; Sp[-1] = (") (x1 "); Sp -= 1; }\n")) >< 1)
                             ())
                         (id? kind 'e1)
                          ((("  Sp[0] = (" x1 ");\n")) >< 0)
                         (id? kind 'opn)
                          (: n (s2n x1) x2 (caup (cuup e))
                             (? (n = 1) ((("  Sp[0] = (" x2 ");\n")) >< 0)
                                ((("  { intptr_t _v = (" x2 "); Sp += ") ((num (n - 1)) "; Sp[0] = _v; }\n"))
                                 >< (0 - (n - 1)))))
                         ()))))))))
   ; walk the rows to (stmts peak net), or () with the first uncovered nom said
   (build rs stmts pk net) (?
    (atom? rs) ()                                            ; ran out with no ret -> decline
    (: r (cap rs) nm (cap r)
     (? (id? nm 'lvm_ret) (L stmts pk net)
        (: p (say1 r)
           (? (nil? p) (jit-say ("uncovered op " + show nm))
              (: net2 (net + (cup p))
                 (build (cup rs) (stmts (cap p)) (? (pk < net2) net2 pk) net2)))))))

   ; the composed function: a room guard up front, the glaze's deopt-to-twin (Ip = cell(Ip[1].x)
   ; BEFORE Sp moves, so the twin sees an untouched frame), the bodies, the nif-cell epilogue.
   (jit-c f) (: rows (? (member? 'dis (names ())) (dis f) ())
    (? (atom? rows) (jit-say "dis answers nothing (is this closure already native?)")
       (id? (caap rows) 'lvm_cur) (jit-say "multi-arg closures are not composed yet")
       !(two? (jit-optab ())) (jit-say ("no op table -- love.c is not beside " +
                                        (? (= "" (jit-tree ())) "the cwd" (jit-tree ()))))
       (: b (build rows "" 0 0)
          (? (nil? b) ()
             (: stmts (cap b) pk (caup b) net (caup (cup b))
                ("// composed by lib/splice.l -- do not edit\n#include \"love.h\"\n"
                 + "struct ai *jitbody(struct ai *restrict g, union u *Ip, ai_word *Hp, ai_word *restrict Sp) {\n"
                 + (? (0 < pk) (("  if (Sp < Hp + " (num pk) " + ai_avail_floor)")
                                " { Ip = cell(Ip[1].x); ai_musttail return Continue(); }\n") "")
                 + stmts
                 + (? (0 < net) (("  Sp[" (num net) "] = Sp[0]; Sp += ") ((num net) ";\n")) "")
                 + "  Ip += 2;\n  ai_musttail return Continue(); }\n"))))))

   ; --- WHAT THE COMPILER WROTE DOWN: this binary's own op handlers, as IR ---------------
   ; mooncc -fir=lvm_ lays the machine-form IR of every op handler under the splice cap into
   ; .rodata as `ai_lvm_ir`, one readable datum. So the JIT's source of truth is the BINARY
   ; IT IS RUNNING IN -- not love.c's text, which a shipped love does not carry, and not a
   ; disassembly, which would hand back bytes it could not reason about. holo assembles these
   ; forms as they stand; that is what they are.
   ; ⚠ a love built by another compiler has no `ai_lvm_ir` and this answers () -- which is the
   ; honest report, not a failure: gcc was never asked to write anything down.
   (irsym-off exe nm)                                ; a symbol's FILE offset in our own exe | ()
    (: (le i n) (bind-le exe i n)
       shoff (le 40 8) shentsz (le 58 2) shnum (le 60 2)
       (sh i f w) (le (shoff + ((i * shentsz) + f)) w)
       symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
       (? (symi < 0) ()
          (: stroff (sh (sh symi 40 4) 24 8)
             symoff (sh symi 24 8) syment (sh symi 56 8) nsyms ((sh symi 32 8) // syment)
             (cstr i) (: (go j) (? (= 0 (peep exe j 0)) (snip exe i j) (go (j + 1))) (go i))
             (go i) (? (i = nsyms) ()
                       (: o (symoff + (i * syment))
                          (? (= nm (cstr (stroff + (le o 4))))
                             (: va (le (o + 8) 8) sx (le (o + 6) 2)   ; st_value, st_shndx
                                ((sh sx 24 8) + (va - (sh sx 16 8))))  ; sh_offset + (va - sh_addr)
                             (go (i + 1)))))
             (go 0))))
   (jit-irtab _)                                     ; nom -> its forms | () when nothing wrote it down
    (: t (peep jit-memo 'irtab ())
       (? (two? t) t
          (: exe (bind-rd "/proc/self/exe")
             off (irsym-off exe "ai_lvm_ir")
             (? (nil? off) (jit-say "this love carries no ai_lvm_ir (built without -fir=lvm_)")
                (: txt (: (go j) (? (= 0 (peep exe j 0)) (snip exe off j) (go (j + 1))) (go off))
                   r (sound txt)
                   rows (? (two? r) (cap r) ())
                   v (: tb (tablet 0)
                        _ (map (\ w (pin tb (cap w) (cup w))) rows)
                        (L 1 tb (tally rows)))
                   _ (pin jit-memo 'irtab v) v)))))
   ; one handler's forms | (). ⚠ WHAT THE SPLICER OWES, and the trap is worth the sentence:
   ; a handler's tail is the fixed dispatch triple (ld r0 r5 8)(add r5 r5 8)(jmpr r0) and comes
   ; off every copy but the last, whose bump becomes the whole segment's. Internal labels are
   ; renamed per copy; `la`/`call` targets bind to LIVE addresses through assemble-at's seeded
   ; label table, so a runtime splice needs no relocation at all -- it knows the answers.
   ; ⚠ THE ONE THING THAT IS NOT MECHANICAL: a handler carries its own room guard, and that
   ; guard jumps to lvm_gc, which RESUMES AT Ip -- which in a spliced body is the nif cell, so
   ; a collection halfway through would re-run the ops that already ran. The requirements must
   ; be summed and hoisted to one guard at the top, ahead of any Sp motion, deopting to the
   ; twin exactly as the C route's guard does. A per-op guard left in place is a silent
   ; double-apply, not a crash.
   (jit-ir nm) (: t (jit-irtab ()) (? (two? t) (peep (caup t) nm ()) ()))

   ; --- the bind: a composed .o -> a cask of bytes aimed at THIS process's symbols ------
   ; mooncc emits an external ref as a 7-byte `lea r,[rip+d32]` (R_X86_64_PC32). flip it to
   ; a same-length `mov r,[rip+d32]` aimed at an 8-byte cell APPENDED to the blob and holding
   ; the symbol's live address -- every ref is then blob-internal, so nif's mmap can land the
   ; bytes anywhere. we own both sides: the symbols come from our own binary's symtab, the
   ; load bias from this process's own maps line. a reloc not wearing the lea shape refuses by
   ; name -- a CALL (ai_please, once allocating ops are covered) is a later rung, same appendix.
   (bind-die m) (: _ (jit-say m) (welp '(bind)))
   (bind-rd p) (: s (jit-slurp p) (? (string? s) s (bind-die ("cannot open " + p))))
   (bind-le s i n) (: (go k a m) (? (k = n) a (go (k + 1) (a + ((peep s (i + k) 0) * m)) (m * 256))) (go 0 0 1))
   (bind-live-tab _) (: exe (bind-rd "/proc/self/exe")
      shoff (bind-le exe 40 8) shentsz (bind-le exe 58 2) shnum (bind-le exe 60 2)
      (sh i f w) (bind-le exe (shoff + ((i * shentsz) + f)) w)
      symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
      _ (? (0 <= symi) 0 (bind-die "no symtab in /proc/self/exe"))
      stroff (sh (sh symi 40 4) 24 8)
      symoff (sh symi 24 8) syment (sh symi 56 8) nsyms ((sh symi 32 8) // syment)
      (cstr i) (: (go j) (? (= 0 (peep exe j 0)) (snip exe i j) (go (j + 1))) (go i))
      (symva nm) (: (go i) (? (i = nsyms) -1
                     (: o (symoff + (i * syment))
                        (? (= nm (cstr (stroff + (bind-le exe o 4)))) (bind-le exe (o + 8) 8) (go (i + 1)))))
                   (go 0))
      exepath (? (member? 'readlink (names ())) (readlink "/proc/self/exe") (bind-die "no readlink nif"))
      base (: mp (bind-rd "/proc/self/maps") n (tally mp)
              (sp? j) (= 32 (peep mp j 0))
              (eol? j) (|| (= j n) (= 10 (peep mp j 0)))
              (nlend j) (: (go e) (? (eol? e) e (go (e + 1))) (go j))
              (skipsp j) (: (go e) (? (&& (< e n) (sp? e)) (go (e + 1)) e) (go j))
              (tokend j) (: (go e) (? (|| (eol? e) (sp? e)) e (go (e + 1))) (go j))
              (fld i f) (: (go j k) (: t (tokend j) (? (= k f) (j >< t) (go (skipsp t) (k + 1)))) (go i 0))
              (hex i e) (: (go j a) (: c (peep mp j 0)
                           (? (>= j e) a
                              (&& (47 < c) (c < 58)) (go (j + 1) ((a * 16) + (c - 48)))
                              (&& (96 < c) (c < 103)) (go (j + 1) ((a * 16) + (c - 87)))
                              a)) (go i 0))
              (lstart i) (: se (fld i 0) (hex (cap se) (tokend (cap se))))
              (loff i) (: se (fld i 2) (hex (cap se) (cup se)))
              (lpath i) (: se (fld i 5) (snip mp (cap se) (nlend (cap se))))
              (go i) (? (>= i n) (bind-die "exe not found in maps")
                        (&& (= 0 (loff i)) (= exepath (lpath i))) (lstart i)
                        (go (+ (nlend i) 1)))
              (go 0))
      (\ nm (: v (symva nm) (? (v < 0) (bind-die ("no live symbol " + nm)) (base + v)))))
   (jit-bind obj) (: s (bind-rd obj)
      ob ((from 'holo 'ld-read) 'bind s 62)
      secs (peep ob 'secs 0) syms (peep ob 'syms 0)
      (nth l i) (? (= i 0) (cap l) (nth (cup l) (i - 1)))
      tx (: (go ks) (? (atom? ks) () (: v (peep secs (cap ks) 0)
                      (? (id? (nth v 0) 'text) (link (cap ks) v) (go (cup ks))))) (go (keys secs)))
      _ (? tx 0 (bind-die "no .text"))
      tix (cap tx) toff (nth (cup tx) 1) tsz (nth (cup tx) 2)
      (symnm i) (: v (peep syms i 0) (? (tally v) (nth v 0) (bind-die ("no symbol " + show i))))
      rels (: (go rs acc) (? (atom? rs) acc (: r (cap rs)
                (go (cup rs) (? ((cap r) = tix) ((cup r) + acc) acc)))) (go (peep ob 'relas ()) ()))
      pad ((8 - (tsz % 8)) % 8)
      (celloff i) ((tsz + pad) + (8 * i))
      (asn nm cells) (: (go l i) (? (atom? l) (i >< ((nm >< i) >< cells))
                                    (id? nm (caap l)) ((cuap l) >< cells)
                                    (go (cup l) (i + 1))) (go cells 0))
      ov (tablet 0)
      fin (: (go rs cells) (? (atom? rs) (rev (map cap cells))
               (: r (cap rs) off (nth r 0) ty (nth r 1) si (nth r 2) ad (nth r 3)
                  _ (? (= ty 2) 0 (bind-die ("reloc type " + show ty + " not bound (want PC32)")))
                  op (peep s ((toff + off) - 2) 0)             ; d32<-ModRM(off-1)<-opcode(off-2)<-REX(off-3)
                  _ (? (= op 141) 0 (bind-die ("reloc at " + show off + " is not a lea (op " + show op + ")")))
                  ac (asn (name (symnm si)) cells) slot (cap ac) cells2 (cup ac)
                  d ((celloff slot) - (off + 4)) du (? (d < 0) (d + 4294967296) d)
                  _ (pin ov ((toff + off) - 2) 139)            ; lea -> mov (8d->8b); ModRM keeps the dest reg
                  _ (pin ov (toff + off) (du % 256))
                  _ (pin ov ((toff + off) + 1) ((du // 256) % 256))
                  _ (pin ov ((toff + off) + 2) ((du // 65536) % 256))
                  _ (pin ov ((toff + off) + 3) ((du // 16777216) % 256))
                  (go (cup rs) cells2))) (go rels ()))
      roster fin
      lv (bind-live-tab 0)
      txt (: (go i acc) (? (i < 0) acc (go (i - 1) ((peep ov (toff + i) (peep s (toff + i) 0)) >< acc))) (go (tsz - 1) ()))
      pd (: (go i acc) (? (i < 0) acc (go (i - 1) (144 >< acc))) (go (pad - 1) ()))
      cells (catmap (\ nm (: v (lv (string nm))
                             (go k w acc) (? (k = 8) (rev acc) (go (k + 1) (w // 256) ((w % 256) >< acc)))
                             (go 0 v ()))) roster)
      (cask (txt + (pd + cells))))

   ; --- the door -----------------------------------------------------------------------
   ; one work stem per jit, in a per-PROCESS directory: two loves jitting at once must not
   ; hand each other half an object, and the pid is the only name that cannot collide.
   (jit-dir _) (: d (peep jit-memo 'dir ())
    (? (string? d) d
       (: p ("/tmp/love-splice-" (show (getpid ())))
          _ (mkdir p 448)                            ; 0700; an existing one answers its own error, which is fine
          _ (pin jit-memo 'dir p) p)))
   (jit-stem _) (: n (peep jit-memo 'n 0) _ (pin jit-memo 'n (n + 1))
                   ((jit-dir ()) + "/j" + show n))
   (jit-write p s) (: q (open p "w") (? q (: _ (say q s) _ (close q) 1) ()))
   (jit f)
    (: c (jit-c f)
       (? (nil? c) ()
          !(member? 'moon-run (names ()))
           (jit-say "no compiler in this image -- wake one that carries the moon cat (mooncc.image)")
          (: stem (jit-stem ())
             cp (stem + ".c") op (stem + ".o")
             _ (? (jit-write cp c) 0 (jit-say ("cannot write " + cp)))
             st (moon-run (L ("-I" + (? (= "" (jit-tree ())) "." (jit-tree ()))) "-c" cp op))
             _ (unlink cp)
             (? !(st = 0) (jit-say "the compile refused the composed body")
                (: code (jit-bind op)
                   _ (unlink op)
                   (? !(hot? code) (jit-say "the bind produced no code")
                      (: nf (from 'glaze 'nif)
                         (? (nil? nf) (jit-say "no glaze nif on this build")
                            (: j (nf code f (L '\ 'x 'x) 1)
                               (? (nil? j) (jit-say "nif declined the bytes") j))))))))))
   )
