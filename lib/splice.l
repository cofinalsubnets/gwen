; lib/splice.l -- the template JIT over the tail-threaded VM. `dis` reads a compiled closure
; back to its op rows, each row's op contributes its OWN machine-form IR -- read out of THIS
; BINARY's .rodata, where the compiler wrote it down -- the dispatch between them is deleted,
; holo assembles the one function that makes, and `nif` installs it. `(use 'splice)`:
;
;   (jit f)        -> a native closure agreeing with f, or () naming the row it could not say
;   (jit-forms f)  -> the IR it would assemble | ()        -- what the splicer built
;   (jit-ir nom)   -> one handler's own IR | ()            -- what the compiler wrote down
;   (jit-irtab ()) -> (1 table count) | ()                 -- every handler this binary carries
;
; ⚠ NO COMPILER, NO SOURCE TREE, NO OBJECT FILE. `mooncc -fir=lvm_` lays each op handler's IR
; into `ai_lvm_ir` at compile time and nifs.l lays the book-name bridge beside it, so a plain
; love reads what it is made of. That is the whole design: the alternative was scraping op
; bodies out of love.c's TEXT (which a shipped love does not carry) or disassembling our own
; machine code (which would hand back bytes the splicer could not reason about). These forms
; are what the compiler emitted, so holo assembles them as they stand.
; ⚠ IN-PROCESS BY CONSTRUCTION: the one external reference a handler carries becomes a movabs
; of the symbol's LIVE address, read from this process's own symtab and load bias. The bytes
; run in the love that made them and nowhere else -- which is what a JIT is, and why none of
; this can be written out for later.
; ⚠ LOVE_NO_GLAZE=1 while jitting: a glazed closure is a native cell, and `dis` reads its husk.
; The glaze is the specializing tier this one sits UNDER, not its competition.
;
; Covered: the load family (arg/argN/quote-of-charm/quoN/quomN) said directly, the fld family
; (argcap/argcup/argtwo) UNFUSED into a push and an op, and every handler whose IR the compiler
; carried and which passes the splice condition. A row it cannot say declines the closure BY
; NAME -- that report is the coverage census (doc/moon-regalloc.md, the splice client).
; ⚠ AHEAD OF THE FORM, never inside it: `@` is a macro, so it is in the chain only once its
; layer is spliced -- and a `use` inside the form runs when the form is already compiled.
; gen.l carries the same line for the same reason.
(use 'pat)
(:
   jit-memo (tablet 0)                               ; the two tables, read once and kept
   (jit-say m) (: _ (say err ("splice: " + m + "\n")) ())
   (jit-slurp p) (: q (open p "r") (? q (: s (slurp q) _ (close q) s) ()))

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
             symoff (sh symi 24 8) syment (sh symi 56 8) nsyms (sh symi 32 8 // syment)
             (cstr i) (: (go j) (? (peep exe j 0 = 0) (snip exe i j) (go (j + 1))) (go i))
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
                   rows (? (two? r) <r ())
                   v (: tb (tablet 0)
                        _ (map (\ w (pin tb <w >w)) rows)
                        [1 tb (tally rows)])
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
   (jit-ir nm) (: t (jit-irtab ()) (? (two? t) (peep <>t nm ()) ()))

   ; --- the BRIDGE: a book name to the lvm_ that runs it ------------------------------
   ; `dis` can only name the book key (`cup`) -- every lvm_ nom died at the hatch -- and the
   ; IR table is keyed by the C symbol (`lvm_cup`). nifs.l holds both and lays the map beside
   ; the IR, in this binary, as `ai_nif_lvm`. Same door, same reason: derived from the registry,
   ; so a nif cannot enter one table and miss the other.
   (jit-lvmtab _)
    (: t (peep jit-memo 'lvmtab ())
       (? (two? t) t
          (: exe (bind-rd "/proc/self/exe")
             off (irsym-off exe "ai_nif_lvm")
             (? (nil? off) (jit-say "this love carries no ai_nif_lvm (nifs.h is older than lib/splice.l)")
                (: txt (: (go j) (? (= 0 (peep exe j 0)) (snip exe off j) (go (j + 1))) (go off))
                   r (sound txt)
                   v [1 (? (two? r) <r ())]
                   _ (pin jit-memo 'lvmtab v) v)))))
   (jit-lvmof nm)                                    ; a dis row's nom -> its handler nom | ()
    (? ("lvm_" = snip (string nm) 0 4) nm
       (: t (jit-lvmtab ())
          s (string nm)
          (go l) (@ l ([n h] >< more) (? (s = n) h (go more)) ())
          (? (two? t) (go <>t) ())))

   ; --- THE SPLICER: op rows -> one function's worth of IR ------------------------------
   ; the VM's own convention, and the reason a handler's body needs no rewriting to be
   ; spliced: g=r6, Ip=r5, Hp=r2, Sp=r1, and every op takes its argument from Sp[0] and
   ; leaves its answer there. Two bodies laid end to end already agree about everything.
   jit-tail '((ld r0 r5 8) (add r5 r5 8) (jmpr r0))   ; the dispatch triple every handler ends in
   (dropn l n) (? (n < 1) l (dropn >l (n - 1)))
   (taken l n) (? ((n < 1) || atom? l) () (<l >< taken >l (n - 1)))
   (endswith? l t) (: n (tally l) k (tally t) ((k <= n) && (dropn l (n - k) = t)))
   ; ⚠ THE SPLICE CONDITION, structural and checked rather than assumed. A handler qualifies
   ; only if, tail off, it: touches no frame (the fld family runs `sub sp`), never reads Ip
   ; (an operand-taking op reads its operand FROM the thread, which a splice does not advance),
   ; and leaves itself nowhere -- `jmp lvm_gc` is the one that matters, because gc RESUMES AT
   ; Ip, which in a spliced body is the nif cell, so a collection halfway through would re-run
   ; the ops that already ran. A body failing any of these declines, by name.
   (jit-frame? f) (member? <f '(sub add lea) && member? 'sp >f)      ; anything that moves the stack
   (jit-ipread? f) (member? 'r5 >f)                                 ; ..or reads the thread
   (jit-leaves? f) (? (id? <f 'call) 1 (id? <f 'jmpr) 1             ; ..or leaves for good: an
                      (id? <f 'jmp) !(jit-lbl? <>f) ())             ; internal jmp is the body's own
   (jit-clean? fs)
    (@ fs (f >< r) (? (jit-frame? f || jit-ipread? f || jit-leaves? f) () (jit-clean? r))
          1)
   ; an internal label. ⚠ `jl`, not gen.l's own `.e5044`: a leading dot does not survive the
   ; reader (it is an ordinary punct symbol, and there are no dotted pairs), so the serializer
   ; respells it -- one convention, stated at both ends.
   (jit-lbl? n) (nom? n && ("jl" = snip (string n) 0 2))
   (jit-rn f k)                                      ; this copy's internal labels, made its own
    (? (atom? f) (? (jit-lbl? f) (intern (string f + "_" + show k)) f)
       (jit-rn <f k >< jit-rn >f k))
   ; the push lowerings. an operand-taking op is not spliced, it is SAID -- three forms -- and
   ; the fld family (argcup/argcap/argtwo) is a push FUSED with an op, so the splicer UNFUSES
   ; it: push Sp[i], then splice that op's own handler. The fusion is the VM's, not the meaning's.
   (jit-push-a i) [['ld 'r0 'r1 (8 * i)] '(st r1 -8 r0) '(sub r1 r1 8)]
   (jit-push-q w) [['li 'r0 w] '(st r1 -8 r0) '(sub r1 r1 8)]
   (cwd v) ((2 * v) + 1)                             ; a charm operand as its tagged word
   (jit-fld nm) (? (id? nm 'lvm_argcup) 'cup  (id? nm 'lvm_argcap) 'cap
                   (id? nm 'lvm_argtwo) 'two?  ())
   (jit-splice h k)                                  ; a handler's body, tail off, renamed | ()
    (: fs (jit-ir h)
       (? !(two? fs) (jit-say ("no IR for " + string h + " in this binary"))
          !(endswith? fs jit-tail) (jit-say (string h + " does not end in the dispatch tail"))
          (: b (taken fs (tally fs - tally jit-tail))
             (? (jit-clean? b) (jit-rn b k)
                (jit-say (string h + " is not splice-clean (a frame, a call, or an Ip read)")))))) 
   ; one row -> (forms >< words-pushed) | ()
   (jit-row r k)
    (: nm <r os >r
       ; ⚠ presence rides the wrapper, and it has to: operand 0 is the commonest one there is
       ; (arg 0, the first slot), and a bare `(nil? i)` reads that legitimate zero as absence --
       ; the row then declines in silence, which is exactly how it read the first time.
       i9 (? (two? os && charm? <os) [1 <os] ())
       (via h) (: b (jit-splice h k) (? (nil? b) () (b >< 0)))
       (? (id? nm 'lvm_arg)  (? (two? i9) (jit-push-a <>i9 >< 1) ())
          (id? nm 'lvm_arg0) (jit-push-a 0 >< 1)   (id? nm 'lvm_arg1) (jit-push-a 1 >< 1)
          (id? nm 'lvm_arg2) (jit-push-a 2 >< 1)   (id? nm 'lvm_arg3) (jit-push-a 3 >< 1)
          (id? nm 'lvm_quote) (? (two? i9) (jit-push-q (cwd <>i9) >< 1) ())
          (id? nm 'lvm_quo0) (jit-push-q 1 >< 1)   (id? nm 'lvm_quo1) (jit-push-q 3 >< 1)
          (id? nm 'lvm_quo2) (jit-push-q 5 >< 1)   (id? nm 'lvm_quo3) (jit-push-q 7 >< 1)
          (id? nm 'lvm_quom1) (jit-push-q -1 >< 1) (id? nm 'lvm_quom2) (jit-push-q -3 >< 1)
          (: fl (jit-fld nm)                        ; a fused push-and-op: unfuse it
             h (jit-lvmof (? (nom? fl) fl nm))
             (? (nil? h) (jit-say ("no handler nom for " + string nm))
                (nom? fl) (? (two? i9)
                             (: b (jit-splice h k) (? (nil? b) () ((jit-push-a <>i9 + b) >< 1)))
                             ())
                (two? os) (jit-say ("operand-taking " + string nm + " is not composed yet"))
                (via h)))))
   ; ⚠ THE REFLOW, and it is the whole seam: a handler DELIVERS with (st r1 0 rX) and the next
   ; one OPENS by reading it straight back with (ld rY r1 0). Laid end to end those are adjacent
   ; -- one base, one name, nothing between -- so this is `stldp`'s law with none of its aliasing
   ; question, because both sides are ours. The load goes; a mov stands in when the registers
   ; differ (they never do today, both being r0). This is the op-boundary reload the whole arc
   ; is about, removed at the level where it is a pattern instead of an allocator.
   (jit-seam fs)
    (@ fs (['st 'r1 0 rX] >< ['ld rY 'r1 0] >< rest)
           (['st 'r1 0 rX] >< (? (id? rX rY) () [['mov rY rX]]) + jit-seam rest)
          (f >< rest) (f >< jit-seam rest)
          ())
   ; the whole body: ONE room guard ahead of any Sp motion, deopting to the twin and never to
   ; gc, then the spliced ops, then the dispatch tail advanced past the nif's own two-word cell.
   ; ⚠ the guard is the C route's, read back off its own emission: r7 = Hp + 8*peak + the avail
   ; floor, and an Sp below that takes Ip = cell(Ip[1].x) and continues into the twin -- which
   ; sees an untouched frame, because nothing has moved yet.
   ai-avail-floor 64
   (jit-forms rows)
    (: (go rs k pk net acc)
        (@ rs (r >< more)
              (? (id? <r 'lvm_ret) [acc pk net]
                 (: p (jit-row r k)
                    (? (nil? p) ()
                       (: n2 (net + >p)
                          (go more (k + 1) (? (pk < n2) n2 pk) n2 (acc + <p))))))
              ())
       b (go rows 0 0 0 ())
       (@ b [body0 pk net]
            ('((label jitbody))
             + (? (0 < pk)
                  [['add 'r7 'r2 (8 * pk)] ['add 'r7 'r7 ai-avail-floor] '(cmp r1 r7)
                   '(br ae jitgo) '(ld r5 r5 8) '(ld r0 r5 0) '(jmpr r0) '(label jitgo)]
                  ())
             + jit-seam body0
             ; ⚠ THE COLLAPSE, and without it the answer is a stack deeper than it started:
             ; an arity-1 nif is handed its argument in Sp[0] and owes its answer in that SAME
             ; slot. Every push the body made rides below it, so the result moves back up and Sp
             ; comes home -- Sp[net] = Sp[0]; Sp += net, the C route's own last line.
             + (? (0 < net) [['ld 'r0 'r1 0] ['st 'r1 (8 * net) 'r0] ['add 'r1 'r1 (8 * net)]] ())
             + '((ld r0 r5 16) (add r5 r5 16) (jmpr r0)))
            ()))

   ; --- the live symbol table: this process's own symtab plus its own load bias -----------
   ; we own both sides, so a JIT here resolves a symbol the way a debugger would and never the
   ; way a loader does: /proc/self/exe carries the table holo wrote, and the maps line whose
   ; path is ours and whose file offset is zero carries the bias. ⚠ never the FIRST maps line
   ; -- that is often an unrelated anonymous r-xp region.
   (bind-die m) (: _ (jit-say m) (welp '(bind)))
   (bind-rd p) (: s (jit-slurp p) (? (string? s) s (bind-die ("cannot open " + p))))
   (bind-le s i n) (: (go k a m) (? (k = n) a (go (k + 1) (a + peep s (i + k) 0 * m) (m * 256))) (go 0 0 1))
   (bind-live-tab _) (: exe (bind-rd "/proc/self/exe")
      shoff (bind-le exe 40 8) shentsz (bind-le exe 58 2) shnum (bind-le exe 60 2)
      (sh i f w) (bind-le exe (shoff + i * shentsz + f) w)
      symi (: (go i) (? (i = shnum) -1 (? ((sh i 4 4) = 2) i (go (i + 1)))) (go 0))
      _ (? (0 <= symi) 0 (bind-die "no symtab in /proc/self/exe"))
      stroff (sh (sh symi 40 4) 24 8)
      symoff (sh symi 24 8) syment (sh symi 56 8) nsyms (sh symi 32 8 // syment)
      (cstr i) (: (go j) (? (peep exe j 0 = 0) (snip exe i j) (go (j + 1))) (go i))
      (symva nm) (: (go i) (? (i = nsyms) -1
                     (: o (symoff + i * syment)
                        (? (nm = cstr (stroff + bind-le exe o 4)) (bind-le exe (o + 8) 8) (go (i + 1)))))
                   (go 0))
      exepath (? (member? 'readlink (names ())) (readlink "/proc/self/exe") (bind-die "no readlink nif"))
      base (: mp (bind-rd "/proc/self/maps") n (tally mp)
              (sp? j) (peep mp j 0 = 32)
              (eol? j) ((j = n) || (peep mp j 0 = 10))
              (nlend j) (: (go e) (? (eol? e) e (go (e + 1))) (go j))
              (skipsp j) (: (go e) (? ((e < n) && sp? e) (go (e + 1)) e) (go j))
              (tokend j) (: (go e) (? (eol? e || sp? e) e (go (e + 1))) (go j))
              (fld i f) (: (go j k) (: t (tokend j) (? (k = f) (j >< t) (go (skipsp t) (k + 1)))) (go i 0))
              (hex i e) (: (go j a) (: c (peep mp j 0)
                           (? (e <= j) a
                              (47 < c < 58) (go (j + 1) (a * 16 + c - 48))
                              (96 < c < 103) (go (j + 1) (a * 16 + c - 87))
                              a)) (go i 0))
              (lstart i) (: se (fld i 0) (hex <se (tokend <se)))
              (loff i) (: se (fld i 2) (hex <se >se))
              (lpath i) (: se (fld i 5) (snip mp <se (nlend <se)))
              (go i) (? (n <= i) (bind-die "exe not found in maps")
                        ((loff i = 0) && (exepath = lpath i)) (lstart i)
                        (go (nlend i + 1)))
              (go 0))
      (\ nm (: v (symva nm) (? (v < 0) (bind-die ("no live symbol " + nm)) (base + v)))))

   ; ⚠ NO RELOCATIONS, because a JIT knows the answers: the one external reference a clean
   ; handler carries is (la rX sym), and at runtime that symbol's live address is a NUMBER --
   ; so it becomes (li rX addr), a movabs, and the whole body is position-independent.
   (jit-abs fs lv)
    (@ fs (['la r s] >< rest) (['li r (lv (string s))] >< jit-abs rest lv)
          (f >< rest) (f >< jit-abs rest lv)
          ())
   ; THE DOOR: compose from what the compiler wrote down, assemble with holo, install.
   ; Nothing here is a fallback -- a row this cannot say declines by name and the closure
   ; stays exactly what it was.
   (jit f)
    (: rows (? (member? 'dis (names ())) (dis f) ())
       (? (atom? rows) (jit-say "dis answers nothing (is this closure already native?)")
          (id? <<rows 'lvm_cur) (jit-say "multi-arg closures are not composed yet")
          (: fs (jit-forms rows)
             (? (nil? fs) ()
                (: bs (peep ((from 'holo 'assemble) 'x64 (jit-abs fs (bind-live-tab 0))) 'bytes 0)
                   nf (from 'glaze 'nif)
                   (? !(two? bs) (jit-say "holo assembled nothing")
                      (nil? nf) (jit-say "no glaze nif on this build")
                      (: j (nf (cask bs) f '(\ x x) 1)
                         (? (nil? j) (jit-say "nif declined the bytes") j))))))))
   )
